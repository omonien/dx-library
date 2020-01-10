unit DX.Sparkle.WebBrokerBridge;

interface

uses
  System.Classes, System.SysUtils,
  Web.WebReq, Web.HTTPApp,
  Sparkle.HttpSys.Server, Sparkle.HttpSys.Context, Sparkle.HttpServer.Request, Sparkle.HttpServer.Context
  // ,
  // Loomis.SoapServer.Service.Types
    ;

type
  ESWBException = class(Exception)
  end;

  ESWBInvalidVarIndex = class(ESWBException)
  end;

  TSparkleWebBrokerBridge = class(THttpSysServer)
  private
    FLogProc: TProc<string>;
    FLogRequestProc: TProc<THttpServerRequest>;
  protected
    procedure HandleRequest(
      const ABaseUri:  string;
      ARequestContext: THttpServerContext);
    procedure Log(const AMessage: string);
    procedure LogRequest(ARequest: THttpServerRequest);
    procedure SetLogProc(ALogProc: TProc<string>);
    procedure SetLogRequestProc(ARequestLogProc: TProc<THttpServerRequest>);
  public
    constructor Create(const AURL: string); reintroduce;
    property LogProc: TProc<string> read FLogProc write SetLogProc;
    property LogRequestProc: TProc<THttpServerRequest> read FLogRequestProc write SetLogRequestProc;
  end;

  TSparkleWebBrokerBridgeRequestHandler = class(TWebRequestHandler)
  private
    class var FWebRequestHandler: TSparkleWebBrokerBridgeRequestHandler;
  private
    FRootPath: string;
  protected
    procedure SetRootPath(const Value: string);
  public
    class Constructor Create;
    class Destructor Destroy;
    class function Instance: TSparkleWebBrokerBridgeRequestHandler;
    property RootPath: string read FRootPath write SetRootPath;
    procedure Handle(AHttpServerContext: THttpServerContext);
  end;

  THttpSysResponseCache = class(TObject)
  private
    FContent: string;
    FContentEncoding: string;
    FContentLength: Integer;
    FContentStream: TStream;
    FContentType: string;
    FContentVersion: string;
    FCookies: TCookieCollection;
    FDate: TDateTime;
    FExpires: TDateTime;
    FHeaders: TStrings;
    FLastModified: TDateTime;
    FLocation: string;
    FRealm: string;
    FStatusCode: Integer;
    FStatusReason: string;
    FWWWAuthenticate: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateCookies(AResponse: TWebResponse);
    property AuthRealm: string read FRealm write FRealm;
    property Content: string read FContent write FContent;
    property ContentEncoding: string read FContentEncoding write FContentEncoding;
    property ContentLength: Integer read FContentLength write FContentLength;
    property ContentStream: TStream read FContentStream write FContentStream;
    property ContentType: string read FContentType write FContentType;
    property ContentVersion: string read FContentVersion write FContentVersion;
    property Cookies: TCookieCollection read FCookies;
    property Date: TDateTime read FDate write FDate;
    property Expires: TDateTime read FExpires write FExpires;
    property Headers: TStrings read FHeaders;
    property LastModified: TDateTime read FLastModified write FLastModified;
    property Location: string read FLocation write FLocation;
    property StatusCode: Integer read FStatusCode write FStatusCode;
    // Additional reason, why a status (usually >200) has been set
    property StatusReason: string read FStatusReason write FStatusReason;
    property WWWAuthenticate: string read FWWWAuthenticate write FWWWAuthenticate;
  end;

  TSparkleRequest = class(TWebRequest)
  private
    // This is a reference to the internal HTTP.SYS API request context
    FContext: THttpServerContext;
    FRootPath: string;
  protected
    FContent: String;
    FContentFields: TStrings;
    FRawContent: TBytes;
    FHeaderFields: TStrings;
    FRemoteIP: string;
    // Used to cache response information, until the response gets finally constructed and sent
    FResponseCache: THttpSysResponseCache;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Integer; override;
    function GetRawContent: TBytes; override;
    function GetRawPathInfo: string; override;
    function GetRemoteIP: string; override;
    function GetStringVariable(Index: Integer): string; override;
  public
    constructor Create(AHttpContext: THttpServerContext);
    destructor Destroy; override;
    // Read field values from header and content fields. Header fields are tried first
    function GetFieldByName(const Name: string): string; override;
    function ReadClient(
      var Buffer;
      Count: Integer): Integer; override;
    function ReadString(Count: Integer): string; override;
    function TranslateURI(const URI: string): string; override;
    function WriteClient(
      var ABuffer;
      ACount: Integer): Integer; override;
    function WriteHeaders(
      StatusCode:                  Integer;
      const ReasonString, Headers: string): boolean; override;
    function WriteString(const AString: string): boolean; override;
    // We are using a response cache, to buffer WriteXyz() operations until the actual response is written
    property ResponseCache: THttpSysResponseCache read FResponseCache;
    property RootPath: string read FRootPath write FRootPath;
  end;

  TSparkleResponse = class(TWebResponse)
  private
    FContext: THttpServerContext;
    function GetRequest: TSparkleRequest;
  protected
    FContentType: string;
    FSent: boolean;
    function GetContent: string; override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Integer; override;
    function GetStatusCode: Integer; override;
    function GetStringVariable(Index: Integer): string; override;
    procedure MoveCookies;
    procedure SetContent(const AValue: string); override;
    procedure SetContentStream(AValue: TStream); override;
    procedure SetDateVariable(
      Index:       Integer;
      const Value: TDateTime); override;
    procedure SetIntegerVariable(
      Index: Integer;
      Value: Integer); override;
    procedure SetStatusCode(AValue: Integer); override;
    procedure SetStringVariable(
      Index:       Integer;
      const Value: string); override;
  public
    constructor Create(AHttpRequest: TSparkleRequest);
    procedure SendRedirect(const AURI: string); override;
    procedure SendResponse; override;
    procedure SendStream(AStream: TStream); override;
    function Sent: boolean; override;
    property Request: TSparkleRequest read GetRequest;
  end;

threadvar FCurrentRequest: TSparkleRequest;

implementation

uses
  System.Rtti, System.NetEncoding,

  Sparkle.HttpServer.Module, Sparkle.URI, Sparkle.Middleware.Compress,
  DX.Sparkle.Utils;

resourcestring
  RSHSWBInvalidVarIndex = 'Invalid Variable Index: %d';

const
  // Borland coder didn't define constants in HTTPApp
  INDEX_Method = 0;
  INDEX_ProtocolVersion = 1;
  INDEX_URL = 2;
  INDEX_Query = 3;
  INDEX_PathInfo = 4;
  INDEX_PathTranslated = 5;
  INDEX_CacheControl = 6;
  INDEX_Date = 7;
  INDEX_Accept = 8;
  INDEX_From = 9;
  INDEX_Host = 10;
  INDEX_IfModifiedSince = 11;
  INDEX_Referer = 12;
  INDEX_UserAgent = 13;
  INDEX_ContentEncoding = 14;
  INDEX_ContentType = 15;
  INDEX_ContentLength = 16;
  INDEX_ContentVersion = 17;
  INDEX_DerivedFrom = 18;
  INDEX_Expires = 19;
  INDEX_Title = 20;
  INDEX_RemoteAddr = 21;
  INDEX_RemoteHost = 22;
  INDEX_ScriptName = 23;
  INDEX_ServerPort = 24;
  INDEX_Content = 25;
  INDEX_Connection = 26;
  INDEX_Cookie = 27;
  INDEX_Authorization = 28;

  INDEX_RESP_Version = 0;
  INDEX_RESP_ReasonString = 1;
  INDEX_RESP_Server = 2;
  INDEX_RESP_WWWAuthenticate = 3;
  INDEX_RESP_Realm = 4;
  INDEX_RESP_Allow = 5;
  INDEX_RESP_Location = 6;
  INDEX_RESP_ContentEncoding = 7;
  INDEX_RESP_ContentType = 8;
  INDEX_RESP_ContentVersion = 9;
  INDEX_RESP_DerivedFrom = 10;
  INDEX_RESP_Title = 11;
  // Starting new for integer variables
  INDEX_RESP_ContentLength = 0;
  // Starting new for datetime variables
  INDEX_RESP_Date = 0;
  INDEX_RESP_Expires = 1;
  INDEX_RESP_LastModified = 2;

function SparkleWebBrokerBridgeRequestHandler: TWebRequestHandler;
begin
  result := TSparkleWebBrokerBridgeRequestHandler.Instance;
end;

{ THttpSysWebBrokerBridgeRequestHandler }

class constructor TSparkleWebBrokerBridgeRequestHandler.Create;
begin
  FWebRequestHandler := TSparkleWebBrokerBridgeRequestHandler.Create(nil);
end;

class destructor TSparkleWebBrokerBridgeRequestHandler.Destroy;
begin
  FreeAndNil(FWebRequestHandler);
end;

class function TSparkleWebBrokerBridgeRequestHandler.Instance: TSparkleWebBrokerBridgeRequestHandler;
begin
  result := FWebRequestHandler;
end;

procedure TSparkleWebBrokerBridgeRequestHandler.SetRootPath(const Value: string);
begin
  FRootPath := Value;
end;

procedure TSparkleWebBrokerBridgeRequestHandler.Handle(AHttpServerContext: THttpServerContext);
var
  LWebRequest: TSparkleRequest;
  LWebResponse: TSparkleResponse;
begin
  LWebRequest := nil;
  LWebResponse := nil;
  try
    LWebRequest := TSparkleRequest.Create(AHttpServerContext);
    LWebRequest.RootPath := Self.RootPath;
    LWebResponse := TSparkleResponse.Create(LWebRequest);
    HandleRequest(LWebRequest, LWebResponse);
  finally
    FreeAndNil(LWebResponse);
    FreeAndNil(LWebRequest);
  end;

end;

constructor TSparkleWebBrokerBridge.Create(const AURL: string);
var
  LModule: TAnonymousServerModule;
  LBaseURL: string;
begin
  inherited Create;
  LBaseURL := AURL;
  LModule := TAnonymousServerModule.Create(AURL,
    procedure(const AContext: THttpServerContext)
    begin
      // Todo:  FCurrentRequest := AContext.Request;
      LogRequest(AContext.Request);
      try
        HandleRequest(LBaseURL, AContext);
        // Todo: Response loggen ...
        Log('Response StatusCode: ' + AContext.Response.StatusCode.ToString);
        Log('Response Headers: ' + AContext.Response.Headers.RawWideHeaders);
      except
        on E: Exception do
        begin
          Log(E.Message);
          raise;
        end;
      end;
    end);
  LModule.AddMiddleware(TCompressMiddleware.Create);
  Dispatcher.AddModule(LModule);
end;

procedure TSparkleWebBrokerBridge.HandleRequest(
  const ABaseUri:  string;
  ARequestContext: THttpServerContext);
var
  LBaseURL: TUri;
begin
  LBaseURL := TUri.Create(ABaseUri);
  try
    TSparkleWebBrokerBridgeRequestHandler.Instance.RootPath := LBaseURL.Path;
    TSparkleWebBrokerBridgeRequestHandler.Instance.Handle(ARequestContext);
  finally
    FreeAndNil(LBaseURL);
  end;
end;

procedure TSparkleWebBrokerBridge.Log(const AMessage: string);
begin
  if Assigned(FLogProc) then
  begin
    FLogProc(AMessage);
  end;
end;

procedure TSparkleWebBrokerBridge.LogRequest(ARequest: THttpServerRequest);
begin
  // First, check if there is a dedicated LogRequest procedure
  if Assigned(FLogRequestProc) then
  begin
    FLogRequestProc(ARequest);
  end
  else
  begin
    // if not, try a simple log
    Log('Incoming request from ' + ARequest.RemoteIP);
    Log('Path: ' + ARequest.RawUri);
  end;
end;

procedure TSparkleWebBrokerBridge.SetLogProc(ALogProc: TProc<string>);
begin
  FLogProc := ALogProc;
end;

procedure TSparkleWebBrokerBridge.SetLogRequestProc(ARequestLogProc: TProc<THttpServerRequest>);
begin
  FLogRequestProc := ARequestLogProc;
end;

{ THTTPRequest }

constructor TSparkleRequest.Create(AHttpContext: THttpServerContext);
var
  LContentType: string;
begin
  FContext := AHttpContext;
  FRawContent := FContext.Request.Content;
  inherited Create;
  FResponseCache := THttpSysResponseCache.Create;

  LContentType := '';
  FContext.Request.Headers.GetIfExists('Content-Type', LContentType);
  FContent := Decode(FContext.Request.Content, LContentType);

  FContentFields := TStringList.Create;
  FContentFields.NameValueSeparator := '=';

  FHeaderFields := TStringList.Create(TDuplicates.dupIgnore, true, false);
  FHeaderFields.NameValueSeparator := ':';
  FHeaderFields.Text := FContext.Request.Headers.RawWideHeaders;

  FRemoteIP := GetFieldByName('RemoteIP');
end;

destructor TSparkleRequest.Destroy;
begin
  FreeAndNil(FResponseCache);
  FreeAndNil(FContentFields);
  FreeAndNil(FHeaderFields);
  inherited;
end;

function TSparkleRequest.GetDateVariable(Index: Integer): TDateTime;
var
  LValue: string;
begin
  LValue := string(GetStringVariable(Index));
  if Length(LValue) > 0 then
  begin
    result := ParseDate(LValue);
  end
  else
  begin
    result := -1;
  end;
end;

function TSparkleRequest.GetFieldByName(const Name: string): string;
var
  i: Integer;
  LName: string;
begin
  // First check header, then check content fields (for POST/PUT)
  Assert(Assigned(FHeaderFields));
  Assert(Assigned(FContentFields));
  LName := Name.Trim;
  result := '';

  // First check if it's a header field
  i := FHeaderFields.IndexOfName(LName);
  if i >= 0 then
  begin
    result := FHeaderFields.ValueFromIndex[i];
  end
  else
  begin
    // Now check if it's a content field
    i := FContentFields.IndexOfName(LName);
    if i >= 0 then
      result := FContentFields.ValueFromIndex[i];
  end;
  result := result.Trim;
end;

function TSparkleRequest.GetIntegerVariable(Index: Integer): Integer;
begin
  result := StrToIntDef(string(GetStringVariable(Index)), -1)
end;

function TSparkleRequest.GetRawContent: TBytes;
begin
  result := FRawContent;
end;

function TSparkleRequest.GetRawPathInfo: string;
begin
  result := PathInfo;
end;

function TSparkleRequest.GetRemoteIP: string;
begin
  result := FRemoteIP;
end;

function TSparkleRequest.GetStringVariable(Index: Integer): string;
var
  LValue: string;
begin
  case Index of
    INDEX_Method:
      LValue := FContext.Request.Method;
    INDEX_ProtocolVersion:
      LValue := '';
    // Todo: Decode(FHttpServerRequest.ProtocolVersion);
    INDEX_URL: // Full URL: http://example.com:80/some/path?param=value
      LValue := FContext.Request.URI.OriginalUri;
    INDEX_Query:
      begin
        LValue := FContext.Request.URI.Query;
        if LValue.StartsWith('?') then
        begin
          LValue := LValue.Remove(0, 1);
        end;
      end;
    INDEX_PathInfo, INDEX_PathTranslated:
      // There is no path translation
      begin
        // Url Path without root and without query part
        // http://example.com:80/root/some/path?param=value
        // => /some/path
        LValue := FContext.Request.URI.Path.Trim;

        if LValue.StartsText(RootPath, LValue) then
        begin
          LValue := LValue.Replace(RootPath, '');
        end;

      end;
    INDEX_CacheControl:
      LValue := GetFieldByName('Cache-Control'); { do not localize }
    INDEX_Date:
      LValue := GetFieldByName('Date'); { do not localize }
    INDEX_Accept:
      LValue := GetFieldByName('Accept'); { do not localize }
    INDEX_From:
      LValue := GetFieldByName('From'); { do not localize }
    INDEX_Host:
      LValue := GetFieldByName('Host'); { do not localize }
    INDEX_IfModifiedSince:
      LValue := GetFieldByName('If-Modified-Since'); { do not localize }
    INDEX_Referer:
      LValue := GetFieldByName('Referer'); { do not localize }
    INDEX_UserAgent:
      LValue := GetFieldByName('User-Agent'); { do not localize }
    INDEX_ContentEncoding: // Actually not defined for HTTP requests (see RFC 2616), but implemented in TWebRequest
      LValue := GetFieldByName('Content-Encoding'); { do not localize }
    INDEX_ContentType:
      LValue := GetFieldByName('Content-Type');
    INDEX_ContentLength:
      LValue := IntToStr(Length(FRawContent));
    INDEX_ContentVersion:
      // Actually not defined for HTTP requests (see RFC 2616), but implemented in TWebRequest
      LValue := GetFieldByName('CONTENT_VERSION'); { do not localize }
    INDEX_DerivedFrom: // Actually not defined for HTTP requests (see RFC 2616), but implemented in TWebRequest
      LValue := GetFieldByName('Derived-From'); { do not localize }
    INDEX_Expires: // Actually not defined for HTTP requests (see RFC 2616), but implemented in TWebRequest
      LValue := GetFieldByName('Expires'); { do not localize }
    INDEX_Title: // Actually not defined for HTTP requests (see RFC 2616), but implemented in TWebRequest
      LValue := GetFieldByName('Title'); { do not localize }
    INDEX_RemoteAddr: // Actually not defined for HTTP requests (see RFC 2616), here it is injected by SynCrtSock!
      LValue := GetFieldByName('RemoteIP'); { do not localize }
    INDEX_RemoteHost: // Actually not defined for HTTP requests (see RFC 2616), but implemented in TWebRequest
      LValue := GetFieldByName('REMOTE_HOST'); { do not localize }
    INDEX_ScriptName:
      LValue := RootPath; // instead of a script we have a root portion of the path

    INDEX_ServerPort:
      begin
        LValue := FContext.Request.URI.Port.ToString
      end;

    INDEX_Content:
      LValue := FContent;

    INDEX_Connection:
      LValue := GetFieldByName('Connection'); { do not localize }
    INDEX_Cookie:
      LValue := GetFieldByName('Cookie'); { do not localize }
    INDEX_Authorization:
      LValue := GetFieldByName('Authorization'); { do not localize }
  else
    LValue := '';
  end;
  result := LValue.Trim;
end;

function TSparkleRequest.ReadClient(
  var Buffer;
  Count: Integer): Integer;
begin
  raise ENotImplemented.Create('Not implemented');
  // result := FRawContent.Read(Buffer, Count);
  // well, it shouldn't be less than 0. but let's not take chances
  if result < 0 then
  begin
    result := 0;
  end;
end;

function TSparkleRequest.ReadString(Count: Integer): string;
var
  LBuffer: TBytes;
begin
  raise ENotImplemented.Create('Not implemented');
  result := EncodingGetString(ContentType, LBuffer);
end;

function TSparkleRequest.TranslateURI(const URI: string): string;
begin
  // There is no concept of a path translation.
  result := URI;
end;

function TSparkleRequest.WriteClient(
  var ABuffer;
  ACount: Integer): Integer;
var
  LBuffer: TBytes;
begin
  SetLength(LBuffer, ACount);
  ResponseCache.Content := ResponseCache.Content + EncodingGetString(ContentType, LBuffer);
  result := ACount;
end;

function TSparkleRequest.WriteHeaders(
  StatusCode:                  Integer;
  const ReasonString, Headers: string): boolean;
begin
  // Writing to internal response cache
  FResponseCache.Headers.Add(Headers);
  FResponseCache.StatusCode := StatusCode;
  FResponseCache.StatusReason := ReasonString;
  result := true;
end;

function TSparkleRequest.WriteString(const AString: string): boolean;
begin
  FResponseCache.Content := FResponseCache.Content + AString;
  result := true;
end;

{ THTTPResponse }

constructor TSparkleResponse.Create(AHttpRequest: TSparkleRequest);
begin
  inherited Create(AHttpRequest);
  FSent := false;
  FContext := AHttpRequest.FContext;

  CustomHeaders.NameValueSeparator := ':';
  Version := FContext.Request.MajorVersion.ToString + '.' + FContext.Request.MinorVersion.ToString;
  StatusCode := 200;
  LastModified := -1;
  Expires := -1;
  Date := -1;
  ContentType := 'text/html'; { do not localize }
end;

function TSparkleResponse.GetContent: string;
begin
  result := Request.FResponseCache.Content;
end;

function TSparkleResponse.GetDateVariable(Index: Integer): TDateTime;
begin
  case Index of
    INDEX_RESP_Date:
      result := Request.ResponseCache.Date;
    INDEX_RESP_Expires:
      result := Request.ResponseCache.Expires;
    INDEX_RESP_LastModified:
      result := Request.ResponseCache.LastModified;
  else
    raise ESWBInvalidVarIndex.Create(Format(RSHSWBInvalidVarIndex, [Index]));
  end;

end;

function TSparkleResponse.GetIntegerVariable(Index: Integer): Integer;
begin
  case Index of
    INDEX_RESP_ContentLength:
      result := Request.ResponseCache.ContentLength;
  else
    raise ESWBInvalidVarIndex.Create(Format(RSHSWBInvalidVarIndex, [Index]));
  end;
end;

function TSparkleResponse.GetRequest: TSparkleRequest;
begin
  result := HTTPRequest as TSparkleRequest;
end;

function TSparkleResponse.GetStatusCode: Integer;
begin
  result := Request.FResponseCache.StatusCode;
end;

function TSparkleResponse.GetStringVariable(Index: Integer): string;
var
  LValue: string;
begin
  case Index of
    INDEX_RESP_Version:
      LValue := ''; // Todo:Decode(FHttpSysRequest.ProtocolVersion);
    INDEX_RESP_ReasonString:
      LValue := Request.ResponseCache.StatusReason;
    INDEX_RESP_Server:
      LValue := FContext.Request.URI.Host; // Todo: Test?
    INDEX_RESP_WWWAuthenticate:
      LValue := Request.ResponseCache.WWWAuthenticate;
    INDEX_RESP_Realm:
      LValue := Request.ResponseCache.AuthRealm;
    INDEX_RESP_Allow:
      LValue := Request.ResponseCache.Headers.Values['Allow']; { do not localize }
    INDEX_RESP_Location:
      LValue := Request.ResponseCache.Location;
    INDEX_RESP_ContentEncoding:
      LValue := Request.ResponseCache.ContentEncoding;
    INDEX_RESP_ContentType:
      LValue := Request.ResponseCache.ContentType;
    INDEX_RESP_ContentVersion:
      LValue := Request.ResponseCache.ContentVersion;
    INDEX_RESP_DerivedFrom:
      LValue := Request.ResponseCache.Headers.Values['Derived-From']; { do not localize }
    INDEX_RESP_Title:
      LValue := Request.ResponseCache.Headers.Values['Title']; { do not localize }
  else
    raise ESWBInvalidVarIndex.Create(Format(RSHSWBInvalidVarIndex, [Index]));
  end;
  result := LValue.Trim;
end;

procedure TSparkleResponse.MoveCookies;
var
  i: Integer;
  LSrcCookie: TCookie;
  LDestCookie: TCookie;
begin
  Request.FResponseCache.CreateCookies(Self);
  for i := 0 to Cookies.Count - 1 do
  begin
    LSrcCookie := Cookies[i];
    LDestCookie := Request.FResponseCache.Cookies.Add;
    LDestCookie.Name := TNetEncoding.URL.Encode(LSrcCookie.Name);
    LDestCookie.Value := TNetEncoding.URL.Encode(LSrcCookie.Value);
    LDestCookie.Domain := LSrcCookie.Domain;
    LDestCookie.Path := LSrcCookie.Path;
    LDestCookie.Expires := LSrcCookie.Expires;
    LDestCookie.Secure := LSrcCookie.Secure;
  end;
end;

procedure TSparkleResponse.SendRedirect(const AURI: string);
begin
  FSent := true;
  Request.FResponseCache.StatusCode := 302;
  Request.FResponseCache.Location := AURI;
  SendResponse;
end;

procedure TSparkleResponse.SendResponse;
begin
  FSent := true;

  MoveCookies;

  FContext.Response.Headers.RawWideHeaders := Request.FResponseCache.Headers.Text;
  FContext.Response.StatusCode := Request.FResponseCache.StatusCode;

  if (Request.FResponseCache.ContentType = '') and
    ((Request.FResponseCache.Content <> '') or (Assigned(Request.FResponseCache.ContentStream))) then
  begin
    // Default content-type/encoding
    FContext.Response.ContentType := Format('text/html; charset=%s', [Web.HTTPApp.DefaultCharSet]); { Do not Localize }
  end
  else
  begin
    FContext.Response.ContentType := Request.FResponseCache.ContentType;
  end;

  if Assigned(Request.FResponseCache.ContentStream) then
  begin
    FContext.Response.Content.CopyFrom(Request.FResponseCache.ContentStream, 0);
  end
  else
  begin
    // Todo: check real Encoding
    FContext.Response.Close(TEncoding.UTF8.GetBytes(Request.FResponseCache.Content));
  end;

end;

procedure TSparkleResponse.SendStream(AStream: TStream);
begin
  // FThread.Connection.IOHandler.Write(AStream);
end;

function TSparkleResponse.Sent: boolean;
begin
  result := FSent;
end;

procedure TSparkleResponse.SetContent(const AValue: string);
begin
  Request.FResponseCache.Content := AValue;
  Request.FResponseCache.ContentLength := Length(AValue);
end;

procedure TSparkleResponse.SetContentStream(AValue: TStream);
begin
  inherited;
  Request.FResponseCache.ContentStream := AValue;
end;

procedure TSparkleResponse.SetDateVariable(
  Index:       Integer;
  const Value: TDateTime);
begin
  // Dates should be passed in as GMT!
  case Index of
    INDEX_RESP_Date:
      Request.ResponseCache.Date := Value;
    INDEX_RESP_Expires:
      Request.ResponseCache.Expires := Value;
    INDEX_RESP_LastModified:
      Request.ResponseCache.LastModified := Value;
  else
    raise ESWBInvalidVarIndex.Create(Format(RSHSWBInvalidVarIndex, [Index]));
  end;
end;

procedure TSparkleResponse.SetIntegerVariable(Index, Value: Integer);
begin
  case Index of
    INDEX_RESP_ContentLength:
      Request.ResponseCache.ContentLength := Value;
  else
    raise ESWBInvalidVarIndex.Create(Format(RSHSWBInvalidVarIndex, [Index]));
  end;
end;

procedure TSparkleResponse.SetStatusCode(AValue: Integer);
begin
  Request.FResponseCache.StatusCode := AValue;
end;

procedure TSparkleResponse.SetStringVariable(
  Index:       Integer;
  const Value: string);
var
  LValue: string;
begin
  LValue := Value.Trim;

  case Index of
    INDEX_RESP_Version:
      ; // ignore, will use same version as it's request

    INDEX_RESP_ReasonString:
      Request.ResponseCache.StatusReason := LValue;
    INDEX_RESP_Server:
      ;
    // Ignore, server name cannot be set per individual request
    INDEX_RESP_WWWAuthenticate:
      Request.ResponseCache.WWWAuthenticate := Value;
    INDEX_RESP_Realm:
      Request.ResponseCache.AuthRealm := LValue;
    INDEX_RESP_Allow:
      Request.ResponseCache.Headers.Values['Allow'] := LValue; { do not localize }
    INDEX_RESP_Location:
      Request.ResponseCache.Location := LValue;
    INDEX_RESP_ContentEncoding:
      Request.ResponseCache.ContentEncoding := LValue;
    INDEX_RESP_ContentType:
      Request.ResponseCache.ContentType := Value;
    INDEX_RESP_ContentVersion:
      Request.ResponseCache.ContentVersion := LValue;
    INDEX_RESP_DerivedFrom:
      Request.ResponseCache.Headers.Values['Derived-From'] := LValue; { do not localize }
    INDEX_RESP_Title:
      Request.ResponseCache.Headers.Values['Title'] := LValue; { do not localize }
  else
    raise ESWBInvalidVarIndex.Create(Format(RSHSWBInvalidVarIndex, [Index]));
  end;
end;

{ THTTPResponseCache }

constructor THttpSysResponseCache.Create;
begin
  inherited;
  FHeaders := TStringList.Create(TDuplicates.dupIgnore, true, false);
  FHeaders.NameValueSeparator := ':';
  FStatusCode := 0;
  FStatusReason := '';
  FContent := '';
  FContentStream := nil;
  FCookies := nil;
end;

procedure THttpSysResponseCache.CreateCookies(AResponse: TWebResponse);
begin
  FreeAndNil(FCookies);
  FCookies := TCookieCollection.Create(AResponse, TCookie);
end;

destructor THttpSysResponseCache.Destroy;
begin
  FreeAndNil(FCookies);
  FreeAndNil(FHeaders);
  inherited;
end;

initialization

Web.WebReq.WebRequestHandlerProc := SparkleWebBrokerBridgeRequestHandler;

end.
