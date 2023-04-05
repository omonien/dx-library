unit DX.Sparkle.WebBrokerBridge;

interface

uses
  System.Classes, System.SysUtils,
  Web.WebReq, Web.HTTPApp,
  Sparkle.HttpSys.Server, Sparkle.HttpSys.Context, Sparkle.HttpServer.Request, Sparkle.HttpServer.Context,
  Sparkle.Security, Sparkle.HttpServer.Module;

type
  ESWBException = class(Exception)
  end;

  ESWBInvalidVarIndex = class(ESWBException)
  end;

  TSparkleWebBrokerBridge = class(THttpSysServer)
  public
{$SCOPEDENUMS ON}
    type
    TLogItem = (Request, Response);
    TLogItems = set of TLogItem;
  private
    FLogProc: TProc<string>;
    FBaseURL: string;
    FLogItems: TLogItems;
    FLogRequestProc: TProc<THttpServerRequest>;
    FLogResponseProc: TProc<THttpServerResponse>;
    FServerModule: THttpServerModule;
  protected
    procedure HandleRequest(
      const ABaseUri: string;
      ARequestContext: THttpServerContext);
    procedure Log(const AMessage: string);
    procedure LogRequest(ARequest: THttpServerRequest);
    procedure LogResponse(AResponse: THttpServerResponse);
    procedure SetLogProc(ALogProc: TProc<string>);
    procedure SetLogRequestProc(const Value: TProc<THttpServerRequest>);
    procedure SetLogResponseProc(const Value: TProc<THttpServerResponse>);
  public
    constructor Create(const AURL: string); reintroduce;
    property BaseURL: string read FBaseURL;
    property LogProc: TProc<string> read FLogProc write SetLogProc;
    property LogRequestProc: TProc<THttpServerRequest> read FLogRequestProc write SetLogRequestProc;
    property LogResponseProc: TProc<THttpServerResponse> read FLogResponseProc write SetLogResponseProc;
    property LogItems: TLogItems read FLogItems write FLogItems;
    property ServerModule: THttpServerModule read FServerModule;
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
    FUser: IUserIdentity;
  protected
    FContent: String;
    FContentFields: TStrings;
    FRawContent: TBytes;
    FHeaderFields: TStrings;
    FRemoteIP: string;
    // Used to cache response information, until the response gets finally constructed and sent
    FResponseCache: THttpSysResponseCache;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Int64; override;
    function GetRawContent: TBytes; override;
    function GetRawPathInfo: string; override;
    function GetRemoteIP: string; override;
    function GetStringVariable(Index: Integer): string; override;
  public
    constructor Create(AHttpContext: THttpServerContext);
    destructor Destroy; override;
    // Read field values from header and content fields. Header fields are tried first
    function GetFieldByName(const Name: string): string; override;
    function ReadClient(var Buffer; Count: Integer): Integer; override;
    function ReadString(Count: Integer): string; override;
    function TranslateURI(const URI: string): string; override;
    function WriteClient(var ABuffer; ACount: Integer): Integer; override;
    function WriteHeaders(StatusCode: Integer; const ReasonString, Headers: string): boolean; override;
    function WriteString(const AString: string): boolean; override;
    procedure InjectHeader(const AHeader: string; const AValue: string); virtual;
    procedure InjectSOAPActionHeader(const ASOAPNameSpace: string); virtual;
    // We are using a response cache, to buffer WriteXyz() operations until the actual response is written
    property ResponseCache: THttpSysResponseCache read FResponseCache;
    property RootPath: string read FRootPath write FRootPath;
    property User: IUserIdentity read FUser write FUser;
  end;

  TSparkleResponse = class(TWebResponse)
  private
    FContext: THttpServerContext;
    function GetRequest: TSparkleRequest;
  protected
    FContentType: string;
    FSent: boolean;
    FLogMessage: String;
    function GetContent: string; override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Int64; override;
    function GetLogMessage: string; override;
    function GetStatusCode: Integer; override;
    function GetStringVariable(Index: Integer): string; override;
    procedure MoveCookies;
    procedure SetContent(const AValue: string); override;
    procedure SetContentStream(AValue: TStream); override;
    procedure SetDateVariable(
      Index: Integer;
      const Value: TDateTime); override;
    procedure SetIntegerVariable(
      Index: Integer;
      Value: Int64); override;
    procedure SetStatusCode(AValue: Integer); override;
    procedure SetStringVariable(
      Index: Integer;
      const Value: string); override;
    procedure SetCurrentResponseContent(AStream: TStream); overload;
    procedure SetLogMessage(const Value: String); override;
  public
    constructor Create(AHttpRequest: TSparkleRequest);
    procedure SendRedirect(const AURI: string); override;
    procedure SendResponse; override;
    procedure SendStream(AStream: TStream); override;
    function Sent: boolean; override;
    property Request: TSparkleRequest read GetRequest;
  end;

implementation

uses
  System.Rtti, System.NetEncoding,
  Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc, Xml.adomxmldom, Xml.Win.msxmldom, Xml.omnixmldom,
  Sparkle.URI, Sparkle.Middleware.Compress,
  DX.Sparkle.Utils, DX.Utils.Logger;

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

threadvar FCurrentResponseContent: string;

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
begin
  inherited Create;
  FLogProc := nil;
  FLogRequestProc := nil;
  FLogResponseProc := nil;
{$IFDEF DEBUG}
  FLogItems := [TLogItem.Request, TLogItem.Response];
{$ELSE}
  FLogItems := [];
{$ENDIF}
  FBaseURL := AURL;
  FServerModule := TAnonymousServerModule.Create(AURL,
    procedure(const AContext: THttpServerContext)
    begin
      // GET /monitor is not logged!
      var
      LLog := not((AContext.Request.MethodType = THttpMethod.Get) and
        (AContext.Request.URI.Path.Trim.ToLower.EndsWith('monitor')));

      if LLog then
        LogRequest(AContext.Request);
      try
        HandleRequest(FBaseURL, AContext);
        if LLog then
          LogResponse(AContext.Response);
      except
        on E: Exception do
        begin
          Log(E.Message);
          raise;
        end;
      end;
    end);
  FServerModule.AddMiddleware(TCompressMiddleware.Create);
  Dispatcher.AddModule(FServerModule);
end;

procedure TSparkleWebBrokerBridge.HandleRequest(
  const ABaseUri: string;
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
  end
  else
  begin
    DXLog(AMessage);
  end;
end;

procedure TSparkleWebBrokerBridge.LogRequest(ARequest: THttpServerRequest);
begin
  if Assigned(FLogRequestProc) then
  begin
    FLogRequestProc(ARequest);
  end
  else if TLogItem.Request in LogItems then
  begin
    Log('Request URL: ' + ARequest.URI.OriginalUri);
    Log('Request Remote IP: ' + ARequest.RemoteIp);
    Log('Request Headers: ' + ARequest.Headers.RawWideHeaders.Replace(#13#10, ' || '));
    Log('Request Content: ' + TEncoding.UTF8.GetString(ARequest.Content));
  end;
end;

procedure TSparkleWebBrokerBridge.LogResponse(AResponse: THttpServerResponse);
begin
  if Assigned(FLogResponseProc) then
  begin
    FLogResponseProc(AResponse);
  end
  else if TLogItem.Response in LogItems then
  begin
    Log('Response Status: ' + AResponse.StatusCode.ToString);
    Log('Response Headers: ' + AResponse.Headers.RawWideHeaders.Replace(#13#10, ' || '));
    Log('Response Content: ' + FCurrentResponseContent);
  end;
end;

procedure TSparkleWebBrokerBridge.SetLogProc(ALogProc: TProc<string>);
begin
  FLogProc := ALogProc;
  // Default: set all log items
  FLogItems := [TLogItem.Request, TLogItem.Response];
end;

procedure TSparkleWebBrokerBridge.SetLogRequestProc(const Value: TProc<THttpServerRequest>);
begin
  FLogRequestProc := Value;
end;

procedure TSparkleWebBrokerBridge.SetLogResponseProc(const Value: TProc<THttpServerResponse>);
begin
  FLogResponseProc := Value;
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

  FRemoteIP := AHttpContext.Request.RemoteIp;

  FUser := AHttpContext.Request.User;
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
    begin
      result := FContentFields.ValueFromIndex[i];
    end;
  end;
  result := result.Trim;
end;

function TSparkleRequest.GetIntegerVariable(Index: Integer): Int64;
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

procedure TSparkleRequest.InjectHeader(const AHeader, AValue: string);
begin
  var
  i := FHeaderFields.IndexOfName(AHeader);
  if i >= 0 then
  begin
    // Die Header sind als TSTringlist implementiert und sortiert
    // Sortierte Stringlisten lassen sich schlecht manipulieren
    (FHeaderFields as TStringList).Sorted := false;
    FHeaderFields.Values[AHeader] := AValue;
    (FHeaderFields as TStringList).Sorted := true;
  end
  else
  begin
    FHeaderFields.AddPair(AHeader, AValue);
  end;
end;

procedure TSparkleRequest.InjectSOAPActionHeader(const ASOAPNameSpace: string);
var
  LAction: string;
begin
  var
  LSOAPEnvelope := TXMLDocument.Create(nil);
  try
    // MSXMLDOM ist vermutlich besser/schneller, aber für OmniXML sind keine DLLs erforderlich!
    // Und OpenXML funktioniert nicht richtig
    LSOAPEnvelope.DOMVendor := GetDOMVendor(OmniXML4Factory.Description);
    LSOAPEnvelope.LoadFromXML(Self.Content);
    if not Assigned(LSOAPEnvelope.DOMDocument) or not Assigned(LSOAPEnvelope.DOMDocument.documentElement) then
      raise Exception.Create('Kein SOAP-Envelope im Request gefunden!');
    // <?xml version="1.0" encoding="UTF-8"?>
    // <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
    // <soapenv:Body>
    // <v4:CreateTicket xmlns:v4="http://w
    var
    LRootNode := LSOAPEnvelope.DOMDocument.documentElement;
    if not(LRootNode.hasChildNodes) then
      raise Exception.Create('SOAP-Envelope invalid');
    var
    LSoapBody := LRootNode.childNodes[0];

    if not LSoapBody.hasChildNodes then
      raise Exception.Create('SOAP-Envelope invalid');
    // Die erste Child-Node im Body ist die Action
    var
    LActionNode := LSoapBody.childNodes[0];
    // <v4:CreateTicket ...>
    LAction := LActionNode.nodeName.Split([':'])[1];
  finally
    FreeAndNil(LSOAPEnvelope);
  end;
  Self.InjectHeader('soapaction', ASOAPNameSpace + LAction);

end;

function TSparkleRequest.ReadClient(
  var Buffer;
Count: Integer): Integer;
begin
  raise ENotImplemented.Create('Not implemented');
  // result := FRawContent.Read(Buffer, Count);
end;

function TSparkleRequest.ReadString(Count: Integer): string;
begin
  raise ENotImplemented.Create('Not implemented');
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
  StatusCode: Integer;
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

function TSparkleResponse.GetIntegerVariable(Index: Integer): Int64;
begin
  case Index of
    INDEX_RESP_ContentLength:
      result := Request.ResponseCache.ContentLength;
  else
    raise ESWBInvalidVarIndex.Create(Format(RSHSWBInvalidVarIndex, [Index]));
  end;
end;

function TSparkleResponse.GetLogMessage: string;
begin
  result := FLogMessage;
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

procedure TSparkleResponse.SetCurrentResponseContent(AStream: TStream);
var
  LStreamBytes: TBytes;
begin
  if Assigned(AStream) then
  begin
    SetLength(LStreamBytes, AStream.Size);
    AStream.Position := 0;
    AStream.Read(LStreamBytes, AStream.Size);
    FCurrentResponseContent := TEncoding.UTF8.GetString(LStreamBytes);
  end;
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
    SetCurrentResponseContent(Request.FResponseCache.ContentStream);
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
  Index: Integer;
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

procedure TSparkleResponse.SetIntegerVariable(Index: Integer; Value: Int64);
begin
  case Index of
    INDEX_RESP_ContentLength:
      Request.ResponseCache.ContentLength := Value;
  else
    raise ESWBInvalidVarIndex.Create(Format(RSHSWBInvalidVarIndex, [Index]));
  end;
end;

procedure TSparkleResponse.SetLogMessage(const Value: String);
begin
  FLogMessage := Value;
end;

procedure TSparkleResponse.SetStatusCode(AValue: Integer);
begin
  Request.FResponseCache.StatusCode := AValue;
end;

procedure TSparkleResponse.SetStringVariable(
  Index: Integer;
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
