///	<summary>
///	  DX.REST.Client is a library that simplifies the creation of specific REST
///	  API Clients.
///	</summary>
///	<remarks>
///	  DX.Rest.Client is part of DX.Librayry<br />See:
///	  http://code.google.com/p/dx-library/
///	</remarks>
unit DX.REST.Client;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  IdHttp, IdCookie, DBXJson;

const
  API_VERSION = '0.3';

type

  /// <summary>
  /// Standard REST Methods
  /// </summary>
  TRESTRequestMethod = (rmPOST, rmPUT, rmGET, rmDELETE);

  /// <summary>
  /// Content types as used in requests.
  /// </summary>
  TContentType = (
    /// <summary>
    /// No content type specified
    /// </summary>
    ctNone,

    /// <summary>
    /// application/x-www-form-urlencoded
    /// </summary>
    ctURLEncoded,

    /// <summary>
    /// application/json
    /// </summary>
    ctJson);

  /// <summary>
  /// Exceptions coming from the API have this a common ancestor
  /// </summary>
  ERESTClientException = class(Exception)
  private
    FErrorCode: integer;
    procedure SetErrorCode(const Value: integer);
  public
    constructor Create(AErrorCode: integer; const AMsg: string);
    property ErrorCode: integer read FErrorCode write SetErrorCode;
  end;

  /// <summary>
  /// TRESTClient is typically used by inheriting it in a specific REST API
  /// class.
  /// </summary>
  /// <example>
  /// TNetflixAPI = class(TRESTClient)
  /// </example>
  TRESTClient = class(TObject)
  private
    FBaseURL: string;
    FHttpClient: TIdHttp;
    FPassword: string;
    FProxyPort: integer;
    FProxyServer: string;
    FUserAgent: string;
    FUserName: string;
    function GetResponse: TIdHTTPResponse;
    function Request(AMethod: TRESTRequestMethod; const AQuery: string; const ABody: string = '';
      AContentType: TContentType = ctJson): string;
    procedure SetPassword(const Value: string);
    procedure SetProxyPort(const Value: integer);
    procedure SetProxyServer(const Value: string);
    procedure SetUserAgent(const Value: string);
    procedure SetUserName(const Value: string);
  protected
    procedure DoDeleteRequest(const AQuery: string);
    function DoGetRequest(const AQuery: string): string;
    function DoPostRequest(const AQuery: string; const ABody: string = ''; AContentType: TContentType = ctJson): string;
    function DoPutRequest(const AQuery: string; const ABody: string = ''; AContentType: TContentType = ctJson): string;
    /// <summary>
    /// Sends a GET request and tries to map the result to a single instance
    /// of class &lt;T&gt;
    /// </summary>
    function GetEntity<T: class, constructor>(const AQuery: string): T;
    /// <summary>
    /// Sends a GET request and tries to map the result to a List of instances
    /// of class &lt;T&gt;
    /// </summary>
    function GetEntityList<T: class>(const AQuery: string): TObjectList<T>;

    function InsertEntity<T: class>(const AQuery: string; AEntity: T): TJSONObject;

  public
    /// <summary>
    /// Instantiates a client to the given REST API service.
    /// </summary>
    /// <param name="ABaseApiURL">
    /// This is the base URL for the REST API. http://your.server.com/xyz/api/
    /// </param>
    constructor Create(const ABaseApiURL: string);
    destructor Destroy; override;
    property Password: string read FPassword write SetPassword;
    property ProxyPort: integer read FProxyPort write SetProxyPort;
    property ProxyServer: string read FProxyServer write SetProxyServer;
    property RawResponse: TIdHTTPResponse read GetResponse;
    /// <summary>
    /// Defaults to 'Developer Experts RestClient/{version}'
    /// </summary>
    property UserAgent: string read FUserAgent write SetUserAgent;
    property Username: string read FUserName write SetUserName;
  end;

implementation

uses IdURI, IdGlobal, DX.Utils.Json;

constructor TRESTClient.Create(const ABaseApiURL: string);
begin
  FHttpClient := TIdHttp.Create(nil);

  FProxyServer := '';
  FProxyPort := 0;
  FUserAgent := 'Developer Experts RestClient/' + API_VERSION;

  FHttpClient.AllowCookies := true;
  FBaseURL := trim(ABaseApiURL);
  if FBaseURL[Length(FBaseURL)] <> '/' then
    FBaseURL := FBaseURL + '/';
end;

destructor TRESTClient.Destroy;
begin
  FreeAndNil(FHttpClient);
  inherited;
end;

procedure TRESTClient.DoDeleteRequest(const AQuery: string);
begin
  Request(rmDELETE, AQuery);
end;

function TRESTClient.DoGetRequest(const AQuery: string): string;
begin
  result := Request(rmGET, AQuery, '', ctNone);
end;

function TRESTClient.DoPostRequest(const AQuery: string; const ABody: string = '';
  AContentType: TContentType = ctJson): string;
begin
  result := Request(rmPOST, AQuery, ABody, AContentType);
end;

function TRESTClient.DoPutRequest(const AQuery: string; const ABody: string = '';
  AContentType: TContentType = ctJson): string;
begin
  result := Request(rmPUT, AQuery, ABody, AContentType);
end;

function TRESTClient.GetEntity<T>(const AQuery: string): T;
var
  LResponse: string;
begin
  LResponse := DoGetRequest(AQuery);

  result := T.Create;
end;

function TRESTClient.GetEntityList<T>(const AQuery: string): TObjectList<T>;
var
  LResponse: string;
  LJsonResponse: TJSONObject;
  LResponseArray: TJSONArray;
  i: integer;
  LItem: T;
  LJsonObject: TJSONObject;
begin
  LResponse := DoGetRequest(AQuery);
  // Parse response as Json and try interpreting it as Array
  LResponseArray := TJSONObject.ParseJSONValue(LResponse) as TJSONArray;
  if assigned(LResponseArray) then
  begin
    result := TObjectList<T>.Create;
    // The array's items are supposed to be representations of class <T>
    for i := 0 to LResponseArray.Size - 1 do
    begin
      LJsonObject := LResponseArray.Get(i) as TJSONObject;
      LItem := TDXJson.JsonToObject<T>(LJsonObject.ToString);
      result.Add(LItem);
    end;
  end
  else
  begin
    raise ERESTClientException.Create(500, 'Response did not return an Array of ' + T.Classname);
  end;

end;

function TRESTClient.InsertEntity<T>(const AQuery: string; AEntity: T): TJSONObject;
var
  LResponse: string;
  LBody: string;
  LEntityJson: string;
begin
  LEntityJson := TDXJson.ObjectToJson(AEntity);
  LBody := LEntityJson;
  LResponse := DoPostRequest(AQuery, LBody);
  // Try to interpret as JSON
  result := TJSONObject.ParseJSONValue(LResponse) as TJSONObject;
end;

function TRESTClient.GetResponse: TIdHTTPResponse;
begin
  result := FHttpClient.Response;
end;

function TRESTClient.Request(AMethod: TRESTRequestMethod; const AQuery: string; const ABody: string = '';
  AContentType: TContentType = ctJson): string;
var
  LRequestBody: TStringStream;
  LRequestURL: string;
  LQuery: string;
  LEncodedBody: string;
  LJsonMessage: TJSONObject;
  LMessage: string;
begin
  FHttpClient.ProxyParams.ProxyServer := FProxyServer;
  FHttpClient.ProxyParams.ProxyPort := FProxyPort;

  FHttpClient.Request.BasicAuthentication := true;
  FHttpClient.Request.Username := Username;
  FHttpClient.Request.Password := Password;
  FHttpClient.Request.BasicAuthentication := FHttpClient.Request.Username > '';
  FHttpClient.Request.UserAgent := UserAgent;

  try
    LQuery := trim(AQuery);
    if (Length(LQuery) > 0) and (LQuery[1] = '/') then // remove leading '/'
      Delete(LQuery, 1, 1);
    LRequestURL := FBaseURL + LQuery;
    LRequestURL := TIdURI.URLEncode(LRequestURL);

    if AContentType = ctURLEncoded then
    begin
      FHttpClient.Request.ContentType := 'application/x-www-form-urlencoded';
      LEncodedBody := TIdURI.ParamsEncode(ABody);
    end
    else if AContentType = ctJson then
    begin
      FHttpClient.Request.ContentType := 'application/json';
      LEncodedBody := ABody;
      // just pass through. Technically we could check if ABody really contains json and if so reformat it to be "compact"
    end
    else if AContentType = ctNone then
    begin
      FHttpClient.Request.ContentType := '';
      LEncodedBody := ABody; // pass through what ever abody is.
    end
    else
      raise ERESTClientException.Create(500, 'Unsupported ContentType!');

    LRequestBody := TStringStream.Create(LEncodedBody);

    FHttpClient.Request.Accept := 'application/json';
    FHttpClient.Request.AcceptCharSet := 'UTF-8 ';

    try
      case AMethod of
        rmPOST:
          begin
            result := FHttpClient.Post(LRequestURL, LRequestBody);
          end;
        rmPUT:
          begin
            result := FHttpClient.Put(LRequestURL, LRequestBody);
          end;
        rmGET:
          begin
            result := FHttpClient.Get(LRequestURL);
          end;
        rmDELETE:
          begin
            FHttpClient.Delete(LRequestURL);
            result := '';
          end;

      end;
    except

      on e: EIdHTTPProtocolException do
      begin

        LJsonMessage := TJSONObject.ParseJSONValue(e.ErrorMessage) as TJSONObject;
        // if error is json, then decode it
        if assigned(LJsonMessage) then
          LMessage := LJsonMessage.ToString
        else
          LMessage := e.ErrorMessage;
        raise ERESTClientException.Create(e.ErrorCode, LMessage);
      end;
      on e: Exception do
      begin
        raise ERESTClientException.Create(500, e.Message);
      end;
    end;

  finally
    FreeAndNil(LRequestBody);
  end;
end;

procedure TRESTClient.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

procedure TRESTClient.SetProxyPort(const Value: integer);
begin
  FProxyPort := Value;
end;

procedure TRESTClient.SetProxyServer(const Value: string);
begin
  FProxyServer := Value;
end;

procedure TRESTClient.SetUserAgent(const Value: string);
begin
  FUserAgent := Value;
end;

procedure TRESTClient.SetUserName(const Value: string);
begin
  FUserName := Value;
end;

constructor ERESTClientException.Create(AErrorCode: integer; const AMsg: string);
begin
  inherited CreateFmt('%d: %s', [AErrorCode, AMsg]);
  FErrorCode := AErrorCode;
end;

procedure ERESTClientException.SetErrorCode(const Value: integer);
begin
  FErrorCode := Value;
end;

end.
