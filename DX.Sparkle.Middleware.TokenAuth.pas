unit DX.Sparkle.Middleware.TokenAuth;

interface

uses
  System.Classes, System.SysUtils,
  Sparkle.HttpServer.Context, Sparkle.HttpServer.Module, Sparkle.Security;

type
  IUserIdentity = Sparkle.Security.IUserIdentity;
  TUserIdentity = Sparkle.Security.TUserIdentity;

  TAuthenticateBasicProc = reference to procedure(const ATokenValue: string; var User: IUserIdentity);

  TTokenAuthMiddleware = class(THttpServerMiddleware)
  private
    FOnAuthenticate: TAuthenticateBasicProc;
    FTokenName: string;
  protected
    function RetrieveIdentity(const ATokenValue: string): IUserIdentity; virtual;
    procedure DoAuthenticate(const ATokenValue: string; var User: IUserIdentity); virtual;
    procedure ProcessRequest(AContext: THttpServerContext; Next: THttpServerProc); override;
  public
    constructor Create; overload;
    constructor Create(AAuthenticateProc: TAuthenticateBasicProc = nil); overload;
    constructor Create(ATokenName: string; AAuthenticateProc: TAuthenticateBasicProc = nil); overload;
    /// <summary>
    /// The default token name is "authentication" and is case-insensitive
    /// </summary>
    property TokenName: string read FTokenName write FTokenName;
    property OnAuthenticate: TAuthenticateBasicProc read FOnAuthenticate write FOnAuthenticate;
  end;

implementation

uses
  System.Net.URLClient, System.NetEncoding;

{ TTokenAuthMiddleware }

constructor TTokenAuthMiddleware.Create;
begin
  Create('', nil);
end;

constructor TTokenAuthMiddleware.Create(ATokenName: string; AAuthenticateProc: TAuthenticateBasicProc = nil);
begin
  var
  LTokenName := ATokenName.Trim;
  if LTokenName = '' then
  begin
    FTokenName := 'authorization'
  end
  else
  begin
    FTokenName := LTokenName;
  end;
  OnAuthenticate := AAuthenticateProc;
end;

constructor TTokenAuthMiddleware.Create(AAuthenticateProc: TAuthenticateBasicProc = nil);
begin
  Create('', AAuthenticateProc);
end;

procedure TTokenAuthMiddleware.DoAuthenticate(const ATokenValue: string; var User: IUserIdentity);
begin
  if Assigned(FOnAuthenticate) then
    FOnAuthenticate(ATokenValue, User);
end;

procedure TTokenAuthMiddleware.ProcessRequest(AContext: THttpServerContext; Next: THttpServerProc);

// Case-insensitive
  function GetParameterValue(AUri: System.Net.URLClient.TURI; AParamName: string): string;
  var
    i: Integer;
    LName: string;
  begin
    Result := '';
    LName := TNetEncoding.URL.EncodeQuery(AParamName).ToLower;
    for i := 0 to Length(AUri.Params) - 1 do
      if AUri.Params[i].Name.ToLower = LName then
      begin
        Result := AUri.Params[i].Value;
        break;
      end;
  end;

var
  LTokenValue: string;
begin
  if AContext.Request.User = nil then
  begin
    // First, check Header
    LTokenValue := AContext.Request.Headers.Get(TokenName);
    // Then check URL Parameter
    if LTokenValue = '' then
    begin
      Var
      LUri := System.Net.URLClient.TURI.Create(AContext.Request.Uri.OriginalUri);
      LTokenValue := GetParameterValue(LUri, TokenName);
    end;
    AContext.Request.User := RetrieveIdentity(LTokenValue);
  end;
  Next(AContext);
end;

function TTokenAuthMiddleware.RetrieveIdentity(const ATokenValue: string): IUserIdentity;
begin
  Result := nil;
  DoAuthenticate(ATokenValue, Result);
end;

end.
