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
    property TokenName: string read FTokenName write FTokenName;
    property OnAuthenticate: TAuthenticateBasicProc read FOnAuthenticate write FOnAuthenticate;
  end;

implementation

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
Create ('', AAuthenticateProc);
end;

procedure TTokenAuthMiddleware.DoAuthenticate(const ATokenValue: string; var User: IUserIdentity);
begin
  if Assigned(FOnAuthenticate) then
    FOnAuthenticate(ATokenValue, User);
end;

procedure TTokenAuthMiddleware.ProcessRequest(AContext: THttpServerContext; Next: THttpServerProc);
var
  LTokenValue: string;
begin
  if AContext.Request.User = nil then
  begin
    LTokenValue := AContext.Request.Headers.Get(TokenName);
    if (LTokenValue <> '') then
    begin
      AContext.Request.User := RetrieveIdentity(LTokenValue);
    end;
  end;
  Next(AContext);
end;

function TTokenAuthMiddleware.RetrieveIdentity(const ATokenValue: string): IUserIdentity;
begin
  Result := nil;
  DoAuthenticate(ATokenValue, Result);
end;

end.
