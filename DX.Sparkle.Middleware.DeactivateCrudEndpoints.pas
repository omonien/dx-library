unit DX.Sparkle.Middleware.DeactivateCrudEndpoints;

interface

uses
  System.Classes, System.SysUtils,
  Sparkle.HttpServer.Module, Sparkle.HttpServer.Context;

type

  /// <summary>
  /// TDeactivateCrudEndpointsMiddleware deactivates all automatically
  /// created CRUD endpoints as workaround until XData has an option for
  /// that.
  /// </summary>
  TDeactivateCrudEndpointsMiddleware = class(THttpServerMiddleware, IHttpServerMiddleware)
  protected
    procedure ProcessRequest(
      Context: THttpServerContext;
      Next:    THttpServerProc); override;
  end;

implementation

uses
  System.Net.UrlClient;

{ TDeactivateCrudEndpointsMiddleware }

procedure TDeactivateCrudEndpointsMiddleware.ProcessRequest(
  Context: THttpServerContext;
  Next:    THttpServerProc);
var
  LUri: TURI;
  LParam: TNameValuePair;
  i: Integer;
begin
  // Ensure "ExcludeEntities=true" with current swaggerui request
  if Context.Request.RawUri.ToLower.Contains('swaggerui') then
  begin
    LUri := TURI.Create(Context.Request.RawUri);
    // change existing
    for i := Low(LUri.Params) to High(LUri.Params) do
    begin
      if LUri.Params[i].Name.Trim.ToLower = 'excludeentities' then
      begin
        LParam.Value := 'true'
      end;
    end;
    // add if not exists
    if not Context.Request.RawUri.ToLower.Contains('excludeentities') then
    begin
      LUri.AddParameter('ExcludeEntities', 'true');
    end;
    Context.Request.RawUri := LUri.ToString;
  end;
  Next(Context);
end;

end.
