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

{ TDeactivateCrudEndpointsMiddleware }

procedure TDeactivateCrudEndpointsMiddleware.ProcessRequest(
  Context: THttpServerContext;
  Next:    THttpServerProc);
var
  LAddParam: string;
  LQuery: string;
begin
  LQuery := Context.Request.Uri.OriginalQuery;

  if LQuery = '' then
  begin
    LAddParam := '?';
  end
  else
  begin
    LAddParam := '&';
  end;

  Context.Request.RawUri := Context.Request.RawUri + LAddParam + 'ExcludeEntities=true';
  Next(Context);
end;

end.
