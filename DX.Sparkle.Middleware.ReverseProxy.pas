unit DX.Sparkle.Middleware.ReverseProxy;

interface

uses
  System.Classes, System.SysUtils,
  Sparkle.HttpServer.Module, Sparkle.HttpServer.Context;

type

  /// <summary>
  /// TReverseProxyMiddleware implements specific header processing for
  /// applications behind a reverse proxy. If no reverse proxy is found,
  /// behaviour is not changed.
  /// </summary>
  TReverseProxyMiddleware = class(THttpServerMiddleware, IHttpServerMiddleware)

  protected
    procedure ProcessRequest(
      Context: THttpServerContext;
      Next:    THttpServerProc); override;
  end;

implementation

uses
  Sparkle.Http.Headers, ELKE.Server.Logger, ELKE.Classes.Logging;

{ TReverseProxyMiddleware }

procedure TReverseProxyMiddleware.ProcessRequest(
  Context: THttpServerContext;
  Next:    THttpServerProc);
var
  LHost: string;
  LHostAlternative: string;
  LFor: string;
  LScheme: string;
  LQuery: string;
  LAddParam: String;
  LPath: string;
  LHeaders: THttpHeaders;
  LOrigPath: string;
  i: Integer;
  LRequestLog: TLogItemRequest;
begin
  // Find relevant proxy headers

  // nginx
  // X-Real-IP         $remote_addr;
  // X-Forwarded-For   $proxy_add_x_forwarded_for;
  // X-Forwarded-Proto $scheme;
  // X-Forwarded-Host  $host;
  // X-Forwarded-Port  $server_port;

  // apache
  // X-Forwarded-For : The IP address of the client.
  // X-Forwarded-Host : The original host requested by the client in the Host HTTP request header.
  // X-Forwarded-Server : The hostname of the proxy server.

  // Alternative
  // X-Forward-Proto
  // X-Forward-For
  // X-Forward-Host

  ELKELog('Processing incomming request ...', TLogLevel.Trace);

  LRequestLog := TLogItemRequest.Create(Context.Request);
  TElkeLogger.Default.Log(LRequestLog);
  if not Context.Request.RawUri.ToLower.Contains('swagger') then
  begin
    Next(Context);
  end
  else
  begin
    ELKELog('Swagger/Proxy before: ' + Context.Request.RawUri, TLogLevel.Trace);

    LHost := Context.Request.Headers.Get('X-Forwarded-Host');
    LHostAlternative := Context.Request.Headers.Get('X-Forward-Host');

    LQuery := Context.Request.Uri.OriginalQuery;
    LScheme := Context.Request.Uri.Scheme;
    LPath := Context.Request.Uri.Path;
    LHeaders := Context.Request.Headers;

    if LHost > '' then
    begin
      // nginx/Apache
      LFor := Context.Request.Headers.Get('X-Forwarded-For');
      LScheme := Context.Request.Headers.Get('X-Forwarded-Proto');
    end
    else if LHostAlternative > '' then
    begin
      // Alternative
      LHost := LHostAlternative;
      LFor := Context.Request.Headers.Get('X-Forward-For');
      LScheme := Context.Request.Headers.Get('X-Forward-Proto');
    end;

    // Todo: PVP spezifischen Code konfigurierbar machen. Nur generische Proxy Regeln allgemein gültig verarbeiten
    LHeaders.GetIfExists('X-PVP-ORIG-SCHEME', LScheme);
    LHeaders.GetIfExists('X-PVP-ORIG-HOST', LHost);

    // PVP specific header, that holds the original path as issued by client
    LOrigPath := '';
    LHeaders.GetIfExists('X-PVP-ORIG-URI', LOrigPath);
    // make sure LorigPath is "clean"
    if LOrigPath > '' then
    begin
      LOrigPath := LOrigPath.Trim;
      if not LOrigPath.StartsWith('/') then
      begin
        LOrigPath := '/' + LOrigPath;
      end;
      ELKELog('X-PVP-ORIG-URI: ' + LOrigPath, TLogLevel.Trace);
      LPath := LOrigPath;
    end;

    if not LScheme.EndsWith('://') then
    begin
      LScheme := LScheme + '://';
    end;

    if LHost > '' then
    begin
      Context.Request.RawUri := LScheme + LHost + LPath + LQuery;

    end;

    ELKELog('Swagger/Proxy after: ' + Context.Request.RawUri, TLogLevel.Trace);
    Next(Context);
  end;
end;

end.
