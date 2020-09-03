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
  Sparkle.Http.Headers,
  XData.Server.Module,
  ELKE.Server.Logger, ELKE.Classes.Logging;

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
  LPath: string;
  LHeaders: THttpHeaders;
  LOrigPath: string;
  LRequestLog: TLogItemRequest;
  LProcessedUri: string;
  LBody: string;
  // LBasePath: string;
  // LRelativePath: string;
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
  ELKELog(LRequestLog);
  ELKELog('Proxy processing before: ' + Context.Request.Method + ' : ' + Context.Request.RawUri, TLogLevel.Trace);

  if Length(Context.Request.Content) > 0 then
  begin
    LBody := TEncoding.UTF8.GetString(Context.Request.Content);
    if LBody > '' then
    begin
      ELKELog('Body: ' + LBody, TLogLevel.Trace);
    end;
  end;

  (*
    LHeaders := Context.Request.Headers;
    LHost := Context.Request.Uri.Authority;
    LScheme := Context.Request.Uri.Scheme;
    LPath := Context.Request.Uri.Path;
    LQuery := Context.Request.Uri.OriginalQuery;

    Context.Request.Headers.GetIfExists('X-Forwarded-Host', LHost);
    LHostAlternative := Context.Request.Headers.Get('X-Forward-Host');

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


  *)

  LHeaders := Context.Request.Headers;
  LScheme := Context.Request.Uri.Scheme;
  LHost := Context.Request.Uri.Host;
  LHostAlternative := '';
  // LBasePath := TXDataOperationContext.Current.Handler.XModule.BaseUri.Path;
  LPath := Context.Request.Uri.Path;
  // LRelativePath := LPath.ToLower.Replace(LBasePath.ToLower, '');
  LQuery := Context.Request.Uri.OriginalQuery;

  Context.Request.Headers.GetIfExists('X-Forwarded-Host', LHost);
  Context.Request.Headers.GetIfExists('X-Forward-Host', LHostAlternative);
  // nginx/Apache
  Context.Request.Headers.GetIfExists('X-Forwarded-For', LFor);
  Context.Request.Headers.GetIfExists('X-Forwarded-Proto', LScheme);
  if LHostAlternative > '' then
  begin
    LHost := LHostAlternative;
    Context.Request.Headers.GetIfExists('X-Forward-For', LFor);
    Context.Request.Headers.GetIfExists('X-Forward-Proto', LScheme);
  end;

  // Todo: PVP spezifischen Code konfigurierbar machen. Nur generische Proxy Regeln allgemein gültig verarbeiten
  LHeaders.GetIfExists('X-PVP-ORIG-SCHEME', LScheme);
  LHeaders.GetIfExists('X-PVP-ORIG-HOST', LHost);

  // PVP specific header, that holds the original path as issued by client
  LOrigPath := '';
  LHeaders.GetIfExists('X-PVP-ORIG-URI', LOrigPath);

  /// LOrigPath := '/rest/ELKE/v1/openapi/swagger.json';

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

  // LPath := LPath.ToLower.Replace(LRelativePath, '');
  // ELKELog('Base path: ' + LPath, TLogLevel.Trace);
  // ELKELog('Relative path: ' + LRelativePath, TLogLevel.Trace);

  LProcessedUri := Format('%s://%s%s%s', [LScheme, LHost, LPath, LQuery]);

  ELKELog('Proxy processing after: ' + Context.Request.Method + ' : ' + LProcessedUri, TLogLevel.Trace);

  // Nur wenn Swagger involviert ist, dann biegen wir tatsächlich die URI um
  if Context.Request.RawUri.ToLower.Contains('swagger') then
  begin
    Context.Request.RawUri := LProcessedUri;
  end;

  Next(Context);
end;

end.
