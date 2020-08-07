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
  Sparkle.Http.Headers;

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
  LxPath: string;
  i: Integer;
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

  //Todo: PVP spezifischen Code konfigurierbar machen. Nur generische Proxy Regeln allgemein gültig verarbeiten
  LHeaders.GetIfExists('X-PVP-ORIG-SCHEME', LScheme);
  LHeaders.GetIfExists('X-PVP-ORIG-HOST', LHost);

  LxPath := '';
  LHeaders.GetIfExists('X-PVP-ORIG-URI', LxPath);
  if LxPath > '' then
  begin
    // If there is no prefix/proxy-LPath such as /ProxyPath/OriginalPath, then we just take the original LPath
    if LxPath.ToLower.StartsWith(LPath.ToLower) or LxPath.IsEmpty then
    begin
      LxPath := LPath;
    end
    else
    begin
      // otherwise lets isolate the prefix
      i := LxPath.ToLower.IndexOf(LPath.ToLower);
      LxPath := LxPath.Remove(i);
      LxPath := LxPath + LPath;
    end;

    if not LxPath.StartsWith('/') then
    begin
      LxPath := '/' + LxPath;
    end;
    if not LxPath.EndsWith('/') then
    begin
      LxPath := LxPath + '/';
    end;

  end;

  if not LScheme.EndsWith('://') then
  begin
    LScheme := LScheme + '://';
  end;


  if LHost > '' then
  begin
    Context.Request.RawUri := LScheme + LHost + LPath + LQuery;
  end;

  Next(Context);
end;

end.
