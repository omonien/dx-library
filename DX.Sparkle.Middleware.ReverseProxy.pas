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

{ TReverseProxyMiddleware }

procedure TReverseProxyMiddleware.ProcessRequest(
  Context: THttpServerContext;
  Next:    THttpServerProc);
var
  LHost: string;
  LHostAlternative: string;
  LFor: string;
  LProto: string;
  LQuery: string;
  LAddParam: String;
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

  if LHost > '' then
  begin
    // nginx/Apache
    LFor := Context.Request.Headers.Get('X-Forwarded-For');
    LProto := Context.Request.Headers.Get('X-Forwarded-Proto');
  end
  else if LHostAlternative > '' then
  begin
    // Alternative
    LHost := LHostAlternative;
    LFor := Context.Request.Headers.Get('X-Forward-For');
    LProto := Context.Request.Headers.Get('X-Forward-Proto');
  end;
  if LHost > '' then
  begin
    Context.Request.RawUri := LProto + LHost;
    Context.Request.RawUri := Context.Request.RawUri + LQuery;
  end;

  if LQuery = '' then
  begin
    LAddParam := '?';
  end
  else
  begin
    LAddParam := '&';
  end;

  // LHeaders := TEncoding.ASCII.GetBytes(Context.Request.Headers.RawWideHeaders);
  // Context.Request.Headers.RawWideHeaders := TEncoding.Default.GetString(LHeaders);

  Next(Context);
end;

end.
