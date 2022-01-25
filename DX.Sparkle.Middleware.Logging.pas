unit DX.Sparkle.Middleware.Logging;

interface

uses
  System.Classes, System.SysUtils,
  Bcl.Logging,
  Sparkle.Middleware.Logging, Sparkle.HttpServer.Context,
  Sparkle.HttpServer.Module;

type
  TLoggingMiddleware = class(Sparkle.Middleware.Logging.TLoggingMiddleware)
  protected
    procedure ProcessRequest(Context: THttpServerContext; Next: THttpServerProc); override;
  public
    constructor Create(ALogger: ILogger);
  end;

  TTrackedRequest = record
    Context: THttpServerContext;
    TransactionID: TGuid;
  end;

threadvar GCurrentRequest: TTrackedRequest;

implementation

constructor TLoggingMiddleware.Create(ALogger: ILogger);
begin
  inherited;
  FormatString := ':remoteaddr :method :url :statuscode - :responsetime ms';
end;

procedure TLoggingMiddleware.ProcessRequest(Context: THttpServerContext; Next: THttpServerProc);
begin
  GCurrentRequest.Context := Context;
  GCurrentRequest.TransactionID := TGuid.NewGuid;
  inherited;
end;

end.
