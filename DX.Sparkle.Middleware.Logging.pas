unit DX.Sparkle.Middleware.Logging;

interface

uses
  System.Classes, System.SysUtils, System.Rtti,
  Bcl.Logging,
  Sparkle.Middleware.Logging, Sparkle.HttpServer.Context, Sparkle.HttpServer.Module,
  DX.Utils.Logger.Intf;

type

  /// <summary>
  /// Logger-Klasse, in der der tatsächlich zu verwendende Logger
  /// konfiguriert werden kann. Muss ILogger implementieren, da dies im XData
  /// Framework erforderlich ist.
  /// </summary>
  TXDataLogger = class(TInterfacedObject, ILogger)
  public
    procedure Debug(const AValue: TValue); inline;
    procedure Error(const AValue: TValue);
    procedure Info(const AValue: TValue); inline;
    procedure Trace(const AValue: TValue); inline;
    procedure Warning(const AValue: TValue); inline;
  end;

  TLoggingMiddleware = class(Sparkle.Middleware.Logging.TLoggingMiddleware)
  protected
    procedure ProcessRequest(Context: THttpServerContext; Next: THttpServerProc); override;
  public
    constructor Create;
  end;

  TTrackedRequest = record
    Context: THttpServerContext;
    TransactionID: TGuid;
  end;

threadvar GCurrentRequest: TTrackedRequest;

implementation

constructor TLoggingMiddleware.Create;
begin
  inherited Create(TXDataLogger.Create);
  FormatString := ':remoteaddr :method :url :statuscode - :responsetime ms';
end;

procedure TLoggingMiddleware.ProcessRequest(Context: THttpServerContext; Next: THttpServerProc);
begin
  GCurrentRequest.Context := Context;
  GCurrentRequest.TransactionID := TGuid.NewGuid;
  inherited;
end;

procedure TXDataLogger.Debug(const AValue: TValue);
begin
  DXLog(AValue.AsString);
end;

procedure TXDataLogger.Error(const AValue: TValue);
begin
  DXLog(AValue.AsString);
end;

procedure TXDataLogger.Info(const AValue: TValue);
begin
  DXLog(AValue.AsString);
end;

procedure TXDataLogger.Trace(const AValue: TValue);
begin
  DXLog(AValue.AsString);
end;

procedure TXDataLogger.Warning(const AValue: TValue);
begin
  DXLog(AValue.AsString);
end;

end.
