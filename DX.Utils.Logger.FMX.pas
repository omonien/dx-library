unit DX.Utils.Logger.FMX;

interface

implementation

uses
  FMX.Forms, DX.Utils.Logger;

initialization

Application.OnException := TDXLogger.Instance.ExceptionHandler;
TDXLogger.Instance.ShowExceptionProc := Application.ShowException;

end.
