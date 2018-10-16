unit DX.Utils.Logger.VCL;

interface

implementation

uses
  VCL.Forms, DX.Utils.Logger;

initialization

Application.OnException := TDXLogger.Instance.ExceptionHandler;
TDXLogger.Instance.ShowExceptionProc := Application.ShowException;

end.
