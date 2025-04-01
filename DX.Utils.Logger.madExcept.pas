unit DX.Utils.Logger.madExcept;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils,
  DX.Utils.Logger.Intf,
  madExcept, madMapFile;

type
  TDXExceptionLogger = class(TObject)
  protected
    class procedure ExceptionHandler(const exceptIntf: IMEException; var handled: boolean);
  public
    class constructor Create;
    class procedure Register;
  end;

implementation

class procedure TDXExceptionLogger.ExceptionHandler(const exceptIntf: IMEException; var handled: boolean);
begin
  var LHandled := ifthen(handled, 'handled', 'unhandled');
  var LLogLevel := TLogLevel.Warn;
  if not handled then
    LLogLevel := TLogLevel.Error; // unhandled exceptions always as level error

  exceptIntf.AutoSave := false;
  exceptIntf.AutoSend := false;
  exceptIntf.AutoClipboard := false;
  exceptIntf.ShowPleaseWaitBox := false;
  exceptIntf.CreateScreenShot := false;
  exceptIntf.ShowDisAsm := false;
  exceptIntf.ShowCpuRegisters := false;
  exceptIntf.ShowStackDump := false;
  exceptIntf.HideUglyItems := true;

  //Log all exceptions in DEBUG mode
{$IFNDEF DEBUG}
  //Unhandled exceptions only in RELEASE mode
  if not handled then
{$ENDIF}
    DXLog('Exception (%s)'#13#10'%s', [LHandled, exceptIntf.BugReport], LLogLevel);
end;

class procedure TDXExceptionLogger.Register;
begin
  // nothing
end;

class constructor TDXExceptionLogger.Create;
begin
  var
  LMapFile := FindMapFile;
  if LMapFile.IsValid then
  begin
    madExcept.RegisterExceptionHandler(ExceptionHandler, TSyncType.stDontSync);
    madExcept.RegisterHiddenExceptionHandler(ExceptionHandler, TSyncType.stDontSync);
    DXLog('Exception Logger registered ...');
  end
  else
  begin
    DXLog('madExcept map file not configured!');
  end;
end;

initialization

  TDXExceptionLogger.Register;

end.

