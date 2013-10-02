{$REGION 'Documentation'}
/// <summary>
/// D.Utils.Logger provides an easy to use logging mechanism
/// </summary>
/// <remarks>
/// <para>
/// DX.Utils.Logger is part of DX.Library
/// </para>
/// <para>
/// See: <see href="http://code.google.com/p/dx-library/" />
/// </para>
/// </remarks>
/// <example>
/// <para>
/// uses
/// </para>
/// <para>
/// DX.Utils.Logger;
/// </para>
/// <para>
/// Log('Some log message')
/// </para>
/// </example>
{$ENDREGION}
unit DX.Utils.Logger;

interface

uses
  Sysutils,
  Classes;

type
{$REGION 'Documentation'}
  /// <summary>
  /// TFXLogger provides a thread-safe logging mechanism.
  /// </summary>
{$ENDREGION}
  TDXLogger = class(TObject)
  private
    class var FInstance: TDXLogger;

  class var
    FTerminating: Boolean;

    FExternalStrings: TStrings;
    FLogBuffer: TStrings;
    FThread: TThread;
  protected
    constructor Create;
    destructor Destroy; override;
    function ExternalStringsAssigned: Boolean;
  public
    // External Strings can be used to log to a TMemo. Updates are done via Synchronize/Main Thread
    class procedure SetExternalStrings(AStrings: TStrings);
    class procedure Log(AMessage: string);
    class function Instance: TDXLogger;
  end;

procedure Log(AMessage: string);

implementation

{$IFDEF MSWINDOWS}
uses
  Windows,
  Messages;
{$ENDIF}

{$IF defined(IOS) or Defined(MACOS)}
uses
  DX.Apple.Utils;
{$ENDIF}

{$IFDEF Android}
uses
  AndroidAPI.Log;
{$ENDIF}

type
  TLogThread = class(TThread)
  private
    procedure UpdateConsole;
    procedure UpdateLogFile;
    procedure UpdateExtrnalStrings;
  public
    FTempBuffer: TStrings; // Used to decouple the main buffer
    FExternalBuffer: TStrings; // Used as a separate buffer for writing to the external strings
    constructor Create;
    destructor Destroy; Override;
    procedure Execute; Override;
    procedure UpdateExternalStrings;
  end;

procedure Log(AMessage: string);
begin
  TDXLogger.Log(AMessage);
end;

{ TDXLogger }
constructor TDXLogger.Create;
begin
  inherited;
  FLogBuffer := TStringList.Create;
  FThread := TLogThread.Create;
  FExternalStrings := nil;
  FThread.Start;
end;

destructor TDXLogger.Destroy;
begin
  FreeAndNil(FThread);
  FreeAndNil(FLogBuffer);
  inherited;
end;

function TDXLogger.ExternalStringsAssigned: Boolean;
begin
  TMonitor.Enter(Instance);
  try
    result := Assigned(FExternalStrings);
  finally
    TMonitor.Exit(Instance);
  end;
end;

class function TDXLogger.Instance: TDXLogger;
begin
  if FTerminating then
    raise EAbort.Create('Terminating');
  result := FInstance;
end;

class procedure TDXLogger.Log(AMessage: string);
var
  LMessage: string;
begin
  LMessage := FormatDateTime('YYYY-MM-DD hh:nn:ss,zzz', now) + ' : ' + AMessage;
  if Assigned(Instance.FLogBuffer) then
  begin
    TMonitor.Enter(Instance);
    try
      FLogBuffer.Add(LMessage);
    finally
      TMonitor.Exit(Instance);
    end;
  end;
end;

class procedure TDXLogger.SetExternalStrings(AStrings: TStrings);
begin
  TMonitor.Enter(Instance);
  try
    FExternalStrings := AStrings;
  finally
    TMonitor.Exit(Instance);
  end;

end;

{ TLogThread }

constructor TLogThread.Create;
begin
  inherited Create(true);
  FTempBuffer := TStringList.Create;
  FExternalBuffer := TStringList.Create;
end;

destructor TLogThread.Destroy;
begin
  FreeAndNil(FExternalBuffer);
  FreeAndNil(FTempBuffer);
  inherited;
end;

procedure TLogThread.Execute;

begin
  while not terminated do
  begin
    sleep(100);
    TMonitor.Enter(TDXLogger.Instance);
    try
      if (TDXLogger.FLogBuffer.Count > 0) then
      begin
        FTempBuffer.AddStrings(TDXLogger.FLogBuffer);
        TDXLogger.FLogBuffer.Clear;
      end;
    finally
      TMonitor.Exit(TDXLogger.Instance);
    end;
    if FTempBuffer.Count > 0 then
    begin
      // From here FTempBuffer will only be used in this LogThread
      // Log to console (iOS: NSLog, Windows: OutputDebugString)
      UpdateConsole;
      // Write Log file (Windows, MacOS: Appname.log)
      UpdateLogFile;
      // Write to External Strings (if, assigned, e.g. Memo, synchronized to Main Thread)
      UpdateExternalStrings;
      // Done with TempBuffer
      FTempBuffer.Clear;
    end;
  end;
end;

procedure TLogThread.UpdateExtrnalStrings;
begin
  if TDXLogger.Instance.ExternalStringsAssigned then
  begin
    TMonitor.Enter(FExternalBuffer); // FEXternalBuffer is cleared in the contex of the main thread (via Synchronize)
    try
      FExternalBuffer.AddStrings(FTempBuffer);
    finally
      TMonitor.Exit(FExternalBuffer);
    end;
    Synchronize(UpdateExternalStrings);
  end;
end;

procedure TLogThread.UpdateLogFile;
var
  F: TextFile;
  s: String;
  LFileName: string;
begin
{$IF defined(MSWindows) or defined(MacOS)}
  try
    LFileName := ParamStr(0) + '.log';
    AssignFile(F, LFileName);
    try
      if FileExists(LFileName) then
        Append(F)
      else
        Rewrite(F);
      for s in FTempBuffer do
      begin
        Writeln(F, s);
      end;
      Flush(F);
    finally
      CloseFile(F);
    end;
  except
    // Nothing - if logging doesn't work, then there nothing we can do about.
  end;
{$ENDIF}
end;

procedure TLogThread.UpdateConsole;
var
  LMessage: string;
begin
  for LMessage in FTempBuffer do
  begin
{$IFDEF IOS}
    NSLog2(LMessage);
{$ENDIF}
{$IFDEF MSWINDOWS}
    OutputDebugString(pchar(LMessage));
{$ENDIF}
{$IFDEF ANDROID}
    LOGI(MarshaledAString(LMessage));
{$ENDIF}
  end;

end;

procedure TLogThread.UpdateExternalStrings;
begin
  if (TDXLogger.Instance <> nil) and Assigned(TDXLogger.FExternalStrings) and Assigned(FExternalBuffer) and
    not TDXLogger.FTerminating then
    try
      TMonitor.Enter(FExternalBuffer);
      try
        TDXLogger.FExternalStrings.AddStrings(FExternalBuffer);
        // We might offer a method to hand over a scrolling control handle
        // SendMessage(FMemo.Handle, EM_LINESCROLL, 0, GLogger.FMemo.Lines.Count);
        FExternalBuffer.Clear;
      finally
        TMonitor.Exit(FExternalBuffer);
      end;
    except
      // If logging fails, then there is nocthng we can do
    end;
end;

initialization

TDXLogger.FTerminating := false;
TDXLogger.FInstance := TDXLogger.Create;

finalization

TDXLogger.FTerminating := true;
FreeAndNil(TDXLogger.FInstance);

end.
