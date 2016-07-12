{$REGION 'Documentation'}
/// <summary>
/// DX.Utils.Logger provides an easy to use logging mechanism. The logging is
/// completely multithreaded and will not change the timing of the application.
/// </summary>
/// <remarks>
/// <para>
/// DX.Utils.Logger is part of DX.Library
/// </para>
/// <para>
/// See:
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
  System.Classes, System.SysUtils;

type
{$REGION 'Documentation'}
  /// <summary>
  /// TDXLogger provides a thread-safe logging mechanism.
  /// </summary>
{$ENDREGION}
  TDXLogger = class(TObject)
  private
    class var FInstance: TDXLogger;
    class var FTerminating: Boolean;
  private
    FExternalStringsOnTop: Boolean;
    FExternalStrings: TStrings;
    FLogBuffer: TStrings;
    FThread: TThread;
    FDateFormat: string;
    class function Lock: TObject; static;
  protected
    constructor Create;
    destructor Destroy; reintroduce;
    class function GetDateFormat: string; static;
    class procedure SetExternalStrings(AStrings: TStrings); static;
    class procedure SetExternalStringsAppendOnTop(const Value: Boolean); static;
    class procedure SetDateFormat(ADateFormat: string); static;
    function ExternalStringsAssigned: Boolean;
  public
    /// <summary>
    /// External Strings can be used to log to a TMemo. Updates are done via Synchronize/Main Thread
    /// </summary>
    class property ExternalStrings: TStrings write SetExternalStrings;
    /// <summary>
    /// Insert new log messages on top of external strings.
    /// </summary>
    class property ExternalStringsAppendOnTop: Boolean write SetExternalStringsAppendOnTop;
    /// <summary>
    /// DateFormat is used to format the timestamp which prefixes every log message.
    /// Defaults to YYYY-MM-DD hh:nn:ss,zzz
    /// </summary>
    class property DateFormat: string read GetDateFormat write SetDateFormat;
    class procedure Log(const AMessage: string); overload;
    class procedure Log(const AFormatString: string; const AValues: array of const); overload;
    class function Instance: TDXLogger; static;
  end;

  /// <summary>
  /// Shortcut to log a message
  /// </summary>
procedure Log(const AMessage: string); overload;
/// <summary>
/// Shortcut to log a message with format string
/// </summary>
procedure Log(const AFormatString: string; const AValues: array of const); overload;
/// <summary>
/// Shortcut to to avoid name clashes with other logging systems
/// </summary>
procedure DXLog(const AMessage: string); overload;
/// Shortcut to to avoid name clashes with other logging systems
procedure DXLog(const AFormatString: string; const AValues: array of const); overload;

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
    procedure UpdateExternalStrings;
  public
    FTempBuffer: TStrings; // Used to decouple the main buffer
    FExternalBuffer: TStrings; // Used as a separate buffer for writing to the external strings
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure SyncronizeExternalStrings;
  end;

var FLock:TDXLogger;

procedure Log(const AMessage: string);
begin
  TDXLogger.Log(AMessage);
end;

procedure Log(const AFormatString: string; const AValues: array of const);
begin
  TDXLogger.Log(AFormatString, AValues);
end;

procedure DXLog(const AMessage: string); overload;
begin
  Log(AMessage);
end;

procedure DXLog(const AFormatString: string; const AValues: array of const); overload;
begin
  Log(AFormatString, AValues);
end;

{ TDXLogger }
constructor TDXLogger.Create;
begin
  inherited;
  FLogBuffer := TStringList.Create;
  FThread := TLogThread.Create;
  FExternalStrings := nil;
  FDateFormat := 'YYYY-MM-DD hh:nn:ss,zzz';
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
  TMonitor.Enter(Lock);
  try
    result := Assigned(FExternalStrings);
  finally
    TMonitor.Exit(Lock);
  end;
end;

class function TDXLogger.GetDateFormat: string;
begin
  TMonitor.Enter(Lock);
  try
    result := Instance.FDateFormat;
  finally
    TMonitor.Exit(Lock);
  end;
end;

class function TDXLogger.Instance: TDXLogger;
begin
  if FTerminating then
    raise EAbort.Create('Terminating');
  if FInstance = nil then
    TDXLogger.FInstance := TDXLogger.Create;
  result := FInstance;
end;

class function TDXLogger.Lock: TObject;
begin
    result := FLock;
end;

class procedure TDXLogger.Log(const AFormatString: string; const AValues: array of const);
begin
  Log(Format(AFormatString, AValues));
end;

class procedure TDXLogger.Log(const AMessage: string);
var
  LMessage: string;
begin
  LMessage := FormatDateTime(DateFormat, now) + ' : ' + AMessage;
  if Assigned(Instance.FLogBuffer) then
  begin
    TMonitor.Enter(Lock);
    try
      Instance.FLogBuffer.Add(LMessage);
    finally
      TMonitor.Exit(Lock);
    end;
  end;
end;

class procedure TDXLogger.SetDateFormat(ADateFormat: string);
begin
  TMonitor.Enter(Lock);
  try
    Instance.FDateFormat := ADateFormat;
  finally
    TMonitor.Exit(Lock);
  end;
end;

class procedure TDXLogger.SetExternalStrings(AStrings: TStrings);
begin
  TMonitor.Enter(Lock);
  try
    Instance.FExternalStrings := AStrings;
  finally
    TMonitor.Exit(Lock);
  end;
end;

class procedure TDXLogger.SetExternalStringsAppendOnTop(const Value: Boolean);
begin
  TMonitor.Enter(Lock);
  try
    Instance.FExternalStringsOnTop := Value;
  finally
    TMonitor.Exit(Lock);
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
  Terminate;
  FreeAndNil(FExternalBuffer);
  FreeAndNil(FTempBuffer);
  inherited;
end;

procedure TLogThread.Execute;

begin
  while not terminated do
  begin
    TMonitor.Wait(TDXLogger.Instance, 100);
    // sleep(100);
    TMonitor.Enter(TDXLogger.Instance);
    try
      if (TDXLogger.Instance.FLogBuffer.Count > 0) then
      begin
        FTempBuffer.AddStrings(TDXLogger.Instance.FLogBuffer);
        TDXLogger.Instance.FLogBuffer.Clear;
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
      SyncronizeExternalStrings;
      // Done with TempBuffer
      FTempBuffer.Clear;
    end;
  end;
end;

procedure TLogThread.SyncronizeExternalStrings;
begin
  if TDXLogger.Instance.ExternalStringsAssigned then
  begin
    TMonitor.Enter(FExternalBuffer);
    // FExternalBuffer is cleared in the contex of the main thread
    try
      FExternalBuffer.AddStrings(FTempBuffer);
    finally
      TMonitor.Exit(FExternalBuffer);
    end;
    // Updating external strings may take a while - queue is good enough here
    Queue(UpdateExternalStrings);
  end;
end;

procedure TLogThread.UpdateLogFile;
{$IF (defined(MSWindows) or defined(MacOS)) and not (defined(IOS)))}
var
  F: TextFile;
  s: string;
  LFileName: string;
{$ENDIF}
begin
{$IF (defined(MSWindows) or defined(MacOS)) and not (defined(IOS))) }
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
{$IFDEF Android}
  LMarshaller: TMarshaller;
{$ENDIF}
begin
  for LMessage in FTempBuffer do
  begin
{$IF defined(IOS) or Defined(MACOS)}
    NSLog2(LMessage);
{$ENDIF}
{$IFDEF MSWINDOWS}
    OutputDebugString(pchar(LMessage));
{$ENDIF}
{$IFDEF ANDROID}
    LOGI(LMarshaller.AsAnsi(LMessage).ToPointer);
{$ENDIF}
  end;

end;

procedure TLogThread.UpdateExternalStrings;
var
  s: string;
begin
  if (TDXLogger.Instance <> nil) and Assigned(TDXLogger.Instance.FExternalStrings) and Assigned(FExternalBuffer) and not TDXLogger.FTerminating then
  begin
    try
      TMonitor.Enter(FExternalBuffer);
      try
        if TDXLogger.Instance.FExternalStringsOnTop then
        begin
          for s in FExternalBuffer do
          begin
            TDXLogger.Instance.FExternalStrings.Insert(0, s);
          end;
        end
        else
          TDXLogger.Instance.FExternalStrings.AddStrings(FExternalBuffer);
        //
        FExternalBuffer.Clear;
      finally
        TMonitor.Exit(FExternalBuffer);
      end;
    except
      // If logging fails, then there is nocthng we can do
    end;
  end;
end;

initialization
TDXLogger.FTerminating := false;
FLock := TDXLogger.Create;

finalization

TDXLogger.FTerminating := true;
FreeAndNil(TDXLogger.FInstance);
FreeAndNil(FLock);
end.
