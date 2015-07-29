{$REGION 'Documentation'}
/// <summary>
/// DX.Utils.Logger provides an easy to use logging mechanism
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
    class var FTerminating: Boolean;

  private
    FExternalStringsOnTop: Boolean;
    FExternalStrings: TStrings;
    FLogBuffer: TStrings;
    FThread: TThread;
  protected
    constructor Create;
    destructor Destroy; reintroduce;
    function ExternalStringsAssigned: Boolean;
  public
    /// <summary>
    /// External Strings can be used to log to a TMemo. Updates are done via Synchronize/Main Thread
    /// </summary>
    class procedure SetExternalStrings(AStrings: TStrings; AInsertOnTop: Boolean = false);
    class procedure Log(AMessage: string);
    class function Instance: TDXLogger;
  end;

  /// <summary>
  /// Shortcut to log a message
  /// </summary>
procedure Log(const AMessage: string); overload;
/// <summary>
/// Shortcut to log a message with format string
/// </summary>
procedure Log(const AFormatString: string; const AValues: Array of const); overload;
/// <summary>
/// Shortcut to to avoid name clashes with other logging systems
/// </summary>
procedure DXLog(const AMessage: string); overload;
/// Shortcut to to avoid name clashes with other logging systems
procedure DXLog(const AFormatString: string; const AValues: Array of const); overload;

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
    destructor Destroy; Override;
    procedure Execute; Override;
    procedure SyncronizeExternalStrings;
  end;

procedure Log(const AMessage: string);
begin
  TDXLogger.Log(AMessage);
end;

procedure Log(const AFormatString: string; const AValues: Array of const);
begin
  TDXLogger.Log(Format(AFormatString, AValues));
end;

procedure DXLog(const AMessage: string); overload;
begin
  Log(AMessage);
end;

procedure DXLog(const AFormatString: string; const AValues: Array of const); overload;
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
      Instance.FLogBuffer.Add(LMessage);
    finally
      TMonitor.Exit(Instance);
    end;
  end;
end;

class procedure TDXLogger.SetExternalStrings(AStrings: TStrings; AInsertOnTop: Boolean = false);
begin
  TMonitor.Enter(Instance);
  try
    Instance.FExternalStrings := AStrings;
    Instance.FExternalStringsOnTop := AInsertOnTop;
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
  Terminate;
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
    TMonitor.Enter(FExternalBuffer); // FExternalBuffer is cleared in the contex of the main thread (via Synchronize)
    try
      FExternalBuffer.AddStrings(FTempBuffer);
    finally
      TMonitor.Exit(FExternalBuffer);
    end;
    Synchronize(UpdateExternalStrings);
  end;
end;

procedure TLogThread.UpdateLogFile;
{$IF (defined(MSWindows) or defined(MacOS)) and not (defined(IOS)))}
var
  F: TextFile;
  s: String;
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
  if (TDXLogger.Instance <> nil) and Assigned(TDXLogger.Instance.FExternalStrings) and Assigned(FExternalBuffer) and not TDXLogger.FTerminating
  then
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
