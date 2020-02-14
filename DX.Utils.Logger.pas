/// <summary>
/// DX.Utils.Logger provides an easy to use logging mechanism. The logging is
/// completely multithreaded and will not change the timing of the
/// application.
/// </summary>
/// <remarks>
/// <para>
/// DX.Utils.Logger is part of DX.Library
/// </para>
/// <para>
/// See:
/// </para>
/// </remarks>
unit DX.Utils.Logger;

interface

uses
  System.Classes, System.SysUtils;

type
  TShowExceptionProc = procedure(E: Exception) of Object;

  /// <summary>
  /// TDXLogger provides a thread-safe logging mechanism.
  /// </summary>

  TDXLogger = class(TObject)
  private
    class var FInstance: TDXLogger;
    class var FTerminating: Boolean;
  private
    FExternalStringsOnTop: Boolean;
    FExternalStrings: TStrings;
    FLogBuffer: TStrings;
    FThread: TThread;
    // Todo: implement custom date format!
    FDateFormat: string;
    FShowExceptionProc: TShowExceptionProc;
    FMaxLogAge: integer;
  protected
    constructor Create;
    class function GetDateFormat: string; static;
    class procedure SetExternalStrings(AStrings: TStrings); static;
    class procedure SetExternalStringsAppendOnTop(const Value: Boolean); static;
    class procedure SetDateFormat(const ADateFormat: string); static;
    class function GetMaxLogAge: integer; static;
    class procedure SetMaxLogAge(const Value: integer); static;
    function ExternalStringsAssigned: Boolean;
    procedure SetShowExceptionProc(const Value: TShowExceptionProc);
  public
    class constructor Create;
    class destructor Destroy;
    destructor Destroy; override;

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

    /// <summary>
    /// MaxLogAge specifies in days how much logging history will be kept.
    /// Older log messages will be rolled over into a sub dir "logs". <br />
    /// If MaxLogAge is zero (default), then there is no limit.
    /// </summary>
    class property MaxLogAge: integer read GetMaxLogAge write SetMaxLogAge;
    class procedure Log(const AMessage: string); overload;
    class procedure Log(
      const AFormatString: string;
      const AValues:       array of const); overload;
    class function Instance: TDXLogger; static;

    procedure ExceptionHandler(
      ASender: TObject;
      E:       Exception);
    property ShowExceptionProc: TShowExceptionProc read FShowExceptionProc write SetShowExceptionProc;
  end;

  /// <summary>
  /// Shortcut to log a message
  /// </summary>
procedure Log(const AMessage: string); overload;
/// <summary>
/// Shortcut to log a message with format string
/// </summary>
procedure Log(
  const AFormatString: string;
  const AValues:       array of const); overload;
/// <summary>
/// Shortcut to to avoid name clashes with other logging systems
/// </summary>
procedure DXLog(const AMessage: string); overload;
/// Shortcut to to avoid name clashes with other logging systems
procedure DXLog(
  const AFormatString: string;
  const AValues:       array of const); overload;

implementation

{$IFDEF MSWINDOWS}

uses
  WinAPI.Windows,
  WinAPI.Messages,
  System.IOUtils, System.DateUtils;
{$ENDIF}
{$IF defined(IOS) or Defined(MACOS)}

uses
  DX.Apple.Utils,
  System.IOUtils;
{$ENDIF}
{$IFDEF Android}

uses
  AndroidAPI.Log,
  System.IOUtils;
{$ENDIF}

type
  TLogThread = class(TThread)
  private
    // Used to decouple the main buffer
    FTempBuffer: TStrings;
    // Used as a separate buffer for writing to the external strings
    FExternalBuffer: TStrings;
    // Todo make configurable
    FLogFileName: string;
    FLastRollOver: TDate;

    procedure UpdateConsole;
    procedure UpdateLogFile;
    procedure UpdateExternalStrings;
    procedure RollOver;
    function GetLogFileDate: TDate;
  protected
    procedure Execute; override;
    procedure SyncronizeExternalStrings;
  public
    constructor Create;
    destructor Destroy; override;
  end;

procedure Log(const AMessage: string);
begin
  TDXLogger.Log(AMessage);
end;

procedure Log(
  const AFormatString: string;
  const AValues:       array of const);
begin
  TDXLogger.Log(AFormatString, AValues);
end;

procedure DXLog(const AMessage: string); overload;
begin
  Log(AMessage);
end;

procedure DXLog(
  const AFormatString: string;
  const AValues:       array of const); overload;
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
  FMaxLogAge := 0;
  FThread.Start;
end;

destructor TDXLogger.Destroy;
begin
  FreeAndNil(FThread);
  FreeAndNil(FLogBuffer);
  inherited;
end;

class destructor TDXLogger.Destroy;
begin
  FTerminating := true;
  FreeAndNil(FInstance);
  inherited;
end;

function TDXLogger.ExternalStringsAssigned: Boolean;
begin
  TMonitor.Enter(FInstance);
  try
    result := Assigned(FExternalStrings);
  finally
    TMonitor.Exit(FInstance);
  end;
end;

class function TDXLogger.GetDateFormat: string;
begin
  TMonitor.Enter(FInstance);
  try
    result := Instance.FDateFormat;
  finally
    TMonitor.Exit(FInstance);
  end;
end;

class function TDXLogger.Instance: TDXLogger;
begin
  if FTerminating then
    raise EAbort.Create('Logger terminating');
  if FInstance = nil then
    raise EAbort.Create('Logger not initialized');
  result := FInstance;
end;

class procedure TDXLogger.Log(
  const AFormatString: string;
  const AValues:       array of const);
begin
  Log(Format(AFormatString, AValues));
end;

procedure TDXLogger.ExceptionHandler(
  ASender: TObject;
  E:       Exception);
var
  LSender: string;
begin
  if Assigned(ASender) then
  begin
    LSender := ASender.ClassName;
  end
  else
  begin
    LSender := 'nil';
  end;
  if E is EAssertionFailed then
    Log('Assertion failed: %s (Sender: %s)', [E.Message, LSender])
  else
    Log('Exception:  %s (Sender: %s)', [E.Message, LSender]);
  if Assigned(FShowExceptionProc) then
  begin
    FShowExceptionProc(E);
  end;
end;

class procedure TDXLogger.Log(const AMessage: string);
var
  LMessage: string;
begin
  LMessage := FormatDateTime(DateFormat, now) + ' : ' + AMessage;
  if Assigned(Instance.FLogBuffer) then
  begin
    TMonitor.Enter(FInstance);
    try
      Instance.FLogBuffer.Add(LMessage);
    finally
      TMonitor.Exit(FInstance);
    end;
  end;
end;

class procedure TDXLogger.SetDateFormat(const ADateFormat: string);
begin
  TMonitor.Enter(FInstance);
  try
    Instance.FDateFormat := ADateFormat;
  finally
    TMonitor.Exit(FInstance);
  end;
end;

class procedure TDXLogger.SetExternalStrings(AStrings: TStrings);
begin
  TMonitor.Enter(FInstance);
  try
    Instance.FExternalStrings := AStrings;
  finally
    TMonitor.Exit(FInstance);
  end;
end;

class procedure TDXLogger.SetExternalStringsAppendOnTop(const Value: Boolean);
begin
  TMonitor.Enter(FInstance);
  try
    Instance.FExternalStringsOnTop := Value;
  finally
    TMonitor.Exit(FInstance);
  end;

end;

procedure TDXLogger.SetShowExceptionProc(const Value: TShowExceptionProc);
begin
  FShowExceptionProc := Value;
end;

class constructor TDXLogger.Create;
begin
  inherited;
  FInstance := TDXLogger.Create;
end;

class function TDXLogger.GetMaxLogAge: integer;
begin
  TMonitor.Enter(FInstance);
  try
    result := Instance.FMaxLogAge;
  finally
    TMonitor.Exit(FInstance);
  end;
end;

class procedure TDXLogger.SetMaxLogAge(const Value: integer);
begin
  TMonitor.Enter(FInstance);
  try
    Instance.FMaxLogAge := Value;
  finally
    TMonitor.Exit(FInstance);
  end;
end;

{ TLogThread }

constructor TLogThread.Create;
var
  s: string;
begin
  inherited Create(true);

  // Logfile goes into the app exe directory with name {Application name}.log
  s := TPath.GetLibraryPath;
  FLogFileName := TPath.Combine(s, TPath.ChangeExtension(TPath.GetFileName(ParamStr(0)), '.log'));

  FTempBuffer := TStringList.Create;
  FTempBuffer.TrailingLineBreak := false;
  FExternalBuffer := TStringList.Create;
  FExternalBuffer.TrailingLineBreak := false;
  FLastRollOver := 0;
end;

destructor TLogThread.Destroy;
begin
  Terminate;
  FreeAndNil(FExternalBuffer);
  FreeAndNil(FTempBuffer);
  inherited;
end;

procedure TLogThread.Execute;
var
  LBufferEmpty: Boolean;
begin
  LBufferEmpty := true;

  while not terminated do
  begin
    // If nothing is in the buffer, then sleep a bit, to avoid hammering
    if LBufferEmpty then
      sleep(100);
    // We don't want to check more frequently than 100 ms, as this could potentially
    // generate way to frequent writes to the log file. If one log message comes in, then
    // an other one might follow within a few ms. We write them in "100ms" blocks, not
    // message by message.

    TMonitor.Enter(TDXLogger.Instance);
    try
      // Copy everything from LogBuffer as fast as possible - to block as little as possible.
      if (TDXLogger.Instance.FLogBuffer.Count > 0) then
      begin
        FTempBuffer.AddStrings(TDXLogger.Instance.FLogBuffer);
        TDXLogger.Instance.FLogBuffer.Clear;
      end;
    finally
      TMonitor.Exit(TDXLogger.Instance);
    end;

    if FTempBuffer.Count = 0 then
    begin
      LBufferEmpty := true;
    end
    else
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

function TLogThread.GetLogFileDate: TDate;
var
  s: string;
  LBuffer: TBytes;
  LFileStream: TFileStream;
  LCount: integer;
  LComponents: TArray<string>;
  LYear: integer;
  LMonth: integer;
  LDay: integer;
  LFileDate: TDate;
begin
  LFileStream := TFile.OpenRead(FLogFileName);
  try
    // Todo: Make DateFormat customizable!
    // At the begining there should a date: 2020-02-14
    // Extract it and use it as the logfile's date/age
    SetLength(LBuffer, 10);
    LCount := LFileStream.Read(LBuffer, 10);
    result := 0;
    if LCount = 10 then
    begin
      s := TEncoding.UTF8.GetString(LBuffer);
      LComponents := s.Split(['-']);
      if Length(LComponents) = 3 then
      begin
        LYear := StrToIntDef(LComponents[0], 0);
        LMonth := StrToIntDef(LComponents[1], 0);
        LDay := StrToIntDef(LComponents[2], 0);
        if (LYear * LMonth * LDay > 0) then
        begin
          result := EncodeDate(LYear, LMonth, LDay);
        end;
      end;
    end;
  finally
    FreeAndNil(LFileStream);
  end;
end;

procedure TLogThread.RollOver;
var
  LLogFileDate: TDate;
  LRollOver: string;
  LLogArchive: string;
  LMaxAge: integer;
begin
  // Only try once a day to roll over
  if FLastRollOver < Today then
  begin
    LMaxAge := TDXLogger.MaxLogAge;
    if LMaxAge > 0 then
    begin
      try
        FLastRollOver := Today;
        LLogFileDate := GetLogFileDate;
        if (LLogFileDate + LMaxAge) < Today then
        begin
          LRollOver := TPath.GetFileNameWithoutExtension(FLogFileName) + '-' +
            FormatDateTime('YYYY-MM-DD', LLogFileDate) + '.log';
          LLogArchive := TPath.Combine(TPath.GetDirectoryName(FLogFileName), 'Logs');
          TDirectory.CreateDirectory(LLogArchive);
          LRollOver := TPath.Combine(LLogArchive, LRollOver);
          TFile.Copy(FLogFileName, LRollOver, true);
          TFile.Delete(FLogFileName);
        end;
      except
        on E: Exception do
        begin
          Log('Roll-Over failed : %s - %s', [LRollOver, E.Message]);
        end;
      end;
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
{$IF (defined(MSWindows)or defined(LINUX) or defined(MacOS)) and not (defined(IOS)))}
var
  F: TextFile;
  s: string;
{$ENDIF}
begin
{$IF (defined(MSWindows) or defined(LINUX) or defined(MacOS)) and not (defined(IOS))) }
  try
    RollOver;
    if TFile.Exists(FLogFileName) then
    begin
      TFile.AppendAllText(FLogFileName, sLineBreak);
    end;
    TFile.AppendAllText(FLogFileName, FTempBuffer.Text, TEncoding.UTF8);
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
  if (TDXLogger.Instance <> nil) and Assigned(TDXLogger.Instance.FExternalStrings) and Assigned(FExternalBuffer) and
    not TDXLogger.FTerminating then
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

end.
