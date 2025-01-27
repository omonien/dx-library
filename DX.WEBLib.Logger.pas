unit DX.WEBLib.Logger;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils, System.Generics.Collections,
  Data.DB,

  JS, jsdelphisystem,
  web,
{$IFDEF PAS2JS}
  rtl.HTMLUtils,
{$ENDIF}
  WEBLib.TMSWEBUtils, WEBLib.Utils, WEBLib.IndexedDb,
  WEBLib.Forms;

type
  TLogLevel = (Error, Info, Warn, Debug);

type
  TLogMessage = record
    TimeStamp: TDateTime;
    LogMessage: string;
    LogLevel: TLogLevel;
    function ToString: string;
  end;

  /// <summary>
  /// Logging system that writes to the console and to the local IndexedDB instance.
  /// Follows the Singleton pattern. Do not create an instance manually!
  /// <br /><br />
  /// Simple usage via DXLog()
  /// </summary>
  TDXLogger = class(TObject)
  private
    class var FLogDB: TWebIndexedDbClientDataset;
  protected
    [async]
    /// <summary>
    /// Logs a message to the database.
    /// </summary>
    /// <param name="ALog">
    /// The log message to be written to the database.
    /// </param>
    class procedure LogToDB(const ALog: TLogMessage);

    [async]
    /// <summary>
    /// Opens the log database.
    /// </summary>
    class procedure OpenLogDB;
  public
    /// <summary>
    /// Class constructor.
    /// </summary>
    class constructor Create;

    /// <summary>
    /// Class destructor.
    /// </summary>
    class destructor Destroy;

    [async]
    /// <summary>
    /// Logs a message with a specified log level.
    /// </summary>
    /// <param name="AMessage">
    /// The message to log.
    /// </param>
    /// <param name="ALogLevel">
    /// The level of the log message. Defaults to Info.
    /// </param>
    class procedure Log(const AMessage: string; ALogLevel: TLogLevel = TLogLevel.Info);

    /// <summary>
    /// Retrieves log data as a string for display in a memo or similar component.
    /// </summary>
    /// <param name="ADoneProc">
    /// Callback for the result.
    /// </param>
    /// <param name="ATodayOnly">
    /// Indicates whether to retrieve only today's logs or all logs.
    /// </param>
    [async]
    class procedure GetLog(ADoneProc: TProc<string>; ATodayOnly: Boolean = true);

    /// <summary>
    /// Clears the log table data in the logDB in the local IndexedDB instance of the web browser.
    /// </summary>
    /// <param name="ADoneProc">
    /// Callback for the completion of the clear operation.
    /// </param>
    [async]
    class procedure ClearLogDb(ADoneProc: TProc);
  end;

  TWebIndexedDbClientDatasetHelper = class helper for TWebIndexedDbClientDataset
  private
    [async]
    function DBisInitialized: Boolean;
  public
    [async]
    procedure OpenDB;
  end;

  TAppErrorHandler = class(TObject)
  public
    class procedure AppError(Sender: TObject; AError: TApplicationError; var Handled: Boolean);
  end;

  /// <summary>
  /// Log procedure, that logs into console and IndexedDB
  /// </summary>
  [async]
procedure DXLog(const AMessage: string; ALogLevel: TLogLevel = TLogLevel.Info); overload;

/// <summary>
/// Log overload with Format String
/// </summary>
procedure DXLog(const AFmtMessage: string; const AArgs: array of const; ALogLevel: TLogLevel = TLogLevel.Info);
  overload;

[async]
function Sleep(AMSec: integer): integer;

implementation

uses
  DX.WEBLib.SysUtils;

function Sleep(AMSec: integer): integer;
begin
  result := AMSec;
{$IFDEF PAS2JS}
  // await(Boolean, asyncsleep(AMSec));
  asyncsleep(AMSec);
{$ENDIF}
end;

procedure DXLog(const AMessage: string; ALogLevel: TLogLevel = TLogLevel.Info);
begin
  TDXLogger.Log(AMessage, ALogLevel);
end;

procedure DXLog(const AFmtMessage: string; const AArgs: array of const; ALogLevel: TLogLevel = TLogLevel.Info);
var
  LMessage: string;
begin
  LMessage := Format(AFmtMessage, AArgs);
  DXLog(LMessage, ALogLevel);
end;

{ TDXLogger }

class procedure TDXLogger.ClearLogDb(ADoneProc: TProc);
begin
  FLogDB.First;
  while not FLogDB.Eof do
  begin
    FLogDB.Delete;
  end;
  if Assigned(ADoneProc) then
  begin
    ADoneProc;
  end;
end;

class constructor TDXLogger.Create;
begin
  FLogDB := TWebIndexedDbClientDataset.Create(nil);
  FLogDB.IDBDatabaseName := 'DXLogDB';
  FLogDB.IDBObjectStoreName := 'Log';
  FLogDB.IDBKeyFieldName := 'ID';
  FLogDB.IDBAutoIncrement := true;

  FLogDB.FieldDefs.Clear;
  FLogDB.FieldDefs.Add('ID', ftInteger);
  FLogDB.FieldDefs.Add('LogTimeStamp', ftDate);
  FLogDB.FieldDefs.Add('LogMessage', ftString);
  FLogDB.AddIDBIndex('IX_TIMESTAMP', 'LogTimeStamp');
  FLogDB.IDBActiveIndex := 'IX_TIMESTAMP';
  FLogDB.IDBIndexDescending := true;

  FLogDB.Init(
    procedure
    begin
      console.Debug('DB initialized');
      FLogDB.Tag := 1;
    end);

  Log('App is starting ...');
  Log(TAppInfo.VersionFull);
end;

class destructor TDXLogger.Destroy;
begin
end;

class procedure TDXLogger.GetLog(ADoneProc: TProc<string>; ATodayOnly: Boolean = true);
var
  LLog: string;
begin
  // await(OpenLogDB);
  if FLogDB.Active then
  begin
    FLogDB.First;
    while not FLogDB.Eof do
    begin
      if (ATodayOnly and (FLogDB.FieldByName('LogTimestamp').AsDateTime <= Today)) then
      begin
        FLogDB.Next;
        Continue;
      end
      else
      begin
        LLog := LLog + FormatDateTime('yyyy-mm-dd hh:nn:ss,zzz', FLogDB.FieldByName('LogTimestamp').AsDateTime) + ' : '
          + FLogDB.FieldByName('LogMessage').AsString + #13#10;
        FLogDB.Next;
      end;
    end;
  end
  else
  begin
    LLog := 'Log not available yet!';
  end;
  if Assigned(ADoneProc) then
  begin
    ADoneProc(LLog);
  end;
end;

class procedure TDXLogger.Log(const AMessage: string; ALogLevel: TLogLevel = TLogLevel.Info);
var
  LLog: TLogMessage;
begin
  // TLogLevel.Debug only writes to the log, when in DEBUG configuration
{$IFNDEF DEBUG}
  if ALogLevel = TLogLevel.Debug then
    exit;
{$ENDIF}
  LLog.TimeStamp := now;
  LLog.LogMessage := AMessage;
  LLog.LogLevel := ALogLevel;
  // First, write to console
  case ALogLevel of
    Error:
      console.Error(LLog.ToString);
    Info:
      console.Info(LLog.ToString);
    Warn:
      console.Warn(LLog.ToString);
    Debug:
      console.Debug(LLog.ToString);
  end;

  // Now, write to db.
  LogToDB(LLog);
end;

class procedure TDXLogger.LogToDB(const ALog: TLogMessage);
begin
  try
    if not FLogDB.Active then
      exit;
    FLogDB.Insert;
    FLogDB.FieldByName('LogTimestamp').AsDateTime := ALog.TimeStamp;
    FLogDB.FieldByName('LogMessage').AsString := ALog.LogMessage;
    FLogDB.Post;
  except
    on E: Exception do
    begin
      // If we cannot write to the db, then there is nothing we can do
      console.Error('Error writing to the LogDB ' + E.Message);

      if Assigned(FLogDB) and (FLogDB.State in dsEditModes) then
      begin
        FLogDB.Cancel;
      end;
    end;
  end;
end;

class procedure TDXLogger.OpenLogDB;
begin
  await(FLogDB.OpenDB);
end;

function TLogMessage.ToString: string;
var
  LLogLevel: string;
begin
  case LogLevel of
    Error:
      LLogLevel := '[ERROR] ';
    Info:
      LLogLevel := '';
    Debug:
      LLogLevel := '[DEBUG] ';
  end;

  result := FormatDateTime('yyyy-mm-dd hh:nn:ss,zzz', TimeStamp) + ' : ' + LLogLevel + LogMessage;
end;

{ TWebIndexedDbClientDatasetHelper }

function TWebIndexedDbClientDatasetHelper.DBisInitialized: Boolean;
begin
  while Tag = 0 do
  begin
    await(Sleep(10));
    console.Debug('waiting for DB ...');
  end;
  result := Tag > 0;
end;

procedure TWebIndexedDbClientDatasetHelper.OpenDB;
begin
  if not (self.Active) then
  begin
    console.Debug('db not open yet');
    await(DBisInitialized);

    Open;

    while not (self.Active) do
    begin
      await(Sleep(10));
    end;

    if not (self.Active) then
    begin
      console.Error(FormatDateTime('hh:nn:ss,zzz', now) + 'FAILURE - LogDB refused to open properly!');
    end
    else
    begin
      console.Debug('db now open');
    end;

  end
  else
  begin
    console.Debug('db is already open');
  end;
end;

class procedure TAppErrorHandler.AppError(Sender: TObject; AError: TApplicationError; var Handled: Boolean);
var
  LError: string;
  LStack: string;
begin
  if Assigned(AError.AError) then
  begin
    LError := AError.AError.ValueOfProperty('FMessage');
    LStack := AError.AError.ValueOfProperty('FStack')
  end
  else
  begin
    LError := 'Unknown error';
    LStack := 'No stack available';
  end;
  // DXLog(LError, TLogLevel.Error);
  DXLog(LStack, TLogLevel.Error);

  if not Handled then
  begin
{$IFDEF DEBUG}
    // let unhandled exceptions pop up only in DEBUG mode!
{$IFDEF PAS2JS}
    asm
      alert('Error: '+ LError);
    end;
{$ENDIF}
{$ENDIF}
  end;
end;

{ TJSConsoleHelper }

{$IFNDEF PAS2JS}

procedure TJSConsoleHelper.Debug(Obj1: JSValue);
begin
  // nothing: just a helper for LSP. Missing in Web.TJSConsole
end;
{$ENDIF}

initialization

  Application.OnError := TAppErrorHandler.AppError; //Everything (incl. Javascript, and Pascal related Exceptions)
end.

