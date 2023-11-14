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
  WEBLib.Forms,
  DX.WEBLib.Utils;

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
  /// Zentrales Logging-System, dass auf die Konsole und in die lokale IndexedDB Instanz schreibt. Folgt dem Singleton Pattern. Keine manuelle Instanz bilden! <br /><br />Einfache Nutzung über
  /// DXLog()
  /// </summary>
  TDXLogger = class(TObject)
  private
    class var FLogDB: TWebIndexedDbClientDataset;
  protected
    [async]
    class procedure LogToDB(const ALog: TLogMessage);
    [async]
    class procedure OpenLogDB;
  public
    class constructor Create;
    class destructor Destroy;
    [async]
    class procedure Log(const AMessage: string; ALogLevel: TLogLevel = TLogLevel.Info);
    /// <summary>
    /// Liefert die Logdaten als String um Sie in einem Memo o.ä. anzuzeigen
    /// </summary>
    /// <param name="ADoneProc">
    /// Callback für den Result.
    /// </param>
    /// <param name="ATodayOnly">
    /// Nur heute oder alle Logs.
    /// </param>
    [async]
    class procedure GetLog(ADoneProc: TProc<string>; ATodayOnly: Boolean = true);
    /// <summary>
    /// Löscht die Daten in der Log-Tabelle derLogDB in der lokalen IndexedDB Instanz des Webbrowsers
    /// </summary>
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

type
  TAppErrorHandler = class(TObject)
  public
    class procedure AppError(Sender: TObject; AError: TApplicationError; var Handled: Boolean);
  end;

  /// <summary>
  /// Log procedure, that logs into console and InexedDB
  /// </summary>
  [async]
procedure DXLog(const AMessage: string; ALogLevel: TLogLevel = TLogLevel.Info);

implementation

[async]
function Sleep(AMSec: integer): integer;
begin
  result := AMSec;
{$IFDEF PAS2JS}
  await(Boolean, asyncsleep(AMSec));
{$ENDIF}
end;

procedure DXLog(const AMessage: string; ALogLevel: TLogLevel = TLogLevel.Info);
begin
  TDXLogger.Log(AMessage, ALogLevel);
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

  Log('App startet ...');
end;

class destructor TDXLogger.Destroy;
begin
end;

class procedure TDXLogger.GetLog(ADoneProc: TProc<string>; ATodayOnly: Boolean = true);
var
  LLog: string;
begin
  await(OpenLogDB);
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
    LLog := 'Log nicht verfügbar - Bitte erneut versuchen!';
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
  // LogLevel DEBUG schreibt NUR im DEBUG Modus in den Log!
{$IFNDEF DEBUG}
  if ALogLevel = TLogLevel.Debug then
    exit;
{$ENDIF}
  LLog.TimeStamp := now;
  LLog.LogMessage := AMessage;
  LLog.LogLevel := ALogLevel;
  // Zunächst in die console
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

  // Und nun in die lokale DB des Browsers
  LogToDB(LLog);
end;

class procedure TDXLogger.LogToDB(const ALog: TLogMessage);
begin
  try
    await(OpenLogDB);
    if not FLogDB.Active then
      exit;
    FLogDB.Insert;
    FLogDB.FieldByName('LogTimestamp').AsDateTime := ALog.TimeStamp;
    FLogDB.FieldByName('LogMessage').AsString := ALog.LogMessage;
    FLogDB.Post;
  except
    on e: Exception do
    begin
      // Wenn wir nicht in die DB loggen können, dann nix weiter tun
      console.Error('Error writing to the LogDB ' + e.Message);

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
  if not(self.Active) then
  begin
    console.Debug('db not open yet');
    await(DBisInitialized);

    Open;

    while not(self.Active) do
    begin
      await(Sleep(10));
    end;

    if not(self.Active) then
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
begin
  if not Handled then
  begin
    if Assigned(AError.AError) then
      LError := AError.AError.ValueOfProperty('FMessage')
    else
    begin
      LError := AError.AMessage;
      DXLog(LError, TLogLevel.Error);
    end;
{$IFDEF DEBUG}
    // Wir zeigen unhandled Exceptions nur im Debug mode an, um Kunden nicht zu erschrecken
{$IFDEF PAS2JS}
    asm
      alert('Error: '+ LError);
    end;
{$ENDIF}
{$ENDIF}
  end;
end;

initialization

Application.OnError := TAppErrorHandler.AppError;

end.
