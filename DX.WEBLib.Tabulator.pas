unit DX.WEBLib.Tabulator;

interface

uses
  System.SysUtils, System.StrUtils, System.DateUtils, System.Generics.Collections,
  Data.DB,
  JS, jsdelphisystem,
  WEBLib.WebCtrls, WEBLib.DB, WEBLib.TMSWEBUtils;

{$M+}

type
  TTabulatorConfig = class;
  TColumn = class;

{$SCOPEDENUMS ON}
  TColumnType = (Text, Memo, Date, Time, DateTime, Number, Money, Boolean);
  TColumnHorizontalAlign = (Left, Center, Right);
{$SCOPEDENUMS OFF}

  TSorterParams = class
  public
    alignEmptyValues: string;
    format: string;
    thousandSeparator: string;
    decimalSeparator: string;
  end;

  TEditorParams = class
  public
    format: string;
  end;

  TFormatterParams = class
  public
    inputFormat: string;
    invalidPlaceholder: string;
    outputFormat: string;
    // date
    timezone: string;
    // money
    decimal: string;
    thousand: string;
    symbol: string;
    symbolAfter: string;
    negativeSign: Boolean;
    precision: Boolean;
    // boolean
    allowEmpty: Boolean;
    allowTruthy: Boolean;
    tickElement: string;
    crossElement: string;
    constructor Create;
  end;

{$SCOPEDENUMS ON}

  TSortDirection = (asc, desc);

  THeaderFilterParams = class
  public
    values: TJSObject;
    clearable: Boolean;
    multiselect: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure AddValue(const Akey: string; const AValue: string);
  end;

  TColumn = class
  strict private
    FConfig: TTabulatorConfig;
    FColumnType: TColumnType;
  public
    editor: string;
    editorParams: TEditorParams;
    field: string;
    formatter: string;
    formatterParams: TFormatterParams;
    headerFilter: JSValue;
    headerFilterPlaceholder: string;
    headerFilterParams: THeaderFilterParams;
    headerFilterFunc: JSValue;
    headerFilterLiveFilter: Boolean;
    hozAlign: string; // left|center|right
    sorter: string;
    sorterParams: TSorterParams;
    title: string;
    constructor Create(AConfig: TTabulatorConfig; const AFieldName: string; const ATitle: string = '');
    destructor Destroy; override;
    property ColumnType: TColumnType read FColumnType write FColumnType;
    // Fluid Interface
    function SetColumnType(AType: TColumnType): TColumn;
    function SetHorizontalAlign(AAlign: TColumnHorizontalAlign): TColumn;

  end;

  /// <summary>
  /// Encapuslates the configuration of a Tabulator instance. Do not create instances of TTabulatorConfig. Use TTabulatorG instead.
  /// </summary>
  TTabulatorConfig = class abstract(TObject)
  private
    FFormatSettings: TFormatSettings;
    FTimezone: string;
    FContainer: TWebHTMLDiv;
    FReadOnly: Boolean;
    FAutoFilter: Boolean;
    function GetColumnCount: integer;
    procedure ShowData(
      AData: TJSArray;
      const ADefaultSortField: string = '';
      ADefaultSortDirection: TSortDirection = TSortDirection.asc);
    function GetHtmlName: string;
  public
    columns: TArray<TColumn>;
    layout: string;
    clipboard: Boolean; // true, false. ? copy, paste ?
    printAsHtml: Boolean;
    placeholder: string;
    constructor Create(AContainer: TWebHTMLDiv); reintroduce;
    destructor Destroy; override;
    property ColumnCount: integer read GetColumnCount;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Autofilter: Boolean read FAutoFilter write FAutoFilter;
    function AddColumn(const AFieldName: string; const ATitle: string = ''): TColumn; overload;
    function AddColumn(AField: TField): TColumn; overload;
    function ColumnByFieldName(const AFieldName: string): TColumn;
    property FormatSettings: TFormatSettings read FFormatSettings write FFormatSettings;
    [async]
    procedure Print;
    [async]
    procedure ExportExcel(const AFileName: string; ADoneProc: TProc = nil);
    /// <summary>
    /// Die Copy (to Clipboard) Funktion wird offenbar nicht mehr von allen Browsern in allen Szenarien unterstützt. Vermutlich ein Rechteproblem. Daher nicht "bewerben"
    /// </summary>
    [async]
    procedure Copy;

  end;

  TTabulator = class(TTabulatorConfig)
  private
    FDataSource: TWebDataSource;
  public
    constructor Create(AContainer: TWebHTMLDiv; ADataSource: TWebDataSource; AAutoCreateColumns: Boolean = true);
      reintroduce;
    /// <summary>
    /// Configures and displays the Table within a TWebHTMLDiv. AData will be used as data source.
    /// </summary>
    procedure Show(
      const ADefaultSortField: string = '';
      ADefaultSortDirection: TSortDirection = TSortDirection.asc);
  end;

  /// <summary>
  /// Extends TTabulatorConfig, using a generic type for the data.
  /// </summary>
  TTabulatorG<T> = class(TTabulatorConfig)
  public
    /// <summary>
    /// Configures and displays the Table within aTWebHTMLDiv. AData will be used as data source.
    /// </summary>
    procedure Show(
      AData: TArray<T>;
      const ADefaultSortField: string = '';
      ADefaultSortDirection: TSortDirection = TSortDirection.asc);
  end;

  // Helper to generate Luxon specific date/time format tokens
  // and keep them consistent with Delphi Date/Time/DateTime formats
  // https://moment.github.io/luxon/#/formatting?id=table-of-tokens
  TFormatSettingsHelper = record helper for TFormatSettings
    function DateFormat: string;
    function TimeFormat: string;
    function DateTimeFormat: string;
    function LuxDateFormat: string;
    function LuxTimeFormat: string;
    function LuxDateTimeFormat: string;

  end;

implementation

uses
  Bcl.Utils, DX.WEBLib.Logger, Web;

{ TTabulatorConfig }

function TTabulatorConfig.AddColumn(const AFieldName: string; const ATitle: string): TColumn;
begin
  SetLength(columns, ColumnCount + 1);
  columns[ColumnCount - 1] := TColumn.Create(self, AFieldName, ATitle);
  result := columns[ColumnCount - 1];
end;

function TTabulatorConfig.AddColumn(AField: TField): TColumn;
begin
  result := AddColumn(AField.FieldName, AField.DisplayName);
  case AField.DataType of
    // Text
    ftUnknown, ftString, ftFixedChar, ftWideString, ftMemo:
      result.SetColumnType(TColumnType.Text);

    // Numbers
    ftAutoInc, ftInteger, ftWord, ftFloat, ftLargeint:
      result.SetColumnType(TColumnType.Number);

    // Boolean
    ftBoolean:
      result.SetColumnType(TColumnType.Boolean);

    // Money
    // ftCurrency:
    // result.SetColumnType(TColumnType.Money);

    // Date
    ftDate:
      result.SetColumnType(TColumnType.Date);
    ftTime:
      result.SetColumnType(TColumnType.Time);
    ftDateTime:
      result.SetColumnType(TColumnType.DateTime);

    (*
      ftBytes
      ftVarBytes: ;
      ftBlob: ;
      ftGraphic: ;
      ftParadoxOle: ;
      ftDBaseOle: ;
      ftTypedBinary: ;
      ftCursor: ;
      ftADT: ;
      ftArray: ;
      ftReference: ;
      ftDataSet: ;
      ftOraBlob: ;
      ftOraClob: ;
      ftVariant: ;
      ftInterface: ;
      ftIDispatch: ;
      ftGuid: ;
      ftTimeStamp: ;
      ftFMTBcd: ;
      ftOraTimeStamp: ;
      ftOraInterval: ;
      ftConnection: ;
      ftParams: ;
      ftStream: ;
      ftTimeStampOffset: ;
      ftObject: ;
    *)
  end;

end;

function TTabulatorConfig.ColumnByFieldName(const AFieldName: string): TColumn;
var
  i: integer;
begin
  result := nil;
  for i := 0 to ColumnCount - 1 do
  begin
    if columns[i].field.ToLower = AFieldName.ToLower then
    begin
      result := columns[i];
      break;
    end;
  end;
end;

procedure TTabulatorConfig.Copy;
begin
{$IFDEF PAS2JS}
  asm
    Tabulator.findTable(this.GetHtmlName())[0].copyToClipboard("active");
  end;
{$ENDIF}
end;

constructor TTabulatorConfig.Create(AContainer: TWebHTMLDiv);
begin
  FContainer := AContainer;
  FReadOnly := true;
  SetLength(columns, 0);
  layout := 'fitDataStretch';
  clipboard := true;
  printAsHtml := true;
  placeholder := 'No Data Available';
  // layout := 'fitDataFill';
  // layout :=  'fitColumns';
  // Use default format settings
  FFormatSettings := System.SysUtils.FormatSettings;
  FTimezone := ''; // TTimeZone.Local.GetDisplayName(now);
end;

destructor TTabulatorConfig.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(columns) - 1 do
  begin
    columns[i].Free;
  end;
  SetLength(columns, 0);

{$IFDEF PAS2JS}
  asm
    var oldTable = Tabulator.findTable(this.GetHtmlName())[0];
    if (oldTable) {
    console.log('Destroying Tabulator instance...');
    oldTable.destroy();
     }
  end;
{$ENDIF}
  inherited;
end;

procedure TTabulatorConfig.ExportExcel(const AFileName: string; ADoneProc: TProc = nil);
var
  LFileName: string;
begin

  LFileName := AFileName.Trim;
  if not LFileName.EndsWith('.xlsx', true) then
  begin
    LFileName := LFileName + '.xlsx';
  end;

{$IFDEF PAS2JS}
  asm
    Tabulator.findTable(this.GetHtmlName())[0].download("xlsx", LFileName, { sheetName:"Data" });
  end;
{$ENDIF}
  if Assigned(ADoneProc) then
  begin
    ADoneProc;
  end;
end;

function TTabulatorConfig.GetColumnCount: integer;
begin
  result := Length(columns);
end;

function TTabulatorConfig.GetHtmlName: string;
begin
  result := '#' + FContainer.ElementID;
end;

procedure TTabulatorConfig.Print;
begin
{$IFDEF PAS2JS}
  asm
    Tabulator.findTable(this.GetHtmlName())[0].print("active", true);
  end;
{$ENDIF PAS2JS}
end;

procedure TTabulatorConfig.ShowData(
  AData: TJSArray;
  const ADefaultSortField: string = '';
  ADefaultSortDirection: TSortDirection = TSortDirection.asc);
var
  LSortColumn: TJSObject;
begin
  DXLog('GetHtmlName: ' + GetHtmlName, TLogLevel.Debug);

  LSortColumn := TJSObject.new;
  if ADefaultSortField = '' then
  begin
    LSortColumn['column'] := columns[0].field;
    LSortColumn['dir'] := 'asc';
  end
  else
  begin
    LSortColumn['column'] := ADefaultSortField;
    case ADefaultSortDirection of
      TSortDirection.asc:
        LSortColumn['dir'] := 'asc';
      TSortDirection.desc:
        LSortColumn['dir'] := 'desc';
    end;
  end;
  {$IFDEF PAS2JS}
  asm
    // Wir suchen zunächst ob es an dem DIV bereits einen Tabulator gibt.
    // Wenn nicht, dann erzeugen wir die Instanz erstmalig
    // Ansonst ersetzen wir nur die Daten.
    var table = Tabulator.findTable(this.GetHtmlName())[0];
    if (!table) {
    console.log('creating new Tabulator instance');
    var table = new Tabulator(this.GetHtmlName(),
    {
    layout: this.layout,
    clipboard: this.clipboard,
    printAsHtml: this.printAsHtml,
    placeholder: this.placeholder,
    data: AData,
    columns: this.columns,
    initialSort: [LSortColumn]
     })
    }

  else {
    console.log('Tabulator replacing data...');
    table.replaceData(AData);
     }

  end;
{$ENDIF}

end;

{ TColumn }

constructor TColumn.Create(AConfig: TTabulatorConfig; const AFieldName: string; const ATitle: string = '');
begin
  inherited Create;
  FConfig := AConfig;
  formatterParams := TFormatterParams.Create;
  sorterParams := TSorterParams.Create;

  field := AFieldName;
  if ATitle > '' then
  begin
    title := ATitle;
  end
  else
  begin
    title := AFieldName;
  end;

  // Defaults
  if AConfig.ReadOnly then
  begin
    editor := '';
  end
  else
  begin
    editor := 'input';
  end;

  formatter := 'plaintext';
  hozAlign := 'left';
  sorter := 'string';

  if FConfig.Autofilter then
  begin
    headerFilter := 'input'; // text-based filtering
  end
  else
  begin
    headerFilter := '';
  end;

  headerFilterParams := THeaderFilterParams.Create;
  headerFilterFunc := nil;
  headerFilterLiveFilter := true;

  SetColumnType(TColumnType.Text);
end;

destructor TColumn.Destroy;
begin
  formatterParams.Free;
  sorterParams.Free;
  headerFilterParams.Free;
  inherited;
end;

function TColumn.SetColumnType(AType: TColumnType): TColumn;
begin
  result := self;
  FColumnType := AType;
  case AType of
    TColumnType.Text:
      begin
        formatter := 'plaintext';
        sorter := 'string';
      end;
    TColumnType.Memo:
      begin
        formatter := 'textarea';
        sorter := 'string';
      end;
    TColumnType.Date:
      begin
        hozAlign := 'right';
        sorter := 'date';
        sorterParams.format := FormatSettings.LuxDateFormat;
        sorterParams.alignEmptyValues := 'top';

        formatter := 'datetime';
        formatterParams.inputFormat := FormatSettings.LuxDateFormat;
        formatterParams.outputFormat := FormatSettings.LuxDateFormat;

        formatterParams.invalidPlaceholder := '(Ungültiges Datum)'; // Todo: localize
        formatterParams.timezone := FConfig.FTimezone;
      end;
    TColumnType.Time:
      begin
        hozAlign := 'right';
        sorter := 'time';
        // by default, we use the same date/times formats in the sorter and formatter sections
        sorterParams.format := FormatSettings.LuxTimeFormat;
        sorterParams.alignEmptyValues := 'top';

        formatter := 'datetime';
        formatterParams.inputFormat := FormatSettings.LuxTimeFormat;
        formatterParams.outputFormat := FormatSettings.LuxTimeFormat;

        formatterParams.invalidPlaceholder := '(Ungültige Uhrzeit)'; // Todo: localize
        formatterParams.timezone := FConfig.FTimezone;
      end;
    TColumnType.DateTime:
      begin
        hozAlign := 'right';
        sorter := 'datetime';
        sorterParams.format := FormatSettings.LuxDateTimeFormat;
        sorterParams.alignEmptyValues := 'top';

        formatter := 'datetime';
        formatterParams.inputFormat := FormatSettings.LuxDateTimeFormat;
        formatterParams.outputFormat := FormatSettings.LuxDateTimeFormat;

        formatterParams.invalidPlaceholder := '(Ungültiges Datum)'; // Todo: localize
        formatterParams.timezone := FConfig.FTimezone;
      end;

    TColumnType.Number:
      begin
        hozAlign := 'right';
        sorter := 'number';
        sorterParams.thousandSeparator := FormatSettings.thousandSeparator;
        sorterParams.decimalSeparator := FormatSettings.decimalSeparator;
        sorterParams.alignEmptyValues := 'top';
      end;

    TColumnType.Money:
      begin
        hozAlign := 'right';
        sorter := 'number';
        sorterParams.thousandSeparator := FormatSettings.thousandSeparator;
        sorterParams.decimalSeparator := FormatSettings.decimalSeparator;
        sorterParams.alignEmptyValues := 'top';

        formatter := 'money';
        formatterParams.decimal := FormatSettings.decimalSeparator;
        formatterParams.thousand := FormatSettings.thousandSeparator;
        formatterParams.symbol := FormatSettings.CurrencyString;
        // CurrencyFormat = 3 means: 0.00 € ("Symbol after") 'p' is in Tabulator docs for "true"
        // https://tabulator.info/docs/5.5/format#formatter-money
        formatterParams.symbolAfter := IfThen(FormatSettings.CurrencyFormat = 3, 'p', '');
        formatterParams.negativeSign := true; // Todo: check FormatSettings
        formatterParams.precision := false; // Todo: check FormatSettings
      end;

    TColumnType.Boolean:
      begin
        hozAlign := 'center';
        sorter := 'boolean';
        // https://jsfiddle.net/nz62suho/7/
        headerFilterPlaceholder := 'true/false';
        headerFilter := 'input';
        // headerFilterParams.multiselect := false;
        // headerFilterParams.values['true'] := 'true';
        // headerFilterParams.values['false'] := 'false';

        headerFilterParams.clearable := true;
        // headerFilterFunc := 'in';

        formatter := 'tickCross';
        formatterParams.allowEmpty := true;
        formatterParams.allowTruthy := true;
        formatterParams.tickElement := '<i class=''fa fa-check''></i>';
        formatterParams.crossElement := '<i class=''fa fa-times''></i>';
        // functional checkbox, see:
        // https://stackoverflow.com/a/58239487/99158
      end;
  end;
end;

function TColumn.SetHorizontalAlign(AAlign: TColumnHorizontalAlign): TColumn;
begin
  case AAlign of
    TColumnHorizontalAlign.Left: hozAlign := 'left';
    TColumnHorizontalAlign.Center: hozAlign := 'center';
    TColumnHorizontalAlign.Right: hozAlign := 'right';
  end;
  result := self;
end;

{ TFormatterParams }

constructor TFormatterParams.Create;
begin
  invalidPlaceholder := 'Ungültige Daten'; // Todo: localize
end;

{ TFormatSettingsHelper }

function TFormatSettingsHelper.DateFormat: string;
begin
  result := ShortDateFormat;
end;

function TFormatSettingsHelper.TimeFormat: string;
begin
  result := LongTimeFormat;
end;

function TFormatSettingsHelper.DateTimeFormat: string;
begin
  result := DateFormat + ' ' + TimeFormat;
end;

function TFormatSettingsHelper.LuxDateFormat: string;
begin
  // Delphi: dd.mm.yyyy
  result := self.DateFormat;
  // luxon: dd.MM.yyyy
  result := result.Replace('m', 'M');
end;

function TFormatSettingsHelper.LuxDateTimeFormat: string;
begin
  result := LuxDateFormat + ' ' + LuxTimeFormat;
end;

function TFormatSettingsHelper.LuxTimeFormat: string;
begin
  // Delphi: hh:mm:ss hh:nn:ss  "hh:nn:ss AMPM"
  // 'n' and 'm' can be used for minutes in Delphi
  result := self.TimeFormat.Replace('n', 'm');
  // luxon: HH:mm:ss  (24h) hh:mm:ss  (12h)
  if result.Contains('AMPM') or result.Contains('a') then // 12h
  begin
    result := result.Replace('H', 'h');
    result := result.Replace('AMPM', 'a');
  end
  else
  begin // 24h
    result := result.Replace('h', 'H');
  end;

end;

procedure TTabulatorG<T>.Show(
  AData: TArray<T>;
  const ADefaultSortField: string = '';
  ADefaultSortDirection: TSortDirection = TSortDirection.asc);
var
  LData: TJSArray;
  LRecord: TJSObject;
  i: integer;
begin
  LData := TJSArray.new;
  for i := 0 to Length(AData) - 1 do
  begin
    // Works that way in JS! IFDEFed because Delphi LSP won't understand
    LRecord := TJSObject.Create{$IFDEF PAS2JS}(TJSObject(AData[i])){$ENDIF};
    LData.push(LRecord);
  end;
  ShowData(LData, ADefaultSortField, ADefaultSortDirection);
end;

{ TTabulator }

constructor TTabulator.Create(AContainer: TWebHTMLDiv;
  ADataSource: TWebDataSource;
  AAutoCreateColumns: Boolean = true);
var
  i: integer;
begin
  inherited Create(AContainer);
  FDataSource := ADataSource;
  if AAutoCreateColumns then
  begin
    for i := 0 to ADataSource.DataSet.FieldCount - 1 do
    begin
      AddColumn(ADataSource.DataSet.Fields[i]);
    end;
  end;
end;

procedure TTabulator.Show(
  const ADefaultSortField: string = '';
  ADefaultSortDirection: TSortDirection = TSortDirection.asc);
var
  i: integer;
  s: string;
  LColumn: TColumn;
  LData: TJSArray;
  LRecord: TJSObject;
  LField: TField;
  LDate: string;

  procedure ProcessField;
  begin
    LColumn := ColumnByFieldName(LField.FieldName);
    // Only if there is a column for the given field, we will further process that field
    if Assigned(LColumn) then
    begin
      case LColumn.ColumnType of
        TColumnType.Text, TColumnType.Memo:
          begin
            s := LField.AsString;
            LRecord[LField.FieldName] := s;
          end;
        TColumnType.Date, TColumnType.Time, TColumnType.DateTime:
          begin
            if LField.IsNull then
            begin
              LDate := '';
            end
            else
            begin
              if LColumn.ColumnType = TColumnType.Time then
              begin
                // Time
                LDate := FormatDateTime(FormatSettings.TimeFormat, LField.AsDateTime);
              end
              else if LColumn.ColumnType = TColumnType.Date then
              begin
                // Date
                LDate := FormatDateTime(FormatSettings.DateFormat, LField.AsDateTime);
              end
              else
              begin
                // DateTime
                LDate := FormatDateTime(FormatSettings.DateTimeFormat, LField.AsDateTime);
              end;
            end;
            LRecord[LField.FieldName] := LDate;
          end;
        TColumnType.Number:
          LRecord[LField.FieldName] := LField.AsFloat;
        TColumnType.Money:
          LRecord[LField.FieldName] := LField.AsFloat;
        TColumnType.Boolean:
          LRecord[LField.FieldName] := LField.AsBoolean;
      else
        raise Exception.Create('Unsupported ColumnType');
      end;
    end;
  end;

begin
  LData := TJSArray.new;

  FDataSource.DataSet.First;
  while not FDataSource.DataSet.Eof do
  begin
    LRecord := TJSObject.new;
    for i := 0 to FDataSource.DataSet.FieldCount - 1 do
    begin
      LField := FDataSource.DataSet.Fields[i];

      if LField.IsNull then
      begin
        LRecord[LField.FieldName] := nil;
      end
      else
      begin
        ProcessField;
      end;
    end;
    LData.push(LRecord);
    FDataSource.DataSet.Next;
  end;

  ShowData(LData, ADefaultSortField, ADefaultSortDirection);
end;

{ THeaderFilterParams }

procedure THeaderFilterParams.AddValue(
  const Akey: string;
  const AValue: string);
begin
end;

constructor THeaderFilterParams.Create;
begin
  inherited;
  clearable := true;
  multiselect := false;
  values := TJSObject.new;
end;

destructor THeaderFilterParams.Destroy;
begin
  inherited;
end;

end.
