unit DX.Data.Utils;

interface

uses
  System.Classes, System.SysUtils,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet,
  System.Generics.Collections;

type

  TSimpleField = record
    Name: string;
    Value: Variant;
  end;

  IRecord = interface(IInterface)
    function GetFields: TList<TSimpleField>;
    procedure SetFields(const AValue: TList<TSimpleField>);
    property Fields: TList<TSimpleField> read GetFields write SetFields;
    function ValueByName(const AFieldName: string): Variant;
  end;

  TRecord = class(TInterfacedObject, IRecord)
  strict private
    FFields: TList<TSimpleField>;
  strict protected
    function GetFields: TList<TSimpleField>;
    procedure SetFields(const AValue: TList<TSimpleField>);
  public
    constructor Create;
    destructor Destroy; override;
    property Fields: TList<TSimpleField> read GetFields write SetFields;
    function ValueByName(const AFieldName: string): Variant;
  end;

  TFDDatasetHelper = class helper for TFDDataSet
  public
    function CloneRecord: IRecord;
    procedure AssignRecord(const ASource: IRecord);
  end;

  /// <summary>
  /// Splits a FireDAC connection string into its "Name=Value" segments.
  /// </summary>
  /// <remarks>
  /// Segments are separated by ";". A ";" inside a {...} block is part of the
  /// value, and the braces are kept verbatim (e.g. ODBCAdvanced={a;b}).
  /// A segment that starts with " is a quoted Name=Value pair (AnsiQuotedStr of the
  /// whole pair): the wrapping quotes are removed, "" is an escaped quote, and a ";"
  /// inside the quotes is not a separator. Quotes that do not wrap the segment have
  /// no special meaning. Nothing is trimmed and empty segments are skipped.
  /// </remarks>
  function SplitConnectionString(const AConnectionString: string): TArray<string>;

type
  TFDConnectionHelper = class helper for TFDConnection
  public
    procedure SetParamsFromConnectionString(const AConnectionString: string);
  end;

implementation

{ TFDDatasetHelper }

procedure TFDDatasetHelper.AssignRecord(const ASource: IRecord);
var
  LField: TField;
begin
  for LField in Self.Fields do
  begin
    if (not LField.ReadOnly) and (LField.AutoGenerateValue = TAutoRefreshFlag.arNone) then
    begin
      LField.Value := ASource.ValueByName(LField.FieldName);
    end;
  end;
end;

function TFDDatasetHelper.CloneRecord: IRecord;
var
  LField: TSimpleField;
  LSourceField: TField;
begin
  result := TRecord.Create;
  for LSourceField in Self.Fields do
  begin
    LField.Name := LSourceField.FieldName;
    LField.Value := LSourceField.Value;
    result.Fields.Add(LField);
  end;
end;

constructor TRecord.Create;
begin
  inherited;
  FFields := TList<TSimpleField>.Create;
end;

destructor TRecord.Destroy;
begin
  FreeAndNIL(FFields);
  inherited;
end;

function TRecord.GetFields: TList<TSimpleField>;
begin
  result := FFields;
end;

procedure TRecord.SetFields(const AValue: TList<TSimpleField>);
begin
  FFields := AValue;
end;

function TRecord.ValueByName(const AFieldName: string): Variant;
var
  LField: TSimpleField;
begin
  for LField in Fields do
  begin
    if SameText(LField.Name, AFieldName) then
    begin
      result := LField.Value;
      break;
    end;
  end;
end;

{ TFDConnectionHelper }

procedure TFDConnectionHelper.SetParamsFromConnectionString(const AConnectionString: string);
var
  LSegment: string;
begin
  Params.Clear;
  for LSegment in SplitConnectionString(AConnectionString) do
  begin
    Params.Add(LSegment);
  end;
end;

function SplitConnectionString(const AConnectionString: string): TArray<string>;
var
  LSegments: TList<string>;
  I: Integer;

  procedure AddSegment(const ASegment: string);
  begin
    if ASegment <> '' then
    begin
      LSegments.Add(ASegment);
    end;
  end;

  // AIndex points at the opening quote. "" inside the pair is one literal quote.
  function ReadQuoted(var AIndex: Integer): string;
  begin
    Inc(AIndex);
    Result := '';
    while AIndex <= Length(AConnectionString) do
    begin
      if AConnectionString[AIndex] = '"' then
      begin
        if (AIndex < Length(AConnectionString)) and (AConnectionString[AIndex + 1] = '"') then
        begin
          Result := Result + '"';
          Inc(AIndex, 2);
        end
        else
        begin
          Inc(AIndex);
          Exit;
        end;
      end
      else
      begin
        Result := Result + AConnectionString[AIndex];
        Inc(AIndex);
      end;
    end;
  end;

  function ReadPlain(var AIndex: Integer): string;
  var
    LStart: Integer;
    LDepth: Integer;
  begin
    LStart := AIndex;
    LDepth := 0;
    while AIndex <= Length(AConnectionString) do
    begin
      case AConnectionString[AIndex] of
        '{':
          Inc(LDepth);
        '}':
          if LDepth > 0 then
          begin
            Dec(LDepth);
          end;
        ';':
          if LDepth = 0 then
          begin
            Break;
          end;
      end;
      Inc(AIndex);
    end;
    Result := Copy(AConnectionString, LStart, AIndex - LStart);
  end;

begin
  LSegments := TList<string>.Create;
  try
    I := 1;
    while I <= Length(AConnectionString) do
    begin
      if AConnectionString[I] = '"' then
      begin
        AddSegment(ReadQuoted(I));
      end
      else
      begin
        AddSegment(ReadPlain(I));
      end;
      if (I <= Length(AConnectionString)) and (AConnectionString[I] = ';') then
      begin
        Inc(I);
      end;
    end;
    Result := LSegments.ToArray;
  finally
    FreeAndNil(LSegments);
  end;
end;

end.
