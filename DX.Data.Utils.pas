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
  LConnectionString: TStringList;
begin
  Params.Clear;
  LConnectionString := TStringList.Create;
  try
    LConnectionString.Delimiter := ';';
    LConnectionString.DelimitedText := AConnectionString;
    Params.AddStrings(LConnectionString);
  finally
    FreeAndNIL(LConnectionString);
  end;
end;

end.
