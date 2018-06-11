unit DX.Settings.Json;

interface

uses
  System.SysUtils, System.Classes, DX.Settings, System.Json;

type
  TConfigFile = class(TInterfacedObject, IConfigFile)
  private
    FFormat: TSettingsFormat;
    FConfig: TJsonObject;
    procedure LoadConfig;
    procedure SaveConfig;
    function LoadValue(const ASection: string; const AName: string): TJsonValue;
    function LoadSection(const ASection: string): TJsonObject;
  public
    constructor Create;
    destructor Destroy; override;
    function Format: TSettingsFormat;
    function Filename: string;
    procedure Write(const ASection: string; const AName: string; AVAlue: string); overload;
    procedure Write(const ASection: string; const AName: string; AVAlue: TObject); overload;

    function Read(const ASection, AName: string; const ADefault: string): string; overload;
    function Read(const ASection, AName: string; AClass: TClass): TObject; overload;

  end;

implementation

uses
  System.IOUtils, DX.Classes.Factory, REST.Json;

{ TConfigFile }

constructor TConfigFile.Create;
begin
  FFormat := TSettingsFormat.Json;
end;

destructor TConfigFile.Destroy;
begin
  inherited;
end;

function TConfigFile.Filename: string;
begin
  result := TPath.ChangeExtension(ParamStr(0), '.config');
end;

function TConfigFile.Format: TSettingsFormat;
begin
  result := TSettingsFormat.Json;
end;

procedure TConfigFile.LoadConfig;
begin
  if FileExists(Filename) then
  begin
    try
      FConfig := TJsonObject.ParseJSONValue(TFile.ReadAllText(Filename)) as TJsonObject;
    except
      raise Exception.Create(Filename + ' is not a valid configuration file');
    end;
  end
  else
  begin
    FreeAndNil(FConfig);
    FConfig := TJsonObject.Create;
  end;
end;

function TConfigFile.LoadSection(const ASection: string): TJsonObject;
var
  LSection: TJsonValue;
begin
  LoadConfig;
  LSection := FConfig.Values[ASection];
  if not Assigned(LSection) then
  begin
    LSection := TJsonObject.Create;
    FConfig.AddPair(ASection, LSection);
  end;
  result := LSection as TJsonObject;
end;

function TConfigFile.LoadValue(const ASection, AName: string): TJsonValue;
var
  LSection: TJsonValue;
begin
  LoadConfig;
  result := nil;
  LSection := FConfig.Values[ASection];
  if Assigned(LSection) then
  begin
    result := (LSection as TJsonObject).Values[AName];
  end;
end;

function TConfigFile.Read(const ASection, AName: string; AClass: TClass): TObject;
var
  LValue: TJsonValue;
begin
  result := AClass.Create;
  LValue := LoadValue(ASection, AName);
  if Assigned(LValue) and (LValue is TJsonObject) then
  begin
    TJson.JsonToObject(result, LValue as TJsonObject);
  end;
  SaveConfig;
end;

procedure TConfigFile.SaveConfig;
begin
  TFile.WriteAllText(Filename, FConfig.ToJSON);
end;

procedure TConfigFile.Write(const ASection, AName: string; AVAlue: TObject);
var
  LSection: TJsonObject;
  LValue: TJsonObject;
begin
  LValue := TJson.ObjectToJsonObject(AVAlue);
  LSection := LoadSection(ASection);
  LSection.RemovePair(AName);
  LSection.AddPair(AName, LValue);
  SaveConfig;
end;

function TConfigFile.Read(const ASection, AName: string; const ADefault: string): string;
var
  LValue: TJsonValue;
  LSection: TJsonValue;
begin
  LoadConfig;
  result := ADefault;
  LSection := FConfig.Values[ASection];
  if Assigned(LSection) then
  begin
    LValue := (LSection as TJsonObject).Values[AName];
    if Assigned(LValue) then
    begin
      result := LValue.Value;
    end;
  end;
end;

procedure TConfigFile.Write(const ASection, AName: string; AVAlue: string);
var
  LSection: TJsonObject;
begin
  LSection := LoadSection(ASection);
  LSection.RemovePair(AName);
  LSection.AddPair(AName, AVAlue);
  SaveConfig;
end;

initialization

DXFactory.RegisterClassForInterface(TConfigFile, IConfigFile);

end.
