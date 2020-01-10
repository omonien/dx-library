unit DX.Classes.Configuration;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles, System.Rtti, System.Generics.Collections,
  DX.Utils.Singleton, DX.Classes.Strings;

Type

  TConfigEntry = record
    Name: string;
    Section: string;
    Default: string;
  end;

  TConfigItems = class(TThreadList<TConfigEntry>)
  public
    function Get(const APropertyName: string): TConfigEntry;
  end;

  TConfigRegistry = class(TSingleton<TConfigItems>)
  end;

  ConfigValueAttribute = class(TCustomAttribute)
    FSection: string;
    FDefault: string;
  public
    constructor Create(
      const ASection: string;
      const ADefault: string);
  end;

  ConfigDescriptionAttribute = class(TCustomAttribute)
    FText: string;
  public
    constructor Create(const AText: string);
  end;

  TConfigurationManager<T: class> = class abstract(TSingleton<T>)
  private
    FStorage: TIniFile;
    FStorageFile: string;
  protected
    procedure CreateDefaultConfig; virtual;
    function GetConfigValueForProperty(const AProperty: string): TValue;
    function GetConfigValueForPropertyAsParams(const AProperty: string): StringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.IOUtils;

constructor ConfigValueAttribute.Create(const ASection, ADefault: string);
begin
  inherited Create;
  FSection := ASection;
  FDefault := ADefault;
end;

constructor TConfigurationManager<T>.Create;
begin
  inherited;
  FStorageFile := TPath.Combine(TPath.GetLibraryPath, TPath.GetFileNameWithoutExtension(ParamStr(0)) + '.ini');
  FStorage := TIniFile.Create(FStorageFile);
  CreateDefaultConfig;
end;

procedure TConfigurationManager<T>.CreateDefaultConfig;
var
  LContext: TRttiContext;
  LConfigType: TRttiType;
  LConfigProperties: TArray<TRttiProperty>;
  LProperty: TRttiProperty;
  LAttributes: TArray<TCustomAttribute>;
  LAttribute: TCustomAttribute;
  LSection: string;
  LDefault: string;
  LDescription: string;
  LConfigItem: TConfigEntry;
  LPersist: Boolean;
begin
  LPersist := not TFile.Exists(FStorageFile);

  LContext := TRttiContext.Create;
  try
    LConfigType := LContext.GetType(T);

    LAttributes := LConfigType.GetAttributes;
    for LAttribute in LAttributes do
    begin
      if LAttribute is ConfigDescriptionAttribute then
      begin
        LDescription := (LAttribute as ConfigDescriptionAttribute).FText;
        if LPersist then
        begin
          TFile.WriteAllText(FStorageFile, '# ' + LDescription);
        end;
      end;
    end;

    TConfigRegistry.Default.Clear;
    LConfigProperties := LConfigType.GetProperties;
    for LProperty in LConfigProperties do
    begin
      LAttributes := LProperty.GetAttributes;
      for LAttribute in LAttributes do
      begin
        if LAttribute is ConfigValueAttribute then
        begin
          LConfigItem.Name := LProperty.Name;
          LConfigItem.Section := (LAttribute as ConfigValueAttribute).FSection;
          LConfigItem.Default := (LAttribute as ConfigValueAttribute).FDefault;
          TConfigRegistry.Default.Add(LConfigItem);
          if LPersist then
          begin
            FStorage.WriteString(LConfigItem.Section, LConfigItem.Name, LConfigItem.Default);
            FStorage.UpdateFile;
          end;
        end;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

destructor TConfigurationManager<T>.Destroy;
begin
  FreeAndNil(FStorage);
  inherited;
end;

function TConfigurationManager<T>.GetConfigValueForProperty(const AProperty: string): TValue;
var
  s: string;
  LConfigItem: TConfigEntry;
begin
  try
    LConfigItem := TConfigRegistry.Default.Get(AProperty);
    s := FStorage.ReadString(LConfigItem.Section, LConfigItem.Name, LConfigItem.Default);
    result := TValue.From(s);
  except
    on e: Exception do
      raise Exception.CreateFmt('Error reading config value for "%s"'#13#10'%s', [AProperty, e.message]);
  end;
end;

function TConfigurationManager<T>.GetConfigValueForPropertyAsParams(const AProperty: string): StringList;
var
  LStrings: TStrings;
begin
  LStrings := TStringList.Create;
  try
    LStrings.Delimiter := ';';
    LStrings.DelimitedText := GetConfigValueForProperty(AProperty).AsString;
    result.AddStrings(LStrings);
  finally
    FreeAndNil(LStrings);
  end;
end;

{ ConfigDescritionAttribute }

constructor ConfigDescriptionAttribute.Create(const AText: string);
begin
  FText := AText;
end;

{ TConfigList }

function TConfigItems.Get(const APropertyName: string): TConfigEntry;
var
  LList: TList<TConfigEntry>;
  LItem: TConfigEntry;
  LFound: Boolean;
begin
  LList := LockList;
  try
    LFound := false;
    for LItem in LList do
    begin
      if SameText(LItem.Name, APropertyName) then
      begin
        LFound := true;
        result := LItem;
        break;
      end;
    end;
    if not LFound then
      raise Exception.CreateFmt('No config item found for property "%s"', [APropertyName]);
  finally
    UnlockList;
  end;
end;

end.
