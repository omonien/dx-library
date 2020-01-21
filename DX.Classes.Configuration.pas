unit DX.Classes.Configuration;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles, System.Rtti, System.Generics.Collections,
  DX.Classes.Singleton, DX.Classes.Strings;

Type

  TConfigEntry = record
    Name: string;
    Description: StringList;
    Section: string;
    Default: string;
    procedure AssignDescription(AProperty: TRttiProperty);
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
    FStorage: TMemIniFile;
    FStorageFile: string;
    FDescription: StringList;
    procedure AddComments(
      var AStrings:    StringList;
      const AComments: StringList);
  protected
    /// <summary>
    /// Writes a configuration file with all default values if no config file exists yet.
    /// Reads an existing configuration file, and adds missing default values.
    /// </summary>
    procedure CreateDefaultConfig; virtual;
    /// <summary>
    /// Returns the config value (default or whats in the config file) for
    /// config property "AProperty" as TValue
    /// </summary>
    /// <remarks>
    /// All GetConfigValueForProperty functions are to be used in Config
    /// Property's Getter methods only;
    /// </remarks>
    function GetConfigValueForProperty(const AProperty: string): Variant; overload;

    function GetConfigValueForProperty<S>(const AProperty: string): S; overload;

    /// <summary>
    /// Reads a ";" delimited string containing name=value pairs and returns
    /// a string list, suitable to be added a TStrings compatible list of
    /// name=value pairs
    /// </summary>
    /// <remarks>
    /// This can be used to read a database connection string and passing it
    /// into an FDConnection.Params property.
    /// </remarks>
    /// <example>
    /// The following config value <br />
    /// DBConnectionString=user=foo;password=bar <br />will be retruned as <br />
    /// user=foo <br />password=bar
    /// </example>
    function GetConfigValueForPropertyAsParams(const AProperty: string): StringList;
    procedure WriteDescription;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteStorage;
  end;

implementation

uses
  System.IOUtils, Loomis.SoapServer.Logger;

constructor ConfigValueAttribute.Create(const ASection, ADefault: string);
begin
  inherited Create;
  FSection := ASection;
  FDefault := ADefault;
end;

procedure TConfigurationManager<T>.AddComments(
  var AStrings:    StringList;
  const AComments: StringList);
var
  AComment: string;
begin
  for AComment in AComments do
  begin
    AStrings.Add('# ' + AComment);
  end;
end;

constructor TConfigurationManager<T>.Create;
var
  LEncoding: TEncoding;
begin
  inherited;
  FStorageFile := TPath.Combine(TPath.GetLibraryPath, TPath.GetFileNameWithoutExtension(ParamStr(0)) + '.ini');
  if TFile.Exists(FStorageFile) then
  begin
    LEncoding := nil; // Use Encoding of existing file
  end
  else
  begin
    LEncoding := TEncoding.UTF8;
  end;
  FStorage := TMemIniFile.Create(FStorageFile, LEncoding);
  FDescription.Clear;
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
  LConfigItem: TConfigEntry;
  LConfigExists: Boolean;
  LConfig: StringList;
begin
  LConfigExists := TFile.Exists(FStorageFile);

  LContext := TRttiContext.Create;
  try
    LConfigType := LContext.GetType(T);

    LAttributes := LConfigType.GetAttributes;
    for LAttribute in LAttributes do
    begin
      if LAttribute is ConfigDescriptionAttribute then
      begin
        // Main description
        FDescription.Add((LAttribute as ConfigDescriptionAttribute).FText);
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
          LConfigItem.AssignDescription(LProperty);
          TConfigRegistry.Default.Add(LConfigItem);

          if not FStorage.ValueExists(LConfigItem.Section, LConfigItem.Name) then
          begin
            FStorage.WriteString(LConfigItem.Section, LConfigItem.Name, LConfigItem.Default);
          end;
        end;
      end;
    end;
  finally
    LContext.Free;
  end;
  if FStorage.Modified then
  begin
    WriteStorage;
  end;
end;

destructor TConfigurationManager<T>.Destroy;
begin
  FreeAndNil(FStorage);
  inherited;
end;

function TConfigurationManager<T>.GetConfigValueForProperty(const AProperty: string): Variant;
var
  S: string;
  LConfigItem: TConfigEntry;
begin
  try
    LConfigItem := TConfigRegistry.Default.Get(AProperty);
    S := FStorage.ReadString(LConfigItem.Section, LConfigItem.Name, LConfigItem.Default);
    result := S;
  except
    on e: Exception do
    begin
      // TLoomisLogger.Log(e);
      raise Exception.CreateFmt('Error reading config value for "%s"'#13#10'%s', [AProperty, e.message]);
    end;
  end;
end;

function TConfigurationManager<T>.GetConfigValueForProperty<S>(const AProperty: string): S;
begin
  // LTypeInfo : =
end;

function TConfigurationManager<T>.GetConfigValueForPropertyAsParams(const AProperty: string): StringList;
var
  LStrings: TStrings;
  s:string;
begin
  LStrings := TStringList.Create;
  try
    LStrings.Delimiter := ';';
    LStrings.DelimitedText := GetConfigValueForProperty(AProperty);
    result.AddStrings(LStrings);
  finally
    FreeAndNil(LStrings);
  end;
end;

procedure TConfigurationManager<T>.WriteStorage;
begin
  FStorage.UpdateFile;
  WriteDescription;
end;

procedure TConfigurationManager<T>.WriteDescription;
var
  LContent: StringList;
  LConfigItem: TConfigEntry;
  LConfigItems: TList<TConfigEntry>;
begin
  LContent.Clear;
  // Main description
  AddComments(LContent, FDescription);

  // Followed by description of all items
  LConfigItems := TConfigRegistry.Default.LockList;
  try
    for LConfigItem in LConfigItems do
    begin
      if not LConfigItem.Description.IsEmpty then
      begin
        LContent.Add('#');
        LContent.Add(Format('# %s / %s', [LConfigItem.Section, LConfigItem.Name]));
        AddComments(LContent, LConfigItem.Description);
      end;
    end;
  finally
    TConfigRegistry.Default.UnlockList;
  end;
  LContent.AddStrings(TFile.ReadAllLines(FStorageFile));
  TFile.WriteAllLines(FStorageFile, LContent);
end;

{ ConfigDescritionAttribute }

constructor ConfigDescriptionAttribute.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

{ TConfigList }

function TConfigItems.Get(const APropertyName: string): TConfigEntry;
var
  LList: TList<TConfigEntry>;
  LItem: TConfigEntry;
begin
  LList := LockList;
  try
    result := Default (TConfigEntry);
    for LItem in LList do
    begin
      if SameText(LItem.Name, APropertyName) then
      begin
        result := LItem;
        break;
      end;
    end;
    if result.Name = '' then
      raise Exception.CreateFmt('No config item found for property "%s"', [APropertyName]);
  finally
    UnlockList;
  end;
end;

{ TConfigEntry }

procedure TConfigEntry.AssignDescription(AProperty: TRttiProperty);
var
  LAttributes: TArray<TCustomAttribute>;
  LAttribute: TCustomAttribute;
begin
  Description.Clear;
  LAttributes := AProperty.GetAttributes;
  for LAttribute in LAttributes do
  begin
    if LAttribute is ConfigDescriptionAttribute then
    begin
      Description.Add((LAttribute as ConfigDescriptionAttribute).FText);
    end;
  end;
end;

end.
