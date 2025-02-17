unit DX.Classes.Configuration;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles, System.Rtti, System.Generics.Collections,
  DX.Classes.Singleton, DX.Classes.Strings, DX.Classes.Attributes, DX.Classes.Configuration.Intf, DX.Utils.Logger;

type

  TIniFileHelper = class helper for TIniFile
  public
    procedure WriteDefault(const Section, Ident: string; Value: string); overload;
    procedure WriteDefault(const Section, Ident: string; Value: Integer); overload;
    procedure WriteDefault(const Section, Ident: string; Value: Boolean); overload;
  end;

  TConfigEntry = record
  private
    function GetKey: string;
  public
    Name: string;
    Description: StringList;
    Section: string;
    Default: string;
    IsEncrypted: Boolean;
    property Key: string read GetKey;
    procedure AssignDescription(AProperty: TRttiProperty);
    constructor Create(const AName: string);
  end;

  TConfigItems = class(TThreadList<TConfigEntry>)
  public
    function Get(const APropertyName: string): TConfigEntry;
  end;

  TConfigRegistry = class(TSingleton<TConfigItems>)
  end;

  ConfigDirectoryAttribute = class(StringValueAttribute)
  public
    constructor Create(const ADirectory: string);
  end;

  ConfigFileAttribute = class(StringValueAttribute)
  public
    constructor Create(const AFilename: string);
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

  ConfigValueEncryptedAttribute = class(TCustomAttribute)
  end;

  TConfigurationManager<T: class> = class abstract(TInterfacedSingleton<T>, IConfiguration)
  strict private
    FEncryptionKey: string;
  private
    FStorage: TMemIniFile;
    FStorageFile: string;
    FDescription: StringList;
    procedure AddComments(
      var AStrings: StringList;
      const AComments: StringList);
    function GetEncoding: TEncoding;
  strict protected
    procedure SetEncoding(const Value: TEncoding);
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

    procedure SetConfigValueForProperty(
      const AProperty: string;
      AValue: Variant);
    procedure WriteDescription;
    function Encrypt(const AText: string): string;
    function Decrypt(const AEncryptedText: string): string;
    function GetConfiguration: TCustomIniFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteStorage; deprecated 'Use method Save()';
    procedure Load; virtual;
    procedure Save; virtual;
    property EncryptionKey: string write FEncryptionKey;
    property Filename: string read FStorageFile;
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property Configuration: TCustomIniFile read GetConfiguration;
  end;

implementation

uses
  System.IOUtils, Data.DBXEncryption, System.NetEncoding, DX.Utils.Rtti;

constructor ConfigValueAttribute.Create(const ASection, ADefault: string);
begin
  inherited Create;
  FSection := ASection;
  FDefault := ADefault;
end;

procedure TConfigurationManager<T>.AddComments(
  var AStrings: StringList;
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
  LContent: TBytes;
  LEncoding: TEncoding;
  LConfigDirectory: string;
begin
  inherited;
  LEncoding := nil;
  FEncryptionKey := 'DeveloperExperts2020';

  // ConfigFile given?
  if self.HasAttribute(ConfigFileAttribute) then
  begin
    FStorageFile := self.AttributeValue(ConfigFileAttribute);
  end
  else
  begin
    // Default: AppName.ini
    FStorageFile := TPath.GetFileNameWithoutExtension(ParamStr(0)) + '.ini';
  end;

  // ConfigDirectory given?
  if self.HasAttribute(ConfigDirectoryAttribute) then
  begin
    LConfigDirectory := self.AttributeValue(ConfigDirectoryAttribute);
  end
  else
  begin
    // Full path given in ConfigFile?
    LConfigDirectory := TPath.GetDirectoryName(FStorageFile);
  end;

  if not TDirectory.Exists(LConfigDirectory) then
  begin
    LConfigDirectory := TPath.GetLibraryPath;
  end;

  Assert(TDirectory.Exists(LConfigDirectory));

  FStorageFile := TPath.Combine(LConfigDirectory, TPath.GetFileName(FStorageFile));
  DXLog('Configuration location' + Filename);
  Load;
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
  LConfig: StringList;
  LValue: string;
  LContent: TBytes;
begin
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
      LConfigItem.Name := '';
      LConfigItem.IsEncrypted := false;
      for LAttribute in LAttributes do
      begin
        if LAttribute is ConfigValueAttribute then
        begin
          LConfigItem.Name := LProperty.Name;
          LConfigItem.Section := (LAttribute as ConfigValueAttribute).FSection;
          LConfigItem.Default := (LAttribute as ConfigValueAttribute).FDefault;
          LConfigItem.AssignDescription(LProperty);
        end;
        if LAttribute is ConfigValueEncryptedAttribute then
        begin
          LConfigItem.IsEncrypted := true;
        end;
      end;
      if LConfigItem.Name > '' then
      begin
        if not FStorage.ValueExists(LConfigItem.Section, LConfigItem.Name) then
        begin
          LValue := LConfigItem.Default;
          if LConfigItem.IsEncrypted then
          begin
            LValue := Encrypt(LValue);
          end;
          FStorage.WriteString(LConfigItem.Section, LConfigItem.Name, LValue);
        end;
        TConfigRegistry.Default.Add(LConfigItem);
      end;
    end;
  finally
    LContext.Free;
  end;
  if FStorage.Modified then
  begin
    Save;
  end;
end;

destructor TConfigurationManager<T>.Destroy;
begin
  FreeAndNil(FStorage);
  inherited;
end;

function TConfigurationManager<T>.Encrypt(const AText: string): string;
var
  LCipher: TPC1Cypher;
  LEncrypted: TBytes;
  LUnencrypted: TBytes;
  i: Integer;
begin
  LCipher := TPC1Cypher.Create(FEncryptionKey);
  try
    LUnencrypted := TEncoding.UTF8.GetBytes(AText);
    SetLength(LEncrypted, Length(LUnencrypted));
    for i := 0 to Length(LUnencrypted) - 1 do
    begin
      LEncrypted[i] := LCipher.Cypher(LUnencrypted[i]);
    end;
    result := TNetEncoding.Base64.EncodeBytesToString(LEncrypted);
  finally
    FreeAndNil(LCipher);
  end;
end;

function TConfigurationManager<T>.Decrypt(const AEncryptedText: string): string;
var
  LCipher: TPC1Cypher;
  LEncrypted: TBytes;
  LUnencrypted: TBytes;
  i: Integer;
begin
  if AEncryptedText.Trim = '' then
  begin
    result := '';
  end
  else
  begin
    LCipher := TPC1Cypher.Create(FEncryptionKey);
    try
      LEncrypted := TNetEncoding.Base64.DecodeStringToBytes(AEncryptedText);
      SetLength(LUnencrypted, Length(LEncrypted));
      for i := 0 to Length(LEncrypted) - 1 do
      begin
        LUnencrypted[i] := LCipher.Decypher(LEncrypted[i]);
      end;
      result := TEncoding.UTF8.GetString(LUnencrypted)
    finally
      FreeAndNil(LCipher);
    end;
  end;
end;

procedure TConfigurationManager<T>.Save;
begin
  FStorage.UpdateFile;
  WriteDescription;
end;

procedure TConfigurationManager<T>.SetConfigValueForProperty(
  const AProperty: string;
  AValue: Variant);
var
  S: string;
  LConfigItem: TConfigEntry;
begin
  try
    LConfigItem := TConfigRegistry.Default.Get(AProperty);
    S := AValue;
    if LConfigItem.IsEncrypted then
    begin
      S := Encrypt(S);
    end;
    FStorage.WriteString(LConfigItem.Section, LConfigItem.Name, S);
  except
    on e: Exception do
    begin
      raise Exception.CreateFmt('Error writing config value for "%s"'#13#10'%s', [AProperty, e.message]);
    end;
  end;
end;

procedure TConfigurationManager<T>.SetEncoding(const Value: TEncoding);
begin
  FStorage.Encoding := Value;
end;

function TConfigurationManager<T>.GetConfiguration: TCustomIniFile;
begin
  result := FStorage
end;

function TConfigurationManager<T>.GetConfigValueForProperty(const AProperty: string): Variant;
var
  S: string;
  LConfigItem: TConfigEntry;
begin
  try
    LConfigItem := TConfigRegistry.Default.Get(AProperty);
    S := FStorage.ReadString(LConfigItem.Section, LConfigItem.Name, LConfigItem.Default);
    if (S <> LConfigItem.Default) and LConfigItem.IsEncrypted then
    begin
      S := Decrypt(S);
    end;
    result := S;
  except
    on e: Exception do
    begin
      raise Exception.CreateFmt('Error reading config value for "%s"'#13#10'%s', [AProperty, e.message]);
    end;
  end;
end;

function TConfigurationManager<T>.GetConfigValueForPropertyAsParams(const AProperty: string): StringList;
var
  LStrings: TStrings;
  S: string;
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

function TConfigurationManager<T>.GetEncoding: TEncoding;
begin
  result := FStorage.Encoding;
end;

procedure TConfigurationManager<T>.Load;
var
  LEncoding: TEncoding;
begin
  LEncoding := nil;
  if TFile.Exists(FStorageFile) then
  begin
    var LContent := TFile.ReadAllBytes(Filename);
    TEncoding.GetBufferEncoding(LContent, LEncoding);
  end
  else
  begin
    LEncoding := TEncoding.UTF8;
  end;

  FStorage.Free;
  FStorage := TMemIniFile.Create(FStorageFile, LEncoding);
end;

procedure TConfigurationManager<T>.WriteStorage;
begin
  Save;
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
  // If the description is updated, then the file is ultimately updated to UTF8 Encoding
  if not LContent.IsEmpty then
  begin
    LContent.AddStrings(TFile.ReadAllLines(FStorageFile, Encoding));
    TFile.WriteAllLines(FStorageFile, LContent, TEncoding.UTF8);
    Encoding := TEncoding.UTF8;
  end;
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
    result := Default(TConfigEntry);
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

constructor TConfigEntry.Create(const AName: string);
begin
  Name := AName;
  IsEncrypted := false;
  Description.Clear;
  Default := '';
  Section := '';
end;

function TConfigEntry.GetKey: string;
begin
  result := Section.Trim.ToLower + '/' + Name.Trim.ToLower;
end;

{ ConfigFileAttribute }

constructor ConfigFileAttribute.Create(const AFilename: string);
begin
  inherited Create(AFilename);
end;

{ TIniFileHelper }

procedure TIniFileHelper.WriteDefault(const Section, Ident: string; Value: string);
begin
  if not ValueExists(Section, Ident) then
  begin
    WriteString(Section, Ident, Value);
  end;
end;

procedure TIniFileHelper.WriteDefault(const Section, Ident: string; Value: Boolean);
const
  // Same implementation as in TIniFile!
  Values: array[Boolean] of string = ('0', '1');
begin
  WriteDefault(Section, Ident, Values[Ord(Value) <> 0]);
end;

procedure TIniFileHelper.WriteDefault(const Section, Ident: string; Value: Integer);
begin
  WriteDefault(Section, Ident, Value.ToString);
end;

{ ConfigDirectoryAttribute }

constructor ConfigDirectoryAttribute.Create(const ADirectory: string);
begin
  inherited Create(ADirectory);
end;

end.

