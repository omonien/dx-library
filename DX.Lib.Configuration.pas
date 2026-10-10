unit DX.Lib.Configuration;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles, System.Rtti, System.Generics.Collections,
  DX.Classes.Singleton, DX.Classes.Strings, DX.Classes.Attributes, DX.Lib.Configuration.Intf, DX.Utils.Logger;

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
    HasConnectionStringEditor: Boolean;
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

  ConnectionStringEditorAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  /// INI storage of TConfigurationManager: a TMemIniFile that handles quoted values like the
  /// Windows profile API (GetPrivateProfileString), which TIniFile uses.
  /// </summary>
  /// <remarks>
  /// A plain TMemIniFile returns a value verbatim: Password="secret" would reach the application
  /// including its quotes, while TIniFile returns secret. ReadString therefore removes one pair of
  /// matching enclosing quotes ("..." or '...'); inner and unpaired quotes are data. WriteString
  /// stores a value in double quotes only if it would otherwise not come back unchanged after a
  /// save and reload (surrounding whitespace, or the value is itself enclosed in matching quotes);
  /// all other values are stored verbatim, so existing files keep their format. Typed reads
  /// (ReadInteger, ReadBool, ...) go through ReadString and see the unquoted value, too.
  /// ReadSectionValues still returns the stored form. Line breaks inside values remain
  /// unsupported, as with any INI file.
  /// </remarks>
  TConfigIniFile = class(TMemIniFile)
  public
    /// <summary>
    /// Returns ARawValue the way GetPrivateProfileString returns an INI value: surrounding
    /// whitespace (characters up to #32, as GetPrivateProfileString and TMemIniFile trim it)
    /// removed, then exactly one pair of matching enclosing quotes removed.
    /// </summary>
    class function UnquoteValue(const ARawValue: string): string; static;
    /// <summary>
    /// Returns the form in which AValue (without line breaks) has to be stored so that reading
    /// it back - from memory or after a save and reload - returns AValue unchanged.
    /// </summary>
    class function QuoteValue(const AValue: string): string; static;
    /// <summary>
    /// Like TMemIniFile.ReadString, but stored values are unquoted (see UnquoteValue).
    /// ADefault is returned unchanged - deliberately unlike GetPrivateProfileString, which strips
    /// its trailing blanks - because TConfigurationManager compares the result against it.
    /// A key line without "=" counts as missing, as in TMemIniFile and GetPrivateProfileString.
    /// </summary>
    function ReadString(const ASection, AIdent, ADefault: string): string; override;
    /// <summary>
    /// Like TMemIniFile.WriteString, but stores AValue in the form returned by QuoteValue.
    /// </summary>
    procedure WriteString(const ASection, AIdent, AValue: string); override;
  end;

  TConfigurationManager<T: class> = class abstract(TInterfacedSingleton<T>, IConfiguration)
  strict private
    FEncryptionKey: string;
  private
    class var FConcreteClass: TClass;
    var FStorage: TConfigIniFile;
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
    /// Property's Getter methods only; <br />
    /// A value enclosed in matching quotes in the config file is returned
    /// without them, as TIniFile does (see TConfigIniFile).
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
    class procedure RegisterConcreteClass(AClass: TClass);
    class function Default: T;
    class function Instance: T;
    property EncryptionKey: string write FEncryptionKey;
    property Filename: string read FStorageFile;
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property Configuration: TCustomIniFile read GetConfiguration;
  end;

implementation

uses
  System.IOUtils, Data.DBXEncryption, System.NetEncoding, DX.Utils.Rtti;

class procedure TConfigurationManager<T>.RegisterConcreteClass(AClass: TClass);
begin
  FConcreteClass := AClass;
  // Wenn der Singleton bereits mit dem falschen (Basis-)Typ erstellt wurde
  // (Race-Condition bei Initialisierungsreihenfolge), zurücksetzen damit
  // er beim nächsten Default-Aufruf korrekt als konkreter Typ erstellt wird.
  if Assigned(FDefaultInstance) and (FDefaultInstance.ClassType <> AClass) then
    FreeAndNil(FDefaultInstance);
end;

class function TConfigurationManager<T>.Default: T;
begin
  if not Assigned(FDefaultInstance) then
  begin
    if Assigned(FConcreteClass) then
      FDefaultInstance := T(TClassConstructor.ConstructClass(FConcreteClass))
    else
      FDefaultInstance := TClassConstructor.Construct<T>;
  end;
  Result := FDefaultInstance;
end;

class function TConfigurationManager<T>.Instance: T;
begin
  Result := Default;
end;

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
  LConfigDirectory: string;
begin
  inherited;
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
  LProperty: TRttiProperty;
  LAttributes: TArray<TCustomAttribute>;
  LAttribute: TCustomAttribute;
  LConfigItem: TConfigEntry;
  LValue: string;
  LConfigItems: TDictionary<string, TConfigEntry>;
  LKey: string;
  LPair: TPair<string, TConfigEntry>;
  LHierarchy: TList<TRttiType>;
  LCurrentType: TRttiType;
  i: Integer;
begin
  LContext := TRttiContext.Create;
  LConfigItems := TDictionary<string, TConfigEntry>.Create;
  LHierarchy := TList<TRttiType>.Create;
  try
    LConfigType := LContext.GetType(Self.ClassType);

    LAttributes := LConfigType.GetAttributes;
    for LAttribute in LAttributes do
    begin
      if LAttribute is ConfigDescriptionAttribute then
      begin
        // Main description
        FDescription.Add((LAttribute as ConfigDescriptionAttribute).FText);
      end;

    end;

    // Build class hierarchy from base to most-derived
    LCurrentType := LConfigType;
    while LCurrentType <> nil do
    begin
      LHierarchy.Insert(0, LCurrentType);
      LCurrentType := LCurrentType.BaseType;
    end;

    // Walk hierarchy from base to derived, using GetDeclaredProperties at each level.
    // AddOrSetValue ensures that derived-class defaults override base-class defaults.
    for i := 0 to LHierarchy.Count - 1 do
    begin
      for LProperty in LHierarchy[i].GetDeclaredProperties do
      begin
        LAttributes := LProperty.GetAttributes;
        LConfigItem.Name := '';
        LConfigItem.IsEncrypted := false;
        LConfigItem.HasConnectionStringEditor := false;
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
          if LAttribute is ConnectionStringEditorAttribute then
          begin
            LConfigItem.HasConnectionStringEditor := true;
          end;
        end;
        if LConfigItem.Name > '' then
        begin
          LKey := LConfigItem.Section + '.' + LConfigItem.Name;
          LConfigItems.AddOrSetValue(LKey, LConfigItem);
        end;
      end;
    end;

    // Second pass: write defaults and register entries
    TConfigRegistry.Default.Clear;
    for LPair in LConfigItems do
    begin
      LConfigItem := LPair.Value;
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
  finally
    FreeAndNil(LHierarchy);
    FreeAndNil(LConfigItems);
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
    // TNetEncoding.Base64 inserts a line break every 76 characters. Since the INI
    // storage is line-based (TMemIniFile), a wrapped value used to split the "Key="
    // entry across several physical lines and corrupted the file on the next load.
    // TNetEncoding.Base64String does not wrap, so the stored value always stays on
    // one line. Decode is unaffected by this change: values that were short enough
    // to never wrap (<= 57 bytes) decode exactly as before; longer, already-wrapped
    // values written by the old code were already corrupted on disk and cannot be
    // recovered here.
    result := TNetEncoding.Base64String.EncodeBytesToString(LEncrypted);
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
  FStorage := TConfigIniFile.Create(FStorageFile, LEncoding);
end;

procedure TConfigurationManager<T>.WriteStorage;
begin
  Save;
end;

procedure TConfigurationManager<T>.WriteDescription;
var
  LLines: TArray<string>;
  LResult: StringList;
  LConfigItems: TList<TConfigEntry>;
  LConfigItem: TConfigEntry;
  LDescMap: TDictionary<string, StringList>;
  LCurrentSection: string;
  LLine, LTrimmed, LKeyName: string;
  LEqPos: Integer;
  LDescLines: StringList;
begin
  // Beschreibungs-Map aufbauen: "section/key" (lowercase) -> Description
  LDescMap := TDictionary<string, StringList>.Create;
  try
    LConfigItems := TConfigRegistry.Default.LockList;
    try
      for LConfigItem in LConfigItems do
      begin
        if not LConfigItem.Description.IsEmpty then
          LDescMap.AddOrSetValue(
            LConfigItem.Section.ToLower + '/' + LConfigItem.Name.ToLower,
            LConfigItem.Description);
      end;
    finally
      TConfigRegistry.Default.UnlockList;
    end;

    // INI-Datei lesen
    LLines := TFile.ReadAllLines(FStorageFile, Encoding);
    LResult.Clear;

    // Haupt-Beschreibung oben einfuegen
    AddComments(LResult, FDescription);
    if not FDescription.IsEmpty then
      LResult.Add('');

    LCurrentSection := '';
    for LLine in LLines do
    begin
      LTrimmed := LLine.Trim;

      // Bestehende Kommentarzeilen ueberspringen (werden neu generiert)
      if LTrimmed.StartsWith('#') then
        Continue;

      // Leerzeilen ueberspringen (werden neu generiert)
      if LTrimmed = '' then
        Continue;

      // Section-Header
      if LTrimmed.StartsWith('[') and LTrimmed.EndsWith(']') then
      begin
        if not LResult.IsEmpty then
          LResult.Add('');
        LCurrentSection := Copy(LTrimmed, 2, Length(LTrimmed) - 2);
        LResult.Add(LLine);
        Continue;
      end;

      // Key=Value Zeile: Beschreibung davor einfuegen
      LEqPos := Pos('=', LTrimmed);
      if (LEqPos > 0) and (LCurrentSection <> '') then
      begin
        LKeyName := Copy(LTrimmed, 1, LEqPos - 1).Trim;
        if LDescMap.TryGetValue(LCurrentSection.ToLower + '/' + LKeyName.ToLower, LDescLines) then
          AddComments(LResult, LDescLines);
      end;
      LResult.Add(LLine);
    end;

    // Zurueckschreiben (immer UTF8)
    TFile.WriteAllLines(FStorageFile, LResult, TEncoding.UTF8);
    Encoding := TEncoding.UTF8;
  finally
    FreeAndNil(LDescMap);
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

{ TConfigIniFile }

class function TConfigIniFile.UnquoteValue(const ARawValue: string): string;
begin
  // Mirrors GetPrivateProfileString (checked against Windows 11): whitespace around the value is
  // not part of it, then a single pair of matching enclosing quotes is removed - "a"b" becomes
  // a"b, ""a"" becomes "a". A lone quote character and unpaired or mismatched quotes stay.
  Result := ARawValue.Trim;
  if (Result.Length >= 2) and CharInSet(Result.Chars[0], ['"', '''']) and
    (Result.Chars[Result.Length - 1] = Result.Chars[0]) then
  begin
    Result := Result.Substring(1, Result.Length - 2);
  end;
end;

class function TConfigIniFile.QuoteValue(const AValue: string): string;
begin
  // Quote only where needed, so ordinary values keep their file format. A value in double quotes
  // always comes back unchanged: UnquoteValue removes exactly that pair, and TMemIniFile's line
  // trimming on load cannot reach past the quotes.
  if UnquoteValue(AValue) = AValue then
    Result := AValue
  else
    Result := '"' + AValue + '"';
end;

function TConfigIniFile.ReadString(const ASection, AIdent, ADefault: string): string;
const
  // Marks "no stored value" (key missing, or a line without "="). A stored value can never be
  // a lone #0: TMemIniFile trims every line on load, and WriteString would quote it.
  cNoValue = #0;
var
  LRawValue: string;
begin
  LRawValue := inherited ReadString(ASection, AIdent, cNoValue);
  if LRawValue = cNoValue then
    Result := ADefault
  else
    Result := UnquoteValue(LRawValue);
end;

procedure TConfigIniFile.WriteString(const ASection, AIdent, AValue: string);
begin
  inherited WriteString(ASection, AIdent, QuoteValue(AValue));
end;

end.

