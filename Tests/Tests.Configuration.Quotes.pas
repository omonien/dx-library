/// <summary>
/// Tests.Configuration.Quotes
/// DUnitX tests for quoted INI values in TConfigurationManager.
/// </summary>
///
/// <remarks>
/// TMemIniFile returns a value verbatim, including enclosing quotes, while the Windows
/// profile API (GetPrivateProfileString, used by TIniFile) removes one pair of matching
/// enclosing quotes and ignores surrounding whitespace. TConfigurationManager has to behave
/// like the Windows API, and values written by it must survive a save/reload cycle unchanged.
/// The expected values of the read tests were verified against GetPrivateProfileStringW
/// on Windows 11 (10.0.26100). One deliberate exception: the default of a missing key is
/// returned unchanged, while the Windows API strips its trailing blanks.
/// </remarks>
///
/// <copyright>
/// Copyright © 2026 Olaf Monien
/// Licensed under MIT
/// </copyright>

unit Tests.Configuration.Quotes;

interface

uses
  DUnitX.TestFramework, DX.Lib.Configuration;

const
  cQuotesConfigFile = 'QuotesTest.ini';
  cQuotesSection = 'Quotes';
  cPaddedDefault = ' a ';

type

  /// <summary>
  /// Configuration read and written through TConfigurationManager.
  /// </summary>
  [ConfigFile(cQuotesConfigFile)]
  TQuotesConfig = class(TConfigurationManager<TQuotesConfig>)
  private
    function GetValue: string;
    procedure SetValue(const AValue: string);
    function GetPaddedDefault: string;
    function GetSecret: string;
    procedure SetSecret(const AValue: string);
  public
    [ConfigValue(cQuotesSection, '')]
    property Value: string read GetValue write SetValue;

    /// <summary>
    /// A default that only survives a reload if it is stored in quotes.
    /// </summary>
    [ConfigValue(cQuotesSection, cPaddedDefault)]
    property PaddedDefault: string read GetPaddedDefault;

    [ConfigValue(cQuotesSection, '')]
    [ConfigValueEncrypted]
    property Secret: string read GetSecret write SetSecret;
  end;

  [TestFixture]
  TConfigQuotesTests = class(TObject)
  private
    FConfigFilename: string;
    /// <summary>
    /// Writes the INI file with the given lines below the section header.
    /// </summary>
    procedure WriteIni(const ALines: string);
    /// <summary>
    /// Writes ARawValue verbatim as "Value=" line, loads the configuration and returns Value.
    /// </summary>
    function ReadFromIni(const ARawValue: string): string;
    /// <summary>
    /// Writes AValue through the configuration, saves, reloads and returns Value.
    /// </summary>
    function RoundTrip(const AValue: string): string;
    /// <summary>
    /// Writes AValue through the configuration and saves.
    /// </summary>
    procedure WriteValue(const AValue: string);
    /// <summary>
    /// The line of key AKey as stored in the INI file, or '' if there is none.
    /// </summary>
    function GetFileLine(const AKey: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('NoQuotes', 'elke-test|elke-test', '|')]
    [TestCase('DoubleQuoted', '"elke-test"|elke-test', '|')]
    [TestCase('SingleQuoted', '''elke-test''|elke-test', '|')]
    [TestCase('UnpairedLeading', '"elke-test|"elke-test', '|')]
    [TestCase('UnpairedTrailing', 'elke-test"|elke-test"', '|')]
    [TestCase('MismatchedPair', '"elke-test''|"elke-test''', '|')]
    [TestCase('InnerQuoteKept', '"a"b"|a"b', '|')]
    [TestCase('InnerQuotesOnly', 'a "b" c|a "b" c', '|')]
    [TestCase('QuotedThenText', '"a"b|"a"b', '|')]
    [TestCase('TwoQuotedParts', '"a" "b"|a" "b', '|')]
    [TestCase('TrailingQuotes', '"a"""|a""', '|')]
    [TestCase('EmptyDoubleQuoted', '""|', '|')]
    [TestCase('EmptySingleQuoted', '''''|', '|')]
    [TestCase('LoneDoubleQuote', '"|"', '|')]
    [TestCase('LoneSingleQuote', '''|''', '|')]
    [TestCase('ThreeDoubleQuotes', '"""|"', '|')]
    [TestCase('ThreeSingleQuotes', '''''''|''', '|')]
    [TestCase('OnlyOnePairRemoved', '""a""|"a"', '|')]
    [TestCase('OtherQuoteInside', '"''a''"|''a''', '|')]
    [TestCase('NoInlineComment', 'a ;comment|a ;comment', '|')]
    procedure ReadRemovesEnclosingQuotes(const ARawValue, AExpected: string);

    /// <summary>
    /// Whitespace around an unquoted value is not part of the value.
    /// </summary>
    [Test]
    procedure ReadTrimsOuterWhitespace;

    /// <summary>
    /// Quotes protect leading/trailing whitespace; whitespace around the quotes is ignored.
    /// </summary>
    [Test]
    procedure ReadKeepsWhitespaceInsideQuotes;

    /// <summary>
    /// Typed reads through Configuration (TCustomIniFile) see the unquoted value, as with TIniFile.
    /// </summary>
    [Test]
    procedure TypedReadsUnquote;

    /// <summary>
    /// The default of a missing key is returned unchanged - it is neither unquoted nor trimmed.
    /// </summary>
    [Test]
    procedure ReadReturnsDefaultUnchanged;

    /// <summary>
    /// An empty stored value is a value, not a missing key; a line without "=" is a missing key.
    /// </summary>
    [Test]
    procedure ReadDistinguishesEmptyFromMissing;

    /// <summary>
    /// Every value comes back unchanged after write, save and reload - including values with
    /// surrounding whitespace, control characters or enclosing quotes of their own.
    /// </summary>
    [Test]
    procedure RoundTripReturnsValueUnchanged;

    /// <summary>
    /// Values that survive a reload anyway are stored verbatim, so existing files keep their format.
    /// </summary>
    [Test]
    procedure WriteKeepsPlainValuesUnquoted;

    /// <summary>
    /// Values that would change on reload are stored in double quotes.
    /// </summary>
    [Test]
    procedure WriteQuotesValuesThatWouldChange;

    /// <summary>
    /// Defaults written into a new file follow the same rule and survive a reload.
    /// </summary>
    [Test]
    procedure DefaultSurvivesReload;

    /// <summary>
    /// An encrypted value that was wrapped in quotes by hand is still decrypted.
    /// </summary>
    [Test]
    procedure QuotedEncryptedValueIsDecrypted;
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

const
  cTab = #9;

{ TQuotesConfig }

function TQuotesConfig.GetValue: string;
begin
  Result := GetConfigValueForProperty('Value');
end;

procedure TQuotesConfig.SetValue(const AValue: string);
begin
  SetConfigValueForProperty('Value', AValue);
end;

function TQuotesConfig.GetPaddedDefault: string;
begin
  Result := GetConfigValueForProperty('PaddedDefault');
end;

function TQuotesConfig.GetSecret: string;
begin
  Result := GetConfigValueForProperty('Secret');
end;

procedure TQuotesConfig.SetSecret(const AValue: string);
begin
  SetConfigValueForProperty('Secret', AValue);
end;

{ TConfigQuotesTests }

procedure TConfigQuotesTests.Setup;
begin
  FConfigFilename := TPath.Combine(TPath.GetLibraryPath, cQuotesConfigFile);
  if TFile.Exists(FConfigFilename) then
    TFile.Delete(FConfigFilename);
end;

procedure TConfigQuotesTests.TearDown;
begin
  if TFile.Exists(FConfigFilename) then
    TFile.Delete(FConfigFilename);
end;

procedure TConfigQuotesTests.WriteIni(const ALines: string);
begin
  TFile.WriteAllText(FConfigFilename, '[' + cQuotesSection + ']' + sLineBreak + ALines + sLineBreak, TEncoding.UTF8);
end;

function TConfigQuotesTests.ReadFromIni(const ARawValue: string): string;
var
  LConfig: TQuotesConfig;
begin
  WriteIni('Value=' + ARawValue);
  LConfig := TQuotesConfig.Create;
  try
    Result := LConfig.Value;
  finally
    FreeAndNil(LConfig);
  end;
end;

function TConfigQuotesTests.RoundTrip(const AValue: string): string;
var
  LConfig: TQuotesConfig;
begin
  LConfig := TQuotesConfig.Create;
  try
    LConfig.Value := AValue;
    // Before the reload, the value comes from memory
    Assert.AreEqual(AValue, LConfig.Value, Format('In-memory read of [%s]', [AValue]));
    LConfig.Save;
  finally
    FreeAndNil(LConfig);
  end;
  LConfig := TQuotesConfig.Create;
  try
    Result := LConfig.Value;
  finally
    FreeAndNil(LConfig);
  end;
end;

procedure TConfigQuotesTests.WriteValue(const AValue: string);
var
  LConfig: TQuotesConfig;
begin
  LConfig := TQuotesConfig.Create;
  try
    LConfig.Value := AValue;
    LConfig.Save;
  finally
    FreeAndNil(LConfig);
  end;
end;

function TConfigQuotesTests.GetFileLine(const AKey: string): string;
var
  LLine: string;
begin
  Result := '';
  for LLine in TFile.ReadAllLines(FConfigFilename, TEncoding.UTF8) do
  begin
    if LLine.StartsWith(AKey + '=') then
      Exit(LLine);
  end;
end;

procedure TConfigQuotesTests.ReadRemovesEnclosingQuotes(const ARawValue, AExpected: string);
begin
  Assert.AreEqual(AExpected, ReadFromIni(ARawValue));
end;

procedure TConfigQuotesTests.ReadTrimsOuterWhitespace;
begin
  Assert.AreEqual('a  b', ReadFromIni('   a  b   '));
  Assert.AreEqual('a', ReadFromIni(cTab + 'a' + cTab));
  Assert.AreEqual('a', ReadFromIni('"a" '));
  Assert.AreEqual('a', ReadFromIni(' "a"'));
end;

procedure TConfigQuotesTests.ReadKeepsWhitespaceInsideQuotes;
begin
  Assert.AreEqual('  a  ', ReadFromIni('"  a  "'));
  Assert.AreEqual('  a  ', ReadFromIni('   "  a  "   '));
  Assert.AreEqual(' a ', ReadFromIni(''' a '''));
  Assert.AreEqual(cTab + 'a' + cTab, ReadFromIni(cTab + '"' + cTab + 'a' + cTab + '"' + cTab));
  Assert.AreEqual(' ', ReadFromIni('" "'));
  Assert.AreEqual('  ', ReadFromIni('"  "'));
end;

procedure TConfigQuotesTests.TypedReadsUnquote;
var
  LConfig: TQuotesConfig;
begin
  WriteIni('Value=x' + sLineBreak + 'Number="42"' + sLineBreak + 'Flag=''1''');
  LConfig := TQuotesConfig.Create;
  try
    Assert.AreEqual(42, LConfig.Configuration.ReadInteger(cQuotesSection, 'Number', 0));
    Assert.IsTrue(LConfig.Configuration.ReadBool(cQuotesSection, 'Flag', False));
    Assert.AreEqual('42', LConfig.Configuration.ReadString(cQuotesSection, 'Number', ''));
  finally
    FreeAndNil(LConfig);
  end;
end;

procedure TConfigQuotesTests.ReadReturnsDefaultUnchanged;
var
  LConfig: TQuotesConfig;
begin
  WriteIni('Value=x');
  LConfig := TQuotesConfig.Create;
  try
    Assert.AreEqual('"d"', LConfig.Configuration.ReadString(cQuotesSection, 'Missing', '"d"'));
    Assert.AreEqual(' d ', LConfig.Configuration.ReadString(cQuotesSection, 'Missing', ' d '));
  finally
    FreeAndNil(LConfig);
  end;
end;

procedure TConfigQuotesTests.ReadDistinguishesEmptyFromMissing;
var
  LConfig: TQuotesConfig;
begin
  WriteIni('Value=x' + sLineBreak + 'Empty=' + sLineBreak + 'EmptyQuoted=""' + sLineBreak + 'NoEquals');
  LConfig := TQuotesConfig.Create;
  try
    Assert.AreEqual('', LConfig.Configuration.ReadString(cQuotesSection, 'Empty', 'd'), 'Empty=');
    Assert.AreEqual('', LConfig.Configuration.ReadString(cQuotesSection, 'EmptyQuoted', 'd'), 'EmptyQuoted=""');
    Assert.AreEqual('d', LConfig.Configuration.ReadString(cQuotesSection, 'NoEquals', 'd'), 'line without "="');
  finally
    FreeAndNil(LConfig);
  end;
end;

procedure TConfigQuotesTests.RoundTripReturnsValueUnchanged;
const
  cValues: TArray<string> = [
    'elke-test', '', ' ', '  ', '  a  ', ' a', 'a ', cTab + 'a' + cTab, cTab, #0, #1'a', 'a'#31,
    '"a"', '''a''', '"a', 'a"', '"a''', '"a"b"', 'a "b" c', '""', '''''', '"', '''',
    ' "a" ', '""a""', '"  a  "', '"a" "b"', 'C:\Program Files\ELKE\cert.pfx'];
var
  LValue: string;
begin
  for LValue in cValues do
  begin
    Assert.AreEqual(LValue, RoundTrip(LValue), Format('Roundtrip of [%s]', [LValue]));
  end;
end;

procedure TConfigQuotesTests.WriteKeepsPlainValuesUnquoted;
const
  cValues: TArray<string> = ['elke-test', 'a "b" c', '"a', 'a''', 'C:\Program Files\ELKE\cert.pfx', ''];
var
  LValue: string;
begin
  for LValue in cValues do
  begin
    WriteValue(LValue);
    Assert.AreEqual('Value=' + LValue, GetFileLine('Value'));
  end;
end;

procedure TConfigQuotesTests.WriteQuotesValuesThatWouldChange;
begin
  WriteValue(' a ');
  Assert.AreEqual('Value=" a "', GetFileLine('Value'));
  WriteValue('"a"');
  Assert.AreEqual('Value=""a""', GetFileLine('Value'));
  WriteValue('''a''');
  Assert.AreEqual('Value="''a''"', GetFileLine('Value'));
  WriteValue('""');
  Assert.AreEqual('Value=""""', GetFileLine('Value'));
end;

procedure TConfigQuotesTests.DefaultSurvivesReload;
var
  LConfig: TQuotesConfig;
begin
  // No file yet: the constructor writes all defaults
  LConfig := TQuotesConfig.Create;
  try
    Assert.AreEqual(cPaddedDefault, LConfig.PaddedDefault, 'before reload');
  finally
    FreeAndNil(LConfig);
  end;
  Assert.AreEqual('PaddedDefault="' + cPaddedDefault + '"', GetFileLine('PaddedDefault'));
  LConfig := TQuotesConfig.Create;
  try
    Assert.AreEqual(cPaddedDefault, LConfig.PaddedDefault, 'after reload');
  finally
    FreeAndNil(LConfig);
  end;
end;

procedure TConfigQuotesTests.QuotedEncryptedValueIsDecrypted;
var
  LConfig: TQuotesConfig;
  LEncrypted: string;
begin
  LConfig := TQuotesConfig.Create;
  try
    LConfig.Secret := 'elke-test';
    LConfig.Save;
  finally
    FreeAndNil(LConfig);
  end;
  LEncrypted := GetFileLine('Secret').Substring(Length('Secret='));
  Assert.IsNotEmpty(LEncrypted);
  Assert.AreNotEqual('elke-test', LEncrypted, 'stored encrypted');

  // An administrator wraps the stored value in quotes by hand
  WriteIni('Value=x' + sLineBreak + 'Secret="' + LEncrypted + '"');
  LConfig := TQuotesConfig.Create;
  try
    Assert.AreEqual('elke-test', LConfig.Secret);
  finally
    FreeAndNil(LConfig);
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TConfigQuotesTests);

end.
