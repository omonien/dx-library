unit Tests.ConfigurationAnsi;

interface

uses
  DUnitX.TestFramework, DX.Lib.Configuration, System.Classes, System.SysUtils;

const
  CONFIG_SECTION = 'test';
  CONFIG_VALUE = 'Some value üöä';
  CONFIG_FILE = 'FoobarAnsi.ini';

type

  [ConfigDescription('Foo Configuration')]
  [ConfigFile(CONFIG_FILE)]
  TFooConfigAnsi = class(TConfigurationManager<TFooConfigAnsi>)
  private
    function GetFoobar: string;
    procedure SetFoobar(const Value: string);
  public
    [ConfigValue(CONFIG_SECTION, CONFIG_VALUE)]
    property Foobar: string read GetFoobar write SetFoobar;

  end;

  [TestFixture]
  TConfigTests = class(TObject)
  private
    FConfigFilename: string;
    FConfig: TFooConfigAnsi;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure FileName;

    [Test]
    procedure Encoding;
  end;

implementation

uses
  System.IOUtils, DX.Classes.Strings;

procedure TConfigTests.Encoding;
var
  LFile: StringList;
begin

 //We start with an existing ANSI file. During setup, a default descriptio n will be written to the config, which converts the
 //file to UTF8
  Assert.AreEqual(TEncoding.UTF8, FConfig.Encoding);
  Assert.AreEqual(CONFIG_VALUE, FConfig.Foobar);

  FConfig.Foobar := 'Umlauts: ÜÄÖß';
  FConfig.WriteStorage;

  LFile := TFile.ReadAllLines(FConfig.FileName, FConfig.Encoding);
  Assert.AreEqual('Foobar=Umlauts: ÜÄÖß', LFile[2]);

end;

procedure TConfigTests.FileName;
begin
  Assert.AreEqual(FConfigFilename, FConfig.FileName);
end;

procedure TConfigTests.Setup;
var
  LContent: StringList;
  LEncoding: TEncoding;
begin
  FConfigFilename := Tpath.Combine(Tpath.GetLibraryPath, CONFIG_FILE);
  if TFile.Exists(FConfigFilename) then
  begin
    TFile.Delete(FConfigFilename);
  end;
  LContent.Add(CONFIG_SECTION);
  LContent.Add('Test=' + CONFIG_VALUE);
//Make sure we are using ANSI / Western-Europe code page, so that umlauts are available
  LEncoding := TMBCSEncoding.Create(1252, 0, 0);
  Assert.IsTrue(LEncoding.IsSingleByte);
  TFile.WriteAllLines(FConfigFilename, LContent, LEncoding);
  FConfig := TFooConfigAnsi.Create;
end;

procedure TConfigTests.TearDown;
begin
  FreeAndNil(FConfig);
end;

function TFooConfigAnsi.GetFoobar: string;
begin
  Result := GetConfigValueForProperty('Foobar');
end;

procedure TFooConfigAnsi.SetFoobar(const Value: string);
begin
  SetConfigValueForProperty('Foobar', Value);
end;

initialization

TDUnitX.RegisterTestFixture(TConfigTests);

end.
