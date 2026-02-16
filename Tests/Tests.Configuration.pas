unit Tests.Configuration;

interface

uses
  DUnitX.TestFramework, DX.Lib.Configuration, System.Classes, System.SysUtils;

const
  CONFIG_SECTION = 'test';
  CONFIG_VALUE = 'Some value üöä';
  CONFIG_FILE = 'Foobar.ini';

type

  [ConfigDescription('Foo Configuration')]
  [ConfigFile(CONFIG_FILE)]
  TFooConfig = class(TConfigurationManager<TFooConfig>)
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

  Assert.AreEqual(TEncoding.UTF8, TFooConfig.Default.Encoding);
  Assert.AreEqual(CONFIG_VALUE, TFooConfig.Default.Foobar);

  TFooConfig.Default.Foobar := 'Привет мир';
  TFooConfig.Default.WriteStorage;

  LFile := TFile.ReadAllLines(TFooConfig.Default.FileName, TFooConfig.Default.Encoding);
  Assert.AreEqual('Foobar=Привет мир', LFile[2]);

end;

procedure TConfigTests.FileName;
begin
  Assert.AreEqual(FConfigFilename, TFooConfig.Default.FileName);
end;

procedure TConfigTests.Setup;
begin
  FConfigFilename := Tpath.Combine(Tpath.GetLibraryPath, CONFIG_FILE);
  if TFile.Exists(FConfigFilename) then
  begin
    TFile.Delete(FConfigFilename);
  end;
end;

procedure TConfigTests.TearDown;
begin
end;

function TFooConfig.GetFoobar: string;
begin
  Result := GetConfigValueForProperty('Foobar');
end;

procedure TFooConfig.SetFoobar(const Value: string);
begin
  SetConfigValueForProperty('Foobar', Value);
end;

initialization

TDUnitX.RegisterTestFixture(TConfigTests);

end.
