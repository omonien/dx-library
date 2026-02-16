unit Tests.Configuration.Subclass;

interface

uses
  DUnitX.TestFramework, DX.Classes.Configuration, System.Classes, System.SysUtils;

const
  BASE_SECTION = 'General';
  BASE_SHARED_DEFAULT = 'base-shared';
  BASE_ONLY_DEFAULT = 'base-only-value';
  SUB_SHARED_DEFAULT = 'sub-shared-override';
  SUB_ONLY_DEFAULT = 'sub-only-value';
  SUB_CONFIG_FILE = 'SubclassTest.ini';

type

  /// <summary>
  /// Base configuration class for subclass tests.
  /// </summary>
  [ConfigFile(SUB_CONFIG_FILE)]
  TTestBaseConfig = class(TConfigurationManager<TTestBaseConfig>)
  protected
    function GetSharedProp: string;
    function GetBaseOnly: string;
    function GetSubOnly: string;
  public
    [ConfigValue(BASE_SECTION, BASE_SHARED_DEFAULT)]
    property SharedProp: string read GetSharedProp;

    [ConfigValue(BASE_SECTION, BASE_ONLY_DEFAULT)]
    property BaseOnly: string read GetBaseOnly;
  end;

  /// <summary>
  /// Subclass configuration that overrides SharedProp default and adds SubOnly.
  /// </summary>
  [ConfigFile(SUB_CONFIG_FILE)]
  TTestSubConfig = class(TTestBaseConfig)
  public
    class function Default: TTestSubConfig; reintroduce;

    [ConfigValue(BASE_SECTION, SUB_SHARED_DEFAULT)]
    property SharedProp: string read GetSharedProp;

    [ConfigValue(BASE_SECTION, SUB_ONLY_DEFAULT)]
    property SubOnly: string read GetSubOnly;
  end;

  [TestFixture]
  TConfigSubclassTests = class(TObject)
  private
    FConfigFilename: string;
    procedure CleanUp;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    /// RegisterConcreteClass + Default creates the correct subclass type
    procedure DefaultCreatesSubclassInstance;

    [Test]
    /// Typed Default returns the correct subclass type
    procedure TypedDefaultReturnsSubclass;

    [Test]
    /// Subclass property default overrides base property default
    procedure SubclassDefaultOverridesBase;

    [Test]
    /// Base-only properties retain their defaults
    procedure BaseOnlyPropertyRetainsDefault;

    [Test]
    /// Sub-only properties are accessible and have correct defaults
    procedure SubOnlyPropertyHasDefault;
  end;

implementation

uses
  System.IOUtils;

{ TTestBaseConfig }

function TTestBaseConfig.GetSharedProp: string;
begin
  Result := GetConfigValueForProperty('SharedProp');
end;

function TTestBaseConfig.GetBaseOnly: string;
begin
  Result := GetConfigValueForProperty('BaseOnly');
end;

function TTestBaseConfig.GetSubOnly: string;
begin
  Result := GetConfigValueForProperty('SubOnly');
end;

{ TTestSubConfig }

class function TTestSubConfig.Default: TTestSubConfig;
begin
  Result := TTestSubConfig(inherited Default);
end;

{ TConfigSubclassTests }

procedure TConfigSubclassTests.CleanUp;
begin
  // Reset singleton so each test starts fresh
  TTestBaseConfig.FDefaultInstance := nil;
  FConfigFilename := TPath.Combine(TPath.GetLibraryPath, SUB_CONFIG_FILE);
  if TFile.Exists(FConfigFilename) then
    TFile.Delete(FConfigFilename);
end;

procedure TConfigSubclassTests.Setup;
begin
  CleanUp;
  TTestBaseConfig.RegisterConcreteClass(TTestSubConfig);
end;

procedure TConfigSubclassTests.TearDown;
begin
  CleanUp;
end;

procedure TConfigSubclassTests.DefaultCreatesSubclassInstance;
var
  LInstance: TTestBaseConfig;
begin
  LInstance := TTestBaseConfig.Default;
  Assert.IsTrue(LInstance is TTestSubConfig,
    'Default should create TTestSubConfig instance, got ' + LInstance.ClassName);
end;

procedure TConfigSubclassTests.TypedDefaultReturnsSubclass;
var
  LInstance: TTestSubConfig;
begin
  LInstance := TTestSubConfig.Default;
  Assert.IsNotNull(LInstance);
  Assert.AreEqual('TTestSubConfig', LInstance.ClassName);
end;

procedure TConfigSubclassTests.SubclassDefaultOverridesBase;
begin
  Assert.AreEqual(SUB_SHARED_DEFAULT, TTestSubConfig.Default.SharedProp,
    'Subclass default should override base default for SharedProp');
end;

procedure TConfigSubclassTests.BaseOnlyPropertyRetainsDefault;
begin
  Assert.AreEqual(BASE_ONLY_DEFAULT, TTestBaseConfig.Default.BaseOnly,
    'Base-only property should retain its default');
end;

procedure TConfigSubclassTests.SubOnlyPropertyHasDefault;
begin
  Assert.AreEqual(SUB_ONLY_DEFAULT, TTestSubConfig.Default.SubOnly,
    'Sub-only property should have correct default');
end;

initialization

TDUnitX.RegisterTestFixture(TConfigSubclassTests);

end.
