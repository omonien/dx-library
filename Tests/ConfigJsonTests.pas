unit ConfigJsonTests;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TConfigJsonTests = class(TObject)
  private
    FConfigPath: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure ReadExistingJson;
    [Test]
    procedure CreateSetting;
    [Test]
    procedure LoadSetting;
    [Test]
    procedure LoadObject;

  end;

implementation

uses
  DX.Settings.Json, DX.Settings, System.IOUtils, System.Json, System.SysUtils;

procedure TConfigJsonTests.CreateSetting;
begin
  TFile.Delete(TAppSettings.Filename);
  Assert.IsFalse(TFile.Exists(TAppSettings.Filename), 'Orphaned config file');
  TAppSettings.Write('Section1', 'Name1', 'Value1');
  TAppSettings.Write('Section1', 'Name2', 'Value2');
  Assert.IsTrue(TFile.Exists(TAppSettings.Filename), 'Missing COnfig file');
  ReadExistingJson;
end;

type
  TFoo = class(TObject)
  private
    FName: string;
    FBirthday: TDate;
    FID: cardinal;
    procedure SetBirthday(const Value: TDate);
    procedure SetID(const Value: cardinal);
    procedure SetName(const Value: string);
  public
    property ID: cardinal read FID write SetID;
    property Name: string read FName write SetName;
    property Birthday: TDate read FBirthday write SetBirthday;
  end;

procedure TConfigJsonTests.LoadObject;
var
  LFoo: TFoo;
  LFoo2: TFoo;
begin
  LFoo := TFoo.Create;
  try
    LFoo.ID := 4711;
    LFoo.Name := 'John Doe';
    LFoo.Birthday := Date;

    TAppSettings.Write('Section1', 'Foo', LFoo);
    LFoo2 := TAppSettings.Read<TFoo>('Section1', 'Foo');
    Assert.AreEqual(LFoo.ID, LFoo2.ID);
    Assert.AreEqual(LFoo.Name, LFoo2.Name);
    Assert.AreEqual(LFoo.Birthday, LFoo2.Birthday);
  finally
    FreeAndNil(LFoo2);
    FreeAndNil(LFoo);
  end;

end;

procedure TConfigJsonTests.LoadSetting;
var
  LValue: string;
begin
  LValue := TAppSettings.Read('Section1', 'Name1', '');
  Assert.AreEqual('Value1', LValue, 'Invalid value for Name1');
  LValue := TAppSettings.Read('Section1', 'Name2', '');
  Assert.AreEqual('Value2', LValue, 'Invalid value for Name2');
  LValue := TAppSettings.Read('Section1', 'Name3', 'XXX');
  Assert.AreEqual('XXX', LValue, 'Invalid default value for Name3');
end;

procedure TConfigJsonTests.ReadExistingJson;
var
  LJson: string;
  LValue: TJSONValue;
  LSection: TJsonObject;
begin
  Assert.IsTrue(TFile.Exists(FConfigPath), 'Missing config file');
  LJson := TFile.ReadAllText(FConfigPath);
  Assert.IsNotEmpty(LJson, 'Config file empty');
  LValue := TJsonObject.ParseJSONValue(LJson);
  Assert.IsNotNull(LValue, 'Config file failed to parse');
  Assert.IsTrue(LValue is TJsonObject, 'Root in config file is not an object');
  Assert.IsTrue((LValue as TJsonObject).Count > 0, 'Too few pairs in config file');
  LSection := (LValue as TJsonObject).Values['Section1'] as TJsonObject;
  LValue := LSection.Values['Name1'];
  Assert.IsTrue(LValue.Value = 'Value1', '"Value1" not found');
  LValue := LSection.Values['Name2'];
  Assert.IsTrue(LValue.Value = 'Value2', '"Value2" not found');
  Assert.AreEqual(TAppSettings.Filename, FConfigPath, 'Config path invalid');
end;

procedure TConfigJsonTests.Setup;
var
  LJson: string;
begin
  FConfigPath := TPath.ChangeExtension(ParamStr(0), '.config');
  LJson := '{"Section1":{"Name1":"Value1","Name2":"Value2"}}';
  TFile.WriteAllText(FConfigPath, LJson);
end;

procedure TConfigJsonTests.TearDown;
begin
  TFile.Delete(FConfigPath);
end;

{ TFoo }

procedure TFoo.SetBirthday(const Value: TDate);
begin
  FBirthday := Value;
end;

procedure TFoo.SetID(const Value: cardinal);
begin
  FID := Value;
end;

procedure TFoo.SetName(const Value: string);
begin
  FName := Value;
end;

initialization

TDUnitX.RegisterTestFixture(TConfigJsonTests);

end.
