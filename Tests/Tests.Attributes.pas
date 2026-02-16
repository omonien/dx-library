unit Tests.Attributes;

interface

uses
  System.Classes, System.SysUtils,
  DUnitX.TestFramework,
  DX.Classes.Attributes, DX.Lib.Configuration;

type

  [ConfigFile('foo')]
  TFoo = class(TObject)
  end;

  TBar = class(TObject)
  end;

  [TestFixture]
  TAttributeTests = class(TObject)
  private
    FFoo: TFoo;
    FBar: TBar;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    [Test]
    procedure StringValues;
  end;

implementation

uses
  DX.Utils.RTTI;

{ TAttributeTests }

procedure TAttributeTests.Setup;
begin
  FFoo := TFoo.Create;
  FBar := TBar.Create;
end;

procedure TAttributeTests.StringValues;
begin
  Assert.IsTrue(FFoo.HasAttribute(ConfigFileAttribute));
  Assert.IsFalse(FBar.HasAttribute(ConfigFileAttribute));
  Assert.AreEqual('foo', FFoo.AttributeValue(ConfigFileAttribute));
  Assert.AreEqual('', FFoo.AttributeValue(ConfigValueAttribute));
  Assert.AreEqual('', FBar.AttributeValue(ConfigFileAttribute));
end;

procedure TAttributeTests.Teardown;
begin
  FreeAndNil(FBar);
  FreeAndNil(FFoo);
end;

initialization

TDUnitX.RegisterTestFixture(TAttributeTests);

end.
