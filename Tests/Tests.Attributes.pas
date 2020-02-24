unit Tests.Attributes;

interface

uses
  System.Classes, System.SysUtils,
  DUnitX.TestFramework,
  DX.Classes.Attributes, DX.Classes.Configuration;

type
  [ConfigFileName('foo.ini')]
  TFoo = class(TObject)
  end;

  [TestFixture]
  TAttributeTests = class(TObject)
  private
    FFoo: TFoo;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    [Test]
    procedure StringValues;
  end;

implementation

{ TAttributeTests }

procedure TAttributeTests.Setup;
begin
  FFoo := TFoo.Create;
end;

procedure TAttributeTests.StringValues;
begin

end;

procedure TAttributeTests.Teardown;
begin
  FreeAndNil(FFoo);
end;

initialization

TDUnitX.RegisterTestFixture(TAttributeTests);

end.
