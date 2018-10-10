unit DX.Generics.Tests.List;

interface

uses
  DUnitX.TestFramework, DX.Generics.Tests.Base;

type

  [TestFixture]
  TListTests = class(TThreadSafeTestBase)
  public
    [Setup]
    procedure Setup; override;
    [TearDown]
    procedure TearDown; override;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('TestA', '1,2')]
    [TestCase('TestB', '3,4')]
    procedure Test2(const AValue1: Integer; const AValue2: Integer);
  end;

implementation

procedure TListTests.Setup;
begin
  inherited;
end;

procedure TListTests.TearDown;
begin
  inherited;
end;

procedure TListTests.Test1;
begin
end;

procedure TListTests.Test2(const AValue1: Integer; const AValue2: Integer);
begin
end;

initialization

TDUnitX.RegisterTestFixture(TListTests);

end.
