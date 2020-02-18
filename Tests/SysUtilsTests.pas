unit SysUtilsTests;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  THashTests = class(TObject)
  private
    FInputString: string;
    FName: string;
    procedure SetName(const Value: string);
    function GetName: string;
  protected
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestMD5;
    [Test]
    procedure TestSHA1;
    [Test]
    procedure TestSHA256;
    // Test with TestCase Attribute to supply parameters.
    // [Test]
    // [TestCase('TestA','1,2')]
    // [TestCase('TestB','3,4')]
    procedure Test2(const AValue1: Integer; const AValue2: Integer);
    [Test]
    procedure TestSHA512;

    property LastName:string read GetName write SetName;
  end;

implementation

uses
  DX.SysUtils, System.SysUtils;

function THashTests.GetName: string;
begin

end;

procedure THashTests.SetName(const Value: string);
begin
  FName := Value;
end;

procedure THashTests.Setup;
begin
  FInputString := 'Hello World';
end;

procedure THashTests.TearDown;
begin
end;

procedure THashTests.TestMD5;
var
  LHash: string;
begin
  // See: http://www.sha1-online.com
  // Or: https://hashgenerator.de/
  LHash := THash.Hash(FInputString, THash.TAlgorithm.MD5);
  Assert.AreEqual(LHash, 'B10A8DB164E0754105B7A99BE72E3FE5');
end;

procedure THashTests.TestSHA1;
var
  LHash: string;
  LInput: string;
begin
  LHash := THash.Hash(FInputString, THash.TAlgorithm.SHA1);
  Assert.AreEqual(LHash.ToLower, '0a4d55a8d778e5022fab701977c5d840bbc486d0');
end;

procedure THashTests.TestSHA256;
var
  LHash: string;
  LInput: string;
begin
  LHash := THash.Hash(FInputString, THash.TAlgorithm.SHA256);
  Assert.AreEqual(LHash.ToLower, 'a591a6d40bf420404a011733cfb7b190d62c65bf0bcda32b57b277d9ad9f146e');
end;

procedure THashTests.TestSHA512;
var
  LInput: string;
  LHash: string;
begin
  LHash := THash.Hash(FInputString, THash.TAlgorithm.SHA512);
  Assert.AreEqual(LHash.ToLower,
    '2c74fd17edafd80e8447b0d46741ee243b7eb74dd2149a0ab1b9246fb30382f27e853d8585719e0e67cbda0daa8f51671064615d645ae27acb15bfb1447f459b');
end;

procedure THashTests.Test2(const AValue1: Integer; const AValue2: Integer);
begin
end;

initialization

TDUnitX.RegisterTestFixture(THashTests);

end.
