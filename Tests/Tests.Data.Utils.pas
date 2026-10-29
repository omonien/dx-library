/// <summary>
/// Tests.Data.Utils
/// DUnitX tests for the connection string parsing in DX.Data.Utils.
/// </summary>
///
/// <copyright>
/// Copyright © 2026 Olaf Monien
/// Licensed under MIT
/// </copyright>

unit Tests.Data.Utils;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TConnectionStringTests = class
  private
    procedure CheckSegments(const AConnectionString: string; const AExpected: array of string);
  public
    [Test]
    procedure SimpleParams;
    [Test]
    procedure BracedODBCAdvanced;
    [Test]
    procedure NestedBraces;
    [Test]
    procedure SpacesInValues;
    [Test]
    procedure DoubleQuotesInPassword;
    [Test]
    procedure EmptySegmentsAreSkipped;
    [Test]
    procedure EmptyString;
    [Test]
    procedure QuotedWholePairs;
    [Test]
    procedure ConnectionHelperSetsParams;
  end;

implementation

uses
  System.SysUtils,
  FireDAC.Comp.Client,
  DX.Data.Utils;

procedure TConnectionStringTests.CheckSegments(const AConnectionString: string; const AExpected: array of string);
var
  LActual: TArray<string>;
  I: Integer;
begin
  LActual := SplitConnectionString(AConnectionString);
  Assert.AreEqual(Length(AExpected), Length(LActual), 'Segment count');
  for I := 0 to High(AExpected) do
  begin
    Assert.AreEqual(AExpected[I], LActual[I], Format('Segment %d', [I]));
  end;
end;

procedure TConnectionStringTests.SimpleParams;
begin
  CheckSegments('DriverID=FB;Database=C:\Data\Test.fdb;User_Name=sysdba',
    ['DriverID=FB', 'Database=C:\Data\Test.fdb', 'User_Name=sysdba']);
end;

procedure TConnectionStringTests.BracedODBCAdvanced;
begin
  CheckSegments('DriverID=ODBC;ODBCAdvanced={TrustServerCertificate=yes;Encrypt=no};Server=db',
    ['DriverID=ODBC', 'ODBCAdvanced={TrustServerCertificate=yes;Encrypt=no}', 'Server=db']);
end;

procedure TConnectionStringTests.NestedBraces;
begin
  CheckSegments('A={x;{y;z}};B=1', ['A={x;{y;z}}', 'B=1']);
end;

procedure TConnectionStringTests.SpacesInValues;
begin
  CheckSegments('Database=C:\My Data\Test DB.fdb;User_Name= John Doe ;Password=a b',
    ['Database=C:\My Data\Test DB.fdb', 'User_Name= John Doe ', 'Password=a b']);
end;

procedure TConnectionStringTests.DoubleQuotesInPassword;
begin
  CheckSegments('User_Name=sysdba;Password=pa"ss" word;Server="host"',
    ['User_Name=sysdba', 'Password=pa"ss" word', 'Server="host"']);
end;

procedure TConnectionStringTests.EmptySegmentsAreSkipped;
begin
  CheckSegments(';DriverID=FB;;Database=x;;;', ['DriverID=FB', 'Database=x']);
end;

procedure TConnectionStringTests.EmptyString;
begin
  CheckSegments('', []);
end;

procedure TConnectionStringTests.QuotedWholePairs;
begin
  CheckSegments('"Password=my secret"', ['Password=my secret']);
  CheckSegments('"Password= lead"', ['Password= lead']);
  CheckSegments('"Password=a;b"', ['Password=a;b']);
  CheckSegments('"Password=a""b"', ['Password=a"b']);
  CheckSegments('User_Name=sysdba;"Password=a;b";Server=db',
    ['User_Name=sysdba', 'Password=a;b', 'Server=db']);
end;

procedure TConnectionStringTests.ConnectionHelperSetsParams;
var
  LConnection: TFDConnection;
begin
  LConnection := TFDConnection.Create(nil);
  try
    LConnection.Params.Add('Stale=1');
    LConnection.SetParamsFromConnectionString(
      'DriverID=ODBC;ODBCAdvanced={A=1;B=2};Password=x "y" z;;');
    Assert.AreEqual(3, LConnection.Params.Count, 'Param count');
    Assert.AreEqual('ODBC', LConnection.Params.Values['DriverID']);
    Assert.AreEqual('{A=1;B=2}', LConnection.Params.Values['ODBCAdvanced']);
    Assert.AreEqual('x "y" z', LConnection.Params.Values['Password']);
  finally
    FreeAndNil(LConnection);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TConnectionStringTests);

end.
