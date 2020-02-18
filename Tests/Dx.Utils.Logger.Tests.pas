unit Dx.Utils.Logger.Tests;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TDXLoggerTest = class
  private
    FLogfileName: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestWriteLog;

  end;

implementation

uses
  Dx.Utils.Logger, System.SysUtils, System.IOUtils, Dx.Classes.Strings,
  System.Types;

procedure TDXLoggerTest.Setup;
var
  s: string;
begin
  s := TPath.GetLibraryPath;
  FLogfileName := TPath.Combine(s,
    TPath.ChangeExtension(TPath.GetFileName(ParamStr(0)), '.log'));
  if TFile.Exists(FLogfileName) then
  begin
    TFile.Delete(FLogfileName);
  end;
end;

procedure TDXLoggerTest.TearDown;
begin
end;

procedure TDXLoggerTest.TestWriteLog;
var
  LMessage: string;
  LText: TStringDynArray;
begin
  LMessage := 'Hello World! Umlauts: üצה';
  DXLog(LMessage);
  DXLog(LMessage);
  Sleep(1000);
  Assert.IsTrue(TFile.Exists(FLogfileName));
  LText := TFile.ReadAllLines(FLogfileName, TEncoding.UTF8);
  //Two lines of text
  Assert.AreEqual(2, Length(LText));
  //append file
  DXLog(LMessage);
  Sleep(1000);
  LText := TFile.ReadAllLines(FLogfileName, TEncoding.UTF8);
  //threee lines of text
  Assert.AreEqual(3, Length(LText));
end;

initialization

TDUnitX.RegisterTestFixture(TDXLoggerTest);

end.
