unit DX.Generics.Tests.Base;

interface

uses
  System.Classes, System.SysUtils,
  System.Threading,
  DUnitX.TestFramework;

type

  TFoo = class(TObject)
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  TThreadSafeTestBase = class abstract(TObject)
  strict protected
    FTasks: TArray<ITask>;
  public
    procedure Setup; virtual;
    procedure TearDown; virtual;
  end;

implementation

procedure TThreadSafeTestBase.Setup;
begin
  SetLength(FTasks, 0);
end;

procedure TThreadSafeTestBase.TearDown;
begin
  SetLength(FTasks, 0);
end;

initialization

TDUnitX.RegisterTestFixture(TThreadSafeTestBase);

end.
