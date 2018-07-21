unit DX.Generics.Threading.Tests;

interface

uses
  System.Classes, System.SysUtils,
  System.Threading,
  TestFramework;

type
  TFoo = class(TObject)
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  TThreadTestCase = class(TTestCase)
  strict protected
    FTasks: TArray<ITask>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

{ TThreadTestCase }

procedure TThreadTestCase.SetUp;
begin
  inherited;
  SetLength(FTasks, 0);
end;

procedure TThreadTestCase.TearDown;
begin
  SetLength(FTasks, 0);
  inherited;
end;

end.
