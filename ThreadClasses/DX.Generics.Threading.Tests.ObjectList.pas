unit DX.Generics.Threading.Tests.ObjectList;
{

  Cas de test DUnit Delphi
  ----------------------
  Cette unité contient une classe cas de test de squelette générée par l'expert Cas de test.
  Modifiez le code généré pour configurer et appeler correctement les méthodes de l'unité
  en cours de test.

}

interface

uses
  System.Classes, System.SysUtils, System.RTLConsts, System.Types,
  System.Generics.Defaults, System.Generics.Collections,
  System.Threading,
  TestFramework,
  DX.Generics.Threading, DX.Generics.Threading.Tests;

type
  // Méthodes de test pour la classe TThreadObjectList

  TestTThreadObjectList = class(TThreadTestCase)
  strict private
    FThreadObjectList: TThreadObjectList<TFoo>;
    function RunTaskAddObjects(ANumberOfObjects: integer = 1000): ITask;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestClear;
    procedure TestLock;
    procedure TestRemove;
  end;

implementation

function TestTThreadObjectList.RunTaskAddObjects(ANumberOfObjects: integer = 1000): ITask;
begin
  result := TTask.Run(
    procedure
    var
      i: integer;
      LFoo: TFoo;
    begin
      for i := 1 to ANumberOfObjects do
      begin
        LFoo := TFoo.Create;
        LFoo.Name := 'FOO' + i.ToString;
        FThreadObjectList.Add(LFoo);
      end;
    end);
end;

procedure TestTThreadObjectList.SetUp;
begin
  inherited;
  FThreadObjectList := TThreadObjectList<TFoo>.Create;
end;

procedure TestTThreadObjectList.TearDown;
begin
  FreeAndNil(FThreadObjectList);
  Inherited;
end;

procedure TestTThreadObjectList.TestAdd;
var
  LNumberOfObjects: integer;
  i: integer;
  LNumberOfTasks: integer;
begin
  LNumberOfObjects := 1000;
  LNumberOfTasks := 10;
  SetLength(FTasks, LNumberOfTasks);
  for i := 0 to LNumberOfTasks - 1 do
    FTasks[i] := RunTaskAddObjects(LNumberOfObjects);

  TTask.WaitForAll(FTasks);

  CheckEquals(LNumberOfObjects * LNumberOfTasks, FThreadObjectList.Count);

  FThreadObjectList.Clear;
  CheckEquals(0, FThreadObjectList.Count);
end;

procedure TestTThreadObjectList.TestClear;
begin
  CheckEquals(FThreadObjectList.Count, 0);
  FThreadObjectList.Clear;
  CheckEquals(FThreadObjectList.Count, 0);
  RunTaskAddObjects(1000).Wait;
  CheckEquals(1000, FThreadObjectList.Count);
  FThreadObjectList.Clear;
  CheckEquals(FThreadObjectList.Count, 0);
end;

procedure TestTThreadObjectList.TestLock;
var
  ReturnValue: TObjectList<TFoo>;
begin
  ReturnValue := FThreadObjectList.Lock;
  CheckIs(ReturnValue, TObjectList<TFoo>);
  FThreadObjectList.Unlock;
end;

procedure TestTThreadObjectList.TestRemove;
var
  LFoo: TFoo;
begin
  RunTaskAddObjects(1000).Wait;
  CheckEquals(1000, FThreadObjectList.Count);
  LFoo := FThreadObjectList.Lock.First;
  FThreadObjectList.Unlock;
  CheckNotNull(LFoo);
  FThreadObjectList.Remove(LFoo);
  CheckEquals(999, FThreadObjectList.Count);
  FThreadObjectList.Remove(LFoo);
  CheckEquals(999, FThreadObjectList.Count);
end;

initialization

// Enregistrer tous les cas de test avec l'exécuteur de test
RegisterTest(TestTThreadObjectList.Suite);

end.
