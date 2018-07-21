unit DX.Generics.Threading.Tests.List;
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

  TestTThreadList = class(TThreadTestCase)
  strict private
    FThreadList: TThreadList<TFoo>;
    function RunTaskAddObjects(ANumberOfObjects: integer = 1000): ITask;
  protected
    procedure FreeThreadListObjects;
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

function TestTThreadList.RunTaskAddObjects(ANumberOfObjects: integer = 1000): ITask;
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
        FThreadList.Add(LFoo);
      end;
    end);
end;

procedure TestTThreadList.FreeThreadListObjects;
var
  LList: TList<TFoo>;
  i: integer;
begin
  // There is no automatic cleanup of objects in a TList<>
  LList := FThreadList.Lock;
  try
    for i := 0 to LList.Count - 1 do
    begin
      LList.Items[i].Free;
      LList.Items[i] := nil;
    end;
  finally
    FThreadList.Lock;
  end;
end;

procedure TestTThreadList.SetUp;
begin
  inherited;
  FThreadList := TThreadList<TFoo>.Create;
end;

procedure TestTThreadList.TearDown;
begin
  FreeThreadListObjects;
  FreeAndNil(FThreadList);
  Inherited;
end;

procedure TestTThreadList.TestAdd;
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

  CheckEquals(LNumberOfObjects * LNumberOfTasks, FThreadList.Count);
  FreeThreadListObjects;
  FThreadList.Clear;
  CheckEquals(0, FThreadList.Count);
end;

procedure TestTThreadList.TestClear;
begin
  CheckEquals(FThreadList.Count, 0);
  FThreadList.Clear;
  CheckEquals(FThreadList.Count, 0);
  RunTaskAddObjects(1000).Wait;
  CheckEquals(1000, FThreadList.Count);

  FreeThreadListObjects;
  FThreadList.Clear;
  CheckEquals(FThreadList.Count, 0);
end;

procedure TestTThreadList.TestLock;
var
  ReturnValue: TList<TFoo>;
begin
  ReturnValue := FThreadList.Lock;
  CheckIs(ReturnValue, TList<TFoo>);
  FThreadList.Unlock;
end;

procedure TestTThreadList.TestRemove;
var
  LFoo: TFoo;
begin
  RunTaskAddObjects(1000).Wait;
  CheckEquals(1000, FThreadList.Count);
  LFoo := FThreadList.Lock.First;
  FThreadList.Unlock;
  CheckNotNull(LFoo);
  FThreadList.Remove(LFoo);
  CheckEquals(999, FThreadList.Count);
  FThreadList.Remove(LFoo);
  CheckEquals(999, FThreadList.Count);
  FreeAndNil(LFoo);
end;

initialization

// Enregistrer tous les cas de test avec l'exécuteur de test
RegisterTest(TestTThreadList.Suite);

end.
