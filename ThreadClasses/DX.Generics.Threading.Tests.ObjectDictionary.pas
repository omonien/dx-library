unit DX.Generics.Threading.Tests.ObjectDictionary;
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
  TestFramework,
  System.Generics.Defaults, System.Generics.Collections, System.Threading,
  DX.Generics.Threading, DX.Generics.Threading.Tests;

type
  // Méthodes de test pour la classe TThreadDictionary

  TestTThreadObjectDictionary = class(TThreadTestCase)
  strict private
    FThreadObjectDictionary: TThreadObjectDictionary<string, TFoo>;
  private
    function RunTaskAddObjects(ANumberOfObjects: integer = 1000; ATaskCount: integer = 1): ITask;
    procedure CreateDuplicate;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLock;
    procedure TestAdd;
    procedure TestDuplicates;
    procedure TestRemove;
    procedure TestClear;
    procedure TestTryGetValue;
    procedure TestAddOrSetValue;
    procedure TestContainsKey;
    procedure TestContainsValue;
  end;

implementation

function TestTThreadObjectDictionary.RunTaskAddObjects(ANumberOfObjects: integer = 1000; ATaskCount: integer = 1): ITask;
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
        LFoo.Name := 'FOO' + ATaskCount.ToString + '-' + i.ToString; // Unique naem over all Tasks
        FThreadObjectDictionary.Add(LFoo.Name, LFoo);
      end;
    end);
end;

procedure TestTThreadObjectDictionary.SetUp;
begin
  inherited;
  FThreadObjectDictionary := TThreadObjectDictionary<string, TFoo>.Create([doOwnsValues]);
end;

procedure TestTThreadObjectDictionary.TearDown;
begin
  FreeAndNil(FThreadObjectDictionary);
  Inherited;
end;

procedure TestTThreadObjectDictionary.TestLock;
var
  ReturnValue: TDictionary<string, TFoo>;
begin
  ReturnValue := FThreadObjectDictionary.Lock;
  CheckNotNull(ReturnValue);
  CheckIs(ReturnValue, TDictionary<string, TFoo>);
  CheckEquals(0, ReturnValue.Count);
  FThreadObjectDictionary.UnLock;
end;

procedure TestTThreadObjectDictionary.TestAdd;
var
  LNumberOfObjects: integer;
  LNumberOfTasks: integer;
  i: integer;
begin
  LNumberOfObjects := 1000;
  LNumberOfTasks := 10;
  SetLength(FTasks, LNumberOfTasks);
  for i := 0 to LNumberOfTasks - 1 do
    FTasks[i] := RunTaskAddObjects(LNumberOfObjects, i);
  TTask.WaitForAll(FTasks);

  CheckEquals(LNumberOfObjects * LNumberOfTasks, FThreadObjectDictionary.Count);

  FThreadObjectDictionary.Clear;
  CheckEquals(0, FThreadObjectDictionary.Count);
end;

procedure TestTThreadObjectDictionary.TestRemove;
var
  LPair: TPair<string, TFoo>;
  LFoo: TFoo;
begin
  RunTaskAddObjects(1000, 1).Wait;
  CheckEquals(1000, FThreadObjectDictionary.Count);
  LFoo := TFoo.Create;
  LFoo.Name := 'TEST';
  FThreadObjectDictionary.Add('TEST', LFoo);
  CheckEquals(1001, FThreadObjectDictionary.Count);

  LPair := FThreadObjectDictionary.ExtractPair('TEST'); // removes from Dictionary
  CheckNotNull(LPair.Value);
  CheckEquals(1000, FThreadObjectDictionary.Count);

  FThreadObjectDictionary.Add('TEST', LFoo); // Put back in
  FThreadObjectDictionary.Remove(LPair.Key); // also removes from Dictionary
  CheckEquals(1000, FThreadObjectDictionary.Count);

  FThreadObjectDictionary.Remove(LPair.Key);
  CheckEquals(1000, FThreadObjectDictionary.Count);
end;

procedure TestTThreadObjectDictionary.TestClear;
begin
  RunTaskAddObjects(1000, 1).Wait;
  CheckEquals(1000, FThreadObjectDictionary.Count);
  FThreadObjectDictionary.Clear;
  CheckEquals(0, FThreadObjectDictionary.Count);
end;

procedure TestTThreadObjectDictionary.TestTryGetValue;
var
  LFoo: TFoo;
  LFoo2: TFoo;

begin
  RunTaskAddObjects(1000, 1).Wait;
  CheckEquals(1000, FThreadObjectDictionary.Count);
  LFoo := TFoo.Create;
  LFoo.Name := 'TEST';
  FThreadObjectDictionary.Add('TEST', LFoo);
  CheckEquals(1001, FThreadObjectDictionary.Count);
  FThreadObjectDictionary.TryGetValue('TEST', LFoo2);
  CheckSame(LFoo, LFoo2);

  FThreadObjectDictionary.TryGetValue('XXX', LFoo2);
  CheckNull(LFoo2);
end;

procedure TestTThreadObjectDictionary.TestAddOrSetValue;
var
  LFoo: TFoo;
  LFoo2: TFoo;
begin
  LFoo := TFoo.Create;
  LFoo.Name := 'TEST';

  FThreadObjectDictionary.AddOrSetValue('TEST', LFoo);
  CheckEquals(1, FThreadObjectDictionary.Count);

  LFoo2 := TFoo.Create;
  LFoo.Name := 'TEST';
  FThreadObjectDictionary.AddOrSetValue('TEST', LFoo2);
  CheckEquals(1, FThreadObjectDictionary.Count);
end;

procedure TestTThreadObjectDictionary.TestContainsKey;
var
  LFoo: TFoo;
begin
  RunTaskAddObjects(1000, 1).Wait;
  CheckEquals(1000, FThreadObjectDictionary.Count);
  LFoo := TFoo.Create;
  LFoo.Name := 'TEST';

  FThreadObjectDictionary.AddOrSetValue('TEST', LFoo);
  CheckEquals(1001, FThreadObjectDictionary.Count);

  CheckTrue(FThreadObjectDictionary.ContainsKey('TEST'));
  CheckFalse(FThreadObjectDictionary.ContainsKey('XXX'));
end;

procedure TestTThreadObjectDictionary.TestContainsValue;
var
  LFoo: TFoo;
begin
  RunTaskAddObjects(1000, 1).Wait;
  CheckEquals(1000, FThreadObjectDictionary.Count);
  LFoo := TFoo.Create;
  LFoo.Name := 'TEST';

  FThreadObjectDictionary.AddOrSetValue('TEST', LFoo);
  CheckEquals(1001, FThreadObjectDictionary.Count);

  CheckTrue(FThreadObjectDictionary.ContainsValue(LFoo));
end;

procedure TestTThreadObjectDictionary.TestDuplicates;
begin
  CheckException(CreateDuplicate, Exception);
end;

procedure TestTThreadObjectDictionary.CreateDuplicate;
var
  LFoo: TFoo;
begin
  LFoo := TFoo.Create;
  LFoo.Name := 'TEST';
  FThreadObjectDictionary.Add(LFoo.Name, LFoo);

  // The second Foo instance (same name) should create cause an exception,
  // thus needs to be freed, to avoid MemLeak Warnings
  try
    LFoo := TFoo.Create;
    LFoo.Name := 'TEST';
    FThreadObjectDictionary.Add(LFoo.Name, LFoo);
  finally
    FreeAndNil(LFoo);
  end;
end;

initialization

// Enregistrer tous les cas de test avec l'exécuteur de test
RegisterTest(TestTThreadObjectDictionary.Suite);

end.
