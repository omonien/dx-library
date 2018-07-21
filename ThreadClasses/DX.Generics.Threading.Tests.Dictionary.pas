unit DX.Generics.Threading.Tests.Dictionary;
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

  TestTThreadDictionary = class(TThreadTestCase)
  strict private
    FThreadDictionary: TThreadDictionary<string, TFoo>;
  private
    function RunTaskAddObjects(ANumberOfObjects: integer = 1000; ATaskCount: integer = 1): ITask;
    procedure CreateDuplicate;
    procedure OnValueNotify(Sender: TObject; const Item: TFoo; Action: TCollectionNotification);

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
    procedure TestTimout;
  end;

implementation

procedure TestTThreadDictionary.OnValueNotify(Sender: TObject; const Item: TFoo; Action: TCollectionNotification);
begin
  if Action = TCollectionNotification.cnRemoved then
  begin
    Item.Free;
  end;
end;

function TestTThreadDictionary.RunTaskAddObjects(ANumberOfObjects: integer = 1000; ATaskCount: integer = 1): ITask;
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
        FThreadDictionary.Add(LFoo.Name, LFoo);
      end;
    end);
end;

procedure TestTThreadDictionary.SetUp;
begin
  inherited;
  FThreadDictionary := TThreadDictionary<string, TFoo>.Create;
  FThreadDictionary.Lock.OnValueNotify := OnValueNotify; // No ownership in TDictionary! Manual cleanup required!
  FThreadDictionary.UnLock;
end;

procedure TestTThreadDictionary.TearDown;
begin
  FreeAndNil(FThreadDictionary);
  Inherited;
end;

procedure TestTThreadDictionary.TestLock;
var
  ReturnValue: TDictionary<string, TFoo>;
begin
  ReturnValue := FThreadDictionary.Lock;
  CheckNotNull(ReturnValue);
  CheckIs(ReturnValue, TDictionary<string, TFoo>);
  CheckEquals(0, ReturnValue.Count);
  FThreadDictionary.UnLock;
end;

procedure TestTThreadDictionary.TestAdd;
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

  CheckEquals(LNumberOfObjects * LNumberOfTasks, FThreadDictionary.Count);

  FThreadDictionary.Clear;
  CheckEquals(0, FThreadDictionary.Count);
end;

procedure TestTThreadDictionary.TestRemove;
var
  LPair: TPair<string, TFoo>;
  LFoo: TFoo;
begin
  RunTaskAddObjects(1000, 1).Wait;
  CheckEquals(1000, FThreadDictionary.Count);
  LFoo := TFoo.Create;
  LFoo.Name := 'TEST';
  FThreadDictionary.Add('TEST', LFoo);
  CheckEquals(1001, FThreadDictionary.Count);

  LPair := FThreadDictionary.ExtractPair('TEST'); // removes from Dictionary
  CheckNotNull(LPair.Value);
  CheckEquals(1000, FThreadDictionary.Count);

  FThreadDictionary.Add('TEST', LFoo); // Put back in
  FThreadDictionary.Remove(LPair.Key); // also removes from Dictionary
  CheckEquals(1000, FThreadDictionary.Count);

  FThreadDictionary.Remove(LPair.Key);
  CheckEquals(1000, FThreadDictionary.Count);
end;

procedure TestTThreadDictionary.TestClear;
begin
  RunTaskAddObjects(1000, 1).Wait;
  CheckEquals(1000, FThreadDictionary.Count);
  FThreadDictionary.Clear;
  CheckEquals(0, FThreadDictionary.Count);
end;

procedure TestTThreadDictionary.TestTimout;
var
  LTask1: ITask;
  LTask2: ITask;
  LException1: Exception;
  LException2: Exception;
begin
  // Make sure we are starting with the default
  TLockConfiguration.SetLockTimout(LOCK_TIMEOUT);

  LException1 := nil;
  LException2 := nil;
  LTask1 := TTask.Run(
    procedure
    begin
      try
        FThreadDictionary.Lock;
        Sleep(3000);
        FThreadDictionary.UnLock;
      except
        on E: Exception do
        begin
          LException1 := E;
        end;
      end;
    end);

  LTask2 := TTask.Run(
    procedure
    begin
      try
        FThreadDictionary.Lock;
        Sleep(3000);
        FThreadDictionary.UnLock;
      except
        on E: Exception do
        begin
          LException2 := E;
        end;
      end;
    end);

  TTask.WaitForAll([LTask1, LTask2]);
  CheckTrue(Assigned(LException1) or Assigned(LException2));

  // Test for lock lock not causing a Timout
  TLockConfiguration.SetLockTimout(500);

  LException1 := nil;
  LException2 := nil;

  LTask1 := TTask.Run(
    procedure
    begin
      try
        FThreadDictionary.Lock;
        Sleep(200);
        FThreadDictionary.UnLock;
      except
        on E: Exception do
        begin
          LException1 := E;
        end;
      end;
    end);

  LTask2 := TTask.Run(
    procedure
    begin
      try
        FThreadDictionary.Lock;
        Sleep(200);
        FThreadDictionary.UnLock;
      except
        on E: Exception do
        begin
          LException2 := E;
        end;
      end;
    end);

  TTask.WaitForAll([LTask1, LTask2]);
  CheckTrue(not Assigned(LException1) and not Assigned(LException2));

  // Restore Default
  TLockConfiguration.SetLockTimout(LOCK_TIMEOUT);
end;

procedure TestTThreadDictionary.TestTryGetValue;
var
  LFoo: TFoo;
  LFoo2: TFoo;

begin
  RunTaskAddObjects(1000, 1).Wait;
  CheckEquals(1000, FThreadDictionary.Count);
  LFoo := TFoo.Create;
  LFoo.Name := 'TEST';
  FThreadDictionary.Add('TEST', LFoo);
  CheckEquals(1001, FThreadDictionary.Count);
  FThreadDictionary.TryGetValue('TEST', LFoo2);
  CheckSame(LFoo, LFoo2);

  FThreadDictionary.TryGetValue('XXX', LFoo2);
  CheckNull(LFoo2);
end;

procedure TestTThreadDictionary.TestAddOrSetValue;
var
  LFoo: TFoo;
begin
  FThreadDictionary.Lock.OnValueNotify := nil;
  FThreadDictionary.UnLock;

  LFoo := TFoo.Create;
  try
    LFoo.Name := 'TEST';

    FThreadDictionary.AddOrSetValue('TEST', LFoo);
    CheckEquals(1, FThreadDictionary.Count);

    FThreadDictionary.AddOrSetValue('TEST', LFoo);
    CheckEquals(1, FThreadDictionary.Count);
  finally
    FreeAndNil(LFoo);
  end;
end;

procedure TestTThreadDictionary.TestContainsKey;
var
  LFoo: TFoo;
begin
  RunTaskAddObjects(1000, 1).Wait;
  CheckEquals(1000, FThreadDictionary.Count);
  LFoo := TFoo.Create;
  LFoo.Name := 'TEST';

  FThreadDictionary.AddOrSetValue('TEST', LFoo);
  CheckEquals(1001, FThreadDictionary.Count);

  CheckTrue(FThreadDictionary.ContainsKey('TEST'));
  CheckFalse(FThreadDictionary.ContainsKey('XXX'));
end;

procedure TestTThreadDictionary.TestContainsValue;
var
  LFoo: TFoo;
begin
  RunTaskAddObjects(1000, 1).Wait;
  CheckEquals(1000, FThreadDictionary.Count);
  LFoo := TFoo.Create;
  LFoo.Name := 'TEST';

  FThreadDictionary.AddOrSetValue('TEST', LFoo);
  CheckEquals(1001, FThreadDictionary.Count);

  CheckTrue(FThreadDictionary.ContainsValue(LFoo));
end;

procedure TestTThreadDictionary.TestDuplicates;
begin
  CheckException(CreateDuplicate, Exception);
end;

procedure TestTThreadDictionary.CreateDuplicate;
var
  LFoo: TFoo;
begin
  LFoo := TFoo.Create;
  LFoo.Name := 'TEST';
  FThreadDictionary.Add(LFoo.Name, LFoo);

  // The second Foo instance (same name) should create cause an exception,
  // thus needs to be freed, to avoid MemLeak Warnings
  try
    LFoo := TFoo.Create;
    LFoo.Name := 'TEST';
    FThreadDictionary.Add(LFoo.Name, LFoo);
  finally
    FreeAndNil(LFoo);
  end;
end;

initialization

// Enregistrer tous les cas de test avec l'exécuteur de test
RegisterTest(TestTThreadDictionary.Suite);

end.
