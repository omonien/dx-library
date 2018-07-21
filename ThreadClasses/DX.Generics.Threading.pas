unit DX.Generics.Threading;

interface

uses
  System.Classes, System.SysUtils, System.RTLConsts,
  System.Types, System.Generics.Defaults, System.Generics.Collections;


// The following thread-safe classes use TMonitor for locking read/write access
// To discover long running locks and possible deadlocks, a LOCK_TIMEOUT can be set
// If a lock cannot be acquired within that time, then an exception is raised.
// The callstack of that exception should then be examined to idetify the reason
// of the long running lock

const
  // Lock timeout in milliseconds
  LOCK_TIMEOUT = 2000;
  // INFINITE timout means no timeout at all
  // LOCKTIMEOUT = INFINITE;

type
  EMonitorLockTimout = class(EMonitorLockException);

  TLockConfiguration = class
  strict private
    class var FLockTimeout: Cardinal;
  public
    class constructor Create;
    // Call this during program initialization to set an other value of the default of 2000ms
    // Note: This procedure should only be called once. It is not meant to be changed multiple
    // times at run time.
    class procedure SetLockTimout(AValue: Cardinal);
    class function GetLockTimeout: Cardinal;
  end;

  TLockable<T: Class> = class abstract(TObject)
  strict protected
    FContainer: T;
  public
    destructor Destroy; override;
    function Lock: T;
    procedure Unlock; inline;
  end;

  TThreadList<T> = class(TLockable < TList < T >> )
  private
    FDuplicates: TDuplicates;
  protected
    function GetItem(AIndex: integer): T;
  public
    constructor Create;
    procedure Add(const Item: T);
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload; inline;
    procedure AddRange(const Collection: TEnumerable<T>); overload; inline;
    procedure Clear;
    function Contains(const Value: T): Boolean; inline;
    function Count: integer;
    procedure Remove(const Item: T); inline;
    procedure RemoveItem(const Item: T; Direction: TDirection);
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    // Items is read-only. read/write would be two separate operations, which would not be threadsafe
    property Items[Index: integer]: T read GetItem; default;

  end;

  TThreadObjectList<T: Class> = class(TLockable < TObjectList < T >> )
  strict private
    FDuplicates: TDuplicates;
  protected
    function GetOwnsObjects: Boolean;
    procedure WriteOwnsObjects(const Value: Boolean);
    function GetCount: integer;

  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean = True); overload;
    constructor Create(const ACollection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;

    procedure Add(const Item: T);
    procedure Clear;

    procedure Remove(const Item: T); inline;
    procedure RemoveItem(const Item: T; Direction: TDirection);

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property OwnsObjects: Boolean read GetOwnsObjects write WriteOwnsObjects;
    property Count: integer read GetCount;
  end;

  TThreadDictionary<TKey, TValue> = class(TLockable < TDictionary < TKey, TValue >> )
  protected
    function GetCount: integer;
  public
    constructor Create(ACapacity: integer = 0); overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(ACapacity: integer; const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const Collection: TEnumerable < TPair < TKey, TValue >> ); overload;
    constructor Create(const Collection: TEnumerable<TPair<TKey, TValue>>; const AComparer: IEqualityComparer<TKey>); overload;

    procedure Add(const Key: TKey; const Value: TValue);
    procedure Remove(const Key: TKey);
    function ExtractPair(const Key: TKey): TPair<TKey, TValue>;
    procedure TrimExcess;
    procedure Clear;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure AddOrSetValue(const Key: TKey; const Value: TValue);
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function ToArray: TArray<TPair<TKey, TValue>>;
    property Count: integer read GetCount;

  end;

  TThreadObjectDictionary<TKey, TValue> = class(TThreadDictionary<TKey, TValue>)
  strict private
  private
    FOwnerships: TDictionaryOwnerships;
  protected
    procedure KeyNotify(ASender: TObject; const Key: TKey; Action: TCollectionNotification);
    procedure ValueNotify(ASender: TObject; const Value: TValue; Action: TCollectionNotification);
  public
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: integer = 0); overload;
    constructor Create(Ownerships: TDictionaryOwnerships; const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: integer; const AComparer: IEqualityComparer<TKey>); overload;
  end;

implementation

uses
  System.TypInfo, System.SysConst;
{ TThreadObjectList<T> }

constructor TThreadObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FContainer := TObjectList<T>.Create(AOwnsObjects);
  FDuplicates := dupIgnore;
end;

constructor TThreadObjectList<T>.Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean);
begin
  inherited Create;
  FContainer := TObjectList<T>.Create(AComparer, AOwnsObjects);
  FDuplicates := dupIgnore;
end;

constructor TThreadObjectList<T>.Create(const ACollection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create;
  FContainer := TObjectList<T>.Create(ACollection, AOwnsObjects);
  FDuplicates := dupIgnore;
end;

function TThreadObjectList<T>.GetCount: integer;
begin
  Lock;
  try
    result := FContainer.Count;
  finally
    Unlock;
  end;
end;

function TThreadObjectList<T>.GetOwnsObjects: Boolean;
begin
  Lock;
  try
    result := FContainer.OwnsObjects;
  finally
    Unlock;
  end;
end;

procedure TThreadObjectList<T>.Add(const Item: T);
begin
  Lock;
  try
    if (Duplicates = dupAccept) or (FContainer.IndexOf(Item) = -1) then
      FContainer.Add(Item)
    else if Duplicates = dupError then
      raise EListError.CreateFmt(SDuplicateItem, [FContainer.ItemValue(Item)]);
  finally
    Unlock;
  end;
end;

procedure TThreadObjectList<T>.Clear;
begin
  Lock;
  try
    FContainer.Clear;
  finally
    Unlock;
  end;
end;

procedure TThreadObjectList<T>.Remove(const Item: T);
begin
  RemoveItem(Item, TDirection.FromBeginning);
end;

procedure TThreadObjectList<T>.RemoveItem(const Item: T; Direction: TDirection);
begin
  Lock;
  try
    FContainer.RemoveItem(Item, Direction);
  finally
    Unlock;
  end;
end;

procedure TThreadObjectList<T>.WriteOwnsObjects(const Value: Boolean);
begin
  Lock;
  try
    FContainer.OwnsObjects := Value;
  finally
    Unlock;
  end;
end;

{ TDictionary<TKey, TValue> }

constructor TThreadDictionary<TKey, TValue>.Create(ACapacity: integer);
begin
  inherited Create;
  FContainer := TDictionary<TKey, TValue>.Create(ACapacity);
end;

constructor TThreadDictionary<TKey, TValue>.Create(const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  FContainer := TDictionary<TKey, TValue>.Create(AComparer);
end;

constructor TThreadDictionary<TKey, TValue>.Create(ACapacity: integer; const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  FContainer := TDictionary<TKey, TValue>.Create(ACapacity, AComparer);
end;

constructor TThreadDictionary<TKey, TValue>.Create(const Collection: TEnumerable<TPair<TKey, TValue>>; const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  FContainer := TDictionary<TKey, TValue>.Create(Collection, AComparer);

end;

constructor TThreadDictionary<TKey, TValue>.Create(const Collection: TEnumerable < TPair < TKey, TValue >> );
begin
  inherited Create;
  FContainer := TDictionary<TKey, TValue>.Create(Collection);
end;

procedure TThreadDictionary<TKey, TValue>.Add(const Key: TKey; const Value: TValue);
begin
  Lock;
  try
    FContainer.Add(Key, Value);
  finally
    Unlock;
  end;
end;

procedure TThreadDictionary<TKey, TValue>.AddOrSetValue(const Key: TKey; const Value: TValue);
begin
  Lock;
  try
    FContainer.AddOrSetValue(Key, Value);
  finally
    Unlock;
  end;
end;

procedure TThreadDictionary<TKey, TValue>.Clear;
begin
  Lock;
  try
    FContainer.Clear;
  finally
    Unlock;
  end;
end;

function TThreadDictionary<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  Lock;
  try
    result := FContainer.ContainsKey(Key);
  finally
    Unlock;
  end;
end;

function TThreadDictionary<TKey, TValue>.ContainsValue(const Value: TValue): Boolean;
begin
  Lock;
  try
    result := FContainer.ContainsValue(Value);
  finally
    Unlock;
  end;
end;

function TThreadDictionary<TKey, TValue>.ExtractPair(const Key: TKey): TPair<TKey, TValue>;
begin
  Lock;
  try
    result := FContainer.ExtractPair(Key);
  finally
    Unlock;
  end;
end;

function TThreadDictionary<TKey, TValue>.GetCount: integer;
begin
  Lock;
  Try
    result := FContainer.Count;
  Finally
    Unlock;
  End;
end;

procedure TThreadDictionary<TKey, TValue>.Remove(const Key: TKey);
begin
  Lock;
  try
    FContainer.Remove(Key);
  finally
    Unlock;
  end;
end;

function TThreadDictionary<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
begin
  Lock;
  try
    result := FContainer.ToArray;
  finally
    Unlock;
  end;
end;

procedure TThreadDictionary<TKey, TValue>.TrimExcess;
begin
  Lock;
  try
    FContainer.TrimExcess;
  finally
    Unlock;
  end;
end;

function TThreadDictionary<TKey, TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
begin
  Lock;
  try
    result := FContainer.TryGetValue(Key, Value);
  finally
    Unlock;
  end;
end;

{ TLockable<T> }

destructor TLockable<T>.Destroy;
begin
  Lock; // Make sure nobody else is inside the list.
  try
    FreeAndNil(FContainer);
    inherited Destroy;
  finally
    Unlock;
  end;
end;

function TLockable<T>.Lock: T;
begin
  if not TMonitor.Enter(self, TLockConfiguration.GetLockTimeout) then
    raise EMonitorLockTimout.Create('TMonitor.Enter Timeout! Possible Deadlock!');
  result := FContainer;
end;

class constructor TLockConfiguration.Create;
begin
  // Set the timeout to a default
  FLockTimeout := LOCK_TIMEOUT;
end;

class function TLockConfiguration.GetLockTimeout: Cardinal;
begin
  result := FLockTimeout;
end;

class procedure TLockConfiguration.SetLockTimout(AValue: Cardinal);
begin
  if AValue <> FLockTimeout then
    FLockTimeout := AValue;
end;

procedure TLockable<T>.Unlock;
begin
  TMonitor.Exit(self);
end;

{ TThreadObjectDictionary<TKey, TValue> }

constructor TThreadObjectDictionary<TKey, TValue>.Create(Ownerships: TDictionaryOwnerships; ACapacity: integer);
begin
  Create(Ownerships, ACapacity, nil);
end;

constructor TThreadObjectDictionary<TKey, TValue>.Create(Ownerships: TDictionaryOwnerships; const AComparer: IEqualityComparer<TKey>);
begin
  Create(Ownerships, 0, AComparer);
end;

constructor TThreadObjectDictionary<TKey, TValue>.Create(Ownerships: TDictionaryOwnerships; ACapacity: integer; const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(ACapacity, AComparer);

  FContainer.OnKeyNotify := KeyNotify;
  FContainer.OnValueNotify := ValueNotify;

  if doOwnsKeys in Ownerships then
  begin
    if (TypeInfo(TKey) = nil) or (PTypeInfo(TypeInfo(TKey))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(@SInvalidCast);
  end;

  if doOwnsValues in Ownerships then
  begin
    if (TypeInfo(TValue) = nil) or (PTypeInfo(TypeInfo(TValue))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(@SInvalidCast);
  end;
  FOwnerships := Ownerships;
end;

procedure TThreadObjectDictionary<TKey, TValue>.KeyNotify(ASender: TObject; const Key: TKey; Action: TCollectionNotification);
begin
  // No lock needed here - this is the direct result of an already locked remove/extract/clear Operation
  if (Action = cnRemoved) and (doOwnsKeys in FOwnerships) then
  begin
    PObject(@Key)^.DisposeOf;
  end;
end;

procedure TThreadObjectDictionary<TKey, TValue>.ValueNotify(ASender: TObject; const Value: TValue; Action: TCollectionNotification);
begin
  // No lock needed here - this is the direct result of an already locked remove/extract/clear Operation
  if (Action = cnRemoved) and (doOwnsValues in FOwnerships) then
  begin
    PObject(@Value)^.DisposeOf;
  end;
end;

{ TThreadList<T> }

procedure TThreadList<T>.Add(const Item: T);
begin
  Lock;
  try
    if (Duplicates = dupAccept) or (FContainer.IndexOf(Item) = -1) then
      FContainer.Add(Item)
    else if Duplicates = dupError then
      raise EListError.CreateFmt(SDuplicateItem, [FContainer.ItemValue(Item)]);
  finally
    Unlock;
  end;
end;

procedure TThreadList<T>.AddRange(const Values: array of T);
begin
  Lock;
  try
    FContainer.AddRange(Values);
  finally
    Unlock;
  end;
end;

procedure TThreadList<T>.AddRange(const Collection: IEnumerable<T>);
begin
  Lock;
  try
    FContainer.AddRange(Collection);
  finally
    Unlock;
  end;
end;

procedure TThreadList<T>.AddRange(const Collection: TEnumerable<T>);
begin
  Lock;
  try
    FContainer.AddRange(Collection);
  finally
    Unlock;
  end;
end;

procedure TThreadList<T>.Clear;
begin
  Lock;
  try
    FContainer.Clear;
  finally
    Unlock;
  end;
end;

function TThreadList<T>.Contains(const Value: T): Boolean;
begin
  Lock;
  try
    result := FContainer.Contains(Value);
  finally
    Unlock;
  end;
end;

function TThreadList<T>.Count: integer;
begin
  Lock;
  try
    result := FContainer.Count;
  finally
    Unlock;
  end;
end;

constructor TThreadList<T>.Create;
begin
  inherited Create;
  FDuplicates := dupIgnore;
  FContainer := TList<T>.Create;
end;

function TThreadList<T>.GetItem(AIndex: integer): T;
begin
  Lock;
  try
    result := FContainer[AIndex];
  finally
    Unlock;
  end;

end;

procedure TThreadList<T>.Remove(const Item: T);
begin
  RemoveItem(Item, TDirection.FromBeginning);
end;

procedure TThreadList<T>.RemoveItem(const Item: T; Direction: TDirection);
begin
  Lock;
  try
    FContainer.RemoveItem(Item, Direction);
  finally
    Unlock;
  end;

end;

end.
