/// <summary>
/// This unit implements a simple "ObjectFactory", which can be used to
/// implement account keeping on created instances.
/// </summary>
unit DX.Classes.ObjectFactory;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TObjectFactoryInstance = class(TObject)
  private
    FTrackedObjects: TThreadList<TObject>;
  public
    constructor Create; overload;
    destructor Destroy; override;
    /// <summary>
    /// Create&lt;T&gt; is the actual Factory Method, and is NOT the constructor of TObjectFactoryClass
    /// </summary>
    /// <typeparam name="T">
    /// The class type to be constructed.
    /// </typeparam>
    function Create<T: class, constructor>: T; overload;
    /// <summary>
    ///   Adds an already existing object instance to the tracked objects, so
    ///   that its life cycle will be tracked. Duplicates are ignored.
    /// </summary>
    /// <param name="AInstance">
    ///   An object to be tracked.
    /// </param>
    function Add(AInstance:TObject):TObject;
  end;

  IObjectFactory = Interface(IInterface)
    ['{35CC2663-9B4E-49A3-99CA-C5488483A8EA}']
    function Instance: TObjectFactoryInstance;

  end;

  /// <summary>
  /// Technically, TObjectFactory does not implement the factory pattern, as
  /// the actual class type is passed as type parameter. The class produces
  /// instances and keeps track of them, so that when destroying the factory,
  /// all of its created class-instances will be destroyed as well.
  /// </summary>
  /// <example>
  /// <para>
  /// First, create a Factory. TObjectFactory is interfaced, thus it will
  /// destroy itself, once the scope its left. <br /><br />F :=
  /// TObjectFactory.Create;
  /// </para>
  /// <para>
  /// Then, use the Factory to create instances of your custom classes.
  /// All instances will be tracked, and automatically destroyed again,
  /// once your Factory goes out of scope.
  /// </para>
  /// <para>
  /// LFoo := F.Instance.Create&lt;TFoo&gt;;
  /// </para>
  /// </example>
  TObjectFactory = class(TInterfacedObject, IObjectFactory)
  private
    FFactory: TObjectFactoryInstance;
  public
    constructor Create;
    destructor Destroy; override;
    function Instance: TObjectFactoryInstance;
  end;

implementation

{ TObjectManager }

function TObjectFactory.Instance: TObjectFactoryInstance;
begin
  result := FFactory;
end;

constructor TObjectFactory.Create;
begin
  inherited;
  FFactory := TObjectFactoryInstance.Create;
end;

destructor TObjectFactory.Destroy;
begin
  FreeAndNil(FFactory);
  inherited;
end;

function TObjectFactoryInstance.Add(AInstance: TObject): TObject;
begin
  //Dupes are ignored
  FTrackedObjects.Add(AInstance);
  result := AInstance;
end;

constructor TObjectFactoryInstance.Create;
begin
  inherited;
  FTrackedObjects:= TThreadList<TObject>.Create;
end;

destructor TObjectFactoryInstance.Destroy;
var
  LList: TList<TObject>;
  LObject: TObject;
begin
  LList := FTrackedObjects.LockList;
  try
    for LObject in LList do
    begin
      LObject.Free;
    end;
  finally
    FTrackedObjects.UnlockList;
  end;
  FreeAndNil(FTrackedObjects);
  inherited;
end;

{ TObjectFactoryClass }

function TObjectFactoryInstance.Create<T>: T;
begin
  result := T.Create;
  FTrackedObjects.Add(result);
end;

end.
