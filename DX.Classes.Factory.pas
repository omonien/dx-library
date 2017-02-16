{$REGION 'Documentation'}
/// <summary>
///   This unit implements the Factory Pattern and uses the Singleton Pattern. <br /><br />
///   To use the factory: <br /><list type="number">
///     <item>
///       Register classes using DXFactory.RegisterClassForInterface(TFoo,
///       IFoo); <br />This should be done in the initialization part of
///       the unit where TFoo is implemented. <br />TFoo has to implement
///       IFoo, where IFoo should be defined in a separate interface unit.
///     </item>
///     <item>
///       Create instances of classes using LFoo :=
///       DXFactory.CreateNew&lt;IFoo&gt;;
///     </item>
///   </list>
/// </summary>
/// <license>MIT</license>
/// <copyright>www.developer-experts.net</copyright>
{$ENDREGION}
unit DX.Classes.Factory;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti;


type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Dictionary-based registry type to hold Interface/Class pairs
  /// </summary>
  {$ENDREGION}
  TClassRegistry = TDictionary<TGUID, TClass>;

  {$REGION 'Documentation'}
  /// <summary>
  ///   Factory class, available as singleton, to create instances of classes,
  ///   that implement requested interfaces.
  /// </summary>
  {$ENDREGION}
  TDXFactory = class(TObject)
  strict private
    FRegisteredClasses: TClassRegistry;
  private
    class var FInstance: TDXFactory;
    class constructor Create;
    class destructor Destroy;
  private
    FRTTI: TRttiContext;
  public
    {$REGION 'Documentation'}
    /// <summary>
    ///   Do not call the constructor manually. Use DXFactory or
    ///   TDXFactory.Instance instead.
    /// </summary>
    {$ENDREGION}
    constructor Create;
    destructor Destroy; override;
    function Instance: TDXFactory;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates a new instance of the class that has been registered for
    ///   implementing I
    /// </summary>
    {$ENDREGION}
    function CreateNew<I: IInterface>: I;
    {$REGION 'Documentation'}
    /// <summary>
    ///   Registers AClass
    /// </summary>
    {$ENDREGION}
    procedure RegisterClassForInterface(AClass: TClass; AInterface: TGUID);
  end;

{$REGION 'Documentation'}
/// <summary>
///   Convenience access to the singleton instance of TDXFactory
/// </summary>
{$ENDREGION}
function DXFactory: TDXFactory;

implementation

uses
  System.TypInfo;

function DXFactory: TDXFactory;
begin
  result := TDXFactory.FInstance;
end;

{ TDXFactory }

class constructor TDXFactory.Create;
begin
  // No lazy init to avoid thread issues
  FInstance := TDXFactory.Create;
end;

constructor TDXFactory.Create;
begin
  inherited;
  FRegisteredClasses := TClassRegistry.Create;
  FRTTI := TRttiContext.Create;
end;

function TDXFactory.CreateNew<I>: I;
var
  LClass: TClass;
  LClassType: TRttiType;
  LInstance: TValue;
  LInterfaceGUID: TGUID;
  LInterface: IInterface;
  LObject: TObject;
begin
  //T is an interface (ensured by its constraint), but we need its GUID
  LInterfaceGUID := GetTypeData(TypeInfo(I))^.Guid;
  if FRegisteredClasses.TryGetValue(LInterfaceGUID, LClass) then
  begin
    if supports(LClass, LInterfaceGUID) then
    begin
      LClassType := FRTTI.GetType(LClass);
      //We call the classes standard constructor
      //Todo: implement constructors with one or more parameters (e.g. TComponent(AOwner: TObject)
      LInstance := LClassType.GetMethod('Create').Invoke(LClassType.AsInstance.MetaclassType, []);
      //use "Supports()" to convert the instance into its interface I
      Supports(LInstance.AsObject, LInterfaceGUID, result);
    end
    else
      raise Exception.Create(LClass.ClassName + ' does not implement ' + GetTypeName(TypeInfo(I)) + '!');
  end
  else
  begin
    raise Exception.Create('No implementation for ' + GetTypeName(TypeInfo(I)) + '  found!');
  end;
end;

class destructor TDXFactory.Destroy;
begin
  FreeAndNil(FInstance);
end;

destructor TDXFactory.Destroy;
begin
  FRTTI.Free;
  FreeAndNil(FInstance);
  FreeAndNil(FRegisteredClasses);
  inherited;
end;

function TDXFactory.Instance: TDXFactory;
begin
  result := FInstance;
end;

procedure TDXFactory.RegisterClassForInterface(AClass: TClass; AInterface: TGUID);
begin
  FRegisteredClasses.AddOrSetValue(AInterface, AClass);
end;

end.
