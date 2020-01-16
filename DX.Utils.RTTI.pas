/// <summary>
/// This unit provides RTTI based utility classes/functions
/// </summary>
unit DX.Utils.RTTI;

interface

uses
  System.Classes, System.SysUtils, System.RTTI, DX.Classes.Strings;

type
  EConstructorNotFound = class(Exception);

  /// <summary>
  /// Offers helper methods for TObject
  /// </summary>
  TObjectHelper = class helper for TObject
    /// <summary>
    /// Lists all properties of the given class instance as 'name = value'
    /// pairs.
    /// </summary>
    /// <param name="AExclude">
    /// Excludes all pairs that match an item in AExcludes
    /// </param>
    function ListProperties(const AExcludes: TArray<string>): StringList; overload;
    /// <summary>
    ///   Lists all properties of the given class instance as 'name = value'
    /// </summary>
    function ListProperties: StringList; overload;
  end;

  TClassConstructor = class
  private
    /// <summary>
    /// Utility class that has the ability to construct a given class (T) by
    /// calling its parameter-less constructor. If T has no such constructor,
    /// then an EConstructorNotFound exception is raised.
    /// </summary>
    class function GetConstructor(AClass: TRttiType): TRttiMethod;
  public
    /// <summary>
    /// Creates and returns an instance of T
    /// </summary>
    class function Construct<T: Class>: T;

    /// <summary>
    /// Creates a deep copy of AInstance, by Marshaling/Unmarshaling
    /// it to/from Json.
    /// </summary>
    class function Clone(AInstance: TObject): TObject;
  end;

implementation

uses
  System.Json,
  Data.DBXJsonReflect;

class function TClassConstructor.Clone(AInstance: TObject): TObject;
var
  LMarshalObj: TJSONMarshal;
  LUnMarshalObj: TJSONUnMarshal;
  LJSONValue: TJSONValue;
begin
  Result := nil;
  LMarshalObj := TJSONMarshal.Create;
  LUnMarshalObj := TJSONUnMarshal.Create;
  try
    LJSONValue := LMarshalObj.Marshal(AInstance);
    try
      if Assigned(LJSONValue) then
        Result := LUnMarshalObj.Unmarshal(LJSONValue);
    finally
      LJSONValue.Free;
    end;
  finally
    LMarshalObj.Free;
    LUnMarshalObj.Free;
  end;
end;

class function TClassConstructor.GetConstructor(AClass: TRttiType): TRttiMethod;
var
  LMethods: TArray<TRttiMethod>;
  LMethod: TRttiMethod;
  LParams: TArray<TRttiParameter>;
begin
  Result := nil;
  LMethods := AClass.GetMethods('Create');
  for LMethod in LMethods do
  begin
    LParams := LMethod.GetParameters();
    // Look for parameter-less constructor
    if (Length(LParams) = 0) then
    begin
      Result := LMethod;
      break;
    end;
  end;
  if not Assigned(Result) then
    raise EConstructorNotFound.CreateFmt('Class %s has no parameter-less constructor!', [AClass.QualifiedName]);
end;

{ TClassConstructor }

class function TClassConstructor.Construct<T>: T;
var
  LInstance: TValue;
  LContext: TRttiContext;
  // LClass: TRttiType;
  LClassType: TRttiType;
  LConstructor: TRttiMethod;
begin
  LContext := TRttiContext.Create();
  // LClass := LContext.GetType(TypeInfo(T));
  LClassType := LContext.GetType(T);
  LConstructor := GetConstructor(LClassType);
  LInstance := LConstructor.Invoke(LClassType.AsInstance.MetaclassType, []);

  Result := LInstance.AsObject as T;
end;

{ TObjectHelper }

function TObjectHelper.ListProperties(const AExcludes: TArray<string>): StringList;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProperties: TArray<TRttiProperty>;
  LProperty: TRttiProperty;
begin
  result.Clear;
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(self.ClassType);
    LProperties := LType.GetProperties;
    for LProperty in LProperties do
    begin
      if not(AExcludes.Contains(LProperty.Name)) then
      begin
        Result.Add(LProperty.Name + ' = ' + LProperty.GetValue(self).ToString);
      end;
    end;
  finally
    LContext.Free;
  end;
end;

function TObjectHelper.ListProperties: StringList;
begin
  Result := self.ListProperties([]);
end;

end.
