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
  /// Offers helper methods for TObject, mainly for simplified property and attribute access
  /// </summary>
  TObjectHelper = class helper for TObject
    /// <summary>
    /// Lists all properties of the given class instance as 'name = value'
    /// pairs.
    /// </summary>
    /// <param name="AExcludes">
    /// Excludes all pairs that match an item in AExcludes
    /// </param>
    function ListProperties(const AExcludes: TArray<string>): StringList; overload;
    /// <summary>
    /// Lists all properties of the given class instance as 'name = value'
    /// </summary>
    function ListProperties: StringList; overload;

    /// <summary>
    /// Returns true if the given class has the specified attribute attached.
    /// </summary>
    function HasAttribute(AAttribute: TClass): boolean;
    function AttributeValue(AAttribute: TClass): string;
    function GetAttribute(AAttribute: TClass): TCustomAttribute;

    function HasProperty(APropertyName: string): boolean;
    function GetProperty(APropertyName: string): TRttiProperty;
    procedure SetProperty(APropertyName: string; AValue: TValue);

    procedure CopyFrom(ASource: TObject);

  end;

  TRTTIPropertyHelper = class helper for TRttiProperty
  public
    /// <summary>
    /// Sets the value of the field, specified by AFieldName. Works with
    /// records!
    /// </summary>
    /// <param name="AInstance">
    /// The containing instance
    /// </param>
    /// <param name="AFieldName">
    /// Name of the field variable to set
    /// </param>
    /// <param name="AValue">
    /// A TValue containing the value to be set
    /// </param>
    procedure SetFieldValue(
      AInstance: Pointer;
      const AFieldName: string;
      AValue: TValue);
  end;

  TClassConstructor = class
  private
    /// <summary>
    /// Utility class that has the ability to construct a given class (T) by
    /// calling its parameter-less constructor. If T has no such constructor,
    /// then an EConstructorNotFound exception is raised.
    /// </summary>
    class function GetConstructor(AClass: TRTTIType): TRttiMethod;
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
  Data.DBXJsonReflect, DX.Classes.Attributes;

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

class function TClassConstructor.GetConstructor(AClass: TRTTIType): TRttiMethod;
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
  LClassType: TRTTIType;
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
  LType: TRTTIType;
  LProperties: TArray<TRttiProperty>;
  LProperty: TRttiProperty;
begin
  Result.Clear;
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(self.ClassType);
    LProperties := LType.GetProperties;
    for LProperty in LProperties do
    begin
      if not(AExcludes.Contains(LProperty.Name)) then
      begin
        var LValue := '';
        if LProperty.IsReadable then
        begin
          LValue := ' = ' + LProperty.GetValue(self).ToString;
        end;
        Result.Add(LProperty.Name + LValue);
      end;
    end;
    Result.Sort;
  finally
    LContext.Free;
  end;
end;

function TObjectHelper.AttributeValue(AAttribute: TClass): string;
var
  LAttribute: StringValueAttribute;
begin
  Result := '';
  if AAttribute.InheritsFrom(StringValueAttribute) then
  begin
    LAttribute := StringValueAttribute(GetAttribute(AAttribute));
    if Assigned(LAttribute) then
    begin
      Result := LAttribute.Value;
    end;
  end;
end;

procedure TObjectHelper.CopyFrom(ASource: TObject);
var
  LContext: TRttiContext;
  LSelfType: TRTTIType;
  LSelfProperties: TArray<TRttiProperty>;
  LSelfProperty: TRttiProperty;
  LSourceType: TRTTIType;
  LSourceProperties: TArray<TRttiProperty>;
  LSourceProperty: TRttiProperty;
  LSourceValue: TValue;

begin
  if Assigned(ASource) then
  begin
    LContext := TRttiContext.Create;
    try
      LSourceType := LContext.GetType(ASource.ClassType);
      LSourceProperties := LSourceType.GetProperties;
      LSelfType := LContext.GetType(self.ClassType);
      LSelfProperties := LSelfType.GetProperties;

      for LSelfProperty in LSelfProperties do
      begin
        if LSelfProperty.IsWritable then
        begin
          // Find in source and copy if type matches
          for LSourceProperty in LSourceProperties do
          begin
            if (LSourceProperty.IsReadable)
              and (LSourceProperty.Name = LSelfProperty.Name)
              and (LSourceProperty.PropertyType = LSelfProperty.PropertyType)
            then
            begin
              LSourceValue := LSourceProperty.GetValue(ASource);
              LSelfProperty.SetValue(self, LSourceValue);
              break;
            end;
          end;
        end;
      end;
    finally
      LContext.Free;
    end;
  end;
end;

function TObjectHelper.GetAttribute(AAttribute: TClass): TCustomAttribute;
var
  LContext: TRttiContext;
  LConfigType: TRTTIType;
  LAttributes: TArray<TCustomAttribute>;
  LAttribute: TCustomAttribute;
begin
  Result := nil;
  LContext := TRttiContext.Create;
  try
    LConfigType := LContext.GetType(self.ClassType);
    LAttributes := LConfigType.GetAttributes;
    for LAttribute in LAttributes do
    begin
      if LAttribute.ClassType = AAttribute then
      begin
        Result := LAttribute;
        break;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

function TObjectHelper.GetProperty(APropertyName: string): TRttiProperty;
var
  LContext: TRttiContext;
  LConfigType: TRTTIType;
  LProperties: TArray<TRttiProperty>;
  LProperty: TRttiProperty;
begin
  Result := nil;
  LContext := TRttiContext.Create;
  try
    LConfigType := LContext.GetType(self.ClassType);
    LProperties := LConfigType.GetProperties;
    for LProperty in LProperties do
    begin
      if LProperty.Name.ToLower = APropertyName.ToLower then
      begin
        Result := LProperty;
        break;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

function TObjectHelper.HasAttribute(AAttribute: TClass): boolean;
var
  LContext: TRttiContext;
  LConfigType: TRTTIType;
  LAttributes: TArray<TCustomAttribute>;
  LAttribute: TCustomAttribute;
begin
  Result := false;
  LContext := TRttiContext.Create;
  try
    LConfigType := LContext.GetType(self.ClassType);
    LAttributes := LConfigType.GetAttributes;
    for LAttribute in LAttributes do
    begin
      if LAttribute is AAttribute then
      begin
        Result := true;
        break;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

function TObjectHelper.HasProperty(APropertyName: string): boolean;
begin
  Result := GetProperty(APropertyName) <> nil;
end;

function TObjectHelper.ListProperties: StringList;
begin
  Result := self.ListProperties([]);
end;

procedure TObjectHelper.SetProperty(APropertyName: string; AValue: TValue);
begin
  GetProperty(APropertyName).SetValue(self, AValue);
end;

{ TRTTIPropertyHelper }

procedure TRTTIPropertyHelper.SetFieldValue(
  AInstance: Pointer;
  const AFieldName: string;
  AValue: TValue);
var
  LField: TRTTIField;
  LValue: TValue;
begin
  // The value of the property, which is considered a record
  LValue := self.GetValue(AInstance);
  // A field of the record
  LField := self.PropertyType.GetField(AFieldName);
  if LField = nil then
    raise EInvalidCast.Create('Invalid type - field "%s" not found!');
  // set the field's value
  LField.SetValue(LValue.GetReferenceToRawData, AValue);
  // finally set the property value with the modified field value(s)
  self.SetValue(AInstance, LValue);
end;

end.
