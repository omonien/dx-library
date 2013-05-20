unit DX.Utils.Json;

/// <summary>
/// DX.Utils.Json provides helper methods for JSON Handling and depends on
/// Embaracdero's Data.DBXJson
/// </summary>
/// <remarks>
/// DX.Utils.Json is part of DX.Librayry<br />See:
/// http://code.google.com/p/dx-library/
/// </remarks>

interface

uses
  Data.DBXJson, Data.DBXJsonReflect;

type
  TDXJson = class(TObject)
  private
    class var FID: Integer;
    class function InjectMetaData(AJsonObject: TJSONObject; AClassName: string): TJSONObject;
    class function RemoveMetaData(AJsonObject: TJSONObject): TJSONObject;
    class procedure RegisterConverters(AMarshaler: TJSONMarshal);
    class procedure RegisterReverters(AUnMarshaler: TJSONUnMarshal); static;
  public
    class function ObjectToJson(AObject: TObject): string;
    class function JsonToObject<T: class>(AJson: string): T;
    class function GetNextID: Integer;
  end;

implementation

uses
  Data.DBXPlatform, Data.DBXJsonCommon, Rtti,
  DX.Types.Nullable,
  System.DateUtils,
  System.SysUtils;


class function TDXJson.GetNextID: Integer;
begin
  result := AtomicIncrement(FID);
end;


// Create JsonReflect meta data block. This is sort of a hack, as DBXJsonReflect relies on Metadata
// We are processing "plain" Json though - without any meta data
class function TDXJson.InjectMetaData(AJsonObject: TJSONObject; AClassName: string): TJSONObject;
var
  LJSONObject: TJSONObject;
  LValue: TJSONString;
  LPair: TJSONPair;
  LInnerObject: TJSONObject;
  LClassNameOfInnerObject: string;

  i: Integer;
  LObjectID: Integer;
  LArray: TJSONArray;
  LCount: Integer;
  LJsonValue: TJSONValue;
  LClassNameOfArrayItem: string;
  LNewArray: TJSONArray;

  // If ATypeName is a Generic tpye such as Nullable<T> or TObjectList<T> then return T
  // otherwise return unchanged
  function InnerGenericType(const ATypeName: string): string;
  var
    LTypeName: string;
  begin
    LTypeName := ATypeName;
    i := Pos('<', LTypeName);
    if i > 0 then
    begin
      delete(LTypeName, 1, i);
      // if there is a < then there is also a > (and we want the last one, in case this is a nested generic type...
      i := LTypeName.LastIndexOf('>');
      if i > 0 then
        LTypeName := LTypeName.Remove(i, 1);
    end;
    // If AQualified, then return type as is, else return unqualified type name
    if true then
      result := LTypeName
    else
    begin
      i := LTypeName.LastIndexOf('.');
      if i >= 0 then
      begin
        result := LTypeName.Remove(0, i + 1);
      end;
    end;
  end;

  function TypeOfInnerObject: TRttiType;
  var
    LFieldName: string;
    LClassname: string;
    LContext: TRTTIContext;
    LType: TRttiType;
    LField: TRttiField;
  begin
    LFieldName := LPair.JsonString.Value;
    LContext := TRTTIContext.Create;
    LClassname := AClassName;

    LClassname := InnerGenericType(LClassname);

    LType := LContext.FindType(LClassname);
    LField := LType.GetField(LFieldName);
    result := LField.FieldType;
  end;

begin
  // Check inner arrays and objects recursively
  for LPair in AJsonObject do
  begin
    if LPair.JsonValue is TJSONArray then

    begin
      // If an Array exists in JSON, then its Delphi counterpart needs to be
      // a generic container type such as TObjectlist<T>
      // Type T is then the type of the array's items
      LClassNameOfArrayItem := InnerGenericType(TypeOfInnerObject.QualifiedName);

      LArray := TJSONArray(LPair.JsonValue);
      //The arry can not easily be changed in place, so we are coping to a new one
      LNewArray := TJSONArray.Create;
      for i := 0 to LArray.Size - 1 do
      begin
        LJsonValue := LArray.Get(i);
        // Todo: This could be an Object OR an Array
        if LJsonValue is TJSONObject then
        begin
          LJsonValue := InjectMetaData(TJSONObject(LJsonValue), LClassNameOfArrayItem);
          LNewArray.AddElement(LJsonValue);
        end;
      end;
      LPair.JsonValue := TJsonValue(LNewArray);

    end
    else if LPair.JsonValue is TJSONObject then
    begin
      LClassNameOfInnerObject := TypeOfInnerObject.QualifiedName;
      LInnerObject := LPair.JsonValue as TJSONObject;
      LPair.JsonValue := InjectMetaData(LInnerObject, LClassNameOfInnerObject);
    end;
  end;

  // Outer object
  LObjectID := GetNextID; // We really don't care about object references in this scenario
  LJSONObject := TJSONObject.ParseJSONValue(Format('{"type":"","id":%d,"fields":{}}', [LObjectID])) as TJSONObject;
  LValue := TJSONString.Create(AClassName);
  LJSONObject.Get('type').JsonValue := LValue;
  // Assign actual payload
  LJSONObject.Get('fields').JsonValue := AJsonObject;
  result := LJSONObject;
end;

class procedure TDXJson.RegisterConverters(AMarshaler: TJSONMarshal);

begin
  AMarshaler.RegisterConverter(TObject,
    function(Data: TObject): TObject
    begin
      if Assigned(Data) then
      begin
      end;
      (*
        Person := TPerson(Data);
        if Person.FAddress.FDescription <> nil then
        Count := Person.FAddress.FDescription.Count
        else
        Count := 0;
        SetLength(Result, Count + 4);
        Result[0] := Person.FAddress.FStreet;
        Result[1] := Person.FAddress.FCity;
        Result[2] := Person.FAddress.FCode;
        Result[3] := Person.FAddress.FCountry;
        for I := 0 to Count - 1 do
        Result[4+I] := Person.FAddress.FDescription[I];
      *)
    end);
end;

class procedure TDXJson.RegisterReverters(AUnMarshaler: TJSONUnMarshal);
var
  LDate: TDateTime;
begin
  AUnMarshaler.RegisterReverter(nil,
    function(Data: TObject): TObject
    begin
      if Assigned(Data) then
      begin
      end;
      (*
        Person := TPerson(Data);
        if Person.FAddress.FDescription <> nil then
        Count := Person.FAddress.FDescription.Count
        else
        Count := 0;
        SetLength(Result, Count + 4);
        Result[0] := Person.FAddress.FStreet;
        Result[1] := Person.FAddress.FCity;
        Result[2] := Person.FAddress.FCode;
        Result[3] := Person.FAddress.FCountry;
        for I := 0 to Count - 1 do
        Result[4+I] := Person.FAddress.FDescription[I];
      *)
    end);
end;

class function TDXJson.RemoveMetaData(AJsonObject: TJSONObject): TJSONObject;
var
  LCleaned: TJSONObject;
  LPair: TJSONPair;
begin
  LCleaned := AJsonObject.Get('fields').JsonValue as TJSONObject;
  for LPair in LCleaned do
  begin
    if LPair.JsonValue is TJSONObject then
      LPair.JsonValue := RemoveMetaData(LPair.JsonValue as TJSONObject);
  end;
  result := LCleaned;
end;

class function TDXJson.ObjectToJson(AObject: TObject): string;
var
  LMarshaler: TJSONMarshal;
  LJSONObject: TJSONObject;
begin
  LMarshaler := TJSONMarshal.Create(TJSONConverter.Create);
  // RegisterConverters(LMarshaler);
  LJSONObject := LMarshaler.Marshal(AObject) as TJSONObject;
  LJSONObject := RemoveMetaData(LJSONObject);
  result := LJSONObject.ToString;
end;

class function TDXJson.JsonToObject<T>(AJson: string): T;
var
  LUnMarshaler: TJSONUnMarshal;
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
  LJSONObject := InjectMetaData(LJSONObject, T.QualifiedClassName);

  LUnMarshaler := TJSONUnMarshal.Create;
  // RegisterReverters(LUnMarshaler);

  result := LUnMarshaler.Unmarshal(LJSONObject) as T;
end;

end.
