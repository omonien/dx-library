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
    class function InjectMetaData(AJsonObject: TJSONObject; AClassName: string): TJSONObject;
    class function RemoveMetaData(AJsonObject: TJSONObject): TJSONObject;
    class procedure RegisterConverters(AMarshaler:TJSONMarshal);
  public
    class function ObjectToJson(AObject: TObject): string;
    class function JsonToObject<T: class>(AJson: string): T;
  end;

implementation

uses
  Data.DBXPlatform, Data.DBXJsonCommon, Rtti,
  DX.Types.Nullable;

// Create JsonRelect meta data block. This is sort of a hack, as DBXJsonReflect relies on Metadata
// We are processing "plain" Json though - without any meta data
class function TDXJson.InjectMetaData(AJsonObject: TJSONObject; AClassName: string): TJSONObject;
var
  LJSONObject: TJSONObject;
  LValue: TJSONString;
  LPair: TJSONPair;
  LInnerObject: TJSONObject;
  LFieldName: string;
  LContext: TRTTIContext;
  LType: TRttiType;
  LField: TRttiField;
  LClassNameOfInnerObject: string;
  LClassname: string;
  i: Integer;
begin
  // Check inner objects recursively
  for LPair in AJsonObject do
  begin
    if LPair.JsonValue is TJSONObject then
    begin
      LFieldName := LPair.JsonString.Value;
      LContext := TRTTIContext.Create;
      LClassname := AClassName;
      // Is this a Generic, specifically Nullable<something> ?
      i := Pos('<', LClassname);
      if i > 0 then
      begin
        delete(LClassname, 1, i);
        // if there is a < then there is also a >
        i := Pos('>', LClassname);
        delete(LClassname, i, maxint);
      end;

      LType := LContext.FindType(LClassname);
      LField := LType.GetField(LFieldName);
      LClassNameOfInnerObject := LField.FieldType.QualifiedName;
      LInnerObject := LPair.JsonValue as TJSONObject;
      LPair.JsonValue := InjectMetaData(LInnerObject, LClassNameOfInnerObject);
    end;
  end;

  // Outer object
  LJSONObject := TJSONObject.ParseJSONValue('{"type":"","id":0,"fields":{}}') as TJSONObject;
  LValue := TJSONString.Create(AClassName);
  LJSONObject.Get('type').JsonValue := LValue;
  // Assign actual payload
  LJSONObject.Get('fields').JsonValue := AJsonObject;
  result := LJSONObject;
end;

// Remove Metadata
class procedure TDXJson.RegisterConverters(AMarshaler:TJSONMarshal);
begin
(*
  AMarshaler.RegisterConverter(DXTypes.Nullable.Nullable<T>, Function(Data: TObject; Field: string):TObject
  begin

  end
  );

  *)
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
  Converter: TJSONMarshal;
  LJSONObject: TJSONObject;
begin
  Converter := TJSONMarshal.Create(TJSONConverter.Create);
  RegisterConverters(Converter);
  LJSONObject := Converter.Marshal(AObject) as TJSONObject;
  LJSONObject := RemoveMetaData(LJSONObject);
  result := LJSONObject.ToString;
end;

class function TDXJson.JsonToObject<T>(AJson: string): T;
var
  Reverter: TJSONUnMarshal;
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
  LJSONObject := InjectMetaData(LJSONObject, T.QualifiedClassName);

  Reverter := TJSONUnMarshal.Create;
  result := Reverter.Unmarshal(LJSONObject) as T;
end;

end.
