unit DX.Sparkle.Utils;

interface

uses
  System.Classes, System.SysUtils;

type
  TBytesHelper = record helper for TBytes
    function ToRawBytesString: RawByteString;
  end;

  TRawByteStringHelper = record helper for RawByteString
    function ToBytes: TBytes;
  end;

function Encode(const AInput: string): RawByteString;
function Decode(
  const AInput:    RawByteString;
  const AContentType: string): String; overload;
function Decode(
  const AInput:    TBytes;
  const AContentType: string): String; overload;

implementation

function Encode(const AInput: string): RawByteString;
begin
  result := TEncoding.UTF8.GetBytes(AInput).ToRawBytesString;
end;

function Decode(
  const AInput:    RawByteString;
  const AContentType: string): String;
begin
  result := Decode(AInput.ToBytes, AContentType);
end;

function Decode(
  const AInput:    TBytes;
  const AContentType: string): String;
var
  LEncoding: TEncoding;
begin
  if AContentType.ToUpper.contains('UTF-8') then
  begin
    result := TEncoding.UTF8.GetString(AInput);
  end
  else
  begin
    LEncoding := nil;
    TEncoding.GetBufferEncoding(AInput, LEncoding);
    result := LEncoding.GetString(AInput);
  end;

end;

{ TEncodingHelper }

function TBytesHelper.ToRawBytesString: RawByteString;
begin
  SetLength(result, Length(self));
  if Length(self) > 0 then
  begin
    Move(self[0], result[1], Length(self));
  end;
end;

{ TRawByteStringHelper }

function TRawByteStringHelper.ToBytes: TBytes;
var
  LResult: TBytes;
begin
  SetLength(LResult, Length(self));
  if Length(self) > 0 then
  begin
    Move(self[1], LResult[0], Length(self));
  end;
  result := LResult;
end;

end.
