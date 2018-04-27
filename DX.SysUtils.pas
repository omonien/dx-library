unit DX.SysUtils;

interface

uses
  System.Classes, System.SysUtils;

type
  THash = class(TObject)
  public
   class function MD5(const fileName: string): string;
  end;

implementation

uses
  IdHashMessageDigest, IdHash;

// returns MD5 has for a file
class function THash.MD5(const fileName: string): string;
var
  LDigest: TIdHashMessageDigest5;
  LStream: TFileStream;
begin
  LDigest := nil;
  LStream := nil;
  try
    LDigest := TIdHashMessageDigest5.Create;
    LStream := TFileStream.Create(fileName, fmOpenRead OR fmShareDenyWrite);
    result := LDigest.HashStreamAsHex(LStream);
  finally
    FreeAndNil(LStream);
    FreeAndNil(LDigest);
  end;
end;

end.
