unit DX.SysUtils;

interface

uses
  System.Classes, System.SysUtils;

type
{$SCOPEDENUMS ON}
{$REGION 'Documentation'}
  /// <summary>
  /// Implemented Hash algorithms.
  /// </summary>
  /// <remarks>
  /// IMPORTANT: MD5 and SHA1 are both UNSAFE in terms of cryptography. Use
  /// for checksums etc only!
  /// </remarks>
{$ENDREGION}
  THashAlgorithm = (MD5, SHA1, SHA256, SHA512);
{$SCOPEDENUMS OFF}

  THash = class(TObject)
  protected
  public
{$REGION 'Documentation'}
    /// <summary>
    /// returns hash/checksum for a stream.
    /// </summary>
{$ENDREGION}
    class function Hash(const AStream: TStream; AAlgorithm: THashAlgorithm): string; overload;
{$REGION 'Documentation'}
    /// <summary>
    /// returns hash/checksum for a string.
    /// </summary>
{$ENDREGION}
    class function Hash(const AValue: string; AAlgorithm: THashAlgorithm): string; overload;
{$REGION 'Documentation'}
    /// <summary>
    /// returns hash/checksum for a file.
    /// </summary>
{$ENDREGION}
    class function HashForFile(const AFileName: string; AAlgorithm: THashAlgorithm): string;
{$REGION 'Documentation'}
    /// <summary>
    /// MD5 Hash for a file.
    /// </summary>
{$ENDREGION}
    class function MD5ForFile(const AFileName: string): string;
{$REGION 'Documentation'}
    /// <summary>
    /// SHA1 Hash for a file.
    /// </summary>
{$ENDREGION}
    class function SHA1ForFile(const AFileName: string): string;
  end;

implementation

uses
  IdHash, IdHashMessageDigest, IdHashSHA;

class function THash.Hash(const AStream: TStream; AAlgorithm: THashAlgorithm): string;
var
  LHash: TIdHash;
begin
  LHash := nil;
  try
    case AAlgorithm of
      THashAlgorithm.MD5:
        LHash := TIdHashMessageDigest5.Create;
      THashAlgorithm.SHA1:
        LHash := TIdHashSHA1.Create;
      THashAlgorithm.SHA256:
        LHash := TIdHashSHA256.Create;
      THashAlgorithm.SHA512:
        LHash := TIdHashSHA512.Create;
    end;
    result := LHash.HashStreamAsHex(AStream);
  finally
    FreeAndNil(LHash);
  end;

end;

class function THash.Hash(const AValue: string; AAlgorithm: THashAlgorithm): string;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create(AValue);
  try
    result := Hash(LStream, AAlgorithm);
  finally
    FreeAndNil(LStream);
  end;

end;

class function THash.HashForFile(const AFileName: string; AAlgorithm: THashAlgorithm): string;
var
  LStream: TFileStream;
begin
  LStream := nil;
  try
    if FileExists(AFileName) then
    begin
      LStream := TFileStream.Create(AFileName, fmOpenRead OR fmShareDenyWrite);
      result := Hash(LStream, AAlgorithm);
    end
    else
    begin
      result := '';
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

class function THash.MD5ForFile(const AFileName: string): string;
begin
  result := HashForFile(AFileName, THashAlgorithm.MD5);
end;

class function THash.SHA1ForFile(const AFileName: string): string;
begin
  result := HashForFile(AFileName, THashAlgorithm.SHA1);
end;

end.
