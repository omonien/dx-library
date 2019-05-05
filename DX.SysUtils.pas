/// <summary>
/// DX.SysUtils provides several system level utility routines.
/// </summary>
unit DX.SysUtils;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils
{$IFDEF IOS}
    , Macapi.Helpers, iOSAPi.Helpers, iOSAPi.Foundation, iOSAPi.UIKit
{$ENDIF}
    ;

type
  /// <summary>
  /// Offers various class functions for easy Hash generation. Based on Indy
  /// implementations.
  /// </summary>
  THash = class(TObject)
  public type
    /// <summary>
    /// Available Hash algorithms.
    /// </summary>
    /// <remarks>
    /// IMPORTANT: MD5 and SHA1 are both UNSAFE in terms of cryptography. Use
    /// for checksums etc only!
    /// </remarks>
    TAlgorithm = (MD5, SHA1, SHA256, SHA512);

  public
    /// <summary>
    /// returns hash/checksum for a stream
    /// </summary>
    class function Hash(const AStream: TStream; AAlgorithm: TAlgorithm): string; overload;
    /// <summary>
    /// Returns hash/checksum for a string
    /// </summary>
    class function Hash(const AValue: string; AAlgorithm: TAlgorithm): string; overload;
    /// <summary>
    /// Returns hash/checksum for a file
    /// </summary>
    class function HashForFile(const AFileName: string; AAlgorithm: TAlgorithm): string;
    /// <summary>
    /// MD5 Hash for a file
    /// </summary>
    class function MD5ForFile(const AFileName: string): string;
    /// <summary>
    /// SHA1 Hash for a file
    /// </summary>
    class function SHA1ForFile(const AFileName: string): string;
  end;

  /// <summary>
  /// Attempts to open the resource at the specified URL asynchronously ,
  /// </summary>
  /// <param name="AUrl">
  /// A URL. Each platform may support many different common schemes, such as
  /// http, https, tel, facetime, and mailto or custom app specific schemes.
  /// </param>
  /// <remarks>
  /// Cuurently supports the iOS platform only.
  /// </remarks>
procedure OpenURL(const AUrl: string);

implementation

uses
  IdHash, IdHashMessageDigest, IdHashSHA;

procedure OpenURL(const AUrl: string);
{$IFDEF IOS}
var
  LUrl: NSURL;
{$ENDIF}
begin
{$IFDEF IOS}
  LUrl := StrToNSUrl(AUrl);
  // Async on iOS level
  TiOSHelper.SharedApplication.OpenURL(LUrl);
{$ENDIF}
end;

class function THash.Hash(const AStream: TStream; AAlgorithm: TAlgorithm): string;
var
  LHash: TIdHash;
begin
  LHash := nil;
  try
    case AAlgorithm of
      TAlgorithm.MD5:
        LHash := TIdHashMessageDigest5.Create;
      TAlgorithm.SHA1:
        LHash := TIdHashSHA1.Create;
      TAlgorithm.SHA256:
        LHash := TIdHashSHA256.Create;
      TAlgorithm.SHA512:
        LHash := TIdHashSHA512.Create;
    end;
    result := LHash.HashStreamAsHex(AStream);
  finally
    FreeAndNil(LHash);
  end;

end;

class function THash.Hash(const AValue: string; AAlgorithm: TAlgorithm): string;
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

class function THash.HashForFile(const AFileName: string; AAlgorithm: TAlgorithm): string;
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
  result := HashForFile(AFileName, TAlgorithm.MD5);
end;

class function THash.SHA1ForFile(const AFileName: string): string;
begin
  result := HashForFile(AFileName, TAlgorithm.SHA1);
end;

end.
