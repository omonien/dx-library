/// <summary>
/// DX.SysUtils provides several system level utility routines.
/// </summary>
unit DX.SysUtils;

interface

{$SCOPEDENUMS ON}


uses
  System.Classes, System.SysUtils, System.Rtti
{$IFDEF MSWINDOWS}
    , WinAPI.Windows, WinAPI.PsAPI
{$ENDIF}
{$IFDEF IOS}
    , Macapi.Helpers, iOSAPi.Helpers, iOSAPi.Foundation, iOSAPi.UIKit
{$ENDIF};

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

/// <summary>
/// Simple, yet flexible IIF implementation
/// </summary>
function IIF(AExpression: Boolean; ATrueValue: TValue; AFalseValue: TValue): TValue;

/// <summary>
/// Emulates a "null" constant for TValue, pointing to TValue.Empty
/// May be moved to DX.Utils.RTTI
/// </summary>
function null: TValue;

/// <summary>
/// Retrieves the current memory status of the application
/// </summary>
/// <returns>
/// Formatted string with memory information (usage, available memory, process memory)
/// </returns>
function GetMemoryStatusString: string;

implementation

uses
  IdHash, IdHashMessageDigest, IdHashSHA, DX.Utils.Windows;

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

function IIF(AExpression: Boolean; ATrueValue: TValue; AFalseValue: TValue): TValue;
begin
  if AExpression then
  begin
    Result := ATrueValue;
  end
  else
  begin
    Result := AFalseValue;
  end;
end;

function null: TValue;
begin
  Result := TValue.Empty;
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
    Result := LHash.HashStreamAsHex(AStream);
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
    Result := Hash(LStream, AAlgorithm);
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
      Result := Hash(LStream, AAlgorithm);
    end
    else
    begin
      Result := '';
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

class function THash.MD5ForFile(const AFileName: string): string;
begin
  Result := HashForFile(AFileName, TAlgorithm.MD5);
end;

class function THash.SHA1ForFile(const AFileName: string): string;
begin
  Result := HashForFile(AFileName, TAlgorithm.SHA1);
end;

function GetMemoryStatusString: string;
{$IFDEF MSWINDOWS}
var
  LMemStatus: TMemoryStatusEx;
  LProcessMemCounters: TProcessMemoryCounters;
{$ENDIF}
begin
  Result := '';
{$IFDEF MSWINDOWS}
  try
    // Global memory status
    LMemStatus.dwLength := SizeOf(TMemoryStatusEx);
    if GlobalMemoryStatusEx(LMemStatus) then
    begin
      Result := Format('Mem: %d%% used, Avail: %s MB',
        [LMemStatus.dwMemoryLoad,
         FormatFloat('#,##0', LMemStatus.ullAvailPhys div (1024 * 1024))]);
    end;

    // Process-specific memory usage
    if GetProcessMemoryInfo(GetCurrentProcess, @LProcessMemCounters, SizeOf(LProcessMemCounters)) then
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + Format('Process: %s MB',
        [FormatFloat('#,##0', LProcessMemCounters.WorkingSetSize div (1024 * 1024))]);
    end;
  except
    Result := 'Mem: N/A';
  end;
{$ELSE}
  Result := 'Mem: N/A (Platform not supported)';
{$ENDIF}
end;

end.
