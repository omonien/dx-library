unit DX.Utils.Windows;

interface

uses
  System.Classes, System.SysUtils;

type
  TVersionData = record
    CompanyName, FileDescription, FileVersion, InternalName, LegalCopyright, LegalTrademarks, OriginalFileName,
      ProductName, ProductVersion, Comments, PrivateBuild, SpecialBuild: string;
  end;

procedure OpenBrowser(const AUrl: string; AActivate: boolean = true);

function GetExeBuildTimestamp: TDateTime;

/// <Summary>
/// Retrieves the version number from the current process, using a format string.
/// The format strings accepts up to four version parts. Eg:
/// %d.%d.%d.%d = 1.2.3.4
/// %d.%d = 1.2
/// </Summary>
function GetExeVersionFmt(const AFormat: string): String; overload;

/// <Summary>
/// Retrieves the version number from the given file, using a format string.
/// The format strings accepts up to four version parts. Eg:
/// %d.%d.%d.%d = 1.2.3.4
/// %d.%d = 1.2
/// </Summary>
function GetExeVersionFmt(const AFilename: string; const AFormat: string): String; overload;

/// <Summary>
/// Retrieves the version number from the current process.
/// </Summary>
function GetExeVersion: String; overload;

/// <Summary>
/// Retrieves the version number from the given file.
/// </Summary>
function GetExeVersion(const AFilename: string): String; overload;

function ExecuteProcess(const AFilename, AParams: string; AFolder: string; AWaitUntilTerminated: boolean;
  var ExitCode: integer): boolean;

function GetExeVersionData: TVersionData; overload;
function GetExeVersionData(const AFilename: string): TVersionData; overload;

function ExecuteCommand(ACommandLine: string; AWork: string = 'C:\'): string;

implementation

uses
  System.DateUtils, System.IOUtils,
  Winapi.Windows, Winapi.ShellAPI;

function ExecuteProcess(const AFilename, AParams: string; AFolder: string; AWaitUntilTerminated: boolean;
  var ExitCode: integer): boolean;
var
  LCmdLine: string;
  LWorkingDir: PChar;
  LStartupInfo: TStartupInfo;
  LProcessInfo: TProcessInformation;
  LExitCode: Cardinal;
begin
  result := true;
  LCmdLine := '"' + AFilename + '" ' + AParams;
  if AFolder = '' then
  begin
    AFolder := ExcludeTrailingPathDelimiter(ExtractFilePath(AFilename));
  end;
  LStartupInfo := Default (TStartupInfo);
  LStartupInfo.cb := SizeOf(LStartupInfo);
  LStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  LStartupInfo.wShowWindow := SW_SHOWMINIMIZED;
  if AFolder <> '' then
    LWorkingDir := PChar(AFolder)
  else
    LWorkingDir := nil;
  if not CreateProcess(nil, PChar(LCmdLine), nil, nil, False, 0, nil, LWorkingDir, LStartupInfo, LProcessInfo) then
  begin
    result := False;
    ExitCode := GetLastError;
    exit;
  end;
  with LProcessInfo do
  begin
    CloseHandle(hThread);
    if AWaitUntilTerminated then
    begin
      WaitForSingleObject(LProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(LProcessInfo.hProcess, LExitCode);
      ExitCode := LExitCode;
    end;
    CloseHandle(hProcess);
  end;
end;

function ExecuteCommand(ACommandLine: string; AWork: string = 'C:\'): string;
var
  LSecurityAttributes: TSecurityAttributes;
  LStartupInfo: TStartupInfo;
  LProcessInfo: TProcessInformation;
  LStdOutPipeRead, StdOutPipeWrite: THandle;
  LWasOK: boolean;
  LBuffer: array [0 .. 255] of AnsiChar;
  LBytesRead: Cardinal;
  LWorkDir: string;
  LHandle: boolean;
begin
  result := '';
  LSecurityAttributes.nLength := SizeOf(LSecurityAttributes);
  LSecurityAttributes.bInheritHandle := true;
  LSecurityAttributes.lpSecurityDescriptor := nil;
  CreatePipe(LStdOutPipeRead, StdOutPipeWrite, @LSecurityAttributes, 0);
  try
    LStartupInfo := Default (TStartupInfo);
    LStartupInfo.cb := SizeOf(LStartupInfo);
    LStartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    LStartupInfo.wShowWindow := SW_HIDE;
    LStartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
    LStartupInfo.hStdOutput := StdOutPipeWrite;
    LStartupInfo.hStdError := StdOutPipeWrite;

    LWorkDir := AWork;
    LHandle := CreateProcess(nil, PChar('cmd.exe /C ' + ACommandLine), nil, nil, true, 0, nil, PChar(LWorkDir),
      LStartupInfo, LProcessInfo);
    CloseHandle(StdOutPipeWrite);
    if LHandle then
      try
        repeat
          // Todo: check if StdOut really is Ansichar only, as assumed here. Probably not
          LWasOK := ReadFile(LStdOutPipeRead, LBuffer, 255, LBytesRead, nil);
          if LBytesRead > 0 then
          begin
            LBuffer[LBytesRead] := #0;
            // todo: get string from Encoding
            result := result + String(LBuffer);
          end;
        until not LWasOK or (LBytesRead = 0);
        WaitForSingleObject(LProcessInfo.hProcess, INFINITE);
      finally
        CloseHandle(LProcessInfo.hThread);
        CloseHandle(LProcessInfo.hProcess);
      end;
  finally
    CloseHandle(LStdOutPipeRead);
  end;
end;

procedure OpenBrowser(const AUrl: string; AActivate: boolean = true);
begin
  var
  LMode := SW_SHOWNORMAL;
  if not AActivate then
  begin
    LMode := SW_SHOWNOACTIVATE;
  end;
  ShellExecute(0, nil, PChar(AUrl), nil, nil, LMode);
end;

function GetExeBuildTimestamp: TDateTime;
var
  LTimeStampUTC: TDateTime;
begin
  LTimeStampUTC := PImageNtHeaders(HInstance + Cardinal(PImageDosHeader(HInstance)^._lfanew))^.FileHeader.TimeDateStamp
    / SecsPerDay + UnixDateDelta;
  result := TTimeZone.Local.ToLocalTime(LTimeStampUTC);
end;

function GetExeVersion: String; overload;
begin
  result := GetExeVersion(ParamStr(0));
end;

function GetExeVersion(const AFilename: string): String; overload;
begin
  result := GetExeVersionFmt(AFilename, '%d.%d.%d.%d');
end;

function GetExeVersionFmt(const AFormat: string): String;
var
  LExeFilename: string;
begin
  LExeFilename := ParamStr(0);
  result := GetExeVersionFmt(LExeFilename, AFormat);
end;

function GetExeVersionFmt(const AFilename: string; const AFormat: string): String;
var

  LVersionInfoSize: DWORD;
  LHandle: DWORD;
  LBuffer: Pointer;
  LFileInfo: Pointer;
  LVersionInfo: array [1 .. 4] of Word;
  LFilename: string;
  LFormat: string;
begin
  result := '';

  LFilename := AFilename.Trim;
  if not TFile.Exists(AFilename) then
    raise EFileNotFoundException.CreateFmt('%s not found!', [LFilename]);

  LVersionInfoSize := GetFileVersionInfoSize(PChar(LFilename), LHandle);
  // if size is zero, then there is no version info in the exe
  if (LVersionInfoSize > 0) then
  begin
    GetMem(LBuffer, LVersionInfoSize);
    try
      GetFileVersionInfo(PChar(LFilename), 0, LVersionInfoSize, LBuffer);
      VerQueryValue(LBuffer, '\', LFileInfo, LHandle);
      LVersionInfo[1] := HiWord(PVSFixedFileInfo(LFileInfo)^.dwFileVersionMS);
      LVersionInfo[2] := LoWord(PVSFixedFileInfo(LFileInfo)^.dwFileVersionMS);
      LVersionInfo[3] := HiWord(PVSFixedFileInfo(LFileInfo)^.dwFileVersionLS);
      LVersionInfo[4] := LoWord(PVSFixedFileInfo(LFileInfo)^.dwFileVersionLS);
    finally
      FreeMem(LBuffer);
    end;
    if AFormat = '' then
    begin
      LFormat := '%d.%d.%d.%d';
    end
    else
    begin
      LFormat := AFormat;
    end;
    result := Format(LFormat, [LVersionInfo[1], LVersionInfo[2], LVersionInfo[3], LVersionInfo[4]]);
  end;
end;

function GetExeVersionData: TVersionData;
begin
  result := GetExeVersionData(ParamStr(0));
end;

function GetExeVersionData(const AFilename: string): TVersionData;

  function Query(ABuffer: Pointer; const ALanguage: string; AFieldName: string): String;
  var
    LQueryResult: Pointer;
    LVerinfoLen: Cardinal;
  begin
    if VerQueryValue(ABuffer, PChar('\StringFileInfo\' + ALanguage + '\' + AFieldName), LQueryResult, LVerinfoLen) then
      result := PChar(LQueryResult)
    else
      result := '';
  end;

type
  PLandCodepage = ^TLandCodepage;

  TLandCodepage = record
    wLanguage, wCodePage: Word;
  end;
var
  LHandle: Cardinal;
  LVerinfoLen: Cardinal;
  LBuffer, LQueryResult: Pointer;
  LLanguage: string;
begin
  LVerinfoLen := GetFileVersionInfoSize(PChar(AFilename), LHandle);
  if LVerinfoLen = 0 then
    RaiseLastOSError;
  GetMem(LBuffer, LVerinfoLen);
  try
    if not GetFileVersionInfo(PChar(AFilename), 0, LVerinfoLen, LBuffer) then
      RaiseLastOSError;

    if not VerQueryValue(LBuffer, '\VarFileInfo\Translation\', LQueryResult, LVerinfoLen) then
      RaiseLastOSError;

    LLanguage := Format('%.4x%.4x', [PLandCodepage(LQueryResult)^.wLanguage, PLandCodepage(LQueryResult)^.wCodePage]);

    result.CompanyName := Query(LBuffer, LLanguage, 'CompanyName');
    result.FileDescription := Query(LBuffer, LLanguage, 'FileDescription');
    result.FileVersion := Query(LBuffer, LLanguage, 'FileVersion');
    result.InternalName := Query(LBuffer, LLanguage, 'InternalName');
    result.LegalCopyright := Query(LBuffer, LLanguage, 'LegalCopyright');
    result.LegalTrademarks := Query(LBuffer, LLanguage, 'LegalTrademarks');
    result.OriginalFileName := Query(LBuffer, LLanguage, 'OriginalFileName');
    result.ProductName := Query(LBuffer, LLanguage, 'ProductName');
    result.ProductVersion := Query(LBuffer, LLanguage, 'ProductVersion');
    result.Comments := Query(LBuffer, LLanguage, 'Comments');
    result.PrivateBuild := Query(LBuffer, LLanguage, 'PrivateBuild');
    result.SpecialBuild := Query(LBuffer, LLanguage, 'SpecialBuild');
  finally
    FreeMem(LBuffer);
  end;
end;

end.
