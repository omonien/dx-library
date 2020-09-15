unit DX.Utils.Windows;

interface

uses
  System.Classes, System.SysUtils;

procedure OpenBrowser(const AUrl: string);

function GetExeBuildTimestamp: TDateTime;

function GetExeVersion: String; overload;
function GetExeVersion(const AFilename: string): String; overload;

implementation

uses
  System.DateUtils, System.IOUtils,
  Winapi.Windows, Winapi.ShellAPI;

procedure OpenBrowser(const AUrl: string);
begin
  ShellExecute(0, nil, PChar(AUrl), nil, nil, SW_SHOWNOACTIVATE);
end;

function GetExeBuildTimestamp: TDateTime;
var
  LTimeStampUTC: TDateTime;
begin
  LTimeStampUTC := PImageNtHeaders(HInstance + Cardinal(PImageDosHeader(HInstance)^._lfanew))^.FileHeader.TimeDateStamp
    / SecsPerDay + UnixDateDelta;
  Result := TTimeZone.Local.ToLocalTime(LTimeStampUTC);
end;

function GetExeVersion: String;
var
  LExeFilename: string;
begin
  LExeFilename := ParamStr(0);
  Result := GetExeVersion(LExeFilename);
end;

function GetExeVersion(const AFilename: string): String;
var

  LVersionInfoSize: DWORD;
  LHandle: DWORD;
  LBuffer: Pointer;
  LFileInfo: Pointer;
  LVersionInfo: array [1 .. 4] of Word;
  LFilename: string;
begin
  Result := '';
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
    Result := Format('%d.%d.%d.%d', [LVersionInfo[1], LVersionInfo[2], LVersionInfo[3], LVersionInfo[4]]);
  end;
end;

end.
