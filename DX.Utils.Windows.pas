unit DX.Utils.Windows;

interface

uses
  System.Classes, System.SysUtils;

procedure OpenBrowser(const AUrl: string);

function GetExeBuildTimestamp: TDateTime;

function GetExeVersion: String;

implementation

uses
  System.DateUtils,
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
  LExeFilename: String;
  LVersionInfoSize: DWORD;
  LHandle: DWORD;
  LBuffer: Pointer;
  LFileInfo: Pointer;
  LVersionInfo: array [1 .. 4] of Word;
begin
  Result := '';
  LExeFilename := ParamStr(0);

  LVersionInfoSize := GetFileVersionInfoSize(PChar(LExeFilename), LHandle);
  // if size is zero, then there is no version info in the exe
  if (LVersionInfoSize > 0) then
  begin
    GetMem(LBuffer, LVersionInfoSize);
    try
      GetFileVersionInfo(PChar(LExeFilename), 0, LVersionInfoSize, LBuffer);
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
