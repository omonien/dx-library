unit DX.WEBLib.SysUtils;

interface

uses
  System.Classes, System.SysUtils,
  Data.DB,
  JS, jsdelphisystem,
  web,

  WEBLib.Forms;

type
  TJSObjectHelper = class helper for TJSObject
    function ValueOfProperty(const APropertyName: string): string;
  end;

  TFormatSettingsHelper = record helper for TFormatSettings
    function NormalizedLocaleName: string;
  end;

  TJSConsoleHelper = class helper for TJSConsole
  public
    // Writes a log only if in DEBUG mode
    procedure debug(Obj1: JSValue);
  end;

  TAppInfo = record
    class function Version: string; static;
    class function BuildTimeStamp: TDateTime; static;
  end;

function JSResponse(AValue: JSValue): TJSResponse;

function JSBlob(AValue: JSValue): TJSBlob;

procedure Assert(ACondition: boolean);

implementation

uses
  WEBLib.TMSWEBUtils;

function JSResponse(AValue: JSValue): TJSResponse;
begin
  result := TJSResponse(AValue);
end;

function JSBlob(AValue: JSValue): TJSBlob;
begin
  result := TJSBlob(AValue);
end;

function TJSObjectHelper.ValueOfProperty(const APropertyName: string): string;
begin
  result := string(self[APropertyName]);
end;

{ TFormatSettingsHelper }

function TFormatSettingsHelper.NormalizedLocaleName: string;
var
  s: string;
begin
{$IFDEF PAS2JS}
  asm
    s = Intl.DateTimeFormat().resolvedOptions().locale
  end;
{$ENDIF}
  result := s;
end;

{ TAppInfo }

class function TAppInfo.BuildTimeStamp: TDateTime;
var
  LAppDate: string;
  LFormat: TFormatSettings;
begin
{$IFDEF PAS2JS}
  asm
    LAppDate = document.lastModified;
  end;
{$ENDIF}
  LFormat := TFormatSettings.Create('en-US');
  result := StrToDateTime(LAppDate, LFormat);
end;

class function TAppInfo.Version: string;
var
  LAppVersion: string;
  LVersionPos: integer;
begin
{$IFDEF PAS2JS}
  asm
    LAppVersion = ProjectNameVersion; // Defined in index.html, catches ProjectName + Version
  end;
{$ENDIF}
  // AppName_2_0_47
  LVersionPos := Pos('_', LAppVersion);
  if LVersionPos = 0 then
  begin
    LAppVersion := 'UNKNOWN';
  end
  else
  begin
    LAppVersion := Copy(LAppVersion, LVersionPos + 1, Length(LAppVersion) - LVersionPos);
    LAppVersion := LAppVersion.Replace('_', '.');
  end;
  result := LAppVersion;
end;

{ TJSConsoleHelper }

procedure TJSConsoleHelper.debug(Obj1: JSValue);
begin
{$IFDEF DEBUG}
{$IFDEF PAS2JS}
  asm
    console.debug(Obj1);
  end;
{$ENDIF}
{$ENDIF}
end;

procedure Assert(ACondition: boolean);
begin
{$IFDEF DEBUG}
  if not ACondition then
  begin
    console.Log('Assert failed');
    raise Exception.Create('Assert failed');
  end;
{$ENDIF}
end;

end.
