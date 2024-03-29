unit DX.WEBLib.Utils;

interface

uses
  System.Classes, System.SysUtils,
  Data.DB,
  JS, jsdelphisystem,
  web,

  WEBLib.Forms,
  XData.web.Client;

type
  TXDataClientResponseHelper = class helper for TXDataClientResponse
    function ResultValue: JSValue;
  end;

  TJSObjectHelper = class helper for TJSObject
    function ValueOfProperty(const APropertyName: string): string;
  end;

  TFormatSettingsHelper = record helper for TFormatSettings
    function NormalizedLocaleName: string;
  end;

  TJSConsoleHelper = class helper for TJSConsole
  public
    // Only writes a log, in DEBUG mode
    procedure debug(Obj1: JSValue);
  end;

  TAppInfo = record
    class function Version: string; static;
    class function BuildTimeStamp: TDateTime; static;
  end;

function JSResponse(AValue: JSValue): TJSResponse;

function JSBlob(AValue: JSValue): TJSBlob;

function XDataClientResponse(AValue: JSValue): TXDataClientResponse;

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

function XDataClientResponse(AValue: JSValue): TXDataClientResponse;
begin
  result := TXDataClientResponse(AValue);
end;

function TXDataClientResponseHelper.ResultValue: JSValue;
begin
  result := self.ResultAsObject['value'];
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
begin
{$IFDEF PAS2JS}
  asm
    LAppVersion = ProjectNameVersion; // Defined in index.html, catches ProjectName + Version
  end;
{$ENDIF}
  // Todo: Make generic
  // ELKE_Intra_2_0_47
  LAppVersion := LAppVersion.Replace('ELKE_Intra_', '').Replace('_', '.');
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

end.
