unit DX.Utils.IO;

interface

uses
  System.Classes, System.SysUtils,
{$IFDEF MACOS}
  Macapi.Helpers,
{$IFDEF IOS}
  iOSApi.Foundation,
{$ELSE !IOS}
  Macapi.Foundation,
  Macapi.Cocoatypes,
{$ENDIF IOS}
{$ENDIF MACOS}
  System.IOUtils;

type

  TPathHelper = record helper for TPath
  public
    class function GetAppPath: string; inline; static;
  end;

implementation

{ TPathHelper }

class function TPathHelper.GetAppPath: string;
{$IFDEF MSWINDOWS}
begin
  Result := ExtractFilePath(ParamStr(0));
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.GetDirectoryName(NSStrToStr(TNSBundle.Wrap(TNSBundle.OCClass.mainBundle).bundlePath));
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  raise Exception.Create('Not implemented yet!');
  // Result := AndroidApi.IOUtils.GetLibraryPath;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  raise Exception.Create('Not implemented yet!');
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

end.
