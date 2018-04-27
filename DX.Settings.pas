unit DX.Settings;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles;

type
  TAppSettings = class(TObject)
  private
    class var FSettingsFile: TIniFile;
  public
    class constructor Create;
    class destructor Destroy;
    class function Filename: string;
    class procedure Write(const ASection: string; const AName: string; AVAlue: string);
    class function Read(const ASection, AName: string; const ADefault: string): string;

  end;

implementation

uses
  System.IOUtils;

{ TAppSettings }

class constructor TAppSettings.Create;
begin
  FSettingsFile := TIniFile.Create(Filename);
end;

class destructor TAppSettings.Destroy;
begin
  FreeAndNil(FSettingsFile);
end;

class function TAppSettings.Filename: string;
begin
  result := TPath.ChangeExtension(ParamStr(0), '.config');
end;

class function TAppSettings.Read(const ASection, AName: string; const ADefault: string): string;
begin
  result := FSettingsFile.ReadString(ASection, AName, ADefault);
end;

class procedure TAppSettings.Write(const ASection, AName: string; AVAlue: string);
begin
  FSettingsFile.WriteString(ASection, AName, AVAlue);
end;

end.
