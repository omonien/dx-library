unit DX.Settings;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles;

type
  TSettingsFormat = (Json, Ini);

  IConfigFile = interface(IInterface)
    ['{E65F406A-95BD-4578-9CB4-66276890DF60}']
    function Filename: string;
    function Format: TSettingsFormat;
    procedure Write(const ASection: string; const AName: string; AVAlue: string); overload;
    function Read(const ASection, AName: string; const ADefault: string): string; overload;
    procedure Write(const ASection: string; const AName: string; AVAlue: TObject); overload;
    function Read(const ASection, AName: string; AClass: TClass): TObject; overload;

  end;

  TAppSettings = class(TObject)
  private
    class var FConfigFile: IConfigFile;
  public
    class constructor Create;
    class destructor Destroy;
    class function ConfigFile: IConfigFile;
    class function Filename: string;
    class procedure Write(const ASection: string; const AName: string; AVAlue: string); overload;
    class function Read(const ASection, AName: string; const ADefault: string): string; overload;
    class procedure Write<T: class>(const ASection: string; const AName: string; AVAlue: T); overload;
    class function Read<T: class, constructor>(const ASection, AName: string): T; overload;

  end;

implementation

uses
  System.IOUtils, DX.Classes.Factory;

{ TAppSettings }

class function TAppSettings.ConfigFile: IConfigFile;
begin
  if not Assigned(FConfigFile) then
    FConfigFile := DXFactory.CreateNew<IConfigFile>;
  result := FConfigFile;
end;

class constructor TAppSettings.Create;
begin

end;

class destructor TAppSettings.Destroy;
begin
end;

class function TAppSettings.Filename: string;
begin
  result := TPath.ChangeExtension(ParamStr(0), '.config');
end;

class function TAppSettings.Read(const ASection, AName: string; const ADefault: string): string;
begin
  result := ConfigFile.Read(ASection, AName, ADefault);
end;

class function TAppSettings.Read<T>(const ASection, AName: string): T;
var
  LObject: TObject;
begin
  LObject := ConfigFile.Read(ASection, AName, T);
  if Assigned(LObject) and (LObject is T) then
    result := LObject as T
  else
    result := nil;
end;

class procedure TAppSettings.Write(const ASection, AName: string; AVAlue: string);
begin
  ConfigFile.Write(ASection, AName, AVAlue);
end;

class procedure TAppSettings.Write<T>(const ASection, AName: string; AVAlue: T);
begin
  ConfigFile.Write(ASection, AName, AVAlue);
end;

end.
