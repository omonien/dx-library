unit DX.Classes.Service;

interface

uses
  System.SysUtils, System.Classes,
  Winapi.Messages,
  Vcl.SvcMgr, Vcl.Graphics, Vcl.Controls, Vcl.Dialogs;

type

  /// <summary>
  /// Allows to register a dscription for a TService
  /// </summary>
  TServiceHelper = class helper for TService
  public
    procedure RegisterDescription(const ADescription: string); overload;
    class procedure RegisterDescription(const AServiceName, ADescription: string); overload;
  end;

  TServiceBase = class(TService)
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  strict private
    class var FName: string;
    class var FDisplayName: string;
    class var FDescription: string;
    class var FServiceInstance: TServiceBase;
  strict private
    class procedure LogService(const AMessage: string);
    class procedure DisplayError(const AMessage: string);
    class function FindSwitch(const ASwitchName: string): Boolean;
  public
    function GetServiceController: TServiceController; override;
  public
    class constructor Create;
    class function Instance: TServiceBase;
    /// <summary>
    /// Returns true if the process is running as Service
    /// </summary>
    class function IsService: Boolean;
    /// <summary>
    /// Returns true if the Service is currently installing
    /// </summary>
    class function IsServiceInstalling: Boolean; static;
    /// <summary>
    /// Registers the Service and sets its name in the Registry
    /// </summary>
    class procedure RegisterService(const AServiceName: string = '');

    class procedure Run;
  end;

implementation

{$R *.DFM}

uses
  System.Win.Registry, Winapi.Windows,
  DX.Utils.Logger, DX.Utils.Windows, System.UITypes;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  TServiceBase.Instance.Controller(CtrlCode);
end;

{ TServiceHelper }
procedure TServiceHelper.RegisterDescription(const ADescription: string);
begin
  TService.RegisterDescription(Name, ADescription);
end;

class procedure TServiceHelper.RegisterDescription(const AServiceName, ADescription: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + AServiceName, false) then
    begin
      Reg.WriteString('Description', ADescription.Trim);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{ TServiceBase }
class constructor TServiceBase.Create;
begin
  FName := GetExeVersionData.InternalName;
  FDisplayName := GetExeVersionData.ProductName;
  FDescription := GetExeVersionData.FileDescription;
  Assert(FName > '', 'VersionInfo.InternalName is empty!');
  Assert(FDisplayName > '', 'VersionInfo.DisplayName is empty!');
  Assert(FDescription > '', 'VersionInfo.FileDescription is empty!')
  // Properties are assigned in OnCreate and AfterInstall
end;

procedure TServiceBase.ServiceCreate(Sender: TObject);
begin
  Name := FName;
  DisplayName := FDisplayName
  // Description is set in AfterInstall
end;

class procedure TServiceBase.DisplayError(const AMessage: string);
begin
  if IsConsole then
    WriteLn(AMessage)
  else
    MessageDlg(AMessage, TMsgDlgType.mtError, [mbOk], 0);
end;

class function TServiceBase.FindSwitch(const ASwitchName: string): Boolean;
begin
  result := FindCmdLineSwitch(ASwitchName, ['-', '/'], True);
end;

function TServiceBase.GetServiceController: TServiceController;
begin
  result := ServiceController;
end;

class function TServiceBase.Instance: TServiceBase;
begin
  result := FServiceInstance;
end;

class function TServiceBase.IsService: Boolean;
begin
  result := (System.ParamCount > 0) and (FindSwitch('service') or FindSwitch('install') or FindSwitch('uninstall'));
end;

class function TServiceBase.IsServiceInstalling: Boolean;
begin
  result := (System.ParamCount > 0) and FindSwitch('install');
end;

class procedure TServiceBase.LogService(const AMessage: string);
begin
  Log('[SERVICE] %s %s', [FDisplayName, AMessage]);
end;

class procedure TServiceBase.RegisterService(const AServiceName: string = '');
begin
  // Hier wird:
  // -der Parameter "-service" gesetzt, um den Prozess als Service zu starten
  // -die Beschreibung gesetzt, was über VCL.SvcMgr nicht möglich ist
  var
  LServiceName := AServiceName;
  if LServiceName = '' then
    LServiceName := FName;
  Assert(LServiceName > '', 'ServiceName has not been configured / is empty!');
  try
    RegisterDescription(FDescription, AServiceName);
  except
    raise Exception.Create('Service could not be registered. Run as Administrator!');
  end;
end;

class procedure TServiceBase.Run;
begin
  try
    if Application.Installing then
    begin
      LogService('Installing/uninstalling ...');
    end
    else
    begin
      LogService('Starting ...');
    end;

    Application.Initialize;
    LogService('Creating service instance...');
    Application.CreateForm(TServiceBase, FServiceInstance);

    if Application.Installing then
    begin
      Application.Run;
      LogService('installed/uninstalled!');
    end
    else
    begin
      LogService('Starting service instance...');
      Application.Run;
    end;
  Except
    on E: Exception do
    begin
      LogService('ERROR: ' + E.Message);
      if Application.Installing then
        DisplayError(E.Message);
    end;
  end;
  LogService('shutting down!');
  Sleep(1000); // Let log finish...
end;

procedure TServiceBase.ServiceAfterInstall(Sender: TService);
begin
  RegisterService;
end;

procedure TServiceBase.ServiceStart(Sender: TService; var Started: Boolean);
begin
  LogService('Service starting...');
end;

procedure TServiceBase.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  LogService('Service stopping...');
end;

end.
