unit DX.Utils.Connectivity;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Collections;

type

  TConnectivityState = (Unkown, Offline, Online);

  TDXServer = class(TObject)
  private
    FPort: Integer;
    FConnectionTimeout: Integer;
    FHostname: string;
  protected
    procedure SetHostname(const Value: string); virtual;
  public
    constructor Create(const AHostname: string; APort: Integer; AConnectionTimeout: Integer = 1000);
  published
    property Port: Integer read FPort write FPort;
    property ConnectionTimeout: Integer read FConnectionTimeout write FConnectionTimeout;
    property Hostname: string read FHostname write SetHostname;
  end;

  TDXConnectivity = class(TComponent)
  private
    FActive: Boolean;
    FOnConnectivityChanged: TNotifyEvent;
    FServers: TObjectList<TDXServer>;
    FState: TConnectivityState;
    function GetDeviceIsOnline: Boolean;
    procedure SetDeviceIsOnline(const Value: Boolean);
    procedure SetState(const Value: TConnectivityState);
  protected
    procedure DoConnectivityChanged; virtual;
    procedure SetActive(const Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive;
    property DeviceIsOnline: Boolean read GetDeviceIsOnline write SetDeviceIsOnline;
    property Servers: TObjectList<TDXServer> read FServers write FServers;
    property State: TConnectivityState read FState write SetState;
    property OnConnectivityChanged: TNotifyEvent read FOnConnectivityChanged write FOnConnectivityChanged;
  end;

implementation

uses
  System.Threading, IdHTTP, IdTCPClient, DX.Utils.Logger;

{ TDXConnectivity }

constructor TDXConnectivity.Create(AOwner: TComponent);
var
  LServer: TDXServer;
begin
  inherited;
  FActive := false;
  FServers := TObjectList<TDXServer>.Create;
  LServer := TDXServer.Create('www.google.com', 443, 1000);
  FServers.Add(LServer);
  FState := TConnectivityState.Unkown;
  Log('Device state unknown!');
end;

destructor TDXConnectivity.Destroy;
begin
  Active := false;
  FreeAndNil(FServers);
  inherited;
end;

procedure TDXConnectivity.DoConnectivityChanged;
begin
  if DeviceIsOnline then
    Log('Device is online!')
  else
    Log('Device is offline!');
  if Assigned(FOnConnectivityChanged) then
    // Synchronize - don't queue. That avoids "hyper activity" if connection is very flaky
    TThread.Synchronize(nil,
      procedure
      begin
        FOnConnectivityChanged(Self)
      end);
end;

function TDXConnectivity.GetDeviceIsOnline: Boolean;
begin
  Result := State = TConnectivityState.Online;
end;

procedure TDXConnectivity.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
    begin
      TTask.Run(
        procedure
        var
          LConnection: TIdTCPClient;
          LSuccess: Boolean;
          i: Integer;
          LServer: TDXServer;
        begin
          LConnection := TIdTCPClient.Create(nil);
          try
            while Active and (Servers.Count > 0) do
            begin
              LSuccess := true;
              for LServer in Servers do
              begin
                if LServer.Hostname = '' then
                  continue;
                LConnection.Host := LServer.Hostname;
                LConnection.Port := LServer.Port;
                LConnection.ConnectTimeout := LServer.ConnectionTimeout;
                try
                  LConnection.Connect;
                  LConnection.Disconnect;
                except
                  LSuccess := false;
                end;
              end;
              DeviceIsOnline := LSuccess;
              // check once a second, terminate early
              for i := 1 to 10 do
              begin
                sleep(100);
                if not Active then
                  break;
              end;
            end;
          finally
            FreeAndNil(LConnection);
          end;
        end);
    end
  end
end;

procedure TDXConnectivity.SetDeviceIsOnline(const Value: Boolean);
begin
  if Value then
    State := TConnectivityState.Online
  else
    State := TConnectivityState.Offline;
end;

procedure TDXConnectivity.SetState(const Value: TConnectivityState);
begin
  if FState <> Value then
  begin
    FState := Value;
    DoConnectivityChanged;
  end;
end;

{ TDXServer }

constructor TDXServer.Create(const AHostname: string; APort: Integer; AConnectionTimeout: Integer = 1000);
begin
  inherited Create;
  FHostname := AHostname;
  FPort := APort;
  FConnectionTimeout := AConnectionTimeout;
end;

procedure TDXServer.SetHostname(const Value: string);
begin
  FHostname := Value.Trim;
end;

end.
