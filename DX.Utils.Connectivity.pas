unit DX.Utils.Connectivity;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Collections;

type

  TDXConnectivity = class(TComponent)

  private
    FActive: Boolean;
    FOnConnectivityChanged: TNotifyEvent;
    FIsOnline: Boolean;
    FServers: TStrings;
  protected
    procedure DoConnectivityChanged; virtual;
    procedure SetActive(const Value: Boolean); virtual;
    procedure SetIsOnline(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive;
    property DeviceIsOnline: Boolean read FIsOnline write SetIsOnline;
    property Servers: TStrings read FServers write FServers;
    property OnConnectivityChanged: TNotifyEvent read FOnConnectivityChanged write FOnConnectivityChanged;
  end;

implementation

uses
  System.Threading, IdHTTP, IdTCPClient, DX.Utils.Logger;

{ TDXConnectivity }

constructor TDXConnectivity.Create(AOwner: TComponent);
begin
  inherited;
  FActive := false;
  FServers := TStringList.Create;
  FServers.Add('www.google.com');
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
    Log('Online!')
  else
    Log('Offline!');
  if Assigned(FOnConnectivityChanged) then
    // Synchronize - don't queue. That avoids "hyper activity" if connection is very flaky
    TThread.Synchronize(nil,
      procedure
      begin
        FOnConnectivityChanged(Self)
      end);
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
          LServer: string;
          LConnection: TIdTCPClient;
          LSuccess: Boolean;
          i: Integer;
        begin
          LConnection := TIdTCPClient.Create(nil);
          try
            while Active and (Servers.Count > 0) do
            begin
              LSuccess := true;
              for LServer in Servers do
              begin
                if LServer.Trim = '' then
                  continue;
                LConnection.Host := LServer;
                LConnection.Port := 80;
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

procedure TDXConnectivity.SetIsOnline(const Value: Boolean);
begin
  if FIsOnline <> Value then
  begin
    FIsOnline := Value;
    DoConnectivityChanged;
  end;
end;

end.
