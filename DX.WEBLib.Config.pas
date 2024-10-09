unit DX.WEBLib.Config;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils,
  JS, jsdelphisystem,
  web,
  WEBLib.REST, WEBLib.JSON,
  DX.WEBLib.SysUtils;

type
  TWebConfig = class(TObject)
  private
    class var FInstance: TWebConfig;
  private
    FConfigFileName: string;
    FConfigUrl: string;
    FConfig: TJSONObject;
    FRequest: TWebHttpRequest;
  protected
    procedure LogLoadError(ARequest: TJSXMLHttpRequest);
  public
    constructor Create(ALoadedProc: TProc);
    destructor Destroy; override;
  public
    class procedure Load(ALoadedProc: TProc = nil);
    class function Value(ASection: string; AKey: string): string; static;
  end;

implementation

uses
  WEBLib.Forms, DX.WEBLib.Logger;

constructor TWebConfig.Create(ALoadedProc: TProc);
var
  LResponse: string;
begin
  DXLog('WebConfig initializing ...');
  FConfigFileName := 'WebConfig.json';
  FConfigUrl := TAppInfo.BaseURL + FConfigFileName;
  FConfig := TJSONObject.Create;
  FRequest := TWebHttpRequest.Create(nil);

  FRequest.URL := FConfigUrl;
  FRequest.Command := THTTPCommand.httpGET;
  FRequest.Execute(
    procedure(AResponse: string; ARequest: TJSXMLHttpRequest)
    begin
      if (ARequest.Status >= 200) and (ARequest.Status < 300) then
      begin
        LResponse := AResponse;
        // Not a leak - we are in Javascript at the end of the day
        FConfig := TJSON.Create.Parse(LResponse) as TJSONObject;
        DXLog(FConfigFileName + ' loaded successfully.');
        if Assigned(ALoadedProc) then
        begin
          DXLog('Executing LoadedProc...', TLogLevel.Debug);
          ALoadedProc;
        end;
      end
      else
      begin
        LResponse := '';
        LogLoadError(ARequest);
      end;
    end,
    procedure(ARequest: TJSXMLHttpRequest)
    begin
      LResponse := '';
      LogLoadError(ARequest);
    end);

end;

destructor TWebConfig.Destroy;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FConfig);
  inherited;
end;

class procedure TWebConfig.Load(ALoadedProc: TProc);
begin
  FInstance := TWebConfig.Create(ALoadedProc);
end;

procedure TWebConfig.LogLoadError(ARequest: TJSXMLHttpRequest);
begin
  DXLog('Error [' + ARequest.Status.ToString + '] while attempting to load ' + FConfigFileName + ' ' + FConfigUrl,
    TLogLevel.Error);
end;

class function TWebConfig.Value(ASection: string; AKey: string): string;
var
  LSection: TJSONObject;
begin
  result := '';
  if FInstance = nil then
  begin
    DXLog('TWebConfig not loaded!', TLogLevel.Error);
    exit;
  end;
  LSection := FInstance.FConfig.GetValue(ASection) as TJSONObject;
  if LSection <> nil then
  begin
    result := LSection.GetJSONValue(AKey);
  end;
  DXLog('TWebConfig loaded %s/%s  =  %s', [ASection, AKey, result], TLogLevel.Debug);
end;

end.
