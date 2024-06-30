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
    class var FConfigIsLoaded: boolean;
    class procedure WaitForLoaded; static;
  private
    FConfigFileName: string;
    FConfigUrl: string;
    FConfig: TJSONObject;
    FRequest: TWebHttpRequest;
  protected
    procedure Load;
    procedure LogLoadError(ARequest: TJSXMLHttpRequest);
  public
    constructor Create;
    destructor Destroy; override;
  public
    class constructor Create;
    class function Value(ASection: string; AKey: string): string; static;
  end;

implementation

uses
  WEBLib.Forms, DX.WEBLib.Logger;

{ TWebConfig }

class constructor TWebConfig.Create;
begin
  FConfigIsLoaded := false;
  FInstance := TWebConfig.Create;
  FInstance.Load;
end;

constructor TWebConfig.Create;
begin
  FConfigFileName := 'WebConfig.json';
  FConfigUrl := TAppInfo.BaseURL + FConfigFileName;
  FConfig := TJSONObject.Create;
  FRequest := TWebHttpRequest.Create(nil);
end;

destructor TWebConfig.Destroy;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FConfig);
  inherited;
end;

procedure TWebConfig.Load;
var
  LResponse: string;
begin
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
        FConfigIsLoaded := true;
        DXLog(FConfigFileName + ' wurde geladen.');
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

procedure TWebConfig.LogLoadError(ARequest: TJSXMLHttpRequest);
begin
  DXLog('Fehler [' + ARequest.Status.ToString + '] beim Laden von ' + FConfigFileName + ' ' + FConfigUrl,
    TLogLevel.Error);
end;

class function TWebConfig.Value(ASection: string; AKey: string): string;
var
  LSection: TJSONObject;
begin
  WaitForLoaded;
  LSection := FInstance.FConfig.GetValue(ASection) as TJSONObject;
  if LSection <> nil then
  begin
    Result := LSection.GetJSONValue(AKey);
  end
  else
  begin
    Result := '';
  end;
end;

class procedure TWebConfig.WaitForLoaded;
begin
  while not FConfigIsLoaded do
  begin
    // This really doesn't wait, as Sleep doesn't block. It's basically just switching context to give the
    // supposedly still running GET request a chance to evetually complete
    Sleep(1);
  end;
end;

end.
