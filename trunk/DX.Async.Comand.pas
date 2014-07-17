unit DX.Async.Comand;

interface

uses
  System.SysUtils, System.Classes;

type
  TAsyncCommand = class(TThread)
  private
    FExecutionProc, FDoneProc: TProc;
    FErrorProc: TProc<string>;
  protected
    procedure Execute; overload; override;
  public
    constructor Execute(const AExecutionProc: TProc; const ADoneProc: TProc = nil; const AErrorProc: TProc<string> = nil); reintroduce; overload;
  end;

implementation

uses
  FMX.Types;

{ TAsyncCommand }

constructor TAsyncCommand.Execute(const AExecutionProc: TProc; const ADoneProc: TProc = nil; const AErrorProc: TProc<string> = nil);
begin
  inherited Create(false);
  FreeOnTerminate := True;
  FExecutionProc := AExecutionProc;
  FDoneProc := ADoneProc;
  FErrorProc := AErrorProc;
end;

procedure TAsyncCommand.Execute;
var
  LMessage: string;
begin
  try
    if Assigned(FExecutionProc) then
      FExecutionProc;
    if Assigned(FDoneProc) then
      Synchronize(
        procedure
        begin
          FDoneProc
        end);
  except
    on E: Exception do
    begin
      if Assigned(FErrorProc) then
      begin
        LMessage := E.Message;
        Synchronize(
          procedure
          begin
            FErrorProc(LMessage);
          end);
      end;
    end;
  end;
end;

initialization

// This is temporary fix for an FMX problem and the way how its Animation Timer works
// The interval calculation defauls to 10ms which prevents TThread.Synchronize to intercept
// Apparently  on at least iOS the PlatformTime is not cooperative in regards of TThread.Synchronize
// see:
(*
  constructor TAniThread.Create;
  begin
  inherited Create(nil);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, IInterface(FTimerService)) then
  raise EUnsupportedPlatformService.Create('IFMXTimerService');
  if AniFrameRate < 5 then
  AniFrameRate := 5;
  if AniFrameRate > 100 then
  AniFrameRate := 100;
  Interval := trunc(1000 / AniFrameRate / 10) * 10;

*)
AniFrameRate := 30;

end.
