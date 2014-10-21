unit DX.Threading.Command;

interface

uses
  System.SysUtils, System.Classes;

type
  TErrorProc = TProc<string>;

  TAsyncCommand = class(TThread)
  private
    FExecutionProc, FDoneProc: TProc;
    FErrorProc: TErrorProc;
    FOwned: Boolean;
    function GetOwned: Boolean;
    procedure SetOwned(const Value: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create(const AExecutionProc: TProc; const ADoneProc: TProc = nil; const AErrorProc: TErrorProc = nil;
      AFreeOnDone: Boolean = true); reintroduce;
    class function Run(const AExecutionProc: TProc; const ADoneProc: TProc = nil; const AErrorProc: TErrorProc = nil;
      AFreeOnDone: Boolean = true): TAsyncCommand;

    property DoneProc: TProc read FDoneProc;
    property ErrorProc: TErrorProc read FErrorProc;
    property ExecutionProc: TProc read FExecutionProc;
    property Owned: Boolean read GetOwned write SetOwned;
  end;

implementation

uses
  FMX.Ani, FMX.Types;

{ TAsyncCommand }

constructor TAsyncCommand.Create(const AExecutionProc: TProc; const ADoneProc: TProc = nil; const AErrorProc: TErrorProc = nil;
  AFreeOnDone: Boolean = true);
begin
  inherited Create(true);
  FreeOnTerminate := AFreeOnDone;
  FExecutionProc := AExecutionProc;
  FDoneProc := ADoneProc;
  FErrorProc := AErrorProc;
  FOwned := false;
end;

procedure TAsyncCommand.Execute;
var
  LMessage: string;
begin
  try
    if Assigned(ExecutionProc) then
      ExecutionProc;
    if Assigned(DoneProc) then
      Synchronize(
        procedure
        begin
          DoneProc
        end);
  except
    on E: Exception do
    begin
      if Assigned(ErrorProc) then
      begin
        LMessage := E.Message;
        Synchronize(
          procedure
          begin
            ErrorProc(LMessage);
          end);
      end;
    end;
  end;
end;

function TAsyncCommand.GetOwned: Boolean;
begin
  TMonitor.Enter(Self);
  try
    result := FOwned;
  finally
    TMonitor.Exit(Self);
  end;
end;

class function TAsyncCommand.Run(const AExecutionProc, ADoneProc: TProc; const AErrorProc: TErrorProc; AFreeOnDone: Boolean): TAsyncCommand;
begin
  result := TAsyncCommand.Create(AExecutionProc, ADoneProc, AErrorProc, AFreeOnDone);
  result.Start;
end;

procedure TAsyncCommand.SetOwned(const Value: Boolean);
begin
  TMonitor.Enter(Self);
  try
    FOwned := Value;
  finally
    TMonitor.Exit(Self);
  end;

end;

initialization

// This is a fix for a bad behaviour of FMX' animation timer
// The interval calculation defaults to 10ms which prevents TThread.Synchronize
// to intercept reasonably. See:
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

// We only care about XE6 and up!
{$IF defined(VER280)} // XE7
TAnimation.AniFrameRate := 30;
{$ELSEIF defined(VER270)}  // XE6
AniFrameRate := 30;
{$ENDIF}

end.
