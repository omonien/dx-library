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
  protected
    procedure Execute; override;
  public
    constructor Create(const AExecutionProc: TProc; const ADoneProc: TProc = nil; const AErrorProc: TErrorProc = nil;
      AFreeOnDone: Boolean = true); reintroduce;
    property DoneProc: TProc read FDoneProc;
    property ErrorProc: TErrorProc read FErrorProc;
    property ExecutionProc: TProc read FExecutionProc;
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
