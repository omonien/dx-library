unit DX.Threading;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Defaults, System.Generics.Collections,
  System.SyncObjs,
  DX.Threading.Command,
  FMX.Forms;

type
  /// <summary>
  /// <para>
  /// TAsyncCommandQueue basically provides a queue like structure which takes anonymous methods - "commands" - (from calls
  /// to SaveAsync, RefreshAsync etc.) and processes them in an internal thread in the order they were added to the
  /// queue.
  /// </para>
  /// <para>
  /// If one of the commands fails then its error handler is executed and the next command in queue will be
  /// processed.
  /// </para>
  /// </summary>

  TAsyncCommandQueue = class(TObject)
  private type

    TCommand = TProc;

    TCommandBuffer = class(TList<TAsyncCommand>)
    private
    public
      /// <summary>
      /// Appends Acommand to the (end of the) list.
      /// </summary>
      procedure Append(ACommand: TAsyncCommand);
      /// <summary>
      /// Returns a reference to the first (head) command.
      /// </summary>
      function First: TAsyncCommand;
      /// <summary>
      /// Returns true if no commands are in queue.
      /// </summary>

      function IsEmpty: Boolean;
      /// <summary>
      /// Removes the first (head) command from the list
      /// </summary>
      procedure Remove(ACommand: TAsyncCommand);
    end;

    TCommandThread = class(TThread)
    protected
      procedure Execute; override;
      procedure CheckForException;
    public
      FCommands: TCommandBuffer;
      FCommandException: TObject;
    end;

  private
    FCommands: TCommandBuffer;
    FCommandThread: TCommandThread;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    /// <summary>
    /// Appends a command to be executed to the command queue.
    /// </summary>
    /// <param name="ACommand">
    /// The command to be executed
    /// </param>
    /// <param name="ACompletionHandler">
    /// A anonymous method to be executed after the asynchronous operation completes successfully.
    /// </param>
    /// <param name="AErrorHandler">
    /// A anonymous method to be executed after the asynchronous operation completes unsuccessfully.
    /// </param>
    procedure Append(AExecutionProc: TProc; ADoneProc: TProc = nil; AErrorProc: TErrorProc = nil);

    procedure WaitForDone;
  end;

  /// <summary>
  /// GAsyncCommandQueue is provided as Singleton Pattern.
  /// </summary>
function GAsyncCommandQueue: TAsyncCommandQueue;

implementation

{ TAsyncCommandQueue }

uses
{$IF Defined(MSWINDOWS)}
  Winapi.Windows,
{$ENDIF}
  REST.Exception;

var
  GQueue: TAsyncCommandQueue = nil;

procedure DoRunLoop;
begin
  if assigned(WakeMainThread) then
    WakeMainThread(nil);
end;

function GAsyncCommandQueue: TAsyncCommandQueue;
begin
  if not assigned(GQueue) then
    GQueue := TAsyncCommandQueue.Create;
  Result := GQueue;
end;

procedure TAsyncCommandQueue.Append(AExecutionProc, ADoneProc: TProc; AErrorProc: TErrorProc);
var
  LCommand: TAsyncCommand;
begin
  LCommand := TAsyncCommand.Create(AExecutionProc, ADoneProc, AErrorProc, False);
  FCommands.Append(LCommand);
end;

constructor TAsyncCommandQueue.Create;
begin
  inherited Create;
  FCommandThread := TCommandThread.Create(true);
  FCommands := TCommandBuffer.Create;
  FCommandThread.FCommands := FCommands;
  FCommandThread.Start;
end;

destructor TAsyncCommandQueue.Destroy;
begin
  FreeAndNil(FCommandThread);
  FreeAndNil(FCommands);
end;

procedure TAsyncCommandQueue.WaitForDone;
begin
  while not FCommands.IsEmpty do
  begin
    if TThread.CurrentThread.ThreadID = MainThreadID then
    begin
      // if MainThread, then we make sure that Synchronize calls can still be executed
      // and if this is a GUI application i.e. there is a MessageLoop, then we keep that one running as well
      if not CheckSynchronize(10) then
        Application.ProcessMessages;
    end
    else
    begin
      // if not Mainthread, then we do just some plain busy waiting
      sleep(10);
    end;
  end;
  // if the queue stopp on an exception this
  FCommandThread.CheckForException;
end;

procedure TAsyncCommandQueue.TCommandThread.Execute;
var
  LCommand: TAsyncCommand;

begin
  while not terminated do
  begin
    // Loop through waiting commands to be executed
    LCommand := FCommands.First;
    if not assigned(LCommand) then
    begin
      // technically worst case: 100ms until a new command in the queue will be executed
      // Todo: use TMonitor.pulse (when new command gets appended) instead of continuous sleep() - First
      sleep(100)
    end
    else
    begin
      // execute the command and block
      LCommand.Start;
      try
        LCommand.WaitFor;
        FCommands.Remove(LCommand);
        FreeAndNil(LCommand);
      except
        on e: Exception do
          FCommandException := e;
      end;
    end;
  end;
end;

{ TAsyncCommandQueue.TCommandBuffer }

procedure TAsyncCommandQueue.TCommandBuffer.Append(ACommand: TAsyncCommand);
begin
  if assigned(ACommand) then
  begin
    TMonitor.Enter(self);
    try
      Add(ACommand);
    finally
      TMonitor.Exit(self);
    end;
  end;
end;

function TAsyncCommandQueue.TCommandBuffer.First: TAsyncCommand;
begin
  TMonitor.Enter(self);
  try
    if Count > 0 then
    begin
      Result := Items[0];
    end
    else
      Result := nil;
  finally
    TMonitor.Exit(self);
  end;
end;

function TAsyncCommandQueue.TCommandBuffer.IsEmpty: Boolean;
begin
  TMonitor.Enter(self);
  try
    Result := Count = 0;
  finally
    TMonitor.Exit(self);
  end;
end;

procedure TAsyncCommandQueue.TCommandBuffer.Remove(ACommand: TAsyncCommand);
begin
  if assigned(ACommand) then
  begin
    TMonitor.Enter(self);
    try
      RemoveItem(ACommand, TDirection.FromBeginning);
    finally
      TMonitor.Exit(self);
    end;
  end;
end;

procedure TAsyncCommandQueue.TCommandThread.CheckForException;
begin
  if assigned(FCommandException) then
  begin
    raise FCommandException;
  end;
end;

initialization

finalization

GQueue.Free;

end.
