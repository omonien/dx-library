unit DX.Async;

interface

uses
  System.Classes, System.SysUtils,
  System.Generics.Defaults, System.Generics.Collections,
  System.SyncObjs,
  FMX.Forms;

type

  /// <summary>
  /// A TCompletionHandler is an anonymous method, that gets called after a asynchronous operation successfully
  /// completes.
  /// </summary>
  TCompletionHandler = TProc;

  /// <summary>
  /// A TErrorHandler is an anonymous method, that gets called if an asynchronous operation fails with an error. The
  /// first parameter is an error message. The second parameter is the actual response text, which might contain a
  /// more detailed error description.
  /// </summary>
  TErrorHandler = TProc<string, string>;

  TDynObjectArray = Array of TObject;

  /// <summary>
  /// A TCommand is an anonymous method, that will be queued and executed in
  /// a TAsyncCommandQueue
  /// </summary>
  TCommand = TProc;

  /// <summary>
  /// TAsyncCommand is a container class for methods that can be executed by a  TCommandQueue
  /// </summary>
  TAsyncCommand = class(TObject)
  private
    FCommand: TProc;
    FCompletionHandler: TCompletionHandler;
    FErrorHandler: TErrorHandler;
    FFSynchronizeCompletionHandler1: Boolean;
    FObjectsToFree: TDynObjectArray;
    FSynchronizeCompletionHandler: Boolean;
  public
    constructor Create(ACommand: TCommand; ACompletionHandler: TCompletionHandler = nil; AErrorHandler: TErrorHandler = nil; ASynchronized: Boolean = true);
      reintroduce; overload;
    constructor Create(ACommand: TCommand; const AObjetcsToFree: TDynObjectArray; ACompletionHandler: TCompletionHandler = nil;
      AErrorHandler: TErrorHandler = nil; ASynchronized: Boolean = true); reintroduce; overload;

    property Command: TProc read FCommand;
    property CompletionHandler: TCompletionHandler read FCompletionHandler;
    property ErrorHandler: TErrorHandler read FErrorHandler;
    property FSynchronizeCompletionHandler1: Boolean read FFSynchronizeCompletionHandler1;
    property ObjectsToFree: TDynObjectArray read FObjectsToFree;

    /// <summary>
    /// Specifies if the CompletionHandler and ErrorHandler (if assigned) should be executed via Synchronize - in the
    /// main thread that is.
    /// </summary>
    property SynchronizeCompletionHandler: Boolean read FSynchronizeCompletionHandler write FSynchronizeCompletionHandler;
  end;

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

    TCommandBuffer = class(TObjectList<TAsyncCommand>)
    private
      FLock: TCriticalSection;
    public
      constructor Create;
      destructor Destroy; override;
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
      FCompletionProc: TCompletionHandler;
      FCommandProc: TCommand;
      FErrorProc: TErrorHandler;
      FObjectsToFree: TDynObjectArray;
      FErrorMessage: string;
      FErrorContent: string;
      procedure Execute; override;
      procedure RunCommand;
      procedure FreeObjects;
      procedure CheckForException;
    public
      FCommands: TCommandBuffer;
      constructor Create(CreateSuspended: Boolean); reintroduce;
    end;

  private
    FCommands: TCommandBuffer;
    FCommandThread: TCommandThread;

  public
    constructor Create; reintroduce;
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
    /// <param name="ASynchronized">
    /// Flag to control the execution
    /// </param>
    procedure Append(ACommand: TProc; ACompletionHandler: TCompletionHandler = nil; AErrorHandler: TErrorHandler = nil; ASynchronized: Boolean = true);
      overload;

    procedure Append(ACommand: TProc; const AObjectsToFree: TDynObjectArray; ACompletionHandler: TCompletionHandler = nil; AErrorHandler: TErrorHandler = nil;
      ASynchronized: Boolean = true); overload;

    procedure Append(ACommand: TAsyncCommand); overload;

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

procedure TAsyncCommandQueue.Append(ACommand: TProc; ACompletionHandler: TCompletionHandler = nil; AErrorHandler: TErrorHandler = nil;
  ASynchronized: Boolean = true);
var
  LObjects: TDynObjectArray;
begin
  Append(ACommand, LObjects, ACompletionHandler, AErrorHandler, ASynchronized);
end;

procedure TAsyncCommandQueue.Append(ACommand: TAsyncCommand);
begin
  FCommands.Append(ACommand);
end;

procedure TAsyncCommandQueue.Append(ACommand: TProc; const AObjectsToFree: TDynObjectArray; ACompletionHandler: TCompletionHandler = nil;
  AErrorHandler: TErrorHandler = nil; ASynchronized: Boolean = true);
begin
  Append(TAsyncCommand.Create(ACommand, AObjectsToFree, ACompletionHandler, AErrorHandler, ASynchronized));
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
  // if the queue stoped on an exception this
  FCommandThread.CheckForException;
end;

{ TAsyncCommandQueue.TStorageThread }

constructor TAsyncCommandQueue.TCommandThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  FCompletionProc := nil;
  FCommandProc := nil;
  FErrorProc := nil;
  FErrorMessage := '';
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
      try
        try
          // execute the command
          FCommandProc := LCommand.FCommand;
          RunCommand;
          // run optional completion handler
          FCompletionProc := LCommand.FCompletionHandler;
          if assigned(FCompletionProc) then
            if LCommand.SynchronizeCompletionHandler then
              Synchronize(RunCommand)
            else
              RunCommand;
        except
          on E: Exception do
          begin
            FErrorProc := LCommand.FErrorHandler;
            FErrorMessage := E.Message;
            if E is ERequestError then
            begin
              FErrorContent := ERequestError(E).ResponseContent;
            end;
            if assigned(FErrorProc) then
            begin
              if LCommand.SynchronizeCompletionHandler then
                Synchronize(RunCommand)
              else
                RunCommand;
            end
            else
            begin
              // Unhandled Exception stops command processing
              FCommands.Clear;
            end;
          end;
        end;
      finally
        // ObjectsToFree will get freed in a main thread loop, the difference to synchronize
        // is that Queue does NOT wait for completion. If we would wait here, then
        // we might run into possible deadlock issues, if any of the objects to be freed were
        // related to this thread or CommandQueue
        FObjectsToFree := LCommand.ObjectsToFree;
        Queue(FreeObjects);
        // TODO: Probably should remove the command from the queue first
        // remove the command from the Queue - in ANY case
        // We do NOT lock the list while executing a command. This means that we technically
        // cannot rely on the fact, that LCommand is still the first command, thus we have to look for it.
        // Remove starts looking at the beginning, so if the command is still there (which it would be in normal cases),
        // this will be an O(1) operation, which will degrade to O(n) in its relatively rare worst case
        FCommands.Remove(LCommand);
      end;
    end;
  end;
end;

constructor TAsyncCommandQueue.TCommandBuffer.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TAsyncCommandQueue.TCommandBuffer.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

{ TAsyncCommandQueue.TCommandBuffer }

procedure TAsyncCommandQueue.TCommandBuffer.Append(ACommand: TAsyncCommand);
begin
  FLock.Enter;
  try
    Add(ACommand);
  finally
    FLock.Leave;
  end;
end;

function TAsyncCommandQueue.TCommandBuffer.First: TAsyncCommand;
begin
  FLock.Enter;
  try
    if Count > 0 then
    begin
      Result := Items[0];
    end
    else
      Result := nil;
  finally
    FLock.Leave;
  end;
end;

function TAsyncCommandQueue.TCommandBuffer.IsEmpty: Boolean;
begin
  FLock.Enter;
  try
    Result := Count = 0;
  finally
    FLock.Leave;
  end;
end;

procedure TAsyncCommandQueue.TCommandBuffer.Remove(ACommand: TAsyncCommand);
begin
  FLock.Enter;
  try
    RemoveItem(ACommand, TDirection.FromBeginning);
  finally
    FLock.Leave;
  end;
end;

procedure TAsyncCommandQueue.TCommandThread.FreeObjects;
var
  LObject: TObject;
begin
  for LObject in FObjectsToFree do
  begin
    LObject.Free;
  end;
end;

procedure TAsyncCommandQueue.TCommandThread.CheckForException;
var
  LMessage: string;
begin
  if FErrorMessage > '' then
  begin
    LMessage := FErrorMessage;
    FErrorMessage := '';
    raise ERESTException.Create(LMessage);
  end;
end;

// TODO: Why not have different methods instead of checking instance variable.
// RunCommand - uses CommandProc
// CompleteCommand - uses CompletionProc
// RunError - uses ErrorProc
// Does that really change anything? It just spreads the code of a single proc into multiple, which no obvious advantage
// The reason for this weird behavior is that "Synchronize" only works with parameter-less procedures.

procedure TAsyncCommandQueue.TCommandThread.RunCommand;
begin
  // Being Paranoid - just in case an empty command was inserted
  try
    if assigned(FCommandProc) then
      FCommandProc
    else if assigned(FCompletionProc) then
      FCompletionProc
    else if assigned(FErrorProc) then
      FErrorProc(FErrorMessage, FErrorContent);
  finally
    // Reset after execution, so that we don't get a duplicate execution by accident
    FCommandProc := nil;
    FCompletionProc := nil;
    FErrorProc := nil;
  end;
end;

{ TAsyncCommand }

constructor TAsyncCommand.Create(ACommand: TCommand; const AObjetcsToFree: TDynObjectArray; ACompletionHandler: TCompletionHandler = nil;
  AErrorHandler: TErrorHandler = nil; ASynchronized: Boolean = true);
begin
  FCommand := ACommand;
  FCompletionHandler := ACompletionHandler;
  FErrorHandler := AErrorHandler;
  FSynchronizeCompletionHandler := ASynchronized;
  FObjectsToFree := AObjetcsToFree;
end;

constructor TAsyncCommand.Create(ACommand: TCommand; ACompletionHandler: TCompletionHandler; AErrorHandler: TErrorHandler; ASynchronized: Boolean);
var
  LObjects: TDynObjectArray;
begin
  Create(ACommand, LObjects, ACompletionHandler, AErrorHandler, ASynchronized);
end;

initialization

finalization

GQueue.Free;

end.
