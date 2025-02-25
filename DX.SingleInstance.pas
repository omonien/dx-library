unit DX.SingleInstance;

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Classes;

type
  /// <summary>
  /// Prevents multiple instances of an application from running simultaneously using a Windows Mutex.
  /// </summary>
  TSingleInstance = class
  private
    FMutex: THandle;
    FAppId: string;
    /// <summary>
    /// Checks if an instance with the same AppId is already running.
    /// </summary>
    /// <returns>True if another instance is running, False otherwise.</returns>
    function IsRunning: Boolean;
  public
    /// <summary>
    /// Creates a new instance of TSingleInstance and terminates immediately if another instance with the same ID is running.
    /// </summary>
    /// <param name="AAppId">A unique identifier for the application. Should be unique across all applications.</param>
    /// <remarks>
    /// Use this constructor as the first line after "begin" in your .dpr file to ensure single instance behavior.
    /// If another instance with the same AAppId is detected, the current process will terminate immediately.
    /// </remarks>
    constructor Create(const AAppId: string);
    /// <summary>
    /// Destroys the instance and releases the mutex handle if it was created.
    /// </summary>
    destructor Destroy; override;
  end;

implementation

constructor TSingleInstance.Create(const AAppId: string);
begin
  inherited Create;
  FAppId := AAppId;
  if IsRunning then
    Halt; // Terminate the application if another instance is detected
end;

destructor TSingleInstance.Destroy;
begin
  if FMutex <> 0 then
    CloseHandle(FMutex); // Release the mutex handle if it exists
  inherited Destroy;
end;

function TSingleInstance.IsRunning: Boolean;
var
  MutexName: string;
begin
  // Unique name for the mutex based on AppId
  MutexName := 'Global\' + FAppId + '_SingleInstanceMutex';

  // Create the mutex
  FMutex := Winapi.Windows.CreateMutex(nil, True, PChar(MutexName));

  // Check if the mutex already exists
  Result := (FMutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS);
end;

end.
