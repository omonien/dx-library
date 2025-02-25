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
    class var FAppId: string;
    class var FMutex: THandle;
      /// <summary>
      /// Checks if an instance with the same AppId is already running.
      /// </summary>
      /// <returns>True if another instance is running, False otherwise.</returns>
    class function IsRunning: Boolean;
  public
    /// <summary>
    /// Checks if an instance with the same AppID is already running and terminates the current process immediately if so.
    /// </summary>
    /// <param name="AAppId">A unique identifier for the application. Should be unique across all applications.</param>
    /// <remarks>
    /// Use this method as the first line after "begin" in your .dpr file to ensure single instance behavior.
    /// If another instance with the same AAppId is detected, the current process will terminate immediately.
    /// </remarks>
    class procedure Check(const AAppId: string);
    /// <summary>
    /// Cleans up and releases the mutex handle if it was created.
    /// </summary>
    class destructor Destroy;
  end;

implementation

class procedure TSingleInstance.Check(const AAppId: string);
begin
  FAppId := AAppId;
  if IsRunning then
    Halt; // Terminate the application if another instance is detected
end;

class destructor TSingleInstance.Destroy;
begin
  if FMutex <> 0 then
    CloseHandle(FMutex); // Release the mutex handle if it exists
end;

class function TSingleInstance.IsRunning: Boolean;
var
  LMutexName: string;
begin
  // Unique name for the mutex based on AppId
  LMutexName := 'Global\' + FAppId + '_SingleInstanceMutex';

  // Create the mutex
  FMutex := Winapi.Windows.CreateMutex(nil, True, PChar(LMutexName));

  // Check if the mutex already exists
  Result := (FMutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS);
end;

end.

