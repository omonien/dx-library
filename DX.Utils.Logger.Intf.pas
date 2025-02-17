/// <summary>
///   ILogger Interface, to make TDXLogger replacible.
/// </summary>
unit DX.Utils.Logger.Intf;

interface
uses
  System.Classes, System.SysUtils;

type
  /// <summary>
  /// Loglevel der Nachrichten
  /// Aktuell noch nicht implementiert
  /// </summary>
    //Todo: implement
  TLogLevel = (None, Error, Debug, Warn, Info);

  ILogger = interface(IInterface)
    ['{05784DCF-00EB-424D-AC65-74D018CD768E}']
    /// <summary>
    /// Sendet eine einfache Log-Nachricht
    /// </summary>

    procedure Log(const AMessage: string; const ADebugLevel: TLogLevel = TLogLevel.Info); overload;

    /// <summary>
    /// Sendet eine Log-Nachricht mit Formatstring
    /// </summary>
    procedure Log(
      const AFormatString: string;
      const AValues: array of const;
      const ADebugLevel: TLogLevel = TLogLevel.Info); overload;
  end;

/// <summary>
/// Shortcut to to avoid name clashes with other logging systems
/// </summary>
procedure DXLog(const AMessage: string; ADebugLevel: TLogLevel = TLogLevel.Info); overload;

/// Shortcut to to avoid name clashes with other logging systems
procedure DXLog(
  const AFormatString: string;
  const AValues: array of const;
  ADebugLevel: TLogLevel = TLogLevel.Info); overload;

/// <summary>
///   Simple way to register an actual logger implementation.
/// </summary>
/// <example>
///   RegisterLogger(TDXLogger.Instance);
/// </example>
procedure RegisterLogger(const ALogger: ILogger);

/// <summary>
///   Cleanup carefully when closing down.
/// </summary>
procedure UnRegisterLogger;

implementation

var
  GLogger: ILogger = nil;

procedure DXLog(const AMessage: string; ADebugLevel: TLogLevel = TLogLevel.Info);
begin
  if Assigned(GLogger) then
  begin
    GLogger.Log(AMessage, ADebugLevel);
  end;
end;

procedure DXLog(const AFormatString: string; const AValues: array of const; ADebugLevel: TLogLevel = TLogLevel.Info);
begin
  if Assigned(GLogger) then
  begin
    GLogger.Log(AFormatString, AValues, ADebugLevel);
  end;
end;

procedure RegisterLogger(const ALogger: ILogger);
begin
  GLogger := ALogger;
end;

procedure UnRegisterLogger;
begin
  GLogger := nil;
end;

initialization

finalization
  GLogger := nil;
end.

