unit DX.Classes.Singleton;

interface

uses
  System.Classes, System.SysUtils;

type
  /// <summary>
  /// TSingleton is an abstract class, which provides the "Default" instance
  /// functionality, based on lazy loading. <br />Descned from TSingleton, to
  /// easily design a singleton class.
  /// </summary>
  /// <typeparam name="T">
  /// T is the actual class that descends from TSingleton
  /// </typeparam>
  /// <remarks>
  /// Do not create direct instances of TSingleton - only descend from it.
  /// </remarks>
  TSingleton<T: Class> = class abstract(TObject)
  strict private
    class var FDefaultInstance: T;
  public
    class constructor Create;
    class destructor Destroy;
    /// <summary>
    /// The default instance.
    /// </summary>
    class function Default: T;
    /// <summary>
    /// Alias of Default
    /// </summary>
    class function Instance: T;
  end;

type
  /// <summary>
  /// Interfaced variant of TInterfacedSingleton.
  /// </summary>
  /// <typeparam name="T">
  /// T is the actual class that descends from TInterfacedSingleton
  /// </typeparam>
  /// <remarks>
  /// Do not create direct instances of TInterfacedSingleton - only descend from it.
  /// </remarks>
  TInterfacedSingleton<T: Class> = class abstract(TInterfacedObject)
  strict private
    class var FDefaultInstance: T;
  public
    class constructor Create;
    class destructor Destroy;
    /// <summary>
    /// The default instance.
    /// </summary>
    class function Default: T;
    /// <summary>
    /// Alias of Default
    /// </summary>
    class function Instance: T;
  end;

implementation

uses
  DX.Utils.RTTI;

class constructor TSingleton<T>.Create;
begin
  FDefaultInstance := nil;
end;

class function TSingleton<T>.Default: T;
begin
  if not Assigned(FDefaultInstance) then
  begin
    // Some RTTI magic is needed here. T.Create cannot be called directly.
    FDefaultInstance := TClassConstructor.Construct<T>;
  end;
  result := FDefaultInstance;
end;

class destructor TSingleton<T>.Destroy;
begin
  FreeAndNil(FDefaultInstance);
end;

class function TSingleton<T>.Instance: T;
begin
  result := Default;
end;

class constructor TInterfacedSingleton<T>.Create;
begin
  FDefaultInstance := nil;
end;

class function TInterfacedSingleton<T>.Default: T;
begin
  if not Assigned(FDefaultInstance) then
  begin
    // Some RTTI magic is needed here. T.Create cannot be called directly.
    FDefaultInstance := TClassConstructor.Construct<T>;
  end;
  result := FDefaultInstance;
end;

class destructor TInterfacedSingleton<T>.Destroy;
begin
  FDefaultInstance := nil;
end;

class function TInterfacedSingleton<T>.Instance: T;
begin
  result := Default;
end;

end.
