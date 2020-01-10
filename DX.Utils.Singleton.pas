unit DX.Utils.Singleton;

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
    class function Default: T;
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

end.
