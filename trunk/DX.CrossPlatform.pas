unit DX.CrossPlatform;

interface

uses
  System.Classes, System.SysUtils;

function IsMobilePlatform: boolean;

implementation

function IsMobilePlatform: boolean;
begin
{$IF (defined(MACOS) or defined(MSWindows)) and not defined(IOS)}
  Result := false;
{$ELSE}
  Result := true;
{$ENDIF}
end;

end.
