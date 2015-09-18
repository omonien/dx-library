unit DX.CrossPlatform;

interface

uses
  System.Classes, System.SysUtils;

function IsMobilePlatform: boolean;

function CanOpenURL(const AUrl:string): boolean;

implementation

function IsMobilePlatform: boolean;
begin
{$IF (defined(MACOS) or defined(MSWindows)) and not defined(IOS)}
  Result := false;
{$ELSE}
  Result := true;
{$ENDIF}
end;

function CanOpenURL(const AUrl:string): boolean;
begin
  Result := false;
end;

end.
