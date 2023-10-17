unit DX.WEBLib.SysUtils;

interface

uses
  System.Classes, System.SysUtils,
  Web;

procedure Assert(ACondition: boolean);

implementation

procedure Assert(ACondition: boolean);
begin
  {$IFDEF DEBUG}
  if not ACondition then
  begin
    console.Log('Assert failed');
    raise Exception.Create('Assert failed');
  end;
  {$ENDIF}
end;

end.
