unit DX.Classes.Service;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.SvcMgr;

type
  TServiceHelper = class helper for TService

    procedure RegisterDescription(const ADescription: string);
  end;

implementation

uses
  System.Win.Registry, Winapi.Windows;

{ TServiceHelper }

procedure TServiceHelper.RegisterDescription(const ADescription: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, false) then
    begin
      Reg.WriteString('Description', ADescription.Trim);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

end.
