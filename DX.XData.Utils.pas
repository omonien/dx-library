unit DX.XData.Utils;

interface

uses
  System.Classes, System.SysUtils,
  DX.Utils.Rtti, XData.Server.Module;

type
  TXDataServerModuleHelper = class helper for TXDataServerModule
  public
    procedure SetPermissions(
      AEntity:      TClass;
      APermissions: TEntitySetPermissions);
  end;

implementation

{ TXDataServerModuleHelper }

procedure TXDataServerModuleHelper.SetPermissions(
  AEntity:      TClass;
  APermissions: TEntitySetPermissions);
var
  LEntityName: string;
begin
  LEntityName := AEntity.ClassName;
  // Remove Leading "T"
  LEntityName := LEntityName.Remove(0, 1);
  self.SetEntitySetPermissions(LEntityName, APermissions);
end;

end.
