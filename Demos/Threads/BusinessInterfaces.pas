unit BusinessInterfaces;

interface

uses System.Classes, System.SysUtils;

type
  IPerson = interface(IInterface)
    ['{DAA37C69-A62E-491C-AB5E-49DDEC3AD0B6}']

    function ToString: string;

    procedure SetVorname(const Value: string);
    function GetVorname: string;

    procedure SetNachname(const Value: string);
    function GetNachname: string;

    property Vorname: string read GetVorname write SetVorname;
    property Nachname: string read GetNachname write SetNachname;
  end;

  IKunde = interface(IPerson)
    ['{6DBAAC87-0199-4DEA-94F7-3B4711B322D7}']
    procedure SetKdNr(const Value: integer);
    function GetKdNr: integer;
    property KdNr: integer read GetKdNr write SetKdNr;
  end;

implementation

end.
