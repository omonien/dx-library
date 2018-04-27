unit BusinessClasses;

interface

uses System.Classes, System.SysUtils,
  BusinessInterfaces;

type

  TPerson = class;

  TPerson = class(TInterfacedObject, IPerson)
  strict private
    FNachname: string;
    FVorname: string;
  private
    FAnrede: string;
    procedure SetAnrede(const Value: string);

    function GetNachname: string;
    function GetVorname: string;
    procedure SetNachname(const Value: string);
    procedure SetVorname(const Value: string);
  public
    constructor Create(const AVorname, ANachname: string); reintroduce;
    destructor Destroy; override;

    property Anrede: string read FAnrede write SetAnrede;
    property Vorname: string read GetVorname write SetVorname;
    property Nachname: string read GetNachname write SetNachname;
    function ToString: string; override;
  end;

  TKunde = class(TPerson, IKunde)
  private
    FKdNr: integer;
    procedure SetKdNr(const Value: integer);
    function GetKdNr: integer;
  public
    property KdNr: integer read GetKdNr write SetKdNr;
    function ToString: string; override;
  end;

implementation

uses
  DX.Classes.Factory;

{ TPerson }

constructor TPerson.Create(const AVorname, ANachname: string);
begin
  inherited Create;
  FVorname := AVorname;
  FNachname := ANachname;
end;

destructor TPerson.Destroy;
begin
  inherited;
end;

function TPerson.GetNachname: string;
begin
  result := FNachname;
end;

function TPerson.GetVorname: string;
begin
  result := FVorname;
end;

procedure TPerson.SetAnrede(const Value: string);
begin
  FAnrede := Value;
end;

procedure TPerson.SetNachname(const Value: string);
begin
  FNachname := Value;
end;

procedure TPerson.SetVorname(const Value: string);
begin
  FVorname := Value;
end;

function TPerson.ToString: string;
begin
  result := (Anrede + ' ' + Vorname + ' ' + Nachname).Trim;
end;

{ TKunde }

function TKunde.GetKdNr: integer;
begin
  result := FKdNr;
end;

procedure TKunde.SetKdNr(const Value: integer);
begin
  FKdNr := Value;
end;

function TKunde.ToString: string;
begin
  result := inherited ToString + ', KdNr: ' + KdNr.ToString;
end;

initialization

DXFactory.RegisterClassForInterface(TPerson, IPerson);
DXFactory.RegisterClassForInterface(TKunde, IKunde);

(* PersonFactory := TPersonFactory.Create;
  begin
  Result := TPerson.Create(AVorname,AName);
  end);
*)
end.
