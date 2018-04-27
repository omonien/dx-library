unit FormMainU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo;

type
  TForm39 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form39: TForm39;

implementation

uses
  DX.Classes.Factory, BusinessInterfaces;

{$R *.fmx}

procedure TForm39.Button1Click(Sender: TObject);
var
  LKunde: Ikunde;
  LPerson: IPerson;
begin
  LPerson := DXFactory.CreateNew(IPerson) as IPerson ;
  LPerson.Vorname := 'Thomas';
  LPerson.Nachname := 'Müller';
  Memo1.Lines.Add(LPerson.ToString);

  LKunde := DXFactory.CreateNew(IKunde) as IKunde;
  LKunde.Vorname := 'Olaf';
  LKunde.Nachname := 'Monien';
  (LKunde as IKunde).KdNr := 42;
  Memo1.Lines.Add(LKunde.ToString);
end;

end.
