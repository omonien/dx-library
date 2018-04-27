unit FormMainU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  BusinessInterfaces, System.Messaging, System.Threading;

type
  TForm39 = class(TForm)
    Button1: TButton;
    EditNumberOfForms: TEdit;
    Label1: TLabel;
    EditNachname: TEdit;
    Label2: TLabel;
    LabelTiming: TLabel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure EditNachnameChangeTracking(Sender: TObject);
    procedure EditNumberOfFormsChangeTracking(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button2Click(Sender: TObject);
  private
    FPerson: IPerson;
    FPersonChangedID: integer;
    procedure UpdateUI;
    procedure PersonChanged;
  public
    { Public-Deklarationen }
  end;

var
  Form39: TForm39;

implementation

uses
  DX.Classes.Factory, FormPersonU, ViewModelPerson, DX.Threading;

{$R *.fmx}

procedure TForm39.Button1Click(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to EditNumberOfForms.Text.ToInteger do
  begin
    TFormPerson.Create(self).Show;
  end;

  // Allen Formularen mitteilen, wie der Anfangszustand ist
  PersonChanged;
end;

procedure TForm39.Button2Click(Sender: TObject);
begin
  BusyWait(5000);
  ShowMessage('Done');
end;

procedure TForm39.EditNachnameChangeTracking(Sender: TObject);
begin
  FPerson.Nachname := EditNachname.Text;
  PersonChanged;
end;

procedure TForm39.EditNumberOfFormsChangeTracking(Sender: TObject);
begin
  UpdateUI;
end;

procedure TForm39.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  TMessageManager.DefaultManager.Unsubscribe(TPersonChanged, FPersonChangedID);
end;

procedure TForm39.FormCreate(Sender: TObject);
begin
  Label2.Text := 'Max worker threads: ' + TThreadPool.Default.MaxWorkerThreads.ToString;
  // Für Benachrichtigungen des Nachrichtentyps "TPersonChanged" anmelden
  FPersonChangedID := TMessageManager.DefaultManager.SubscribeToMessage(TPersonChanged,
    procedure(const ASender: TObject; const AMessage: TMessage)
    begin
      UpdateUI;
    end);

  // Eine Person als Anfangszustand erzeugen
  FPerson := DXFactory.CreateNew(IKunde) as IPerson;
  FPerson.Vorname := 'Olaf';
  FPerson.Nachname := 'Monien';

  UpdateUI;
end;

procedure TForm39.UpdateUI;
begin
  // Dies wird durch das Empfangen der TPersonChanged Message getriggert. Wir nehmen hier für das Beispiel an,
  // das das hinreichend schnell läuft.
  // Im TFormPerson dagegen simulieren wir ein langsames Verarbeiten
  EditNachname.Text := FPerson.Nachname;
  LabelTiming.Text := 'Benachrichtigungszeit ohne Threads: ' + ((EditNumberOfForms.Text.ToInteger * 500) div 1000).ToString + ' sec';

end;

procedure TForm39.PersonChanged;
begin
  // Messages müssen im MainThread verschickt werden. Daher ist drauf zu achten, dass die Empfänger nicht unnötig
  // blockieren, da das die Oberfläche zähe machen kann
  TMessageManager.DefaultManager.SendMessage(self, TPersonChanged.Create(FPerson));
end;

end.
