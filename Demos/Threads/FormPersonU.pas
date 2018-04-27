unit FormPersonU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, ViewModelPerson;

type
  TFormPerson = class(TForm)
    EditNachname: TEdit;
    ClearEditButton1: TClearEditButton;
    Label1: TLabel;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FPersonChangedID: Integer;
  public
    { Public-Deklarationen }
  end;

implementation

uses
  System.Messaging, System.Threading, BusinessInterfaces, DX.Threading;

{$R *.fmx}

procedure TFormPerson.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  TMessageManager.DefaultManager.Unsubscribe(TPErsonChanged, FPersonChangedID);
end;

procedure TFormPerson.FormCreate(Sender: TObject);
begin
  FPersonChangedID := TMessageManager.DefaultManager.SubscribeToMessage(TPersonChanged,
    procedure(const ASender: TObject; const AMessage: TMessage)
    var
      LPerson: IPerson;
    begin
      // Die Nachrichten kommen im MainThread an, daher sollte die VErarbeitung schnell gehen.
      // Wir simulieren hier per sleep eine langsame Verarbeitung und das Auslagern in einen Thread
      LPerson := (AMessage as TPersonChanged).Value;

      TTask.Run(
        procedure
        begin
          // Dies simuliert eine aufwändige Berechnung/Verarbeitung der Nachricht, die daher in einem
          // Hintergrundthread ausgeführt wird. Ohne TTask.Run wären wir 100*500ms = 50 Sekunden in der
          // Oberfäche blockiert
          BusyWait(500);
          // Das eigentliche Updaten der UI muss synchronisiert werden (d.h. im Mainthread erfolgen)
          // Wir können viele FormPerson-Instanzen haben, daher hier ein Queue, um die Oberfläche zu entlasten
          TThread.Synchronize(nil,
            procedure
            begin
              EditNachname.Text := LPerson.Nachname;
            end);

        end);

    end);
end;

end.
