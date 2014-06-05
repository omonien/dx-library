unit FormMainU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects;

type
  TFormMain = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Rectangle1: TRectangle;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormMain: TFormMain;

implementation

uses
  DX.FMX.Wait, System.IOUtils, Form2U;

{$R *.fmx}

type
  TFormTest = class(TCustomPopupForm)
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  TWait.Start('Loading data...', CheckBox1.IsChecked);
  Timer1.Enabled := true;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  TWait.Stop;
  Timer1.Enabled := false;
end;

end.
