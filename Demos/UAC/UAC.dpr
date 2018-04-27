program UAC;

uses
  Vcl.Forms,
  FormMainU in 'FormMainU.pas' {Form1},
  DX.Windows.UAC in '..\..\DX.Windows.UAC.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
