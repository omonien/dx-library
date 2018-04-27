program UAC;

uses
  Vcl.Forms,
  FormMainU in 'FormMainU.pas' {Form94};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm94, Form94);
  Application.Run;
end.
