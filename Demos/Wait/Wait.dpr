program Wait;

{$R 'Wheel.res' '..\..\Wheel.rc'}
{$R *.dres}

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  FormMainU in 'FormMainU.pas' {FormMain},
  DX.FMX.Wait in '..\..\DX.FMX.Wait.pas',
  Form2U in 'Form2U.pas' {Form2},
  DX.CrossPlatform in '..\..\DX.CrossPlatform.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
