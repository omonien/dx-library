program ThreadDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormMainU in 'FormMainU.pas' {Form39},
  BusinessInterfaces in 'BusinessInterfaces.pas',
  BusinessClasses in 'BusinessClasses.pas',
  FormPersonU in 'FormPersonU.pas' {FormPerson},
  ViewModelPerson in 'ViewModelPerson.pas',
  DX.Classes.Factory in '..\..\DX.Classes.Factory.pas',
  DX.Threading in '..\..\DX.Threading.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TForm39, Form39);
  Application.Run;
end.
