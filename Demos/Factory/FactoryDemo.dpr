program FactoryDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormMainU in 'FormMainU.pas' {Form39},
  DX.Classes.Factory in '..\..\DX.Classes.Factory.pas',
  BusinessInterfaces in 'BusinessInterfaces.pas',
  BusinessClasses in 'BusinessClasses.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TForm39, Form39);
  Application.Run;
end.
