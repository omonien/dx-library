program SortDemo;

uses
  Vcl.Forms,
  FormMainU in 'FormMainU.pas' {Form71},
  DX.Sort.Introsort in '..\..\DX.Sort.Introsort.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm71, Form71);
  Application.Run;
end.
