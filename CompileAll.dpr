program CompileAll;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,

{$IF Defined(IOS) or Defined(MACOS)}
  DX.Apple.Utils in 'DX.Apple.Utils.pas',
{$ENDIF}
  DX.Classes.Factory in 'DX.Classes.Factory.pas',
  DX.CrossPlatform in 'DX.CrossPlatform.pas',
  DX.FMX.Wait in 'DX.FMX.Wait.pas',
  DX.Settings in 'DX.Settings.pas',
  DX.Sort.Introsort in 'DX.Sort.Introsort.pas',
  DX.SysUtils in 'DX.SysUtils.pas',
  DX.Threading.Command in 'DX.Threading.Command.pas',
  DX.Threading in 'DX.Threading.pas',
  DX.Types.GeoLocation in 'DX.Types.GeoLocation.pas',
  DX.Types.Nullable in 'DX.Types.Nullable.pas',
  DX.Utils.Connectivity in 'DX.Utils.Connectivity.pas',
  DX.Utils.IO in 'DX.Utils.IO.pas',
  DX.Utils.Logger in 'DX.Utils.Logger.pas';

begin
  try
    Writeln('DX.Library compiled successfully!');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
