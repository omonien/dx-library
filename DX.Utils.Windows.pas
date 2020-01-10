unit DX.Utils.Windows;

interface

uses
  System.Classes, System.SysUtils;

procedure OpenBrowser(const AUrl: string);

implementation

uses
  Winapi.Windows, Winapi.ShellAPI;

procedure OpenBrowser(const AUrl: string);
begin
  ShellExecute(0, nil, PChar(AURL), nil, nil, SW_SHOWNOACTIVATE);
end;

end.
