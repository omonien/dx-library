unit FormMainU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure EndWait;
    procedure StartWait;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
  DX.Windows.UAC;

{$R *.dfm}

const
  SOME_ELEVATED_JOB = '/SomethingElevated';

procedure TForm1.FormCreate(Sender: TObject);
begin
  Label1.Caption := Format('IsAdministrator: %s', [BoolToStr(IsAdministrator, true)]);
  Label2.Caption := Format('IsAdministratorAccount: %s', [BoolToStr(IsAdministratorAccount, true)]);
  Label3.Caption := Format('IsUACEnabled: %s', [BoolToStr(IsUACEnabled, true)]);
  Label4.Caption := Format('IsElevated: %s', [BoolToStr(IsElevated, true)]);

  Button1.Caption := 'Do something elevated';
  SetButtonElevated(Button1.Handle);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  StartWait;
  try
    SetLastError(RunElevated(SOME_ELEVATED_JOB, Handle, Application.ProcessMessages));
    if GetLastError <> ERROR_SUCCESS then
      RaiseLastOSError;
  finally
    EndWait
  end;
end;

function RunElevatedJob(const AParameters: String): Cardinal;

  procedure DoSomethingElevated;
  var
    Msg: String;
  begin
    Msg := 'Hello from SomethingElevated!' + sLineBreak + sLineBreak + 'This function is running elevated under full administrator rights.' + sLineBreak +
      'This means that you have write-access to Program Files folder and you''re able to overwrite files (e.g. install updates).' + sLineBreak +
      'However, note that your executable is still running.' + sLineBreak + sLineBreak + 'IsAdministrator: ' + BoolToStr(IsAdministrator, true) + sLineBreak +
      'IsAdministratorAccount: ' + BoolToStr(IsAdministratorAccount, true) + sLineBreak + 'IsUACEnabled: ' + BoolToStr(IsUACEnabled, true) + sLineBreak +
      'IsElevated: ' + BoolToStr(IsElevated, true);
    MessageBox(0, PChar(Msg), 'Hello from InstallUpdate!', MB_OK or MB_ICONINFORMATION);
  end;

begin
  Result := ERROR_SUCCESS;
  if AParameters = SOME_ELEVATED_JOB then
    DoSomethingElevated
  else
    Result := ERROR_GEN_FAILURE;
end;

procedure TForm1.StartWait;
begin
  Cursor := crHourglass;
  Screen.Cursor := crHourglass;
  Button1.Enabled := false;
  Application.ProcessMessages;
end;

procedure TForm1.EndWait;
begin
  Cursor := crDefault;
  Screen.Cursor := crDefault;
  Button1.Enabled := true;
  Application.ProcessMessages;
end;

initialization
  OnElevateProc := RunElevatedJob;
  CheckForElevatedTask;

end.
