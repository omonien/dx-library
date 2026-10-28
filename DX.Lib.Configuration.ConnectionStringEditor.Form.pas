unit DX.Lib.Configuration.ConnectionStringEditor.Form;

interface

uses
  System.SysUtils, System.Classes, System.UITypes,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ValEdit, Vcl.ExtCtrls, Vcl.Controls;

type
  /// <summary>
  /// Tests a connection string and returns a human-readable report
  /// (success/failure plus driver details). Must not raise.
  /// </summary>
  TConnectionTestFunc = reference to function(const AConnectionString: string): string;

  TFormConnectionStringEditor = class(TForm)
    PanelButtons: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonTestConnection: TButton;
    ValueListEditor: TValueListEditor;
    MemoTestResult: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonTestConnectionClick(Sender: TObject);
  private
    class var FConnectionTester: TConnectionTestFunc;
  private
    FConnectionString: string;
    procedure ParseConnectionString(const AConnectionString: string);
    function BuildConnectionString: string;
  public
    class function Execute(var AConnectionString: string): Boolean;
    /// <summary>
    /// Optional application-wide connection test. When assigned, the editor shows a
    /// "Verbindung testen" button that runs the tester against the connection string
    /// currently being edited (unsaved values included) and shows its report.
    /// The library itself stays driver-agnostic - the application plugs in e.g. FireDAC.
    /// </summary>
    class property ConnectionTester: TConnectionTestFunc read FConnectionTester write FConnectionTester;
  end;

implementation

{$R *.dfm}

class function TFormConnectionStringEditor.Execute(var AConnectionString: string): Boolean;
var
  LForm: TFormConnectionStringEditor;
begin
  LForm := TFormConnectionStringEditor.Create(nil);
  try
    LForm.FConnectionString := AConnectionString;
    Result := LForm.ShowModal = mrOk;
    if Result then
      AConnectionString := LForm.BuildConnectionString;
  finally
    LForm.Free;
  end;
end;

procedure TFormConnectionStringEditor.FormCreate(Sender: TObject);
begin
  ButtonTestConnection.Visible := Assigned(FConnectionTester);
  MemoTestResult.Visible := Assigned(FConnectionTester);
end;

procedure TFormConnectionStringEditor.FormShow(Sender: TObject);
begin
  ParseConnectionString(FConnectionString);
end;

procedure TFormConnectionStringEditor.ButtonTestConnectionClick(Sender: TObject);
var
  LOldCursor: TCursor;
begin
  if not Assigned(FConnectionTester) then
    Exit;
  MemoTestResult.Lines.Text := 'Verbindung wird getestet ...';
  MemoTestResult.Update;
  LOldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  ButtonTestConnection.Enabled := False;
  try
    try
      MemoTestResult.Lines.Text := FConnectionTester(BuildConnectionString);
    except
      // The tester should not raise; if it does, show the error instead of losing the dialog
      on E: Exception do
        MemoTestResult.Lines.Text := E.ClassName + ': ' + E.Message;
    end;
  finally
    ButtonTestConnection.Enabled := True;
    Screen.Cursor := LOldCursor;
  end;
  MemoTestResult.SelStart := 0;
end;

procedure TFormConnectionStringEditor.ParseConnectionString(const AConnectionString: string);
var
  LParts: TStringList;
  LPart, LKey, LValue: string;
  LPos: Integer;
begin
  ValueListEditor.Strings.Clear;

  if AConnectionString.Trim = '' then
    Exit;

  LParts := TStringList.Create;
  try
    LParts.Delimiter := ';';
    LParts.StrictDelimiter := True;
    LParts.DelimitedText := AConnectionString;

    for LPart in LParts do
    begin
      LPos := Pos('=', LPart);
      if LPos > 0 then
      begin
        LKey := Copy(LPart, 1, LPos - 1).Trim;
        LValue := Copy(LPart, LPos + 1, MaxInt).Trim;
        if LKey <> '' then
          ValueListEditor.Values[LKey] := LValue;
      end;
    end;
  finally
    LParts.Free;
  end;
end;

function TFormConnectionStringEditor.BuildConnectionString: string;
var
  I: Integer;
  LKey, LValue: string;
  LParts: TStringList;
begin
  LParts := TStringList.Create;
  try
    for I := 0 to ValueListEditor.Strings.Count - 1 do
    begin
      LKey := ValueListEditor.Strings.Names[I];
      LValue := ValueListEditor.Values[LKey];
      if (LKey <> '') and (LValue <> '') then
        LParts.Add(LKey + '=' + LValue);
    end;
    LParts.Delimiter := ';';
    LParts.StrictDelimiter := True;
    Result := LParts.DelimitedText;
  finally
    LParts.Free;
  end;
end;

end.
