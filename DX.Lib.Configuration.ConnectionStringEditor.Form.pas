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
  protected
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
  I, LPos: Integer;
  LKey, LValue, LPart: string;

  procedure AddPair(const APair: string);
  begin
    LPos := Pos('=', APair);
    if LPos > 0 then
    begin
      // Keys may be trimmed. Values stay verbatim, including leading spaces.
      LKey := Copy(APair, 1, LPos - 1).Trim;
      LValue := Copy(APair, LPos + 1, MaxInt);
      if LKey <> '' then
        ValueListEditor.Values[LKey] := LValue;
    end;
  end;

  // AIndex points at the opening quote. "" inside the pair is one literal quote.
  function ReadQuoted(var AIndex: Integer): string;
  begin
    Inc(AIndex);
    Result := '';
    while AIndex <= Length(AConnectionString) do
    begin
      if AConnectionString[AIndex] = '"' then
      begin
        if (AIndex < Length(AConnectionString)) and (AConnectionString[AIndex + 1] = '"') then
        begin
          Result := Result + '"';
          Inc(AIndex, 2);
        end
        else
        begin
          Inc(AIndex);
          Exit;
        end;
      end
      else
      begin
        Result := Result + AConnectionString[AIndex];
        Inc(AIndex);
      end;
    end;
  end;

  // ';' inside {...} is part of the value, same as SplitConnectionString.
  function ReadPlain(var AIndex: Integer): string;
  var
    LStart, LDepth: Integer;
  begin
    LStart := AIndex;
    LDepth := 0;
    while AIndex <= Length(AConnectionString) do
    begin
      case AConnectionString[AIndex] of
        '{':
          Inc(LDepth);
        '}':
          if LDepth > 0 then
            Dec(LDepth);
        ';':
          if LDepth = 0 then
            Break;
      end;
      Inc(AIndex);
    end;
    Result := Copy(AConnectionString, LStart, AIndex - LStart);
  end;

begin
  ValueListEditor.Strings.Clear;

  if AConnectionString.Trim = '' then
    Exit;

  I := 1;
  while I <= Length(AConnectionString) do
  begin
    if AConnectionString[I] = '"' then
      LPart := ReadQuoted(I)
    else
      LPart := ReadPlain(I);
    if LPart <> '' then
      AddPair(LPart);
    if (I <= Length(AConnectionString)) and (AConnectionString[I] = ';') then
      Inc(I);
  end;
end;

function TFormConnectionStringEditor.BuildConnectionString: string;
var
  I: Integer;
  LKey, LValue, LPair: string;
  LParts: TStringList;
begin
  LParts := TStringList.Create;
  try
    for I := 0 to ValueListEditor.Strings.Count - 1 do
    begin
      LKey := ValueListEditor.Strings.Names[I];
      LValue := ValueListEditor.Values[LKey];
      if (LKey <> '') and (LValue <> '') then
      begin
        LPair := LKey + '=' + LValue;
        // Quote the whole pair, not only the value. A non-strict TStringList
        // (Delimiter=';') treats space as a delimiter and honors '"' only when
        // it is the first character of a segment. AnsiQuotedStr escapes '"' as '""'.
        if (Pos(' ', LValue) > 0) or (Pos(';', LValue) > 0) or (Pos('"', LValue) > 0) then
          LParts.Add(AnsiQuotedStr(LPair, '"'))
        else
          LParts.Add(LPair);
      end;
    end;
    LParts.Delimiter := ';';
    LParts.StrictDelimiter := True;
    // Reading DelimitedText would quote again any item that already contains '"' or ';'.
    Result := '';
    for I := 0 to LParts.Count - 1 do
    begin
      if I > 0 then
        Result := Result + LParts.Delimiter;
      Result := Result + LParts[I];
    end;
  finally
    LParts.Free;
  end;
end;

end.
