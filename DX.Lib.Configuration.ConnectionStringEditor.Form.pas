unit DX.Lib.Configuration.ConnectionStringEditor.Form;

interface

uses
  System.SysUtils, System.Classes, System.UITypes,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ValEdit, Vcl.ExtCtrls, Vcl.Controls;

type
  TFormConnectionStringEditor = class(TForm)
    PanelButtons: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ValueListEditor: TValueListEditor;
    procedure FormShow(Sender: TObject);
  private
    FConnectionString: string;
    procedure ParseConnectionString(const AConnectionString: string);
    function BuildConnectionString: string;
  public
    class function Execute(var AConnectionString: string): Boolean;
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

procedure TFormConnectionStringEditor.FormShow(Sender: TObject);
begin
  ParseConnectionString(FConnectionString);
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
