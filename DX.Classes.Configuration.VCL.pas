unit DX.Classes.Configuration.VCL;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  Winapi.Windows, Winapi.Messages,
  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.Grids, VCL.ValEdit, DX.Classes.Configuration.Intf,
  VCL.ComCtrls;

type
  TConfigurationUI = class(TForm)
    ListboxKeyValues: TValueListEditor;
    StatusBar: TStatusBar;
    procedure StatusBarDblClick(Sender: TObject);
  private
  class var
    FInstance: TConfigurationUI;
    FConfig: IConfiguration;
  protected
    procedure LoadConfig;
  public
    procedure Show; reintroduce; overload;
  public
    class procedure Show<I: IConfiguration>(AConfig: I); overload;
  end;

implementation

uses
  DX.Classes.Configuration, DX.CrossPlatform, DX.Utils.Windows, System.IOUtils, Winapi.ShellAPI;

{$R *.dfm}

{ TFormDXConfiguration }

procedure TConfigurationUI.LoadConfig;
var
  LSections: TStrings;
begin
  StatusBar.Panels[0].Text := FConfig.Configuration.FileName;

  ListboxKeyValues.Strings.Clear;
  LSections := TStringList.Create;
  try
    FConfig.Configuration.ReadSections(LSections);
    for var LSection in LSections do
    begin
      var
      LKeys := TStringList.Create;
      try
        FConfig.Configuration.ReadSection(LSection, LKeys);
        for var LKey in LKeys do
        begin
          ListboxKeyValues.Strings.AddPair(LSection + ' / ' + LKey, FConfig.Configuration.ReadString(LSection,
            LKey, ''));
        end;
      finally
        FreeAndNil(LKeys);
      end;
    end;
  finally
    FreeAndNil(LSections);
  end;
  if ListboxKeyValues.Strings is TStringList then
  begin
    TStringList(ListboxKeyValues.Strings).Sort;
  end;
end;

procedure TConfigurationUI.Show;
begin
  inherited Show;
  LoadConfig;
end;

class procedure TConfigurationUI.Show<I>(AConfig: I);
begin
  if FInstance = nil then
  begin
    FInstance := TConfigurationUI.Create(Application);
  end;
  FConfig := AConfig;
  FInstance.Show;
end;

procedure TConfigurationUI.StatusBarDblClick(Sender: TObject);
var
  LHandle: HWND;
begin
  ShellExecute(LHandle, 'open', PChar(FConfig.Configuration.FileName), nil, nil, SW_SHOW);
end;

end.
