unit DX.Classes.Configuration.VCL;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.Generics.Collections,
  Winapi.Windows, Winapi.Messages,
  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.Grids, VCL.ValEdit,
  VCL.ComCtrls, VCL.StdCtrls, VCL.ExtCtrls,
  DX.Classes.Configuration.Intf;

type
  TConfigurationUI = class(TForm)
    ScrollBox: TScrollBox;
    StatusBar: TStatusBar;
    PanelDescription: TPanel;
    LabelDescription: TLabel;
    PanelButtons: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure StatusBarDblClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
  class var
    FInstance: TConfigurationUI;
    FConfig: IConfiguration;
    FNewConfigVersion: string;
  var
    FDescriptions: TDictionary<string, string>;
    FConfigVersionKey: string;
    FConfigVersionValue: string;
    FSectionEditors: TDictionary<string, TValueListEditor>;
  protected
    procedure LoadConfig;
    procedure SaveConfig;
    procedure ClearSections;
    procedure UpdateDescription(const AKey: string);
    procedure EditorSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    function CreateSectionHeader(const ACaption: string;
      ATop: Integer; AInvalid: Boolean = False): TPanel;
    function CreateSectionEditor(ATop: Integer;
      AReadOnly: Boolean = False; AInvalid: Boolean = False): TValueListEditor;
    procedure AdjustEditorHeight(AEditor: TValueListEditor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show; reintroduce; overload;
  public
    class procedure Show(AConfig: IConfiguration); overload;
    /// <summary>
    /// Zeigt den Konfigurationsdialog modal an.
    /// Nur registrierte Eintraege werden angezeigt und gespeichert.
    /// ANewConfigVersion: wenn gesetzt, wird ConfigVersion automatisch auf
    /// diesen Wert gesetzt (nicht manuell editierbar).
    /// </summary>
    class function ShowModal(AConfig: IConfiguration;
      const ANewConfigVersion: string = ''): TModalResult; reintroduce;
  end;

implementation

uses
  DX.Classes.Configuration, DX.Classes.Strings, DX.CrossPlatform,
  DX.Utils.Windows, System.IOUtils, Winapi.ShellAPI;

{$R *.dfm}

const
  SECTION_HEADER_HEIGHT = 28;
  ROW_HEIGHT = 22;
  SECTION_GAP = 6;
  HEADER_COLOR = $005A5A5A;
  INVALID_HEADER_COLOR = $002020C0;
  INVALID_BG_COLOR = $00E8E0F0;

{ TConfigurationUI }

constructor TConfigurationUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDescriptions := TDictionary<string, string>.Create;
  FSectionEditors := TDictionary<string, TValueListEditor>.Create;
end;

destructor TConfigurationUI.Destroy;
begin
  FreeAndNil(FSectionEditors);
  FreeAndNil(FDescriptions);
  inherited;
end;

procedure TConfigurationUI.ClearSections;
var
  I: Integer;
begin
  for I := ScrollBox.ControlCount - 1 downto 0 do
    ScrollBox.Controls[I].Free;
  FSectionEditors.Clear;
end;

function TConfigurationUI.CreateSectionHeader(const ACaption: string;
  ATop: Integer; AInvalid: Boolean): TPanel;
var
  LLabel: TLabel;
begin
  Result := TPanel.Create(Self);
  Result.Parent := ScrollBox;
  Result.Top := ATop;
  Result.Height := SECTION_HEADER_HEIGHT;
  Result.Align := alTop;
  Result.BevelOuter := bvNone;
  Result.ParentBackground := False;

  if AInvalid then
    Result.Color := INVALID_HEADER_COLOR
  else
    Result.Color := HEADER_COLOR;

  LLabel := TLabel.Create(Result);
  LLabel.Parent := Result;
  LLabel.Align := alClient;
  LLabel.AlignWithMargins := True;
  LLabel.Margins.Left := 8;
  LLabel.Margins.Top := 2;
  LLabel.Margins.Right := 8;
  LLabel.Margins.Bottom := 2;
  LLabel.Font.Style := [fsBold];
  LLabel.Font.Color := clWhite;
  LLabel.Font.Size := 10;
  LLabel.Caption := ACaption;
  LLabel.Layout := tlCenter;
end;

function TConfigurationUI.CreateSectionEditor(ATop: Integer;
  AReadOnly: Boolean; AInvalid: Boolean): TValueListEditor;
begin
  Result := TValueListEditor.Create(Self);
  Result.Parent := ScrollBox;
  Result.Top := ATop;
  Result.Height := ROW_HEIGHT + 4;
  Result.Align := alTop;
  Result.DefaultRowHeight := ROW_HEIGHT;
  Result.KeyOptions := [keyUnique];
  Result.TitleCaptions.Clear;
  Result.TitleCaptions.Add('Key');
  Result.TitleCaptions.Add('Value');
  Result.OnSelectCell := EditorSelectCell;
  Result.ColWidths[0] := 250;
  Result.ColWidths[1] := ScrollBox.ClientWidth - 250 - 4;
  Result.Options := Result.Options + [goColSizing, goThumbTracking];

  if AInvalid then
  begin
    Result.Font.Color := clRed;
    Result.Color := INVALID_BG_COLOR;
  end;

  if AReadOnly then
    Result.Options := Result.Options - [goEditing];
end;

procedure TConfigurationUI.AdjustEditorHeight(AEditor: TValueListEditor);
begin
  AEditor.Height := AEditor.RowCount * (ROW_HEIGHT + 1) + 4;
end;

procedure TConfigurationUI.EditorSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  LEditor: TValueListEditor;
  LSection: string;
  LKey: string;
begin
  CanSelect := True;
  LEditor := Sender as TValueListEditor;
  if (ARow >= 1) and (ARow <= LEditor.Strings.Count) then
  begin
    LSection := LEditor.HelpKeyword;
    LKey := LSection + '/' + LEditor.Keys[ARow];
    UpdateDescription(LKey);
  end;
end;

procedure TConfigurationUI.LoadConfig;
var
  LConfigItems: TList<TConfigEntry>;
  LConfigItem: TConfigEntry;
  LSectionOrder: TStringList;
  LSectionItems: TObjectDictionary<string, TList<TConfigEntry>>;
  LItemList: TList<TConfigEntry>;
  LRegisteredKeys: TDictionary<string, Boolean>;
  LKey, LValue, LDesc, LSection: string;
  LEditor: TValueListEditor;
  LTop: Integer;
  LIniSections, LIniKeys: TStrings;
  LInvalidEntries: TList<TPair<string, string>>;
begin
  StatusBar.Panels[0].Text := FConfig.Configuration.FileName;
  FDescriptions.Clear;
  FConfigVersionKey := '';
  FConfigVersionValue := '';
  ClearSections;

  LSectionOrder := TStringList.Create;
  LSectionItems := TObjectDictionary<string, TList<TConfigEntry>>.Create([doOwnsValues]);
  LRegisteredKeys := TDictionary<string, Boolean>.Create;
  LInvalidEntries := TList<TPair<string, string>>.Create;
  try
    // Group registered entries by section, skip ConfigVersion
    LConfigItems := TConfigRegistry.Default.LockList;
    try
      for LConfigItem in LConfigItems do
      begin
        LSection := LConfigItem.Section;
        LRegisteredKeys.AddOrSetValue(
          UpperCase(LSection) + '/' + UpperCase(LConfigItem.Name), True);

        // ConfigVersion: nur merken, nicht ins Grid
        if SameText(LConfigItem.Name, 'ConfigVersion') then
        begin
          FConfigVersionKey := LSection + '/' + LConfigItem.Name;
          FConfigVersionValue := FConfig.Configuration.ReadString(
            LSection, LConfigItem.Name, LConfigItem.Default);
          if FNewConfigVersion <> '' then
            FConfigVersionValue := FNewConfigVersion;
          Continue;
        end;

        if not LSectionItems.ContainsKey(LSection) then
        begin
          LSectionOrder.Add(LSection);
          LSectionItems.Add(LSection, TList<TConfigEntry>.Create);
        end;
        LSectionItems[LSection].Add(LConfigItem);

        // Collect descriptions
        if not LConfigItem.Description.IsEmpty then
        begin
          LDesc := '';
          for var S in LConfigItem.Description do
          begin
            if LDesc <> '' then
              LDesc := LDesc + ' ';
            LDesc := LDesc + S;
          end;
          FDescriptions.AddOrSetValue(LSection + '/' + LConfigItem.Name, LDesc);
        end;
      end;
    finally
      TConfigRegistry.Default.UnlockList;
    end;

    // ConfigVersion in StatusBar anzeigen
    if FConfigVersionValue <> '' then
      StatusBar.Panels[1].Text := 'ConfigVersion: ' + FConfigVersionValue
    else
      StatusBar.Panels[1].Text := '';

    // Sektionen alphabetisch sortieren
    LSectionOrder.Sort;

    // Scan INI for unregistered entries
    LIniSections := TStringList.Create;
    try
      FConfig.Configuration.ReadSections(LIniSections);
      for var LIniSection in LIniSections do
      begin
        LIniKeys := TStringList.Create;
        try
          FConfig.Configuration.ReadSection(LIniSection, LIniKeys);
          for var LIniKey in LIniKeys do
          begin
            if not LRegisteredKeys.ContainsKey(
              UpperCase(LIniSection) + '/' + UpperCase(LIniKey)) then
            begin
              LValue := FConfig.Configuration.ReadString(LIniSection, LIniKey, '');
              LInvalidEntries.Add(TPair<string, string>.Create(
                LIniSection + '/' + LIniKey, LValue));
            end;
          end;
        finally
          FreeAndNil(LIniKeys);
        end;
      end;
    finally
      FreeAndNil(LIniSections);
    end;

    // Alignment waehrend der Erstellung unterdruecken
    ScrollBox.DisableAlign;
    try
      // Build per-section UI
      LTop := 0;
      for var I := 0 to LSectionOrder.Count - 1 do
      begin
        LSection := LSectionOrder[I];
        LItemList := LSectionItems[LSection];

        // Section header
        CreateSectionHeader(LSection, LTop);
        Inc(LTop, SECTION_HEADER_HEIGHT);

        // Section editor
        LEditor := CreateSectionEditor(LTop);
        LEditor.HelpKeyword := LSection;
        FSectionEditors.Add(LSection, LEditor);

        // Populate entries
        for LConfigItem in LItemList do
        begin
          LKey := LConfigItem.Name;
          LValue := FConfig.Configuration.ReadString(
            LConfigItem.Section, LConfigItem.Name, LConfigItem.Default);
          LEditor.Values[LKey] := LValue;
        end;

        // Remove default empty entry if present
        for var J := LEditor.Strings.Count - 1 downto 0 do
          if LEditor.Strings.Names[J] = '' then
            LEditor.Strings.Delete(J);

        // Adjust height to fit rows
        AdjustEditorHeight(LEditor);
        Inc(LTop, LEditor.Height + SECTION_GAP);
      end;

      // Invalid entries section
      if LInvalidEntries.Count > 0 then
      begin
        CreateSectionHeader('Ungueltige Eintraege (nicht registriert)', LTop, True);
        Inc(LTop, SECTION_HEADER_HEIGHT);

        LEditor := CreateSectionEditor(LTop, True, True);
        LEditor.HelpKeyword := '';

        for var LPair in LInvalidEntries do
          LEditor.Values[LPair.Key] := LPair.Value;

        // Remove default empty entry if present
        for var J := LEditor.Strings.Count - 1 downto 0 do
          if LEditor.Strings.Names[J] = '' then
            LEditor.Strings.Delete(J);

        AdjustEditorHeight(LEditor);
      end;
    finally
      ScrollBox.EnableAlign;
    end;
  finally
    FreeAndNil(LInvalidEntries);
    FreeAndNil(LRegisteredKeys);
    FreeAndNil(LSectionItems);
    FreeAndNil(LSectionOrder);
  end;
end;

procedure TConfigurationUI.SaveConfig;
var
  LConfigItems: TList<TConfigEntry>;
  LConfigItem: TConfigEntry;
  LEditor: TValueListEditor;
  LValue: string;
  LSections: TStrings;
  LKeys: TStrings;
begin
  // Delete all existing INI keys (cleanup)
  LSections := TStringList.Create;
  try
    FConfig.Configuration.ReadSections(LSections);
    for var LSection in LSections do
    begin
      LKeys := TStringList.Create;
      try
        FConfig.Configuration.ReadSection(LSection, LKeys);
        for var LKeyName in LKeys do
          FConfig.Configuration.DeleteKey(LSection, LKeyName);
      finally
        FreeAndNil(LKeys);
      end;
    end;
  finally
    FreeAndNil(LSections);
  end;

  // Write only registered entries from section editors
  LConfigItems := TConfigRegistry.Default.LockList;
  try
    for LConfigItem in LConfigItems do
    begin
      // ConfigVersion wird separat geschrieben
      if SameText(LConfigItem.Name, 'ConfigVersion') then
        Continue;
      if FSectionEditors.TryGetValue(LConfigItem.Section, LEditor) then
      begin
        LValue := LEditor.Values[LConfigItem.Name];
        FConfig.Configuration.WriteString(
          LConfigItem.Section, LConfigItem.Name, LValue);
      end;
    end;
  finally
    TConfigRegistry.Default.UnlockList;
  end;

  // ConfigVersion schreiben
  if FConfigVersionKey <> '' then
  begin
    var LSepPos := Pos('/', FConfigVersionKey);
    if LSepPos > 0 then
    begin
      var LSection := Copy(FConfigVersionKey, 1, LSepPos - 1);
      var LName := Copy(FConfigVersionKey, LSepPos + 1, MaxInt);
      FConfig.Configuration.WriteString(LSection, LName, FConfigVersionValue);
    end;
  end;

  FConfig.Save;
end;

procedure TConfigurationUI.UpdateDescription(const AKey: string);
var
  LDesc: string;
begin
  if FDescriptions.TryGetValue(AKey, LDesc) then
    LabelDescription.Caption := LDesc
  else
    LabelDescription.Caption := '';
end;

procedure TConfigurationUI.ButtonOKClick(Sender: TObject);
begin
  SaveConfig;
end;

procedure TConfigurationUI.Show;
begin
  inherited Show;
  LoadConfig;
end;

class procedure TConfigurationUI.Show(AConfig: IConfiguration);
begin
  if FInstance = nil then
    FInstance := TConfigurationUI.Create(Application);
  FConfig := AConfig;
  FNewConfigVersion := '';
  FInstance.Show;
end;

class function TConfigurationUI.ShowModal(AConfig: IConfiguration;
  const ANewConfigVersion: string): TModalResult;
var
  LForm: TConfigurationUI;
begin
  LForm := TConfigurationUI.Create(Application);
  try
    FConfig := AConfig;
    FNewConfigVersion := ANewConfigVersion;
    LForm.LoadConfig;
    Result := TForm(LForm).ShowModal;
  finally
    FreeAndNil(LForm);
  end;
end;

procedure TConfigurationUI.StatusBarDblClick(Sender: TObject);
var
  LHandle: HWND;
begin
  if Assigned(Application.MainForm) then
    LHandle := Application.MainForm.Handle
  else
    LHandle := 0;
  ShellExecute(LHandle, 'open',
    PChar(FConfig.Configuration.FileName), nil, nil, SW_SHOW);
end;

end.
