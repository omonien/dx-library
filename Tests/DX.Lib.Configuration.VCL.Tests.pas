unit DX.Lib.Configuration.VCL.Tests;

interface

uses
  DUnitX.TestFramework;

type

  /// <summary>
  /// HighDPI-Tests fuer TConfigurationUI: Die zur Laufzeit erzeugten
  /// Sektions-Controls (Header-Panels, ValueListEditoren) muessen ihre
  /// 96-DPI-Basismasse auf die aktuelle Form-PPI skalieren.
  /// HighDPI wird headless via TControl.ScaleForPPI simuliert.
  /// </summary>
  [TestFixture]
  TConfigurationUIHighDPITests = class
  public
    [Test]
    procedure SectionEditor_RowHeightScalesTo192PPI;
    [Test]
    procedure SectionEditor_KeyColumnWidthScalesTo192PPI;
    [Test]
    procedure SectionHeader_HeightScalesTo192PPI;
    [Test]
    procedure AdjustEditorHeight_FitsAllRowsAt192PPI;
    [Test]
    procedure SectionEditor_BaseValuesAt96PPI;
  end;

  /// <summary>
  /// Connection test hook of TFormConnectionStringEditor: the button exists only
  /// with a registered tester, and the tester gets the currently edited values.
  /// </summary>
  [TestFixture]
  TConnectionStringEditorTests = class
  public
    [Test]
    procedure WithoutTester_ButtonHidden;
    [Test]
    procedure WithTester_ReceivesEditedValuesAndShowsReport;
  end;

implementation

uses
  System.SysUtils, Winapi.Windows, VCL.Controls, VCL.ExtCtrls, VCL.ValEdit,
  DX.Lib.Configuration.VCL, DX.Lib.Configuration.ConnectionStringEditor.Form;

const
  // 96-DPI-Basiswerte des Dialogs (gepinnt gegen DX.Lib.Configuration.VCL)
  cRowHeightBase = 22;
  cHeaderHeightBase = 28;
  cKeyColumnWidthBase = 250;
  cHighPPI = 192; // entspricht 200 % Skalierung

type
  // Zugriff auf die protected Factory-Methoden des Dialogs
  TConfigurationUIAccess = class(TConfigurationUI);

function CreateFormAtPPI(APPI: Integer): TConfigurationUIAccess;
begin
  Result := TConfigurationUIAccess.Create(nil);
  Result.ScaleForPPI(APPI);
end;

procedure TConfigurationUIHighDPITests.SectionEditor_RowHeightScalesTo192PPI;
var
  LForm: TConfigurationUIAccess;
begin
  LForm := CreateFormAtPPI(cHighPPI);
  try
    var LEditor := LForm.CreateSectionEditor(0);
    Assert.AreEqual(MulDiv(cRowHeightBase, cHighPPI, 96), LEditor.DefaultRowHeight,
      'DefaultRowHeight muss mit der Form-PPI skalieren');
  finally
    LForm.Free;
  end;
end;

procedure TConfigurationUIHighDPITests.SectionEditor_KeyColumnWidthScalesTo192PPI;
var
  LForm: TConfigurationUIAccess;
begin
  LForm := CreateFormAtPPI(cHighPPI);
  try
    var LEditor := LForm.CreateSectionEditor(0);
    Assert.AreEqual(MulDiv(cKeyColumnWidthBase, cHighPPI, 96), LEditor.ColWidths[0],
      'Key-Spaltenbreite muss mit der Form-PPI skalieren');
  finally
    LForm.Free;
  end;
end;

procedure TConfigurationUIHighDPITests.SectionHeader_HeightScalesTo192PPI;
var
  LForm: TConfigurationUIAccess;
begin
  LForm := CreateFormAtPPI(cHighPPI);
  try
    var LHeader := LForm.CreateSectionHeader('Test', 0);
    Assert.AreEqual(MulDiv(cHeaderHeightBase, cHighPPI, 96), LHeader.Height,
      'Sektions-Header-Hoehe muss mit der Form-PPI skalieren');
  finally
    LForm.Free;
  end;
end;

procedure TConfigurationUIHighDPITests.AdjustEditorHeight_FitsAllRowsAt192PPI;
var
  LForm: TConfigurationUIAccess;
begin
  LForm := CreateFormAtPPI(cHighPPI);
  try
    var LEditor := LForm.CreateSectionEditor(0);
    LEditor.Values['Alpha'] := '1';
    LEditor.Values['Beta'] := '2';
    LEditor.Values['Gamma'] := '3';
    LForm.AdjustEditorHeight(LEditor);
    var LMinHeight := LEditor.Strings.Count * MulDiv(cRowHeightBase, cHighPPI, 96);
    Assert.IsTrue(LEditor.Height >= LMinHeight,
      Format('Editor-Hoehe %d muss alle %d Zeilen aufnehmen (mindestens %d)',
      [LEditor.Height, LEditor.Strings.Count, LMinHeight]));
  finally
    LForm.Free;
  end;
end;

procedure TConfigurationUIHighDPITests.SectionEditor_BaseValuesAt96PPI;
var
  LForm: TConfigurationUIAccess;
begin
  // Regressionsschutz: bei 96 DPI (100 %) bleiben die Basiswerte unveraendert
  LForm := CreateFormAtPPI(96);
  try
    var LEditor := LForm.CreateSectionEditor(0);
    Assert.AreEqual(cRowHeightBase, LEditor.DefaultRowHeight,
      'DefaultRowHeight bei 96 DPI muss dem Basiswert entsprechen');
    Assert.AreEqual(cKeyColumnWidthBase, LEditor.ColWidths[0],
      'Key-Spaltenbreite bei 96 DPI muss dem Basiswert entsprechen');
    var LHeader := LForm.CreateSectionHeader('Test', 0);
    Assert.AreEqual(cHeaderHeightBase, LHeader.Height,
      'Header-Hoehe bei 96 DPI muss dem Basiswert entsprechen');
  finally
    LForm.Free;
  end;
end;

procedure TConnectionStringEditorTests.WithoutTester_ButtonHidden;
var
  LForm: TFormConnectionStringEditor;
begin
  TFormConnectionStringEditor.ConnectionTester := nil;
  LForm := TFormConnectionStringEditor.Create(nil);
  try
    Assert.IsFalse(LForm.ButtonTestConnection.Visible, 'Ohne Tester kein Test-Button');
    Assert.IsFalse(LForm.MemoTestResult.Visible, 'Ohne Tester kein Ergebnisfeld');
  finally
    LForm.Free;
  end;
end;

procedure TConnectionStringEditorTests.WithTester_ReceivesEditedValuesAndShowsReport;
var
  LForm: TFormConnectionStringEditor;
  LReceived: string;
begin
  TFormConnectionStringEditor.ConnectionTester :=
    function(const AConnectionString: string): string
    begin
      LReceived := AConnectionString;
      Result := 'REPORT-OK';
    end;
  try
    LForm := TFormConnectionStringEditor.Create(nil);
    try
      Assert.IsTrue(LForm.ButtonTestConnection.Visible, 'Mit Tester muss der Test-Button sichtbar sein');
      LForm.ValueListEditor.Values['Server'] := 'srv1';
      LForm.ValueListEditor.Values['DriverID'] := 'MSSQL';
      LForm.ButtonTestConnectionClick(nil);
      Assert.AreEqual('Server=srv1;DriverID=MSSQL', LReceived);
      Assert.IsTrue(LForm.MemoTestResult.Text.Contains('REPORT-OK'), LForm.MemoTestResult.Text);
    finally
      LForm.Free;
    end;
  finally
    TFormConnectionStringEditor.ConnectionTester := nil;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TConfigurationUIHighDPITests);
TDUnitX.RegisterTestFixture(TConnectionStringEditorTests);

end.
