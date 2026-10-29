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
    /// <summary>
    /// Values that contain a space, ';' or '"' are quoted as a whole pair and
    /// come back unchanged. SetParamsFromConnectionString of that string yields
    /// the same password.
    /// </summary>
    [Test]
    procedure QuotedValuesRoundTripAndSurviveParams;
    /// <summary>
    /// Unquoted legacy strings keep spaces in values, and keys may still be trimmed.
    /// </summary>
    [Test]
    procedure ParseKeepsUnquotedValueText;
  end;

implementation

uses
  System.SysUtils, Winapi.Windows, VCL.Controls, VCL.ExtCtrls, VCL.ValEdit,
  FireDAC.Comp.Client,
  DX.Lib.Configuration.VCL, DX.Lib.Configuration.ConnectionStringEditor.Form,
  DX.Data.Utils;

const
  // 96-DPI-Basiswerte des Dialogs (gepinnt gegen DX.Lib.Configuration.VCL)
  cRowHeightBase = 22;
  cHeaderHeightBase = 28;
  cKeyColumnWidthBase = 250;
  cHighPPI = 192; // entspricht 200 % Skalierung

type
  // Zugriff auf die protected Factory-Methoden des Dialogs
  TConfigurationUIAccess = class(TConfigurationUI);
  TConnectionStringEditorAccess = class(TFormConnectionStringEditor);

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
      Assert.IsTrue(LForm.MemoTestResult.Lines.Text.Contains('REPORT-OK'), LForm.MemoTestResult.Lines.Text);
    finally
      LForm.Free;
    end;
  finally
    TFormConnectionStringEditor.ConnectionTester := nil;
  end;
end;

procedure TConnectionStringEditorTests.QuotedValuesRoundTripAndSurviveParams;
const
  cPasswords: array[0..3] of string = ('my secret', ' lead', 'a;b', 'a"b');
var
  LForm: TConnectionStringEditorAccess;
  LPassword, LBuilt, LExpected: string;
  LConnection: TFDConnection;
begin
  TFormConnectionStringEditor.ConnectionTester := nil;
  LForm := TConnectionStringEditorAccess.Create(nil);
  try
    for LPassword in cPasswords do
    begin
      LForm.ValueListEditor.Strings.Clear;
      LForm.ValueListEditor.Values['User_Name'] := 'sysdba';
      LForm.ValueListEditor.Values['Password'] := LPassword;
      LBuilt := LForm.BuildConnectionString;
      LExpected := 'User_Name=sysdba;' + AnsiQuotedStr('Password=' + LPassword, '"');
      Assert.AreEqual(LExpected, LBuilt, 'built string for [' + LPassword + ']');

      LForm.ParseConnectionString(LBuilt);
      Assert.AreEqual('sysdba', LForm.ValueListEditor.Values['User_Name'],
        'parsed user for [' + LPassword + ']');
      Assert.AreEqual(LPassword, LForm.ValueListEditor.Values['Password'],
        'parsed password [' + LPassword + ']');

      LConnection := TFDConnection.Create(nil);
      try
        LConnection.SetParamsFromConnectionString(LBuilt);
        Assert.AreEqual('sysdba', LConnection.Params.Values['User_Name'],
          'params user for [' + LPassword + ']');
        Assert.AreEqual(LPassword, LConnection.Params.Values['Password'],
          'params password [' + LPassword + ']');
      finally
        LConnection.Free;
      end;
    end;
  finally
    LForm.Free;
  end;
end;

procedure TConnectionStringEditorTests.ParseKeepsUnquotedValueText;
var
  LForm: TConnectionStringEditorAccess;
begin
  LForm := TConnectionStringEditorAccess.Create(nil);
  try
    LForm.ParseConnectionString(
      'Database=C:\My Data\Test.fdb; Password= lead;ODBCAdvanced={A=1;B=2}');
    Assert.AreEqual('C:\My Data\Test.fdb', LForm.ValueListEditor.Values['Database']);
    // The key is trimmed; the leading space in the value is part of the value.
    Assert.AreEqual(' lead', LForm.ValueListEditor.Values['Password']);
    Assert.AreEqual('{A=1;B=2}', LForm.ValueListEditor.Values['ODBCAdvanced']);
  finally
    LForm.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TConfigurationUIHighDPITests);
TDUnitX.RegisterTestFixture(TConnectionStringEditorTests);

end.
