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

implementation

uses
  System.SysUtils, Winapi.Windows, VCL.Controls, VCL.ExtCtrls, VCL.ValEdit,
  DX.Lib.Configuration.VCL;

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

initialization

TDUnitX.RegisterTestFixture(TConfigurationUIHighDPITests);

end.
