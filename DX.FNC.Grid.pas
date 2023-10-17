unit DX.FNC.Grid;

interface

uses
  System.Classes, System.SysUtils,
  WEBLib.Graphics,
  Vcl.TMSFNCGrid, Vcl.TMSFNCGridOptions, Vcl.TMSFNCGridDatabaseAdapter,
  Vcl.TMSFNCCustomScrollControl;

type

  TTMSFNCGridHelper = class helper for TTMSFNCGrid
    procedure SetDefaults(AAdapter: TTMSFNCGridDatabaseAdapter);
    procedure LoadData;
  end;

  TTMSFNCGridAccess = class(TTMSFNCGrid);

implementation

uses
  Data.DB;

{ TTMSFNCGridHelper }

procedure TTMSFNCGridHelper.LoadData;
begin
  TTMSFNCGridDatabaseAdapter(Adapter).LoadAllDataAndDisconnect;
end;

procedure TTMSFNCGridHelper.SetDefaults(AAdapter: TTMSFNCGridDatabaseAdapter);
var
  i: Integer;
  LDateTimeFormat : string;
  LDataset: TDataSet;
begin
  Adapter := AAdapter;
  Adapter.Active := true;

  // grid
  DefaultRowHeight := 30.000000000000000000;
  Options.ColumnSize.Stretch := true;
  Options.ColumnSize.StretchColumn := 1;
  Options.Filtering.DropDown := true;
  Options.Filtering.MultiColumn := true;

  // Filter Popup/Dropdown Size
  Options.Filtering.DropDownHeight := 200;
  Options.Filtering.DropDownWidth := 200;
  // Filter Popup/Dropdown Font
  TTMSFNCGridAccess(self).FilterListbox.Font.Size := 12;

  Options.Lookup.Enabled := true;
  Options.Sorting.Mode := gsmNormal;
  Options.Selection.Mode := smSingleRow;
  DefaultFont.Color := clWindowText;
  DefaultFont.Height := -15;
  DefaultFont.Name := 'Arial';
  DefaultFont.Style := [];
  Appearance.FixedLayout.Fill.Color := 16380654;
  Appearance.FixedLayout.Font.Color := 4539717;
  Appearance.FixedLayout.Font.Height := -13;
  Appearance.FixedLayout.Font.Name := 'Arial';
  Appearance.FixedLayout.Font.Style := [fsBold];
  Appearance.NormalLayout.Fill.Color := 16578806;
  Appearance.NormalLayout.Font.Color := 8026746;
  Appearance.NormalLayout.Font.Height := -11;
  Appearance.NormalLayout.Font.Name := 'Arial';
  Appearance.GroupLayout.Fill.Color := 12817262;
  Appearance.GroupLayout.Font.Color := clBlack;
  Appearance.GroupLayout.Font.Height := -11;
  Appearance.GroupLayout.Font.Name := 'Arial';
  Appearance.SummaryLayout.Fill.Color := 14009785;
  Appearance.SummaryLayout.Font.Color := clBlack;
  Appearance.SummaryLayout.Font.Height := -11;
  Appearance.SummaryLayout.Font.Name := 'Arial';
  Appearance.SelectedLayout.Fill.Color := 16441019;
  Appearance.SelectedLayout.Font.Color := 4539717;
  Appearance.SelectedLayout.Font.Height := -11;
  Appearance.SelectedLayout.Font.Name := 'Arial';
  Appearance.FocusedLayout.Fill.Color := 16039284;
  Appearance.FocusedLayout.Font.Color := 4539717;
  Appearance.FocusedLayout.Font.Height := -11;
  Appearance.FocusedLayout.Font.Name := 'Arial';
  Appearance.FixedSelectedLayout.Fill.Color := 14599344;
  Appearance.FixedSelectedLayout.Font.Color := clBlack;
  Appearance.FixedSelectedLayout.Font.Height := -11;
  Appearance.FixedSelectedLayout.Font.Name := 'Arial';
  Appearance.BandLayout.Fill.Color := 16711679;
  Appearance.BandLayout.Font.Color := 8026746;
  Appearance.BandLayout.Font.Height := -11;
  Appearance.BandLayout.Font.Name := 'Arial';
  ScrollMode := scmItemScrolling;

  // Columns
  for i := 0 to ColumnCount - 1 do
  begin
    with Columns[i] do
    begin
      BorderWidth := 1;
      FixedFont.Color := 4539717;
      FixedFont.Height := -15;
      FixedFont.Name := 'Arial';
      FixedFont.Style := [fsBold];
      Font.Color := 4539717;
      Font.Height := -15;
      Font.Name := 'Arial';
      Font.Style := [];
      ReadOnly := true
    end;
  end;

  // DateTime Format of connected Dataset
  LDateTimeFormat := FormatSettings.ShortDateFormat + ' ' + FormatSettings.ShortTimeFormat;
  LDataset := AAdapter.DataSource.Dataset;
  for i := 0 to LDataset.FieldCount - 1 do
  begin
    if LDataset.Fields[i] is TDateTimeField then
    begin
      TDateTimeField(LDataset.Fields[i]).DisplayFormat := LDateTimeFormat;
    end;
  end;

end;

end.
