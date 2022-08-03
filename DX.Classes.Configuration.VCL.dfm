object ConfigurationUI: TConfigurationUI
  Left = 0
  Top = 0
  Caption = 
    'Configuration - Please re-start application to re-load configura' +
    'tion from file.'
  ClientHeight = 872
  ClientWidth = 1148
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -21
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 168
  TextHeight = 30
  object ListboxKeyValues: TValueListEditor
    Left = 0
    Top = 0
    Width = 1148
    Height = 839
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    DefaultColWidth = 263
    DefaultRowHeight = 32
    KeyOptions = [keyUnique]
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
    Strings.Strings = (
      '=')
    TabOrder = 0
    TitleCaptions.Strings = (
      'Section / Key'
      'Value')
    ColWidths = (
      263
      879)
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 839
    Width = 1148
    Height = 33
    Cursor = crHandPoint
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Panels = <
      item
        Width = 200
      end>
    OnDblClick = StatusBarDblClick
  end
end
