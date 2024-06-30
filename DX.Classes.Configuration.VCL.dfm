object ConfigurationUI: TConfigurationUI
  Left = 0
  Top = 0
  Caption = 
    'Configuration - Please re-start application to re-load configura' +
    'tion from file.'
  ClientHeight = 498
  ClientWidth = 656
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object ListboxKeyValues: TValueListEditor
    Left = 0
    Top = 0
    Width = 656
    Height = 479
    Align = alClient
    KeyOptions = [keyUnique]
    Strings.Strings = (
      '=')
    TabOrder = 0
    TitleCaptions.Strings = (
      'Section / Key'
      'Value')
    ColWidths = (
      150
      500)
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 479
    Width = 656
    Height = 19
    Cursor = crHandPoint
    Panels = <
      item
        Width = 114
      end>
    OnDblClick = StatusBarDblClick
  end
end
