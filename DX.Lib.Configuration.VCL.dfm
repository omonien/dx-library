object ConfigurationUI: TConfigurationUI
  Left = 0
  Top = 0
  Caption = 'Configuration'
  ClientHeight = 620
  ClientWidth = 850
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object ScrollBox: TScrollBox
    Left = 0
    Top = 0
    Width = 850
    Height = 521
    HorzScrollBar.Visible = False
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    Color = clWindow
    ParentColor = False
    TabOrder = 0
  end
  object PanelDescription: TPanel
    Left = 0
    Top = 521
    Width = 850
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Color = clInfoBk
    Padding.Left = 8
    Padding.Top = 4
    Padding.Right = 8
    Padding.Bottom = 4
    ParentBackground = False
    TabOrder = 2
    object LabelDescription: TLabel
      Left = 8
      Top = 4
      Width = 834
      Height = 32
      Align = alClient
      Caption = 'Waehlen Sie ein Setting aus, um die Beschreibung anzuzeigen.'
      WordWrap = True
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 561
    Width = 850
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    object ButtonOK: TButton
      Left = 656
      Top = 4
      Width = 90
      Height = 28
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 752
      Top = 4
      Width = 90
      Height = 28
      Cancel = True
      Caption = 'Abbrechen'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 601
    Width = 850
    Height = 19
    Cursor = crHandPoint
    Panels = <
      item
        Width = 500
      end
      item
        Alignment = taRightJustify
        Width = 330
      end>
    OnDblClick = StatusBarDblClick
  end
end
