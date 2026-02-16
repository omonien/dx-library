object FormConnectionStringEditor: TFormConnectionStringEditor
  Left = 0
  Top = 0
  Caption = 'Connection String bearbeiten'
  ClientHeight = 400
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Size = 9
  Position = poOwnerFormCenter
  OnShow = FormShow
  object PanelButtons: TPanel
    Left = 0
    Top = 359
    Width = 600
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonOK: TButton
      Left = 432
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object ButtonCancel: TButton
      Left = 513
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Abbrechen'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ValueListEditor: TValueListEditor
    Left = 0
    Top = 0
    Width = 600
    Height = 359
    Align = alClient
    KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
    TabOrder = 0
    TitleCaptions.Strings = (
      'Parameter'
      'Wert')
    ColWidths = (
      200
      394)
  end
end
