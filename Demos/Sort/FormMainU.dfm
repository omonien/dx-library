object Form71: TForm71
  Left = 0
  Top = 0
  Caption = 'Form71'
  ClientHeight = 411
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 423
    Top = 32
    Width = 98
    Height = 25
    Caption = 'Test Int Array'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 32
    Width = 409
    Height = 193
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button2: TButton
    Left = 423
    Top = 63
    Width = 98
    Height = 25
    Caption = 'Test String Array'
    TabOrder = 2
    OnClick = Button2Click
  end
end
