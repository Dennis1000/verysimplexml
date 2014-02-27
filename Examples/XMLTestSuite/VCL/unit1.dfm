object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 540
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 629
    Height = 509
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 0
    Top = 515
    Width = 635
    Height = 25
    Align = alBottom
    Caption = 'Run Tests'
    TabOrder = 1
    OnClick = Button1Click
  end
end
