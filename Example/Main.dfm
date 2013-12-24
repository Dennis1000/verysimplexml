object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'VerySimpleXML - Example'
  ClientHeight = 586
  ClientWidth = 704
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    704
    586)
  PixelsPerInch = 96
  TextHeight = 13
  object BtnGenerate: TButton
    Left = 8
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Generate XML'
    TabOrder = 0
    OnClick = BtnGenerateClick
  end
  object Memo1: TMemo
    Left = 104
    Top = 8
    Width = 592
    Height = 570
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Lucida Console'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WantReturns = False
    WordWrap = False
  end
  object BtnModify: TButton
    Left = 8
    Top = 39
    Width = 81
    Height = 25
    Caption = 'Modifiy'
    Enabled = False
    TabOrder = 2
    OnClick = BtnModifyClick
  end
  object BtnCompact: TButton
    Left = 8
    Top = 70
    Width = 81
    Height = 25
    Caption = 'Compact'
    Enabled = False
    TabOrder = 3
    OnClick = BtnCompactClick
  end
  object BtnSave: TButton
    Left = 8
    Top = 101
    Width = 81
    Height = 25
    Caption = 'Save'
    Enabled = False
    TabOrder = 4
    OnClick = BtnSaveClick
  end
  object BtnLoad: TButton
    Left = 8
    Top = 132
    Width = 81
    Height = 25
    Caption = 'Load'
    Enabled = False
    TabOrder = 5
    OnClick = BtnLoadClick
  end
  object BtnGet: TButton
    Left = 8
    Top = 163
    Width = 81
    Height = 25
    Caption = 'Get Desc'
    Enabled = False
    TabOrder = 6
    OnClick = BtnGetClick
  end
end
