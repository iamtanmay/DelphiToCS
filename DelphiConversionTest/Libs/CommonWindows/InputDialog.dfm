object frmInputDialog: TfrmInputDialog
  Tag = 13000
  Left = 212
  Top = 203
  BorderStyle = bsDialog
  Caption = 'Select entry'
  ClientHeight = 121
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Shape2: TShape
    Left = 0
    Top = 0
    Width = 368
    Height = 73
    Align = alTop
    Pen.Color = clWhite
  end
  object Bevel2: TBevel
    Left = 0
    Top = 73
    Width = 368
    Height = 48
    Align = alTop
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 56
    Top = 16
    Width = 64
    Height = 15
    Caption = 'Select item:'
    Color = clWhite
    ParentColor = False
  end
  object btnOK: TButton
    Tag = 530
    Left = 188
    Top = 86
    Width = 74
    Height = 23
    Caption = 'Continue'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Tag = 540
    Left = 276
    Top = 86
    Width = 74
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 56
    Top = 35
    Width = 281
    Height = 23
    TabOrder = 0
  end
end
