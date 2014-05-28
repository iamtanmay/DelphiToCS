object frmReagentEditor: TfrmReagentEditor
  Left = 318
  Top = 329
  Caption = 'Reagent definition'
  ClientHeight = 260
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 71
    Height = 15
    Caption = 'Substance ID:'
  end
  object Label2: TLabel
    Left = 24
    Top = 53
    Width = 57
    Height = 15
    Caption = 'Full Name:'
  end
  object Label3: TLabel
    Left = 24
    Top = 82
    Width = 63
    Height = 15
    Caption = 'Description:'
  end
  object Label4: TLabel
    Left = 24
    Top = 127
    Width = 93
    Height = 15
    Caption = 'Liquid Parameter:'
  end
  object Edit2: TEdit
    Left = 128
    Top = 50
    Width = 385
    Height = 23
    TabOrder = 0
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 128
    Top = 79
    Width = 385
    Height = 23
    TabOrder = 1
    OnChange = Edit3Change
  end
  object ComboBox1: TComboBox
    Left = 128
    Top = 124
    Width = 209
    Height = 23
    TabOrder = 2
    OnChange = ComboBox1Change
  end
  object GroupBox7: TGroupBox
    Tag = 51550
    Left = 81
    Top = 164
    Width = 168
    Height = 70
    Caption = 'Appearance'
    TabOrder = 3
    object Shape1: TShape
      Left = 14
      Top = 26
      Width = 33
      Height = 33
      Brush.Color = clScrollBar
    end
    object btnColor: TButton
      Tag = 51560
      Left = 63
      Top = 32
      Width = 89
      Height = 25
      Caption = 'Change Color'
      TabOrder = 0
      OnClick = btnColorClick
    end
  end
  object ColorDialog1: TColorDialog
    Left = 428
    Top = 179
  end
end
