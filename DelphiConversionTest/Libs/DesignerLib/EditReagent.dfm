object frmEditReagent: TfrmEditReagent
  Left = 136
  Top = 328
  Caption = 'frmEditReagent'
  ClientHeight = 166
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 56
    Width = 46
    Height = 15
    Caption = 'Reagent:'
  end
  object Label2: TLabel
    Left = 16
    Top = 96
    Width = 71
    Height = 15
    Caption = 'Start Volume:'
  end
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 26
    Height = 15
    Caption = 'Row:'
  end
  object Label4: TLabel
    Left = 120
    Top = 16
    Width = 46
    Height = 15
    Caption = 'Column:'
  end
  object uL: TLabel
    Left = 240
    Top = 104
    Width = 13
    Height = 15
    Caption = 'uL'
  end
  object edVolume: TEdit
    Left = 96
    Top = 96
    Width = 121
    Height = 23
    TabOrder = 0
  end
  object Button1: TButton
    Left = 216
    Top = 136
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object edCol: TEdit
    Left = 176
    Top = 16
    Width = 57
    Height = 23
    Enabled = False
    TabOrder = 3
  end
  object edRow: TEdit
    Left = 64
    Top = 16
    Width = 33
    Height = 23
    Enabled = False
    TabOrder = 4
  end
  object cbReagentName: TComboBox
    Left = 96
    Top = 56
    Width = 217
    Height = 23
    TabOrder = 5
  end
end
