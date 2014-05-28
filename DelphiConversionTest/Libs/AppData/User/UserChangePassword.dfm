object frmUserChangePassword: TfrmUserChangePassword
  Tag = 12200
  Left = 348
  Top = 223
  ActiveControl = Edit1
  BorderStyle = bsDialog
  Caption = 'Change password'
  ClientHeight = 179
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Tag = 12210
    Left = 16
    Top = 40
    Width = 154
    Height = 15
    Caption = 'Please enter new password'
  end
  object Label2: TLabel
    Tag = 12220
    Left = 16
    Top = 88
    Width = 162
    Height = 15
    Caption = 'Please  retype new password'
  end
  object Label3: TLabel
    Tag = 12230
    Left = 16
    Top = 10
    Width = 67
    Height = 15
    Caption = 'User Name:'
  end
  object Button1: TButton
    Tag = 510
    Left = 103
    Top = 145
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Tag = 520
    Left = 199
    Top = 145
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 16
    Top = 56
    Width = 273
    Height = 23
    PasswordChar = '*'
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 16
    Top = 104
    Width = 273
    Height = 23
    PasswordChar = '*'
    TabOrder = 1
  end
  object stxUserName: TStaticText
    Left = 96
    Top = 8
    Width = 193
    Height = 17
    AutoSize = False
    BorderStyle = sbsSunken
    TabOrder = 4
  end
end
