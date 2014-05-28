object frmUserLogin: TfrmUserLogin
  Tag = 12000
  Left = 350
  Top = 194
  ActiveControl = edUsername
  BorderStyle = bsDialog
  Caption = 'Logon information'
  ClientHeight = 173
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Tag = 12020
    Left = 18
    Top = 52
    Width = 65
    Height = 15
    Caption = 'User name:'
  end
  object Label2: TLabel
    Tag = 12030
    Left = 18
    Top = 100
    Width = 59
    Height = 15
    Caption = 'Password:'
  end
  object Label3: TLabel
    Tag = 12010
    Left = 8
    Top = 16
    Width = 385
    Height = 15
    Caption = 
      'Please enter a  username  and a password that is valid for this ' +
      'system'
  end
  object btnOk: TButton
    Tag = 510
    Left = 88
    Top = 137
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnAbort: TButton
    Tag = 530
    Left = 192
    Top = 137
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    TabOrder = 1
    OnClick = btnAbortClick
  end
  object edUsername: TEdit
    Left = 104
    Top = 48
    Width = 205
    Height = 23
    TabOrder = 2
  end
  object edPassword: TEdit
    Left = 104
    Top = 96
    Width = 205
    Height = 23
    PasswordChar = '*'
    TabOrder = 3
  end
end
