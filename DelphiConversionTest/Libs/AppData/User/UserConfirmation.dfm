object frmUserConfirmation: TfrmUserConfirmation
  Left = 128
  Top = 444
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Confirmation'
  ClientHeight = 232
  ClientWidth = 454
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 128
    Width = 422
    Height = 15
    Caption = 
      'The CFR 21 compliance requires a confirmation with a statement b' +
      'y the user.'
  end
  object Label2: TLabel
    Left = 16
    Top = 40
    Width = 129
    Height = 15
    Caption = 'You are changing data:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Tag = 12230
    Left = 16
    Top = 10
    Width = 67
    Height = 15
    Caption = 'User Name:'
  end
  object ComboBox1: TComboBox
    Left = 16
    Top = 152
    Width = 417
    Height = 23
    TabOrder = 0
    OnChange = ComboBox1Change
  end
  object btnOK: TButton
    Tag = 510
    Left = 247
    Top = 193
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Tag = 520
    Left = 343
    Top = 193
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 2
  end
  object stxUserName: TStaticText
    Left = 96
    Top = 8
    Width = 193
    Height = 17
    AutoSize = False
    BorderStyle = sbsSunken
    TabOrder = 3
  end
  object Memo1: TMemo
    Left = 16
    Top = 64
    Width = 417
    Height = 49
    TabOrder = 4
  end
end
