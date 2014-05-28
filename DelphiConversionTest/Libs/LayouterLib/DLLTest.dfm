object frmDLLTest: TfrmDLLTest
  Tag = 29760
  Left = 314
  Top = 194
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DLL Test'
  ClientHeight = 226
  ClientWidth = 631
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Tag = 29700
    Left = 5
    Top = 24
    Width = 70
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'DLL Name:'
  end
  object Label2: TLabel
    Tag = 29710
    Left = 5
    Top = 56
    Width = 70
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Function:'
  end
  object Label3: TLabel
    Tag = 29720
    Left = 5
    Top = 88
    Width = 70
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Parameter:'
  end
  object Label4: TLabel
    Tag = 29730
    Left = 5
    Top = 168
    Width = 70
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Result:'
  end
  object Label5: TLabel
    Left = 424
    Top = 23
    Width = 96
    Height = 15
    Caption = 'Number of retries:'
  end
  object Edit1: TEdit
    Left = 80
    Top = 20
    Width = 145
    Height = 23
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 80
    Top = 52
    Width = 145
    Height = 23
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 80
    Top = 84
    Width = 521
    Height = 23
    TabOrder = 2
  end
  object btnExe: TButton
    Tag = 29740
    Left = 80
    Top = 120
    Width = 121
    Height = 25
    Caption = 'Execute'
    TabOrder = 3
    OnClick = btnExeClick
  end
  object Edit4: TEdit
    Left = 81
    Top = 165
    Width = 520
    Height = 23
    ReadOnly = True
    TabOrder = 4
  end
  object btnThreadExe: TButton
    Left = 256
    Top = 120
    Width = 121
    Height = 25
    Caption = 'Execute in new thread'
    TabOrder = 5
    OnClick = btnThreadExeClick
  end
  object rgVersion: TRadioGroup
    Left = 272
    Top = 8
    Width = 137
    Height = 49
    Caption = 'Version'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'V8'
      'V6')
    TabOrder = 6
  end
  object spMaxRetries: TSpinEdit
    Left = 536
    Top = 20
    Width = 49
    Height = 24
    MaxValue = 0
    MinValue = 0
    TabOrder = 7
    Value = 0
  end
end
