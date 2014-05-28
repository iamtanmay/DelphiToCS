object frmParserEditIdent: TfrmParserEditIdent
  Left = 339
  Top = 205
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Variable _XXX'
  ClientHeight = 307
  ClientWidth = 762
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
    Tag = 41720
    Left = 16
    Top = 16
    Width = 32
    Height = 15
    Caption = 'Name'
  end
  object Label2: TLabel
    Tag = 41730
    Left = 16
    Top = 56
    Width = 63
    Height = 15
    Caption = 'Description:'
  end
  object Label3: TLabel
    Tag = 41740
    Left = 16
    Top = 85
    Width = 73
    Height = 15
    Caption = 'Default Value:'
  end
  object Label4: TLabel
    Tag = 41750
    Left = 16
    Top = 120
    Width = 88
    Height = 15
    Caption = 'Minimum Value:'
  end
  object Label5: TLabel
    Tag = 41760
    Left = 16
    Top = 152
    Width = 89
    Height = 15
    Caption = 'Maximum Value:'
  end
  object Label6: TLabel
    Tag = 41780
    Left = 16
    Top = 190
    Width = 34
    Height = 15
    Caption = 'Label6'
  end
  object Label7: TLabel
    Tag = 1000
    Left = 458
    Top = 19
    Width = 90
    Height = 15
    Caption = 'Parameter Order:'
  end
  object Label9: TLabel
    Tag = 41770
    Left = 458
    Top = 61
    Width = 29
    Height = 15
    Caption = 'Type:'
  end
  object Label10: TLabel
    Left = 537
    Top = 115
    Width = 122
    Height = 15
    Caption = 'Length refers to Order :'
  end
  object edMinValue: TEdit
    Left = 128
    Top = 120
    Width = 121
    Height = 23
    TabOrder = 3
  end
  object edMaxValue: TEdit
    Left = 128
    Top = 152
    Width = 121
    Height = 23
    TabOrder = 4
  end
  object edName: TEdit
    Left = 128
    Top = 16
    Width = 121
    Height = 23
    Enabled = False
    TabOrder = 0
  end
  object edDefault: TEdit
    Left = 128
    Top = 88
    Width = 121
    Height = 23
    TabOrder = 2
  end
  object edDescr: TEdit
    Left = 128
    Top = 56
    Width = 273
    Height = 23
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 266
    Width = 762
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 7
    DesignSize = (
      762
      41)
    object btnOK: TButton
      Tag = 510
      Left = 504
      Top = 9
      Width = 81
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
    object Button2: TButton
      Tag = 520
      Left = 601
      Top = 9
      Width = 81
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object edPickList: TEdit
    Left = 16
    Top = 211
    Width = 393
    Height = 23
    TabOrder = 5
  end
  object Button1: TButton
    Left = 415
    Top = 212
    Width = 23
    Height = 21
    Caption = '...'
    TabOrder = 6
    OnClick = Button1Click
  end
  object spinRequestOrder: TSpinEdit
    Left = 571
    Top = 16
    Width = 49
    Height = 24
    MaxValue = 0
    MinValue = 0
    TabOrder = 8
    Value = 0
  end
  object cbDialogHide: TCheckBox
    Tag = 1003
    Left = 458
    Top = 155
    Width = 201
    Height = 17
    Caption = 'Hide in last page'
    TabOrder = 9
  end
  object cbDialogFormat: TComboBox
    Left = 458
    Top = 82
    Width = 296
    Height = 23
    TabOrder = 10
  end
  object spArrayLengthRefToOrder: TSpinEdit
    Left = 681
    Top = 111
    Width = 49
    Height = 24
    MaxValue = 0
    MinValue = 0
    TabOrder = 11
    Value = 0
  end
  object cbArray: TCheckBox
    Left = 458
    Top = 114
    Width = 63
    Height = 17
    Caption = 'Array,'
    TabOrder = 12
  end
end
