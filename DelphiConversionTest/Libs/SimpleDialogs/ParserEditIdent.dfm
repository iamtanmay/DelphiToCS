object frmParserEditIdent: TfrmParserEditIdent
  Left = 339
  Top = 205
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Variable _XXX'
  ClientHeight = 305
  ClientWidth = 483
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
    Top = 88
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
    Top = 192
    Width = 34
    Height = 15
    Caption = 'Label6'
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
    Width = 337
    Height = 23
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 264
    Width = 483
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 8
    object btnApply: TButton
      Tag = 590
      Left = 372
      Top = 9
      Width = 81
      Height = 25
      Caption = 'Apply'
      TabOrder = 2
      OnClick = btnApplyClick
    end
    object btnOK: TButton
      Tag = 510
      Left = 192
      Top = 9
      Width = 81
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
    object Button2: TButton
      Tag = 520
      Left = 282
      Top = 9
      Width = 81
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object rgValueFormat: TRadioGroup
    Tag = 41770
    Left = 280
    Top = 88
    Width = 185
    Height = 81
    ItemIndex = 0
    Items.Strings = (
      'No format'
      'Value is a path name'
      'Value is a file name')
    TabOrder = 5
  end
  object edPickList: TEdit
    Left = 16
    Top = 216
    Width = 425
    Height = 23
    TabOrder = 6
  end
  object Button1: TButton
    Left = 442
    Top = 216
    Width = 23
    Height = 21
    Caption = '...'
    TabOrder = 7
    OnClick = Button1Click
  end
end
