object FrmSelTablePath: TFrmSelTablePath
  Tag = 9690
  Left = 432
  Top = 311
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'FrmSelTablePath'
  ClientHeight = 188
  ClientWidth = 433
  Color = clBtnHighlight
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel1: TBevel
    Left = 0
    Top = 149
    Width = 433
    Height = 3
    Align = alBottom
    ExplicitTop = 295
    ExplicitWidth = 242
  end
  object Label4: TLabel
    Tag = 14230
    Left = 8
    Top = 79
    Width = 67
    Height = 15
    Caption = 'Table Name:'
  end
  object Label2: TLabel
    Tag = 14240
    Left = 8
    Top = 20
    Width = 63
    Height = 15
    Caption = 'Alias Name:'
  end
  object Panel1: TPanel
    Left = 0
    Top = 152
    Width = 433
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Tag = 510
      Left = 208
      Top = 6
      Width = 91
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Tag = 520
      Left = 313
      Top = 6
      Width = 91
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object cmbTableName: TComboBox
    Left = 8
    Top = 100
    Width = 410
    Height = 23
    Style = csDropDownList
    TabOrder = 1
  end
  object cmbAlias: TComboBox
    Left = 8
    Top = 41
    Width = 281
    Height = 23
    Style = csDropDownList
    TabOrder = 2
    OnChange = cmbAliasChange
  end
  object RefreshAliasNames1: TButton
    Left = 295
    Top = 40
    Width = 123
    Height = 25
    Caption = 'Refresh Alias Names'
    TabOrder = 3
    OnClick = RefreshAliasNames1Click
  end
end
