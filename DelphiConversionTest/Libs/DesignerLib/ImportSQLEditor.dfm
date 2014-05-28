object frmSQLEditor: TfrmSQLEditor
  Left = 0
  Top = 0
  Caption = 'SQL Editor'
  ClientHeight = 337
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel4: TBevel
    Left = 0
    Top = 291
    Width = 635
    Height = 3
    Align = alBottom
    ExplicitTop = 334
    ExplicitWidth = 452
  end
  object pnlSQLMiddle: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 291
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 145
      Top = 0
      Height = 291
      ExplicitHeight = 345
    end
    object mmoSQLBuilder: TMemo
      Left = 148
      Top = 0
      Width = 487
      Height = 291
      Align = alClient
      TabOrder = 0
    end
    object pnlSQLTools: TPanel
      Left = 0
      Top = 0
      Width = 145
      Height = 291
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      OnResize = pnlSQLToolsResize
      DesignSize = (
        145
        291)
      object lblSQLFieldName: TLabel
        Tag = 9360
        Left = 8
        Top = 78
        Width = 56
        Height = 13
        Caption = 'Field Name:'
      end
      object lblSQLTablePath: TLabel
        Tag = 9350
        Left = 8
        Top = 5
        Width = 55
        Height = 13
        Caption = 'Table Path:'
      end
      object btnInsertTableName: TButton
        Tag = 9370
        Left = 8
        Top = 46
        Width = 113
        Height = 25
        Caption = 'Insert Table Path'
        TabOrder = 2
        OnClick = btnInsertTableNameClick
      end
      object cmbSQLFieldName: TComboBox
        Left = 8
        Top = 91
        Width = 130
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
      end
      object btnSQLInsertFiledName: TButton
        Tag = 9370
        Left = 8
        Top = 116
        Width = 113
        Height = 25
        Caption = 'Insert Field Name'
        TabOrder = 4
        OnClick = btnSQLInsertFiledNameClick
      end
      object edSQLTablePath: TEdit
        Left = 8
        Top = 21
        Width = 105
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object btnSQLSelectTablePath: TButton
        Left = 113
        Top = 19
        Width = 25
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        TabStop = False
        OnClick = btnSQLSelectTablePathClick
      end
    end
  end
  object pnSQLBottom: TPanel
    Left = 0
    Top = 294
    Width = 635
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      635
      43)
    object btnSQLOK: TButton
      Tag = 510
      Left = 404
      Top = 6
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button1: TButton
      Tag = 520
      Left = 526
      Top = 6
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
