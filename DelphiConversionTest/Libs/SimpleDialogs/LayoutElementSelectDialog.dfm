object frmLayoutElementSelectDialog: TfrmLayoutElementSelectDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Layout Element Selection Dialog'
  ClientHeight = 412
  ClientWidth = 752
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 22
    Top = 319
    Width = 25
    Height = 15
    Caption = 'Rack'
  end
  object Label2: TLabel
    Left = 304
    Top = 319
    Width = 43
    Height = 15
    Caption = 'Position'
  end
  object Label3: TLabel
    Left = 414
    Top = 319
    Width = 3
    Height = 15
    Caption = ':'
  end
  object Label4: TLabel
    Left = 561
    Top = 319
    Width = 83
    Height = 15
    Caption = 'Number of pos.'
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 752
    Height = 44
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 22
      Top = 12
      Width = 155
      Height = 21
      Caption = 'Select Layout Element:'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object rbInteger: TRadioButton
      Left = 431
      Top = 14
      Width = 113
      Height = 17
      Caption = 'columnwise'
      TabOrder = 0
    end
    object rbMatrix: TRadioButton
      Left = 312
      Top = 14
      Width = 113
      Height = 17
      Caption = 'square'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
  end
  object pnlLayout: TPanel
    Left = 0
    Top = 44
    Width = 752
    Height = 258
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 302
    Width = 752
    Height = 110
    ActivePage = tbsRack
    Align = alBottom
    MultiLine = True
    TabOrder = 2
    TabPosition = tpRight
    object tbsCarrier: TTabSheet
      Caption = 'Carrier'
      object lblCarrierName: TLabel
        Left = 28
        Top = 16
        Width = 35
        Height = 15
        Caption = 'Carrier'
      end
      object lblCarrierSlot: TLabel
        Left = 372
        Top = 16
        Width = 20
        Height = 15
        Caption = 'Slot'
      end
      object cmbCarrierName: TComboBox
        Left = 73
        Top = 12
        Width = 256
        Height = 23
        TabOrder = 0
        OnChange = cmbCarrierNameChange
      end
      object cmbCarrierSlot: TComboBox
        Left = 417
        Top = 12
        Width = 56
        Height = 23
        TabOrder = 1
      end
    end
    object tbsRack: TTabSheet
      Caption = 'tbsRack'
      ImageIndex = 1
      object lblPositionSeparator: TLabel
        Left = 410
        Top = 40
        Width = 3
        Height = 15
        Caption = ':'
      end
      object lblRack: TLabel
        Left = 18
        Top = 40
        Width = 23
        Height = 15
        Caption = 'Dest'
      end
      object lblPosition: TLabel
        Left = 300
        Top = 40
        Width = 43
        Height = 15
        Caption = 'Position'
      end
      object lblNoOfPos: TLabel
        Left = 557
        Top = 40
        Width = 83
        Height = 15
        Caption = 'Number of pos.'
      end
      object lblVolume: TLabel
        Left = 18
        Top = 69
        Width = 41
        Height = 15
        Caption = 'Volume'
      end
      object lblSubstance: TLabel
        Left = 524
        Top = 69
        Width = 54
        Height = 15
        Caption = 'Substance'
      end
      object lblLiqParam: TLabel
        Left = 203
        Top = 69
        Width = 140
        Height = 15
        Caption = 'Liquid handling parameter'
      end
      object lblSource: TLabel
        Left = 18
        Top = 11
        Width = 36
        Height = 15
        Caption = 'Source'
      end
      object lblSourcePosition: TLabel
        Left = 300
        Top = 11
        Width = 43
        Height = 15
        Caption = 'Position'
      end
      object Label7: TLabel
        Left = 410
        Top = 11
        Width = 3
        Height = 15
        Caption = ':'
      end
      object lblNoOfSourcePos: TLabel
        Left = 557
        Top = 11
        Width = 83
        Height = 15
        Caption = 'Number of pos.'
      end
      object edRackFirstPos: TEdit
        Left = 350
        Top = 36
        Width = 49
        Height = 23
        Alignment = taRightJustify
        TabOrder = 0
        Visible = False
      end
      object edRackLastPos: TEdit
        Left = 426
        Top = 36
        Width = 49
        Height = 23
        Alignment = taRightJustify
        TabOrder = 1
        Visible = False
      end
      object cmbRackName: TComboBox
        Left = 65
        Top = 36
        Width = 226
        Height = 23
        TabOrder = 2
      end
      object edNoOfPos: TEdit
        Left = 656
        Top = 36
        Width = 49
        Height = 23
        Alignment = taRightJustify
        ReadOnly = True
        TabOrder = 3
      end
      object btnRefresh: TButton
        Left = 481
        Top = 35
        Width = 62
        Height = 25
        Caption = 'Refresh'
        TabOrder = 4
        OnClick = btnRefreshClick
      end
      object edVolume: TEdit
        Left = 65
        Top = 65
        Width = 121
        Height = 23
        TabOrder = 5
      end
      object cmbLiqParam: TComboBox
        Left = 349
        Top = 65
        Width = 158
        Height = 23
        TabOrder = 6
      end
      object cmbSubstance: TComboBox
        Left = 584
        Top = 66
        Width = 121
        Height = 23
        TabOrder = 7
      end
      object cmbSourceRackname: TComboBox
        Left = 65
        Top = 7
        Width = 226
        Height = 23
        TabOrder = 8
      end
      object edSourceRackFirstPos: TEdit
        Left = 350
        Top = 7
        Width = 49
        Height = 23
        Alignment = taRightJustify
        TabOrder = 9
      end
      object edSourceRackLastPos: TEdit
        Left = 426
        Top = 7
        Width = 49
        Height = 23
        Alignment = taRightJustify
        TabOrder = 10
      end
      object btnRefreshSource: TButton
        Left = 481
        Top = 6
        Width = 62
        Height = 25
        Caption = 'Refresh'
        TabOrder = 11
        OnClick = btnRefreshClick
      end
      object edNoOfSourcePos: TEdit
        Left = 656
        Top = 7
        Width = 49
        Height = 23
        Alignment = taRightJustify
        ReadOnly = True
        TabOrder = 12
      end
    end
  end
  object pnlSourceLayout: TPanel
    Left = 0
    Top = 44
    Width = 752
    Height = 258
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
  end
end
