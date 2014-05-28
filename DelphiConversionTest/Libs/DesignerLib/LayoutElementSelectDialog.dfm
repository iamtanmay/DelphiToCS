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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
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
  object pnlBottom: TPanel
    Left = 0
    Top = 368
    Width = 752
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      752
      44)
    object btnOK: TButton
      Tag = 530
      Left = 572
      Top = 11
      Width = 74
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TButton
      Tag = 540
      Left = 660
      Top = 11
      Width = 74
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ModalResult = 2
      ParentFont = False
      TabOrder = 1
    end
  end
  object pnlLayout: TPanel
    Left = 0
    Top = 44
    Width = 752
    Height = 274
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 318
    Width = 752
    Height = 50
    ActivePage = tbsRack
    Align = alBottom
    MultiLine = True
    TabOrder = 3
    TabPosition = tpRight
    object tbsCarrier: TTabSheet
      Caption = 'Carrier'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        Text = 'cmbCarrierName'
        OnChange = cmbCarrierNameChange
      end
      object cmbCarrierSlot: TComboBox
        Left = 417
        Top = 12
        Width = 56
        Height = 23
        TabOrder = 1
        Text = 'cmbCarrierSlot'
      end
    end
    object tbsRack: TTabSheet
      Caption = 'tbsRack'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblPositionSeparator: TLabel
        Left = 400
        Top = 16
        Width = 3
        Height = 15
        Caption = ':'
      end
      object lblRack: TLabel
        Left = 8
        Top = 16
        Width = 25
        Height = 15
        Caption = 'Rack'
      end
      object lblPosition: TLabel
        Left = 290
        Top = 16
        Width = 43
        Height = 15
        Caption = 'Position'
      end
      object lblNoOfPos: TLabel
        Left = 547
        Top = 16
        Width = 83
        Height = 15
        Caption = 'Number of pos.'
      end
      object edRackFirstPos: TEdit
        Left = 340
        Top = 12
        Width = 49
        Height = 23
        Alignment = taRightJustify
        TabOrder = 0
        Visible = False
      end
      object edRackLastPos: TEdit
        Left = 416
        Top = 12
        Width = 49
        Height = 23
        Alignment = taRightJustify
        TabOrder = 1
        Visible = False
      end
      object cmbRackName: TComboBox
        Left = 50
        Top = 12
        Width = 231
        Height = 23
        TabOrder = 2
        Text = 'cmbRackName'
      end
      object edNoOfPos: TEdit
        Left = 646
        Top = 12
        Width = 49
        Height = 23
        Alignment = taRightJustify
        ReadOnly = True
        TabOrder = 3
      end
      object btnRefresh: TButton
        Left = 471
        Top = 11
        Width = 62
        Height = 25
        Caption = 'Refresh'
        TabOrder = 4
        OnClick = btnRefreshClick
      end
    end
  end
end
