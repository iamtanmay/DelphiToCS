object frmParserCodeGenerateDlg: TfrmParserCodeGenerateDlg
  Tag = 44740
  Left = 497
  Top = 293
  Caption = 'Insert Loop'
  ClientHeight = 467
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Shape2: TShape
    Left = 0
    Top = 0
    Width = 461
    Height = 58
    Align = alTop
    Pen.Color = clWhite
    ExplicitWidth = 469
  end
  object Bevel2: TBevel
    Left = 0
    Top = 58
    Width = 461
    Height = 2
    Align = alTop
    Shape = bsTopLine
    ExplicitWidth = 469
  end
  object Bevel1: TBevel
    Left = 0
    Top = 425
    Width = 461
    Height = 1
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 427
    ExplicitWidth = 469
  end
  object mmoHeaderDescription: TMemo
    Left = 23
    Top = 24
    Width = 266
    Height = 25
    TabStop = False
    BorderStyle = bsNone
    Color = clWhite
    Lines.Strings = (
      'Description')
    ReadOnly = True
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 426
    Width = 461
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      461
      41)
    object btnNext: TButton
      Left = 309
      Top = 10
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Tag = 44820
      Left = 388
      Top = 10
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 3
      TabOrder = 1
    end
    object btnPreview: TButton
      Tag = 44810
      Left = 229
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Preview'
      TabOrder = 2
      OnClick = btnPreviewClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 60
    Width = 461
    Height = 365
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object pgCtrl: TPageControl
      Left = 0
      Top = 0
      Width = 461
      Height = 169
      ActivePage = shtDataLoop
      Align = alTop
      TabOrder = 0
      object shtDataLoop: TTabSheet
        Caption = 'DATA LOOP'
        ImageIndex = 2
        DesignSize = (
          453
          139)
        object Label8: TLabel
          Tag = 44980
          Left = 18
          Top = 63
          Width = 71
          Height = 15
          Caption = 'Data variable:'
        end
        object Label9: TLabel
          Tag = 44960
          Left = 18
          Top = 15
          Width = 93
          Height = 15
          Caption = 'Import definition:'
        end
        object Label10: TLabel
          Tag = 44970
          Left = 18
          Top = 39
          Width = 29
          Height = 15
          Caption = 'Filter:'
        end
        object edDataHandleName: TEdit
          Left = 119
          Top = 60
          Width = 325
          Height = 23
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          Text = '_$0Data'
        end
        object edDataFilter: TEdit
          Left = 119
          Top = 36
          Width = 325
          Height = 23
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
        end
        object cmbDataDefName: TComboBox
          Left = 119
          Top = 11
          Width = 326
          Height = 23
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
      end
    end
    object cxTreeList1: TcxTreeList
      Left = 0
      Top = 169
      Width = 461
      Height = 196
      Align = alClient
      Bands = <
        item
        end>
      LookAndFeel.Kind = lfUltraFlat
      LookAndFeel.NativeStyle = True
      OptionsBehavior.AlwaysShowEditor = True
      OptionsBehavior.GoToNextCellOnTab = True
      OptionsBehavior.ImmediateEditor = False
      OptionsBehavior.Sorting = False
      OptionsCustomizing.BandCustomizing = False
      OptionsCustomizing.BandHorzSizing = False
      OptionsCustomizing.BandMoving = False
      OptionsCustomizing.BandVertSizing = False
      OptionsCustomizing.ColumnVertSizing = False
      OptionsView.CellAutoHeight = True
      OptionsView.GridLineColor = clBtnFace
      OptionsView.GridLines = tlglBoth
      OptionsView.PaintStyle = tlpsCategorized
      OptionsView.TreeLineStyle = tllsNone
      TabOrder = 1
      object cxTreeList1cxTreeListColumn2: TcxTreeListColumn
        Caption.MultiLine = True
        Caption.Text = 'Summary'
        DataBinding.ValueType = 'String'
        Options.Customizing = False
        Options.Moving = False
        Options.Sorting = False
        Width = 446
        Position.ColIndex = 0
        Position.LineCount = 2
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
    end
  end
end
