object ParserSetValueForm: TParserSetValueForm
  Tag = 41500
  Left = 378
  Top = 17
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = 'Method Parameter Values'
  ClientHeight = 451
  ClientWidth = 768
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 768
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    object lblHeaderTitle: TLabel
      Left = 16
      Top = 8
      Width = 3
      Height = 13
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Tag = 41520
      Left = 32
      Top = 24
      Width = 176
      Height = 15
      Caption = 'Please enter the parameter values'
    end
  end
  object cxTreeList1: TcxTreeList
    Left = 0
    Top = 49
    Width = 768
    Height = 402
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
    OptionsView.GridLineColor = clBtnFace
    OptionsView.GridLines = tlglBoth
    OptionsView.PaintStyle = tlpsCategorized
    OptionsView.TreeLineStyle = tllsNone
    TabOrder = 1
    OnEdited = cxTreeList1Edited
    OnExit = cxTreeList1Exit
    OnKeyDown = cxTreeList1KeyDown
    object cxTreeList1cxTreeListColumn1: TcxTreeListColumn
      PropertiesClassName = 'TcxPopupEditProperties'
      Caption.Text = 'Parameter'
      DataBinding.ValueType = 'String'
      Options.Customizing = False
      Options.Editing = False
      Options.Focusing = False
      Options.Moving = False
      Options.Sorting = False
      Width = 412
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxTreeList1cxTreeListColumn2: TcxTreeListColumn
      Caption.Text = 'Value'
      DataBinding.ValueType = 'String'
      Options.Customizing = False
      Options.Moving = False
      Options.Sorting = False
      Width = 318
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
      OnGetEditingProperties = cxTreeList1cxTreeListColumn2GetEditingProperties
    end
    object cxTreeList1cxTreeListColumn3: TcxTreeListColumn
      Visible = False
      Caption.Text = 'Index'
      DataBinding.ValueType = 'String'
      Options.Sizing = False
      Options.Customizing = False
      Options.Moving = False
      Options.Sorting = False
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxTreeList1cxTreeListColumn4: TcxTreeListColumn
      Visible = False
      DataBinding.ValueType = 'String'
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  object cxEditRepository1: TcxEditRepository
    Left = 264
    Top = 8
  end
end
