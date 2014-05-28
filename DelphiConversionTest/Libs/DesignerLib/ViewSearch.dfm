object frmViewSearch: TfrmViewSearch
  Tag = 44300
  Left = 291
  Top = 376
  Align = alClient
  BorderStyle = bsNone
  Caption = 'Search in Methods'
  ClientHeight = 383
  ClientWidth = 809
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object cxTreeList1: TcxTreeList
    Left = 0
    Top = 0
    Width = 809
    Height = 383
    Hint = 'jklfhdjaksfhdkjla'
    Align = alClient
    Bands = <
      item
      end>
    DragMode = dmAutomatic
    OptionsBehavior.ExpandOnDblClick = False
    OptionsCustomizing.BandCustomizing = False
    OptionsCustomizing.BandHorzSizing = False
    OptionsCustomizing.BandMoving = False
    OptionsCustomizing.BandVertSizing = False
    OptionsCustomizing.ColumnCustomizing = False
    OptionsCustomizing.ColumnMoving = False
    OptionsCustomizing.ColumnVertSizing = False
    OptionsData.Editing = False
    OptionsData.Deleting = False
    TabOrder = 0
    OnCustomDrawDataCell = cxTreeList1CustomDrawDataCell
    OnDblClick = cxTreeList1DblClick
    object TVcxTreeListColumn1: TcxTreeListColumn
      Tag = 44230
      Caption.Text = 'Method'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Options.Sorting = False
      Width = 199
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TVcxTreeListColumn2: TcxTreeListColumn
      Tag = 44240
      Caption.Text = 'Line'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Options.Sorting = False
      Width = 45
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TVcxTreeListColumn3: TcxTreeListColumn
      Caption.Text = 'Text'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Options.Sorting = False
      Width = 554
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
end
