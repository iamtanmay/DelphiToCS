object frmViewHierarchy: TfrmViewHierarchy
  Tag = 44200
  Left = 302
  Top = 274
  BorderStyle = bsNone
  Caption = 'Method Hierarchy'
  ClientHeight = 587
  ClientWidth = 377
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
    Width = 377
    Height = 587
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
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnDblClick = cxTreeList1DblClick
    object TVcxTreeListColumn1: TcxTreeListColumn
      Tag = 44230
      Caption.Text = 'Method'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Options.Sorting = False
      Width = 312
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TVcxTreeListColumn2: TcxTreeListColumn
      Tag = 44240
      Caption.Text = 'Option'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Options.Sorting = False
      Width = 250
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 56
    Top = 184
    object pmnuExpandAll: TMenuItem
      Tag = 44210
      Caption = 'Expand all'
      OnClick = pmnuExpandAllClick
    end
    object pmnuCollapseAll: TMenuItem
      Tag = 44220
      Caption = 'Collapse all'
      OnClick = pmnuCollapseAllClick
    end
  end
end
