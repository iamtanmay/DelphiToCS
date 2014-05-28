object frmViewCompileMessages: TfrmViewCompileMessages
  Tag = 44340
  Left = 88
  Top = 464
  BorderStyle = bsNone
  Caption = 'Compile Messages'
  ClientHeight = 271
  ClientWidth = 870
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object cxTreeList1: TcxTreeList
    Left = 0
    Top = 0
    Width = 870
    Height = 271
    Hint = 'jklfhdjaksfhdkjla'
    Align = alClient
    Bands = <
      item
      end>
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
    PopupMenu = pmnuPopup
    TabOrder = 0
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
  object pmnuPopup: TPopupMenu
    Left = 424
    Top = 120
    object mnuClear: TMenuItem
      Caption = 'Clear'
      OnClick = mnuClearClick
    end
  end
end
