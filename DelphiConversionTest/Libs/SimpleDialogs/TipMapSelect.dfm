object fraTipMapSelect: TfraTipMapSelect
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  ClientHeight = 278
  ClientWidth = 157
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 15
  object cxTipGrid: TcxGrid
    Left = 7
    Top = 3
    Width = 145
    Height = 272
    PopupMenu = pmnuTips
    TabOrder = 0
    object cxTipGridTableView1: TcxGridTableView
      NavigatorButtons.ConfirmDelete = False
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.GroupByBox = False
      object cxTipGridTableView1Column1: TcxGridColumn
        Options.Filtering = False
        Options.FilteringMRUItemsList = False
        Options.Grouping = False
        Width = 48
      end
      object cxTipGridTableView1Column2: TcxGridColumn
        Options.Editing = False
        Options.Filtering = False
        Options.FilteringMRUItemsList = False
        Options.Grouping = False
        Width = 94
      end
    end
    object cxTipGridLevel1: TcxGridLevel
      GridView = cxTipGridTableView1
    end
  end
  object cxEditRepository1: TcxEditRepository
    Left = 80
    object cxEditRepository1CheckBoxItem1: TcxEditRepositoryCheckBoxItem
      Properties.OnEditValueChanged = cxEditRepository1CheckBoxItem1PropertiesEditValueChanged
    end
    object cxEditRepository1TextItem1: TcxEditRepositoryTextItem
    end
  end
  object pmnuTips: TPopupMenu
    Left = 32
    Top = 120
    object SelectAll1: TMenuItem
      Tag = 31210
      Caption = 'Select All'
      OnClick = SelectAll1Click
    end
  end
end
