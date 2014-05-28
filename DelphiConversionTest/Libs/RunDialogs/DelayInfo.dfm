object fraDelayInfo: TfraDelayInfo
  Left = 385
  Top = 403
  Align = alClient
  ClientHeight = 206
  ClientWidth = 328
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 15
  object grdDelays: TcxGrid
    Left = 0
    Top = 0
    Width = 328
    Height = 206
    Align = alClient
    TabOrder = 0
    object grdDelaysTableView1: TcxGridTableView
      NavigatorButtons.ConfirmDelete = False
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.ColumnSorting = False
      OptionsData.Deleting = False
      OptionsData.Inserting = False
      OptionsSelection.HideFocusRectOnExit = False
      OptionsView.CellAutoHeight = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object grdDelaysTableView1ColCancel: TcxGridColumn
        PropertiesClassName = 'TcxButtonEditProperties'
        Properties.Buttons = <
          item
            Caption = 'Cancel'
            Default = True
            Kind = bkText
            Width = 56
          end>
        Properties.OnButtonClick = grdDelaysTableView1ColCancelPropertiesButtonClick
        MinWidth = 50
        Options.Filtering = False
        Options.Grouping = False
        Options.Moving = False
        Options.Sorting = False
        Width = 56
      end
      object grdDelaysTableView1ColID: TcxGridColumn
        Options.Editing = False
        Options.Filtering = False
        Options.Grouping = False
        Options.Moving = False
        Options.Sorting = False
        Width = 114
      end
      object grdDelaysTableView1ColTime: TcxGridColumn
        OnGetProperties = grdDelaysTableView1ColTimeGetProperties
        Options.Editing = False
        Options.Filtering = False
        Options.Grouping = False
        Options.Sorting = False
        Width = 81
      end
      object grdDelaysTableView1ColDescription: TcxGridColumn
        Options.Filtering = False
        Options.Grouping = False
        Options.Moving = False
        Options.Sorting = False
        Width = 284
      end
      object grdDelaysTableView1ColStatus: TcxGridColumn
        DataBinding.ValueType = 'Boolean'
        Visible = False
        Options.Editing = False
        Options.Filtering = False
        Options.Grouping = False
        Options.Sorting = False
      end
    end
    object grdDelaysLevel1: TcxGridLevel
      GridView = grdDelaysTableView1
    end
  end
  object cxEditRepository1: TcxEditRepository
    Left = 24
    Top = 48
  end
end
