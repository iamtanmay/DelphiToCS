object frmMethodVariablesEditor: TfrmMethodVariablesEditor
  Left = 186
  Top = 250
  Caption = 'Parameters'
  ClientHeight = 539
  ClientWidth = 787
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
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 497
    Top = 0
    Height = 503
    ExplicitLeft = 432
    ExplicitTop = 248
    ExplicitHeight = 100
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 503
    Width = 787
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      787
      36)
    object btnOK: TButton
      Tag = 510
      Left = 558
      Top = 6
      Width = 100
      Height = 25
      Anchors = [akRight]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Tag = 520
      Left = 674
      Top = 6
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnLeft: TPanel
    Left = 0
    Top = 0
    Width = 497
    Height = 503
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 240
      Width = 497
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitWidth = 147
    end
    object cxGrid1: TcxGrid
      Left = 0
      Top = 0
      Width = 497
      Height = 240
      Align = alTop
      PopupMenu = PopupMenu1
      TabOrder = 0
      LookAndFeel.Kind = lfStandard
      object cxGrid1TableView1: TcxGridTableView
        PopupMenu = PopupMenu1
        OnDblClick = cxGrid1TableView1DblClick
        NavigatorButtons.ConfirmDelete = False
        DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost, dcoInsertOnNewItemRowFocusing]
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsBehavior.DragFocusing = dfDragDrop
        OptionsBehavior.GoToNextCellOnEnter = True
        OptionsBehavior.PullFocusing = True
        OptionsCustomize.ColumnFiltering = False
        OptionsCustomize.ColumnGrouping = False
        OptionsCustomize.ColumnHidingOnGrouping = False
        OptionsCustomize.ColumnMoving = False
        OptionsCustomize.ColumnSorting = False
        OptionsData.Deleting = False
        OptionsData.DeletingConfirmation = False
        OptionsData.Inserting = False
        OptionsSelection.MultiSelect = True
        OptionsView.GridLineColor = clBtnFace
        OptionsView.GridLines = glHorizontal
        OptionsView.GroupByBox = False
        OptionsView.Indicator = True
        Preview.Visible = True
        object cxGrid1TableView1Column1: TcxGridColumn
          Caption = 'Variable Name'
          Options.Editing = False
          Options.Filtering = False
          Options.FilteringMRUItemsList = False
          Options.Focusing = False
          Width = 186
        end
        object cxGrid1TableView1Column2: TcxGridColumn
          Caption = 'Order Index'
          Options.Editing = False
          Options.Filtering = False
          Options.FilteringMRUItemsList = False
          Options.Focusing = False
          Width = 72
        end
        object cxGrid1TableView1Column3: TcxGridColumn
          Caption = 'Ref. to Order index'
          Options.Editing = False
          Options.Filtering = False
          Options.FilteringMRUItemsList = False
          Options.Focusing = False
          Width = 108
        end
        object cxGrid1TableView1Column4: TcxGridColumn
          Caption = 'Hide in last page'
          Options.Editing = False
          Options.Filtering = False
          Options.FilteringMRUItemsList = False
          Options.Focusing = False
          Width = 101
        end
      end
      object cxGrid1Level1: TcxGridLevel
        GridView = cxGrid1TableView1
      end
    end
    object Panel1: TPanel
      Left = 0
      Top = 243
      Width = 497
      Height = 260
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object cxGrid2: TcxGrid
        Left = 0
        Top = 25
        Width = 497
        Height = 235
        Align = alClient
        TabOrder = 0
        LookAndFeel.Kind = lfStandard
        object cxGrid2TableView1: TcxGridTableView
          PopupMenu = PopupMenu2
          OnDblClick = cxGrid1TableView1DblClick
          NavigatorButtons.ConfirmDelete = False
          DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost, dcoInsertOnNewItemRowFocusing]
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          OptionsBehavior.DragFocusing = dfDragDrop
          OptionsBehavior.GoToNextCellOnEnter = True
          OptionsBehavior.PullFocusing = True
          OptionsCustomize.ColumnFiltering = False
          OptionsCustomize.ColumnGrouping = False
          OptionsCustomize.ColumnHidingOnGrouping = False
          OptionsCustomize.ColumnMoving = False
          OptionsCustomize.ColumnSorting = False
          OptionsData.Inserting = False
          OptionsSelection.MultiSelect = True
          OptionsView.GridLineColor = clBtnFace
          OptionsView.GridLines = glHorizontal
          OptionsView.GroupByBox = False
          OptionsView.Indicator = True
          Preview.Visible = True
          object cxGridColumn2: TcxGridColumn
            Caption = 'First Order Index'
            Options.Filtering = False
            Options.FilteringMRUItemsList = False
            Width = 100
          end
          object cxGridColumn3: TcxGridColumn
            Caption = 'Last Order index (optional)'
            Options.Filtering = False
            Options.FilteringMRUItemsList = False
            Width = 156
          end
          object cxGridColumn4: TcxGridColumn
            Caption = 'Caption (optional)'
            Options.Filtering = False
            Options.FilteringMRUItemsList = False
            Width = 212
          end
        end
        object cxGrid2Level1: TcxGridLevel
          GridView = cxGrid2TableView1
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 497
        Height = 25
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          Left = 8
          Top = 6
          Width = 216
          Height = 15
          Caption = 'Page Definitions (for Multi-Page-Dialog):'
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 500
    Top = 0
    Width = 287
    Height = 503
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object cxTreeList1: TcxTreeList
      Left = 0
      Top = 41
      Width = 287
      Height = 462
      Align = alClient
      Bands = <
        item
        end>
      DefaultLayout = True
      DragMode = dmAutomatic
      LookAndFeel.Kind = lfOffice11
      LookAndFeel.NativeStyle = True
      OptionsCustomizing.BandCustomizing = False
      OptionsCustomizing.BandHorzSizing = False
      OptionsCustomizing.BandMoving = False
      OptionsCustomizing.BandVertSizing = False
      OptionsCustomizing.ColumnCustomizing = False
      OptionsCustomizing.ColumnHorzSizing = False
      OptionsCustomizing.ColumnMoving = False
      OptionsCustomizing.ColumnVertSizing = False
      OptionsData.Editing = False
      OptionsData.Deleting = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.Headers = False
      OptionsView.TreeLineStyle = tllsNone
      TabOrder = 0
      object TVcxTreeListColumn1: TcxTreeListColumn
        DataBinding.ValueType = 'String'
        Width = 100
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 287
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Button1: TButton
        Left = 16
        Top = 10
        Width = 75
        Height = 25
        Caption = 'Refresh'
        TabOrder = 0
        OnClick = Button1Click
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 160
    Top = 97
    object pmnuEdit: TMenuItem
      Caption = 'Edit'
      OnClick = pmnuEditClick
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 112
    Top = 313
    object pmnuAddPage: TMenuItem
      Caption = 'Add'
      OnClick = pmnuAddPageClick
    end
    object DeletePage1: TMenuItem
      Caption = 'Delete selected line(s)'
      OnClick = DeletePage1Click
    end
  end
end
