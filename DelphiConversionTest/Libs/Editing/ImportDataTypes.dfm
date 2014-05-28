object FrmImportDataTypes: TFrmImportDataTypes
  Tag = 9815
  Left = 368
  Top = 229
  Caption = 'Change Dataypes'
  ClientHeight = 340
  ClientWidth = 612
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
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 612
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      612
      41)
    object lblImportDefName: TLabel
      Tag = 9200
      Left = 16
      Top = 16
      Width = 129
      Height = 15
      Caption = 'Import Definition Name:'
    end
    object edImportDefName: TEdit
      Left = 152
      Top = 8
      Width = 400
      Height = 23
      Anchors = [akLeft, akRight]
      TabOrder = 0
    end
  end
  object cxGrid1: TcxGrid
    Left = 0
    Top = 41
    Width = 612
    Height = 258
    Align = alClient
    TabOrder = 1
    LookAndFeel.Kind = lfStandard
    object cxGrid1TableView1: TcxGridTableView
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
        Tag = 9360
        Caption = 'Field'
        Options.Filtering = False
        Options.FilteringMRUItemsList = False
        Width = 186
      end
      object cxGrid1TableView1Column2: TcxGridColumn
        Tag = 9810
        Caption = 'Type'
        RepositoryItem = cxEditRepository1ComboBoxItem1
        Options.Filtering = False
        Options.FilteringMRUItemsList = False
        Width = 120
      end
    end
    object cxGrid1Level1: TcxGridLevel
      GridView = cxGrid1TableView1
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 299
    Width = 612
    Height = 41
    Align = alBottom
    TabOrder = 2
    object btnOK: TButton
      Tag = 510
      Left = 440
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Tag = 520
      Left = 528
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object cxEditRepository1: TcxEditRepository
    Left = 88
    Top = 120
    object cxEditRepository1ComboBoxItem1: TcxEditRepositoryComboBoxItem
      Properties.Items.Strings = (
        'AutoInc'
        'String'
        'Int16'
        'Int32'
        'Boolean'
        'Float64'
        'BCD value'
        'Date+Time')
      Properties.ReadOnly = False
    end
  end
end
