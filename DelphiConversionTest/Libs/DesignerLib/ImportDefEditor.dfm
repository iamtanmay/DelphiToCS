object frmImportDefEditor: TfrmImportDefEditor
  Tag = 9050
  Left = 163
  Top = 239
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Import Definition'
  ClientHeight = 489
  ClientWidth = 568
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel2: TBevel
    Left = 0
    Top = 0
    Width = 568
    Height = 3
    Align = alTop
    ExplicitTop = 38
    ExplicitWidth = 472
  end
  object pnlMiddle: TPanel
    Left = 0
    Top = 3
    Width = 568
    Height = 486
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlTargetAndSourceTops: TPanel
      Left = 0
      Top = 0
      Width = 568
      Height = 49
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 0
      object Splitter1: TSplitter
        Left = 220
        Top = 1
        Height = 47
        Beveled = True
        MinSize = 60
        ExplicitLeft = 206
      end
      object pnlSourceTop: TPanel
        Left = 223
        Top = 1
        Width = 344
        Height = 47
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object Label2: TLabel
          Tag = 9080
          Left = 8
          Top = 3
          Width = 115
          Height = 15
          Caption = 'Source File Definition:'
        end
        object cmbSourceDef: TComboBox
          Left = 8
          Top = 18
          Width = 193
          Height = 23
          Style = csDropDownList
          TabOrder = 0
          OnChange = cmbSourceDefChange
          OnDropDown = cmbSourceDefDropDown
        end
        object btnOpenFileDef: TButton
          Left = 207
          Top = 17
          Width = 26
          Height = 25
          Caption = '...'
          TabOrder = 1
          OnClick = btnOpenFileDefClick
        end
      end
      object pnlTargetTop: TPanel
        Left = 1
        Top = 1
        Width = 219
        Height = 47
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          219
          47)
        object Label1: TLabel
          Tag = 9070
          Left = 6
          Top = 3
          Width = 58
          Height = 15
          Caption = 'Target File:'
        end
        object edTargetFile: TEdit
          Left = 7
          Top = 18
          Width = 180
          Height = 23
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 0
        end
        object btnGetTargetFileName: TButton
          Left = 188
          Top = 16
          Width = 24
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 1
          OnClick = btnGetTargetFileNameClick
        end
      end
    end
    object cxGrid1: TcxGrid
      Left = 0
      Top = 49
      Width = 568
      Height = 437
      Align = alClient
      PopupMenu = PopupMenu1Grid
      TabOrder = 1
      LookAndFeel.Kind = lfStandard
      LookAndFeel.NativeStyle = True
      object cxGrid1TableView1: TcxGridTableView
        NavigatorButtons.ConfirmDelete = False
        OnEditChanged = cxGrid1TableView1EditChanged
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
          Caption = 'Target Name'
          Options.Filtering = False
          Options.FilteringMRUItemsList = False
          Width = 186
        end
        object cxGrid1TableView1Column2: TcxGridColumn
          Caption = 'Source Field'
          RepositoryItem = cxEditRepository1ComboBoxItem1
          Options.Filtering = False
          Options.FilteringMRUItemsList = False
          Width = 120
        end
        object cxGrid1TableView1Column3: TcxGridColumn
          Caption = 'Required'
          RepositoryItem = cxEditRepository1CheckBoxItem1
          Options.Filtering = False
          Options.FilteringMRUItemsList = False
          Width = 60
        end
        object cxGrid1TableView1Column4: TcxGridColumn
          Caption = 'Default value'
          Options.Filtering = False
          Options.FilteringMRUItemsList = False
          Width = 89
        end
      end
      object cxGrid1Level1: TcxGridLevel
        GridView = cxGrid1TableView1
      end
    end
  end
  object cxEditRepository1: TcxEditRepository
    Left = 88
    Top = 120
    object cxEditRepository1CheckBoxItem1: TcxEditRepositoryCheckBoxItem
    end
    object cxEditRepository1ComboBoxItem1: TcxEditRepositoryComboBoxItem
    end
  end
  object PopupMenu1Grid: TPopupMenu
    Left = 184
    Top = 121
    object pmnuAppend: TMenuItem
      Tag = 9100
      Caption = 'Append Line'
      OnClick = pmnuAppendClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmnuDelete: TMenuItem
      Tag = 9110
      Caption = 'Delete Selected Line(s)'
      OnClick = pmnuDeleteClick
    end
    object pmnuMatchTarget: TMenuItem
      Tag = 9130
      Caption = 'Match to target'
      OnClick = pmnuMatchTargetClick
    end
  end
end
