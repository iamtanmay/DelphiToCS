object frmDialogInputRunStepEditor: TfrmDialogInputRunStepEditor
  Left = 186
  Top = 250
  Caption = 'Parameters for INPUT Action'
  ClientHeight = 258
  ClientWidth = 794
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object pnlBottom: TPanel
    Left = 0
    Top = 222
    Width = 794
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      794
      36)
    object btnOK: TButton
      Tag = 510
      Left = 589
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
      Left = 697
      Top = 6
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object cxGrid1: TcxGrid
    Left = 0
    Top = 0
    Width = 794
    Height = 222
    Align = alClient
    PopupMenu = PopupMenu1Grid
    TabOrder = 1
    LookAndFeel.Kind = lfStandard
    object cxGrid1TableView1: TcxGridTableView
      PopupMenu = PopupMenu1Grid
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
        Options.Filtering = False
        Options.FilteringMRUItemsList = False
        Width = 186
      end
      object cxGrid1TableView1Column2: TcxGridColumn
        Caption = 'Description'
        Options.Filtering = False
        Options.FilteringMRUItemsList = False
        Width = 120
      end
      object cxGrid1TableView1Column3: TcxGridColumn
        Caption = 'Default Value'
        Options.Filtering = False
        Options.FilteringMRUItemsList = False
        Width = 84
      end
      object cxGrid1TableView1Column4: TcxGridColumn
        Caption = 'Min. Value'
        Options.Filtering = False
        Options.FilteringMRUItemsList = False
        Width = 66
      end
      object cxGrid1TableView1Column5: TcxGridColumn
        Caption = 'Max. Value'
        Width = 60
      end
      object cxGrid1TableView1Column6: TcxGridColumn
        Caption = 'Edit Type'
        RepositoryItem = cxEditRepository1RadioGroupItem1
        Width = 82
      end
      object cxGrid1TableView1Column7: TcxGridColumn
        Caption = 'Dropdown List'
        Width = 179
      end
    end
    object cxGrid1Level1: TcxGridLevel
      GridView = cxGrid1TableView1
    end
  end
  object PopupMenu1Grid: TPopupMenu
    Left = 72
    Top = 49
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
  end
  object cxEditRepository1: TcxEditRepository
    Left = 32
    Top = 56
    object cxEditRepository1ComboBoxItem1: TcxEditRepositoryComboBoxItem
      Properties.Items.Strings = (
        'Default'
        'File Name'
        'Directory Name')
    end
    object cxEditRepository1RadioGroupItem1: TcxEditRepositoryRadioGroupItem
      Properties.Items = <
        item
          Caption = 'Default'
          Value = '0'
        end
        item
          Caption = 'Is path name'
          Value = '1'
        end
        item
          Caption = 'Is file name'
          Value = '2'
        end>
    end
  end
end
