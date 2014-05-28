object frmTubeEditor: TfrmTubeEditor
  Left = 35
  Top = 95
  BorderIcons = [biSystemMenu]
  Caption = 'EDIT TUBES'
  ClientHeight = 545
  ClientWidth = 760
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 15
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 760
    Height = 545
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 0
      Top = 0
      Height = 545
      ExplicitLeft = 113
      ExplicitHeight = 552
    end
    object Panel1: TPanel
      Left = 3
      Top = 0
      Width = 757
      Height = 545
      Align = alClient
      TabOrder = 0
      object cxGrid1: TcxGrid
        Left = 1
        Top = 1
        Width = 755
        Height = 543
        Align = alClient
        PopupMenu = PopupMenu1
        TabOrder = 0
        LookAndFeel.Kind = lfStandard
        object cxGrid1TableView1: TcxGridTableView
          OnDragDrop = cxGrid1TableView1DragDrop
          OnDragOver = cxGrid1TableView1DragOver
          NavigatorButtons.ConfirmDelete = False
          OnEditValueChanged = cxGrid1TableView1EditValueChanged
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
          object cxGrid1TableView1Column7: TcxGridColumn
            Caption = 'Rack ID'
            Width = 96
          end
          object cxGrid1TableView1ColumnPos: TcxGridColumn
            Caption = 'Pos.'
            Width = 33
          end
          object cxGrid1TableView1Column1: TcxGridColumn
            Caption = 'Substance ID'
            Options.Filtering = False
            Options.FilteringMRUItemsList = False
            Width = 129
          end
          object cxGrid1TableView1Column2: TcxGridColumn
            Caption = 'Volume [uL]'
            Options.Filtering = False
            Options.FilteringMRUItemsList = False
            Width = 100
          end
          object cxGrid1TableView1Column4: TcxGridColumn
            Caption = 'Min. Volume 1'
            Width = 100
          end
          object cxGrid1TableView1Column3: TcxGridColumn
            Caption = 'Max. Volume'
            Width = 100
          end
          object cxGrid1TableView1Column5: TcxGridColumn
            Caption = 'Min. Volume 2'
            Width = 90
          end
        end
        object cxGrid1Level1: TcxGridLevel
          GridView = cxGrid1TableView1
        end
      end
    end
  end
  object cxEditRepository1: TcxEditRepository
    Left = 336
    Top = 152
    object cxEditRepository1SpinItem1: TcxEditRepositorySpinItem
    end
    object cxEditRepository1ComboBoxItem1: TcxEditRepositoryComboBoxItem
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 364
    Top = 217
    object AddRow1: TMenuItem
      Caption = 'Append Line'
      OnClick = AddRow1Click
    end
    object Appendlinesbyselectingpositions1: TMenuItem
      Caption = 'Append lines by selecting positions'
      OnClick = Appendlinesbyselectingpositions1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmnuDelete: TMenuItem
      Caption = 'Delete Selected Reagents'
      OnClick = pmnuDeleteClick
    end
  end
  object ColorDialog1: TColorDialog
    Left = 44
    Top = 27
  end
end
