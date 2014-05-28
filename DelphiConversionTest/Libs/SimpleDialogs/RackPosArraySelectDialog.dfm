object frmRackPosArraySelectDialog: TfrmRackPosArraySelectDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Layout Element Selection Dialog'
  ClientHeight = 440
  ClientWidth = 725
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 725
    Height = 440
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 0
      Top = 320
      Width = 725
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitTop = 328
      ExplicitWidth = 752
    end
    object cxGrid1: TcxGrid
      Left = 0
      Top = 323
      Width = 725
      Height = 117
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = cxcbsNone
      TabOrder = 0
      LookAndFeel.Kind = lfStandard
      ExplicitTop = 404
      ExplicitHeight = 36
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
      end
      object cxGrid1Level1: TcxGridLevel
        GridView = cxGrid1TableView1
      end
    end
    object PanelTop: TPanel
      Left = 0
      Top = 0
      Width = 725
      Height = 320
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object pnlSourceLayout: TPanel
        Left = 0
        Top = 0
        Width = 725
        Height = 184
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 0
        ExplicitHeight = 265
      end
      object Panel3: TPanel
        Left = 0
        Top = 184
        Width = 725
        Height = 136
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object lblTitle: TLabel
          Left = 8
          Top = 8
          Width = 119
          Height = 15
          Caption = 'Select Layout Element:'
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object btnClear: TButton
          Left = 631
          Top = 85
          Width = 88
          Height = 25
          Caption = 'Clear List'
          TabOrder = 0
          OnClick = btnClearClick
        end
        object btnAddPositions: TButton
          Left = 631
          Top = 54
          Width = 88
          Height = 25
          Caption = 'Add Positions'
          TabOrder = 1
          OnClick = btnAddPositionsClick
        end
        object rbInteger: TRadioButton
          Left = 243
          Top = 6
          Width = 116
          Height = 17
          Caption = 'columnwise'
          TabOrder = 2
          OnClick = rbIntegerClick
        end
        object rbMatrix: TRadioButton
          Left = 143
          Top = 6
          Width = 98
          Height = 17
          Caption = 'square'
          TabOrder = 3
          OnClick = rbMatrixClick
        end
        object GroupBox1: TGroupBox
          Left = 6
          Top = 29
          Width = 203
          Height = 105
          Caption = 'Source Position'
          TabOrder = 4
          object lblPositionSeparator: TLabel
            Left = 120
            Top = 52
            Width = 3
            Height = 15
            Caption = ':'
          end
          object lblSourcePosition: TLabel
            Left = 8
            Top = 52
            Width = 43
            Height = 15
            Caption = 'Position'
          end
          object lblSourceRack: TLabel
            Left = 8
            Top = 24
            Width = 25
            Height = 15
            Caption = 'Rack'
          end
          object lblSourceNoOfPos: TLabel
            Left = 8
            Top = 80
            Width = 83
            Height = 15
            Caption = 'Number of pos.'
          end
          object edSourceRackLastPos: TEdit
            Left = 137
            Top = 49
            Width = 56
            Height = 23
            Alignment = taRightJustify
            ReadOnly = True
            TabOrder = 0
          end
          object edSourceRackFirstPos: TEdit
            Left = 60
            Top = 49
            Width = 48
            Height = 23
            Alignment = taRightJustify
            ReadOnly = True
            TabOrder = 1
          end
          object edSourceRackName: TEdit
            Left = 60
            Top = 21
            Width = 133
            Height = 23
            TabOrder = 2
          end
          object edSourceNoOfPos: TEdit
            Left = 137
            Top = 77
            Width = 56
            Height = 23
            Alignment = taRightJustify
            ReadOnly = True
            TabOrder = 3
          end
          object edSourcePositionArray: TEdit
            Left = 97
            Top = 57
            Width = 48
            Height = 23
            Alignment = taRightJustify
            ReadOnly = True
            TabOrder = 4
          end
        end
        object GroupBox2: TGroupBox
          Left = 423
          Top = 29
          Width = 202
          Height = 105
          Caption = 'Additional'
          TabOrder = 5
          object lblLiqParam: TLabel
            Left = 8
            Top = 52
            Width = 22
            Height = 15
            Caption = 'LHP'
          end
          object lblVolume: TLabel
            Left = 8
            Top = 24
            Width = 41
            Height = 15
            Caption = 'Volume'
          end
          object lblSubstance: TLabel
            Left = 8
            Top = 80
            Width = 29
            Height = 15
            Caption = 'Subst'
          end
          object edVolume: TEdit
            Left = 55
            Top = 21
            Width = 138
            Height = 23
            TabOrder = 0
          end
          object cmbLiqParam: TComboBox
            Left = 55
            Top = 49
            Width = 138
            Height = 23
            TabOrder = 1
          end
          object cmbSubstance: TComboBox
            Left = 55
            Top = 77
            Width = 138
            Height = 23
            TabOrder = 2
          end
        end
        object GroupBox3: TGroupBox
          Left = 215
          Top = 29
          Width = 202
          Height = 105
          Caption = 'Destination Position'
          TabOrder = 6
          object Label1: TLabel
            Left = 120
            Top = 52
            Width = 3
            Height = 15
            Caption = ':'
          end
          object Label2: TLabel
            Left = 8
            Top = 52
            Width = 43
            Height = 15
            Caption = 'Position'
          end
          object Label3: TLabel
            Left = 8
            Top = 24
            Width = 25
            Height = 15
            Caption = 'Rack'
          end
          object Label4: TLabel
            Left = 8
            Top = 80
            Width = 83
            Height = 15
            Caption = 'Number of pos.'
          end
          object edRackLastPos: TEdit
            Left = 137
            Top = 49
            Width = 56
            Height = 23
            Alignment = taRightJustify
            ReadOnly = True
            TabOrder = 0
          end
          object edRackFirstPos: TEdit
            Left = 60
            Top = 49
            Width = 48
            Height = 23
            Alignment = taRightJustify
            ReadOnly = True
            TabOrder = 1
          end
          object edRackName: TEdit
            Left = 60
            Top = 21
            Width = 133
            Height = 23
            TabOrder = 2
          end
          object edNoOfPos: TEdit
            Left = 137
            Top = 77
            Width = 56
            Height = 23
            Alignment = taRightJustify
            ReadOnly = True
            TabOrder = 3
          end
          object edPositionArray: TEdit
            Left = 97
            Top = 57
            Width = 48
            Height = 23
            Alignment = taRightJustify
            ReadOnly = True
            TabOrder = 4
          end
        end
        object rbArray: TRadioButton
          Left = 343
          Top = 6
          Width = 116
          Height = 17
          Caption = 'cherry pick'
          Checked = True
          TabOrder = 7
          TabStop = True
          OnClick = rbArrayClick
        end
      end
      object pnlLayout: TPanel
        Left = 0
        Top = 0
        Width = 725
        Height = 184
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        ExplicitLeft = 88
        ExplicitWidth = 637
        ExplicitHeight = 265
      end
    end
  end
  object cxEditRepository1: TcxEditRepository
    Left = 656
    Top = 344
  end
end
