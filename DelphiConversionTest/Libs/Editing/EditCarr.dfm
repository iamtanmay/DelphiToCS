object frmEdCarr: TfrmEdCarr
  Left = 232
  Top = 50
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Carrier Type Definition'
  ClientHeight = 422
  ClientWidth = 762
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFF
    FFFFFFFFFAAAAAAFFFFFFFFFFFFFFFFFFFFFFFAAAFFFFFFAAAFFFFFFFFFFFFFF
    FFFFAAFFFFFFFFFFFFAAFFFFFFFFFFFFFFFAFFFFFFAAAAFFFFFFAFFFFFFFFFFF
    FAAFFFFAAAFFFFAAAFFFFAAFFFFFFFFFAFFFFAAFFFFFFFFFFAFFFFFAFFFFFFFF
    AFFAAFFFFFFFFFFFFFFAAFFAFFFFFFFAFFAFFFFF00000000FFFFFAFFAFFFFFAF
    FAFFFFAA0FFFFFF0FAFFFFFFFAFFFAFFFFFFFFFF0FFFFFF0FFAFFFAFFFAFFAFF
    AFFFFAFF0FFFFFF0FFFFFFFAFFAFFAFFAFFFFFFF0FFFFFF0FFFAFFFAFFAFAFFF
    AFFFAFFF0FFFFFF0FFFFFFFAFFFAAFFAFFFFFFFF0FFFFFF0AFFFAFFFAFFAAFFA
    FFFAFFAF0FFFFFF0FFFFAFFFAFFAAFFAFFFAFFAF0FFFFFF0FAFFAFFFAFFAAFFA
    FFFAFFAF0FFFFFF0FAFFAFFFAFFAAFFAFFFAFFFA0FFFFFF0FFFFAFFFAFFAAFFA
    FFFFAFFF0FFFFFF0FFFFAFFFAFFAAFFFAFFFAFFF0FFFFFF0FFFFFFFAFFFAFAFF
    AFFFFAFF00FFFF00FFFAFFFAFFAFFAFFAFFFFFFFF000000FFFAFFFFAFFAFFAFF
    FFFFFFAAFFFFFFFFFAFFFFAFFFAFFFAFFAFFFFFA00000000AFFFFFFFFAFFFFFA
    FFAFFFFF00000000FFFFFAFFAFFFFFFFAFFAAFFFFFFFFFFFFFFAAFFAFFFFFFFF
    AFFFFAFFFFFFFFFFFFAFFFFAFFFFFFFFFAAFFFFAAAFFFFAAAFFFFAAFFFFFFFFF
    FFFAFFFFFFAAAAFFFFFFAFFFFFFFFFFFFFFFAAFFFFFFFFFFFFAAFFFFFFFFFFFF
    FFFFFFAAAFFFFFFAAAFFFFFFFFFFFFFFFFFFFFFFFAAAAAAFFFFFFFFFFFFF0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 288
    Top = 0
    Width = 474
    Height = 384
    ActivePage = TabSheet4
    Align = alRight
    TabOrder = 0
    object TabSheet1: TTabSheet
      Tag = 50001
      Caption = 'Basic'
      object GroupBox5: TGroupBox
        Tag = 50130
        Left = 15
        Top = 10
        Width = 188
        Height = 47
        Caption = 'Carrier Type'
        TabOrder = 0
        object Label27: TLabel
          Tag = 50140
          Left = 8
          Top = 21
          Width = 32
          Height = 15
          Caption = 'Name'
        end
        object edTypeName: TEdit
          Left = 56
          Top = 15
          Width = 121
          Height = 23
          ReadOnly = True
          TabOrder = 0
        end
      end
      object GroupBox4: TGroupBox
        Tag = 50160
        Left = 15
        Top = 64
        Width = 188
        Height = 92
        Caption = 'Carrier Dimensions [mm] '
        TabOrder = 1
        object Label24: TLabel
          Tag = 50170
          Left = 8
          Top = 24
          Width = 61
          Height = 15
          Caption = 'Length   (X)'
        end
        object Label25: TLabel
          Tag = 50180
          Left = 8
          Top = 48
          Width = 62
          Height = 15
          Caption = 'Width     (Y)'
        end
        object Label26: TLabel
          Tag = 50190
          Left = 8
          Top = 72
          Width = 63
          Height = 15
          Caption = 'Height    (Z)'
        end
        object edSizeX: TEdit
          Left = 124
          Top = 16
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edSizeY: TEdit
          Left = 124
          Top = 40
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edSizeZ: TEdit
          Left = 124
          Top = 64
          Width = 50
          Height = 23
          TabOrder = 2
        end
      end
      object GroupBox2: TGroupBox
        Tag = 50200
        Left = 15
        Top = 161
        Width = 188
        Height = 68
        Caption = 'Positions '
        TabOrder = 2
        object Label13: TLabel
          Tag = 50210
          Left = 8
          Top = 24
          Width = 28
          Height = 15
          Caption = 'Rows'
        end
        object Label15: TLabel
          Tag = 50220
          Left = 8
          Top = 48
          Width = 48
          Height = 15
          Caption = 'Columns'
        end
        object edRows: TEdit
          Left = 124
          Top = 16
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edCols: TEdit
          Left = 124
          Top = 40
          Width = 50
          Height = 23
          TabOrder = 1
        end
      end
      object SlotMeasures: TGroupBox
        Left = 15
        Top = 234
        Width = 188
        Height = 71
        Caption = 'Slot - Dimension [mm]'
        TabOrder = 3
        object Label29: TLabel
          Tag = 50170
          Left = 8
          Top = 24
          Width = 61
          Height = 15
          Caption = 'Length   (X)'
        end
        object Label30: TLabel
          Tag = 50180
          Left = 8
          Top = 48
          Width = 62
          Height = 15
          Caption = 'Width     (Y)'
        end
        object edSlotXmm: TEdit
          Left = 124
          Top = 16
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edSlotYmm: TEdit
          Left = 124
          Top = 40
          Width = 50
          Height = 23
          TabOrder = 1
        end
      end
      object SlotPos: TGroupBox
        Tag = 50320
        Left = 223
        Top = 10
        Width = 188
        Height = 121
        Caption = 'Slots [mm / rel. to carrier]'
        TabOrder = 4
        object Label18: TLabel
          Tag = 50330
          Left = 8
          Top = 20
          Width = 89
          Height = 15
          Caption = 'First Position  (X)'
        end
        object Label19: TLabel
          Tag = 50340
          Left = 8
          Top = 44
          Width = 89
          Height = 15
          Caption = 'First Position  (Y)'
        end
        object Label20: TLabel
          Tag = 50360
          Left = 8
          Top = 68
          Width = 85
          Height = 15
          Caption = 'Last Position (X)'
        end
        object Label21: TLabel
          Tag = 50370
          Left = 8
          Top = 90
          Width = 85
          Height = 15
          Caption = 'Last Position (Y)'
        end
        object edSlotXFirst: TEdit
          Left = 124
          Top = 16
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edSlotYFirst: TEdit
          Left = 124
          Top = 40
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edSlotXLast: TEdit
          Left = 124
          Top = 64
          Width = 50
          Height = 23
          TabOrder = 2
        end
        object edSlotYLast: TEdit
          Left = 124
          Top = 88
          Width = 50
          Height = 23
          TabOrder = 3
        end
      end
    end
    object TabSheet2: TTabSheet
      Tag = 50229
      Caption = 'Stacker'
      ImageIndex = 1
      object GroupBox1: TGroupBox
        Tag = 50300
        Left = 15
        Top = 9
        Width = 242
        Height = 48
        TabOrder = 0
        object Label33: TLabel
          Tag = 50230
          Left = 8
          Top = 19
          Width = 81
          Height = 15
          Caption = 'Levels (Stacker)'
        end
        object edFloors: TEdit
          Left = 156
          Top = 16
          Width = 50
          Height = 23
          TabOrder = 0
        end
      end
      object rdoStackerButtonAlign: TRadioGroup
        Tag = 50570
        Left = 15
        Top = 184
        Width = 242
        Height = 73
        Caption = 'Buttons (Stacker)'
        Columns = 2
        Items.Strings = (
          'Front'
          'Back'
          'Left'
          'Right'
          'Invisible')
        TabOrder = 2
      end
      object GroupBox3: TGroupBox
        Tag = 50320
        Left = 15
        Top = 64
        Width = 242
        Height = 113
        Caption = 'Slots [mm / rel. to carrier]'
        TabOrder = 1
        object lblFirstZPos: TLabel
          Tag = 50350
          Left = 8
          Top = 54
          Width = 89
          Height = 15
          Caption = 'First Position  (Z)'
        end
        object lblLastZPos: TLabel
          Tag = 50380
          Left = 8
          Top = 86
          Width = 85
          Height = 15
          Caption = 'Last Position (Z)'
        end
        object edSlotZFirst: TEdit
          Left = 156
          Top = 84
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edSlotZLast: TEdit
          Left = 156
          Top = 52
          Width = 50
          Height = 23
          TabOrder = 2
        end
        object chkCalcStackHeights: TCheckBox
          Tag = 50660
          Left = 8
          Top = 24
          Width = 228
          Height = 17
          Caption = 'Calc. height by rack stack heights'
          TabOrder = 0
          OnClick = chkCalcStackHeightsClick
        end
      end
    end
    object TabSheet3: TTabSheet
      Tag = 50620
      Caption = 'Movement'
      ImageIndex = 2
      object Label6: TLabel
        Tag = 50630
        Left = 3
        Top = 10
        Width = 209
        Height = 15
        Caption = 'Plate movement (and tube movement):'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object gbHandler: TGroupBox
        Tag = 50260
        Left = 3
        Top = 168
        Width = 218
        Height = 74
        Caption = 'Retake [mm / rel. to carrier] '
        TabOrder = 2
        object Label40: TLabel
          Left = 8
          Top = 24
          Width = 10
          Height = 15
          Caption = 'X:'
        end
        object Label32: TLabel
          Left = 8
          Top = 48
          Width = 10
          Height = 15
          Caption = 'Z:'
        end
        object Label8: TLabel
          Left = 108
          Top = 24
          Width = 10
          Height = 15
          Caption = 'Y:'
        end
        object Label10: TLabel
          Left = 108
          Top = 48
          Width = 42
          Height = 15
          Caption = 'R (abs.):'
        end
        object edHXRetake: TEdit
          Left = 52
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edHYRetake: TEdit
          Left = 156
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edHZRetake: TEdit
          Left = 52
          Top = 46
          Width = 50
          Height = 23
          TabOrder = 2
        end
        object edHRRetake: TEdit
          Left = 156
          Top = 45
          Width = 50
          Height = 23
          TabOrder = 3
        end
      end
      object GroupBox6: TGroupBox
        Tag = 50240
        Left = 3
        Top = 111
        Width = 218
        Height = 46
        Caption = 'Pre-Start Position [mm / rel. to start pos] '
        TabOrder = 1
        object Label2: TLabel
          Left = 8
          Top = 24
          Width = 10
          Height = 15
          Caption = 'X:'
        end
        object edHXPreStart: TEdit
          Left = 52
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 0
        end
      end
      object GroupBox7: TGroupBox
        Tag = 50250
        Left = 3
        Top = 31
        Width = 218
        Height = 74
        Caption = 'Start Position [mm / rel. to carrier] '
        TabOrder = 0
        object Label38: TLabel
          Left = 108
          Top = 24
          Width = 10
          Height = 15
          Caption = 'Y:'
        end
        object Label39: TLabel
          Left = 8
          Top = 48
          Width = 10
          Height = 15
          Caption = 'Z:'
        end
        object Label37: TLabel
          Left = 8
          Top = 24
          Width = 10
          Height = 15
          Caption = 'X:'
        end
        object Label7: TLabel
          Left = 108
          Top = 48
          Width = 42
          Height = 15
          Caption = 'R (abs.):'
        end
        object edHXStart: TEdit
          Left = 52
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edHYStart: TEdit
          Left = 156
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edHZStart: TEdit
          Left = 52
          Top = 46
          Width = 50
          Height = 23
          TabOrder = 2
        end
        object edHRStart: TEdit
          Left = 156
          Top = 45
          Width = 50
          Height = 23
          TabOrder = 3
        end
      end
      object GroupBox8: TGroupBox
        Tag = 50270
        Left = 3
        Top = 248
        Width = 218
        Height = 46
        Caption = 'Put Plate Offset'
        TabOrder = 3
        object Label1: TLabel
          Left = 8
          Top = 24
          Width = 10
          Height = 15
          Caption = 'Z:'
        end
        object edHZPut: TEdit
          Left = 52
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 0
        end
      end
      object chkHTubeUseValues: TCheckBox
        Tag = 50640
        Left = 232
        Top = 10
        Width = 241
        Height = 17
        Caption = 'Use different values for tube movement:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsUnderline]
        ParentFont = False
        TabOrder = 4
      end
      object GroupBox9: TGroupBox
        Tag = 50250
        Left = 232
        Top = 33
        Width = 231
        Height = 122
        Caption = 'Start Position [mm / rel. to carrier] '
        TabOrder = 5
        object Label4: TLabel
          Left = 8
          Top = 48
          Width = 10
          Height = 15
          Caption = 'Z:'
        end
        object Label5: TLabel
          Left = 8
          Top = 24
          Width = 10
          Height = 15
          Caption = 'X:'
        end
        object Label3: TLabel
          Left = 108
          Top = 24
          Width = 10
          Height = 15
          Caption = 'Y:'
        end
        object Label9: TLabel
          Left = 108
          Top = 48
          Width = 42
          Height = 15
          Caption = 'R (abs.):'
        end
        object edHTubeXStart: TEdit
          Left = 52
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edHTubeYStart: TEdit
          Left = 156
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edHTubeZStart: TEdit
          Left = 52
          Top = 46
          Width = 50
          Height = 23
          TabOrder = 2
        end
        object chkHTubeYStartBeforeX: TCheckBox
          Tag = 50650
          Left = 8
          Top = 73
          Width = 201
          Height = 17
          Caption = 'Move to Y-Start before X-Start '
          TabOrder = 3
        end
        object edHTubeRStart: TEdit
          Left = 156
          Top = 45
          Width = 50
          Height = 23
          TabOrder = 4
        end
        object chkHTubeKeepRotation: TCheckBox
          Tag = 50629
          Left = 8
          Top = 96
          Width = 220
          Height = 17
          Caption = 'Keep rotation value for XY movement'
          TabOrder = 5
        end
      end
    end
    object TabSheet5: TTabSheet
      Tag = 50600
      Caption = 'Other'
      ImageIndex = 4
      object rdoCarrierType: TRadioGroup
        Tag = 50610
        Left = 15
        Top = 16
        Width = 242
        Height = 73
        Caption = 'Carrier Type'
        Items.Strings = (
          'Default'
          'Barrier for XY-movement'
          'Corridor (after aspirate sample)')
        TabOrder = 0
      end
      object rdoSlotRotation: TRadioGroup
        Tag = 50280
        Left = 15
        Top = 108
        Width = 242
        Height = 37
        Caption = 'Default rack direction on slot'
        Columns = 4
        Items.Strings = (
          '0'#176
          '90'#176
          '180'#176
          '270'#176)
        TabOrder = 1
      end
      object rdoDirectionTurnType: TRadioGroup
        Tag = 50290
        Left = 16
        Top = 152
        Width = 241
        Height = 73
        Caption = 'Other possible directions'
        Items.Strings = (
          'Never turn plate'
          'Always drop plate turned (180'#176')'
          'Both possible (normal and turned drop) (*)')
        TabOrder = 2
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Special Forms'
      ImageIndex = 4
      object GroupBox10: TGroupBox
        Left = 16
        Top = 16
        Width = 225
        Height = 105
        Caption = 'Discrete Positions'
        TabOrder = 0
      end
      object cbDiscretePositions: TCheckBox
        Left = 35
        Top = 48
        Width = 185
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Carrier with discrete positions'
        TabOrder = 1
        OnClick = cbDiscretePositionsClick
      end
      object btnConvertCarrierDisPos: TButton
        Left = 35
        Top = 71
        Width = 189
        Height = 28
        Caption = 'Convert carrier in discrete pos'
        TabOrder = 2
        OnClick = btnConvertCarrierDisPosClick
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Edit Positions'
      ImageIndex = 5
      object cxGrid1: TcxGrid
        Left = 0
        Top = 0
        Width = 466
        Height = 354
        Align = alClient
        TabOrder = 0
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
            Caption = 'Position'
            Options.Filtering = False
            Options.FilteringMRUItemsList = False
            Width = 114
          end
          object cxGrid1TableView1Column2: TcxGridColumn
            Caption = 'Pos_X_mm'
            Options.Filtering = False
            Options.FilteringMRUItemsList = False
            Width = 120
          end
          object cxGrid1TableView1Column3: TcxGridColumn
            Caption = 'Pos_Y_mm'
            Options.Filtering = False
            Options.FilteringMRUItemsList = False
            Width = 118
          end
          object cxGrid1TableView1Column4: TcxGridColumn
            Caption = 'Pos_Z_mm'
            Width = 120
          end
        end
        object cxGrid1Level1: TcxGridLevel
          GridView = cxGrid1TableView1
        end
      end
    end
  end
  object Panel2x: TPanel
    Left = 0
    Top = 0
    Width = 288
    Height = 384
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clBtnShadow
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 384
    Width = 762
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnApply: TButton
      Tag = 590
      Left = 652
      Top = 6
      Width = 81
      Height = 25
      Caption = 'Apply'
      TabOrder = 0
      OnClick = btnApplyClick
    end
    object btnOK: TButton
      Tag = 510
      Left = 472
      Top = 6
      Width = 81
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Tag = 520
      Left = 562
      Top = 6
      Width = 81
      Height = 25
      Caption = 'Cancel'
      TabOrder = 2
      OnClick = btnCancelClick
    end
  end
end
