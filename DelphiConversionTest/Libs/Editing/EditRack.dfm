object frmEdRack: TfrmEdRack
  Left = 644
  Top = 127
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Rack Type Definition'
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
  PrintScale = poPrintToFit
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Panel3: TPanel
    Left = 0
    Top = 384
    Width = 762
    Height = 38
    Align = alBottom
    Anchors = [akLeft, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object btnApply: TButton
      Tag = 590
      Left = 649
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
  object Panel2x: TPanel
    Left = 0
    Top = 0
    Width = 232
    Height = 384
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clBtnShadow
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 232
    Top = 0
    Width = 530
    Height = 384
    ActivePage = TabSheet3
    Align = alRight
    TabOrder = 2
    object TabSheet1: TTabSheet
      Tag = 51490
      Caption = 'Basic'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object RackTyp: TGroupBox
        Tag = 51090
        Left = 3
        Top = 9
        Width = 168
        Height = 66
        Caption = 'Rack Type'
        TabOrder = 0
        object Label11: TLabel
          Tag = 50140
          Left = 8
          Top = 20
          Width = 32
          Height = 15
          Caption = 'Name'
        end
        object lblTypeNo: TLabel
          Tag = 50150
          Left = 8
          Top = 45
          Width = 26
          Height = 15
          Caption = 'Type'
        end
        object edTypeName: TEdit
          Left = 48
          Top = 16
          Width = 113
          Height = 23
          MaxLength = 20
          ReadOnly = True
          TabOrder = 0
        end
        object edTypeNo: TEdit
          Left = 48
          Top = 40
          Width = 113
          Height = 23
          ParentCustomHint = False
          TabOrder = 1
        end
      end
      object gbAbmessungen: TGroupBox
        Tag = 51100
        Left = 3
        Top = 82
        Width = 168
        Height = 90
        Caption = 'Rack Dimensions [mm] '
        TabOrder = 1
        object Label1: TLabel
          Tag = 50170
          Left = 8
          Top = 24
          Width = 61
          Height = 15
          Caption = 'Length   (X)'
        end
        object Label2: TLabel
          Tag = 50180
          Left = 8
          Top = 48
          Width = 62
          Height = 15
          Caption = 'Width     (Y)'
        end
        object lblSizeZ: TLabel
          Tag = 50190
          Left = 8
          Top = 72
          Width = 63
          Height = 15
          Caption = 'Height    (Z)'
        end
        object edSizeX: TEdit
          Left = 108
          Top = 16
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edSizeY: TEdit
          Left = 108
          Top = 40
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edSizeZ: TEdit
          Left = 108
          Top = 64
          Width = 50
          Height = 23
          TabOrder = 2
        end
      end
      object GroupBox1: TGroupBox
        Tag = 50200
        Left = 185
        Top = 7
        Width = 140
        Height = 69
        Caption = 'Positions '
        TabOrder = 3
        object Label4: TLabel
          Tag = 50210
          Left = 8
          Top = 20
          Width = 28
          Height = 15
          Caption = 'Rows'
        end
        object Label5: TLabel
          Tag = 50220
          Left = 8
          Top = 44
          Width = 48
          Height = 15
          Caption = 'Columns'
        end
        object edRows: TEdit
          Left = 76
          Top = 18
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edCols: TEdit
          Left = 76
          Top = 43
          Width = 50
          Height = 23
          TabOrder = 1
        end
      end
      object gbZHeights: TGroupBox
        Tag = 51110
        Left = 3
        Top = 178
        Width = 168
        Height = 120
        Caption = 'Heights (Z)  [mm / rel. to rack]'
        TabOrder = 2
        object ZTravelMM: TLabel
          Tag = 51120
          Left = 8
          Top = 20
          Width = 35
          Height = 15
          Caption = 'Travel '
        end
        object ZScanMM: TLabel
          Tag = 51130
          Left = 8
          Top = 44
          Width = 25
          Height = 15
          Caption = 'Scan'
        end
        object ZDispMM: TLabel
          Tag = 51140
          Left = 8
          Top = 68
          Width = 47
          Height = 15
          Caption = 'Dispense'
        end
        object ZMaxMM: TLabel
          Tag = 51150
          Left = 8
          Top = 92
          Width = 54
          Height = 15
          Caption = 'Maximum'
        end
        object edZTravel: TEdit
          Left = 108
          Top = 16
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edZScan: TEdit
          Left = 108
          Top = 40
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edZDisp: TEdit
          Left = 108
          Top = 64
          Width = 50
          Height = 23
          TabOrder = 2
        end
        object edZMax: TEdit
          Left = 108
          Top = 88
          Width = 50
          Height = 23
          TabOrder = 3
        end
      end
      object rdoTubeType: TRadioGroup
        Tag = 51160
        Left = 185
        Top = 135
        Width = 140
        Height = 50
        Caption = 'Tube - Type'
        Items.Strings = (
          'SingleTip Tube'
          'Multi  Tip Tube')
        TabOrder = 5
      end
      object TubeGeoGroup: TRadioGroup
        Tag = 51170
        Left = 185
        Top = 79
        Width = 140
        Height = 50
        Caption = 'Tube - Geometry'
        ItemIndex = 0
        Items.Strings = (
          'Circle'
          'Rectangle')
        TabOrder = 4
        OnClick = TubeGeoGroupClick
      end
      object TubeMeasure: TGroupBox
        Tag = 51180
        Left = 338
        Top = 15
        Width = 181
        Height = 116
        Caption = 'Tube - Dimensions [mm]'
        TabOrder = 7
        object lblTubeX: TLabel
          Tag = 51190
          Left = 8
          Top = 20
          Width = 66
          Height = 15
          Caption = 'X - Diameter'
        end
        object lblTubeY: TLabel
          Tag = 51200
          Left = 8
          Top = 44
          Width = 66
          Height = 15
          Caption = 'Y - Diameter'
        end
        object lblTubeZ: TLabel
          Tag = 51210
          Left = 8
          Top = 92
          Width = 66
          Height = 15
          Caption = 'Height     (Z)'
        end
        object lblTubeYOut: TLabel
          Tag = 51060
          Left = 8
          Top = 68
          Width = 92
          Height = 15
          Caption = 'Outside Diameter'
        end
        object edTubeX: TEdit
          Left = 118
          Top = 18
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edTubeY: TEdit
          Left = 118
          Top = 42
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edTubeZ: TEdit
          Left = 118
          Top = 90
          Width = 50
          Height = 23
          TabOrder = 3
        end
        object edTubeYOut: TEdit
          Left = 118
          Top = 66
          Width = 50
          Height = 23
          TabOrder = 2
        end
      end
      object gbTubepos: TGroupBox
        Tag = 51220
        Left = 338
        Top = 137
        Width = 181
        Height = 117
        Caption = 'Tube - Positions [mm]'
        TabOrder = 8
        object PosXFirst: TLabel
          Tag = 50330
          Left = 8
          Top = 20
          Width = 89
          Height = 15
          Caption = 'First Position  (X)'
        end
        object Label10: TLabel
          Tag = 50340
          Left = 8
          Top = 44
          Width = 89
          Height = 15
          Caption = 'First Position  (Y)'
        end
        object Label7: TLabel
          Tag = 50360
          Left = 8
          Top = 68
          Width = 88
          Height = 15
          Caption = 'Last Position  (X)'
        end
        object Label9: TLabel
          Tag = 50370
          Left = 8
          Top = 92
          Width = 88
          Height = 15
          Caption = 'Last Position  (Y)'
        end
        object edPosXFirst: TEdit
          Left = 118
          Top = 16
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edPosYFirst: TEdit
          Left = 118
          Top = 40
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edPosXLast: TEdit
          Left = 118
          Top = 64
          Width = 50
          Height = 23
          TabOrder = 2
        end
        object edPosYLast: TEdit
          Left = 118
          Top = 88
          Width = 50
          Height = 23
          TabOrder = 3
        end
      end
      object GroupBox7: TGroupBox
        Tag = 51550
        Left = 185
        Top = 191
        Width = 140
        Height = 70
        Caption = 'Appearance'
        TabOrder = 6
        object Shape1: TShape
          Left = 6
          Top = 26
          Width = 33
          Height = 33
          Brush.Color = clScrollBar
        end
        object btnColor: TButton
          Tag = 51560
          Left = 46
          Top = 32
          Width = 89
          Height = 25
          Caption = 'Change Color'
          TabOrder = 0
          OnClick = btnColorClick
        end
      end
      object cbDoNotPaintTubes: TCheckBox
        Tag = 51840
        Left = 341
        Top = 260
        Width = 165
        Height = 17
        Caption = 'Do not paint tubes'
        TabOrder = 9
      end
    end
    object TabSheet2: TTabSheet
      Tag = 51495
      Caption = 'Rack Gripping'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object HandlerPara: TGroupBox
        Tag = 51270
        Left = 15
        Top = 15
        Width = 186
        Height = 122
        Caption = 'Handler  [mm / rel. to rack]'
        TabOrder = 0
        object HXTake: TLabel
          Tag = 51280
          Left = 12
          Top = 24
          Width = 43
          Height = 15
          Caption = 'X - Take'
        end
        object HYTake: TLabel
          Tag = 51290
          Left = 12
          Top = 48
          Width = 43
          Height = 15
          Caption = 'Y - Take'
        end
        object HZTake: TLabel
          Tag = 51300
          Left = 12
          Top = 72
          Width = 43
          Height = 15
          Caption = 'Z - Take'
        end
        object Label26: TLabel
          Tag = 51590
          Left = 12
          Top = 96
          Width = 43
          Height = 15
          Caption = 'R - Take'
        end
        object edHXTakeMM: TEdit
          Left = 124
          Top = 18
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edHYTakeMM: TEdit
          Left = 124
          Top = 42
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edHZTakeMM: TEdit
          Left = 124
          Top = 66
          Width = 50
          Height = 23
          TabOrder = 2
        end
        object edHRTake: TEdit
          Left = 124
          Top = 90
          Width = 50
          Height = 23
          TabOrder = 3
        end
      end
      object GroupBox2: TGroupBox
        Tag = 51310
        Left = 15
        Top = 144
        Width = 188
        Height = 65
        Caption = 'Handler Varispan [ Steps ]'
        TabOrder = 1
        object HVOpen: TLabel
          Tag = 51320
          Left = 12
          Top = 20
          Width = 57
          Height = 15
          Caption = 'Max. Open'
        end
        object HVClose: TLabel
          Tag = 51330
          Left = 12
          Top = 44
          Width = 57
          Height = 15
          Caption = 'Max. Close'
        end
        object edVOpen_mm: TEdit
          Left = 124
          Top = 12
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edVClose_mm: TEdit
          Left = 124
          Top = 36
          Width = 50
          Height = 23
          TabOrder = 1
        end
      end
      object GroupBox14: TGroupBox
        Tag = 51860
        Left = 15
        Top = 215
        Width = 188
        Height = 56
        Caption = 'Rack Movement'
        TabOrder = 2
        object Label33: TLabel
          Left = 12
          Top = 24
          Width = 99
          Height = 15
          Caption = 'Offset RackZTravel'
        end
        object edRackZTravelOffset: TEdit
          Left = 124
          Top = 16
          Width = 50
          Height = 23
          TabOrder = 0
        end
      end
    end
    object TabSheet3: TTabSheet
      Tag = 51500
      Caption = 'Special Forms'
      ImageIndex = 2
      object GroupBox3: TGroupBox
        Tag = 51230
        Left = 15
        Top = 15
        Width = 210
        Height = 74
        Caption = 'Offsets [mm]'
        TabOrder = 0
        object Label14: TLabel
          Tag = 51570
          Left = 8
          Top = 20
          Width = 104
          Height = 15
          Caption = 'X - Offset (2nd row)'
        end
        object Label18: TLabel
          Tag = 51580
          Left = 8
          Top = 48
          Width = 128
          Height = 15
          Caption = 'Y - Offset (2nd column):'
        end
        object edPosX_Offset_mm: TEdit
          Left = 152
          Top = 17
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edPosY_Offset_mm: TEdit
          Left = 152
          Top = 48
          Width = 50
          Height = 23
          TabOrder = 1
        end
      end
      object GroupBox9: TGroupBox
        Tag = 51240
        Left = 15
        Top = 96
        Width = 210
        Height = 81
        Caption = 'Offset zwischen 2 Reihen/Kolonnen'
        TabOrder = 1
        object Label22: TLabel
          Tag = 51250
          Left = 8
          Top = 28
          Width = 67
          Height = 15
          Caption = 'X: After Pos.:'
        end
        object Label19: TLabel
          Tag = 51260
          Left = 8
          Top = 52
          Width = 67
          Height = 15
          Caption = 'Y: After Pos.:'
        end
        object Label21: TLabel
          Left = 143
          Top = 52
          Width = 25
          Height = 15
          Caption = 'mm:'
        end
        object Label20: TLabel
          Left = 143
          Top = 28
          Width = 25
          Height = 15
          Caption = 'mm:'
        end
        object edAddOffsetX_AfterPos: TEdit
          Left = 104
          Top = 24
          Width = 29
          Height = 23
          TabOrder = 0
        end
        object edAddOffsetX: TEdit
          Left = 174
          Top = 25
          Width = 30
          Height = 23
          TabOrder = 1
        end
        object edAddOffsetY_AfterPos: TEdit
          Left = 104
          Top = 48
          Width = 29
          Height = 23
          TabOrder = 2
        end
        object edAddOffsetY: TEdit
          Left = 174
          Top = 49
          Width = 30
          Height = 23
          TabOrder = 3
        end
      end
      object gbSlantedRack: TGroupBox
        Tag = 51450
        Left = 15
        Top = 183
        Width = 188
        Height = 138
        Caption = 'Slanted Rack Height [mm]'
        TabOrder = 2
        object Label23: TLabel
          Tag = 51470
          Left = 12
          Top = 107
          Width = 96
          Height = 15
          Caption = 'Height (Last Pos.) '
        end
        object edZ_LastPos_mm: TEdit
          Left = 124
          Top = 104
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object rdoSlopeType: TRadioGroup
          Tag = 51460
          Left = 8
          Top = 24
          Width = 169
          Height = 74
          Items.Strings = (
            'No Slope...'
            'Slope in X...'
            'Slope in Y...')
          TabOrder = 0
        end
      end
      object GroupBox8: TGroupBox
        Left = 255
        Top = 15
        Width = 226
        Height = 124
        Caption = 'Well around a center'
        TabOrder = 3
        object Label3: TLabel
          Left = 16
          Top = 60
          Width = 78
          Height = 15
          Caption = 'Center X [mm]'
        end
        object Label12: TLabel
          Left = 16
          Top = 88
          Width = 81
          Height = 15
          Caption = 'Center Y [mm]:'
        end
        object edCenterX_mm: TEdit
          Left = 160
          Top = 57
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edCenterY_mm: TEdit
          Left = 160
          Top = 88
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object cbWellsAroundCenter: TCheckBox
          Left = 12
          Top = 20
          Width = 189
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Arrange wells around a center'
          TabOrder = 2
        end
      end
      object GroupBox13: TGroupBox
        Tag = 51850
        Left = 255
        Top = 145
        Width = 226
        Height = 88
        Caption = 'Discrete Positions'
        TabOrder = 4
        object cbDiscretePositions: TCheckBox
          Tag = 51855
          Left = 12
          Top = 20
          Width = 189
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Rack with discrete positions'
          TabOrder = 0
          OnClick = cbDiscretePositionsClick
        end
      end
      object btnConvertDiscretePos: TButton
        Tag = 51865
        Left = 267
        Top = 191
        Width = 190
        Height = 25
        Caption = 'Convert rack in discrete pos'
        TabOrder = 5
        OnClick = btnConvertRackInDiscretePosClick
      end
    end
    object TabSheet4: TTabSheet
      Tag = 51505
      Caption = 'Special Uses'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox4: TGroupBox
        Tag = 51390
        Left = 7
        Top = 15
        Width = 180
        Height = 105
        Caption = 'Move Offset [mm/rel. to posit.]'
        TabOrder = 0
        object Label6: TLabel
          Tag = 51400
          Left = 12
          Top = 22
          Width = 71
          Height = 15
          Caption = 'Move up to X'
        end
        object Label8: TLabel
          Tag = 51410
          Left = 12
          Top = 48
          Width = 71
          Height = 15
          Caption = 'Move up to Y'
        end
        object Label13: TLabel
          Tag = 51380
          Left = 12
          Top = 74
          Width = 71
          Height = 15
          Caption = 'Move up to Z'
        end
        object edMOffsetUpX: TEdit
          Left = 124
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edMOffsetUpY: TEdit
          Left = 124
          Top = 46
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edMOffsetZ: TEdit
          Left = 124
          Top = 71
          Width = 50
          Height = 23
          TabOrder = 2
        end
      end
      object GroupBox5: TGroupBox
        Tag = 51420
        Left = 7
        Top = 129
        Width = 180
        Height = 78
        Caption = 'Aspiration Shifting'
        TabOrder = 1
        object Label15: TLabel
          Tag = 51430
          Left = 7
          Top = 22
          Width = 68
          Height = 15
          Caption = 'Radius [mm]'
        end
        object Label16: TLabel
          Tag = 51440
          Left = 7
          Top = 48
          Width = 102
          Height = 15
          Caption = 'Zahl der Positionen'
        end
        object edShift_Radius_mm: TEdit
          Left = 124
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edShift_NoOfSteps: TEdit
          Left = 124
          Top = 46
          Width = 50
          Height = 23
          TabOrder = 1
        end
      end
      object GroupBox6: TGroupBox
        Tag = 51510
        Left = 7
        Top = 211
        Width = 180
        Height = 102
        Caption = 'Tube Caps'
        TabOrder = 2
        object Label28: TLabel
          Tag = 51520
          Left = 16
          Top = 29
          Width = 26
          Height = 15
          Caption = 'Type'
        end
        object Label29: TLabel
          Tag = 51540
          Left = 16
          Top = 79
          Width = 94
          Height = 15
          Caption = 'Grip Height [mm]'
          Visible = False
        end
        object Label30: TLabel
          Tag = 51530
          Left = 16
          Top = 50
          Width = 81
          Height = 15
          Caption = 'Diameter [mm]'
          Visible = False
        end
        object edCapType: TEdit
          Left = 72
          Top = 23
          Width = 100
          Height = 23
          MaxLength = 20
          TabOrder = 0
        end
        object edCapDiameter: TEdit
          Left = 116
          Top = 50
          Width = 50
          Height = 23
          TabOrder = 1
          Visible = False
        end
        object edCapGripHeight: TEdit
          Left = 116
          Top = 76
          Width = 50
          Height = 23
          TabOrder = 2
          Visible = False
        end
      end
      object GroupBox10: TGroupBox
        Tag = 51600
        Left = 208
        Top = 15
        Width = 210
        Height = 192
        Caption = 'Tube Handling [mm]'
        TabOrder = 3
        object Label17: TLabel
          Tag = 51610
          Left = 8
          Top = 22
          Width = 80
          Height = 15
          Caption = 'Tube Get Open'
        end
        object Label24: TLabel
          Tag = 51615
          Left = 8
          Top = 48
          Width = 80
          Height = 15
          Caption = 'Tube Get Close'
        end
        object Label25: TLabel
          Tag = 51620
          Left = 8
          Top = 74
          Width = 80
          Height = 15
          Caption = 'Tube Put Open'
        end
        object Label31: TLabel
          Tag = 51625
          Left = 8
          Top = 98
          Width = 155
          Height = 15
          Caption = 'Z-Offset: (Put tube/get tube) '
        end
        object Label32: TLabel
          Tag = 51635
          Left = 8
          Top = 124
          Width = 124
          Height = 15
          Caption = 'Z-Offset: Grip (Get/Put)'
        end
        object edTubeGetOpen_mm: TEdit
          Left = 148
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 0
        end
        object edTubeGetClose_mm: TEdit
          Left = 148
          Top = 46
          Width = 50
          Height = 23
          TabOrder = 1
        end
        object edTubePutOpen_mm: TEdit
          Left = 148
          Top = 71
          Width = 50
          Height = 23
          TabOrder = 2
        end
        object edTubePutOffset_mm: TEdit
          Left = 148
          Top = 95
          Width = 50
          Height = 23
          TabOrder = 3
        end
        object edTubeGripOffset_mm: TEdit
          Left = 148
          Top = 119
          Width = 50
          Height = 23
          TabOrder = 4
        end
      end
      object GroupBox11: TGroupBox
        Tag = 51630
        Left = 208
        Top = 211
        Width = 209
        Height = 50
        Caption = 'Blocks Movement'
        TabOrder = 4
        object cbBalanceDoor: TCheckBox
          Tag = 51640
          Left = 12
          Top = 22
          Width = 125
          Height = 17
          Alignment = taLeftJustify
          Caption = 'BalanceDoor'
          TabOrder = 0
        end
      end
      object GroupBox12: TGroupBox
        Tag = 51650
        Left = 208
        Top = 263
        Width = 209
        Height = 50
        Caption = 'Stacking'
        TabOrder = 5
        object Label27: TLabel
          Tag = 51660
          Left = 7
          Top = 22
          Width = 100
          Height = 15
          Caption = 'Stack Height [mm]'
        end
        object edStackHeight_mm: TEdit
          Left = 121
          Top = 20
          Width = 50
          Height = 23
          TabOrder = 0
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Edit Positions'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object cxGrid1: TcxGrid
        Left = 0
        Top = 0
        Width = 522
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
        end
        object cxGrid1Level1: TcxGridLevel
          GridView = cxGrid1TableView1
        end
      end
    end
  end
  object ColorDialog1: TColorDialog
    Left = 44
    Top = 27
  end
end
