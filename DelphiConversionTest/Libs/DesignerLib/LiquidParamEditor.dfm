object frmLiquidParEditor: TfrmLiquidParEditor
  Tag = 35730
  Left = 210
  Top = 338
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Liquid Handling Settings'
  ClientHeight = 504
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 660
    Height = 474
    ActivePage = tbsSampleAsp
    Align = alClient
    TabOrder = 0
    object tbsSampleAsp: TTabSheet
      Tag = 35110
      Caption = 'Aspirate Sample'
      object gbVolume: TGroupBox
        Tag = 35610
        Left = 120
        Top = 5
        Width = 107
        Height = 115
        Caption = ' Volume ['#181'l] '
        TabOrder = 0
        object EditSysAirAspVol: TDBEdit
          Left = 19
          Top = 23
          Width = 50
          Height = 23
          DataField = 'SysAirAspVol'
          DataSource = DataSource1
          TabOrder = 0
        end
        object EditTransAirVol: TDBEdit
          Left = 19
          Top = 79
          Width = 50
          Height = 23
          DataField = 'TransAirVol'
          DataSource = DataSource1
          TabOrder = 1
        end
      end
      object GroupBox1: TGroupBox
        Tag = 35630
        Left = 298
        Top = 5
        Width = 85
        Height = 115
        Caption = ' Speed ['#181'l/s] '
        TabOrder = 1
        object EditSampleAspSpeed: TDBEdit
          Left = 11
          Top = 51
          Width = 54
          Height = 23
          DataField = 'SampleAspSpeed'
          DataSource = DataSource1
          TabOrder = 1
        end
        object EditSysAirAspSpeed: TDBEdit
          Left = 11
          Top = 23
          Width = 54
          Height = 23
          DataField = 'SysAirAspSpeed'
          DataSource = DataSource1
          TabOrder = 0
        end
        object EditTransAirSpeed: TDBEdit
          Left = 11
          Top = 79
          Width = 54
          Height = 23
          DataField = 'TransAirSpeed'
          DataSource = DataSource1
          TabOrder = 2
        end
      end
      object gbDelay: TGroupBox
        Tag = 35640
        Left = 388
        Top = 5
        Width = 85
        Height = 115
        Caption = ' Delay [sec] '
        TabOrder = 2
        object EditSampleAspDelay: TDBEdit
          Left = 11
          Top = 51
          Width = 50
          Height = 23
          DataField = 'SampleAspDelay'
          DataSource = DataSource1
          TabOrder = 0
        end
        object edSysAirAspDelay: TDBEdit
          Left = 11
          Top = 23
          Width = 50
          Height = 23
          DataField = 'SysAirAspDelay'
          DataSource = DataSource1
          TabOrder = 1
        end
        object edTransAirAspDelay: TDBEdit
          Left = 11
          Top = 79
          Width = 50
          Height = 23
          DataField = 'TransAirAspDelay'
          DataSource = DataSource1
          TabOrder = 2
        end
      end
      object gBCalculate: TGroupBox
        Tag = 35620
        Left = 232
        Top = 5
        Width = 61
        Height = 115
        Caption = 'Calculate'
        TabOrder = 3
        object cbSysAirAspCalc: TDBCheckBox
          Left = 16
          Top = 25
          Width = 40
          Height = 17
          Caption = '-->'
          DataField = 'SysAirAspCalc'
          DataSource = DataSource1
          TabOrder = 0
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
        object cbTransAirAspCalc: TDBCheckBox
          Left = 16
          Top = 81
          Width = 40
          Height = 17
          Caption = '-->'
          DataField = 'TransAirAspCalc'
          DataSource = DataSource1
          TabOrder = 1
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
        object DBCheckBox3: TDBCheckBox
          Left = 16
          Top = 53
          Width = 40
          Height = 17
          Caption = '-->'
          DataField = 'SampleAspCalc'
          DataSource = DataSource1
          TabOrder = 2
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
      end
      object rgrAspSwitchPos: TDBRadioGroup
        Tag = 35650
        Left = 5
        Top = 260
        Width = 217
        Height = 81
        Caption = 'Switch Position'
        DataField = 'AspSwitchPos'
        DataSource = DataSource1
        Items.Strings = (
          'Switch outside tube'
          'Switch inside tube'
          'Don'#39't switch')
        ParentBackground = True
        TabOrder = 4
        Values.Strings = (
          '1'
          '2'
          '0')
        OnChange = rgrAspSwitchPosChange
      end
      object dbTransAir: TDBRadioGroup
        Tag = 35690
        Left = 334
        Top = 344
        Width = 139
        Height = 89
        Caption = 'Transport Air Suck Position'
        DataField = 'TransAirPos'
        DataSource = DataSource1
        Items.Strings = (
          'Z Travel'
          'Z Scan'
          'Z Dispense')
        ParentBackground = True
        TabOrder = 5
        Values.Strings = (
          '0'
          '1'
          '2')
      end
      object GroupBox2: TGroupBox
        Left = 5
        Top = 5
        Width = 110
        Height = 115
        TabOrder = 6
        object Label24: TLabel
          Tag = 35570
          Left = 4
          Top = 25
          Width = 56
          Height = 15
          Caption = 'System Air'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label26: TLabel
          Tag = 35740
          Left = 4
          Top = 52
          Width = 39
          Height = 15
          Caption = 'Sample'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label27: TLabel
          Tag = 35600
          Left = 4
          Top = 80
          Width = 69
          Height = 15
          Caption = 'Transport Air'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
      end
      object GroupBox19: TGroupBox
        Tag = 35670
        Left = 227
        Top = 260
        Width = 246
        Height = 81
        Caption = 'Movement during aspiration'
        TabOrder = 7
        object pnSampleAspRetractPos: TPanel
          Left = 160
          Top = 52
          Width = 73
          Height = 23
          BevelOuter = bvNone
          TabOrder = 3
          object Label16: TLabel
            Left = 57
            Top = 4
            Width = 22
            Height = 15
            Caption = 'mm'
          end
          object EditSampleAspRetractPos: TDBEdit
            Left = 2
            Top = 1
            Width = 49
            Height = 23
            DataField = 'SampleAspRetractPos'
            DataSource = DataSource1
            TabOrder = 0
          end
        end
        object rbNoTracking: TRadioButton
          Tag = 35410
          Left = 8
          Top = 16
          Width = 129
          Height = 17
          Caption = 'rbNoTracking'
          TabOrder = 0
          OnMouseUp = boxSampleAspLevTrMouseUp
        end
        object rbLevelTracking: TRadioButton
          Tag = 35360
          Left = 8
          Top = 35
          Width = 129
          Height = 17
          Caption = 'Level Tracking'
          TabOrder = 1
          OnMouseUp = boxSampleAspLevTrMouseUp
        end
        object rbRetractFromZMax: TRadioButton
          Tag = 35430
          Left = 8
          Top = 54
          Width = 153
          Height = 17
          Caption = 'Retract from Z Max'
          TabOrder = 2
          OnMouseUp = boxSampleAspLevTrMouseUp
        end
      end
      object GroupBox11: TGroupBox
        Tag = 35150
        Left = 5
        Top = 123
        Width = 468
        Height = 134
        Caption = 'Movement into well'
        TabOrder = 8
        object Label5: TLabel
          Tag = 35420
          Left = 8
          Top = 46
          Width = 84
          Height = 15
          Caption = 'Submerge[mm]'
        end
        object lblSampleAspScanSpeed: TLabel
          Tag = 35885
          Left = 8
          Top = 20
          Width = 63
          Height = 15
          Caption = 'Scan Speed:'
        end
        object EditSampleAspSubmerge: TDBEdit
          Left = 100
          Top = 44
          Width = 50
          Height = 23
          DataField = 'SampleAspSubmerge'
          DataSource = DataSource1
          TabOrder = 1
        end
        object boxSampleAspLiqDet: TCheckBox
          Tag = 35350
          Left = 178
          Top = 12
          Width = 115
          Height = 17
          Caption = 'Liquid Detection'
          TabOrder = 4
          OnClick = boxSampleAspLiqDetClick
          OnMouseUp = boxSampleAspLiqDetMouseUp
        end
        object edSampleAspLiqDet: TDBEdit
          Left = 318
          Top = 1
          Width = 24
          Height = 23
          DataField = 'SampleAspLiqDet'
          DataSource = DataSource1
          TabOrder = 5
          Visible = False
          OnChange = edSampleAspLiqDetChange
        end
        object edSampleAspScanSpeed: TDBEdit
          Left = 100
          Top = 18
          Width = 50
          Height = 23
          DataField = 'SampleAspScanSpeed'
          DataSource = DataSource1
          TabOrder = 0
        end
        object EditSampleAspErrFlag: TDBCheckBox
          Tag = 35250
          Left = 8
          Top = 70
          Width = 168
          Height = 17
          Caption = 'Disable Z Error at Aspirate'
          DataField = 'SampleAspErrFlag'
          DataSource = DataSource1
          TabOrder = 2
          ValueChecked = '1'
          ValueUnchecked = '0'
        end
        object cbSampleAspNoCalc: TCheckBox
          Tag = 35930
          Left = 8
          Top = 90
          Width = 168
          Height = 17
          Caption = 'Do not use level calculation'
          TabOrder = 3
          OnMouseUp = boxSampleAspLiqDetMouseUp
        end
        object pnSampleAspLiqDet: TPanel
          Left = 174
          Top = 27
          Width = 289
          Height = 84
          BevelOuter = bvNone
          TabOrder = 6
          object grbSampleAspScanMode: TDBRadioGroup
            Tag = 35870
            Left = 4
            Top = 10
            Width = 129
            Height = 31
            Caption = 'Scan Mode (Sensitivity)'
            Columns = 4
            DataField = 'SampleAspScanMode'
            DataSource = DataSource1
            Items.Strings = (
              '1'
              '2'
              '3'
              '4')
            ParentBackground = True
            TabOrder = 0
            Values.Strings = (
              '0'
              '1'
              '2'
              '3')
          end
          object DBCheckBox9: TDBCheckBox
            Tag = 35710
            Left = 4
            Top = 62
            Width = 233
            Height = 17
            Caption = 'Store Volume in Database'
            DataField = 'RecordDetectionVolume'
            DataSource = DataSource1
            TabOrder = 1
            ValueChecked = 'Wahr'
            ValueUnchecked = 'Falsch'
          end
          object grbSampleAspErrType: TGroupBox
            Tag = 35370
            Left = 140
            Top = 3
            Width = 145
            Height = 55
            Caption = 'in case of error'
            TabOrder = 2
            object boxSampleAspGotoZ: TRadioButton
              Tag = 35380
              Left = 8
              Top = 16
              Width = 130
              Height = 17
              Caption = 'boxSampleAspGotoZ'
              Checked = True
              TabOrder = 0
              TabStop = True
              OnMouseUp = boxSampleAspGotoZMouseUp
            end
            object boxSampleAspDispErr: TRadioButton
              Tag = 35390
              Left = 8
              Top = 34
              Width = 130
              Height = 17
              Caption = 'Display Error'
              TabOrder = 1
              OnMouseUp = boxSampleAspDispErrMouseUp
            end
          end
        end
        object cbSampleAspUseDetectedLevel: TCheckBox
          Tag = 36120
          Left = 8
          Top = 110
          Width = 168
          Height = 17
          Caption = 'Use detected level'
          TabOrder = 7
          OnMouseUp = boxSampleAspLiqDetMouseUp
        end
        object rgSampleAspInsertMoveType: TDBRadioGroup
          Tag = 36110
          Left = 180
          Top = 50
          Width = 141
          Height = 55
          Caption = 'Insert Move Type'
          Columns = 2
          DataField = 'SampleAspInsertMoveType'
          DataSource = DataSource1
          Items.Strings = (
            'Default'
            'Block 1'
            'Block 2'
            'Block 3')
          ParentBackground = True
          TabOrder = 8
          Values.Strings = (
            '0'
            '1'
            '2'
            '3')
        end
        object boxSampleAspLiqDetSingle: TCheckBox
          Tag = 35340
          Left = 300
          Top = 12
          Width = 125
          Height = 17
          Caption = 'Single Tip'
          TabOrder = 9
          OnMouseUp = boxSampleAspLevTrMouseUp
        end
      end
      object DBRadioGroup3: TDBRadioGroup
        Tag = 35770
        Left = 197
        Top = 344
        Width = 132
        Height = 89
        Caption = 'System Air Suck Position'
        DataField = 'SysAirAspPos'
        DataSource = DataSource1
        Items.Strings = (
          'Wash Station'
          'Z Travel'
          'Z Scan'
          'Z Dispense')
        ParentBackground = True
        TabOrder = 9
        Values.Strings = (
          '0'
          '1'
          '2'
          '3')
      end
      object cbSAspSwitchModule: TDBComboBox
        Left = 104
        Top = 315
        Width = 113
        Height = 23
        DataField = 'AspSwitchModule'
        DataSource = DataSource1
        TabOrder = 10
      end
      object GroupBox13: TGroupBox
        Tag = 35910
        Left = 5
        Top = 344
        Width = 187
        Height = 89
        Caption = 'Retract after aspiration'
        TabOrder = 11
        object Label37: TLabel
          Tag = 35880
          Left = 8
          Top = 38
          Width = 75
          Height = 15
          Caption = 'Retract Speed:'
        end
        object Label4: TLabel
          Tag = 35900
          Left = 8
          Top = 62
          Width = 88
          Height = 15
          Caption = 'Retract Distance:'
        end
        object Label6: TLabel
          Left = 164
          Top = 64
          Width = 22
          Height = 15
          Caption = 'mm'
        end
        object DBcbAspSingleRetract: TDBCheckBox
          Tag = 35680
          Left = 8
          Top = 16
          Width = 161
          Height = 17
          Caption = 'Single'
          DataField = 'SampleAspTipSingleRetract'
          DataSource = DataSource1
          TabOrder = 0
          ValueChecked = 'true'
          ValueUnchecked = 'false'
        end
        object DBEdit10: TDBEdit
          Left = 111
          Top = 36
          Width = 50
          Height = 23
          DataField = 'SampleAspRetrSpeed'
          DataSource = DataSource1
          TabOrder = 1
        end
        object DBEdit2: TDBEdit
          Left = 111
          Top = 60
          Width = 50
          Height = 23
          DataField = 'SampleAspRetrDistance'
          DataSource = DataSource1
          TabOrder = 2
        end
      end
      object gbLiqClassAsp: TGroupBox
        Tag = 35960
        Left = 544
        Top = 8
        Width = 81
        Height = 113
        Caption = 'gbLiqClassAsp'
        TabOrder = 12
        object lLiqClassAsp: TLabel
          Tag = 35970
          Left = 8
          Top = 40
          Width = 66
          Height = 15
          Caption = 'lLiqClassAsp'
        end
        object stLiqClassNameAsp: TStaticText
          Left = 8
          Top = 56
          Width = 108
          Height = 19
          BorderStyle = sbsSunken
          Caption = 'stLiqClassNameAsp'
          TabOrder = 0
        end
      end
      object GroupBox37: TGroupBox
        Tag = 35315
        Left = 478
        Top = 260
        Width = 150
        Height = 173
        Caption = 'Tip Touch'
        TabOrder = 13
        object DBChkSampleAspTipTouch: TDBCheckBox
          Tag = 35315
          Left = 12
          Top = 18
          Width = 76
          Height = 16
          Caption = 'Tip Touch'
          DataField = 'SampleAspTipTouch'
          DataSource = DataSource1
          TabOrder = 0
          ValueChecked = 'True'
          ValueUnchecked = 'False'
          OnClick = DBChkSampleAspTipTouchClick
        end
        object pnlTipTouchAsp: TPanel
          Left = 8
          Top = 32
          Width = 137
          Height = 137
          BevelOuter = bvNone
          TabOrder = 1
          object Label45: TLabel
            Tag = 35320
            Left = 4
            Top = 86
            Width = 57
            Height = 15
            Caption = 'Delay [sec]'
          end
          object Label54: TLabel
            Tag = 35425
            Left = 4
            Top = 114
            Width = 54
            Height = 15
            Caption = 'Submerge'
          end
          object DBEdSampleAspTipTouchDelay: TDBEdit
            Left = 82
            Top = 84
            Width = 50
            Height = 23
            DataField = 'SampleAspTipTouchDelay'
            DataSource = DataSource1
            TabOrder = 0
          end
          object DBRadioGroup4: TDBRadioGroup
            Tag = 35870
            Left = 3
            Top = 47
            Width = 129
            Height = 31
            Caption = 'Scan Mode (Sensitivity)'
            Columns = 4
            DataField = 'SampleAspTipTouchScanMode'
            DataSource = DataSource1
            Items.Strings = (
              '1'
              '2'
              '3'
              '4')
            ParentBackground = True
            TabOrder = 1
            Values.Strings = (
              '0'
              '1'
              '2'
              '3')
          end
          object DBCheckBox1: TDBCheckBox
            Tag = 35340
            Left = 4
            Top = 28
            Width = 125
            Height = 17
            Caption = 'Single Tip'
            DataField = 'SampleAspTipTouchSingle'
            DataSource = DataSource1
            TabOrder = 2
            ValueChecked = 'True'
            ValueUnchecked = 'False'
          end
          object DBEdit7: TDBEdit
            Left = 82
            Top = 112
            Width = 50
            Height = 23
            DataField = 'SampleAspTipTouchSubmerge'
            DataSource = DataSource1
            TabOrder = 3
          end
          object DBCheckBox14: TDBCheckBox
            Tag = 35350
            Left = 4
            Top = 7
            Width = 125
            Height = 17
            Caption = 'Liquid Detection'
            DataField = 'SampleAspTipTouchScan'
            DataSource = DataSource1
            TabOrder = 4
            ValueChecked = 'True'
            ValueUnchecked = 'False'
          end
        end
      end
      object GroupBox12: TGroupBox
        Tag = 36150
        Left = 478
        Top = 123
        Width = 150
        Height = 134
        Caption = 'Special Movement'
        TabOrder = 14
        object Label38: TLabel
          Tag = 36130
          Left = 8
          Top = 20
          Width = 131
          Height = 15
          Caption = 'Move to System Air Suck'
        end
        object Label53: TLabel
          Tag = 36140
          Left = 8
          Top = 41
          Width = 81
          Height = 15
          Caption = 'Position Speed:'
        end
        object edMoveToSysAirPosSpeed: TDBEdit
          Left = 95
          Top = 38
          Width = 50
          Height = 23
          DataField = 'MoveToSysAirPosSpeed'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
    end
    object tbsSampleAspOptions: TTabSheet
      Tag = 35115
      Caption = 'Aspirate Sample Options'
      object gbWaste: TGroupBox
        Tag = 35200
        Left = 5
        Top = 96
        Width = 110
        Height = 88
        Caption = 'Additional Aspir.'
        TabOrder = 4
        object Label15: TLabel
          Tag = 35210
          Left = 4
          Top = 35
          Width = 32
          Height = 15
          Caption = 'Waste'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
      end
      object gbAfterAsp: TGroupBox
        Tag = 35790
        Left = 5
        Top = 187
        Width = 110
        Height = 88
        Caption = 'After Sample Aspir.'
        TabOrder = 6
        object Label18: TLabel
          Tag = 35230
          Left = 4
          Top = 25
          Width = 48
          Height = 15
          Caption = 'Spit Back'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object lbl1SmpAspCh2: TLabel
          Tag = 35800
          Left = 4
          Top = 55
          Width = 55
          Height = 15
          Caption = 'Wash with'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object lbl2SmpAspCh2: TLabel
          Tag = 35810
          Left = 12
          Top = 69
          Width = 53
          Height = 15
          Caption = 'Channel 2'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
      end
      object gbLiqClassSpit: TGroupBox
        Tag = 35960
        Left = 528
        Top = 200
        Width = 122
        Height = 89
        Caption = 'gbLiqClassSpit'
        TabOrder = 11
        Visible = False
        object lLiqClassSpit: TLabel
          Tag = 35970
          Left = 8
          Top = 32
          Width = 66
          Height = 15
          Caption = 'lLiqClassSpit'
        end
        object stLiqClassNameSpit: TStaticText
          Left = 8
          Top = 48
          Width = 108
          Height = 19
          BorderStyle = sbsSunken
          Caption = 'stLiqClassNameSpit'
          TabOrder = 0
        end
      end
      object gbLiqClassWaste: TGroupBox
        Tag = 35960
        Left = 511
        Top = 111
        Width = 224
        Height = 73
        Caption = 'gbLiqClassWaste'
        Color = clBtnFace
        ParentColor = False
        TabOrder = 12
        Visible = False
        object lLiqClassWaste: TLabel
          Tag = 35970
          Left = 8
          Top = 23
          Width = 78
          Height = 15
          Caption = 'lLiqClassWaste'
        end
        object stLiqClassWaste: TStaticText
          Left = 8
          Top = 40
          Width = 88
          Height = 19
          BorderStyle = sbsSunken
          Caption = 'stLiqClassWaste'
          TabOrder = 0
        end
      end
      object gbAfterAspVol: TGroupBox
        Tag = 35610
        Left = 120
        Top = 187
        Width = 107
        Height = 88
        Caption = ' Volume ['#181'l] '
        TabOrder = 7
        object lblSampleAspSpitBackX: TLabel
          Left = 67
          Top = 25
          Width = 5
          Height = 15
          Caption = 'x'
        end
        object edSampleAspSpitBackVol: TDBEdit
          Left = 11
          Top = 23
          Width = 50
          Height = 23
          DataField = 'SampleAspSpitBack'
          DataSource = DataSource1
          TabOrder = 0
        end
        object edSmpAspCh2Vol: TDBEdit
          Left = 11
          Top = 51
          Width = 50
          Height = 23
          DataField = 'SampleAspCh2WashVol'
          DataSource = DataSource1
          TabOrder = 1
        end
        object edSampleAspSpitBackCount: TDBEdit
          Left = 79
          Top = 23
          Width = 19
          Height = 23
          DataField = 'SampleAspSpitBackCount'
          DataSource = DataSource1
          TabOrder = 2
        end
      end
      object gbAfterAspCalc: TGroupBox
        Tag = 35620
        Left = 232
        Top = 187
        Width = 61
        Height = 88
        Caption = 'Calculate'
        TabOrder = 8
        object cbSampleAspSpitBackCalc: TDBCheckBox
          Left = 16
          Top = 25
          Width = 40
          Height = 17
          Caption = '-->'
          DataField = 'SampleAspSpitBackCalc'
          DataSource = DataSource1
          TabOrder = 0
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
        object cbSmpAspCh2Calc: TDBCheckBox
          Left = 16
          Top = 53
          Width = 40
          Height = 17
          Caption = '-->'
          DataField = 'SampleAspCh2WashCalc'
          DataSource = DataSource1
          TabOrder = 1
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
      end
      object gbAfterAspSpeed: TGroupBox
        Tag = 35630
        Left = 298
        Top = 187
        Width = 85
        Height = 88
        Caption = ' Speed ['#181'l/s] '
        TabOrder = 9
        object edSmpAspCh2Speed: TDBEdit
          Left = 15
          Top = 51
          Width = 50
          Height = 23
          DataField = 'SampleAspCh2WashSpeed'
          DataSource = DataSource1
          TabOrder = 1
        end
        object edSampleAspSpitBackSpeed: TDBEdit
          Left = 15
          Top = 23
          Width = 50
          Height = 23
          DataField = 'SampleAspSpitBackSpeed'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
      object gbAfterAspDelay: TGroupBox
        Tag = 35640
        Left = 388
        Top = 187
        Width = 85
        Height = 88
        Caption = ' Delay [sec] '
        TabOrder = 10
        object edSpitBackDelay: TDBEdit
          Left = 11
          Top = 36
          Width = 50
          Height = 23
          DataField = 'SampleAspCh2WashDelay'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
      object gbBeforeAspVol: TGroupBox
        Tag = 35610
        Left = 120
        Top = 5
        Width = 107
        Height = 88
        Caption = ' Volume ['#181'l] '
        TabOrder = 1
        object Label49: TLabel
          Left = 67
          Top = 37
          Width = 5
          Height = 15
          Caption = 'x'
        end
        object edExtraGapAirVol: TDBEdit
          Left = 11
          Top = 51
          Width = 50
          Height = 23
          DataField = 'ExtraGapAirVol'
          DataSource = DataSource1
          TabOrder = 1
        end
        object edExtraGapWasteVol: TDBEdit
          Left = 11
          Top = 23
          Width = 50
          Height = 23
          DataField = 'ExtraGapWasteVol'
          DataSource = DataSource1
          TabOrder = 0
        end
        object edExtraGapCount: TDBEdit
          Left = 79
          Top = 35
          Width = 19
          Height = 23
          DataField = 'ExtraGapCount'
          DataSource = DataSource1
          TabOrder = 2
        end
      end
      object gbBeforeAsp: TGroupBox
        Tag = 35795
        Left = 5
        Top = 5
        Width = 110
        Height = 88
        Caption = 'Before Sample Aspir.'
        TabOrder = 0
        object Label48: TLabel
          Tag = 35325
          Left = 4
          Top = 35
          Width = 56
          Height = 15
          Caption = 'Waste Gap'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label44: TLabel
          Tag = 35330
          Left = 73
          Top = 23
          Width = 32
          Height = 15
          Caption = 'Waste'
        end
        object Label46: TLabel
          Tag = 35335
          Left = 75
          Top = 51
          Width = 15
          Height = 15
          Caption = 'Air'
        end
      end
      object gbWasteVol: TGroupBox
        Tag = 35455
        Left = 120
        Top = 96
        Width = 109
        Height = 88
        Caption = 'Volume'
        TabOrder = 5
        object Label50: TLabel
          Left = 62
          Top = 26
          Width = 18
          Height = 15
          Caption = '['#181'l]'
        end
        object Label51: TLabel
          Left = 63
          Top = 55
          Width = 18
          Height = 15
          Caption = '[%]'
        end
        object edWasteperCent: TDBEdit
          Left = 11
          Top = 51
          Width = 50
          Height = 23
          DataField = 'SampleAspWastePerCent'
          DataSource = DataSource1
          TabOrder = 1
        end
        object edSampleAspWasteVol: TDBEdit
          Left = 11
          Top = 23
          Width = 49
          Height = 23
          DataField = 'SampleAspWasteVol'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
      object gbBeforeAspSpeed: TGroupBox
        Tag = 35630
        Left = 298
        Top = 5
        Width = 85
        Height = 88
        Caption = ' Speed ['#181'l/s] '
        TabOrder = 2
        object edExtraGapWasteSpeed: TDBEdit
          Left = 11
          Top = 23
          Width = 54
          Height = 23
          DataField = 'ExtraGapWasteSpeed'
          DataSource = DataSource1
          TabOrder = 0
        end
        object edExtraGapAirSpeed: TDBEdit
          Left = 11
          Top = 51
          Width = 54
          Height = 23
          DataField = 'ExtraGapAirSpeed'
          DataSource = DataSource1
          TabOrder = 1
        end
      end
      object gbBeforeAspDelay: TGroupBox
        Tag = 35640
        Left = 388
        Top = 5
        Width = 72
        Height = 88
        Caption = ' Delay [sec] '
        TabOrder = 3
        object dbExtraGapWasteDelay: TDBEdit
          Left = 11
          Top = 23
          Width = 54
          Height = 23
          DataField = 'ExtraGapWasteDelay'
          DataSource = DataSource1
          TabOrder = 0
        end
        object dbExtraGapAirDelay: TDBEdit
          Left = 11
          Top = 51
          Width = 54
          Height = 23
          DataField = 'ExtraGapAirDelay'
          DataSource = DataSource1
          TabOrder = 1
        end
      end
      object gbBeforeAspCalc: TGroupBox
        Tag = 35620
        Left = 232
        Top = 5
        Width = 61
        Height = 88
        Caption = 'Calculate'
        TabOrder = 13
        object DBCheckBox4: TDBCheckBox
          Left = 16
          Top = 25
          Width = 40
          Height = 17
          Caption = '-->'
          DataField = 'ExtraGapWasteCalc'
          DataSource = DataSource1
          TabOrder = 0
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
        object DBCheckBox6: TDBCheckBox
          Left = 16
          Top = 53
          Width = 40
          Height = 17
          Caption = '-->'
          DataField = 'ExtraGapAirCalc'
          DataSource = DataSource1
          TabOrder = 1
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
      end
      object rgExtraGapAirPos: TDBRadioGroup
        Tag = 35695
        Left = 466
        Top = 3
        Width = 119
        Height = 89
        Caption = 'Air Suck Position'
        DataField = 'ExtraGapAirPos'
        DataSource = DataSource1
        Items.Strings = (
          'Z Travel'
          'Z Scan'
          'Z Dispense')
        ParentBackground = True
        TabOrder = 14
        Values.Strings = (
          '0'
          '1'
          '2'
          '3')
      end
      object DBCheckBox16: TDBCheckBox
        Tag = 35232
        Left = 17
        Top = 281
        Width = 268
        Height = 17
        Caption = 'Spit back at aspiration position'
        DataField = 'SampleAspSpitBackAtAspPos'
        DataSource = DataSource1
        TabOrder = 15
        ValueChecked = 'Wahr'
        ValueUnchecked = 'Falsch'
      end
    end
    object tbsDilAsp: TTabSheet
      Tag = 35120
      Caption = 'Aspirate Diluent'
      object GroupBox15: TGroupBox
        Left = 5
        Top = 5
        Width = 110
        Height = 115
        TabOrder = 0
        object Label32: TLabel
          Tag = 35570
          Left = 4
          Top = 25
          Width = 56
          Height = 15
          Caption = 'System Air'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label33: TLabel
          Tag = 35750
          Left = 4
          Top = 52
          Width = 38
          Height = 15
          Caption = 'Diluent'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label35: TLabel
          Tag = 35600
          Left = 4
          Top = 80
          Width = 69
          Height = 15
          Caption = 'Transport Air'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
      end
      object GroupBox21: TGroupBox
        Tag = 35610
        Left = 120
        Top = 5
        Width = 107
        Height = 115
        Caption = ' Volume ['#181'l] '
        TabOrder = 1
        object lblSysAirAspVol: TDBText
          Left = 11
          Top = 25
          Width = 47
          Height = 17
          Alignment = taRightJustify
          DataField = 'SysAirAspVol'
          DataSource = DataSource1
        end
        object DBText2: TDBText
          Left = 11
          Top = 82
          Width = 47
          Height = 17
          Alignment = taRightJustify
          DataField = 'TransAirVol'
          DataSource = DataSource1
        end
      end
      object GroupBox22: TGroupBox
        Tag = 35620
        Left = 232
        Top = 5
        Width = 61
        Height = 115
        Caption = 'Calculate'
        TabOrder = 2
        object DBCheckBox10: TDBCheckBox
          Left = 16
          Top = 53
          Width = 40
          Height = 17
          Caption = '-->'
          DataField = 'DilAspCalc'
          DataSource = DataSource1
          TabOrder = 0
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
      end
      object GroupBox23: TGroupBox
        Tag = 35630
        Left = 298
        Top = 5
        Width = 85
        Height = 115
        Caption = ' Speed ['#181'l/s] '
        TabOrder = 3
        object DBText3: TDBText
          Left = 11
          Top = 82
          Width = 47
          Height = 17
          Alignment = taRightJustify
          DataField = 'TransAirSpeed'
          DataSource = DataSource1
        end
        object DBText4: TDBText
          Left = 11
          Top = 25
          Width = 47
          Height = 17
          Alignment = taRightJustify
          DataField = 'SysAirAspSpeed'
          DataSource = DataSource1
        end
        object edDilAspSpeed: TDBEdit
          Left = 11
          Top = 51
          Width = 50
          Height = 23
          DataField = 'DilAspSpeed'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
      object GroupBox24: TGroupBox
        Tag = 35640
        Left = 388
        Top = 5
        Width = 85
        Height = 115
        Caption = ' Delay [sec] '
        TabOrder = 4
        object DBText9: TDBText
          Left = 11
          Top = 25
          Width = 47
          Height = 17
          Alignment = taRightJustify
          DataField = 'SysAirAspDelay'
          DataSource = DataSource1
        end
        object Label25: TLabel
          Left = 11
          Top = 82
          Width = 47
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = '0'
        end
        object edDilAspDelay: TDBEdit
          Left = 11
          Top = 51
          Width = 50
          Height = 23
          DataField = 'DilAspDelay'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
      object rgrDilAspSwichPos: TDBRadioGroup
        Tag = 35650
        Left = 5
        Top = 244
        Width = 217
        Height = 81
        Caption = 'Switch Position'
        DataField = 'DilAspSwitchPos'
        DataSource = DataSource1
        Items.Strings = (
          'Switch outside tube'
          'Switch inside tube'
          'Don'#39't switch')
        ParentBackground = True
        TabOrder = 5
        Values.Strings = (
          '1'
          '2'
          '0')
        OnChange = rgrDilAspSwichPosChange
      end
      object GroupBox25: TGroupBox
        Tag = 35670
        Left = 227
        Top = 244
        Width = 246
        Height = 81
        TabOrder = 6
        object boxDilAspLevTr: TCheckBox
          Tag = 35360
          Left = 10
          Top = 17
          Width = 157
          Height = 17
          Caption = 'Level Tracking'
          TabOrder = 0
          OnMouseUp = boxDilAspLevTrMouseUp
        end
      end
      object GroupBox20: TGroupBox
        Tag = 35150
        Left = 5
        Top = 123
        Width = 468
        Height = 118
        Caption = 'Diluent Aspiration '
        TabOrder = 7
        object lblDilAspScanSpeed: TLabel
          Tag = 35885
          Left = 8
          Top = 20
          Width = 63
          Height = 15
          Caption = 'Scan Speed:'
        end
        object Label41: TLabel
          Tag = 35420
          Left = 8
          Top = 46
          Width = 84
          Height = 15
          Caption = 'Submerge[mm]'
        end
        object EditDilAspSubmerge: TDBEdit
          Left = 100
          Top = 42
          Width = 50
          Height = 23
          DataField = 'DilAspSubmerge'
          DataSource = DataSource1
          TabOrder = 0
        end
        object boxDilAspLiqDet: TCheckBox
          Tag = 35350
          Left = 178
          Top = 12
          Width = 119
          Height = 17
          Caption = 'Liquid Detection'
          TabOrder = 1
          OnClick = boxDilAspLiqDetClick
          OnMouseUp = boxDilAspLiqDetMouseUp
        end
        object edDilAspLiqDet: TDBEdit
          Left = 286
          Top = 1
          Width = 24
          Height = 23
          DataField = 'DilAspLiqDet'
          DataSource = DataSource1
          TabOrder = 2
          Visible = False
          OnChange = edDilAspLiqDetChange
        end
        object edDilAspScanSpeed: TDBEdit
          Left = 100
          Top = 18
          Width = 50
          Height = 23
          DataField = 'DilAspScanSpeed'
          DataSource = DataSource1
          TabOrder = 3
        end
        object cbDilAspNoCalc: TCheckBox
          Tag = 35930
          Left = 8
          Top = 90
          Width = 168
          Height = 17
          Caption = 'Do not use level calculation'
          TabOrder = 4
          OnMouseUp = boxDilAspLiqDetMouseUp
        end
        object pnDilAspLiqDet: TPanel
          Left = 174
          Top = 28
          Width = 289
          Height = 84
          BevelOuter = bvNone
          TabOrder = 5
          object GroupBox32: TGroupBox
            Tag = 35370
            Left = 140
            Top = 3
            Width = 145
            Height = 55
            Caption = 'in case of error'
            TabOrder = 0
            object boxDilAspGotoZ: TRadioButton
              Tag = 35380
              Left = 8
              Top = 16
              Width = 130
              Height = 17
              Caption = 'Goto Zmax'
              Checked = True
              TabOrder = 0
              TabStop = True
              OnMouseUp = boxDilAspGotoZMouseUp
            end
            object boxDilAspDispErr: TRadioButton
              Tag = 35390
              Left = 8
              Top = 34
              Width = 130
              Height = 17
              Caption = 'Display Error'
              TabOrder = 1
              OnMouseUp = boxDilAspDispErrMouseUp
            end
          end
          object grbDilAspScanMode: TDBRadioGroup
            Tag = 35870
            Left = 4
            Top = 10
            Width = 129
            Height = 31
            Caption = 'Scan Mode (Sensitivity)'
            Columns = 4
            DataField = 'DilAspScanMode'
            DataSource = DataSource1
            Items.Strings = (
              '1'
              '2'
              '3'
              '4')
            ParentBackground = True
            TabOrder = 1
            Values.Strings = (
              '0'
              '1'
              '2'
              '3')
          end
        end
        object EditDilAspErrFlag: TDBCheckBox
          Tag = 35250
          Left = 8
          Top = 70
          Width = 168
          Height = 17
          Caption = 'Disable Z Error at Diluent Aspirate'
          DataField = 'DilAspErrFlag'
          DataSource = DataSource1
          TabOrder = 6
          ValueChecked = '1'
          ValueUnchecked = '0'
        end
        object rgDilAspInsertMoveType: TDBRadioGroup
          Tag = 36110
          Left = 180
          Top = 42
          Width = 141
          Height = 55
          Caption = 'Insert Move Type'
          Columns = 2
          DataField = 'DilAspInsertMoveType'
          DataSource = DataSource1
          Items.Strings = (
            'Default'
            'Block 1'
            'Block 2'
            'Block 3')
          ParentBackground = True
          TabOrder = 7
          Values.Strings = (
            '0'
            '1'
            '2'
            '3')
        end
        object boxDilAspLiqDetSingle: TCheckBox
          Tag = 35340
          Left = 316
          Top = 12
          Width = 133
          Height = 17
          Caption = 'Single Tip'
          TabOrder = 8
          OnMouseUp = boxDilAspLevTrMouseUp
        end
      end
      object cbDAspSwitchModule: TDBComboBox
        Left = 104
        Top = 298
        Width = 113
        Height = 23
        DataField = 'DilAspSwitchModule'
        DataSource = DataSource1
        TabOrder = 8
      end
      object GroupBox31: TGroupBox
        Tag = 35910
        Left = 5
        Top = 328
        Width = 188
        Height = 89
        Caption = 'Retract after aspiration'
        TabOrder = 9
        object Label36: TLabel
          Tag = 35880
          Left = 8
          Top = 38
          Width = 75
          Height = 15
          Caption = 'Retract Speed:'
        end
        object Label39: TLabel
          Tag = 35900
          Left = 8
          Top = 62
          Width = 88
          Height = 15
          Caption = 'Retract Distance:'
        end
        object Label40: TLabel
          Left = 164
          Top = 64
          Width = 22
          Height = 15
          Caption = 'mm'
        end
        object DBEdit8: TDBEdit
          Left = 111
          Top = 60
          Width = 50
          Height = 23
          DataField = 'DilAspRetrDistance'
          DataSource = DataSource1
          TabOrder = 0
        end
        object DBCheckBox2: TDBCheckBox
          Tag = 35680
          Left = 8
          Top = 16
          Width = 173
          Height = 17
          Caption = 'Single'
          DataField = 'DilAspTipSingleRetract'
          DataSource = DataSource1
          TabOrder = 1
          ValueChecked = 'true'
          ValueUnchecked = 'false'
        end
        object DBEdit6: TDBEdit
          Left = 111
          Top = 36
          Width = 50
          Height = 23
          DataField = 'DilAspRetrSpeed'
          DataSource = DataSource1
          TabOrder = 2
        end
      end
      object GroupBox35: TGroupBox
        Left = 198
        Top = 328
        Width = 276
        Height = 89
        TabOrder = 10
        object cbDilAspChannel2: TDBCheckBox
          Tag = 35780
          Left = 10
          Top = 15
          Width = 200
          Height = 17
          Caption = 'Use Channel 2'
          DataField = 'DilAspChannel2'
          DataSource = DataSource1
          TabOrder = 0
          ValueChecked = '1'
          ValueUnchecked = '0'
        end
      end
      object gbLiqClassDil: TGroupBox
        Tag = 35960
        Left = 504
        Top = 8
        Width = 73
        Height = 113
        Caption = 'gbLiqClassDil'
        TabOrder = 11
        object lLiqClassDil: TLabel
          Tag = 35970
          Left = 8
          Top = 40
          Width = 60
          Height = 15
          Caption = 'lLiqClassDil'
        end
        object stLiqClassNameDil: TStaticText
          Left = 8
          Top = 56
          Width = 108
          Height = 19
          BorderStyle = sbsSunken
          Caption = 'stLiqClassNameAsp'
          TabOrder = 0
        end
      end
    end
    object tbsDispense: TTabSheet
      Tag = 35130
      Caption = 'Dispense'
      object GroupBox5: TGroupBox
        Tag = 35610
        Left = 120
        Top = 5
        Width = 107
        Height = 115
        Caption = ' Volume ['#181'l] '
        TabOrder = 0
        object DBText5: TDBText
          Left = 11
          Top = 82
          Width = 47
          Height = 17
          Alignment = taRightJustify
          Color = clBtnFace
          DataField = 'TransAirVol'
          DataSource = DataSource1
          ParentColor = False
        end
        object EditSysAirDispVol: TDBEdit
          Left = 11
          Top = 23
          Width = 50
          Height = 23
          DataField = 'SysAirDispVol'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
      object GroupBox6: TGroupBox
        Tag = 35630
        Left = 298
        Top = 5
        Width = 85
        Height = 115
        Caption = ' Speed ['#181'l/s] '
        TabOrder = 1
        object DBText7: TDBText
          Left = 11
          Top = 27
          Width = 47
          Height = 17
          Alignment = taRightJustify
          Color = clBtnFace
          DataField = 'DispSpeed'
          DataSource = DataSource1
          ParentColor = False
        end
        object DBText8: TDBText
          Left = 11
          Top = 82
          Width = 47
          Height = 17
          Alignment = taRightJustify
          Color = clBtnFace
          DataField = 'DispSpeed'
          DataSource = DataSource1
          ParentColor = False
        end
        object EditDispSpeed: TDBEdit
          Left = 11
          Top = 51
          Width = 50
          Height = 23
          DataField = 'DispSpeed'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
      object GroupBox7: TGroupBox
        Tag = 35640
        Left = 388
        Top = 5
        Width = 85
        Height = 115
        Caption = ' Delay [sec] '
        TabOrder = 2
        object EditDispDelay: TDBEdit
          Left = 11
          Top = 51
          Width = 50
          Height = 23
          DataField = 'DispDelay'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
      object GroupBox10: TGroupBox
        Tag = 35620
        Left = 232
        Top = 5
        Width = 61
        Height = 115
        Caption = 'Calculate'
        TabOrder = 3
        object DBCheckBox5: TDBCheckBox
          Left = 16
          Top = 53
          Width = 40
          Height = 17
          Caption = '-->'
          DataField = 'DispCalc'
          DataSource = DataSource1
          TabOrder = 0
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
      end
      object rgrDspSwitchPos: TDBRadioGroup
        Tag = 35650
        Left = 5
        Top = 260
        Width = 217
        Height = 81
        Caption = 'Switch Position'
        DataField = 'DspSwitchPos'
        DataSource = DataSource1
        Items.Strings = (
          'Switch outside tube'
          'Switch inside tube'
          'Don'#39't switch')
        ParentBackground = True
        TabOrder = 4
        Values.Strings = (
          '1'
          '2'
          '0')
        OnChange = rgrDspSwitchPosChange
      end
      object GroupBox17: TGroupBox
        Tag = 35660
        Left = 198
        Top = 344
        Width = 275
        Height = 89
        Caption = 'Step - Performing'
        TabOrder = 5
        object Label22: TLabel
          Tag = 35610
          Left = 10
          Top = 28
          Width = 62
          Height = 15
          Caption = 'Volume ['#181'l]'
        end
        object Label23: TLabel
          Tag = 35640
          Left = 10
          Top = 58
          Width = 57
          Height = 15
          Caption = 'Delay [sec]'
        end
        object edDispStepVolume: TDBEdit
          Left = 96
          Top = 23
          Width = 50
          Height = 23
          DataField = 'DispStepVolume'
          DataSource = DataSource1
          MaxLength = 6
          TabOrder = 0
        end
        object edDispStepDelay: TDBEdit
          Left = 96
          Top = 53
          Width = 50
          Height = 23
          DataField = 'DispStepDelay'
          DataSource = DataSource1
          MaxLength = 6
          TabOrder = 1
        end
      end
      object GroupBox18: TGroupBox
        Left = 227
        Top = 260
        Width = 246
        Height = 81
        TabOrder = 6
        object boxDispLevTr: TCheckBox
          Tag = 35360
          Left = 8
          Top = 6
          Width = 191
          Height = 17
          Caption = 'Level Tracking'
          TabOrder = 0
          OnMouseUp = boxDispLevTrMouseUp
        end
        object cbTransAirRetakeAfterDisp: TDBCheckBox
          Tag = 36151
          Left = 8
          Top = 28
          Width = 233
          Height = 17
          Caption = 'Retake Transport air between multi disps'
          DataField = 'TransAirRetakeBetweenDisp'
          DataSource = DataSource1
          TabOrder = 1
          ValueChecked = 'true'
          ValueUnchecked = 'false'
        end
        object DBCheckBox11: TDBCheckBox
          Tag = 36152
          Left = 8
          Top = 50
          Width = 233
          Height = 17
          Caption = 'Retake Transport air after (last) dispense'
          DataField = 'TransAirRetakeAfterLastDisp'
          DataSource = DataSource1
          TabOrder = 2
          ValueChecked = 'true'
          ValueUnchecked = 'false'
        end
      end
      object GroupBox9: TGroupBox
        Left = 5
        Top = 5
        Width = 110
        Height = 115
        TabOrder = 7
        object Label2: TLabel
          Tag = 35570
          Left = 4
          Top = 25
          Width = 56
          Height = 15
          Caption = 'System Air'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label3: TLabel
          Tag = 35580
          Left = 4
          Top = 47
          Width = 43
          Height = 15
          Caption = 'Diluent/'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label9: TLabel
          Tag = 35590
          Left = 12
          Top = 60
          Width = 39
          Height = 15
          Caption = 'Sample'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label10: TLabel
          Tag = 35600
          Left = 4
          Top = 80
          Width = 69
          Height = 15
          Caption = 'Transport Air'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
      end
      object GroupBox14: TGroupBox
        Tag = 35150
        Left = 5
        Top = 123
        Width = 468
        Height = 134
        Caption = 'Dispense'
        TabOrder = 8
        object lblDispScanSpeed: TLabel
          Tag = 35885
          Left = 8
          Top = 20
          Width = 63
          Height = 15
          Caption = 'Scan Speed:'
        end
        object Label34: TLabel
          Tag = 35420
          Left = 8
          Top = 46
          Width = 84
          Height = 15
          Caption = 'Submerge[mm]'
        end
        object boxDispLiqDet: TCheckBox
          Tag = 35350
          Left = 178
          Top = 12
          Width = 130
          Height = 17
          Caption = 'Liquid Detection'
          TabOrder = 0
          OnClick = boxDispLiqDetClick
          OnMouseUp = boxDispLiqDetMouseUp
        end
        object edDispSubmerge: TDBEdit
          Left = 100
          Top = 42
          Width = 50
          Height = 23
          DataField = 'DispSubmerge'
          DataSource = DataSource1
          TabOrder = 1
        end
        object edDispLiqDet: TDBEdit
          Left = 318
          Top = 1
          Width = 24
          Height = 23
          DataField = 'DispLiqDet'
          DataSource = DataSource1
          TabOrder = 2
          Visible = False
          OnChange = edDispLiqDetChange
        end
        object edDispScanSpeed: TDBEdit
          Left = 100
          Top = 18
          Width = 50
          Height = 23
          DataField = 'DispScanSpeed'
          DataSource = DataSource1
          TabOrder = 3
        end
        object cbDispNoCalc: TCheckBox
          Tag = 35930
          Left = 8
          Top = 90
          Width = 168
          Height = 17
          Caption = 'Do not use level calculation'
          TabOrder = 4
          OnMouseUp = boxDispLiqDetMouseUp
        end
        object pnDispLiqDet: TPanel
          Left = 174
          Top = 28
          Width = 289
          Height = 84
          BevelOuter = bvNone
          TabOrder = 5
          object GroupBox33: TGroupBox
            Tag = 35370
            Left = 140
            Top = 3
            Width = 145
            Height = 55
            Caption = 'in case of error'
            TabOrder = 0
            object boxDispGotoZ: TRadioButton
              Tag = 35400
              Left = 8
              Top = 16
              Width = 130
              Height = 17
              Caption = 'Goto ZDisp'
              Checked = True
              TabOrder = 0
              TabStop = True
              OnMouseUp = boxDispGotoZMouseUp
            end
            object boxDispDispErr: TRadioButton
              Tag = 35390
              Left = 8
              Top = 34
              Width = 130
              Height = 17
              Caption = 'Display Error'
              TabOrder = 1
              OnMouseUp = boxDispDispErrMouseUp
            end
          end
          object grbDispScanMode: TDBRadioGroup
            Tag = 35870
            Left = 4
            Top = 10
            Width = 129
            Height = 31
            Caption = 'Scan Mode (Sensitivity)'
            Columns = 4
            DataField = 'DispScanMode'
            DataSource = DataSource1
            Items.Strings = (
              '1'
              '2'
              '3'
              '4')
            ParentBackground = True
            TabOrder = 1
            Values.Strings = (
              '0'
              '1'
              '2'
              '3')
          end
        end
        object EditDispErrFlag: TDBCheckBox
          Tag = 35250
          Left = 8
          Top = 70
          Width = 168
          Height = 17
          Caption = 'Disable Z Error at Dispense '
          DataField = 'DispErrFlag'
          DataSource = DataSource1
          TabOrder = 6
          ValueChecked = '1'
          ValueUnchecked = '0'
        end
        object cbDispUseDetectedLevel: TCheckBox
          Tag = 36120
          Left = 8
          Top = 110
          Width = 168
          Height = 17
          Caption = 'Use detected level'
          TabOrder = 7
          OnMouseUp = boxDispLiqDetMouseUp
        end
        object rgDispInsertMoveType: TDBRadioGroup
          Tag = 36110
          Left = 180
          Top = 42
          Width = 141
          Height = 55
          Caption = 'Insert Move Type'
          Columns = 2
          DataField = 'DispInsertMoveType'
          DataSource = DataSource1
          Items.Strings = (
            'Default'
            'Block 1'
            'Block 2'
            'Block 3')
          ParentBackground = True
          TabOrder = 8
          Values.Strings = (
            '0'
            '1'
            '2'
            '3')
        end
        object boxDispLiqDetSingle: TCheckBox
          Tag = 35340
          Left = 324
          Top = 12
          Width = 121
          Height = 17
          Caption = 'Single Tip'
          TabOrder = 9
          OnMouseUp = boxDispLevTrMouseUp
        end
      end
      object cbDispSwitchModule: TDBComboBox
        Left = 104
        Top = 314
        Width = 113
        Height = 23
        DataField = 'DspSwitchModule'
        DataSource = DataSource1
        TabOrder = 9
      end
      object GroupBox34: TGroupBox
        Tag = 35910
        Left = 5
        Top = 344
        Width = 188
        Height = 89
        Caption = 'Retract after aspiration'
        TabOrder = 10
        object Label13: TLabel
          Tag = 35880
          Left = 8
          Top = 38
          Width = 75
          Height = 15
          Caption = 'Retract Speed:'
        end
        object Label21: TLabel
          Tag = 35900
          Left = 8
          Top = 62
          Width = 88
          Height = 15
          Caption = 'Retract Distance:'
        end
        object Label42: TLabel
          Left = 164
          Top = 64
          Width = 22
          Height = 15
          Caption = 'mm'
        end
        object DBEdit12: TDBEdit
          Left = 111
          Top = 60
          Width = 50
          Height = 23
          DataField = 'DispRetrDistance'
          DataSource = DataSource1
          TabOrder = 0
        end
        object DBCheckBox8: TDBCheckBox
          Tag = 35680
          Left = 8
          Top = 16
          Width = 169
          Height = 17
          Caption = 'Single'
          DataField = 'DispTipSingleRetract'
          DataSource = DataSource1
          TabOrder = 1
          ValueChecked = 'true'
          ValueUnchecked = 'false'
        end
        object DBEdit3: TDBEdit
          Left = 111
          Top = 36
          Width = 50
          Height = 23
          DataField = 'DispRetrSpeed'
          DataSource = DataSource1
          TabOrder = 2
        end
      end
      object gbLiqClassDisp: TGroupBox
        Tag = 35960
        Left = 520
        Top = 8
        Width = 89
        Height = 113
        Caption = 'gbLiqClassDisp'
        TabOrder = 11
        object lLiqClassDisp: TLabel
          Tag = 35970
          Left = 8
          Top = 40
          Width = 69
          Height = 15
          Caption = 'lLiqClassDisp'
        end
        object stLiqClassNameDisp: TStaticText
          Left = 8
          Top = 56
          Width = 108
          Height = 19
          BorderStyle = sbsSunken
          Caption = 'stLiqClassNameAsp'
          TabOrder = 0
        end
      end
      object GroupBox36: TGroupBox
        Tag = 35315
        Left = 478
        Top = 233
        Width = 150
        Height = 200
        Caption = 'Tip Touch'
        TabOrder = 12
        object DBChkDispTipTouch: TDBCheckBox
          Tag = 35315
          Left = 12
          Top = 18
          Width = 76
          Height = 16
          Caption = 'Tip Touch'
          DataField = 'DispTipTouch'
          DataSource = DataSource1
          TabOrder = 0
          ValueChecked = 'True'
          ValueUnchecked = 'False'
          OnClick = DBChkDispTipTouchClick
        end
        object pnlTipTouchDisp: TPanel
          Left = 8
          Top = 32
          Width = 137
          Height = 161
          BevelOuter = bvNone
          TabOrder = 1
          object Label47: TLabel
            Tag = 35320
            Left = 4
            Top = 110
            Width = 57
            Height = 15
            Caption = 'Delay [sec]'
          end
          object Label55: TLabel
            Tag = 35425
            Left = 4
            Top = 138
            Width = 54
            Height = 15
            Caption = 'Submerge'
          end
          object DBEdit5: TDBEdit
            Left = 82
            Top = 108
            Width = 50
            Height = 23
            DataField = 'DispTipTouchDelay'
            DataSource = DataSource1
            TabOrder = 0
          end
          object DBRadioGroup5: TDBRadioGroup
            Tag = 35870
            Left = 3
            Top = 71
            Width = 129
            Height = 31
            Caption = 'Scan Mode (Sensitivity)'
            Columns = 4
            DataField = 'DispTipTouchScanMode'
            DataSource = DataSource1
            Items.Strings = (
              '1'
              '2'
              '3'
              '4')
            ParentBackground = True
            TabOrder = 1
            Values.Strings = (
              '0'
              '1'
              '2'
              '3')
          end
          object DBChkDispTipTouchSingle: TDBCheckBox
            Tag = 35340
            Left = 4
            Top = 52
            Width = 125
            Height = 17
            Caption = 'Single Tip'
            DataField = 'DispTipTouchSingle'
            DataSource = DataSource1
            TabOrder = 2
            ValueChecked = 'True'
            ValueUnchecked = 'False'
          end
          object DBChkDispTipTouchScan: TDBCheckBox
            Tag = 35350
            Left = 4
            Top = 7
            Width = 133
            Height = 17
            Caption = 'Liquid Detection'
            DataField = 'DispTipTouchScan'
            DataSource = DataSource1
            TabOrder = 3
            ValueChecked = 'True'
            ValueUnchecked = 'False'
            OnClick = DBChkDispTipTouchScanClick
          end
          object DBEdit9: TDBEdit
            Left = 82
            Top = 136
            Width = 50
            Height = 23
            DataField = 'DispTipTouchSubmerge'
            DataSource = DataSource1
            TabOrder = 4
          end
          object DBChkDispTipTouchStoreVol: TDBCheckBox
            Tag = 36180
            Left = 20
            Top = 29
            Width = 133
            Height = 17
            Caption = 'Store detected vol'
            DataField = 'DispTipTouchScanStoreVol'
            DataSource = DataSource1
            TabOrder = 5
            ValueChecked = 'True'
            ValueUnchecked = 'False'
          end
        end
      end
    end
    object tbsMix: TTabSheet
      Tag = 35140
      Caption = 'Mix'
      object GroupBox3: TGroupBox
        Tag = 35140
        Left = 1
        Top = 5
        Width = 158
        Height = 372
        Caption = 'Mix'
        TabOrder = 0
        object LblCycles: TLabel
          Tag = 35440
          Left = 4
          Top = 30
          Width = 150
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Cycles :'
        end
        object LblVolume: TLabel
          Tag = 35450
          Left = 4
          Top = 60
          Width = 150
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Volume ['#181'L]:'
        end
        object LblSpeed: TLabel
          Tag = 35470
          Left = 5
          Top = 146
          Width = 150
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Mix Aspiration Speed ['#181'L/s]:'
        end
        object Label28: TLabel
          Tag = 35480
          Left = 5
          Top = 170
          Width = 150
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Mix Dispense Speed ['#181'L/s]:'
        end
        object Label30: TLabel
          Tag = 35460
          Left = 4
          Top = 84
          Width = 150
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Min. Rest Volume ['#181'L]:'
        end
        object Label31: TLabel
          Tag = 35500
          Left = 5
          Top = 200
          Width = 150
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'ZMax-Offset [mm]:'
        end
      end
      object GroupBox4: TGroupBox
        Tag = 35510
        Left = 162
        Top = 5
        Width = 219
        Height = 372
        Caption = 'Before Sample Aspiration '
        TabOrder = 1
        object edSampleAspMixCycl: TDBEdit
          Left = 47
          Top = 26
          Width = 74
          Height = 23
          DataField = 'SampleAspMixCycl'
          DataSource = DataSource1
          TabOrder = 0
        end
        object edSampleAspMixVol: TDBEdit
          Left = 47
          Top = 56
          Width = 74
          Height = 23
          DataField = 'SampleAspMixVol'
          DataSource = DataSource1
          TabOrder = 1
        end
        object edSampleAspMixSpeed: TDBEdit
          Left = 47
          Top = 143
          Width = 74
          Height = 23
          DataField = 'SampleAspMixSpeed'
          DataSource = DataSource1
          TabOrder = 2
        end
        object edSampleAspMixDispSpeed: TDBEdit
          Left = 47
          Top = 167
          Width = 74
          Height = 23
          DataField = 'SampleAspMixDispSpeed'
          DataSource = DataSource1
          TabOrder = 3
        end
        object edSampleAspMixZOffset: TDBEdit
          Left = 47
          Top = 197
          Width = 74
          Height = 23
          DataField = 'SampleAspMixZOffset'
          DataSource = DataSource1
          TabOrder = 4
        end
        object dbSampleAspMixVolPercent: TDBEdit
          Left = 47
          Top = 80
          Width = 74
          Height = 23
          DataField = 'SampleAspMixMinRestVol'
          DataSource = DataSource1
          TabOrder = 5
        end
        object DBCheckBox7: TDBCheckBox
          Tag = 35540
          Left = 16
          Top = 233
          Width = 181
          Height = 17
          Caption = 'Mix only at first aspiration'
          DataField = 'SampleAspMixFirstOnly'
          DataSource = DataSource1
          TabOrder = 6
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
        object gbSampleAspMixMethod: TGroupBox
          Tag = 35520
          Left = 8
          Top = 257
          Width = 185
          Height = 104
          Caption = 'gbSampleAspMixMethod'
          TabOrder = 7
          object cbSampleAspMixMethod1: TCheckBox
            Tag = 36060
            Left = 8
            Top = 16
            Width = 174
            Height = 17
            Caption = 'Dispense at zmax-offset'
            TabOrder = 0
            OnMouseUp = cbSampleAspMixMethodMouseUp
          end
          object cbSampleAspMixMethod2: TCheckBox
            Tag = 36070
            Left = 8
            Top = 32
            Width = 174
            Height = 17
            Caption = 'Mix with air'
            TabOrder = 1
            OnMouseUp = cbSampleAspMixMethodMouseUp
          end
          object edSampleAspMixMethod: TDBEdit
            Left = 135
            Top = 12
            Width = 26
            Height = 23
            DataField = 'SampleAspMixMethod'
            DataSource = DataSource1
            TabOrder = 2
            Visible = False
            OnChange = edSampleAspMixMethodChange
          end
          object cbSampleAspMixMethod5: TCheckBox
            Tag = 36071
            Left = 8
            Top = 80
            Width = 174
            Height = 17
            Caption = 'Use retract speed'
            TabOrder = 3
            OnMouseUp = cbSampleAspMixMethodMouseUp
          end
        end
        object DBCheckBox12: TDBCheckBox
          Tag = 36160
          Left = 16
          Top = 109
          Width = 200
          Height = 17
          Caption = 'Use Calculated Volume'
          DataField = 'SampleAspMixUseCalculatedVol'
          DataSource = DataSource1
          TabOrder = 8
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
      end
      object GroupBox8: TGroupBox
        Tag = 35530
        Left = 387
        Top = 3
        Width = 214
        Height = 372
        Caption = 'At Dispense '
        TabOrder = 2
        object Label57: TLabel
          Tag = 35421
          Left = 20
          Top = 235
          Width = 84
          Height = 15
          Caption = 'Submerge[mm]'
        end
        object edDispMixCycl: TDBEdit
          Left = 39
          Top = 26
          Width = 74
          Height = 23
          DataField = 'DispMixCycl'
          DataSource = DataSource1
          TabOrder = 0
        end
        object edDispMixVol: TDBEdit
          Left = 39
          Top = 56
          Width = 74
          Height = 23
          DataField = 'DispMixVol'
          DataSource = DataSource1
          TabOrder = 1
        end
        object edDispMixSpeed: TDBEdit
          Left = 43
          Top = 143
          Width = 74
          Height = 23
          DataField = 'DispMixSpeed'
          DataSource = DataSource1
          TabOrder = 2
        end
        object edDispMixDispSpeed: TDBEdit
          Left = 43
          Top = 167
          Width = 74
          Height = 23
          DataField = 'DispMixDispSpeed'
          DataSource = DataSource1
          TabOrder = 3
        end
        object edDispMixZOffset: TDBEdit
          Left = 43
          Top = 197
          Width = 74
          Height = 23
          DataField = 'DispMixZOffset'
          DataSource = DataSource1
          TabOrder = 4
        end
        object edDispMixVolPercent: TDBEdit
          Left = 39
          Top = 80
          Width = 74
          Height = 23
          DataField = 'DispMixMinRestVol'
          DataSource = DataSource1
          TabOrder = 5
        end
        object gbDispMixMethod: TGroupBox
          Tag = 35520
          Left = 12
          Top = 257
          Width = 185
          Height = 104
          Caption = 'gbDispMixMethod'
          TabOrder = 6
          object cbDispMixMethod1: TCheckBox
            Tag = 36060
            Left = 8
            Top = 16
            Width = 174
            Height = 17
            Caption = 'Dispense at zmax-offset'
            TabOrder = 0
            OnMouseUp = cbDispMixMethodMouseUp
          end
          object cbDispMixMethod2: TCheckBox
            Tag = 36070
            Left = 8
            Top = 32
            Width = 174
            Height = 17
            Caption = 'Mix with air'
            TabOrder = 1
            OnMouseUp = cbDispMixMethodMouseUp
          end
          object edDispMixMethod: TDBEdit
            Left = 135
            Top = 12
            Width = 26
            Height = 23
            DataField = 'DispMixMethod'
            DataSource = DataSource1
            TabOrder = 2
            Visible = False
            OnChange = edDispMixMethodChange
          end
          object cbDispMixMethod3: TCheckBox
            Tag = 36080
            Left = 8
            Top = 48
            Width = 174
            Height = 17
            Caption = 'Tracking at asp'
            TabOrder = 3
            OnMouseUp = cbDispMixMethodMouseUp
          end
          object cbDispMixMethod4: TCheckBox
            Tag = 36090
            Left = 8
            Top = 64
            Width = 174
            Height = 17
            Caption = 'Liquid detection'
            TabOrder = 4
            OnMouseUp = cbDispMixMethodMouseUp
          end
          object cbDispMixMethod5: TCheckBox
            Tag = 36071
            Left = 8
            Top = 80
            Width = 174
            Height = 17
            Caption = 'Use retract speed'
            TabOrder = 5
            OnMouseUp = cbDispMixMethodMouseUp
          end
        end
        object DBCheckBox13: TDBCheckBox
          Tag = 36160
          Left = 16
          Top = 109
          Width = 195
          Height = 17
          Caption = 'Use Calculated Volume'
          DataField = 'DispMixUseCalculatedVol'
          DataSource = DataSource1
          TabOrder = 7
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
        end
        object edDispMixAspSubmerge: TDBEdit
          Left = 150
          Top = 232
          Width = 50
          Height = 23
          DataField = 'DispMixAspSubmerge'
          DataSource = DataSource1
          TabOrder = 8
        end
      end
    end
    object tbsOptions: TTabSheet
      Tag = 35160
      Caption = 'Options'
      ImageIndex = 6
      object gbVolCorrCurve: TGroupBox
        Tag = 35985
        Left = 4
        Top = 3
        Width = 226
        Height = 50
        Caption = 'gbVolCorrCurve'
        TabOrder = 0
        object dcbVolCorrCurve: TDBComboBox
          Left = 9
          Top = 18
          Width = 208
          Height = 23
          DataField = 'VolCorrCurve'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
      object gbWash: TGroupBox
        Tag = 35170
        Left = 236
        Top = 2
        Width = 226
        Height = 407
        Caption = 'Washstation'
        TabOrder = 1
        object cbWashAfterDisp: TDBCheckBox
          Tag = 35180
          Left = 11
          Top = 18
          Width = 180
          Height = 17
          Caption = 'Wash after dispense'
          DataField = 'WashFlag'
          DataSource = DataSource1
          TabOrder = 0
          ValueChecked = '1'
          ValueUnchecked = '0'
          OnClick = cbWashAfterDispClick
        end
        object pnWashAfterDisp: TPanel
          Left = 3
          Top = 126
          Width = 214
          Height = 149
          BevelOuter = bvNone
          TabOrder = 1
          Visible = False
          object Label12: TLabel
            Tag = 35190
            Left = 72
            Top = 8
            Width = 84
            Height = 15
            Caption = '> Volume['#181'l]  >'
          end
          object lblWashCh2: TLabel
            Tag = 35820
            Left = 10
            Top = 55
            Width = 116
            Height = 15
            Caption = 'Wash With Channel 2:'
          end
          object lWashVolFactor: TLabel
            Tag = 35980
            Left = 10
            Top = 34
            Width = 124
            Height = 15
            Caption = 'F (wash volume factor):'
          end
          object EditWashVolMax: TDBEdit
            Left = 10
            Top = 5
            Width = 50
            Height = 23
            DataField = 'WashVolMax'
            DataSource = DataSource1
            TabOrder = 0
          end
          object DBEdit1: TDBEdit
            Left = 158
            Top = 5
            Width = 50
            Height = 23
            DataField = 'WashVolMin'
            DataSource = DataSource1
            TabOrder = 1
          end
          object edWashCh2: TDBEdit
            Left = 158
            Top = 53
            Width = 50
            Height = 23
            DataField = 'WashVolChannel2'
            DataSource = DataSource1
            TabOrder = 2
          end
          object cbDryAfterWash: TDBCheckBox
            Tag = 35940
            Left = 10
            Top = 76
            Width = 183
            Height = 17
            Caption = 'Dry after wash'
            DataField = 'DryAfterWash'
            DataSource = DataSource1
            TabOrder = 3
            ValueChecked = 'Wahr'
            ValueUnchecked = 'Falsch'
          end
          object edWashVolFactor: TDBEdit
            Left = 158
            Top = 30
            Width = 50
            Height = 23
            DataField = 'WashVolFactor'
            DataSource = DataSource1
            TabOrder = 4
          end
          object cbWashUsePeripump: TDBCheckBox
            Tag = 35944
            Left = 10
            Top = 99
            Width = 183
            Height = 17
            Caption = 'Use Peripump'
            DataField = 'WashUsePeripump'
            DataSource = DataSource1
            TabOrder = 5
            ValueChecked = 'Wahr'
            ValueUnchecked = 'Falsch'
          end
        end
        object cbWashIsForced: TDBCheckBox
          Tag = 35830
          Left = 11
          Top = 40
          Width = 155
          Height = 17
          Caption = 'Wash is forced'
          DataField = 'WashIsForced'
          DataSource = DataSource1
          TabOrder = 2
          ValueChecked = 'Wahr'
          ValueUnchecked = 'Falsch'
          Visible = False
          OnClick = cbWashIsForcedClick
        end
        object pnWashMacro: TPanel
          Left = 3
          Top = 126
          Width = 214
          Height = 76
          BevelOuter = bvNone
          TabOrder = 3
          Visible = False
          object lbChooseMacro: TLabel
            Tag = 35845
            Left = 10
            Top = 11
            Width = 110
            Height = 15
            Caption = 'Wash method name:'
          end
          object tdbWashMacroName: TDBComboBox
            Left = 16
            Top = 32
            Width = 177
            Height = 23
            DataField = 'WashMacroName'
            DataSource = DataSource1
            TabOrder = 0
          end
        end
        object rgUseWashMacro: TDBRadioGroup
          Tag = 35850
          Left = 11
          Top = 63
          Width = 185
          Height = 57
          Caption = 'rgUseWashMacro'
          DataField = 'UseWashMacro'
          DataSource = DataSource1
          Items.Strings = (
            'Use default wash'
            'Use wash method')
          ParentBackground = True
          TabOrder = 4
          Values.Strings = (
            '0'
            '1')
          OnClick = rgUseWashMacroClick
        end
      end
      object GroupBox16: TGroupBox
        Tag = 35280
        Left = 4
        Top = 56
        Width = 226
        Height = 134
        Caption = 'Multi Pipetting'
        TabOrder = 2
        object cbMultiPipett: TDBCheckBox
          Tag = 35290
          Left = 11
          Top = 18
          Width = 180
          Height = 16
          Caption = 'Multi Pipett Enable'
          DataField = 'SampleAspMultipip'
          DataSource = DataSource1
          TabOrder = 0
          ValueChecked = '1'
          ValueUnchecked = '0'
          OnClick = cbMultiPipettClick
        end
        object pnMultiPipett: TPanel
          Left = 2
          Top = 35
          Width = 214
          Height = 84
          BevelOuter = bvNone
          TabOrder = 1
          object Label19: TLabel
            Tag = 35300
            Left = 10
            Top = 6
            Width = 95
            Height = 15
            Caption = 'Max. Asp. Volume'
          end
          object Label8: TLabel
            Tag = 35305
            Left = 10
            Top = 34
            Width = 94
            Height = 15
            Caption = 'Min. Asp. Volume'
          end
          object Label43: TLabel
            Tag = 35310
            Left = 10
            Top = 63
            Width = 139
            Height = 15
            Caption = 'Split if volume larger than:'
          end
          object MPwashPanel: TPanel
            Left = 56
            Top = 97
            Width = 153
            Height = 37
            BevelOuter = bvNone
            TabOrder = 2
            Visible = False
            object Label7: TLabel
              Tag = 35320
              Left = 8
              Top = 0
              Width = 38
              Height = 15
              Caption = 'Vol ['#181'l]'
            end
            object Label17: TLabel
              Left = 72
              Top = 0
              Width = 34
              Height = 15
              Caption = 'Cycles'
            end
          end
          object DBEdAspMultiMaxVol: TDBEdit
            Left = 152
            Top = 4
            Width = 50
            Height = 23
            DataField = 'SampleAspMultiMaxVol'
            DataSource = DataSource1
            TabOrder = 0
          end
          object cbMPwash: TDBCheckBox
            Tag = 35315
            Left = 6
            Top = 108
            Width = 51
            Height = 17
            Caption = 'Wash'
            DataField = 'SampleAspMultiWash'
            DataSource = DataSource1
            TabOrder = 1
            ValueChecked = '1'
            ValueUnchecked = '0'
            Visible = False
          end
          object DBEdAspMultiMinVol: TDBEdit
            Left = 152
            Top = 31
            Width = 50
            Height = 23
            DataField = 'SampleAspMultiMinVol'
            DataSource = DataSource1
            TabOrder = 3
          end
          object DBEdAspMultiSplitVol: TDBEdit
            Left = 152
            Top = 58
            Width = 50
            Height = 23
            DataField = 'SampleAspMultiSplitVol'
            DataSource = DataSource1
            TabOrder = 4
          end
        end
      end
      object gbLiqClassWash: TGroupBox
        Tag = 35960
        Left = 478
        Top = 16
        Width = 122
        Height = 89
        Caption = 'gbLiqClassWash'
        TabOrder = 3
        Visible = False
        object lLiqClassWash: TLabel
          Tag = 35970
          Left = 8
          Top = 32
          Width = 75
          Height = 15
          Caption = 'lLiqClassWash'
        end
        object stLiqClassNameWash: TStaticText
          Left = 8
          Top = 48
          Width = 117
          Height = 19
          BorderStyle = sbsSunken
          Caption = 'stLiqClassNameWash'
          TabOrder = 0
        end
      end
      object GroupBox38: TGroupBox
        Tag = 35125
        Left = 3
        Top = 196
        Width = 226
        Height = 77
        Caption = 'Split Aspiration Volume'
        TabOrder = 4
        object Label20: TLabel
          Tag = 35145
          Left = 137
          Top = 42
          Width = 68
          Height = 15
          Caption = '% of max.vol'
        end
        object Label52: TLabel
          Tag = 35135
          Left = 10
          Top = 18
          Width = 93
          Height = 15
          Caption = '.. if 2nd volume <'
        end
        object DBEdit4: TDBEdit
          Left = 131
          Top = 13
          Width = 50
          Height = 23
          DataField = 'AspSplitMinVolPercent'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
    end
    object tbsTipConfig: TTabSheet
      Tag = 35550
      Caption = 'Tip Configuration'
      ImageIndex = 5
      object grbTips: TGroupBox
        Tag = 35550
        Left = 8
        Top = 5
        Width = 233
        Height = 372
        Caption = 'Tip Configuration'
        TabOrder = 0
        object Label1: TLabel
          Tag = 35560
          Left = 11
          Top = 22
          Width = 49
          Height = 15
          Caption = 'Tip Type:'
        end
        object Label29: TLabel
          Left = 11
          Top = 46
          Width = 58
          Height = 15
          Caption = 'Pip Device:'
        end
        object Label56: TLabel
          Tag = 36170
          Left = 11
          Top = 341
          Width = 60
          Height = 15
          Caption = 'Pumpndex:'
        end
        object edUsedTips: TDBEdit
          Left = 201
          Top = 273
          Width = 24
          Height = 23
          DataField = 'UsedTips'
          DataSource = DataSource1
          TabOrder = 0
          Visible = False
          OnChange = edUsedTipsChange
        end
        object rbAllTips: TRadioButton
          Tag = 35260
          Left = 11
          Top = 67
          Width = 217
          Height = 17
          Caption = 'All Tips'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnMouseUp = rbAllTipsMouseUp
        end
        object rbChooseTips: TRadioButton
          Tag = 35270
          Left = 11
          Top = 86
          Width = 217
          Height = 17
          Caption = 'Choose Tips'
          TabOrder = 2
          OnMouseUp = rbChooseTipsMouseUp
        end
        object cbUsedTipType: TDBComboBox
          Left = 72
          Top = 18
          Width = 153
          Height = 23
          DataField = 'UsedTipType'
          DataSource = DataSource1
          Sorted = True
          TabOrder = 3
          OnChange = cbUsedTipTypeChange
        end
        object cbUseDipTipsWithoutTips: TDBCheckBox
          Tag = 35760
          Left = 11
          Top = 310
          Width = 213
          Height = 17
          Caption = 'Use Dip Tips Without Tips'
          DataField = 'UseDispTipWithoutTip'
          DataSource = DataSource1
          TabOrder = 4
          ValueChecked = '1'
          ValueUnchecked = '0'
        end
        object cbUsedPipDevice: TDBComboBox
          Left = 72
          Top = 42
          Width = 153
          Height = 23
          DataField = 'UsedPipDevice'
          DataSource = DataSource1
          Sorted = True
          TabOrder = 5
          OnChange = cbUsedPipDeviceChange
        end
        object DBEdit11: TDBEdit
          Left = 128
          Top = 337
          Width = 57
          Height = 23
          DataField = 'Ch1PumpNumber'
          DataSource = DataSource1
          TabOrder = 6
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 474
    Width = 660
    Height = 30
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvNone
    TabOrder = 1
    object Label14: TLabel
      Tag = 35070
      Left = 8
      Top = 5
      Width = 73
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Description:'
    end
    object Label11: TLabel
      Tag = 35970
      Left = 328
      Top = 5
      Width = 35
      Height = 15
      Caption = 'Klasse:'
    end
    object edDescription: TDBEdit
      Left = 88
      Top = 2
      Width = 233
      Height = 23
      DataField = 'Description'
      DataSource = DataSource1
      TabOrder = 0
    end
    object lbLiqClassName: TDBComboBox
      Left = 367
      Top = 2
      Width = 115
      Height = 23
      DataField = 'LIQCLASS'
      DataSource = DataSource1
      TabOrder = 1
      OnChange = lbLiqClassNameChange
    end
  end
  object DataSource1: TDataSource
    Left = 423
    Top = 29
  end
end
