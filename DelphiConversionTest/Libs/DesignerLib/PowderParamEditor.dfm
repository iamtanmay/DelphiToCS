object frmPowderParEditor: TfrmPowderParEditor
  Tag = 35730
  Left = 395
  Top = 184
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Powder Handling Settings'
  ClientHeight = 477
  ClientWidth = 658
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
    Width = 658
    Height = 452
    ActivePage = tbsSampleAsp
    Align = alClient
    TabOrder = 0
    object tbsSampleAsp: TTabSheet
      Tag = 36030
      Caption = 'Aspirate Sample'
      object rgrAspSwitchPos: TDBRadioGroup
        Tag = 35650
        Left = 5
        Top = 224
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
        TabOrder = 0
        Values.Strings = (
          '1'
          '2'
          '0')
        OnChange = rgrAspSwitchPosChange
      end
      object GroupBox2: TGroupBox
        Tag = 36035
        Left = 5
        Top = 5
        Width = 204
        Height = 84
        Caption = 'Aspiration'
        TabOrder = 1
        object Label2: TLabel
          Tag = 36040
          Left = 8
          Top = 24
          Width = 121
          Height = 15
          Caption = 'Number of aspirations:'
        end
        object Label3: TLabel
          Tag = 36100
          Left = 8
          Top = 56
          Width = 89
          Height = 15
          Caption = 'Shake Time [sec]'
        end
        object edSampleAspSpitBackCount: TDBEdit
          Left = 152
          Top = 23
          Width = 41
          Height = 23
          DataField = 'SampleAspSpitBackCount'
          DataSource = DataSource1
          TabOrder = 0
        end
        object edSysAirAspDelay: TDBEdit
          Left = 152
          Top = 55
          Width = 41
          Height = 23
          DataField = 'SysAirAspDelay'
          DataSource = DataSource1
          TabOrder = 1
        end
      end
      object GroupBox19: TGroupBox
        Tag = 35670
        Left = 227
        Top = 224
        Width = 246
        Height = 81
        Caption = 'Movement during aspiration'
        TabOrder = 2
        Visible = False
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
        Top = 96
        Width = 420
        Height = 121
        Caption = 'Movement into well'
        TabOrder = 3
        object Label5: TLabel
          Tag = 35420
          Left = 8
          Top = 46
          Width = 84
          Height = 15
          Caption = 'Submerge[mm]'
        end
        object lblSampleAspScanSpeed: TLabel
          Tag = 35895
          Left = 8
          Top = 20
          Width = 67
          Height = 15
          Caption = 'Insert Speed:'
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
          Top = 92
          Width = 153
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
          Top = 70
          Width = 209
          Height = 17
          Caption = 'Do not use level calculation'
          TabOrder = 3
          OnMouseUp = boxSampleAspLiqDetMouseUp
        end
        object DBCheckBox1: TDBCheckBox
          Tag = 36045
          Left = 188
          Top = 12
          Width = 185
          Height = 17
          Caption = 'Use XY shifting:'
          DataField = 'SampleAspXYShifting'
          DataSource = DataSource1
          TabOrder = 4
          ValueChecked = 'True'
          ValueUnchecked = 'False'
        end
        object boxDispLiqDetSingle: TCheckBox
          Tag = 35340
          Left = 188
          Top = 32
          Width = 121
          Height = 17
          Caption = 'Single Tip'
          TabOrder = 5
          OnMouseUp = boxDispLevTrMouseUp
        end
        object DBRadioGroup1: TDBRadioGroup
          Tag = 36110
          Left = 188
          Top = 54
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
          TabOrder = 6
          Values.Strings = (
            '0'
            '1'
            '2'
            '3')
        end
      end
      object cbSAspSwitchModule: TDBComboBox
        Left = 104
        Top = 238
        Width = 113
        Height = 23
        DataField = 'AspSwitchModule'
        DataSource = DataSource1
        TabOrder = 4
      end
      object GroupBox13: TGroupBox
        Tag = 35910
        Left = 5
        Top = 312
        Width = 187
        Height = 89
        Caption = 'Retract after aspiration'
        TabOrder = 5
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
      object edSampleAspLiqDet: TDBEdit
        Left = 358
        Top = 105
        Width = 24
        Height = 23
        DataField = 'SampleAspLiqDet'
        DataSource = DataSource1
        TabOrder = 6
        Visible = False
        OnChange = edSampleAspLiqDetChange
      end
      object gbDelay: TGroupBox
        Tag = 35640
        Left = 224
        Top = 5
        Width = 201
        Height = 84
        Caption = ' Delay [sec] '
        TabOrder = 7
        object Label7: TLabel
          Tag = 36050
          Left = 8
          Top = 24
          Width = 108
          Height = 15
          Caption = 'Between aspirations:'
        end
        object Label8: TLabel
          Tag = 36055
          Left = 8
          Top = 56
          Width = 56
          Height = 15
          Caption = 'Last Delay:'
        end
        object edSpitBackDelay: TDBEdit
          Left = 144
          Top = 20
          Width = 45
          Height = 23
          DataField = 'SampleAspCh2WashDelay'
          DataSource = DataSource1
          TabOrder = 0
        end
        object EditSampleAspDelay: TDBEdit
          Left = 144
          Top = 51
          Width = 45
          Height = 23
          DataField = 'SampleAspDelay'
          DataSource = DataSource1
          TabOrder = 1
        end
      end
    end
    object tbsDispense: TTabSheet
      Tag = 35130
      Caption = 'Dispense'
      object GroupBox7: TGroupBox
        Tag = 35640
        Left = 236
        Top = 5
        Width = 165
        Height = 84
        Caption = ' Delay [sec] '
        TabOrder = 0
        object EditDispDelay: TDBEdit
          Left = 27
          Top = 27
          Width = 50
          Height = 23
          DataField = 'DispDelay'
          DataSource = DataSource1
          TabOrder = 0
        end
      end
      object rgrDspSwitchPos: TDBRadioGroup
        Tag = 35650
        Left = 5
        Top = 224
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
        TabOrder = 1
        Values.Strings = (
          '1'
          '2'
          '0')
        OnChange = rgrDspSwitchPosChange
      end
      object GroupBox17: TGroupBox
        Tag = 35660
        Left = 238
        Top = 224
        Width = 163
        Height = 81
        Caption = 'Step - Performing'
        TabOrder = 2
        Visible = False
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
          Left = 104
          Top = 23
          Width = 50
          Height = 23
          DataField = 'DispStepVolume'
          DataSource = DataSource1
          MaxLength = 6
          TabOrder = 0
        end
        object edDispStepDelay: TDBEdit
          Left = 104
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
        Tag = 35920
        Left = 5
        Top = 4
        Width = 214
        Height = 85
        TabOrder = 3
        object boxDispLevTr: TCheckBox
          Tag = 35360
          Left = 8
          Top = 54
          Width = 97
          Height = 17
          Caption = 'Level Tracking'
          TabOrder = 0
          Visible = False
          OnMouseUp = boxDispLevTrMouseUp
        end
        object cbEmptyVarRedi: TDBCheckBox
          Tag = 34990
          Left = 8
          Top = 24
          Width = 201
          Height = 17
          Caption = 'Empty variable powder pipette'
          DataField = 'DispEmptyVarRedi'
          DataSource = DataSource1
          TabOrder = 1
          ValueChecked = '1'
          ValueUnchecked = '0'
        end
      end
      object GroupBox14: TGroupBox
        Tag = 35150
        Left = 5
        Top = 96
        Width = 396
        Height = 121
        Caption = 'Dispense'
        TabOrder = 4
        object lblDispScanSpeed: TLabel
          Tag = 35895
          Left = 8
          Top = 20
          Width = 67
          Height = 15
          Caption = 'Insert Speed:'
        end
        object Label34: TLabel
          Tag = 35420
          Left = 8
          Top = 46
          Width = 84
          Height = 15
          Caption = 'Submerge[mm]'
        end
        object edDispSubmerge: TDBEdit
          Left = 100
          Top = 44
          Width = 50
          Height = 23
          DataField = 'DispSubmerge'
          DataSource = DataSource1
          TabOrder = 0
        end
        object edDispScanSpeed: TDBEdit
          Left = 100
          Top = 18
          Width = 50
          Height = 23
          DataField = 'DispScanSpeed'
          DataSource = DataSource1
          TabOrder = 1
        end
        object cbDispNoCalc: TCheckBox
          Tag = 35930
          Left = 8
          Top = 70
          Width = 153
          Height = 17
          Caption = 'Do not use level calculation'
          TabOrder = 2
          OnMouseUp = boxDispLiqDetMouseUp
        end
        object EditDispErrFlag: TDBCheckBox
          Tag = 35250
          Left = 8
          Top = 92
          Width = 161
          Height = 17
          Caption = 'Disable Z Error at Dispense '
          DataField = 'DispErrFlag'
          DataSource = DataSource1
          TabOrder = 3
          ValueChecked = '1'
          ValueUnchecked = '0'
        end
        object DBCheckBox2: TDBCheckBox
          Tag = 36045
          Left = 188
          Top = 12
          Width = 185
          Height = 17
          Caption = 'Use XY shifting:'
          DataField = 'DispXYShifting'
          DataSource = DataSource1
          TabOrder = 4
          ValueChecked = 'True'
          ValueUnchecked = 'False'
        end
        object boxSampleAspLiqDetSingle: TCheckBox
          Tag = 35340
          Left = 188
          Top = 32
          Width = 125
          Height = 17
          Caption = 'Single Tip'
          TabOrder = 5
          OnMouseUp = boxSampleAspLevTrMouseUp
        end
        object DBRadioGroup2: TDBRadioGroup
          Tag = 36110
          Left = 188
          Top = 54
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
          TabOrder = 6
          Values.Strings = (
            '0'
            '1'
            '2'
            '3')
        end
      end
      object cbDispSwitchModule: TDBComboBox
        Left = 104
        Top = 238
        Width = 113
        Height = 23
        DataField = 'DspSwitchModule'
        DataSource = DataSource1
        TabOrder = 5
      end
      object GroupBox34: TGroupBox
        Tag = 35910
        Left = 5
        Top = 312
        Width = 188
        Height = 89
        Caption = 'Retract after aspiration'
        TabOrder = 6
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
      object edDispLiqDet: TDBEdit
        Left = 366
        Top = 105
        Width = 24
        Height = 23
        DataField = 'DispLiqDet'
        DataSource = DataSource1
        TabOrder = 7
        Visible = False
        OnChange = edDispLiqDetChange
      end
    end
    object tbsOptions: TTabSheet
      Tag = 35160
      Caption = 'Options'
      ImageIndex = 6
      object gbWash: TGroupBox
        Tag = 35170
        Left = 236
        Top = 2
        Width = 226
        Height = 239
        Caption = 'Washstation'
        TabOrder = 0
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
          Left = 4
          Top = 158
          Width = 214
          Height = 67
          BevelOuter = bvNone
          TabOrder = 1
          Visible = False
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
          Left = 2
          Top = 158
          Width = 214
          Height = 59
          BevelOuter = bvNone
          TabOrder = 3
          Visible = False
          object lbChooseMacro: TLabel
            Tag = 35845
            Left = 10
            Top = 8
            Width = 110
            Height = 15
            Caption = 'Wash method name:'
          end
          object tdbWashMacroName: TDBComboBox
            Left = 16
            Top = 27
            Width = 177
            Height = 23
            DataField = 'WashMacroName'
            DataSource = DataSource1
            TabOrder = 0
          end
        end
        object rgUseWashMacro: TDBRadioGroup
          Tag = 35850
          Left = 24
          Top = 64
          Width = 185
          Height = 73
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
      object GroupBox38: TGroupBox
        Tag = 35125
        Left = 4
        Top = 4
        Width = 226
        Height = 69
        Caption = 'Split Aspiration Volume'
        TabOrder = 1
        object Label20: TLabel
          Tag = 35145
          Left = 128
          Top = 43
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
          Left = 128
          Top = 14
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
        Height = 340
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
        object Label9: TLabel
          Left = 11
          Top = 46
          Width = 58
          Height = 15
          Caption = 'Pip Device:'
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
        object cbUsedPipDevice: TDBComboBox
          Left = 72
          Top = 42
          Width = 153
          Height = 23
          DataField = 'UsedPipDevice'
          DataSource = DataSource1
          Sorted = True
          TabOrder = 4
          OnChange = cbUsedPipDeviceChange
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 452
    Width = 658
    Height = 25
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
    object edDescription: TDBEdit
      Left = 88
      Top = 2
      Width = 409
      Height = 23
      DataField = 'Description'
      DataSource = DataSource1
      TabOrder = 0
    end
  end
  object DataSource1: TDataSource
    OnDataChange = DataSource1DataChange
    OnUpdateData = DataSource1UpdateData
    Left = 487
    Top = 37
  end
end
