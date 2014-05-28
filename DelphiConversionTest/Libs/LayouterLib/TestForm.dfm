object frmTest: TfrmTest
  Tag = 34000
  Left = 230
  Top = 202
  Caption = 'Test Modules'
  ClientHeight = 435
  ClientWidth = 795
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter2: TSplitter
    Left = 217
    Top = 21
    Height = 414
    ExplicitHeight = 275
  end
  object pnlTopLeft: TPanel
    Left = 220
    Top = 21
    Width = 575
    Height = 414
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlDeviceTabs: TPanel
      Left = 0
      Top = 0
      Width = 575
      Height = 414
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object pgctlDevices: TPageControl
        Left = 0
        Top = 0
        Width = 281
        Height = 414
        ActivePage = shtStateSignal
        Align = alLeft
        MultiLine = True
        TabOrder = 0
        object shtBalance: TTabSheet
          Caption = 'Balance'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbBalance: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 177
            Caption = 'Balance'
            TabOrder = 0
            object Label1: TLabel
              Left = 16
              Top = 104
              Width = 48
              Height = 15
              Caption = '-------->'
            end
            object Label2: TLabel
              Left = 56
              Top = 104
              Width = 39
              Height = 15
              Caption = 'Rack ID'
            end
            object Label3: TLabel
              Left = 56
              Top = 128
              Width = 43
              Height = 15
              Caption = 'Position'
            end
            object Label5: TLabel
              Tag = 34180
              Left = 56
              Top = 152
              Width = 68
              Height = 15
              Caption = 'Substance ID'
            end
            object btnBalInit: TButton
              Left = 16
              Top = 24
              Width = 100
              Height = 25
              Caption = 'Init'
              TabOrder = 0
              OnClick = btnBalInitClick
            end
            object btnOpenDoor: TButton
              Tag = 34160
              Left = 16
              Top = 56
              Width = 100
              Height = 25
              Caption = 'Open Door'
              TabOrder = 1
              OnClick = btnOpenDoorClick
            end
            object btnBalTara: TButton
              Left = 136
              Top = 24
              Width = 100
              Height = 25
              Caption = 'Tara'
              TabOrder = 2
              OnClick = btnBalTaraClick
            end
            object btnStoreWeight: TButton
              Tag = 34170
              Left = 136
              Top = 56
              Width = 100
              Height = 25
              Caption = 'Store Weight'
              TabOrder = 3
              OnClick = btnStoreWeightClick
            end
            object Edit1: TEdit
              Left = 136
              Top = 96
              Width = 105
              Height = 23
              TabOrder = 4
              Text = '12345'
            end
            object Edit2: TEdit
              Left = 136
              Top = 120
              Width = 105
              Height = 23
              TabOrder = 5
              Text = '1'
            end
            object Edit3: TEdit
              Left = 136
              Top = 144
              Width = 105
              Height = 23
              TabOrder = 6
              Text = '444555'
            end
          end
        end
        object shtSwitch: TTabSheet
          Caption = 'Switch'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object rgrSwitch: TRadioGroup
            Left = 10
            Top = 36
            Width = 254
            Height = 65
            Caption = 'Switch'
            Columns = 2
            Items.Strings = (
              'Off'
              'Default'
              'On'
              'Both On')
            TabOrder = 0
            OnClick = rgrSwitchClick
          end
          object btnSwitchInitRelay: TButton
            Left = 10
            Top = 6
            Width = 75
            Height = 25
            Caption = 'Init Relay'
            TabOrder = 1
            OnClick = btnSwitchInitRelayClick
          end
        end
        object shtShaker: TTabSheet
          Caption = 'Shaker'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbCATShaker: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 249
            Caption = 'Shaker'
            TabOrder = 0
            object Label7: TLabel
              Tag = 34110
              Left = 16
              Top = 73
              Width = 85
              Height = 13
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Speed'
            end
            object Label8: TLabel
              Tag = 34120
              Left = 16
              Top = 97
              Width = 85
              Height = 13
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'On Pulse [sec]'
            end
            object Label9: TLabel
              Tag = 34130
              Left = 16
              Top = 121
              Width = 85
              Height = 13
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Off Pulse [sec]'
            end
            object rgFixation: TRadioGroup
              Tag = 34150
              Left = 16
              Top = 197
              Width = 225
              Height = 41
              Caption = 'Fixation'
              Columns = 2
              Items.Strings = (
                'Off'
                'On')
              TabOrder = 0
              OnClick = rgFixationClick
            end
            object edSpeed: TEdit
              Left = 113
              Top = 69
              Width = 105
              Height = 23
              TabOrder = 1
              Text = '0'
            end
            object edOnPulse: TEdit
              Left = 113
              Top = 93
              Width = 105
              Height = 23
              TabOrder = 2
              Text = '0'
            end
            object edOffPulse: TEdit
              Left = 113
              Top = 117
              Width = 105
              Height = 23
              TabOrder = 3
              Text = '0'
            end
            object rgShaker: TRadioGroup
              Tag = 34140
              Left = 16
              Top = 149
              Width = 225
              Height = 41
              Caption = 'Shaker'
              Columns = 2
              Items.Strings = (
                'Off'
                'On')
              TabOrder = 4
              OnClick = rgShakerClick
            end
            object btnShakerInit: TButton
              Left = 16
              Top = 19
              Width = 100
              Height = 25
              Caption = 'Init'
              TabOrder = 5
              OnClick = btnShakerInitClick
            end
          end
        end
        object shtThermostat: TTabSheet
          Caption = 'Thermostat'
          ImageIndex = 3
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbThermostat: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 201
            Caption = 'Thermostat'
            TabOrder = 0
            object Label4: TLabel
              Tag = 34080
              Left = 16
              Top = 59
              Width = 68
              Height = 15
              Caption = 'Temperature'
            end
            object Label6: TLabel
              Left = 160
              Top = 51
              Width = 3
              Height = 15
              Caption = '.'
            end
            object Label14: TLabel
              Left = 16
              Top = 144
              Width = 77
              Height = 15
              Caption = 'Current Temp.'
            end
            object Label15: TLabel
              Left = 16
              Top = 168
              Width = 71
              Height = 15
              Caption = 'Target Temp.'
            end
            object SpinEdit2: TSpinEdit
              Left = 88
              Top = 51
              Width = 65
              Height = 24
              MaxValue = 2000
              MinValue = -45
              TabOrder = 0
              Value = 20
            end
            object Button8: TButton
              Tag = 34090
              Left = 18
              Top = 83
              Width = 100
              Height = 25
              Caption = 'Get Actual Temp.'
              TabOrder = 1
              OnClick = Button8Click
            end
            object Button7: TButton
              Tag = 34100
              Left = 136
              Top = 83
              Width = 100
              Height = 25
              Caption = 'Set Target Temp.'
              TabOrder = 2
              OnClick = Button7Click
            end
            object SpinEdit1: TSpinEdit
              Left = 176
              Top = 51
              Width = 33
              Height = 24
              MaxValue = 9
              MinValue = 0
              TabOrder = 3
              Value = 0
            end
            object btnThermoInit: TButton
              Left = 16
              Top = 19
              Width = 100
              Height = 25
              Caption = 'Init'
              TabOrder = 4
              OnClick = btnThermoInitClick
            end
            object stCurrentTemp: TStaticText
              Left = 96
              Top = 144
              Width = 57
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              BorderStyle = sbsSunken
              TabOrder = 5
            end
            object Button13: TButton
              Tag = 34200
              Left = 136
              Top = 115
              Width = 100
              Height = 25
              Caption = 'Get Target Temp.'
              TabOrder = 6
              OnClick = Button13Click
            end
            object stTargetTemp: TStaticText
              Left = 96
              Top = 168
              Width = 57
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              BorderStyle = sbsSunken
              TabOrder = 7
            end
          end
        end
        object shtREDI: TTabSheet
          Caption = 'REDI'
          ImageIndex = 4
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbRediMotor: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 129
            Caption = 'REDI-Motor'
            TabOrder = 0
            object Label11: TLabel
              Tag = 34060
              Left = 8
              Top = 90
              Width = 44
              Height = 15
              Caption = 'Volume:'
            end
            object Label12: TLabel
              Tag = 34040
              Left = 8
              Top = 28
              Width = 74
              Height = 13
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Min. Volume:'
            end
            object Label13: TLabel
              Tag = 34050
              Left = 8
              Top = 52
              Width = 74
              Height = 13
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Max. Volume:'
            end
            object Button1: TButton
              Left = 168
              Top = 48
              Width = 68
              Height = 25
              Caption = 'Init'
              TabOrder = 0
              OnClick = Button1Click
            end
            object Button2: TButton
              Tag = 34070
              Left = 132
              Top = 88
              Width = 113
              Height = 25
              Caption = 'Set Volume'
              TabOrder = 1
              OnClick = Button2Click
            end
            object spinVol: TSpinEdit
              Left = 56
              Top = 88
              Width = 65
              Height = 24
              MaxValue = 1000
              MinValue = 0
              TabOrder = 2
              Value = 125
            end
            object spinMinVol: TSpinEdit
              Left = 88
              Top = 24
              Width = 65
              Height = 24
              MaxValue = 1000
              MinValue = 0
              TabOrder = 3
              Value = 0
            end
            object spinMaxVol: TSpinEdit
              Left = 88
              Top = 48
              Width = 65
              Height = 24
              MaxValue = 1000
              MinValue = 0
              TabOrder = 4
              Value = 250
            end
          end
        end
        object shtDecapper: TTabSheet
          Caption = 'Decapper'
          ImageIndex = 5
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbDecapper: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 153
            Caption = 'Decapper'
            TabOrder = 0
            object Button3: TButton
              Left = 16
              Top = 56
              Width = 100
              Height = 25
              Caption = 'Decap'
              TabOrder = 0
              OnClick = Button3Click
            end
            object Button4: TButton
              Left = 136
              Top = 56
              Width = 100
              Height = 25
              Caption = 'Read BC'
              TabOrder = 1
              OnClick = Button4Click
            end
            object Button5: TButton
              Left = 136
              Top = 24
              Width = 100
              Height = 25
              Caption = 'Cap'
              TabOrder = 2
              OnClick = Button5Click
            end
            object Button6: TButton
              Left = 136
              Top = 88
              Width = 100
              Height = 25
              Caption = 'Decap And Read'
              TabOrder = 3
              OnClick = Button6Click
            end
            object edTubeBC: TEdit
              Left = 136
              Top = 124
              Width = 105
              Height = 23
              ReadOnly = True
              TabOrder = 4
            end
            object Button10: TButton
              Left = 16
              Top = 24
              Width = 100
              Height = 25
              Caption = 'Go Back'
              TabOrder = 5
              OnClick = Button10Click
            end
            object Button11: TButton
              Left = 16
              Top = 88
              Width = 105
              Height = 25
              Caption = 'Decap and cap (x)'
              TabOrder = 6
              OnClick = Button11Click
            end
            object Edit4: TEdit
              Left = 80
              Top = 120
              Width = 41
              Height = 23
              TabOrder = 7
              Text = '1'
            end
            object Button9: TButton
              Left = 16
              Top = 120
              Width = 57
              Height = 25
              Caption = 'w. Reading'
              TabOrder = 8
              OnClick = Button9Click
            end
          end
        end
        object shtBCReader: TTabSheet
          Caption = 'BCReader'
          ImageIndex = 6
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbBCReader: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 137
            Caption = 'BC Reader'
            TabOrder = 0
            object Button12: TButton
              Left = 16
              Top = 56
              Width = 100
              Height = 25
              Caption = 'Long Read'
              TabOrder = 0
              OnClick = Button12Click
            end
            object Button14: TButton
              Left = 128
              Top = 56
              Width = 100
              Height = 25
              Caption = 'Reset (Last Read)'
              TabOrder = 1
              OnClick = Button14Click
            end
            object edBarcode: TEdit
              Left = 128
              Top = 92
              Width = 105
              Height = 23
              ReadOnly = True
              TabOrder = 2
            end
            object btnShortRead: TButton
              Left = 16
              Top = 88
              Width = 100
              Height = 25
              Caption = 'Short Read'
              TabOrder = 3
              OnClick = btnShortReadClick
            end
            object btnBCReaderInitAll: TButton
              Left = 85
              Top = 24
              Width = 143
              Height = 25
              Caption = 'Init all barcode readers'
              TabOrder = 4
            end
            object btnBCReaderInit: TButton
              Left = 16
              Top = 24
              Width = 57
              Height = 25
              Caption = 'Init'
              TabOrder = 5
              OnClick = btnBCReaderInitClick
            end
          end
        end
        object shtSensor: TTabSheet
          Caption = 'Sensor'
          ImageIndex = 7
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbSensor: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 57
            Caption = 'Sensor'
            TabOrder = 0
            object Button15: TButton
              Left = 24
              Top = 24
              Width = 75
              Height = 25
              Caption = 'Ask Sensor'
              TabOrder = 0
              OnClick = Button15Click
            end
            object stSensor: TStaticText
              Left = 120
              Top = 28
              Width = 89
              Height = 17
              AutoSize = False
              BorderStyle = sbsSingle
              Caption = 'stSensor'
              TabOrder = 1
            end
          end
        end
        object shtXWayValve: TTabSheet
          Caption = 'XWayValve'
          ImageIndex = 8
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbXWayValve: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 225
            Caption = 'X-Way-Valve'
            TabOrder = 0
            object btnXWayReadPos: TButton
              Left = 152
              Top = 16
              Width = 83
              Height = 25
              Caption = 'Read Position'
              TabOrder = 0
              OnClick = btnXWayReadPosClick
            end
            object rgInPorts: TRadioGroup
              Left = 16
              Top = 48
              Width = 225
              Height = 161
              Caption = 'Input ports'
              TabOrder = 1
              OnClick = rgInPortsClick
            end
          end
        end
        object shtPipPump: TTabSheet
          Caption = 'PipPump'
          ImageIndex = 9
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbDilutor: TGroupBox
            Left = 3
            Top = 3
            Width = 254
            Height = 222
            Caption = 'Pip Pump'
            TabOrder = 0
            object Label10: TLabel
              Left = 16
              Top = 56
              Width = 38
              Height = 15
              Caption = 'In Port:'
            end
            object lblUnitAsp: TLabel
              Left = 170
              Top = 88
              Width = 10
              Height = 15
              Caption = 'ul'
            end
            object lblUnitDsp: TLabel
              Left = 170
              Top = 120
              Width = 10
              Height = 15
              Caption = 'ul'
            end
            object stDilutorIn: TStaticText
              Left = 56
              Top = 56
              Width = 13
              Height = 19
              BorderStyle = sbsSunken
              Caption = '...'
              TabOrder = 0
            end
            object btnInitPipPump: TButton
              Left = 16
              Top = 24
              Width = 57
              Height = 25
              Caption = 'Init'
              TabOrder = 1
              OnClick = btnInitPipPumpClick
            end
            object btnPipPumpAsp: TButton
              Left = 17
              Top = 81
              Width = 75
              Height = 25
              Caption = 'Asp'
              TabOrder = 2
              OnClick = btnPipPumpAspClick
            end
            object edPipPumpAspVol: TEdit
              Left = 104
              Top = 82
              Width = 60
              Height = 23
              TabOrder = 3
              Text = '0'
            end
            object edPipPumpDspVol: TEdit
              Left = 104
              Top = 113
              Width = 60
              Height = 23
              TabOrder = 4
              Text = '0'
            end
            object btnPipPumpDsp: TButton
              Left = 17
              Top = 111
              Width = 75
              Height = 25
              Caption = 'Dsp'
              TabOrder = 5
              OnClick = btnPipPumpDspClick
            end
            object btnPipPumpTurnvalve: TButton
              Left = 17
              Top = 142
              Width = 75
              Height = 25
              Caption = 'Turn Valve'
              TabOrder = 6
              OnClick = btnPipPumpTurnvalveClick
            end
            object rbSyrTip: TRadioButton
              Left = 112
              Top = 142
              Width = 113
              Height = 17
              Caption = 'Syringe - Tip'
              TabOrder = 7
            end
            object rbSyrSys: TRadioButton
              Left = 112
              Top = 165
              Width = 113
              Height = 17
              Caption = 'Syringe - System'
              TabOrder = 8
            end
          end
        end
        object shtStateSignal: TTabSheet
          Caption = 'StateSignal'
          ImageIndex = 10
          object rdoStateSignal: TRadioGroup
            Left = 10
            Top = 8
            Width = 249
            Height = 145
            Caption = 'State'
            Items.Strings = (
              'Active'
              'Ready'
              'Error'
              'Message'
              'Test')
            TabOrder = 0
            OnClick = rdoStateSignalClick
          end
          object edsStateSignalActiveSwitch: TStaticText
            Left = 88
            Top = 26
            Width = 163
            Height = 19
            Caption = '.....................................................'
            TabOrder = 1
          end
          object edsStateSignalReadySwitch: TStaticText
            Left = 88
            Top = 53
            Width = 163
            Height = 19
            Caption = '.....................................................'
            TabOrder = 2
          end
          object edsStateSignalErrorSwitch: TStaticText
            Left = 88
            Top = 80
            Width = 163
            Height = 19
            Caption = '.....................................................'
            TabOrder = 3
          end
          object edsStateSignalMessageSwitch: TStaticText
            Left = 88
            Top = 105
            Width = 163
            Height = 19
            Caption = '.....................................................'
            TabOrder = 4
          end
          object edsStateSignalTestSwitch: TStaticText
            Left = 88
            Top = 130
            Width = 163
            Height = 19
            Caption = '.....................................................'
            TabOrder = 5
          end
        end
        object shtPHMeter: TTabSheet
          Caption = 'PHMeter'
          ImageIndex = 11
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbPHMeter: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 57
            Caption = 'PHMeter'
            TabOrder = 0
            object btnAskPH: TButton
              Left = 24
              Top = 24
              Width = 75
              Height = 25
              Caption = 'Ask PH'
              TabOrder = 0
              OnClick = btnAskPHClick
            end
            object stPHMeter: TStaticText
              Left = 120
              Top = 28
              Width = 89
              Height = 17
              AutoSize = False
              BorderStyle = sbsSingle
              TabOrder = 1
            end
          end
        end
        object shtThermometer: TTabSheet
          Caption = 'Thermometer'
          ImageIndex = 12
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbThermometer: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 57
            Caption = 'Thermometer'
            TabOrder = 0
            object btnAskTemp: TButton
              Left = 24
              Top = 24
              Width = 75
              Height = 25
              Caption = 'Ask Temp'
              TabOrder = 0
              OnClick = btnAskTempClick
            end
            object stThermometer: TStaticText
              Left = 120
              Top = 28
              Width = 89
              Height = 17
              AutoSize = False
              BorderStyle = sbsSingle
              TabOrder = 1
            end
          end
        end
        object shtMemory: TTabSheet
          Caption = 'Memory'
          ImageIndex = 13
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbMemory: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 105
            Caption = 'Memory Device'
            TabOrder = 0
            object btnWriteValue: TButton
              Left = 24
              Top = 24
              Width = 75
              Height = 25
              Caption = 'Write Value'
              TabOrder = 0
              OnClick = btnWriteValueClick
            end
            object stGetValue: TStaticText
              Left = 120
              Top = 68
              Width = 121
              Height = 17
              AutoSize = False
              BorderStyle = sbsSingle
              TabOrder = 1
            end
            object btnReadValue: TButton
              Left = 24
              Top = 64
              Width = 75
              Height = 25
              Caption = 'Read Value'
              TabOrder = 2
              OnClick = btnReadValueClick
            end
            object edSetValue: TEdit
              Left = 120
              Top = 26
              Width = 121
              Height = 23
              TabOrder = 3
            end
          end
        end
        object shtTrigger: TTabSheet
          Caption = 'Trigger'
          ImageIndex = 14
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grbTrigger: TGroupBox
            Left = 10
            Top = 8
            Width = 254
            Height = 65
            Caption = 'Trigger'
            TabOrder = 0
            object btnTrigger: TButton
              Left = 88
              Top = 24
              Width = 145
              Height = 25
              Caption = 'StartTrigger'
              TabOrder = 0
              OnClick = btnTriggerClick
            end
          end
        end
        object shtLocationBasedMotionDevice: TTabSheet
          Caption = 'MotionDevice'
          ImageIndex = 15
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Button16: TButton
            Left = 40
            Top = 24
            Width = 129
            Height = 41
            Caption = 'Show Teach Program'
            TabOrder = 0
            OnClick = Button16Click
          end
        end
      end
      object cxTreeList1: TcxTreeList
        Left = 281
        Top = 0
        Width = 294
        Height = 414
        Align = alClient
        Bands = <
          item
          end>
        LookAndFeel.NativeStyle = True
        OptionsBehavior.MultiSort = False
        OptionsBehavior.Sorting = False
        OptionsData.Editing = False
        OptionsData.Deleting = False
        TabOrder = 1
        object cxTreeList1cxTreeListColumn1: TcxTreeListColumn
          Caption.Text = 'Key'
          DataBinding.ValueType = 'String'
          Width = 158
          Position.ColIndex = 0
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object cxTreeList1cxTreeListColumn2: TcxTreeListColumn
          Caption.Text = 'Value'
          DataBinding.ValueType = 'String'
          Width = 305
          Position.ColIndex = 1
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
      end
    end
  end
  object tv: TTreeView
    Left = 0
    Top = 21
    Width = 217
    Height = 414
    Align = alLeft
    Images = ImageList1
    Indent = 19
    TabOrder = 1
    OnClick = tvClick
    OnGetSelectedIndex = tvGetSelectedIndex
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 795
    Height = 21
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object rbLayoutDevices: TRadioButton
      Left = 112
      Top = 3
      Width = 113
      Height = 17
      Caption = 'Layout Devices'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbLayoutDevicesClick
    end
    object rbAllDevices: TRadioButton
      Left = 8
      Top = 3
      Width = 100
      Height = 17
      Caption = 'All Devices'
      TabOrder = 1
      OnClick = rbAllDevicesClick
    end
  end
  object ImageList1: TImageList
    Left = 192
    Top = 272
    Bitmap = {
      494C010102000400240010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      00000000000000000000000000000000000000000000C6C6C60000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C60000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C6009CCECE009CFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C6000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000C6C6C600C6C6C6005252520052525200B5B5B500CEFF
      9C009CCECE009CFFFF009CFFCE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C60000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000C6C6C600C6C6C6003100CE00CECECE008484840052525200C6C6C6009CCE
      630000000000C6C6C600C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00000000000000
      000000000000000000000000000000000000FFFFFF00C6C6C60000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008484840000000000C6C6C600C6C6
      C60000FF0000C6C6C600C6C6C60000000000C6C6C600848484009CCE63009CFF
      CE00C6C6C600CEFF9C00CEFF9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084848400D6D6D60000FF0000DEDE
      DE00DEDEDE0000000000000000008484840084848400B5B5B500848484009CFF
      FF00B5B5B500CEFF9C004A4A4A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0084848400DEDEDE00E7E7E7000000
      000000000000C6C6C600C6C6C600000000000000000000000000848484008484
      8400848484008484840052525200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000000000000000C6C6
      C600000000000000000000000000C6C6C600C6C6C60084848400000000000000
      000084848400C6C6C600C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF0000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF000000000000000000848484000000
      000000000000C6C6C600C6C6C600000000008484840073737300009C9C00009C
      9C00000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00C6C6C600C6C6C600C6C6C600C6C6
      C6000000000000000000FFFFFF00FFFFFF00848484000000000000000000C6C6
      C600C6C6C600000000008484840073737300B5005200B5005200848484008484
      8400009C9C00009C9C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00C6C6C600C6C6C600C6C6C600C6C6
      C6000000000000000000FFFFFF00FFFFFF008484840000000000848484000000
      00007373730084848400FF429400FF4294008484840084848400B5005200B500
      5200848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00C6C6C600C6C6C600C6C6C600C6C6
      C6000000000000000000FFFFFF00FFFFFF008484840000000000848484008484
      8400FF429400FF4294007373730084848400B5005200FF429400737373008484
      8400B5005200B500520000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00C6C6C600C6C6C600C6C6C600C6C6
      C6000000000000000000FFFFFF00FFFFFF000000000084848400000000008484
      840084848400FF4294003100CE0073FF31008484840084848400FF429400B500
      5200B5005200B500520084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C6000000000000000000FFFFFF000000000000000000000000000000
      0000000000008484840084848400FF4294000000CE0073FF3100B5005200FF42
      9400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      00000000000000000000000000008484840084848400FF429400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF000084840000FFFF000084840000FFFF000084
      840000FFFF0000848400FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF008000FFE3000000008000FC4100000000
      8000F000000000004000C000000000003F000100000000000080060000000000
      00E0180000000000000068000000000000708101000000000208440100000000
      020850030000000002084000000000000208A000000000000404F80300000000
      0400FE0F000000000000FFBF0000000000000000000000000000000000000000
      000000000000}
  end
end
