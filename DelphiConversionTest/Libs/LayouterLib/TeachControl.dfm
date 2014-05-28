object frmTeachControl: TfrmTeachControl
  Left = 20
  Top = 18
  Caption = 'Teach Positions for location based motion device'
  ClientHeight = 770
  ClientWidth = 1157
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Panel6: TPanel
    Left = 0
    Top = 0
    Width = 217
    Height = 770
    Align = alLeft
    Caption = 'Panel6'
    TabOrder = 0
    object DBNavigator: TDBNavigator
      Left = 1
      Top = 1
      Width = 215
      Height = 25
      DataSource = dmRobotTrack.dsRTMaster
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbPost, nbCancel]
      Align = alTop
      Flat = True
      Ctl3D = False
      ParentCtl3D = False
      ParentShowHint = False
      ConfirmDelete = False
      ShowHint = True
      TabOrder = 0
    end
    object dbgNavRec: TDBGrid
      Left = 1
      Top = 26
      Width = 215
      Height = 743
      Align = alClient
      DataSource = dmRobotTrack.dsRTMaster
      Options = [dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
      TabOrder = 1
      TitleFont.Charset = ANSI_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -12
      TitleFont.Name = 'Segoe UI'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'NAME'
          Title.Caption = 'Name:'
          Visible = True
        end>
    end
  end
  object Panel2: TPanel
    Left = 217
    Top = 0
    Width = 940
    Height = 770
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    Caption = 'Panel2'
    TabOrder = 1
    object ScrollBox: TScrollBox
      Left = 1
      Top = 446
      Width = 938
      Height = 323
      HorzScrollBar.Margin = 6
      VertScrollBar.Margin = 6
      Align = alBottom
      BorderStyle = bsNone
      TabOrder = 0
      object gbDataFromMachine: TGroupBox
        Left = 6
        Top = 0
        Width = 331
        Height = 313
        Caption = 'Data From Machine'
        TabOrder = 0
        object Label2: TLabel
          Left = 79
          Top = 15
          Width = 98
          Height = 15
          Caption = 'Joints (in degrees):'
        end
        object Label3: TLabel
          Left = 199
          Top = 15
          Width = 97
          Height = 15
          Caption = 'Positions (in mm):'
        end
        object Label38: TLabel
          Left = 47
          Top = 31
          Width = 31
          Height = 15
          Caption = 'J1 (X):'
        end
        object Label39: TLabel
          Left = 47
          Top = 46
          Width = 31
          Height = 15
          Caption = 'J2 (Y):'
        end
        object Label40: TLabel
          Left = 47
          Top = 62
          Width = 31
          Height = 15
          Caption = 'J3 (Z):'
        end
        object Label41: TLabel
          Left = 39
          Top = 77
          Width = 38
          Height = 15
          Caption = 'J4 (RX):'
        end
        object Label42: TLabel
          Left = 39
          Top = 93
          Width = 38
          Height = 15
          Caption = 'J5 (RY):'
        end
        object Label43: TLabel
          Left = 39
          Top = 108
          Width = 38
          Height = 15
          Caption = 'J6 (RZ):'
        end
        object Label44: TLabel
          Left = 26
          Top = 124
          Width = 53
          Height = 15
          Caption = 'J7 (Track):'
        end
        object lstJoints: TListBox
          Left = 80
          Top = 31
          Width = 113
          Height = 110
          Style = lbOwnerDrawVariable
          ItemHeight = 15
          TabOrder = 0
        end
        object lstPositions: TListBox
          Left = 200
          Top = 31
          Width = 113
          Height = 110
          Style = lbOwnerDrawVariable
          ItemHeight = 15
          TabOrder = 1
        end
        object btnReadPositionsFromMachine: TBitBtn
          Left = 82
          Top = 148
          Width = 233
          Height = 25
          Caption = '&Read Values from Machine'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 2
        end
        object btnAssignToCurRecAsWorld: TBitBtn
          Left = 6
          Top = 186
          Width = 316
          Height = 25
          Caption = '&Assign Values to Current Record (Position) as World Movement'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 3
        end
        object btnAssignToNewRecAsWorld: TBitBtn
          Left = 6
          Top = 210
          Width = 316
          Height = 25
          Caption = '&Assign Values to a New Record as World Movement'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 4
        end
      end
      object GroupBox22: TGroupBox
        Left = 593
        Top = 4
        Width = 336
        Height = 185
        Caption = 'WinLissy Final Movement'
        TabOrder = 1
        object Label115: TLabel
          Left = 11
          Top = 83
          Width = 142
          Height = 15
          Caption = 'Current Robot (Z) Position:'
        end
        object Label116: TLabel
          Left = 11
          Top = 43
          Width = 142
          Height = 15
          Caption = 'Current Robot (X) Position:'
        end
        object Label117: TLabel
          Left = 19
          Top = 16
          Width = 89
          Height = 15
          Caption = 'Current Position:'
        end
        object dbeCurrentPosition: TDBText
          Left = 102
          Top = 16
          Width = 65
          Height = 17
          Color = clBtnFace
          DataField = 'INDEX'
          DataSource = dmRobotTrack.dsRTJointsDetails
          ParentColor = False
        end
        object dbeXPosition: TDBText
          Left = 74
          Top = 59
          Width = 59
          Height = 17
          Color = clBtnFace
          DataField = 'J1'
          DataSource = dmRobotTrack.dsRTJointsDetails
          ParentColor = False
        end
        object dbeZPosition: TDBText
          Left = 74
          Top = 99
          Width = 59
          Height = 17
          Color = clBtnFace
          DataField = 'J3'
          DataSource = dmRobotTrack.dsRTJointsDetails
          ParentColor = False
        end
        object Label118: TLabel
          Left = 22
          Top = 135
          Width = 71
          Height = 15
          Caption = 'X Movement:'
        end
        object Label119: TLabel
          Left = 22
          Top = 155
          Width = 71
          Height = 15
          Caption = 'Z Movement:'
        end
        object btnGoForward: TBitBtn
          Left = 145
          Top = 40
          Width = 179
          Height = 25
          Caption = 'Go Forward (First X, then Z)'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 0
        end
        object btnGoBackward: TBitBtn
          Left = 145
          Top = 111
          Width = 179
          Height = 25
          Caption = 'Go Backward (First Z, then X)'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 1
        end
        object edtExtraX: TEdit
          Left = 96
          Top = 131
          Width = 41
          Height = 23
          TabOrder = 2
          Text = '0'
        end
        object edtExtraZ: TEdit
          Left = 96
          Top = 152
          Width = 41
          Height = 23
          TabOrder = 3
          Text = '0'
        end
        object btnGoForwardX: TBitBtn
          Left = 145
          Top = 64
          Width = 90
          Height = 25
          Caption = 'Go Forward (X)'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 4
        end
        object btnGoForwardZ: TBitBtn
          Left = 234
          Top = 64
          Width = 90
          Height = 25
          Caption = 'Go Forward (Z)'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 5
        end
        object btnGoBackwardX: TBitBtn
          Left = 145
          Top = 135
          Width = 90
          Height = 25
          Caption = 'Go Backward (X)'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 6
        end
        object btnGoBackwardZ: TBitBtn
          Left = 234
          Top = 135
          Width = 90
          Height = 25
          Caption = 'Go Backward (Z)'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 7
        end
      end
      object GroupBox21: TGroupBox
        Left = 342
        Top = 2
        Width = 243
        Height = 87
        Caption = 'Gripper'
        TabOrder = 2
        object fSensor1: TLabel
          Left = 96
          Top = 56
          Width = 63
          Height = 15
          Caption = 'Sensor Port:'
        end
        object BitBtn7: TBitBtn
          Left = 10
          Top = 22
          Width = 110
          Height = 25
          Caption = 'Open Gripper'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 0
        end
        object BitBtn8: TBitBtn
          Left = 124
          Top = 22
          Width = 110
          Height = 25
          Caption = 'Close Gripper'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 1
        end
        object pnlSensor1State: TPanel
          Left = 162
          Top = 54
          Width = 17
          Height = 17
          Color = clBlack
          TabOrder = 2
        end
      end
      object GroupBox23: TGroupBox
        Left = 592
        Top = 192
        Width = 337
        Height = 121
        Caption = 'Calculator'
        TabOrder = 3
        object Label121: TLabel
          Left = 23
          Top = 17
          Width = 10
          Height = 15
          Caption = 'X:'
          Visible = False
        end
        object Label122: TLabel
          Left = 23
          Top = 42
          Width = 10
          Height = 15
          Caption = 'Y:'
          Visible = False
        end
        object Label123: TLabel
          Left = 23
          Top = 67
          Width = 10
          Height = 15
          Caption = 'Z:'
          Visible = False
        end
        object edtXt: TEdit
          Left = 43
          Top = 15
          Width = 70
          Height = 23
          TabOrder = 0
          Text = '0'
          Visible = False
        end
        object StaticText1: TStaticText
          Left = 123
          Top = 17
          Width = 70
          Height = 17
          AutoSize = False
          BorderStyle = sbsSunken
          TabOrder = 1
          Visible = False
        end
        object edtYt: TEdit
          Left = 43
          Top = 40
          Width = 70
          Height = 23
          TabOrder = 2
          Text = '0'
          Visible = False
        end
        object StaticText2: TStaticText
          Left = 123
          Top = 42
          Width = 70
          Height = 17
          AutoSize = False
          BorderStyle = sbsSunken
          TabOrder = 3
          Visible = False
        end
        object edtZt: TEdit
          Left = 43
          Top = 65
          Width = 70
          Height = 23
          TabOrder = 4
          Text = '0'
          Visible = False
        end
        object StaticText3: TStaticText
          Left = 123
          Top = 67
          Width = 70
          Height = 17
          AutoSize = False
          BorderStyle = sbsSunken
          TabOrder = 5
          Visible = False
        end
        object BitBtn9: TBitBtn
          Left = 120
          Top = 88
          Width = 137
          Height = 25
          Caption = '&Calculate Target Position'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 6
          Visible = False
        end
      end
      object rgValves: TGroupBox
        Left = 344
        Top = 96
        Width = 241
        Height = 89
        Caption = 'Change Gripper'
        TabOrder = 4
        object fSensor2: TLabel
          Left = 96
          Top = 60
          Width = 63
          Height = 15
          Caption = 'Sensor Port:'
        end
        object btnOpenValves: TBitBtn
          Left = 10
          Top = 16
          Width = 110
          Height = 25
          Caption = 'Release Gripper'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 0
        end
        object btnCloseValves: TBitBtn
          Left = 122
          Top = 16
          Width = 110
          Height = 25
          Caption = 'Catch Gripper'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 1
        end
        object pnlSensor2State: TPanel
          Left = 162
          Top = 56
          Width = 17
          Height = 17
          Color = clBlack
          TabOrder = 2
        end
      end
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 938
      Height = 48
      Align = alTop
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 1
      object Label1: TLabel
        Left = 6
        Top = 6
        Width = 35
        Height = 15
        Caption = '&Name:'
        FocusControl = eEditNAME
      end
      object Label98: TLabel
        Left = 311
        Top = 25
        Width = 79
        Height = 15
        Caption = 'External Speed:'
      end
      object Label99: TLabel
        Left = 452
        Top = 25
        Width = 69
        Height = 15
        Caption = 'Acceleration:'
      end
      object Label100: TLabel
        Left = 579
        Top = 25
        Width = 69
        Height = 15
        Caption = 'Deceleration:'
      end
      object eEditNAME: TDBEdit
        Left = 6
        Top = 21
        Width = 288
        Height = 23
        DataField = 'NAME'
        DataSource = dmRobotTrack.dsRTMaster
        TabOrder = 0
      end
      object eEditSPEED: TDBEdit
        Left = 390
        Top = 21
        Width = 49
        Height = 23
        DataField = 'SPEED'
        DataSource = dmRobotTrack.dsRTMaster
        TabOrder = 1
      end
      object eEditACCELERATION: TDBEdit
        Left = 518
        Top = 21
        Width = 49
        Height = 23
        DataField = 'ACCELERATION'
        DataSource = dmRobotTrack.dsRTMaster
        TabOrder = 2
      end
      object eEditDECELERATION: TDBEdit
        Left = 646
        Top = 21
        Width = 49
        Height = 23
        DataField = 'DECELERATION'
        DataSource = dmRobotTrack.dsRTMaster
        TabOrder = 3
      end
      object DBCheckBox1: TDBCheckBox
        Left = 710
        Top = 24
        Width = 201
        Height = 17
        Caption = 'Arm and Track Simultaneously Move'
        DataField = 'SIMULTANEOUSARMTRACK'
        DataSource = dmRobotTrack.dsRTMaster
        TabOrder = 4
        ValueChecked = 'True'
        ValueUnchecked = 'False'
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 49
      Width = 938
      Height = 397
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Panel3'
      TabOrder = 2
      DesignSize = (
        938
        397)
      object spUndo: TSpeedButton
        Left = 480
        Top = 4
        Width = 26
        Height = 25
        Hint = 'Undo'
        Flat = True
        Glyph.Data = {
          360C0000424D360C000000000000360000002800000020000000200000000100
          180000000000000C000000000000000000000000000000000000D8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC800000D8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC8000008000008000008000008000
          00D8E9ECD8E9ECD8E9ECD8E9ECD8E9EC800000D8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC800000800000800000800000D8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC800000D8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC800000800000800000D8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC800000D8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC800000800000D8E9EC800000D8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC800000D8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC800000D8E9ECD8E9ECD8E9EC8000
          00800000D8E9ECD8E9ECD8E9ECD8E9EC800000D8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9EC800000800000800000800000D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECFFFFFFD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
          D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
          ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
          E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFD8E9ECD8E9ECD8E9ECFFFFFF}
        ParentShowHint = False
        ShowHint = True
      end
      object dbgPath: TDBGrid
        Left = 8
        Top = 38
        Width = 709
        Height = 346
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dmRobotTrack.dsRTJointsDetails
        Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        TitleFont.Charset = ANSI_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -12
        TitleFont.Name = 'Segoe UI'
        TitleFont.Style = []
        Columns = <
          item
            Color = clInactiveCaptionText
            Expanded = False
            FieldName = 'INDEX'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Position'
            Width = 43
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'MOVEMENTTYPE'
            PickList.Strings = (
              'World'
              'Joint')
            Title.Caption = 'Type'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'INTERPOLATIONTYPE'
            Title.Caption = 'Interpolat.'
            Width = 32
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'J1'
            Title.Caption = 'J1 (X)'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'J2'
            Title.Caption = 'J2 (Y)'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'J3'
            Title.Caption = 'J3 (Z)'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'J4'
            Title.Caption = 'J4 (RX)'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'J5'
            Title.Caption = 'J5 (RY)'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'J6'
            Title.Caption = 'J6 (RZ)'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'J7'
            Title.Caption = 'J7 (Track)'
            Width = 59
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'XOFFSET'
            Title.Caption = 'X-Offset'
            Width = 48
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'ZOFFSET'
            Title.Caption = 'Z-Offset'
            Visible = True
          end>
      end
      object btnMoveToCurrentRecordPosition: TBitBtn
        Left = 732
        Top = 25
        Width = 200
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Move Arm to Current &Position'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 1
      end
      object btnMoveForwAlongTheWholePath: TBitBtn
        Left = 732
        Top = 118
        Width = 200
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Move &Forwards Along the Whole Path'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 4
      end
      object GroupBox1: TGroupBox
        Left = 738
        Top = 294
        Width = 185
        Height = 98
        Anchors = [akTop, akRight]
        Caption = 'Move Arm:'
        TabOrder = 6
        object Label12: TLabel
          Left = 24
          Top = 21
          Width = 77
          Height = 15
          Caption = 'Fro&m Position:'
        end
        object Label13: TLabel
          Left = 34
          Top = 45
          Width = 63
          Height = 15
          Caption = 'T&o Position:'
        end
        object btnMoveFromTo: TBitBtn
          Left = 59
          Top = 67
          Width = 75
          Height = 25
          Caption = 'Mo&ve'
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 2
        end
        object spFromPosition: TSpinEdit
          Left = 104
          Top = 16
          Width = 57
          Height = 24
          MaxValue = 256
          MinValue = 1
          TabOrder = 0
          Value = 1
        end
        object spToPosition: TSpinEdit
          Left = 104
          Top = 40
          Width = 57
          Height = 24
          MaxValue = 256
          MinValue = 1
          TabOrder = 1
          Value = 1
        end
      end
      object btnMoveFromCurPosToEnd: TBitBtn
        Left = 732
        Top = 63
        Width = 200
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Move Arm from Position 0 to &End'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 2
      end
      object btnMoveFromCurPosToBegin: TBitBtn
        Left = 732
        Top = 87
        Width = 200
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Move Arm from Position 0 to Be&gin'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 3
      end
      object btnMoveBackAlongTheWholePath: TBitBtn
        Left = 732
        Top = 142
        Width = 200
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Move &Backwards Along the Whole Path'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 5
      end
      object cbMoveTrack: TCheckBox
        Left = 732
        Top = 4
        Width = 137
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Also move track together'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object BitBtn12: TBitBtn
        Left = 744
        Top = 179
        Width = 179
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Go Forward (First X, then Z)'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 8
      end
      object BitBtn13: TBitBtn
        Left = 744
        Top = 243
        Width = 179
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Go Backward (First Z, then X)'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 9
      end
      object BitBtn14: TBitBtn
        Left = 744
        Top = 204
        Width = 90
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Go Forward (X)'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 10
      end
      object BitBtn15: TBitBtn
        Left = 833
        Top = 204
        Width = 90
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Go Forward (Z)'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 11
      end
      object BitBtn16: TBitBtn
        Left = 744
        Top = 267
        Width = 90
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Go Backward (X)'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 12
      end
      object BitBtn17: TBitBtn
        Left = 833
        Top = 267
        Width = 90
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Go Backward (Z)'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 13
      end
      object DBNavigator1: TDBNavigator
        Left = 8
        Top = 4
        Width = 232
        Height = 25
        DataSource = dmRobotTrack.dsRTJointsDetails
        VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbPost, nbCancel]
        Flat = True
        Ctl3D = False
        ParentCtl3D = False
        ParentShowHint = False
        ConfirmDelete = False
        ShowHint = True
        TabOrder = 14
      end
      object dbeJ1: TDBEdit
        Left = 254
        Top = 7
        Width = 59
        Height = 23
        DataField = 'J1'
        DataSource = dmRobotTrack.dsRTJointsDetails
        TabOrder = 15
      end
    end
  end
end
