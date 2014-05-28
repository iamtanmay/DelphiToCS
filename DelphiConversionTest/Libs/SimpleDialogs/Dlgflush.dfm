object dlgFlushSystem: TdlgFlushSystem
  Tag = 31020
  Left = 181
  Top = 196
  BorderIcons = [biSystemMenu]
  Caption = 'Flush System'
  ClientHeight = 412
  ClientWidth = 752
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    752
    412)
  PixelsPerInch = 96
  TextHeight = 15
  object GroupBox1: TGroupBox
    Tag = 31030
    Left = 8
    Top = 8
    Width = 187
    Height = 360
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Diluent'
    TabOrder = 0
    object btnCheckAll: TButton
      Left = 28
      Top = 16
      Width = 25
      Height = 25
      Caption = '...'
      TabOrder = 0
      OnClick = btnCheckAllClick
    end
  end
  object GroupBox4: TGroupBox
    Tag = 31150
    Left = 376
    Top = 8
    Width = 176
    Height = 169
    Caption = 'Amount'
    TabOrder = 1
    object Label2: TLabel
      Tag = 31040
      Left = 16
      Top = 28
      Width = 113
      Height = 15
      Caption = 'Flush cycles:'
    end
    object rbUseSyrVol: TRadioButton
      Tag = 31160
      Left = 16
      Top = 87
      Width = 145
      Height = 17
      Caption = 'Syringe volume'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = cbSetVolumeClick
    end
    object rbDefineVolume: TRadioButton
      Tag = 31161
      Left = 16
      Top = 110
      Width = 145
      Height = 17
      Caption = 'Defined volume'
      TabOrder = 1
      OnClick = cbSetVolumeClick
    end
    object spnCycles: TSpinEdit
      Left = 32
      Top = 50
      Width = 81
      Height = 24
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 2
      Value = 10
    end
    object spnVolume: TSpinEdit
      Left = 32
      Top = 133
      Width = 81
      Height = 24
      Increment = 10
      MaxValue = 1000000
      MinValue = 0
      TabOrder = 3
      Value = 1000
      Visible = False
    end
  end
  object grbOptions: TGroupBox
    Tag = 31180
    Left = 558
    Top = 8
    Width = 176
    Height = 89
    Caption = 'grbOptions'
    TabOrder = 2
    object CheckPeriPump: TCheckBox
      Tag = 31050
      Left = 17
      Top = 60
      Width = 156
      Height = 17
      Caption = 'Peripump'
      TabOrder = 0
      Visible = False
      OnClick = CheckPeriPumpClick
    end
    object cbDryAfterWash: TCheckBox
      Tag = 31051
      Left = 17
      Top = 35
      Width = 156
      Height = 17
      Caption = 'Dry After Wash'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckPeriPumpClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 206
    Top = 8
    Width = 164
    Height = 360
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Tips'
    TabOrder = 3
    object Panel1: TPanel
      Left = 2
      Top = 17
      Width = 160
      Height = 46
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Tag = 31201
        Left = 5
        Top = 1
        Width = 61
        Height = 15
        Caption = 'Arm Name:'
      end
      object cmbArmName: TComboBox
        Left = 6
        Top = 19
        Width = 149
        Height = 23
        TabOrder = 0
        OnChange = cmbArmNameChange
      end
    end
  end
  object grbChannels: TGroupBox
    Tag = 31220
    Left = 558
    Top = 103
    Width = 176
    Height = 66
    Caption = 'Channel'
    TabOrder = 4
    Visible = False
    object chkUseCh1: TCheckBox
      Tag = 31230
      Left = 12
      Top = 18
      Width = 157
      Height = 17
      Caption = 'Use channel 1'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckUseChannelClick
    end
    object chkUseCh2: TCheckBox
      Tag = 31240
      Left = 12
      Top = 41
      Width = 157
      Height = 17
      Caption = 'Use channel 2'
      TabOrder = 1
      OnClick = CheckUseChannelClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 374
    Width = 752
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 5
    object btnStart: TButton
      Tag = 32340
      Left = 520
      Top = 4
      Width = 81
      Height = 25
      Caption = 'Start'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Tag = 32350
      Left = 618
      Top = 4
      Width = 81
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
