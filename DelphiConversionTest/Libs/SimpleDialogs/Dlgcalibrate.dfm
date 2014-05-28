object dlgCalibSystem: TdlgCalibSystem
  Tag = 31020
  Left = 181
  Top = 196
  BorderIcons = [biSystemMenu]
  Caption = 'Calibration'
  ClientHeight = 412
  ClientWidth = 694
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
    694
    412)
  PixelsPerInch = 96
  TextHeight = 15
  object GroupBox1: TGroupBox
    Tag = 31030
    Left = 488
    Top = 8
    Width = 198
    Height = 360
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'System liquid'
    TabOrder = 0
    object SystemLiquid: TComboBox
      Left = 40
      Top = 35
      Width = 149
      Height = 23
      TabOrder = 0
      OnChange = SystemLiquidChange
    end
  end
  object GroupBox4: TGroupBox
    Tag = 31170
    Left = 201
    Top = 8
    Width = 281
    Height = 360
    Caption = 'Options'
    TabOrder = 1
    object Label2: TLabel
      Tag = 31040
      Left = 16
      Top = 18
      Width = 84
      Height = 15
      Caption = 'Averaging steps'
    end
    object Label4: TLabel
      Tag = 31160
      Left = 16
      Top = 168
      Width = 71
      Height = 15
      Caption = 'Max Error (%)'
    end
    object Label5: TLabel
      Tag = 31161
      Left = 16
      Top = 219
      Width = 40
      Height = 15
      Caption = 'Max CV'
    end
    object Label3: TLabel
      Tag = 31050
      Left = 16
      Top = 66
      Width = 50
      Height = 15
      Caption = 'Max Tries'
    end
    object Label6: TLabel
      Tag = 31150
      Left = 16
      Top = 117
      Width = 39
      Height = 15
      Caption = 'Density'
    end
    object Label1: TLabel
      Tag = 31154
      Left = 200
      Top = 168
      Width = 68
      Height = 15
      Caption = 'Balance Wait'
    end
    object Label8: TLabel
      Tag = 31155
      Left = 178
      Top = 219
      Width = 92
      Height = 15
      Caption = 'Balance precision'
    end
    object Label9: TLabel
      Tag = 31153
      Left = 168
      Top = 117
      Width = 102
      Height = 15
      Caption = 'Manual Fill Volume'
    end
    object Label10: TLabel
      Tag = 31156
      Left = 145
      Top = 18
      Width = 125
      Height = 15
      Caption = 'Liquid Handling Param '
    end
    object Label11: TLabel
      Tag = 31152
      Left = 249
      Top = 67
      Width = 22
      Height = 15
      Caption = 'Tips'
    end
    object spnAveragingSteps: TSpinEdit
      Left = 16
      Top = 39
      Width = 81
      Height = 24
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 0
      Value = 1
    end
    object spnMaxIncorrectness: TSpinEdit
      Left = 16
      Top = 189
      Width = 81
      Height = 24
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 1
      Value = 4
    end
    object spnMaxCV: TSpinEdit
      Left = 17
      Top = 240
      Width = 81
      Height = 24
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 2
      Value = 4
    end
    object cbUseSystemLiquid: TCheckBox
      Tag = 31051
      Left = 17
      Top = 300
      Width = 156
      Height = 17
      Caption = 'Calibrate with Sys liquid '
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object LHP: TEdit
      Left = 189
      Top = 39
      Width = 81
      Height = 23
      TabOrder = 4
      Text = 'Default'
      OnChange = CustomProcedureNameChange
    end
    object TipArray: TEdit
      Left = 189
      Top = 88
      Width = 81
      Height = 23
      TabOrder = 5
      Text = '1'
      OnChange = CustomProcedureNameChange
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 374
    Width = 694
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnStart: TButton
      Tag = 32340
      Left = 503
      Top = 0
      Width = 81
      Height = 25
      Caption = 'Start'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = Start
    end
    object btnCancel: TButton
      Tag = 32350
      Left = 605
      Top = 0
      Width = 81
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 8
    Width = 187
    Height = 360
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Procedure'
    TabOrder = 3
    object Label7: TLabel
      Tag = 31220
      Left = 10
      Top = 83
      Width = 134
      Height = 15
      Caption = 'Custom Procedure Name'
    end
    object Panel2: TPanel
      Left = 2
      Top = 17
      Width = 183
      Height = 46
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object ProcedureName: TComboBox
        Left = 8
        Top = 18
        Width = 149
        Height = 23
        TabOrder = 0
        OnChange = ProcedureNameChange
      end
    end
    object CustomProcedureName: TEdit
      Left = 10
      Top = 104
      Width = 121
      Height = 23
      TabOrder = 1
      Text = 'Default'
      OnChange = CustomProcedureNameChange
    end
  end
  object spnMaxTries: TSpinEdit
    Left = 217
    Top = 95
    Width = 81
    Height = 24
    MaxValue = 1000000
    MinValue = 1
    TabOrder = 4
    Value = 1
  end
  object spnDensity: TSpinEdit
    Left = 217
    Top = 146
    Width = 81
    Height = 24
    MaxValue = 1000000
    MinValue = 1
    TabOrder = 5
    Value = 1
  end
  object spnBalancePrecision: TSpinEdit
    Left = 390
    Top = 248
    Width = 81
    Height = 24
    MaxValue = 1000000
    MinValue = 1
    TabOrder = 6
    Value = 1
  end
  object spnBalanceWait: TSpinEdit
    Left = 390
    Top = 197
    Width = 81
    Height = 24
    MaxValue = 1000000
    MinValue = 1
    TabOrder = 7
    Value = 1
  end
  object spnManualFill: TSpinEdit
    Left = 390
    Top = 146
    Width = 81
    Height = 24
    MaxValue = 1000000
    MinValue = 1
    TabOrder = 8
    Value = 1
  end
end
