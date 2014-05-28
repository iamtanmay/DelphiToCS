object frmEdTipType: TfrmEdTipType
  Left = 1005
  Top = 126
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 520
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Tag = 12
    Left = 24
    Top = 48
    Width = 94
    Height = 15
    Caption = 'Basic Type Name:'
  end
  object Label2: TLabel
    Tag = 13
    Left = 24
    Top = 72
    Width = 70
    Height = 15
    Caption = 'Device Name'
  end
  object Label3: TLabel
    Tag = 11
    Left = 24
    Top = 24
    Width = 32
    Height = 15
    Caption = 'Name'
  end
  object Label4: TLabel
    Tag = 14
    Left = 24
    Top = 96
    Width = 59
    Height = 15
    Caption = 'Tool Name'
  end
  object Label5: TLabel
    Tag = 15
    Left = 24
    Top = 120
    Width = 93
    Height = 15
    Caption = 'Max. Volume [uL]'
  end
  object Label6: TLabel
    Tag = 16
    Left = 24
    Top = 144
    Width = 92
    Height = 15
    Caption = 'Min. Volume [uL]'
  end
  object Label7: TLabel
    Tag = 17
    Left = 24
    Top = 168
    Width = 172
    Height = 15
    Caption = 'Relative Length (Z-Offset) [mm]:'
  end
  object Label8: TLabel
    Tag = 18
    Left = 24
    Top = 192
    Width = 160
    Height = 15
    Caption = 'Removable Tip Z Offset [mm]:'
  end
  object Label9: TLabel
    Tag = 19
    Left = 24
    Top = 216
    Width = 74
    Height = 15
    Caption = 'Dilutor Name:'
  end
  object Label11: TLabel
    Tag = 20
    Left = 24
    Top = 240
    Width = 78
    Height = 15
    Caption = 'X Offset [mm]:'
  end
  object Label12: TLabel
    Tag = 21
    Left = 24
    Top = 264
    Width = 78
    Height = 15
    Caption = 'Y Offset [mm]:'
  end
  object Label13: TLabel
    Tag = 22
    Left = 24
    Top = 288
    Width = 110
    Height = 15
    Caption = 'Wash Z Offset [mm]:'
  end
  object Label14: TLabel
    Tag = 23
    Left = 24
    Top = 312
    Width = 113
    Height = 15
    Caption = 'Waste Z Offset [mm]:'
  end
  object Label15: TLabel
    Tag = 24
    Left = 24
    Top = 336
    Width = 99
    Height = 15
    Caption = 'Dry Z Offset [mm]:'
  end
  object Label16: TLabel
    Tag = 25
    Left = 24
    Top = 360
    Width = 81
    Height = 15
    Caption = 'Dry After Flush:'
  end
  object Label17: TLabel
    Tag = 26
    Left = 24
    Top = 384
    Width = 154
    Height = 15
    Caption = 'Method Name to fetch tip(s):'
  end
  object Label18: TLabel
    Tag = 27
    Left = 24
    Top = 408
    Width = 167
    Height = 15
    Caption = 'Method Name to remove tip(s):'
  end
  object Label10: TLabel
    Tag = 28
    Left = 24
    Top = 432
    Width = 59
    Height = 15
    Caption = 'Do not Init:'
  end
  object Panel3: TPanel
    Left = 0
    Top = 479
    Width = 337
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      337
      41)
    object btnOK: TButton
      Tag = 510
      Left = 146
      Top = 9
      Width = 81
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
    object Button2: TButton
      Tag = 520
      Left = 237
      Top = 9
      Width = 81
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object edTypeName: TEdit
    Left = 184
    Top = 24
    Width = 121
    Height = 23
    TabOrder = 1
  end
  object edDeviceName: TEdit
    Left = 184
    Top = 72
    Width = 121
    Height = 23
    TabOrder = 2
  end
  object edToolName: TEdit
    Left = 184
    Top = 96
    Width = 121
    Height = 23
    TabOrder = 3
  end
  object edMaxVolume: TEdit
    Left = 184
    Top = 120
    Width = 121
    Height = 23
    TabOrder = 4
  end
  object edMinVolume: TEdit
    Left = 184
    Top = 144
    Width = 121
    Height = 23
    TabOrder = 5
  end
  object EdZOffset: TEdit
    Left = 184
    Top = 168
    Width = 121
    Height = 23
    TabOrder = 6
  end
  object edZOffsetRemovable: TEdit
    Left = 184
    Top = 192
    Width = 121
    Height = 23
    TabOrder = 7
  end
  object edDilutorName: TEdit
    Left = 184
    Top = 216
    Width = 121
    Height = 23
    TabOrder = 8
  end
  object edXOffset: TEdit
    Left = 184
    Top = 240
    Width = 121
    Height = 23
    TabOrder = 9
  end
  object edYOffset: TEdit
    Left = 184
    Top = 264
    Width = 121
    Height = 23
    TabOrder = 10
  end
  object edWashZOffset: TEdit
    Left = 184
    Top = 288
    Width = 121
    Height = 23
    TabOrder = 11
  end
  object edWasteZOffset: TEdit
    Left = 184
    Top = 312
    Width = 121
    Height = 23
    TabOrder = 12
  end
  object edDryZOffset: TEdit
    Left = 184
    Top = 336
    Width = 121
    Height = 23
    TabOrder = 13
  end
  object edFetchTipsMethodName: TEdit
    Left = 184
    Top = 384
    Width = 121
    Height = 23
    TabOrder = 14
  end
  object edRemoveTipsMethodName: TEdit
    Left = 184
    Top = 408
    Width = 121
    Height = 23
    TabOrder = 15
  end
  object chkDryAfterFlush: TCheckBox
    Left = 184
    Top = 360
    Width = 97
    Height = 17
    TabOrder = 16
  end
  object cboBasicTypeName: TComboBox
    Left = 184
    Top = 48
    Width = 129
    Height = 23
    TabOrder = 17
    Items.Strings = (
      'DEFAULT'
      'DITI'
      'REDI')
  end
  object chkDoNotInit: TCheckBox
    Left = 184
    Top = 432
    Width = 97
    Height = 17
    TabOrder = 18
  end
end
