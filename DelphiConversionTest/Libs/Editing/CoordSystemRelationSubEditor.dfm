object fraCoordSystemRelation: TfraCoordSystemRelation
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 311
  ClientWidth = 132
  Color = clWhite
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 6
    Top = 6
    Width = 102
    Height = 98
    Caption = 'Offset (mm)'
    TabOrder = 0
    object Label1: TLabel
      Left = 13
      Top = 20
      Width = 7
      Height = 15
      Caption = 'X'
    end
    object Label2: TLabel
      Left = 13
      Top = 44
      Width = 7
      Height = 15
      Caption = 'Y'
    end
    object Label3: TLabel
      Left = 13
      Top = 69
      Width = 7
      Height = 15
      Caption = 'Z'
    end
    object edTranslateX: TEdit
      Left = 37
      Top = 19
      Width = 53
      Height = 23
      TabOrder = 0
      Text = '0'
    end
    object edTranslateY: TEdit
      Left = 37
      Top = 43
      Width = 53
      Height = 23
      TabOrder = 1
      Text = '0'
    end
    object edTranslateZ: TEdit
      Left = 37
      Top = 67
      Width = 53
      Height = 23
      TabOrder = 2
      Text = '0'
    end
  end
  object GroupBox2: TGroupBox
    Left = 6
    Top = 107
    Width = 102
    Height = 98
    Caption = 'Reflect Axis'
    TabOrder = 1
    object chkReflectX: TCheckBox
      Left = 10
      Top = 20
      Width = 80
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Reflect X'
      TabOrder = 0
    end
    object chkReflectY: TCheckBox
      Left = 10
      Top = 46
      Width = 80
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Reflect Y'
      TabOrder = 1
    end
    object chkReflectZ: TCheckBox
      Left = 10
      Top = 73
      Width = 80
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Reflect Z'
      TabOrder = 2
    end
  end
  object GroupBox3: TGroupBox
    Left = 6
    Top = 207
    Width = 102
    Height = 98
    Caption = 'Rotate in Axis'
    TabOrder = 2
    object Label7: TLabel
      Left = 13
      Top = 20
      Width = 7
      Height = 15
      Caption = 'X'
    end
    object Label8: TLabel
      Left = 13
      Top = 44
      Width = 7
      Height = 15
      Caption = 'Y'
    end
    object Label9: TLabel
      Left = 13
      Top = 69
      Width = 7
      Height = 15
      Caption = 'Z'
    end
    object edRotateX: TEdit
      Left = 37
      Top = 19
      Width = 53
      Height = 23
      TabOrder = 0
      Text = '0'
    end
    object edRotateY: TEdit
      Left = 37
      Top = 43
      Width = 53
      Height = 23
      TabOrder = 1
      Text = '0'
    end
    object edRotateZ: TEdit
      Left = 37
      Top = 67
      Width = 53
      Height = 23
      TabOrder = 2
      Text = '0'
    end
  end
end
