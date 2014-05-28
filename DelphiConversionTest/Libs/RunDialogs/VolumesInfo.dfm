object frmVolumesInfo: TfrmVolumesInfo
  Left = 207
  Top = 522
  BorderStyle = bsNone
  Caption = 'frmVolumesInfo'
  ClientHeight = 215
  ClientWidth = 359
  Color = clWhite
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object pnVolumes: TPanel
    Left = 0
    Top = 0
    Width = 359
    Height = 215
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object lblW: TLabel
      Tag = 24060
      Left = 5
      Top = 5
      Width = 35
      Height = 15
      Caption = 'Waste:'
    end
    object Label1: TLabel
      Left = 125
      Top = 5
      Width = 17
      Height = 15
      Caption = 'mL'
    end
    object Bevel1: TBevel
      Left = 0
      Top = 5
      Width = 146
      Height = 18
    end
    object Label3: TLabel
      Left = 125
      Top = 25
      Width = 17
      Height = 15
      Caption = 'mL'
    end
    object Label4: TLabel
      Left = 0
      Top = 25
      Width = 62
      Height = 15
      Caption = '(Maximum)'
    end
    object lblWasteVol: TStaticText
      Left = 70
      Top = 5
      Width = 50
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '300'
      Color = clLime
      ParentColor = False
      TabOrder = 0
      Transparent = False
    end
    object lblMaxWaste: TStaticText
      Left = 70
      Top = 25
      Width = 50
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '1000'
      Color = clWhite
      ParentColor = False
      TabOrder = 1
    end
    object btnEmpty: TButton
      Tag = 24050
      Left = 150
      Top = 4
      Width = 60
      Height = 20
      Caption = 'Empty'
      TabOrder = 2
      OnClick = btnEmptyClick
    end
  end
end
