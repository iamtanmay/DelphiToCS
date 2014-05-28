object frmTableEditor: TfrmTableEditor
  Left = 0
  Top = 0
  Caption = 'Data'
  ClientHeight = 349
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 15
  object DBGrid1: TDBGrid
    Left = 0
    Top = 41
    Width = 645
    Height = 308
    Align = alClient
    DataSource = dsData
    TabOrder = 0
    TitleFont.Charset = ANSI_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    OnDblClick = DBGrid1DblClick
    OnTitleClick = DBGrid1TitleClick
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 645
    Height = 41
    Align = alTop
    TabOrder = 1
    object DBNavigator1: TDBNavigator
      Left = -69
      Top = 2
      Width = 712
      Height = 39
      DataSource = dsData
      Align = alCustom
      TabOrder = 0
    end
  end
  object dsData: TDataSource
    Left = 296
    Top = 64
  end
end
