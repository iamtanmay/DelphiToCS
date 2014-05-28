object frmLogInfo: TfrmLogInfo
  Left = 165
  Top = 291
  Align = alClient
  Caption = 'frmLogInfo'
  ClientHeight = 384
  ClientWidth = 696
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 15
  object MainLogDisplay: TMemo
    Left = 0
    Top = 0
    Width = 696
    Height = 384
    Align = alClient
    Color = clWhite
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
