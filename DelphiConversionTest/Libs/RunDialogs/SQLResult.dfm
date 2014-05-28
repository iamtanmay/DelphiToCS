object SQLResultForm: TSQLResultForm
  Left = 210
  Top = 193
  Caption = 'SQLResult'
  ClientHeight = 386
  ClientWidth = 772
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 772
    Height = 386
    Align = alClient
    Columns = <>
    MultiSelect = True
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
    ExplicitWidth = 557
    ExplicitHeight = 237
  end
end
