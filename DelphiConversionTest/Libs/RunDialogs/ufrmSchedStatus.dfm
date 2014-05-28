object frmSchedStatus: TfrmSchedStatus
  Left = 490
  Top = 210
  Caption = 'Scheduler Debug Information'
  ClientHeight = 424
  ClientWidth = 586
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 586
    Height = 424
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 584
      Height = 32
      Align = alTop
      TabOrder = 0
      object edTime: TEdit
        Left = 7
        Top = 3
        Width = 111
        Height = 23
        TabOrder = 0
        Text = '0'
      end
    end
    object sgStats: TStringGrid
      Left = 1
      Top = 33
      Width = 584
      Height = 390
      Align = alClient
      ColCount = 4
      DefaultRowHeight = 14
      DefaultDrawing = False
      FixedCols = 0
      RowCount = 100
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing]
      TabOrder = 1
      OnDrawCell = sgStatsDrawCell
      ColWidths = (
        1
        40
        1
        387)
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 728
    Top = 8
  end
end
