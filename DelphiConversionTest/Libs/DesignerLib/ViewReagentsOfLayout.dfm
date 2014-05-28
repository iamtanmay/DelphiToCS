object frmReagentsOfLayout: TfrmReagentsOfLayout
  Left = 270
  Top = 192
  BorderStyle = bsNone
  Caption = 'Reagent List'
  ClientHeight = 489
  ClientWidth = 409
  Color = clBtnFace
  DragKind = dkDock
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object rgAll: TRadioGroup
    Left = 0
    Top = 0
    Width = 409
    Height = 49
    Align = alTop
    ItemIndex = 0
    Items.Strings = (
      'All'
      'Racks of this layout')
    TabOrder = 0
    OnClick = rgAllClick
  end
  object lvReagents: TListView
    Left = 0
    Top = 49
    Width = 409
    Height = 440
    Align = alClient
    Columns = <
      item
        Caption = 'Reagent Name'
        Width = 300
      end
      item
        Caption = 'MolWeight'
      end>
    DragMode = dmAutomatic
    ReadOnly = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvReagentsDblClick
  end
end
