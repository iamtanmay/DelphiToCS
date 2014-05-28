object frmRunAbortInfo: TfrmRunAbortInfo
  Tag = 25300
  Left = 410
  Top = 187
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Run Abort Information'
  ClientHeight = 360
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel1: TBevel
    Left = 0
    Top = 313
    Width = 536
    Height = 47
    Align = alBottom
    Shape = bsTopLine
  end
  object btnOK: TButton
    Tag = 510
    Left = 411
    Top = 325
    Width = 74
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 1
    TabOrder = 0
  end
  object mmoMessage: TMemo
    Left = 24
    Top = 24
    Width = 489
    Height = 265
    ReadOnly = True
    TabOrder = 1
  end
end
