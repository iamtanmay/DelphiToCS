object BarcodeDLG: TBarcodeDLG
  Left = 168
  Top = 235
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'BarcodeDLG'
  ClientHeight = 114
  ClientWidth = 366
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 34
    Height = 15
    Caption = 'Label1'
  end
  object btnOK: TButton
    Tag = 510
    Left = 276
    Top = 81
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnAbort: TButton
    Tag = 530
    Left = 4
    Top = 81
    Width = 75
    Height = 25
    Caption = '&Abort'
    ModalResult = 3
    TabOrder = 1
  end
  object btnNoRack: TButton
    Tag = 31500
    Left = 188
    Top = 81
    Width = 75
    Height = 25
    Caption = 'No Rack'
    ModalResult = 40
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 8
    Top = 48
    Width = 345
    Height = 23
    TabOrder = 3
    Text = 'Edit1'
    OnChange = Edit1Change
  end
end
