object frmSQLTermParams: TfrmSQLTermParams
  Tag = 41500
  Left = 41
  Top = 180
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Method Parameter Values'
  ClientHeight = 345
  ClientWidth = 681
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel1: TBevel
    Left = 0
    Top = 298
    Width = 681
    Height = 10
    Align = alBottom
    Shape = bsTopLine
  end
  object Panel1: TPanel
    Left = 0
    Top = 308
    Width = 681
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOK: TButton
      Tag = 510
      Left = 404
      Top = 3
      Width = 74
      Height = 23
      Cancel = True
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Tag = 520
      Left = 491
      Top = 3
      Width = 74
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 3
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 681
    Height = 49
    ParentCustomHint = False
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 1
    object lblHeaderTitle: TLabel
      Left = 16
      Top = 8
      Width = 3
      Height = 15
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI Semibold'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Tag = 41520
      Left = 32
      Top = 24
      Width = 176
      Height = 15
      Caption = 'Please enter the parameter values'
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 49
    Width = 681
    Height = 249
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
  end
end
