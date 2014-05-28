object StopDlg: TStopDlg
  Tag = 10700
  Left = 120
  Top = 225
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'User interrupt ...'
  ClientHeight = 101
  ClientWidth = 276
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 276
    Height = 59
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object Label1: TLabel
      Tag = 10710
      Left = 54
      Top = 22
      Width = 165
      Height = 21
      Caption = 'Prozess unterbrechen?'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI Semibold'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 59
    Width = 276
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      276
      42)
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 276
      Height = 1
      Align = alTop
      Shape = bsTopLine
    end
    object Button2: TButton
      Tag = 570
      Left = 193
      Top = 10
      Width = 74
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&No'
      ModalResult = 7
      TabOrder = 0
    end
    object Button1: TButton
      Tag = 560
      Left = 105
      Top = 10
      Width = 79
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Yes'
      Default = True
      ModalResult = 6
      TabOrder = 1
    end
  end
end
