object frmEdXY: TfrmEdXY
  Left = 262
  Top = 254
  BorderIcons = [biSystemMenu]
  Caption = 'Change XY'
  ClientHeight = 118
  ClientWidth = 255
  Color = clBtnFace
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
  object Label1: TLabel
    Left = 24
    Top = 18
    Width = 10
    Height = 15
    Caption = 'X:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI Semibold'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 24
    Top = 50
    Width = 10
    Height = 15
    Caption = 'Y:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI Semibold'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Panel3: TPanel
    Left = 0
    Top = 78
    Width = 255
    Height = 40
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object btnOK: TButton
      Tag = 510
      Left = 72
      Top = 8
      Width = 81
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
    object Button2: TButton
      Tag = 520
      Left = 159
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object edPosX: TEdit
    Left = 49
    Top = 16
    Width = 121
    Height = 23
    TabOrder = 1
  end
  object edPosY: TEdit
    Left = 49
    Top = 48
    Width = 121
    Height = 23
    TabOrder = 2
  end
end
