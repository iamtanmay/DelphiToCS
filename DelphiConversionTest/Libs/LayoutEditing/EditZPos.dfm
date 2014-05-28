object frmEdZPos: TfrmEdZPos
  Tag = 58500
  Left = 378
  Top = 33
  BorderIcons = [biSystemMenu]
  Caption = 'Edit Layout Height'
  ClientHeight = 141
  ClientWidth = 270
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
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Label2: TLabel
    Left = 8
    Top = 10
    Width = 38
    Height = 15
    Caption = 'Carrier:'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsUnderline]
    ParentFont = False
  end
  object Panel3: TPanel
    Left = 0
    Top = 101
    Width = 270
    Height = 40
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object btnOK: TButton
      Tag = 510
      Left = 168
      Top = 8
      Width = 81
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = btnOKClick
    end
    object Button2: TButton
      Tag = 520
      Left = 56
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 31
    Width = 270
    Height = 70
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Tag = 58510
      Left = 60
      Top = 12
      Width = 103
      Height = 15
      Caption = 'Workbench Height:'
    end
    object Edit1: TEdit
      Left = 190
      Top = 8
      Width = 57
      Height = 23
      ReadOnly = True
      TabOrder = 0
      Text = 'Edit1'
    end
    object Button1: TButton
      Tag = 58520
      Left = 56
      Top = 36
      Width = 193
      Height = 25
      Caption = 'Set All Values To Z-Max'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
end
