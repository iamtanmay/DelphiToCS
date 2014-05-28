object frmWizard: TfrmWizard
  Left = 208
  Top = 472
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 422
  ClientWidth = 762
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Shape2: TShape
    Left = 0
    Top = 0
    Width = 762
    Height = 78
    Align = alTop
    Pen.Color = clWhite
  end
  object Bevel2: TBevel
    Left = 0
    Top = 78
    Width = 762
    Height = 2
    Align = alTop
    Shape = bsTopLine
    ExplicitTop = 58
    ExplicitWidth = 495
  end
  object lblHeaderTitle: TLabel
    Left = 16
    Top = 8
    Width = 69
    Height = 15
    Caption = 'Header Title'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Image1: TImage
    Left = 651
    Top = 8
    Width = 64
    Height = 64
    Center = True
  end
  object Memo1: TMemo
    Left = 32
    Top = 24
    Width = 569
    Height = 50
    BorderStyle = bsNone
    Color = clWhite
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    TabOrder = 0
  end
  object Panel3: TPanel
    Left = 0
    Top = 382
    Width = 762
    Height = 40
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object btnCancel: TButton
      Tag = 630
      Left = 667
      Top = 7
      Width = 80
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 3
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object btnNext: TButton
      Tag = 610
      Left = 558
      Top = 7
      Width = 80
      Height = 25
      Caption = '&Next >'
      Default = True
      Enabled = False
      TabOrder = 1
      OnClick = btnNextClick
    end
    object btnBack: TButton
      Tag = 600
      Left = 472
      Top = 7
      Width = 80
      Height = 25
      Cancel = True
      Caption = '< &Back'
      Enabled = False
      TabOrder = 2
      OnClick = btnBackClick
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 80
    Width = 762
    Height = 302
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 2
  end
end
