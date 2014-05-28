object frmMultiPageDialog: TfrmMultiPageDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'frmMultiPageDialog'
  ClientHeight = 444
  ClientWidth = 778
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel3: TPanel
    Left = 0
    Top = 404
    Width = 778
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      778
      40)
    object btnCancel: TButton
      Tag = 630
      Left = 672
      Top = 6
      Width = 80
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 3
      TabOrder = 1
    end
    object btnNext: TButton
      Tag = 610
      Left = 563
      Top = 5
      Width = 80
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Next >'
      TabOrder = 0
      OnClick = btnNextClick
    end
    object btnBack: TButton
      Tag = 600
      Left = 477
      Top = 5
      Width = 80
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '< &Back'
      Enabled = False
      TabOrder = 2
      OnClick = btnBackClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 778
    Height = 404
    Align = alClient
    Style = tsButtons
    TabOrder = 1
  end
end
