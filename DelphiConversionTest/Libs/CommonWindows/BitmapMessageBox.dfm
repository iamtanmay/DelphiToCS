object frmBitmapMessageBox: TfrmBitmapMessageBox
  Tag = 13000
  Left = 212
  Top = 203
  BorderStyle = bsDialog
  Caption = 'Select entry'
  ClientHeight = 422
  ClientWidth = 762
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Shape2: TShape
    Left = 0
    Top = 0
    Width = 762
    Height = 378
    Align = alClient
    Pen.Color = clWhite
    ExplicitWidth = 368
    ExplicitHeight = 73
  end
  object Panel1: TPanel
    Left = 0
    Top = 378
    Width = 762
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      762
      44)
    object btnOK: TButton
      Tag = 530
      Left = 562
      Top = 8
      Width = 81
      Height = 28
      Anchors = [akTop, akRight]
      Caption = 'Continue'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 35
    end
    object btnCancel: TButton
      Tag = 540
      Left = 658
      Top = 8
      Width = 86
      Height = 28
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 131
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 762
    Height = 378
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 345
    ExplicitWidth = 417
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 762
      Height = 233
      Align = alTop
      AutoSize = True
      Center = True
      ExplicitWidth = 724
    end
    object Memo1: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 249
      Width = 756
      Height = 113
      Margins.Top = 16
      Margins.Bottom = 16
      Align = alClient
      Alignment = taCenter
      BorderStyle = bsNone
      Lines.Strings = (
        'Memo1'
        'jkhdfjkalshjkfl'
        'fwehjqkflheuiopqa')
      TabOrder = 0
      ExplicitTop = 239
      ExplicitWidth = 664
      ExplicitHeight = 50
    end
  end
end
