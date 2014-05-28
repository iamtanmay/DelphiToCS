object GetNameDlg: TGetNameDlg
  Tag = 13000
  Left = 212
  Top = 203
  BorderStyle = bsDialog
  Caption = 'Select entry'
  ClientHeight = 261
  ClientWidth = 576
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
  object Panel1: TPanel
    Left = 0
    Top = 217
    Width = 576
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      576
      44)
    object btnOK: TButton
      Tag = 530
      Left = 396
      Top = 11
      Width = 74
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Continue'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Tag = 540
      Left = 484
      Top = 11
      Width = 74
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 576
    Height = 217
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 18
      Width = 61
      Height = 15
      Caption = 'Select item:'
      Color = clWhite
      ParentColor = False
    end
    object ComboBox1: TComboBox
      AlignWithMargins = True
      Left = 77
      Top = 48
      Width = 481
      Height = 169
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      AutoDropDown = True
      Style = csSimple
      DropDownCount = 12
      TabOrder = 0
      OnDblClick = ComboBox1DblClick
      OnKeyDown = ComboBox1KeyDown
    end
  end
end
