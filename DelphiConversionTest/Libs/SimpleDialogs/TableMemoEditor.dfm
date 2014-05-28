object frmTableMemoEditor: TfrmTableMemoEditor
  Left = 0
  Top = 0
  ClientHeight = 412
  ClientWidth = 752
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
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 752
    Height = 63
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object Label1: TLabel
      Left = 22
      Top = 12
      Width = 88
      Height = 21
      Caption = 'Editing Field:'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object edFieldName: TEdit
      Left = 48
      Top = 39
      Width = 457
      Height = 19
      TabStop = False
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      Text = 'FieldName'
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 368
    Width = 752
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      752
      44)
    object btnOK: TButton
      Tag = 530
      Left = 572
      Top = 11
      Width = 74
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ModalResult = 1
      ParentFont = False
      TabOrder = 0
    end
    object btnCancel: TButton
      Tag = 540
      Left = 660
      Top = 11
      Width = 74
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlMiddle: TPanel
    Left = 0
    Top = 63
    Width = 752
    Height = 305
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 752
      Height = 305
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
end
