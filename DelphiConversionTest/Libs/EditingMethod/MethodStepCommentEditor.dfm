object frmMethodStepCommentEditor: TfrmMethodStepCommentEditor
  Left = 0
  Top = 0
  Caption = 'Method Step Comment Editor'
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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 752
    Height = 90
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object Label1: TLabel
      Left = 22
      Top = 12
      Width = 124
      Height = 21
      Caption = 'Editing Comment:'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object mmoActionDescription: TMemo
      Left = 56
      Top = 40
      Width = 641
      Height = 44
      TabStop = False
      BorderStyle = bsNone
      Lines.Strings = (
        'mmoActionDescription')
      ReadOnly = True
      TabOrder = 0
    end
  end
  object pnlBottom: TPanel
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
    object lblLineBreakInfo: TLabel
      Left = 40
      Top = 16
      Width = 21
      Height = 15
      Caption = 'info'
    end
    object btnOK: TButton
      Tag = 530
      Left = 572
      Top = 11
      Width = 74
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      ModalResult = 1
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
    Top = 90
    Width = 752
    Height = 278
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 752
      Height = 278
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
      OnKeyDown = Memo1KeyDown
    end
  end
end
