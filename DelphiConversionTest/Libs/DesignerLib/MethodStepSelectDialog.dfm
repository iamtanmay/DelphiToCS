object frmMethodStepSelectDialog: TfrmMethodStepSelectDialog
  Left = 0
  Top = 0
  Caption = 'frmMethodStepSelectDialog'
  ClientHeight = 412
  ClientWidth = 752
  Color = clWhite
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 752
    Height = 109
    CustomHint = BalloonHint1
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object Label1: TLabel
      Left = 12
      Top = 19
      Width = 93
      Height = 15
      CustomHint = BalloonHint1
      Caption = 'Name der Aktion:'
      Color = clWhite
      ParentColor = False
    end
    object imgActionImage: TImage
      Left = 669
      Top = 16
      Width = 64
      Height = 64
      CustomHint = BalloonHint1
      IncrementalDisplay = True
    end
    object mmoDescription: TMemo
      Left = 112
      Top = 47
      Width = 544
      Height = 50
      CustomHint = BalloonHint1
      Enabled = False
      ReadOnly = True
      TabOrder = 0
    end
  end
  object cbActionName: TComboBox
    AlignWithMargins = True
    Left = 112
    Top = 16
    Width = 193
    Height = 23
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    CustomHint = BalloonHint1
    AutoDropDown = True
    DropDownCount = 12
    TabOrder = 0
    OnKeyUp = cbActionNameKeyUp
  end
  object Panel1: TPanel
    Left = 588
    Top = 109
    Width = 164
    Height = 261
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 0
      Width = 106
      Height = 15
      Caption = 'Most Recently used:'
    end
    object Splitter1: TSplitter
      Left = 0
      Top = 0
      Height = 261
      ExplicitLeft = 40
      ExplicitTop = 24
      ExplicitHeight = 100
    end
    object lstMRU: TListBox
      AlignWithMargins = True
      Left = 6
      Top = 18
      Width = 155
      Height = 240
      Margins.Top = 18
      CustomHint = BalloonHint1
      Align = alClient
      ItemHeight = 15
      TabOrder = 0
      OnClick = lstMRUClick
      OnDblClick = lstMRUDblClick
      OnKeyUp = lstMRUKeyUp
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 370
    Width = 752
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      752
      42)
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 752
      Height = 1
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 1076
    end
    object btnCancel: TButton
      Tag = 520
      Left = 669
      Top = 10
      Width = 74
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Tag = 510
      Left = 569
      Top = 10
      Width = 79
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 109
    Width = 588
    Height = 261
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel3'
    TabOrder = 4
    object CategoryButtons1: TCategoryButtons
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 582
      Height = 255
      ParentCustomHint = False
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      ButtonFlow = cbfVertical
      ButtonHeight = 30
      ButtonWidth = 104
      ButtonOptions = [boGradientFill, boShowCaptions, boUsePlusMinus]
      Categories = <>
      HotButtonColor = clHotLight
      RegularButtonColor = clMenu
      SelectedButtonColor = clMenuHighlight
      TabOrder = 0
      OnGetHint = CategoryButtons1GetHint
      OnHotButton = CategoryButtons1HotButton
      OnKeyUp = CategoryButtons1KeyUp
      OnMouseDown = CategoryButtons1MouseDown
      OnSelectedItemChange = CategoryButtons1SelectedItemChange
    end
  end
  object BalloonHint1: TBalloonHint
    HideAfter = 3000
    Left = 184
    Top = 96
  end
end
