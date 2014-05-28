object FrmAskRestart: TFrmAskRestart
  Left = 0
  Top = 0
  Caption = 'Restart'
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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel2: TBevel
    Left = 0
    Top = 58
    Width = 752
    Height = 1
    Align = alTop
    Shape = bsTopLine
    ExplicitTop = 8
    ExplicitWidth = 643
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 752
    Height = 58
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    object lblTitle: TLabel
      Left = 22
      Top = 7
      Width = 133
      Height = 25
      Caption = 'Restart Method:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object edMethodName: TEdit
      Left = 38
      Top = 34
      Width = 345
      Height = 19
      TabStop = False
      BorderStyle = bsNone
      Ctl3D = False
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 0
      Text = 'Method name'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 370
    Width = 752
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 474
    ExplicitWidth = 742
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
    object Button2: TButton
      Tag = 520
      Left = 675
      Top = 10
      Width = 74
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 665
    end
    object btnOK: TButton
      Tag = 510
      Left = 587
      Top = 10
      Width = 79
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
      ExplicitLeft = 577
    end
  end
  object pnlMiddle: TPanel
    Left = 0
    Top = 59
    Width = 752
    Height = 311
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 70
    ExplicitWidth = 742
    ExplicitHeight = 404
    object pnlMiddleTop: TPanel
      Left = 0
      Top = 0
      Width = 752
      Height = 73
      Align = alTop
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      object mmoRestartDescription: TMemo
        Left = 38
        Top = 18
        Width = 393
        Height = 55
        TabStop = False
        BevelInner = bvNone
        BorderStyle = bsNone
        Lines.Strings = (
          'Please select where to continue:')
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
    end
    object pnlMiddleBottom: TPanel
      Left = 0
      Top = 224
      Width = 752
      Height = 87
      Align = alBottom
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 1
      ExplicitTop = 248
      object btnChooseSpecific: TButton
        Left = 50
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Advanced'
        TabOrder = 0
        Visible = False
        OnClick = btnChooseSpecificClick
      end
      object mmoSpecificStepDescription: TMemo
        AlignWithMargins = True
        Left = 50
        Top = 44
        Width = 652
        Height = 33
        Margins.Left = 50
        Margins.Top = 0
        Margins.Right = 50
        Margins.Bottom = 10
        TabStop = False
        Align = alBottom
        Lines.Strings = (
          'Please select a step')
        ParentColor = True
        ReadOnly = True
        TabOrder = 1
        ExplicitTop = 40
      end
    end
    object pnlMiddleMid: TPanel
      Left = 0
      Top = 73
      Width = 752
      Height = 151
      Align = alClient
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 2
      ExplicitTop = 93
      ExplicitHeight = 119
      object rdgChoices: TRadioGroup
        AlignWithMargins = True
        Left = 50
        Top = 3
        Width = 652
        Height = 145
        Margins.Left = 50
        Margins.Right = 50
        Align = alClient
        TabOrder = 0
        WordWrap = True
        OnClick = rdgChoicesClick
        ExplicitHeight = 110
      end
    end
  end
end
