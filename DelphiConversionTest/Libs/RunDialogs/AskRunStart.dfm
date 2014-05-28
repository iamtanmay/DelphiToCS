object frmAskRunStart: TfrmAskRunStart
  Left = 263
  Top = 322
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Start'
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
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Shape1: TShape
    Left = 0
    Top = 65
    Width = 762
    Height = 2
    Align = alTop
    Pen.Color = 14061151
    ExplicitWidth = 442
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 762
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      762
      65)
    object lblTitle: TLabel
      Left = 22
      Top = 13
      Width = 102
      Height = 21
      Caption = 'Start Method:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI Semibold'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edMethodName: TEdit
      Left = 56
      Top = 38
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
    object edSimulationCaption: TEdit
      Left = 661
      Top = 14
      Width = 86
      Height = 18
      TabStop = False
      Anchors = [akTop, akRight]
      BorderStyle = bsNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -16
      Font.Name = 'Segoe UI Semibold'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
      Text = 'Simulation'
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 67
    Width = 762
    Height = 318
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 762
      Height = 318
      ActivePage = TabSheet2
      Align = alClient
      Style = tsFlatButtons
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'TabSheet1'
        ImageIndex = 2
        object chkSimulation: TCheckBox
          Left = 32
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Simulation'
          TabOrder = 0
          OnClick = chkSimulationClick
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'TabSheet2'
        object mmoWarnings: TMemo
          Left = 24
          Top = 8
          Width = 388
          Height = 79
          TabStop = False
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          ReadOnly = True
          TabOrder = 0
        end
        object gbSimulation: TGroupBox
          Left = 18
          Top = 176
          Width = 590
          Height = 97
          Caption = 'Simulation'
          TabOrder = 1
          object lblSimSpeed: TLabel
            Left = 287
            Top = 56
            Width = 65
            Height = 15
            Caption = 'lblSimSpeed'
          end
          object trbSimulation: TTrackBar
            Left = 278
            Top = 25
            Width = 294
            Height = 25
            Hint = 'Simulation Speed'
            Max = 100
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = trbSimulationChange
          end
          object cbSimInputWeight: TCheckBox
            Tag = 26680
            Left = 21
            Top = 24
            Width = 220
            Height = 17
            Caption = 'Input weights manually'
            TabOrder = 1
          end
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 385
    Width = 762
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      762
      37)
    object Shape2: TShape
      Left = 0
      Top = 0
      Width = 762
      Height = 2
      Align = alTop
      Pen.Color = 14061151
      ExplicitWidth = 442
    end
    object btnCancel: TButton
      Tag = 520
      Left = 676
      Top = 8
      Width = 74
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnNext: TButton
      Left = 557
      Top = 8
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Next >'
      TabOrder = 0
      OnClick = btnNextClick
    end
    object btnBack: TButton
      Left = 465
      Top = 8
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '< Back'
      TabOrder = 2
      OnClick = btnBackClick
    end
  end
end
