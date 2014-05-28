object frmRestartPreparations: TfrmRestartPreparations
  Left = 0
  Top = 0
  Caption = 'Restart Preparations'
  ClientHeight = 341
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 65
    Width = 643
    Height = 2
    Align = alTop
    Pen.Color = 14061151
    ExplicitWidth = 442
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 643
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 22
      Top = 13
      Width = 310
      Height = 20
      Caption = 'Required Preparations Before Restart:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
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
  end
  object Panel3: TPanel
    Left = 0
    Top = 67
    Width = 643
    Height = 237
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 643
      Height = 237
      ActivePage = tbsLayout
      Align = alClient
      Style = tsFlatButtons
      TabOrder = 0
      object tbsWelcome: TTabSheet
        Caption = 'tbsWelcome'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label1: TLabel
          Left = 18
          Top = 16
          Width = 3
          Height = 13
          WordWrap = True
        end
        object mmoIntro: TMemo
          Left = 18
          Top = 24
          Width = 414
          Height = 140
          BorderStyle = bsNone
          Lines.Strings = (
            
              'In order to ensure a correct restart, one or more prerparations ' +
              'have to made.'
            'Please follow the instructions on the following pages.')
          TabOrder = 0
        end
      end
      object tbsLayout: TTabSheet
        Caption = 'tbsLayout'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object tlLayout: TcxTreeList
          AlignWithMargins = True
          Left = 12
          Top = 12
          Width = 611
          Height = 182
          Margins.Left = 12
          Margins.Top = 12
          Margins.Right = 12
          Margins.Bottom = 12
          Align = alClient
          Bands = <
            item
            end>
          OptionsBehavior.AlwaysShowEditor = True
          OptionsBehavior.ImmediateEditor = False
          OptionsBehavior.Sorting = False
          OptionsCustomizing.BandCustomizing = False
          OptionsCustomizing.BandHorzSizing = False
          OptionsCustomizing.BandMoving = False
          OptionsCustomizing.BandVertSizing = False
          OptionsCustomizing.ColumnVertSizing = False
          OptionsView.GridLineColor = clBtnFace
          OptionsView.GridLines = tlglBoth
          OptionsView.PaintStyle = tlpsCategorized
          TabOrder = 0
          object cxTreeList1cxTreeListColumn1: TcxTreeListColumn
            PropertiesClassName = 'TcxPopupEditProperties'
            Caption.Text = 'Instructions'
            DataBinding.ValueType = 'String'
            Options.Customizing = False
            Options.Editing = False
            Options.Focusing = False
            Options.Moving = False
            Options.Sorting = False
            Width = 608
            Position.ColIndex = 0
            Position.LineCount = 2
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
        end
      end
      object tbsCompletion: TTabSheet
        Caption = 'tbsCompletion'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 304
    Width = 643
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      643
      37)
    object Shape2: TShape
      Left = 0
      Top = 0
      Width = 643
      Height = 2
      Align = alTop
      Pen.Color = 14061151
      ExplicitWidth = 442
    end
    object btnCancel: TButton
      Tag = 520
      Left = 557
      Top = 8
      Width = 74
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnNext: TButton
      Left = 438
      Top = 8
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Next >'
      TabOrder = 0
    end
    object btnBack: TButton
      Left = 346
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
