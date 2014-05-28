object frmActionProps: TfrmActionProps
  Left = 135
  Top = 214
  BorderIcons = [biSystemMenu]
  Caption = 'Action Properties'
  ClientHeight = 412
  ClientWidth = 752
  Color = clWhite
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object cxTreeList1: TcxTreeList
    Left = 0
    Top = 76
    Width = 752
    Height = 294
    Align = alClient
    Bands = <
      item
      end>
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    OptionsBehavior.Sorting = False
    OptionsCustomizing.BandCustomizing = False
    OptionsCustomizing.BandHorzSizing = False
    OptionsCustomizing.BandMoving = False
    OptionsCustomizing.BandVertSizing = False
    OptionsCustomizing.ColumnVertSizing = False
    OptionsView.ColumnAutoWidth = True
    OptionsView.GridLineColor = clBtnFace
    OptionsView.GridLines = tlglBoth
    OptionsView.PaintStyle = tlpsCategorized
    OptionsView.ShowRoot = False
    ParentFont = False
    TabOrder = 0
    OnEdited = cxTreeList1Edited
    OnFocusedNodeChanged = cxTreeList1FocusedNodeChanged
    OnKeyDown = cxTreeList1KeyDown
    object cxTreeList1cxTreeListColumn1: TcxTreeListColumn
      Caption.Text = 'Key'
      DataBinding.ValueType = 'String'
      Options.Customizing = False
      Options.Editing = False
      Options.Focusing = False
      Options.Moving = False
      Options.Sorting = False
      Width = 292
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxTreeList1cxTreeListColumn2: TcxTreeListColumn
      Caption.Text = 'Value'
      DataBinding.ValueType = 'String'
      Options.Customizing = False
      Options.Moving = False
      Options.Sorting = False
      Width = 473
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
      OnGetEditingProperties = cxTreeList1cxTreeListColumn2GetEditingProperties
    end
    object cxTreeList1cxTreeListColumn3: TcxTreeListColumn
      Visible = False
      Caption.Text = 'Index'
      DataBinding.ValueType = 'String'
      Options.Sizing = False
      Options.Customizing = False
      Options.Moving = False
      Options.Sorting = False
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxTreeList1cxTreeListColumn4: TcxTreeListColumn
      Visible = False
      DataBinding.ValueType = 'String'
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
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
  object pnlTop: TPanel
    AlignWithMargins = True
    Left = 6
    Top = 6
    Width = 740
    Height = 64
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object pnlActionImage: TPanel
      Left = 0
      Top = 0
      Width = 64
      Height = 64
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'pnlActionImage'
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
      object imgActionImage: TImage
        Left = 0
        Top = 0
        Width = 64
        Height = 64
        Align = alClient
        IncrementalDisplay = True
        ExplicitLeft = 5
        ExplicitTop = 5
        ExplicitHeight = 62
      end
    end
    object pnlTopRight: TPanel
      Left = 606
      Top = 0
      Width = 134
      Height = 64
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object edActionName: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 128
        Height = 23
        TabStop = False
        Align = alTop
        Alignment = taRightJustify
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
        Text = 'edActionName'
      end
    end
    object pnlTopMiddle: TPanel
      Left = 64
      Top = 0
      Width = 542
      Height = 64
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object mmoActionDescription: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 485
        Height = 58
        TabStop = False
        Align = alLeft
        BorderStyle = bsNone
        Lines.Strings = (
          'Action Description')
        ReadOnly = True
        TabOrder = 0
      end
    end
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Kind = lfFlat
    NativeStyle = True
    Left = 600
    Top = 8
  end
  object cxEditRepository1: TcxEditRepository
    Left = 560
    Top = 8
  end
end
