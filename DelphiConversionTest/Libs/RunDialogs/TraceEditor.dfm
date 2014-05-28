object frmTraceEditor: TfrmTraceEditor
  Left = 80
  Top = 346
  Caption = 'Continue from a Previous Execution'
  ClientHeight = 529
  ClientWidth = 1069
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
  object pnlMiddle: TPanel
    Left = 0
    Top = 69
    Width = 1069
    Height = 418
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object cxTreeList1: TcxTreeList
      Left = 0
      Top = 0
      Width = 1069
      Height = 377
      Align = alClient
      Bands = <
        item
        end>
      DragMode = dmAutomatic
      OptionsBehavior.ImmediateEditor = False
      OptionsBehavior.ExpandOnDblClick = False
      OptionsBehavior.Sorting = False
      OptionsCustomizing.BandCustomizing = False
      OptionsCustomizing.BandHorzSizing = False
      OptionsCustomizing.BandMoving = False
      OptionsCustomizing.BandVertSizing = False
      OptionsCustomizing.ColumnMoving = False
      OptionsData.Editing = False
      OptionsData.Deleting = False
      OptionsSelection.CellSelect = False
      OptionsView.CellAutoHeight = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.GridLines = tlglBoth
      OptionsView.PaintStyle = tlpsCategorized
      PopupMenu = pmnuOptions
      TabOrder = 0
      OnCustomDrawDataCell = cxTreeList1CustomDrawDataCell
      OnDblClick = cxTreeList1DblClick
      object colStepName: TcxTreeListColumn
        Tag = 44230
        Caption.Text = 'Method'
        DataBinding.ValueType = 'String'
        Options.Editing = False
        Options.Sorting = False
        Width = 312
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object colStepDescription: TcxTreeListColumn
        Tag = 44240
        Caption.Text = 'Option'
        DataBinding.ValueType = 'String'
        Options.Editing = False
        Options.Sorting = False
        Width = 275
        Position.ColIndex = 1
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object colSourceName: TcxTreeListColumn
        Caption.Text = 'Source Method'
        DataBinding.ValueType = 'String'
        Width = 164
        Position.ColIndex = 2
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object colSourceLine: TcxTreeListColumn
        Caption.Text = 'Source Line'
        DataBinding.ValueType = 'String'
        Width = 66
        Position.ColIndex = 3
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object colStartTime: TcxTreeListColumn
        Caption.Text = 'StartTime'
        DataBinding.ValueType = 'String'
        Width = 127
        Position.ColIndex = 4
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object colFinishTime: TcxTreeListColumn
        Caption.Text = 'FinishTime'
        DataBinding.ValueType = 'String'
        Width = 121
        Position.ColIndex = 5
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object colActionData: TcxTreeListColumn
        Visible = False
        DataBinding.ValueType = 'Variant'
        Position.ColIndex = 6
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object colActionSegmentIndex: TcxTreeListColumn
        Visible = False
        DataBinding.ValueType = 'String'
        Position.ColIndex = 7
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
    end
    object pnlMiddleBottom: TPanel
      Left = 0
      Top = 377
      Width = 1069
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 487
    Width = 1069
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      1069
      42)
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 1069
      Height = 1
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 1076
    end
    object Button2: TButton
      Tag = 520
      Left = 992
      Top = 10
      Width = 74
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnOK: TButton
      Tag = 510
      Left = 904
      Top = 10
      Width = 79
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = btnOKClick
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1069
    Height = 69
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 2
    object Label2: TLabel
      Left = 48
      Top = 40
      Width = 319
      Height = 15
      Caption = 'Please select the line at which the execution should continue'
    end
  end
  object pmnuOptions: TPopupMenu
    OnPopup = pmnuOptionsPopup
    Left = 224
    Top = 209
    object mnuContinueAtThisLine: TMenuItem
      Caption = 'Continue at this line'
      OnClick = mnuContinueAtThisLineClick
    end
    object AdvancedView1: TMenuItem
      AutoCheck = True
      Caption = 'Advanced View'
      OnClick = AdvancedView1Click
    end
  end
end
