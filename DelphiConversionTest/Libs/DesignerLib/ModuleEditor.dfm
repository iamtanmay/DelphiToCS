object frmModuleEditor: TfrmModuleEditor
  Left = 131
  Top = 182
  Caption = 'frmModuleEditor'
  ClientHeight = 543
  ClientWidth = 860
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 15
  object pnlGrid: TPanel
    Left = 0
    Top = 0
    Width = 860
    Height = 543
    Align = alClient
    TabOrder = 0
    object cxTreeList1: TcxTreeList
      Left = 1
      Top = 1
      Width = 858
      Height = 541
      Align = alClient
      Bands = <
        item
        end>
      LookAndFeel.Kind = lfUltraFlat
      LookAndFeel.NativeStyle = True
      OptionsBehavior.Sorting = False
      OptionsCustomizing.BandCustomizing = False
      OptionsCustomizing.BandHorzSizing = False
      OptionsCustomizing.BandMoving = False
      OptionsCustomizing.BandVertSizing = False
      OptionsCustomizing.ColumnVertSizing = False
      OptionsView.GridLineColor = clBtnFace
      OptionsView.GridLines = tlglBoth
      OptionsView.PaintStyle = tlpsCategorized
      OptionsView.ShowRoot = False
      PopupMenu = PopupMenu1
      TabOrder = 0
      OnCustomDrawDataCell = cxTreeList1CustomDrawDataCell
      OnEdited = cxTreeList1Edited
      OnEditValueChanged = cxTreeList1EditValueChanged
      OnExit = cxTreeList1Exit
      OnMouseDown = cxTreeList1MouseDown
      object cxTreeList1cxTreeListColumn1: TcxTreeListColumn
        Caption.Text = 'Key'
        DataBinding.ValueType = 'String'
        Options.Customizing = False
        Options.Editing = False
        Options.Focusing = False
        Options.Moving = False
        Options.Sorting = False
        Width = 178
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
        Width = 263
        Position.ColIndex = 1
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetEditingProperties = cxTreeList1cxTreeListColumn2GetEditingProperties
      end
      object cxTreeList1cxTreeListColumn3: TcxTreeListColumn
        Caption.Text = 'Description'
        DataBinding.ValueType = 'String'
        Options.Customizing = False
        Options.Moving = False
        Options.Sorting = False
        Width = 416
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
  end
  object cxEditRepository1: TcxEditRepository
    Left = 496
    Top = 40
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 232
    Top = 224
    object mnuRestoreDefault: TMenuItem
      Caption = 'Restore Default value'
      OnClick = mnuRestoreDefaultClick
    end
  end
end
