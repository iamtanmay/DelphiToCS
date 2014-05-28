object frmRunInformation: TfrmRunInformation
  Left = 267
  Top = 411
  Align = alClient
  BorderStyle = bsNone
  ClientHeight = 283
  ClientWidth = 579
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 15
  object cxTreeList1: TcxTreeList
    Left = 0
    Top = 0
    Width = 579
    Height = 283
    Align = alClient
    Bands = <
      item
      end>
    OptionsData.Editing = False
    OptionsData.Deleting = False
    OptionsSelection.CellSelect = False
    OptionsView.CellAutoHeight = True
    OptionsView.ColumnAutoWidth = True
    TabOrder = 0
    object cxTreeList1cxTreeListColumn1: TcxTreeListColumn
      Caption.Text = 'Ident'
      DataBinding.ValueType = 'String'
      Width = 170
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxTreeList1cxTreeListColumn2: TcxTreeListColumn
      Caption.Text = 'Ereignis'
      DataBinding.ValueType = 'String'
      Width = 283
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxTreeList1cxTreeListColumn3: TcxTreeListColumn
      Caption.Text = 'Zeit'
      DataBinding.ValueType = 'String'
      Width = 122
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
end
