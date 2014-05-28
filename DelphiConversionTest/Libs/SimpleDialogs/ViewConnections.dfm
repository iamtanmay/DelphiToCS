object frmViewConnections: TfrmViewConnections
  Left = 407
  Top = 282
  BorderStyle = bsDialog
  Caption = 'Connections'
  ClientHeight = 432
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object cxTreeList1: TcxTreeList
    Left = 0
    Top = 33
    Width = 539
    Height = 355
    Align = alClient
    Bands = <
      item
      end>
    LookAndFeel.NativeStyle = True
    OptionsCustomizing.BandCustomizing = False
    OptionsCustomizing.BandHorzSizing = False
    OptionsCustomizing.BandMoving = False
    OptionsCustomizing.BandVertSizing = False
    OptionsCustomizing.ColumnCustomizing = False
    OptionsCustomizing.ColumnMoving = False
    OptionsCustomizing.ColumnVertSizing = False
    TabOrder = 0
    OnEditing = cxTreeList1Editing
    object TVcxTreeListColumn1: TcxTreeListColumn
      Caption.Text = 'Connections'
      DataBinding.ValueType = 'String'
      Options.Editing = False
      Width = 219
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object TVcxTreeListColumn2: TcxTreeListColumn
      Caption.Text = 'Enabled'
      DataBinding.ValueType = 'Boolean'
      Width = 51
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 539
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btnSelectAll: TButton
      Tag = 42155
      Left = 16
      Top = 5
      Width = 115
      Height = 23
      Caption = 'Select All'
      TabOrder = 0
      OnClick = btnSelectAllClick
    end
    object btnDeselectAll: TButton
      Left = 137
      Top = 5
      Width = 115
      Height = 23
      Caption = 'Deselect All'
      TabOrder = 1
      OnClick = btnDeselectAllClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 388
    Width = 539
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      539
      44)
    object btnOK: TButton
      Tag = 530
      Left = 359
      Top = 11
      Width = 74
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Tag = 540
      Left = 447
      Top = 11
      Width = 74
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
