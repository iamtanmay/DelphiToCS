object frmLayoutWorkspaceEditor: TfrmLayoutWorkspaceEditor
  Tag = 3000
  Left = 389
  Top = 230
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Layout Workspace Editor'
  ClientHeight = 388
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object pnlBottom: TPanel
    Left = 0
    Top = 347
    Width = 341
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object btnOK: TButton
      Tag = 510
      Left = 156
      Top = 9
      Width = 81
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Tag = 520
      Left = 248
      Top = 9
      Width = 81
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 341
    Height = 347
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object rdoViewType: TRadioGroup
      Tag = 51900
      Left = 8
      Top = 8
      Width = 104
      Height = 61
      Caption = 'rdoViewType'
      Items.Strings = (
        'Def'
        'Cust')
      TabOrder = 0
      OnClick = rdoViewTypeClick
    end
  end
end
