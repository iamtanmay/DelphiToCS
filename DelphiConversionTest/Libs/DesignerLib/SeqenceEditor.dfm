object frmSequenceEditor: TfrmSequenceEditor
  Left = 226
  Top = 146
  Caption = 'Edit Sequence (Reaktionblockformat)'
  ClientHeight = 460
  ClientWidth = 721
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 15
  object Panel3: TPanel
    Left = 0
    Top = 49
    Width = 721
    Height = 411
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 721
      Height = 411
      Align = alClient
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 721
    Height = 49
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Label3: TLabel
      Tag = 52630
      Left = 144
      Top = 14
      Width = 51
      Height = 15
      Caption = 'Columns:'
    end
    object Label4: TLabel
      Tag = 52640
      Left = 16
      Top = 16
      Width = 31
      Height = 15
      Caption = 'Rows:'
    end
    object spinCols: TSpinEdit
      Left = 200
      Top = 10
      Width = 50
      Height = 24
      MaxValue = 32000
      MinValue = 1
      TabOrder = 0
      Value = 12
    end
    object spinRows: TSpinEdit
      Left = 72
      Top = 12
      Width = 50
      Height = 24
      MaxValue = 32000
      MinValue = 1
      TabOrder = 1
      Value = 8
    end
    object btAddLevel: TButton
      Tag = 52690
      Left = 292
      Top = 10
      Width = 129
      Height = 24
      Caption = 'Add Level'
      TabOrder = 2
      OnClick = btAddLevelClick
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 216
    Top = 121
    object mnuSetToValue: TMenuItem
      Caption = 'Set to value'
      OnClick = mnuSetToValueClick
    end
    object pmnuDelete: TMenuItem
      Caption = 'Delete selected positions'
      OnClick = pmnuDeleteClick
    end
    object pmnuEdit: TMenuItem
      Caption = 'Edit single position'
      OnClick = pmnuEditClick
    end
  end
end
