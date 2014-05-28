object GetMethDlg: TGetMethDlg
  Left = 159
  Top = 69
  ActiveControl = ListView1
  Caption = 'Select Method'
  ClientHeight = 300
  ClientWidth = 496
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 263
    Width = 496
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      496
      37)
    object btnStart: TButton
      Left = 394
      Top = 6
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Start'
      TabOrder = 0
      OnClick = sbStartSamplerClick
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 496
    Height = 263
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object ListView1: TListView
      Left = 0
      Top = 48
      Width = 496
      Height = 215
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      Columns = <
        item
          Caption = 'Method'
          Width = 170
        end
        item
          Caption = 'Layout'
          Width = 150
        end>
      HotTrack = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnColumnClick = ListView1ColumnClick
      OnDblClick = ListView1DblClick
      OnKeyDown = ListView1KeyDown
      OnSelectItem = ListView1SelectItem
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 496
      Height = 48
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
      object lblTitle: TLabel
        Left = 22
        Top = 12
        Width = 58
        Height = 20
        Caption = 'Method:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI Semibold'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label3: TLabel
        Left = 92
        Top = 12
        Width = 93
        Height = 20
        Caption = 'Method name'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
    end
  end
end
