object frmSQLTermEditor: TfrmSQLTermEditor
  Left = 220
  Top = 241
  Caption = 'SQL Term Editor'
  ClientHeight = 502
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 347
    Width = 688
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 351
    ExplicitWidth = 696
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 347
    Align = alClient
    TabOrder = 0
    object Panel3: TPanel
      Left = 1
      Top = 252
      Width = 686
      Height = 94
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 0
        Top = 3
        Width = 66
        Height = 15
        Align = alLeft
        Caption = ' Comment:  '
      end
      object Splitter2: TSplitter
        Left = 0
        Top = 0
        Width = 686
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ExplicitWidth = 694
      end
      object Memo2: TMemo
        Left = 66
        Top = 3
        Width = 620
        Height = 91
        Align = alClient
        TabOrder = 0
        OnChange = Memo2Change
      end
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 686
      Height = 251
      Align = alClient
      TabOrder = 1
      object Panel2: TPanel
        Left = 507
        Top = 1
        Width = 178
        Height = 249
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object sbExecuteQuery: TSpeedButton
          Left = 16
          Top = 72
          Width = 25
          Height = 25
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000C30E0000C30E00000000000000000000D8E9ECD8E9EC
            D8E9ECD8E9ECD8E9EC000000808080D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
            ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC80808000000080
            8080D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
            D8E9ECD8E9ECD8E9ECD8E9EC000000000000808080D8E9ECD8E9ECD8E9ECD8E9
            ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00000000
            FFFF000000808080D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
            D8E9ECD8E9EC00000000000000000000000000FFFF000000808080D8E9ECD8E9
            ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00000000FFFF00FFFFFF
            FFFF00FFFF00FFFF000000808080D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
            D8E9ECD8E9ECD8E9EC000000FFFFFF00FFFF000000000000000000000000D8E9
            ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00000000FFFFFF
            FFFF00FFFF000000808080D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
            00000000000000000000000000000000FFFFFFFFFF00FFFF000000808080D8E9
            ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC000000FFFFFF00FFFFFFFFFF00FFFFFF
            FFFF00FFFFFFFFFF00FFFF000000808080D8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
            D8E9EC000000FFFFFF00FFFFFFFFFF00FFFF000000000000000000000000D8E9
            ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00000000FFFFFFFFFF00FFFFFF
            FFFF00FFFF000000808080D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
            D8E9ECD8E9EC00000000FFFFFFFFFF00FFFFFFFFFF00FFFF000000808080D8E9
            ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC000000FFFFFFFFFFFFFF
            FFFF00FFFFFFFFFFFFFFFF000000808080D8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
            D8E9ECD8E9ECD8E9EC000000FFFFFF00FFFFFFFFFFFFFFFF00FFFFFFFFFF0000
            00808080D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00000000000000
            0000000000000000000000000000000000000000D8E9ECD8E9EC}
          OnClick = sbExecuteQueryClick
        end
        object rgQueryType: TRadioGroup
          Left = 16
          Top = 8
          Width = 113
          Height = 57
          Caption = 'Type'
          Enabled = False
          Items.Strings = (
            'SELECT'
            'CHANGE DATA')
          TabOrder = 0
        end
        object StringGrid1: TStringGrid
          Left = 0
          Top = 104
          Width = 139
          Height = 105
          TabStop = False
          ColCount = 1
          DefaultRowHeight = 16
          FixedCols = 0
          RowCount = 1
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
          TabOrder = 1
          Visible = False
        end
      end
      object Memo1: TMemo
        Left = 1
        Top = 1
        Width = 506
        Height = 249
        Align = alClient
        TabOrder = 1
        OnChange = Memo1Change
      end
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 350
    Width = 688
    Height = 152
    Align = alBottom
    TabOrder = 1
    object sgPreviewRecords: TStringGrid
      Left = 1
      Top = 1
      Width = 686
      Height = 150
      TabStop = False
      Align = alClient
      ColCount = 1
      DefaultRowHeight = 16
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
      TabOrder = 0
      Visible = False
    end
  end
end
