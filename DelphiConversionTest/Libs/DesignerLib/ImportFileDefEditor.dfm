object frmImportFileDefEditor: TfrmImportFileDefEditor
  Tag = 9190
  Left = 100
  Top = 191
  BorderIcons = [biSystemMenu]
  Caption = 'Import File Definition'
  ClientHeight = 389
  ClientWidth = 962
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object pnlTab: TPanel
    Left = 0
    Top = 0
    Width = 446
    Height = 389
    Align = alLeft
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 0
    object pnlFileDefinitionBottom: TPanel
      Left = 0
      Top = 44
      Width = 442
      Height = 341
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object lblPathName: TLabel
        Tag = 9210
        Left = 10
        Top = 15
        Width = 62
        Height = 15
        Caption = 'Path Name:'
      end
      object lblTableName: TLabel
        Tag = 9220
        Left = 10
        Top = 42
        Width = 67
        Height = 15
        Caption = 'Table Name:'
      end
      object lblDelimiter: TLabel
        Tag = 9230
        Left = 10
        Top = 68
        Width = 72
        Height = 15
        Caption = 'List Delimiter:'
      end
      object lblHasHeader: TLabel
        Tag = 9240
        Left = 10
        Top = 95
        Width = 72
        Height = 15
        Caption = 'Header Exists:'
      end
      object lblRowOffset: TLabel
        Tag = 9250
        Left = 10
        Top = 123
        Width = 61
        Height = 15
        Caption = 'Row Offset:'
      end
      object lblFilter: TLabel
        Tag = 9260
        Left = 10
        Top = 150
        Width = 29
        Height = 15
        Caption = 'Filter:'
      end
      object lblSQL: TLabel
        Tag = 9270
        Left = 10
        Top = 204
        Width = 24
        Height = 15
        Caption = 'SQL:'
      end
      object lblUsername: TLabel
        Tag = 9290
        Left = 10
        Top = 232
        Width = 56
        Height = 15
        Caption = 'Username:'
      end
      object lblPassword: TLabel
        Tag = 9300
        Left = 10
        Top = 260
        Width = 53
        Height = 15
        Caption = 'Password:'
      end
      object lblOrderBy: TLabel
        Tag = 9280
        Left = 10
        Top = 177
        Width = 43
        Height = 15
        Caption = 'OrderBy'
      end
      object lblDatatypes: TLabel
        Tag = 9820
        Left = 10
        Top = 288
        Width = 58
        Height = 15
        Caption = 'Data types:'
      end
      object lblSkipLines: TLabel
        Tag = 9245
        Left = 226
        Top = 125
        Width = 52
        Height = 15
        Caption = 'Skip lines:'
      end
      object edPathName: TEdit
        Left = 104
        Top = 11
        Width = 305
        Height = 23
        TabOrder = 0
        OnChange = ChangeAnyControl
      end
      object btnPathName: TButton
        Left = 409
        Top = 9
        Width = 25
        Height = 25
        Caption = '...'
        TabOrder = 1
        OnClick = btnPathNameClick
      end
      object cmbTableName: TComboBox
        Left = 105
        Top = 38
        Width = 177
        Height = 23
        TabOrder = 2
        OnChange = ChangeAnyControl
        OnDropDown = cmbTableNameDropDown
      end
      object cmbDelimiter: TComboBox
        Left = 105
        Top = 65
        Width = 67
        Height = 23
        TabOrder = 3
        OnChange = ChangeAnyControl
        Items.Strings = (
          ';'
          ',')
      end
      object chkHeader: TCheckBox
        Left = 105
        Top = 94
        Width = 17
        Height = 17
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
        TabOrder = 4
        OnClick = ChangeAnyControl
      end
      object edRowOffset: TEdit
        Left = 107
        Top = 117
        Width = 65
        Height = 23
        TabOrder = 5
        Text = '1'
        OnChange = edRowOffsetChange
      end
      object edFilter: TEdit
        Left = 105
        Top = 146
        Width = 304
        Height = 23
        TabOrder = 6
        OnChange = ChangeAnyControl
      end
      object edSQL: TEdit
        Left = 105
        Top = 200
        Width = 304
        Height = 23
        TabOrder = 7
        OnChange = ChangeAnyControl
      end
      object edUsername: TEdit
        Left = 105
        Top = 228
        Width = 177
        Height = 23
        TabOrder = 8
        OnChange = ChangeAnyControl
      end
      object edPassword: TEdit
        Left = 105
        Top = 256
        Width = 177
        Height = 23
        PasswordChar = '*'
        TabOrder = 9
        OnChange = ChangeAnyControl
      end
      object btnSQL: TButton
        Left = 409
        Top = 198
        Width = 27
        Height = 25
        Caption = 'SQL'
        TabOrder = 10
        OnClick = btnSQLClick
      end
      object edOrderBy: TEdit
        Left = 105
        Top = 173
        Width = 304
        Height = 23
        TabOrder = 11
        OnChange = ChangeAnyControl
      end
      object btnChangeDatatypes: TButton
        Tag = 9805
        Left = 96
        Top = 284
        Width = 80
        Height = 25
        Caption = 'Change'
        TabOrder = 12
        OnClick = btnChangeDatatypesClick
      end
      object chkDatatypes: TCheckBox
        Tag = 9800
        Left = 272
        Top = 288
        Width = 169
        Height = 17
        Caption = 'use changed'
        Enabled = False
        TabOrder = 13
        OnClick = ChangeAnyControl
      end
      object btnResetDatatypes: TButton
        Tag = 9815
        Left = 184
        Top = 284
        Width = 80
        Height = 25
        Caption = 'zur'#252'cksetzen'
        TabOrder = 14
        OnClick = btnResetDatatypesClick
      end
      object edSkipLines: TEdit
        Left = 344
        Top = 117
        Width = 65
        Height = 23
        TabOrder = 15
        Text = '0'
        OnChange = ChangeAnyControl
      end
    end
    object pnlFileDefinitionButtons: TPanel
      Left = 0
      Top = 0
      Width = 442
      Height = 44
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object sbtnUseSQL: TSpeedButton
        Tag = 9390
        Left = 284
        Top = 13
        Width = 92
        Height = 25
        AllowAllUp = True
        GroupIndex = 2
        Caption = 'SQL'
        OnClick = sbtnUseSQLClick
      end
      object sbtnASCIIMode: TSpeedButton
        Tag = 8010
        Left = 2
        Top = 13
        Width = 92
        Height = 25
        AllowAllUp = True
        GroupIndex = 2
        Caption = 'ASCII'
        OnClick = sbtnASCIIModeClick
      end
      object sbtnExcelMode: TSpeedButton
        Left = 96
        Top = 13
        Width = 92
        Height = 25
        AllowAllUp = True
        GroupIndex = 2
        Caption = 'Excel'
        OnClick = sbtnExcelModeClick
      end
      object sbtnAccessMode: TSpeedButton
        Left = 190
        Top = 13
        Width = 92
        Height = 25
        AllowAllUp = True
        GroupIndex = 2
        Caption = 'Access'
        OnClick = sbtnAccessModeClick
      end
    end
  end
  object pnlPreview: TPanel
    Left = 446
    Top = 0
    Width = 516
    Height = 389
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlPreview'
    TabOrder = 1
    object pnlPreviewBottom: TPanel
      Left = 0
      Top = 0
      Width = 516
      Height = 86
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object btnPreview: TButton
        Tag = 9330
        Left = 11
        Top = 9
        Width = 100
        Height = 25
        Caption = 'Preview'
        TabOrder = 1
        OnClick = btnPreviewClick
      end
      object rgrpPreviewColResize: TRadioGroup
        Tag = 9380
        Left = 11
        Top = 40
        Width = 251
        Height = 39
        Caption = 'Optimize Column Size'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Use header'
          'Ignore header')
        TabOrder = 0
        OnClick = rgrpPreviewColResizeClick
      end
    end
    object pgctlPreview: TPageControl
      Left = 0
      Top = 86
      Width = 516
      Height = 303
      ActivePage = shtPreviewRecords
      Align = alClient
      TabOrder = 1
      TabStop = False
      OnChange = pgctlPreviewChange
      object shtPreviewRecords: TTabSheet
        Tag = 14200
        Caption = 'Records'
        object sgPreviewRecords: TStringGrid
          Left = 0
          Top = 0
          Width = 508
          Height = 273
          TabStop = False
          Align = alClient
          ColCount = 1
          DefaultRowHeight = 16
          FixedCols = 0
          RowCount = 1
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
          TabOrder = 0
        end
      end
      object shtPreviewFields: TTabSheet
        Tag = 14210
        Caption = 'Fields'
        ImageIndex = 1
        object sgPreviewFields: TStringGrid
          Left = 0
          Top = 0
          Width = 508
          Height = 273
          TabStop = False
          Align = alClient
          ColCount = 1
          DefaultRowHeight = 16
          FixedCols = 0
          RowCount = 1
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
          TabOrder = 0
        end
      end
    end
  end
  object opdlgPathName: TOpenDialog
    Left = 360
    Top = 83
  end
end
