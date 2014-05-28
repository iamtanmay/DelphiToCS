object frmCompleteMethodImport: TfrmCompleteMethodImport
  Left = 280
  Top = 171
  Caption = 'Complete Method Import'
  ClientHeight = 383
  ClientWidth = 945
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object cxGrid1: TcxGrid
    Left = 0
    Top = 177
    Width = 945
    Height = 206
    Align = alClient
    TabOrder = 0
    object cxGrid1DBTableView1: TcxGridDBTableView
      NavigatorButtons.ConfirmDelete = False
      DataController.DataSource = DataSource1
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.ColumnFiltering = False
      OptionsCustomize.ColumnGrouping = False
      OptionsView.GroupByBox = False
      object cxGrid1DBTableView1DBColumn1: TcxGridDBColumn
        DataBinding.FieldName = 'KEYNAME'
        Width = 108
      end
      object cxGrid1DBTableView1DBColumn2: TcxGridDBColumn
        DataBinding.FieldName = 'ACTION'
        Width = 103
      end
      object cxGrid1DBTableView1DBColumn3: TcxGridDBColumn
        DataBinding.FieldName = 'OPTIONS'
        Width = 237
      end
      object cxGrid1DBTableView1DBColumn4: TcxGridDBColumn
        DataBinding.FieldName = 'COMMENT'
      end
      object cxGrid1DBTableView1DBColumn5: TcxGridDBColumn
        DataBinding.FieldName = 'SCHEDMIN'
      end
      object cxGrid1DBTableView1DBColumn6: TcxGridDBColumn
        DataBinding.FieldName = 'SCHEDMAX'
      end
      object cxGrid1DBTableView1DBColumn7: TcxGridDBColumn
        DataBinding.FieldName = 'RESID'
      end
    end
    object cxGrid1Level1: TcxGridLevel
      GridView = cxGrid1DBTableView1
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 945
    Height = 177
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label3: TLabel
      Tag = 29510
      Left = 16
      Top = 64
      Width = 57
      Height = 15
      Caption = 'ImportFile:'
    end
    object Label1: TLabel
      Left = 8
      Top = 160
      Width = 60
      Height = 15
      Caption = 'Definitions:'
    end
    object cbImportFile: TComboBox
      Left = 96
      Top = 56
      Width = 337
      Height = 23
      TabOrder = 0
      OnChange = cbImportFileChange
    end
    object btnFinish: TButton
      Left = 48
      Top = 88
      Width = 297
      Height = 25
      Caption = 'Finish method Import'
      Enabled = False
      TabOrder = 1
      OnClick = btnFinishClick
    end
    object btnBrowse: TBitBtn
      Tag = 29530
      Left = 446
      Top = 52
      Width = 27
      Height = 25
      Caption = '...'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 2
      OnClick = btnBrowseClick
    end
    object StaticText2: TStaticText
      Left = 16
      Top = 128
      Width = 457
      Height = 17
      AutoSize = False
      BevelKind = bkTile
      Caption = 'StaticText2'
      Color = 8421631
      ParentColor = False
      TabOrder = 3
      Visible = False
    end
    object btnCreateImportFile: TButton
      Left = 48
      Top = 16
      Width = 297
      Height = 25
      Caption = 'Create Import File(s)'
      Default = True
      TabOrder = 4
      OnClick = btnCreateImportFileClick
    end
  end
  object DataSource1: TDataSource
    Left = 480
    Top = 8
  end
end
