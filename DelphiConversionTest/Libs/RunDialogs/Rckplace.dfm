object RackPlaceList: TRackPlaceList
  Left = 246
  Top = 262
  Caption = 'Rack Placement List'
  ClientHeight = 207
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFF
    FFFFFFFFFAAAAAAFFFFFFFFFFFFFFFFFFFFFFFAAAFFFFFFAAAFFFFFFFFFFFFFF
    FFFFAAFFFFFFFFFFFFAAFFFFFFFFFFFFFFFAFFFFFFAAAAFFFFFFAFFFFFFFFFFF
    FAAFFFFAAAFFFFAAAFFFFAAFFFFFFFFFAFFFFAAFFFFFFFFFFAFFFFFAFFFFFFFF
    AFFAAFFFFFFFFFFFFFFAAFFAFFFFFFFAFFAFFFFF00000000FFFFFAFFAFFFFFAF
    FAFFFFAA0FFFFFF0FAFFFFFFFAFFFAFFFFFFFFFF0FFFFFF0FFAFFFAFFFAFFAFF
    AFFFFAFF0FFFFFF0FFFFFFFAFFAFFAFFAFFFFFFF0FFFFFF0FFFAFFFAFFAFAFFF
    AFFFAFFF0FFFFFF0FFFFFFFAFFFAAFFAFFFFFFFF0FFFFFF0AFFFAFFFAFFAAFFA
    FFFAFFAF0FFFFFF0FFFFAFFFAFFAAFFAFFFAFFAF0FFFFFF0FAFFAFFFAFFAAFFA
    FFFAFFAF0FFFFFF0FAFFAFFFAFFAAFFAFFFAFFFA0FFFFFF0FFFFAFFFAFFAAFFA
    FFFFAFFF0FFFFFF0FFFFAFFFAFFAAFFFAFFFAFFF0FFFFFF0FFFFFFFAFFFAFAFF
    AFFFFAFF00FFFF00FFFAFFFAFFAFFAFFAFFFFFFFF000000FFFAFFFFAFFAFFAFF
    FFFFFFAAFFFFFFFFFAFFFFAFFFAFFFAFFAFFFFFA00000000AFFFFFFFFAFFFFFA
    FFAFFFFF00000000FFFFFAFFAFFFFFFFAFFAAFFFFFFFFFFFFFFAAFFAFFFFFFFF
    AFFFFAFFFFFFFFFFFFAFFFFAFFFFFFFFFAAFFFFAAAFFFFAAAFFFFAAFFFFFFFFF
    FFFAFFFFFFAAAAFFFFFFAFFFFFFFFFFFFFFFAAFFFFFFFFFFFFAAFFFFFFFFFFFF
    FFFFFFAAAFFFFFFAAAFFFFFFFFFFFFFFFFFFFFFFFAAAAAAFFFFFFFFFFFFF0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object RackPlaceListView: TListView
    Left = 0
    Top = 41
    Width = 660
    Height = 126
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        Caption = 'caption'
        Width = 0
      end
      item
        Caption = '49220'
        Width = 150
      end
      item
        Alignment = taCenter
        Caption = '49230'
        Width = 55
      end
      item
        Caption = '49240'
        Width = 180
      end
      item
        Caption = '49250'
        Width = 120
      end
      item
        Caption = '49260'
        Width = 120
      end>
    ColumnClick = False
    HotTrack = True
    ReadOnly = True
    RowSelect = True
    SortType = stText
    TabOrder = 0
    TabStop = False
    ViewStyle = vsReport
    OnChanging = RackPlaceListViewChanging
    OnDblClick = RackPlaceListViewDblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 167
    Width = 660
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      660
      40)
    object btnCancel: TButton
      Tag = 520
      Left = 551
      Top = 8
      Width = 90
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Abort'
      ModalResult = 2
      TabOrder = 0
    end
    object btnOK: TButton
      Tag = 510
      Left = 435
      Top = 8
      Width = 90
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Continue'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 660
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    object Label1: TLabel
      Tag = 49010
      Left = 16
      Top = 13
      Width = 61
      Height = 15
      Caption = 'Place Racks'
    end
  end
end
