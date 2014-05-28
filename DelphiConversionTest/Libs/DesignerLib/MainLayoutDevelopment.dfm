object frmMainLayoutDevelopment: TfrmMainLayoutDevelopment
  Left = 0
  Top = 0
  Caption = 'frmMainLayoutDevelopment'
  ClientHeight = 386
  ClientWidth = 597
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 253
    Top = 0
    Width = 5
    Height = 386
    Color = clBtnFace
    ParentColor = False
  end
  object PageControl1: TPageControl
    Left = 288
    Top = 0
    Width = 309
    Height = 386
    Align = alClient
    DockSite = True
    DragKind = dkDock
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnChange = PageControl1Change
    OnDockOver = PageControl1DockOver
    OnGetSiteInfo = PageControl1GetSiteInfo
    OnMouseDown = PageControl1MouseDown
    OnUnDock = PageControl1UnDock
  end
  object pnLayoutDevelopment: TPanel
    Left = 0
    Top = 0
    Width = 253
    Height = 386
    Align = alLeft
    BevelOuter = bvNone
    DragKind = dkDock
    TabOrder = 1
  end
  object ToolBar1: TToolBar
    Left = 258
    Top = 0
    Width = 30
    Height = 386
    Align = alLeft
    List = True
    TabOrder = 2
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 128
    Top = 56
    object pmnuClose: TMenuItem
      Caption = '&Close Page'
      OnClick = pmnuCloseClick
    end
    object pmnuCloseAllPages: TMenuItem
      Caption = 'Close &All Pages'
      OnClick = pmnuCloseAllPagesClick
    end
  end
end
