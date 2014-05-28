object frmMainMethodDevelopment: TfrmMainMethodDevelopment
  Left = 0
  Top = 0
  Caption = 'frmMainMethodDevelopment'
  ClientHeight = 412
  ClientWidth = 833
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 253
    Top = 0
    Width = 5
    Height = 412
    Color = clBtnFace
    ParentColor = False
    ExplicitHeight = 422
  end
  object pnlEditMain: TPanel
    Left = 258
    Top = 0
    Width = 575
    Height = 412
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object SplitterBottom: TSplitter
      Left = 0
      Top = 407
      Width = 575
      Height = 4
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = 1
      ExplicitTop = 388
      ExplicitWidth = 843
    end
    object PanelMiddle: TPanel
      Left = 0
      Top = 0
      Width = 575
      Height = 407
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object SplitterTop: TSplitter
        Left = 0
        Top = 30
        Width = 575
        Height = 4
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 1
        ExplicitWidth = 835
      end
      object PageControl1: TPageControl
        Left = 30
        Top = 34
        Width = 545
        Height = 373
        Align = alClient
        DockSite = True
        PopupMenu = PopupMenu1
        TabOrder = 0
        OnChange = PageControl1Change
        OnDockOver = PageControl1DockOver
        OnGetSiteInfo = PageControl1GetSiteInfo
        OnMouseDown = PageControl1MouseDown
        OnUnDock = PageControl1UnDock
      end
      object PanelTop: TPanel
        Left = 0
        Top = 0
        Width = 575
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        OnResize = PanelTopResize
      end
      object ToolBar1: TToolBar
        Left = 0
        Top = 34
        Width = 30
        Height = 373
        Align = alLeft
        List = True
        TabOrder = 2
      end
    end
    object PageControl2: TPageControl
      Left = 0
      Top = 411
      Width = 575
      Height = 1
      Align = alBottom
      DockSite = True
      PopupMenu = PopupMenu2
      TabOrder = 1
      OnDockOver = PageControl2DockOver
      OnGetSiteInfo = PageControl2GetSiteInfo
      OnMouseDown = PageControl2MouseDown
      OnUnDock = PageControl2UnDock
    end
  end
  object pnAllItems: TPanel
    Left = 0
    Top = 0
    Width = 253
    Height = 412
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 128
    Top = 56
    object pmnuClose: TMenuItem
      Caption = '&Close Page'
      ShortCut = 16499
      OnClick = pmnuCloseClick
    end
    object pmnuCloseAllPages: TMenuItem
      Caption = 'Close &All Pages'
      OnClick = pmnuCloseAllPagesClick
    end
  end
  object PopupMenu2: TPopupMenu
    OnPopup = PopupMenu2Popup
    Left = 128
    Top = 120
    object pmnuCloseBottomPage: TMenuItem
      Caption = '&Close Page'
      ShortCut = 16499
      OnClick = pmnuCloseBottomPageClick
    end
  end
end
