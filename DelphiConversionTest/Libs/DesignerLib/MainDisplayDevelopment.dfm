object frmMainDisplayDevelopment: TfrmMainDisplayDevelopment
  Left = 0
  Top = 0
  Caption = 'frmMainDisplayDevelopment'
  ClientHeight = 391
  ClientWidth = 852
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
  object pnlEditMain: TPanel
    Left = 0
    Top = 0
    Width = 852
    Height = 391
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object SplitterLeft: TSplitter
      Left = 253
      Top = 0
      Width = 5
      Height = 387
      Color = clBtnFace
      ParentColor = False
    end
    object SplitterBottom: TSplitter
      Left = 0
      Top = 387
      Width = 852
      Height = 4
      Cursor = crVSplit
      Align = alBottom
      Visible = False
      ExplicitLeft = 1
      ExplicitTop = 388
      ExplicitWidth = 843
    end
    object PanelMiddle: TPanel
      Left = 258
      Top = 0
      Width = 594
      Height = 387
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object SplitterTop: TSplitter
        Left = 0
        Top = 191
        Width = 594
        Height = 4
        Cursor = crVSplit
        Align = alTop
      end
      object pnlRightBottom: TPanel
        Left = 0
        Top = 195
        Width = 594
        Height = 192
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitTop = 194
        ExplicitHeight = 193
        object PageControl1: TPageControl
          Left = 30
          Top = 0
          Width = 564
          Height = 192
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
          ExplicitHeight = 193
        end
        object ToolBar1: TToolBar
          Left = 0
          Top = 0
          Width = 30
          Height = 192
          Align = alLeft
          List = True
          TabOrder = 1
          ExplicitHeight = 193
        end
      end
      object PanelTop: TPanel
        Left = 0
        Top = 0
        Width = 594
        Height = 191
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        TabOrder = 1
        OnResize = PanelTopResize
      end
    end
    object DockPanelLeft: TPanel
      Left = 0
      Top = 0
      Width = 253
      Height = 387
      Align = alLeft
      BevelOuter = bvNone
      DragKind = dkDock
      TabOrder = 1
    end
    object DockPanelBottom: TPanel
      Left = 0
      Top = 387
      Width = 852
      Height = 0
      Align = alBottom
      BevelOuter = bvNone
      DockSite = True
      TabOrder = 2
    end
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
