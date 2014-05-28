object frmMainDeviceDevelopment: TfrmMainDeviceDevelopment
  Left = 0
  Top = 0
  Caption = 'frmMainDeviceDevelopment'
  ClientHeight = 449
  ClientWidth = 702
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object pnlEditMain: TPanel
    Left = 0
    Top = 0
    Width = 702
    Height = 449
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object SplitterLeft: TSplitter
      Left = 253
      Top = 0
      Width = 5
      Height = 449
      Color = clBtnFace
      ParentColor = False
    end
    object PanelMiddle: TPanel
      Left = 258
      Top = 0
      Width = 444
      Height = 449
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 440
      object PageControl1: TPageControl
        Left = 30
        Top = 0
        Width = 414
        Height = 449
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
        ExplicitWidth = 410
      end
      object ToolBar1: TToolBar
        Left = 0
        Top = 0
        Width = 30
        Height = 449
        Align = alLeft
        List = True
        TabOrder = 1
      end
    end
    object DockPanelLeft: TPanel
      Left = 0
      Top = 0
      Width = 253
      Height = 449
      Align = alLeft
      BevelOuter = bvNone
      DragKind = dkDock
      TabOrder = 1
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
