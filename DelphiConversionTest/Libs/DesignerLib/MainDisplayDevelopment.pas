{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  09.06.10 wl                                    TN5116   initial revision
  03.12.10 pk                                    TN5381   now inherits from TMainCustomDevelopment
  30.06.11 wl                                    TN5620   Es lassen sich keine Fenster mehr (einfach so) andocken
  13.03.13 wl                                    TN5960   verwendet TMainCustomDevelopment.CreateOverviewInstance
  26.08.13 wl                                    TN6236   uses geändert
  ------------------------------------------------------------------------------------------------------------ }

unit MainDisplayDevelopment;


interface


uses
    Forms,
    ImgList,
    Controls,
    Buttons,
    ToolWin,
    ComCtrls,
    ExtCtrls,
    Classes,
    cxEditRepositoryItems,
    cxEdit,
    Menus,
    DockableForm,
    ViewItemEditForm,
    Types,
    Dialogs,
    MainCustomDevelopment;

type
    TfrmMainDisplayDevelopment = class(TMainCustomDevelopment)
        PopupMenu1: TPopupMenu;
        pmnuClose: TMenuItem;
        pmnuCloseAllPages: TMenuItem;
        pnlEditMain: TPanel;
        SplitterLeft: TSplitter;
        SplitterBottom: TSplitter;
        PanelMiddle: TPanel;
        DockPanelLeft: TPanel;
        DockPanelBottom: TPanel;
        pnlRightBottom: TPanel;
        PageControl1: TPageControl;
        PanelTop: TPanel;
        SplitterTop: TSplitter;
        ToolBar1: TToolBar;
        procedure FormCreate(Sender: TObject);
        procedure pmnuCloseClick(Sender: TObject);
        procedure PopupMenu1Popup(Sender: TObject);
        procedure PageControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure PageControl1GetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
            MousePos: TPoint; var CanDock: Boolean);
        procedure PageControl1DockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
        procedure PageControl1UnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl;
            var Allow: Boolean);
        procedure PageControl1Change(Sender: TObject);
        procedure pmnuCloseAllPagesClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure PanelTopResize(Sender: TObject);

    private
        fStarted: boolean;
    protected
        function GetPageControl: TPageControl; override;
    end;


implementation


{$R *.dfm}

uses
    ControlUtils,
    AppSettings,
    GeneralTypes,
    MethodDataAdaptor,
    MethodEditor,
    ViewItem,
    ViewDisplayComponentPreview;

procedure TfrmMainDisplayDevelopment.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStarted := true;
    self.Caption := TLanguageString.Read('Display development', 'Display-Entwicklung');
    self.AddStandardToolBar(self.ToolBar1);
end;

function TfrmMainDisplayDevelopment.GetPageControl: TPageControl;
begin
    result := self.PageControl1;
end;

procedure TfrmMainDisplayDevelopment.FormDestroy(Sender: TObject);
begin
    TfrmViewDisplayComponentPreview.DestoryInstance();
end;

procedure TfrmMainDisplayDevelopment.FormShow(Sender: TObject);
begin
    if fStarted then
    begin
        self.CreateOverviewInstance(aemDisplayDevelopment, self.DockPanelLeft);

        TfrmViewDisplayComponentPreview.CreateInstance(Application);
        TfrmViewDisplayComponentPreview.Instance.Parent := PanelTop;
        TfrmViewDisplayComponentPreview.Instance.Align := alClient;
        TfrmViewDisplayComponentPreview.Instance.BorderStyle := bsNone;
        TfrmViewDisplayComponentPreview.Instance.Visible := true;
    end;

    fStarted := false;
end;

procedure TfrmMainDisplayDevelopment.PageControl1Change(Sender: TObject);
begin
    RefreshButtons(); // im Page-Control ändert sich die ActivePage
end;

procedure TfrmMainDisplayDevelopment.PageControl1DockOver(Sender: TObject; Source: TDragDockObject;
    X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
    Accept := (Source.Control is TViewItemEditForm);

    if (Source.Control.HostDockSite = PageControl1) then
        Accept := false;
end;

procedure TfrmMainDisplayDevelopment.PageControl1GetSiteInfo(Sender: TObject; DockClient: TControl;
    var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
    CanDock := (DockClient is TViewItemEditForm);
end;

procedure TfrmMainDisplayDevelopment.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    if (Button = mbRight) then // make page active from a right mouse click
        PageControl1.ActivePageIndex := PageControl1.IndexOfTabAt(X, Y);
end;

procedure TfrmMainDisplayDevelopment.PageControl1UnDock(Sender: TObject; Client: TControl;
    NewTarget: TWinControl; var Allow: Boolean);
begin
    Allow := false; // man darf einfach keine Edit-Fenster aus dem Tab-Control lösen!
end;

procedure TfrmMainDisplayDevelopment.PanelTopResize(Sender: TObject);
begin
    if self.PanelTop.Height <= 30 then
        self.PanelTop.Height := 30;
end;

procedure TfrmMainDisplayDevelopment.pmnuCloseAllPagesClick(Sender: TObject);
begin
    self.CloseAllPages();
end;

procedure TfrmMainDisplayDevelopment.pmnuCloseClick(Sender: TObject);
begin
    CloseCurrentPage();
end;

procedure TfrmMainDisplayDevelopment.PopupMenu1Popup(Sender: TObject);
var
    xIndex: integer;
begin
    xIndex := PageControl1.ActivePageIndex;
    if (xIndex < 0) then
        EXIT;
    pmnuClose.Caption := TLanguageString.Read('Close Page: {0}', 'Seite schließen: {0}',
        [PageControl1.Pages[xIndex].Caption]);
    pmnuCloseAllPages.Caption := TLanguageString.Read('Close All Pages', 'Alle Seiten schließen');
end;


end.
