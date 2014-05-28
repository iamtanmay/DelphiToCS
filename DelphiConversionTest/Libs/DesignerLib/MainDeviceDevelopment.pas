{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  02.06.10 wl                                    TN5116   initial revision
  09.06.10 wl                                    TN5116   ViewConnections ist jetzt fester Bestandteil
  11.11.10 wl                                    TN5211   ViewConnections wieder entfernt
  03.12.10 pk                                    TN5381   now inherits from TMainCustomDevelopment
  30.06.11 wl                                    TN5620   Es lassen sich keine Fenster mehr (einfach so) andocken
  13.03.13 wl                                    TN5960   verwendet TMainCustomDevelopment.CreateOverviewInstance
  26.08.13 wl                                    TN6236   uses geändert
  ------------------------------------------------------------------------------------------------------------ }

unit MainDeviceDevelopment;


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
    TfrmMainDeviceDevelopment = class(TMainCustomDevelopment)
        PopupMenu1: TPopupMenu;
        pmnuClose: TMenuItem;
        pmnuCloseAllPages: TMenuItem;
        pnlEditMain: TPanel;
        SplitterLeft: TSplitter;
        PanelMiddle: TPanel;
        PageControl1: TPageControl;
        DockPanelLeft: TPanel;
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
    ViewItem,
    MethodDataAdaptor,
    MethodEditor;

procedure TfrmMainDeviceDevelopment.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStarted := true;
    self.Caption := TLanguageString.Read('Device development', 'Device-Entwicklung');
    self.AddStandardToolBar(self.ToolBar1);
end;

function TfrmMainDeviceDevelopment.GetPageControl: TPageControl;
begin
    result := self.PageControl1;
end;

procedure TfrmMainDeviceDevelopment.FormShow(Sender: TObject);
begin
    if fStarted then
    begin
        self.CreateOverviewInstance(aemDeviceDevelopment, self.DockPanelLeft);
    end;

    fStarted := false;
end;

procedure TfrmMainDeviceDevelopment.PageControl1Change(Sender: TObject);
begin
    RefreshButtons(); // im Page-Control ändert sich die ActivePage
end;

procedure TfrmMainDeviceDevelopment.PageControl1DockOver(Sender: TObject; Source: TDragDockObject;
    X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
    Accept := (Source.Control is TViewItemEditForm);

    if (Source.Control.HostDockSite = PageControl1) then
        Accept := false;
end;

procedure TfrmMainDeviceDevelopment.PageControl1GetSiteInfo(Sender: TObject; DockClient: TControl;
    var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
    CanDock := (DockClient is TViewItemEditForm);
end;

procedure TfrmMainDeviceDevelopment.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    if (Button = mbRight) then // make page active from a right mouse click
        PageControl1.ActivePageIndex := PageControl1.IndexOfTabAt(X, Y);
end;

procedure TfrmMainDeviceDevelopment.PageControl1UnDock(Sender: TObject; Client: TControl;
    NewTarget: TWinControl; var Allow: Boolean);
begin
    Allow := false; // man darf einfach keine Edit-Fenster aus dem Tab-Control lösen!
end;

procedure TfrmMainDeviceDevelopment.pmnuCloseAllPagesClick(Sender: TObject);
begin
    self.CloseAllPages();
end;

procedure TfrmMainDeviceDevelopment.pmnuCloseClick(Sender: TObject);
begin
    CloseCurrentPage();
end;

procedure TfrmMainDeviceDevelopment.PopupMenu1Popup(Sender: TObject);
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
