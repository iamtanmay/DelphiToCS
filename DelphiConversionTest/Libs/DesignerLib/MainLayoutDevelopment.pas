{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  25.05.10 wl                                    TN5116   initial revision
  26.05.10 wl                                    TN5116   Caption hinzugefügt
  02.06.10 wl                                    TN5116   Funktionen hinzugefügt
  07.06.10 wl                                    TN5116   Layout editieren vorbereiten
  09.06.10 wl                                    TN5116   uses geändert
  03.12.10 pk                                    TN5381   now inherits from TMainCustomDevelopment
  30.06.11 wl                                    TN5620   Es lassen sich keine Fenster mehr (einfach so) andocken
  13.03.13 wl                                    TN5960   verwendet TMainCustomDevelopment.CreateOverviewInstance
  ------------------------------------------------------------------------------------------------------------ }

unit MainLayoutDevelopment;


interface


uses
    Forms,
    Controls,
    ExtCtrls,
    ComCtrls,
    Classes,
    Types,
    DockableForm,
    ViewItemEditForm,
    Menus,
    MainCustomDevelopment,
    ToolWin;

type
    TfrmMainLayoutDevelopment = class(TMainCustomDevelopment)
        PageControl1: TPageControl;
        Splitter1: TSplitter;
        pnLayoutDevelopment: TPanel;
        PopupMenu1: TPopupMenu;
        pmnuClose: TMenuItem;
        pmnuCloseAllPages: TMenuItem;
        ToolBar1: TToolBar;
        procedure FormCreate(Sender: TObject);
        procedure pmnuCloseClick(Sender: TObject);
        procedure pmnuCloseAllPagesClick(Sender: TObject);
        procedure PopupMenu1Popup(Sender: TObject);
        procedure PageControl1Change(Sender: TObject);
        procedure PageControl1DockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
        procedure PageControl1GetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
            MousePos: TPoint; var CanDock: Boolean);
        procedure PageControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure PageControl1UnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl;
            var Allow: Boolean);

    protected
        function GetPageControl: TPageControl; override;
    end;


implementation


{$R *.dfm}

uses
    GeneralTypes,
    ControlUtils,
    AppSettings,
    ViewItem;

{ TfrmMainLayoutDevelopment }

procedure TfrmMainLayoutDevelopment.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    self.Caption := TLanguageString.Read('Layout development', 'Layout-Entwicklung');

    self.CreateOverviewInstance(aemLayoutDevelopment, pnLayoutDevelopment);
    self.AddStandardToolBar(self.ToolBar1);
end;

function TfrmMainLayoutDevelopment.GetPageControl: TPageControl;
begin
    result := self.PageControl1;
end;

procedure TfrmMainLayoutDevelopment.PageControl1Change(Sender: TObject);
begin
    RefreshButtons(); // im Page-Control ändert sich die ActivePage
end;

procedure TfrmMainLayoutDevelopment.PageControl1DockOver(Sender: TObject; Source: TDragDockObject;
    X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
    Accept := (Source.Control is TViewItemEditForm);

    if (Source.Control.HostDockSite = PageControl1) then
        Accept := false;
end;

procedure TfrmMainLayoutDevelopment.PageControl1GetSiteInfo(Sender: TObject; DockClient: TControl;
    var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
    CanDock := (DockClient is TViewItemEditForm);
end;

procedure TfrmMainLayoutDevelopment.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    if (Button = mbRight) then // make page active from a right mouse click
        PageControl1.ActivePageIndex := PageControl1.IndexOfTabAt(X, Y);
end;

procedure TfrmMainLayoutDevelopment.PageControl1UnDock(Sender: TObject; Client: TControl;
    NewTarget: TWinControl; var Allow: Boolean);
begin
    Allow := false; // man darf einfach keine Edit-Fenster aus dem Tab-Control lösen!
end;

procedure TfrmMainLayoutDevelopment.pmnuCloseAllPagesClick(Sender: TObject);
begin
    self.CloseAllPages();
end;

procedure TfrmMainLayoutDevelopment.pmnuCloseClick(Sender: TObject);
begin
    CloseCurrentPage();
end;

procedure TfrmMainLayoutDevelopment.PopupMenu1Popup(Sender: TObject);
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
