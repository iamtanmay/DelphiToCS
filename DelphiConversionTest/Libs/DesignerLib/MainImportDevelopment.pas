{ --------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.02.11 wl                               TN5486   initial revision
  30.06.11 wl                               TN5620   Es lassen sich keine Fenster mehr (einfach so) andocken
  13.03.13 wl                               TN5960   verwendet TMainCustomDevelopment.CreateOverviewInstance
  26.08.13 wl                               TN6236   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit MainImportDevelopment;


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
    TfrmMainImportDevelopment = class(TMainCustomDevelopment)
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
    ViewItem,
    GeneralTypes,
    MethodDataAdaptor,
    MethodEditor;

procedure TfrmMainImportDevelopment.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStarted := true;
    self.Caption := TLanguageString.Read('Import development', 'Import-Entwicklung');
    self.AddStandardToolBar(self.ToolBar1);
end;

function TfrmMainImportDevelopment.GetPageControl: TPageControl;
begin
    result := self.PageControl1;
end;

procedure TfrmMainImportDevelopment.FormShow(Sender: TObject);
begin
    if fStarted then
    begin
        self.CreateOverviewInstance(aemImportDevelopment, self.DockPanelLeft);
    end;

    fStarted := false;
end;

procedure TfrmMainImportDevelopment.PageControl1Change(Sender: TObject);
begin
    RefreshButtons(); // im Page-Control ändert sich die ActivePage
end;

procedure TfrmMainImportDevelopment.PageControl1DockOver(Sender: TObject; Source: TDragDockObject;
    X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
    Accept := (Source.Control is TViewItemEditForm);

    if (Source.Control.HostDockSite = PageControl1) then
        Accept := false;
end;

procedure TfrmMainImportDevelopment.PageControl1GetSiteInfo(Sender: TObject; DockClient: TControl;
    var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
    CanDock := (DockClient is TViewItemEditForm);
end;

procedure TfrmMainImportDevelopment.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    if (Button = mbRight) then // make page active from a right mouse click
        PageControl1.ActivePageIndex := PageControl1.IndexOfTabAt(X, Y);
end;

procedure TfrmMainImportDevelopment.PageControl1UnDock(Sender: TObject; Client: TControl;
    NewTarget: TWinControl; var Allow: Boolean);
begin
    Allow := false; // man darf einfach keine Edit-Fenster aus dem Tab-Control lösen!
end;

procedure TfrmMainImportDevelopment.pmnuCloseAllPagesClick(Sender: TObject);
begin
    self.CloseAllPages();
end;

procedure TfrmMainImportDevelopment.pmnuCloseClick(Sender: TObject);
begin
    CloseCurrentPage();
end;

procedure TfrmMainImportDevelopment.PopupMenu1Popup(Sender: TObject);
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
