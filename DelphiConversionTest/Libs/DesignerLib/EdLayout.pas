{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  : Dockable Layout Form
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  06.08.04 wl                               TN2008.2  initial version
  17.08.04 wl                               TN2008.2  jetzt als singleton
  24.08.04 wl                               TN2008.2  mit Laden und Speichern der DockableForms
  24.11.05 pk  ShowInstanceRect             TN2805    use TZADesignGlobals instead of global
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  08.09.08 pk                               TN4215    No longer calls LayoutManager.Applicationend
  06.04.09 pk                               TN4503    Caption removed, color changed to white
  17.04.09 pk  fViewLayout                  TN4532    New form with navigation buttons
  27.07.09 pk                               TN4604    New PageControl is docksite for multiple layouts
  27.08.09 pk                               TN4753    uses LayoutManager removed
  02.09.09 pk  FinalizeRemoveLayout         TN4753    New
  02.09.09 pk  PopMenu1                     TN4753    New close and refresh menu options
  23.04.10 wl                               TN5070    implementiert IlayoutDisplay
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  08.02.11 wl                               TN5474   ComboBox zur Auswahl von Layouts, nicht mehr dockable
  30.06.11 wl                               TN5620   Align = alClient
  13.03.13 wl                               TN5960   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit EdLayout;


interface


uses
    Forms,
    Controls,
    Classes,
    Menus,
    StdCtrls,
    ComCtrls,
    ExtCtrls,
    Windows,
    SceneGraphics,
    ViewLayout,
    LayoutDisplay;

type
    TfrmEdLayout = class(TForm)
        Panel1: TPanel;
        pgctrlLayouts: TPageControl;
        PopupMenu1: TPopupMenu;
        Close1: TMenuItem;
        Refresh1: TMenuItem;
        Panel2: TPanel;
        Label1: TLabel;
        cbLayoutName: TComboBox;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure pgctrlLayoutsChange(Sender: TObject);
        procedure pgctrlLayoutsDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
        procedure pgctrlLayoutsUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl;
            var Allow: Boolean);
        procedure pgctrlLayoutsGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
            MousePos: TPoint; var CanDock: Boolean);
        procedure Close1Click(Sender: TObject);
        procedure Refresh1Click(Sender: TObject);
        procedure pgctrlLayoutsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure cbLayoutNameChange(Sender: TObject);
    private
        fViewLayouts: TStringList;
        fDeletingPage: boolean;

        procedure PrepareAddLayoutIntern(const aLayoutName: string; out oBackground: TWinControl);
        procedure FinalizeRemoveLayoutIntern(const aLayoutName: string);
        procedure LayoutAddedIntern(const aLayoutName: string; const aSceneGraphics: TSceneGraphics);
        procedure LayoutRemovedIntern(const aLayoutName: string);
        function FindViewLayout(const aLayoutName: string): TfrmViewLayout;
        function GetActiveLayoutName(): string;
        procedure RefreshActivePage;
    public
        constructor Create(aOwner: TComponent); override;
        destructor Destroy(); override;
    end;

    TReadOnlyLayoutDisplay = class(TInterfacedObject, ILayoutDisplay)
    private
        fLayoutForm: TfrmEdLayout;
    public
        procedure PrepareAddLayout(const aLayoutName: string; out oBackground: TWinControl);
        procedure FinalizeRemoveLayout(const aLayoutName: string);
        procedure LayoutAdded(const aLayoutName: string; const aSceneGraphics: TSceneGraphics);
        procedure LayoutRemoved(const aLayoutName: string);
        procedure SetCaption(aRunName, aLayoutName: string);
        property LayoutForm: TfrmEdLayout read fLayoutForm write fLayoutForm;
    end;


implementation


{$R *.DFM}

uses
    SysUtils,
    Dialogs,
    SamGlobe,
    ControlUtils,
    LayoutDataAdaptor,
    ViewItemsWorkflow;

procedure TfrmEdLayout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caNone;
end;

procedure TfrmEdLayout.cbLayoutNameChange(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.LoadLayout(cbLayoutName.Text);
end;

procedure TfrmEdLayout.Close1Click(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.UnloadLayout(GetActiveLayoutName());
end;

constructor TfrmEdLayout.Create(aOwner: TComponent);
begin
    inherited Create(aOwner);

    // Layout Manager erzeugen!
    TControlUtils.ResetFontForWinXP(self);

    fViewLayouts := TStringList.Create();
    fDeletingPage := false;
    self.OnClose := FormClose;
    TControlUtils.AddValuesToComboBox(TLayoutDataAdaptor.InstReadAllNames, self.cbLayoutName, true);
end;

destructor TfrmEdLayout.Destroy;
begin
    FreeAndNil(fViewLayouts);
    inherited;
end;

procedure TfrmEdLayout.Refresh1Click(Sender: TObject);
begin
    self.RefreshActivePage();
end;

procedure TfrmEdLayout.RefreshActivePage();
begin
    if pgctrlLayouts.ActivePageIndex < 0 then
        EXIT;

    TViewItemsWorkflow.Instance.LoadLayout(GetActiveLayoutName());
end;

procedure TfrmEdLayout.PrepareAddLayoutIntern(const aLayoutName: string; out oBackground: TWinControl);
var
    xViewLayout: TfrmViewLayout;
begin
    xViewLayout := TfrmViewLayout.Create(self);
    xViewLayout.Align := alClient;
    xViewLayout.ManualDock(self.pgctrlLayouts);
    xViewLayout.Visible := true;
    xViewLayout.Caption := aLayoutName;
    // xViewLayout.SceneGraphics := TZADesignLayoutManager.Instance.SceneGraphics;
    oBackground := xViewLayout.DrawPanel;
    fViewLayouts.AddObject(aLayoutName, xViewLayout);
end;

procedure TfrmEdLayout.FinalizeRemoveLayoutIntern(const aLayoutName: string);
var
    xViewLayout: TfrmViewLayout;
    xPageToClose, xNextAvailablePageIndex: integer;
begin
    xViewLayout := FindViewLayout(aLayoutName);
    ASSERT(Assigned(xViewLayout));

    xPageToClose := pgctrlLayouts.ActivePageIndex;

    if pgctrlLayouts.PageCount = 1 then // no more pages
        xNextAvailablePageIndex := -1
    else if pgctrlLayouts.ActivePageIndex = 0 then // reopen first page
        xNextAvailablePageIndex := 0
    else // reopen previous page
        xNextAvailablePageIndex := xPageToClose - 1;

    fDeletingPage := true;
    // deleting a page causes pagecontrol.onchange to be called, and the first page to be loaded. we dont want this!
    try
        xViewLayout.ManualDock(nil);
        xViewLayout.Visible := false;
        xViewLayout.Free;
        fViewLayouts.Delete(fViewLayouts.IndexOf(aLayoutName));
    finally
        fDeletingPage := false;
    end;

    // Set the activeindex so that the next available page is opened in refreshbuttons
    pgctrlLayouts.ActivePageIndex := xNextAvailablePageIndex;
    RefreshActivePage();
end;

function TfrmEdLayout.GetActiveLayoutName(): string;
begin
    result := (pgctrlLayouts.DockClients[pgctrlLayouts.ActivePageIndex] as TfrmViewLayout).Caption;
end;

function TfrmEdLayout.FindViewLayout(const aLayoutName: string): TfrmViewLayout;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := fViewLayouts.IndexOf(aLayoutName);
    if xIndex < 0 then
        EXIT;
    result := fViewLayouts.Objects[xIndex] as TfrmViewLayout;
end;

procedure TfrmEdLayout.LayoutRemovedIntern(const aLayoutName: string);
var
    xViewLayout: TfrmViewLayout;
begin
    xViewLayout := FindViewLayout(aLayoutName);
    ASSERT(Assigned(xViewLayout));
    xViewLayout.SceneGraphics := nil;

end;

procedure TfrmEdLayout.LayoutAddedIntern(const aLayoutName: string; const aSceneGraphics: TSceneGraphics);
var
    xViewLayout: TfrmViewLayout;
    x: integer;
begin
    xViewLayout := FindViewLayout(aLayoutName);
    ASSERT(Assigned(xViewLayout));
    xViewLayout.SceneGraphics := aSceneGraphics;

    for x := 0 to pgctrlLayouts.DockClientCount - 1 do
    begin
        if xViewLayout = pgctrlLayouts.DockClients[x] then
        begin
            pgctrlLayouts.ActivePageIndex := x;
            BREAK;
        end;
    end;
end;

procedure TfrmEdLayout.pgctrlLayoutsChange(Sender: TObject);
begin
    if fDeletingPage then
        EXIT; // deleting a page causes pagecontrol.onchange to be called, and the first page to be loaded. we dont want this!
    RefreshActivePage();

end;

procedure TfrmEdLayout.pgctrlLayoutsDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
begin
    Accept := (Source.Control is TfrmViewLayout);

    if (Source.Control.HostDockSite = pgctrlLayouts) then
        Accept := false;
end;

procedure TfrmEdLayout.pgctrlLayoutsUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl;
    var Allow: Boolean);
begin
    Allow := false;
end;

procedure TfrmEdLayout.pgctrlLayoutsGetSiteInfo(Sender: TObject; DockClient: TControl;
    var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
    CanDock := (DockClient is TfrmViewLayout);

end;

procedure TfrmEdLayout.pgctrlLayoutsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
    if (Button <> mbRight) then
        EXIT; // make page active from a right mouse click
    pgctrlLayouts.ActivePageIndex := pgctrlLayouts.IndexOfTabAt(X, Y);
    RefreshActivePage();
end;

{ TReadOnlyLayoutDisplay }

procedure TReadOnlyLayoutDisplay.FinalizeRemoveLayout(const aLayoutName: string);
begin
    fLayoutForm.FinalizeRemoveLayoutIntern(aLayoutName);
end;

procedure TReadOnlyLayoutDisplay.LayoutAdded(const aLayoutName: string; const aSceneGraphics: TSceneGraphics);
begin
    fLayoutForm.LayoutAddedIntern(aLayoutName, aSceneGraphics);
end;

procedure TReadOnlyLayoutDisplay.LayoutRemoved(const aLayoutName: string);
begin
    fLayoutForm.LayoutRemovedIntern(aLayoutName);
end;

procedure TReadOnlyLayoutDisplay.PrepareAddLayout(const aLayoutName: string; out oBackground: TWinControl);
begin
    fLayoutForm.PrepareAddLayoutIntern(aLayoutName, oBackground);
end;

procedure TReadOnlyLayoutDisplay.SetCaption(aRunName, aLayoutName: string);
begin
    if not Assigned(fLayoutForm) then
        EXIT;

    fLayoutForm.cbLayoutName.Text := aLayoutName;
end;


end.
