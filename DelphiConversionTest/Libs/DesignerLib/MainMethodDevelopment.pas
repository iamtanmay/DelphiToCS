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
  27.05.10 wl                                    TN5116   Toolbar entfernt
  02.06.10 wl                                    TN5116   vereinfacht
  09.06.10 wl                                    TN5116   ViewDisplayComponentPreview entfernt
  09.06.10 wl                                    TN5116   ViewFavourites entfernt
  21.09.10 pk                                    TN5047   TfrmActionProps is no longer dockable. Only opened modal from MethodEditor
  23.09.10 pk                                    TN4989   Shortcut Ctrl + F4 to close current Editor Tab
  03.12.10 pk                                    TN5381   now inherits from TMainCustomDevelopment
  14.12.10 pk                                    TN5381   correct ordering of toolbuttons
  10.02.11 wl                                    TN5474   neu: LayoutForm ist fester Bestandteil
  30.06.11 wl                                    TN5620   links, rechts und oben lässt sich nichts mehr andocken
  30.06.11 wl  PanelTopResize                    TN5620   sorgt dafür, dass das Layout-Fenster immer mindestens 30 Pixel hoch ist
  26.07.11 wl                                    TN5614   verwaltet jetzt die 4 View-Fenster komplett
  02.08.11 wl                                    TN5645   angepasst an MainCustomDevelopment
  16.08.11 wl                                    TN5670   SearchFor statt FindInMethods
  22.06.12 wl                                    TN5924   ViewReagentsOfLayout ist nicht mehr enthalten
  13.03.13 wl                                    TN5960   verwendet TMainCustomDevelopment.CreateOverviewInstance
  30.08.13 wl  UpdateAttributes                  TN6236   entfernt
  ------------------------------------------------------------------------------------------------------------ }

unit MainMethodDevelopment;


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
    Types,
    Dialogs,
    DockableForm,
    MethodCompile,
    EdLayout,
    ViewItemEditForm,
    MethodSettingsDataAdaptor,
    MainCustomDevelopment,
    LayoutDisplay,
    SceneGraphics;

type
    TFormClass = class of TForm;

    TfrmMainMethodDevelopment = class(TMainCustomDevelopment)
        PopupMenu1: TPopupMenu;
        pmnuClose: TMenuItem;
        pmnuCloseAllPages: TMenuItem;
        pnlEditMain: TPanel;
        SplitterBottom: TSplitter;
        PanelMiddle: TPanel;
        SplitterTop: TSplitter;
        PageControl1: TPageControl;
        PanelTop: TPanel;
        Splitter1: TSplitter;
        pnAllItems: TPanel;
        ToolBar1: TToolBar;
        PageControl2: TPageControl;
        PopupMenu2: TPopupMenu;
        pmnuCloseBottomPage: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
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
        procedure FormDestroy(Sender: TObject);
        procedure PanelTopResize(Sender: TObject);
        procedure PageControl2UnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl;
            var Allow: Boolean);
        procedure PageControl2DockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
        procedure PageControl2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure PageControl2GetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
            MousePos: TPoint; var CanDock: Boolean);
        procedure PopupMenu2Popup(Sender: TObject);
        procedure pmnuCloseBottomPageClick(Sender: TObject);
    private
        fStarted: boolean;
        fLayoutForm: TfrmEdLayout;
        procedure LoadDockableForms;
        function ShowViewForm(aFormClass: TFormClass): TForm;
    protected
        procedure DoAddStandardToolBar(const aToolbar: TToolBar; const aPixelSize: integer); override;
        function GetPageControl: TPageControl; override;
    public
        procedure ShowViewHierarchy;
        procedure ShowViewSearch;
        procedure SearchFor(const aSearchText: string);
        procedure ShowViewCompilerMessages;
        function CompilerMessagesStart: TCompilerResult;
        function CompilerMessagesLoad: integer;
        property LayoutForm: TfrmEdLayout read fLayoutForm;
        procedure LoadHierarchy(const aName: string);
    end;


implementation


{$R *.dfm}

uses
    SysUtils,
    ControlUtils,
    AppSettings,
    ViewHierarchy,
    ViewItem,
    ViewSearch,
    ViewCompileMessages,
    GeneralTypes,
    MethodDataAdaptor,
    MethodEditor;

{ TfrmMainMethodDevelopment }

procedure TfrmMainMethodDevelopment.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStarted := true;
    self.Caption := TLanguageString.Read('Method development', 'Methodenentwicklung');
    self.AddStandardToolBar(self.ToolBar1);
    fLayoutForm := TfrmEdLayout.Create(nil);
end;

procedure TfrmMainMethodDevelopment.FormDestroy(Sender: TObject);
begin
    FreeAndNil(fLayoutForm);
end;

function TfrmMainMethodDevelopment.CompilerMessagesStart: TCompilerResult;
var
    xForm: TForm;
begin
    xForm := ShowViewForm(TfrmViewCompileMessages);
    (xForm as TfrmViewCompileMessages).ClearMessages();
    EXIT((xForm as TfrmViewCompileMessages).CompilerResult);
end;

function TfrmMainMethodDevelopment.CompilerMessagesLoad: integer;
var
    xForm: TForm;
begin
    xForm := ShowViewForm(TfrmViewCompileMessages);
    (xForm as TfrmViewCompileMessages).LoadMessages();
    (xForm as TfrmViewCompileMessages).RedirectFirstCompilerMessage();
    result := (xForm as TfrmViewCompileMessages).CompilerResult.ErrorMessages.Count;
end;

procedure TfrmMainMethodDevelopment.DoAddStandardToolBar(const aToolbar: TToolBar; const aPixelSize: integer);
begin
    AddSaveToolButtons(aToolBar, aPixelSize);
    AddEditToolButtons(aToolBar, aPixelSize);
    fToolbarUtils.CreateStartOpenMethodToolButton(aToolbar, aPixelSize);
end;

function TfrmMainMethodDevelopment.GetPageControl: TPageControl;
begin
    result := self.PageControl1;
end;

procedure TfrmMainMethodDevelopment.LoadDockableForms;
begin
    self.CreateOverviewInstance(aemMethodDevelopment, pnAllItems);
end;

procedure TfrmMainMethodDevelopment.pmnuCloseBottomPageClick(Sender: TObject);
var
    xPageToClose: integer;
    xNextAvailablePageIndex: integer;
begin
    xPageToClose := PageControl2.ActivePageIndex;
    if not(PageControl2.DockClients[xPageToClose] is TForm) then
        EXIT;

    // determine the next page that should be opened after this one is closed

    if PageControl2.PageCount = 1 then // no more pages
        xNextAvailablePageIndex := -1
    else if PageControl2.ActivePageIndex = 0 then // reopen first page
        xNextAvailablePageIndex := 0
    else // reopen previous page
        xNextAvailablePageIndex := xPageToClose - 1;

    self.CloseDockClient(PageControl2.DockClients[xPageToClose] as TForm);

    // Set the activeindex so that the next available page is opened in refreshbuttons
    PageControl2.ActivePageIndex := xNextAvailablePageIndex;

    if PageControl2.PageCount = 0 then // no more pages
        PageControl2.Height := 1;
end;

procedure TfrmMainMethodDevelopment.PageControl1Change(Sender: TObject);
begin
    RefreshButtons(); // im Page-Control ändert sich die ActivePage
end;

procedure TfrmMainMethodDevelopment.PageControl1DockOver(Sender: TObject; Source: TDragDockObject;
    X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
    Accept := (Source.Control is TViewItemEditForm);

    if (Source.Control.HostDockSite = PageControl1) then
        Accept := false;
end;

procedure TfrmMainMethodDevelopment.PageControl1GetSiteInfo(Sender: TObject; DockClient: TControl;
    var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
    CanDock := (DockClient is TViewItemEditForm);
end;

procedure TfrmMainMethodDevelopment.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    if (Button = mbRight) then // make page active from a right mouse click
        PageControl1.ActivePageIndex := PageControl1.IndexOfTabAt(X, Y);
end;

procedure TfrmMainMethodDevelopment.PageControl1UnDock(Sender: TObject; Client: TControl;
    NewTarget: TWinControl; var Allow: Boolean);
begin
    Allow := false; // man darf einfach keine Edit-Fenster aus dem Tab-Control lösen!
end;

procedure TfrmMainMethodDevelopment.PageControl2DockOver(Sender: TObject; Source: TDragDockObject;
    X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
    Accept := (Source.Control is TfrmViewHierarchy) or (Source.Control is TfrmViewSearch) or
        (Source.Control is TfrmViewCompileMessages);

    if (Source.Control.HostDockSite = PageControl2) then
        Accept := false;
end;

procedure TfrmMainMethodDevelopment.PageControl2GetSiteInfo(Sender: TObject; DockClient: TControl;
    var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
    CanDock := (DockClient is TfrmViewHierarchy) or (DockClient is TfrmViewSearch) or
        (DockClient is TfrmViewCompileMessages);
end;

procedure TfrmMainMethodDevelopment.PageControl2MouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    if (Button = mbRight) then // make page active from a right mouse click
        PageControl2.ActivePageIndex := PageControl2.IndexOfTabAt(X, Y);
end;

procedure TfrmMainMethodDevelopment.PageControl2UnDock(Sender: TObject; Client: TControl;
    NewTarget: TWinControl; var Allow: Boolean);
begin
    Allow := false; // man darf einfach keine Edit-Fenster aus dem Tab-Control lösen!
end;

procedure TfrmMainMethodDevelopment.PanelTopResize(Sender: TObject);
begin
    if self.PanelTop.Height <= 30 then
        self.PanelTop.Height := 30;
end;

procedure TfrmMainMethodDevelopment.pmnuCloseAllPagesClick(Sender: TObject);
begin
    self.CloseAllPages();
end;

procedure TfrmMainMethodDevelopment.pmnuCloseClick(Sender: TObject);
begin
    CloseCurrentPage();
end;

procedure TfrmMainMethodDevelopment.PopupMenu1Popup(Sender: TObject);
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

procedure TfrmMainMethodDevelopment.PopupMenu2Popup(Sender: TObject);
var
    xIndex: integer;
begin
    xIndex := PageControl2.ActivePageIndex;
    if (xIndex < 0) then
        EXIT;
    pmnuCloseBottomPage.Caption := TLanguageString.Read('Close Page: {0}', 'Seite schließen: {0}',
        [PageControl2.Pages[xIndex].Caption]);
end;

procedure TfrmMainMethodDevelopment.ShowViewCompilerMessages;
begin
    ShowViewForm(TfrmViewCompileMessages);
end;

function TfrmMainMethodDevelopment.ShowViewForm(aFormClass: TFormClass): TForm;
var
    x: integer;
begin
    for x := 0 to PageControl2.DockClientCount - 1 do
    begin
        if (PageControl2.DockClients[x] is aFormClass) then
        begin
            PageControl2.ActivePageIndex := x;

            EXIT(PageControl2.DockClients[x] as TForm);
        end;
    end;

    result := aFormClass.Create(Application);
    result.Align := alClient;
    result.ManualDock(PageControl2);
    result.DragKind := dkDock;
    result.Visible := true;
    if self.PageControl2.Height < 200 then
    begin
        self.PageControl2.Height := 200;
    end;

    for x := 0 to PageControl2.DockClientCount - 1 do
    begin
        if (PageControl2.DockClients[x] is aFormClass) then
        begin
            PageControl2.ActivePageIndex := x;

            EXIT(PageControl2.DockClients[x] as TForm);
        end;
    end;
end;

procedure TfrmMainMethodDevelopment.ShowViewHierarchy;
begin
    ShowViewForm(TfrmViewHierarchy);
end;

procedure TfrmMainMethodDevelopment.ShowViewSearch;
begin
    ShowViewForm(TfrmViewSearch);
end;

procedure TfrmMainMethodDevelopment.SearchFor(const aSearchText: string);
var
    xForm: TForm;
begin
    Screen.Cursor := crHourglass;
    try
        xForm := ShowViewForm(TfrmViewSearch);
        Application.ProcessMessages;
        (xForm as TfrmViewSearch).SearchFor(aSearchText);
    finally
        Screen.Cursor := crDefault;
    end;
end;

procedure TfrmMainMethodDevelopment.LoadHierarchy(const aName: string);
var
    xForm: TForm;
begin
    Screen.Cursor := crHourglass;
    try
        xForm := ShowViewForm(TfrmViewHierarchy);
        (xForm as TfrmViewHierarchy).LoadMethod(aName, '', 'ADDM', nil);
    finally
        Screen.Cursor := crDefault;
    end;
end;

procedure TfrmMainMethodDevelopment.FormShow(Sender: TObject);
begin
    if FStarted then
    begin

        // nach dem Senden des DDE-Status 'Ready'
        // gmReadGlobalsIniFile();

        LoadDockableForms;

        fLayoutForm.Parent := PanelTop;
        fLayoutForm.Visible := true;
    end;

    // UserSetAccess;
    // TZADesignLayoutManager.Instance.LoadEmpty();

    FStarted := false;
end;


end.
