{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ---------------------------------------------------------------
  03.12.10 pk                                    TN5381    Initial Revisions
  14.12.10 pk                                    TN5381    correct ordering of toolbuttons
  02.08.11 wl                                    TN5645   aufgeräumt, damit Enable/Disable der Start-Buttons endlich funktioniert
  11.04.12 wl  Destroy                           TN5861   zerstört fToolbarUtils
  13.03.13 wl  TAppEditMode                      TN5960   von ViewItemEditForm hierher
  13.03.13 wl  CreateOverviewInstance            TN5960   von ViewItemOverviewManager hierher
  ----------------------------------------------------------------------------------------------------------------------- }

unit MainCustomDevelopment;


interface


uses
    Forms,
    ExtCtrls,
    ImgList,
    Classes,
    Menus,
    StdCtrls,
    Controls,
    StringLoader,
    ComCtrls,
    ViewItemEditForm,
    ActnList,
    ViewItem,
    ViewAllItems,
    DockableForm;

type
    TAppEditMode = (aemUnknown, aemRun, aemMethodDevelopment, aemLayoutDevelopment, aemDeviceDevelopment,
        aemDisplayDevelopment, aemImportDevelopment);

    TCloseDockClientEvent = procedure(aForm: TForm; aEditMode: TAppEditMode) of object;

    TMainDevelopmentGetToolbarImageListEvent = procedure(const aSender: TObject;
        out oImageList: TCustomImageList) of object;
    TMainDevelopmentGetStringLoaderEvent = procedure(const aSender: TObject; out oStringLoader: TStringLoader)
        of object;

    TMainDevelopmentExternalFunctions = class
    strict private
        fOnGetStandardToolbarImageList: TMainDevelopmentGetToolbarImageListEvent;
        fOnGetStringLoader: TMainDevelopmentGetStringLoaderEvent;
        fOnRefreshButtons: TNotifyEvent;
        fOnCloseDockClient: TCloseDockClientEvent;

        fOnEditCut: TBasicAction;
        fOnEditCopy: TBasicAction;
        fOnEditPaste: TBasicAction;
        fOnStart: TBasicAction;
        fOnStartLastStarted: TBasicAction;
        fOnStartOpenMethod: TBasicAction;
        fOnSave: TBasicAction;
        fOnSaveAll: TBasicAction;
        fOnInit: TBasicAction;
        fOnFlush: TBasicAction;
    public
        property OnGetStandardToolbarImageList: TMainDevelopmentGetToolbarImageListEvent
            read fOnGetStandardToolbarImageList write fOnGetStandardToolbarImageList;
        property OnGetStringLoader: TMainDevelopmentGetStringLoaderEvent read fOnGetStringLoader
            write fOnGetStringLoader;
        property OnEditCut: TBasicAction read fOnEditCut write fOnEditCut;
        property OnEditCopy: TBasicAction read fOnEditCopy write fOnEditCopy;
        property OnEditPaste: TBasicAction read fOnEditPaste write fOnEditPaste;
        property OnStart: TBasicAction read fOnStart write fOnStart;
        property OnStartLastStarted: TBasicAction read fOnStartLastStarted write fOnStartLastStarted;
        property OnStartOpenMethod: TBasicAction read fOnStartOpenMethod write fOnStartOpenMethod;
        property OnSave: TBasicAction read fOnSave write fOnSave;
        property OnSaveAll: TBasicAction read fOnSaveAll write fOnSaveAll;
        property OnInit: TBasicAction read fOnInit write fOnInit;
        property OnFlush: TBasicAction read fOnFlush write fOnFlush;

        property OnRefreshButtons: TNotifyEvent read fOnRefreshButtons write fOnRefreshButtons;
        property OnCloseDockClient: TCloseDockClientEvent read fOnCloseDockClient write fOnCloseDockClient;
    end;

    TMainDevelopmentToolbarUtils = class
    strict private
        fExternalFunctions: TMainDevelopmentExternalFunctions;
    public
        function CreateToolButton(const aToolbar: TToolbar; const aPixelSize: integer;
            const aAction: TBasicAction): TToolButton;
        function CreateSaveToolButton(const aToolbar: TToolbar; const aPixelSize: integer): TToolButton;
        function CreateSaveAllToolButton(const aToolbar: TToolbar; const aPixelSize: integer): TToolButton;
        function CreateCutToolButton(const aToolbar: TToolbar; const aPixelSize: integer): TToolButton;
        function CreateCopyToolButton(const aToolbar: TToolbar; const aPixelSize: integer): TToolButton;
        function CreatePasteToolButton(const aToolbar: TToolbar; const aPixelSize: integer): TToolButton;
        function CreateStartLastStartedToolButton(const aToolbar: TToolbar; const aPixelSize: integer)
            : TToolButton;
        function CreateStartOpenMethodToolButton(const aToolbar: TToolbar; const aPixelSize: integer)
            : TToolButton;
        function CreateStartToolButton(const aToolbar: TToolbar; const aPixelSize: integer): TToolButton;
        function CreateFlushToolButton(const aToolbar: TToolbar; const aPixelSize: integer): TToolButton;
        function CreateInitToolButton(const aToolbar: TToolbar; const aPixelSize: integer): TToolButton;
        procedure IncrementToolPos(var vTop: integer; var vLeft: integer; const aPixelSize: integer);
        procedure AddButtonToToolBar(const aToolbar: TToolBar; const aToolbutton: TToolButton;
            const aPixelSize: integer; var vTop, vLeft: integer);

        constructor Create(const aExternalFunctions: TMainDevelopmentExternalFunctions);
    end;

    TMainCustomDevelopment = class(TForm)
    private const
        cMethodDevelopmentName: string = 'MethodDevelopment';
        cLayoutDevelopmentName: string = 'LayoutDevelopment';
        cDeviceDevelopmentName: string = 'DeviceDevelopment';
        cDisplayDevelopmentName: string = 'DisplayDevelopment';
        cImportDevelopmentName: string = 'ImportDevelopment';
    private
        fExternalFunctions: TMainDevelopmentExternalFunctions;
        fFileSaveButton: TToolButton;
        fFileSaveAllButton: TToolButton;
        fEditCutButton: TToolButton;
        fEditCopyButton: TToolButton;
        fEditPasteButton: TToolButton;
        class function GetFormNameByEditMode(aEditMode: TAppEditMode): string;
        class function GetItemTypesByName(const aFormName: string; out oCaption: string)
            : TViewItemTypes; static;
    protected
        fToolbarUtils: TMainDevelopmentToolbarUtils;
        function CreateOverviewInstance(aEditMode: TAppEditMode; aNewDockSite: TWinControl): TfrmViewAllItems;
        procedure DoAddStandardToolBar(const aToolbar: TToolBar; const aPixelSize: integer); virtual;
        procedure AddStandardToolBar(const aToolbar: TToolBar);

        procedure RefreshButtons();
        procedure CloseDockClient(aForm: TForm);

        procedure CloseCurrentPage;
        function GetDataChanged: boolean;

        function GetPageControl: TPageControl; virtual; abstract;
        function GetCurrentEditor: TViewItemEditForm; virtual;

        procedure AddEditToolButtons(const aToolbar: TToolBar; const aPixelSize: integer);
        procedure AddSaveToolButtons(const aToolbar: TToolBar; const aPixelSize: integer);

        procedure AddStartToolButtons(const aToolbar: TToolBar; const aPixelSize: integer);
        procedure AddRunUtilityToolButtons(const aToolbar: TToolBar; const aPixelSize: integer);
    public
        constructor Create(const aOwner: TComponent;
            const aExternalFunctions: TMainDevelopmentExternalFunctions); reintroduce;
        destructor Destroy; override;
        function CloseAllPages(): boolean;
        procedure DockAndLoadForm(aForm: TDockableEditForm);
        procedure SaveAll;
        function FindDockClient(aCaption: string; aActivate: boolean): TForm;
        procedure SetFileSaveEnabled(const aEnabled: boolean);
        procedure SetFileSaveAllEnabled(const aEnabled: boolean);
        procedure SetEditCutEnabled(const aEnabled: boolean);
        procedure SetEditCopyEnabled(const aEnabled: boolean);
        procedure SetEditPasteEnabled(const aEnabled: boolean);

        property CurrentEditor: TViewItemEditForm read GetCurrentEditor;
        property PageControl: TPageControl read GetPageControl;
        property DataChanged: boolean read GetDataChanged;

    end;


implementation


uses
    SysUtils,
    AppSettings,
    GeneralTypes,
    ViewItemsWorkflow;

{ TMainDevelopmentToolbarUtils }

constructor TMainDevelopmentToolbarUtils.Create(const aExternalFunctions: TMainDevelopmentExternalFunctions);
begin
    inherited Create();
    fExternalFunctions := aExternalFunctions;
end;

procedure TMainDevelopmentToolbarUtils.AddButtonToToolBar(const aToolbar: TToolBar;
    const aToolbutton: TToolButton; const aPixelSize: integer; var vTop, vLeft: integer);
begin

    aToolbutton.Width := aPixelSize;
    aToolbutton.Height := aPixelSize;
    aToolbutton.Top := vTop;
    aToolbutton.Left := vLeft;
    aToolbutton.Parent := aToolbar;
    aToolbutton.Visible := true;
    IncrementToolPos(vTop, vLeft, aPixelSize);
end;

function TMainDevelopmentToolbarUtils.CreateToolButton(const aToolbar: TToolbar; const aPixelSize: integer;
    const aAction: TBasicAction): TToolButton;
var
    xTop, xLeft: integer;
begin
    result := TToolButton.Create(aToolbar);
    AddButtonToToolBar(aToolbar, result, aPixelSize, xTop, xLeft);
    result.Action := aAction;
    result.Tag := aAction.Tag;
    result.ShowHint := true;
end;

function TMainDevelopmentToolbarUtils.CreateSaveToolButton(const aToolbar: TToolbar;
    const aPixelSize: integer): TToolButton;
begin
    result := CreateToolButton(aToolbar, aPixelSize, fExternalFunctions.OnSave);
end;

function TMainDevelopmentToolbarUtils.CreateSaveAllToolButton(const aToolbar: TToolbar;
    const aPixelSize: integer): TToolButton;
begin
    result := CreateToolButton(aToolbar, aPixelSize, fExternalFunctions.OnSaveAll);
end;

function TMainDevelopmentToolbarUtils.CreateCopyToolButton(const aToolbar: TToolbar;
    const aPixelSize: integer): TToolButton;
begin
    result := CreateToolButton(aToolbar, aPixelSize, fExternalFunctions.OnEditCopy);
end;

function TMainDevelopmentToolbarUtils.CreateCutToolButton(const aToolbar: TToolbar; const aPixelSize: integer)
    : TToolButton;
begin
    result := CreateToolButton(aToolbar, aPixelSize, fExternalFunctions.OnEditCut);
end;

function TMainDevelopmentToolbarUtils.CreatePasteToolButton(const aToolbar: TToolbar;
    const aPixelSize: integer): TToolButton;
begin
    result := CreateToolButton(aToolbar, aPixelSize, fExternalFunctions.OnEditPaste);
end;

function TMainDevelopmentToolbarUtils.CreateStartToolButton(const aToolbar: TToolbar;
    const aPixelSize: integer): TToolButton;
begin
    result := CreateToolButton(aToolbar, aPixelSize, fExternalFunctions.OnStart);
end;

function TMainDevelopmentToolbarUtils.CreateStartLastStartedToolButton(const aToolbar: TToolbar;
    const aPixelSize: integer): TToolButton;
begin
    result := CreateToolButton(aToolbar, aPixelSize, fExternalFunctions.OnStartLastStarted);
end;

function TMainDevelopmentToolbarUtils.CreateStartOpenMethodToolButton(const aToolbar: TToolbar;
    const aPixelSize: integer): TToolButton;
begin
    result := CreateToolButton(aToolbar, aPixelSize, fExternalFunctions.OnStartOpenMethod);
end;

function TMainDevelopmentToolbarUtils.CreateFlushToolButton(const aToolbar: TToolbar;
    const aPixelSize: integer): TToolButton;
begin
    result := CreateToolButton(aToolbar, aPixelSize, fExternalFunctions.OnFlush);
end;

function TMainDevelopmentToolbarUtils.CreateInitToolButton(const aToolbar: TToolbar;
    const aPixelSize: integer): TToolButton;
begin
    result := CreateToolButton(aToolbar, aPixelSize, fExternalFunctions.OnInit);
end;

procedure TMainDevelopmentToolbarUtils.IncrementToolPos(var vTop: integer; var vLeft: integer;
    const aPixelSize: integer);
begin
    vTop := vTop + aPixelSize;
end;

{ TMainCustomDevelopment }

constructor TMainCustomDevelopment.Create(const aOwner: TComponent;
    const aExternalFunctions: TMainDevelopmentExternalFunctions);
begin
    inherited Create(aOwner);
    fExternalFunctions := aExternalFunctions;
    fToolbarUtils := TMainDevelopmentToolbarUtils.Create(fExternalFunctions);
end;

destructor TMainCustomDevelopment.Destroy;
begin
    FreeAndNil(fToolbarUtils);

    inherited;
end;

class function TMainCustomDevelopment.GetItemTypesByName(const aFormName: string; out oCaption: string)
    : TViewItemTypes;
begin
    result := [];
    if aFormName = cMethodDevelopmentName then
    begin
        result := [ntAction, { ntVariable, } ntMethod, ntSequence, ntWashProg, ntSubstance, ntSubstanceSet,
            ntLiquidPar, ntPowderPar, ntSQLTerm, ntTipType];
        oCaption := TLanguageString.Read('Method development', 'Methodenentwicklung');
    end;
    if aFormName = cLayoutDevelopmentName then
    begin
        if TAppSettings.IsDebugMode then
            result := [ntRack, ntCarrier, ntWorkspace, ntLayout, ntTipType]
        else
            result := [ntRack, ntCarrier, ntWorkspace, ntTipType];
        oCaption := TLanguageString.Read('Layout development', 'Layout-Entwicklung');
    end;
    if aFormName = cDeviceDevelopmentName then
    begin
        result := [ntDevice, ntDriver, ntConnection];
        oCaption := TLanguageString.Read('Device development', 'Device-Entwicklung');
    end;
    if aFormName = cImportDevelopmentName then
    begin
        result := [ntVarImportDef, ntTableImportDef, ntImportFileDef];
        oCaption := TLanguageString.Read('Import development', 'Import-Entwicklung');
    end;
    if aFormName = cDisplayDevelopmentName then
    begin
        result := [ntDisplayComponent, ntSQLTerm];
        oCaption := TLanguageString.Read('Display development', 'Display-Entwicklung');
    end;
end;

class function TMainCustomDevelopment.GetFormNameByEditMode(aEditMode: TAppEditMode): string;
begin
    case (aEditMode) of
        aemDeviceDevelopment:
            result := cDeviceDevelopmentName;
        aemImportDevelopment:
            result := cImportDevelopmentName;
        aemDisplayDevelopment:
            result := cDisplayDevelopmentName;
        aemMethodDevelopment:
            result := cMethodDevelopmentName;
        aemLayoutDevelopment:
            result := cLayoutDevelopmentName;
        else
            result := 'XXX';
    end;
end;

function TMainCustomDevelopment.CreateOverviewInstance(aEditMode: TAppEditMode; aNewDockSite: TWinControl)
    : TfrmViewAllItems;
var
    xCaption: string;
    xTypes: TViewItemTypes;
    xFormName: string;
begin
    xFormName := GetFormNameByEditMode(aEditMode);
    xTypes := GetItemTypesByName(xFormName, xCaption);

    EXIT(TViewItemsWorkflow.Instance.CreateOverviewInstance(xCaption, xTypes, xFormName, aNewDockSite));
end;

procedure TMainCustomDevelopment.SetFileSaveEnabled(const aEnabled: boolean);
begin
    if not Assigned(fFileSaveButton) then
        EXIT;
    fFileSaveButton.Enabled := aEnabled;
end;

procedure TMainCustomDevelopment.SetFileSaveAllEnabled(const aEnabled: boolean);
begin
    if not Assigned(fFileSaveAllButton) then
        EXIT;
    fFileSaveAllButton.Enabled := aEnabled;
end;

procedure TMainCustomDevelopment.SetEditCutEnabled(const aEnabled: boolean);
begin
    if not Assigned(fEditCutButton) then
        EXIT;
    fEditCutButton.Enabled := aEnabled;
end;

procedure TMainCustomDevelopment.SetEditCopyEnabled(const aEnabled: boolean);
begin
    if not Assigned(fEditCopyButton) then
        EXIT;
    fEditCopyButton.Enabled := aEnabled;
end;

procedure TMainCustomDevelopment.SetEditPasteEnabled(const aEnabled: boolean);
begin
    if not Assigned(fEditPasteButton) then
        EXIT;
    fEditPasteButton.Enabled := aEnabled;
end;

procedure TMainCustomDevelopment.AddEditToolButtons(const aToolbar: TToolBar; const aPixelSize: integer);
begin
    fEditCutButton := fToolbarUtils.CreateCutToolButton(aToolbar, aPixelSize);

    fEditCopyButton := fToolbarUtils.CreateCopyToolButton(aToolbar, aPixelSize);

    fEditPasteButton := fToolbarUtils.CreatePasteToolButton(aToolbar, aPixelSize);
end;

procedure TMainCustomDevelopment.AddSaveToolButtons(const aToolbar: TToolBar; const aPixelSize: integer);
begin
    fFileSaveAllButton := fToolbarUtils.CreateSaveAllToolButton(aToolbar, aPixelSize);

    fFileSaveButton := fToolbarUtils.CreateSaveToolButton(aToolbar, aPixelSize);
end;

procedure TMainCustomDevelopment.AddStartToolButtons(const aToolbar: TToolBar; const aPixelSize: integer);
begin
    fToolbarUtils.CreateStartToolButton(aToolbar, aPixelSize);
    fToolbarUtils.CreateStartLastStartedToolButton(aToolbar, aPixelSize);
end;

procedure TMainCustomDevelopment.AddRunUtilityToolButtons(const aToolbar: TToolBar;
    const aPixelSize: integer);
begin
    fToolbarUtils.CreateFlushToolButton(aToolbar, aPixelSize);
    fToolbarUtils.CreateInitToolButton(aToolbar, aPixelSize);
end;

procedure TMainCustomDevelopment.DoAddStandardToolBar(const aToolbar: TToolBar; const aPixelSize: integer);
begin
    AddSaveToolButtons(aToolBar, aPixelSize);
    AddEditToolButtons(aToolBar, aPixelSize);
end;

procedure TMainCustomDevelopment.AddStandardToolBar(const aToolbar: TToolBar);
const
    cToolbarSize = 35;
var
    // x : integer;
    xImageList: TCustomImageList;
    // xToolButton : TToolButton;
    xStringLoader: TStringLoader;
begin
    DoAddStandardToolBar(aToolbar, cToolbarSize);
    aToolBar.Width := cToolbarSize;

    fExternalFunctions.OnGetStandardToolbarImageList(self, xImageList);
    fExternalFunctions.OnGetStringLoader(self, xStringLoader);

    aToolbar.Images := xImageList;

    // for x := 0 to aToolbar.ComponentCount - 1 do begin
    // xToolButton := ( aToolbar.Components[x] as TToolButton );
    // xToolButton. := xImageList;
    // end;
    xStringLoader.LoadLanguage(aToolbar);
end;

function TMainCustomDevelopment.GetCurrentEditor: TViewItemEditForm;
begin
    result := nil;
    if not Assigned(PageControl.ActivePage) then
        EXIT;
    if (PageControl.DockClientCount <= PageControl.ActivePageIndex) then
        EXIT;
    if not(PageControl.DockClients[PageControl.ActivePageIndex] is TViewItemEditForm) then
        EXIT;

    result := PageControl.DockClients[PageControl.ActivePageIndex] as TViewItemEditForm;
end;

procedure TMainCustomDevelopment.SaveAll;
var
    x: integer;
begin
    for x := PageControl.DockClientCount - 1 downto 0 do
    begin
        if (PageControl.DockClients[x] is TViewItemEditForm) and
            (PageControl.DockClients[x] as TViewItemEditForm).DataChanged then
        begin
            (PageControl.DockClients[x] as TViewItemEditForm).Save();
        end;
    end;
end;

procedure TMainCustomDevelopment.CloseDockClient(aForm: TForm);
begin
    if Assigned(fExternalFunctions.OnCloseDockClient) then
        fExternalFunctions.OnCloseDockClient(aForm, aemLayoutDevelopment);
end;

procedure TMainCustomDevelopment.RefreshButtons;
begin
    if Assigned(fExternalFunctions.OnRefreshButtons) then
        fExternalFunctions.OnRefreshButtons(self);
end;

procedure TMainCustomDevelopment.DockAndLoadForm(aForm: TDockableEditForm);
begin
    if not Assigned(aForm) then
        EXIT;

    // Diese Reihenfolge ist für alle Editoren gleich,
    // dann wird RefreshButtons nur einmal aufgerufen!
    aForm.ManualDock(PageControl);
    aForm.Show();
    PageControl.ActivePageIndex := PageControl.PageCount - 1;
    aForm.FirstLoad();
end;

function TMainCustomDevelopment.GetDataChanged: boolean;
var
    x: integer;
begin
    result := false;
    for x := PageControl.DockClientCount - 1 downto 0 do
    begin
        if (PageControl.DockClients[x] is TViewItemEditForm) and
            (PageControl.DockClients[x] as TViewItemEditForm).DataChanged then
        begin
            result := true;
            BREAK;
        end;
    end;
end;

function TMainCustomDevelopment.FindDockClient(aCaption: string; aActivate: boolean): TForm;
var
    x: integer;
    xFormCompareCaption: string;
begin
    result := nil;

    for x := 0 to PageControl.DockClientCount - 1 do
    begin
        if (PageControl.DockClients[x] is TViewItemEditForm) then
        begin
            xFormCompareCaption := (PageControl.DockClients[x] as TViewItemEditForm).CompareCaption;
        end
        else if (PageControl.DockClients[x] is TForm) then
        begin
            xFormCompareCaption := (PageControl.DockClients[x] as TForm).Caption;
        end
        else
            CONTINUE;

        if (xFormCompareCaption = aCaption) then
        begin
            result := (PageControl.DockClients[x] as TForm);
            if (aActivate) then
            begin
                PageControl.ActivePageIndex := x;
                self.RefreshButtons();
            end;
            EXIT;
        end;
    end;
end;

procedure TMainCustomDevelopment.CloseCurrentPage();
var
    xPageToClose: integer;
    xNextAvailablePageIndex: integer;
begin
    xPageToClose := PageControl.ActivePageIndex;
    if not(PageControl.DockClients[xPageToClose] is TForm) then
        EXIT;

    // determine the next page that should be opened after this one is closed

    if PageControl.PageCount = 1 then // no more pages
        xNextAvailablePageIndex := -1
    else if PageControl.ActivePageIndex = 0 then // reopen first page
        xNextAvailablePageIndex := 0
    else // reopen previous page
        xNextAvailablePageIndex := xPageToClose - 1;

    self.CloseDockClient(PageControl.DockClients[xPageToClose] as TForm);

    // Set the activeindex so that the next available page is opened in refreshbuttons
    PageControl.ActivePageIndex := xNextAvailablePageIndex;

    self.RefreshButtons(); // nachdem ein Editor geschlossen wurde
end;

function TMainCustomDevelopment.CloseAllPages(): boolean;
var
    x: integer;
begin
    result := true;
    for x := PageControl.DockClientCount - 1 downto 0 do
    begin

        if (PageControl.DockClients[x] is TViewItemEditForm) then
        begin
            if not(PageControl.DockClients[x] as TViewItemEditForm).RequestSaveChanges() then
            begin
                // User hat "Abbrechen" gewählt!
                result := false;
                EXIT;
            end;
        end;

    end;

    while PageControl.DockClientCount > 0 do
    begin
        CloseCurrentPage();
    end;
end;


end.
