{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  13.03.13 wl                                      TN5960   Initial Revision
  14.03.13 wl                                      TN5960   Erweiterungen, um im Runner, die richtigen Menüpunkte zu zeigen
  30.08.13 wl  OpenEditFormInternExt               TN6236   Abfrage auf OpenIsAllowed eingebaut
  30.08.13 wl  HierarchyIsAllowed                  TN6236   nicht, wenn Method Hidden ist
  ----------------------------------------------------------------------------------------------------------- }

unit DesignerViewItemsWorkflow;


interface


uses
    cxTL,
    StdCtrls,
    Classes,
    Controls,
    Types,
    Forms,
    CommonTypes,
    ViewItemEditForm,
    ViewItem,
    AppTypes,
    MethodSettingsDataAdaptor,
    DockableForm,
    ViewAllItems,
    ViewItemTypeLibrary,
    ActionImages,
    MainCustomDevelopment,
    ViewItemsWorkflow;

type
    TDesignerViewItemsWorkflow = class(TViewItemsWorkflow)
    private
        fItemTypeLibrary: TViewItemTypeLibrary;
    protected
        function FindOpenEditor(const aCaption: string; aActivate: boolean): TForm; override;
        procedure FindAndCloseOpenEditor(const aCaption: string); override;
        function OpenEditFormIntern(aViewItem: TViewItem; aItemMustExist: boolean = false)
            : TDockableEditForm; override;
        function OpenEditFormInternExt(aViewItem: TViewItem; aItemMustExist: boolean; aEditMode: TAppEditMode)
            : TDockableEditForm;
    public
        constructor Create(aActionImages: TfrmActionImages);
        destructor Destroy; override;

        procedure AddItemToMethod(aItem: TViewItem); override;
        procedure AddItemToMethodParallel(aItem: TViewItem); override;

        function OpenEditFormExt(const aName: string; aViewType: TViewItemType; aItemMustExist: boolean;
            aEditMode: TAppEditMode): TDockableEditForm;
        function CreateAnyViewItem(aViewType: TViewItemType; const aName: string): TViewItem; override;

        procedure LoadHierarchy(const aName: string); override;
        procedure SearchFor(const aName: string); override;
        procedure LoadLayout(const aLayoutName: string); override;
        procedure UnloadLayout(const aLayoutName: string); override;

        function SearchPossible(): boolean; override;
        function HierarchyIsAllowed(aViewItem: TViewItem): boolean; override;

        // IsAllowed method
        function MethodOpenAndEditable: boolean; override;
    end;


implementation


uses
    SysUtils,
    MethodTypes,
    ParserStoredIdentifier,
    RunStepBuilderTypeDictionary,
    ParserEditIdent,
    AppSettings,
    DialogUtils,
    GeneralTypes,
    UtilLib,
    ConfigurationFile,
    MathUtils,
    MethodVariableTypes,

    // Diese Units sollten hier alle raus:
    DesignerMain,
    MethodEditor,
    SpecialViewItems,
    EditRack,
    EditCarr,
    WorkspaceEditor,
    TipTypeEditor;

{ TDesignerViewItemsWorkflow }

constructor TDesignerViewItemsWorkflow.Create(aActionImages: TfrmActionImages);
begin
    inherited Create(aActionImages, true, dceOpen);

    fItemTypeLibrary := TViewItemTypeLibrary.Create;
end;

function TDesignerViewItemsWorkflow.CreateAnyViewItem(aViewType: TViewItemType; const aName: string)
    : TViewItem;
begin
    EXIT(fItemTypeLibrary.CreateAnyViewItem(aViewType, aName));
end;

destructor TDesignerViewItemsWorkflow.Destroy;
begin
    FreeAndNil(fItemTypeLibrary);

    inherited;
end;

procedure TDesignerViewItemsWorkflow.FindAndCloseOpenEditor(const aCaption: string);
var
    xEditMode: TAppEditMode;
    xEditor: TForm;
begin
    xEditor := frmDesignerMain.FindDockClient(aCaption, false, xEditMode);
    if (xEditor is TViewItemEditForm) then
    begin
        (xEditor as TViewItemEditForm).IsDeleted := true;
        frmDesignerMain.CloseDockClient(xEditor, xEditMode);
        frmDesignerMain.RefreshButtons();
    end;
end;

function TDesignerViewItemsWorkflow.FindOpenEditor(const aCaption: string; aActivate: boolean): TForm;
var
    xEditMode: TAppEditMode;
begin
    EXIT(frmDesignerMain.FindDockClient(aCaption, aActivate, xEditMode));
end;

function TDesignerViewItemsWorkflow.HierarchyIsAllowed(aViewItem: TViewItem): boolean;
begin
    EXIT((aViewItem.ItemType = ntMethod) and OpenIsAllowed(aViewItem));
end;

procedure TDesignerViewItemsWorkflow.LoadHierarchy(const aName: string);
begin
    frmDesignerMain.MethodDevelopment.LoadHierarchy(aName);
end;

procedure TDesignerViewItemsWorkflow.LoadLayout(const aLayoutName: string);
begin
    frmDesignerMain.LoadLayout(aLayoutName);
end;

function TDesignerViewItemsWorkflow.MethodOpenAndEditable: boolean;
begin
    EXIT((frmDesignerMain.CurrentEditor is TfrmMethodEditor) and
        ((frmDesignerMain.CurrentEditor as TfrmMethodEditor).Attribute = meaDefault));
end;

procedure TDesignerViewItemsWorkflow.UnloadLayout(const aLayoutName: string);
begin
    frmDesignerMain.CloseLayout(aLayoutName);
end;

procedure TDesignerViewItemsWorkflow.AddItemToMethod(aItem: TViewItem);
begin
    if (frmDesignerMain.CurrentEditor is TfrmMethodEditor) and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
    begin
        case (aItem.ItemType) of
            ntAction:
                (frmDesignerMain.CurrentEditor as TfrmMethodEditor).AddAction(aItem.Name);
            ntMethod:
                (frmDesignerMain.CurrentEditor as TfrmMethodEditor).AddMethod(aItem.Name);
    end;
end;
end;

procedure TDesignerViewItemsWorkflow.AddItemToMethodParallel(aItem: TViewItem);
begin
    if (frmDesignerMain.CurrentEditor is TfrmMethodEditor) and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
    begin
        case (aItem.ItemType) of
            ntMethod:
                begin
                    (frmDesignerMain.CurrentEditor as TfrmMethodEditor).AddMethodParallel(aItem.Name);
                end;
        end;
    end;
end;

function TDesignerViewItemsWorkflow.OpenEditFormIntern(aViewItem: TViewItem; aItemMustExist: boolean)
    : TDockableEditForm;
begin
    EXIT(OpenEditFormInternExt(aViewItem, aItemMustExist, aemUnknown));
end;

function TDesignerViewItemsWorkflow.OpenEditFormInternExt(aViewItem: TViewItem; aItemMustExist: boolean;
    aEditMode: TAppEditMode): TDockableEditForm;
var
    xForm: TForm;
    xEditMode: TAppEditMode;
begin
    if (aViewItem.Name = '') or not OpenIsAllowed(aViewItem) then
        EXIT(nil);

    // suchen nach offenen Fenster
    xForm := frmDesignerMain.FindDockClient(aViewItem.FullCaption, true, xEditMode);
    if (xForm is TViewItemEditForm) then
        EXIT(xForm as TViewItemEditForm);

    // testen ob es dieses Item gibt
    if (aItemMustExist) and (not aViewItem.ItemNameExists(true, true)) then
        EXIT(nil);

    result := aViewItem.CreateEditForm(Application, frmDesignerMain.AfterSaveStatusChanged);
    frmDesignerMain.DockAndLoadForm(result, aEditMode);
end;

function TDesignerViewItemsWorkflow.OpenEditFormExt(const aName: string; aViewType: TViewItemType;
    aItemMustExist: boolean; aEditMode: TAppEditMode): TDockableEditForm;
var
    xViewItem: TViewItem;
begin
    xViewItem := TViewItemsWorkflow.Instance.CreateAnyViewItem(aViewType, aName);
    try
        EXIT(OpenEditFormInternExt(xViewItem, aItemMustExist, aEditMode))
    finally
        xViewItem.Free;
    end;
end;

procedure TDesignerViewItemsWorkflow.SearchFor(const aName: string);
begin
    frmDesignerMain.SearchFor(aName);
end;

function TDesignerViewItemsWorkflow.SearchPossible: boolean;
begin
    EXIT(true);
end;


end.
