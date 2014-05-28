{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  ViewItems.pas:
  10.08.05 wl                               TN2501.1  initial version
  25.08.05 pk                               TN2547    references to TParserIdentifier class functions changed to TParserStoredIdent method calls
  05.10.05 wl                               TN2575    Funktionen für Menüpunkte: Open, Add(3x), ChangeComments
  08.11.05 wl                               TN2745    uses DatabaseConstants entfernt
  24.11.05 pk  TDatasetViewItem             TN2765    New hierarchy of editor helper-classes
  24.11.05 pk  TViewItemsWorkflow           TN2765    New use TDatasetViewItem for New, open, savas, etc
  30.11.05 wl  TMethodViewItem.AskName      TN2815    prüft Namen mit der Funktion TMethodDataAdaptor.CheckNewName
  30.11.05 wl  TViewItemsWorkflow.AskName   TN2815    ruft TDatasetViewItem.CheckNewName auf
  06.12.05 wl  TViewItemsWorkflow.GetIconIndex  TN2837   Action-Icons werden immer richtig angezeigt, auch wenn z.B. Command statt COMMA geschrieben wird
  03.01.06 wl  TDatasetViewItem.AskNewName      TN2541.0 Name darf gleich dem vorgeschlagenen sein (für CompleteMethodImport)
  03.01.06 wl  TDatasetViewItem.AskSaveAsName   TN2541.0 Name darf nicht gleich dem vorgeschlagenen sein
  04.03.06 wl  TReagentViewItem                 TN2541.4  Anzeige von Reagents (ersetzt ViewReagentsOfLayout)
  04.03.06 wl  TReagentRackViewItem             TN2554    Anzeige von Reagent racks!
  09.05.06 pk  ntGroupTable                     TN3091    New : show Group run lines
  19.05.06 wl  TDatasetViewItem.AskNameDialog    TN3109    Bei Eingabe von best. Zeichenfolge wird HiddenMethodsAreShown = true
  19.05.06 wl  TViewItemsWorkflow.ChangeHidden   TN3109    Changes method attribute "Hidden"
  19.05.06 wl  TViewItemsWorkflow.ChangeReadOnly TN3109    Changes method attribute "ReadOnly"
  12.09.06 wl  TViewItemsWorkflow.GetIconIndex   TN3287    neue Symbole für WGWIZ, REMAR und unbekannte Actions
  03.10.06 wl                                    TN3317    uses LiquidParamEditor, PowderParamEditor
  24.11.06 wl  TMethodViewItem.SaveAs            TN3421    Bei Save as von Methoden werden auch Methodsettings mit kopiert
  28.11.06 wl  TViewItemsWorkflow.DeleteName     TN3434    benutzt frmEdMain.CloseDockClient statt einfach nur Close
  28.11.06 wl  TViewItemsWorkflow.DeleteName     TN3434    IsDeleted wird gesetzt, damit bei Änderungen nicht noch mal nachgefragt wird
  08.12.06 wl                                    TN3459    TRichEdit durch TMemo ersetzt
  19.12.06 wl  TViewItem                         TN3409    von DockableForm hierher
  19.12.06 wl  TViewItemEditForm                 TN3409    neue Zwischenstufe von TDockableEditForm
  19.12.06 wl  TViewItemEditForm.ExcelPut/Get,Start,Build      TN3409    von DockableForm hierher
  19.12.06 wl  viele Methoden                   TN3409    User management überarbeitet
  18.01.07 pk  GetIconIndex                      TN3482    Symbol for ADSEQ action
  16.04.07 wl  TSQLTermViewItem                  TN3547    neu: SQL-Term kann auch wie alles andere editiert werden
  17.04.07 wl  OpenEditForm                      TN3547    neuer Parameter aItemMustExist: Soll geprüft werden, ob Item existiert?
  17.04.07 wl  ItemNameExists                    TN3547    Prüft, ob Item existiert (in der Basisklasse keine Prüfung)
  27.04.07 wl  DeleteName                        TN3669    benutzt die neuen Funktionen ConfirmDeleteName, DeleteNamePhysically & ItemNameExists
  22.06.07 wl  TDatasetViewItem.ItemNameExists   TN3740    Neue Option CaseSensitive und GoodResult (wenn keine spezifische Implementierung existiert, dann gib diesen Wert zurück)
  22.06.07 wl  TViewItemsWorkflow.AskName        TN3740    Neu: Neue Namen werden immer geprüft, ob es sie schon gibt (NOT CASE-SENSITIVE)
  22.06.07 wl  TSessionViewItem.ItemNameExists   TN3740    Neu, benutzt TSessionDataAdaptor.SessionNameExists
  22.06.07 wl  TMethodViewItem.ItemNameExists    TN3740    Neue Option: CaseSensitive
  22.06.07 wl  TPipetteParamViewItem.ItemNameExists  TN3740 Neu, benutzt TLiqHDataAdaptor.LiqParamNameExists
  23.07.07 wl  TWashProgViewItem.GetNames        TN3792    WashProgDataAdaptor ohne Instance
  25.07.07 wl  TViewItemsWorkflow.GetIconIndex   TN3792    WASHP Action mit Washprogram-Icon
  26.07.07 pk  TViewItemsWorkflow.GetIconIndex   TN3547    WRSQL, SLSQL and USQL Actions get the SqlTerms Icon
  26.07.07 wl  TWashProgViewItem.CreateNewName   TN3792    legt neuen Datensatz an
  07.08.07 wl                                    TN3811.3  TCommandDataAdaptor.Create statt Instance
  07.08.07 wl  TSequenceViewItem                 TN3811.3  benutzt ReadAllItemNames
  27.08.07 pk                                    TN3788    Reference to New ParserStoredIdentifier.Pas unit
  30.08.07 pk  TLayoutViewItem                   TN3840.1  New
  03.09.07 pk  CheckNewName                      TN3847    uses TMethodDataAdaptorExt
  03.09.07 pk                                    TN3847    uses LiqHDataAdaptorExt
  02.10.07 wl                                    TN3811.5  diverse interne Änderungen
  09.11.07 pk                                    TN3922    Dataset changed to DataProvider
  19.11.07 wl  TWashProgViewItem.GetTableName    TN3929    Benutzt neuen Tabellennamen für Delete und SaveAs
  07.01.08 pk  OpenEditForm                      TN3864    Set result and exit if Edit Form is already open
  09.01.08 wl                                    TN3972    benutzt gRunStepBuilderTypeDictionary
  09.01.08 wl  GetIconIndex                      TN3972    --> RunStepBuilderTypeDictionary
  09.01.08 wl  const INT_IM_INDEX_..             TN3972    --> MethodTypes
  11.07.08 wl                                    TN4164    für neue Nodes im Designer: Workspace, Rack, Carrier, TipType
  02.09.08 pk                                    TN4125   GetIconIndex function moved to TRunStepInfoFactory
  23.09.08 wl                                    TN4236   uses WashProgFieldnames entfernt
  25.09.08 wl                                    TN4242   Command macros endgültig entfernt
  07.10.08 pk                                    TN4265   Updates the MethodDataCache and LiqHDataCache when items are added/deleted
  13.10.08 pk                                    TN4272.2 DataAdaptor replaced by DataCache for device, driver, connection
  06.11.08 pk                                    TN4279   uses ParserIdentDataType
  19.11.08 pk  AddItemsToMethod3                 TN4280   ThreadStart removed
  17.12.08 pk  SimStart                          TN4372   New
  ViewItemWorkflow.pas
  16.01.09 wl                                    TN4362   alles außer TViewItemsWorkflow entfernt
  06.04.09 pk                                    TN4503   New: ntDisplayComponent
  16.06.09 wl                                    TN4606   alle Bezüge auf ntRunTable und RunEditor entfernt
  27.07.09 pk                                    TN4604   Layouts can now be opened
  11.08.09 wl  ReadAllNames                      TN4702   TStringArray statt TStringList
  24.08.09 wl  AddMethodParallel                 TN4702   vobereitet
  26.10.09 wl  ReadInfoHeight                    TN4831   IConfigurationSet replaces ILocalIniFile
  04.11.09 pk                                 	TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  30.04.10 wl  OpenFirstItem                     TN5052   Öffnet den ersten Namen, der existiert - wenn keiner existiert wird einer erzeugt
  06.05.10 wl                                    TN5052   benutzt OverviewManager, um mehrere Item-Fenster zu verwalten
  06.05.10 wl                                    TN5052   neu: Layout Development Page (nur in ZAArchitect sichtbar)
  06.05.10 wl  EditRepository                    TN5087   Items entfernt
  07.05.10 pk                                    TN5092   CreateWithReaderwritersettings no longer needs type parameter
  10.05.10 wl  CurrentEditMode                   TN5052   Kapselt die Edit-Modi des Hauptfensters
  02.06.10 wl  DeviceDevelopmentName             TN5116   neuer Tab Device-Entwicklung
  04.06.10 wl  ReadDllMenuItems                  TN5116   --> EdExtern
  09.06.10 wl                                    TN5116   neu: DisplayDevelopment
  09.06.10 wl  GetItemTypesByName                TN5116   Layouts editieren nur im DEBUG-Mode möglich (da noch nicht fertig)
  09.06.10 wl  OverviewManager                   TN5116    ViewAllItems ist kein DockableForm mehr
  11.02.11 wl                                    TN5474   Variablen und Session entfernt
  23.02.11 wl                                    TN5486   neu: ImportViewItems
  01.07.11 wl                                    TN5619   keine Extrawurst mehr für Import-Definition
  14.12.11 wl                                    TN5765   ohne Session
  03.02.12 wl                                    TN5792   neue Namen: SubstanceSet,Substance statt ReagentRack,Reagent
  14.03.12 wl  OpenEditForm                      TN5832  neuer optionaler Parameter EditMode: Der Editor wird in einem best. tab geöffnet
  10.12.12 wl  ChangeAttribute                   TN6045   von ZADesignObjects hierher
  14.12.12 wl  TVariableViewItem                 TN6054   entfernt
  22.02.13 wl  OpenVariableModal                 TN6055   an Änderungen angepasst
  13.03.13 wl                                    TN5960   massive Änderungen: Unit von DesignerLib nach Editing verschoben
  13.03.13 wl  OpenPipetteParameter,UnloadLayout  TN5960   von DesignerMain hierher
  14.03.13 ts  GetIndexByType/GetTypeByNode       TN6092   new Icons für Startable methods
  14.03.13 wl                                     TN5960   Erweiterungen, um im Runner, die richtigen Menüpunkte zu zeigen
  15.03.13 wl  CreateOverviewInstance             TN5960   kann ExpandMethodNode aufrufen
  18.03.13 wl  AddNewAndOpen                      TN6045   ViewItem wird nicht neu erzeugt sondern übergeben
  27.03.13 wl  StartItem                          TN6045   IsStartable wird abgefragt
  27.03.13 pp  DeleteWithoutAsking                TN5975   Eintrag ohne Abfrage löschen
  17.04.13 wl  FileImport,FileExport              TN6106   neu
  24.05.13 wl  ReadAllNames                       TN6159   neuer Parameter ShowInTreeList
  15.08.13 wl  OpenPipetteParameter               TN6217   LHDataAdaptor-Instance-Variable gelöscht
  30.08.13 wl  ... IsAllowed                      TN6236   Anzeige der Menüpunkte in Abhängigkeit von User-Rechten verbessert
  03.04.14 ts                                     TN6387   new ntMethodEditableInRunner
  -------------------------------------------------------------------------------------------------- }

unit ViewItemsWorkflow;


interface


uses
    StdCtrls,
    Generics.Collections,
    Forms,
    Controls,
    cxTL,

    ViewItem,
    DockableForm,
    ViewAllItems,
    ActionImages;

type
    TViewItemsWorkflow = class
    private
        fEditAllowed: boolean;
        fOverviewForms: TDictionary<string, TfrmViewAllItems>;
        fActionImages: TfrmActionImages;
        fHiddenMethodsAreShown: boolean;
        fDblClickEventType: TViewItemDblClickEventType;
        class var uInstance: TViewItemsWorkflow;
        class function GetAllowedTypes(aTypes: TViewItemTypes): TViewItemTypes;
        function AskName(aViewItem: TViewItem; var vNewName: string; aIsSaveAs: boolean): boolean;
        function AddNewAndOpen(aViewItem: TViewItem): TDockableEditForm;
        function GetInstanceByName(const aFormName: string): TfrmViewAllItems;
        function SelectImportFile(const aExtension: string): string;
    protected
        function FindOpenEditor(const aCaption: string; aActivate: boolean): TForm; virtual;
        procedure FindAndCloseOpenEditor(const aCaption: string); virtual;
        procedure SetHiddenMethodsAreShown(aValue: boolean); virtual;
        function OpenEditFormIntern(aViewItem: TViewItem; aItemMustExist: boolean = false): TDockableEditForm;
            virtual; abstract;
    public
        constructor Create(aActionImages: TfrmActionImages; aEditAllowed: boolean;
            aDblClickEventType: TViewItemDblClickEventType);
        destructor Destroy; override;

        class procedure SetInstance(aInstance: TViewItemsWorkflow);
        class procedure DestroyInstance;
        class property Instance: TViewItemsWorkflow read uInstance;

        procedure AddItemToMethod(aItem: TViewItem); virtual;
        procedure AddItemToMethodParallel(aItem: TViewItem); virtual;
        function OpenItem(aItem: TViewItem): TDockableEditForm;
        procedure StartItem(const aItem: TViewItemDataRec);

        function GetIndexByType(aItemType: TViewItemType): integer;
        function GetImageIndexByNode(aNode: TcxTreeListNode; const aName: string): integer;
        function ReadAllNames(aViewType: TViewItemType; aShowInTreeList: boolean = false): TArray<string>;

        // Funktionen, die GUI-Controls benutzen
        procedure ShowInfo(aNode: TcxTreeListNode; aMemo: TMemo);
        function GetTypeByNode(aNode: TcxTreeListNode): TViewItemType;
        function OpenEditForm(const aName: string; aViewType: TViewItemType; aItemMustExist: boolean = false)
            : TDockableEditForm;
        function SaveAs(const aSourceName: string; aViewType: TViewItemType): boolean;
        function NewEditForm(aViewType: TViewItemType; const aNewNameDefault: string = ''): TDockableEditForm;
        function FileImportEditForm(aViewType: TViewItemType; const aNewNameDefault: string = '')
            : TDockableEditForm;
        procedure ChooseAndOpen(aViewType: TViewItemType);
        function DeleteName(aViewType: TViewItemType; const aName: string): boolean;
        function DeleteWithoutAsking(aViewType: TViewItemType; const aName: string): boolean;
        procedure OpenFirstItem(aViewType: TViewItemType);
        procedure OpenPipetteParameter(const aName: string);

        function CreateAnyViewItem(aViewType: TViewItemType; const aName: string): TViewItem;
            virtual; abstract;

        procedure LoadHierarchy(const aName: string); virtual;
        procedure SearchFor(const aName: string); virtual;
        procedure LoadLayout(const aLayoutName: string); virtual;
        procedure UnloadLayout(const aLayoutName: string); virtual;
        procedure FileExport(aData: TViewItemDataRec);

        function SearchPossible(): boolean; virtual;
        function HierarchyIsAllowed(aViewItem: TViewItem): boolean; virtual;

        // IsAllowed method
        function OpenIsAllowed(aViewItem: TViewItem): boolean;
        function DeleteIsAllowed(aViewItem: TViewItem): boolean;
        function NewIsAllowed(aViewItem: TViewItem): boolean;
        function FileImportIsAllowed(aViewItem: TViewItem): boolean;
        function FileExportIsAllowed(aViewItem: TViewItem): boolean;
        function SaveAsIsAllowed(aViewItem: TViewItem): boolean;
        function MethodOpenAndEditable: boolean; virtual;
        procedure StartMethod(const aMethodName: string);
        function ShowHiddenReadOnlyFlag(aItemType: TViewItemType): boolean;

        property ActionImages: TfrmActionImages read fActionImages;

        // Overwiew manager
        function CreateOverviewInstance(aCaption: string; aTypes: TViewItemTypes; aFormName: string;
            aNewDockSite: TWinControl; aExpandMethodNode: boolean = false): TfrmViewAllItems;

        procedure OverviewFormsUpdateItems(aUpdateModes: TViewItemTypes);
        procedure OverviewFormsAddChild(aNodeType: TViewItemType; const aNodeName: string);
        procedure OverviewFormsDeleteSelected();
        procedure RegisterAllItemsFormDestruction(aForm: TfrmViewAllItems);
        property HiddenMethodsAreShown: boolean read fHiddenMethodsAreShown write SetHiddenMethodsAreShown;
    end;


implementation


uses
    Dialogs,
    SysUtils,
    DialogUtils,
    FileUtilities,
    CommonTypes,
    Generaltypes,
    EdExtern,
    MethodTypes,
    ViewItemEditForm,
    TipTypeDataAdaptor,
    EditRack,
    EditCarr,
    WorkspaceEditor,
    TipTypeEditor,
    ControlUtils,
    LiqHDataAdaptor,
    AppSettings;

{ TViewItemsWorkflow }

class procedure TViewItemsWorkflow.SetInstance(aInstance: TViewItemsWorkflow);
begin
    uInstance := aInstance;
end;

constructor TViewItemsWorkflow.Create(aActionImages: TfrmActionImages; aEditAllowed: boolean;
    aDblClickEventType: TViewItemDblClickEventType);
begin
    inherited Create;
    fActionImages := aActionImages;
    fOverviewForms := TDictionary<string, TfrmViewAllItems>.Create;
    fHiddenMethodsAreShown := false;
    fEditAllowed := aEditAllowed;
    fDblClickEventType := aDblClickEventType;
end;

destructor TViewItemsWorkflow.Destroy;
begin
    FreeAndNil(fOverviewForms);
    inherited;
end;

class procedure TViewItemsWorkflow.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

function TViewItemsWorkflow.GetTypeByNode(aNode: TcxTreeListNode): TViewItemType;
begin
    result := ntUnknown;

    if not Assigned(aNode) then
        EXIT;

    case (aNode.StateIndex) of
        INT_IM_INDEX_FOLDER:
            result := ntFolder;
        INT_IM_INDEX_METHOD:
            result := ntMethod;
        cImageIndexMethodEditableInRunner:
            result := ntMethodEditableInRunner;
        INT_IM_INDEX_SEQUENCE:
            result := ntSequence;
        INT_IM_INDEX_WASHPROG:
            result := ntWashProg;
        INT_IM_INDEX_ACTION:
            result := ntAction;
        INT_IM_INDEX_LAYOUT:
            result := ntLayout;
        INT_IM_INDEX_LIQUIDPAR:
            result := ntLiquidPar;
        INT_IM_INDEX_POWDERPAR:
            result := ntPowderPar;
        INT_IM_INDEX_REAGENT:
            result := ntSubstance;
        INT_IM_INDEX_REAGENTRACK:
            result := ntSubstanceSet;
        INT_IM_INDEX_SQLTERM:
            result := ntSQLTerm;
        INT_IM_INDEX_DEVICE:
            result := ntDevice;
        INT_IM_INDEX_DRIVER:
            result := ntDriver;
        INT_IM_INDEX_CONNECTION:
            result := ntConnection;
        INT_IM_INDEX_Workspace:
            result := ntWorkspace;
        INT_IM_INDEX_Carrier:
            result := ntCarrier;
        INT_IM_INDEX_Rack:
            result := ntRack;
        INT_IM_INDEX_TipType:
            result := ntTipType;
        INT_IM_INDEX_ImportFileDef:
            result := ntImportFileDef;
        INT_IM_INDEX_TableImportDef:
            result := ntTableImportDef;
        INT_IM_INDEX_VarImportDef:
            result := ntVarImportDef;
        INT_IM_INDEX_DisplayComponent:
            result := ntDisplayComponent;
        INT_IM_INDEX_FAVFOLDER:
            result := ntFavFolder;
        else
            result := ntUnknown;
    end;
end;

function TViewItemsWorkflow.GetImageIndexByNode(aNode: TcxTreeListNode; const aName: string): integer;
var
    xItemType: TViewItemType;
    xViewItem: TViewItem;
begin
    xItemType := GetTypeByNode(aNode);

    if (xItemType in [ntMethod, ntAction, ntMethodEditableInRunner]) then
    begin // wird aus Performance-Gründen nur für die durchgeführt, die wirklich individuelle Icons haben
        xViewItem := self.CreateAnyViewItem(xItemType, aName);
        try
            EXIT(xViewItem.GetIndividualIconIndex());
        finally
            FreeAndNil(xViewItem);
        end;
    end;

    EXIT(aNode.StateIndex);
end;

function TViewItemsWorkflow.GetIndexByType(aItemType: TViewItemType): integer;
begin
    case (aItemType) of
        ntFolder:
            result := INT_IM_INDEX_FOLDER;
        ntMethod:
            result := INT_IM_INDEX_METHOD;
        ntMethodEditableInRunner:
            result := cImageIndexMethodEditableInRunner;
        ntSequence:
            result := INT_IM_INDEX_SEQUENCE;
        ntWashProg:
            result := INT_IM_INDEX_WASHPROG;
        ntAction:
            result := INT_IM_INDEX_ACTION;
        ntLayout:
            result := INT_IM_INDEX_LAYOUT;
        ntLiquidPar:
            result := INT_IM_INDEX_LIQUIDPAR;
        ntPowderPar:
            result := INT_IM_INDEX_POWDERPAR;
        ntSubstance:
            result := INT_IM_INDEX_REAGENT;
        ntSubstanceSet:
            result := INT_IM_INDEX_REAGENTRACK;
        ntSQLTerm:
            result := INT_IM_INDEX_SQLTERM;
        ntDevice:
            result := INT_IM_INDEX_DEVICE;
        ntDriver:
            result := INT_IM_INDEX_DRIVER;
        ntConnection:
            result := INT_IM_INDEX_CONNECTION;
        ntWorkspace:
            result := INT_IM_INDEX_Workspace;
        ntCarrier:
            result := INT_IM_INDEX_Carrier;
        ntRack:
            result := INT_IM_INDEX_Rack;
        ntTipType:
            result := INT_IM_INDEX_TipType;
        ntDisplayComponent:
            result := INT_IM_INDEX_DisplayComponent;
        ntImportFileDef:
            result := INT_IM_INDEX_ImportFileDef;
        ntTableImportDef:
            result := INT_IM_INDEX_TableImportDef;
        ntVarImportDef:
            result := INT_IM_INDEX_VarImportDef;
        ntFavFolder:
            result := INT_IM_INDEX_FAVFOLDER;
        else
            result := INT_IM_INDEX_UNKNOWN;
    end;
end;

function TViewItemsWorkflow.ShowHiddenReadOnlyFlag(aItemType: TViewItemType): boolean;
begin
    EXIT(fEditAllowed and (aItemType = ntMethod));
end;

procedure TViewItemsWorkflow.ShowInfo(aNode: TcxTreeListNode; aMemo: TMemo);
var
    xViewType: TViewItemType;
    xViewItem: TViewItem;
begin
    aMemo.Lines.Clear;

    if not Assigned(aNode) then
        EXIT;

    xViewType := GetTypeByNode(aNode);
    xViewItem := self.CreateAnyViewItem(xViewType, aNode.Texts[0]);
    try
        TControlUtils.AddValuesToMemo(xViewItem.AddInfos(), aMemo, false);
    finally
        xViewItem.Free;
    end;
end;

procedure TViewItemsWorkflow.StartItem(const aItem: TViewItemDataRec);
var
    xViewItem: TViewItem;
begin
    if (aItem.ItemType = ntMethod) then
    begin
        xViewItem := self.CreateAnyViewItem(aItem.ItemType, aItem.name);
        try
            if xViewItem.IsStartable() then
                StartMethod(aItem.name);
        finally
            xViewItem.Free;
        end;
    end;
end;

procedure TViewItemsWorkflow.StartMethod(const aMethodName: string);
begin
    TEdExtern.Instance.MethodStart(aMethodName, false, false);
end;

function TViewItemsWorkflow.OpenItem(aItem: TViewItem): TDockableEditForm;
begin
    result := nil;
    case aItem.ItemType of
        ntAction: // entspricht Add Action
            AddItemToMethod(aItem);
        ntRack:
            begin
                if OpenIsAllowed(aItem) then
                    TfrmEdRack.InstanceShowToEdit(aItem.Name);
            end;
        ntCarrier:
            begin
                if OpenIsAllowed(aItem) then
                    TfrmEdCarr.InstanceShowToEdit(aItem.Name);
            end;
        ntWorkspace:
            begin
                if OpenIsAllowed(aItem) then
                    TfrmWorkspaceEditor.InstanceShowToEdit(aItem.Name);
            end;
        ntTipType:
            begin
                if OpenIsAllowed(aItem) then
                    TfrmEdTipType.EditTipType(aItem.Name);
            end;
        ntLayout:
            begin
                if OpenIsAllowed(aItem) then
                    self.LoadLayout(aItem.Name);
            end;
        ntMethod, ntSequence, ntWashProg, ntLiquidPar, ntPowderPar, ntSubstance, ntSubstanceSet, ntSQLTerm,
            ntDevice, ntDriver, ntConnection, ntDisplayComponent, ntTableImportDef, ntVarImportDef,
            ntImportFileDef, ntMethodEditableInRunner:
            begin
                result := OpenEditFormIntern(aItem);
            end;
    end;
end;

function TViewItemsWorkflow.OpenEditForm(const aName: string; aViewType: TViewItemType;
    aItemMustExist: boolean): TDockableEditForm;
var
    xViewItem: TViewItem;
begin
    xViewItem := TViewItemsWorkflow.Instance.CreateAnyViewItem(aViewType, aName);
    try
        EXIT(OpenEditFormIntern(xViewItem))
    finally
        xViewItem.Free;
    end;
end;

procedure TViewItemsWorkflow.OpenPipetteParameter(const aName: string);
var
    xDA: TLiqHDataAdaptor;
    xUsedTipType: string;
begin
    xDA := TLiqHDataAdaptor.Create;
    try
        if not xDA.ReadUsedTipType(aName, xUsedTipType) then
            EXIT;
    finally
        FreeAndNil(xDA);
    end;

    if TTipTypeDataAdaptor.TipNameIsRedi(xUsedTipType) then
        TViewItemsWorkflow.Instance.OpenEditForm(aName, ntPowderPar)
    else
        TViewItemsWorkflow.Instance.OpenEditForm(aName, ntLiquidPar)
end;

procedure TViewItemsWorkflow.AddItemToMethod(aItem: TViewItem);
begin
    //
end;

procedure TViewItemsWorkflow.AddItemToMethodParallel(aItem: TViewItem);
begin
    //
end;

function TViewItemsWorkflow.ReadAllNames(aViewType: TViewItemType; aShowInTreeList: boolean): TArray<string>;
var
    xViewItem: TViewItem;
begin
    xViewItem := self.CreateAnyViewItem(aViewType, '');
    try
        if aShowInTreeList then
            EXIT(xViewItem.ReadAllNamesForTree)
        else
            EXIT(xViewItem.ReadAllNames);
    finally
        xViewItem.Free;
    end;
end;

function TViewItemsWorkflow.AskName(aViewItem: TViewItem; var vNewName: string; aIsSaveAs: boolean): boolean;
begin
    // ask new method name
    if aIsSaveAs then
        result := aViewItem.AskSaveAsName(vNewName)
    else
        result := aViewItem.AskNewName(vNewName);
    if not result then
        EXIT;

    if (vNewName = ':):):)') then
    begin
        SetHiddenMethodsAreShown(true);
        EXIT(false);
    end;

    result := aViewItem.CheckNewName(vNewName);
    if not result then
        EXIT;

    aViewItem.Name := vNewName;

    // make sure editor for target method is not already open
    if Assigned(self.FindOpenEditor(aViewItem.FullCaption, false)) then
        raise Exception.CreateFmt('A %s with the name [%s] is already being edited',
            [aViewItem.TypeCaption, vNewName]);

    // make sure the item does not already exist (NOT CASE SENSITIVE!)
    if (aViewItem.ItemNameExists(false, false)) then
        raise Exception.CreateFmt('A %s with the name [%s] already exists',
            [aViewItem.TypeCaption, vNewName]);

    result := true;
end;

function TViewItemsWorkflow.AddNewAndOpen(aViewItem: TViewItem): TDockableEditForm;
begin
    aViewItem.AddName(aViewItem.name);

    // add a node to view all items
    OverviewFormsAddChild(aViewItem.ItemType, aViewItem.name);

    // open an editor for this method
    result := OpenItem(aViewItem);
end;

function TViewItemsWorkflow.OpenIsAllowed(aViewItem: TViewItem): boolean;
begin
    if not fEditAllowed then
        EXIT(false);

    EXIT(aViewItem.OpenIsAllowed());
end;

function TViewItemsWorkflow.SaveAs(const aSourceName: string; aViewType: TViewItemType): boolean;
// can be called by viewallitems or by currenteditor.
var
    xNewName: string;
    xEditor: TForm;
    xViewItem: TViewItem;
begin

    result := false;

    xViewItem := self.CreateAnyViewItem(aViewType, aSourceName);
    try
        if not SaveAsIsAllowed(xViewItem) then
            EXIT;

        // save source method, if it is being edited
        xEditor := self.FindOpenEditor(xViewItem.FullCaption, false);
        if (xEditor is TViewItemEditForm) and (not(xEditor as TViewItemEditForm).RequestSaveChanges()) then
            EXIT;

        // ask new method name
        xNewName := aSourceName;
        if not AskName(xViewItem, xNewName, true) then
            EXIT;

        // CFR21: Ask for reason
        if not TAppSettings.ItemConfirmAdd(xViewItem.TypeCaption, xNewName, aSourceName) then
            EXIT;

        // copy method directly in database
        xViewItem.SaveAs(aSourceName, xNewName);

        xViewItem.name := xNewName;
        AddNewAndOpen(xViewItem);

        result := true;
    finally
        FreeAndNil(xViewItem);
    end;
end;

function TViewItemsWorkflow.NewEditForm(aViewType: TViewItemType; const aNewNameDefault: string)
    : TDockableEditForm;
// can be called by viewallitems or by currenteditor.
var
    xNewName: string;
    xViewItem: TViewItem;
begin
    result := nil;

    xViewItem := self.CreateAnyViewItem(aViewType, '');
    try
        if not NewIsAllowed(xViewItem) then
            EXIT;
        // ask new item name
        xNewName := aNewNameDefault;
        if not AskName(xViewItem, xNewName, false) then
            EXIT;

        // CFR21: Ask for reason
        if not TAppSettings.ItemConfirmAdd(xViewItem.TypeCaption, xNewName, '') then
            EXIT;

        // create new dataset
        xViewItem.CreateNewName(xNewName);

        xViewItem.name := xNewName;
        result := AddNewAndOpen(xViewItem);
    finally
        xViewItem.Free;
    end;
end;

procedure TViewItemsWorkflow.ChooseAndOpen(aViewType: TViewItemType);
var
    xViewItem: TViewItem;
    xChosenName: string;
    xNames: TArray<string>;
begin
    xViewItem := self.CreateAnyViewItem(aViewType, '');
    try
        xNames := xViewItem.ReadAllNames();
        xChosenName := TDialogUtils.SelectItemBox(xNames, TLanguageString.Read('{0} name:', '{0}-Name:',
            [xViewItem.TypeCaption]), TLanguageString.Read('Open {0}', '{0} öffnen', [xViewItem.TypeCaption]),
            TLanguageString.Read('Open', 'Öffnen'));

        OpenEditForm(xChosenName, aViewType);
    finally
        xViewItem.Free;
    end;
end;

procedure TViewItemsWorkflow.OpenFirstItem(aViewType: TViewItemType);
const
    cDefaultName: string = 'Default';
var
    xViewItem: TViewItem;
    xNames: TArray<string>;
begin
    xViewItem := self.CreateAnyViewItem(aViewType, '');
    try
        xNames := xViewItem.ReadAllNames();
        if Length(xNames) > 0 then
        begin
            xViewItem.name := xNames[0];
            OpenItem(xViewItem);
        end
        else
        begin
            // create new dataset
            xViewItem.CreateNewName(cDefaultName);
            xViewItem.name := cDefaultName;
            AddNewAndOpen(xViewItem);
        end;
    finally
        xViewItem.Free;
    end;
end;

function TViewItemsWorkflow.DeleteIsAllowed(aViewItem: TViewItem): boolean;
begin
    EXIT(fEditAllowed and aViewItem.DeleteIsAllowed() and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin));
end;

function TViewItemsWorkflow.NewIsAllowed(aViewItem: TViewItem): boolean;
begin
    EXIT(fEditAllowed and aViewItem.NewIsAllowed() and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin));
end;

function TViewItemsWorkflow.SaveAsIsAllowed(aViewItem: TViewItem): boolean;
begin
    EXIT(fEditAllowed and aViewItem.SaveAsIsAllowed() and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin));
end;

function TViewItemsWorkflow.DeleteName(aViewType: TViewItemType; const aName: string): boolean;
var
    xViewItem: TViewItem;
begin
    xViewItem := self.CreateAnyViewItem(aViewType, aName);
    try
        result := xViewItem.DeleteName();
        if not result then
            EXIT;

        // Offene Editoren schließen
        self.FindAndCloseOpenEditor(xViewItem.FullCaption);
    finally
        FreeAndNil(xViewItem);
    end;

    self.OverviewFormsDeleteSelected();
end;

function TViewItemsWorkflow.DeleteWithoutAsking(aViewType: TViewItemType; const aName: string): boolean;
var
    xViewItem: TViewItem;
begin
    xViewItem := self.CreateAnyViewItem(aViewType, aName);
    try
        result := xViewItem.DeleteWithoutAsking();
        if not result then
            EXIT;

        // Offene Editoren schließen
        self.FindAndCloseOpenEditor(xViewItem.FullCaption);
    finally
        FreeAndNil(xViewItem);
    end;

    self.OverviewFormsDeleteSelected();
end;

class function TViewItemsWorkflow.GetAllowedTypes(aTypes: TViewItemTypes): TViewItemTypes;
var
    xUseWashPrograms, xUseSequences, xUseSubstances, xUseSubstanceSets: boolean;
    xUseLiquids: boolean;
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateAppIni;
    xUseWashPrograms := xIniAccess.ReadBool('Display', 'UseWashPrograms');
    xUseSequences := xIniAccess.ReadBool('Display', 'UseSequences');
    xUseSubstanceSets := xIniAccess.ReadBool('Display', 'UseReagentRacks');
    xUseSubstances := xIniAccess.ReadBool('Display', 'UseSubstances');
    xUseLiquids := xIniAccess.ReadBool('Display', 'UseLiquids');

    result := aTypes;

    if (not TAppSettings.IsRedi) then
    begin
        result := result - [ntPowderPar];
    end;
    if not((TAppSettings.IsSophas) and (xUseSequences)) then
    begin
        result := result - [ntSequence];
    end;
    if not((TAppSettings.IsSophas) and (xUseSubstanceSets)) then
    begin
        result := result - [ntSubstanceSet];
    end;
    if not(xUseSubstances) then
    begin
        result := result - [ntSubstance];
    end;
    if not((TAppSettings.IsSophas) and (xUseWashPrograms)) then
    begin
        result := result - [ntWashProg];
    end;
    if (TAppSettings.IsOneWorkspaceMode) then
    begin
        result := result - [ntWorkspace];
    end;
    if (not TAppSettings.UseCarriers) then
    begin
        result := result - [ntCarrier];
    end;
    if (TAppSettings.IsOneTipTypeMode) then
    begin
        result := result - [ntTipType];
    end;
    if (not TAppSettings.UseDisplayComponents) then
    begin
        result := result - [ntDisplayComponent];
    end;
    if not(xUseLiquids) then
    begin
        result := result - [ntLiquidPar];
    end;
end;

function TViewItemsWorkflow.GetInstanceByName(const aFormName: string): TfrmViewAllItems;
begin
    if not fOverviewForms.TryGetValue(aFormName, result) then
        result := nil;
end;

function TViewItemsWorkflow.CreateOverviewInstance(aCaption: string; aTypes: TViewItemTypes;
    aFormName: string; aNewDockSite: TWinControl; aExpandMethodNode: boolean): TfrmViewAllItems;
begin
    result := GetInstanceByName(aFormName);
    if Assigned(result) then
    begin
        EXIT;
    end;

    // neues Fester erzeugen
    result := TfrmViewAllItems.Create(Application, aFormName, fDblClickEventType);
    fOverviewForms.Add(aFormName, result);

    result.CreateTopNodes(GetAllowedTypes(aTypes), aCaption);
    result.UpdateTV([ntAll]);
    if (aExpandMethodNode) then
        result.ExpandMethodNode();

    result.JustStarted := false;
    TViewItemsWorkflow.Instance.ShowInfo(result.cxTreeList1.FocusedNode, result.Memo1);

    result.Parent := aNewDockSite;
    result.Align := alClient;
    result.Visible := true;
end;

procedure TViewItemsWorkflow.OverviewFormsAddChild(aNodeType: TViewItemType; const aNodeName: string);
var
    xPair: TPair<string, TfrmViewAllItems>;
begin
    for xPair in fOverviewForms do
    begin
        xPair.Value.AddChildAsLast(aNodeType, aNodeName);
    end;
end;

procedure TViewItemsWorkflow.OverviewFormsDeleteSelected;
var
    xPair: TPair<string, TfrmViewAllItems>;
begin
    for xPair in fOverviewForms do
    begin
        xPair.Value.DeleteSelectedNode();
    end;
end;

procedure TViewItemsWorkflow.OverviewFormsUpdateItems(aUpdateModes: TViewItemTypes);
var
    xPair: TPair<string, TfrmViewAllItems>;
begin
    for xPair in fOverviewForms do
    begin
        xPair.Value.UpdateTV(aUpdateModes);
    end;
end;

procedure TViewItemsWorkflow.RegisterAllItemsFormDestruction(aForm: TfrmViewAllItems);
var
    xPair: TPair<string, TfrmViewAllItems>;
begin
    for xPair in fOverviewForms do
    begin
        if aForm = xPair.Value then
        begin
            fOverviewForms.Remove(xPair.Key);
            EXIT;
        end;
    end;
end;

procedure TViewItemsWorkflow.SearchFor(const aName: string);
begin
    //
end;

function TViewItemsWorkflow.HierarchyIsAllowed(aViewItem: TViewItem): boolean;
begin
    EXIT(false);
end;

function TViewItemsWorkflow.SearchPossible: boolean;
begin
    EXIT(false);
end;

function TViewItemsWorkflow.SelectImportFile(const aExtension: string): string;
var
    xOpenDialog: TOpenDialog;
begin
    xOpenDialog := TOpenDialog.Create(nil);
    try
        xOpenDialog.InitialDir := TAppSettings.DataPath;
        // xOpenDialog.FileName := FImageFileEdit.Text;
        xOpenDialog.Filter := TLanguageString.Read('Import files (*.' + aExtension + ')|*.' + aExtension +
            '|', 'Importdateien (*.' + aExtension + ')|*.' + aExtension + '|');
        if (xOpenDialog.Execute) then
            EXIT(xOpenDialog.FileName)
        else
            EXIT('');
    finally
        xOpenDialog.Free;
    end;
end;

procedure TViewItemsWorkflow.SetHiddenMethodsAreShown(aValue: boolean);
begin
    fHiddenMethodsAreShown := aValue;
    TViewItemsWorkflow.Instance.OverviewFormsUpdateItems([ntMethod]);
end;

procedure TViewItemsWorkflow.FindAndCloseOpenEditor(const aCaption: string);
begin

end;

function TViewItemsWorkflow.FindOpenEditor(const aCaption: string; aActivate: boolean): TForm;
begin
    EXIT(nil);
end;

procedure TViewItemsWorkflow.UnloadLayout(const aLayoutName: string);
begin
    //
end;

procedure TViewItemsWorkflow.LoadHierarchy(const aName: string);
begin
    //
end;

procedure TViewItemsWorkflow.LoadLayout(const aLayoutName: string);
begin
    //
end;

function TViewItemsWorkflow.MethodOpenAndEditable: boolean;
begin
    EXIT(false);
end;

procedure TViewItemsWorkflow.FileExport(aData: TViewItemDataRec);
var
    xViewItem: TViewItem;
begin
    if (aData.ItemType = ntMethod) then
    begin
        xViewItem := self.CreateAnyViewItem(aData.ItemType, aData.name);
        try
            xViewItem.FileExport();
        finally
            xViewItem.Free;
        end;
    end;
end;

function TViewItemsWorkflow.FileExportIsAllowed(aViewItem: TViewItem): boolean;
begin
    EXIT(fEditAllowed and aViewItem.FileExportIsAllowed and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystem));
end;

function TViewItemsWorkflow.FileImportEditForm(aViewType: TViewItemType; const aNewNameDefault: string)
    : TDockableEditForm;
var
    xNewName, xImportFileName: string;
    xViewItem: TViewItem;
begin
    result := nil;

    xViewItem := self.CreateAnyViewItem(aViewType, '');
    try
        if not NewIsAllowed(xViewItem) then
            EXIT;

        xImportFileName := SelectImportFile(xViewItem.FileExportExtension);
        if xImportFileName = '' then
            EXIT;

        // ask new item name
        xNewName := TFileUtilities.DeleteExtension(ExtractFileName(xImportFileName));
        if not AskName(xViewItem, xNewName, true) then
            EXIT;

        // CFR21: Ask for reason
        if not TAppSettings.ItemConfirmAdd(xViewItem.TypeCaption, xNewName, '') then
            EXIT;

        xViewItem.name := xNewName;
        xViewItem.FileImport(xImportFileName);

        // create new dataset
        // xViewItem.CreateNewName(xNewName);

        result := AddNewAndOpen(xViewItem);
    finally
        xViewItem.Free;
    end;
end;

function TViewItemsWorkflow.FileImportIsAllowed(aViewItem: TViewItem): boolean;
begin
    EXIT(fEditAllowed and aViewItem.FileImportIsAllowed and aViewItem.NewIsAllowed() and
        gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin));
end;


end.
