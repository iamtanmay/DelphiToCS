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
  07.03.13 ts  GetIndexByType/GetTypeByNode      TN6092   neue Icons für startable methods
  -------------------------------------------------------------------------------------------------- }

unit ViewItemsWorkflow;


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
    DockableForm,
    GeneralTypes,
    ViewItemOverviewManager;

type
    TViewItemsWorkflow = record
    private
        class var uOverviewManager: TViewItemOverviewManager;
        class function GetCurrentEditMode: TAppEditMode; static;
    public
        class function OpenItem(const aItem: TViewItemDataRec): TDockableEditForm; static;
        class procedure AddItemToMethod(const aItem: TViewItemDataRec); static;
        class procedure AddItemToMethodParallel(const aItem: TViewItemDataRec); static;
        class function ChangeReadOnly(const aItem: TViewItemDataRec): boolean; static;
        class function ChangeHidden(const aItem: TViewItemDataRec): boolean; static;

        class procedure OpenVariableModal(const aVarName: string); static;
        class function GetIndexByType(aItemType: TViewItemType): integer; static;

        class function ReadInfoHeight(const aFormName: string; aDefault: integer): integer; static;
        class procedure WriteInfoHeight(const aFormName: string; aHeight: integer); static;

        // Funktionen, die GUI-Controls benutzen
        class procedure ShowInfo(aNode: TcxTreeListNode; aMemo: TMemo); static;
        class function GetTypeByNode(aNode: TcxTreeListNode): TViewItemType; static;

        class function ReadAllNames(aViewType: TViewItemType): TStringArray; static;
        class function GetNames(aViewType: TViewItemType): TStringArray; static;

        class function OpenEditForm(const aName: string; aViewType: TViewItemType;
            aItemMustExist: boolean = false; aEditMode: TAppEditMode = aemUnknown): TDockableEditForm; static;
        class function AskName(aViewItem: TViewItem; var vNewName: string; aIsSaveAs: boolean)
            : boolean; static;
        class function AddNewAndOpen(aViewType: TViewItemType; const aNewName: string)
            : TDockableEditForm; static;
        class function SaveAs(const aSourceName: string; aViewType: TViewItemType): boolean; static;
        class function NewEditForm(aViewType: TViewItemType; const aNewNameDefault: string = '')
            : TDockableEditForm; static;
        class procedure ChooseAndOpen(aViewType: TViewItemType); static;
        class function DeleteName(aViewType: TViewItemType; const aName: string): boolean; static;
        class procedure OpenFirstItem(aViewType: TViewItemType); static;

        // IsAllowed method
        class function OpenIsAllowed(aViewType: TViewItemType): boolean; static;
        class property CurrentEditMode: TAppEditMode read GetCurrentEditMode;

        class property OverviewManager: TViewItemOverviewManager read uOverviewManager write uOverviewManager;
    end;


implementation


uses
    SysUtils,
    MethodTypes,
    ParserStoredIdentifier,
    MethodSettings,
    RunStepBuilderTypeDictionary,
    ParserIdentDataType,
    ParserEditIdent,
    ZADesignObjects,
    ZADesignMain,
    AppSettings,
    DialogUtils,
    ViewItemTypeLibrary,
    UtilLib,
    ConfigurationFile,
    MathUtils,

    // Diese Units sollten hier alle raus:
    SpecialViewItems,
    MethodEditor,
    EditRack,
    EditCarr,
    WorkspaceEditor,
    TipTypeEditor;

{ TViewItemsWorkflow }

class function TViewItemsWorkflow.GetTypeByNode(aNode: TcxTreeListNode): TViewItemType;
begin
    result := ntUnknown;

    if not Assigned(aNode) then
        EXIT;

    case (aNode.StateIndex) of
        INT_IM_INDEX_FOLDER:
            result := ntFolder;
        INT_IM_INDEX_METHOD, INT_IM_INDEX_STARTABLEMETHOD:
            result := ntMethod;
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
        // INT_IM_INDEX_VARIABLE  : result := ntVariable;
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

class function TViewItemsWorkflow.GetIndexByType(aItemType: TViewItemType): integer;
begin
    case (aItemType) of
        ntFolder:
            result := INT_IM_INDEX_FOLDER;
        ntMethod:
            result := INT_IM_INDEX_METHOD;
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
        // ntVariable : result := INT_IM_INDEX_VARIABLE;
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
        ntStartableMethod:
            result := INT_IM_INDEX_STARTABLEMETHOD;
        else
            result := INT_IM_INDEX_UNKNOWN;
    end;
end;

class procedure TViewItemsWorkflow.ShowInfo(aNode: TcxTreeListNode; aMemo: TMemo);
var
    xViewType: TViewItemType;
    xViewItem: TViewItem;
begin
    aMemo.Lines.Clear;

    if not Assigned(aNode) then
        EXIT;

    xViewType := GetTypeByNode(aNode);

    xViewItem := TViewItemTypeLibrary.CreateAnyViewItem(xViewType, aNode.Texts[0]);
    try
        xViewItem.AddInfos(aMemo.Lines);
    finally
        xViewItem.Free;
    end;
end;

class function TViewItemsWorkflow.OpenItem(const aItem: TViewItemDataRec): TDockableEditForm;
begin
    result := nil;
    case aItem.ItemType of
        ntAction:
            begin // entspricht Add Action
                if (frmEdMain.CurrentEditor is TfrmMethodEditor) and
                    gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
                    (frmEdMain.CurrentEditor as TfrmMethodEditor).AddAction(aItem.Name);
            end;

        ntVariable:
            begin
                if TViewItemsWorkflow.OpenIsAllowed(aItem.ItemType) then
                    OpenVariableModal(aItem.Name);
            end;
        ntRack:
            begin
                if TViewItemsWorkflow.OpenIsAllowed(aItem.ItemType) then
                    TfrmEdRack.InstanceShowToEdit(aItem.Name);
            end;
        ntCarrier:
            begin
                if TViewItemsWorkflow.OpenIsAllowed(aItem.ItemType) then
                    TfrmEdCarr.InstanceShowToEdit(aItem.Name);
            end;
        ntWorkspace:
            begin
                if TViewItemsWorkflow.OpenIsAllowed(aItem.ItemType) then
                    TfrmWorkspaceEditor.InstanceShowToEdit(aItem.Name);
            end;
        ntTipType:
            begin
                if TViewItemsWorkflow.OpenIsAllowed(aItem.ItemType) then
                    TfrmEdTipType.EditTipType(aItem.Name);
            end;
        ntLayout:
            begin
                if TViewItemsWorkflow.OpenIsAllowed(aItem.ItemType) then
                    frmEdMain.LoadLayout(aItem.Name);
            end;
        ntMethod, ntSequence, ntWashProg, ntLiquidPar, ntPowderPar, ntSubstance, ntSubstanceSet, ntSQLTerm,
            ntDevice, ntDriver, ntConnection, ntDisplayComponent, ntTableImportDef, ntVarImportDef,
            ntImportFileDef:
            begin
                if TViewItemsWorkflow.OpenIsAllowed(aItem.ItemType) then
                    result := TViewItemsWorkflow.OpenEditForm(aItem.Name, aItem.ItemType);
            end;
    end;
end;

class procedure TViewItemsWorkflow.OpenVariableModal(const aVarName: string);
var
    xEditIdentForm: TfrmParserEditIdent;
    xIdent: TParserStoredIdent;
begin
    xIdent := TParserStoredIdent.CreateWithReaderWriterSettings(aVarName);
    xEditIdentForm := TfrmParserEditIdent.Create(Application, xIdent);
    xEditIdentForm.ShowModal;
    xEditIdentForm.Free;
    xIdent.Free;
end;

class procedure TViewItemsWorkflow.AddItemToMethod(const aItem: TViewItemDataRec);
begin
    if (frmEdMain.CurrentEditor is TfrmMethodEditor) then
    begin
        case (aItem.ItemType) of
            ntAction:
                (frmEdMain.CurrentEditor as TfrmMethodEditor).AddAction(aItem.Name);
            ntMethod:
                (frmEdMain.CurrentEditor as TfrmMethodEditor).AddMethod(aItem.Name);
    end;
end;
end;

class procedure TViewItemsWorkflow.AddItemToMethodParallel(const aItem: TViewItemDataRec);
begin
    if (frmEdMain.CurrentEditor is TfrmMethodEditor) then
    begin
        case (aItem.ItemType) of
            ntMethod:
                (frmEdMain.CurrentEditor as TfrmMethodEditor).AddMethodParallel(aItem.Name);
    end;
end;
end;

class function TViewItemsWorkflow.ChangeHidden(const aItem: TViewItemDataRec): boolean;
begin
    result := false;
    case (aItem.ItemType) of
        ntMethod:
            begin
                result := TZADesignLinkAll.ChangeAttribute(aItem.Name, msaHidden);
            end;
    end;
end;

class function TViewItemsWorkflow.ChangeReadOnly(const aItem: TViewItemDataRec): boolean;
begin
    result := false;
    case (aItem.ItemType) of
        ntMethod:
            begin
                result := TZADesignLinkAll.ChangeAttribute(aItem.Name, msaReadOnly);
            end;
    end;
end;

class function TViewItemsWorkflow.ReadInfoHeight(const aFormName: string; aDefault: integer): integer;
var
    xLocalIniFile: IConfigurationSet;
begin
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);
    try
        result := xLocalIniFile.ReadInteger('InfoHeight', aFormName, aDefault);
    finally
        xLocalIniFile.Close;
    end;
end;

class procedure TViewItemsWorkflow.WriteInfoHeight(const aFormName: string; aHeight: integer);
var
    xLocalIniFile: IConfigurationSet;
begin
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(false);
    try
        xLocalIniFile.WriteInteger('InfoHeight', aFormName, aHeight);
    finally
        xLocalIniFile.Close;
    end;
end;

class function TViewItemsWorkflow.GetNames(aViewType: TViewItemType): TStringArray;
begin
    result := ReadAllNames(aViewType);
end;

class function TViewItemsWorkflow.ReadAllNames(aViewType: TViewItemType): TStringArray;
var
    xViewItem: TViewItem;
begin
    xViewItem := TViewItemTypeLibrary.CreateAnyViewItem(aViewType, '');
    try
        result := xViewItem.ReadAllNames;
    finally
        xViewItem.Free;
    end;
end;

class function TViewItemsWorkflow.OpenEditForm(const aName: string; aViewType: TViewItemType;
    aItemMustExist: boolean; aEditMode: TAppEditMode): TDockableEditForm;
var
    xForm: TForm;
    xViewItem: TViewItem;
    xEditMode: TAppEditMode;
begin
    result := nil;
    if (aName = '') then
        EXIT;

    xViewItem := TViewItemTypeLibrary.CreateAnyViewItem(aViewType, aName);
    try
        // suchen nach offenen Fenster
        xForm := frmEdMain.FindDockClient(xViewItem.FullCaption, true, xEditMode);
        if (xForm is TViewItemEditForm) then
        begin
            result := xForm as TViewItemEditForm;
            EXIT;
        end;

        // testen ob es dieses Item gibt
        if (aItemMustExist) and (not xViewItem.ItemNameExists(true, true)) then
            EXIT;

        result := xViewItem.CreateEditForm(Application, frmEdMain.AfterSaveStatusChanged);
        frmEdMain.DockAndLoadForm(result, aEditMode);
    finally
        xViewItem.Free;
    end;
end;

class function TViewItemsWorkflow.AskName(aViewItem: TViewItem; var vNewName: string;
    aIsSaveAs: boolean): boolean;
var
    xEditMode: TAppEditMode;
begin
    // ask new method name
    if aIsSaveAs then
        result := aViewItem.AskSaveAsName(vNewName)
    else
        result := aViewItem.AskNewName(vNewName);

    if (vNewName = ':):):)') then
    begin
        frmEdMain.HiddenMethodsAreShown := true;
        result := false;
    end;

    if not result then
        EXIT;

    result := aViewItem.CheckNewName(vNewName);
    if not result then
        EXIT;

    // make sure editor for target method is not already opend FindDockClient( xNewName )
    aViewItem.Name := vNewName;
    if Assigned(frmEdMain.FindDockClient(aViewItem.FullCaption, false, xEditMode)) then
        raise Exception.CreateFmt('A %s with the name [%s] is already being edited',
            [aViewItem.TypeCaption, vNewName]);

    // make sure the item does not already exist (NOT CASE SENSITIVE!)
    if (aViewItem.ItemNameExists(false, false)) then
        raise Exception.CreateFmt('A %s with the name [%s] already exists',
            [aViewItem.TypeCaption, vNewName]);

    result := true;
end;

class function TViewItemsWorkflow.AddNewAndOpen(aViewType: TViewItemType; const aNewName: string)
    : TDockableEditForm;
var
    xViewItem: TViewItem;
    xItem: TViewItemDataRec;
begin
    xViewItem := TViewItemTypeLibrary.CreateAnyViewItem(aViewType, aNewName);
    try
        xViewItem.AddName(aNewName);

        // add a node to view all items
        uOverviewManager.InstanceAddChildAsFirstOrLast(aViewType, aNewName);

        // open an editor for this method
        xItem.Name := aNewName;
        xItem.ItemType := aViewType;
        result := OpenItem(xItem);
    finally
        FreeAndNil(xViewItem);
    end;
end;

class function TViewItemsWorkflow.OpenIsAllowed(aViewType: TViewItemType): boolean;
begin
    result := false;

    if (aViewType = ntVariable) // Variablen-Interna braucht nur der Admin zu sehen
        and gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
    begin
        result := true;
        EXIT;
    end;

    if (aViewType in [ntMethod, ntSequence, ntWashProg, ntSQLTerm, ntLiquidPar, ntPowderPar, ntSubstance,
        ntSubstanceSet, ntDevice, ntDriver, ntConnection, ntRack, ntCarrier, ntWorkspace, ntTipType,
        ntDisplayComponent, ntLayout, ntVarImportDef, ntTableImportDef, ntImportFileDef]) then
    begin
        result := true;
        EXIT;
    end;
end;

class function TViewItemsWorkflow.SaveAs(const aSourceName: string; aViewType: TViewItemType): boolean;
// can be called by viewallitems or by currenteditor.
var
    xNewName: string;
    xEditor: TViewItemEditForm;
    xViewItem: TViewItem;
    xEditMode: TAppEditMode;
begin

    result := false;

    xViewItem := TViewItemTypeLibrary.CreateAnyViewItem(aViewType, aSourceName);
    try
        if not xViewItem.SaveAsIsAllowed() then
            EXIT;

        // save source method, if it is being edited
        xEditor := frmEdMain.FindDockClient(xViewItem.FullCaption, false, xEditMode) as TViewItemEditForm;
        if Assigned(xEditor) and (not xEditor.RequestSaveChanges()) then
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

        AddNewAndOpen(aViewType, xNewName);

        result := true;
    finally
        FreeAndNil(xViewItem);
    end;
end;

class function TViewItemsWorkflow.NewEditForm(aViewType: TViewItemType; const aNewNameDefault: string)
    : TDockableEditForm;
// can be called by viewallitems or by currenteditor.
var
    xNewName: string;
    xViewItem: TViewItem;
begin
    result := nil;

    xViewItem := TViewItemTypeLibrary.CreateAnyViewItem(aViewType, '');
    try
        if not xViewItem.NewIsAllowed() then
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

        result := AddNewAndOpen(aViewType, xNewName);
    finally
        xViewItem.Free;
    end;
end;

class procedure TViewItemsWorkflow.ChooseAndOpen(aViewType: TViewItemType);
var
    xViewItem: TViewItem;
    xChosenName: string;
    xNames: TStringArray;
begin
    xViewItem := TViewItemTypeLibrary.CreateAnyViewItem(aViewType, '');
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

class function TViewItemsWorkflow.GetCurrentEditMode: TAppEditMode;
begin
    result := frmEdMain.CurrentEditMode;
end;

class procedure TViewItemsWorkflow.OpenFirstItem(aViewType: TViewItemType);
const
    cDefaultName: string = 'Default';
var
    xViewItem: TViewItem;
    xNames: TStringArray;
    xItem: TViewItemDataRec;
begin
    xViewItem := TViewItemTypeLibrary.CreateAnyViewItem(aViewType, '');
    try
        xNames := xViewItem.ReadAllNames();
        if Length(xNames) > 0 then
        begin
            xItem.Name := xNames[0];
            xItem.ItemType := aViewType;
            OpenItem(xItem);
        end
        else
        begin
            // create new dataset
            xViewItem.CreateNewName(cDefaultName);
            AddNewAndOpen(aViewType, cDefaultName);
        end;
    finally
        xViewItem.Free;
    end;
end;

class function TViewItemsWorkflow.DeleteName(aViewType: TViewItemType; const aName: string): boolean;
var
    xViewItem: TViewItem;
    xEditor: TForm;
    xEditMode: TAppEditMode;
begin
    xViewItem := TViewItemTypeLibrary.CreateAnyViewItem(aViewType, aName);
    try
        result := xViewItem.DeleteName();
        if not result then
            EXIT;

        xEditor := frmEdMain.FindDockClient(xViewItem.FullCaption, false, xEditMode);
        if (xEditor is TViewItemEditForm) then
        begin
            (xEditor as TViewItemEditForm).IsDeleted := true;
            frmEdMain.CloseDockClient(xEditor, xEditMode);
            frmEdMain.RefreshButtons();
        end;
    finally
        FreeAndNil(xViewItem);
    end;

    uOverviewManager.InstanceDeleteSelectedNode();
end;


end.
