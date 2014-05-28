{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  : Dockable Method Explorer Form
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  06.08.04 wl                               TN2008.2  initial version
  06.08.04 wl  TV                           TN2008.2  Treeview with all functionality from EdMain
  06.08.04 wl  FormCreate                   TN2008.2  if not IsSophas -> no Washprog- & Sequence-Node
  17.08.04 wl                               TN2008.2  Fenster läßt sich jetzt schließen
  17.08.04 wl                               TN2008.2  Refresh-Button entfernt
  17.08.04 wl                               TN2008.2  jetzt als singleton
  24.08.04 wl                               TN2008.2  mit Laden und Speichern der DockableForms
  24.09.04 wl                               TN2008.2  Laden und Speichern verbessert
  03.12.04 wl                               TN2008.2  alles bezüglich Script entfernt
  28.04.05 wl                               TN2248.8  jetzt mit Variablen
  20.06.05 wl  ShowInfo                     TN2441    Zeigt Methoden-bezogene Kommentare
  22.06.05 wl                               TN2440    uses MethodEditor
  06.07.05 wl  FormActivate                 TN2495    beim Activate wird Info-Fenster neu erstellt
  12.07.05 wl  UpdateTV                     TN2439.3  unter Layouts werden jetzt auch die Layout angezeigt!
  01.08.05 wl                               TN2506    uses LiqHDataAdaptor statt PipetteParamDataAdaptor
  05.08.05 wl                               TN2501.1  Sessions werden mit angezeigt
  10.08.05 wl                               TN2501.1  Sessions: New, Open, Delete implementiert
  10.08.05 wl                               TN2501.1  EdExplorer in ViewAllItems umbenannt
  10.08.05 wl                               TN2501.1  ViewAllItems-Ressourcen ausgelagert
  17.08.05 wl                               TN2558.6  Das Methodenfenster wird geschlossen, wenn die Methode gelöscht wurde
  22.08.05 wl                               TN2558.8  uses ScheUtil entfernt, ScheUtil-Methoden werden mit TDMScript. oder global. aufgerufen
  25.08.05 pk                               TN2547    references to TParserIdentifier class functions changed to TParserStoredIdent method calls
  08.09.05 wl  AddChildAsFirstOrLast        TN2595    Bei SaveAs wird jetzt der Fokus auf das neue Element gesetzt
  05.10.05 wl  pmnuChangeCommentsClick      TN2584    neuer Menüpunkt "Change Comments"
  12.10.05 wl  pmnuSaveAsClick              TN2595    ruft frmEdMain.FillRepositoryLiqParItems auf
  12.10.05 wl  pmnuDeleteClick              TN2595    ruft frmEdMain.FillRepositoryLiqParItems auf
  08.11.05 wl                               TN2745    uses DatabaseConstants entfernt
  15.11.05 pk  DeleteChild                  TN2671    Do not call UpdateTV
  15.11.05 pk  pmnuRefreshClick             TN2776    Refresh list: delete all children and reload
  24.11.05 pk                               TN2765    use TViewItemsWorkflow class for open, new, etcs
  04.03.06 wl                               TN2541.4  Anzeige von Reagents (ersetzt ViewReagentsOfLayout)
  04.03.06 wl                               TN2554    Anzeige von Reagent racks!
  04.03.06 wl                               TN2553    SOPHAS: Anzeige von Sequences und Washprograms kann mit Use..-Einträgen unterdrückt werden
  19.05.06 wl  pmnuReadOnly, pmnuHidden     TN3109    neue Punkte im Kontext-Menü, nur enabled wenn HiddenMethodsAreShown
  19.05.06 wl  pmnu1Popup                   TN3109    Delete, SaveAs, ChangeComment, Add wird in Abhängigkeit vom Methoden-Attribut gezeigt
  14.07.06 pk  pmnu1Popup                   TN3165    Invalid typecast removed
  21.07.06 pk  pmnuViewHierarchyClick       TN3213    New
  03.10.06 wl                               TN3317    uses LiquidParamEditor, PowderParamEditor
  08.12.06 wl                               TN3459    TRichEdit durch TMemo ersetzt
  19.12.06 wl  viele Methoden               TN3409    User management überarbeitet
  17.04.07 wl                               TN3547    neu: SQL-Term kann auch wie alles andere editiert werden
  07.08.07 wl                               TN3811.3  TCommandDataAdapter.TableExists ohne Parameter
  27.08.07 pk                               TN3788    Reference to New ParserStoredIdentifier.Pas unit
  30.08.07 pk  UpdateTV                     TN3840.1  For Layout Node call GetNamesAndAddChildren
  12.11.07 pk                               TN3922    function CommandDataAdaptor.TableExists removed
  28.11.07 wl  fReadOnlyVisible             TN3915    wenn gesetzt, kann Read-Only auch ohne "geheimes" Passwort benutzt werden
  09.01.08 wl                               TN3972    benutzt gRunStepBuilderTypeDictionary
  20.06.08 pk                               TN4139    uses changed
  06.08.08 pk                               TN4165.1  calls InstGetAllExistingVariables instead of GetAllExistingVariables
  11.07.08 wl                               TN4164    für neue Nodes im Designer: Workspace, Rack, Carrier, TipType
  02.09.08 pk                               TN4125    GetAllActions moved to TRunStepInfoFactory
  02.09.08 pk                               TN4125    pmnuAdd2 removed
  25.09.08 wl                               TN4242   Command macros endgültig entfernt
  06.11.08 pk                               TN4279    uses ParserIdentDataType
  19.11.08 pk  Add3                         TN4280    ThreadStart removed
  10.12.08 pk  pmnu1                        TN4344    Resources for Popumpmenu changed
  16.01.09 wl                               TN4362   an Änderungen in TViewItem angepasst
  06.04.09 pk                               TN4503   New: ntDisplayComponent
  15.04.09 pk  pmnu1Popup                   TN4519   if top node exit before creating ViewItem
  10.06.09 pk  CreateTopNodes               TN4599   uRunTableTopNode - create commented out for now
  16.06.09 wl                               TN4606   alle Bezüge auf ntRunTable und RunEditor entfernt
  17.06.09 ts  pmnu1Popup                   TN4603   new: View Connections to open ViewConnections-Form
  11.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  11.08.09 wl  UpdateTV                     TN4702   Standardfunktion auch für Actions
  24.08.09 wl  AddMethodParallel            TN4702   disabled
  31.08.09 pk                               TN4753   uses ObjModul removed
  21.09.09 pk  UpdateTV                     TN4788   GetNamesAndAddChildren for Rack, Carrier, Workspace
  03.09.09 wl  cxTreeList1                  TN4800   Anpassungen an Treelist Version 5 (nicht kompatibel)
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  19.11.09 ts  GetNamesAndAddChildren       TN4878   if TopNode was expanded before REFRESH it will expand again
  13.04.10 wl                               TN5044   uses geändert
  30.04.10 wl                               TN5070   bei Traysy-Lizenz keine Carrier anzeigen
  30.04.10 wl                               TN5052   bei OneTipTypeMode = 1 werden TipTypes ausgeblendet
  30.04.10 wl                               TN5052   bei OneWorkspaceMode = 1 werden Workspaces ausgeblendet
  04.05.10 wl                               TN5052   bei UseDisplayComponents = 0 werden Display Components ausgeblendet
  06.05.10 wl                               TN5052   es kann jetzt mehrere Instanzen dieses Fensters geben
  07.05.10 pk                               TN5092   CreateWithReaderwritersettings no longer needs type parameter
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  09.06.10 wl                               TN5116   ist kein DockableForm mehr
  10.06.10 wl                               TN5116   Neu: Favourites!
  29.07.10 wl                               TN5204   Favoriten können jetzt in Ordner sortiert werden
  29.07.10 wl                               TN5204   Favoriten werden geladen und gespeichert
  30.07.10 wl  cxTreeList1KeyDown           TN5204   Entfernen von Favoriten mit Taste Entf möglich
  20.08.10 wl                               TN5223   Favoriten werden beim Laden geprüft (ob es sie noch gibt)
  20.09.10 wl  cxTreeList1DragDrop          TN5223   SubFolder klappen nicht mehr auf
  24.09.10 pk                               TN5089   New imagelist for 64 Action Icons
  08.10.10 pk                               TN5089   Imagelist name changed
  08.10.10 pk                               TN5117   Font now really changed to Segoe UI
  03.11.10 wl  pmnuDeleteClick              TN5141   TipType lässt sich jetzt auch löschen
  23.02.11 wl                               TN5486   neu: ImportViewItems
  01.07.11 wl                               TN5619   New funktioniert auch für Import-Definitionen
  22.07.11 wl  pmnuDeleteClick              TN5622   Import-Definitionen lassen sich jetzt auch löschen
  26.07.11 wl                               TN5614   Zugriff auf ViewHierarchy geändert
  01.08.11 wl  CreateTopNodes               TN5642   DisplayComponenets werden vor SQLTerms angezeigt
  26.08.11 wl  pmnuSearchForClick           TN5670   Neu: Direkter Suchen-Aufruf
  14.12.11 wl                               TN5765   ohne Session
  03.02.12 wl                               TN5792   neue Namen: SubstanceSet,Substance statt ReagentRack,Reagent
  20.02.12 wl                               TN5812   SubstanceSet auch mit SaveAs und Delete
  15.03.12 wl                               TN5835   sieht jetzt in Windows XP weniger scheußlich aus
  07.03.13 ts  AddChildAsLast               TN6092   neue Icons für startable methods
  -------------------------------------------------------------------------------------------------- }

unit ViewAllItems;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    ImgList,
    Menus,
    ComCtrls,
    Buttons,
    ExtCtrls,
    StdCtrls,
    cxGraphics,
    cxCustomData,
    cxStyles,
    cxTL,
    cxControls,
    cxInplaceContainer,
    cxTextEdit,

    GeneralTypes,
    ViewItem,
    StringLoader,
    TreeListNodesReaderWriter,

    cxCheckBox,
    cxLookAndFeels,
    cxLookAndFeelPainters;

type
    TViewAllItemsStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmViewAllItems = class(TForm)
        pmnu1: TPopupMenu;
        pmnuNew: TMenuItem;
        N_New: TMenuItem;
        pmnuSaveAs: TMenuItem;
        N_Delete: TMenuItem;
        pmnuDelete: TMenuItem;
        iliPopupMenu: TImageList;
        pmnuOpen: TMenuItem;
        pmnuAdd1: TMenuItem;
        pmnuAddParallel: TMenuItem;
        Memo1: TMemo;
        Splitter1: TSplitter;
        cxTreeList1: TcxTreeList;
        TVcxTreeListColumn1: TcxTreeListColumn;
        pmnuRefresh: TMenuItem;
        N_Open: TMenuItem;
        pmnuReadOnly: TMenuItem;
        pmnuHidden: TMenuItem;
        pmnuViewHierarchy: TMenuItem;
        pmnuAddFavFolder: TMenuItem;
        pmnuAddToFavs: TMenuItem;
        pmnuRemoveFromFavs: TMenuItem;
        pmnuRenameFavFolder: TMenuItem;
        pmnuSearchFor: TMenuItem;
        procedure pmnuNewClick(Sender: TObject);
        procedure pmnuSaveAsClick(Sender: TObject);
        procedure pmnuDeleteClick(Sender: TObject);
        procedure pmnu1Popup(Sender: TObject);
        procedure pmnuOpenClick(Sender: TObject);
        procedure pmnuAdd1Click(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure cxTreeList1DblClick(Sender: TObject);
        procedure cxTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cxTreeList1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure cxTreeList1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
            var Accept: Boolean);
        procedure pmnuRefreshClick(Sender: TObject);
        procedure pmnuReadOnlyClick(Sender: TObject);
        procedure pmnuHiddenClick(Sender: TObject);
        procedure pmnuViewHierarchyClick(Sender: TObject);
        procedure pmnuAddParallelClick(Sender: TObject);
        procedure cxTreeList1FocusedNodeChanged(Sender: TcxCustomTreeList;
            APrevFocusedNode, AFocusedNode: TcxTreeListNode);
        procedure FormDestroy(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure pmnuAddFavFolderClick(Sender: TObject);
        procedure pmnuAddToFavsClick(Sender: TObject);
        procedure pmnuRemoveFromFavsClick(Sender: TObject);
        procedure cxTreeList1DragDrop(Sender, Source: TObject; X, Y: Integer);
        procedure cxTreeList1EndDrag(Sender, Target: TObject; X, Y: Integer);
        procedure cxTreeList1StartDrag(Sender: TObject; var DragObject: TDragObject);
        procedure pmnuRenameFavFolderClick(Sender: TObject);
        procedure pmnuSearchForClick(Sender: TObject);
    private
        FRightSelected: TcxTreeListNode;
        fJustStarted: boolean;
        fReadOnlyVisible: boolean;
        fStringLoader: TViewAllItemsStringLoader;
        fFavouritesNode, fSpaceNode, fAllTopNode, fMethodTopNode, fSequenceTopNode, fWashProgTopNode,
            fLiquidParTopNode, fPowderParTopNode, fActionTopNode, fLayoutTopNode, fSubstanceTopNode,
            fSubstanceSetTopNode, fSQLTermTopNode, fVariableTopNode, fDeviceTopNode, fDriverTopNode,
            fConnectionTopNode, fRackTopNode, fCarrierTopNode, fWorkspaceTopNode, fTipTypeTopNode,
            fImportFileDefTopNode, fTableImportDefTopNode, fVarImportDefTopNode, fDisplayComponentTopNode
            : TcxTreeListNode;
        fFormName: string;
        fLastDragNodeTypes: TViewItemTypes;
        //
        function GetTopNodeByType(aNodeType: TViewItemType): TcxTreeListNode;
        function GetTypeByTopNode(aNode: TcxTreeListNode): TViewItemType;
        function GetTypeByNodeIntern(aNode: TcxTreeListNode; out oIsTopNode, oIsFavourite: boolean)
            : TViewItemType;
        function CreateTopNode(const aText: string): TcxTreeListNode;
        //
        procedure AddChildren(aNodeType: TViewItemType; const aLstNodeNames: TStringArray);
        procedure SelectElement;
        procedure DeleteNode(aNode: TcxTreeListNode);
        function GetNamesAndAddChildren(aNodeType: TViewItemType): TStringArray;
        procedure OpenSelectedItem();
        function GetDataFromNode(aNode: TcxTreeListNode; out oData: TViewItemDataRec): boolean;
        function GetGroup(aGroupName: string): TcxTreeListNode;
        procedure LoadFavourites;
        procedure SaveFavourites;
        function GetFavouritesPathName: string;
        procedure RefreshImageIndex(aSender: TObject; aNode: TcxTreeListNode);
        function CheckFavouriteNodeData(aSender: TObject; aNodeData: TTreeListNodeData): boolean;
    public
        constructor Create(aOwner: TComponent; const aFormName: string); reintroduce;

        procedure UpdateTV(aUpdateModes: TViewItemTypes);
        procedure CreateTopNodes(aTypes: TViewItemTypes; aCaption: string);
        function DecodeNodeID(aIntNodeID: integer): integer;
        function AddChildAsLast(aNodeType: TViewItemType; const aNodeName: string; aSelect: boolean = true;
            aParent: TcxTreeListNode = nil; aStartableMethod: boolean = false): TcxTreeListNode;
        procedure DeleteSelectedNode();
        function EncodeNodeID(aNodeType: TViewItemType; aIntNodeID: integer): integer;

        property JustStarted: boolean read fJustStarted write fJustStarted;
    end;


implementation


uses
    MathUtils,
    AppSettings,
    CommonTypes,
    RunStepInfoFactory,
    MethodTypes,
    ParserStoredIdentifier,
    ParserIdentifier,
    ParserIdentDataType,
    MethodSettings,
    MethVarDataAdaptor,
    RackDataAdaptor,
    CarrierDataAdaptor,
    WorkspaceDataAdaptor,
    TipTypeEditor,
    UtilLib,
    ViewItemsWorkflow,
    AppTypes,
    ViewItemTypeLibrary,
    ControlUtils,
    // muss raus:
    MethodEditor,
    ZADesignMain;

{$R *.dfm}
{ TViewAllItemsStringLoader }

procedure TViewAllItemsStringLoader.AddAllItems;
begin
    AddSingle(170, 'Refresh', 'Aktualisieren');
    AddSingle(44500, 'Overview', 'Übersicht');
    AddSingle(44600, '&Open', 'Ö&ffnen');
    AddSingle(44610, '&New', '&Neu');
    AddSingle(44620, '&Add to current method', '&Hinzufügen zur aktuellen Methode');
    AddSingle(44650, 'Add to current method (Execute in &parallel)',
        'Hinzufügen zur aktuellen Methode (&Parallel ausführen)');
    AddSingle(44690, '&Change Comments', '&Kommentare ändern');
    AddSingle(44710, 'Read-Only', 'Schreibgeschützt');
    AddSingle(44720, 'Hidden', 'Verborgen');
    AddSingle(44675, 'View all connections', 'Alle Connections anzeigen');
    AddSingle(44730, '&View method hierarchy', 'Methoden-Hierarchie anzeigen');
    AddSingle(44740, 'Add folder to favourites', 'Favoriten-Ordner hinzufügen');
    AddSingle(44750, 'Add to favourites', 'Zu den Favoriten hinzufügen');
    AddSingle(44760, 'Remove from favourites', 'Aus den Favoriten entfernen');
    AddSingle(44567, '&Rename', '&Umbenennnen');

end;

{ TfrmViewAllItems }

function TfrmViewAllItems.CheckFavouriteNodeData(aSender: TObject; aNodeData: TTreeListNodeData): boolean;
var
    x: integer;
begin
    if aNodeData.StateIndex = INT_IM_INDEX_FAVFOLDER then
        EXIT(true); // Folder nicht prüfen

    // einen Favoriten muss es schon geben
    for x := 0 to self.cxTreeList1.AbsoluteCount - 1 do
    begin
        if (self.cxTreeList1.AbsoluteItems[x].Texts[0] = aNodeData.Text1) and
            (self.cxTreeList1.AbsoluteItems[x].StateIndex = aNodeData.StateIndex) then
            EXIT(true); // Item existiert - Favourite darf erzeugt werden
    end;

    EXIT(false);
end;

constructor TfrmViewAllItems.Create(aOwner: TComponent; const aFormName: string);
var
    xIniAccess: IWinlissyIniAccess;
begin
    inherited Create(aOwner);

    fFormName := aFormName;

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TViewAllItemsStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    FJustStarted := true;

    xIniAccess := gCommonDll.CreateAppIni;
    fReadOnlyVisible := (UpperCase(xIniAccess.ReadString('View', 'VisiblemenuItems')) = 'READONLY');
end;

function TfrmViewAllItems.GetTypeByNodeIntern(aNode: TcxTreeListNode; out oIsTopNode, oIsFavourite: boolean)
    : TViewItemType;
begin
    oIsTopNode := false;
    oIsFavourite := false;

    result := TViewItemsWorkflow.GetTypeByNode(aNode);

    if (result = ntFolder) then
    begin
        result := GetTypeByTopNode(aNode);
        oIsTopNode := true;
        EXIT;
    end;

    oIsFavourite := (result = ntFavFolder) or (aNode.Parent = fFavouritesNode) or
        (TViewItemsWorkflow.GetTypeByNode(aNode.Parent) = ntFavFolder);
end;

function TfrmViewAllItems.GetTopNodeByType(aNodeType: TViewItemType): TcxTreeListNode;
begin
    case aNodeType of
        ntMethod:
            result := fMethodTopNode;
        ntSequence:
            result := fSequenceTopNode;
        ntWashProg:
            result := fWashProgTopNode;
        ntAction:
            result := fActionTopNode;
        ntLayout:
            result := fLayoutTopNode;
        ntLiquidPar:
            result := fLiquidParTopNode;
        ntPowderPar:
            result := fPowderParTopNode;
        ntVariable:
            result := fVariableTopNode;
        ntSubstance:
            result := fSubstanceTopNode;
        ntSubstanceSet:
            result := fSubstanceSetTopNode;
        ntSQLTerm:
            result := fSQLTermTopNode;
        ntDevice:
            result := fDeviceTopNode;
        ntDriver:
            result := fDriverTopNode;
        ntConnection:
            result := fConnectionTopNode;
        ntRack:
            result := fRackTopNode;
        ntCarrier:
            result := fCarrierTopNode;
        ntWorkspace:
            result := fWorkspaceTopNode;
        ntTipType:
            result := fTipTypeTopNode;
        ntImportFileDef:
            result := fImportFileDefTopNode;
        ntTableImportDef:
            result := fTableImportDefTopNode;
        ntVarImportDef:
            result := fVarImportDefTopNode;
        ntDisplayComponent:
            result := fDisplayComponentTopNode;
        ntFavFolder:
            result := fFavouritesNode;
        else
            result := nil;
    end;
end;

function TfrmViewAllItems.GetTypeByTopNode(aNode: TcxTreeListNode): TViewItemType;
begin
    result := ntUnknown;

    if (aNode = fAllTopNode) then
        result := ntAll;
    if (aNode = fMethodTopNode) then
        result := ntMethod;
    if (aNode = fSequenceTopNode) then
        result := ntSequence;
    if (aNode = fWashProgTopNode) then
        result := ntWashProg;
    if (aNode = fActionTopNode) then
        result := ntAction;
    if (aNode = fLayoutTopNode) then
        result := ntLayout;
    if (aNode = fLiquidParTopNode) then
        result := ntLiquidPar;
    if (aNode = fPowderParTopNode) then
        result := ntPowderPar;
    if (aNode = fVariableTopNode) then
        result := ntVariable;
    if (aNode = fSubstanceTopNode) then
        result := ntSubstance;
    if (aNode = fSubstanceSetTopNode) then
        result := ntSubstanceSet;
    if (aNode = fSQLTermTopNode) then
        result := ntSQLTerm;
    if (aNode = fDeviceTopNode) then
        result := ntDevice;
    if (aNode = fDriverTopNode) then
        result := ntDriver;
    if (aNode = fConnectionTopNode) then
        result := ntConnection;
    if (aNode = fRackTopNode) then
        result := ntRack;
    if (aNode = fCarrierTopNode) then
        result := ntCarrier;
    if (aNode = fWorkspaceTopNode) then
        result := ntWorkspace;
    if (aNode = fTipTypeTopNode) then
        result := ntTipType;
    if (aNode = fImportFileDefTopNode) then
        result := ntImportFileDef;
    if (aNode = fTableImportDefTopNode) then
        result := ntTableImportDef;
    if (aNode = fVarImportDefTopNode) then
        result := ntVarImportDef;
    if (aNode = fDisplayComponentTopNode) then
        result := ntDisplayComponent;
    if (aNode = fFavouritesNode) then
        result := ntFavFolder;
end;

function TfrmViewAllItems.EncodeNodeID(aNodeType: TViewItemType; aIntNodeID: integer): integer;
var
    intBaseID: integer;
    parentNode: TcxTreeListNode;
begin
    intBaseID := INT_MAX_ELEMENTS_PER_TYPE * TViewItemsWorkflow.GetIndexByType(aNodeType);
    parentNode := GetTopNodeByType(aNodeType);
    if Assigned(parentNode) then
    begin
        aIntNodeID := aIntNodeID - parentNode.AbsoluteIndex;
        aIntNodeID := intBaseID + aIntNodeID;
        result := aIntNodeID;
    end
    else
        result := -1;
end;

function TfrmViewAllItems.DecodeNodeID(aIntNodeID: integer): integer;
var
    intBaseID, intOffSetID: integer;
    parentNode: TcxTreeListNode;
begin
    intBaseID := aIntNodeID div INT_MAX_ELEMENTS_PER_TYPE;
    parentNode := GetTopNodeByType(TViewItemType(intBaseID));
    if not Assigned(parentNode) then
    begin
        result := -1;
        EXIT;
    end;
    intOffSetID := aIntNodeID mod INT_MAX_ELEMENTS_PER_TYPE;
    result := parentNode.AbsoluteIndex + intOffSetID;
end;

procedure TfrmViewAllItems.OpenSelectedItem();
begin
    if (cxTreeList1.FocusedNode = nil) then
        EXIT;
    if (cxTreeList1.FocusedNode.Parent = nil) then
        EXIT;

    SelectElement();
end;

procedure TfrmViewAllItems.DeleteNode(aNode: TcxTreeListNode);
begin
    aNode.Delete;
end;

procedure TfrmViewAllItems.DeleteSelectedNode();
begin
    if not Assigned(fRightSelected) then
        EXIT;
    DeleteNode(FRightSelected);
end;

procedure TfrmViewAllItems.pmnuNewClick(Sender: TObject);
var
    xIsTopNode, xIsFavourite: boolean;
    xNodeType: TViewItemType;
begin
    xNodeType := GetTypeByNodeIntern(FRightSelected, xIsTopNode, xIsFavourite);
    if xIsFavourite then
        EXIT;

    case (xNodeType) of
        ntMethod:
            frmEdMain.actNewMethodExecute(Sender);

        ntSequence, ntWashProg, ntSQLTerm, ntLiquidPar, ntPowderPar, ntSubstance, ntSubstanceSet, ntDevice,
            ntDriver, ntConnection, ntDisplayComponent, ntTipType, ntVarImportDef, ntTableImportDef,
            ntImportFileDef:
            TViewItemsWorkflow.NewEditForm(xNodeType);
    end;
end;

procedure TfrmViewAllItems.RefreshImageIndex(aSender: TObject; aNode: TcxTreeListNode);
var
    xStateIndex: integer;
begin
    xStateIndex := aNode.StateIndex;
    if (xStateIndex = INT_IM_INDEX_ACTION) then
        aNode.ImageIndex := TRunStepInfoFactory.GetIconIndex(aNode.Texts[0])
    else
        aNode.ImageIndex := xStateIndex;
    aNode.SelectedIndex := aNode.ImageIndex;
end;

function TfrmViewAllItems.AddChildAsLast(aNodeType: TViewItemType; const aNodeName: string; aSelect: boolean;
    aParent: TcxTreeListNode; aStartableMethod: boolean): TcxTreeListNode;
var
    xParentNode: TcxTreeListNode;
begin
    result := nil;

    if not Assigned(aParent) then
    begin
        xParentNode := GetTopNodeByType(aNodeType);
        if not Assigned(xParentNode) then
            EXIT;
    end
    else
    begin
        xParentNode := aParent;
    end;

    result := cxTreeList1.AddChild(xParentNode);
    result.Texts[0] := aNodeName;
    if aStartableMethod then
        result.StateIndex := TViewItemsWorkflow.GetIndexByType(ntStartableMethod)
    else
        result.StateIndex := TViewItemsWorkflow.GetIndexByType(aNodeType);
    RefreshImageIndex(nil, result);

    if (aSelect) then
        cxTreeList1.FocusedNode := result;
end;

procedure TfrmViewAllItems.pmnuSaveAsClick(Sender: TObject);
var
    xSourceName: string;
    xIsTopNode, xIsFavourite: boolean;
    xNodeType: TViewItemType;
begin
    xSourceName := FRightSelected.Texts[0];

    if (xSourceName = '') then
        EXIT;
    xNodeType := GetTypeByNodeIntern(FRightSelected, xIsTopNode, xIsFavourite);
    if (xIsTopNode) then
        EXIT;

    if not TViewItemsWorkflow.SaveAs(xSourceName, xNodeType) then
        EXIT;
end;

procedure TfrmViewAllItems.pmnuDeleteClick(Sender: TObject);
var
    xIsTopNode, xIsFavourite: boolean;
    xNodeType: TViewItemType;
    xIdent: TParserStoredIdent;
begin
    if (FRightSelected.Texts[0] = '') then
        EXIT;
    xNodeType := GetTypeByNodeIntern(FRightSelected, xIsTopNode, xIsFavourite);
    if (xIsTopNode) then
        EXIT;

    case (xNodeType) of
        ntMethod, ntSequence, ntWashProg, ntSQLTerm, ntLiquidPar, ntPowderPar, ntSubstance, ntSubstanceSet,
            ntDevice, ntDriver, ntConnection, ntDisplayComponent, ntTipType, ntVarImportDef, ntTableImportDef,
            ntImportFileDef:
            begin
                TViewItemsWorkflow.DeleteName(xNodeType, FRightSelected.Texts[0]);
            end;
        ntVariable:
            begin
                xIdent := TParserStoredIdent.CreateWithReaderWriterSettings(FRightSelected.Texts[0]);
                try
                    if xIdent.DeleteIdentifier() then
                    begin
                        DeleteNode(FRightSelected);
                    end;
                finally
                    FreeAndNil(xIdent);
                end;
            end;
    end;
end;

procedure TfrmViewAllItems.pmnu1Popup(Sender: TObject);
var
    i: integer;
    xIsTopNode, xIsFavourite: boolean;
    xNodeType: TViewItemType;
    xViewItem: TViewItem;
    xAttributes: TMethodSetAttributes;
begin
    for i := 0 to pmnu1.Items.count - 1 do
        pmnu1.Items.Items[i].Visible := false;

    if (cxTreeList1.FocusedNode = nil) then
        EXIT;
    if (cxTreeList1.FocusedNode.Parent = nil) then
        EXIT;
    if (cxTreeList1.FocusedNode.Texts[0] = '') then
        EXIT;

    FRightSelected := cxTreeList1.FocusedNode;

    if (cxTreeList1.FocusedNode = fFavouritesNode) then
    begin

        pmnuAddFavFolder.Visible := true;
        EXIT;
    end;

    xNodeType := GetTypeByNodeIntern(FRightSelected, xIsTopNode, xIsFavourite);

    if not xIsFavourite then
    begin
        pmnuRefresh.Visible := true;
    end;

    if cxTreeList1.FocusedNode = fAllTopNode then
        EXIT;

    xViewItem := TViewItemTypeLibrary.CreateAnyViewItem(xNodeType, cxTreeList1.FocusedNode.Texts[0]);
    try
        if not xIsFavourite then
        begin
            pmnuNew.Visible := xViewItem.NewIsAllowed();
        end;

        if (xIsTopNode) then
            EXIT;

        pmnuSearchFor.Visible := true;
        pmnuSearchFor.Caption := TLanguageString.Read('Search for "{0}"', 'Suche nach "{0}"',
            [FRightSelected.Texts[0]]);

        if not xIsFavourite then
        begin
            pmnuAddToFavs.Visible := true;
        end
        else
        begin
            N_Delete.Visible := true;
            pmnuRemoveFromFavs.Visible := true;
        end;

        if (xNodeType = ntFavFolder) then
            pmnuRenameFavFolder.Visible := true;

        if xViewItem.SaveAsIsAllowed() then
        begin
            pmnuSaveAs.Visible := true;
            pmnuSaveAs.Caption := TLanguageString.Read('&Save {0} as ..', '{0} &speichern unter ..',
                [FRightSelected.Texts[0]]);
            N_New.Visible := true;
        end;

        if TViewItemsWorkflow.OpenIsAllowed(xNodeType) then
        begin
            pmnuOpen.Visible := true;
        end;

        if (not xIsFavourite) and xViewItem.DeleteIsAllowed() then
        begin
            N_Delete.Visible := true;
            pmnuDelete.Visible := true;
            pmnuDelete.Caption := TLanguageString.Read('&Delete {0}', '{0} &löschen',
                [FRightSelected.Texts[0]]);
        end;

        if (xNodeType in [ntMethod]) then
        begin
            pmnuReadOnly.Visible := true;
            pmnuHidden.Visible := true;
            pmnuReadOnly.Enabled := fReadOnlyVisible or frmEdMain.HiddenMethodsAreShown;
            pmnuHidden.Enabled := frmEdMain.HiddenMethodsAreShown;
            xAttributes := TMethodSettings.GetAttributes(cxTreeList1.FocusedNode.Texts[0]);
            pmnuReadOnly.Checked := msaReadOnly in xAttributes;
            pmnuHidden.Checked := msaHidden in xAttributes;
            pmnuViewHierarchy.Visible := true;
        end;
    finally
        xViewItem.Free;
    end;

    if (frmEdMain.CurrentEditor is TfrmMethodEditor) and
        ((frmEdMain.CurrentEditor as TfrmMethodEditor).Attribute = meaDefault) and
        (xNodeType in [ntMethod, ntAction]) then
    begin
        pmnuAdd1.Visible := true;
        N_New.Visible := true;
        pmnuAdd1.Caption := TLanguageString.Read('&Add to current method',
            '&Hinzufügen zur aktuellen Methode');
    end;

    if (frmEdMain.CurrentEditor is TfrmMethodEditor) and
        ((frmEdMain.CurrentEditor as TfrmMethodEditor).Attribute = meaDefault) and
        (xNodeType in [ntMethod]) then
    begin
        // pmnuAddParallel.Visible := true;
        N_New.Visible := true;
    end;
end;

procedure TfrmViewAllItems.AddChildren(aNodeType: TViewItemType; const aLstNodeNames: TStringArray);
var
    i: integer;
    xNode: TcxTreeListNode;
begin

    xNode := GetTopNodeByType(aNodeType);
    if xNode = nil then
        EXIT;
    xNode.DeleteChildren;

    for i := 0 to Length(aLstNodeNames) - 1 do
    begin
        if (aLstNodeNames[i] <> '') then
        begin
            if (aNodeType = ntMethod) and (TMethodSettings.LoadStartableOption(aLstNodeNames[i])
                = msoStartable) then
                AddChildAsLast(aNodeType, aLstNodeNames[i], false, nil, true)
            else
                AddChildAsLast(aNodeType, aLstNodeNames[i], false);
        end;
    end;
end;

procedure TfrmViewAllItems.pmnuAddToFavsClick(Sender: TObject);
var
    xData: TViewItemDataRec;
begin
    if GetDataFromNode(FRightSelected, xData) then
    begin
        self.AddChildAsLast(xData.ItemType, xData.Name, true, fFavouritesNode);
    end;

    fFavouritesNode.Expand(false);
    self.SaveFavourites;
end;

function TfrmViewAllItems.CreateTopNode(const aText: string): TcxTreeListNode;
begin
    result := cxTreeList1.AddChild(fAllTopNode);
    result.Texts[0] := aText;
    result.StateIndex := INT_IM_INDEX_FOLDER;
end;

procedure TfrmViewAllItems.CreateTopNodes(aTypes: TViewItemTypes; aCaption: string);
begin
    cxTreeList1.Columns[0].Caption.Text := aCaption;

    fFavouritesNode := cxTreeList1.AddFirst;
    fFavouritesNode.Texts[0] := TLanguageString.Read('Favourites', 'Favoriten');
    fFavouritesNode.StateIndex := INT_IM_INDEX_FOLDER;
    fFavouritesNode.ImageIndex := INT_IM_INDEX_FAVOURITES;
    fFavouritesNode.SelectedIndex := INT_IM_INDEX_FAVOURITES;

    fSpaceNode := cxTreeList1.Add(nil);
    fSpaceNode.Enabled := false;

    fSpaceNode.ImageIndex := -1;
    fSpaceNode.SelectedIndex := -1;
    fSpaceNode.StateIndex := -1;

    fAllTopNode := cxTreeList1.Add(nil);
    fAllTopNode.Texts[0] := TLanguageString.Read('All Items', 'Alle Einträge');
    fAllTopNode.StateIndex := INT_IM_INDEX_FOLDER;

    if (ntMethod in aTypes) then
    begin
        fMethodTopNode := CreateTopNode(TLanguageString.Read('Methods', 'Methoden'));
    end;
    if (ntLiquidPar in aTypes) then
    begin
        fLiquidParTopNode := CreateTopNode(TLanguageString.Read('Liquid handling parameters',
            'Liquid Handling-Parameter'));
    end;
    if (ntPowderPar in aTypes) then
    begin
        fPowderParTopNode := CreateTopNode(TLanguageString.Read('Powder handling parameters',
            'Pulverpipettier-Parameter'));
    end;
    if (ntAction in aTypes) then
    begin
        fActionTopNode := CreateTopNode(TLanguageString.Read('Actions', 'Aktionen'));
    end;
    if (ntTableImportDef in aTypes) then
    begin
        fTableImportDefTopNode := CreateTopNode(TLanguageString.Read('Table import definitions',
            'Tabellen-Importdefinitionen'));
    end;
    if (ntVarImportDef in aTypes) then
    begin
        fVarImportDefTopNode := CreateTopNode(TLanguageString.Read('Variable import definitions',
            'Variablen-Importdefinitionen'));
    end;
    if (ntImportFileDef in aTypes) then
    begin
        fImportFileDefTopNode := CreateTopNode(TLanguageString.Read('Import file definitions',
            'Import-Dateidefinitionen'));
    end;
    if (ntDisplayComponent in aTypes) then
    begin
        fDisplayComponentTopNode := CreateTopNode(TLanguageString.Read('Display Components',
            'Display-Komponenten'));
    end;
    if (ntSQLTerm in aTypes) then
    begin
        fSQLTermTopNode := CreateTopNode(TLanguageString.Read('SQL Statements', 'SQL-Statements'));
    end;
    if (ntSequence in aTypes) then
    begin
        fSequenceTopNode := CreateTopNode(TLanguageString.Read('Sequences', 'Sequenzen'));
    end;
    if (ntSubstanceSet in aTypes) then
    begin
        fSubstanceSetTopNode := CreateTopNode(TLanguageString.Read('Substance sets', 'Substanz-Sets'));
    end;
    if (ntSubstance in aTypes) then
    begin
        fSubstanceTopNode := CreateTopNode(TLanguageString.Read('Substances', 'Substanzen'));
    end;
    if (ntWashProg in aTypes) then
    begin
        fWashProgTopNode := CreateTopNode(TLanguageString.Read('Wash programs', 'Waschprogramme'));
    end;
    if (ntVariable in aTypes) then
    begin
        fVariableTopNode := CreateTopNode(TLanguageString.Read('Variables', 'Variablen'));
    end;
    if (ntDevice in aTypes) then
    begin
        fDeviceTopNode := CreateTopNode(TLanguageString.Read('Devices', 'Devices'));
    end;
    if (ntDriver in aTypes) then
    begin
        fDriverTopNode := CreateTopNode(TLanguageString.Read('Drivers', 'Drivers'));
    end;
    if (ntConnection in aTypes) then
    begin
        fConnectionTopNode := CreateTopNode(TLanguageString.Read('Connections', 'Connections'));
    end;
    if (ntLayout in aTypes) then
    begin
        fLayoutTopNode := CreateTopNode(TLanguageString.Read('Layouts', 'Layouts'));
    end;
    if (ntWorkspace in aTypes) then
    begin
        fWorkspaceTopNode := CreateTopNode(TLanguageString.Read('Workspaces', 'Workspaces'));
    end;
    if (ntCarrier in aTypes) then
    begin
        fCarrierTopNode := CreateTopNode(TLanguageString.Read('Carriers', 'Carrier'));
    end;
    if (ntRack in aTypes) then
    begin
        fRackTopNode := CreateTopNode(TLanguageString.Read('Racks', 'Racks'));
    end;
    if (ntTipType in aTypes) then
    begin
        fTipTypeTopNode := CreateTopNode(TLanguageString.Read('Tip Types', 'Tiptypen'));
    end;
end;

function TfrmViewAllItems.GetNamesAndAddChildren(aNodeType: TViewItemType): TStringArray;
var
    xExpanded: boolean;
    xTopNode: TcxTreeListNode;
begin
    xTopNode := GetTopNodeByType(aNodeType);
    if not Assigned(xTopNode) then
        EXIT;

    xExpanded := xTopNode.Expanded;

    result := TViewItemsWorkflow.GetNames(aNodeType);
    AddChildren(aNodeType, result);

    xTopNode.Expand(xExpanded);
end;

procedure TfrmViewAllItems.UpdateTV(aUpdateModes: TViewItemTypes);
var
    xNames: TStringArray;
begin
    if (ntMethod in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntMethod);
    end;
    if (ntSequence in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntSequence);
    end;
    if (ntWashProg in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntWashProg);
    end;
    if (ntSQLTerm in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntSQLTerm);
    end;
    if ((ntLiquidPar in aUpdateModes) or (ntAll in aUpdateModes)) then
    begin
        GetNamesAndAddChildren(ntLiquidPar);
    end;
    if ((ntPowderPar in aUpdateModes) or (ntAll in aUpdateModes)) then
    begin
        GetNamesAndAddChildren(ntPowderPar);
    end;
    if (ntSubstance in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntSubstance);
    end;
    if (ntSubstanceSet in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntSubstanceSet);
    end;
    if (ntLayout in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntLayout);
    end;
    if (ntDevice in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntDevice);
    end;
    if (ntDriver in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntDriver);
    end;
    if (ntConnection in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntConnection);
    end;
    if (ntDisplayComponent in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntDisplayComponent);
    end;
    if (ntAction in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntAction);
    end;
    if (ntVariable in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        xNames := TMethVarDataAdaptor.InstGetAllExistingVariables();
        AddChildren(ntVariable, xNames);
    end;
    if (ntRack in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntRack);
    end;
    if (ntCarrier in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntCarrier);
    end;
    if (ntWorkspace in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntWorkspace);
    end;
    if (ntTipType in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        xNames := TfrmEdTipType.GetAllTypes();
        AddChildren(ntTipType, xNames);
    end;
    if (ntImportFileDef in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntImportFileDef);
    end;
    if (ntTableImportDef in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntTableImportDef);
    end;
    if (ntVarImportDef in aUpdateModes) or (ntAll in aUpdateModes) then
    begin
        GetNamesAndAddChildren(ntVarImportDef);
    end;

    if (ntAll in aUpdateModes) then
    begin

        // Favoriten aus XML-Datei laden
        LoadFavourites();

        cxTreeList1.FullCollapse;
        fFavouritesNode.Expand(false);
        fAllTopNode.Expand(false);
    end;
end;

function TfrmViewAllItems.GetDataFromNode(aNode: TcxTreeListNode; out oData: TViewItemDataRec): boolean;
var
    xIsTopNode, xIsFavourite: boolean;
begin
    oData.Name := aNode.Texts[0];
    oData.ItemType := GetTypeByNodeIntern(aNode, xIsTopNode, xIsFavourite);
    result := not xIsTopNode;
end;

procedure TfrmViewAllItems.SelectElement();
var
    xData: TViewItemDataRec;
begin
    if self.GetDataFromNode(cxTreeList1.FocusedNode, xData) then
        TViewItemsWorkflow.OpenItem(xData);
end;

procedure TfrmViewAllItems.pmnuOpenClick(Sender: TObject);
begin
    OpenSelectedItem();
end;

procedure TfrmViewAllItems.pmnuAdd1Click(Sender: TObject);
var
    xData: TViewItemDataRec;
begin
    if self.GetDataFromNode(cxTreeList1.FocusedNode, xData) then
        TViewItemsWorkflow.AddItemToMethod(xData);
end;

procedure TfrmViewAllItems.pmnuAddFavFolderClick(Sender: TObject);
var
    xName: string;
begin
    if not InputQuery('New folder name', 'Ordnername:', xName) then
        EXIT;

    GetGroup(xName);
    fFavouritesNode.Expand(false);
    self.SaveFavourites;
end;

procedure TfrmViewAllItems.pmnuRenameFavFolderClick(Sender: TObject);
var
    xName: string;
begin
    if not InputQuery('Folder name', 'Ordnername:', xName) then
        EXIT;

    cxTreeList1.FocusedNode.Texts[0] := xName;
    self.SaveFavourites;
end;

function TfrmViewAllItems.GetGroup(aGroupName: string): TcxTreeListNode;
var
    x: integer;
begin
    result := nil;
    if (aGroupName = '') then
        EXIT;

    // Gruppe suchen
    for x := 0 to fFavouritesNode.ChildVisibleCount - 1 do
    begin
        if not Assigned(fFavouritesNode.Items[x]) then
            CONTINUE;
        if (fFavouritesNode.Items[x].StateIndex <> INT_IM_INDEX_FOLDER) then
            CONTINUE;
        if (fFavouritesNode.Items[x].Texts[0] <> aGroupName) then
            CONTINUE;

        result := fFavouritesNode.Items[x];
        EXIT;
    end;

    // neue Gruppe erzeugen
    result := AddChildAsLast(ntFavFolder, aGroupName, true);
end;

procedure TfrmViewAllItems.pmnuAddParallelClick(Sender: TObject);
var
    xData: TViewItemDataRec;
begin
    if self.GetDataFromNode(cxTreeList1.FocusedNode, xData) then
        TViewItemsWorkflow.AddItemToMethodParallel(xData);
end;

procedure TfrmViewAllItems.FormActivate(Sender: TObject);
begin
    TViewItemsWorkflow.ShowInfo(cxTreeList1.FocusedNode, Memo1); // Refresh information
end;

procedure TfrmViewAllItems.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    TViewItemsWorkflow.WriteInfoHeight(fFormName, Memo1.Height);
end;

procedure TfrmViewAllItems.FormDestroy(Sender: TObject);
begin
    TViewItemsWorkflow.OverviewManager.RegisterAllItemsFormDestruction(self);
end;

procedure TfrmViewAllItems.cxTreeList1FocusedNodeChanged(Sender: TcxCustomTreeList;
    APrevFocusedNode, AFocusedNode: TcxTreeListNode);
begin
    if (FJustStarted) then
        EXIT;

    if cxTreeList1.FocusedNode = fSpaceNode then
        cxTreeList1.FocusedNode := self.fAllTopNode;

    TViewItemsWorkflow.ShowInfo(cxTreeList1.FocusedNode, Memo1);
end;

procedure TfrmViewAllItems.cxTreeList1DblClick(Sender: TObject);
begin
    OpenSelectedItem();
end;

procedure TfrmViewAllItems.cxTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    xIsTopNode, xIsFavourite: boolean;
begin
    case (Key) of
        VK_RETURN:
            OpenSelectedItem();
        VK_DELETE:
            begin
                if Assigned(cxTreeList1.FocusedNode) then
                begin
                    GetTypeByNodeIntern(cxTreeList1.FocusedNode, xIsTopNode, xIsFavourite);
                    if xIsFavourite then
                        cxTreeList1.FocusedNode.Delete;
                end;
            end;
    end;
end;

procedure TfrmViewAllItems.cxTreeList1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
    if (Button = mbRight) then
    begin
        cxTreeList1.FocusedNode := cxTreeList1.GetNodeAt(X, Y);
        if cxTreeList1.FocusedNode = fSpaceNode then
            cxTreeList1.FocusedNode := self.fAllTopNode;

        TViewItemsWorkflow.ShowInfo(cxTreeList1.FocusedNode, Memo1);
    end;
end;

procedure TfrmViewAllItems.cxTreeList1StartDrag(Sender: TObject; var DragObject: TDragObject);
begin
    fLastDragNodeTypes := [];
end;

procedure TfrmViewAllItems.cxTreeList1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
    self.UpdateTV(fLastDragNodeTypes);
end;

procedure TfrmViewAllItems.cxTreeList1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
    xTargetNode, xAddedNode: TcxTreeListNode;
begin
    if (Sender = Source) and Assigned(cxTreeList1.DragNode) and
        not(TViewItemsWorkflow.GetTypeByNode(cxTreeList1.DragNode) in [ntFolder, ntFavFolder]) then
    begin

        xTargetNode := cxTreeList1.GetNodeAt(X, Y);

        if (TViewItemsWorkflow.GetTypeByNode(xTargetNode) in [ntFolder, ntFavFolder]) then
            xAddedNode := cxTreeList1.AddChildFirst(xTargetNode)
        else
            xAddedNode := cxTreeList1.InsertEx(nil, xTargetNode);

        if Assigned(xAddedNode) then
        begin

            // neues Node soll gleiche Eigenschaften haben
            xAddedNode.Texts[0] := cxTreeList1.DragNode.Texts[0];
            xAddedNode.StateIndex := cxTreeList1.DragNode.StateIndex;
            xAddedNode.ImageIndex := cxTreeList1.DragNode.ImageIndex;
            xAddedNode.SelectedIndex := cxTreeList1.DragNode.SelectedIndex;

            // bei Ordnern Inhalt zeigen
            if Assigned(xTargetNode) then
                xTargetNode.Expand(false);

            // DragNode merken für Folder-Update
            if (TViewItemsWorkflow.GetTypeByNode(cxTreeList1.DragNode.Parent) <> ntFavFolder) and
                (cxTreeList1.DragNode.Parent <> self.fFavouritesNode) then
                fLastDragNodeTypes := [TViewItemsWorkflow.GetTypeByNode(cxTreeList1.DragNode)];

            // automatisches Verschieben unterbinden
            cxTreeList1.DragNode.Delete;

            self.SaveFavourites;
        end;
    end;
end;

procedure TfrmViewAllItems.cxTreeList1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
var
    xNode: TcxTreeListNode;
begin
    Accept := false;

    if (Sender = Source) and Assigned((Source as TcxTreeList).DragNode) and
        not(TViewItemsWorkflow.GetTypeByNode(cxTreeList1.DragNode) in [ntFolder, ntFavFolder]) then
    begin

        xNode := cxTreeList1.GetNodeAt(X, Y);

        // Kann in Favourites-Folder kopiert werden
        if (xNode = fFavouritesNode) then
            Accept := true;

        if (TViewItemsWorkflow.GetTypeByNode(xNode) = ntFavFolder) then
            Accept := true;
    end;
end;

procedure TfrmViewAllItems.pmnuRefreshClick(Sender: TObject);
var
    xIsTopNode, xIsFavourite: boolean;
    xNodeType: TViewItemType;
begin
    xNodeType := GetTypeByNodeIntern(FRightSelected, xIsTopNode, xIsFavourite);
    // for ntAll refresh all folders
    self.UpdateTV([xNodeType]);
end;

procedure TfrmViewAllItems.pmnuRemoveFromFavsClick(Sender: TObject);
begin
    fRightSelected.Delete();
    self.SaveFavourites;
end;

procedure TfrmViewAllItems.pmnuReadOnlyClick(Sender: TObject);
var
    xData: TViewItemDataRec;
begin
    if (not frmEdMain.HiddenMethodsAreShown) and (not fReadOnlyVisible) then
        EXIT;

    if self.GetDataFromNode(cxTreeList1.FocusedNode, xData) then
        pmnuReadOnly.Checked := TViewItemsWorkflow.ChangeReadOnly(xData);
end;

procedure TfrmViewAllItems.pmnuHiddenClick(Sender: TObject);
var
    xData: TViewItemDataRec;
begin
    if not frmEdMain.HiddenMethodsAreShown then
        EXIT;

    if self.GetDataFromNode(cxTreeList1.FocusedNode, xData) then
        pmnuHidden.Checked := TViewItemsWorkflow.ChangeHidden(xData);
end;

procedure TfrmViewAllItems.pmnuViewHierarchyClick(Sender: TObject);
var
    xData: TViewItemDataRec;
begin
    if self.GetDataFromNode(cxTreeList1.FocusedNode, xData) then
        frmEdMain.MethodDevelopment.LoadHierarchy(xData.Name);
end;

function TfrmViewAllItems.GetFavouritesPathName: string;
begin
    result := TAppSettings.DataPath + 'Fav' + fFormName;
end;

procedure TfrmViewAllItems.SaveFavourites;
var
    aReaderWriter: TTreeListNodesReaderWriter;
begin
    aReaderWriter := TTreeListNodesReaderWriter.Create;
    try
        aReaderWriter.Write(GetFavouritesPathName, fFavouritesNode);
    finally
        FreeAndNil(aReaderWriter);
    end;
end;

procedure TfrmViewAllItems.LoadFavourites;
var
    aReaderWriter: TTreeListNodesReaderWriter;
begin
    aReaderWriter := TTreeListNodesReaderWriter.Create;
    try
        try
            aReaderWriter.OnSetNodeAfterRead := self.RefreshImageIndex;
            aReaderWriter.OnCheckNodeData := self.CheckFavouriteNodeData;
            aReaderWriter.Read(GetFavouritesPathName, fFavouritesNode);
        except
            ShowMessage('Can not load favourites');
        end;
    finally
        FreeAndNil(aReaderWriter);
    end;

    // Prüfen ob die Favoriten noch existieren

end;

procedure TfrmViewAllItems.pmnuSearchForClick(Sender: TObject);
begin
    frmEdMain.SearchFor(FRightSelected.Texts[0]);
end;


end.
