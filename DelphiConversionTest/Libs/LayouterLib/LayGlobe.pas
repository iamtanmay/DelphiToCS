{ --------------------------------------------------------------------------------------------------
  SETUP
  --------------------------------------------------------------------------------------------------
  spezifische globale Einstellungen
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  14.02.00 wl  InitXYZ                LogText() durch gmLogText() ersetzt (für Delphi 5)
  GetPlate,PutPlate      MsgShow durch ResSyncBox ersetzt
  gmDeleteLayout         Funktion jetzt von WB-Objekt unabhängig
  23.02.00 wl  gmCheckLayout          Funktion überarbeitet
  gmDeleteLayout,gmDeleteTipset  --> LayMain
  28.04.00 wl  TLayouterThread        baut auf TBasicThread auf
  22.05.00 wl                         TipNode entfernt / Main.sbLEdit statt Main.rbEdit
  25.05.00 wl  TLayouterThread        --> LObjMove
  26.05.00 wl                         uses geändert
  05.07.00 wl  type TRackNames        vorher: RackNameArray
  13.09.00 wl  gmCheckRun             cTestRunName wird nicht gecheckt
  14.10.01 mo                         TN1067 Merge mit SIAS Änderungen
  13.09.02 wl                         TN1283 Merge mit SIAS
  26.09.02 wl                         TN1283 Kein fester Inifile Name mehr
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  23.10.02 wl  SetSpeeds              TN1293.1 verwendet TWinlissyIniAccess
  12.12.02 wl  SetSpeeds              TN1345  Umschreiben Sam-Daten --> RoboticInterface
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  04.01.03 wl  gmUserSetAccess              TN1334.1 einfacher Ersatz für SamplerUser
  07.01.03 wl  gmUserSetAccess        TN1334.3 --> LayMain
  07.01.03 wl  gmCheckRun             TN1334.3 --> LayoutDataAdaptor (Layouts mit Run-Layout werden nicht mehr gelöscht)
  07.01.03 wl                         TN1334.3 gLayoutDA,.. neu: DataAdaptoren (erstmal) als globale Objekte
  19.02.03 wl  gmRepairDBIndex        TN1334.3 Events.DB entfernt
  19.02.03 wl  TSetupThreadManager.UserInterrupt  TN1334.3 abstrakte Methode wird speziell für Layouter überschrieben
  02.08.04 pk  TRackNames             TN2068   removed
  19.04.05 pk  UserInterrupt          TN2389   call new functions ResumeExec, SuspendExec
  27.04.05 pk  gmRepairDBIndex        TN2398   DMRack.PosInfo removed
  30.11.05 wl  gmRepairDBIndex        TN2517   --> SettingEditor
  07.12.06 pk  StopRunInterruptRoutine TN3445  replaces UserInterrupt
  07.12.06 wl  gmCheckTubePos          TN3243  aus SamCmd hierher (vielleicht nicht so gut)
  14.02.07 wl  gmCheckTubePos          TN3147    geänderter Aufruf von gmCalculatePipStep, alles in try-finally-Blöcken
  06.03.07 wl  SetSpeeds               TN3622    --> LayMain
  08.03.07 wl                          TN3620   uses geändert
  30.08.07 pk  gLayoutDA               TN3840.1 removed
  30.08.07 pk  gmCheckLayout           TN3840.1 move to TLayoutDataAdaptorExt.RackTypeIsUsed
  07.01.08 pk  gmSave/DeleteRack       TN3922   new functions. These functions are temporary and should be later reprogrammed in the Designer
  29.01.08 pk  gmSaveCarrierAs         TN3922   Bugs fixed
  24.04.08 wl  TipTypeNode             TN4073   new item for Tip types
  23.06.08 pk  WorkspaceNode           TN4139   new
  24.06.08 wl                          TN4143   uses geändert
  02.07.08 pk  gmCheckTubePos          TN4139   instead of col and row takes a pos as parameter
  16.07.08 wl                          TN4164   uses WorkspaceFieldNames entfernt
  23.09.08 wl                          TN4236   Verweis auf Rack-/CarrierFieldnames entfernt
  06.11.08 pk                          TN4280   SetupThreadManager changed to TLayouterThreadManager
  10.08.09 wl                          TN4702   Strings werden jetzt direkt geladen
  12.09.09 wl                          TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  13.04.10 wl                          TN5044   uses geändert
  23.04.10 wl                          TN5070   uses geändert
  30.04.10 wl                          TN5070   RackNode,.. -> LayMain
  03.05.10 wl  TLayouterWorkflow       TN5052   benutzt jetzt auch TViewItem
  28.05.10 wl                          TN5116   uses geändert
  14.01.11 wl  gmCheckTubePos          TN5426   --> RackPositionMoveAction
  08.02.11 wl  gmDeleteRun             TN5475   entfernt
  10.02.11 wl                          TN5475   Zugriffe auf ..DataAdaptor.Instance geändert
  10.11.11 ts  gmDeleteRack/SaveRackAs TN5702   Racks aus DISCRETERACKS löschen/kopieren
  14.12.11 wl                          TN5765   ohne Session
  03.02.12 wl                          TN5792   neue Namen: SubstanceSet,Substance statt ReagentRack,Reagent
  20.04.12 wl  StopRunInterruptRoutine TN5858   speziell für den Layouter noch gelassen
  20.04.12 wl  RequestStopRunInterrupt TN5858   hierher verschoben
  14.12.12 wl                          TN6054   ntVariable entfernt
  -------------------------------------------------------------------------------------------------- }

unit LayGlobe;


interface


uses
    ThrdMan,
    ThreadClasses,
    ViewItem;

type

    TLayouterThreadManager = class(TThreadManagerSetup)
    protected
        function StopRunInterruptRoutine(aSender: TObject; aIRArgs: TInterruptRoutineArgs)
            : TInterruptRoutineResult;
    public
        procedure RequestStopRunInterrupt(const aInterruptText: string; aWaitTillHandled: boolean); override;
    end;

    // Type Library (noch nicht fertig)
    TLayouterViewItemTypeLibrary = record
    public
        class function CreateAnyViewItem(aViewType: TViewItemType; const aName: string): TViewItem; static;
    end;

    TLayouterWorkflow = record
        class procedure AddNewAndOpen(aViewType: TViewItemType; const aNewName: string); static;
        class procedure OpenFirstItem(aViewType: TViewItemType); static;
        class procedure OpenItem(const aItem: TViewItemDataRec); static;
        class function OpenIsAllowed(aViewType: TViewItemType): boolean; static;
    end;

function gmSaveWorkspaceAs(const aName: string): string;
function gmSaveCarrierAs(const aName: string): string;
function gmSaveRackAs(const aName: string): string;

function gmDeleteWorkspace(const aName: string): boolean;
function gmDeleteCarrier(const aName: string): boolean;
function gmDeleteRack(const aName: string): boolean;


implementation


uses
    Controls,
    Variants,
    SysUtils,
    LayMain,
    EditingLayoutElements,
    GeneralTypes,
    ArrayUtils,
    CommonTypes,
    AppSettings,
    GUIManager,
    GUIManagerLayouter,
    AppTypes,
    RackDataAdaptor,
    CarrierDataAdaptor,
    ErrorManager,
    WorkspaceDataAdaptor,
    TipMapUtils,
    LogManager,
    LayoutEditingViewItem,
    WorkspaceEditor,
    TipTypeEditor,
    EditRack,
    EditCarr,
    DiscreteRackDataAdaptor;

procedure TLayouterThreadManager.RequestStopRunInterrupt(const aInterruptText: string;
    aWaitTillHandled: boolean);
var
    xDummy: TInterruptRoutineResult;
begin
    RequestInterrupt(aInterruptText, StopRunInterruptRoutine, null, xDummy, aWaitTillHandled, nil);
end;

function TLayouterThreadManager.StopRunInterruptRoutine(aSender: TObject; aIRArgs: TInterruptRoutineArgs)
    : TInterruptRoutineResult;
var
    Btn: Integer;
begin

    result := 0;
    FStopBtnpressed := true;
    if SamThreadRunning(false) then
    begin

        // stop main thread (and animate thread)
        SuspendExec;
        gLogManager.Log('STOP Button pressed', false);

        // show user interrupt form (until abort or continue button is pressed)
        btn := (gGUIManager as TGUIManagerLayouter).Stop_PromptModal();

        // continue main thread (or abort)
        if Btn = mrYes then
        begin
            gLogManager.Log('==> User Interrupt', false);
            gErrorManager.SetGlobalErr(ERR_USER);
        end;
        ResumeExec;
    end;
    FStopBtnpressed := false;

end;

function gmSaveWorkspaceAs(const aName: string): string;
var
    xNewName: string;
    xDA: TWorkspaceDataAdaptor;
begin
    result := '';
    xDA := TWorkspaceDataAdaptor.Create;
    try
        if not xDA.NameExists(aName) then
            EXIT;

        xNewName := gGUIManager.InputBox(TLanguageString.Read('Please insert new Workspace type name!',
            'Bitte einen neuen Workspacetyp-Namen eingeben!'), TLanguageString.Read('New Type Name',
            'Neuer Typname'), aName);

        xNewName := Copy(xNewName, 1, TWorkspaceDataAdaptor.GetFieldLengthName);

        if (xNewName = '') then
            EXIT;

        if xDA.NameExists(xNewName) then
        begin
            gGUIManager.MessageBox(TLanguageString.Read('{0} already exists. Action can not be continued!',
                '{0} existiert bereits. Die Aktion kann nicht fortgesetzt werden!', [xNewName]),
                TLanguageString.Read('Confirmation', 'Bestätigung'), 16);
        end
        else
        begin
            if TAppSettings.ItemConfirmAdd('Workspace', xNewName, aName) then
            begin
                xDA.SaveWorkspaceAs(aName, xNewName);
                result := xNewName;
            end;
        end;
    finally
        FreeAndNil(xDA);
    end;

end;

function gmSaveRackAs(const aName: string): string;
var
    xNewName: string;
    xDA: TRackDataAdaptor;
    xDADiscrete: TDiscreteRackDataAdaptor;
begin
    result := '';
    xDA := TRackDataAdaptor.Create;
    try
        if not xDA.NameExists(aName) then
            EXIT;

        xNewName := gGUIManager.InputBox(TLanguageString.Read('Please insert new Rack type name!',
            'Bitte einen neuen Racktyp-Namen eingeben!'), TLanguageString.Read('New Type Name',
            'Neuer Typname'), aName);

        xNewName := Copy(xNewName, 1, TRackDataAdaptor.NameFieldLength);

        if (xNewName = '') then
            EXIT;

        if xDA.NameExists(xNewName) then
        begin
            gGUIManager.MessageBox(TLanguageString.Read('{0} already exists. Action can not be continued!',
                '{0} existiert bereits. Die Aktion kann nicht fortgesetzt werden!', [xNewName]),
                TLanguageString.Read('Confirmation', 'Bestätigung'), 16);
        end
        else
        begin
            if TAppSettings.ItemConfirmAdd('Rack', xNewName, aName) then
            begin

                xDA.SaveNameAs(aName, xNewName);

                xDADiscrete := TDiscreteRackDataAdaptor.Create;
                try
                    xDADiscrete.SaveNameAs(aName, xNewName);
                finally
                    FreeAndNil(xDADiscrete);
                end;
                result := xNewName;
            end;
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

function gmSaveCarrierAs(const aName: string): string;
var
    xNewName: string;
    xDA: TCarrierDataAdaptor;
begin
    result := '';
    xDA := TCarrierDataAdaptor.Create;
    try
        if not xDA.NameExists(aName) then
            EXIT;

        xNewName := aName;

        xNewName := gGUIManager.InputBox(TLanguageString.Read('Please insert new Carrier type name!',
            'Bitte einen neuen Carriertyp-Namen eingeben!'), TLanguageString.Read('New Type Name',
            'Neuer Typname'), aName);

        xNewName := Copy(xNewName, 1, TCarrierDataAdaptor.NameFieldLength);

        if (xNewName = '') then
            EXIT;

        if xDA.NameExists(xNewName) then
        begin
            gGUIManager.MessageBox(TLanguageString.Read('{0} already exists. Action can not be continued!',
                '{0} existiert bereits. Die Aktion kann nicht fortgesetzt werden!', [xNewName]),
                TLanguageString.Read('Confirmation', 'Bestätigung'), 16);
        end
        else if TAppSettings.ItemConfirmAdd('Carrier', xNewName, aName) then
        begin
            xDA.SaveNameAs(aName, xNewName);
            result := xNewName;
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

function gmDeleteWorkspace(const aName: string): boolean;
var
    xDA: TWorkspaceDataAdaptor;
begin
    result := false;
    if not TAppSettings.ItemConfirmDelete('Workspace', aName) then
        EXIT;
    xDA := TWorkspaceDataAdaptor.Create;
    try
        xDA.DeleteName(aName);
    finally
        FreeAndNil(xDA);
    end;
    result := true;
end;

function gmDeleteCarrier(const aName: string): boolean;
var
    xDA: TCarrierDataAdaptor;
begin
    result := false;
    if not TAppSettings.ItemConfirmDelete('Carrier', aName) then
        EXIT;
    xDA := TCarrierDataAdaptor.Create;
    try
        xDA.DeleteName(aName);
    finally
        FreeAndNil(xDA);
    end;
    result := true;
end;

function gmDeleteRack(const aName: string): boolean;
var
    xDA: TRackDataAdaptor;
    xDADiscrete: TDiscreteRackDataAdaptor;
begin
    result := false;
    if not TAppSettings.ItemConfirmDelete('Rack', aName) then
        EXIT;
    xDA := TRackDataAdaptor.Create;
    try
        xDA.DeleteName(aName);
    finally
        FreeAndNil(xDA);
    end;

    xDADiscrete := TDiscreteRackDataAdaptor.Create;
    try
        xDADiscrete.DeleteName(aName);
    finally
        FreeAndNil(xDADiscrete);
    end;
    result := true;
end;

{ TViewItemTypeLibrary }

class function TLayouterViewItemTypeLibrary.CreateAnyViewItem(aViewType: TViewItemType; const aName: string)
    : TViewItem;
begin
    result := nil;
    case (aViewType) of
        ntFolder:
            result := TFolderViewItem.Create(aName);
        ntRack:
            result := TRackViewItem.Create(aName);
        ntCarrier:
            result := TCarrierViewItem.Create(aName);
        ntWorkspace:
            result := TWorkspaceViewItem.Create(aName);
        ntTipType:
            result := TTipTypeViewItem.Create(aName);
        // ntLayout    : result := TLayoutViewItem.Create( aName );
        else
            ASSERT(false, 'Node does not have a corresponding ViewItem');
    end;
end;

{ TLayouterWorkflow }

class procedure TLayouterWorkflow.AddNewAndOpen(aViewType: TViewItemType; const aNewName: string);
var
    xViewItem: TViewItem;
    xItem: TViewItemDataRec;
begin
    xViewItem := TLayouterViewItemTypeLibrary.CreateAnyViewItem(aViewType, aNewName);
    try
        xViewItem.AddName(aNewName);

        // add a node to view all items
        // TfrmViewAllItems.InstanceAddChildAsFirstOrLast( aViewType, aNewName );

        // open an editor for this method
        xItem.Name := aNewName;
        xItem.ItemType := aViewType;
        OpenItem(xItem);
    finally
        xViewItem.Free;
    end;
end;

class procedure TLayouterWorkflow.OpenFirstItem(aViewType: TViewItemType);
const
    cDefaultName: string = 'Default';
var
    xViewItem: TViewItem;
    xNames: TStringArray;
    xItem: TViewItemDataRec;
begin
    xViewItem := TLayouterViewItemTypeLibrary.CreateAnyViewItem(aViewType, '');
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

class function TLayouterWorkflow.OpenIsAllowed(aViewType: TViewItemType): boolean;
begin
    result := false;

    if (aViewType in [ntMethod, ntSequence, ntWashProg, ntSQLTerm, ntLiquidPar, ntPowderPar, ntSubstance,
        ntSubstanceSet, ntDevice, ntDriver, ntConnection, ntRack, ntCarrier, ntWorkspace, ntTipType,
        ntDisplayComponent, ntLayout]) then
    begin
        result := true;
        EXIT;
    end;
end;

class procedure TLayouterWorkflow.OpenItem(const aItem: TViewItemDataRec);
begin
    case aItem.ItemType of
        ntRack:
            begin
                if OpenIsAllowed(aItem.ItemType) then
                    TfrmEdRack.InstanceShowToEdit(aItem.Name);
            end;
        ntCarrier:
            begin
                if OpenIsAllowed(aItem.ItemType) then
                    TfrmEdCarr.InstanceShowToEdit(aItem.Name);
            end;
        ntWorkspace:
            begin
                if OpenIsAllowed(aItem.ItemType) then
                    TfrmWorkspaceEditor.InstanceShowToEdit(aItem.Name);
            end;
        ntTipType:
            begin
                if OpenIsAllowed(aItem.ItemType) then
                    TfrmEdTipType.EditTipType(aItem.Name);
            end;
        ntLayout:
            begin
                // if OpenIsAllowed( aItem.ItemType ) then
                // frmEdMain.LoadLayout( aItem.Name );
            end;
    end;

end;


end.
