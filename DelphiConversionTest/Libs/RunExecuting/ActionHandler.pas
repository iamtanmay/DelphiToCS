{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Action Handlers should
  1. execute functions that should run before the given IExecutable,
  2. execute the given IExecutable
  3. execute functions that should run after the given IExecutable
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  -------------------------------   -------- -----------------------------------------------
  28.02.05 pk                                    TN2314.1 New
  04.03.05 pk  TBasicLiveRunHandler.Execute      TN2329   EnableShowRun( false  ) done before Finalize data
  07.03.05 pk  TMultipleItemsHandler.ExecuteItems TN2314.1 Check for Global Error in loop
  08.03.05 pk  TMultipleItemsHandler.BeforeItemExecution TN2337 Check if DilutionManager is Assigned
  08.03.05 pk  TLiveRunHandler.FinalizeData      TN2329   Call inherited FinalizeData before calling archive and delete
  11.03.05 pk                                    TN2339.2 uses variant
  16.03.05 pk  TBasicLiveRunHandler              TN2352.1 New field fNestedRunCallStack to avoid using global Nested Run Call Stack
  16.03.05 pk  TMultipleActionHandler            TN2353   implements IDataAction
  16.03.05 pk  TMultipleItemsHandler             TN2353   New : BringBackTool
  16.03.05 pk  TBasicLiveRunHandlerSched         TN2353   does not bring back tool
  16.03.05 pk  TLiveRunHandler.AfterExecuteItems TN2353   Brings back tool
  31.03.05 pk                                    TN2362   Massive Changes
  31.03.05 pk  TExecHanlder                      TN2362   BeforeInitialization, AfterFinalization
  06.04.05 pk  TSessionExecHandler               TN2363   New: Code from SchedRunnersExt
  06.04.05 pk  TMultipleItems                    TN2373   was previously in Action.pas with the name TCompositeExecutable
  06.04.05 pk  TMultipleItemsItem                TN2373   Stores pointers to various interfaces needed for TMultipleItmesHandler
  06.04.05 pk  TMultipleItemsHandler             TN2373   IExecutable changed to TMultipleItemsItem
  19.04.05 pk  TSessionExecHandler               TN2392   New : InterruptStart, InterruptFinish, ErrorSet - extracted from ThreadManagerExt
  19.04.05 pk  TGroupRunHandler                  TN2391   New : a group of actions executed by reading actions live from run
  20.04.05 pk  TBasicLiveRunHandler              TN2390   use TRunDataAdaptorExt  instead of TRunDataAdaptor
  02.05.05 pk  TMultipleItemsHandler.BeforeItemExecution  TN2409 : if MustWashBeforeExecute then Wash tips
  23.06.05 pk  TBasicLiveRunHandler.EnableShowRunForm TN2474 Do not open run dialog if global error set
  05.07.05 pk  TLiveRunHandler.ArchiveAndDelete  TN2490   Show run dialog moved to TExecHandler
  19.08.05 pk  TExecHandler.ShowDoneDialog       TN2568   exit if fCompleted=false
  22.08.05 wl                                TN2558.8  uses ScheUtil entfernt, ScheUtil-Methoden werden mit TDMScript. oder global. aufgerufen
  11.07.05 pk  TSessionExecHandler               TN2737   Various changes for Dynamic Scheduling
  08.11.05 wl  TLiveScriptRunHandler             TN2745   entfernt
  08.11.05 wl  TLiveScriptTransferRunHandler     TN2745   entfernt
  08.11.05 wl  TScriptSingleActionHandler        TN2745   entfernt
  08.11.05 wl  TScriptDelayActionHandler         TN2745   entfernt
  11.14.05 pk  TSessionExecHandler               TN2758   Various changes for Dynamic Scheduling
  17.11.05 wl  TLiveRunHandler.FinalizeData      TN2771   GetRealBarcodes wird immer mit dem erstbesten Gripper-Arm durchgeführt
  24.11.05 pk                                    TN2805   ObjSampl replaced by ZARunnerObjects
  22.12.05 pk  TExecHandler.BeforeInitialize     TN2875   Set Cancel init because global error has no effect on Init function
  05.01.06 pk  TMultipleItemsHandler.ExecuteItem TN2877   call AcquireResources, ReleaseResources
  06.04.06 pk  TRunDynamicActionHandler.Initialize TN3001 Pass Cur Priority to RunCreate
  10.04.06 pk  TRunTimeExecHandler               TN3031   New parameter : FullInit
  10.04.06 pk  IsItemExecutable                  TN3032   Call IsStartNewRunState do determine if action should be executed
  11.04.06 pk  TRunDynamicActionHandler.Initialize TN3001 Create methoddataadaptor instead of using instance - caused access viol. in scheduler
  12.04.06 pk  TMultipleItemsHandler             TN3042   Log when an action is started and when it is ended
  04.05.06 pk  TRunDynamicActionHandler          TN3081   New field fRunCallStack - check circular run before build
  04.05.06 pk  TLiveRunHandler.RegisterRunName   TN3081   Removed - this is already done in TBasicLiveRunHandler
  11.05.06 pk  TLiveRunHandler.RegisterRunName   TN3081   changes undone - GetCurrentRunName didnt work
  11.05.06 pk  TBasicRunHandler.RegisterRunName  TN3081   changes undone - GetCurrentRunName didnt work
  15.05.06 pk  TRunDynamicActionHandler.Initialize TN3081 Don't check for methodexists, and dont verify layoutname - this already done in RunCreate
  16.05.06 pk  TRunDynamicActionHandler.Initialize TN3105 DLLFU with RUNST causes accessviolation because runstack is nil
  30.05.06 pk  TMultipleItemsHandler.AfterItemExecution   TN3120 use GetFailedState to get the failed state for the item
  09.06.06 wl  TMultipleItemsHandler.ExecuteItem          TN3137   bei unbehandelter Exception wird Fehlerfenster OHNE Retry-Button gezeigt
  09.06.06 wl  TRunDynamicActionHandler.Initialize        TN3138   Fehlerbehandlung mit Retry wenn es beim Build Fehler gibt
  19.07.06 pk  TMultipleItemsHandler.IsItemExecutable     TN3192   item is executable if runstate is asRestart
  25.08.06 thr TExecHandler.BeforeInitialize     TN3264   Abfrage zum Start des Vortexerthreads geändert
  19.09.06 pk  TRunDynamicActionHandler          TN3304   call gmRunCreate
  14.11.06 pk  TSimpleRunTimeExecHandler         TN3404   New uses system events
  14.11.06 pk  gmCreateRunActionHandlerThread    TN3404   Now creates TSimpleRunTimeExecHandler - for system events and user protection
  21.11.06 pk  TSimpleRunTimeExecHandler         TN3425   System events were shut off at afterinitialize instead of afterfinalize
  21.11.06 pk  TSessionExecHandler               TN3424   call suspendsafe and resumesafe in interruptstart and interruptfinish if session is not yet started
  01.12.06 pk  gmCreateRunActionHandlerThread    TN3441   thrUnknown changed to thrGeneralAction
  03.12.06 wl                                    TN3243   alle ErrBox-Aufrufe durch ErrBoxSimple oder ErrBoxSerialIntf ersetzt
  07.12.06 pk  TSessionExecHandler               TN3455   SimpleLock and CreateThread
  07.12.06 wl  TExecHandler.ShowDoneDialog       TN3409   keine abweichende Behandlung im CFR21-Mode
  20.08.07 pk  TRunDynamicActionHandler          TN3830   Removed.
  20.08.07 pk  BringBackTool                     TN3830   Bring back tool only if not running in session
  09.11.07 pk                                    TN3922   references to Dataset changed to DataProvider
  09.11.07 pk                                    TN3923   class references to TErrorMessageFactory changed to gErrorMessageFactory
  27.11.07 wl  TMultipleItemsHandler.BeforeItemExecution   TN3897    geänderte Parameter bei WashAllDirtyTips
  09.01.08 wl  SwitchPortsOnRestart              TN3972   entfernt
  17.03.08 wl                                    TN4043    uses IntfMixerDevice
  14.04.08 wl                                    TN4060   uses DialogUtils
  20.06.08 pk                                    TN4139    WB global object replaced by LayoutManager
  03.07.08 wl                                         TN4157
  11.07.08 wl                                         TN4164   TActionModules wird ohne "gModules as" aufgerufen
  08.09.08 pk                                    TN4215   Massive changes. Some code moved to ExecHandler
  20.09.08 pk                                    TN4215   Washmanager and Bringbacktool activated again
  25.09.08 pk                                    TN4241   GUIManagerRun removed
  06.11.08 pk                                    TN4279   various changes
  06.11.08 pk                                    TN4280   various changes
  10.11.08 pk                                    TN4280   uses changed
  10.11.08 pk GetItemDescription                 TN4280   now with MethodName and Line number
  17.11.08 pk                                    TN4280   Unused code removed
  17.11.08 pk                                    TN4280   uses changed
  19.11.08 pk                                    TN4280   Unused code removed
  19.11.08 pk                                    TN4280   UnRegisterUseXMotorAll called in AfterExecuteItems
  24.11.08 pk                                    TN4280   Unused code removed
  02.12.08 pk ExecuteItems                       TN4335   break if IsSafeExitRequested (no longer sets global error)
  09.12.08 pk ExecuteItem                        TN4279   logging changed
  10.12.08 pk TBasicLiveRunHandler.Initialize    TN4279   Call MarkStopAddress
  17.02.09 pk                                    TN4232   various changes
  20.02.09 pk                                    TN4232   Changes for multithreaded trace
  20.02.09 pk TMultipleItemsHandler              TN4232   New fIsEvent
  24.02.09 pk ExecuteItem                        TN4232   Call TraceManager.CurrentActionFinished
  25.02.09 pk TBasicLiveRunHandler.Initialize    TN4279   pass IsEvent to CallMethod
  06.03.09 pk ExecuteItems                       TN4335   Log when IsSafeExitRequested
  16.03.09 pk GetLineDescription                 TN4470   calls function from TRunStepBuilderProcessor
  10.08.09 wl                                    TN4702   Strings werden jetzt direkt geladen
  25.08.09 pk TLiveRunHandler.Finalize           TN4745   call EndAllBlocksForCurrentThread
  27.08.09 pk                                    TN4753   uses RunWriter removed
  08.09.09 pk                                    TN4753   uses ErrorMessage replaced by ErrorInfo
  24.11.09 ts BeforeItemExecution                TN4741   no washing if fIsEvent is true (avoid endless loop while flushing with method)
  04.02.10 pk                                    TN4972   Changes for Restart
  07.05.10 pk TRunStartInfo.Params               TN5092    now TArgValueList instead of KeyValueArray
  07.06.10 pk DoCreateAction                     TN5077   Call TraceManager.AddAction and PendingStepIterator.BookmarkCurrentCursorPos
  09.06.10 pk                                    TN5077   Call TraceManager.AddAction in CreateRunSteps
  17.06.10 wl  TBasicLiveRunHandler.Create       TN5150   übergibt Layout
  13.09.10 pk  DoOnFindRackByName                TN5269   handle case where no layout is loaded
  27.10.10 pk Iterate, IsEof                     TN5297   Check for fProcessor.IsPendingSteps
  15.11.10 pk                                    TN5340   Changes to prevent memory leak
  13.07.11 ts  TBasicLiveRunHandler.Destroy      TN5627   explicit close/destroy of database, fThreadID set in Execute
  17.08.11 wl  TBasicLiveRunHandler.Destroy      TN5654   Änderungen wieder rückgängig gemacht
  17.11.11 wl  DoOnFindRackPosByTubeID,DoOnFindRackDataByName  TN5725   neu: Funktionen können beim Build verwendet werden
  21.11.11 wl  DoOnFindRackPosBySubstID          TN5730   --> RunStepBuilderHelper
  14.12.11 wl                                    TN5765   uses geändert
  02.03.12 wl                                    TN5822   uses geändert
  29.05.12 wl  fMethodDebugMode              TN5903   neu: wird durch AppPurpose bestimmt und ist nur für den Designer aktiv
  29.05.12 wl  DoExecuteItem                 TN5903   Logging für jeden Step nur wenn fMethodDebugMode = true
  05.07.12 wl  DoOnFindXYByRackPosition          TN5931   neue Funktion, um XY-Position des Racks zu bestimmen
  07.08.12 wl  DoExecuteItem                     TN5894   Bei Step-By-Step-Abarbeitung wird bei neuen Schritt gestoppt
  10.09.12 wl  DoOnFindRackPosBySubstID          TN5979   neuer Parameter UseDeadVolume
  10.12.12 wl  TLiveRunHandler.BeforeFinalize    TN6049   TWashManager.WashAllDirtyTips wird am Schluss ausgeführt
  11.02.13 wl  DoOnFindAllStorageRackPos         TN6078   neu
  13.02.13 wl  Create                            TN6075   erzeugt TLiveRunStepBuilderHelper
  25.04.13 wl  MustWashBeforeExecute             TN6139   entfernt
  30.04.13 wl  AfterItemExecution                TN6139.1 speichert Reoperation-Task, das von einer Action erzeugt wird, in einer Liste
  30.04.13 wl  ExecuteReoperationTasks           TN6139.1 führt gespeicherte Reoperation-Tasks aus
  24.05.13 wl  AfterExecuteItems                 TN6139.1 darf im Fehlerfall nicht ausgeführt werden
  14.08.13 wl  TBasicLiveRunHandler.ExecuteItem  TN6218   CurrentBatchIndex wird auf 0 zurückgesetzt
  21.08.13 wl                                    TN6231   an Änderungen in ToolHandling angepasst
  01.10.13 wl                                    TN6251   Rückgabewerte für Threads vorbereitet
  -------------------------------------------------------------------------------------------------- }

unit ActionHandler;


interface


uses
    Generics.Collections,
    Streamable,
    MemoryClasses,
    ThreadClasses,
    MethodTypes,
    RunActionFactory,
    RunAction,
    RunStep,
    ExecHandlerMinimal,
    Executable,
    RunStepBuilder,
    RunStepBuilderProcessor,
    RunStepBuilderHelper;

type
    TMultipleItemsItem = class
    protected
        fIntfExecutable: IExecutable;
        fIntfMultipleItemsItem: IMultipleItemsItem;
        fIntfResourcedItem: IResourcedItem;
        fRunStep: TRunStep;
    public
        constructor Create(const aIExecutable: IExecutable; const aIMultipleItemsItem: IMultipleItemsItem;
            const aRunStep: TRunStep; const aIResourcedItem: IResourcedItem); overload;
        property IntfExecutable: IExecutable read fIntfExecutable;
        property IntfMultipleItemsItem: IMultipleItemsItem read fIntfMultipleItemsItem;
        property IntfResourcedItem: IResourcedItem read fIntfResourcedItem;
        property RunStep: TRunStep read fRunStep;
    end;

    TMultipleItems = class
    protected
        fList: TObjectList<TMultipleItemsItem>;
        fItemIndex: integer;
        function GetItemAt(aIndex: integer): TMultipleItemsItem;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure AddItems(aItems: TMultipleItems);
        procedure AddItem(aItem: TMultipleItemsItem);
        function ItemCount(): integer;
        property Items[aIndex: integer]: TMultipleItemsItem read GetItemAt;
        function FindNextItem(): TMultipleItemsItem;
    end;

    // ----------------------------------------------------------------------------------------------
    TMultipleItemsHandler = class(TExecHandlerMinimal)
    strict private
        procedure ExecuteReoperationTasks(aRunStep: TRunStep);
        function GetBasicItemDescription(const aRunStep: TRunStep): string;
        procedure AfterExecuteItems();
    strict protected
        fTaskList: TObjectList<TRunActionReoperationTask>;
        fItem: TMultipleItemsItem;
        fIsEvent: boolean;
        procedure ExecuteItems();
        function ExecuteItem(): boolean; virtual;
        function IsItemExecutable(const aRunStep: TRunStep): boolean;
        procedure BeforeItemExecution();
        procedure AfterItemExecution();
        function GetLineDescription(): string;
        function GetItemDescription(const aRunStep: TRunStep): string; virtual;
        procedure AcquireResources();
        procedure ReleaseResources();
        procedure ShowError(const aLineDescription, aItemDescription, aError: string);
        procedure Iterate(); virtual; abstract;
        function IsEOF(): boolean; virtual; abstract;
    public
        constructor Create();
        destructor Destroy; override;

        procedure Execute(); override;
    end;

    TRunStartInfo = record
        IsMainThread: boolean;
        IsRestart: boolean;
        MethodName: string;
        Params: TKeyArgValueList;
    end;

    TBasicLiveRunHandler = class(TMultipleItemsHandler)
    private const
        STR_EXCEPTION_NOMESSAGE = '[EXCEPTIONNOMESSAGE]';
    private
        fActionFactory: TRunActionFactory;
        fProcessor: TRunStepBuilderProcessor;
        fStartInfo: TRunStartInfo;
        fRunStepBuilderHelper: TRunStepBuilderHelper;
        fCurrentLineDescription: string;
        fMethodDebugMode: boolean;
        procedure TranslateRunSteps(const aSourceRunSteps, aDestRunSteps: TRunStepList);
        procedure DoCreateRunSteps();
        function DoCreateAction(): boolean;
        function DoExecuteItem(): boolean;
        procedure DoInitialize();
        procedure DoFinalize();
    protected
        function IsEOF(): boolean; override;
        procedure Iterate(); override;
        function ExecuteItem(): boolean; override;
        procedure Initialize(); override;
        procedure Finalize(); override;

        procedure BeforeInitialize(); virtual;
        procedure BeforeFinalize(); virtual;
    public
        constructor Create(const aStartInfo: TRunStartInfo; aIsEvent: boolean;
            aOnSetReturnKeyValues: TSetReturnKeyValuesEvent);
        destructor Destroy(); override;
        procedure Execute(); override;
    end;

    TLiveRunHandler = class(TBasicLiveRunHandler)
    private
        fReturnKeyValues: TArray<TStreamableItem>;
    protected
        procedure BeforeInitialize(); override;
        procedure BeforeFinalize(); override;
    public
        constructor Create(const aStartInfo: TRunStartInfo; aIsEvent: boolean;
            aOnSetReturnKeyValues: TSetReturnKeyValuesEvent);
        property ReturnKeyValues: TArray<TStreamableItem>read fReturnKeyValues;
    end;

    TEventLiveRunHandler = class(TBasicLiveRunHandler)
    protected
        fOldProcessor: TProcessor;
        procedure BeforeInitialize(); override;
        procedure BeforeFinalize(); override;
    public
        constructor Create(const aStartInfo: TRunStartInfo; aIsEvent: boolean);
    end;


implementation


uses
    SysUtils,
    TypeMapTranslator,
    ErrorManager,
    AppSettings,
    CommonTypes,
    LogManager,
    RunFlow,
    AppTypes,
    DevicesConflictManager,
    ErrorMessageFactory,
    ResourceManager,
    LiveRunStepBuilderHelper,
    SamGlobe,
    RunStepFactory,
    GUIManagerRun,
    ExecFactory,
    ErrorInfo,
    RunTraceManager,
    RunStepBlockManager,
    RunStepTranslator,
    RunStepTranslatorFactory,
    ToolHandling,
    WashHandling;

{ TMultipleItems }

constructor TMultipleItems.Create();
begin
    inherited Create();
    fList := TObjectList<TMultipleItemsItem>.Create(false);
    fItemIndex := -1;
end;

destructor TMultipleItems.Destroy();
begin
    fList.Free;
    inherited;
end;

procedure TMultipleItems.AddItem(aItem: TMultipleItemsItem);
begin
    fList.Add(aItem);
end;

function TMultipleItems.ItemCount(): integer;
begin
    result := fList.Count;
end;

function TMultipleItems.GetItemAt(aIndex: integer): TMultipleItemsItem;
begin
    result := nil;
    if aIndex >= ItemCount then
        EXIT;
    result := fList[aIndex];
end;

function TMultipleItems.FindNextItem(): TMultipleItemsItem;
begin
    Inc(fItemIndex);
    result := self.Items[fItemIndex];
end;

procedure TMultipleItems.AddItems(aItems: TMultipleItems);
var
    i: integer;
begin
    for i := 0 to aItems.ItemCount - 1 do
    begin
        AddItem(aItems.Items[i]);
    end;
end;

{ TMultipleItemsItem }

constructor TMultipleItemsItem.Create(const aIExecutable: IExecutable;
    const aIMultipleItemsItem: IMultipleItemsItem; const aRunStep: TRunStep;
    const aIResourcedItem: IResourcedItem);
begin
    inherited Create;
    fIntfExecutable := aIExecutable;
    fIntfMultipleItemsItem := aIMultipleItemsItem;
    fRunStep := aRunStep;
    fIntfResourcedItem := aIResourcedItem;
end;

{ TMultipleItemsHandler }

constructor TMultipleItemsHandler.Create( { ; aWashManager : TWashManager } );
begin
    inherited Create(nil);

    fTaskList := TObjectList<TRunActionReoperationTask>.Create;
end;

destructor TMultipleItemsHandler.Destroy;
begin
    FreeAndNil(fTaskList);

    inherited;
end;

function TMultipleItemsHandler.IsItemExecutable(const aRunStep: TRunStep): boolean;
begin
    result := true;
end;

procedure TMultipleItemsHandler.ExecuteReoperationTasks(aRunStep: TRunStep);
var
    x: integer;
begin
    // Reoperation-Tasks ausführen
    for x := 0 to fTaskList.Count - 1 do
    begin
        fTaskList[x].Execute(aRunStep);
    end;

    // Reoperation-Tasks löschen, die erledigt sind
    for x := fTaskList.Count - 1 downto 0 do
    begin
        if (fTaskList[x].CanBeDeleted) then
            fTaskList.Delete(x);
    end;
end;

procedure TMultipleItemsHandler.BeforeItemExecution();
begin
    // Reoperation-Tasks ausführen
    ExecuteReoperationTasks(fItem.RunStep);

    if Assigned(fItem.IntfMultipleItemsItem) and
        (Pos(fItem.RunStep.StepName, UpperCase(gDontPutBackToolForActions)) = 0) // 17.09.08 pk dirty hack!
        and fItem.IntfMultipleItemsItem.MustBringBackToolBeforeExecute() then
        TToolHandling.BringBackTools(false); // Bring tool back if it is a danger to this action
end;

procedure TMultipleItemsHandler.AfterItemExecution();
var
    xReoperationTask: TRunActionReoperationTask;
begin
    // hat die aktuelle Action einen Reoperation-Task?
    if Assigned(fItem.IntfMultipleItemsItem) then
    begin
        xReoperationTask := fItem.IntfMultipleItemsItem.CreateReoperationTask();
        if Assigned(xReoperationTask) then
            fTaskList.Add(xReoperationTask);
    end;
end;

procedure TMultipleItemsHandler.AcquireResources();
begin
    if Assigned(fItem.IntfResourcedItem) then
    begin
        fItem.IntfResourcedItem.AcquireResources();
    end;
end;

procedure TMultipleItemsHandler.ReleaseResources();
begin
    if Assigned(fItem.IntfResourcedItem) then
    begin
        fItem.IntfResourcedItem.ReleaseResources();
    end;
end;

function TMultipleItemsHandler.GetBasicItemDescription(const aRunStep: TRunStep): string;
begin
    result := '';
    if Assigned(aRunStep) then
        result := aRunStep.Description;
end;

function TMultipleItemsHandler.GetLineDescription(): string;
var
    xMethodName: string;
    xLineNumber: integer;
begin
    TRunStepBuilderProcessor.GetCurrentProgramCounterInfo(xMethodName, xLineNumber);
    result := Format('Method: %s, Line: %d', [xMethodName, xLineNumber]);
end;

function TMultipleItemsHandler.GetItemDescription(const aRunStep: TRunStep): string;
begin
    result := GetBasicItemDescription(aRunStep);
end;

procedure TMultipleItemsHandler.ShowError(const aLineDescription, aItemDescription, aError: string);
var
    xErrorInfo: TErrorInfo;
begin
    xErrorInfo := TErrorInfo.CreateAndInit(aItemDescription, aLineDescription, eibAbort);
    try
        xErrorInfo.AddText(aError);
        gErrorMessageFactory.ShowAnyError(xErrorInfo);
    finally
        xErrorInfo.Free;
    end;
end;

function TMultipleItemsHandler.ExecuteItem(): boolean;
begin
    result := true;
end;

procedure TMultipleItemsHandler.ExecuteItems();
begin
    while true do
    begin
        if gErrorManager.IsGlobalErr then
            BREAK;

        // if SafeExit flag has been set, don't create the next action. just Break
        if gRunFlow.IsSafeExitRequested then
        begin
            gLogManager.Log('A Safe Exit request was detected, execution will be stopped.', true);
            BREAK;
        end;

        Iterate();
        if IsEOF() then
            EXIT;
        if not ExecuteItem() then
            BREAK;
    end;

    TRunTraceManager.Instance.ForceFlushCurrentProcessAndThread();

    AfterExecuteItems();
end;

procedure TMultipleItemsHandler.Execute();
begin
    ExecuteItems();
end;

procedure TMultipleItemsHandler.AfterExecuteItems();
begin
    // Reoperation-Tasks zum letzten Mal ausführen
    if not gErrorManager.IsGlobalErr() then
        ExecuteReoperationTasks(fItem.RunStep);
end;

{ TBasicLiveRunHandler }

constructor TBasicLiveRunHandler.Create(const aStartInfo: TRunStartInfo; aIsEvent: boolean;
    aOnSetReturnKeyValues: TSetReturnKeyValuesEvent);
begin
    inherited Create();
    fMethodDebugMode := (TAppSettings.AppPurpose = TAppPurpose.appEditAndStart);
    fStartInfo := aStartInfo;
    fStartInfo.Params := TObjectCopy<TKeyArgValueList>.Copy(aStartInfo.Params);
    fIsEvent := aIsEvent;
    fProcessor := TRunStepBuilderProcessor.Create(aOnSetReturnKeyValues);
    fRunStepBuilderHelper := TLiveRunStepBuilderHelper.Create();
end;

destructor TBasicLiveRunHandler.Destroy();
begin
    FreeAndNil(fRunStepBuilderHelper);
    FreeAndNil(fProcessor);
    FreeAndNil(fStartInfo.Params);
    inherited;
end;

procedure TBasicLiveRunHandler.Execute();
begin
    Initialize();
    try
        ExecuteItems();
    finally
        Finalize();
    end;
end;

procedure TBasicLiveRunHandler.Iterate();
begin
    if fProcessor.IsPendingSteps then
        EXIT;
    fProcessor.ProcessNext();
end;

function TBasicLiveRunHandler.IsEOF(): boolean;
begin
    result := (not fProcessor.IsPendingSteps) and (fProcessor.CurrentStep = nil);
end;

procedure TBasicLiveRunHandler.TranslateRunSteps(const aSourceRunSteps, aDestRunSteps: TRunStepList);
var
    xFirstRunStep: TRunStep;
    xTranslated: boolean;
    xRunStepTranslator: TRunStepTranslator;
begin
    xTranslated := false;
    aDestRunSteps.Clear();
    xFirstRunStep := aSourceRunSteps[0];
    xRunStepTranslator := TRunStepTranslatorFactory.CreateTranslatorByRunStep(xFirstRunStep);
    if Assigned(xRunStepTranslator) then
    begin
        xTranslated := xRunStepTranslator.TranslateSteps(aSourceRunSteps, aDestRunSteps)
    end;

    if not xTranslated then
    begin
        aDestRunSteps.AddSteps(aSourceRunSteps);
    end;

end;

procedure TBasicLiveRunHandler.DoCreateRunSteps();
var
    xCurrentStep: TRunStepByMethodStepBuilder;
    xRunSteps, xRunStepsBeforeTranslate: TRunStepList;
begin
    if fProcessor.IsPendingSteps then
        EXIT;

    try
        xCurrentStep := fProcessor.CurrentStep; // xTempLine as TRunStepByMethodStepBuilder;
        ASSERT(Assigned(xCurrentStep));

        TRunTraceManager.Instance.AddAction(xCurrentStep.ActionName);

        xRunStepsBeforeTranslate := TRunStepList.Create(false);
        try
            xCurrentStep.CreateRunSteps(xRunStepsBeforeTranslate, fRunStepBuilderHelper);

            xRunSteps := TRunStepList.Create(false);
            try
                TranslateRunSteps(xRunStepsBeforeTranslate, xRunSteps);

                fProcessor.AddStepsToPendingSteps(xRunSteps, true);
            finally
                FreeAndNil(xRunSteps);
            end;
        finally
            FreeAndNil(xRunStepsBeforeTranslate);
        end;

    except
        on E: Exception do
        begin
            if (E.Message <> STR_EXCEPTION_NOMESSAGE) then
            begin
                ShowError(fCurrentLineDescription, fCurrentLineDescription, E.Message);
            end;
            gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
            EXIT;
        end;
    end;
end;

function TBasicLiveRunHandler.DoCreateAction(): boolean;
var
    xAction: TRunAction;
    xIntfExec: IExecutable;

begin
    result := false;

    if fProcessor.PendingRunStepsIterator.IsEof then
        EXIT;

    // for restart: keep track of old position before iterating
    fProcessor.PendingRunStepsIterator.BookmarkCurrentCursorPos();

    xAction := fActionFactory.CreateNextAction(fProcessor.PendingRunStepsIterator);

    if not Assigned(xAction) then
        EXIT;

    xIntfExec := TExecFactory.CreateExec(xAction);

    if not Assigned(xIntfExec) then
        EXIT;

    fItem := TMultipleItemsItem.Create(xIntfExec, xAction, xAction.RunStep, xAction);

    result := true;
end;

function TBasicLiveRunHandler.DoExecuteItem(): boolean;
var
    xLineDescription: string;
    xItemDescription: string;
    xItemLogDescription: string;
begin
    if not Assigned(fItem) then
        EXIT(false);

    if not IsItemExecutable(fItem.RunStep) then
        EXIT(true);

    if fMethodDebugMode then
    begin
        // Pause-Zustand herstellen, wenn StepByStep = true
        if TGUIManagerRun.Instance.GetStepByStepMode then
            TGUIManagerRun.Instance.StateSetPause;
    end;

    xLineDescription := fCurrentLineDescription;
    xItemDescription := GetItemDescription(fItem.RunStep);
    xItemLogDescription := xLineDescription + ', ' + xItemDescription;
    gLogManager.Log(xItemLogDescription + ' - START', fMethodDebugMode);

    TRunTraceManager.Instance.CurrentActionStarted(fItem.RunStep);

    self.BeforeItemExecution();

    self.AcquireResources();
    try
        try
            fItem.IntfExecutable.Execute();
        except
            on E: Exception do
            begin
                if (E.Message <> STR_EXCEPTION_NOMESSAGE) then
                begin
                    ShowError(xLineDescription, xItemDescription, E.Message);
                end;
                gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
                EXIT(false);
            end;
        end;
    finally
        self.ReleaseResources();
    end;

    self.AfterItemExecution();

    TRunTraceManager.Instance.CurrentActionFinished();
    TRunTraceManager.Instance.FlushCurrentProcessAndThread();

    gLogManager.Log(xItemLogDescription + ' - FINISH', fMethodDebugMode);

    EXIT(true);
end;

function TBasicLiveRunHandler.ExecuteItem(): boolean;
begin
    result := false;

    fCurrentLineDescription := GetLineDescription;

    DoCreateRunSteps();

    fProcessor.PendingRunStepsIterator.CurrentBatchIndex := 0;

    while true do
    begin
        if gErrorManager.IsGlobalErr() then
            EXIT;

        try
            result := DoCreateAction();
            if not result then
                EXIT;

            result := DoExecuteItem();

        finally
            FreeAndNil(fItem);
        end;

        fProcessor.PendingStepsIteratorPosChanged();
        if not fProcessor.IsPendingSteps then
        begin
            fProcessor.ClearPendingSteps();
            BREAK;
        end;

        if not result then
            EXIT;
    end;
end;

procedure TBasicLiveRunHandler.DoInitialize();
begin
    if not fStartInfo.IsRestart then
    begin
        fProcessor.MarkStopAddress();
        fProcessor.PrepareCallMethod(fStartInfo.MethodName, fStartInfo.Params, fIsEvent);
    end;

    fActionFactory := TRunActionFactory.Create();
end;

procedure TBasicLiveRunHandler.DoFinalize();
begin
    FreeAndNil(fActionFactory);
end;

procedure TBasicLiveRunHandler.BeforeFinalize();
begin
end;

procedure TBasicLiveRunHandler.BeforeInitialize();
begin
end;

procedure TBasicLiveRunHandler.Initialize();
begin
    BeforeInitialize();
    DoInitialize();
end;

procedure TBasicLiveRunHandler.Finalize();
begin
    BeforeFinalize();
    DoFinalize();
end;

{ TLiveRunHandler }

procedure TLiveRunHandler.BeforeInitialize();
begin
    TRunStepBuilderProcessor.AssignProcessorForCurrentThread(fProcessor);
    TRunTraceManager.Instance.AddThreadTraceForCurrentThread(fStartInfo.IsMainThread);
end;

constructor TLiveRunHandler.Create(const aStartInfo: TRunStartInfo; aIsEvent: boolean;
    aOnSetReturnKeyValues: TSetReturnKeyValuesEvent);
begin
    inherited Create(aStartInfo, aIsEvent, aOnSetReturnKeyValues);
end;

procedure TLiveRunHandler.BeforeFinalize();
begin
    TWashHandling.WashAllDirtyTips();

    TRunStepBlockManager.Instance.EndAllBlocksForCurrentThread();

    TRunTraceManager.Instance.RemoveThreadTraceForCurrentThread(not gErrorManager.IsGlobalErr);

    gResourceManager.ReleaseAllResources(rtResScheme);

    TDevicesConflictManager.Instance.UnRegisterUseXMotorAll();
end;

{ TEventLiveRunHandler }

procedure TEventLiveRunHandler.BeforeInitialize;
begin
    // disable tracing in events
    TRunTraceManager.Instance.SetPauseTracingForCurrentThread(true);

    // this is a hack! it would be nice to just have one processor for a thread.  If I had more time I would implement this better
    // the problem is that the Processor.PendingSteps cannot be shared by between the eventhandler and the methodhandler
    fOldProcessor := TRunStepBuilderProcessor.GetProcessorForCurrentThread();
    TRunStepBuilderProcessor.AssignProcessorForCurrentThread(fProcessor);
end;

constructor TEventLiveRunHandler.Create(const aStartInfo: TRunStartInfo; aIsEvent: boolean);
begin
    inherited Create(aStartInfo, aIsEvent, nil);
end;

procedure TEventLiveRunHandler.BeforeFinalize;
begin
    // set the oldprocessor back
    TRunStepBuilderProcessor.AssignProcessorForCurrentThread(fOldProcessor);

    // enable tracing again
    TRunTraceManager.Instance.SetPauseTracingForCurrentThread(false);
end;


end.
