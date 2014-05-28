{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  08.09.08 pk                                        TN4215    Initial Revision
  20.09.08 pk  GetRealBarcodes                       TN4215    reactivated
  25.09.08 pk                                        TN4241   GUIManagerRun removed
  06.11.08 pk                                        TN4279   various changes
  06.11.08 pk                                        TN4280   various changes
  10.11.08 pk  StartThreadMethod                     TN4280   Call EventManager.StartThreadedMethod
  17.11.08 pk  fSecondaryThreadCount                 TN4280   New
  17.11.08 pk  TExecHandler.MainMethodName           TN4280   removed
  17.11.08 wl  StopButton                            TN4312   TBitBtn statt TButton
  19.11.08 pk  TExecHanlder                          TN4280   calls BringBackTools at end
  24.11.08 pk  gmCreateRunExecHandlerThread          TN4280   returns SysHandleID
  27.11.08 pk  gmCreateRunExecHandlerThread          TN4280   call Unpause
  02.12.08 pk  gmCreateRunExecHandlerThread          TN4280   Create Process instead of just a thread
  09.12.08 pk                                        TN4279   uses changed
  17.12.08 pk  gmCreateRunExecHandlerThread          TN4372   CreateProcess with new IsSimulated parmater
  17.12.08 pk  ShowDoneDialog                        TN4279   Method Done Dialog Reactivated
  08.01.09 pk  TExecHandler.Create                   TN4381   new StartParams parameter
  17.02.09 pk                                        TN4232   various changes
  20.02.09 pk                                        TN4232   Changes for multithreaded trace
  06.03.09 pk  ShowDonDialog                         TN4335   Dont show when IsSafeExitRequested
  30.03.09 pk  AfterFinalize                         TN4495   Clear the ReferenceIdentValueMemoryManager list and close datasets
  06.04.09 pk  StartDisplayComponentEventHandler     TN4503   New: start new thread in background to handle events for displaycomponents
  14.04.09 pk  BeforeInitialize                      TN4523   Call sysevents.Init
  09.06.09 pk  DoPerform                             TN4595   Call StartInternalSecondaryThreads even on restart
  09.06.09 pk  RestartSecondaryThreads               TN4595   Dont restart DisplayEventHandlerThread
  10.08.09 wl                                        TN4702   Strings werden jetzt direkt geladen
  08.09.09 pk                                        TN4753   uses ErrorMessage replaced by ErrorInfo
  04.02.10 pk                                        TN4972   Changes for Restart
  09.03.10 pk                                        TN5015   Massive Changes in ThreadClasses
  04.09.10 pk  EnableControls                        TN5042   now calls DisplayComponents_RunEnable
  07.05.10 pk  TExecHandler                          TN5092   fStartParams is TKeyArgValueList instead of array
  07.06.10 pk  TExecHandler                          TN5077   New DatasourceStart, etc. from LisGlobe
  07.06.10 ts  TExecHandler                          TN5124   AskBeforeStart: AskForRackPlacement reimplemented
  07.06.10 ts  TExecHandler                          TN5124   AskBeforeStart: AskForRackPlacement reimplemented -> LoadRun removed
  08.06.10 pk  TExecHandler                          TN5077   Simulation Mode Change reimplemented
  11.06.10 pk  gmCreateRunExecHandlerThread          TN5138   New IsSystemProcess Param
  21.06.10 wl  TExecHandler.AskBeforeStart           TN5116   Im Designer ist per Default "Simulation" ausgewählt
  27.07.10 wl  TExecHandler.AfterFinalize            TN5123   ExpAfterRun auskommentiert
  19.10.10 wl  TExecHandler.AfterFinalize            TN5288   Aufruf von gmGetRealBarcodes ohne GripperArm -> kein "Read Again"-Button#
  27.10.10 pk  DatasourceCheck                       TN5313   use ProcessDescription as parameter for LoadDisplayComponent func
  29.10.10 pk  ExecuteRestartEvent                   TN5320   New
  16.11.10 wl                                        TN5351   uses ActionLow entfernt
  19.11.10 wl  evStartMethodOrScript                 TN5358   kein Rückgabewert mehr
  07.12.10 pk  DoPrepare                             TN5386   call RemoveProcessTrace if prepare was not successful
  13.12.10 wl                                        TN5370   TLock.WaitForLock statt TLock.WaitFor
  02.02.11 wl  DatasourceCheck                       TN5466   wenn LayoutName = '*' dann erscheint ein Layout-Auswahlfenster
  21.03.11 wl  AskBeforeStart                        TN5508   neu: SimulationAskWeight und SimulationSpeed_Percent werden ausgewertet
  27.04.11 wl  AskBeforeStart                        TN5554   ruft gRunFlow.WriteValues auf
  20.10.11 wl  RunDoneDlg                            TN5723   von LisGlobe hierher
  14.12.11 wl  ShowDoneDialog                        TN5765   MethDone-Dialog durch MessageBox ersetzt
  14.12.11 wl  DatasourceCheck                       TN5765   verwendet TRunGlobals.ReloadRunLayoutAndDeletePosinfo
  27.12.11 wl  DatasourceCheck                       TN5768   Startfenster erscheint, bevor das Layout geladen wird
  27.12.11 wl  DatasourceCheck                       TN5768   wenn DeleteRunDataOption = false, werden vor dem Laden des Layouts keine Daten gelöscht!
  27.12.11 wl  LoadRunLayoutAndPosinfoToLayout       TN5768   wenn DeleteRunDataOption = false, werden die Posinfo-Daten in das Layout geladen
  02.03.12 wl                                        TN5822   uses geändert
  13.03.12 wl  DatasourceCheck                       TN5798   es wird jetzt wirklich das Run-Layout geladen!
  09.05.12 wl  LogRun                                TN5858   wird jetzt über den GUIManager aufgerufen
  18.06.12 wl  DatasourceCheck                       TN5899   Nach dem Laden der Display-Komponenten werden die Liquids auf die "echten" Volumina zurückgesetzt
  07.08.12 wl  gmCreateRunExecHandlerThread          TN5946   entfernt
  07.08.12 wl                                        TN5946   TSourceDataType entfernt
  21.11.12 wl  DatasourceCheck                       TN6021   direkt nach dem Laden des Layouts wird SubstanceData neu in den Cache geladen
  30.07.13 wl                                        TN6160   an TSystemEvents angepasst
  15.08.13 wl                                        TN6223   uses geändert
  21.08.13 wl                                        TN6231   an Änderungen in ToolHandling angepasst
  22.08.13 wl                                        TN6233   an TDeviceInitHandling angepasst
  30.08.13 wl  fMethodSettings                       TN6236   ersetzt mehrere interne Variablen
  ----------------------------------------------------------------------------------------------------------------------- }

unit ExecHandler;


interface


uses
    Buttons,
    GeneralTypes,
    ThreadClasses,
    AppTypes,
    MethodTypes,
    MethodSettingsDataAdaptor,
    ActionHandlerLow,
    Executable,
    DisplayComponentEventHandler;

type
    TSimpleRunTimeExecHandler = class(TBasicSingleActionExecHandler)
    protected
        procedure DoInitAction(aFullInit: boolean); override;
        procedure BeforeInitialize(); override;
        procedure AfterFinalize(); override;
    end;

    TRunTimeExecHandler = class(TBasicExecHandler)
    protected
        procedure DoInitAction(aFullInit: boolean); override;
        procedure SystemEventsInitialize();
        procedure SystemEventsFinalize();
    end;

    TExecLogRunType = (elrStart, elrEnd);

    TExecHandlerRestartParams = record
        IsRestart: boolean;
        TraceName: string;
        ContinueAtActionID: integer;
        ContinueAtSegmentIndex: integer;
    end;

    TExecHandler = class(TRunTimeExecHandler)
    private
        function GetDisplayContextID: string;
        function GetDisplayEventHandlerThreadDescription(): string;
        function DatasourceCheck(const aResultStartParams: TKeyArgValueList; aDisplayMessages: boolean;
            var vIsSimulated: boolean): boolean;
        function DatasourceStart(): boolean;
        class function AskBeforeStart(const aDatasourceName: string; const aSimChangeAllowed: boolean;
            var vIsSimulated: boolean): boolean;
        class function ShowRackPlacement: boolean; static;
        procedure RemoveProcessTrace();
        procedure RestartSecondaryThreads();
        procedure StartDisplayComponentEventHandler();
        procedure StopDisplayComponentEventHandler();
        procedure StopInternalSecondaryThreads();
        procedure StartInternalSecondaryThreads();
        procedure ExecuteRestartEvent(const aRestartEventName: string);
        procedure LogRun(aLogRunType: TExecLogRunType);
    protected
        fInitOK: boolean;
        fUserLogRun: boolean;
        fNoMessages: boolean;
        fInternalSecondaryThreadCount: integer;
        fStartParams: TKeyArgValueList;
        fDisplayComponentEventHandler: IDisplayComponentEventHandler;
        fRestartParams: TExecHandlerRestartParams;
        fMethodSettings: TMethodSettingsRec;
        fIsSimulated: boolean;
        fProcessDescription: string;

        procedure ShowDoneDialog(); virtual;
        function DoPrepare(): boolean; override;
        procedure EnableControls(aEnable: boolean); override;
        procedure BeforeInitialize(); override;
        procedure AfterInitialize(); override;
        procedure AfterFinalize(); override;

        procedure DoSecondaryThreadAdded(const aThreadID: cardinal); override;
        procedure DoSecondaryThreadRemoved(const aThreadID: cardinal); override;
        procedure DoPerform(); override;
    public
        constructor Create(const aMethodSettings: TMethodSettingsRec;
            aInitAtBegin, aInitAtEnd, aUserLogRun, aNoMessages: boolean; const aIsSimulated: boolean;
            const aRestartParams: TExecHandlerRestartParams; aStopButton: TBitBtn = nil);
        destructor Destroy(); override;
        class function DetermineArchiveName(const aDataName: string): string;
        property InitOK: boolean read fInitOK;
    end;


implementation


uses
    SysUtils,
    Controls,
    ErrorManager,
    LogManager,
    RunFlow,
    LayoutManager,
    SamGlobe,
    AppSettings,
    CommonTypes,
    Liquids,
    ThrdMan,
    ThreadAPI,
    TubeHandling,
    ToolHandling,
    SystemEvents,
    EventManager,
    RunTraceManager,
    ReferenceIdentValueMemory,
    GUIManagerRun,
    RunGlobals,
    DeviceInitHandling,
    SubstanceLoading,
    LayoutDataAdaptorExt,
    RestartPreparationStep;

{ TSimpleRunTimeExecHandler }

procedure TSimpleRunTimeExecHandler.DoInitAction(aFullInit: boolean);
begin
    TDeviceInitHandling.ConnectAndInitRun(aFullInit);
end;

procedure TSimpleRunTimeExecHandler.BeforeInitialize();
begin
    TThreadManagerSetup.Instance.ThreadIsStarted();
    TSystemEvents.Instance.MethodStart();
end;

procedure TSimpleRunTimeExecHandler.AfterFinalize();
begin
    TSystemEvents.Instance.MethodFinish();
    TThreadManagerSetup.Instance.ThreadIsStopped();
end;

// ************************************************* TRunTimeActionHandler ******************************
// Same as Basic Action Handler but, uses TInitAction instead of TBasicInitAction
// This is used by the Flush Dialog Module

{ TRunTimeExecHandler }

procedure TRunTimeExecHandler.DoInitAction(aFullInit: boolean);
begin
    TDeviceInitHandling.ConnectAndInitRun(aFullInit);
end;

procedure TRunTimeExecHandler.SystemEventsInitialize();
begin
    TSystemEvents.Instance.MethodStart();
end;

procedure TRunTimeExecHandler.SystemEventsFinalize();
begin
    TSystemEvents.Instance.MethodFinish();
end;

{ TExecHandler }

constructor TExecHandler.Create(const aMethodSettings: TMethodSettingsRec;
    aInitAtBegin, aInitAtEnd, aUserLogRun, aNoMessages: boolean; const aIsSimulated: boolean;
    const aRestartParams: TExecHandlerRestartParams; aStopButton: TBitBtn = nil);
begin
    inherited Create(aInitAtBegin, aInitAtEnd, aStopButton);

    fMethodSettings := aMethodSettings;
    fUserLogRun := aUserLogRun;
    fNoMessages := aNoMessages;
    fSecondaryThreadCount := 0;
    fInternalSecondaryThreadCount := 0;
    fIsSimulated := aIsSimulated;
    fStartParams := TKeyArgValueList.Create(); // TObjectCopy<TKeyArgValueList>.Copy( aStartParams );
    fDisplayComponentEventHandler := nil;
    fRestartParams := aRestartParams;
end;

destructor TExecHandler.Destroy();
begin
    FreeAndNil(fStartParams);
    inherited;
end;

procedure TExecHandler.DoSecondaryThreadAdded(const aThreadID: cardinal);
begin
    Inc(fSecondaryThreadCount);
end;

procedure TExecHandler.DoSecondaryThreadRemoved(const aThreadID: cardinal);
begin
    Dec(fSecondaryThreadCount);
    if fSecondaryThreadCount - fInternalSecondaryThreadCount = 0 then
    begin
        StopInternalSecondaryThreads();
        TThreadAPI.ExitProcess();
    end;
end;

procedure TExecHandler.EnableControls(aEnable: boolean);
var
    xRunControlsEnabled: boolean;
begin
    inherited EnableControls(aEnable);
    // reversed logic
    xRunControlsEnabled := not aEnable;
    TGUIManagerRun.Instance.DisplayComponents_RunEnable(xRunControlsEnabled);
end;

procedure TExecHandler.RemoveProcessTrace();
begin
    TRunTraceManager.Instance.RemoveProcessTraceForCurrentProcess(not gErrorManager.IsGlobalErr);
end;

procedure TExecHandler.RestartSecondaryThreads();
var
    x: integer;
    xSecondaryThreadTraceNames: TStringArray;
    xThreadDescription: string;
    xDisplayEventHandlerThreadDescription: string;
begin
    xDisplayEventHandlerThreadDescription := self.GetDisplayEventHandlerThreadDescription();

    xSecondaryThreadTraceNames := TRunTraceManager.Instance.GetSecondaryThreadTraceNamesForCurrentProcess();
    for x := 0 to high(xSecondaryThreadTraceNames) do
    begin
        xThreadDescription := xSecondaryThreadTraceNames[x];

        if not SameText(xThreadDescription, xDisplayEventHandlerThreadDescription) then
        begin
            TEventManager.Instance.RestartThreadByTraceName(xThreadDescription, false);
        end;
    end;
end;

function TExecHandler.GetDisplayContextID: string;
begin
    result := fProcessDescription;
end;

function TExecHandler.GetDisplayEventHandlerThreadDescription(): string;
begin
    result := GetDisplayContextID + ' DisplayEventHandler';
end;

procedure TExecHandler.StartDisplayComponentEventHandler();
var
    xThreadDescription: string;
begin
    Inc(fInternalSecondaryThreadCount);
    xThreadDescription := GetDisplayEventHandlerThreadDescription();
    fDisplayComponentEventHandler := TDisplayComponentEventHandler.Create(xThreadDescription);
    TThreadAPI.CreateThread(xThreadDescription, false, true, true, fDisplayComponentEventHandler);
    fDisplayComponentEventHandler.Unpause();
end;

procedure TExecHandler.StopDisplayComponentEventHandler();
begin
    fDisplayComponentEventHandler.Quit();
end;

procedure TExecHandler.StartInternalSecondaryThreads();
begin
    StartDisplayComponentEventHandler();
end;

procedure TExecHandler.StopInternalSecondaryThreads();
begin
    StopDisplayComponentEventHandler();
end;

procedure TExecHandler.ExecuteRestartEvent(const aRestartEventName: string);
begin
    if aRestartEventName = '' then
        EXIT;
    fDisplayComponentEventHandler.RequestExecuteEvent(nil, aRestartEventName, true);
end;

procedure TExecHandler.DoPerform;
begin
    if TRunTraceManager.Instance.IsCurrentProcessRestart then
    begin
        StartInternalSecondaryThreads();
        ExecuteRestartEvent(fMethodSettings.RestartEventName);
        TEventManager.Instance.RestartThreadByTraceName
            (TRunTraceManager.Instance.GetMainThreadTraceNameForCurrentProcess(), true);
        RestartSecondaryThreads();
    end
    else
    begin
        TEventManager.Instance.StartThreadedMethod(fMethodSettings.MethodName, fStartParams, true);
        StartInternalSecondaryThreads();
    end;
end;

class function TExecHandler.DetermineArchiveName(const aDataName: string): string;
var
    xPrefix: string;
begin
    xPrefix := 'M';

    result := xPrefix + '~' + aDataName;
end;

procedure TExecHandler.LogRun(aLogRunType: TExecLogRunType);
var
    xMessage: string;
    xLogRunType: TLogRunType;
begin
    if not fUserLogRun then
        EXIT;

    if aLogRunType = elrStart then
    begin
        TGUIManagerRun.Instance.SetUserLogRun(true);
        xMessage := ' started';
        xLogRunType := lrtStart;
    end
    else
    begin
        TGUIManagerRun.Instance.SetUserLogRun(false);
        xMessage := ' worked up';
        xLogRunType := lrtWorkedUp;
    end;
    xMessage := 'Method' + xMessage;
    gCommonDll.CurrentUser.LogRun(rkdMethod, fMethodSettings.MethodName, xMessage, '', xLogRunType);
end;

class function TExecHandler.ShowRackPlacement(): boolean;
var
    xLayoutRunName: string;
    xLayoutName: string;
begin
    result := true;
    // ----------------------------------------------------------------- Letzte Nachfrage vor dem Start
    if (gAskForRackPlacement = 1) then
    begin
        xLayoutRunName := TLayoutManager.Instance.CurrentLayout.LayoutRunName;
        xLayoutName := TLayoutManager.Instance.CurrentLayout.LayoutName;
        result := TGUIManagerRun.Instance.RackPlaceList_PromptModal(xLayoutRunName, xLayoutName, '', '', true,
            false) = mrOK;
    end;
end;

class function TExecHandler.AskBeforeStart(const aDatasourceName: string; const aSimChangeAllowed: boolean;
    var vIsSimulated: boolean): boolean;
const
    cSimulationText = '(Simulation)';
var
    xIsSimChangeAllowed: boolean;
    xIsSimulated: boolean;
    xSimulationAskWeight: boolean;
    xSimulationSpeed_Percent: integer;
begin
    // if exe was started in simulation, we should not change the mode
    xIsSimChangeAllowed := aSimChangeAllowed and (TRunGlobals.Instance.CanSimulateInRealMode);
    xIsSimulated := vIsSimulated;

    xSimulationAskWeight := gRunFlow.SimulationAskWeight;
    xSimulationSpeed_Percent := gRunFlow.SimulationSpeed_Percent;
    result := TGUIManagerRun.Instance.AskRunStart_PromptModal(aDatasourceName, xIsSimChangeAllowed,
        xIsSimulated, xSimulationAskWeight, xSimulationSpeed_Percent) = mrOK;
    gRunFlow.SimulationAskWeight := xSimulationAskWeight;
    gRunFlow.SimulationSpeed_Percent := xSimulationSpeed_Percent;
    gRunFlow.WriteValues();

    if not result then
        EXIT;
    if xIsSimChangeAllowed then
        vIsSimulated := xIsSimulated;
end;

function TExecHandler.DatasourceCheck(const aResultStartParams: TKeyArgValueList; aDisplayMessages: boolean;
    var vIsSimulated: boolean): boolean;
var
    xLayoutName: string;
    xResultPreparationStepList: TRestartPreparationStepList;
begin
    result := true;

    xResultPreparationStepList := TRestartPreparationStepList.Create();

    if fRestartParams.IsRestart then
    begin
        TRunTraceManager.Instance.ReadPreparationSteps(fRestartParams.TraceName,
            fRestartParams.ContinueAtActionID, xResultPreparationStepList);
        TRunTraceManager.Instance.UndoThreadTraceTillActionID(fRestartParams.TraceName,
            fRestartParams.ContinueAtActionID, fRestartParams.ContinueAtSegmentIndex);

        { TODO : Der LayoutName muss aus dem TraceManager gelesen werden }
        xLayoutName := fMethodSettings.LayoutName;
    end
    else
    begin
        xLayoutName := fMethodSettings.LayoutName;
        if (xLayoutName = TMethodSettingsDataAdaptor.cChooseLayoutAtRuntime) then
        begin
            xLayoutName := TGUIManagerRun.Instance.ChooseLayout('Start method');
        end;
    end;

    TRunTraceManager.Instance.AddProcessTraceForCurrentProcess(fRestartParams.IsRestart);

    TLayoutManager.Instance.SyncUnRegisterCurrentLayout();
    if (xLayoutName <> '') then
    begin
        TLayoutManager.Instance.SyncRegisterLayout(fMethodSettings.MethodName, xLayoutName);
    end;

    TGUIManagerRun.Instance.LoadDisplayComponentToMain(fMethodSettings.DisplayComponentName,
        self.GetDisplayContextID());
    TLiquids.Instance.ResetVolumesToRealValues;

    TGUIManagerRun.Instance.GroupDisplay_StartNewRun(fMethodSettings.MethodName);
    TGUIManagerRun.Instance.SimulationMode_Enable(gRunFlow.AppSimulationMode);

    // Start-Meldung
    if aDisplayMessages then
    begin
        result := TExecHandler.AskBeforeStart(fMethodSettings.MethodName, not vIsSimulated, vIsSimulated);
        if not result then
            EXIT;
    end;

    if (not fRestartParams.IsRestart) then
    begin
        // Run-Layout und Posinfo löschen
        if (fMethodSettings.DeleteRunDataAtStart) then
            TLayoutDataAdaptorExt.DeleteRunLayoutAndPosinfo(fMethodSettings.MethodName);

        // Run-Layout neu in die Tabelle schreiben
        TLayoutDataAdaptorExt.WriteRunLayoutIfNecessary(fMethodSettings.MethodName, xLayoutName);
    end;

    if xLayoutName <> '' then
        TLayoutManager.Instance.CurrentLayout.DataCache.ReadByRun(fMethodSettings.MethodName);

    TRunTraceManager.Instance.PrepareProcessTraceForCurrentProcess();

    if xLayoutName <> '' then
        TLayoutManager.Instance.SyncLoadRun();

    // SubstanceData-Tabelle neu laden
    TSubstanceLoading.Instance.RefreshSubstanceDatabase;

    // Posinfo-Daten in das Layout schreiben
    if (not fMethodSettings.DeleteRunDataAtStart) then
        TSubstanceLoading.Instance.LoadPosinfoDataToLayout();

    // Restart: Restart preparations
    if fRestartParams.IsRestart then
    begin
        if xResultPreparationStepList.Count > 0 then
            result := TGUIManagerRun.Instance.RestartPreparations_PromptModal(fMethodSettings.MethodName,
                xResultPreparationStepList) = mrOK;
        if not result then
            EXIT;
    end;

    // Kein Restart: Parameter abfragen
    if not fRestartParams.IsRestart then
    begin
        result := TGUIManagerRun.Instance.EditMethodParameters_PromptModal(fMethodSettings.MethodName,
            aResultStartParams, true, true);
        if not result then
            EXIT;
    end;

    // Rack Placement
    if not ShowRackPlacement() then
        EXIT;

    FreeAndNil(xResultPreparationStepList);
end;

function TExecHandler.DatasourceStart(): boolean;
begin
    result := DatasourceCheck(fStartParams, not fNoMessages, fIsSimulated);

    if not result then
        EXIT;

    TGUIManagerRun.Instance.SimulationMode_Enable(fIsSimulated);
    TThreadAPI.ChangeCurrentProcessSimulated(fIsSimulated);
    if fIsSimulated then
    begin
        gLogManager.LogF('Method %s is running in SIMULATION mode', [fMethodSettings.MethodName], false);
    end;
end;

function TExecHandler.DoPrepare(): boolean;
begin
    fProcessDescription := TThreadAPI.GetCurrentProcessDescription();

    result := self.DatasourceStart();

    if not result then
    begin
        RemoveProcessTrace();
        self.Quit();
    end;
end;

procedure TExecHandler.BeforeInitialize();
var
    xDataTypeStr: string;
begin
    if not fPrepareSuccess then
        EXIT;

    inherited;

    TThreadManagerSetup.Instance.ThreadIsStarted();
    // global.OpenZipArchive( DetermineArchiveName( fExecDataName, fSourceDataType ) );
    xDataTypeStr := 'Method';
    gLogManager.Log(Format('Process log started for %s : %s',
        [xDataTypeStr, fMethodSettings.MethodName]), false);
    LogRun(elrStart);

    SystemEventsInitialize();
end;

procedure TExecHandler.AfterInitialize();
begin
end;

procedure TExecHandler.AfterFinalize();
begin
    RemoveProcessTrace();

    if not fPrepareSuccess then
        EXIT;
    TReferenceIdentValueMemoryManager.Instance.Clear();

    if not gErrorManager.IsGlobalErr then
        TToolHandling.BringBackToolsAtReset(false);

    // 24h mode: Am Schluss alle nicht gelesenen Barcodes abfragen
    if (g24hMode = 1) then
        gmGetRealBarcodes(false, nil, '');

    SystemEventsFinalize();
    ShowDoneDialog();

    // ----------------------------------------------------- Log worked up method
    if (not gErrorManager.IsGlobalErr) then
        LogRun(elrEnd);

    { TODO -oPK -cAction Package : Archiving disabled, must be re-implemented }
    // global.CloseZipArchive;

    TThreadManagerSetup.Instance.ThreadIsStopped();
    inherited;
end;

procedure TExecHandler.ShowDoneDialog();
var
    xCaption, xText: string;
begin
    if (not fExecCompleted) or gErrorManager.IsGlobalErr then
        EXIT;

    if fNoMessages then
        EXIT;

    if not gRunFlow.IsSafeExitRequested then
    begin
        xCaption := TLanguageString.Read('Method Completed', 'Methode beendet');
        xText := TLanguageString.Read('Method {0} is completed', 'Methode {0} ist beendet',
            [fMethodSettings.MethodName]);
        TGUIManagerRun.Instance.MessageBox(xText, xCaption);
    end;
end;


end.
