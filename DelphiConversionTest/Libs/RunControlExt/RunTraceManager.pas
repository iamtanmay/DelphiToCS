{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232      Initial Revision
  20.02.09 pk                                        TN4232      Changes for multithreaded trace
  20.02.09 pk AddAction                              TN4232      now with IsInEvent parameter
  24.02.09 pk CurrentActionFinished                  TN4232      New
  24.02.09 pk RewindThreadTraceToActionID            TN4232      changed to class function
  25.02.09 pk AddSideEffectDataToCurrentAction       TN4232      handle case where there is no currentaction
  04.03.09 pk CurrentActionFinished                  TN4232      Exit if not enabled
  04.11.09 pk                               	        TN4843   	Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.02.10 pk                                        TN4972      Various changes
  15.04.10 pk                                        TN5050      Changes for PipDeviceListDataCache
  23.04.10 pk RewindThreadTraceToActionID            TN5072      Changes to TMultiListDataCacheIterator
  07.06.10 pk                                        TN5077      Changes for restarting racks moves
  09.06.10 pk AddAction                              TN5077      Now with StepName
  24.06.10 pk RackTypeChanged                        TN5167      Check IsEnabled
  26.10.10 pk                                        TN5297      Changes for ActionData Segment concept
  16.11.10 pk AddProcessTraceForCurrentProcess       TN5340      Add Process Details if details are missing
  01.03.12 wl                                        TN5822   uses geändert
  27.03.13 wl                                        TN6045   uses geändert
  25.06.13 wl                                        TN6178   uses Identifier
  14.08.13 wl                                        TN6218   verwendet TRunStepListIterator
  15.08.13 wl                                        TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunTraceManager;


interface


uses
    TraceManager,
    IdentItemData,
    ProcessTraceDataCache,
    HeapDataCache,
    CallStackDataCache,
    ReferenceIdentValueMemoryDataCache,
    ActionData,
    Streamable,
    ActionIDDataCache,
    Identifier,
    MemoryClasses,
    ReferenceIdentValueMemory,
    RunStep,
    RunStepBlock,
    RestartPreparationStep,
    RunEffectData,
    RunEffect,
    TraceThreadDetails,
    TraceProcessDetails;

type

    TRunTraceManager = class(TTraceManager)
    private
        function PrepareRestartCurrentThread(): boolean;
        function CreateRunEffect(const aRunEffectData: TRunEffectData): TRunEffect;
        procedure UndoAction(const aProcessDetails: TProcessDetailsItem;
            const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData;
            const aActionSegmentIndex: integer; out oRemoveAction: boolean);
        procedure UndoActionRunEffects(const aProcessDetails: TProcessDetailsItem;
            const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData;
            const aRunEffects: TRunEffectListData);
        procedure ReadPreparationStepsForRunEffects(const aRunEffects: TRunEffectListData;
            const aResultPreparationStep: TRestartPreparationStepList);
        function CreateActionData(const aActionID: TActionID; const aStepName: string): TActionData;
    protected
        procedure AddStandardTypeInfos(); override;

    public
        class procedure CreateInstance();
        class function Instance(): TRunTraceManager;
        procedure AddProcessTraceForCurrentProcess(const aIsRestart: boolean);
        procedure PrepareProcessTraceForCurrentProcess();
        procedure AddThreadTraceForCurrentThread(const aIsMainThread: boolean);

        procedure CurrentProgramCounterChanged(const aProgramCounter: TRelativeMemAddress);
        procedure RemoveCurrentCallStackFrameForCurrentThread();
        procedure AddGlobalIdentToTrace(aIdent: TIdentifier);
        procedure AddLocalIdentToTrace(aIdent: TIdentifier);
        procedure AddReferenceIdentValueMemory(const aValue: TReferenceIdentValueMemory);
        procedure RemoveReferenceIdentValueMemory(const aValue: TReferenceIdentValueMemory);
        procedure ReferenceIdentValueMemoryChanged(const aValue: TReferenceIdentValueMemory);
        function GetTraceMainThreadAddress(const aProcessTraceName: string;
            const aResultRelativeMemAddress: TRelativeMemAddress): boolean;
        procedure AddIdentValueChangedRunEffect(aIdent: TIdentifier; const aOldValue: TStreamableItem;
            const aIsGlobalIdent: boolean);
        procedure AddCallStackFrameForCurrentThread(const aCallStackFrame: TCallStackFrame);
        procedure AddAction(const aStepName: string);
        procedure CurrentActionStarted(const aRunStep: TRunStep);
        procedure CurrentActionFinished();

        procedure AddBlock(const aBlock: TRunStepBlock);
        procedure RemoveBlock(const aBlock: TRunStepBlock);
        procedure AddStepsToBlock(const aBlock: TRunStepBlock; const aRunSteps: TRunStepList);

        procedure AddPendingSteps(const aRunSteps: TRunStepList);
        procedure ClearPendingSteps(const aPendingSteps: TRunStepList);
        procedure PendingStepsIteratorPosChanged(const aRunStepListIterator: TRunStepListLinearIterator);

        procedure AddPipDevice(const aPipDeviceName: string; const aTipCount: integer); override;
        procedure PipDeviceTipTypeChanged(const aPipDeviceName: string; const aTipIndex: integer;
            const aTipTypeName: string); override;
        procedure PipDeviceInitTipTypes(const aPipDeviceName: string;
            const aTipTypeNames: TArray<string>); override;

        procedure RackMoved(const aRackName, aOldCarrierName: string;
            const aOldCarrierSlot, aOldRotation: integer); override;
        procedure RackChangedID(const aRackName, aOldRackID: string); override;
        procedure RackTypeChanged(const aRackName: string; const aOldTypeName: string); override;
        procedure CarrierTypeChanged(const aCarrierName: string; const aOldTypeName: string); override;

        function GetMainThreadTraceNameForCurrentProcess(): string;
        function GetSecondaryThreadTraceNamesForCurrentProcess: TArray<string>;

        procedure UndoThreadTraceTillActionID(const aProcessTraceName: string; const aActionID: integer;
            const aActionSegmentIndex: integer);
        procedure ReadPreparationSteps(const aProcessTraceName: string; const aActionID: integer;
            const aResultPreparationStep: TRestartPreparationStepList);
    end;


implementation


uses
    SysUtils,
    TypeMapTranslator,
    ThreadAPI,
    ThreadClasses,
    RunStepBuilderProcessor,
    RelativeMemAddressData,
    ThreadListDataCache,
    ProcessInfoDataCache,
    ProgramCounterDataCache,
    RunStepBlockDataCache,
    PendingStepsDataCache,
    PipDeviceDataCache,
    RunTraceDataMaker,
    ActionDataCache,
    StandardRunEffects,
    StandardRunEffectData,
    LayoutRackTrace,
    LayoutCarrierTrace,
    PendingRunStepsTrace;

{ TRunTraceManager }
class procedure TRunTraceManager.CreateInstance();
begin
    SetInstance(TRunTraceManager.Create());
end;

class function TRunTraceManager.Instance(): TRunTraceManager;
begin
    result := inherited Instance() as TRunTraceManager;
end;

procedure TRunTraceManager.AddStandardTypeInfos();
begin
    // LayoutRacks
    fProcessDetailsDataCacheCreatorTypeInfoList.Add(TLayoutRackDataCacheCreatorTypeInfo.Create());
    fDataLoaderCreatorTypeInfoList.Add(TLayoutRackDataLoaderCreatorTypeInfo.Create());
    // LayoutRacks - RunEffects
    fRunEffectCreatorTypeInfoList.Add(TRackMoveRunEffectCreatorTypeInfo.Create());
    fRestartPreparationStepCreatorTypInfoList.Add(TRackMoveRestartPreparationStepCreatorTypeInfo.Create());

    fRunEffectCreatorTypeInfoList.Add(TRackChangeIDRunEffectCreatorTypeInfo.Create());

    fRunEffectCreatorTypeInfoList.Add(TRackTypeChangedRunEffectCreatorTypeInfo.Create());

    // LayoutCarriers
    fProcessDetailsDataCacheCreatorTypeInfoList.Add(TLayoutCarrierDataCacheCreatorTypeInfo.Create());
    fDataLoaderCreatorTypeInfoList.Add(TLayoutCarrierDataLoaderCreatorTypeInfo.Create());

    // LayoutCarriers - RunEffects
    fRunEffectCreatorTypeInfoList.Add(TCarrierTypeChangedRunEffectCreatorTypeInfo.Create());

    // PendingSteps
    fThreadDetailsDataCacheCreatorTypeInfoList.Add(TPendingStepDataCacheCreatorTypeInfo.Create());
    fDataLoaderCreatorTypeInfoList.Add(TPendingStepDataLoaderCreatorTypeInfo.Create());
    fRunEffectCreatorTypeInfoList.Add(TPendingRunStepsMoveCursorRunEffectCreatorTypeInfo.Create());
    fRunEffectCreatorTypeInfoList.Add(TPendingRunStepsClearRunEffectCreatorTypeInfo.Create());
    fRunEffectCreatorTypeInfoList.Add(TPendingRunStepsAddStepsRunEffectCreatorTypeInfo.Create());

end;

procedure TRunTraceManager.AddGlobalIdentToTrace(aIdent: TIdentifier);
var
    xHeapDataCache: THeapDataCache;
    xIdentData: TIdentItemData;
begin
    if not self.IsEnabled then
        EXIT;
    // we dont check IsTracingPaused here because changes to the entire process must always be traced...otherwise they will be out of sync

    xHeapDataCache := FindHeapDataCacheForCurrentProcess();
    xIdentData := xHeapDataCache.FindOrCreateIdent(aIdent.Key);
    TIdentItemDataMaker.SetIdentItemValueData(aIdent, xIdentData);
    xHeapDataCache.IdentChanged(xIdentData);
end;

procedure TRunTraceManager.AddLocalIdentToTrace(aIdent: TIdentifier);
var
    xCallStackDataCache: TCallStackDataCache;
    xIdentData: TIdentItemData;
    xThreadDetails: TThreadDetailsItem;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xCallStackDataCache := xThreadDetails.CallStackDataCache;
    xIdentData := xCallStackDataCache.FindOrCreateLocalIdent(aIdent.Key);
    TIdentItemDataMaker.SetIdentItemValueData(aIdent, xIdentData);
    xCallStackDataCache.LocalIdentChanged(xIdentData);
end;

procedure TRunTraceManager.CurrentProgramCounterChanged(const aProgramCounter: TRelativeMemAddress);
var
    xThreadDetails: TThreadDetailsItem;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;
    xThreadDetails.ProgramCounterDataCache.ChangeProgramCounter(aProgramCounter.LabelName,
        aProgramCounter.RelativeAddress);
end;

procedure TRunTraceManager.AddCallStackFrameForCurrentThread(const aCallStackFrame: TCallStackFrame);
var
    xThreadDetails: TThreadDetailsItem;
    xCallStackCache: TCallStackDataCache;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xCallStackCache := FindCallStackDataCacheForCurrentThread();
    xCallStackCache.AddCallStackFrameData(TCallStackDataMaker.CreateStackFrameData(aCallStackFrame));
    AddRunEffectDataToCurrentAction(TSubMethodCallRunEffectData.Create());
end;

procedure TRunTraceManager.RemoveCurrentCallStackFrameForCurrentThread();
var
    xCallStackCache: TCallStackDataCache;
    xThreadDetails: TThreadDetailsItem;
    xCurrentStackeFrame: TCallStackFrameData;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xCallStackCache := xThreadDetails.CallStackDataCache;
    xCurrentStackeFrame := xCallStackCache.CurrentCallStackFrameData;

    AddRunEffectDataToCurrentAction(TSubMethodEndRunEffectData.Create(xCurrentStackeFrame));
    xCallStackCache.RemoveLastCallStackFrameData();
end;

procedure TRunTraceManager.AddReferenceIdentValueMemory(const aValue: TReferenceIdentValueMemory);
var
    xDataCache: TReferenceIdentValueMemoryListDataCache;
    xData: TReferenceIdentValueMemoryData;
    xThreadDetails: TThreadDetailsItem;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xDataCache := FindReferenceIdentValueMemoryListDataCacheForCurrentProcess();
    xData := TReferenceIdentValueMemoryData.Create(aValue.ID);
    xDataCache.Add(xData);
    TReferenceIdentValueMemoryDataMaker.InitData(aValue, xData);
    xDataCache.DataChanged(xData);

    AddRunEffectDataToCurrentAction(TAllocateReferenceRunEffectData.Create(aValue.ID));

end;

procedure TRunTraceManager.RemoveReferenceIdentValueMemory(const aValue: TReferenceIdentValueMemory);
var
    xDataCache: TReferenceIdentValueMemoryListDataCache;
    xData: TReferenceIdentValueMemoryData;
    xThreadDetails: TThreadDetailsItem;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xDataCache := FindReferenceIdentValueMemoryListDataCacheForCurrentProcess();
    xData := xDataCache.Find(aValue.ID);
    ASSERT(Assigned(xData));
    AddRunEffectDataToCurrentAction(TDeallocateReferenceRunEffectData.Create(xData));

    xDataCache.Remove(aValue.ID);
end;

procedure TRunTraceManager.ReferenceIdentValueMemoryChanged(const aValue: TReferenceIdentValueMemory);
var
    xDataCache: TReferenceIdentValueMemoryListDataCache;
    xData: TReferenceIdentValueMemoryData;
    xThreadDetails: TThreadDetailsItem;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xDataCache := FindReferenceIdentValueMemoryListDataCacheForCurrentProcess();
    xData := xDataCache.Find(aValue.ID);
    TReferenceIdentValueMemoryDataMaker.SetData(aValue, xData);
    xDataCache.DataChanged(xData);
end;

procedure TRunTraceManager.AddBlock(const aBlock: TRunStepBlock);
var
    xThreadDetails: TThreadDetailsItem;
    xBlockData: TRunStepBlockData;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xBlockData := TRunStepBlockDataMaker.CreateBlockData(aBlock);
    xThreadDetails.BlockListDataCache.AddRunStepBlock(xBlockData);
    AddRunEffectDataToCurrentAction(TRunStepBlockAddRunEffectData.Create(xBlockData.BlockDataID));
end;

procedure TRunTraceManager.RemoveBlock(const aBlock: TRunStepBlock);
var
    xThreadDetails: TThreadDetailsItem;
    xBlockDataID: string;
    xBlockData: TRunStepBlockData;

begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xBlockDataID := TRunStepBlockDataMaker.GetBlockDataID(aBlock);
    xBlockData := xThreadDetails.BlockListDataCache.FindRunStepBlock(xBlockDataID);
    AddRunEffectDataToCurrentAction(TRunStepBlockRemoveRunEffectData.Create(xBlockData));
    xThreadDetails.BlockListDataCache.RemoveRunStepBlock(xBlockDataID);

end;

procedure TRunTraceManager.AddStepsToBlock(const aBlock: TRunStepBlock; const aRunSteps: TRunStepList);
var
    xThreadDetails: TThreadDetailsItem;
    xBlockDataID: string;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xBlockDataID := TRunStepBlockDataMaker.GetBlockDataID(aBlock);
    xThreadDetails.BlockListDataCache.AddStepsToRunStepBlock(xBlockDataID, aRunSteps);
    AddRunEffectDataToCurrentAction(TRunStepBlockAddStepRunEffectData.Create(xBlockDataID, aRunSteps.Count));
end;

procedure TRunTraceManager.AddPendingSteps(const aRunSteps: TRunStepList);
var
    xThreadDetails: TThreadDetailsItem;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    if aRunSteps.Count > 1 then
    begin
        // xThreadDetails.PendingStepsDataCache.AddSteps( aRunSteps );
        AddRunEffectDataToCurrentAction(TPendingRunStepsAddStepsRunEffectData.Create());
    end;
end;

procedure TRunTraceManager.ClearPendingSteps(const aPendingSteps: TRunStepList);
var
    xThreadDetails: TThreadDetailsItem;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    if aPendingSteps.Count > 1 then
        AddRunEffectDataToCurrentAction(TPendingRunStepsClearRunEffectData.Create(aPendingSteps));

end;

procedure TRunTraceManager.PendingStepsIteratorPosChanged(const aRunStepListIterator
    : TRunStepListLinearIterator);
var
    xThreadDetails: TThreadDetailsItem;
    xOldCursorPos, xNewCursorPos: integer;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;
    //
    xOldCursorPos := aRunStepListIterator.BookmarkedCursorPos;
    xNewCursorPos := aRunStepListIterator.DataCursor;
    // xThreadDetails.PendingStepsDataCache.SetCursorPos( xNewCursorPos );

    // if <= 1 : means normal/not-multistep action so dont keep track of it
    if (xNewCursorPos - xOldCursorPos) > 1 then
        AddRunEffectDataToCurrentAction(TPendingRunStepsMoveCursorRunEffectData.Create(xOldCursorPos,
            xNewCursorPos));
end;

procedure TRunTraceManager.AddPipDevice(const aPipDeviceName: string; const aTipCount: integer);
var
    xProcessDetails: TProcessDetailsItem;
begin
    if not self.IsEnabled then
        EXIT;
    xProcessDetails := self.FindProcessDetailsForCurrentProcess();
    if not Assigned(xProcessDetails) then
        EXIT;

    xProcessDetails.PipDeviceListDataCache.AddPipDevice(aPipDeviceName, aTipCount);
end;

procedure TRunTraceManager.PipDeviceInitTipTypes(const aPipDeviceName: string;
    const aTipTypeNames: TArray<string>);
var
    xProcessDetails: TProcessDetailsItem;
begin
    if not self.IsEnabled then
        EXIT;
    xProcessDetails := self.FindProcessDetailsForCurrentProcess();
    if not Assigned(xProcessDetails) then
        EXIT;
    xProcessDetails.PipDeviceListDataCache.PipDeviceInitTipTypes(aPipDeviceName, aTipTypeNames);
end;

procedure TRunTraceManager.PipDeviceTipTypeChanged(const aPipDeviceName: string; const aTipIndex: integer;
    const aTipTypeName: string);
var
    xProcessDetails: TProcessDetailsItem;
    xOldTipTypeName: string;
begin
    if not self.IsEnabled then
        EXIT;
    xProcessDetails := self.FindProcessDetailsForCurrentProcess();
    if not Assigned(xProcessDetails) then
        EXIT;
    xOldTipTypeName := xProcessDetails.PipDeviceListDataCache.PipDeviceGetTipTypeName(aPipDeviceName,
        aTipIndex);
    xProcessDetails.PipDeviceListDataCache.PipDeviceSetTipTypeName(aPipDeviceName, aTipIndex, aTipTypeName);

    AddRunEffectDataToCurrentAction(TPipDeviceTipTypeChangedRunEffectData.Create(aPipDeviceName, aTipIndex,
        xOldTipTypeName));
end;

procedure TRunTraceManager.AddIdentValueChangedRunEffect(aIdent: TIdentifier;
    const aOldValue: TStreamableItem; const aIsGlobalIdent: boolean);
var
    xOldIdentValueData: TStreamableItem;
begin
    if not self.IsEnabled then
        EXIT;

    xOldIdentValueData := TIdentItemDataMaker.CreateIdentValueData(aOldValue);
    try
        AddRunEffectDataToCurrentAction(TIdentValueChangedRunEffectData.Create(aIdent.Key, xOldIdentValueData,
            aIsGlobalIdent));
    finally
        FreeAndNil(xOldIdentValueData);
    end;
end;

procedure TRunTraceManager.RackMoved(const aRackName, aOldCarrierName: string;
    const aOldCarrierSlot, aOldRotation: integer);
begin
    if not self.IsEnabled then
        EXIT;
    self.AddRunEffectDataToCurrentAction(TRackMoveRunEffectData.Create(aRackName, aOldCarrierName,
        aOldCarrierSlot, aOldRotation));
end;

procedure TRunTraceManager.RackChangedID(const aRackName, aOldRackID: string);
begin
    if not self.IsEnabled then
        EXIT;
    self.AddRunEffectDataToCurrentAction(TRackChangeIDRunEffectData.Create(aRackName, aOldRackID));
end;

procedure TRunTraceManager.RackTypeChanged(const aRackName: string; const aOldTypeName: string);
begin
    if not self.IsEnabled then
        EXIT;
    self.AddRunEffectDataToCurrentAction(TRackTypeChangedRunEffectData.Create(aRackName, aOldTypeName));
end;

procedure TRunTraceManager.CarrierTypeChanged(const aCarrierName: string; const aOldTypeName: string);
begin
    if not self.IsEnabled then
        EXIT;
    self.AddRunEffectDataToCurrentAction(TCarrierTypeChangedRunEffectData.Create(aCarrierName, aOldTypeName));
end;

function TRunTraceManager.CreateActionData(const aActionID: TActionID; const aStepName: string): TActionData;
var
    xTypeInfo: TActionDataCreatorTypeInfo;
begin
    xTypeInfo := fActionDataCreatorTypeDictionary.FindBySupportsData(aStepName);
    result := xTypeInfo.CreateActionData(aActionID, aStepName);
end;

procedure TRunTraceManager.AddAction(const aStepName: string);
var
    xActionListDataCache: TActionListDataCache;
    xActionID: TActionID;
    xActionData: TActionData;
    xThreadDetails: TThreadDetailsItem;
    xProcessDetails: TProcessDetailsItem;

begin
    if not self.IsEnabled then
        EXIT;
    xProcessDetails := self.FindProcessDetailsForCurrentProcess();
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xActionListDataCache := xThreadDetails.ActionListDataCache;
    xActionID := xProcessDetails.ActionIDDataCache.GenerateNextActionID();

    xActionData := CreateActionData(xActionID, aStepName);
    // we assume here that the ProgramCounterDataCache has already been updated and has been set correctly to the current address
    xActionData.Address.LabelName := xThreadDetails.ProgramCounterDataCache.ProgramCounterData.LabelName;
    xActionData.Address.RelativeAddress := xThreadDetails.ProgramCounterDataCache.ProgramCounterData.
        RelativeAddress;
    xActionListDataCache.Add(xActionData);
end;

procedure TRunTraceManager.CurrentActionStarted(const aRunStep: TRunStep);
var
    xThreadDetails: TThreadDetailsItem;
    xRunStep: TRunStep;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xRunStep := TObjectCopy<TRunStep>.Copy(aRunStep);
    xThreadDetails.ActionListDataCache.ChangedStarted(Now(), xRunStep);
end;

procedure TRunTraceManager.CurrentActionFinished();
var
    xThreadDetails: TThreadDetailsItem;
begin
    if not self.IsEnabled then
        EXIT;
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
        EXIT;

    xThreadDetails.ActionListDataCache.ChangedFinished(Now());
end;

function TRunTraceManager.GetTraceMainThreadAddress(const aProcessTraceName: string;
    const aResultRelativeMemAddress: TRelativeMemAddress): boolean;
var
    xProcessTracePath, xThreadListPathName: string;
    xThreadListDataCache: TThreadListDataCache;
    xMainThreadData: TThreadData;
    xMainThreadTraceName: string;
    xThreadTracePath, xProgramCounterPathName: string;
    xProgramCounterDataCache: TProgramCounterDataCache;
begin
    result := false;
    xProcessTracePath := GetTracePathForProcess(aProcessTraceName);
    // Get Tracename of main thread
    xThreadListPathName := GetTracePathForProcessThreadList(xProcessTracePath);
    xThreadListDataCache := TThreadListDataCache.Create(xThreadListPathName);
    try
        if (not xThreadListDataCache.Read()) or (xThreadListDataCache.Count = 0) then
            EXIT;
        xMainThreadData := xThreadListDataCache.MainThread;
        xMainThreadTraceName := xMainThreadData.TraceName;
    finally
        xThreadListDataCache.Free;
    end;

    // Get exit address of main thread
    xThreadTracePath := GetTracePathForThread(aProcessTraceName, xMainThreadTraceName);
    xProgramCounterPathName := GetTracePathForThreadProgramCounter(xThreadTracePath);
    xProgramCounterDataCache := TProgramCounterDataCache.Create(xProgramCounterPathName);
    try
        if not xProgramCounterDataCache.Read() then
            EXIT;
        aResultRelativeMemAddress.LabelName := xProgramCounterDataCache.ProgramCounterData.LabelName;
        aResultRelativeMemAddress.RelativeAddress :=
            xProgramCounterDataCache.ProgramCounterData.RelativeAddress;
    finally
        xProgramCounterDataCache.Free;
    end;

    result := true;
end;

procedure TRunTraceManager.AddProcessTraceForCurrentProcess(const aIsRestart: boolean);
var
    xTraceName: string;
    xProcessTraceData: TProcessTraceData;
    xProcessInfoDataCache: TProcessInfoDataCache;
    xProcessDetails: TProcessDetailsItem;
    xSourceDataName: string;
begin

    if not self.IsEnabled then
        EXIT;
    xTraceName := GetTraceNameForCurrentProcess();

    if aIsRestart then
    begin
        xProcessTraceData := fProcessTraceListDataCache.FindProcessTrace(xTraceName);
        ASSERT(Assigned(xProcessTraceData));
        xProcessDetails := fProcessDetailsManager.Find(xTraceName);
        if xProcessDetails = nil then
        begin
            AddProcessDetails(xTraceName);
            xProcessDetails := fProcessDetailsManager.Find(xTraceName);
        end;
        ASSERT(Assigned(xProcessDetails));
        xProcessInfoDataCache := xProcessDetails.ProcessInfoDataCache;
        xProcessInfoDataCache.Read();
        xProcessInfoDataCache.ProcessInfoData.IsRestart := aIsRestart;
    end
    else
    begin
        AddProcessTrace(TProcessTraceData.Create(xTraceName));
        AddProcessDetails(xTraceName);
        xSourceDataName := TThreadAPI.GetCurrentSourceDataName();
        xProcessDetails := fProcessDetailsManager.Find(xTraceName);
        xProcessDetails.ProcessInfoDataCache.Init(xTraceName, xSourceDataName, false, Now());
    end;
end;

procedure TRunTraceManager.PrepareProcessTraceForCurrentProcess();
var
    xTraceName: string;
    xPath: string;

    xHeap: TThreadSafeIdentifierList;
    xHeapDataCache: THeapDataCache;
    xHeapInitNeeded: boolean;
    xThreadListDataCache: TThreadListDataCache;
    xThreadListInitNeeded: boolean;
    xReferenceIdentValueMemoryListDataCache: TReferenceIdentValueMemoryListDataCache;
    xReferenceIdentValueMemoryListInitNeeded: boolean;
    xProcessInfoDataCache: TProcessInfoDataCache;
    xActionIDDataCache: TActionIDDataCache;
    xActionIDInitNeeded: boolean;
    xPipDeviceListDataCache: TPipDeviceListDataCache;
    xPipDeviceListInitNeeded: boolean;

    xReadOK: boolean;
    xProcessDetails: TProcessDetailsItem;
    xIsRestart: boolean;
begin

    if not self.IsEnabled then
        EXIT;
    xTraceName := GetTraceNameForCurrentProcess();
    xHeapInitNeeded := true;
    xThreadListInitNeeded := true;
    xReferenceIdentValueMemoryListInitNeeded := true;
    xActionIDInitNeeded := true;
    xPipDeviceListInitNeeded := true;

    xProcessDetails := fProcessDetailsManager.Find(xTraceName);
    xProcessInfoDataCache := xProcessDetails.ProcessInfoDataCache;
    xIsRestart := xProcessInfoDataCache.ProcessInfoData.IsRestart;

    // if Assigned( xProcessTraceData ) then begin
    // xProcessInfoDataCache := xProcessDetails.ProcessInfoDataCache;
    // xReadOK := xProcessInfoDataCache.Read();
    // if xReadOK then begin
    // xProcessInfoInitNeeded := false;
    // xIsRestart := xProcessInfoDataCache.ProcessInfoData.IsRestart;
    // end;
    // end;

    if not xIsRestart then
    begin

        xPath := GetTracePathForCurrentProcess();
        // AddProcessDetails( xTraceName );

        xProcessInfoDataCache := xProcessDetails.ProcessInfoDataCache;
        xHeapDataCache := xProcessDetails.HeapDataCache;
        xThreadListDataCache := xProcessDetails.ThreadListDataCache;
        xReferenceIdentValueMemoryListDataCache := xProcessDetails.ReferenceIdentValueMemoryListDataCache;
        xActionIDDataCache := xProcessDetails.ActionIDDataCache;
        xPipDeviceListDataCache := xProcessDetails.PipDeviceListDataCache;
    end
    else
    begin

        // Load Heap
        xHeap := TThreadAPI.GetCurrentProcess.AddressSpace.Heap;
        xHeapDataCache := xProcessDetails.HeapDataCache;
        xReadOK := xHeapDataCache.Read();
        if xReadOK then
        begin
            xHeapInitNeeded := false;
            THeapDataMaker.LoadHeapDataToHeap(xHeap, xHeapDataCache.HeapData);
        end;

        // Load ThreadList
        xThreadListDataCache := xProcessDetails.ThreadListDataCache;
        xReadOK := xThreadListDataCache.Read();
        if xReadOK then
        begin
            xThreadListInitNeeded := false;
        end;

        // Load ReferenceIdentValueMemroy
        xReferenceIdentValueMemoryListDataCache := xProcessDetails.ReferenceIdentValueMemoryListDataCache;
        xReadOK := xReferenceIdentValueMemoryListDataCache.Read();
        if xReadOK then
        begin
            xReferenceIdentValueMemoryListInitNeeded := false;
            TReferenceIdentValueMemoryDataMaker.LoadListDataToReferenceIdentValueMemoryManager
                (xReferenceIdentValueMemoryListDataCache.ReferenceIdentValueMemoryListData);
        end;

        // Load ActionID
        xActionIDDataCache := xProcessDetails.ActionIDDataCache;
        xReadOK := xActionIDDataCache.Read();
        if xReadOK then
        begin
            xActionIDInitNeeded := false;
        end;

        // Load PipDeviceList
        xPipDeviceListDataCache := xProcessDetails.PipDeviceListDataCache;
        xReadOK := xPipDeviceListDataCache.Read();
        if xReadOK then
        begin
            xPipDeviceListInitNeeded := false;
            // TPipDeviceDataMaker.LoadTipTypes( xPipDeviceListDataCache.ListData );
        end;
    end;

    if xHeapInitNeeded then
    begin
        xHeapDataCache.Init;
    end;

    if xThreadListInitNeeded then
    begin
        xThreadListDataCache.Init;
    end;

    if xReferenceIdentValueMemoryListInitNeeded then
    begin
        xReferenceIdentValueMemoryListDataCache.Init;
    end;

    if xActionIDInitNeeded then
    begin
        xActionIDDataCache.Init;
    end;

    if xPipDeviceListInitNeeded then
    begin
        xPipDeviceListDataCache.Init;
    end;

    if xIsRestart then
        xProcessDetails.LoadDataToRun()
    else
        xProcessDetails.InitProcessDetails();

    xProcessInfoDataCache.ChangedDateLastStarted(Now());
end;

function TRunTraceManager.PrepareRestartCurrentThread(): boolean;
var
    xThreadDetails: TThreadDetailsItem;
    xCallStackCache: TCallStackDataCache;
    xThreadImage: TThreadImage;
    xCallStack: TCallStack;
    xFileReadOK: boolean;
    xProgramCounterDataCache: TProgramCounterDataCache;
    xActionListDataCache: TActionListDataCache;
    xBlockDataCache: TRunStepBlockListDataCache;
    xCallStackInitNeeded, xProgramCounterInitNeeded, xActionListInitNeeded, xBlockInitNeeded: boolean;
begin
    result := false;

    // read Run file into cache
    xThreadDetails := FindThreadDetailsForCurrentThread();
    xCallStackCache := xThreadDetails.CallStackDataCache;
    xProgramCounterDataCache := xThreadDetails.ProgramCounterDataCache;
    xActionListDataCache := xThreadDetails.ActionListDataCache;
    xBlockDataCache := xThreadDetails.BlockListDataCache;
    // xPendingStepsDataCache := xThreadDetails.PendingStepsDataCache;

    xCallStackInitNeeded := true;
    xProgramCounterInitNeeded := true;
    xActionListInitNeeded := true;
    xBlockInitNeeded := true;

    if IsCurrentProcessRestart then
    begin
        xThreadImage := TThreadAPI.GetCurrentThreadImage();

        xFileReadOK := xCallStackCache.Read();
        if xFileReadOK then
        begin
            xCallStackInitNeeded := false;
            // Load the data into call stack
            xCallStack := xThreadImage.CallStack;
            TCallStackDataMaker.SetCallStack(xCallStackCache.CallStackData, xCallStack);
        end;

        xFileReadOK := xProgramCounterDataCache.Read();
        if xFileReadOK then
        begin
            xProgramCounterInitNeeded := false;
            // Load the Program counter data into the Program Counter
            TRunStepBuilderProcessor.Jump(xProgramCounterDataCache.ProgramCounterData.LabelName,
                xProgramCounterDataCache.ProgramCounterData.RelativeAddress);
        end;

        xFileReadOK := xActionListDataCache.Read();
        if xFileReadOK then
        begin
            xActionListInitNeeded := false;
        end;

        xFileReadOK := xBlockDataCache.Read();
        if xFileReadOK then
        begin
            xBlockInitNeeded := false;
            TRunStepBlockDataMaker.LoadBlockListDataToBlockManager(xBlockDataCache.RunStepBlocks);
        end;

        // xFileReadOK := xPendingStepsDataCache.Read();
        // if xFileReadOK then begin
        // xPendingStepsInitNeeded := false;
        // //TPendingStepsDataMaker.LoadPendingStepsDataToPendingSteps( xPendingStepsDataCache.PendingStepsData );
        // end;
    end;

    if xCallStackInitNeeded then
        xCallStackCache.Init();

    if xProgramCounterInitNeeded then
        xProgramCounterDataCache.Init();

    if xActionListInitNeeded then
        xActionListDataCache.Init();

    if xBlockInitNeeded then
        xBlockDataCache.Init();

    if IsCurrentProcessRestart then
        xThreadDetails.LoadDataToRun()
    else
        xThreadDetails.InitData();
end;

procedure TRunTraceManager.AddThreadTraceForCurrentThread(const aIsMainThread: boolean);
var
    xTraceName: string;
    xThreadListDataCache: TThreadListDataCache;
begin
    if not self.IsEnabled then
        EXIT;
    xTraceName := GetTraceNameForCurrentThread();
    xThreadListDataCache := self.FindThreadListDataCacheForCurrentProcess();
    if not Assigned(xThreadListDataCache.FindThread(xTraceName)) then
    begin
        xThreadListDataCache.AddThread(TThreadData.Create(xTraceName, aIsMainThread));
    end;

    AddThreadDetails(xTraceName);

    PrepareRestartCurrentThread();
end;

function TRunTraceManager.CreateRunEffect(const aRunEffectData: TRunEffectData): TRunEffect;
var
    xTypeInfo: TRunEffectCreatorTypeInfo;
begin

    result := nil;

    xTypeInfo := fRunEffectCreatorTypeInfoList.FindBySupportsData(aRunEffectData);
    if Assigned(xTypeInfo) then
    begin
        result := xTypeInfo.CreateRunEffect(aRunEffectData);
    end;

    if aRunEffectData is TIdentValueChangedRunEffectData then
    begin
        result := TIdentValueChangedRunEffect.Create(aRunEffectData as TIdentValueChangedRunEffectData);
    end
    else if aRunEffectData is TSubMethodCallRunEffectData then
    begin
        result := TSubMethodCallRunEffect.Create(aRunEffectData as TSubMethodCallRunEffectData);
    end
    else if aRunEffectData is TSubMethodEndRunEffectData then
    begin
        result := TSubMethodEndRunEffect.Create(aRunEffectData as TSubMethodEndRunEffectData);
    end
    else if aRunEffectData is TAllocateReferenceRunEffectData then
    begin
        result := TAllocateReferenceRunEffect.Create(aRunEffectData as TAllocateReferenceRunEffectData);
    end
    else if aRunEffectData is TDeallocateReferenceRunEffectData then
    begin
        result := TDeallocateReferenceRunEffect.Create(aRunEffectData as TDeallocateReferenceRunEffectData);
    end
    else if aRunEffectData is TDatasetCursorMoveRunEffectData then
    begin
        result := TDatasetCursorMoveRunEffect.Create(aRunEffectData as TDatasetCursorMoveRunEffectData);
    end

    else if aRunEffectData is TRunStepBlockAddRunEffectData then
    begin
        result := TRunStepBlockAddRunEffect.Create(aRunEffectData as TRunStepBlockAddRunEffectData);
    end
    else if aRunEffectData is TRunStepBlockRemoveRunEffectData then
    begin
        result := TRunStepBlockRemoveRunEffect.Create(aRunEffectData as TRunStepBlockRemoveRunEffectData);
    end
    else if aRunEffectData is TRunStepBlockAddStepRunEffectData then
    begin
        result := TRunStepBlockAddStepRunEffect.Create(aRunEffectData as TRunStepBlockAddStepRunEffectData);
    end
    else if aRunEffectData is TPipDeviceTipTypeChangedRunEffectData then
    begin
        result := TPipDeviceTipTypeChangedRunEffect.Create
            (aRunEffectData as TPipDeviceTipTypeChangedRunEffectData);
    end;

end;

procedure TRunTraceManager.UndoActionRunEffects(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData;
    const aRunEffects: TRunEffectListData);
var
    xRunEffectData: TRunEffectData;
    xRunEffect: TRunEffect;
    x: integer;
begin
    for x := aRunEffects.Count - 1 downto 0 do
    begin
        xRunEffectData := aRunEffects[x];
        xRunEffect := CreateRunEffect(xRunEffectData);
        ASSERT(Assigned(xRunEffect));
        try
            xRunEffect.Undo(aProcessDetails, aThreadDetails, aActionData);
        except
            on E: exception do
            begin
                raise Exception.CreateFmt('Error undoing side effect for Method %s, Line %d - %s',
                    [aActionData.Address.LabelName, aActionData.Address.RelativeAddress, E.Message]);
            end;
        end;
    end;
end;

procedure TRunTraceManager.UndoAction(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData;
    const aActionSegmentIndex: integer; out oRemoveAction: boolean);
var
    xRunEffects: TRunEffectListData;
    xActionSegment: TActionRunEffectSegment;
    x: integer;
begin
    oRemoveAction := false;

    for x := aActionData.ActionSegmentCount - 1 downto 0 do
    begin
        xActionSegment := aActionData.ActionSegments[x];
        xRunEffects := xActionSegment.RunEffects;
        UndoActionRunEffects(aProcessDetails, aThreadDetails, aActionData, xRunEffects);
        aActionData.RemoveActionSegment(xActionSegment);
        if x = aActionSegmentIndex then
            BREAK;
    end;

    if aActionSegmentIndex = -1 then
    begin
        UndoActionRunEffects(aProcessDetails, aThreadDetails, aActionData,
            aActionData.PrepareSegment.RunEffects);
        aActionData.ClearPrepareSegment();
        oRemoveAction := true;
    end;

    // aActionData.Undone := true;
end;

procedure TRunTraceManager.UndoThreadTraceTillActionID(const aProcessTraceName: string;
    const aActionID: integer; const aActionSegmentIndex: integer);
var
    xActionListDataCache: TActionListDataCache;
    xProcessDetails: TProcessDetailsItem;
    xThreadDetails: TThreadDetailsItem;
    xActionData: TActionData;
    xActionIterator: TActionListDataCacheIterator;
    xThreadTraceName: string;
    xActionSegmentIndex: integer;
    xUndoComplete: boolean;
    xRemoveAction: boolean;
    xAddress: TRelativeMemAddressData;
begin
    // if we want to continue at next step just exit
    if aActionID = -1 then
        EXIT;

    xProcessDetails := CreateProcessDetailsForProcessTrace(1, aProcessTraceName);
    xProcessDetails.ThreadListDataCache.Read();
    xProcessDetails.HeapDataCache.Read;
    xProcessDetails.ReferenceIdentValueMemoryListDataCache.Read();
    xProcessDetails.PipDeviceListDataCache.Read();
    xProcessDetails.ReadProcessDetails();
    xThreadTraceName := xProcessDetails.ThreadListDataCache.MainThread.TraceName;

    xThreadDetails := CreateThreadDetailsForThreadTrace(aProcessTraceName, xThreadTraceName);
    xThreadDetails.CallStackDataCache.Read();
    xThreadDetails.BlockListDataCache.Read();
    // xThreadDetails.PendingStepsDataCache.Read();
    xThreadDetails.ProgramCounterDataCache.Read();
    xThreadDetails.ReadData();
    xActionListDataCache := xThreadDetails.ActionListDataCache;

    xActionListDataCache.Read;
    xActionIterator := xActionListDataCache.CreateBackwardIterator();

    xUndoComplete := false;

    xAddress := nil;

    while xActionIterator.MoveNext do
    begin
        xActionData := xActionIterator.Current;
        // if xActionData.Undone then begin
        // CONTINUE;
        // end;

        xActionSegmentIndex := -1;
        if xActionData.ActionID = aActionID then
        begin
            xUndoComplete := true;
            xAddress := TObjectCopy<TRelativeMemAddressData>.Copy(xActionData.Address);
            xActionSegmentIndex := aActionSegmentIndex;
        end;

        UndoAction(xProcessDetails, xThreadDetails, xActionData, xActionSegmentIndex, xRemoveAction);
        xActionListDataCache.ActionDataChanged();

        if xRemoveAction then
        begin
            xActionListDataCache.Remove(xActionData);
        end;

        if xUndoComplete then
            BREAK;
    end;

    ASSERT(Assigned(xAddress));
    xThreadDetails.ProgramCounterDataCache.ChangeProgramCounter(xAddress.LabelName,
        xAddress.RelativeAddress - 1);

    xThreadDetails.Flush();
    xProcessDetails.ForceFlushProcessDetails();

end;

procedure TRunTraceManager.ReadPreparationStepsForRunEffects(const aRunEffects: TRunEffectListData;
    const aResultPreparationStep: TRestartPreparationStepList);
var
    xTypeInfo: TRestartPreparationStepCreatorTypeInfo;
    j: integer;
    xRunEffectData: TRunEffectData;
begin
    for j := aRunEffects.Count - 1 downto 0 do
    begin
        xRunEffectData := aRunEffects[j];
        xTypeInfo := fRestartPreparationStepCreatorTypInfoList.FindBySupportsData(xRunEffectData);
        if not Assigned(xTypeInfo) then
            CONTINUE;

        xTypeInfo.CreateAndAddRestartPreparationSteps(aResultPreparationStep, xRunEffectData);
    end;
end;

procedure TRunTraceManager.ReadPreparationSteps(const aProcessTraceName: string; const aActionID: integer;
    const aResultPreparationStep: TRestartPreparationStepList);
var
    xActionListDataCache: TActionListDataCache;
    xThreadDetails: TThreadDetailsItem;
    xProcessDetails: TProcessDetailsItem;
    xActionData: TActionData;
    x: integer;
    xActionIterator: TActionListDataCacheIterator;
    xThreadTraceName: string;
    xRunEffects: TRunEffectListData;
begin
    // if we want to continue at next step just exit
    if aActionID = -1 then
        EXIT;

    xProcessDetails := CreateProcessDetailsForProcessTrace(1, aProcessTraceName);
    xProcessDetails.ThreadListDataCache.Read();
    xThreadTraceName := xProcessDetails.ThreadListDataCache.MainThread.TraceName;

    xThreadDetails := CreateThreadDetailsForThreadTrace(aProcessTraceName, xThreadTraceName);

    xActionListDataCache := xThreadDetails.ActionListDataCache;

    xActionListDataCache.Read;
    xActionIterator := xActionListDataCache.CreateBackwardIterator();
    // ASSERT( xActionListDataCache.ReadAll( xActions ) );
    while xActionIterator.MoveNext do
    begin
        xActionData := xActionIterator.Current;

        // if xActionData.Undone then begin
        // CONTINUE;
        // end;

        for x := xActionData.ActionSegmentCount - 1 downto 0 do
        begin
            xRunEffects := xActionData.ActionSegments[x].RunEffects;
            self.ReadPreparationStepsForRunEffects(xRunEffects, aResultPreparationStep);
        end;
        self.ReadPreparationStepsForRunEffects(xActionData.PrepareSegment.RunEffects, aResultPreparationStep);

        if xActionData.ActionID = aActionID then
            BREAK;
    end;
end;

function TRunTraceManager.GetSecondaryThreadTraceNamesForCurrentProcess: TArray<string>;
var
    xProcessDetails: TProcessDetailsItem;
begin
    xProcessDetails := self.FindProcessDetailsForCurrentProcess();

    result := xProcessDetails.ThreadListDataCache.GetTraceNamesForSecondaryThreads();
end;

function TRunTraceManager.GetMainThreadTraceNameForCurrentProcess(): string;
var
    xProcessDetails: TProcessDetailsItem;
begin
    xProcessDetails := self.FindProcessDetailsForCurrentProcess();
    result := xProcessDetails.ThreadListDataCache.MainThread.TraceName
end;


end.
