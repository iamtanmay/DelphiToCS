{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  07.06.10 pk                                        TN5077     initial revision
  21.10.10 pk  DoStoreDataFromRun                    TN5297     Move iterator to first position before copying steps
  26.10.10 pk  TPendingRunStepsAddStepsRunEffect     TN5297     DoUndo. Always clear steps
  26.10.10 pk  MoveCursorToSafePosition              TN5297     New:  Move cursor so that we dont restart with dispense steps
  14.08.13 wl                                TN6218   verwendet TRunStepListIterator
  15.08.13 wl                                TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit PendingRunStepsTrace;


interface


uses
    GeneralTypes,
    TraceDataCache,
    ProcessTraceDataCache,
    ProcessInfoDataCache,
    HeapDataCache,
    ThreadListDataCache,
    ReferenceIdentValueMemoryDataCache,
    ActionIDDataCache,
    ThreadClasses,
    TraceThreadDetails,
    TraceProcessDetails,
    PendingStepsDataCache,
    RunEffect,
    RunEffectData,
    StandardRunEffectData,
    ActionData;

type
    TPendingStepDataCacheCreatorTypeInfo = class(TTraceDataCacheCreatorTypeInfo)
    public
        function CreateDataCache(): TTraceDataCache; override;
    end;

    TPendingStepListDataLoader = class(TTraceDataLoader)
    private
        procedure MoveCursorToSafePosition(const aTraceDataCache: TPendingStepsDataCache);
    protected
        procedure DoLoadDataToRun(); override;
        procedure DoStoreDataFromRun(); override;
    end;

    TPendingStepDataLoaderCreatorTypeInfo = class(TTraceDataLoaderCreatorTypeInfo)
    protected
        function DoSupportsDataCache(const aDataCache: TTraceDataCache): boolean; override;
        function DoCreateDataLoader(const aDataCache: TTraceDataCache): TTraceDataLoader; override;
    end;

    TPendingRunStepsMoveCursorRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TPendingRunStepsMoveCursorRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TPendingRunStepsMoveCursorRunEffectData read GetRunEffectData;
    end;

    TPendingRunStepsMoveCursorRunEffectCreatorTypeInfo = class(TRunEffectCreatorTypeInfo)
    protected
        function DoCreateRunEffect(const aRunEffectData: TRunEffectData): TRunEffect; override;
        function DoSupportsData(const aRunEffectData: TRunEffectData): boolean; override;
    end;

    TPendingRunStepsClearRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TPendingRunStepsClearRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TPendingRunStepsClearRunEffectData read GetRunEffectData;
    end;

    TPendingRunStepsClearRunEffectCreatorTypeInfo = class(TRunEffectCreatorTypeInfo)
    protected
        function DoCreateRunEffect(const aRunEffectData: TRunEffectData): TRunEffect; override;
        function DoSupportsData(const aRunEffectData: TRunEffectData): boolean; override;
    end;

    TPendingRunStepsAddStepsRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TPendingRunStepsAddStepsRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
        // procedure DoUndoTill(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
        // const aActionData: TActionData; const aUndoTillCusroPos : TRunEffectUndoCursorPos ); override;
    public
        property RunEffectData: TPendingRunStepsAddStepsRunEffectData read GetRunEffectData;
    end;

    TPendingRunStepsAddStepsRunEffectCreatorTypeInfo = class(TRunEffectCreatorTypeInfo)
    protected
        function DoCreateRunEffect(const aRunEffectData: TRunEffectData): TRunEffect; override;
        function DoSupportsData(const aRunEffectData: TRunEffectData): boolean; override;
    end;


implementation


uses
    SysUtils,
    ThreadAPI,
    EventManager,
    RunStepBuilderProcessor,
    RunStep,
    TypeMapTranslator;

{ TPendingStepListDataLoader }

procedure TPendingStepListDataLoader.MoveCursorToSafePosition(const aTraceDataCache: TPendingStepsDataCache);
var
    xStep: TRunStep;
begin
    while true do
    begin
        if aTraceDataCache.PendingStepsData.CursorPos >= (aTraceDataCache.PendingStepsData.Steps.Count) then
            BREAK;
        xStep := aTraceDataCache.PendingStepsData.Steps[aTraceDataCache.PendingStepsData.CursorPos];
        if xStep.RestartAtStepAllowed then
            BREAK;
        aTraceDataCache.PendingStepsData.CursorPos := aTraceDataCache.PendingStepsData.CursorPos + 1;
    end;
end;

procedure TPendingStepListDataLoader.DoLoadDataToRun;
var
    xTraceDataCache: TPendingStepsDataCache;
    xSteps: TRunStepList;
    xThreadImage: TThreadImage;

begin
    xThreadImage := TThreadAPI.GetCurrentThreadImage();
    if not Assigned(xThreadImage.TCB.Processor) then
        EXIT;

    xTraceDataCache := fDataCache as TPendingStepsDataCache;
    // move the cursor forward so actions such as disp steps dont run when dilutors are empty
    MoveCursorToSafePosition(xTraceDataCache);

    xSteps := TObjectCopy<TRunStepList>.Copy(xTraceDataCache.PendingStepsData.Steps);
    (xThreadImage.TCB.Processor as TRunStepBuilderProcessor).AddStepsToPendingSteps(xSteps, false);

    if xTraceDataCache.PendingStepsData.Steps.Count > 0 then
        (xThreadImage.TCB.Processor as TRunStepBuilderProcessor)
            .PendingStepsMoveCursor(xTraceDataCache.PendingStepsData.CursorPos);

    xTraceDataCache.ClearSteps();
end;

procedure TPendingStepListDataLoader.DoStoreDataFromRun;
var
    xThreadImage: TThreadImage;
    xTraceDataCache: TPendingStepsDataCache;
    xProcRunStepIterator: TRunStepListLinearIterator;
    xTempRunStepIterator: TRunStepListLinearIterator;
    xRunStep: TRunStep;
begin
    xTraceDataCache := fDataCache as TPendingStepsDataCache;

    xThreadImage := TThreadAPI.GetCurrentThreadImage();
    if not Assigned(xThreadImage.TCB.Processor) then
        EXIT;

    xProcRunStepIterator := (xThreadImage.TCB.Processor as TRunStepBuilderProcessor).PendingRunStepsIterator;
    xTraceDataCache.PendingStepsData.CursorPos := xProcRunStepIterator.DataCursor;

    xTempRunStepIterator := TRunStepListLinearIterator.Create(xProcRunStepIterator.CompositeRunStep);
    try
        xTempRunStepIterator.MoveFirst;
        while not xTempRunStepIterator.IsEOF do
        begin
            xRunStep := xTempRunStepIterator.CurrentStep;
            xTraceDataCache.AddStep(TObjectCopy<TRunStep>.Copy(xRunStep));
            xTempRunStepIterator.MoveNext;
        end;
    finally
        FreeAndNil(xTempRunStepIterator);
    end;
end;

{ TPendingStepDataLoaderCreatorTypeInfo }

function TPendingStepDataLoaderCreatorTypeInfo.DoCreateDataLoader(const aDataCache: TTraceDataCache)
    : TTraceDataLoader;
begin
    result := TPendingStepListDataLoader.Create(aDataCache);
end;

function TPendingStepDataLoaderCreatorTypeInfo.DoSupportsDataCache(const aDataCache: TTraceDataCache)
    : boolean;
begin
    result := aDataCache is TPendingStepsDataCache;
end;

{ TPendingRunStepsMoveCursorRunEffect }

procedure TPendingRunStepsMoveCursorRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xDataCache: TPendingStepsDataCache;
begin
    xDataCache := aThreadDetails.FindDataCache(cPendingStepsTypeID) as TPendingStepsDataCache;

    xDataCache.SetCursorPos(self.RunEffectData.OldCursorPos);
end;

function TPendingRunStepsMoveCursorRunEffect.GetRunEffectData: TPendingRunStepsMoveCursorRunEffectData;
begin
    result := fRunEffectData as TPendingRunStepsMoveCursorRunEffectData;
end;

{ TPendingRunStepsMoveCursorRunEffectCreatorTypeInfo }

function TPendingRunStepsMoveCursorRunEffectCreatorTypeInfo.DoCreateRunEffect(const aRunEffectData
    : TRunEffectData): TRunEffect;
begin
    result := TPendingRunStepsMoveCursorRunEffect.Create(aRunEffectData);
end;

function TPendingRunStepsMoveCursorRunEffectCreatorTypeInfo.DoSupportsData(const aRunEffectData
    : TRunEffectData): boolean;
begin
    result := (aRunEffectData is TPendingRunStepsMoveCursorRunEffectData);
end;

{ TPendingRunStepsClearRunEffect }

procedure TPendingRunStepsClearRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xOldSteps: TRunStepList;
    xDataCache: TPendingStepsDataCache;
begin
    xDataCache := aThreadDetails.FindDataCache(cPendingStepsTypeID) as TPendingStepsDataCache;

    xOldSteps := TObjectCopy<TRunStepList>.Copy(self.RunEffectData.OldSteps);
    xDataCache.AddSteps(xOldSteps);
end;

function TPendingRunStepsClearRunEffect.GetRunEffectData: TPendingRunStepsClearRunEffectData;
begin
    result := fRunEffectData as TPendingRunStepsClearRunEffectData;
end;

procedure TPendingRunStepsAddStepsRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xDataCache: TPendingStepsDataCache;
begin
    xDataCache := aThreadDetails.FindDataCache(cPendingStepsTypeID) as TPendingStepsDataCache;
    xDataCache.ClearSteps();
end;

function TPendingRunStepsAddStepsRunEffect.GetRunEffectData: TPendingRunStepsAddStepsRunEffectData;
begin
    result := fRunEffectData as TPendingRunStepsAddStepsRunEffectData;
end;

{ TPendingStepDataCacheCreatorTypeInfo }

function TPendingStepDataCacheCreatorTypeInfo.CreateDataCache: TTraceDataCache;
begin
    result := TPendingStepsDataCache.Create();
end;

{ TPendingRunStepsClearRunEffectCreatorTypeInfo }

function TPendingRunStepsClearRunEffectCreatorTypeInfo.DoCreateRunEffect(const aRunEffectData: TRunEffectData)
    : TRunEffect;
begin
    result := TPendingRunStepsClearRunEffect.Create(aRunEffectData);
end;

function TPendingRunStepsClearRunEffectCreatorTypeInfo.DoSupportsData(const aRunEffectData
    : TRunEffectData): boolean;
begin
    result := aRunEffectData is TPendingRunStepsClearRunEffectData;
end;

{ TPendingRunStepsAddStepsRunEffectCreatorTypeInfo }

function TPendingRunStepsAddStepsRunEffectCreatorTypeInfo.DoCreateRunEffect(const aRunEffectData
    : TRunEffectData): TRunEffect;
begin
    result := TPendingRunStepsAddStepsRunEffect.Create(aRunEffectData);
end;

function TPendingRunStepsAddStepsRunEffectCreatorTypeInfo.DoSupportsData(const aRunEffectData
    : TRunEffectData): boolean;
begin
    result := aRunEffectData is TPendingRunStepsAddStepsRunEffectData;
end;


end.
