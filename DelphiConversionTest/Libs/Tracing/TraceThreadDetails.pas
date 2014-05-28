{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  07.06.10 pk                                        TN5077     initial revision
  15.11.10 pk                                        TN5340     Changes to prevent memory leak
  11.04.13 wl                                        TN6045   verwendet Generics.Collections
  15.08.13 wl                                        TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit TraceThreadDetails;


interface


uses
    Generics.Collections,
    TraceDataCache,
    CallStackDataCache,
    ActionDataCache,
    ProgramCounterDataCache,
    RunStepBlockDataCache,
    PendingStepsDataCache,
    TrackingSemaphore;

type
    TThreadDetailsItem = class
    private
        fThreadTraceRootPath: string;
        fCallStackDataCache: TCallStackDataCache;
        fProgramCounterDataCache: TProgramCounterDataCache;
        fActionListDataCache: TActionListDataCache;
        fBlockListDataCache: TRunStepBlockListDataCache;
        // fPendingStepsDataCache : TPendingStepsDataCache;
        fPluginDataCaches: TTraceDataCacheList;
        fIsTracingPaused: boolean;
        fDataLoaders: TTraceDataLoaderList;
    public
        constructor Create(const aThreadTraceRootPath, aCallStackPathName, aProgramCounterPathName,
            aActionListPathName, aBlockListPathName, aPendingStepsPathName: string);
        destructor Destroy(); override;
        procedure Clear();
        procedure ReadData();
        procedure InitData();
        procedure StoreDataFromRun();
        procedure LoadDataToRun;
        procedure Flush();
        procedure AddDataCache(const aDataCache: TTraceDataCache);
        function FindDataCache(const aTypeID: string): TTraceDataCache;
        procedure AddDataLoader(const aDataLoader: TTraceDataLoader);
        property IsTracingPaused: boolean read fIsTracingPaused write fIsTracingPaused;
        property CallStackDataCache: TCallStackDataCache read fCallStackDataCache;
        property ProgramCounterDataCache: TProgramCounterDataCache read fProgramCounterDataCache;
        property ActionListDataCache: TActionListDataCache read fActionListDataCache;
        property BlockListDataCache: TRunStepBlockListDataCache read fBlockListDataCache;
        // property PendingStepsDataCache : TPendingStepsDataCache read fPendingStepsDataCache;
        property ThreadTraceRootPath: string read fThreadTraceRootPath;
    end;

    TThreadDetailsManager = class
    private
        fCriticalSection: TTrackingSemaphore;
        fList: TObjectDictionary<string, TThreadDetailsItem>;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Clear();
        procedure Add(const aID: string; const aThreadDetailsItem: TThreadDetailsItem);
        function Find(const aID: string): TThreadDetailsItem;
        procedure Remove(const aID: string);
    end;


implementation


uses
    SysUtils,
    ThreadAPI;

{ TThreadDetailsItem }

constructor TThreadDetailsItem.Create(const aThreadTraceRootPath, aCallStackPathName, aProgramCounterPathName,
    aActionListPathName, aBlockListPathName, aPendingStepsPathName: string);
begin
    inherited Create();
    fIsTracingPaused := false;
    fThreadTraceRootPath := aThreadTraceRootPath;
    fCallStackDataCache := TCallStackDataCache.Create(aCallStackPathName);
    fProgramCounterDataCache := TProgramCounterDataCache.Create(aProgramCounterPathName);
    fActionListDataCache := TActionListDataCache.Create(aActionListPathName);
    fBlockListDataCache := TRunStepBlockListDataCache.Create(aBlockListPathName);
    // fPendingStepsDataCache := TPendingStepsDataCache.Create( aPendingStepsPathName );
    fPluginDataCaches := TTraceDataCacheList.Create();
    fDataLoaders := TTraceDataLoaderList.Create();

end;

destructor TThreadDetailsItem.Destroy();
begin
    Clear();
    FreeAndNil(fDataLoaders);
    FreeAndNil(fPluginDataCaches);
    // FreeAndNil( fPendingStepsDataCache );
    FreeAndNil(fBlockListDataCache);
    FreeAndNil(fActionListDataCache);
    FreeAndNil(fProgramCounterDataCache);
    FreeAndNil(fCallStackDataCache);
    inherited;
end;

procedure TThreadDetailsItem.Flush;
begin
    if self.IsTracingPaused then
        EXIT;
    fPluginDataCaches.Flush();

    fActionListDataCache.Flush();
    fProgramCounterDataCache.Flush();
    fCallStackDataCache.Flush();
    fBlockListDataCache.Flush();
    // fPendingStepsDataCache.Flush();
end;

function TThreadDetailsItem.FindDataCache(const aTypeID: string): TTraceDataCache;
begin
    result := fPluginDataCaches.Find(aTypeID);
end;

procedure TThreadDetailsItem.AddDataCache(const aDataCache: TTraceDataCache);
begin
    fPluginDataCaches.Add(aDataCache);
end;

procedure TThreadDetailsItem.AddDataLoader(const aDataLoader: TTraceDataLoader);
begin
    fDataLoaders.Add(aDataLoader);
end;

procedure TThreadDetailsItem.Clear;
begin
    if self.IsTracingPaused then
        EXIT;
    fPluginDataCaches.Clear();
    fCallStackDataCache.Clear();
    fProgramCounterDataCache.Clear();
    fActionListDataCache.Clear();
    fBlockListDataCache.Clear();
    // fPendingStepsDataCache.Clear();
end;

procedure TThreadDetailsItem.StoreDataFromRun;
var
    xDataLoader: TTraceDataLoader;
begin
    for xDataLoader in fDataLoaders do
        xDataLoader.StoreDataFromRun();

    Flush();
end;

procedure TThreadDetailsItem.InitData();
var
    xTraceDataCache: TTraceDataCache;
begin
    for xTraceDataCache in fPluginDataCaches do
    begin
        xTraceDataCache.Init();
    end;
end;

procedure TThreadDetailsItem.ReadData();
var
    xTraceDataCache: TTraceDataCache;
begin
    for xTraceDataCache in fPluginDataCaches do
    begin
        xTraceDataCache.Read()
    end;
end;

procedure TThreadDetailsItem.LoadDataToRun;
var
    xDataLoader: TTraceDataLoader;
begin
    ReadData();

    for xDataLoader in fDataLoaders do
        xDataLoader.LoadDataToRun();
end;

{ TThreadDetailsManager }
procedure TThreadDetailsManager.Clear;
begin
    fCriticalSection.Enter();
    try
        fList.Clear;
    finally
        fCriticalSection.Leave();
    end;
end;

constructor TThreadDetailsManager.Create;
begin
    inherited Create();
    fList := TObjectDictionary<string, TThreadDetailsItem>.Create([doOwnsValues]);
    fCriticalSection := TThreadAPI.CreateSemaphore(1);
end;

destructor TThreadDetailsManager.Destroy;
begin
    fCriticalSection.Free;
    fList.Free;
    inherited;
end;

procedure TThreadDetailsManager.Add(const aID: string; const aThreadDetailsItem: TThreadDetailsItem);
begin
    fCriticalSection.Enter();
    try
        fList.Add(aID, aThreadDetailsItem);
    finally
        fCriticalSection.Leave();
    end;
end;

function TThreadDetailsManager.Find(const aID: string): TThreadDetailsItem;
begin
    fCriticalSection.Enter();
    try
        if fList.ContainsKey(aID) then
            EXIT(fList[aID])
        else
            EXIT(nil);
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TThreadDetailsManager.Remove(const aID: string);
begin
    fCriticalSection.Enter();
    try
        if fList.ContainsKey(aID) then
            fList.Remove(aID);
    finally
        fCriticalSection.Leave();
    end;
end;


end.
