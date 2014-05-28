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

unit TraceProcessDetails;


interface


uses
    Generics.Collections,
    TraceDataCache,
    ProcessTraceDataCache,
    ProcessInfoDataCache,
    HeapDataCache,
    ThreadListDataCache,
    ReferenceIdentValueMemoryDataCache,
    ActionIDDataCache,
    PipDeviceDataCache,
    TrackingSemaphore,
    TraceThreadDetails;

type
    TProcessDetailsItem = class
    private
        fDirtyCount: integer;
        fCriticalSection: TTrackingSemaphore;

        fAutoFlushCycle: integer;
        fProcessInfoDataCache: TProcessInfoDataCache;
        fThreadDetailsManager: TThreadDetailsManager;
        fHeapDataCache: THeapDataCache;
        fThreadListDataCache: TThreadListDataCache;
        fReferenceIdentValueMemoryListDataCache: TReferenceIdentValueMemoryListDataCache;
        fActionIDDataCache: TActionIDDataCache;
        fPipDeviceListDataCache: TPipDeviceListDataCache;
        fPluginDataCaches: TTraceDataCacheList;
        fProcessTraceRootPath: string;
        fDataLoaders: TTraceDataLoaderList;

        procedure DoFlushProcessDetailsAndThread(const aThreadTraceName: string);
    public
        constructor Create(const aAutoFlushCycle: integer; const aProcessTraceRootPath: string;
            const aProcessInfoPathName, aHeapPathName, aThreadListPathName,
            aReferenceIdentValueMemoryPathName, aActionIDPathName, aPipDeviceListPathName: string);
        destructor Destroy(); override;
        procedure Clear();
        procedure StoreDataFromRun();
        procedure LoadDataToRun;
        procedure InitProcessDetails();
        procedure ReadProcessDetails();
        procedure ForceFlushProcessDetails();
        procedure FlushProcessDetailsAndThread(const aThreadTraceName: string);
        procedure ForceFlushProcessDetailsAndThread(const aThreadTraceName: string);
        function FindDataCache(const aTypeID: string): TTraceDataCache;
        procedure AddDataCache(const aDataCache: TTraceDataCache);
        procedure AddDataLoader(const aDataLoader: TTraceDataLoader);
        property ProcessTraceRootPath: string read fProcessTraceRootPath;
        property ProcessInfoDataCache: TProcessInfoDataCache read fProcessInfoDataCache;
        property ThreadDetailsManager: TThreadDetailsManager read fThreadDetailsManager;
        property HeapDataCache: THeapDataCache read fHeapDataCache;
        property ThreadListDataCache: TThreadListDataCache read fThreadListDataCache;
        property ReferenceIdentValueMemoryListDataCache: TReferenceIdentValueMemoryListDataCache
            read fReferenceIdentValueMemoryListDataCache;
        property ActionIDDataCache: TActionIDDataCache read fActionIDDataCache;
        property PipDeviceListDataCache: TPipDeviceListDataCache read fPipDeviceListDataCache;
    end;

    TProcessDetailsManager = class
    private
        fList: TObjectDictionary<string, TProcessDetailsItem>;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Add(const aID: string; aItem: TProcessDetailsItem);
        function Find(const aID: string): TProcessDetailsItem;
        procedure Remove(const aID: string);
    end;


implementation


uses
    SysUtils,
    ThreadAPI;

{ TProcessDetailsItem }

constructor TProcessDetailsItem.Create(const aAutoFlushCycle: integer; const aProcessTraceRootPath: string;
    const aProcessInfoPathName, aHeapPathName, aThreadListPathName, aReferenceIdentValueMemoryPathName,
    aActionIDPathName, aPipDeviceListPathName: string);
begin
    inherited Create();
    fDirtyCount := 0;
    fAutoFlushCycle := aAutoFlushCycle;
    fProcessTraceRootPath := aProcessTraceRootPath;
    fCriticalSection := TThreadAPI.CreateSemaphore(1);

    fThreadDetailsManager := TThreadDetailsManager.Create();
    fPluginDataCaches := TTraceDataCacheList.Create();
    fDataLoaders := TTraceDataLoaderList.Create();

    fProcessInfoDataCache := TProcessInfoDataCache.Create(aProcessInfoPathName);
    fHeapDataCache := THeapDataCache.Create(aHeapPathName);
    fThreadListDataCache := TThreadListDataCache.Create(aThreadListPathName);
    fReferenceIdentValueMemoryListDataCache := TReferenceIdentValueMemoryListDataCache.Create
        (aReferenceIdentValueMemoryPathName);
    fActionIDDataCache := TActionIDDataCache.Create(aActionIDPathName);
    fPipDeviceListDataCache := TPipDeviceListDataCache.Create(aPipDeviceListPathName);
end;

destructor TProcessDetailsItem.Destroy();
begin
    Clear();
    FreeAndNil(fPipDeviceListDataCache);
    FreeAndNil(fActionIDDataCache);
    FreeAndNil(fReferenceIdentValueMemoryListDataCache);
    FreeAndNil(fThreadListDataCache);
    FreeAndNil(fHeapDataCache);
    FreeAndNil(fProcessInfoDataCache);
    FreeAndNil(fDataLoaders);
    FreeAndNil(fPluginDataCaches);
    FreeAndNil(fThreadDetailsManager);
    FreeAndNil(fCriticalSection);
    inherited;
end;

function TProcessDetailsItem.FindDataCache(const aTypeID: string): TTraceDataCache;
begin
    result := fPluginDataCaches.Find(aTypeID);
end;

procedure TProcessDetailsItem.AddDataCache(const aDataCache: TTraceDataCache);
begin
    fPluginDataCaches.Add(aDataCache);
end;

procedure TProcessDetailsItem.AddDataLoader(const aDataLoader: TTraceDataLoader);
begin
    fDataLoaders.Add(aDataLoader);
end;

procedure TProcessDetailsItem.Clear;
begin
    fCriticalSection.Enter();
    try
        fPluginDataCaches.Clear();
        fProcessInfoDataCache.Clear();
        fThreadListDataCache.Clear();
        fHeapDataCache.Clear();
        fReferenceIdentValueMemoryListDataCache.Clear();
        fActionIDDataCache.Clear();
        fPipDeviceListDataCache.Clear();

        fThreadDetailsManager.Clear();
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TProcessDetailsItem.ForceFlushProcessDetails();
begin
    fCriticalSection.Enter();
    try
        // we assume here that ProcessInfo and ThredList Cache have an autoflush after each change and dont need an explicit flush here
        fPluginDataCaches.Flush();

        fActionIDDataCache.Flush();
        fHeapDataCache.Flush();
        fReferenceIdentValueMemoryListDataCache.Flush();
        fPipDeviceListDataCache.Flush();
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TProcessDetailsItem.DoFlushProcessDetailsAndThread(const aThreadTraceName: string);
var
    xThreadDetails: TThreadDetailsItem;
begin
    xThreadDetails := self.ThreadDetailsManager.Find(aThreadTraceName);
    if xThreadDetails.IsTracingPaused then
        EXIT;

    self.ForceFlushProcessDetails();
    xThreadDetails.Flush();
    fDirtyCount := 0;
end;

procedure TProcessDetailsItem.FlushProcessDetailsAndThread(const aThreadTraceName: string);
begin

    fCriticalSection.Enter();
    try
        Inc(fDirtyCount);
        if (fAutoFlushCycle <= 0) then
            EXIT;
        if (fDirtyCount < fAutoFlushCycle) then
            EXIT;

        DoFlushProcessDetailsAndThread(aThreadTraceName);
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TProcessDetailsItem.ForceFlushProcessDetailsAndThread(const aThreadTraceName: string);
begin
    fCriticalSection.Enter();
    try
        DoFlushProcessDetailsAndThread(aThreadTraceName);
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TProcessDetailsItem.StoreDataFromRun;
var
    xDataLoader: TTraceDataLoader;
begin
    for xDataLoader in fDataLoaders do
        xDataLoader.StoreDataFromRun();

    ForceFlushProcessDetails();
end;

procedure TProcessDetailsItem.InitProcessDetails();
var
    xTraceDataCache: TTraceDataCache;
begin
    for xTraceDataCache in fPluginDataCaches do
    begin
        xTraceDataCache.Init();
    end;
end;

procedure TProcessDetailsItem.ReadProcessDetails();
var
    xTraceDataCache: TTraceDataCache;
begin
    for xTraceDataCache in fPluginDataCaches do
    begin
        xTraceDataCache.Read()
    end;
end;

procedure TProcessDetailsItem.LoadDataToRun;
var
    xDataLoader: TTraceDataLoader;
begin
    ReadProcessDetails();

    for xDataLoader in fDataLoaders do
        xDataLoader.LoadDataToRun();
end;

{ TProcessDetailsManager }

constructor TProcessDetailsManager.Create;
begin
    inherited Create();
    fList := TObjectDictionary<string, TProcessDetailsItem>.Create([doOwnsValues]);
end;

destructor TProcessDetailsManager.Destroy;
begin
    fList.Free;
    inherited;
end;

procedure TProcessDetailsManager.Add(const aID: string; aItem: TProcessDetailsItem);
begin
    fList.Add(aID, aItem);
end;

function TProcessDetailsManager.Find(const aID: string): TProcessDetailsItem;
begin
    if fList.ContainsKey(aID) then
        EXIT(fList[aID])
    else
        EXIT(nil);
end;

procedure TProcessDetailsManager.Remove(const aID: string);
begin
    if fList.ContainsKey(aID) then
        fList.Remove(aID);
end;


end.
