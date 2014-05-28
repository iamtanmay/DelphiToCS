{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232    Initial Revision
  20.02.09 pk                                        TN4232    Changes for multithreaded trace
  24.02.09 pk  TProcessDetailsItem                   TN4232    New ActionIDDataCache
  24.02.09 pk                                        TN4232    various functions changed to class functions
  13.07.09 pk  GetTraceRootPath                      TN4585.4  RunAliasPath changed to RunPath
  04.11.09 pk                               	        TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  25.11.09 pk                                        TN4898    New Flush Function
  04.02.10 pk                                        TN4972    Various Changes
  03.09.10 pk  SetPauseTracingForCurrentThread       TN4972    Check IsEnabled
  15.04.10 wl  					    				TN5044
  15.04.10 pk                                        TN5050    Changes for PipDeviceListDataCache
  23.04.10 pk  TThreadDetailsManager.Clear           TN5050    New
  19.05.10 pk  RestartOnlyAtMarks                    TN5113    New
  07.06.10 pk                                        TN5077    Changes for restarting racks moves
  26.10.10 pk  TTraceManager                         TN5297    changes for ActionData segment concept
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  15.11.10 pk  ArchiveProcessTrace                   TN5347    MoveFile changed to DeleteDirectory for now - archiving
  22.07.11 ts  RemoveThreadTraceForCurrentThread     TN5632    StoreThreadTraceForCurrentThread will be executed only if globalErr has been set to true
  11.04.13 wl                                        TN6045   verwendet Generics.Collections
  15.08.13 wl                                        TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit TraceManager;


interface


uses
    Generics.Collections,
    TraceDataCache,
    ProcessTraceDataCache,
    ProcessInfoDataCache,
    HeapDataCache,
    ThreadListDataCache,
    CallStackDataCache,
    ReferenceIdentValueMemoryDataCache,
    ActionDataCache,
    ProgramCounterDataCache,
    ActionIDDataCache,
    RunStepBlockDataCache,
    PendingStepsDataCache,
    PipDeviceDataCache,
    ThreadClasses,
    RestartPreparationStep,
    TraceProcessDetails,
    TraceThreadDetails,
    RunEffect,
    RunEffectData,
    ActionData;

type

    TTraceManager = class
    private
        function GetRestartOnlyAtMarks: boolean;
        procedure AddThreadDetailDataCache(const aThreadDetailsItem: TThreadDetailsItem;
            const aTypeInfo: TTraceDataCacheCreatorTypeInfo);
        procedure AddThreadDetailDataLoaderForDataCache(const aThreadDetailsItem: TThreadDetailsItem;
            const aDataCache: TTraceDataCache);

        procedure AddProcessDetailDataCache(const aProcessDetailsItem: TProcessDetailsItem;
            const aTypeInfo: TTraceDataCacheCreatorTypeInfo);
        procedure AddProcessDetailDataLoaderForDataCache(const aProcessDetailsItem: TProcessDetailsItem;
            const aDataCache: TTraceDataCache);
    protected
        fProcessTraceListDataCache: TProcessTraceListDataCache;
        fProcessDetailsManager: TProcessDetailsManager;
        fIsEnabled: boolean;
        fAutoFlushCycle: integer;
        fRestartOnlyAtRestartMarkers: boolean;
        fProcessDetailsDataCacheCreatorTypeInfoList: TTraceDataCacheCreatorTypeInfoList;
        fThreadDetailsDataCacheCreatorTypeInfoList: TTraceDataCacheCreatorTypeInfoList;
        fRunEffectCreatorTypeInfoList: TRunEffectCreatorTypeInfoList;
        fRestartPreparationStepCreatorTypInfoList: TRestartPreparationStepCreatorTypeInfoList;
        fDataLoaderCreatorTypeInfoList: TTraceDataLoaderCreatorTypeInfoList;
        fActionDataCreatorTypeDictionary: TActionDataCreatorTypeDictionary;
        constructor Create();
        procedure AddStandardTypeInfos(); virtual;
        procedure ReadEnabled();
        procedure PrepareProcessDetailsManager();
        procedure AddProcessDetails(const aProcessTraceName: string);
        procedure AddThreadDetails(const aThreadTraceName: string);
        function FindThreadDetailsManagerForCurrentProcess(): TThreadDetailsManager;
        function CreateThreadDetails(const aThreadTracePath: string): TThreadDetailsItem;
        function CreateProcessDetails(const aAutoFlushCycle: integer; const aProcessTracePath: string)
            : TProcessDetailsItem;

        class function GetTraceRootPath(): string;
        class function ConcatPaths(const aPath1, aPath2: string): string;
        class function GetProcessTraceDirFromTracePath(const aTracePath, aTraceName: string): string;
        class function GetTracePathForProcess(const aProcessTraceName: string): string;
        class function GetTracePathForThread(const aProcessTraceName: string;
            const aThreadTraceName: string): string;
        class function GetTracePathForThreadInCurrentProcess(const aThreadTraceName: string): string;
        class function GetTracePathForProcessInfo(const aProcessTracePath: string): string;
        class function GetTracePathForProcessThreadList(const aProcessTracePath: string): string;
        class function GetTracePathForThreadProgramCounter(const aThreadTracePath: string): string;
        class function GetTraceArchivePath(): string;
    public
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstance;
        class procedure SetInstance(const aInstance: TTraceManager);
        class function Instance: TTraceManager;
        class function GetTraceCurrentPath(): string;
        class function GetTraceNameForCurrentProcess(): string;
        class function GetTraceNameForCurrentThread(): string;

        class function GetTracePathForCurrentProcess(): string;
        class function GetTracePathForCurrentThread(): string;

        procedure InitProcessList();

        // ProcessTrace
        procedure AddProcessTrace(aProcessTraceData: TProcessTraceData);
        function FindProcessTrace(const aTraceName: string): TProcessTraceData;
        function IsCurrentProcessRestart(): boolean;

        class procedure RemoveProcessTrace(const aProcessListDataCache: TProcessTraceListDataCache;
            const aTraceName: string);
        class procedure ArchiveProcessTrace(const aProcessListDataCache: TProcessTraceListDataCache;
            const aTraceName: string);

        procedure StoreProcessTraceForCurrentProcess();

        // Process Trace
        procedure RemoveProcessTraceForCurrentProcess(const aCompleted: boolean);
        function FindProcessTraceForCurrentProcess(): TProcessTraceData;

        // Process Details
        function FindProcessDetailsForCurrentProcess(): TProcessDetailsItem;
        function FindHeapDataCacheForCurrentProcess(): THeapDataCache;
        function FindThreadListDataCacheForCurrentProcess: TThreadListDataCache;
        function FindReferenceIdentValueMemoryListDataCacheForCurrentProcess
            : TReferenceIdentValueMemoryListDataCache;

        procedure StoreThreadTraceForCurrentThread();
        procedure RemoveThreadTraceForCurrentThread(const aRemoveFromFile: boolean);
        function FindThreadDetailsForCurrentThread(): TThreadDetailsItem;
        function FindCallStackDataCacheForCurrentThread(): TCallStackDataCache;
        procedure SetPauseTracingForCurrentThread(const aPause: boolean);

        procedure FlushCurrentProcessAndThread();
        procedure ForceFlushCurrentProcessAndThread();

        procedure AddPipDevice(const aPipDeviceName: string; const aTipCount: integer); virtual;
        procedure PipDeviceInitTipTypes(const aPipDeviceName: string;
            const aTipTypeNames: TArray<string>); virtual;
        function PipDeviceGetCurrentTipTypes(const aPipDeviceName: string): TArray<string>;

        // RunEffects
        procedure AddRunEffectDataToCurrentAction(const aRunEffectData: TRunEffectData);
        procedure RackMoved(const aRackName: string; const aOldCarrierName: string;
            const aOldCarrierSlot: integer; const aOldRotation: integer); virtual;
        procedure RackChangedID(const aRackName, aOldRackID: string); virtual;
        procedure RackTypeChanged(const aRackName: string; const aOldTypeName: string); virtual;
        procedure CarrierTypeChanged(const aCarrierName: string; const aOldTypeName: string); virtual;
        procedure PipDeviceTipTypeChanged(const aPipDeviceName: string; const aTipIndex: integer;
            const aTipTypeName: string); virtual;

        function CreateProcessDetailsForProcessTrace(const aAutoFlushCycle: integer;
            const aProcessTraceName: string): TProcessDetailsItem;
        function CreateThreadDetailsForThreadTrace(const aProcessTraceName, aThreadTraceName: string)
            : TThreadDetailsItem;

        class function CreateProcessInfoDataCacheBySourceDataName(const aProcessListDataCache
            : TProcessTraceListDataCache; const aSourceDataName: string): TProcessInfoDataCache;

        class function CreateProcesListDataCache(): TProcessTraceListDataCache;
        class function GetMainThreadTraceNameForProcess(const aProcessTraceName: string): string;
        class function MakeTraceName(const aSourceDataName: string): string;

        property IsEnabled: boolean read fIsEnabled;
        property RestartOnlyAtMarks: boolean read GetRestartOnlyAtMarks;
    end;


implementation


uses
    SysUtils,
    AppSettings,
    FileUtilities,
    ThreadAPI,
    CommonTypes;

var
    uTraceManager: TTraceManager = nil;

    { TTraceManager }
class procedure TTraceManager.SetInstance(const aInstance: TTraceManager);
begin
    uTraceManager := aInstance;
end;

class procedure TTraceManager.CreateInstance();
begin
    if Assigned(uTraceManager) then
        EXIT;
    SetInstance(TTraceManager.Create());

end;

class procedure TTraceManager.DestroyInstance();
begin
    uTraceManager.Free;
end;

class function TTraceManager.Instance(): TTraceManager;
begin
    result := uTraceManager;
end;

class function TTraceManager.MakeTraceName(const aSourceDataName: string): string;
const
    cFormatArchiveTimeStamp = 'yyyymmdd-hhnnss';
begin
    result := 'P~' + aSourceDataName + '~' + FormatDateTime(cFormatArchiveTimeStamp, now);
end;

constructor TTraceManager.Create();
begin
    inherited Create();

    fProcessTraceListDataCache := CreateProcesListDataCache();

    fProcessDetailsManager := TProcessDetailsManager.Create();

    fProcessDetailsDataCacheCreatorTypeInfoList := TTraceDataCacheCreatorTypeInfoList.Create();
    fThreadDetailsDataCacheCreatorTypeInfoList := TTraceDataCacheCreatorTypeInfoList.Create();

    fRunEffectCreatorTypeInfoList := TRunEffectCreatorTypeInfoList.Create();
    fRestartPreparationStepCreatorTypInfoList := TRestartPreparationStepCreatorTypeInfoList.Create();
    fDataLoaderCreatorTypeInfoList := TTraceDataLoaderCreatorTypeInfoList.Create();

    fActionDataCreatorTypeDictionary := TActionDataCreatorTypeDictionary.Create();

    AddStandardTypeInfos();

    ReadEnabled();
end;

destructor TTraceManager.Destroy;
begin
    FreeAndNil(fDataLoaderCreatorTypeInfoList);
    FreeAndNil(fRestartPreparationStepCreatorTypInfoList);
    FreeAndNil(fRunEffectCreatorTypeInfoList);
    FreeAndNil(fThreadDetailsDataCacheCreatorTypeInfoList);
    FreeAndNil(fProcessDetailsDataCacheCreatorTypeInfoList);
    FreeAndNil(fActionDataCreatorTypeDictionary);
    fProcessDetailsManager.Free;
    fProcessTraceListDataCache.Free;
    inherited;
end;

procedure TTraceManager.AddStandardTypeInfos();
begin
    // fProcessDetailsDataCacheCreatorTypeInfoList.Add( TLayoutRackDataCacheCreatorTypeInfo.Create() );
end;

procedure TTraceManager.AddProcessDetailDataLoaderForDataCache(const aProcessDetailsItem: TProcessDetailsItem;
    const aDataCache: TTraceDataCache);
var
    xDataLoader: TTraceDataLoader;
    xTypeInfo: TTraceDataLoaderCreatorTypeInfo;
begin
    xTypeInfo := fDataLoaderCreatorTypeInfoList.FindBySupportsDataCache(aDataCache);
    ASSERT(Assigned(xTypeInfo), 'TTraceDataLoaderCreatorTypeInfo not found for ' + aDataCache.ClassName);
    xDataLoader := xTypeInfo.CreateDataLoader(aDataCache);
    aProcessDetailsItem.AddDataLoader(xDataLoader);
end;

procedure TTraceManager.AddProcessDetailDataCache(const aProcessDetailsItem: TProcessDetailsItem;
    const aTypeInfo: TTraceDataCacheCreatorTypeInfo);
var
    xDataCache: TTraceDataCache;

begin
    xDataCache := aTypeInfo.CreateDataCache();
    aProcessDetailsItem.AddDataCache(xDataCache);
    xDataCache.SetPathName(ConcatPaths(aProcessDetailsItem.ProcessTraceRootPath, xDataCache.TypeID));
    self.AddProcessDetailDataLoaderForDataCache(aProcessDetailsItem, xDataCache);

end;

function TTraceManager.CreateProcessDetails(const aAutoFlushCycle: integer; const aProcessTracePath: string)
    : TProcessDetailsItem;
var
    xTypeInfo: TTraceDataCacheCreatorTypeInfo;
begin
    result := TProcessDetailsItem.Create(aAutoFlushCycle, aProcessTracePath,
        GetTracePathForProcessInfo(aProcessTracePath), ConcatPaths(aProcessTracePath, 'Heap'),
        GetTracePathForProcessThreadList(aProcessTracePath), ConcatPaths(aProcessTracePath,
        'ReferenceValueMemory'), ConcatPaths(aProcessTracePath, 'ActionID'),
        ConcatPaths(aProcessTracePath, 'PipettingDevices'));

    for xTypeInfo in fProcessDetailsDataCacheCreatorTypeInfoList do
    begin
        AddProcessDetailDataCache(result, xTypeInfo);
    end;

end;

procedure TTraceManager.ReadEnabled();
var
    xIniAccess: IWinLissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateAppIni();
    fIsEnabled := xIniAccess.ReadBool('Restart', 'Enabled');
    fRestartOnlyAtRestartMarkers := xIniAccess.ReadBool('Restart', 'OnlyAtRestartMarkers');
    fAutoFlushCycle := 0;

end;

procedure TTraceManager.InitProcessList();
begin
    if not self.IsEnabled then
        EXIT;
    fProcessTraceListDataCache.Init();
    // PrepareProcessDetailsManager();
end;

function TTraceManager.FindThreadDetailsManagerForCurrentProcess(): TThreadDetailsManager;
begin
    result := FindProcessDetailsForCurrentProcess().ThreadDetailsManager;
end;

procedure TTraceManager.AddProcessDetails(const aProcessTraceName: string);
var
    xPath: string;
begin
    xPath := GetTracePathForProcess(aProcessTraceName);
    fProcessDetailsManager.Add(aProcessTraceName, CreateProcessDetails(fAutoFlushCycle, xPath));
end;

procedure TTraceManager.AddThreadDetailDataLoaderForDataCache(const aThreadDetailsItem: TThreadDetailsItem;
    const aDataCache: TTraceDataCache);
var
    xDataLoader: TTraceDataLoader;
    xTypeInfo: TTraceDataLoaderCreatorTypeInfo;
begin
    xTypeInfo := fDataLoaderCreatorTypeInfoList.FindBySupportsDataCache(aDataCache);
    ASSERT(Assigned(xTypeInfo), 'TTraceDataLoaderCreatorTypeInfo not found for ' + aDataCache.ClassName);
    xDataLoader := xTypeInfo.CreateDataLoader(aDataCache);
    aThreadDetailsItem.AddDataLoader(xDataLoader);
end;

procedure TTraceManager.AddThreadDetailDataCache(const aThreadDetailsItem: TThreadDetailsItem;
    const aTypeInfo: TTraceDataCacheCreatorTypeInfo);
var
    xDataCache: TTraceDataCache;

begin
    xDataCache := aTypeInfo.CreateDataCache();
    aThreadDetailsItem.AddDataCache(xDataCache);
    xDataCache.SetPathName(ConcatPaths(aThreadDetailsItem.ThreadTraceRootPath, xDataCache.TypeID));
    self.AddThreadDetailDataLoaderForDataCache(aThreadDetailsItem, xDataCache);

end;

function TTraceManager.CreateThreadDetails(const aThreadTracePath: string): TThreadDetailsItem;
var
    xTypeInfo: TTraceDataCacheCreatorTypeInfo;
begin
    result := TThreadDetailsItem.Create(aThreadTracePath, ConcatPaths(aThreadTracePath, 'CallStack'),
        GetTracePathForThreadProgramCounter(aThreadTracePath),
        ConcatPaths(aThreadTracePath, ConcatPaths('Actions', 'Actions')),
        ConcatPaths(aThreadTracePath, 'Blocks'), ConcatPaths(aThreadTracePath, 'PendingSteps'));

    for xTypeInfo in fThreadDetailsDataCacheCreatorTypeInfoList do
    begin
        AddThreadDetailDataCache(result, xTypeInfo);
    end;
end;

procedure TTraceManager.AddThreadDetails(const aThreadTraceName: string);
var
    xThreadTracePath: string;
    xThreadDetailsManager: TThreadDetailsManager;
begin
    xThreadTracePath := GetTracePathForThreadInCurrentProcess(aThreadTraceName);
    xThreadDetailsManager := FindThreadDetailsManagerForCurrentProcess();
    if Assigned(xThreadDetailsManager.Find(aThreadTraceName)) then
        EXIT;

    xThreadDetailsManager.Add(aThreadTraceName, CreateThreadDetails(xThreadTracePath));
end;

procedure TTraceManager.SetPauseTracingForCurrentThread(const aPause: boolean);
var
    xThreadDetails: TThreadDetailsItem;
begin
    if not self.IsEnabled then
        EXIT;

    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    xThreadDetails.IsTracingPaused := aPause;
end;

procedure TTraceManager.PrepareProcessDetailsManager();
var
    x: integer;
    xTraceName: string;
begin
    for x := 0 to fProcessTraceListDataCache.Count - 1 do
    begin
        xTraceName := fProcessTraceListDataCache[x].TraceName;
        if Assigned(fProcessDetailsManager.Find(xTraceName)) then
            CONTINUE;
        AddProcessDetails(xTraceName);
    end;
end;

class function TTraceManager.GetTraceRootPath: string;
begin
    result := TAppSettings.RunPath;
end;

class function TTraceManager.GetTraceCurrentPath(): string;
begin
    result := ConcatPaths(GetTraceRootPath(), 'Current');
end;

class function TTraceManager.GetTraceArchivePath(): string;
begin
    result := ConcatPaths(GetTraceRootPath(), 'Archive');
end;

class function TTraceManager.GetTraceNameForCurrentProcess(): string;
begin
    result := TThreadAPI.GetCurrentProcessDescription();
end;

procedure TTraceManager.CarrierTypeChanged(const aCarrierName, aOldTypeName: string);
begin

end;

class function TTraceManager.ConcatPaths(const aPath1, aPath2: string): string;
begin
    result := TFileUtilities.IncludeTrailingPathDelimiter(aPath1) + aPath2;
end;

class function TTraceManager.GetTracePathForProcess(const aProcessTraceName: string): string;
begin
    result := ConcatPaths(GetTraceCurrentPath, aProcessTraceName);
end;

class function TTraceManager.GetProcessTraceDirFromTracePath(const aTracePath, aTraceName: string): string;
begin
    // for now we will assum that the trace dir is just the tracename
    result := aTraceName;
end;

function TTraceManager.GetRestartOnlyAtMarks: boolean;
begin
    result := fRestartOnlyAtRestartMarkers;
end;

class function TTraceManager.GetTracePathForCurrentProcess(): string;
begin
    result := GetTracePathForProcess(TThreadAPI.GetCurrentProcessDescription());
end;

class function TTraceManager.GetTracePathForThread(const aProcessTraceName: string;
    const aThreadTraceName: string): string;
begin
    result := ConcatPaths(GetTracePathForProcess(aProcessTraceName), aThreadTraceName);
end;

class function TTraceManager.GetTracePathForThreadInCurrentProcess(const aThreadTraceName: string): string;
begin
    result := GetTracePathForThread(TThreadAPI.GetCurrentProcessDescription(), aThreadTraceName);
end;

class function TTraceManager.GetTracePathForCurrentThread(): string;
begin
    result := GetTracePathForThreadInCurrentProcess(TThreadAPI.GetCurrentThreadDescription());
end;

class function TTraceManager.GetTracePathForProcessThreadList(const aProcessTracePath: string): string;
begin
    result := ConcatPaths(aProcessTracePath, 'Threads');
end;

class function TTraceManager.GetTracePathForProcessInfo(const aProcessTracePath: string): string;
begin
    result := ConcatPaths(aProcessTracePath, 'ProcessInfo');
end;

class function TTraceManager.GetTracePathForThreadProgramCounter(const aThreadTracePath: string): string;
begin
    result := ConcatPaths(aThreadTracePath, 'ProgramCounter');
end;

{
  procedure TTraceManager.PrepareHeapDataCacheForCurrentProcess();
  var

  begin
  xHeapInitNeeded := true;
  if IsCurrentProcessRestart then begin
  xReadOK := FindHeapDataCacheForCurrentProcess().Read;
  if xReadOK then begin
  xHeapInitNeeded := false;
  LoadHeapDataCacheForCurrentProcessToHeap();
  end;
  end;


  end;
}
procedure TTraceManager.StoreProcessTraceForCurrentProcess();
var
    xProcessDetailsItem: TProcessDetailsItem;
begin
    xProcessDetailsItem := self.FindProcessDetailsForCurrentProcess();
    xProcessDetailsItem.StoreDataFromRun();
end;

procedure TTraceManager.RemoveProcessTraceForCurrentProcess(const aCompleted: boolean);
var
    xProcessInfoDataCache: TProcessInfoDataCache;
    xTraceName: string;
begin
    if not self.IsEnabled then
        EXIT;
    xTraceName := GetTraceNameForCurrentProcess();

    StoreProcessTraceForCurrentProcess();

    xProcessInfoDataCache := self.FindProcessDetailsForCurrentProcess.ProcessInfoDataCache;

    xProcessInfoDataCache.ChangedDateLastStopped(Now());
    xProcessInfoDataCache.ChangeIsCompleted(aCompleted);

    if aCompleted then
    begin
        fProcessDetailsManager.Remove(xTraceName);
        self.RemoveProcessTrace(fProcessTraceListDataCache, xTraceName);
    end
    else
    begin
        fProcessDetailsManager.Find(xTraceName).Clear();
    end;

end;

function TTraceManager.FindProcessTraceForCurrentProcess: TProcessTraceData;
var
    xTraceName: string;
begin
    xTraceName := GetTraceNameForCurrentProcess();
    result := fProcessTraceListDataCache.FindProcessTrace(xTraceName);
end;

function TTraceManager.FindProcessDetailsForCurrentProcess(): TProcessDetailsItem;
var
    xTraceName: string;
begin
    xTraceName := GetTraceNameForCurrentProcess();
    result := fProcessDetailsManager.Find(xTraceName);
end;

function TTraceManager.FindHeapDataCacheForCurrentProcess: THeapDataCache;
begin
    result := FindProcessDetailsForCurrentProcess().HeapDataCache;
end;

function TTraceManager.FindThreadListDataCacheForCurrentProcess: TThreadListDataCache;
begin
    result := FindProcessDetailsForCurrentProcess().ThreadListDataCache;
end;

procedure TTraceManager.FlushCurrentProcessAndThread;
var
    xProcessDetails: TProcessDetailsItem;
    xThreadTraceName: string;
begin
    if not self.IsEnabled then
        EXIT;

    xProcessDetails := self.FindProcessDetailsForCurrentProcess();
    xThreadTraceName := self.GetTraceNameForCurrentThread();
    xProcessDetails.FlushProcessDetailsAndThread(xThreadTraceName);
end;

procedure TTraceManager.ForceFlushCurrentProcessAndThread;
var
    xProcessDetails: TProcessDetailsItem;
    xThreadTraceName: string;
begin
    if not self.IsEnabled then
        EXIT;
    xProcessDetails := self.FindProcessDetailsForCurrentProcess();
    xThreadTraceName := self.GetTraceNameForCurrentThread();
    xProcessDetails.ForceFlushProcessDetailsAndThread(xThreadTraceName);
end;

function TTraceManager.FindReferenceIdentValueMemoryListDataCacheForCurrentProcess
    : TReferenceIdentValueMemoryListDataCache;
begin
    result := FindProcessDetailsForCurrentProcess().ReferenceIdentValueMemoryListDataCache;
end;

procedure TTraceManager.AddProcessTrace(aProcessTraceData: TProcessTraceData);
begin
    fProcessTraceListDataCache.AddProcessTrace(aProcessTraceData);
end;

function TTraceManager.FindProcessTrace(const aTraceName: string): TProcessTraceData;
begin
    result := fProcessTraceListDataCache.FindProcessTrace(aTraceName);
end;

class function TTraceManager.CreateProcessInfoDataCacheBySourceDataName(const aProcessListDataCache
    : TProcessTraceListDataCache; const aSourceDataName: string): TProcessInfoDataCache;
var
    x: integer;
    xProcessTracePath, xTraceName: string;
    xProcessInfoPathName: string;
    xProcessInfoDataCache: TProcessInfoDataCache;
begin
    result := nil;
    for x := 0 to aProcessListDataCache.Count - 1 do
    begin
        xTraceName := (aProcessListDataCache[x] as TProcessTraceData).TraceName;
        xProcessTracePath := self.GetTracePathForProcess(xTraceName);
        xProcessInfoPathName := GetTracePathForProcessInfo(xProcessTracePath);
        xProcessInfoDataCache := TProcessInfoDataCache.Create(xProcessInfoPathName);
        try
            if (not xProcessInfoDataCache.Read()) then
                CONTINUE;
            if SameText(aSourceDataName, xProcessInfoDataCache.ProcessInfoData.SourceDataName) then
            begin
                result := xProcessInfoDataCache;
            end;
        finally
            if not Assigned(result) then
                xProcessInfoDataCache.Free;
        end;
    end;
end;

class procedure TTraceManager.ArchiveProcessTrace(const aProcessListDataCache: TProcessTraceListDataCache;
    const aTraceName: string);
var
    xProcessTrace: TProcessTraceData;
    xProcessTracePath: string;
    // xProcessTraceDir : string;
    // xArchiveRootPath : string;
    // xArchivePath : string;
begin
    xProcessTrace := aProcessListDataCache.FindProcessTrace(aTraceName);

    xProcessTracePath := GetTracePathForProcess(xProcessTrace.TraceName);

    // 15.11.10 pk Archiving worked, but since there was no editor with which you could reactivate old archives
    // the archiving was useless and only took up harddrive space, so we delete for now till Archiving is really implemented in the GUI
    TFileUtilities.DeleteDirectory(xProcessTracePath, true);

    // 15.11.10 pk Archiving:
    // xProcessTraceDir := GetProcessTraceDirFromTracePath( xProcessTracePath, xProcessTrace.TraceName );
    // xArchiveRootPath := GetTraceArchivePath();
    // xArchivePath := self.ConcatPaths( xArchiveRootPath, xProcessTraceDir );
    // TFileUtilities.ForceDirectories( xArchiveRootPath );
    // TFileUtilities.MoveFile( xProcessTracePath, xArchivePath );
end;

class procedure TTraceManager.RemoveProcessTrace(const aProcessListDataCache: TProcessTraceListDataCache;
    const aTraceName: string);
begin
    ArchiveProcessTrace(aProcessListDataCache, aTraceName);
    aProcessListDataCache.RemoveProcessTrace(aTraceName, true);
end;

class function TTraceManager.GetTraceNameForCurrentThread(): string;
begin
    result := TThreadAPI.GetCurrentThreadDescription();
end;

procedure TTraceManager.StoreThreadTraceForCurrentThread();
var
    xThreadDetails: TThreadDetailsItem;
begin
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    xThreadDetails.StoreDataFromRun();
end;

procedure TTraceManager.RemoveThreadTraceForCurrentThread(const aRemoveFromFile: boolean);
var
    xTraceName: string;
    xThreadListDataCache: TThreadListDataCache;
begin
    if not self.IsEnabled then
        EXIT;
    xTraceName := GetTraceNameForCurrentThread();

    // StoreThreadTraceForCurrentThread(); -> removed to if clause

    if aRemoveFromFile then
    begin
        self.FindThreadDetailsManagerForCurrentProcess.Remove(xTraceName);

        xThreadListDataCache := self.FindThreadListDataCacheForCurrentProcess();
        xThreadListDataCache.RemoveThread(xTraceName, aRemoveFromFile);
    end
    else
    begin
        StoreThreadTraceForCurrentThread();
        self.FindThreadDetailsManagerForCurrentProcess.Find(xTraceName).Clear();
    end;
end;

function TTraceManager.FindThreadDetailsForCurrentThread: TThreadDetailsItem;
var
    xTraceName: string;
begin
    xTraceName := GetTraceNameForCurrentThread();
    result := self.FindThreadDetailsManagerForCurrentProcess.Find(xTraceName);
end;

function TTraceManager.IsCurrentProcessRestart: boolean;
var
    xTraceName: string;
begin
    result := false;
    if not self.IsEnabled then
        EXIT;
    xTraceName := GetTraceNameForCurrentProcess();
    result := self.FindProcessDetailsForCurrentProcess.ProcessInfoDataCache.ProcessInfoData.IsRestart;
end;

function TTraceManager.FindCallStackDataCacheForCurrentThread: TCallStackDataCache;
begin
    result := FindThreadDetailsForCurrentThread().CallStackDataCache;
end;

procedure TTraceManager.PipDeviceTipTypeChanged(const aPipDeviceName: string; const aTipIndex: integer;
    const aTipTypeName: string);
begin

end;

procedure TTraceManager.PipDeviceInitTipTypes(const aPipDeviceName: string;
    const aTipTypeNames: TArray<string>);
begin

end;

procedure TTraceManager.AddPipDevice(const aPipDeviceName: string; const aTipCount: integer);
begin

end;

function TTraceManager.PipDeviceGetCurrentTipTypes(const aPipDeviceName: string): TArray<string>;
begin

end;

procedure TTraceManager.RackChangedID(const aRackName, aOldRackID: string);
begin

end;

procedure TTraceManager.RackMoved(const aRackName, aOldCarrierName: string;
    const aOldCarrierSlot, aOldRotation: integer);
begin

end;

procedure TTraceManager.RackTypeChanged(const aRackName, aOldTypeName: string);
begin

end;

procedure TTraceManager.AddRunEffectDataToCurrentAction(const aRunEffectData: TRunEffectData);
var
    xThreadDetails: TThreadDetailsItem;
    xActionListDataCache: TActionListDataCache;
begin
    xThreadDetails := self.FindThreadDetailsForCurrentThread();
    if xThreadDetails.IsTracingPaused then
    begin
        aRunEffectData.Free;
        EXIT;
    end;

    xActionListDataCache := xThreadDetails.ActionListDataCache;
    if not Assigned(xActionListDataCache.CurrentActionData) then
    begin
        // There is no current action when we are starting the MAIN method and setting variable values which were entered by the user from the methodparameters dialog box
        aRunEffectData.Free;
        EXIT;
    end;
    xActionListDataCache.AddRunEffect(aRunEffectData);

end;

class function TTraceManager.GetMainThreadTraceNameForProcess(const aProcessTraceName: string): string;
var
    xProcessTracePath, xThreadListPathName: string;
    xThreadListDataCache: TThreadListDataCache;
    xMainThreadData: TThreadData;
begin
    result := '';
    xProcessTracePath := GetTracePathForProcess(aProcessTraceName);
    // Get Tracename of main thread
    xThreadListPathName := GetTracePathForProcessThreadList(xProcessTracePath);
    xThreadListDataCache := TThreadListDataCache.Create(xThreadListPathName);
    try
        if (not xThreadListDataCache.Read()) or (xThreadListDataCache.Count = 0) then
            EXIT;
        xMainThreadData := xThreadListDataCache.MainThread;
        if not Assigned(xMainThreadData) then
            EXIT;

        result := xMainThreadData.TraceName;
    finally
        xThreadListDataCache.Free;
    end;
end;

function TTraceManager.CreateProcessDetailsForProcessTrace(const aAutoFlushCycle: integer;
    const aProcessTraceName: string): TProcessDetailsItem;
var
    xProcessTracePath: string;
begin
    xProcessTracePath := GetTracePathForProcess(aProcessTraceName);
    result := CreateProcessDetails(aAutoFlushCycle, xProcessTracePath);
end;

function TTraceManager.CreateThreadDetailsForThreadTrace(const aProcessTraceName, aThreadTraceName: string)
    : TThreadDetailsItem;
var
    xProcessTracePath: string;
    xThreadTracePath: string;
begin
    xProcessTracePath := GetTracePathForProcess(aProcessTraceName);
    xThreadTracePath := GetTracePathForThread(aProcessTraceName, aThreadTraceName);
    result := CreateThreadDetails(xThreadTracePath);
end;

class function TTraceManager.CreateProcesListDataCache(): TProcessTraceListDataCache;
begin
    result := TProcessTraceListDataCache.Create(self.ConcatPaths(GetTraceCurrentPath(), 'Processes'));
end;


end.
