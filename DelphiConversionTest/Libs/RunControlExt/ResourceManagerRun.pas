{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.01.06 pk                               TN2877   Initial version
  05.07.06 pk  fVirtualResources            TN3179   New : Resources are read from resource and stored in this list
  05.07.06 pk  AcquireVirtualResource       TN3179   New : do acqire for a virtual resource
  --------------------------------------------------------------------------------------------------
  07.12.06 pk                               TN3455   Renamed from ResourceManager
  07.12.06 pk  TRunResource                 TN3455   from resource.pas unit
  22.07.07 pk  TVirtualResource             TN3583   New
  24.06.08 wl                               TN4143   uses geändert
  03.07.08 wl                                         TN4157
  17.11.08 pk                               TN4280   Massive Changes
  07.04.09 pk  ReleaseResource              TN4507   Wait until message is handled
  08.09.09 pk                               TN4753   uses ErrorMessage replaced by ErrorInfo
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  09.03.10 pk  TThreadResourceData          TN5015   New:  Wrapper for ThreadAPI functions
  13.12.10 wl                               TN5370   TLock.WaitForLock statt TLock.WaitFor
  10.03.11 wl                               TN5499   an geänderten ResSchemeDataAdaptor angepasst
  27.03.13 wl                               TN6045   verwendet Generics.Collections
  15.08.13 wl                               TN6223   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit ResourceManagerRun;


interface


uses
    Generics.Collections,
    Classes,
    SyncObjs,
    GeneralTypes,
    MessageQueue,
    ThreadClasses,
    InterfacedNoRef,
    LockHandle,
    ResourceManager,
    ListClasses;

type
    TResourceUsageCacheItem = class
    private
        fResID: string;
    public
        constructor Create(const aResID: string);
        property ResID: string read fResID;
    end;

    TResourceUsageSchemeCacheItem = class
    private
        fSchemeID: string;
        fResourceUsages: TObjectList<TResourceUsageCacheItem>;
    public
        constructor Create(const aSchemeID: string);
        destructor Destroy(); override;
        property SchemeID: string read fSchemeID;
        property ResourceUsages: TObjectList<TResourceUsageCacheItem>read fResourceUsages;
    end;

    TResourceUsageSchemeCache = class
    private
        fList: TObjectList<TResourceUsageSchemeCacheItem>;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TResourceUsageSchemeCacheItem;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Add(aItem: TResourceUsageSchemeCacheItem);
        function FindByID(const aID: string; aMustFind: boolean): TResourceUsageSchemeCacheItem;
        property Items[aIndex: integer]: TResourceUsageSchemeCacheItem read GetItemAt; default;
        property Count: integer read GetCount;
    end;

    TVirtualResource = class
    protected
        fResName: string;
        fInUseByThreadID: cardinal;
        fPendingThreadIDs: TList<integer>;
        fUseCount: integer;
        procedure ResetInUseBy();
        property PendingThreadIDs: TList<integer>read fPendingThreadIDs;
    public
        constructor Create(const aResName: string);
        destructor Destroy(); override;
        procedure CancelPending(const aThreadID: cardinal);
        function CanAcquire(const aThreadID: cardinal): boolean;
        procedure Acquire(const aThreadID: cardinal);
        procedure Release();
        property ResName: string read fResName;
        property InUseByThreadID: cardinal read fInUseByThreadID write fInUseByThreadID;
        property UseCount: integer read fUseCount;
    end;

    TReleaseResourceMessageInfo = class(TMessageInfo)
    private
        fResourceName: string;
        fResourceType: TResourceType;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aResourceName: string; const aResourceType: TResourceType);
        property ResourceName: string read fResourceName;
        property ResourceType: TResourceType read fResourceType;
    end;

    TRequestAcquireResourceMessageInfo = class(TMessageInfo)
    private
        fResourceName: string;
        fResourceType: TResourceType;
        fLock: TLock;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aResourceName: string; const aResourceType: TResourceType;
            const aLock: TLock);
        property ResourceName: string read fResourceName;
        property ResourceType: TResourceType read fResourceType;
        property Lock: TLock read fLock;
    end;

    TProcessRequestsMessageInfo = class(TMessageInfo)
    protected
        function GetMessageID(): integer; override;
    end;

    TReleaseAllResourcesMessageInfo = class(TMessageInfo)
    private
        fResourceType: TResourceType;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aResourceType: TResourceType);
        property ResourceType: TResourceType read fResourceType;
    end;

    TResourceRequest = class
    protected
        fThreadID: cardinal;
        fWaitTillRequestProcessedLock: TLock;
    public
        constructor Create(const aThreadID: cardinal; const aWaitTillRequestProcessedLock: TLock);
        destructor Destroy(); override;
        property ThreadID: cardinal read fThreadID;
        property WaitTillRequestProcessedLock: TLock read fWaitTillRequestProcessedLock;
    end;

    TDeadLockReportItem = class
    private
        fBlockingThreadID: cardinal;
        fWaitingThreadID: cardinal;
        fBlockingThreadDescription: string;
        fWaitingThreadDescription: string;
        fResource: TVirtualResource;
    public
        constructor Create(const aBlockingThreadID: cardinal; const aBlockingThreadDescription: string;
            const aWaitingThreadID: cardinal; const aWaitingThreadDescription: string;
            aResource: TVirtualResource);
        property BlockingThreadID: cardinal read fBlockingThreadID write fBlockingThreadID;
        property BlockingThreadDescription: string read fBlockingThreadDescription;
        property WaitingThreadID: cardinal read fWaitingThreadID write fWaitingThreadID;
        property WaitingThreadDescription: string read fWaitingThreadDescription;
        property Resource: TVirtualResource read fResource write fResource;
    end;

    TDeadLockReportList = class(TIntegerKeyObjectValueList)
    private
        function GetItemAt(aIndex: integer): TDeadLockReportItem;
    public
        destructor Destroy(); override;
        procedure Add(aItem: TDeadLockReportItem);
        property Items[aIndex: integer]: TDeadLockReportItem read GetItemAt; default;
    end;

    TThreadResourceData = class
    private
        fTID: cardinal;
        fThreadSysHandleID: TSysHandleID;
        fThreadImage: TThreadImage;
        function GetResourcesPending: TResourceUsageList;
        function GetResourcesUsed: TResourceUsageList;
        function GetThreadDescription: string;
    public
        constructor Create(const aTID: cardinal);
        destructor Destroy(); override;
        property TID: cardinal read fTID;
        property ResourcesUsed: TResourceUsageList read GetResourcesUsed;
        property ResourcesPending: TResourceUsageList read GetResourcesPending;
        property ThreadDescription: string read GetThreadDescription;
    end;

    TResourceManagerRun = class(TResourceManager)
    private
        fVirtualResources: TObjectList<TVirtualResource>;
        fResourceUsageSchemeCache: TResourceUsageSchemeCache;
        fPendingResourceRequests: TObjectList<TResourceRequest>;
        class function FindByThreadID(aList: TObjectList<TResourceRequest>; const aThreadID: cardinal)
            : TResourceRequest;
        function FindByResName(const aResName: string): TVirtualResource;
        procedure InitVirtualResources;
        procedure ReadResourceUsageSchemes();

        procedure RegisterProcessRequestsMessage();
        procedure AddSchemeToPending(const aResourceRequest: TResourceRequest; const aSchemeID: string);
        procedure ReleaseScheme(const aThreadID: cardinal; const aSchemeID: string);
        function WhichThreadsAreBlockingThread(const aStopAtThreadID: cardinal;
            const aThreadResourceData: TThreadResourceData;
            const aDeadlockReport: TDeadlockReportList): boolean;
        procedure ShowDeadlockError(const aDeadlockReport: TDeadlockReportList);
        function DetectDeadlock(): boolean;
        procedure InternRequestAcquireResource(const aCallingThreadID: cardinal; const aLock: TLock;
            const aResourceName: string; const aResourceType: TResourceType);
        procedure InternReleaseResource(const aCallingThreadID: cardinal; const aResourceName: string;
            const aResourceType: TResourceType);
        procedure InternReleaseAllResources(const aCallingThreadID: cardinal;
            const aResourceType: TResourceType);
        procedure ProcessRequest(aResourceRequest: TResourceRequest);
        procedure ProcessRequests();
        procedure CancelRequest(aResourceRequest: TResourceRequest);
        procedure CancelRequests();
        function CanProcessRequest(aResourceRequest: TResourceRequest): boolean;
        procedure UpdateThreadResourceUsage(const aThreadResourceData: TThreadResourceData);
        procedure ProcessOrCancelRequests();
        procedure HandleRequestAcquireResource(const aMessage: TRequestAcquireResourceMessageInfo);
        procedure HandleReleaseResource(const aMessage: TReleaseResourceMessageInfo);
        procedure HandleProcessRequests(const aMessage: TProcessRequestsMessageInfo);
        procedure HandleReleaseAllResources(const aMessage: TReleaseAllResourcesMessageInfo);
    protected
        procedure SetOriginalMask(); override;
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); override;
    public
        constructor Create();
        destructor Destroy; override;
        procedure AcquireResource(const aResourceName: string; const aResourceType: TResourceType); override;
        procedure ReleaseResource(const aResourceName: string; const aResourceType: TResourceType); override;
        procedure ReleaseAllResources(const aResourceType: TResourceType); override;
    end;


implementation


uses
    Windows,
    SysUtils,
    ThreadAPI,
    ResSchemeDataAdaptor,
    ResourcesDataAdaptor,
    UtilLib,
    LogManager,
    ErrorManager,
    RunFlow,
    ErrorMessageFactory,
    ErrorInfo,
    AppTypes;

const
    INT_RESOURCE_ACQUIRE_TIMEOUT = INFINITE; // 2 hours

    cMessageReleaseResource = 9010;
    cMessageRequestAcquireResource = 9011;
    cMessageProcessRequests = 9012;
    cMessageReleaseAllResources = 9013;

    { TVirtualResource }

constructor TVirtualResource.Create(const aResName: string);
begin
    inherited Create();
    fPendingThreadIDs := TList<integer>.Create();
    fResName := aResName;
    ResetInUseBy();
    fUseCount := 0;
end;

destructor TVirtualResource.Destroy();
begin
    fPendingThreadIDs.Free;
    inherited;
end;

procedure TVirtualResource.ResetInUseBy();
begin
    fInUseByThreadID := cThreadIDNone;
end;

procedure TVirtualResource.Acquire(const aThreadID: cardinal);
var
    xWaitingThreadIDIndex: integer;
begin
    if self.InUseByThreadID <> aThreadID then
    begin
        ASSERT(self.UseCount = 0);
        // get index of this thread in PendingThreadIDs list
        xWaitingThreadIDIndex := self.PendingThreadIDs.IndexOf(aThreadID);
        if (xWaitingThreadIDIndex = 0) then
            self.PendingThreadIDs.Delete(xWaitingThreadIDIndex);
    end;

    fInUseByThreadID := aThreadID;
    Inc(fUseCount);

    gLogManager.LogF('Resource %s - Acquired, Count: %d', [self.ResName, self.UseCount], false);
end;

procedure TVirtualResource.Release;
begin
    Dec(fUseCount);
    if fUseCount = 0 then
        ResetInUseBy();

    gLogManager.LogF('Resource %s - Released, Count: %d', [self.ResName, self.UseCount], false);
end;

function TVirtualResource.CanAcquire(const aThreadID: cardinal): boolean;
var

    xWaitingThreadIDIndex: integer;
begin
    result := true;
    // this thread is already using this resource so return true
    if self.InUseByThreadID = aThreadID then
        EXIT;

    // this resource is in use by another thread so return false
    if self.UseCount > 0 then
    begin
        result := false;
        EXIT;
    end;

    // get index of this thread in PendingThreadIDs list
    xWaitingThreadIDIndex := self.PendingThreadIDs.IndexOf(aThreadID);

    // if there are entries in the WaitingThreaIDs list and this thread is not the first entry then we cannot acquire this resource
    if (self.PendingThreadIDs.Count > 0) and (xWaitingThreadIDIndex <> 0) then
    begin
        result := false;
        EXIT;
    end;

end;

procedure TVirtualResource.CancelPending(const aThreadID: cardinal);
// cancel if waiting for resource
var
    xWaitingThreadIDIndex: integer;
begin
    // this thread is using this resource so return true
    if self.InUseByThreadID = aThreadID then
        EXIT;

    // get index of this thread in PendingThreadIDs list
    xWaitingThreadIDIndex := self.PendingThreadIDs.IndexOf(aThreadID);

    if (xWaitingThreadIDIndex < 0) then
        EXIT;
    self.PendingThreadIDs.Delete(xWaitingThreadIDIndex);
end;

{ TResourceUsageSchemeCacheItem }

constructor TResourceUsageSchemeCacheItem.Create(const aSchemeID: string);
begin
    inherited Create();

    fSchemeID := aSchemeID;
    fResourceUsages := TObjectList<TResourceUsageCacheItem>.Create();
end;

destructor TResourceUsageSchemeCacheItem.Destroy;
begin
    fResourceUsages.Free;
    inherited;
end;

{ TResourceUsageSchemeCache }

constructor TResourceUsageSchemeCache.Create;
begin
    inherited Create();
    fList := TObjectList<TResourceUsageSchemeCacheItem>.Create(false); // kein OwnsObject: Memory leak???
end;

destructor TResourceUsageSchemeCache.Destroy;
begin
    fList.Free;
    inherited;
end;

procedure TResourceUsageSchemeCache.Add(aItem: TResourceUsageSchemeCacheItem);
begin
    fList.Add(aItem);
end;

function TResourceUsageSchemeCache.GetCount: integer;
begin
    result := fList.Count;
end;

function TResourceUsageSchemeCache.GetItemAt(aIndex: integer): TResourceUsageSchemeCacheItem;
begin
    result := fList[aIndex];
end;

function TResourceUsageSchemeCache.FindByID(const aID: string; aMustFind: boolean)
    : TResourceUsageSchemeCacheItem;
var
    x: integer;
begin
    for x := 0 to fList.Count - 1 do
    begin
        if (aID = fList[x].SchemeID) then
            EXIT(fList[x]);
    end;

    if aMustFind then
        raise Exception.CreateFmt('Resource Scheme %s could not be found', [aID]);

    EXIT(nil);
end;

{ TResourceUsageCacheItem }

constructor TResourceUsageCacheItem.Create(const aResID: string);
begin
    inherited Create();
    fResID := aResID;
end;

{ TResourceRequest }

constructor TResourceRequest.Create(const aThreadID: cardinal; const aWaitTillRequestProcessedLock: TLock);
begin
    inherited Create();
    fThreadID := aThreadID;
    fWaitTillRequestProcessedLock := aWaitTillRequestProcessedLock;
end;

destructor TResourceRequest.Destroy;
begin
    inherited;
end;

{ TDeadLockReportItem }

constructor TDeadLockReportItem.Create(const aBlockingThreadID: cardinal;
    const aBlockingThreadDescription: string; const aWaitingThreadID: cardinal;
    const aWaitingThreadDescription: string; aResource: TVirtualResource);
begin
    inherited Create();
    fBlockingThreadID := aBlockingThreadID;
    fBlockingThreadDescription := aBlockingThreadDescription;
    fWaitingThreadDescription := aWaitingThreadDescription;
    fWaitingThreadID := aWaitingThreadID;
    fResource := aResource;
end;

{ TDeadLockReportList }

procedure TDeadLockReportList.Add(aItem: TDeadLockReportItem);
begin
    self.AddObject(aItem.BlockingThreadID, aItem);
end;

destructor TDeadLockReportList.Destroy;
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
        self.Objects[x].Free;
    inherited;
end;

function TDeadLockReportList.GetItemAt(aIndex: integer): TDeadLockReportItem;
begin
    result := self.Objects[aIndex] as TDeadLockReportItem
end;

{ TReleaseResourceMessageInfo }

constructor TReleaseResourceMessageInfo.Create(const aResourceName: string;
    const aResourceType: TResourceType);
begin
    inherited Create();
    fResourceName := aResourceName;
    fResourceType := aResourceType;
end;

function TReleaseResourceMessageInfo.GetMessageID: integer;
begin
    result := cMessageReleaseResource;
end;

{ TRequestAcquireResourceMessageInfo }

constructor TRequestAcquireResourceMessageInfo.Create(const aResourceName: string;
    const aResourceType: TResourceType; const aLock: TLock);
begin
    inherited Create();
    fResourceName := aResourceName;
    fResourceType := aResourceType;
    fLock := aLock;
end;

function TRequestAcquireResourceMessageInfo.GetMessageID: integer;
begin
    result := cMessageRequestAcquireResource;
end;

{ TProcessRequestsMessageInfo }

function TProcessRequestsMessageInfo.GetMessageID: integer;
begin
    result := cMessageProcessRequests;
end;

{ TReleaseAllResourcesMessageInfo }

constructor TReleaseAllResourcesMessageInfo.Create(const aResourceType: TResourceType);
begin
    inherited Create();
    fResourceType := aResourceType;
end;

function TReleaseAllResourcesMessageInfo.GetMessageID: integer;
begin
    result := cMessageReleaseAllResources;
end;

{ TThreadResourceData }

constructor TThreadResourceData.Create(const aTID: cardinal);
begin
    inherited Create();
    fThreadSysHandleID := TThreadAPI.OpenThread(aTID);
    fThreadImage := TThreadAPI.GetThreadImage(fThreadSysHandleID);
    if not Assigned(fThreadImage) then
        raise Exception.CreateFmt('Thread %d not found', [aTID]);
end;

destructor TThreadResourceData.Destroy;
begin
    TThreadAPI.CloseSysHandle(fThreadSysHandleID);
    inherited;
end;

function TThreadResourceData.GetResourcesPending: TResourceUsageList;
begin
    result := fThreadImage.ResourcesPending;
end;

function TThreadResourceData.GetResourcesUsed: TResourceUsageList;
begin
    result := fThreadImage.ResourcesUsed;
end;

function TThreadResourceData.GetThreadDescription: string;
begin
    result := fThreadImage.Description;
end;

{ TResourceManagerRun }

constructor TResourceManagerRun.Create;
begin
    inherited;
    fVirtualResources := TObjectList<TVirtualResource>.Create();
    InitVirtualResources();

    fResourceUsageSchemeCache := TResourceUsageSchemeCache.Create;
    ReadResourceUsageSchemes();

    fPendingResourceRequests := TObjectList<TResourceRequest>.Create();
end;

destructor TResourceManagerRun.Destroy();
begin
    fPendingResourceRequests.Free;
    fResourceUsageSchemeCache.Free;
    fVirtualResources.Free;
    inherited;
end;

procedure TResourceManagerRun.InitVirtualResources();
var
    xDataAdaptor: TResourcesDataAdaptor;
    xResourcesArray: TResourceRecArray;
    i: integer;
begin
    xDataAdaptor := TResourcesDataAdaptor.Create();
    try
        xResourcesArray := xDataAdaptor.ReadAll();
    finally
        xDataAdaptor.Free;
    end;

    for i := 0 to high(xResourcesArray) do
    begin
        fVirtualResources.Add(TVirtualResource.Create(xResourcesArray[i].ResName));
    end;
end;

procedure TResourceManagerRun.ReadResourceUsageSchemes();
var
    xDataAdaptor: TResSchemeDataAdaptor;
    xSchemeArray: TArray<TResSchemeRec>;
    x: integer;
    xResourceUsageSchemeCacheItem: TResourceUsageSchemeCacheItem;
    xSchemeID: string;
begin
    xDataAdaptor := TResSchemeDataAdaptor.Create();
    try
        xSchemeArray := xDataAdaptor.ReadSchemes();
    finally
        xDataAdaptor.Free;
    end;

    for x := 0 to high(xSchemeArray) do
    begin
        xSchemeID := IntToStr(xSchemeArray[x].SchemeID);
        xResourceUsageSchemeCacheItem := self.fResourceUsageSchemeCache.FindByID(xSchemeID, false);
        if not Assigned(xResourceUsageSchemeCacheItem) then
        begin
            xResourceUsageSchemeCacheItem := TResourceUsageSchemeCacheItem.Create(xSchemeID);
            fResourceUsageSchemeCache.Add(xResourceUsageSchemeCacheItem);
        end;
        xResourceUsageSchemeCacheItem.ResourceUsages.Add
            (TResourceUsageCacheItem.Create(xSchemeArray[x].ResID));
    end;
end;

procedure TResourceManagerRun.AddSchemeToPending(const aResourceRequest: TResourceRequest;
    const aSchemeID: string);
var
    xResourceUsageSchemeCacheItem: TResourceUsageSchemeCacheItem;
    x: integer;
    xThreadResourcesPending: TResourceUsageList;
    xResID: string;
    xResource: TVirtualResource;
    xThreadResourceData: TThreadResourceData;
begin
    xThreadResourceData := TThreadResourceData.Create(aResourceRequest.ThreadID);
    try
        xThreadResourcesPending := xThreadResourceData.ResourcesPending;

        xResourceUsageSchemeCacheItem := self.fResourceUsageSchemeCache.FindByID(aSchemeID, true);
        for x := 0 to xResourceUsageSchemeCacheItem.ResourceUsages.Count - 1 do
        begin
            xResID := xResourceUsageSchemeCacheItem.ResourceUsages[x].ResID;
            xThreadResourcesPending.Add(TResourceUsage.Create(xResID, 0));
            xResource := FindByResName(xResID);
            if xResource.InUseByThreadID = aResourceRequest.ThreadID then
                CONTINUE;
            if xResource.PendingThreadIDs.IndexOf(aResourceRequest.ThreadID) >= 0 then
                CONTINUE;
            xResource.PendingThreadIDs.Add(aResourceRequest.ThreadID);
        end;
    finally
        FreeAndNil(xThreadResourceData);
    end;
end;

procedure TResourceManagerRun.ReleaseScheme(const aThreadID: cardinal; const aSchemeID: string);
var
    xResourceUsageSchemeCacheItem: TResourceUsageSchemeCacheItem;
    x: integer;
    xThreadResourcesUsed: TResourceUsageList;
    xResource: TVirtualResource;
    xThreadResourceData: TThreadResourceData;
begin
    xThreadResourceData := TThreadResourceData.Create(aThreadID);
    try
        xThreadResourcesUsed := xThreadResourceData.ResourcesUsed;

        xResourceUsageSchemeCacheItem := self.fResourceUsageSchemeCache.FindByID(aSchemeID, true);
        for x := 0 to xResourceUsageSchemeCacheItem.ResourceUsages.Count - 1 do
        begin
            xResource := FindByResName(xResourceUsageSchemeCacheItem.ResourceUsages[x].ResID);
            xResource.Release();
            xThreadResourcesUsed.RemoveByID(xResourceUsageSchemeCacheItem.ResourceUsages[x].ResID);
        end;
    finally
        FreeAndNil(xThreadResourceData);
    end;
end;

procedure TResourceManagerRun.RegisterProcessRequestsMessage();
begin
    self.RegisterMessageAndLeave(TProcessRequestsMessageInfo.Create());
end;

procedure TResourceManagerRun.InternRequestAcquireResource(const aCallingThreadID: cardinal;
    const aLock: TLock; const aResourceName: string; const aResourceType: TResourceType);
var
    xResourceRequest: TResourceRequest;

begin
    xResourceRequest := TResourceRequest.Create(aCallingThreadID, aLock);
    fPendingResourceRequests.Add(xResourceRequest);

    case aResourceType of
        rtResScheme:
            AddSchemeToPending(xResourceRequest, aResourceName);
        // rtDevice : AcquireDeviceResource( aResourceName );
        else
            raise Exception.Create('ResourceType not implemented');
    end;

    RegisterProcessRequestsMessage();
end;

procedure TResourceManagerRun.InternReleaseResource(const aCallingThreadID: cardinal;
    const aResourceName: string; const aResourceType: TResourceType);
begin
    case aResourceType of
        rtResScheme:
            ReleaseScheme(aCallingThreadID, aResourceName);
        // rtDevice : ReleaseDeviceResource( aResourceName );
        else
            raise Exception.Create('ResourceType not implemented');
    end;

    RegisterProcessRequestsMessage();
end;

procedure TResourceManagerRun.UpdateThreadResourceUsage(const aThreadResourceData: TThreadResourceData);
var
    xThreadResourcesUsed, xThreadResourcesPending: TResourceUsageList;
    x: integer;
begin
    xThreadResourcesUsed := aThreadResourceData.ResourcesUsed;
    xThreadResourcesPending := aThreadResourceData.ResourcesPending;
    for x := xThreadResourcesPending.Count - 1 downto 0 do
    begin
        xThreadResourcesUsed.Add(xThreadResourcesPending[x]);
        xThreadResourcesPending.Extract(x);
    end;
end;

procedure TResourceManagerRun.ProcessRequest(aResourceRequest: TResourceRequest);
var
    x: integer;
    xResourceUsage: TResourceUsage;
    xThreadResourcesPending: TResourceUsageList;
    xResource: TVirtualResource;
    xThreadResourceData: TThreadResourceData;
begin
    xThreadResourceData := TThreadResourceData.Create(aResourceRequest.ThreadID);
    try
        xThreadResourcesPending := xThreadResourceData.ResourcesPending;
        for x := 0 to xThreadResourcesPending.Count - 1 do
        begin
            xResourceUsage := xThreadResourcesPending[x];
            xResource := FindByResName(xResourceUsage.ResID);
            xResource.Acquire(aResourceRequest.ThreadID);
        end;

        UpdateThreadResourceUsage(xThreadResourceData);
    finally
        FreeAndNil(xThreadResourceData);
    end;
    aResourceRequest.WaitTillRequestProcessedLock.Unlock();
end;

function TResourceManagerRun.CanProcessRequest(aResourceRequest: TResourceRequest): boolean;
var
    x: integer;
    xResourceUsage: TResourceUsage;
    xThreadResourcesPending: TResourceUsageList;
    xResource: TVirtualResource;
    xThreadResourceData: TThreadResourceData;
begin
    xThreadResourceData := TThreadResourceData.Create(aResourceRequest.ThreadID);
    try
        xThreadResourcesPending := xThreadResourceData.ResourcesPending;
        result := true;
        for x := 0 to xThreadResourcesPending.Count - 1 do
        begin
            xResourceUsage := xThreadResourcesPending[x];
            xResource := FindByResName(xResourceUsage.ResID);
            if not xResource.CanAcquire(aResourceRequest.ThreadID) then
            begin
                result := false;
                EXIT;
            end;
        end;

    finally
        FreeAndNil(xThreadResourceData);
    end;
end;

procedure TResourceManagerRun.CancelRequest(aResourceRequest: TResourceRequest);
var
    x: integer;
    xResourceUsage: TResourceUsage;
    xThreadResourcesPending: TResourceUsageList;
    xResource: TVirtualResource;
    xThreadResourceData: TThreadResourceData;
begin
    xThreadResourceData := TThreadResourceData.Create(aResourceRequest.ThreadID);
    try
        xThreadResourcesPending := xThreadResourceData.ResourcesPending;
        for x := 0 to xThreadResourcesPending.Count - 1 do
        begin
            xResourceUsage := xThreadResourcesPending[x];
            xResource := FindByResName(xResourceUsage.ResID);
            xResource.CancelPending(aResourceRequest.ThreadID);
        end;
        xThreadResourcesPending.Clear();
    finally
        FreeAndNil(xThreadResourceData);
    end;

    aResourceRequest.WaitTillRequestProcessedLock.Unlock();
end;

function TResourceManagerRun.WhichThreadsAreBlockingThread(const aStopAtThreadID: cardinal;
    const aThreadResourceData: TThreadResourceData; const aDeadlockReport: TDeadlockReportList): boolean;
var
    xBlockingThreadID: cardinal;
    xResourceRequest: TResourceRequest;
    xResourceUsage: TResourceUsage;
    xResource: TVirtualResource;
    x: integer;
    xThreadResourcesPending: TResourceUsageList;
    xStopFound: boolean;
    xBlockingThreadResourceData: TThreadResourceData;
begin
    result := false;

    xResourceRequest := FindByThreadID(fPendingResourceRequests, aThreadResourceData.TID);
    if not Assigned(xResourceRequest) then
        EXIT;

    xThreadResourcesPending := aThreadResourceData.ResourcesPending;

    for x := 0 to xThreadResourcesPending.Count - 1 do
    begin
        xResourceUsage := xThreadResourcesPending[x];
        xResource := FindByResName(xResourceUsage.ResID);

        xBlockingThreadID := xResource.InUseByThreadID;

        xStopFound := xBlockingThreadID = aStopAtThreadID;

        if (not xStopFound) and (aDeadlockReport.IndexOf(xBlockingThreadID) >= 0) then
            CONTINUE;

        xBlockingThreadResourceData := TThreadResourceData.Create(xBlockingThreadID);
        try
            aDeadlockReport.Add(TDeadLockReportItem.Create(xBlockingThreadResourceData.TID,
                xBlockingThreadResourceData.ThreadDescription, aThreadResourceData.TID,
                aThreadResourceData.ThreadDescription, xResource));

            if xStopFound then
            begin
                result := true;
                EXIT;
            end;

            result := WhichThreadsAreBlockingThread(aStopAtThreadID, xBlockingThreadResourceData,
                aDeadlockReport);
        finally
            FreeAndNil(xBlockingThreadResourceData);
        end;

        if result then
            EXIT;
    end;
end;

procedure TResourceManagerRun.ShowDeadlockError(const aDeadlockReport: TDeadlockReportList);
var
    xErrorInfo: TErrorInfo;
    xBlockingThreadDescription, xThreadDescription: string;
    x: integer;
    xDeadLockReportItem: TDeadlockReportItem;
begin
    xErrorInfo := TErrorInfo.Create();
    try
        xErrorInfo.Init('A Resource deadlock was detected', 'Resource Deadlock Detected', eibAbort);
        for x := 0 to aDeadlockReport.Count - 1 do
        begin
            xDeadLockReportItem := aDeadlockReport.Items[x];
            xThreadDescription := xDeadLockReportItem.WaitingThreadDescription;
            xBlockingThreadDescription := xDeadLockReportItem.BlockingThreadDescription;
            xErrorInfo.AddText(Format('"%s" is waiting for resource "%s" blocked by "%s"',
                [xThreadDescription, xDeadLockReportItem.Resource.ResName, xBlockingThreadDescription]));
        end;
        gErrorMessageFactory.ShowAnyError(xErrorInfo);
    finally
        xErrorInfo.Free;
    end;
end;

function TResourceManagerRun.DetectDeadlock(): boolean;
var
    x: integer;
    xResourceRequest: TResourceRequest;
    xThreadID: cardinal;
    xDeadlockReport: TDeadlockReportList;
    xDeadLockFound: boolean;
    xThreadResourceData: TThreadResourceData;
begin
    result := false;
    xDeadlockReport := TDeadlockReportList.Create();
    try
        for x := 0 to fPendingResourceRequests.Count - 1 do
        begin
            xResourceRequest := fPendingResourceRequests[x];
            xThreadID := xResourceRequest.ThreadID;
            xDeadlockReport.Clear();

            xThreadResourceData := TThreadResourceData.Create(xThreadID);
            try
                xDeadLockFound := WhichThreadsAreBlockingThread(xThreadID, xThreadResourceData,
                    xDeadlockReport);
            finally
                FreeAndNil(xThreadResourceData);
            end;

            if xDeadLockFound then
            begin
                ShowDeadlockError(xDeadlockReport);
                gErrorManager.SetGlobalErr(ERR_USER);
                result := true;
                EXIT;
            end;
        end;
    finally
        xDeadlockReport.Free;
    end;
end;

function TResourceManagerRun.FindByResName(const aResName: string): TVirtualResource;
var
    x: integer;
begin
    for x := 0 to fVirtualResources.Count - 1 do
    begin
        if (fVirtualResources[x].ResName = aResName) then
            EXIT(fVirtualResources[x]);
    end;

    raise Exception.Create('Resource [' + aResName + '] not found');
end;

class function TResourceManagerRun.FindByThreadID(aList: TObjectList<TResourceRequest>;
    const aThreadID: cardinal): TResourceRequest;
var
    x: integer;
    xItem: TResourceRequest;
begin
    result := nil;
    for x := 0 to aList.Count - 1 do
    begin
        xItem := aList[x];
        if xItem.ThreadID = aThreadID then
        begin
            result := xItem;
            EXIT;
        end;
    end;
end;

procedure TResourceManagerRun.ProcessRequests();
var
    x: integer;
    xResourceRequest: TResourceRequest;
begin
    for x := fPendingResourceRequests.Count - 1 downto 0 do
    begin
        xResourceRequest := fPendingResourceRequests[x];
        if not CanProcessRequest(xResourceRequest) then
            CONTINUE;
        ProcessRequest(xResourceRequest);
        fPendingResourceRequests.Delete(x);
    end;

    if DetectDeadlock() then
    begin
        CancelRequests();
    end;

end;

procedure TResourceManagerRun.CancelRequests();
var
    x: integer;
    xResourceRequest: TResourceRequest;
begin
    for x := fPendingResourceRequests.Count - 1 downto 0 do
    begin
        xResourceRequest := fPendingResourceRequests[x];
        CancelRequest(xResourceRequest);
        fPendingResourceRequests.Delete(x);
    end;
end;

procedure TResourceManagerRun.ProcessOrCancelRequests();
begin
    if gErrorManager.IsGlobalErr then
        self.CancelRequests()
    else
        self.ProcessRequests();
end;

procedure TResourceManagerRun.InternReleaseAllResources(const aCallingThreadID: cardinal;
    const aResourceType: TResourceType);
var
    x: integer;
    xThreadResourcesUsed: TResourceUsageList;
    xResource: TVirtualResource;
    xThreadResourceData: TThreadResourceData;
begin
    xThreadResourceData := TThreadResourceData.Create(aCallingThreadID);
    try
        xThreadResourcesUsed := xThreadResourceData.ResourcesUsed;
        for x := xThreadResourcesUsed.Count - 1 downto 0 do
        begin
            xResource := FindByResName(xThreadResourcesUsed[x].ResID);
            xResource.Release();
            xThreadResourcesUsed.RemoveByID(xThreadResourcesUsed[x].ResID);
        end;
    finally
        FreeAndNil(xThreadResourceData);
    end;
    RegisterProcessRequestsMessage();
end;

procedure TResourceManagerRun.HandleRequestAcquireResource(const aMessage
    : TRequestAcquireResourceMessageInfo);
begin
    self.InternRequestAcquireResource(aMessage.CallingThreadID, aMessage.Lock, aMessage.ResourceName,
        aMessage.ResourceType);
end;

procedure TResourceManagerRun.HandleReleaseResource(const aMessage: TReleaseResourceMessageInfo);
begin
    self.InternReleaseResource(aMessage.CallingThreadID, aMessage.ResourceName, aMessage.ResourceType);
end;

procedure TResourceManagerRun.HandleProcessRequests(const aMessage: TProcessRequestsMessageInfo);
begin
    self.ProcessOrCancelRequests();
end;

procedure TResourceManagerRun.HandleReleaseAllResources(const aMessage: TReleaseAllResourcesMessageInfo);
begin
    self.InternReleaseAllResources(aMessage.CallingThreadID, aMessage.ResourceType);
end;

procedure TResourceManagerRun.SetOriginalMask();
begin
    fMessageQueue.SetMask([INT_MESSAGE_EXECUTABLE_TERMINATE, INT_MESSAGE_EXECUTABLE_PAUSE,
        cMessageReleaseAllResources, cMessageReleaseResource, cMessageRequestAcquireResource,
        cMessageProcessRequests]);
end;

procedure TResourceManagerRun.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
    inherited;
    if aMessage.MessageInfo is TReleaseResourceMessageInfo then
    begin
        HandleReleaseResource(aMessage.MessageInfo as TReleaseResourceMessageInfo);
    end
    else if aMessage.MessageInfo is TRequestAcquireResourceMessageInfo then
    begin
        HandleRequestAcquireResource(aMessage.MessageInfo as TRequestAcquireResourceMessageInfo);
    end
    else if aMessage.MessageInfo is TProcessRequestsMessageInfo then
    begin
        HandleProcessRequests(aMessage.MessageInfo as TProcessRequestsMessageInfo);
    end
    else if aMessage.MessageInfo is TReleaseAllResourcesMessageInfo then
    begin
        HandleReleaseAllResources(aMessage.MessageInfo as TReleaseAllResourcesMessageInfo);
    end;

end;

procedure TResourceManagerRun.AcquireResource(const aResourceName: string;
    const aResourceType: TResourceType);
var
    xMessageInfo: TRequestAcquireResourceMessageInfo;
    xLock: TLock;
begin
    xLock := TSimpleLock.Create(true, false, true);
    try
        xMessageInfo := TRequestAcquireResourceMessageInfo.Create(aResourceName, aResourceType, xLock);
        try
            self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
            xLock.WaitForLock(INFINITE);
        finally
            xMessageInfo.Free;
        end;
    finally
        xLock.Free;
    end;

end;

procedure TResourceManagerRun.ReleaseResource(const aResourceName: string;
    const aResourceType: TResourceType);
begin
    self.RegisterMessageAndWait(TReleaseResourceMessageInfo.Create(aResourceName, aResourceType), false,
        INFINITE);
end;

procedure TResourceManagerRun.ReleaseAllResources(const aResourceType: TResourceType);
var
    xMessageInfo: TReleaseAllResourcesMessageInfo;
begin
    xMessageInfo := TReleaseAllResourcesMessageInfo.Create(aResourceType);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
    finally
        xMessageInfo.Free;
    end;
end;


end.
