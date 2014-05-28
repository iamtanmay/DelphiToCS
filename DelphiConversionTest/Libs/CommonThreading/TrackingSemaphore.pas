{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  15.08.13 wl                                      TN6223   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit TrackingSemaphore;


interface


uses
    SyncObjs,
    ThreadClasses,
    LockHandle;

type
    TSizableArray = class
    protected
        fCount: integer;
        fCapacity: integer;
        fArray: array of Pointer;
        fStartingCapacity: integer;
        function GetItem(aIndex: integer): Pointer;
        procedure SetItem(aIndex: integer; aItem: Pointer);
    public
        constructor Create(aStartingCapacity: integer);
        procedure SetArrayLength(aCapacity: integer);
        procedure Grow();
        procedure Clear();
        function IsFull(): boolean;
        function IsEmpty(): boolean;
        property Count: integer read fCount write fCount;
        property Capacity: integer read fCapacity;
        property this[aIndex: integer]: Pointer read GetItem write SetItem; default;
    end;

    TWaitQueue = class
    private const
        INT_QUEUE_DEF_CAPACITY = 4;
    private
        fArray: TSizableArray;
        fFront: integer;
        fBack: integer;
        fOwnsObjects: boolean; // not implemented!
        procedure Increment(var aValue: integer);
        function GetCount(): integer;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Clear();
        function Peek(): TObject;
        function Dequeue(): TObject;
        procedure Enqueue(aObject: TObject);

        property Count: integer read GetCount;
        property OwnsObjects: boolean read fOwnsObjects write fOwnsObjects;
    end;

    TSemaphoreEnteredItem = record
        ID: integer;
        Count: integer;
    end;

    TSemaphoreQueueItem = class
    private
        fID: integer;
        fWaiting: boolean;
    public
        constructor Create(aID: integer);
        property ID: integer read fID write fID;
        property Waiting: boolean read fWaiting write fWaiting;
    end;

    TTrackingSemaphore = class
    private
        fEnteredArrayCount: integer;
        fDoDebug: boolean;
        procedure EnteredArrayAddID(aIndex, aID: integer);
        procedure EnteredArrayDeleteID(aIndex: integer);
        procedure EnteredArraySetAt(aIndex, aID, aCount: integer);
        function EnteredArrayIndexOf(aID: integer): integer;
        function EnteredArrayNextAvailIndex(): integer;
        procedure DebugLog(const aText: string);
    protected
        fInitialized: integer;
        fCriticalSection: TCriticalSection;
        fEnteredArray: array of TSemaphoreEnteredItem;
        fWaitQueue: TWaitQueue;
        fLock: TSimpleLock;
        fMaxCount: integer;
        procedure Entering();
        procedure Leaving();
        function DoWait(aWaitTime: cardinal): TWaitResult;
        procedure DoInitialize();
        procedure Initialize();
        function AttemptEnter(aID: integer): boolean;
        function RemoveID(aID: integer): boolean;
        function PromotionAllowed(aID: integer): boolean;
        function GetNextWaitingItem(): TSemaphoreQueueItem;
        procedure PromoteNextWatingItem(aAvailableIndex: integer);
        procedure AttemptPromoteItem(aQueueItem: TSemaphoreQueueItem);

    public
        constructor Create(aMaxCount: integer);
        destructor Destroy(); override;
        function Enter(aWaitTime: cardinal = INFINITE): boolean;
        procedure Leave();
    end;


implementation


uses
    Windows,
    SysUtils,
    ThreadRegistry;

{ TSizableArray }

constructor TSizableArray.Create(aStartingCapacity: integer);
begin
    inherited Create();
    fStartingCapacity := aStartingCapacity;
    Clear();
end;

function TSizableArray.GetItem(aIndex: integer): Pointer;
begin
    result := fArray[aIndex];
end;

procedure TSizableArray.SetItem(aIndex: integer; aItem: Pointer);
begin
    fArray[aIndex] := aItem;
end;

function TSizableArray.IsFull(): boolean;
begin
    result := (fCount = fCapacity);
end;

function TSizableArray.IsEmpty(): boolean;
begin
    result := (fCount = 0);
end;

procedure TSizableArray.SetArrayLength(aCapacity: integer);
begin
    SetLength(fArray, aCapacity);
    fCapacity := aCapacity;
end;

procedure TSizableArray.Grow();
var
    xDelta: integer;
begin
    if fCapacity > 64 then
        xDelta := fCapacity div 4
    else if fCapacity > 8 then
        xDelta := 16
    else
        xDelta := 4;

    SetArrayLength(fCapacity + xDelta);

end;

procedure TSizableArray.Clear();
begin
    SetArrayLength(fStartingCapacity);
    fCount := 0;
end;

{ TWaitQueue }

constructor TWaitQueue.Create();
begin
    inherited Create();
    fArray := TSizableArray.Create(INT_QUEUE_DEF_CAPACITY);
    Clear();
end;

destructor TWaitQueue.Destroy();
begin
    FreeAndNil(fArray);
    inherited;
end;

procedure TWaitQueue.Increment(var aValue: integer);
begin
    Inc(aValue);
    if aValue = fArray.Capacity then
        aValue := 0;
end;

procedure TWaitQueue.Enqueue(aObject: TObject);
begin
    if fArray.IsFull then
        fArray.Grow();
    self.Increment(fBack);
    fArray[fBack] := aObject;
    fArray.Count := fArray.Count + 1;
end;

function TWaitQueue.Peek(): TObject;
begin
    if fArray.IsEmpty() then
        result := nil
    else
        result := fArray[fFront];
end;

function TWaitQueue.Dequeue(): TObject;
begin
    result := Peek();
    if not Assigned(result) then
        Exit;
    self.Increment(fFront);
    fArray.Count := fArray.Count - 1;
end;

procedure TWaitQueue.Clear();
begin
    fArray.Clear();
    fBack := -1;
    fFront := 0;
end;

function TWaitQueue.GetCount(): integer;
begin
    result := fArray.Count;
end;

{ TSemaphoreQueueItem }

constructor TSemaphoreQueueItem.Create(aID: integer);
begin
    inherited Create();
    fID := aID;
    fWaiting := true;
end;

{ TTrackingSemaphore }

// PK: Using the windows Semaphore does not give us much control.
// With this semaphore we can keep track of exactly which threads are waiting
// Also, if there is no contention, we only use a critical section and an event instead of a windows semaphore
const
    INT_SEMAPHORE_INITIALIZED: integer = 1;
    INT_SEMAPHORE_NOT_INITIALIZED: integer = 0;

constructor TTrackingSemaphore.Create(aMaxCount: integer);
begin
    inherited Create;
    fMaxCount := aMaxCount;
    fInitialized := INT_SEMAPHORE_NOT_INITIALIZED;
    fEnteredArrayCount := 0;
    fDoDebug := false;
end;

destructor TTrackingSemaphore.Destroy();
begin
    fCriticalSection.Free;
    fWaitQueue.Free;
    fLock.Free;
    inherited;
end;

procedure TTrackingSemaphore.DebugLog(const aText: string);
begin
    if not fDoDebug then
        EXIT;
    gmDebugLog(Format('TTrackingSemaphore %d - %s', [fLock.Handle, aText]));
end;

procedure TTrackingSemaphore.DoInitialize();
var
    x: integer;
begin
    fCriticalSection := TCriticalSection.Create();

    SetLength(fEnteredArray, fMaxCount);
    for x := 0 to high(fEnteredArray) do
        fEnteredArray[x].Count := 0;

    fWaitQueue := TWaitQueue.Create();
    fWaitQueue.OwnsObjects := true;

    fLock := TSimpleLock.Create(true, false, true);

end;

procedure TTrackingSemaphore.Initialize();
begin
    // If fInitialized is already set then EXIT
    // If fInitialized is not set then set it and dont EXIT
    // We need to read and set finitialized with one CPU instruction to avoid more than one thread initializing
    if Integer(InterlockedCompareExchange(fInitialized, INT_SEMAPHORE_INITIALIZED,
        INT_SEMAPHORE_NOT_INITIALIZED)) = INT_SEMAPHORE_INITIALIZED then
        EXIT;
    DoInitialize();
end;

function TTrackingSemaphore.EnteredArrayIndexOf(aID: integer): integer;
var
    x: integer;
begin
    result := -1;
    for x := 0 to high(fEnteredArray) do
    begin
        if (fEnteredArray[x].Count > 0) and (fEnteredArray[x].ID = aID) then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

procedure TTrackingSemaphore.EnteredArraySetAt(aIndex: integer; aID: integer; aCount: integer);
begin
    fEnteredArray[aIndex].ID := aID;
    fEnteredArray[aIndex].Count := aCount;
end;

procedure TTrackingSemaphore.EnteredArrayAddID(aIndex: integer; aID: integer);
begin
    EnteredArraySetAt(aIndex, aID, 1);
    Inc(fEnteredArrayCount);

end;

procedure TTrackingSemaphore.EnteredArrayDeleteID(aIndex: integer);
begin
    EnteredArraySetAt(aIndex, 0, 0);
    Dec(fEnteredArrayCount);

end;

function TTrackingSemaphore.EnteredArrayNextAvailIndex(): integer;
var
    x: integer;
begin
    result := -1;
    for x := 0 to high(fEnteredArray) do
    begin
        if fEnteredArray[x].Count = 0 then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

function TTrackingSemaphore.AttemptEnter(aID: integer): boolean;
var
    xAvailableIndex: integer;
begin
    xAvailableIndex := EnteredArrayNextAvailIndex();
    result := xAvailableIndex >= 0;
    if result then
    begin
        EnteredArrayAddID(xAvailableIndex, aID);
    end
end;

function TTrackingSemaphore.PromotionAllowed(aID: integer): boolean;
var
    xThreadImage: TThreadImage;
begin

    result := true;
    if fLock.CanSafeSuspend then
    begin
        xThreadImage := TThreadRegistry.Instance.FindThreadImageByThreadID(aID);
        if Assigned(xThreadImage) then
        begin
            result := not xThreadImage.Thread.IsSafeSuspended;
        end;
    end;
end;

function TTrackingSemaphore.GetNextWaitingItem(): TSemaphoreQueueItem;
var
    xQueueItem: TSemaphoreQueueItem;
    x: integer;
    xQueueCount: integer;
begin
    // promote next queueitem to the entered list
    result := nil;
    xQueueCount := fWaitQueue.Count;

    for x := 0 to xQueueCount - 1 do
    begin
        xQueueItem := fWaitQueue.Dequeue as TSemaphoreQueueItem;
        if xQueueItem = nil then
            EXIT;
        if PromotionAllowed(xQueueItem.ID) then
        begin
            result := xQueueItem;
            EXIT;
        end;
        DebugLog(Format('Did not promote queueitem %d', [xQueueItem.ID]));
        fWaitQueue.Enqueue(xQueueItem);
    end;
end;

procedure TTrackingSemaphore.PromoteNextWatingItem(aAvailableIndex: integer);
var
    xQueueItem: TSemaphoreQueueItem;
begin
    xQueueItem := GetNextWaitingItem();
    if xQueueItem = nil then
        EXIT;
    xQueueItem.Waiting := false;
    DebugLog(Format('Promote queueitem %d', [xQueueItem.ID]));
    EnteredArrayAddID(aAvailableIndex, xQueueItem.ID);
end;

procedure TTrackingSemaphore.AttemptPromoteItem(aQueueItem: TSemaphoreQueueItem);
var
    xAvailableIndex: integer;
begin
    xAvailableIndex := EnteredArrayNextAvailIndex();
    if xAvailableIndex < 0 then
        EXIT;

    if aQueueItem <> fWaitQueue.Peek then
        EXIT;

    PromoteNextWatingItem(xAvailableIndex);
end;

function TTrackingSemaphore.RemoveID(aID: integer): boolean;
var
    xIndex: integer;

begin
    result := false;
    xIndex := EnteredArrayIndexOf(aID);
    if xIndex < 0 then
        EXIT;
    Dec(fEnteredArray[xIndex].Count);
    if fEnteredArray[xIndex].Count > 0 then
        EXIT;

    // mark item as removed
    result := true;
    EnteredArrayDeleteID(xIndex);
    PromoteNextWatingItem(xIndex);
end;

procedure TTrackingSemaphore.Entering();
begin
    DebugLog('Entering - BEGIN');
    if fEnteredArrayCount = fMaxCount then
    begin
        fLock.Lock;
    end;
    DebugLog('Entering - END');
end;

procedure TTrackingSemaphore.Leaving();
begin
    DebugLog('Leaving - BEGIN');
    fLock.Unlock;
    DebugLog('Leaving - END');
end;

function TTrackingSemaphore.DoWait(aWaitTime: cardinal): TWaitResult;
begin
    DebugLog('DoWait - BEGIN');
    result := fLock.WaitForLock(aWaitTime);
    DebugLog('DoWait - END');
end;

function TTrackingSemaphore.Enter(aWaitTime: cardinal): boolean;
var
    xID, xIndex: integer;
    xWaitResult: TWaitResult;
    xQueueItem: TSemaphoreQueueItem;
begin
    result := true;

    xID := TThreadRegistry.Instance.FindCurrentThreadID();
    ASSERT(xID <> cHandleIDNone, 'CurrentThreadID = 0');
    Initialize();
    fCriticalSection.Acquire();
    try
        xIndex := EnteredArrayIndexOf(xID);
        if xIndex >= 0 then
        begin
            Inc(fEnteredArray[xIndex].Count);
            EXIT;
        end;

        if AttemptEnter(xID) then
        begin
            Entering();
            EXIT;
        end;

        xQueueItem := TSemaphoreQueueItem.Create(xID);
        DebugLog('Add to queue');
        fWaitQueue.Enqueue(xQueueItem);
    finally
        fCriticalSection.Release();
    end;

    while true do
    begin
        xWaitResult := DoWait(aWaitTime);
        if xWaitResult in [wrError, wrAbandoned, wrTimeout] then
        begin
            result := false;
            EXIT;
        end;
        fCriticalSection.Acquire();
        try
            DebugLog('Check queue');
            // it is possible that this queueItem was not promoted by the last leaving item because this queueitem was suspended
            if xQueueItem.Waiting then
                AttemptPromoteItem(xQueueItem);

            if not xQueueItem.Waiting then
            begin
                xQueueItem.Free;
                Entering();
                DebugLog('Ended Waiting');
                EXIT;
            end;
        finally
            fCriticalSection.Release();
        end;
    end;
end;

procedure TTrackingSemaphore.Leave();
var
    xID: integer;
begin
    ASSERT(fInitialized = INT_SEMAPHORE_INITIALIZED, 'Semaphore not intialized');
    xID := TThreadRegistry.Instance.FindCurrentThreadID();
    ASSERT(xID <> cHandleIDNone, 'CurrentThreadID = 0');
    fCriticalSection.Acquire();
    try
        if RemoveID(xID) then
            Leaving;
    finally
        fCriticalSection.Release();
    end;
end;


end.
