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

unit MessageQueue;


interface


uses
    Windows,
    Classes,
    SyncObjs,
    SysUtils,
    Generics.Collections,
    Generics.Defaults,
    InterfacedNoRef,
    LockHandle,
    ThreadClasses,
    Executable,
    MemoryClasses,
    ClockClass;

const
    INT_MESSAGE_QUIT = 0;

type
    TQuitMessageInfo = class(TMessageInfo)
    protected
        function GetMessageID(): integer; override;
    end;

    TMessageQueueItem = class;
    TMessageProc = procedure(var aAccept: boolean; aMessageQueueItem: TMessageQueueItem) of object;

    TMessageQueueItem = class
    protected
        fLock: TLock;
        fPriority: integer;
        fMessageType: integer;
        fTriggerWhenHandled: boolean;
        fArgs: TMsgArgs;
        fSeparateThread: boolean;
        fOnHandle: TMessageProc;
        fOwnsArgs: boolean;
        fMessageInfo: TMessageInfo;
        fOwnsMessageInfo: boolean;
    public
        constructor Create(aMessageType: integer; aPriority: integer; aLock: TLock; aSeparateThread: boolean;
            aHandler: TMessageProc; aArgs: TMsgArgs; aOwnsArgs: boolean); overload;

        constructor Create(const aMessageInfo: TMessageInfo; aLock: TLock; aSeparateThread: boolean;
            aHandler: TMessageProc; const aOwnsMessageInfo: boolean); overload;

        destructor Destroy(); override;
        function MustTriggerWhenHandled(): boolean;
        procedure DoOnHandle();
        procedure TriggerLock();
        function Log(): string;
        property MessageType: integer read FMessageType;
        property Lock: TLock read FLock;
        property TriggerWhenHandled: boolean read FTriggerWhenHandled write FTriggerWhenHandled;
        property OnHandle: TMessageProc read FOnHandle write FOnHandle;
        property Args: TMsgArgs read fArgs write fArgs;
        property Priority: integer read FPriority write FPriority;
        property SeparateThread: boolean read fSeparateThread;
        property MessageInfo: TMessageInfo read fMessageInfo;
    end;

    TTimedMessageQueueItem = class(TMessageQueueItem)
    private
        fTime: DWORD;
    public
        constructor Create(aMessageType: integer; aPriority: integer; aLock: TLock; aSeparateThread: boolean;
            aHandler: TMessageProc; aTime: DWORD; aArgs: TMsgArgs; aOwnsArgs: boolean);
        property Time: DWORD read fTime;
    end;

    TMessageQueueCountChangedReason = (qcrEnqueued, qcrDequeued, qcrOther);
    TMessageQueueCountChangedEvent = procedure(aSender: TObject; aReason: TMessageQueueCountChangedReason)
        of object;

    TMessageQueueComparer = class(TInterfacedObject, IComparer<TMessageQueueItem>)
        function Compare(const aItem1, aItem2: TMessageQueueItem): integer;
    end;

    TMessageQueue = class(TInterfacedNoRef, IExecutable)
    protected
        fThread: TThread;
        FLock: TLock;
        FQueue: TObjectList<TMessageQueueItem>;
        FCriticalSection: TCriticalSection;
        fIdleTime: DWORD;
        fOnCountChanged: TMessageQueueCountChangedEvent;
        function IsQuitCondition(aMessageQueueItem: TMessageQueueItem): boolean; virtual;
        function IsEmpty(): boolean;
        procedure RefreshLock(); virtual;
        function DequeueMessage(aMessageQueueItem: TMessageQueueItem): boolean; virtual;
        function DequeueMessageByIndex(aQueueIndex: integer): boolean;
        function Peek(): TMessageQueueItem;
        function GetCount(): integer; virtual;
        procedure BeforeAddMessage(aMessageQueueItem: TMessageQueueItem); virtual;
        procedure AfterAddMessage(aMessageQueueItem: TMessageQueueItem; aQueueIndex: integer); virtual;
        procedure AfterDeleteMessage(aMessageQueueItem: TMessageQueueItem; aQueueIndex: integer); virtual;
        procedure Idle(); virtual;
        procedure Sort(); virtual;
        procedure SetThread(aThread: TThread);
        procedure QueueCountChanged(aReason: TMessageQueueCountChangedReason);
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Execute(); virtual;
        procedure HandleAllMessages();
        function Next(): TMessageQueueItem; virtual;
        procedure HandleMessage(aMessageQueueItem: TMessageQueueItem);
        function Log(): string;
        function GetItem(aIndex: integer): TMessageQueueItem;
        procedure EnqueueMessage(aMessageQueueItem: TMessageQueueItem);
        function DequeueMessageByType(aMessageType: integer): boolean;
        procedure Terminate(); virtual;
        property OnCountChanged: TMessageQueueCountChangedEvent read fOnCountChanged write fOnCountChanged;
        property Lock: TLock read FLock write FLock;
        property Count: integer read GetCount;
    end;

    TMaskedMessageQueueEditMask = (emSet, emAdd, emDel, emIns);

const
    INST_MESSAGE_QUEUE_ITEM_PRIORITY_UNASSIGNED = MaxInt;

type
    TMaskedMessageQueue = class(TMessageQueue)
    private
        fAllowedMessageTypes: TIntMap;
        fSavedMask: TArray<integer>;
    protected
        procedure RefreshItemPriority(aItem: TMessageQueueItem);
        procedure Sort(); override;
        function GetCount(): integer; override;
        procedure BeforeAddMessage(aMessageQueueItem: TMessageQueueItem); override;
        procedure MaskChanged();
        procedure EditMask(aEditType: TMaskedMessageQueueEditMask; aParam: integer;
            const aMsgTypes: TArray<integer>; aSingleEdit: boolean = true);
    public
        constructor Create();
        destructor Destroy(); override;
        procedure SaveMask(aSingleEdit: boolean);
        procedure RestoreMask();
        procedure ChangeMaskTemp(const aDeleteMsgTypes, aInsertMsgTypes: array of integer;
            aInsertAt: integer);
        procedure SetMaskTemp(const aTempMask: array of integer);
        procedure SetMask(const aMsgTypes: array of integer; aSingleEdit: boolean = true);
        procedure AddToMask(const aMsgTypes: array of integer; aSingleEdit: boolean = true);
        procedure InsertToMask(aInsertAt: integer; const aMsgTypes: array of integer;
            aSingleEdit: boolean = true);
        procedure DeleteFromMask(const aMsgTypes: array of integer; aSingleEdit: boolean = true);
        procedure Acquire();
        procedure Release();
        class function MaskArgsToMaskArray(const aMsgTypes: array of integer): TArray<integer>;
        class function ConcatMaskArrays(const aMsgTypes1, aMsgTypes2: TArray<integer>): TArray<integer>;
    end;

    TTimedMessageQueue = class(TMessageQueue)
    private
        fClock: TClock;
        fFirstMessageChanged: boolean;
    protected
        procedure Idle(); override;
        procedure AfterAddMessage(aMessageQueueItem: TMessageQueueItem; aQueueIndex: integer); override;
        procedure AfterDeleteMessage(aMessageQueueItem: TMessageQueueItem; aQueueIndex: integer); override;
        procedure RefreshLock(); override;
        procedure QueueChanged(aQueueIndex: integer);
    public
        constructor Create(aClock: TClock);
        destructor Destroy(); override;

    end;

    TMessageQueueItemThread = class(TThread)
    protected
        fMessageQueueItem: TMessageQueueItem;
    public
        constructor Create(aMessageQueueItem: TMessageQueueItem);
        procedure Execute; override;
    end;


implementation


{ TQuitMessageInfo }

function TQuitMessageInfo.GetMessageID(): integer;
begin
    result := INT_MESSAGE_QUIT;
end;

{ TMessageQueueComparer }

function TMessageQueueComparer.Compare(const aItem1, aItem2: TMessageQueueItem): integer;
begin
    result := aItem1.Priority - aItem2.Priority;
end;

{ TMessageQueue }

constructor TMessageQueue.Create();
begin
    inherited Create();
    fQueue := TObjectList<TMessageQueueItem>.Create(false);
    fLock := TSimpleLock.Create(true, false, true);
    fIdleTime := INFINITE;
    FCriticalSection := TCriticalSection.Create();

end;

destructor TMessageQueue.Destroy();
var
    i: integer;
begin
    FCriticalSection.Free;
    for i := 0 to fQueue.Count - 1 do
    begin
        fQueue[i].Free;
    end;
    fQueue.Free;
    FLock.Free;
    inherited Destroy();
end;

procedure TMessageQueue.Terminate();
begin

end;

function TMessageQueue.Log(): string;
var
    i: integer;
    function NextLog(aIndex: integer): string;
    begin
        result := (fQueue[aIndex] as TMessageQueueItem).Log;
    end;

begin
    if fQueue.Count > 0 then
        result := NextLog(0)
    else
        result := '';

    for i := 1 to fQueue.Count - 1 do
    begin
        result := result + ' ; ' + NextLog(i);
    end;

    result := '[' + result + ']';

end;

procedure TMessageQueue.SetThread(aThread: TThread);
begin
    fThread := aThread;
end;

procedure TMessageQueue.Idle();
begin
    FLock.WaitForLock(fIdleTime);
end;

procedure TMessageQueue.HandleAllMessages();
var
    xMessage: TMessageQueueItem;
begin
    while true do
    begin
        if self.IsEmpty then
            EXIT;
        xMessage := self.Next();
        if IsQuitCondition(xMessage) then
            EXIT;

        self.HandleMessage(xMessage);
        Sleep(1); // Take a break to Give other threads a chance to register higher priority messages
    end;
end;

procedure TMessageQueue.Execute();
var
    xMessage: TMessageQueueItem;
begin
    while true do
    begin
        Idle();
        xMessage := self.Next();
        if IsQuitCondition(xMessage) then
        begin
            self.DequeueMessage(xMessage);
            EXIT;
        end;

        self.HandleMessage(xMessage);
        Sleep(1); // Take a break to Give other threads a chance to register higher priority messages
    end;
end;

function TMessageQueue.IsQuitCondition(aMessageQueueItem: TMessageQueueItem): boolean;
begin
    result := aMessageQueueItem.MessageType = INT_MESSAGE_QUIT;
end;

function TMessageQueue.IsEmpty(): boolean;
begin
    result := Count = 0;
end;

function TMessageQueue.GetCount(): integer;
begin
    result := fQueue.Count;
end;

function TMessageQueue.GetItem(aIndex: integer): TMessageQueueItem;
begin
    result := fQueue[aIndex];
end;

procedure TMessageQueue.BeforeAddMessage(aMessageQueueItem: TMessageQueueItem);
begin
end;

procedure TMessageQueue.Sort();
begin
    fQueue.Sort(TMessageQueueComparer.Create);
end;

procedure TMessageQueue.AfterAddMessage(aMessageQueueItem: TMessageQueueItem; aQueueIndex: integer);
begin
    Sort();
end;

procedure TMessageQueue.AfterDeleteMessage(aMessageQueueItem: TMessageQueueItem; aQueueIndex: integer);
begin
end;

procedure TMessageQueue.EnqueueMessage(aMessageQueueItem: TMessageQueueItem);
// Register an Lock in the MessageQueue
var
    xIndex: integer;
begin
    FCriticalSection.Acquire();
    try
        // Since the list is sorted, the AddObject will place the Lock Item in the correct position in the Queue
        BeforeAddMessage(aMessageQueueItem);
        xIndex := FQueue.Add(aMessageQueueItem);
        AfterAddMessage(aMessageQueueItem, xIndex);
        QueueCountChanged(qcrEnqueued);

    finally
        FCriticalSection.Release();
    end;
end;

function TMessageQueue.Peek(): TMessageQueueItem;
begin
    result := nil;
    if IsEmpty() then
        EXIT;
    result := GetItem(0);
end;

function TMessageQueue.Next(): TMessageQueueItem;
// Find next item in Queue to handle
begin
    FCriticalSection.Acquire();
    try
        result := Peek();
    finally
        FCriticalSection.Release();
    end;
end;

procedure TMessageQueue.HandleMessage(aMessageQueueItem: TMessageQueueItem);
begin

    ASSERT(Assigned(aMessageQueueItem)); // raise Exception.Create('HandleLock: Next Lock is invalid');

    try
        aMessageQueueItem.DoOnHandle();
    finally
        // Remove the Lock from the queue
        self.DequeueMessage(aMessageQueueItem);
    end;

end;

procedure TMessageQueue.RefreshLock();
// As soon as the LockCount =1 then Set the MessageQueue's Lock.
begin
    if IsEmpty() then
        FLock.Lock()
    else
        FLock.UnLock();
end;

function TMessageQueue.DequeueMessageByIndex(aQueueIndex: integer): boolean;
var
    xMessageQueueItem: TMessageQueueItem;
begin
    result := false;
    xMessageQueueItem := GetItem(aQueueIndex);
    if xMessageQueueItem = nil then
        EXIT;
    FQueue.Delete(aQueueIndex);
    AfterDeleteMessage(xMessageQueueItem, aQueueIndex);
    if not xMessageQueueItem.SeparateThread then
        xMessageQueueItem.Free;
    QueueCountChanged(qcrDequeued);
    result := true;
end;

function TMessageQueue.DequeueMessage(aMessageQueueItem: TMessageQueueItem): boolean;
// The MessageQueue's Lock itself will remain set until all LockItems are handled and FLockCount=0
// at which point the MessageQueue's Lock will be Reset
var
    xIndex: integer;
begin
    fCriticalSection.Acquire();
    try
        xIndex := FQueue.IndexOf(aMessageQueueItem);
        result := DequeueMessageByIndex(xIndex);
    finally
        fCriticalSection.Release;
    end;
end;

function TMessageQueue.DequeueMessageByType(aMessageType: integer): boolean;
var
    i, xCount: integer;
begin
    result := false;
    fCriticalSection.Acquire();
    try
        xCount := fQueue.Count; // it is important to use fQueue.count instead of self.Count
        for i := xCount - 1 downto 0 do
        begin
            if GetItem(i).MessageType = aMessageType then
                if DequeueMessageByIndex(i) then
                    result := true;
        end;
    finally
        fCriticalSection.Release;
    end;
end;

procedure TMessageQueue.QueueCountChanged(aReason: TMessageQueueCountChangedReason);
begin
    if Assigned(fOnCountChanged) then
        fOnCountChanged(self, aReason);
    self.RefreshLock();
end;

{ TMaskedMessageQueue }

constructor TMaskedMessageQueue.Create();
begin
    inherited Create();
    SetLength(fSavedMask, 0);
    fAllowedMessageTypes := TIntMap.Create(); // dont sort the allowed message types!
end;

destructor TMaskedMessageQueue.Destroy();
begin
    fAllowedMessageTypes.Free;
    inherited Destroy();
end;

function TMaskedMessageQueue.GetCount(): integer;
var
    i: integer;
begin
    result := fQueue.Count;
    if fQueue.Count = 0 then
        EXIT;

    for i := 0 to fQueue.Count - 1 do
    begin
        if (GetItem(i)).Priority <> INST_MESSAGE_QUEUE_ITEM_PRIORITY_UNASSIGNED then
            CONTINUE;
        result := i;
        EXIT;
    end;

end;

procedure TMaskedMessageQueue.BeforeAddMessage(aMessageQueueItem: TMessageQueueItem);
begin
    RefreshItemPriority(aMessageQueueItem);
end;

procedure TMaskedMessageQueue.RefreshItemPriority(aItem: TMessageQueueItem);
var
    xPriority: integer;
begin

    xPriority := fAllowedMessageTypes.IndexOf(aItem.MessageType);
    if xPriority < 0 then
    begin
        xPriority := INST_MESSAGE_QUEUE_ITEM_PRIORITY_UNASSIGNED;
    end;
    aItem.Priority := xPriority;
end;

procedure TMaskedMessageQueue.Sort();
var
    i: integer;
begin
    for i := 0 to fQueue.Count - 1 do
    begin
        RefreshItemPriority(GetItem(i));
    end;

    inherited Sort();
end;

procedure TMaskedMessageQueue.MaskChanged();
begin
    self.Sort;
    QueueCountChanged(qcrOther);
end;

procedure TMaskedMessageQueue.SaveMask(aSingleEdit: boolean);
begin
    if aSingleEdit then
        Acquire();
    try
        fSavedMask := fAllowedMessageTypes.ToArray;
    finally
        if aSingleEdit then
            Release();
    end;
end;

procedure TMaskedMessageQueue.RestoreMask();
begin
    EditMask(emSet, 0, fSavedMask);
end;

procedure TMaskedMessageQueue.SetMaskTemp(const aTempMask: array of integer);
begin
    Acquire();
    try
        SaveMask(false);
        SetMask(aTempMask, false);
        MaskChanged();
    finally
        Release();
    end;
end;

procedure TMaskedMessageQueue.ChangeMaskTemp(const aDeleteMsgTypes, aInsertMsgTypes: array of integer;
    aInsertAt: integer);
begin
    Acquire();
    try
        SaveMask(false);
        DeleteFromMask(aDeleteMsgTypes, false);
        InsertToMask(aInsertAt, aInsertMsgTypes, false);
        MaskChanged();
    finally
        Release();
    end;
end;

class function TMaskedMessageQueue.MaskArgsToMaskArray(const aMsgTypes: array of integer): TArray<integer>;
var
    i: integer;
begin
    SetLength(result, Length(aMsgTypes));
    for i := 0 to high(aMsgTypes) do
    begin
        result[i] := aMsgTypes[i];
    end;
end;

class function TMaskedMessageQueue.ConcatMaskArrays(const aMsgTypes1, aMsgTypes2: TArray<integer>)
    : TArray<integer>;
var
    x, xOffset: integer;
begin
    SetLength(result, Length(aMsgTypes1) + Length(aMsgTypes2));

    for x := 0 to Length(aMsgTypes1) - 1 do
        result[x] := aMsgTypes1[x];

    xOffset := Length(aMsgTypes1);

    for x := 0 to Length(aMsgTypes2) - 1 do
        result[xOffset + x] := aMsgTypes2[x];

end;

procedure TMaskedMessageQueue.SetMask(const aMsgTypes: array of integer; aSingleEdit: boolean = true);
begin
    EditMask(emSet, 0, MaskArgsToMaskArray(aMsgTypes), aSingleEdit);
end;

procedure TMaskedMessageQueue.AddToMask(const aMsgTypes: array of integer; aSingleEdit: boolean = true);
begin
    EditMask(emAdd, 0, MaskArgsToMaskArray(aMsgTypes), aSingleEdit);
end;

procedure TMaskedMessageQueue.DeleteFromMask(const aMsgTypes: array of integer; aSingleEdit: boolean = true);
begin
    EditMask(emDel, 0, MaskArgsToMaskArray(aMsgTypes), aSingleEdit);
end;

procedure TMaskedMessageQueue.InsertToMask(aInsertAt: integer; const aMsgTypes: array of integer;
    aSingleEdit: boolean = true);
begin
    EditMask(emIns, aInsertAt, MaskArgsToMaskArray(aMsgTypes), aSingleEdit);
end;

procedure TMaskedMessageQueue.EditMask(aEditType: TMaskedMessageQueueEditMask; aParam: integer;
    const aMsgTypes: TArray<integer>; aSingleEdit: boolean);
begin
    if aSingleEdit then
        Acquire();
    try
        case aEditType of
            emSet:
                begin
                    fAllowedMessageTypes.Clear();
                    fAllowedMessageTypes.AddInts(aMsgTypes);
                end;

            emAdd:
                begin
                    fAllowedMessageTypes.AddInts(aMsgTypes);
                end;

            emDel:
                begin
                    fAllowedMessageTypes.DeleteInts(aMsgTypes);
                end;
            emIns:
                begin
                    fAllowedMessageTypes.InsertInts(aParam, aMsgTypes);
                end;
        end;
        if aSingleEdit then
            MaskChanged();
    finally
        if aSingleEdit then
            Release();
    end;
end;

procedure TMaskedMessageQueue.Acquire();
begin
    FCriticalSection.Acquire();
end;

procedure TMaskedMessageQueue.Release();
begin
    FCriticalSection.Release();
end;

{ TTimedMessageQueue }

constructor TTimedMessageQueue.Create(aClock: TClock);
begin
    inherited Create();
    fClock := aClock;
end;

destructor TTimedMessageQueue.Destroy();
begin
    inherited Destroy();
end;

procedure TTimedMessageQueue.QueueChanged(aQueueIndex: integer);
begin
    // THIS FUNCTION MUST BE CALLED INSIDE OF THE CRITICAL SECTION.

    if aQueueIndex <> 0 then
        EXIT;
    // if the first message has been changed we need to wake up the thread
    FLock.UnLock();
    // we set the firstmessagechanged flag to true
    fFirstMessageChanged := true;
end;

procedure TTimedMessageQueue.AfterAddMessage(aMessageQueueItem: TMessageQueueItem; aQueueIndex: integer);
begin
    Sort();
    QueueChanged(aQueueIndex);
end;

procedure TTimedMessageQueue.AfterDeleteMessage(aMessageQueueItem: TMessageQueueItem; aQueueIndex: integer);
begin
    QueueChanged(aQueueIndex);
end;

procedure TTimedMessageQueue.Idle();
var
    xFirstMessage: TTimedMessageQueueItem;
    xWaitTime: DWORD;
    xElapsedTime: DWORD;
    xResult: TWaitResult;
    xIsMessageAvailable: boolean;
begin
    xWaitTime := 0;
    while true do
    begin
        // 1. Calculate wait time if message available
        fCriticalSection.Acquire();
        try
            // reset the firstmessagechangedflag
            fFirstMessageChanged := false;

            xFirstMessage := Peek() as TTimedMessageQueueItem;
            xIsMessageAvailable := Assigned(xFirstMessage);

            if xIsMessageAvailable then
            begin
                if IsQuitCondition(xFirstMessage) then
                    EXIT;

                // calculate the wait time
                xElapsedTime := fClock.ElapsedTime();
                ASSERT(xElapsedTime < xFirstMessage.Time,
                    Format('No idle time -> current time %d >= message time %d',
                    [xElapsedTime, xFirstMessage.Time]));
                xWaitTime := xFirstMessage.Time - xElapsedTime;
                fLock.Lock();
            end;

        finally
            fCriticalSection.Release();
        end;

        // 2. wait for till end of waittime reached OR wait for next message to arrive
        if xIsMessageAvailable then
        begin
            // if message available wait until waittime is over
            xResult := fLock.WaitForLock(fClock.ConvertFromClockTimeUnit(xWaitTime, tuThousandth));

            // if the lock timed out, it means that we have reached the end of our wait time.
            // otherwise, if the lock was unlocked, we continue the loop
            if xResult = wrTimeout then
            begin
                fCriticalSection.Acquire();
                try
                    // It is also possible that we timed out exactly when the first message was being changed.
                    // So we have to check this case as well.  If we dont check fFirstMessageChanged then it is possible that
                    // xFirstMessage has already been freed
                    if (not fFirstMessageChanged) and (fClock.ElapsedTime() >= xFirstMessage.Time) then
                        BREAK;
                finally
                    fCriticalSection.Release();
                end;
            end;
        end
        else
        begin
            // if no messages we wait in the lock until a message arrives and wakes us up
            fLock.WaitForLock(fIdleTime);
            fLock.Lock();
        end;
    end;

end;

procedure TTimedMessageQueue.RefreshLock();
begin
end;

constructor TTimedMessageQueueItem.Create(aMessageType: integer; aPriority: integer; aLock: TLock;
    aSeparateThread: boolean; aHandler: TMessageProc; aTime: DWORD; aArgs: TMsgArgs; aOwnsArgs: boolean);
begin
    inherited Create(aMessageType, aPriority, aLock, aSeparateThread, aHandler, aArgs, aOwnsArgs);
    fTime := aTime;
end;

{ TMessageQueueItem }

constructor TMessageQueueItem.Create(const aMessageInfo: TMessageInfo; aLock: TLock; aSeparateThread: boolean;
    aHandler: TMessageProc; const aOwnsMessageInfo: boolean);
begin
    inherited Create();
    fLock := aLock;
    fMessageType := aMessageInfo.MessageID;
    fPriority := 0;
    fSeparateThread := aSeparateThread;
    fOnHandle := aHandler;
    fTriggerWhenHandled := true;
    fArgs := nil;
    fOwnsArgs := true;
    fMessageInfo := aMessageInfo;
    fOwnsMessageInfo := aOwnsMessageInfo;
end;

constructor TMessageQueueItem.Create(aMessageType: integer; aPriority: integer; aLock: TLock;
    aSeparateThread: boolean; aHandler: TMessageProc; aArgs: TMsgArgs; aOwnsArgs: boolean);
begin
    inherited Create();
    fLock := aLock;
    fMessageType := aMessageType;
    fPriority := aPriority;
    fSeparateThread := aSeparateThread;
    fOnHandle := aHandler;
    fTriggerWhenHandled := true;
    fArgs := aArgs;
    fOwnsArgs := aOwnsArgs;
    fMessageInfo := nil;
    fOwnsMessageInfo := false;
end;

destructor TMessageQueueItem.Destroy();
begin
    if fOwnsArgs then
        FreeAndNil(fArgs);

    if fOwnsMessageInfo then
        FreeAndNil(fMessageInfo);
    inherited;
end;

procedure TMessageQueueItem.TriggerLock();
begin
    if MustTriggerWhenHandled() then
        fLock.UnLock();

end;

procedure TMessageQueueItem.DoOnHandle();
var
    xThread: TThread;
    xAccept: boolean;
begin
    // Call the Handler for the Item.
    if not Assigned(self.OnHandle) then
        raise Exception.Create('DoOnHandle: No Handler Found for Lock');
    xAccept := true;
    if not fSeparateThread then
    begin
        self.OnHandle(xAccept, self);
        if xAccept then
            self.TriggerLock();
        EXIT;
    end;

    xThread := TMessageQueueItemThread.Create(self);
    xThread.Start();
end;

function TMessageQueueItem.MustTriggerWhenHandled(): boolean;
begin
    result := Assigned(fLock) and (fTriggerWhenHandled);
end;

function TMessageQueueItem.Log(): string;
begin
    result := Format('(M:%d, P:%d)', [fMessageType, fPriority]);
end;

// *************************************** TMessageQueueItemThread **********************************
constructor TMessageQueueItemThread.Create(aMessageQueueItem: TMessageQueueItem);
begin
    inherited Create(true);
    FreeOnTerminate := true;
    fMessageQueueItem := aMessageQueueItem;
end;

procedure TMessageQueueItemThread.Execute();
var
    xAccept: boolean;
begin
    fMessageQueueItem.OnHandle(xAccept, fMessageQueueItem);
    fMessageQueueItem.TriggerLock();
    fMessageQueueItem.Free;
end;


end.
