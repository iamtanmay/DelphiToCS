{ --------------------------------------------------------------------------------------------------
  General Data Structures/objects
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  19.07.02 pk                                  Initial version
  08.08.03 pk                                  Documentation
  08.12.03 pk  TClock                 TN1697   Gives the milliseconds that have passed by since the clock was started
  25.05.04 pk  TStringStack           TN1909.0 New Class: a Stack based on TStringlist
  16.11.04 pk  TStringStack           TN2231   PeekBottom: return element at bottom of stack
  03.01.05 pk  TIntMap, TDetailedEventTN2281.0 From ObjSchedThreads
  12.01.05 pk  TArgs, TArg            TN2281.0 New
  19.01.05 pk  TMessageQueue          TN2281.0 From ObjSchedThreads
  27.01.05 pk  TIntMap.InsertInts     TN2281.2 New
  27.01.05 pk  TMaskedMessageQueue    TN2281.2 New AcquireMask/ReleaseMask
  27.01.05 pk  TClock                 TN2281.2 ElapsedTimeAt : subtract PausedDuration
  02.03.05 pk  TBoolArg               TN2327   New
  11.03.05 pk  TVarArg                TN2339.2 New
  18.03.05 pk  TMessageQueue.Log      TN2356   New
  18.03.05 pk  TMessageQueue.DequeueMessageByType TN2356 use fQueue.Count instead of self.Count
  19.04.05 pk  TClock.Reset           TN2393   New
  07.06.05 pk  TSortedStringList      TN2449   New create TStringList with Sorted = true
  07.06.05 pk  TDblArg                TN2449   New
  07.11.05 pk                         TN2737   Dynamic Scheduling - various changes
  14.11.05 pk  TIntfArg               TN2759   New
  22.12.05 pk  TInterruptMonitor      TN2875   New
  05.01.06 pk  TSemaphore             TN2877   New
  23.01.06 pk  TLockableObject        TN28     New : Allows Threadsafe use of fObject
  23.05.06 pk  TTimedMessageQueue.Idle TN3115  Changes to prevent access violation
  09.06.06 pk  DequeueMessageByIndex  TN3140   move critical section to DequeueMessage and DequeueMessageByType
  01.11.06 pk  TInterruptRequestItem  TN3391   New WaitLock
  09.11.06 pk  TInterruptRequestItem  TN3399   Interrupts can now have arguements and results
  13.11.06 pk  TInterruptRequestItem  TN3401   assert whether interrupt pointer is set
  21.11.06 pk  TInterruptMonitor      TN3424   changed to work if start is done and then stop - before this would have been handled in the order Stop and then Start
  21.11.06 pk  TInterruptRequestItem  TN3424   Can now be created with an explicit Lock object
  01.12.06 pk  TViewableLock          TN3441   Create can now be called with unlocked parameter
  01.12.06 pk  TSemaphore             TN3441   call OnBeforeWait function before waiting in sempahore
  01.12.06 pk  TMessageQueue          TN3441   MessageCountChangedEvent
  01.12.06 pk  TSelectableLock        TN3441   New
  07.12.06 pk                         TN3455   thread-related classes moved to threadclasses unit
  08.12.06 pk  TQueue, TSizableArray  TN3458   New
  09.12.06 pk  TQueue.Create          TN3458   Clear is now called in create
  20.02.07 wl                         TN3016   entfernt: uses postools
  22.02.07 pk  TQueue                 TN3583   inherits from TInterfacedNoRef
  22.02.07 pk  TQueue                 TN3583   fArray.Free
  03.08.07 wl  TIntList, TIntMap      TN3811.2 --> IntMap.pas
  04.09.07 pk  TLockHandleObj,TLock   TN       --> LockHandle
  04.09.07 pk  TArg                   TN       --> ArgClass
  04.09.07 pk  TClock                          --> ClockClass
  03.07.08 wl                                         TN4157
  02.09.08 pk  TStringStack           TN4215   New: pushname
  04.11.09 pk                         TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }

unit ObjStructures;
{$DEBUGINFO OFF}


interface


uses
    Windows,
    SyncObjs,
    SysUtils,
    Variants,
    GeneralTypes,
    LockHandle,
    InterfacedNoRef,
    ListClasses;

type

    // An object that holds two strings
    TStringPair = class
    private
        FFirst: string;
        FSecond: string;
    public
        constructor Create(aStrFirst, aStrSecond: string);
        property First: string read FFirst write FFirst;
        property Second: string read FSecond write FSecond;
    end;

    TItem = TObject;

    // IAssignable = interface
    // procedure Assign( aObject : TItem );
    // end;

    IIterator = interface
        function Item(): TItem;
    end;

    IForwardIterator = interface(IIterator)
        procedure Inc();
    end;

    IContainer = interface
        function Start(): IIterator;
        // function Finish() : IIterator;
        function IsEmpty(): boolean;
        function GetCount: integer;
        property Count: integer read GetCount;
    end;

    IQueue = interface
        procedure Clear();
        function Peek(): TQueueItem;
        function Dequeue(): TQueueItem;
        procedure Enqueue(aObject: TQueueItem);
        function GetCount(): integer;
        property Count: integer read GetCount;
    end;

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
        property Items[aIndex: integer]: Pointer read GetItem write SetItem; default;
    end;

    TQueue = class(TInterfacedNoRef, IQueue)
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
        function Peek(): TQueueItem;
        function Dequeue(): TQueueItem;
        procedure Enqueue(aObject: TQueueItem);

        property Count: integer read GetCount;
        property OwnsObjects: boolean read fOwnsObjects write fOwnsObjects;
    end;

    // Linked List
    TDoublyLinkedListNode = class
    private
        FValue: TItem;
        FNext: TDoublyLinkedListNode;
        FPrevious: TDoublyLinkedListNode;
    public
        constructor Create(aValue: TItem; aNext, aPrevious: TDoublyLinkedListNode);
        property Previous: TDoublyLinkedListNode read FPrevious write FPrevious;
        property Next: TDoublyLinkedListNode read FNext write FNext;
        property Value: TItem read FValue;
    end;

    TLinkedListIterator = class(TInterfacedObject, IForwardIterator)
    protected
        fCurrent: TDoublyLinkedListNode;
    public
        constructor Create(aCurrent: TDoublyLinkedListNode);
        function Item(): TItem;
        procedure Inc();
    end;

    TLinkedList = class(TInterfacedObject, IContainer)
    protected
        FHead: TDoublyLinkedListNode;
        FTail: TDoublyLinkedListNode;
        FCount: integer;
        function GetCount: integer;
        function ForwStart: IIterator;
        function IContainer.Start = ForwStart;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure InsertAtHead(aItem: TItem);
        function DeleteAtTail(): TItem;
        procedure Clear();
        function IsEmpty(): boolean;
        function Start(): IForwardIterator;
    end;


implementation


uses
    Dialogs;

// ************************************** TStringPair ****************************************
// --------------------------------------------------------------------------------------------------
constructor TStringPair.Create(aStrFirst, aStrSecond: string);
// --------------------------------------------------------------------------------------------------
begin
    inherited Create;
    FFirst := aStrFirst;
    FSecond := aStrSecond;
end;

/// /************************************** TStringPair ****************************************
// procedure TStringStack.Push( const aID : string; aObject : TObject );
// begin
// self.InsertObject( 0, aID, aObject );
// end;
//
// procedure TStringStack.PushName( const aID : string );
// begin
// self.Push( aID, nil );
// end;
//
// procedure TStringStack.Pop();
// begin
// if self.Count = 0 then Exit;
// self.Delete( 0 );
// end;
//
// function TStringStack.IsEmpty: boolean;
// begin
// result := self.Count = 0;
// end;
//
// function TStringStack.PeekName(): string;
// begin
// result := '';
// if IsEmpty() then Exit;
// result := self[0];
// end;
//
// function TStringStack.PeekObject() : TObject;
// begin
// result := nil;
// if self.Count = 0 then Exit;
// result := self.Objects[0];
// end;
//
// function TStringStack.PeekBottom(): string;
// begin
// result := '';
// if self.Count = 0 then Exit;
// result := self[ self.Count - 1 ];
// end;

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

const
    INT_QUEUE_DEF_CAPACITY = 4;

constructor TQueue.Create();
begin
    inherited Create();
    fArray := TSizableArray.Create(INT_QUEUE_DEF_CAPACITY);
    Clear();
end;

destructor TQueue.Destroy();
begin
    FreeAndNil(fArray);
    inherited;
end;

procedure TQueue.Increment(var aValue: integer);
begin
    Inc(aValue);
    if aValue = fArray.Capacity then
        aValue := 0;
end;

procedure TQueue.Enqueue(aObject: TQueueItem);
begin
    if fArray.IsFull then
        fArray.Grow();
    self.Increment(fBack);
    fArray[fBack] := aObject;
    fArray.Count := fArray.Count + 1;
end;

function TQueue.Peek(): TQueueItem;
begin
    if fArray.IsEmpty() then
        result := nil
    else
        result := fArray[fFront];
end;

function TQueue.Dequeue(): TQueueItem;
begin
    result := Peek();
    if not Assigned(result) then
        Exit;
    self.Increment(fFront);
    fArray.Count := fArray.Count - 1;
end;

procedure TQueue.Clear();
begin
    fArray.Clear();
    fBack := -1;
    fFront := 0;
end;

function TQueue.GetCount(): integer;
begin
    result := fArray.Count;
end;

constructor TDoublyLinkedListNode.Create(aValue: TItem; aNext, aPrevious: TDoublyLinkedListNode);
begin
    inherited Create();
    fValue := aValue;
    fNext := aNext;
    fPrevious := aPrevious;
end;

constructor TLinkedListIterator.Create(aCurrent: TDoublyLinkedListNode);
begin
    inherited Create;
    fCurrent := aCurrent;
end;

function TLinkedListIterator.Item(): TItem;
begin
    if Assigned(fCurrent) then
        result := fCurrent.Value
    else
        result := nil;
end;

procedure TLinkedListIterator.Inc();
begin
    if Assigned(fCurrent) then
        fCurrent := fCurrent.Next;
end;

constructor TLinkedList.Create();
begin
    inherited Create();
    fHead := nil;
    Clear();
end;

destructor TLinkedList.Destroy();
begin
    Clear();
    inherited;
end;

function TLinkedList.ForwStart(): IIterator;
begin
    result := IForwardIterator(TLinkedListIterator.Create(fHead));
end;

function TLinkedList.Start(): IForwardIterator;
begin
    result := IForwardIterator(ForwStart());
end;

function TLinkedList.GetCount: integer;
begin
    result := fCount;
end;

function TLinkedList.IsEmpty(): boolean;
begin
    result := fCount = 0;
end;

procedure TLinkedList.Clear();
var
    xNode, xNextNode: TDoublyLinkedListNode;
begin
    xNode := fHead;
    while Assigned(xNode) do
    begin
        xNextNode := xNode.Next;
        FreeAndNil(xNode);
        xNode := xNextNode;
    end;
    fHead := nil;
    fTail := nil;
    fCount := 0;
end;

procedure TLinkedList.InsertAtHead(aItem: TItem);
var
    xNextNode: TDoublyLinkedListNode;
begin
    xNextNode := fHead;
    fHead := TDoublyLinkedListNode.Create(aItem, xNextNode, nil);
    if Assigned(xNextNode) then
    begin
        xNextNode.Previous := fHead;
    end
    else
    begin // first node
        fTail := fHead;
    end;
    Inc(fCount);
end;

function TLinkedList.DeleteAtTail(): TItem;
var
    xPrevNode: TDoublyLinkedListNode;
begin
    result := nil;
    if (not Assigned(fTail)) then
        Exit;
    result := fTail.Value;
    xPrevNode := fTail.Previous;
    FreeAndNil(fTail);
    fTail := xPrevNode;

    if (Assigned(xPrevNode)) then
    begin
        xPrevNode.Next := nil;
    end
    else
    begin //
        fHead := nil;
    end;

    Dec(fCount);
end;


end.
