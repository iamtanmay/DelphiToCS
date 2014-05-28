{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : General threading classes
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.12.06 pk                                TN3455  TExecthread from ThreadUtils
  07.12.06 pk                                TN3455  classes from objStructures
  07.12.06 pk  TExecThread.WaitForLock       TN3455  reset wakeupsignal after waking up from wait
  08.12.06 pk  TSemaphore                    TN3458  Now uses WaitQueue and EnteredArray instead of a single TIntList
  09.12.06 pk  TSempahore                    TN3458  Bugs fixed.  DebugLog lines added
  29.01.07 pk  TTimedMessageQueueItem        TN3527  new aOwnsArgs param
  05.02.07 wl  TSemaphore.Initialize         TN3543  intern: Unterschiedliche Behandlung Delphi 7 und 2006
  22.02.07 pk  ClearCompletedThreads         TN3583  Wait for threads to complete and then free the threads and the registry items
  22.02.07 pk  TThreadRegistryItem.Destroy   TN3583  calls Thread.Free if fOwnsThread= true
  22.02.07 pk  TMessageQueue.Execute         TN3583  free quit message object
  12.03.07 pk  TMessagableExecutable         TN3631  New
  11.04.07 pk  TSempahore                    TN3659  changed so that non-suspended threads enter the semaphore first
  23.07.07 pk  TMessagableExecutable         TN3797  New callback functions
  03.08.07 wl                                TN3811.2 uses IntMap statt ObjStructures
  06.08.07 pk  TExecThread.WaitTillSuspended TN3820  New: if fExecuteDone dont wait on WaitTillSuspended lock.
  03.07.08 wl                                TN4157
  16.07.08 pk  TThreadType                   TN4175  new thrBalance
  02.09.08 pk  TThreadRegistry               TN4215  New GetCurrentPriority
  06.11.08 pk  TProcess                      TN4280  New
  06.11.08 pk  TThreadManager                TN4280  New
  06.11.08 pk  TThreadImage                  TN4280  New
  06.11.08 pk  TThreadAPI                    TN4280  New
  10.11.08 pk  GenerateUniqueThreadDescr...  TN4280  New
  17.11.08 pk  TResourceUsage                TN4280  New
  17.11.08 pk  GetCurrentSourceDataName      TN4280  New
  19.11.08 pk  ThreadState                   TN4280  moved to ThreadImage
  24.11.08 pk  TSysObject, TSysHandleID      TN4280  New
  24.11.08 pk  TThreadManager                TN4280  New CreateLock, WaitForLock, etc.
  27.11.08 pk  RegisterThreadingProcess      TN4280  Create thread suspended, then unpause then resume
  02.12.08 pk  CheckAllProcessesExited       TN4335  New
  10.12.08 pk  SetOriginalMask               TN4280  order of message priorities changed
  17.12.08 pk  GetCurrentThreadImage         TN4280  Does not post a message, directly accesses threadregistry
  17.12.08 pk  TThreadRegistryList           TN4280  New threadsafe list
  17.12.08 pk  ThreadImage                   TN4372  New fContractorThreadID keeps track of for which thread this thread is working
  07.01.09 pk  TThreadRegistry.GetThreadInfo TN4280  Does not Assert. Caused software to crash if thread was not registered (eg. CANIO thread)
  08.01.09 pk  InternalGetSysObjectData      TN4280  Do not raise exception if id is not found
  08.01.09 pk  TThreadingProcessHandler      TN4280  MessageProc new try except
  17.02.09 pk  GetCurrentProcessDescription  TN4232  New
  06.04.09 pk  GetThreadImageByDescription   TN4503  New
  08.04.09 pk  IsAPIAvailable                TN4513  New
  16.04.09 pk  GetIsCurrentThreadSimulated   TN4528  No longer posts a message, posting takes too much time
  08.06.09 pk  TMessageQueue                 TN4585.1 Next, HandleMessage
  09.06.09 pk  IMessagableExecutable         TN4595  New
  24.08.09 pk  GetCurrentSourceDataName      TN4735  New
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.12.09 pk  GetIsCurrentThreadSimulated   TN4920  calls AtleasOneNonSimulatedProcessExists if the current thread is a system thread
  04.02.10 pk                                TN4972  Changes for Restart
  09.02.10 pk                                TN4973  Thread.Resume changed to Thread.Start
  09.03.10 pk                                TN5015  Massive Changes in ThreadClasses
  06.04.10 pk  ThreadingProcessHandler       TN5015  keep a pointer to the interface as well due to reference counting, otherwise freed too early
  08.06.10 pk  ChangeCurrentProcessSimulated TN5077  New
  11.06.10 pk  CreateProcess                 TN5138  Now with IsSystemProcess param
  19.10.10 pk                                TN5305  changes needed for CoreClient/Server
  29.10.10 pk  DoOnThreadException           TN5321  use TCB.TID instead of TThread.ThreadID
  15.11.10 pk                                TN5340  Changes to prevent memory leak
  13.12.10 wl                                TN5370   TLock.WaitForLock statt TLock.WaitFor
  14.09.11 wl  TThreadAPICallbacks           TN5672   in Einzelfunktionen aufgeteilt
  19.01.12 wl  WaitTillAllProcessesSuspended TN5759.2 neuer Parameter MaxWaitTime
  01.03.12 wl                                TN5822   uses geändert
  29.05.12 wl  TPlatformSpecificOS.GetCurrentThreadID  TN5904   --> ThreadUtils
  27.03.13 wl                                          TN6045   uses Generics.Collections
  11.04.13 wl                                          TN6045   uses HiddenListClasses
  25.06.13 wl                                          TN6178   uses Identifier: TThreadSafeIdentifierList
  15.08.13 wl                                          TN6223   Teile dieser Unit in 8 neue Units aufgeteilt
  ------------------------------------------------------------------------------------------------- }

unit ThreadClasses;

{$DEBUGINFO ON}


interface


uses
    Windows,
    SyncObjs,
    Classes,
    SysUtils,
    GeneralTypes,
    ListClasses,
    HiddenListClasses,
    Generics.Defaults,
    Generics.Collections,
    Streamable,
    ClockClass,
    LockHandle,
    Executable,
    Identifier,
    InterfacedNoRef,
    MemoryClasses,
    ThreadUtils;

const
    cThreadIDNone = 0;
    cHandleIDNone = 0;
    cCurrentThreadPsuedoSysHandleID = 1;
    cFirstRealSysHandleID = 11;

type
    TSysObjectID = cardinal;

    TSysObject = class
    private
        fID: TSysObjectID;
        fData: TObject;
        fRefCount: integer;
    public
        constructor Create(const aID: TSysObjectID; const aData: TObject);
        procedure IncRefCount();
        procedure DecRefCount();
        property ID: TSysObjectID read fID;
        property Data: TObject read fData;
        property RefCount: integer read fRefCount;
    end;

    TSysObjectList = class
    private
        fList: TIntegerKeyObjectValueList;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TSysObject;
    public
        constructor Create();
        destructor Destroy(); override;

        function IndexOf(const aID: TSysObjectID): integer;
        procedure Add(aItem: TSysObject);
        function FindByID(const aID: TSysObjectID; aMustFind: boolean): TSysObject;
        procedure RemoveByID(const aID: TSysObjectID);
        procedure Clear();
        property Items[aIndex: integer]: TSysObject read GetItemAt; default;
        property Count: integer read GetCount;
    end;

    TSysHandleID = cardinal;

    TSysHandle = class
    private
        fID: TSysHandleID;
        fSysObjectID: TSysObjectID;
    public
        constructor Create(const aID: TSysHandleID; const aSysObjectID: TSysObjectID);
        property ID: TSysHandleID read fID;
        property SysObjectID: TSysObjectID read fSysObjectID;
    end;

    TSysHandleList = class
    private
        fList: TIntegerKeyObjectValueList;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TSysHandle;
    public
        constructor Create();
        destructor Destroy(); override;

        function IndexOf(const aID: TSysHandleID): integer;
        procedure Add(aItem: TSysHandle);
        function FindByID(const aID: TSysHandleID; aMustFind: boolean): TSysHandle;
        procedure RemoveByID(const aID: TSysHandleID);
        procedure Clear();
        property Items[aIndex: integer]: TSysHandle read GetItemAt; default;
        property Count: integer read GetCount;
    end;

    TResourceUsage = class
    private
        fResID: string;
        fPriority: integer;
    public
        constructor Create(const aResID: string; const aPriority: integer);
        property ResID: string read fResID;
        property Priority: integer read fPriority;
    end;

    TResourceUsageList = class
    private
        fList: TObjectList<TObject>;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TResourceUsage;
    public
        constructor Create(const aOwnsObjects: boolean = true);
        destructor Destroy(); override;
        procedure Add(aItem: TResourceUsage);
        procedure Extract(aIndex: integer);
        function IndexOf(const aID: string): integer;
        function FindByID(const aID: string; aMustFind: boolean): TResourceUsage;
        procedure RemoveByID(const aID: string);
        procedure Clear();
        procedure AddItems(aItems: TResourceUsageList);
        property Items[aIndex: integer]: TResourceUsage read GetItemAt; default;
        property Count: integer read GetCount;
    end;

    TExecThread = class;
    TThreadNotifyEvent = procedure(aThread: TExecThread) of object;
    TThreadExceptionEvent = procedure(aThread: TExecThread; aException: Exception) of object;

    TProcessor = class
    public
        function ProcessNext(): boolean; virtual; abstract;
    end;

    TMessageInfo = class
    private
        function GetCallingThreadID: cardinal;
        function GetIsCallingThreadIDAssigned: boolean;
    protected
        fCallingThreadID: cardinal;
        fCallingThreadSysHandleID: TSysHandleID;
        function GetMessageID(): integer; virtual; abstract;
    public
        constructor Create();
        destructor Destroy(); override;
        property MessageID: integer read GetMessageID;
        property CallingThreadID: cardinal read GetCallingThreadID write fCallingThreadID;
        property IsCallingThreadIDAssigned: boolean read GetIsCallingThreadIDAssigned;
        property CallingThreadSysHandleID: TSysHandleID read fCallingThreadSysHandleID
            write fCallingThreadSysHandleID;
    end;

    TThreadControlBlock = class
    private
        fPID: cardinal;
        fTID: cardinal;
        fOSThreadID: cardinal;
        fClientRunID: string;
        fPriority: integer;
        fProgramCounter: TRelativeMemAddress;
        fProcessor: TProcessor;
        fExecutable: IExecutable;
    public
        constructor Create();
        destructor Destroy(); override;
        property TID: cardinal read fTID write fTID;
        property PID: cardinal read fPID write fPID;
        property OSThreadID: cardinal read fOSThreadID write fOSThreadID;
        property Priority: integer read fPriority;
        property ClientRunID: string read fClientRunID write fClientRunID;
        property ProgramCounter: TRelativeMemAddress read fProgramCounter;
        property Executable: IExecutable read fExecutable write fExecutable;
        property Processor: TProcessor read fProcessor write fProcessor;
    end;

    TThreadState = (tsBorn, tsActive, tsSuspended, tsTerminated);

    TCloseUserSysHandleOnFinishInfo = record
        SysHandleID: TSysHandleID;
        CloseSysHandleOnFinish: boolean;
    end;

    TThreadImage = class
    private
        // function GetIsTerminated: boolean;
        fDescription: string;

        // Todo: resources should be SysObjects with handles
        fResourcesUsed: TResourceUsageList;
        fResourcesPending: TResourceUsageList;

        fTCB: TThreadControlBlock;
        fCallStack: TCallStack;
        fThread: TExecThread;
        // fState : TThreadState;
        fSysObjectID: TSysObjectID;
        fEndLock: TLock;
        fContractorThreadID: TSysHandleID;

        // the handle added on creating a thread dont use this internally
        fCloseUserSysHandleOnFinishInfo: TCloseUserSysHandleOnFinishInfo;

        // procedure SetState( const aValue : TThreadState );
        function GetOSThreadID: cardinal;
    public
        constructor Create();
        destructor Destroy(); override;

        procedure SetThread(const aValue: TExecThread);
        procedure SetSysObjectID(const aValue: TSysObjectID);
        procedure ResetContractorThreadID();
        procedure SetContractorThreadID(const aValue: TSysHandleID);
        procedure SetCloseUserSysHandleOnFinishInfo(const aCloseSysHandleOnFinish: boolean;
            const aThreadSysHandleID: TSysHandleID);
        function HasContractorThreadID(): boolean;

        property Description: string read fDescription write fDescription;
        property TCB: TThreadControlBlock read fTCB;
        property CallStack: TCallStack read fCallStack;
        property Thread: TExecThread read fThread;
        property OSThreadID: cardinal read GetOSThreadID;
        property ResourcesUsed: TResourceUsageList read fResourcesUsed;
        property ResourcesPending: TResourceUsageList read fResourcesPending;
        // property State : TThreadState read fState;
        property SysObjectID: TSysObjectID read fSysObjectID;
        property ContractorThreadID: cardinal read fContractorThreadID;
        // property IsTerminated : boolean read GetIsTerminated;
        property CloseUserSysHandleOnFinishInfo: TCloseUserSysHandleOnFinishInfo
            read fCloseUserSysHandleOnFinishInfo;
        property EndLock: TLock read fEndLock;
    end;

    TExecThread = class(TThread)
    private
        fThreadImage: TThreadImage;
        fCreateSuspended: boolean;
        fExecutableIntf: IExecutable;
        fCanSuspendSafe: boolean;
        fWaitTillSuspendedLock, fSuspendLock, fWakeupSignal: TLock;
        fOnStart: TThreadNotifyEvent;
        fOnFinishSuccessFull: TThreadNotifyEvent;
        fOnFinish: TThreadNotifyEvent;
        fOnException: TThreadExceptionEvent;
        fPWaitForLockHandleArray: PWOHandleArray;
        fSafeSuspendCount: integer;
        fExecuteDone: boolean;
        function GetIsSafeSuspended: boolean;
        procedure ResumeDangerous;
        procedure SuspendDangerous;
        procedure ResumeAfterCreate();
    protected
        procedure DoExecute(); virtual;
    public
        constructor Create(aThreadImage: TThreadImage; aCreateSuspended, aFreeSelfOnTerminate: boolean;
            const aExecutableIntf: IExecutable; aCanSuspendSafe: boolean); virtual;
        destructor Destroy(); override;
        procedure Execute(); override;
        procedure FirstResume();
        procedure SuspendSafe();
        procedure ResumeSafe();
        function WaitForLock(aLock: TObject; aTimeout: DWORD; out oWaitOK: boolean): TWaitResult;
        procedure CheckSuspend();
        function WaitTillSuspended(const aMaxWaitTime: cardinal): TWaitResult;

        property OnStart: TThreadNotifyEvent read fOnStart write fOnStart;
        property OnFinishSuccessFull: TThreadNotifyEvent read fOnFinishSuccessFull write fOnFinishSuccessFull;
        property OnFinish: TThreadNotifyEvent read fOnFinish write fOnFinish;
        property OnException: TThreadExceptionEvent read fOnException write fOnException;
        property Terminated;
        property ThreadImage: TThreadImage read fThreadImage;
        property CanSuspendSafe: boolean read fCanSuspendSafe;
        property IsSafeSuspended: boolean read GetIsSafeSuspended;
    end;

    TProgramCode = class
    end;

    TCompiledFile = class
    end;

    TAddressSpace = class
    protected
        fHeap: TThreadSafeIdentifierList;
        fProgramCode: TProgramCode;
        procedure CreateProgramCode(); virtual; abstract;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Load(const aFile: TCompiledFile); virtual;
        property Heap: TThreadSafeIdentifierList read fHeap;
        property ProgramCode: TProgramCode read fProgramCode;
    end;

    TProcessControlBlock = class
    protected
        fPID: cardinal;
        fParentPID: cardinal;
        fSuspended: boolean;
        fClientRunID: string;
    public
        constructor Create(const aPID: cardinal);
        property PID: cardinal read fPID;
        property ParentPID: cardinal read fParentPID;
        property Suspended: boolean read fSuspended write fSuspended;
        property ClientRunID: string read fClientRunID write fClientRunID;
    end;

    IMessagable = interface
        ['{9BFAC639-6444-42E3-BA4F-26E2AB4D96D9}']
        procedure RegisterMessageWithLock(aMessageInfo: TMessageInfo; aLock: TLock;
            aOwnsMessageInfo: boolean);
        procedure RegisterMessageAndWait(aMessageInfo: TMessageInfo; aOwnsMessageInfo: boolean;
            aMaxWaitTime: cardinal);
        procedure RegisterMessageAndLeave(aMessageInfo: TMessageInfo);
    end;

    IProcessHandler = interface(IExecutable)
        ['{53F72A07-6117-4D50-9946-C703F92E78B5}']
        procedure Quit();
        procedure Pause();
        procedure Unpause();
    end;

    TInterruptRoutineArgs = variant;
    TInterruptRoutineResult = variant;
    TInterruptRoutineEvent = function(aSender: TObject; aIRArgs: TInterruptRoutineArgs)
        : TInterruptRoutineResult of object;

    TThreadingShowThreadExceptionCallback = procedure(const aCaption: string; aException: Exception)
        of object;
    TThreadingLogCallback = procedure(const aText: string) of object;

const
    INT_MESSAGE_EXECUTABLE_TERMINATE = 7000;
    INT_MESSAGE_EXECUTABLE_UNPAUSE = 7001;
    INT_MESSAGE_EXECUTABLE_PAUSE = 7002;
    INT_MESSAGE_EXECUTABLE_PERFORM = 7003;

    INT_MESSAGE_PROCESSHANDLER_SECONDARYTHREADADDED = 8001;
    INT_MESSAGE_PROCESSHANDLER_SECONDARYTHREADREMOVED = 8002;

    INT_MESSAGE_THREADAPI_ERROR_SET = 9000;

    INT_MESSAGE_THREADAPI_THREAD_GETIMAGE = 9010;
    INT_MESSAGE_THREADAPI_THREAD_CREATE = 9011;
    INT_MESSAGE_THREADAPI_THREAD_FINISHED = 9012;
    INT_MESSAGE_THREADAPI_THREAD_START = 9013;
    INT_MESSAGE_THREADAPI_THREAD_OPEN = 9014;
    INT_MESSAGE_THREADAPI_THREAD_GETIMAGEBYDESCR = 9015;

    INT_MESSAGE_THREADAPI_PROCESS_GETCURRENT = 9020;
    INT_MESSAGE_THREADAPI_PROCESS_ADD = 9021;
    INT_MESSAGE_THREADAPI_PROCESS_EXIT = 9022;
    INT_MESSAGE_THREADAPI_PROCESS_OPEN = 9023;
    INT_MESSAGE_THREADAPI_PROCESS_TERMINATETHREADS = 9024;
    INT_MESSAGE_THREADAPI_PROCESS_WAITTILLTHREADSTERMINATE = 9025;

    INT_MESSAGE_THREADAPI_PROCESSES_SUSPENDALL = 9030;
    INT_MESSAGE_THREADAPI_PROCESSES_RESUMEALL = 9031;
    INT_MESSAGE_THREADAPI_PROCESSES_WAITSUSPENDEDALL = 9032;
    INT_MESSAGE_THREADAPI_PROCESSES_CHECKALLEXITED = 9033;

    INT_MESSAGE_THREADAPI_PROCESSES_NONSIMPROCEXISTS = 9035;

    INT_MESSAGE_THREADAPI_LOCK_CREATE = 9040;
    INT_MESSAGE_THREADAPI_LOCK_DESTROY = 9041;
    INT_MESSAGE_THREADAPI_LOCK_GET = 9042;

    INT_MESSAGE_THREADAPI_SYS_DUPLICATEHANDLE = 9050;
    INT_MESSAGE_THREADAPI_SYS_CLOSEHANDLE = 9051;

procedure gmDebugLog(const aText: string);


implementation


procedure gmDebugLog(const aText: string);
begin
    // if not gDolog then EXIT;
    outputdebugstring(PChar(Format('%d - %s', [TPlatformSpecificOS.GetCurrentThreadID(), aText])));
end;

{ TSysObject }

constructor TSysObject.Create(const aID: TSysObjectID; const aData: TObject);
begin
    inherited Create();
    fID := aID;
    fData := aData;
    fRefCount := 0;
end;

procedure TSysObject.DecRefCount;
begin
    Dec(fRefCount);
end;

procedure TSysObject.IncRefCount;
begin
    Inc(fRefCount);
end;

{ TSysObjectList }

constructor TSysObjectList.Create();
begin
    inherited Create();
    fList := TIntegerKeyObjectValueList.Create(true);
end;

destructor TSysObjectList.Destroy;
begin
    fList.Free;
    inherited;
end;

procedure TSysObjectList.Add(aItem: TSysObject);
begin
    fList.AddObject(aItem.ID, aItem);
end;

function TSysObjectList.GetCount: integer;
begin
    result := fList.Count;
end;

function TSysObjectList.GetItemAt(aIndex: integer): TSysObject;
begin
    result := fList.Objects[aIndex] as TSysObject;
end;

function TSysObjectList.IndexOf(const aID: TSysObjectID): integer;
begin
    result := fList.IndexOf(aID);
end;

function TSysObjectList.FindByID(const aID: TSysObjectID; aMustFind: boolean): TSysObject;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := self.IndexOf(aID);
    if xIndex < 0 then
    begin
        if aMustFind then
            raise Exception.CreateFmt('ID %d could not be found', [aID]);
        EXIT;
    end;

    result := self[xIndex];
end;

procedure TSysObjectList.Clear;
begin
    fList.Clear();
end;

procedure TSysObjectList.RemoveByID(const aID: TSysObjectID);
var
    xIndex: integer;
begin
    xIndex := IndexOf(aID);
    fList.Delete(xIndex);
end;

{ TSysHandle }

constructor TSysHandle.Create(const aID: TSysHandleID; const aSysObjectID: TSysObjectID);
begin
    inherited Create();
    fID := aID;
    fSysObjectID := aSysObjectID;
end;

{ TSysHandleList }

constructor TSysHandleList.Create();
begin
    inherited Create();
    fList := TIntegerKeyObjectValueList.Create(true);
end;

destructor TSysHandleList.Destroy;
begin
    fList.Free;
    inherited;
end;

procedure TSysHandleList.Add(aItem: TSysHandle);
begin
    fList.AddObject(aItem.ID, aItem);
end;

function TSysHandleList.GetCount: integer;
begin
    result := fList.Count;
end;

function TSysHandleList.GetItemAt(aIndex: integer): TSysHandle;
begin
    result := fList.Objects[aIndex] as TSysHandle;
end;

function TSysHandleList.IndexOf(const aID: TSysHandleID): integer;
begin
    result := fList.IndexOf(aID);
end;

function TSysHandleList.FindByID(const aID: TSysHandleID; aMustFind: boolean): TSysHandle;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := self.IndexOf(aID);
    if xIndex < 0 then
    begin
        if aMustFind then
            raise Exception.CreateFmt('ID %d could not be found', [aID]);
        EXIT;
    end;

    result := self[xIndex];
end;

procedure TSysHandleList.Clear;
begin
    fList.Clear();
end;

procedure TSysHandleList.RemoveByID(const aID: TSysHandleID);
var
    xIndex: integer;
begin
    xIndex := IndexOf(aID);
    fList.Delete(xIndex);
end;

{ TResourceUsage }

constructor TResourceUsage.Create(const aResID: string; const aPriority: integer);
begin
    inherited Create();
    fResID := aResID;
    fPriority := aPriority;
end;

{ TResourceUsageList }

constructor TResourceUsageList.Create(const aOwnsObjects: boolean = true);
begin
    inherited Create();
    fList := TObjectList<TObject>.Create(aOwnsObjects);
end;

destructor TResourceUsageList.Destroy;
begin
    fList.Free;
    inherited;
end;

procedure TResourceUsageList.Add(aItem: TResourceUsage);
begin
    fList.Add(aItem);
end;

function TResourceUsageList.GetCount: integer;
begin
    result := fList.Count;
end;

function TResourceUsageList.GetItemAt(aIndex: integer): TResourceUsage;
begin
    result := fList[aIndex] as TResourceUsage;
end;

function TResourceUsageList.IndexOf(const aID: string): integer;
var
    x: integer;
begin
    result := -1;
    for x := 0 to self.Count - 1 do
    begin
        if SameText(self[x].ResID, aID) then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

function TResourceUsageList.FindByID(const aID: string; aMustFind: boolean): TResourceUsage;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := self.IndexOf(aID);
    if xIndex < 0 then
    begin
        if aMustFind then
            raise Exception.CreateFmt('Resource Scheme %s could not be found', [aID]);
        EXIT;
    end;

    result := self[xIndex];
end;

procedure TResourceUsageList.Extract(aIndex: integer);
begin
    ASSERT(aIndex >= 0);
    fList.Extract(fList[aIndex]);
end;

procedure TResourceUsageList.Clear;
begin
    fList.Clear();
end;

procedure TResourceUsageList.AddItems(aItems: TResourceUsageList);
var
    x: integer;
begin
    for x := 0 to aItems.Count - 1 do
    begin
        self.Add(aItems[x]);
    end;
end;

procedure TResourceUsageList.RemoveByID(const aID: string);
var
    xItem: TObject;
begin
    xItem := FindByID(aID, true);
    fList.Remove(xItem);
end;

{ TExecThread }

constructor TExecThread.Create(aThreadImage: TThreadImage; aCreateSuspended, aFreeSelfOnTerminate: boolean;
    const aExecutableIntf: IExecutable; aCanSuspendSafe: boolean);
begin
    inherited Create(true);
    fThreadImage := aThreadImage;
    fSafeSuspendCount := 0;

    self.FreeOnTerminate := aFreeSelfOnTerminate;
    fCreateSuspended := aCreateSuspended;
    fExecutableIntf := aExecutableIntf;

    fExecuteDone := false;
    fCanSuspendSafe := aCanSuspendSafe;
    fWaitTillSuspendedLock := nil;
    fSuspendLock := nil;
    fWakeupSignal := nil;
    fPWaitForLockHandleArray := nil;

    if fCanSuspendSafe then
    begin
        fSuspendLock := TSimpleLock.Create(true, true, false);
        fWaitTillSuspendedLock := TSimpleLock.Create(true, true, false);
        fWakeupSignal := TSimpleLock.Create(true, false, false);
        New(fPWaitForLockHandleArray);
        fPWaitForLockHandleArray[1] := fWakeupSignal.Handle;
    end;
end;

destructor TExecThread.Destroy();
begin
    Dispose(fPWaitForLockHandleArray);
    fWakeupSignal.Free;
    fWaitTillSuspendedLock.Free;
    fSuspendLock.Free;
    inherited;
end;

procedure TExecThread.SuspendDangerous();
begin
{$WARN SYMBOL_DEPRECATED OFF}
    self.Suspend();
{$WARN SYMBOL_DEPRECATED ON}
end;

procedure TExecThread.ResumeDangerous();
begin
{$WARN SYMBOL_DEPRECATED OFF}
    self.Resume();
{$WARN SYMBOL_DEPRECATED ON}
end;

procedure TExecThread.ResumeAfterCreate();
begin
    self.Start();
end;

// Another thread calls suspend on this thread
// This thread calls Checksuspend once in a while and waits on supendlock if thread is supended
// If this thread wants to wait on any synchronization object, it has to call WaitForLock
procedure TExecThread.SuspendSafe();
//
begin
    if fCanSuspendSafe then
    begin
        Inc(fSafeSuspendCount);
        if fSafeSuspendCount > 1 then
            EXIT;
        // gmDebugLog( Format( 'SuspendSafe on %d - BEGIN', [ThreadID] ) );
        fSuspendLock.Lock;
        fWakeupSignal.Unlock;
        // gmDebugLog( Format( 'SuspendSafe on %d - END', [ThreadID] ) );
    end
    else
    begin
        SuspendDangerous();
    end;
end;

procedure TExecThread.ResumeSafe();
begin

    if fCanSuspendSafe then
    begin
        Dec(fSafeSuspendCount);
        ASSERT(fSafeSuspendCount >= 0);
        if fSafeSuspendCount > 0 then
            EXIT;
        // gmDebugLog( Format( 'ResumeSafe on %d - BEGIN', [ThreadID] ) );
        fSuspendLock.UnLock();
        fWaitTillSuspendedLock.Lock();
        // gmDebugLog( Format( 'ResumeSafe on %d - END', [ThreadID] ) );
    end
    else
    begin
        ResumeDangerous();
    end;
end;

function TExecThread.GetIsSafeSuspended: boolean;
begin
    result := fSafeSuspendCount > 0;
end;

procedure TExecThread.CheckSuspend();
begin
    if fCanSuspendSafe then
    begin
        // gmDebugLog( 'CheckSuspend - BEGIN' );
        fWaitTillSuspendedLock.Unlock();
        fSuspendLock.WaitForLock(INFINITE);
        // gmDebugLog( 'CheckSuspend - END' );
    end;
end;

function TExecThread.WaitTillSuspended(const aMaxWaitTime: cardinal): TWaitResult;
begin
    result := wrSignaled;
    if fCanSuspendSafe then
    begin
        // if the execution of thread is already finished don't need to wait for thread to call checksuspend
        if fExecuteDone then
            EXIT;
        // gmDebugLog( 'WaitTillSuspended - BEGIN' );
        result := fWaitTillSuspendedLock.WaitForLock(aMaxWaitTime);
        // gmDebugLog( 'WaitTillSuspended - END' );
    end;
end;

procedure TExecThread.FirstResume();
begin
    self.ResumeAfterCreate;
end;

function TExecThread.WaitForLock(aLock: TObject; aTimeout: DWORD; out oWaitOK: boolean): TWaitResult;
const
    INT_WAKEUPSIGNAL_SIGNALED = WAIT_OBJECT_0 + 1;
    BOOL_WAITFORALL = false; // just wait for one wait object to be signalled
var
    xWaitResult: DWORD;
begin
    result := wrError;
    if not fCanSuspendSafe then
    begin
        oWaitOK := false;
        EXIT;
    end;

    oWaitOK := true;

    fPWaitForLockHandleArray[0] := (aLock as TLock).Handle;
    // gmDebugLog( Format( 'WaitForLock %d - BEGIN', [ fPWaitForLockHandleArray[0] ] ) );
    // the thread will wait on the lock aLock, and exit with the waitresult when aLock is unlocked.
    // but it is possible to wake up from the wait so that checksuspend can be called. After calling checksuspend
    // the thread will wait on the lock aLock again

    while true do
    begin
        // wait for either aLock to be unlocked or for the WakeUpsignal to be set
        // Note: if both are unlocked, xWaitResult = WAIT_OBJECT_0
        xWaitResult := WaitForMultipleObjects(2, fPWaitForLockHandleArray, BOOL_WAITFORALL, aTimeout);

        if xWaitResult = INT_WAKEUPSIGNAL_SIGNALED then
        begin
            // gmDebugLog( 'Wakeup to check suspend' );
            // 1. the WakeUpsignal allowed us to temporarily get out of the wait
            // reset the WakeUpsignal so that we can wait on it the next time
            fWakeUpSignal.Lock();
            // check suspend and possible wait on the suspend lock
            CheckSuspend();
        end
        else
        begin
            // 2. aLock was signaled or there was a problem
            case xWaitResult of
                WAIT_OBJECT_0:
                    result := wrSignaled;
                WAIT_FAILED:
                    result := wrError;
                WAIT_TIMEOUT:
                    result := wrTimeout;
                WAIT_ABANDONED_0, WAIT_ABANDONED_0 + 1:
                    result := wrAbandoned;
            end;
            EXIT;
        end;

    end;
end;

procedure TExecThread.DoExecute();
begin
    fExecutableIntf.Execute();
    fExecuteDone := true;
    CheckSuspend();
end;

procedure TExecThread.Execute();
begin
    try
        try
            CheckSuspend();

            if Assigned(fOnStart) then
                fOnStart(self);

            DoExecute();

            if Assigned(fOnFinishSuccessFull) then
                fOnFinishSuccessFull(self);

            outputdebugstring(Pchar(format('Thread %d - Ended', [ThreadID])));
        except
            on E: Exception do
                if Assigned(fOnException) then
                    fOnException(self, E);
        end;
    finally
        if Assigned(fOnFinish) then
            fOnFinish(self);
    end;
end;

{ TProcessControlBlock }

constructor TProcessControlBlock.Create(const aPID: cardinal);
begin
    inherited Create();
    fPID := aPID;
    fSuspended := false;
end;

{ TAddressSpace }

constructor TAddressSpace.Create;
begin
    inherited Create();
    fHeap := TThreadSafeIdentifierList.Create();
    CreateProgramCode();
end;

destructor TAddressSpace.Destroy();
begin
    FreeAndNil(fProgramCode);
    FreeAndNil(fHeap);
    inherited;
end;

procedure TAddressSpace.Load(const aFile: TCompiledFile);
begin

end;

{ TThreadControlBlock }

constructor TThreadControlBlock.Create;
begin
    inherited Create();
    fProgramCounter := TRelativeMemAddress.Create();
end;

destructor TThreadControlBlock.Destroy;
begin
    fProgramCounter.Free;
    inherited;
end;

{ TThreadImage }

constructor TThreadImage.Create;
begin
    inherited Create();
    fTCB := TThreadControlBlock.Create();
    fCallStack := TCallStack.Create();
    fResourcesUsed := TResourceUsageList.Create(true);
    fResourcesPending := TResourceUsageList.Create(true);
    self.ResetContractorThreadID();
    fEndLock := TSimpleLock.Create(false, false, true);
    self.SetCloseUserSysHandleOnFinishInfo(false, cHandleIDNone);
end;

destructor TThreadImage.Destroy;
begin
    FreeAndNil(fEndLock);
    fResourcesPending.Free;
    fResourcesUsed.Free;
    fCallStack.Free;
    fTCB.Free;
    inherited;
end;

function TThreadImage.GetOSThreadID: cardinal;
begin
    result := fTCB.OSThreadID;
end;

procedure TThreadImage.SetSysObjectID(const aValue: TSysObjectID);
begin
    fSysObjectID := aValue;
end;

procedure TThreadImage.SetThread(const aValue: TExecThread);
begin
    fThread := aValue;
    if Assigned(aValue) then
        fTCB.OSThreadID := fThread.ThreadID
    else
        fTCB.OSThreadID := 0;
end;

procedure TThreadImage.SetCloseUserSysHandleOnFinishInfo(const aCloseSysHandleOnFinish: boolean;
    const aThreadSysHandleID: TSysHandleID);
var
    xSysHandleID: TSysHandleID;
begin
    fCloseUserSysHandleOnFinishInfo.CloseSysHandleOnFinish := aCloseSysHandleOnFinish;

    xSysHandleID := aThreadSysHandleID;
    if not fCloseUserSysHandleOnFinishInfo.CloseSysHandleOnFinish then
        xSysHandleID := cHandleIDNone;

    fCloseUserSysHandleOnFinishInfo.SysHandleID := xSysHandleID;
end;

procedure TThreadImage.SetContractorThreadID(const aValue: TSysHandleID);
begin
    fContractorThreadID := aValue;
end;

procedure TThreadImage.ResetContractorThreadID();
begin
    SetContractorThreadID(cHandleIDNone);
end;

function TThreadImage.HasContractorThreadID: boolean;
begin
    result := fContractorThreadID <> cHandleIDNone;
end;

{ TMessageInfo }

constructor TMessageInfo.Create();
begin
    inherited Create();
    fCallingThreadSysHandleID := cHandleIDNone;
end;

destructor TMessageInfo.Destroy;
begin
    inherited;
end;

function TMessageInfo.GetCallingThreadID: cardinal;
begin
    ASSERT(IsCallingThreadIDAssigned, 'CallingThreadID not assigned');
    result := fCallingThreadID;
end;

function TMessageInfo.GetIsCallingThreadIDAssigned: boolean;
begin
    result := fCallingThreadID <> cHandleIDNone;
end;


end.
