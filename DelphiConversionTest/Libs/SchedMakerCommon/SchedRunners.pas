{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  27.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  07.04.03 wl  TSchedActionThread           TN1345   --> SchedActionThread.pas
  08.12.03 pk                               TN1697   Various changes
  21.07.04 pk  TSchedActionThread           TN2049  <-- SchedActionThread.pas
  21.07.04 pk                               TN2049   Various changes
  12.01.05 pk                               TN2281   Various internal changes
  12.01.05 pk  TMsgArgs                     TN2281   Changed from array of TVarRec to TArgs ( TObjectList ) --> ObjStructures
  18.01.05 pk  TSchedThread                 TN2281   --> ThreadUtils.TExecThread
  19.01.05 pk  TSchedSessionManager.HandlePauseEvent TN2281.0 from TSchedSessionManagerExt
  19.01.05 pk                               TN2281.0 TMessageQueue --> ObjStructures
  27.01.05 pk                               TN2281.2 varisou changes for Suspend/Resume
  --------------------------------------------------------------------------------------------------
  15.02.05 pk                                                     Unit name changed from objSchedThreads
  15.02.05 pk                                            TN2315   New : TSchedActionRunner
  28.02.05 pk  TSchedProcessRunner.MessageProc           TN2314.1 Log when exception occurs
  02.03.05 pk  TSchedProcessRunner.RegisterLateAction    TN2327   No longer waits on MainEvent. No longer called in separate thread
  02.03.05 pk  TSchedSessionRunner.HandleLateAction      TN2327   Broadcasts INT_MESSAGE_PROCESS_RESCHEDULED_OTHER
  08.03.05 pk                                            TN2327   Broadcast INT_MESSAGE_PROCESS_RESCHEDULE_WAIT on register late action
  11.03.05 pk                               TN2339.2 passes nil for IExceptionHandler to TSchedThread.Create
  17.03.05 pk                               TN2352.1 Calls to TExexThread.Create removed
  18.03.05 pk TSchedProcessRunner.MessageProc            TN2356   on INT_MESSAGE_PROCESS_RESCHEDULED_OTHER must also Dequeue MessageQueue
  29.03.05 pk TSchedSessionRunner.HandleLateAction       TN2356   always broadcast INT_MESSAGE_PROCESS_RESCHEDULED_OTHER
  31.03.05 pk TSchedSessionRunner                        TN2362   inherits from TExecutable
  06.04.05 pk TSchedProcessRunner.MessageProc            TN2356   on INT_MESSAGE_PROCESS_RESCHEDULED_OTHER dont dequeue
  19.04.05 pk TSchedActionRunner                         TN2365   ActionName replaced by Description
  19.04.05 pk TSchedSessionRunner.Create                 TN2393   Do not create Clock
  22.04.05 pk HandleTermProcEvent                        TN2399   More detailed exception
  29.04.05 pk TSchedProcessRunner.MessageProc            TN2408   INT_MESSAGE_PROCESS_RESCHEDULE_WAIT added to the mask
  02.05.05 pk TSchedProcessRunner.MessageProc            TN2408   INT_MESSAGE_PROCESS_RESCHEDULED_OTHER
  04.10.05 pk                                            TN2650   Better error handling for PrepNext
  12.10.05 pk                                                     Various changes needed for Unshift feature
  07.11.05 pk                                            TN2737   Various changes for Dynamic Scheduling
  14.11.05 pk                                            TN2758   Various changes for Dynamic Scheduling
  18.11.05 pk TSchedActionRunner.ExecuteStep             TN2758   send INT_MESSAGE_PROCESS_ACTIONSTARTED message
  02.05.06 pk TSchedActionRunner                         TN3081   New message - Action ended
  16.05.06 pk TSchedSessionRunner.AllProcsIdle           TN3104   INT_MIN_SHIFT_IN_SECS changed to 2 seconds to avoid NO START WAIT problem after unshift
  23.05.06 pk TSchedSessionRunner.AllProcsIdle           TN3104   if TIdleProcItem.Changed then exit
  29.05.06 pk TSchedSessionRunner.HandleIdleBegin        TN3104   RegisterAllProcsIdle
  06.06.06 pk TSchedSessionRunner.Execute                TN3133   call flushdata before ending
  20.07.06 pk TSchedProcessRunner.Quit                   TN3212   The process should wait for the actionrunner before ending
  16.10.06 pk TSchedSessionRunner.SetStandardMask        TN3380   INT_MESSAGE_SESSION_IDLE_BEGIN now has a higher priority than INT_MESSAGE_SESSION_LATEACTION
  21.11.06 pk                                            TN3424   New: INT_MESSAGE_SESSION_FATALERROR, can be called by ProcessRunner or ActionRunner to register exception
  07.12.06 pk TSchedSessionRunner.Create                 TN3455   create spin lock with safelock=false
  07.12.06 pk BOOL_LOGTHREAD_FREE_ON_TERMINATE           TN3455   new: prevent logthread.waitfor to be called
  08.12.06 pk TSchedProcessRunner.RegisterMessage        TN3458   call registersessionmessage with aMessageType instead of constant
  09.12.06 pk TSchedActionRunner                         TN3458   New fPriority field. Used in error message of exception
  29.01.07 pk                                            TN3527   Shift/Unshift is now based on Scheduler.ocx
  29.01.07 pk                                            TN3527   Various changes needed since schedtimes based on 1/100th of a second instead of second
  26.03.07 pk                                            TN3645   Change priority to processID
  30.05.07 pk TSchedActionRunner.MessageProc             TN3712   Log the message queue
  30.05.07 pk TSchedActionRunner.Log                     TN3712   Prepend the text 'A:' to all log texts
  03.08.07 wl                                            TN3811.2 uses IntMap
  03.07.08 wl                                            TN4157
  31.07.08 pk TSchedRunnerHandle.Create                  TN4125   parameter changed to const
  24.11.08 pk                                            TN4280   changes needed because CreateThread now returns SysHandleID instead of TExecThread
  07.08.09 wl                                            TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               		        TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  09.02.10 pk                                            TN4973   Thread.Resume changed to Thread.Start
  29.11.10 wl                                            TN5370   TLock.WaitForLock statt TLock.WaitFor
  01.03.12 wl                                            TN5822   angepasst
  -------------------------------------------------------------------------------------------------- }

unit SchedRunners;


interface


uses
    Classes,
    syncobjs,
    windows,
    sysutils,
    contnrs,
    ListClasses,
    Streamable,
    MemoryClasses,
    ClockClass,
    ThreadClasses,
    GeneralTypes,
    LockHandle,
    Executable,
    InterfacedNoRef;

const
    INT_MESSAGE_LOG_TEST = 1;
    INT_MESSAGE_LOG_APP = 2;
    INT_MESSAGE_LOG_APPANDTEST = 3;

    INT_MESSAGE_SESSION_BASE = 1000;
    INT_MESSAGE_SESSION_TERMPROC = INT_MESSAGE_SESSION_BASE + 1;
    INT_MESSAGE_SESSION_ERROR = INT_MESSAGE_SESSION_BASE + 2;
    INT_MESSAGE_SESSION_SUSPEND = INT_MESSAGE_SESSION_BASE + 3;
    INT_MESSAGE_SESSION_RESUME = INT_MESSAGE_SESSION_BASE + 4;

    INT_MESSAGE_SESSION_ACTIONSTARTED = INT_MESSAGE_SESSION_BASE + 5;
    INT_MESSAGE_SESSION_ACTIONFINISHED = INT_MESSAGE_SESSION_BASE + 6;
    INT_MESSAGE_SESSION_ACTIONLATE = INT_MESSAGE_SESSION_BASE + 7;

    INT_MESSAGE_SESSION_RESCHED_BEGIN = INT_MESSAGE_SESSION_BASE + 9;
    INT_MESSAGE_SESSION_RESCHED_END = INT_MESSAGE_SESSION_BASE + 10;

    // INT_MESSAGE_SESSION_IDLE_BEGIN          = INT_MESSAGE_SESSION_BASE + 12;
    // INT_MESSAGE_SESSION_IDLE_END            = INT_MESSAGE_SESSION_BASE + 13;
    // INT_MESSAGE_SESSION_ALLPROCSIDLE        = INT_MESSAGE_SESSION_BASE + 14;
    INT_MESSAGE_SESSION_FATALERROR = INT_MESSAGE_SESSION_BASE + 15;

    INT_MESSAGE_SESSION_DATACHANGED = INT_MESSAGE_SESSION_BASE + 20;
    INT_MESSAGE_SESSION_LOG = INT_MESSAGE_SESSION_BASE + 99;

    INT_MESSAGE_PROCESS_BASE = 2000;
    INT_MESSAGE_PROCESS_TERMINATE_ERROR = INT_MESSAGE_PROCESS_BASE + 0;
    INT_MESSAGE_PROCESS_TERMINATE_NORMAL = INT_MESSAGE_PROCESS_BASE + 1;

    INT_MESSAGE_PROCESS_START = INT_MESSAGE_PROCESS_BASE + 2;
    INT_MESSAGE_PROCESS_PREPNEXT = INT_MESSAGE_PROCESS_BASE + 3;

    INT_MESSAGE_PROCESS_FINISHINGWAIT = INT_MESSAGE_PROCESS_BASE + 4;
    INT_MESSAGE_PROCESS_FINISHINGWAIT_DONE = INT_MESSAGE_PROCESS_BASE + 5;

    INT_MESSAGE_PROCESS_SETCURRENTACTION = INT_MESSAGE_PROCESS_BASE + 7;

    INT_MESSAGE_PROCESS_STARTINGWAIT = INT_MESSAGE_PROCESS_BASE + 8;
    INT_MESSAGE_PROCESS_STARTINGWAIT_DONE = INT_MESSAGE_PROCESS_BASE + 9;

    INT_MESSAGE_PROCESS_EXECUTE = INT_MESSAGE_PROCESS_BASE + 10;

    INT_MESSAGE_PROCESS_RESCHEDULE_WAIT = INT_MESSAGE_PROCESS_BASE + 12;
    INT_MESSAGE_PROCESS_REQUESTRESCHEDULE = INT_MESSAGE_PROCESS_BASE + 13;
    INT_MESSAGE_PROCESS_RESCHEDULED = INT_MESSAGE_PROCESS_BASE + 14;
    INT_MESSAGE_PROCESS_RESCHEDULED_OTHER = INT_MESSAGE_PROCESS_BASE + 15;

    INT_MESSAGE_PROCESS_SUSPEND = INT_MESSAGE_PROCESS_BASE + 19;
    INT_MESSAGE_PROCESS_RESUME = INT_MESSAGE_PROCESS_BASE + 20;

    INT_MESSAGE_PROCESS_TERMINATE_INTERN = INT_MESSAGE_PROCESS_BASE + 25;
    INT_MESSAGE_PROCESS_TERMINATE_INTERNWAIT_DONE = INT_MESSAGE_PROCESS_BASE + 26;

    INT_MESSAGE_PROCESS_ACTIONSTARTED = INT_MESSAGE_PROCESS_BASE + 50;
    INT_MESSAGE_PROCESS_ACTIONFINISHED = INT_MESSAGE_PROCESS_BASE + 51;

    INT_MESSAGE_PROCESS_LOG = INT_MESSAGE_PROCESS_BASE + 99;

    INT_STARTING_WAIT_TIMEOUT = 1;
    INT_STARTING_WAIT_TERMINATE = 2;
    INT_STARTING_WAIT_RESCHEDULE = 3;

    INT_ENDING_WAIT_ACTIONEND = 1;
    INT_ENDING_WAIT_TIMEOUT = 2;
    INT_ENDING_WAIT_OTHER = 3;

    INT_MESSAGE_ACTION_BASE = 3000;
    INT_MESSAGE_ACTION_ENDED = INT_MESSAGE_ACTION_BASE + 1;
    INT_MESSAGE_ACTION_TERMINATE = INT_MESSAGE_ACTION_BASE + 2;
    INT_MESSAGE_ACTION_EXECUTE = INT_MESSAGE_ACTION_BASE + 3;

    GUID_SCHED_ACTION_HANDLE: TGUID = '{A5E4130B-7223-4289-88C7-5B7F0A77FF83}';

    BOOL_LOGTHREAD_FREE_ON_TERMINATE = false;

type
    TSchedThread = TExecThread;
    TProcessIDType = cardinal;

    TSchedTime = cardinal;

    TLogType = (ltTest, ltApp, ltBoth);
    TLogProc = procedure(aLogType: TLogType; aCrdTime: cardinal; aMessage: string);
    // ************************************************************General Concept

    TOnRegisterMessage = procedure(aMessageType: TMsgType; aLock: TLock; aSeparateThread: boolean;
        aArgs: TMsgArgs; aFreeArgs: boolean) of object;

    TSchedRunnerHandle = class
    private
        fProcessID: TProcessIDType;
        fOnRegisterMessage: TOnRegisterMessage;
        fThread: TThread;
        fIntfExec: IExecutable;
    public
        constructor Create(aProcessID: TProcessIDType; aOnRegisterMessage: TOnRegisterMessage;
            aThread: TThread; const aIntfExec: IExecutable);
        property ProcessID: TProcessIDType read fProcessID;
        property OnRegisterMessage: TOnRegisterMessage read fOnRegisterMessage write fOnRegisterMessage;
        property Thread: TThread read fThread write fThread;
        property IntfExec: IExecutable read fIntfExec;
    end;

    TSchedActionStep = class
    protected
        function GetDescription(): string; virtual;
        function GetStartTime(): TSchedTime; virtual;
        procedure SetEndTime(aTime: TSchedTime); virtual;
        function GetEndTime(): TSchedTime; virtual;
        function GetStepID(): integer; virtual;
        function GetProcessID(): TProcessIDType; virtual;
    public
        procedure Start(aTime: TSchedTime); virtual;
        procedure Finish(aTime: TSchedTime); virtual;
        procedure Execute(); virtual;
        function IsError(): boolean; virtual;
        property StartTime: TSchedTime read GetStartTime;
        property EndTime: TSchedTime read GetEndTime write SetEndTime;
        property Description: string read GetDescription;
        property StepID: integer read GetStepID;
        property ProcessID: TProcessIDType read GetProcessID;
    end;

    TSchedDataChangeType = (sdcStartTime, sdcEndTime, sdcOther);

    TSchedActionRunnerHandle = class(TSchedRunnerHandle);

    TSchedActionRunner = class(TExecutable)
    protected
        fProcessID: TProcessIDType;
        fStep: TSchedActionStep;
        fMessageQueue: TMaskedMessageQueue;
        fOnProcessRegisterMessage: TOnRegisterMessage;
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
        procedure Quit();
        procedure QuitInternal();
        procedure ExecuteStep();
        procedure StepStarted();
        procedure StepFinished();
        procedure RegisterProcessMessage(aMessageType: TMsgType; aArgs: TMsgArgs);
        procedure Log(aLogType: TLogType; aTime: TSchedTime; const aText: string);
    public
        constructor Create(aProcessID: TProcessIDType; aOnProcessRegisterMessage: TOnRegisterMessage);
        procedure Execute(); override;
        procedure RegisterMessage(aMessageType: TMsgType; aLock: TLock; aSeparateThread: boolean;
            aArgs: TMsgArgs; aFreeArgs: boolean);
        procedure RegisterSimpleMessage(aMessageType: TMsgType);
    end;

    TNextActionMode = (namOk, namEOF, namError);

    TNextActionResultRec = record
        Mode: TNextActionMode;
        Msg: string;
        Action: TSchedActionStep;
    end;

    TSchedProcessRunnerHandle = class(TSchedRunnerHandle);

    TSchedProcessRunner = class(TExecutable)

    protected
        fProcessID: TProcessIDType;
        fMessageQueue: TMaskedMessageQueue;
        fThread: TThread;

        fCurrentStep: TSchedActionStep;
        fPreparedAction: TNextActionResultRec;
        fOnSessionRegisterMessage: TOnRegisterMessage;
        fSessionLock: TSpinLock;
        fTimedMessageQueue: TTimedMessageQueue;
        fActionRunnerHandle: TSchedActionRunnerHandle;

        procedure CreateActionThread(const aSchedActionStep: TSchedActionStep); virtual;
        procedure CreateTimedMessageQueueThread(aTimedMessageQueue: TTimedMessageQueue); virtual;
        procedure RegisterTerminateProcToSession(); virtual;
        procedure Log(aLogType: TLogType; aTime: TSchedTime; const aText: string);
        procedure SetCurrentAction();
        procedure CurrentActionStartExecute();
        procedure CurrentActionEndExecute();
        function NextActionPrepare(): boolean;
        function PrepareNextSchedAction(): TNextActionResultRec; virtual;
        function CurrentActionStartingWait(): integer;
        function CurrentActionEndingWait(): integer;
        procedure RegisterLateActionToSession(const aSchedActionStep: TSchedActionStep;
            aActionStepTime: TSchedTime);
        procedure CurrentActionStarted(aTime: TSchedTime); virtual;
        procedure CurrentActionEnded(aTime: TSchedTime); virtual;
        procedure RegisterStart;
        procedure ActionThreadEnded(Sender: TObject);
        procedure ResumeCurrentActionThread(); virtual;
        procedure SuspendCurrentActionThread(); virtual;
        procedure RegisterMessage(aMessageType: TMsgType; aLock: TLock; aSeparateThread: boolean;
            aArgs: TMsgArgs; aFreeArgs: boolean);
        procedure RegisterSimpleMessage(aMessageType: TMsgType; aSeparateThread: boolean);
        procedure RegisterSessionMessage(aMessageType: TMsgType; aLock: TLock; aSeparateThread: boolean;
            aArgs: TMsgArgs);
        procedure RegisterActionMessage(aMessageType: TMsgType);

        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
        procedure QuitInternal();
        procedure Quit();
        // procedure   ChangeIdle( aIsIdle : boolean; aStartTime : TSchedTime );
        procedure TimedMessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
        procedure RegisterTimedMessage(aMessageType: TMsgType; aTime: TSchedTime;
            aActionStepTime: TSchedTime);
        procedure FirstActionPrepare();

        function CreateActionRunner(aOnRegisterMessage: TOnRegisterMessage)
            : TSchedActionRunnerHandle; virtual;
        procedure CreateActionRunnerThread(aActionRunnerHandle: TSchedActionRunnerHandle); virtual;
    public
        constructor Create(aProcessID: TProcessIDType; aOnSessionRegisterMessage: TOnRegisterMessage;
            aSessionLock: TSpinLock); virtual;
        destructor Destroy; override;
        procedure Execute(); override;
    end;

    TSchedLogManager = class;
    TEnSchedMode = (smStatic, smActive);

    TSchedSessionRunner = class(TExecutable)
    protected
        fThread: TThread;
        fProcessList: TIntegerKeyObjectValueList;
        // fIdleProcs         : TIntList;
        fMessageQueue: TMaskedMessageQueue;
        fGeneralLock: TSpinLock;
        fLogManager: TSchedLogManager;
        fClock: TClock;
        fLatestDataChangeAt: TSchedTime;
        fReschedWaitAt: TSchedTime;
        fSuspendedPriorities: TIntArray;
        fAllProcsIdleAllowed: boolean;
        function CreateLogManager(): TSchedLogManager; virtual;
        procedure StarTSchedLogManager();
        procedure EndLogManager();
        function GetProcessHandleByProcessID(aProcessID: cardinal): TSchedProcessRunnerHandle;
        function GetProcessHandle(aIndex: integer): TSchedProcessRunnerHandle;
        procedure InitResourceList();
        function CreateProcessRunnerThread(aProcessHandle: TSchedProcessRunnerHandle): TThread; virtual;
        function CreateProcessRunner(aProcessID: cardinal; aOnRegisterMessage: TOnRegisterMessage)
            : TSchedProcessRunnerHandle; virtual;
        function CreateMessageQueue(): TMaskedMessageQueue; virtual;
        // procedure   InitProcessRunners();
        procedure FinalizeProcessRunners();
        procedure StartProcess(aProcessHandle: TSchedProcessRunnerHandle);
        procedure StartProcesses();
        procedure AddProcessRunners(); virtual;
        function AddProcessRunner(aProcessID: cardinal): integer;
        procedure SetTimer(aBlnValue: boolean); virtual;
        procedure Log(aLogType: TLogType; aTime: TSchedTime; aText: string); virtual;
        procedure DoLog(aLogType: TLogType; aID: cardinal; aTime: TSchedTime; aText: string); virtual;
        procedure QuitInternal();
        procedure RegisterDataChanged();
        procedure CreateLogManagerThread(); virtual;
        // procedure   AllProcsIdle( aRequestedByProc : TProcessIDType );
        // procedure   SetChangingIdleStatus();
        function AttemptShiftDueToAction(const aSchedActionStep: TSchedActionStep; aActionTime: TSchedTime;
            out oShiftBy: cardinal): boolean; virtual;
        function AttemptShift(aShiftBy: TSchedTime): boolean; virtual;
        function ActionTimeChanged(const aSchedActionStep: TSchedActionStep; aTime: TSchedTime;
            aChangeType: TSchedDataChangeType): boolean; virtual;
        procedure ScheduleUnshift(var vShiftBy: cardinal);
        function AttemptUnshift(aCurrentTime: TSchedTime; var vShiftBy: TSchedTime): boolean; virtual;
        procedure Rescheduled; virtual;
        procedure ShiftToAvoidMissedTime(aShiftInClockUnits: TSchedTime);
        procedure SetStandardMask;
        function SuspendProcs(): TSchedTime;
        procedure ResumeProcs();
        // procedure   RemoveProcessFromIdle( aProcessID : TProcessIDType );
        procedure FlushData(); virtual;
        // MessageQueue Event Handlers

        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
        procedure HandleGlobalError(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
        procedure HandleFatalError(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
        procedure HandleTermProc(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
        procedure HandleLateAction(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
        procedure HandleActionStarted(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
        procedure HandleActionFinished(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
        procedure HandleSuspend(var vAccept: boolean; aMessage: TMessageQueueItem);
        procedure HandleResume(var vAccept: boolean; aMessage: TMessageQueueItem);
        procedure HandleDataChanged(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
        procedure HandleReschedBegin(var vAccept: boolean; aMessage: TMessageQueueItem);
        procedure HandleReschedEnd(var vAccept: boolean; aMessage: TMessageQueueItem);
        // procedure   HandleIdleBegin( var vAccept : boolean; aMessage : TMessageQueueItem ); virtual;
        // procedure   HandleIdleEnd( var vAccept : boolean; aMessage : TMessageQueueItem ); virtual;
        // procedure   HandleAllProcsIdle( var vAccept : boolean; aMessage : TMessageQueueItem );
    public
        constructor Create(aClock: TClock);
        destructor Destroy(); override;
        procedure Execute(); override;

        // MessageQueue Event Registers
        procedure RegisterMessage(aMessageType: TMsgType; aLock: TLock; aSeparateThread: boolean;
            aArgs: TMsgArgs; aFreeArgs: boolean); virtual;
        procedure RegisterLogMessage(aMsgType: TMsgType; aID: cardinal; aTime: TSchedTime; aText: string);
        procedure BroadcastProcMessageExclusive(aMessageType: TMsgType; aLock: TLock; aArgs: TMsgArgs;
            aExcludeProcessID: TProcessIDType);
        procedure BroadcastProcMessage(aMessageType: TMsgType; aLock: TLock; aArgs: TMsgArgs);
        procedure RegisterProcMessage(aProcessHandle: TSchedProcessRunnerHandle; aMessageType: TMsgType;
            aLock: TLock; aArgs: TMsgArgs);
        // procedure   RegisterAllProcsIdle( aRequestedByProc : TProcessIDType );

        procedure MessageLog(aStrMessage: string);
        procedure Quit();

        property MessageQueue: TMaskedMessageQueue read FMessageQueue;
        property ProcessHandles[aIntIndex: integer]: TSchedProcessRunnerHandle read GetProcessHandle;
        property ProcessHandlesByProcessID[aProcessID: TProcessIDType]: TSchedProcessRunnerHandle
            read GetProcessHandleByProcessID;
        property Clock: TClock read fClock;
        property ReschedWaitAt: cardinal read fReschedWaitAt;
    end;

    TSchedLogManager = class(TInterfacedNoRef, IExecutable)
    protected
        fMessageQueue: TMessageQueue;
        fThread: TThread;
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure RegisterMessage(aMsgType: TMsgType; aID: cardinal; aTime: TSchedTime; aText: string);
        procedure Close(aTime: TSchedTime);
        property Intf: TMessageQueue read fMessageQueue implements IExecutable;
    end;

    TIdleProcItem = class
    private
        fTime: TSchedTime;
        fChanging: boolean;
    public
        constructor Create(aTime: TSchedTime; aChanging: boolean);
        property Time: TSchedTime read fTime write fTime;
        property Changing: boolean read fChanging write fChanging;
    end;

    // ************************************** Globals ***************************************************

var
    gClock: TClock;

function ElapsedTime(): TSchedTime;
function IntToSchedTime(aValue: integer): TSchedTime;


// ##################################################################################################


implementation


uses
    Dialogs,
    Math,
    UtilLib;

const
    STR_CHILD_SYNCH_EVENT_NAME: string = 'SynchChild:%d';
    STR_PARENT_SYNCH_EVENT_NAME: string = 'SynchParent:%d';

function ElapsedTime(): TSchedTime;
begin
    result := 0;
    if Assigned(gClock) then
        result := gClock.ElapsedTime();
end;

function IntToSchedTime(aValue: integer): TSchedTime;
begin
    result := Abs(aValue);
end;

constructor TSchedRunnerHandle.Create(aProcessID: TProcessIDType; aOnRegisterMessage: TOnRegisterMessage;
    aThread: TThread; const aIntfExec: IExecutable);
begin
    inherited Create();
    fProcessID := aProcessID;
    fOnRegisterMessage := aOnRegisterMessage;
    fThread := aThread;
    fIntfExec := aIntfExec;
end;

constructor TIdleProcItem.Create(aTime: TSchedTime; aChanging: boolean);
begin
    inherited Create;
    fTime := aTime;
    fChanging := aChanging;
end;

// ************************************* TSchedSessionRunner ***************************************
// --------------------------------------------------------------------------------------------------------------
constructor TSchedSessionRunner.Create(aClock: TClock);
// --------------------------------------------------------------------------------------------------------------
begin
    inherited Create();
    fProcessList := TIntegerKeyObjectValueList.Create();
    // fIdleProcs := TIntList.Create();
    // fIdleProcs.Duplicates := dupError;
    fMessageQueue := CreateMessageQueue();
    SetStandardMask();
    fGeneralLock := TSpinLock.Create(false);
    fLogManager := CreateLogManager();
    fClock := aClock;
    gClock := aClock;
    fLatestDataChangeAt := 0;
    SetLength(fSuspendedPriorities, 0);
    fAllProcsIdleAllowed := true;
end;

// --------------------------------------------------------------------------------------------------------------
destructor TSchedSessionRunner.Destroy();
// --------------------------------------------------------------------------------------------------------------
begin
    fMessageQueue.Free;
    // fIdleProcs.Free;
    fProcessList.Free;
    fGeneralLock.Free;
    fLogManager.Free;
    inherited Destroy();
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.InitResourceList();
// --------------------------------------------------------------------------------------------------------------
begin
    {
      // Build the Session Resourcelist
      var i:integer;
      //TempResource:TSchedResource;
      begin

      for i:=0 to FSchedSession.ResourceCount-1 do begin
      TempResource:=FSchedSession.Resources[i];
      FResourceList.AddObject(IntToStr(TempResource.ID), TSchedResourceExt.Create(TempResource));
      end;
    }
end;

procedure TSchedSessionRunner.CreateLogManagerThread();
begin
end;

function TSchedSessionRunner.CreateLogManager(): TSchedLogManager;
begin
    result := TSchedLogManager.Create();
end;

procedure TSchedSessionRunner.StarTSchedLogManager();
begin
    CreateLogManagerThread();
end;

procedure TSchedSessionRunner.EndLogManager();
const
    INT_LATER_THAN_LAST_ELAPSED_TIME = 1;
var
    xTime: TSchedTime;
begin
    xTime := gClock.ElapsedTime() + INT_LATER_THAN_LAST_ELAPSED_TIME;
    fLogManager.Close(xTime);

    // since the log thread uses the log function of the session, we need to make sure the log thread
    // is finished before destroying the session, otherwise the log thread will try to call a function
    // of an object which has been destroyed
    if not BOOL_LOGTHREAD_FREE_ON_TERMINATE then
    begin
        { TODO -oPK : no fLogManagerThread }
        // fLogManagerThread.WaitFor;
        // fLogManagerThread.Free;
    end;
end;

procedure TSchedSessionRunner.SetStandardMask();
begin
    // 16.10.06 pk: INT_MESSAGE_SESSION_IDLE_BEGIN must have higher priority than INT_MESSAGE_SESSION_LATEACTION in order to
    // avoid the case where an old INT_MESSAGE_SESSION_IDLE_BEGIN of a process is handled AFTER a newer INT_MESSAGE_SESSION_IDLE_BEGIN
    // for the same process which is regitered after a shift
    fMessageQueue.SetMask([INT_MESSAGE_SESSION_TERMPROC, INT_MESSAGE_SESSION_FATALERROR,
        INT_MESSAGE_SESSION_ERROR,
        // INT_MESSAGE_SESSION_IDLE_BEGIN, INT_MESSAGE_SESSION_IDLE_END,
        INT_MESSAGE_SESSION_ACTIONLATE, INT_MESSAGE_SESSION_ACTIONFINISHED, INT_MESSAGE_SESSION_SUSPEND,
        INT_MESSAGE_SESSION_RESCHED_BEGIN, INT_MESSAGE_SESSION_ACTIONSTARTED,
        INT_MESSAGE_SESSION_DATACHANGED]);
end;

// --------------------------------------------------------------------------------------------------------------
function TSchedSessionRunner.CreateMessageQueue(): TMaskedMessageQueue;
// --------------------------------------------------------------------------------------------------------------
begin
    result := TMaskedMessageQueue.Create();

end;
{
  //--------------------------------------------------------------------------------------------------------------
  function TSchedSessionRunner.Schedule( var aStrError:string; aIntStartTime:integer; aScheduleMode : TEnSchedMode ) : boolean;
  //--------------------------------------------------------------------------------------------------------------
  begin
  result := true;
  end;
}

// *********Register
// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.RegisterMessage(aMessageType: TMsgType; aLock: TLock; aSeparateThread: boolean;
    aArgs: TMsgArgs; aFreeArgs: boolean);
// --------------------------------------------------------------------------------------------------------------
var
    xMessageItem: TMessageQueueItem;
    xSeperateThread: boolean;
begin
    if (aMessageType = INT_MESSAGE_SESSION_ACTIONLATE) then
    begin
        BroadcastProcMessageExclusive(INT_MESSAGE_PROCESS_RESCHEDULE_WAIT, nil, TMsgArgs.EmptyArgs(),
            aArgs[0].AsCard);
    end;

    if (aMessageType = INT_MESSAGE_SESSION_LOG) then
    begin
        DoLog(TLogType(aArgs[0].AsInt), aArgs[1].AsCard, aArgs[2].AsCard, aArgs[3].AsStr);
    end
    else
    begin
        xSeperateThread := aSeparateThread or (aMessageType = INT_MESSAGE_SESSION_FATALERROR);
        xMessageItem := TMessageQueueItem.Create(aMessageType, 0, aLock, xSeperateThread, self.MessageProc,
            aArgs, aFreeArgs);
        FMessageQueue.EnqueueMessage(xMessageItem);
    end;

end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.RegisterLogMessage(aMsgType: TMsgType; aID: cardinal; aTime: TSchedTime;
    aText: string);
// --------------------------------------------------------------------------------------------------------------
begin
    fLogManager.RegisterMessage(aMsgType, aID, aTime, aText);
end;

procedure TSchedSessionRunner.RegisterProcMessage(aProcessHandle: TSchedProcessRunnerHandle;
    aMessageType: TMsgType; aLock: TLock; aArgs: TMsgArgs);
begin
    aProcessHandle.OnRegisterMessage(aMessageType, aLock, false, aArgs, true);
end;

procedure TSchedSessionRunner.BroadcastProcMessageExclusive(aMessageType: TMsgType; aLock: TLock;
    aArgs: TMsgArgs; aExcludeProcessID: TProcessIDType);
var
    i: integer;
    xProcessHandle: TSchedProcessRunnerHandle;
begin
    for i := 0 to FProcessList.Count - 1 do
    begin
        xProcessHandle := self.ProcessHandles[i];
        if xProcessHandle.ProcessID = aExcludeProcessID then
            CONTINUE;
        RegisterProcMessage(xProcessHandle, aMessageType, aLock, aArgs);
    end;
end;

procedure TSchedSessionRunner.BroadcastProcMessage(aMessageType: TMsgType; aLock: TLock; aArgs: TMsgArgs);
var
    i: integer;
    xProcessHandle: TSchedProcessRunnerHandle;
begin
    for i := 0 to FProcessList.Count - 1 do
    begin
        xProcessHandle := self.ProcessHandles[i];
        RegisterProcMessage(xProcessHandle, aMessageType, aLock, aArgs);
    end;
end;

// procedure TSchedSessionRunner.RegisterAllProcsIdle( aRequestedByProc : TProcessIDType );
// begin
// RegisterMessage( INT_MESSAGE_SESSION_ALLPROCSIDLE, nil, false, TMsgArgs.ArgsArrayOf( [ TCardArg.Create( aRequestedByProc ) ] ), false );
// end;

// *********Handlers
// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.HandleGlobalError(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
begin
    self.Quit();
    // if FDetailedMessages then
    self.MessageLog('Global Error');
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.HandleFatalError(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
begin
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.HandleTermProc(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
var
    xIndex: integer;
    xProcessID: TProcessIDType;
    xProcessHandle: TSchedProcessRunnerHandle;
begin
    xProcessID := aMessage.Args[0].AsCard;
    xIndex := fProcessList.IndexOf(xProcessID);
    if xIndex = -1 then
        raise Exception.CreateFmt('Could not remove process [%d]!', [xProcessID]);

    xProcessHandle := self.ProcessHandles[xIndex];
    xProcessHandle.Free;

    fProcessList.Delete(xIndex);

    if FProcessList.Count = 0 then
        self.QuitInternal();

    // RemoveProcessFromIdle( xProcessID );

    // if FDetailedMessages then
    self.MessageLog(format('Process removed [%d]', [xProcessID]));


    // if ( fProcessList.Count > 0 ) and fAllProcsIdleAllowed  then
    // AllProcsIdle( xProcessID );

end;

function TSchedSessionRunner.ActionTimeChanged(const aSchedActionStep: TSchedActionStep; aTime: TSchedTime;
    aChangeType: TSchedDataChangeType): boolean;
begin
    result := false;
end;

function TSchedSessionRunner.AttemptShiftDueToAction(const aSchedActionStep: TSchedActionStep;
    aActionTime: TSchedTime; out oShiftBy: TSchedTime): boolean;
begin
    result := false;
end;

function TSchedSessionRunner.AttemptShift(aShiftBy: TSchedTime): boolean;
begin
    result := false;
end;

// --------------------------------------------------------------------------------------------------------------
function TSchedSessionRunner.AttemptUnshift(aCurrentTime: TSchedTime; var vShiftBy: TSchedTime): boolean;
// --------------------------------------------------------------------------------------------------------------
begin
    result := false;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.ScheduleUnshift(var vShiftBy: cardinal);
// --------------------------------------------------------------------------------------------------------------
var
    xShifted: boolean;
begin
    BroadcastProcMessage(INT_MESSAGE_PROCESS_RESCHEDULE_WAIT, nil, TMsgArgs.EmptyArgs());
    xShifted := AttemptUnshift(gClock.ElapsedTimeAs(tuSec) + 1, vShiftBy);
    if xShifted then
    begin
        RegisterDataChanged();
        self.MessageLog(Format('Session requested unshift of %d!', [vShiftBy]));
    end
    else
    begin
        self.MessageLog('Session unshift request - IGNORED');
    end;

    BroadcastProcMessage(INT_MESSAGE_PROCESS_RESCHEDULED_OTHER, nil, TMsgArgs.EmptyArgs());
end;

function GetStepFromObj(const aObj: TObject): TSchedActionStep;
begin
    ASSERT(aObj is TSchedActionStep, 'Action handle interface not supported');
    result := aObj as TSchedActionStep;
end;

procedure TSchedSessionRunner.HandleActionFinished(var vAccept: boolean; aMessage: TMessageQueueItem);
var
    xProcessHandle: TSchedProcessRunnerHandle;

    xSchedActionStep: TSchedActionStep;
    xShifted: boolean;
begin
    xSchedActionStep := GetStepFromObj(aMessage.Args[2].AsObj);
    xShifted := ActionTimeChanged(xSchedActionStep, aMessage.Args[1].AsCard, sdcEndTime);

    xProcessHandle := self.ProcessHandlesByProcessID[xSchedActionStep.ProcessID];
    RegisterProcMessage(xProcessHandle, INT_MESSAGE_PROCESS_START, nil, TMsgArgs.EmptyArgs());

    if xShifted then
    begin
        self.MessageLog(Format('Process %d, Action %s changed times!', [xSchedActionStep.ProcessID,
            xSchedActionStep.Description]));
        BroadcastProcMessageExclusive(INT_MESSAGE_PROCESS_RESCHEDULE_WAIT, nil, TMsgArgs.EmptyArgs(),
            xSchedActionStep.ProcessID);
        BroadcastProcMessageExclusive(INT_MESSAGE_PROCESS_RESCHEDULED_OTHER, nil, TMsgArgs.EmptyArgs(),
            xSchedActionStep.ProcessID);
    end
    else
    begin
        self.MessageLog(Format('Process %d, shift request - IGNORED', [xSchedActionStep.ProcessID]));
    end;
end;

procedure TSchedSessionRunner.HandleActionStarted(var vAccept: boolean; aMessage: TMessageQueueItem);
var
    xSchedActionStep: TSchedActionStep;
begin
    xSchedActionStep := GetStepFromObj(aMessage.Args[2].AsObj);
    ActionTimeChanged(xSchedActionStep, aMessage.Args[1].AsCard, sdcStartTime);
end;

procedure TSchedSessionRunner.HandleLateAction(var vAccept: boolean; aMessage: TMessageQueueItem);

var
    xShiftBy: TSchedTime;
    xProcessID: TProcessIDType;
    xProcessHandle: TSchedProcessRunnerHandle;

    xSchedActionStep: TSchedActionStep;
    xShifted: boolean;
    xActionTime: TSchedTime;
begin
    xProcessID := aMessage.Args[0].AsCard;
    xSchedActionStep := GetStepFromObj(aMessage.Args[1].AsObj);
    xActionTime := aMessage.Args[2].AsCard;
    xShifted := AttemptShiftDueToAction(xSchedActionStep, xActionTime, xShiftBy);

    RegisterDataChanged();

    xProcessHandle := self.ProcessHandlesByProcessID[xProcessID];
    RegisterProcMessage(xProcessHandle, INT_MESSAGE_PROCESS_RESCHEDULED, nil, TMsgArgs.EmptyArgs());

    if xShifted then
    begin
        // SetChangingIdleStatus();
        self.MessageLog(Format('Process %d, requested shift of %d for Action %s!',
            [xProcessID, xShiftBy, xSchedActionStep.Description]));
    end
    else
    begin
        self.MessageLog(Format('Process %d, shift request - IGNORED', [xProcessID]));
    end;

    BroadcastProcMessageExclusive(INT_MESSAGE_PROCESS_RESCHEDULED_OTHER, nil, TMsgArgs.EmptyArgs(),
        xProcessID);

    // if FDetailedMessages then

end;

function TSchedSessionRunner.SuspendProcs(): TSchedTime;
var
    i: integer;
    xSuspendedProcessCount: integer;
    xCurProcessID: TProcessIDType;
begin
    xSuspendedProcessCount := fProcessList.Count;

    if xSuspendedProcessCount > 0 then
    begin
        SetLength(fSuspendedPriorities, xSuspendedProcessCount);

        for i := 0 to fProcessList.Count - 1 do
        begin
            xCurProcessID := self.ProcessHandles[i].ProcessID;
            fSuspendedPriorities[i] := xCurProcessID;
        end;
        fGeneralLock.Lock(xSuspendedProcessCount);

        self.BroadcastProcMessage(INT_MESSAGE_PROCESS_SUSPEND, fGeneralLock, TMsgArgs.EmptyArgs());
        fGeneralLock.WaitForLock(INFINITE);
    end;

    result := gClock.Pause;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.HandleSuspend(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
var
    xPausedAt: cardinal;
begin
    xPausedAt := SuspendProcs();
    self.MessageLog(Format('Suspended @%d', [xPausedAt]));
end;

procedure TSchedSessionRunner.ResumeProcs();
var
    i: integer;
    xProcessHandle: TSchedProcessRunnerHandle;
    xSuspendedProcessCount: integer;
begin
    xSuspendedProcessCount := Length(fSuspendedPriorities);
    if xSuspendedProcessCount > 0 then
    begin
        fGeneralLock.Lock(xSuspendedProcessCount);
        for i := 0 to xSuspendedProcessCount - 1 do
        begin
            xProcessHandle := self.ProcessHandlesByProcessID[fSuspendedPriorities[i]];
            RegisterProcMessage(xProcessHandle, INT_MESSAGE_PROCESS_RESUME, fGeneralLock,
                TMsgArgs.EmptyArgs());
        end;
        fGeneralLock.WaitForLock(INFINITE);
    end;

    SetLength(fSuspendedPriorities, 0);
    gClock.Unpause;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.HandleResume(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
begin
    if gClock.ClockState <> csPaused then
        EXIT;
    ResumeProcs();
    self.MessageLog(Format('Resumed @%d', [gClock.ElapsedTime()]));
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.HandleDataChanged(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
begin
    fLatestDataChangeAt := Max(fLatestDataChangeAt, aMessage.Args[0].AsCard);
    self.MessageLog(Format('DataChanged @%d', [gClock.ElapsedTime()]));
end;
{
  //--------------------------------------------------------------------------------------------------------------
  procedure TSchedSessionRunner.HandleIdleBegin(var vAccept : boolean; aMessage : TMessageQueueItem );
  //--------------------------------------------------------------------------------------------------------------
  var
  xProcessID : TProcessIDType;
  xIndex : integer;
  xIdleItem : TIdleProcItem;
  begin
  xProcessID := aMessage.Args[0].AsCard;
  self.MessageLog( Format( 'IdleBegin @%d - Proc: %d', [ gClock.ElapsedTime(), integer( xProcessID ) ] ) );
  xIndex := fIdleProcs.IndexOf( xProcessID );
  // an Idle item exists already for a process if the process is in its startingwait phase, in this case
  //  we have received an IdleBegin because the process was shifted due to a late action in another process
  if xIndex >= 0 then begin
  xIdleItem := ( fIdleProcs.Objects[ xIndex ] as TIdleProcItem );
  xIdleItem.Time := aMessage.Args[1].AsCard;
  xIdleItem.Changing := false;
  // exit and do NOT call allprocsidle if we are handling this due to a shift
  EXIT;
  end;
  fIdleProcs.AddObject( xProcessID, TIdleProcItem.Create( aMessage.Args[1].AsCard, false ) );
  RegisterAllProcsIdle( xProcessID );
  end;

  procedure TSchedSessionRunner.RemoveProcessFromIdle( aProcessID : TProcessIDType );
  var
  xIndex : integer;
  begin
  xIndex := fIdleProcs.IndexOf( aProcessID );
  if xIndex < 0 then EXIT;
  fIdleProcs.Objects[ xIndex ].Free;
  fIdleProcs.Delete( xIndex );
  end;

  //--------------------------------------------------------------------------------------------------------------
  procedure TSchedSessionRunner.HandleIdleEnd( var vAccept : boolean; aMessage : TMessageQueueItem );
  //--------------------------------------------------------------------------------------------------------------
  var
  xProcessID : TProcessIDType;

  begin
  xProcessID := aMessage.Args[0].AsCard;
  self.MessageLog( Format( 'IdleEnd @%d - Proc: %d', [ gClock.ElapsedTime(), integer( xProcessID ) ] ) );
  RemoveProcessFromIdle( xProcessID );
  end;

  //--------------------------------------------------------------------------------------------------------------
  procedure TSchedSessionRunner.HandleAllProcsIdle( var vAccept : boolean; aMessage : TMessageQueueItem );
  //--------------------------------------------------------------------------------------------------------------
  var
  xProcessID : TProcessIDType;

  begin
  xProcessID := aMessage.Args[0].AsCard;
  self.MessageLog( Format( 'AllProcsIdle @%d - Proc: %d', [ gClock.ElapsedTime(), integer( xProcessID ) ] ) );
  AllProcsIdle( xProcessID );
  end;
}

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.HandleReschedBegin(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
begin
    fReschedWaitAt := 0;
    self.MessageLog('Reschedule - BEGIN');
    SuspendProcs();
    self.FlushData();
    fReschedWaitAt := gClock.ElapsedTime();
end;

procedure TSchedSessionRunner.Rescheduled;
begin
end;

procedure TSchedSessionRunner.ShiftToAvoidMissedTime(aShiftInClockUnits: TSchedTime);
var
    xShift: TSchedTime;
begin
    self.MessageLog(Format('Shifted by - %d', [aShiftInClockUnits]));
    xShift := aShiftInClockUnits;
    if xShift > 0 then
    begin
        AttemptShift(xShift);
    end;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.HandleReschedEnd(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
var
    xCancelled: boolean;
begin
    xCancelled := aMessage.Args[0].AsBool;
    if xCancelled then
    begin
        self.MessageLog('Reschedule - CANCELLED');
    end
    else
    begin
        Rescheduled();
        self.MessageLog('Reschedule - END');
    end;

    // ShiftToAvoidMissedTime();
    RegisterDataChanged();
    ResumeProcs();
end;

// *********Choose Handler
// -----------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
var
    xError: string;
begin
    Log(ltTest, gClock.ElapsedTime(), Format('Message:%d, Queued:%s ', [aMessage.MessageType,
        fMessageQueue.Log]));
    try
        // OutputDebugString( PChar( Format( 'S - Handle Message %d', [ aMessage.MessageType ] ) ) );
        case aMessage.MessageType of
            INT_MESSAGE_SESSION_TERMPROC:
                begin
                    HandleTermProc(vAccept, aMessage);
                    // Don't change mask
                end;
            INT_MESSAGE_SESSION_FATALERROR:
                begin
                    HandleFatalError(vAccept, aMessage);
                end;
            INT_MESSAGE_SESSION_ERROR:
                begin
                    HandleGlobalError(vAccept, aMessage);
                    fMessageQueue.DeleteFromMask([INT_MESSAGE_SESSION_SUSPEND,
                        INT_MESSAGE_SESSION_RESCHED_BEGIN]);
                end;
            INT_MESSAGE_SESSION_ACTIONLATE:
                begin
                    HandleLateAction(vAccept, aMessage);
                    // Don't change mask
                end;
            INT_MESSAGE_SESSION_SUSPEND:
                begin
                    HandleSuspend(vAccept, aMessage);
                    fMessageQueue.ChangeMaskTemp([INT_MESSAGE_SESSION_SUSPEND,
                        INT_MESSAGE_SESSION_RESCHED_BEGIN, INT_MESSAGE_SESSION_ERROR,
                        INT_MESSAGE_SESSION_ACTIONLATE], [INT_MESSAGE_SESSION_RESUME], 0);
                end;
            INT_MESSAGE_SESSION_RESUME:
                begin
                    fMessageQueue.RestoreMask;
                    HandleResume(vAccept, aMessage);
                end;

            INT_MESSAGE_SESSION_ACTIONSTARTED:
                begin
                    HandleActionStarted(vAccept, aMessage);
                end;

            INT_MESSAGE_SESSION_ACTIONFINISHED:
                begin
                    HandleActionFinished(vAccept, aMessage);
                end;

            INT_MESSAGE_SESSION_DATACHANGED:
                begin
                    HandleDataChanged(vAccept, aMessage);
                end;
            INT_MESSAGE_SESSION_RESCHED_BEGIN:
                begin
                    HandleReschedBegin(vAccept, aMessage);
                    fMessageQueue.ChangeMaskTemp([INT_MESSAGE_SESSION_RESCHED_BEGIN,
                        INT_MESSAGE_SESSION_SUSPEND, INT_MESSAGE_SESSION_ERROR,
                        INT_MESSAGE_SESSION_ACTIONLATE], [INT_MESSAGE_SESSION_RESCHED_END], 0);
                end;
            INT_MESSAGE_SESSION_RESCHED_END:
                begin
                    fMessageQueue.RestoreMask;
                    HandleReschedEnd(vAccept, aMessage);
                end;
            // INT_MESSAGE_SESSION_IDLE_BEGIN      :
            // begin
            // HandleIdleBegin( vAccept, aMessage );
            // end;
            // INT_MESSAGE_SESSION_IDLE_END        :
            // begin
            // HandleIdleEnd( vAccept, aMessage );
            // end;
            // INT_MESSAGE_SESSION_ALLPROCSIDLE   :
            // begin
            // HandleAllProcsIdle( vAccept, aMessage );
            // end;
        end;
    except
        on E: Exception do
        begin
            xError := Format('Session MessageProc -> Message:%d -> %s', [aMessage.MessageType, E.Message]);
            Log(ltBoth, gClock.ElapsedTime(), xError);
            self.RegisterMessage(INT_MESSAGE_SESSION_FATALERROR, nil, false,
                TMsgArgs.ArgsArrayOf([TStrStreamableItem.Create(xError)]), true);
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.MessageLog(aStrMessage: string);
// --------------------------------------------------------------------------------------------------------------
begin
    Log(ltTest, Max(gClock.ElapsedTime(), 0), format('Session.HandleEvent => %s!', [aStrMessage]));
end;

procedure TSchedSessionRunner.RegisterDataChanged();
begin
    self.RegisterMessage(INT_MESSAGE_SESSION_DATACHANGED, nil, false,
        TMsgArgs.ArgsArrayOf([TCardStreamableItem.Create(gClock.ElapsedTime())]), true);
end;

procedure TSchedSessionRunner.Quit();
begin
    fAllProcsIdleAllowed := false;
    BroadcastProcMessage(INT_MESSAGE_PROCESS_TERMINATE_ERROR, nil, TMsgArgs.EmptyArgs());
end;

procedure TSchedSessionRunner.QuitInternal();
begin
    fMessageQueue.SetMask([INT_MESSAGE_QUIT]);
    RegisterMessage(INT_MESSAGE_QUIT, nil, false, TMsgArgs.EmptyArgs(), true);
end;

{
  procedure TSchedSessionRunner.SetChangingIdleStatus();
  var
  i : integer;
  xIdleItem : TIdleProcItem;
  begin
  for i := 0 to fIdleProcs.Count - 1 do begin
  xIdleItem := ( fIdleProcs.Objects[ i ] as TIdleProcItem );
  xIdleItem.Changing := true;
  end;
  end;

  procedure TSchedSessionRunner.AllProcsIdle( aRequestedByProc : TProcessIDType );
  const
  INT_IDLE_QUALIFY_IN_SECS = 2; // 2 seconds
  INT_MIN_SHIFT_IN_SECS = 2;
  var
  i : integer;
  xCurTime, xIdleQualify, xEarliestProcTime, xShiftBy : TSchedTime;
  xIdleItem : TIdleProcItem;
  begin
  if fIdleProcs.Count <> fProcessList.Count then EXIT;
  xCurTime := gClock.ElapsedTime();
  xIdleQualify := gClock.ConvertToClockTimeUnit( INT_IDLE_QUALIFY_IN_SECS, tuSec );
  xEarliestProcTime := High( TSchedTime );
  for i := 0 to fIdleProcs.Count - 1 do begin
  xIdleItem := ( fIdleProcs.Objects[ i ] as TIdleProcItem );
  if xIdleItem.Changing then begin
  RegisterAllProcsIdle( aRequestedByProc );
  EXIT;
  end;
  if ( xIdleItem.Time - xCurTime ) < xIdleQualify then EXIT;
  xEarliestProcTime := Min( xEarliestProcTime, xIdleItem.Time );
  end;

  xShiftBy := xEarliestProcTime - xCurTime;
  if xShiftBy <= INT_MIN_SHIFT_IN_SECS then EXIT;
  xShiftBy := xShiftBy - INT_MIN_SHIFT_IN_SECS; //subtract by min shift time to give scheduler some buffer time to react to changes
  self.MessageLog( Format( 'ALL PROCS IDLE - Earliest start @%d', [ xEarliestProcTime ] ) );
  ScheduleUnshift( xShiftBy );
  end;
}
// --------------------------------------------------------------------------------------------------------------
function TSchedSessionRunner.GetProcessHandleByProcessID(aProcessID: TProcessIDType)
    : TSchedProcessRunnerHandle;
// --------------------------------------------------------------------------------------------------------------
var
    intIndex: integer;
begin
    result := nil;
    intIndex := FProcessList.IndexOf(aProcessID);
    if intIndex = -1 then
        Exit;
    result := ProcessHandles[intIndex];

end;

// --------------------------------------------------------------------------------------------------------------
function TSchedSessionRunner.GetProcessHandle(aIndex: integer): TSchedProcessRunnerHandle;
// --------------------------------------------------------------------------------------------------------------
begin
    result := nil;
    if aIndex < FProcessList.Count then
        result := FProcessList.Objects[aIndex] as TSchedProcessRunnerHandle;
end;

function TSchedSessionRunner.CreateProcessRunnerThread(aProcessHandle: TSchedProcessRunnerHandle): TThread;
begin
    result := nil;
    // result := TSchedThread.Create( true, true, false, aProcessRunner, nil );
end;

// --------------------------------------------------------------------------------------------------------------
function TSchedSessionRunner.CreateProcessRunner(aProcessID: TProcessIDType;
    aOnRegisterMessage: TOnRegisterMessage): TSchedProcessRunnerHandle;
// --------------------------------------------------------------------------------------------------------------
begin
    result := nil;
end;

procedure TSchedSessionRunner.AddProcessRunners();
begin
end;

// --------------------------------------------------------------------------------------------------------------
function TSchedSessionRunner.AddProcessRunner(aProcessID: TProcessIDType): integer;
// --------------------------------------------------------------------------------------------------------------
// Create a Process Thread and Initialize its Optional fields.
var
    xProcessHandle: TSchedProcessRunnerHandle;
begin
    xProcessHandle := CreateProcessRunner(aProcessID, RegisterMessage);
    result := FProcessList.AddObject(aProcessID, xProcessHandle);
    xProcessHandle.Thread := CreateProcessRunnerThread(xProcessHandle);
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.FinalizeProcessRunners();
// --------------------------------------------------------------------------------------------------------------
begin
    // WaitForProcessThreads();
    {
      for i:=0 to FProcessList.Count - 1 do begin
      ProcessRunner := FProcessList[i] as TSchedProcessRunner;
      if not ProcessRunner.Terminated
      end;
    }
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.StartProcess(aProcessHandle: TSchedProcessRunnerHandle);
// --------------------------------------------------------------------------------------------------------------
begin
    if not aProcessHandle.Thread.Suspended then
        EXIT;
    aProcessHandle.Thread.Start();
    Self.Log(ltTest, gClock.ElapsedTime(), 'Session.StartProcess: ' + IntToStr(aProcessHandle.ProcessID));
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.StartProcesses();
// --------------------------------------------------------------------------------------------------------------
// Start all Processes
var
    i: integer;
begin
    for i := 0 to FProcessList.Count - 1 do
    begin
        StartProcess(self.ProcessHandles[i]);
    end;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.SetTimer(aBlnValue: boolean);
// --------------------------------------------------------------------------------------------------------------
begin
    if aBlnValue then
    begin
        gClock.Start(0);
    end;
end;

procedure TSchedSessionRunner.FlushData();
begin
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.Execute();
// --------------------------------------------------------------------------------------------------------------
begin
    self.StarTSchedLogManager();
    try // try..finally StarTSchedLogManager/EndLogManager
        try
            self.Log(ltTest, ElapsedTime(), 'Session.Execute.Start');

            self.InitResourceList();
            self.AddProcessRunners();

            self.SetTimer(true);
            try // try..finally SetTimer
                self.StartProcesses();
                try
                    FMessageQueue.Execute();
                finally
                    self.FlushData();
                end;
                self.FinalizeProcessRunners();
            finally
                self.SetTimer(false);
            end;
            self.Log(ltTest, ElapsedTime(), 'Session.Execute.End');
        except
            on E: Exception do
            begin
                self.Log(ltBoth, ElapsedTime(), 'Session.Execute: Exception Occured!' + E.Message);
                raise;
            end;
        end;

    finally
        self.EndLogManager();
    end;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedSessionRunner.DoLog(aLogType: TLogType; aID: cardinal; aTime: TSchedTime; aText: string);
// --------------------------------------------------------------------------------------------------------------
var
    xMessageType: TMsgType;
begin
    case aLogType of
        ltTest:
            xMessageType := INT_MESSAGE_LOG_TEST;
        ltApp:
            xMessageType := INT_MESSAGE_LOG_APP;
        else
            xMessageType := INT_MESSAGE_LOG_APPANDTEST;
    end;
    RegisterLogMessage(xMessageType, aID, aTime, aText);
end;

procedure TSchedSessionRunner.Log(aLogType: TLogType; aTime: TSchedTime; aText: string);
begin
    DoLog(aLogType, 0, aTime, aText);
end;

// ************************************* TSchedProcessRunner ***************************************
// --------------------------------------------------------------------------------------------------------------
constructor TSchedProcessRunner.Create(aProcessID: TProcessIDType;
    aOnSessionRegisterMessage: TOnRegisterMessage; aSessionLock: TSpinLock);
// --------------------------------------------------------------------------------------------------------------
begin
    inherited Create();
    fProcessID := aProcessID;
    fSessionLock := aSessionLock;
    fOnSessionRegisterMessage := aOnSessionRegisterMessage;
    fMessageQueue := TMaskedMessageQueue.Create();

    fTimedMessageQueue := TTimedMessageQueue.Create(gClock);
    CreateTimedMessageQueueThread(fTimedMessageQueue);

    fActionRunnerHandle := CreateActionRunner(RegisterMessage);
    CreateActionRunnerThread(fActionRunnerHandle);
end;

// --------------------------------------------------------------------------------------------------------------
destructor TSchedProcessRunner.Destroy;
// --------------------------------------------------------------------------------------------------------------
begin
    try
        fMessageQueue.Free;
        inherited Destroy;
    except
        on E: Exception do
            raise Exception.CreateFmt('TSchedProcessRunner.Destroy -> %s', [e.Message]);
    end;
end;

// *********Choose Handler
// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
var
    xError: string;
    xTime: TSchedTime;
begin

    try
        Log(ltTest, gClock.ElapsedTime(), Format('Message:%d, Queued:%s ',
            [aMessage.MessageType, fMessageQueue.Log]));
        // OutputDebugString( PChar( Format( 'P%d - Handle Event %d', [ self.fProcessID, aMessage.MessageType ] ) ) );

        case aMessage.MessageType of

            INT_MESSAGE_PROCESS_TERMINATE_ERROR, INT_MESSAGE_PROCESS_TERMINATE_NORMAL:
                begin
                    Quit();
                end;

            INT_MESSAGE_PROCESS_TERMINATE_INTERN:
                begin
                    QuitInternal();
                end;

            INT_MESSAGE_PROCESS_TERMINATE_INTERNWAIT_DONE:
                begin
                    Log(ltApp, gClock.ElapsedTime(), 'Action thread did not respond to terminate signal.');
                    QuitInternal();
                end;

            INT_MESSAGE_PROCESS_ACTIONFINISHED:
                begin
                    fTimedMessageQueue.DequeueMessageByType(INT_MESSAGE_PROCESS_FINISHINGWAIT_DONE);
                    CurrentActionEnded(aMessage.Args[0].AsCard);
                    self.CurrentActionEndExecute();

                    if not fCurrentStep.IsError then
                    begin
                        // self.RegisterStart();
                        FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_START,
                            INT_MESSAGE_PROCESS_TERMINATE_ERROR]);
                    end;
                end;

            INT_MESSAGE_PROCESS_RESCHEDULE_WAIT:
                begin
                    fMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_RESCHEDULED_OTHER,
                        INT_MESSAGE_PROCESS_TERMINATE_ERROR]);
                end;

            INT_MESSAGE_PROCESS_REQUESTRESCHEDULE:
                begin
                    xTime := aMessage.Args[0].AsCard;
                    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_RESCHEDULED]);
                    RegisterLateActionToSession(fCurrentStep, xTime);
                end;

            INT_MESSAGE_PROCESS_RESCHEDULED:
                begin
                    // 29.04.05 pk if another process has just requested a Shift, a INT_MESSAGE_PROCESS_RESCHEDULE_WAIT will
                    // be in the queue.  If we ignore it, and go to the Finishing Wait, the INT_MESSAGE_PROCESS_RESCHEDULED_OTHER
                    // will eventually be registered by the other process when the shift is finished - this will mess things up, so
                    // we will also accept INT_MESSAGE_PROCESS_RESCHEDULE_WAIT here before proceeding
                    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_RESCHEDULE_WAIT,
                        INT_MESSAGE_PROCESS_FINISHINGWAIT]);
                    RegisterSimpleMessage(INT_MESSAGE_PROCESS_FINISHINGWAIT, false);
                end;

            INT_MESSAGE_PROCESS_RESCHEDULED_OTHER:
                begin
                    // 3 possibilities
                    // 1 - TimedMessageQueue has not yet registered the Starting wait done
                    // 2 - StartingWaitDone is registered in fMessageQueue but has not been handled yet
                    // 3 - StartingWaitDone has been handled and Execute has been registered in fMessageQueue

                    if fTimedMessageQueue.DequeueMessageByType(INT_MESSAGE_PROCESS_STARTINGWAIT_DONE) or
                        fMessageQueue.DequeueMessageByType(INT_MESSAGE_PROCESS_STARTINGWAIT_DONE) or
                        fMessageQueue.DequeueMessageByType(INT_MESSAGE_PROCESS_EXECUTE) then
                    begin
                        // ChangeIdle( false, 0 );
                        // SetMask : 06.04.05 pk added Reschedule_Wait due to case : 2012, 2012, 2015, 2015
                        FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND,
                            INT_MESSAGE_PROCESS_RESCHEDULE_WAIT, INT_MESSAGE_PROCESS_RESCHEDULED_OTHER,
                            INT_MESSAGE_PROCESS_STARTINGWAIT]);
                        RegisterSimpleMessage(INT_MESSAGE_PROCESS_STARTINGWAIT, false);
                    end
                    else
                    begin
                        // SetMask : 06.04.05 pk added Reschedule_Wait due to case : 2012, 2012, 2015, 2015
                        // SetMask : 02.05.05 pk added finishing Wait due to case 2013 2012, 2014, 2015
                        FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND,
                            INT_MESSAGE_PROCESS_RESCHEDULE_WAIT, INT_MESSAGE_PROCESS_RESCHEDULED_OTHER,
                            INT_MESSAGE_PROCESS_STARTINGWAIT, INT_MESSAGE_PROCESS_FINISHINGWAIT]);
                    end;
                end;
            INT_MESSAGE_PROCESS_START:
                begin
                    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_TERMINATE_NORMAL,
                        INT_MESSAGE_PROCESS_TERMINATE_ERROR, INT_MESSAGE_PROCESS_SETCURRENTACTION]);
                    RegisterSimpleMessage(INT_MESSAGE_PROCESS_SETCURRENTACTION, false);
                end;
            INT_MESSAGE_PROCESS_SETCURRENTACTION:
                begin
                    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_STARTINGWAIT]);
                    self.SetCurrentAction();
                    RegisterSimpleMessage(INT_MESSAGE_PROCESS_STARTINGWAIT, false);
                end;

            INT_MESSAGE_PROCESS_STARTINGWAIT:
                begin
                    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_TERMINATE_ERROR,
                        INT_MESSAGE_PROCESS_RESCHEDULE_WAIT, INT_MESSAGE_PROCESS_STARTINGWAIT_DONE]);
                    self.CurrentActionStartingWait();
                end;
            INT_MESSAGE_PROCESS_STARTINGWAIT_DONE:
                begin
                    // ChangeIdle( false, 0 );
                    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_RESCHEDULE_WAIT,
                        INT_MESSAGE_PROCESS_EXECUTE]);
                    RegisterSimpleMessage(INT_MESSAGE_PROCESS_EXECUTE, false);
                end;
            INT_MESSAGE_PROCESS_EXECUTE:
                begin
                    self.CurrentActionStartExecute();
                    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_ACTIONSTARTED]);

                end;
            INT_MESSAGE_PROCESS_ACTIONSTARTED:
                begin
                    CurrentActionStarted(aMessage.Args[0].AsCard);
                    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_PREPNEXT,
                        INT_MESSAGE_PROCESS_FINISHINGWAIT]);

                    RegisterSimpleMessage(INT_MESSAGE_PROCESS_FINISHINGWAIT, false);
                    RegisterSimpleMessage(INT_MESSAGE_PROCESS_PREPNEXT, false);
                end;

            INT_MESSAGE_PROCESS_PREPNEXT:
                begin
                    if not self.NextActionPrepare() then
                        RegisterSimpleMessage(INT_MESSAGE_PROCESS_TERMINATE_NORMAL, false);

                end;

            INT_MESSAGE_PROCESS_FINISHINGWAIT:
                begin
                    // Remove all reschedule requests
                    fMessageQueue.DequeueMessageByType(INT_MESSAGE_PROCESS_RESCHEDULED_OTHER);
                    fMessageQueue.DequeueMessageByType(INT_MESSAGE_PROCESS_RESCHEDULE_WAIT);

                    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_ACTIONFINISHED,
                        INT_MESSAGE_PROCESS_TERMINATE_ERROR, INT_MESSAGE_PROCESS_SUSPEND,
                        INT_MESSAGE_PROCESS_FINISHINGWAIT_DONE]);
                    self.CurrentActionEndingWait();
                end;
            INT_MESSAGE_PROCESS_FINISHINGWAIT_DONE:
                begin
                    // Remove all reschedule requests
                    fMessageQueue.DequeueMessageByType(INT_MESSAGE_PROCESS_RESCHEDULED_OTHER);
                    fMessageQueue.DequeueMessageByType(INT_MESSAGE_PROCESS_RESCHEDULE_WAIT);

                    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND,
                        INT_MESSAGE_PROCESS_REQUESTRESCHEDULE]);
                    xTime := aMessage.Args[0].AsCard;
                    self.RegisterMessage(INT_MESSAGE_PROCESS_REQUESTRESCHEDULE, nil, false,
                        TMsgArgs.ArgsArrayOf([TCardStreamableItem.Create(xTime)]), true);

                end;
            INT_MESSAGE_PROCESS_SUSPEND:
                begin
                    fMessageQueue.SetMaskTemp([INT_MESSAGE_PROCESS_RESUME]);
                    SuspendCurrentActionThread();
                end;
            INT_MESSAGE_PROCESS_RESUME:
                begin
                    fMessageQueue.RestoreMask();
                    ResumeCurrentActionThread();
                end;

        end; // end case
    except
        on E: Exception do
        begin
            xError := Format('P%d: Process MessageProc -> Message:%d -> %s',
                [fProcessID, aMessage.MessageType, E.Message]);
            Log(ltBoth, gClock.ElapsedTime(), xError);
            fMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_TERMINATE_ERROR]);
            self.RegisterSessionMessage(INT_MESSAGE_SESSION_FATALERROR, nil, false,
                TMsgArgs.ArgsArrayOf([TStrStreamableItem.Create(xError)]));
        end;
    end;
end;

// *********Register
// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.RegisterMessage(aMessageType: TMsgType; aLock: TLock; aSeparateThread: boolean;
    aArgs: TMsgArgs; aFreeArgs: boolean);
// --------------------------------------------------------------------------------------------------------------
var
    xMessageItem: TMessageQueueItem;
begin
    if (aMessageType = INT_MESSAGE_SESSION_FATALERROR) then
    begin
        RegisterSessionMessage(aMessageType, aLock, aSeparateThread, aArgs);
    end
    else if (aMessageType = INT_MESSAGE_PROCESS_LOG) then
    begin
        Log(TLogType(aArgs[0].AsInt), aArgs[1].AsCard, aArgs[2].AsStr);
    end
    else
    begin
        xMessageItem := TMessageQueueItem.Create(aMessageType, 0, aLock, aSeparateThread, self.MessageProc,
            aArgs, aFreeArgs);
        FMessageQueue.EnqueueMessage(xMessageItem);
    end;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.RegisterSimpleMessage(aMessageType: TMsgType; aSeparateThread: boolean);
// --------------------------------------------------------------------------------------------------------------
begin
    RegisterMessage(aMessageType, nil, aSeparateThread, TMsgArgs.EmptyArgs(), true);
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.RegisterSessionMessage(aMessageType: TMsgType; aLock: TLock;
    aSeparateThread: boolean; aArgs: TMsgArgs);
// --------------------------------------------------------------------------------------------------------------
begin
    fOnSessionRegisterMessage(aMessageType, aLock, false, aArgs, true);
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.TimedMessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
begin
    RegisterMessage(aMessage.MessageType, nil, false,
        TMsgArgs.ArgsArrayOf([TCardStreamableItem.Create(aMessage.Args[0].AsCard)]), true);
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.RegisterTimedMessage(aMessageType: TMsgType; aTime: TSchedTime;
    aActionStepTime: TSchedTime);
// --------------------------------------------------------------------------------------------------------------
var
    xMessageItem: TTimedMessageQueueItem;
begin
    xMessageItem := TTimedMessageQueueItem.Create(aMessageType, aTime, nil, false, self.TimedMessageProc,
        aTime, TMsgArgs.ArgsArrayOf([TCardStreamableItem.Create(aActionStepTime)]), true);
    FTimedMessageQueue.EnqueueMessage(xMessageItem);

end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.RegisterLateActionToSession(const aSchedActionStep: TSchedActionStep;
    aActionStepTime: TSchedTime);
// --------------------------------------------------------------------------------------------------------------
begin
    self.Log(ltTest, gClock.ElapsedTime(), 'Registering Late Action');
    RegisterSessionMessage(INT_MESSAGE_SESSION_ACTIONLATE, nil, false,
        TMsgArgs.ArgsArrayOf([TCardStreamableItem.Create(fProcessID),
        TObjStreamableItem.Create(aSchedActionStep), TCardStreamableItem.Create(aActionStepTime)]));
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.RegisterTerminateProcToSession;
// --------------------------------------------------------------------------------------------------------------
begin
    RegisterSessionMessage(INT_MESSAGE_SESSION_TERMPROC, nil, false,
        TMsgArgs.ArgsArrayOf([TCardStreamableItem.Create(fProcessID)]));
end;

procedure TSchedProcessRunner.RegisterStart();
begin
    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_SUSPEND, INT_MESSAGE_PROCESS_START]);
    RegisterSimpleMessage(INT_MESSAGE_PROCESS_START, false);
end;

procedure TSchedProcessRunner.RegisterActionMessage(aMessageType: TMsgType);
begin
    fActionRunnerHandle.OnRegisterMessage(aMessageType, nil, false,
        TMsgArgs.ArgsArrayOf([TObjStreamableItem.Create(fCurrentStep)]), true);
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.CreateActionThread(const aSchedActionStep: TSchedActionStep);
// --------------------------------------------------------------------------------------------------------------
begin
end;

procedure TSchedProcessRunner.CreateTimedMessageQueueThread(aTimedMessageQueue: TTimedMessageQueue);
begin
end;

procedure TSchedProcessRunner.SuspendCurrentActionThread();
begin
end;

procedure TSchedProcessRunner.ResumeCurrentActionThread();
begin
end;

function TSchedProcessRunner.CreateActionRunner(aOnRegisterMessage: TOnRegisterMessage)
    : TSchedActionRunnerHandle;
begin
    result := nil;
end;

procedure TSchedProcessRunner.CreateActionRunnerThread(aActionRunnerHandle: TSchedActionRunnerHandle);
begin
end;

procedure TSchedProcessRunner.CurrentActionStarted(aTime: TSchedTime);
begin
    RegisterSessionMessage(INT_MESSAGE_SESSION_ACTIONSTARTED, nil, false,
        TMsgArgs.ArgsArrayOf([TCardStreamableItem.Create(fProcessID), TCardStreamableItem.Create(aTime),
        TObjStreamableItem.Create(fCurrentStep)]));

end;

procedure TSchedProcessRunner.CurrentActionEnded(aTime: TSchedTime);
begin
    RegisterSessionMessage(INT_MESSAGE_SESSION_ACTIONFINISHED, nil, false,
        TMsgArgs.ArgsArrayOf([TCardStreamableItem.Create(fProcessID), TCardStreamableItem.Create(aTime),
        TObjStreamableItem.Create(fCurrentStep)]));
end;

{
  procedure TSchedProcessRunner.ChangeIdle( aIsIdle : boolean; aStartTime : TSchedTime );
  begin
  if aIsIdle then
  RegisterSessionMessage( INT_MESSAGE_SESSION_IDLE_BEGIN, nil, false, TMsgArgs.ArgsArrayOf( [ TCardStreamableItem.Create( fProcessID ), TCardStreamableItem.Create( aStartTime ) ] ) )
  else
  RegisterSessionMessage( INT_MESSAGE_SESSION_IDLE_END, nil, false, TMsgArgs.ArgsArrayOf( [ TCardStreamableItem.Create( fProcessID ) ] ) )


  end;
}
// --------------------------------------------------------------------------------------------------------------
function TSchedProcessRunner.PrepareNextSchedAction(): TNextActionResultRec;
// --------------------------------------------------------------------------------------------------------------
begin
    result.Mode := namOK;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.ActionThreadEnded(Sender: TObject);
// --------------------------------------------------------------------------------------------------------------
begin
    RegisterSimpleMessage(INT_MESSAGE_PROCESS_ACTIONFINISHED, false);
end;

// --------------------------------------------------------------------------------------------------------------
function TSchedProcessRunner.NextActionPrepare(): boolean;
// --------------------------------------------------------------------------------------------------------------
// Find the next runnable action. If no actions exist then return false
// If an action is found then create an action handle
// If an error occurs while creating the action save the error message.
begin
    result := false;
    try
        fPreparedAction.Action := nil;
        fPreparedAction.Mode := namOK;
        fPreparedAction.Msg := '';

        fPreparedAction := self.PrepareNextSchedAction();
        result := fPreparedAction.Mode <> namEOF;
    except
        on E: Exception do
        begin
            fPreparedAction.Action := nil;
            fPreparedAction.Mode := namError;
            fPreparedAction.Msg := E.Message;
            EXIT;
        end;
    end;

end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.FirstActionPrepare();
// --------------------------------------------------------------------------------------------------------------
begin
    if not self.NextActionPrepare() then
    begin
        RegisterSimpleMessage(INT_MESSAGE_PROCESS_TERMINATE_NORMAL, false);
    end;

    FMessageQueue.SetMask([INT_MESSAGE_PROCESS_START]);
    RegisterSimpleMessage(INT_MESSAGE_PROCESS_START, false);
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.SetCurrentAction();
// --------------------------------------------------------------------------------------------------------------
begin
    if fPreparedAction.Mode = namError then
    begin
        raise Exception.Create('Action could not be created -> ' + fPreparedAction.Msg);
    end;
    fCurrentStep := fPreparedAction.Action;
    fPreparedAction.Action := nil;
end;

// --------------------------------------------------------------------------------------------------------------
function TSchedProcessRunner.CurrentActionStartingWait(): integer;
// --------------------------------------------------------------------------------------------------------------
// const
// INT_START_BUFFER_MSECS = 250;
var
    xTimeSoFar: TSchedTime;
    xTimeStart: TSchedTime;
begin
    try
        result := INT_STARTING_WAIT_TIMEOUT;

        // ASSERT( fCurrentStep.StartTime >= 0, 'StartTime < 0' );
        xTimeStart := fCurrentStep.StartTime;
        // +  gClock.ConvertToClockTimeUnit( INT_START_BUFFER_MSECS, tuThousandth );

        xTimeSoFar := gClock.ElapsedTime();
        // 1. Starting Wait
        if xTimeStart <= xTimeSoFar then
        begin
            raise Exception.CreateFmt('NO START WAIT for %s - Start time [%d]',
                [fCurrentStep.Description, xTimeStart]);
        end;

        self.Log(ltTest, xTimeSoFar, Format('Waiting to Start @%d... => %s',
            [xTimeStart, fCurrentStep.Description]));
        self.Log(ltApp, xTimeSoFar, 'Waiting...');

        // ChangeIdle( true, xTimeStart );
        RegisterTimedMessage(INT_MESSAGE_PROCESS_STARTINGWAIT_DONE, xTimeStart, xTimeStart);
    except
        on E: Exception do
        begin
            self.Log(ltBoth, gClock.ElapsedTime(), 'Process.StartingWait: Exception Occured!' + E.Message);
            raise;
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.CurrentActionStartExecute();
// --------------------------------------------------------------------------------------------------------------
// Resume the FCurrentThread
// Add the Thread to the Process's ActionList so that at the end the process could make sure
// that all the actions have Terminated, before finishing
begin
    self.RegisterActionMessage(INT_MESSAGE_ACTION_EXECUTE);
end;

// --------------------------------------------------------------------------------------------------------------
function TSchedProcessRunner.CurrentActionEndingWait(): integer;
// --------------------------------------------------------------------------------------------------------------
const
    INT_END_BUFFER_MSECS = 250;
var
    xTimeSoFar: TSchedTime;
    xTimeEnd: TSchedTime;
begin
    try
        result := INT_ENDING_WAIT_OTHER;
        // ASSERT(  fCurrentStep.EndTime >= 0, 'EndTime < 0' );
        xTimeEnd := fCurrentStep.EndTime - gClock.ConvertToClockTimeUnit(INT_END_BUFFER_MSECS, tuThousandth);
        xTimeSoFar := gClock.ElapsedTime();

        if xTimeEnd <= xTimeSoFar then
        begin // if no End Wait
            self.Log(ltBoth, xTimeSoFar, Format('NO END WAIT @%d  => %s',
                [xTimeEnd, fCurrentStep.Description]));
            raise Exception.CreateFmt('No end wait for action %s', [fCurrentStep.Description]);
        end;
        // if xTimeEnd < xTimeSoFar then xTimeEnd := xTimeSoFar + 5;
        // Wait
        self.Log(ltBoth, xTimeSoFar, Format('Waiting to End @%d... => %s',
            [xTimeEnd, fCurrentStep.Description]));
        RegisterTimedMessage(INT_MESSAGE_PROCESS_FINISHINGWAIT_DONE, xTimeEnd, fCurrentStep.EndTime);
    except
        on E: Exception do
        begin
            self.Log(ltBoth, gClock.ElapsedTime(), 'Process.EndingWait: Exception Occured!' + E.Message);
            raise;
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.CurrentActionEndExecute();
// --------------------------------------------------------------------------------------------------------------
begin

end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.QuitInternal();
// --------------------------------------------------------------------------------------------------------------
begin
    self.Log(ltApp, gClock.ElapsedTime(), 'Ended => Process');
    self.RegisterTimedMessage(INT_MESSAGE_QUIT, 0, 0);

    FMessageQueue.SetMask([INT_MESSAGE_QUIT]);
    RegisterSimpleMessage(INT_MESSAGE_QUIT, false);
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.Quit();
// --------------------------------------------------------------------------------------------------------------
const
    INT_WAITFORACTION_MSECS = 10000;
var
    xWaitForActionEndTime: TSchedTime;
begin
    fMessageQueue.SetMask([INT_MESSAGE_PROCESS_TERMINATE_INTERN,
        INT_MESSAGE_PROCESS_TERMINATE_INTERNWAIT_DONE]);
    self.RegisterActionMessage(INT_MESSAGE_ACTION_TERMINATE);
    xWaitForActionEndTime := gClock.ElapsedTime + gClock.ConvertToClockTimeUnit(INT_WAITFORACTION_MSECS,
        tuThousandth);
    RegisterTimedMessage(INT_MESSAGE_PROCESS_TERMINATE_INTERNWAIT_DONE, xWaitForActionEndTime, 0);

end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.Log(aLogType: TLogType; aTime: TSchedTime; const aText: string);
// --------------------------------------------------------------------------------------------------------------
begin
    RegisterSessionMessage(INT_MESSAGE_SESSION_LOG, nil, false,
        TMsgArgs.ArgsArrayOf([TIntStreamableItem.Create(TMsgType(aLogType)),
        TCardStreamableItem.Create(fProcessID), TCardStreamableItem.Create(aTime),
        TStrStreamableItem.Create(aText)]));
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedProcessRunner.Execute();
// --------------------------------------------------------------------------------------------------------------
begin
    try
        self.Log(ltTest, gClock.ElapsedTime(), 'Process.Execute.Start');
        // if not self.Terminated then begin
        self.FirstActionPrepare();
        FMessageQueue.Execute();

        // end;
        self.Log(ltTest, gClock.ElapsedTime(), 'Process.Execute.End: ');
    finally
        try
            self.RegisterTerminateProcToSession();
        except
        end;
    end;

end;

{ TSchedActionRunner }

constructor TSchedActionRunner.Create(aProcessID: TProcessIDType;
    aOnProcessRegisterMessage: TOnRegisterMessage);
begin
    inherited Create();
    fOnProcessRegisterMessage := aOnProcessRegisterMessage;
    fMessageQueue := TMaskedMessageQueue.Create();
    fMessageQueue.SetMask([INT_MESSAGE_ACTION_ENDED, INT_MESSAGE_ACTION_TERMINATE,
        INT_MESSAGE_ACTION_EXECUTE]);
    fProcessID := aProcessID;
end;

procedure TSchedActionRunner.Execute();
begin
    FMessageQueue.Execute();
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedActionRunner.RegisterMessage(aMessageType: TMsgType; aLock: TLock; aSeparateThread: boolean;
    aArgs: TMsgArgs; aFreeArgs: boolean);
// --------------------------------------------------------------------------------------------------------------
var
    xMessageItem: TMessageQueueItem;
begin
    xMessageItem := TMessageQueueItem.Create(aMessageType, 0, aLock, false, self.MessageProc, aArgs,
        aFreeArgs);
    FMessageQueue.EnqueueMessage(xMessageItem);

end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedActionRunner.RegisterSimpleMessage(aMessageType: TMsgType);
// --------------------------------------------------------------------------------------------------------------
begin
    RegisterMessage(aMessageType, nil, false, TMsgArgs.EmptyArgs(), true);
end;

procedure TSchedActionRunner.RegisterProcessMessage(aMessageType: TMsgType; aArgs: TMsgArgs);
begin
    fOnProcessRegisterMessage(aMessageType, nil, false, aArgs, true);
end;

procedure TSchedActionRunner.StepStarted();
var
    xElapsedTime: TSchedTime;
begin
    xElapsedTime := gClock.ElapsedTime();
    fStep.Start(xElapsedTime);
    RegisterProcessMessage(INT_MESSAGE_PROCESS_ACTIONSTARTED,
        TMsgArgs.ArgsArrayOf([TCardStreamableItem.Create(xElapsedTime)]));
    self.Log(ltBoth, xElapsedTime, Format('Started => %s ', [fStep.Description]));
    Beep();
end;

procedure TSchedActionRunner.StepFinished();
var
    xElapsedTime: TSchedTime;
begin
    xElapsedTime := gClock.ElapsedTime();
    fStep.Finish(xElapsedTime);
    RegisterProcessMessage(INT_MESSAGE_PROCESS_ACTIONFINISHED,
        TMsgArgs.ArgsArrayOf([TCardStreamableItem.Create(xElapsedTime)]));
    self.Log(ltBoth, xElapsedTime, format('Ended => %s', [fStep.Description]));
    Beep();
end;

procedure TSchedActionRunner.ExecuteStep();
const
    INT_MIN_EXEC_TIME_MSECS = 3000;
var
    xStartTime, xExecTime: TSchedTime;
    xMinExecTime: TSchedTime;
begin
    RegisterMessage(INT_MESSAGE_ACTION_ENDED, nil, false, nil, false);
    xStartTime := gClock.ElapsedTime();
    fStep.Execute();
    xExecTime := gClock.ElapsedTime() - xStartTime;
    xMinExecTime := gClock.ConvertToClockTimeUnit(INT_MIN_EXEC_TIME_MSECS, tuThousandth);
    if xExecTime < xMinExecTime then
        Sleep(gClock.ConvertFromClockTimeUnit(xMinExecTime - xExecTime, tuThousandth));
end;

procedure TSchedActionRunner.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
var
    xError: string;
begin

    try
        Log(ltTest, gClock.ElapsedTime(), Format('Message:%d, Queued:%s ',
            [aMessage.MessageType, fMessageQueue.Log]));
        // OutputDebugString( PChar( Format( 'P%d - Handle Event %d', [ self.fProcessID, aMessage.MessageType ] ) ) );

        case aMessage.MessageType of

            INT_MESSAGE_ACTION_TERMINATE:
                begin
                    self.Quit();
                end;
            INT_MESSAGE_ACTION_EXECUTE:
                begin
                    fStep := aMessage.Args[0].AsObj as TSchedActionStep;
                    StepStarted();
                    ExecuteStep();
                end;
            INT_MESSAGE_ACTION_ENDED:
                begin
                    StepFinished();
                end;
        end; // end case
    except
        on E: Exception do
        begin
            xError := Format('P%d: Action MessageProc -> Message:%d -> %s',
                [fProcessID, aMessage.MessageType, E.Message]);
            Log(ltBoth, gClock.ElapsedTime(), xError);
            fMessageQueue.SetMask([INT_MESSAGE_ACTION_TERMINATE]);
            self.RegisterProcessMessage(INT_MESSAGE_SESSION_FATALERROR,
                TMsgArgs.ArgsArrayOf([TStrStreamableItem.Create(xError)]));
        end;
    end;
end;

procedure TSchedActionRunner.Quit();
begin
    RegisterProcessMessage(INT_MESSAGE_PROCESS_TERMINATE_INTERN, TMsgArgs.EmptyArgs());
    self.QuitInternal();
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedActionRunner.QuitInternal();
// --------------------------------------------------------------------------------------------------------------
begin
    fMessageQueue.SetMask([INT_MESSAGE_QUIT]);
    RegisterSimpleMessage(INT_MESSAGE_QUIT);
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedActionRunner.Log(aLogType: TLogType; aTime: TSchedTime; const aText: string);
// --------------------------------------------------------------------------------------------------------------
begin
    RegisterProcessMessage(INT_MESSAGE_PROCESS_LOG,
        TMsgArgs.ArgsArrayOf([TIntStreamableItem.Create(TMsgType(aLogType)),
        TCardStreamableItem.Create(aTime), TStrStreamableItem.Create('A:' + aText)]));
end;

{ T LogManager }

// --------------------------------------------------------------------------------------------------------------
constructor TSchedLogManager.Create();
// --------------------------------------------------------------------------------------------------------------
begin
    inherited Create();
    fMessageQueue := TMessageQueue.Create();

    // FreeOnTerminate:=true;
    // if not aBlnCreateSuspended then
    // Resume();
end;

destructor TSchedLogManager.Destroy();
begin
    fMessageQueue.Free;
    inherited;
end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedLogManager.RegisterMessage(aMsgType: TMsgType; aID: cardinal; aTime: TSchedTime;
    aText: string);
// --------------------------------------------------------------------------------------------------------------
var
    xMessageItem: TMessageQueueItem;
begin
    xMessageItem := TMessageQueueItem.Create(aMsgType, integer(aTime), nil, false, self.MessageProc,
        TMsgArgs.ArgsArrayOf([TCardStreamableItem.Create(aID), TCardStreamableItem.Create(aTime),
        TStrStreamableItem.Create(aText)]), true);
    fMessageQueue.EnqueueMessage(xMessageItem);

end;

// --------------------------------------------------------------------------------------------------------------
procedure TSchedLogManager.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
// --------------------------------------------------------------------------------------------------------------
begin
end;

procedure TSchedLogManager.Close(aTime: TSchedTime);
begin
    RegisterMessage(INT_MESSAGE_QUIT, 0, aTime, '');
end;

{ TSchedActionStep }

procedure TSchedActionStep.Execute;
begin
end;

procedure TSchedActionStep.Finish(aTime: TSchedTime);
begin
end;

function TSchedActionStep.GetDescription: string;
begin
    result := '';
end;

function TSchedActionStep.GetEndTime: TSchedTime;
begin
    result := 0;
end;

function TSchedActionStep.GetProcessID: TProcessIDType;
begin
    result := 0;
end;

function TSchedActionStep.GetStartTime: TSchedTime;
begin
    result := 0;
end;

function TSchedActionStep.GetStepID: integer;
begin
    result := 0;
end;

function TSchedActionStep.IsError: boolean;
begin
    result := false;
end;

procedure TSchedActionStep.SetEndTime(aTime: TSchedTime);
begin
end;

procedure TSchedActionStep.Start(aTime: TSchedTime);
begin
end;


end.
