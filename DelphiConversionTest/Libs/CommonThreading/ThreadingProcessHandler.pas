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

unit ThreadingProcessHandler;


interface


uses
    ThreadClasses,
    MessagableExecutable,
    MessageQueue,
    LockHandle,
    MessageInfo,
    ProcessRegistry,
    Executable;

type
    TProcessHandler = class(TMessagableExecutable, IProcessHandler, IMessagable)
    private
        function GetOriginalMaskDef(): TArray<integer>;
    protected
        fSecondaryThreadCount: integer;
        function GetAdditionalMask(): TArray<integer>; virtual;
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); override;
        procedure SetOriginalMask(); override;
        procedure DoHandleTerminate(aMessageInfo: TTerminateMessageInfo); override;
        procedure HandlePaused(aPauseMessageInfo: TPauseMessageInfo); override;
        procedure DoSecondaryThreadAdded(const aTID: cardinal); virtual;
        procedure DoSecondaryThreadRemoved(const aTID: cardinal); virtual;
        procedure HandleUnPaused(aUnPauseMessageInfo: TUnPauseMessageInfo); override;
        procedure HandleMessageSecondaryThreadAdded(const aMessage
            : TSecondaryThreadAddedMessageInfo); virtual;
        procedure HandleMessageSecondaryThreadRemoved(const aMessage
            : TSecondaryThreadRemovedMessageInfo); virtual;
    end;

    TThreadingProcessHandler = class(TProcessHandler)
    private
        class var uInstanceIntf: IProcessHandler;
        class var uInstance: TThreadingProcessHandler;
        function InternGetSysObjectData(const aSysHandleID: TSysHandleID): TObject;
        function InternGetLock(const aSysHandleID: TSysHandleID): TLock;
        function AtleastOneNonSimulatedProcessExists(): boolean;
        function GetCallingThreadSysHandleID(const aMessage: TMessageInfo): TSysHandleID;
    protected
        procedure SetOriginalMask(); override;
        procedure SetQuitMask(); override;
        procedure DoHandleTerminate(aMessageInfo: TTerminateMessageInfo); override;
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); override;
    public
        procedure HandleMessageSetError(aMessage: TSetErrorMessageInfo);
        procedure HandleMessageCreateThread(aMessage: TCreateThreadMessageInfo);
        procedure HandleMessageThreadFinished(aMessage: TThreadFinishedMessageInfo);
        procedure HandleMessageStartThread(aMessage: TStartThreadMessageInfo);
        procedure HandleMessageOpenThread(aMessage: TOpenThreadMessageInfo);

        procedure HandleMessageAddProcess(aMessage: TAddProcessMessageInfo);
        procedure HandleMessageExitProcess(aMessage: TExitProcessMessageInfo);
        procedure HandleMessageOpenProcess(aMessage: TOpenProcessMessageInfo);
        procedure HandleMessageSuspendAllProcesses(aMessage: TSuspendAllProcessesMessageInfo);
        procedure HandleMessageResumeAllProcesses(aMessage: TResumeAllProcessesMessageInfo);
        procedure HandleMessageWaitTillAllProcessesSuspended
            (aMessage: TWaitTillAllProcessesSuspendedMessageInfo);
        procedure HandleMessageCheckAllProcessesExited(aMessage: TCheckAllProcessesExitedMessageInfo);
        procedure HandleMessageAtleastOneNonSimulatedProcessExists
            (aMessage: TAtleastOneNonSimulatedProcessExistsMessageInfo);
        procedure HandleMessageTerminateSecondaryThreads(aMessage: TTerminateSecondaryThreadsMessageInfo);
        procedure HandleMessageWaitTillSecondaryThreadsTerminate
            (aMessage: TWaitTillSecondaryThreadsTerminateMessageInfo);
        procedure HandleMessageCreateLock(aMessage: TCreateLockMessageInfo);
        procedure HandleMessageDestroyLock(aMessage: TDestroyLockMessageInfo);
        procedure HandleMessageGetSysObjectData(aMessage: TGetSysObjectDataMessageInfo);
        procedure HandleMessageDuplicateSysHandle(aMessage: TDuplicateSysHandleMessageInfo);
        procedure HandleMessageCloseSysHandle(aMessage: TCloseSysHandleMessageInfo);

        // thread
        function CreateThread(const aDescription: string; const aCreateSuspended, aCloseHandleOnFinish,
            aCanSuspendSafe: boolean; const aExecutable: IExecutable): TSysHandleID;
        procedure ThreadFinished(const aTID: cardinal);
        function JoinThread(const aSysHandleID: TSysHandleID; const aMaxWaitTime: cardinal): boolean;
        procedure StartThread(const aSysHandleID: TSysHandleID);
        function OpenThread(const aTID: cardinal): TSysHandleID;

        // process
        function CreateProcess(const aDescription: string; const aProcessHandler: IProcessHandler;
            const aAddressSpace: TAddressSpace; const aSourceDataName: string;
            const aIsSystemProcess: boolean; const aIsSimulated: boolean): TSysHandleID;
        procedure ExitProcess();
        function OpenProcess(const aPID: cardinal): TSysHandleID;
        procedure SuspendAllProcesses();
        procedure ResumeAllProcesses();
        function WaitTillAllProcessesSuspended(const aMaxWaitTime: cardinal): boolean;
        function WaitTillAllProcessesExited(const aCheckSystemProcesses: boolean;
            const aMaxWaitTime: cardinal): boolean;
        procedure TerminateSecondaryThreads();
        procedure WaitTillSecondaryThreadsTerminate(const aMaxWaitTries: integer);
        function GetIsCurrentThreadSimulated(): boolean;

        // Lock
        function CreateLock(): TSysHandleID;
        procedure DestroyLock(const aSysHandleID: TSysHandleID);
        function WaitForLock(const aSysHandleID: TSysHandleID; const aMaxWaitTime: cardinal): boolean;
        procedure OpenLock(const aSysHandleID: TSysHandleID);

        // these class functions MUST be threadsafe, because they dont post messages
        class function GetCurrentThreadImage(): TThreadImage;
        class function GetCurrentProcess(): TProcess;
        class function GetCurrentSourceDataName(): string;
        class function GetCurrentProcessDescription(): string;
        class function GetCurrentThreadDescription(): string;
        class function GetCurrentThreadID(): cardinal;
        class function GetThreadImage(const aSysHandleID: TSysHandleID): TThreadImage;
        class procedure ChangeCurrentProcessSimulated(const aIsSimulated: boolean);

        function DuplicateSysHandle(const aOriginalSysHandleID: TSysHandleID;
            out oNewHandleID: TSyshandleID): boolean;
        procedure CloseSysHandle(const aSysHandleID: TSysHandleID);

        procedure SetError();
    public
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TThreadingProcessHandler;
    end;


implementation


uses
    SysUtils,
    SyncObjs,
    ThreadAPI,
    ThreadingManager;

{ TProcessHandler }
function TProcessHandler.GetOriginalMaskDef(): TArray<integer>;
begin
    result := fMessageQueue.MaskArgsToMaskArray([INT_MESSAGE_EXECUTABLE_TERMINATE,
        INT_MESSAGE_EXECUTABLE_PAUSE, INT_MESSAGE_EXECUTABLE_PERFORM,
        INT_MESSAGE_PROCESSHANDLER_SECONDARYTHREADADDED, INT_MESSAGE_PROCESSHANDLER_SECONDARYTHREADREMOVED]);
end;

function TProcessHandler.GetAdditionalMask(): TArray<integer>;
begin
    result := fMessageQueue.MaskArgsToMaskArray([]);
end;

procedure TProcessHandler.SetOriginalMask();
var
    xOriginalMask: TArray<integer>;
begin
    xOriginalMask := fMessageQueue.ConcatMaskArrays(GetOriginalMaskDef(), GetAdditionalMask());
    fMessageQueue.SetMask(xOriginalMask);
end;

procedure TProcessHandler.DoSecondaryThreadAdded(const aTID: cardinal);
begin
    Inc(fSecondaryThreadCount);
end;

procedure TProcessHandler.DoSecondaryThreadRemoved(const aTID: cardinal);
begin
    Dec(fSecondaryThreadCount);
    if fSecondaryThreadCount = 0 then
    begin
        TThreadAPI.ExitProcess();
    end;
end;

procedure TProcessHandler.HandleMessageSecondaryThreadAdded(const aMessage: TSecondaryThreadAddedMessageInfo);
begin
    DoSecondaryThreadAdded(aMessage.TID);
end;

procedure TProcessHandler.HandleMessageSecondaryThreadRemoved
    (const aMessage: TSecondaryThreadRemovedMessageInfo);
begin
    DoSecondaryThreadRemoved(aMessage.TID);
end;

procedure TProcessHandler.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
    gmDebugLog(fMessageQueue.Log);
    inherited;
    if aMessage.MessageInfo is TSecondaryThreadAddedMessageInfo then
    begin
        HandleMessageSecondaryThreadAdded(aMessage.MessageInfo as TSecondaryThreadAddedMessageInfo);
    end
    else if aMessage.MessageInfo is TSecondaryThreadRemovedMessageInfo then
    begin
        HandleMessageSecondaryThreadRemoved(aMessage.MessageInfo as TSecondaryThreadRemovedMessageInfo);
    end
end;

procedure TProcessHandler.HandleUnPaused(aUnPauseMessageInfo: TUnPauseMessageInfo);
begin
    fMessageQueue.RestoreMask();
    fPaused := false;
end;

procedure TProcessHandler.HandlePaused(aPauseMessageInfo: TPauseMessageInfo);
begin
    SetPauseMask();
    fPaused := true;
end;

procedure TProcessHandler.DoHandleTerminate(aMessageInfo: TTerminateMessageInfo);
begin
    TThreadAPI.TerminateSecondaryThreads();
    TThreadAPI.WaitTillSecondaryThreadsTerminate(5);
end;

{ TThreadingProcessHandler }
class procedure TThreadingProcessHandler.CreateInstance;
begin
    uInstance := TThreadingProcessHandler.Create;
    uInstanceIntf := uInstance;
end;

class procedure TThreadingProcessHandler.DestroyInstance;
begin
    uInstance := nil;
    uInstanceIntf := nil;
    // we assume that TThreadingProcessHandler is reference counted setting to nil will destroy it
end;

class function TThreadingProcessHandler.Instance: TThreadingProcessHandler;
begin
    result := uInstance;
end;

procedure TThreadingProcessHandler.SetOriginalMask();
begin
    fMessageQueue.SetMask([INT_MESSAGE_EXECUTABLE_TERMINATE, INT_MESSAGE_THREADAPI_ERROR_SET,
        INT_MESSAGE_THREADAPI_PROCESS_TERMINATETHREADS, INT_MESSAGE_EXECUTABLE_PAUSE,
        INT_MESSAGE_EXECUTABLE_PERFORM, INT_MESSAGE_THREADAPI_PROCESSES_WAITSUSPENDEDALL,
        INT_MESSAGE_THREADAPI_PROCESSES_SUSPENDALL, INT_MESSAGE_THREADAPI_PROCESSES_RESUMEALL,
        INT_MESSAGE_THREADAPI_PROCESSES_CHECKALLEXITED, INT_MESSAGE_THREADAPI_PROCESSES_NONSIMPROCEXISTS,
        INT_MESSAGE_THREADAPI_PROCESS_OPEN, INT_MESSAGE_THREADAPI_PROCESS_EXIT,
        INT_MESSAGE_THREADAPI_PROCESS_ADD, INT_MESSAGE_THREADAPI_THREAD_FINISHED,
        INT_MESSAGE_THREADAPI_THREAD_CREATE, INT_MESSAGE_THREADAPI_THREAD_START,
        INT_MESSAGE_THREADAPI_THREAD_OPEN, INT_MESSAGE_THREADAPI_LOCK_CREATE,
        INT_MESSAGE_THREADAPI_LOCK_DESTROY, INT_MESSAGE_THREADAPI_LOCK_GET,
        INT_MESSAGE_THREADAPI_PROCESS_WAITTILLTHREADSTERMINATE,
        INT_MESSAGE_PROCESSHANDLER_SECONDARYTHREADADDED, INT_MESSAGE_PROCESSHANDLER_SECONDARYTHREADREMOVED,
        INT_MESSAGE_THREADAPI_THREAD_GETIMAGE, INT_MESSAGE_THREADAPI_PROCESS_GETCURRENT,
        INT_MESSAGE_THREADAPI_THREAD_GETIMAGEBYDESCR, INT_MESSAGE_THREADAPI_SYS_DUPLICATEHANDLE,
        INT_MESSAGE_THREADAPI_SYS_CLOSEHANDLE]);
end;

procedure TThreadingProcessHandler.SetQuitMask();
begin
    fMessageQueue.SetMask([INT_MESSAGE_EXECUTABLE_TERMINATE, INT_MESSAGE_THREADAPI_ERROR_SET,
        INT_MESSAGE_THREADAPI_PROCESS_TERMINATETHREADS, INT_MESSAGE_THREADAPI_PROCESSES_WAITSUSPENDEDALL,
        INT_MESSAGE_THREADAPI_PROCESSES_SUSPENDALL, INT_MESSAGE_THREADAPI_PROCESSES_RESUMEALL,
        INT_MESSAGE_THREADAPI_PROCESSES_CHECKALLEXITED, INT_MESSAGE_THREADAPI_PROCESSES_NONSIMPROCEXISTS,
        INT_MESSAGE_THREADAPI_PROCESS_OPEN, INT_MESSAGE_THREADAPI_PROCESS_EXIT,
        INT_MESSAGE_THREADAPI_THREAD_FINISHED, INT_MESSAGE_THREADAPI_THREAD_OPEN,
        INT_MESSAGE_THREADAPI_LOCK_DESTROY, INT_MESSAGE_THREADAPI_LOCK_GET,
        INT_MESSAGE_THREADAPI_PROCESS_WAITTILLTHREADSTERMINATE, INT_MESSAGE_THREADAPI_THREAD_GETIMAGE,
        INT_MESSAGE_THREADAPI_PROCESS_GETCURRENT, INT_MESSAGE_THREADAPI_THREAD_GETIMAGEBYDESCR,
        INT_MESSAGE_THREADAPI_SYS_DUPLICATEHANDLE, INT_MESSAGE_THREADAPI_SYS_CLOSEHANDLE, INT_MESSAGE_QUIT]);
end;

function TThreadingProcessHandler.GetCallingThreadSysHandleID(const aMessage: TMessageInfo): TSysHandleID;
var
    xThreadImage: TThreadImage;
    xProcess: TProcess;
begin
    ASSERT(aMessage.CallingThreadSysHandleID <> cHandleIDNone);
    if aMessage.CallingThreadSysHandleID = cCurrentThreadPsuedoSysHandleID then
    begin
        // dont open a new handle, just use the processes handle to the thread
        xThreadImage := TThreadingManager.Instance.FindThreadImageByThreadID(aMessage.CallingThreadID);
        xProcess := TThreadingManager.Instance.FindProcessByPID(xThreadImage.TCB.PID);
        result := xProcess.ThreadSysHandleIDs.FindByTID(aMessage.CallingThreadID).SysHandleID;
    end
    else
    begin
        result := aMessage.CallingThreadSysHandleID;
    end;

end;

procedure TThreadingProcessHandler.HandleMessageCreateThread(aMessage: TCreateThreadMessageInfo);
var
    xSysHandleID: TSysHandleID;
begin
    xSysHandleID := GetCallingThreadSysHandleID(aMessage);
    aMessage.ResultSysHandleID := TThreadingManager.Instance.InternalCreateThread(xSysHandleID,
        aMessage.Description, aMessage.CreateSuspended, aMessage.CloseHandleOnFinish, aMessage.CanSuspendSafe,
        aMessage.Executable);

end;

procedure TThreadingProcessHandler.HandleMessageThreadFinished(aMessage: TThreadFinishedMessageInfo);
begin
    TThreadingManager.Instance.InternalThreadFinished(aMessage.TID, INFINITE);
end;

procedure TThreadingProcessHandler.HandleMessageStartThread(aMessage: TStartThreadMessageInfo);
begin
    TThreadingManager.Instance.InternalStartThread(aMessage.SysHandleID);
end;

procedure TThreadingProcessHandler.HandleMessageOpenThread(aMessage: TOpenThreadMessageInfo);
begin
    aMessage.ResultSysHandleID := TThreadingManager.Instance.InternalOpenThread(aMessage.TID);
end;

procedure TThreadingProcessHandler.HandleMessageAddProcess(aMessage: TAddProcessMessageInfo);
begin
    aMessage.ResultProcessSysHandleID := TThreadingManager.Instance.InternalCreateThreadedProcess
        (aMessage.Description, aMessage.ProcessHandler, aMessage.AddressSpace, aMessage.SourceDataName,
        aMessage.IsSystemProcess, aMessage.IsSimulated);
    aMessage.ProcessHandler.Unpause();
end;

procedure TThreadingProcessHandler.HandleMessageExitProcess(aMessage: TExitProcessMessageInfo);
var
    xSysHandleID: TSysHandleID;
begin
    xSysHandleID := GetCallingThreadSysHandleID(aMessage);
    TThreadingManager.Instance.InternalExitProcess(xSysHandleID);
end;

procedure TThreadingProcessHandler.HandleMessageOpenProcess(aMessage: TOpenProcessMessageInfo);
begin
    aMessage.ResultSysHandleID := TThreadingManager.Instance.InternalOpenProcess(aMessage.PID);
end;

procedure TThreadingProcessHandler.HandleMessageWaitTillAllProcessesSuspended
    (aMessage: TWaitTillAllProcessesSuspendedMessageInfo);
begin
    aMessage.ResultSuccess := TThreadingManager.Instance.InternalWaitTillAllProcessesSuspended(0);
end;

procedure TThreadingProcessHandler.HandleMessageCheckAllProcessesExited
    (aMessage: TCheckAllProcessesExitedMessageInfo);
var
    xActiveProcessSysHandleID: TSysHandleID;
begin
    aMessage.ResultAllExited := TThreadingManager.Instance.InternalCheckAllProcessesExited
        (aMessage.CheckSystemProcesses, xActiveProcessSysHandleID);
    aMessage.ResultActiveProcessSysHandleID := xActiveProcessSysHandleID;
end;

procedure TThreadingProcessHandler.HandleMessageSetError(aMessage: TSetErrorMessageInfo);
begin
    TThreadingManager.Instance.InternalSetError();
end;

procedure TThreadingProcessHandler.HandleMessageAtleastOneNonSimulatedProcessExists
    (aMessage: TAtleastOneNonSimulatedProcessExistsMessageInfo);
begin
    aMessage.ResultNonSimulatedProcessExists :=
        TThreadingManager.Instance.InternalAtleastOneNonSimulatedProcessExists();
end;

procedure TThreadingProcessHandler.HandleMessageSuspendAllProcesses
    (aMessage: TSuspendAllProcessesMessageInfo);
begin
    TThreadingManager.Instance.InternalSuspendAllProcesses();
end;

procedure TThreadingProcessHandler.HandleMessageResumeAllProcesses(aMessage: TResumeAllProcessesMessageInfo);
begin
    TThreadingManager.Instance.InternalResumeAllProcesses();
end;

procedure TThreadingProcessHandler.HandleMessageTerminateSecondaryThreads
    (aMessage: TTerminateSecondaryThreadsMessageInfo);
var
    xSysHandleID: TSysHandleID;
begin
    xSysHandleID := GetCallingThreadSysHandleID(aMessage);
    TThreadingManager.Instance.InternalTerminateSecondaryThreads(xSysHandleID);
end;

procedure TThreadingProcessHandler.HandleMessageWaitTillSecondaryThreadsTerminate
    (aMessage: TWaitTillSecondaryThreadsTerminateMessageInfo);
var
    xSysHandleID: TSysHandleID;
begin
    xSysHandleID := GetCallingThreadSysHandleID(aMessage);
    aMessage.ResultSuccess := TThreadingManager.Instance.InternalAreSecondaryThreadsTerminated(xSysHandleID);
end;

procedure TThreadingProcessHandler.HandleMessageCreateLock(aMessage: TCreateLockMessageInfo);
begin
    aMessage.ResultSysHandleID := TThreadingManager.Instance.InternalCreateLock(false);
end;

procedure TThreadingProcessHandler.HandleMessageDestroyLock(aMessage: TDestroyLockMessageInfo);
begin
    TThreadingManager.Instance.InternalRemoveLock(aMessage.SysHandleID);
end;

procedure TThreadingProcessHandler.HandleMessageGetSysObjectData(aMessage: TGetSysObjectDataMessageInfo);
begin
    aMessage.ResultObject := TThreadingManager.Instance.InternalGetSysObjectData(aMessage.SysHandleID);
end;

procedure TThreadingProcessHandler.HandleMessageDuplicateSysHandle(aMessage: TDuplicateSysHandleMessageInfo);
var
    xResultSysHandleID: TSysHandleID;
begin
    aMessage.ResultOriginalSysHandleFound := TThreadingManager.Instance.DuplicateSysHandle
        (aMessage.OriginalSysHandleID, xResultSysHandleID);
    aMessage.ResultSysHandleID := xResultSysHandleID;
end;

procedure TThreadingProcessHandler.HandleMessageCloseSysHandle(aMessage: TCloseSysHandleMessageInfo);
begin
    TThreadingManager.Instance.CloseSysHandle(aMessage.SysHandleID);
end;

procedure TThreadingProcessHandler.DoHandleTerminate(aMessageInfo: TTerminateMessageInfo);
begin
    // usually does not have secondary threads

    // const
    // cWaitTime = 500;
    // cMaxWaitTries = 3;
    // var
    // x : integer;
    // xSysHandleID : TSysHandleID;
    // begin
    // xSysHandleID := GetCallingThreadSysHandleID( aMessageInfo );
    // TThreadingManager.Instance.InternalTerminateSecondaryThreads( xSysHandleID );
    //
    // for x  := 1 to cMaxWaitTries do begin
    // if TThreadingManager.Instance.InternalAreSecondaryThreadsTerminated( xSysHandleID ) then EXIT;
    // Sleep( cWaitTime );
    // end;
end;

procedure TThreadingProcessHandler.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
    try
        inherited;

        if aMessage.MessageInfo is TSetErrorMessageInfo then
        begin
            HandleMessageSetError(aMessage.MessageInfo as TSetErrorMessageInfo);
        end
        else if aMessage.MessageInfo is TCreateThreadMessageInfo then
        begin
            HandleMessageCreateThread(aMessage.MessageInfo as TCreateThreadMessageInfo);
        end
        else if aMessage.MessageInfo is TThreadFinishedMessageInfo then
        begin
            HandleMessageThreadFinished(aMessage.MessageInfo as TThreadFinishedMessageInfo);
        end
        else if aMessage.MessageInfo is TStartThreadMessageInfo then
        begin
            HandleMessageStartThread(aMessage.MessageInfo as TStartThreadMessageInfo);
        end
        else if aMessage.MessageInfo is TOpenThreadMessageInfo then
        begin
            HandleMessageOpenThread(aMessage.MessageInfo as TOpenThreadMessageInfo);
        end
        else if aMessage.MessageInfo is TAddProcessMessageInfo then
        begin
            HandleMessageAddProcess(aMessage.MessageInfo as TAddProcessMessageInfo);
        end
        else if aMessage.MessageInfo is TExitProcessMessageInfo then
        begin
            HandleMessageExitProcess(aMessage.MessageInfo as TExitProcessMessageInfo);
        end
        else if aMessage.MessageInfo is TOpenProcessMessageInfo then
        begin
            HandleMessageOpenProcess(aMessage.MessageInfo as TOpenProcessMessageInfo);
        end
        else if aMessage.MessageInfo is TWaitTillAllProcessesSuspendedMessageInfo then
        begin
            HandleMessageWaitTillAllProcessesSuspended
                (aMessage.MessageInfo as TWaitTillAllProcessesSuspendedMessageInfo);
        end
        else if aMessage.MessageInfo is TCheckAllProcessesExitedMessageInfo then
        begin
            HandleMessageCheckAllProcessesExited(aMessage.MessageInfo as TCheckAllProcessesExitedMessageInfo);
        end
        else if aMessage.MessageInfo is TAtleastOneNonSimulatedProcessExistsMessageInfo then
        begin
            HandleMessageAtleastOneNonSimulatedProcessExists
                (aMessage.MessageInfo as TAtleastOneNonSimulatedProcessExistsMessageInfo);
        end
        else if aMessage.MessageInfo is TSuspendAllProcessesMessageInfo then
        begin
            HandleMessageSuspendAllProcesses(aMessage.MessageInfo as TSuspendAllProcessesMessageInfo);
        end
        else if aMessage.MessageInfo is TResumeAllProcessesMessageInfo then
        begin
            HandleMessageResumeAllProcesses(aMessage.MessageInfo as TResumeAllProcessesMessageInfo);
        end
        else if aMessage.MessageInfo is TTerminateSecondaryThreadsMessageInfo then
        begin
            HandleMessageTerminateSecondaryThreads
                (aMessage.MessageInfo as TTerminateSecondaryThreadsMessageInfo);
        end
        else if aMessage.MessageInfo is TWaitTillSecondaryThreadsTerminateMessageInfo then
        begin
            HandleMessageWaitTillSecondaryThreadsTerminate
                (aMessage.MessageInfo as TWaitTillSecondaryThreadsTerminateMessageInfo);
        end
        else if aMessage.MessageInfo is TCreateLockMessageInfo then
        begin
            HandleMessageCreateLock(aMessage.MessageInfo as TCreateLockMessageInfo);
        end
        else if aMessage.MessageInfo is TDestroyLockMessageInfo then
        begin
            HandleMessageDestroyLock(aMessage.MessageInfo as TDestroyLockMessageInfo);
        end
        else if aMessage.MessageInfo is TGetSysObjectDataMessageInfo then
        begin
            HandleMessageGetSysObjectData(aMessage.MessageInfo as TGetSysObjectDataMessageInfo);
        end
        // else if aMessage.MessageInfo is TOpenThreadByDescriptionMessageInfo then begin
        // HandleMessageOpenThreadByDescription( aMessage.MessageInfo as TOpenThreadByDescriptionMessageInfo );
        // end
        else if aMessage.MessageInfo is TDuplicateSysHandleMessageInfo then
        begin
            HandleMessageDuplicateSysHandle(aMessage.MessageInfo as TDuplicateSysHandleMessageInfo);
        end
        else if aMessage.MessageInfo is TCloseSysHandleMessageInfo then
        begin
            HandleMessageCloseSysHandle(aMessage.MessageInfo as TCloseSysHandleMessageInfo);
        end;
    except
        on E: exception do
            TThreadingManager.Instance.ShowThreadExceptionForCurrentThread(E);
    end;
end;

function TThreadingProcessHandler.CreateThread(const aDescription: string;
    const aCreateSuspended, aCloseHandleOnFinish, aCanSuspendSafe: boolean; const aExecutable: IExecutable)
    : TSysHandleID;
var
    xMessageInfo: TCreateThreadMessageInfo;
begin
    xMessageInfo := TCreateThreadMessageInfo.Create(aDescription, aCreateSuspended, aCloseHandleOnFinish,
        aCanSuspendSafe, aExecutable);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultSysHandleID;
    finally
        xMessageInfo.Free;
    end;
end;

procedure TThreadingProcessHandler.ThreadFinished(const aTID: cardinal);
begin
    self.RegisterMessageAndLeave(TThreadFinishedMessageInfo.Create(aTID));
end;

function TThreadingProcessHandler.CreateProcess(const aDescription: string;
    const aProcessHandler: IProcessHandler; const aAddressSpace: TAddressSpace; const aSourceDataName: string;
    const aIsSystemProcess: boolean; const aIsSimulated: boolean): TSysHandleID;
var
    xMessageInfo: TAddProcessMessageInfo;
begin
    xMessageInfo := TAddProcessMessageInfo.Create(aDescription, aProcessHandler, aAddressSpace,
        aSourceDataName, aIsSystemProcess, aIsSimulated);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultProcessSysHandleID;
    finally
        xMessageInfo.Free;
    end;
end;

procedure TThreadingProcessHandler.ExitProcess();
begin
    self.RegisterMessageAndWait(TExitProcessMessageInfo.Create(), true, INFINITE);
end;

function TThreadingProcessHandler.OpenProcess(const aPID: cardinal): TSysHandleID;
var
    xMessageInfo: TOpenProcessMessageInfo;
begin
    xMessageInfo := TOpenProcessMessageInfo.Create(aPID);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultSysHandleID;
    finally
        xMessageInfo.Free;
    end;
end;

function TThreadingProcessHandler.WaitTillAllProcessesSuspended(const aMaxWaitTime: cardinal): boolean;
const
    cSleepTime = 200;
var
    xMessageInfo: TWaitTillAllProcessesSuspendedMessageInfo;
    xTime_msec: int64;
begin
    xTime_msec := 0;
    xMessageInfo := TWaitTillAllProcessesSuspendedMessageInfo.Create();
    try
        while true do
        begin
            self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
            if xMessageInfo.ResultSuccess then
                BREAK;

            if (xTime_msec >= aMaxWaitTime) then
                EXIT(false);

            Sleep(cSleepTime);
            xTime_msec := xTime_msec + cSleepTime;
        end;
        EXIT(true);
    finally
        xMessageInfo.Free;
    end;
end;

function TThreadingProcessHandler.WaitTillAllProcessesExited(const aCheckSystemProcesses: boolean;
    const aMaxWaitTime: cardinal): boolean;
var
    xMessageInfo: TCheckAllProcessesExitedMessageInfo;
    xTimeout: boolean;
begin
    result := false;
    xMessageInfo := TCheckAllProcessesExitedMessageInfo.Create(aCheckSystemProcesses);
    try
        while true do
        begin
            self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
            if xMessageInfo.ResultAllExited then
            begin
                result := true;
                BREAK;
            end;
            xTimeout := not self.JoinThread(xMessageInfo.ResultActiveProcessSysHandleID, aMaxWaitTime);
            if xTimeout then
                EXIT;
        end;
    finally
        xMessageInfo.Free;
    end;
end;

procedure TThreadingProcessHandler.SuspendAllProcesses();
begin
    self.RegisterMessageAndWait(TSuspendAllProcessesMessageInfo.Create(), true, INFINITE);
end;

procedure TThreadingProcessHandler.ResumeAllProcesses();
begin
    self.RegisterMessageAndWait(TResumeAllProcessesMessageInfo.Create(), true, INFINITE);
end;

procedure TThreadingProcessHandler.TerminateSecondaryThreads;
begin
    self.RegisterMessageAndWait(TTerminateSecondaryThreadsMessageInfo.Create(), true, INFINITE);
end;

procedure TThreadingProcessHandler.WaitTillSecondaryThreadsTerminate(const aMaxWaitTries: integer);
var
    xMessageInfo: TWaitTillSecondaryThreadsTerminateMessageInfo;
    x: integer;
begin
    xMessageInfo := TWaitTillSecondaryThreadsTerminateMessageInfo.Create();
    try
        for x := 1 to aMaxWaitTries do
        begin
            self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
            if xMessageInfo.ResultSuccess then
                BREAK;
            Sleep(200);
        end;
    finally
        xMessageInfo.Free;
    end;
end;

function TThreadingProcessHandler.CreateLock: TSysHandleID;
var
    xMessageInfo: TCreateLockMessageInfo;
begin
    xMessageInfo := TCreateLockMessageInfo.Create();
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultSysHandleID;
    finally
        xMessageInfo.Free;
    end;
end;

procedure TThreadingProcessHandler.DestroyLock(const aSysHandleID: TSysHandleID);
begin
    self.RegisterMessageAndWait(TDestroyLockMessageInfo.Create(aSysHandleID), true, INFINITE);
end;

function TThreadingProcessHandler.InternGetSysObjectData(const aSysHandleID: TSysHandleID): TObject;
var
    xMessageInfo: TGetSysObjectDataMessageInfo;
begin
    xMessageInfo := TGetSysObjectDataMessageInfo.Create(aSysHandleID);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultObject;
    finally
        xMessageInfo.Free;
    end;
end;

function TThreadingProcessHandler.InternGetLock(const aSysHandleID: TSysHandleID): TLock;
var
    xObject: TObject;
begin
    result := nil;
    xObject := InternGetSysObjectData(aSysHandleID);
    if not(xObject is TLock) then
        EXIT;
    result := xObject as TLock;
end;

function TThreadingProcessHandler.WaitForLock(const aSysHandleID: TSysHandleID;
    const aMaxWaitTime: cardinal): boolean;
var
    xLock: TLock;
begin
    xLock := InternGetLock(aSysHandleID);
    result := xLock.WaitForLock(aMaxWaitTime) <> wrTimeout;
end;

procedure TThreadingProcessHandler.OpenLock(const aSysHandleID: TSysHandleID);
var
    xLock: TLock;
begin
    xLock := InternGetLock(aSysHandleID);
    xLock.Unlock()
end;

function TThreadingProcessHandler.DuplicateSysHandle(const aOriginalSysHandleID: TSysHandleID;
    out oNewHandleID: TSyshandleID): boolean;
var
    xMessageInfo: TDuplicateSysHandleMessageInfo;
begin
    xMessageInfo := TDuplicateSysHandleMessageInfo.Create(aOriginalSysHandleID);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        oNewHandleID := xMessageInfo.ResultSysHandleID;
        result := xMessageInfo.ResultOriginalSysHandleFound;
    finally
        xMessageInfo.Free;
    end;
end;

procedure TThreadingProcessHandler.CloseSysHandle(const aSysHandleID: TSysHandleID);
begin
    self.RegisterMessageAndWait(TCloseSysHandleMessageInfo.Create(aSysHandleID), true, INFINITE);
end;

function TThreadingProcessHandler.JoinThread(const aSysHandleID: TSysHandleID;
    const aMaxWaitTime: cardinal): boolean;
var
    xObject: TObject;
    xThreadImage: TThreadImage;
    xSysHandleID: TSysHandleID;
    xSysHandleIsFound: boolean;
begin
    result := true;

    // we have to duplicate the handle if we are going to wait to avoid having the threadimage be destroyed while waiting on it
    xSysHandleIsFound := self.DuplicateSysHandle(aSysHandleID, xSysHandleID);
    if not xSysHandleIsFound then
        EXIT;

    try

        xObject := self.InternGetSysObjectData(xSysHandleID);

        if xObject is TThreadImage then
        begin
            xThreadImage := (xObject as TThreadImage);
        end
        else
        begin
            EXIT;
        end;
        ASSERT(Assigned(xThreadImage));

        result := xThreadImage.EndLock.WaitForLock(aMaxWaitTime) <> wrTimeout;
    finally
        self.CloseSysHandle(xSysHandleID);
    end;
end;

procedure TThreadingProcessHandler.StartThread(const aSysHandleID: TSysHandleID);
begin
    self.RegisterMessageAndWait(TStartThreadMessageInfo.Create(aSysHandleID), true, INFINITE);
end;

function TThreadingProcessHandler.OpenThread(const aTID: cardinal): TSysHandleID;
var
    xMessageInfo: TOpenThreadMessageInfo;
begin
    xMessageInfo := TOpenThreadMessageInfo.Create(aTID);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultSysHandleID;
    finally
        xMessageInfo.Free;
    end;
end;

procedure TThreadingProcessHandler.SetError;
begin
    self.RegisterMessageAndLeave(TSetErrorMessageInfo.Create());
end;

function TThreadingProcessHandler.AtleastOneNonSimulatedProcessExists(): boolean;
var
    xMessageInfo: TAtleastOneNonSimulatedProcessExistsMessageInfo;
begin
    xMessageInfo := TAtleastOneNonSimulatedProcessExistsMessageInfo.Create();
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultNonSimulatedProcessExists;
    finally
        xMessageInfo.Free;
    end;
end;

function TThreadingProcessHandler.GetIsCurrentThreadSimulated(): boolean;
var
    xIsSystemThread: boolean;
begin
    // 15.04.09 pk : IMPORTANT: this function does not post a message.  However it is thread safe because the
    // ThreadRegistry, and ProcessRegistry are threadsafe.  Posting a message took too much time

    result := TThreadingManager.Instance.IsCurrentThreadSimulated(xIsSystemThread);
    if not xIsSystemThread then
        EXIT;

    // 05.12.09 pk : if the current thread or the contractor for the current thread is a system thread, we have to
    // post a message to ask if any non-simulated processes exist.  Posting this message costs a lot of time, but
    // it is the easiest way to get this information and be threadsafe.
    result := not AtleastOneNonSimulatedProcessExists();
end;

class function TThreadingProcessHandler.GetCurrentThreadImage(): TThreadImage;
var
    xCurrentTID: cardinal;
begin
    // 12.12.08 pk : IMPORTANT: this function does not post a message.  However it is thread safe because the
    // ThreadRegistry is threadsafe.
    xCurrentTID := TThreadingManager.Instance.GetCurrentThreadID();
    result := TThreadingManager.Instance.FindThreadImageByThreadID(xCurrentTID)
end;

class function TThreadingProcessHandler.GetCurrentProcess(): TProcess;
var
    xCurrentPID: cardinal;
begin
    xCurrentPID := TThreadingManager.Instance.GetCurrentProcessID;
    result := TThreadingManager.Instance.FindProcessByPID(xCurrentPID);
end;

class function TThreadingProcessHandler.GetCurrentSourceDataName(): string;
var
    xThreadImage: TThreadImage;
    xProcess: TProcess;
begin
    xThreadImage := GetCurrentThreadImage();
    ASSERT(Assigned(xThreadImage));
    if xThreadImage.HasContractorThreadID then
    begin
        xThreadImage := TThreadingManager.Instance.FindThreadImageBySysHandleID
            (xThreadImage.ContractorThreadID, true);
        ASSERT(Assigned(xThreadImage));
    end;
    xProcess := TThreadingManager.Instance.FindProcessByPID(xThreadImage.TCB.PID);
    ASSERT(Assigned(xProcess));
    result := xProcess.SourceDataName;
end;

class function TThreadingProcessHandler.GetCurrentThreadID(): cardinal;
begin
    result := TThreadingManager.Instance.GetCurrentThreadID();
end;

class function TThreadingProcessHandler.GetCurrentProcessDescription(): string;
var
    xProcess: TProcess;
begin
    xProcess := GetCurrentProcess();
    result := TThreadingManager.Instance.FindThreadImageByThreadID(xProcess.PrimaryThreadTID).Description;
end;

class function TThreadingProcessHandler.GetCurrentThreadDescription(): string;
var
    xCurrentThreadImage: TThreadImage;
begin
    xCurrentThreadImage := GetCurrentThreadImage();
    result := xCurrentThreadImage.Description;
end;

class function TThreadingProcessHandler.GetThreadImage(const aSysHandleID: TSysHandleID): TThreadImage;
begin
    result := TThreadingManager.Instance.FindThreadImageBySysHandleID(aSysHandleID, true);
end;

class procedure TThreadingProcessHandler.ChangeCurrentProcessSimulated(const aIsSimulated: boolean);
var
    xProcess: TProcess;
begin
    xProcess := GetCurrentProcess();
    xProcess.IsSimulated := aIsSimulated;
end;


end.
