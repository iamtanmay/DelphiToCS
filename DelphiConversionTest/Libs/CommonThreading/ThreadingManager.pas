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

unit ThreadingManager;


interface


uses
    SyncObjs,
    SysUtils,
    Executable,
    ThreadClasses,
    LockHandle,
    ThreadRegistry,
    ProcessRegistry;

type
    TThreadingThreadFinishedCallback = procedure(const aTID: cardinal) of object;

    TThreadingManager = class
    private
        fThreadingProcess: TProcess;
        fMainAppProcessPrimaryThreadSysHandleID: TSysHandleID;
        fThreadingProcessPrimaryThreadSysHandleID: TSysHandleID;
        fSysHandles: TSysHandleList;
        fSysObjects: TSysObjectList;
        fShowThreadExceptionCallback: TThreadingShowThreadExceptionCallback;
        fLogCallback: TThreadingLogCallback;
        fThreadFinishedCallback: TThreadingThreadFinishedCallback;
        class var uThreadingManager: TThreadingManager;
        function GetThreadRegistry(): TThreadRegistry;
        function GetProcessRegistry(): TProcessRegistry;
        procedure RegisterThreadFinished(const aTID: cardinal);

        procedure TerminateThread(const aSysHandleID: TSysHandleID);
        procedure DestroyThread(const aThreadImage: TThreadImage);
        procedure DestroyProcess(const aProcess: TProcess);
        procedure DestroyLock(const aLock: TLock);
        function GenerateNewSysHandleID(): TSysHandleID;
        function GenerateNewSysObjectID(): TSysObjectID;
        function AddSysObject(const aData: TObject): TSysObjectID;
        function AddSysHandle(const aSysObjectID: TSysObjectID): TSysHandleID;
        function FindSysObjectIDBySysHandleID(const aSysHandleID: TSysHandleID; const aMustFind: boolean)
            : TSysObjectID;
        function FindSysObjectBySysHandleID(const aSysHandleID: TSysHandleID; const aMustFind: boolean)
            : TSysObject;
        function FindThreadImageBySysObjectID(const aSysObjectID: TSysObjectID): TThreadImage;
        procedure DestroySysObjectData(const aData: TObject);

        procedure OpenAllLocks();
        function FindContractorProcessForCurrent(): TProcess;
        function GetSafeSuspendableThreadByID(const aSysHandleID: TSysHandleID): TExecThread;
        procedure SuspendThreadByID(const aSysHandleID: TSysHandleID);
        procedure ResumeThreadByID(const aSysHandleID: TSysHandleID);
        function WaitTillThreadSuspendedByID(const aSysHandleID: TSysHandleID; const aMaxWaitTime: cardinal)
            : TWaitResult;

        // process management
        procedure AddThreadToProcess(const aProcess: TProcess; const aTID: cardinal);
        procedure RemoveThreadFromProcess(const aProcess: TProcess; const aTID: cardinal);
        procedure ProcessSuspendAll(const aProcess: TProcess);
        procedure ProcessResumeAll(const aProcess: TProcess);
        function ProcessWaitTillSuspendedAll(const aProcess: TProcess; const aMaxWaitTime: cardinal): boolean;
        function ProcessGetPrimaryThreadImage(const aProcess: TProcess): TThreadImage;
        function ProcessGetMessagableIntf(const aProcess: TProcess): IMessagable;

        function CreateProcess(): TProcess;
        function CreateThreadInProcess(const aPID: cardinal; const aDescription: string;
            const aCreateSuspended, aCloseHandleOnFinish, aCanSuspendSafe: boolean;
            const aExecutable: IExecutable): TSysHandleID;

        procedure CreateProcessRegistry;
        procedure CreateThreadRegistry;
        procedure RegisterAppProcess();
        procedure UnRegisterAppProcess();
        procedure DoThreadRegistryRegistered(aRegistryItem: TThreadRegistryItem);
        procedure DoThreadRegistryUnRegistered(aRegistryItem: TThreadRegistryItem);
        procedure DoOnThreadStart(aThread: TExecThread);
        procedure DoOnThreadFinish(aThread: TExecThread);
        procedure DoOnThreadingProcessHandlerThreadFinish(aThread: TExecThread);
        procedure DoOnThreadException(aThread: TExecThread; aException: Exception);
        function WaitForThread(const aSysHandleID: TSysHandleID; aMaxWaitTime: cardinal): cardinal;
        procedure ShowThreadExceptionForThreadID(const aTID: integer; aException: Exception);
        procedure ShowThreadException(const aCaption: string; aException: Exception);
        procedure Log(const aText: string);
    public
        constructor Create();
        destructor Destroy(); override;
        class procedure CreateInstance;
        class procedure DestroyInstance;
        class function Instance: TThreadingManager;
        class procedure SetInstance(const aValue: TThreadingManager);

        function FindThreadImageBySysHandleID(const aSysHandleID: TSysHandleID; const aMustFind: boolean)
            : TThreadImage;
        function FindThreadImageByThreadID(const aTID: cardinal): TThreadImage;
        function FindProcessByPID(const aPID: cardinal): TProcess;
        procedure CloseSysHandle(const aSysHandleID: TSysHandleID);
        function DuplicateSysHandle(const aSysHandleID: TSysHandleID; out oNewHandleID: TSysHandleID)
            : boolean;

        function InternalCreateThread(const aCurrentThreadSysHandleID: TSysHandleID;
            const aDescription: string; const aCreateSuspended, aCloseHandleOnFinish,
            aCanSuspendSafe: boolean; const aExecutable: IExecutable): TSysHandleID;

        function InternalCreateThreadedProcess(const aDescription: string;
            const aProcessHandler: IProcessHandler; const aAddressSpace: TAddressSpace;
            const aSourceDataName: string; const aIsSystemProcess: boolean; const aIsSimulated: boolean)
            : TSysHandleID;

        function InternalThreadFinished(const aTID: cardinal; const aMaxWaitTime: cardinal): boolean;
        procedure InternalStartThread(const aSysHandleID: TSysHandleID);
        function InternalOpenThread(const aTID: cardinal): TSysHandleID;
        function InternalOpenThreadByDescription(const aDescription: string): TSysHandleID;
        procedure InternalExitProcess(const aCurrentThreadSysHandleID: TSysHandleID);
        function InternalOpenProcess(const aPID: cardinal): TSysHandleID;
        procedure InternalTerminateSecondaryThreads(const aCurrentThreadSysHandleID: TSysHandleID);
        function InternalAreSecondaryThreadsTerminated(const aCurrentThreadSysHandleID: TSysHandleID)
            : boolean;
        function InternalWaitTillAllProcessesSuspended(const aMaxWaitTime: cardinal): boolean;
        function InternalCheckAllProcessesExited(const aCheckSystemProcesses: boolean;
            out oActiveProcessSysHandleID: TSysHandleID): boolean;
        function InternalAtleastOneNonSimulatedProcessExists(): boolean;
        procedure InternalSuspendAllProcesses();
        procedure InternalResumeAllProcesses();

        function InternalGetSysObjectData(const aSysHandleID: TSysHandleID): TObject;

        function IsCurrentThreadSimulated(out oIsSystemThread: boolean): boolean;
        function GetCurrentThreadID(): cardinal;
        function GetCurrentProcessID: cardinal;

        // lock objects
        function InternalCreateLock(aIsManualReset: boolean): TSysHandleID;
        procedure InternalRemoveLock(const aSysHandleID: TSysHandleID);
        procedure InternalOpenLock(const aSysHandleID: TSysHandleID);

        procedure InternalSetError();
        // threading process
        property ProcessRegistry: TProcessRegistry read GetProcessRegistry;

        procedure StartThreadingProcess(const aThreadingProcessHandler: IProcessHandler);
        procedure CloseThreadingProcess();

        procedure ShowThreadExceptionForCurrentThread(aException: Exception);

        property ThreadRegistry: TThreadRegistry read GetThreadRegistry;
        property ShowThreadExceptionCallback: TThreadingShowThreadExceptionCallback
            read fShowThreadExceptionCallback write fShowThreadExceptionCallback;
        property LogCallback: TThreadingLogCallback read fLogCallback write fLogCallback;
        property ThreadFinishedCallback: TThreadingThreadFinishedCallback read fThreadFinishedCallback
            write fThreadFinishedCallback;
    end;


implementation


uses
    Windows,
    ThreadUtils,
    MessageInfo;

{ TThreadingManager }

constructor TThreadingManager.Create;
begin
    inherited Create();

    fSysObjects := TSysObjectList.Create();
    fSysHandles := TSysHandleList.Create();
    CreateProcessRegistry();
    CreateThreadRegistry();
    self.ThreadRegistry.OnAfterRegistered := DoThreadRegistryRegistered;
    self.ThreadRegistry.OnBeforeUnRegistered := DoThreadRegistryUnRegistered;
    RegisterAppProcess();
end;

destructor TThreadingManager.Destroy();
begin
    UnRegisterAppProcess();
    TThreadRegistry.DestroyInstance();
    TProcessRegistry.DestroyInstance();
    fSysHandles.Free;
    fSysObjects.Free;

    inherited;
end;

function TThreadingManager.CreateProcess(): TProcess;
var
    xSysObjectID: TSysObjectID;
    xSysHandleID: TSysHandleID;
begin
    result := self.ProcessRegistry.CreateProcessAndRegister();
    xSysObjectID := AddSysObject(result);
    result.SetSysObjectID(xSysObjectID);
    xSysHandleID := AddSysHandle(xSysObjectID);
    result.SetCloseUserSysHandleOnFinishInfo(true, xSysHandleID);
    Log(Format('Created Process - PID: %d, ObjectID: %d, HandleID: %d', [result.PCB.PID, xSysObjectID,
        xSysHandleID]));
end;

procedure TThreadingManager.RegisterAppProcess();
var
    xProcess: TProcess;
    xMainAppThreadOSThreadID: cardinal;
    xThreadImage: TThreadImage;
    xSysObjectID: TSysObjectID;
    xSysHandleID: TSysHandleID;
begin
    xProcess := self.CreateProcess();
    xProcess.IsAppProcess := true;
    xProcess.IsSystemProcess := true;
    // we assume that this function is called by the main application thread.  We could also have used Application.MainThreadID
    xMainAppThreadOSThreadID := TPlatformSpecificOS.GetCurrentThreadID();

    xThreadImage := TThreadImage.Create();
    xThreadImage.Description := 'App Thread';
    xSysObjectID := self.AddSysObject(xThreadImage);
    xThreadImage.TCB.TID := xSysObjectID;
    xThreadImage.SetSysObjectID(xSysObjectID);
    xThreadImage.SetThread(nil);
    xThreadImage.TCB.OSThreadID := xMainAppThreadOSThreadID;
    xThreadImage.TCB.PID := xProcess.PCB.PID;
    xSysHandleID := self.AddSysHandle(xSysObjectID);
    xThreadImage.SetCloseUserSysHandleOnFinishInfo(false, xSysHandleID);
    fMainAppProcessPrimaryThreadSysHandleID := xSysHandleID;

    self.ThreadRegistry.RegisterAppThread(xThreadImage);
end;

procedure TThreadingManager.StartThreadingProcess(const aThreadingProcessHandler: IProcessHandler);
var
    xProcess: TProcess;
    xThreadImage: TThreadImage;
begin
    xProcess := self.CreateProcess();
    fThreadingProcess := xProcess;
    fThreadingProcess.IsSystemProcess := true;
    fThreadingProcessPrimaryThreadSysHandleID := self.CreateThreadInProcess(fThreadingProcess.PCB.PID,
        'Threading Manager', true, false, false, aThreadingProcessHandler);
    xThreadImage := self.FindThreadImageBySysHandleID(fThreadingProcessPrimaryThreadSysHandleID, true);
    xThreadImage.Thread.OnFinish := DoOnThreadingProcessHandlerThreadFinish;
    // aThreadingProcessHandler.fTID := xThreadImage.TCB.TID;
    aThreadingProcessHandler.Unpause();
    // it is important to resume After we call unpause because unpause will set a new mask
    self.InternalStartThread(fThreadingProcessPrimaryThreadSysHandleID);

end;

procedure TThreadingManager.CloseThreadingProcess();
const
    cWaitTime = 5000;
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := self.FindThreadImageBySysHandleID(fThreadingProcessPrimaryThreadSysHandleID, true);

    self.InternalThreadFinished(xThreadImage.TCB.TID, cWaitTime);

    self.CloseSysHandle(fThreadingProcessPrimaryThreadSysHandleID);
end;

procedure TThreadingManager.UnRegisterAppProcess;
begin
    CloseSysHandle(fMainAppProcessPrimaryThreadSysHandleID);
end;

function TThreadingManager.GetThreadRegistry(): TThreadRegistry;
begin
    result := TThreadRegistry.Instance();
end;

function TThreadingManager.GetProcessRegistry: TProcessRegistry;
begin
    result := TProcessRegistry.Instance();
end;

procedure TThreadingManager.CreateThreadRegistry();
begin
    TThreadRegistry.CreateInstance();
end;

procedure TThreadingManager.CreateProcessRegistry();
begin
    TProcessRegistry.CreateInstance();
end;

class procedure TThreadingManager.CreateInstance();
begin
    if Assigned(uThreadingManager) then
        EXIT;
    SetInstance(TThreadingManager.Create());
end;

class procedure TThreadingManager.DestroyInstance();
begin
    uThreadingManager.Free;
end;

class function TThreadingManager.Instance(): TThreadingManager;
begin
    result := uThreadingManager;
end;

class procedure TThreadingManager.SetInstance(const aValue: TThreadingManager);
begin
    uThreadingManager := aValue;
end;

procedure TThreadingManager.DoThreadRegistryRegistered(aRegistryItem: TThreadRegistryItem);
begin
end;

procedure TThreadingManager.DoThreadRegistryUnRegistered(aRegistryItem: TThreadRegistryItem);
begin
end;

procedure TThreadingManager.DoOnThreadStart(aThread: TExecThread);
begin
end;

procedure TThreadingManager.ShowThreadException(const aCaption: string; aException: Exception);
begin
    if not Assigned(fShowThreadExceptionCallback) then
        EXIT;
    fShowThreadExceptionCallback(aCaption, aException);
end;

procedure TThreadingManager.Log(const aText: string);
begin
    if not Assigned(fLogCallback) then
        EXIT;
    fLogCallback(aText);
end;

function TThreadingManager.GenerateNewSysHandleID(): TSysHandleID;
var
    xID: TSysHandleID;
begin
    // dont use any psuedohandles
    xID := cFirstRealSysHandleID;

    while true do
    begin
        if fSysHandles.IndexOf(xID) < 0 then
        begin
            result := xID;
            EXIT;
        end;
        Inc(xID);
    end;
end;

function TThreadingManager.GenerateNewSysObjectID(): TSysObjectID;
var
    xID: TSysObjectID;
begin
    xID := 1;
    while true do
    begin
        if fSysObjects.IndexOf(xID) < 0 then
        begin
            result := xID;
            EXIT;
        end;
        Inc(xID);
    end;
end;

function TThreadingManager.AddSysObject(const aData: TObject): TSysObjectID;
begin
    result := GenerateNewSysObjectID();
    fSysObjects.Add(TSysObject.Create(result, aData));
end;

function TThreadingManager.AddSysHandle(const aSysObjectID: TSysObjectID): TSysHandleID;
var
    xSysObject: TSysObject;
begin
    result := GenerateNewSysHandleID();
    xSysObject := fSysObjects.FindByID(aSysObjectID, true);
    xSysObject.IncRefCount();
    fSysHandles.Add(TSysHandle.Create(result, aSysObjectID));
    Log(Format('Add Sys Handle HandleID: %d to ObjectID %d', [result, aSysObjectID]));
end;

function TThreadingManager.FindSysObjectIDBySysHandleID(const aSysHandleID: TSysHandleID;
    const aMustFind: boolean): TSysObjectID;
var
    xSysHandle: TSysHandle;
begin
    result := 0;
    xSysHandle := fSysHandles.FindByID(aSysHandleID, aMustFind);
    if not Assigned(xSysHandle) then
        EXIT;
    result := xSysHandle.SysObjectID;
end;

function TThreadingManager.FindSysObjectBySysHandleID(const aSysHandleID: TSysHandleID;
    const aMustFind: boolean): TSysObject;
var
    xSysObjectID: TSysObjectID;
begin
    xSysObjectID := FindSysObjectIDBySysHandleID(aSysHandleID, aMustFind);
    result := fSysObjects.FindByID(xSysObjectID, aMustFind);
end;

function TThreadingManager.FindThreadImageBySysObjectID(const aSysObjectID: TSysObjectID): TThreadImage;
var
    xSysObject: TSysObject;
begin
    xSysObject := fSysObjects.FindByID(aSysObjectID, true);
    ASSERT(xSysObject.Data is TThreadImage);
    result := (xSysObject.Data as TThreadImage);
end;

function TThreadingManager.FindThreadImageByThreadID(const aTID: cardinal): TThreadImage;
begin
    result := self.ThreadRegistry.FindThreadImageByThreadID(aTID);
end;

function TThreadingManager.FindThreadImageBySysHandleID(const aSysHandleID: TSysHandleID;
    const aMustFind: boolean): TThreadImage;
var
    xSysObjectID: TSysObjectID;
begin
    xSysObjectID := FindSysObjectIDBySysHandleID(aSysHandleID, aMustFind);
    result := FindThreadImageBySysObjectID(xSysObjectID);
end;

function TThreadingManager.FindProcessByPID(const aPID: cardinal): TProcess;
begin
    result := self.ProcessRegistry.FindProcessByID(aPID);
end;

function TThreadingManager.DuplicateSysHandle(const aSysHandleID: TSysHandleID;
    out oNewHandleID: TSysHandleID): boolean;
var
    xSysObject: TSysObject;
begin
    oNewHandleID := cHandleIDNone;
    xSysObject := FindSysObjectBySysHandleID(aSysHandleID, false);
    result := Assigned(xSysObject);
    if not result then
        EXIT;

    oNewHandleID := AddSysHandle(xSysObject.ID);
    Log(Format('Duplicate Sys Handle Original HandleID %d, New HandleID: %d', [aSysHandleID, oNewHandleID]));
end;

function TThreadingManager.GetSafeSuspendableThreadByID(const aSysHandleID: TSysHandleID): TExecThread;
var
    xThreadImage: TThreadImage;
    xThread: TExecThread;
begin
    result := nil;
    xThreadImage := self.FindThreadImageBySysHandleID(aSysHandleID, false);
    xThread := xThreadImage.Thread;
    if not(xThread is TExecThread) then
        EXIT;
    if not xThread.CanSuspendSafe then
        EXIT;

    result := xThread;
end;

procedure TThreadingManager.SuspendThreadByID(const aSysHandleID: TSysHandleID);
var
    xThread: TExecThread;
begin
    xThread := GetSafeSuspendableThreadByID(aSysHandleID);
    if not Assigned(xThread) then
        EXIT;
    xThread.SuspendSafe();
end;

procedure TThreadingManager.ResumeThreadByID(const aSysHandleID: TSysHandleID);
var
    xThread: TExecThread;
begin
    xThread := GetSafeSuspendableThreadByID(aSysHandleID);
    if not Assigned(xThread) then
        EXIT;
    xThread.ResumeSafe();
end;

function TThreadingManager.WaitTillThreadSuspendedByID(const aSysHandleID: TSysHandleID;
    const aMaxWaitTime: cardinal): TWaitResult;
var
    xThread: TExecThread;
begin
    result := wrSignaled;
    xThread := GetSafeSuspendableThreadByID(aSysHandleID);
    if not Assigned(xThread) then
        EXIT;
    result := xThread.WaitTillSuspended(aMaxWaitTime);
end;

procedure TThreadingManager.ProcessResumeAll(const aProcess: TProcess);
var
    x: integer;
    xSysHandleID: TSysHandleID;
begin
    for x := 0 to aProcess.ThreadSysHandleIDs.Count - 1 do
    begin
        xSysHandleID := aProcess.ThreadSysHandleIDs[x].SysHandleID;
        ResumeThreadByID(xSysHandleID);
    end;

    aProcess.PCB.Suspended := false;
end;

procedure TThreadingManager.ProcessSuspendAll(const aProcess: TProcess);
var
    x: integer;
    xSysHandleID: TSysHandleID;
begin
    for x := 0 to aProcess.ThreadSysHandleIDs.Count - 1 do
    begin
        xSysHandleID := aProcess.ThreadSysHandleIDs[x].SysHandleID;
        SuspendThreadByID(xSysHandleID);
    end;

    aProcess.PCB.Suspended := true;
end;

function TThreadingManager.ProcessWaitTillSuspendedAll(const aProcess: TProcess;
    const aMaxWaitTime: cardinal): boolean;
var
    x: integer;
    xWaitResult: TWaitResult;
    xSysHandleID: TSysHandleID;
begin
    result := true;
    for x := 0 to aProcess.ThreadSysHandleIDs.Count - 1 do
    begin
        xSysHandleID := aProcess.ThreadSysHandleIDs[x].SysHandleID;
        xWaitResult := WaitTillThreadSuspendedByID(xSysHandleID, aMaxWaitTime);
        if xWaitResult = wrTimeout then
        begin
            result := false;
            EXIT;
        end;
    end;
end;

function TThreadingManager.ProcessGetPrimaryThreadImage(const aProcess: TProcess): TThreadImage;
var
    xSysHandleID: TSysHandleID;
begin
    xSysHandleID := aProcess.PrimaryThreadSysHandleID;
    result := self.FindThreadImageBySysHandleID(xSysHandleID, true);
end;

function TThreadingManager.ProcessGetMessagableIntf(const aProcess: TProcess): IMessagable;
begin
    Supports(ProcessGetPrimaryThreadImage(aProcess).TCB.Executable, IMessagable, result);
end;

procedure TThreadingManager.AddThreadToProcess(const aProcess: TProcess; const aTID: cardinal);
var
    xThreadImage: TThreadImage;
    xSysHandleID: TSysHandleID;
begin
    xThreadImage := self.FindThreadImageByThreadID(aTID);

    xSysHandleID := AddSysHandle(xThreadImage.SysObjectID);
    ASSERT(Assigned(xThreadImage));

    xThreadImage.TCB.PID := aProcess.PCB.PID;

    aProcess.AddThread(aTID);
    aProcess.AddThreadSysHandle(aTID, xSysHandleID);

end;

procedure TThreadingManager.RemoveThreadFromProcess(const aProcess: TProcess; const aTID: cardinal);
begin

    aProcess.RemoveThread(aTID);
end;

function TThreadingManager.CreateThreadInProcess(const aPID: cardinal; const aDescription: string;
    const aCreateSuspended, aCloseHandleOnFinish, aCanSuspendSafe: boolean; const aExecutable: IExecutable)
    : TSysHandleID;
var
    xThreadRegistryItem: TThreadRegistryItem;
    xDescription: string;
    xProcess: TProcess;
    xThread: TExecThread;
    xSysObjectID: TSysObjectID;
    xThreadImage: TThreadImage;
    xSysHandleID: TSysHandleID;
    xLogText: string;
begin
    xThreadImage := TThreadImage.Create();
    xThreadRegistryItem := TThreadRegistry.Instance.CreateRegistryItem();
    xThreadRegistryItem.ThreadImage := xThreadImage;

    xSysObjectID := AddSysObject(xThreadImage);
    xThreadImage.SetSysObjectID(xSysObjectID);

    // here we create a handle to the threadimage that the user can use for other thread calls
    xSysHandleID := AddSysHandle(xSysObjectID);

    // for now TID is same as SysObjID, but there is no real reason for this
    xThreadImage.TCB.TID := xSysObjectID;

    // sometimes the user wants the thread to close its own handle when it is finished.
    // This should be false when the user wants to join or wait for the thread
    xThreadImage.SetCloseUserSysHandleOnFinishInfo(aCloseHandleOnFinish, xSysHandleID);

    xDescription := self.ThreadRegistry.GenerateUniqueThreadDescription(aDescription);

    xProcess := self.FindProcessByPID(aPID);

    xThread := self.ThreadRegistry.CreateThreadWithRegistryItem(xProcess.PCB.PID, xThreadRegistryItem,
        xDescription, aCreateSuspended, aCanSuspendSafe, aExecutable);

    xThread.OnStart := DoOnThreadStart;
    xThread.OnFinish := DoOnThreadFinish;
    xThread.OnException := DoOnThreadException;

    AddThreadToProcess(xProcess, xThreadImage.TCB.TID);

    if xProcess.Threads.Count > 1 then
        ProcessGetMessagableIntf(xProcess).RegisterMessageAndLeave
            (TSecondaryThreadAddedMessageInfo.Create(xThreadImage.TCB.TID));

    if xProcess.PCB.Suspended then
    begin
        xThread.SuspendSafe();
    end;

    if not aCreateSuspended then
        xThread.FirstResume();

    xLogText :=
        Format('Created Thread - PID: %d, OSThreadID: %d, TID: %d, ObjectID: %d, HandleID: %d, Description: %s',
        [xThreadImage.TCB.PID, xThreadImage.OSThreadID, xThreadImage.TCB.TID, xSysObjectID, xSysHandleID,
        xThreadImage.Description]);
    Log(xLogText);

    outputdebugstring(PChar(xLogText));

    result := xSysHandleID;
end;

function TThreadingManager.InternalCreateThread(const aCurrentThreadSysHandleID: TSysHandleID;
    const aDescription: string; const aCreateSuspended, aCloseHandleOnFinish, aCanSuspendSafe: boolean;
    const aExecutable: IExecutable): TSysHandleID;
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := self.FindThreadImageBySysHandleID(aCurrentThreadSysHandleID, true);
    result := self.CreateThreadInProcess(xThreadImage.TCB.PID, aDescription, aCreateSuspended,
        aCloseHandleOnFinish, aCanSuspendSafe, aExecutable);
end;

function TThreadingManager.InternalCreateThreadedProcess(const aDescription: string;
    const aProcessHandler: IProcessHandler; const aAddressSpace: TAddressSpace; const aSourceDataName: string;
    const aIsSystemProcess: boolean; const aIsSimulated: boolean): TSysHandleID;
const
    cProcessPrimaryThreadCanSuspendSafe = true;
var
    xProcess: TProcess;
begin
    xProcess := CreateProcess();
    result := xProcess.CloseUserSysHandleOnFinishInfo.SysHandleID;

    xProcess.SetAddressSpace(aAddressSpace);

    // assumption: if an addressspace is defined, then it is not a system process
    xProcess.IsSystemProcess := aIsSystemProcess;

    xProcess.SourceDataName := aSourceDataName;
    xProcess.IsSimulated := aIsSimulated;
    TThreadingManager.Instance.CreateThreadInProcess(xProcess.PCB.PID, aDescription, false, true,
        cProcessPrimaryThreadCanSuspendSafe, aProcessHandler);
end;

procedure TThreadingManager.ShowThreadExceptionForThreadID(const aTID: integer; aException: Exception);
var
    xThreadImage: TThreadImage;
    xCaption: string;
begin
    xThreadImage := self.ThreadRegistry.FindThreadImageByThreadID(aTID);
    xCaption := 'Thread ';
    if Assigned(xThreadImage) then
        xCaption := xCaption + xThreadImage.Description;
    ShowThreadException(xCaption, aException);
end;

procedure TThreadingManager.ShowThreadExceptionForCurrentThread(aException: Exception);
begin
    ShowThreadExceptionForThreadID(GetCurrentThreadID(), aException);
end;

procedure TThreadingManager.DoOnThreadFinish(aThread: TExecThread);
begin
    // register a message to remove this thread and leave
    RegisterThreadFinished(aThread.ThreadImage.TCB.TID);
end;

procedure TThreadingManager.DoOnThreadingProcessHandlerThreadFinish(aThread: TExecThread);
begin
end;

procedure TThreadingManager.DoOnThreadException(aThread: TExecThread; aException: Exception);
begin
    ShowThreadExceptionForThreadID(aThread.ThreadImage.TCB.TID, aException);
end;

procedure TThreadingManager.RegisterThreadFinished(const aTID: cardinal);
begin
    ASSERT(Assigned(fThreadFinishedCallback));
    fThreadFinishedCallback(aTID);
end;

function TThreadingManager.WaitForThread(const aSysHandleID: TSysHandleID; aMaxWaitTime: cardinal): cardinal;
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := self.FindThreadImageBySysHandleID(aSysHandleID, true);

    // first try to see if thread is already terminated
    result := WaitForSingleObject(xThreadImage.Thread.Handle, 0);

    // if the maxwaittime is greater than 0 we want to log and wait for the thread
    if aMaxWaitTime > 0 then
    begin
        if result <> WAIT_OBJECT_0 then
        begin
            result := WaitForSingleObject(xThreadImage.Thread.Handle, aMaxWaitTime);
        end;
    end;
end;

function TThreadingManager.InternalThreadFinished(const aTID: cardinal; const aMaxWaitTime: cardinal)
    : boolean;
var
    xThreadImage: TThreadImage;
    xProcess: TProcess;
    xProcessThreadSysHandleID: TSysHandleID;
    xSysHandleID: TSysHandleID;
begin
    xThreadImage := self.FindThreadImageByThreadID(aTID);
    xProcess := self.FindProcessByPID(xThreadImage.TCB.PID);
    xProcessThreadSysHandleID := xProcess.ThreadSysHandleIDs.FindByTID(aTID).SysHandleID;

    result := self.WaitForThread(xProcessThreadSysHandleID, aMaxWaitTime) <> WAIT_TIMEOUT;
    if not result then
        EXIT;

    Log(Format('Thread Finished - OSThreadID: %d, TID: %d', [xThreadImage.OSThreadID, xThreadImage.TCB.TID]));

    if xThreadImage.CloseUserSysHandleOnFinishInfo.CloseSysHandleOnFinish then
    begin
        xSysHandleID := xThreadImage.CloseUserSysHandleOnFinishInfo.SysHandleID;
        Log(Format('Close Thread Handle - TID: %d, Handle %d', [xThreadImage.TCB.TID, xSysHandleID]));

        self.CloseSysHandle(xSysHandleID);
    end;

    if xProcess.PrimaryThreadTID <> xThreadImage.TCB.TID then
    begin
        ProcessGetMessagableIntf(xProcess).RegisterMessageAndLeave
            (TSecondaryThreadRemovedMessageInfo.Create(aTID));
    end;

    xProcess.RemoveThreadSysHandle(aTID);

    Log(Format('Close ProcessThreadHandle - TID: %d, Handle %d', [xThreadImage.TCB.TID,
        xProcessThreadSysHandleID]));

    xThreadImage.EndLock.Unlock;

    self.CloseSysHandle(xProcessThreadSysHandleID);
end;

procedure TThreadingManager.InternalStartThread(const aSysHandleID: TSysHandleID);
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := FindThreadImageBySysHandleID(aSysHandleID, true);
    xThreadImage.Thread.FirstResume();
end;

function TThreadingManager.InternalOpenThread(const aTID: cardinal): TSysHandleID;
var
    xThreadImage: TThreadImage;
begin
    result := cHandleIDNone;
    xThreadImage := self.FindThreadImageByThreadID(aTID);
    if not Assigned(xThreadImage) then
        EXIT;

    result := AddSysHandle(xThreadImage.SysObjectID);
end;

function TThreadingManager.InternalOpenThreadByDescription(const aDescription: string): TSysHandleID;
var
    xThreadImage: TThreadImage;
begin
    result := cHandleIDNone;
    xThreadImage := TThreadingManager.Instance.ThreadRegistry.GetThreadInfoByDescription(aDescription);;
    if not Assigned(xThreadImage) then
        EXIT;

    result := AddSysHandle(xThreadImage.SysObjectID);
end;

procedure TThreadingManager.TerminateThread(const aSysHandleID: TSysHandleID);
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := FindThreadImageBySysHandleID(aSysHandleID, true);
    xThreadImage.TCB.Executable.Terminate();
end;

procedure TThreadingManager.InternalExitProcess(const aCurrentThreadSysHandleID: TSysHandleID);
var
    xProcess: TProcess;
    xPrimaryThreadSysHandleID: TSysHandleID;
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := self.FindThreadImageBySysHandleID(aCurrentThreadSysHandleID, true);
    xProcess := self.FindProcessByPID(xThreadImage.TCB.PID);
    ASSERT(Assigned(xProcess));
    xPrimaryThreadSysHandleID := xProcess.PrimaryThreadSysHandleID;
    ASSERT(xPrimaryThreadSysHandleID <> cHandleIDNone);
    // We assume that if we terminate the primary thread of the process, that the primary thread will terminate the rest
    self.TerminateThread(xPrimaryThreadSysHandleID);
end;

function TThreadingManager.InternalOpenProcess(const aPID: cardinal): TSysHandleID;
var
    xProcess: TProcess;
begin
    result := cHandleIDNone;
    xProcess := self.FindProcessByPID(aPID);
    if not Assigned(xProcess) then
        EXIT;

    result := AddSysHandle(xProcess.SysObjectID);
end;

procedure TThreadingManager.InternalTerminateSecondaryThreads(const aCurrentThreadSysHandleID: TSysHandleID);
var
    x: integer;
    xCurrentThreadImage: TThreadImage;
    xProcess: TProcess;
    xPID: cardinal;
begin
    xCurrentThreadImage := self.FindThreadImageBySysHandleID(aCurrentThreadSysHandleID, true);
    xPID := xCurrentThreadImage.TCB.PID;
    xProcess := self.FindProcessByPID(xPID);
    // use the ThreadSysHandleIDs list, because these are the only threads that are still active
    for x := 0 to xProcess.ThreadSysHandleIDs.Count - 1 do
    begin
        if xProcess.PrimaryThreadTID = xProcess.ThreadSysHandleIDs[x].TID then
            CONTINUE;
        self.TerminateThread(aCurrentThreadSysHandleID);
    end;
end;

function TThreadingManager.InternalAreSecondaryThreadsTerminated(const aCurrentThreadSysHandleID
    : TSysHandleID): boolean;
var
    x: integer;
    xCurrentThreadImage: TThreadImage;
    xProcess: TProcess;
    xPID: cardinal;
begin
    result := true;
    xCurrentThreadImage := self.FindThreadImageBySysHandleID(aCurrentThreadSysHandleID, true);
    xPID := xCurrentThreadImage.TCB.PID;
    xProcess := self.FindProcessByPID(xPID);
    for x := 0 to xProcess.ThreadSysHandleIDs.Count - 1 do
    begin
        if xProcess.PrimaryThreadTID <> xProcess.ThreadSysHandleIDs[x].TID then
        begin
            result := false;
            EXIT;
        end;
    end;
end;

function TThreadingManager.InternalWaitTillAllProcessesSuspended(const aMaxWaitTime: cardinal): boolean;
var
    x: integer;
    xProcess: TProcess;
begin
    result := true;
    for x := 0 to self.ProcessRegistry.ProcessCount - 1 do
    begin
        xProcess := self.ProcessRegistry.Processes[x];
        if xProcess.IsSystemProcess then
            CONTINUE;
        if xProcess.IsAppProcess then
            CONTINUE;
        result := ProcessWaitTillSuspendedAll(xProcess, aMaxWaitTime);
        if not result then
            EXIT;
    end;
end;

function TThreadingManager.InternalCheckAllProcessesExited(const aCheckSystemProcesses: boolean;
    out oActiveProcessSysHandleID: TSysHandleID): boolean;
var
    x: integer;
    xProcess: TProcess;
    xSysHandleID: TSysHandleID;
begin
    result := true;
    oActiveProcessSysHandleID := cHandleIDNone;
    for x := 0 to self.ProcessRegistry.ProcessCount - 1 do
    begin
        xProcess := self.ProcessRegistry.Processes[x];
        if xProcess.IsAppProcess then
            CONTINUE;
        if xProcess.PCB.PID = fThreadingProcess.PCB.PID then
            CONTINUE;

        if (not aCheckSystemProcesses) and xProcess.IsSystemProcess then
            CONTINUE;

        if xProcess.ThreadSysHandleIDs.Count = 0 then
            CONTINUE;
        // we will return the mainthreadsyshandleid, but if it is not valid return the first syshandleid.
        xSysHandleID := xProcess.PrimaryThreadSysHandleID;
        if xSysHandleID = cHandleIDNone then
        begin
            xSysHandleID := xProcess.ThreadSysHandleIDs[0].SysHandleID;
        end;

        oActiveProcessSysHandleID := xSysHandleID;
        result := false;
        EXIT;
    end;
end;

procedure TThreadingManager.InternalSuspendAllProcesses();
var
    x: integer;
    xProcess: TProcess;
begin
    for x := 0 to self.ProcessRegistry.ProcessCount - 1 do
    begin
        xProcess := self.ProcessRegistry.Processes[x];
        if xProcess.IsSystemProcess then
            CONTINUE;
        if xProcess.IsAppProcess then
            CONTINUE;
        ProcessSuspendAll(xProcess);
    end;
end;

procedure TThreadingManager.InternalResumeAllProcesses();
var
    x: integer;
    xProcess: TProcess;
begin
    for x := 0 to self.ProcessRegistry.ProcessCount - 1 do
    begin
        xProcess := self.ProcessRegistry.Processes[x];
        if xProcess.IsSystemProcess then
            CONTINUE;
        if xProcess.IsAppProcess then
            CONTINUE;
        ProcessResumeAll(xProcess);
    end;
end;

procedure TThreadingManager.DestroyThread(const aThreadImage: TThreadImage);
var
    xProcess: TProcess;
    xLogText: string;
    xProcessSysHandleID: TSysHandleID;
begin
    xProcess := self.FindProcessByPID(aThreadImage.TCB.PID);
    self.RemoveThreadFromProcess(xProcess, aThreadImage.TCB.TID);
    xLogText := Format('Destroyed Thread - PID: %d, OSThreadID: %d, TID: %d, ObjectID: %d, Description: %s',
        [aThreadImage.TCB.PID, aThreadImage.OSThreadID, aThreadImage.TCB.TID, aThreadImage.SysObjectID,
        aThreadImage.Description]);
    Log(xLogText);
    outputdebugstring(Pchar(xLogText));
    self.ThreadRegistry.UnRegisterThread(aThreadImage.TCB.TID);
    aThreadImage.Thread.Free;
    aThreadImage.Free;

    if (xProcess.Threads.Count = 0) and (xProcess.CloseUserSysHandleOnFinishInfo.CloseSysHandleOnFinish) then
    begin
        xProcessSysHandleID := xProcess.CloseUserSysHandleOnFinishInfo.SysHandleID;
        Log(Format('Close Process Handle - PID: %d, HandleID: %d', [xProcess.PCB.PID, xProcessSysHandleID]));
        CloseSysHandle(xProcessSysHandleID);
    end;
end;

procedure TThreadingManager.DestroyProcess(const aProcess: TProcess);
begin
    Log(Format('Destroyed Process - PID: %d, ObjectID: %d', [aProcess.PCB.PID, aProcess.SysObjectID]));
    self.ProcessRegistry.DestroyProcessAndUnRegister(aProcess.PCB.PID);
    aProcess.Free;
end;

procedure TThreadingManager.DestroyLock(const aLock: TLock);
begin
    aLock.Free;
end;

procedure TThreadingManager.DestroySysObjectData(const aData: TObject);
begin
    if aData is TThreadImage then
    begin
        DestroyThread(aData as TThreadImage);
    end
    else if aData is TProcess then
    begin
        DestroyProcess(aData as TProcess);
    end
    else if aData is TLock then
    begin
        DestroyLock(aData as TLock);
    end;
end;

procedure TThreadingManager.CloseSysHandle(const aSysHandleID: TSysHandleID);
var
    xSysObject: TSysObject;
begin
    xSysObject := FindSysObjectBySysHandleID(aSysHandleID, true);
    fSysHandles.RemoveByID(aSysHandleID);
    Log(Format('CloseHandle HandleID: %d to ObjectID: %d', [aSysHandleID, xSysObject.ID]));

    xSysObject.DecRefCount();
    if xSysObject.RefCount = 0 then
    begin
        DestroySysObjectData(xSysObject.Data);
        fSysObjects.RemoveByID(xSysObject.ID);
    end;
end;

function TThreadingManager.InternalCreateLock(aIsManualReset: boolean): TSysHandleID;
var
    xLock: TLock;
    xSysObjectID: TSysObjectID;
begin
    xLock := TSimpleLock.Create(false, false, true);
    xSysObjectID := AddSysObject(xLock);

    result := AddSysHandle(xSysObjectID);
end;

procedure TThreadingManager.InternalRemoveLock(const aSysHandleID: TSysHandleID);
begin
    CloseSysHandle(aSysHandleID);
end;

function TThreadingManager.InternalGetSysObjectData(const aSysHandleID: TSysHandleID): TObject;
var
    xSysHandle: TSysHandle;
    xSysObject: TSysObject;
begin
    result := nil;
    xSysHandle := fSysHandles.FindByID(aSysHandleID, false);
    if not Assigned(xSysHandle) then
        EXIT;
    xSysObject := fSysObjects.FindByID(xSysHandle.SysObjectID, true);
    result := xSysObject.Data;
end;

procedure TThreadingManager.InternalOpenLock(const aSysHandleID: TSysHandleID);
var
    xObject: TObject;
begin
    xObject := InternalGetSysObjectData(aSysHandleID);
    if not(xObject is TLock) then
        EXIT;
    (xObject as TLock).Unlock();
end;

procedure TThreadingManager.OpenAllLocks();
var
    x: integer;
    xSysObjectData: TObject;
begin
    for x := 0 to fSysObjects.Count - 1 do
    begin
        xSysObjectData := fSysObjects[x].Data;
        if xSysObjectData is TLock then
        begin
            (xSysObjectData as TLock).UnLock();
        end;
    end;
end;

function TThreadingManager.InternalAtleastOneNonSimulatedProcessExists(): boolean;
var
    x: integer;
    xProcess: TProcess;
begin
    result := true; // it is safer to assume that we are in live-mode if there are no methods running

    for x := 0 to self.ProcessRegistry.ProcessCount - 1 do
    begin
        xProcess := self.ProcessRegistry.Processes[x];
        if xProcess.IsSystemProcess then
            CONTINUE; // threading process
        if xProcess.IsAppProcess then
            CONTINUE;
        result := not xProcess.IsSimulated;
        if result then
            EXIT;
    end;
end;

procedure TThreadingManager.InternalSetError();
begin
    OpenAllLocks();
end;

function TThreadingManager.FindContractorProcessForCurrent(): TProcess;
// Threadsafe
var
    xCurrentThreadID: cardinal;
    xThreadImage: TThreadImage;
begin
    xCurrentThreadID := GetCurrentThreadID();
    xThreadImage := self.FindThreadImageByThreadID(xCurrentThreadID);

    ASSERT(Assigned(xThreadImage));
    if xThreadImage.HasContractorThreadID then
    begin
        xThreadImage := self.FindThreadImageByThreadID(xThreadImage.ContractorThreadID);
    end;
    result := self.FindProcessByPID(xThreadImage.TCB.PID);
end;

function TThreadingManager.IsCurrentThreadSimulated(out oIsSystemThread: boolean): boolean;
// Threadsafe
// This function only gives the correct information if aThreadID is the ID of a non-system thread
var
    xProcess: TProcess;
begin
    xProcess := FindContractorProcessForCurrent();
    ASSERT(Assigned(xProcess));
    oIsSystemThread := (xProcess.IsSystemProcess) or (xProcess.IsAppProcess);

    if oIsSystemThread then
        result := false
    else
        result := xProcess.IsSimulated;
end;

function TThreadingManager.GetCurrentThreadID: cardinal;
begin
    result := self.ThreadRegistry.FindCurrentThreadID();
end;

function TThreadingManager.GetCurrentProcessID: cardinal;
var
    xCurrentThreadID: cardinal;
    xThreadImage: TThreadImage;
begin
    xCurrentThreadID := GetCurrentThreadID();
    xThreadImage := self.FindThreadImageByThreadID(xCurrentThreadID);
    result := xThreadImage.TCB.PID;
end;


end.
