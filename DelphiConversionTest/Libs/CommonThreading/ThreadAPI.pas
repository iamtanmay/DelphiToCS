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

unit ThreadAPI;


interface


uses
    ThreadClasses,
    ThreadingProcessHandler,
    Executable,
    ProcessRegistry,
    TrackingSemaphore;

type
    TThreadAPI = class
    private
        class function ThreadingProcessHandler(): TThreadingProcessHandler;
        class procedure DoOnThreadFinished(const aTID: cardinal);
    public
        // API
        class procedure Initialize(aShowThreadExceptionCallback: TThreadingShowThreadExceptionCallback;
            aLogCallback: TThreadingLogCallback);
        class procedure Finalize();
        class function IsAPIAvailable(): boolean;
        class function CreateThread(const aDescription: string; const aCreateSuspended, aCloseHandleOnFinish,
            aCanSuspendSafe: boolean; const aExecutable: IExecutable): TSysHandleID;

        class function CreateProcess(const aDescription: string; const aProcessHandler: IProcessHandler;
            const aAddressSpace: TAddressSpace; const aSourceDataName: string;
            const aIsSystemProcess: boolean; const aIsSimulated: boolean): TSysHandleID;

        class procedure StartThread(const aSysHandleID: TSysHandleID);
        class function JoinThread(const aSysHandleID: TSysHandleID; const aMaxWaitTime: cardinal): boolean;

        class procedure ExitProcess();
        class function WaitTillAllProcessesSuspended(const aMaxWaitTime: cardinal): boolean;
        class function WaitTillAllProcessesExited(const aMaxWaitTime: cardinal): boolean;
        class procedure SuspendAllProcesses();
        class procedure ResumeAllProcesses();

        class function CreateSemaphore(aMaxCount: integer): TTrackingSemaphore;
        class function GetCurrentThreadID(): cardinal;
        class function GetCurrentThreadDescription(): string;
        class function GetCurrentProcessDescription: string;
        class function GetCurrentThreadPriority(): integer; virtual;
        class function GetCurrentThreadImage: TThreadImage;
        class function GetCurrentProcess(): TProcess;
        class procedure WaitIfInterrupt();
        class procedure TerminateSecondaryThreads();
        class procedure WaitTillSecondaryThreadsTerminate(const aMaxWaitTries: integer);
        class function GetCurrentSourceDataName(): string;
        class function GetCurrentExeDataPathName(): string;
        class function GetIsCurrentThreadSimulated(): boolean;
        class procedure SetCurrentContractorThreadID(const aContractorThreadID: cardinal);
        class procedure ResetCurrentContractorThreadID();
        class function GetThreadImage(const aSysHandleID: TSysHandleID): TThreadImage;
        class procedure ChangeCurrentProcessSimulated(const aIsSimulated: boolean);

        class function CreateLock(): TSysHandleID;
        class procedure DestroyLock(const aSysHandleID: TSysHandleID);
        class procedure WaitForLock(const aSysHandleID: TSysHandleID);
        class procedure OpenLock(const aSysHandleID: TSysHandleID);

        class function DuplicateSysHandle(const aOriginalSysHandleID: TSysHandleID;
            out oNewHandleID: TSysHandleID): boolean;
        class function OpenThread(const aTID: cardinal): TSysHandleID;
        class procedure CloseSysHandle(const aSysHandleID: TSysHandleID);
        class procedure SetError();
    end;


implementation


uses
    ThreadingManager;

{ TThreadAPI }

class procedure TThreadAPI.Initialize(aShowThreadExceptionCallback: TThreadingShowThreadExceptionCallback;
    aLogCallback: TThreadingLogCallback);
begin
    TThreadingManager.CreateInstance();
    TThreadingProcessHandler.CreateInstance;
    TThreadingManager.Instance.ShowThreadExceptionCallback := aShowThreadExceptionCallback;
    TThreadingManager.Instance.LogCallback := aLogCallback;
    TThreadingManager.Instance.ThreadFinishedCallback := self.DoOnThreadFinished;
    TThreadingManager.Instance.StartThreadingProcess(TThreadingProcessHandler.Instance);
end;

class procedure TThreadAPI.Finalize();
begin
    TThreadingManager.Instance.ShowThreadExceptionCallback := nil;
    self.ThreadingProcessHandler.WaitTillAllProcessesExited(true, 5000);
    self.ThreadingProcessHandler.Terminate();
    TThreadingManager.Instance.CloseThreadingProcess;
    TThreadingProcessHandler.DestroyInstance;
    TThreadingManager.DestroyInstance();
end;

class function TThreadAPI.ThreadingProcessHandler(): TThreadingProcessHandler;
begin
    result := nil;
    if TThreadingManager.Instance = nil then
        EXIT;
    result := TThreadingProcessHandler.Instance;
end;

class procedure TThreadAPI.DoOnThreadFinished(const aTID: cardinal);
begin
    self.ThreadingProcessHandler.ThreadFinished(aTID);
end;

class function TThreadAPI.IsAPIAvailable(): boolean;
begin
    result := (ThreadingProcessHandler <> nil);
end;

class function TThreadAPI.CreateThread(const aDescription: string;
    const aCreateSuspended, aCloseHandleOnFinish, aCanSuspendSafe: boolean; const aExecutable: IExecutable)
    : TSysHandleID;
begin
    result := self.ThreadingProcessHandler.CreateThread(aDescription, aCreateSuspended, aCloseHandleOnFinish,
        aCanSuspendSafe, aExecutable);
end;

class function TThreadAPI.CreateProcess(const aDescription: string; const aProcessHandler: IProcessHandler;
    const aAddressSpace: TAddressSpace; const aSourceDataName: string; const aIsSystemProcess: boolean;
    const aIsSimulated: boolean): TSysHandleID;
begin
    result := self.ThreadingProcessHandler.CreateProcess(aDescription, aProcessHandler, aAddressSpace,
        aSourceDataName, aIsSystemProcess, aIsSimulated);
end;

class procedure TThreadAPI.StartThread(const aSysHandleID: TSysHandleID);
begin
    self.ThreadingProcessHandler.StartThread(aSysHandleID);
end;

class function TThreadAPI.JoinThread(const aSysHandleID: TSysHandleID; const aMaxWaitTime: cardinal): boolean;
begin
    result := self.ThreadingProcessHandler.JoinThread(aSysHandleID, aMaxWaitTime);
end;

class procedure TThreadAPI.SuspendAllProcesses();
begin
    self.ThreadingProcessHandler.SuspendAllProcesses();
end;

class procedure TThreadAPI.ResumeAllProcesses();
begin
    self.ThreadingProcessHandler.ResumeAllProcesses();
end;

class function TThreadAPI.GetCurrentThreadImage: TThreadImage;
begin
    result := self.ThreadingProcessHandler.GetCurrentThreadImage();
end;

class function TThreadAPI.GetCurrentThreadID: cardinal;
begin
    result := self.ThreadingProcessHandler.GetCurrentThreadID;
end;

class function TThreadAPI.GetCurrentThreadDescription: string;
begin
    result := self.ThreadingProcessHandler.GetCurrentThreadDescription;
end;

class function TThreadAPI.GetCurrentSourceDataName(): string;
begin
    result := self.ThreadingProcessHandler.GetCurrentSourceDataName();
end;

class function TThreadAPI.GetCurrentExeDataPathName(): string;
begin
    // NOT IMPLEMENTED CORRECTLY: This used to return the name of the current run table. For now just return name of main method
    result := GetCurrentSourceDataName();
end;

class function TThreadAPI.GetCurrentProcessDescription: string;
begin
    result := self.ThreadingProcessHandler.GetCurrentProcessDescription();
end;

class function TThreadAPI.GetCurrentThreadPriority: integer;
begin
    result := GetCurrentThreadID();
end;

class function TThreadAPI.GetCurrentProcess(): TProcess;
begin
    result := self.ThreadingProcessHandler.GetCurrentProcess();
end;

class procedure TThreadAPI.ExitProcess;
begin
    self.ThreadingProcessHandler.ExitProcess();
end;

class function TThreadAPI.CreateSemaphore(aMaxCount: integer): TTrackingSemaphore;
begin
    result := TTrackingSemaphore.Create(aMaxCount);
end;

class procedure TThreadAPI.WaitIfInterrupt();
var
    xCurrentThreadImage: TThreadImage;
begin
    xCurrentThreadImage := GetCurrentThreadImage();
    if (not Assigned(xCurrentThreadImage)) or (not(xCurrentThreadImage.Thread is TExecThread)) then
        EXIT;
    xCurrentThreadImage.Thread.CheckSuspend();
end;

class function TThreadAPI.WaitTillAllProcessesSuspended(const aMaxWaitTime: cardinal): boolean;
begin
    EXIT(self.ThreadingProcessHandler.WaitTillAllProcessesSuspended(aMaxWaitTime));
end;

class procedure TThreadAPI.TerminateSecondaryThreads;
begin
    self.ThreadingProcessHandler.TerminateSecondaryThreads();
end;

class procedure TThreadAPI.WaitTillSecondaryThreadsTerminate(const aMaxWaitTries: integer);
begin
    self.ThreadingProcessHandler.WaitTillSecondaryThreadsTerminate(aMaxWaitTries);
end;

class procedure TThreadAPI.CloseSysHandle(const aSysHandleID: TSysHandleID);
begin
    self.ThreadingProcessHandler.CloseSysHandle(aSysHandleID);
end;

class function TThreadAPI.CreateLock(): TSysHandleID;
begin
    result := self.ThreadingProcessHandler.CreateLock();
end;

class procedure TThreadAPI.DestroyLock(const aSysHandleID: TSysHandleID);
begin
    self.ThreadingProcessHandler.DestroyLock(aSysHandleID);
end;

class procedure TThreadAPI.WaitForLock(const aSysHandleID: TSysHandleID);
begin
    self.ThreadingProcessHandler.WaitForLock(aSysHandleID, INFINITE);
end;

class procedure TThreadAPI.OpenLock(const aSysHandleID: TSysHandleID);
begin
    self.ThreadingProcessHandler.OpenLock(aSysHandleID);
end;

class procedure TThreadAPI.SetError();
begin
    self.ThreadingProcessHandler.SetError();
end;

class function TThreadAPI.WaitTillAllProcessesExited(const aMaxWaitTime: cardinal): boolean;
begin
    result := self.ThreadingProcessHandler.WaitTillAllProcessesExited(false, aMaxWaitTime);
end;

class function TThreadAPI.GetIsCurrentThreadSimulated(): boolean;
begin
    result := self.ThreadingProcessHandler.GetIsCurrentThreadSimulated();
end;

class procedure TThreadAPI.SetCurrentContractorThreadID(const aContractorThreadID: cardinal);
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := GetCurrentThreadImage();
    xThreadImage.SetContractorThreadID(aContractorThreadID);
end;

class procedure TThreadAPI.ResetCurrentContractorThreadID();
var
    xThreadImage: TThreadImage;
begin
    xThreadImage := GetCurrentThreadImage();
    xThreadImage.ResetContractorThreadID();
end;

class function TThreadAPI.DuplicateSysHandle(const aOriginalSysHandleID: TSysHandleID;
    out oNewHandleID: TSysHandleID): boolean;
begin
    result := self.ThreadingProcessHandler.DuplicateSysHandle(aOriginalSysHandleID, oNewHandleID);
end;

class function TThreadAPI.OpenThread(const aTID: cardinal): TSysHandleID;
begin
    result := self.ThreadingProcessHandler.OpenThread(aTID);
end;

class function TThreadAPI.GetThreadImage(const aSysHandleID: TSysHandleID): TThreadImage;
begin
    result := self.ThreadingProcessHandler.GetThreadImage(aSysHandleID);
end;

class procedure TThreadAPI.ChangeCurrentProcessSimulated(const aIsSimulated: boolean);
begin
    self.ThreadingProcessHandler.ChangeCurrentProcessSimulated(aIsSimulated);
end;


end.
