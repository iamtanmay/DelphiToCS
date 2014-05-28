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

unit MessageInfo;


interface


uses
    ThreadClasses,
    Executable,
    ProcessRegistry;

type
    TSecondaryThreadRemovedMessageInfo = class(TMessageInfo)
    private
        fTID: cardinal;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aTID: cardinal);
        property TID: cardinal read fTID;
    end;

    TSecondaryThreadAddedMessageInfo = class(TMessageInfo)
    private
        fTID: cardinal;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aTID: cardinal);
        property TID: cardinal read fTID;
    end;

    TCreateThreadMessageInfo = class(TMessageInfo)
    protected
        fCreateSuspended: boolean;
        fCloseHandleOnFinish: boolean;
        fCanSuspendSafe: boolean;
        fDescription: string;
        fExecutable: IExecutable;
        fResultSysHandleID: TSysHandleID;
        function GetMessageID(): integer; override;
    public
        constructor Create(const aDescription: string; const aCreateSuspended, aCloseHandleOnFinish,
            aCanSuspendSafe: boolean; const aExecutable: IExecutable);
        property Description: string read fDescription;
        property CreateSuspended: boolean read fCreateSuspended;
        property CloseHandleOnFinish: boolean read fCloseHandleOnFinish;
        property CanSuspendSafe: boolean read fCanSuspendSafe;
        property Executable: IExecutable read fExecutable;
        property ResultSysHandleID: TSysHandleID read fResultSysHandleID write fResultSysHandleID;
    end;

    TAddProcessMessageInfo = class(TMessageInfo)
    protected
        fDescription: string;
        fProcessHandler: IProcessHandler;
        fAddressSpace: TAddressSpace;
        fSourceDataName: string;
        fIsSystemProcess: boolean;
        fIsSimulated: boolean;
        fResultProcessSysHandleID: TSysHandleID;
        function GetMessageID(): integer; override;
    public
        constructor Create(const aDescription: string; const aProcessHandler: IProcessHandler;
            const aAddressSpace: TAddressSpace; const aSourceDataName: string;
            const aIsSystemProcess: boolean; const aIsSimulated: boolean);
        property Description: string read fDescription;
        property ProcessHandler: IProcessHandler read fProcessHandler;
        property AddressSpace: TAddressSpace read fAddressSpace;
        property SourceDataName: string read fSourceDataName;
        property IsSystemProcess: boolean read fIsSystemProcess;
        property IsSimulated: boolean read fIsSimulated;
        property ResultProcessSysHandleID: TSysHandleID read fResultProcessSysHandleID
            write fResultProcessSysHandleID;
    end;

    TStartThreadMessageInfo = class(TMessageInfo)
    protected
        fSysHandleID: TSysHandleID;
        function GetMessageID(): integer; override;
    public
        constructor Create(const aSysHandleID: TSysHandleID);
        property SysHandleID: TSysHandleID read fSysHandleID;
    end;

    TThreadFinishedMessageInfo = class(TMessageInfo)
    private
        fTID: cardinal;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aTID: cardinal);
        property TID: cardinal read fTID;
    end;

    TOpenThreadMessageInfo = class(TMessageInfo)
    private
        fTID: cardinal;
        fResultSysHandleID: TSysHandleID;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aTID: cardinal);
        property TID: cardinal read fTID;
        property ResultSysHandleID: TSysHandleID read fResultSysHandleID write fResultSysHandleID;
    end;

    TExitProcessMessageInfo = class(TMessageInfo)
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create();
    end;

    TOpenProcessMessageInfo = class(TMessageInfo)
    private
        fPID: cardinal;
        fResultSysHandleID: TSysHandleID;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aPID: cardinal);
        property PID: cardinal read fPID;
        property ResultSysHandleID: TSysHandleID read fResultSysHandleID write fResultSysHandleID;
    end;

    TSetErrorMessageInfo = class(TMessageInfo)
    protected
        function GetMessageID(): integer; override;
    end;

    TSuspendAllProcessesMessageInfo = class(TMessageInfo)
    protected
        function GetMessageID(): integer; override;
    end;

    TResumeAllProcessesMessageInfo = class(TMessageInfo)
    protected
        function GetMessageID(): integer; override;
    end;

    TWaitTillAllProcessesSuspendedMessageInfo = class(TMessageInfo)
    protected
        fResultSuccess: boolean;
        function GetMessageID(): integer; override;
    public
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TCheckAllProcessesExitedMessageInfo = class(TMessageInfo)
    protected
        fCheckSystemProcesses: boolean;
        fResultAllExited: boolean;
        fResultActiveProcessSysHandleID: TSysHandleID;
        function GetMessageID(): integer; override;
    public
        constructor Create(const aCheckSystemProcesses: boolean);
        property CheckSystemProcesses: boolean read fCheckSystemProcesses;
        property ResultAllExited: boolean read fResultAllExited write fResultAllExited;
        property ResultActiveProcessSysHandleID: TSysHandleID read fResultActiveProcessSysHandleID
            write fResultActiveProcessSysHandleID;
    end;

    TTerminateSecondaryThreadsMessageInfo = class(TMessageInfo)
    protected
        fResultSuccess: boolean;
        function GetMessageID(): integer; override;
    public
        constructor Create();
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TWaitTillSecondaryThreadsTerminateMessageInfo = class(TMessageInfo)
    protected
        fResultSuccess: boolean;
        function GetMessageID(): integer; override;
    public
        constructor Create();
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TGetThreadImageMessageInfo = class(TMessageInfo)
    protected
        fResultThreadImage: TThreadImage;
        function GetMessageID(): integer; override;
    public
        constructor Create();
        property ResultThreadImage: TThreadImage read fResultThreadImage write fResultThreadImage;
    end;

    TGetCurrentProcessMessageInfo = class(TMessageInfo)
    protected
        fResultProcess: TProcess;
        function GetMessageID(): integer; override;
    public
        constructor Create();
        property ResultProcess: TProcess read fResultProcess write fResultProcess;
    end;

    TCreateLockMessageInfo = class(TMessageInfo)
    protected
        fResultSysHandleID: TSysHandleID;
        function GetMessageID(): integer; override;
    public
        constructor Create();
        property ResultSysHandleID: TSysHandleID read fResultSysHandleID write fResultSysHandleID;
    end;

    TDestroyLockMessageInfo = class(TMessageInfo)
    protected
        fSysHandleID: TSysHandleID;
        function GetMessageID(): integer; override;
    public
        constructor Create(const aSysHandleID: TSysHandleID);
        property SysHandleID: TSysHandleID read fSysHandleID;
    end;

    TGetSysObjectDataMessageInfo = class(TMessageInfo)
    protected
        fSysHandleID: TSysHandleID;
        fResultObject: TObject;
        function GetMessageID(): integer; override;
    public
        constructor Create(const aSysHandleID: TSysHandleID);
        property SysHandleID: TSysHandleID read fSysHandleID;
        property ResultObject: TObject read fResultObject write fResultObject;
    end;

    TAtleastOneNonSimulatedProcessExistsMessageInfo = class(TMessageInfo)
    protected
        fResultNonSimulatedProcessExists: boolean;
        function GetMessageID(): integer; override;
    public
        constructor Create();
        property ResultNonSimulatedProcessExists: boolean read fResultNonSimulatedProcessExists
            write fResultNonSimulatedProcessExists;
    end;

    TDuplicateSysHandleMessageInfo = class(TMessageInfo)
    protected
        fOriginalSysHandleID: TSysHandleID;
        fResultOriginalSysHandleFound: boolean;
        fResultSysHandleID: TSysHandleID;
        function GetMessageID(): integer; override;
    public
        constructor Create(const aOriginalSysHandleID: TSysHandleID);
        property OriginalSysHandleID: TSysHandleID read fOriginalSysHandleID;
        property ResultOriginalSysHandleFound: boolean read fResultOriginalSysHandleFound
            write fResultOriginalSysHandleFound;
        property ResultSysHandleID: TSysHandleID read fResultSysHandleID write fResultSysHandleID;
    end;

    TCloseSysHandleMessageInfo = class(TMessageInfo)
    protected
        fSysHandleID: TSysHandleID;
        function GetMessageID(): integer; override;
    public
        constructor Create(const aSysHandleID: TSysHandleID);
        property SysHandleID: TSysHandleID read fSysHandleID;
    end;


implementation


{ TSetErrorMessageInfo }

function TSetErrorMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_ERROR_SET;
end;

{ TCreateThreadMessageInfo }

constructor TCreateThreadMessageInfo.Create(const aDescription: string;
    const aCreateSuspended, aCloseHandleOnFinish, aCanSuspendSafe: boolean; const aExecutable: IExecutable);
begin
    inherited Create();
    fDescription := aDescription;
    fCreateSuspended := aCreateSuspended;
    fCloseHandleOnFinish := aCloseHandleOnFinish;
    fCanSuspendSafe := aCanSuspendSafe;
    fExecutable := aExecutable;
    fResultSysHandleID := cHandleIDNone;
end;

function TCreateThreadMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_THREAD_CREATE;
end;
{ TThreadFinishedMessageInfo }

constructor TThreadFinishedMessageInfo.Create(const aTID: cardinal);
begin
    inherited Create();
    fTID := aTID;
end;

function TThreadFinishedMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_THREAD_FINISHED;
end;

{ TStartThreadMessageInfo }

constructor TStartThreadMessageInfo.Create(const aSysHandleID: TSysHandleID);
begin
    inherited Create();
    fSysHandleID := aSysHandleID;
end;

function TStartThreadMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_THREAD_START;
end;

{ TOpenThreadMessageInfo }

constructor TOpenThreadMessageInfo.Create(const aTID: cardinal);
begin
    inherited Create();
    fTID := aTID;
end;

function TOpenThreadMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_THREAD_OPEN;
end;

{ TSecondaryThreadRemovedMessageInfo }

constructor TSecondaryThreadRemovedMessageInfo.Create(const aTID: cardinal);
begin
    inherited Create();
    fTID := aTID;
end;

function TSecondaryThreadRemovedMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_PROCESSHANDLER_SECONDARYTHREADREMOVED;
end;

{ TSecondaryThreadAddedMessageInfo }

constructor TSecondaryThreadAddedMessageInfo.Create(const aTID: cardinal);
begin
    inherited Create();
    fTID := aTID;
end;

function TSecondaryThreadAddedMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_PROCESSHANDLER_SECONDARYTHREADADDED;
end;

{ TCreateLockMessageInfo }

constructor TCreateLockMessageInfo.Create;
begin
    inherited;
    fResultSysHandleID := 0;
end;

function TCreateLockMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_LOCK_CREATE;
end;

{ TDestroyLockMessageInfo }

constructor TDestroyLockMessageInfo.Create(const aSysHandleID: TSysHandleID);
begin
    inherited Create();
    fSysHandleID := aSysHandleID;
end;

function TDestroyLockMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_LOCK_DESTROY;
end;

{ TGetSysObjectDataMessageInfo }

constructor TGetSysObjectDataMessageInfo.Create(const aSysHandleID: TSysHandleID);
begin
    inherited Create();
    fSysHandleID := aSysHandleID;
    fResultObject := nil;
end;

function TGetSysObjectDataMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_LOCK_GET;
end;

{ TOpenProcessMessageInfo }

constructor TOpenProcessMessageInfo.Create(const aPID: cardinal);
begin
    inherited Create();
    fPID := aPID;
end;

function TOpenProcessMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_PROCESS_OPEN;
end;

{ TRemoveProcessMessageInfo }

constructor TExitProcessMessageInfo.Create();
begin
    inherited Create();
end;

function TExitProcessMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_PROCESS_EXIT;
end;

{ TAddProcessMessageInfo }

constructor TAddProcessMessageInfo.Create(const aDescription: string; const aProcessHandler: IProcessHandler;
    const aAddressSpace: TAddressSpace; const aSourceDataName: string; const aIsSystemProcess: boolean;
    const aIsSimulated: boolean);
begin
    inherited Create();
    fDescription := aDescription;
    fProcessHandler := aProcessHandler;
    fAddressSpace := aAddressSpace;
    fSourceDataName := aSourceDataName;
    fIsSystemProcess := aIsSystemProcess;
    fIsSimulated := aIsSimulated;
    fResultProcessSysHandleID := cHandleIDNone;
end;

function TAddProcessMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_PROCESS_ADD;
end;

{ TSuspendAllProcessesMessageInfo }

function TSuspendAllProcessesMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_PROCESSES_SUSPENDALL;
end;

{ TResumeAllProcessesMessageInfo }

function TResumeAllProcessesMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_PROCESSES_RESUMEALL;
end;

{ TWaitTillAllProcessesSuspendedMessageInfo }

function TWaitTillAllProcessesSuspendedMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_PROCESSES_WAITSUSPENDEDALL;
end;

{ TGetThreadImageMessageInfo }

constructor TGetThreadImageMessageInfo.Create();
begin
    inherited Create();
    fResultThreadImage := nil;
end;

function TGetThreadImageMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_THREAD_GETIMAGE;
end;

{ TGetCurrentProcessMessageInfo }

constructor TGetCurrentProcessMessageInfo.Create();
begin
    inherited Create();
    fResultProcess := nil;
end;

function TGetCurrentProcessMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_PROCESS_GETCURRENT;
end;

{ TWaitTillSecondaryThreadsTerminateMessageInfo }

constructor TWaitTillSecondaryThreadsTerminateMessageInfo.Create();
begin
    inherited Create();
    fResultSuccess := false;
end;

function TWaitTillSecondaryThreadsTerminateMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_PROCESS_WAITTILLTHREADSTERMINATE;
end;

{ TTerminateSecondaryThreadsMessageInfo }

constructor TTerminateSecondaryThreadsMessageInfo.Create();
begin
    inherited Create();
    fResultSuccess := false;
end;

function TTerminateSecondaryThreadsMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_PROCESS_TERMINATETHREADS;
end;

{ TCheckAllProcessesExited }

constructor TCheckAllProcessesExitedMessageInfo.Create(const aCheckSystemProcesses: boolean);
begin
    inherited Create();
    fCheckSystemProcesses := aCheckSystemProcesses;
    fResultAllExited := false;
    fResultActiveProcessSysHandleID := 0;
end;

function TCheckAllProcessesExitedMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_PROCESSES_CHECKALLEXITED;
end;

{ TAtleastOneNonSimulatedProcessExistsMessageInfo }

constructor TAtleastOneNonSimulatedProcessExistsMessageInfo.Create;
begin
    inherited Create();
    fResultNonSimulatedProcessExists := false;
end;

function TAtleastOneNonSimulatedProcessExistsMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_PROCESSES_NONSIMPROCEXISTS;
end;

{ TDuplicateSysHandleMessageInfo }

constructor TDuplicateSysHandleMessageInfo.Create(const aOriginalSysHandleID: TSysHandleID);
begin
    inherited Create();
    fOriginalSysHandleID := aOriginalSysHandleID;
end;

function TDuplicateSysHandleMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_SYS_DUPLICATEHANDLE;
end;

{ TCloseSysHandleMessageInfo }

constructor TCloseSysHandleMessageInfo.Create(const aSysHandleID: TSysHandleID);
begin
    inherited Create();
    fSysHandleID := aSysHandleID;
end;

function TCloseSysHandleMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_THREADAPI_SYS_CLOSEHANDLE;
end;


end.
