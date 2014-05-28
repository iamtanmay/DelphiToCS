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

unit MessagableExecutable;


interface


uses
    Classes,
    ThreadClasses,
    MessageQueue,
    LockHandle,
    Executable;

type
    TTerminateMessageInfo = class(TMessageInfo)
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create();
    end;

    TUnpauseMessageInfo = class(TMessageInfo)
    protected
        function GetMessageID(): integer; override;
    end;

    TPauseMessageInfo = class(TMessageInfo)
    protected
        function GetMessageID(): integer; override;
    end;

    TPerformMessageInfo = class(TMessageInfo)
    protected
        function GetMessageID(): integer; override;
    end;

    IMessagableExecutable = interface(IExecutable)
        ['{3224189B-DE72-47EF-BD93-65B2FE2AA05C}']
        procedure Quit();
        procedure Pause();
        procedure Unpause();
    end;

    TMessagableExecutable = class(TExecutable, IMessagableExecutable)
    private
        fOnPerform: TNotifyEvent;
        fOnQuit: TNotifyEvent;
        fOnPause: TNotifyEvent;
        fOnUnPause: TNotifyEvent;
        fInitCompleted: boolean;
        fTID: cardinal;
        function CreateMessageQueueItem(aMessageInfo: TMessageInfo; aLock: TLock; aOwnsMessageInfo: boolean)
            : TMessageQueueItem;
        procedure WaitOnLockTillMessageHandled(aLock: TLock; aMaxWaitTime: cardinal);
        procedure HandleMessageWithoutRegistering(const aMessageInfo: TMessageInfo;
            const aOwnsMessageInfo: boolean);
        procedure RegisterMessageWithQueueItem(const aItem: TMessageQueueItem);
        procedure RegisterMessageWithLock(aMessageInfo: TMessageInfo; aLock: TLock;
            aOwnsMessageInfo: boolean);
        procedure RegisterPause;
        procedure RegisterUnpause;
        procedure Perform();
        procedure HandleTerminate(aMessageInfo: TTerminateMessageInfo);
        function GetCurrentThreadID(): cardinal;
    protected
        fExecCompleted: boolean;
        fPaused: boolean;
        fMessageQueue: TMaskedMessageQueue;
        procedure RegisterPerform;
        procedure DoPerform; virtual;
        procedure Initialize(); virtual;
        procedure Finalize(); virtual;
        procedure SetPauseMask(); virtual;
        procedure SetQuitMask(); virtual;
        procedure SetOriginalMask(); virtual;
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); virtual;
        procedure DoHandleTerminate(aMessageInfo: TTerminateMessageInfo); virtual;
        procedure HandlePaused(aPauseMessageInfo: TPauseMessageInfo); virtual;
        procedure HandleUnPaused(aUnPauseMessageInfo: TUnPauseMessageInfo); virtual;
    public
        constructor Create();
        destructor Destroy(); override;

        procedure RegisterMessageAndWait(aMessageInfo: TMessageInfo; aOwnsMessageInfo: boolean;
            aMaxWaitTime: cardinal); overload;
        procedure RegisterMessageAndWait(aMessageInfo: TMessageInfo); overload;
        procedure RegisterMessageAndLeave(aMessageInfo: TMessageInfo);

        procedure Execute(); override;
        procedure Terminate(); override;
        procedure Quit();
        procedure Pause();
        procedure Unpause();
        procedure Reperform();
        property Paused: boolean read fPaused;
        property OnPause: TNotifyEvent read fOnPause write fOnPause;
        property OnUnPause: TNotifyEvent read fOnUnPause write fOnUnPause;
        property OnPerform: TNotifyEvent read fOnPerform write fOnPerform;
        property OnQuit: TNotifyEvent read fOnQuit write fOnQuit;
    end;


implementation


uses
    SysUtils,
    ThreadRegistry;

{ TTerminateMessageInfo }

constructor TTerminateMessageInfo.Create();
begin
    inherited Create();
end;

function TTerminateMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_EXECUTABLE_TERMINATE;
end;

{ TUnpauseMessageInfo }

function TUnpauseMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_EXECUTABLE_UNPAUSE;
end;

{ TPauseMessageInfo }

function TPauseMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_EXECUTABLE_PAUSE;
end;

{ TPerformMessageInfo }

function TPerformMessageInfo.GetMessageID: integer;
begin
    result := INT_MESSAGE_EXECUTABLE_PERFORM;
end;

{ TMessagableExecutable }

constructor TMessagableExecutable.Create();
begin
    inherited Create();
    fMessageQueue := TMaskedMessageQueue.Create();
    fPaused := true;
    SetOriginalMask();
    SetPauseMask();
    fOnPerform := nil;
    fOnQuit := nil;
    fOnPause := nil;
    fOnUnPause := nil;
end;

destructor TMessagableExecutable.Destroy();
begin
    fMessageQueue.Free;
    inherited;
end;

procedure TMessagableExecutable.SetOriginalMask();
begin
    fMessageQueue.SetMask([INT_MESSAGE_EXECUTABLE_TERMINATE, INT_MESSAGE_EXECUTABLE_PAUSE,
        INT_MESSAGE_EXECUTABLE_PERFORM]);
end;

procedure TMessagableExecutable.SetPauseMask();
begin
    fMessageQueue.SetMaskTemp([INT_MESSAGE_EXECUTABLE_TERMINATE, INT_MESSAGE_EXECUTABLE_UNPAUSE]);
end;

procedure TMessagableExecutable.SetQuitMask();
begin
    fMessageQueue.SetMask([INT_MESSAGE_QUIT]);
end;

procedure TMessagableExecutable.Terminate();
begin
    RegisterMessageAndLeave(TTerminateMessageInfo.Create())
end;

procedure TMessagableExecutable.Quit();
begin
    if Assigned(fOnQuit) then
        fOnQuit(self);
    SetQuitMask();
    RegisterMessageAndLeave(TQuitMessageInfo.Create());
end;

procedure TMessagableExecutable.Pause();
begin
    if self.fPaused then
        EXIT;
    if Assigned(fOnPause) then
        fOnPause(self);
    RegisterPause();
end;

procedure TMessagableExecutable.Unpause();
begin
    if not self.fPaused then
        EXIT;
    if Assigned(fOnUnPause) then
        fOnUnPause(self);
    RegisterUnpause();
end;

procedure TMessagableExecutable.Reperform();
begin
    RegisterPerform();
end;

procedure TMessagableExecutable.RegisterMessageWithQueueItem(const aItem: TMessageQueueItem);
begin
    fMessageQueue.EnqueueMessage(aItem);
end;

function TMessagableExecutable.CreateMessageQueueItem(aMessageInfo: TMessageInfo; aLock: TLock;
    aOwnsMessageInfo: boolean): TMessageQueueItem;
begin
    result := TMessageQueueItem.Create(aMessageInfo, aLock, false, self.MessageProc, aOwnsMessageInfo);
end;

procedure TMessagableExecutable.RegisterMessageWithLock(aMessageInfo: TMessageInfo; aLock: TLock;
    aOwnsMessageInfo: boolean);
begin
    RegisterMessageWithQueueItem(CreateMessageQueueItem(aMessageInfo, aLock, aOwnsMessageInfo));
end;

procedure TMessagableExecutable.WaitOnLockTillMessageHandled(aLock: TLock; aMaxWaitTime: cardinal);
begin
    aLock.WaitForLock(aMaxWaitTime);
end;

procedure TMessagableExecutable.HandleMessageWithoutRegistering(const aMessageInfo: TMessageInfo;
    const aOwnsMessageInfo: boolean);
var
    xAccept: boolean;
    xMessageQueueItem: TMessageQueueItem;
begin
    gmDebugLog('Handle Message without Queuing - START');
    xMessageQueueItem := CreateMessageQueueItem(aMessageInfo, nil, aOwnsMessageInfo);
    try
        self.MessageProc(xAccept, xMessageQueueItem);
    finally
        FreeAndNil(xMessageQueueItem);
    end;
    gmDebugLog('Handle Message without Queuing - FINISH');
end;

function TMessagableExecutable.GetCurrentThreadID(): cardinal;
begin
    result := TThreadRegistry.Instance.FindCurrentThreadID();
end;

procedure TMessagableExecutable.RegisterMessageAndWait(aMessageInfo: TMessageInfo; aOwnsMessageInfo: boolean;
    aMaxWaitTime: cardinal);
var
    xLock: TLock;
    xCurrentTID: cardinal;
begin
    xCurrentTID := GetCurrentThreadID;
    aMessageInfo.CallingThreadID := xCurrentTID;
    // if the current thread is the the thread that is executing the TMessagableExecutable Dont register the message, handle it directly.
    // otherwise deadlock because I will be waiting for myself.
    if fTID = xCurrentTID then
    begin
        HandleMessageWithoutRegistering(aMessageInfo, aOwnsMessageInfo);
        EXIT;
    end;

    aMessageInfo.CallingThreadSysHandleID := cCurrentThreadPsuedoSysHandleID;
    xLock := TSimpleLock.Create(true, false, false);
    try
        RegisterMessageWithLock(aMessageInfo, xLock, aOwnsMessageInfo);
        WaitOnLockTillMessageHandled(xLock, aMaxWaitTime);
    finally
        xLock.Free;
    end
end;

procedure TMessagableExecutable.RegisterMessageAndWait(aMessageInfo: TMessageInfo);
begin
    RegisterMessageAndWait(aMessageInfo, true, INFINITE);
end;

procedure TMessagableExecutable.RegisterMessageAndLeave(aMessageInfo: TMessageInfo);
begin
    aMessageInfo.CallingThreadID := GetCurrentThreadID;
    RegisterMessageWithLock(aMessageInfo, nil, true);
end;

procedure TMessagableExecutable.RegisterPerform();
begin
    RegisterMessageAndLeave(TPerformMessageInfo.Create());
end;

procedure TMessagableExecutable.RegisterUnPause();
begin
    RegisterMessageAndLeave(TUnpauseMessageInfo.Create());
end;

procedure TMessagableExecutable.RegisterPause();
begin
    RegisterMessageAndLeave(TPauseMessageInfo.Create());
end;

procedure TMessagableExecutable.HandleUnPaused(aUnPauseMessageInfo: TUnPauseMessageInfo);
begin
    fMessageQueue.RestoreMask();
    fMessageQueue.DequeueMessageByType(INT_MESSAGE_EXECUTABLE_PERFORM);
    RegisterPerform;
    fPaused := false;
end;

procedure TMessagableExecutable.HandlePaused(aPauseMessageInfo: TPauseMessageInfo);
begin
    SetPauseMask();
    fMessageQueue.DequeueMessageByType(INT_MESSAGE_EXECUTABLE_PERFORM);
    fPaused := true;
end;

procedure TMessagableExecutable.DoHandleTerminate(aMessageInfo: TTerminateMessageInfo);
begin

end;

procedure TMessagableExecutable.HandleTerminate(aMessageInfo: TTerminateMessageInfo);
begin
    DoHandleTerminate(aMessageInfo);
    Quit();
end;

procedure TMessagableExecutable.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
    case aMessage.MessageType of
        INT_MESSAGE_EXECUTABLE_UNPAUSE:
            begin
                HandleUnPaused(aMessage.MessageInfo as TUnPauseMessageInfo);
            end;

        INT_MESSAGE_EXECUTABLE_PAUSE:
            begin
                HandlePaused(aMessage.MessageInfo as TPauseMessageInfo);

            end;

        INT_MESSAGE_EXECUTABLE_PERFORM:
            begin
                Perform();
            end;

        INT_MESSAGE_EXECUTABLE_TERMINATE:
            begin
                HandleTerminate(aMessage.MessageInfo as TTerminateMessageInfo);
            end;
    end;
end;

procedure TMessagableExecutable.Initialize();
begin
end;

procedure TMessagableExecutable.Finalize();
begin
end;

procedure TMessagableExecutable.Execute();
begin
    try
        fTID := TThreadRegistry.Instance.FindCurrentThreadID();
        fInitCompleted := false;
        fExecCompleted := false;
        try
            Initialize();
            fInitCompleted := true;
            fMessageQueue.Execute();
            fExecCompleted := true // set completed to true if there were no exceptions
        finally
            Finalize(); // Warning: we do finalize even if exception occured in Initialize
        end;
    except
        on E: Exception do
        begin
            raise Exception.Create('Execute -->' + E.Message);
        end;
    end;
end;

procedure TMessagableExecutable.DoPerform;
begin
end;

procedure TMessagableExecutable.Perform;
begin
    DoPerform();
    if Assigned(fOnPerform) then
        fOnPerform(self);
end;


end.
