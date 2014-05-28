{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  03.07.08 wl                                         TN4157
  19.01.12 wl  TInterruptRequestEvent                 TN5759.2 neuer Parameter IgnoreInterruptStartError
  01.03.12 wl                                TN5822   uses geändert
  27.03.13 wl                                TN6045   uses geändert
  15.08.13 wl                                TN6223   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit InterruptMonitor;


interface


uses
    GeneralTypes,
    ThreadClasses,
    MessageQueue,
    LockHandle,
    Variants,
    Executable;

type

    TInterruptRequestEvent = function(const aInterruptText: string; aInterruptRoutine: TInterruptRoutineEvent;
        aIRArgs: TInterruptRoutineArgs; var vIRResult: TInterruptRoutineResult; aWaitTillHandled: boolean;
        aWaitTillHandledLock: TLock; const aIgnoreInterruptStartError: boolean = false): boolean of object;
    TInterruptCheckEvent = procedure(aSender: TObject; var vDoInterrupt: boolean) of object;

    TInterruptMonitor = class(TExecutable)
    protected
        fPollDelayInMSecs: integer;
        fMessageQueue: TMaskedMessageQueue;
        fInterruptText: string;
        fOnInterruptRequest: TInterruptRequestEvent;
        fOnInterruptRoutine: TInterruptRoutineEvent;
        fOnCheck: TInterruptCheckEvent;
        fWaitForInterruptLock: TLock;
        function IsInterruptRequired(): boolean;
        procedure RegisterMessage(aMessageType: integer);
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
        function RequestInterrupt(): boolean;
        procedure RegisterCheck;
        procedure RegisterStart;
        procedure RegisterStop;
        procedure StartMonitorThread(); virtual; abstract;
    public
        constructor Create(aPollDelayInMSecs: integer);
        destructor Destroy(); override;
        procedure Execute(); override;
        procedure StartMonitor();
        procedure Quit();
        procedure Pause();
        procedure Unpause();
        property OnInterruptRequest: TInterruptRequestEvent write fOnInterruptRequest;
        property OnInterruptRoutine: TInterruptRoutineEvent read fOnInterruptRoutine
            write fOnInterruptRoutine;
        property OnInterruptCheck: TInterruptCheckEvent write fOnCheck;
        property InterruptText: string read fInterruptText write fInterruptText;

    end;


implementation


uses
    SysUtils,
    MemoryClasses;

{ TInterruptMonitor }
const
    INT_MESSAGE_MONITOR_STOP = 1;
    INT_MESSAGE_MONITOR_START = 2;
    INT_MESSAGE_MONITOR_CHECK = 3;

constructor TInterruptMonitor.Create(aPollDelayInMSecs: integer);
begin
    inherited Create();
    fMessageQueue := TMaskedMessageQueue.Create();
    fMessageQueue.SetMask([INT_MESSAGE_MONITOR_START]);
    fPollDelayInMSecs := aPollDelayInMSecs;
    fWaitForInterruptLock := TSimpleLock.Create(false, false, false);
end;

destructor TInterruptMonitor.Destroy();
begin
    fMessageQueue.Free;
    fWaitForInterruptLock.Free;
    inherited;
end;

procedure TInterruptMonitor.StartMonitor();
begin
    StartMonitorThread();
end;

procedure TInterruptMonitor.Quit();
begin
    fMessageQueue.SetMask([INT_MESSAGE_QUIT]);
    RegisterMessage(INT_MESSAGE_QUIT);
end;

procedure TInterruptMonitor.Pause();
begin
    RegisterStop();
end;

procedure TInterruptMonitor.Unpause();
begin
    RegisterStart();
end;

function TInterruptMonitor.IsInterruptRequired(): boolean;
begin
    result := false;
    if Assigned(fOnCheck) then
        fOnCheck(self, result);
end;

function TInterruptMonitor.RequestInterrupt(): boolean;
var
    xDummy: TInterruptRoutineResult;
begin
    result := false;
    if Assigned(fOnInterruptRequest) then
    begin
        result := fOnInterruptRequest(fInterruptText, fOnInterruptRoutine, null, xDummy, true,
            fWaitForInterruptLock);
    end;
end;

procedure TInterruptMonitor.RegisterMessage(aMessageType: integer);
begin
    fMessageQueue.EnqueueMessage(TMessageQueueItem.Create(aMessageType, Integer(aMessageType), nil, false,
        self.MessageProc, TMsgArgs.EmptyArgs(), true));
end;

procedure TInterruptMonitor.RegisterCheck();
begin
    RegisterMessage(INT_MESSAGE_MONITOR_CHECK);
end;

procedure TInterruptMonitor.RegisterStart();
begin
    RegisterMessage(INT_MESSAGE_MONITOR_START);
end;

procedure TInterruptMonitor.RegisterStop();
begin
    RegisterMessage(INT_MESSAGE_MONITOR_STOP);
end;

procedure TInterruptMonitor.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
    // outputdebugstring( pchar( aMessage.Log ) );
    case aMessage.MessageType of
        INT_MESSAGE_MONITOR_START:
            begin
                fMessageQueue.SetMask([INT_MESSAGE_MONITOR_STOP, INT_MESSAGE_MONITOR_CHECK]);
                fMessageQueue.DequeueMessageByType(INT_MESSAGE_MONITOR_CHECK);
                RegisterCheck;
            end;

        INT_MESSAGE_MONITOR_STOP:
            begin
                fMessageQueue.SetMask([INT_MESSAGE_MONITOR_START]);
                fMessageQueue.DequeueMessageByType(INT_MESSAGE_MONITOR_CHECK);
            end;

        INT_MESSAGE_MONITOR_CHECK:
            begin
                if IsInterruptRequired() then
                begin
                    RequestInterrupt();
                end;
                if fPollDelayInMSecs > 0 then
                begin
                    Sleep(fPollDelayInMSecs);
                    RegisterCheck;
                end;
            end;
    end;
end;

procedure TInterruptMonitor.Execute();
begin
    try
        fMessageQueue.Execute();
    except
        on E: Exception do
        begin
            raise Exception.Create('Interrupt Monitor -->' + E.Message);
        end;
    end;
end;


end.
