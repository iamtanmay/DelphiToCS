{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Register and handle interrupts such as Errormessage, Escape, Stop button
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  -------------------------------   -------- ------------------------------------------
  07.12.06 pk                                    TN3445   from ThrdMan
  22.02.07 pk  Terminate                         TN3583   New: Used to stop the
  03.07.08 wl                                    TN4157
  06.11.08 pk  Terminate                         TN4280   new MaxWaitTime param
  19.11.08 pk                                    TN4280   various changes
  17.12.08 pk  HandleProcessInterruptMessage     TN4372   Set contractor thread ID before handling Interrupt
  08.06.09 pk  RequestInterruptStart/Finish      TN4585.1 New
  18.06.09 pk  HandleRequestInterruptFinishMessage  TN4585.1 set resultsuccess to true
  10.08.09 wl                                    TN4702   Strings werden jetzt direkt geladen
  09.03.10 pk  Terminate                         TN5015   parameter removed
  29.11.10 wl                                    TN5370   TLock.WaitForLock statt TLock.WaitFor
  19.01.12 wl  TInterruptRequest                 TN5759.2  neue property IgnoreInterruptStartError
  01.03.12 wl                                    TN5822   uses geändert
  27.03.13 wl                                    TN6045   uses Generics.Collections
  15.04.13 ts  TInterruptManager.Create          TN6130   TObjectList<TInterruptRequest>.Create(false)-> aOwnsObjects = false
  15.08.13 wl                                    TN6223   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit InterruptManager;


interface


uses
    Classes,
    Generics.Collections,
    GeneralTypes,
    MessagableExecutable,
    MessageQueue,
    ThreadClasses,
    InterruptMonitor,
    LockHandle,
    Executable;

type
    TInterruptRequest = class
    private
        fInterruptText: string;
        fInterruptRoutine: TInterruptRoutineEvent;
        fWaitLock: TLock;
        fOwnsWaitLock: boolean;
        fIRArgs: TInterruptRoutineArgs;
        fIRResult: TInterruptRoutineResult;
        fMustWaitTillHandled: boolean;
        fInterruptorThreadID: cardinal;
        fIgnoreInterruptStartError: boolean;
    public
        constructor Create(const aInterruptorThreadID: cardinal; const aInterruptText: string;
            aInterruptRoutine: TInterruptRoutineEvent; const aIRArgs: TInterruptRoutineArgs;
            aIRResult: TInterruptRoutineResult; aMustWaitTillHandled: boolean; aWaitLock: TLock;
            const aIgnoreInterruptStartError: boolean);
        destructor Destroy(); override;
        procedure WaitTillHandled;
        function CanWaitForResult(): boolean;
        procedure HandleInterrupt();
        procedure Handled();
        property InterruptText: string read fInterruptText write fInterruptText;
        property InterruptRoutine: TInterruptRoutineEvent read fInterruptRoutine;
        property IRArgs: TInterruptRoutineArgs read fIRArgs;
        property IRResult: TInterruptRoutineResult read fIRResult;
        property InterruptorThreadID: cardinal read fInterruptorThreadID;
        property IgnoreInterruptStartError: boolean read fIgnoreInterruptStartError;
    end;

    TRequestInterruptMessageInfo = class(TMessageInfo)
    private
        fInterruptRequestItem: TInterruptRequest;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aInterruptRequestItem: TInterruptRequest);
        property InterruptRequestItem: TInterruptRequest read fInterruptRequestItem;
    end;

    TProcessInterruptMessageInfo = class(TMessageInfo)
    private
        fInterruptRequestItem: TInterruptRequest;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aInterruptRequestItem: TInterruptRequest);
        property InterruptRequestItem: TInterruptRequest read fInterruptRequestItem;
    end;

    TRequestInterruptStartMessageInfo = class(TMessageInfo)
    strict private
        fInterruptorThreadID: cardinal;
        fInterruptText: string;
        fInterruptMaxWaitTime: cardinal;
        fResultSuccess: boolean;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aInterruptorThreadID: cardinal; const aInterruptText: string;
            const aInterruptMaxWaitTime: cardinal);
        property InterruptorThreadID: cardinal read fInterruptorThreadID;
        property InterruptText: string read fInterruptText;
        property InterruptMaxWaitTime: cardinal read fInterruptMaxWaitTime;
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TRequestInterruptFinishMessageInfo = class(TMessageInfo)
    private
        fInterruptorThreadID: cardinal;
        fInterruptText: string;
        fResultSuccess: boolean;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aInterruptorThreadID: cardinal; const aInterruptText: string);
        property InterruptorThreadID: cardinal read fInterruptorThreadID;
        property InterruptText: string read fInterruptText;
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TInterruptManagerStatus = (imsIdle, imsActivated);
    TInterruptStartEvent = function(const aInterruptorThreadID: cardinal; const aInterruptText: string;
        const aInterruptMaxWaitTime: cardinal): boolean of object;
    TInterruptFinishEvent = function(const aInterruptorThreadID: cardinal; const aInterruptText: string)
        : boolean of object;

    TInterruptManagerStatusChanged = procedure(aSender: TObject; aStatus: TInterruptManagerStatus) of object;

    TInterruptManager = class(TMessagableExecutable)
    private
        fInterruptRequests: TObjectList<TInterruptRequest>;
        // procedure DoRequestQueueCountChanged( aSender: TObject; aReason : TMessageQueueCountChangedReason );
        procedure HandleProcessInterruptMessage(const aMessage: TProcessInterruptMessageInfo);
        procedure HandleRequestInterruptMessage(const aMessage: TRequestInterruptMessageInfo);

        procedure HandleRequestInterruptStartMessage(const aMessage: TRequestInterruptStartMessageInfo);
        procedure HandleRequestInterruptFinishMessage(const aMessage: TRequestInterruptFinishMessageInfo);
    protected
        fOnInterruptStart: TInterruptStartEvent;
        fOnInterruptFinish: TInterruptFinishEvent;
        fOnHandleInterrupt: TNotifyEvent;
        fOnStatusChanged: TInterruptManagerStatusChanged;
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); override;
        procedure SetOriginalMask(); override;
    public
        constructor Create();
        destructor Destroy; override;
        procedure Terminate(); override;
        function RequestInterrupt(aInterruptorThreadID: integer; const aInterruptText: string;
            aInterruptRoutine: TInterruptRoutineEvent; aIRArgs: TInterruptRoutineArgs;
            var vIRResult: TInterruptRoutineResult; aWaitTillHandled: boolean; aWaitTillHandledLock: TLock;
            const aIgnoreInterruptStartError: boolean): boolean;
        function RequestInterruptStart(const aInterruptorThreadID: cardinal;
            const aInterruptText: string): boolean;
        function RequestInterruptFinish(const aInterruptorThreadID: cardinal;
            const aInterruptText: string): boolean;

        property OnInterruptStart: TInterruptStartEvent read fOnInterruptStart write fOnInterruptStart;
        property OnInterruptFinish: TInterruptFinishEvent read fOnInterruptFinish write fOnInterruptFinish;
        property OnHandleInterrupt: TNotifyEvent read fOnHandleInterrupt write fOnHandleInterrupt;
        property OnStatusChanged: TInterruptManagerStatusChanged read fOnStatusChanged write fOnStatusChanged;
    end;


implementation


uses
    Windows,
    SysUtils,
    Variants,
    AppTypes,
    ThreadAPI,
    LogManager,
    ErrorManager,
    GUIManager;

const
    cMessageRequestInterruptStart = 9001;
    cMessageRequestInterruptFinish = 9002;

    cMessageRequestInterrupt = 9011;
    cMessageProcessInterrupt = 9012;

    { TInterruptRequest }

constructor TInterruptRequest.Create(const aInterruptorThreadID: cardinal; const aInterruptText: string;
    aInterruptRoutine: TInterruptRoutineEvent; const aIRArgs: TInterruptRoutineArgs;
    aIRResult: TInterruptRoutineResult; aMustWaitTillHandled: boolean; aWaitLock: TLock;
    const aIgnoreInterruptStartError: boolean);
begin
    inherited Create;
    fInterruptorThreadID := aInterruptorThreadID;
    fInterruptText := aInterruptText;
    fInterruptRoutine := aInterruptRoutine;
    fIRArgs := aIRArgs;
    fIRResult := aIRResult;
    fMustWaitTillHandled := aMustWaitTillHandled;
    fOwnsWaitLock := not Assigned(aWaitLock);
    fWaitLock := aWaitLock;
    fIgnoreInterruptStartError := aIgnoreInterruptStartError;
    if fMustWaitTillHandled and fOwnsWaitLock then
        fWaitLock := TSimpleLock.Create(false, false, true);
end;

destructor TInterruptRequest.Destroy();
begin
    if Assigned(fWaitLock) and fOwnsWaitLock then
        fWaitLock.Free;
    inherited;
end;

procedure TInterruptRequest.HandleInterrupt();
begin
    ASSERT(Assigned(fInterruptRoutine), 'No interrupt routine');
    fIRResult := fInterruptRoutine(self, fIRArgs);
end;

procedure TInterruptRequest.WaitTillHandled();
begin
    if Assigned(fWaitLock) then
        fWaitLock.WaitForLock(INFINITE);
end;

function TInterruptRequest.CanWaitForResult(): boolean;
begin
    result := Assigned(fWaitLock);
end;

procedure TInterruptRequest.Handled();
begin
    if Assigned(fWaitLock) then
        fWaitLock.Unlock;
end;

{ TRequestInterruptMessageInfo }

constructor TRequestInterruptMessageInfo.Create(const aInterruptRequestItem: TInterruptRequest);
begin
    inherited Create();
    fInterruptRequestItem := aInterruptRequestItem;
end;

function TRequestInterruptMessageInfo.GetMessageID: integer;
begin
    result := cMessageRequestInterrupt;
end;

{ TProcessInterruptMessageInfo }

constructor TProcessInterruptMessageInfo.Create(const aInterruptRequestItem: TInterruptRequest);
begin
    inherited Create();
    fInterruptRequestItem := aInterruptRequestItem;
end;

function TProcessInterruptMessageInfo.GetMessageID: integer;
begin
    result := cMessageProcessInterrupt;
end;

{ TInterruptManager }

constructor TInterruptManager.Create();
begin
    inherited;
    fInterruptRequests := TObjectList<TInterruptRequest>.Create(false);
    // fMessageQueue.OnCountChanged := DoRequestQueueCountChanged;
end;

destructor TInterruptManager.Destroy;
begin
    fInterruptRequests.Free;
    inherited;
end;

procedure TInterruptManager.SetOriginalMask();
begin
    fMessageQueue.SetMask([INT_MESSAGE_EXECUTABLE_TERMINATE, cMessageRequestInterruptFinish,
        cMessageRequestInterruptStart, cMessageRequestInterrupt, cMessageProcessInterrupt]);
end;

procedure TInterruptManager.HandleProcessInterruptMessage(const aMessage: TProcessInterruptMessageInfo);
var
    xInterruptItem: TInterruptRequest;
    xIndex: integer;
begin
    TThreadAPI.SetCurrentContractorThreadID(aMessage.InterruptRequestItem.InterruptorThreadID);
    try
        xInterruptItem := aMessage.InterruptRequestItem;
        try
            fOnHandleInterrupt(xInterruptItem);
        except
            on E: Exception do
            begin
                gGUIManager.MessageBox(e.Message, 'Thread Error', 0);
                gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
            end;
        end;

        if xInterruptItem.CanWaitForResult then
        begin
            xInterruptItem.Handled;
        end
        else
        begin
            xInterruptItem.Free;
        end;

        xIndex := fInterruptRequests.IndexOf(xInterruptItem);
        fInterruptRequests.Delete(xIndex);

        if fInterruptRequests.Count = 0 then
        begin
            fOnStatusChanged(self, imsIdle);
        end;
    finally
        TThreadAPI.ResetCurrentContractorThreadID();
    end;
end;

procedure TInterruptManager.HandleRequestInterruptMessage(const aMessage: TRequestInterruptMessageInfo);
begin
    if fInterruptRequests.Count = 0 then
    begin
        fOnStatusChanged(self, imsActivated);
    end;

    fInterruptRequests.Add(aMessage.InterruptRequestItem);
    self.RegisterMessageAndLeave(TProcessInterruptMessageInfo.Create(aMessage.InterruptRequestItem));
end;

procedure TInterruptManager.HandleRequestInterruptStartMessage
    (const aMessage: TRequestInterruptStartMessageInfo);
begin

    if fInterruptRequests.Count = 0 then
    begin
        fOnStatusChanged(self, imsActivated);
    end;

    self.fOnInterruptStart(aMessage.InterruptorThreadID, aMessage.InterruptText,
        aMessage.InterruptMaxWaitTime);

    fMessageQueue.SetMaskTemp([INT_MESSAGE_EXECUTABLE_TERMINATE, cMessageRequestInterruptFinish]);

    aMessage.ResultSuccess := true;
end;

procedure TInterruptManager.HandleRequestInterruptFinishMessage
    (const aMessage: TRequestInterruptFinishMessageInfo);
begin
    fMessageQueue.RestoreMask();

    self.fOnInterruptFinish(aMessage.InterruptorThreadID, aMessage.InterruptText);

    if fInterruptRequests.Count = 0 then
    begin
        fOnStatusChanged(self, imsIdle);
    end;

    aMessage.ResultSuccess := true;
end;

procedure TInterruptManager.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);

begin
    inherited;
    if aMessage.MessageInfo is TRequestInterruptStartMessageInfo then
    begin
        HandleRequestInterruptStartMessage(aMessage.MessageInfo as TRequestInterruptStartMessageInfo);
    end
    else if aMessage.MessageInfo is TRequestInterruptFinishMessageInfo then
    begin
        HandleRequestInterruptFinishMessage(aMessage.MessageInfo as TRequestInterruptFinishMessageInfo);
    end
    else if aMessage.MessageInfo is TProcessInterruptMessageInfo then
    begin
        HandleProcessInterruptMessage(aMessage.MessageInfo as TProcessInterruptMessageInfo);
    end
    else if aMessage.MessageInfo is TRequestInterruptMessageInfo then
    begin
        HandleRequestInterruptMessage(aMessage.MessageInfo as TRequestInterruptMessageInfo);
    end
end;

function TInterruptManager.RequestInterrupt(aInterruptorThreadID: integer; const aInterruptText: string;
    aInterruptRoutine: TInterruptRoutineEvent; aIRArgs: TInterruptRoutineArgs;
    var vIRResult: TInterruptRoutineResult; aWaitTillHandled: boolean; aWaitTillHandledLock: TLock;
    const aIgnoreInterruptStartError: boolean): boolean;
// Register an interrupt
// An interrupt causes the threads which perform actions to be suspended
// aWaitTillHandled : there are two reasons why aWaitTillHandled would be needed.
// 1. We want to wait until the message is handled
// 2. We want to wait for vIRResult to be set, so we wait till the message is handled
var
    xInterruptItem: TInterruptRequest;
    xMessageInfo: TRequestInterruptMessageInfo;
begin
    result := true;

    gLogManager.LogF('Registering interrupt: %s - BEGIN', [aInterruptText], false);

    xInterruptItem := TInterruptRequest.Create(aInterruptorThreadID, aInterruptText, aInterruptRoutine,
        aIRArgs, vIRResult, aWaitTillHandled, aWaitTillHandledLock, aIgnoreInterruptStartError);

    xMessageInfo := TRequestInterruptMessageInfo.Create(xInterruptItem);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);

        vIRResult := null;
        // becareful, if aWaitTillHandle=false, the interrupt item may have already been freed
        if aWaitTillHandled then
        begin
            xInterruptItem.WaitTillHandled;
            vIRResult := xInterruptItem.IRResult;
            xInterruptItem.Free;
        end;
    finally
        xMessageInfo.Free;
    end;

    gLogManager.LogF('Registering interrupt: %s - END', [aInterruptText], false);

end;

procedure TInterruptManager.Terminate();
begin
    fMessageQueue.SetMask([INT_MESSAGE_QUIT]);
    fMessageQueue.EnqueueMessage(TMessageQueueItem.Create(INT_MESSAGE_QUIT, 1, nil, false, MessageProc,
        nil, true));
end;

function TInterruptManager.RequestInterruptStart(const aInterruptorThreadID: cardinal;
    const aInterruptText: string): boolean;
var
    xMessageInfo: TRequestInterruptStartMessageInfo;
begin
    xMessageInfo := TRequestInterruptStartMessageInfo.Create(aInterruptorThreadID, aInterruptText, 2000);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultSuccess;
    finally
        xMessageInfo.Free;
    end;
end;

function TInterruptManager.RequestInterruptFinish(const aInterruptorThreadID: cardinal;
    const aInterruptText: string): boolean;
var
    xMessageInfo: TRequestInterruptFinishMessageInfo;
begin
    xMessageInfo := TRequestInterruptFinishMessageInfo.Create(aInterruptorThreadID, aInterruptText);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultSuccess;
    finally
        xMessageInfo.Free;
    end;
end;

{ TRequestInterruptStartMessageInfo }

constructor TRequestInterruptStartMessageInfo.Create(const aInterruptorThreadID: cardinal;
    const aInterruptText: string; const aInterruptMaxWaitTime: cardinal);
begin
    inherited Create();
    fInterruptorThreadID := aInterruptorThreadID;
    fInterruptText := aInterruptText;
    fInterruptMaxWaitTime := aInterruptMaxWaitTime;
    fResultSuccess := false;
end;

function TRequestInterruptStartMessageInfo.GetMessageID: integer;
begin
    result := cMessageRequestInterruptStart;
end;

{ TRequestInterruptFinishMessageInfo }

constructor TRequestInterruptFinishMessageInfo.Create(const aInterruptorThreadID: cardinal;
    const aInterruptText: string);
begin
    inherited Create();
    fInterruptorThreadID := aInterruptorThreadID;
    fInterruptText := aInterruptText;
    fResultSuccess := false;
end;

function TRequestInterruptFinishMessageInfo.GetMessageID: integer;
begin
    result := cMessageRequestInterruptFinish;
end;


end.
