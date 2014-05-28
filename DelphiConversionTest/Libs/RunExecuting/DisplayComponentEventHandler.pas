{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  09.06.09 pk  Initialize, Finalize          TN4595  New: Add/Remove Trace
  04.02.10 pk                                TN4972  Changes for Restart
  09.03.10 pk                                TN5015  New TDisplayComponentEventHandlerManager
  29.10.10 pk                                TN5320  RequestExecuteEvent has new WaitTillFinished Parameter
  27.03.13 wl                                TN6045   uses geändert
  15.08.13 wl                                TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit DisplayComponentEventHandler;


interface


uses
    Generics.Collections,
    Executable,
    MessagableExecutable,
    MessageQueue,
    ThreadClasses,
    DisplayComponentIntf;

type
    TExecuteEventMessageInfo = class(TMessageInfo)
    private
        fSender: IDisplayComponent;
        fEventName: string;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aSender: IDisplayComponent; const aEventName: string);
        property Sender: IDisplayComponent read fSender;
        property EventName: string read fEventName;
    end;

    IDisplayComponentEventHandler = interface(IMessagableExecutable)
        ['{4E74F58F-6532-44A2-BEAB-61129B333695}']
        procedure RequestExecuteEvent(const aSender: IDisplayComponent; const aEventName: string;
            const aWaitTillFinished: boolean);
        function GetContextID(): string;
        property ContextID: string read GetContextID;
    end;

    TDisplayComponentEventHandler = class(TMessagableExecutable, IDisplayComponentEventHandler)
    private
        fContextID: string;
        procedure ExecuteEvent(const aSender: IDisplayComponent; const aEventName: string);
        procedure HandleMessageExecuteEvent(const aMessageInfo: TExecuteEventMessageInfo);
    protected
        procedure Initialize(); override;
        procedure Finalize(); override;
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); override;
        procedure SetOriginalMask(); override;
    public
        constructor Create(const aContextID: string);
        procedure RequestExecuteEvent(const aSender: IDisplayComponent; const aEventName: string;
            const aWaitTillFinished: boolean);
        function GetContextID(): string;
    end;

    TDisplayComponentEventHandlerManager = class
    private
        fEventHandlers: TList<IDisplayComponentEventHandler>;
        constructor Create();
        class var uInstance: TDisplayComponentEventHandlerManager;

    class var
        uRefCount: integer;
        class property RefCount: integer read uRefCount write uRefCount;
    public
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TDisplayComponentEventHandlerManager;
        procedure AddEventHandler(const aContextID: string; const aHandler: IDisplayComponentEventHandler);
        procedure RemoveEventHandler(const aContextID: string);
        function FindEventHandler(const aContextID: string): IDisplayComponentEventHandler;
    end;


implementation


uses
    SysUtils,
    EventManager,
    RunTraceManager,
    ErrorManager;

const
    cMessageDisplayComponentEvent = 9001;

    { TDisplayComponentEventHandler }

constructor TDisplayComponentEventHandler.Create(const aContextID: string);
begin
    inherited Create();
    fContextID := aContextID;
end;

procedure TDisplayComponentEventHandler.ExecuteEvent(const aSender: IDisplayComponent;
    const aEventName: string);
var
    xLogText: string;
begin
    xLogText := Format('Execute Display Event - %s', [aEventName]);
    TEventManager.Instance.ExecuteRunStartWithNameAndParams(xLogText, aEventName, nil);
end;

procedure TDisplayComponentEventHandler.SetOriginalMask();
begin
    fMessageQueue.SetMask([INT_MESSAGE_EXECUTABLE_TERMINATE, INT_MESSAGE_EXECUTABLE_PAUSE,
        cMessageDisplayComponentEvent]);
end;

procedure TDisplayComponentEventHandler.HandleMessageExecuteEvent(const aMessageInfo
    : TExecuteEventMessageInfo);
begin
    ExecuteEvent(aMessageInfo.Sender, aMessageInfo.EventName);
end;

procedure TDisplayComponentEventHandler.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
    inherited;
    if aMessage.MessageInfo is TExecuteEventMessageInfo then
    begin
        HandleMessageExecuteEvent(aMessage.MessageInfo as TExecuteEventMessageInfo);
    end;
end;

procedure TDisplayComponentEventHandler.RequestExecuteEvent(const aSender: IDisplayComponent;
    const aEventName: string; const aWaitTillFinished: boolean);
var
    xMessageInfo: TExecuteEventMessageInfo;
begin
    xMessageInfo := TExecuteEventMessageInfo.Create(aSender, aEventName);
    if aWaitTillFinished then
    begin
        try
            self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        finally
            xMessageInfo.Free;
        end;
    end
    else
    begin
        self.RegisterMessageAndLeave(xMessageInfo);
    end;
end;

function TDisplayComponentEventHandler.GetContextID(): string;
begin
    result := fContextID;
end;

procedure TDisplayComponentEventHandler.Initialize;
begin
    inherited;
    TDisplayComponentEventHandlerManager.Instance.AddEventHandler(fContextID, self);
    TRunTraceManager.Instance.AddThreadTraceForCurrentThread(false);
end;

procedure TDisplayComponentEventHandler.Finalize;
begin
    TRunTraceManager.Instance.RemoveThreadTraceForCurrentThread(not gErrorManager.IsGlobalErr);
    TDisplayComponentEventHandlerManager.Instance.RemoveEventHandler(fContextID);
    inherited;
end;

{ TExecuteEventMessageInfo }

constructor TExecuteEventMessageInfo.Create(const aSender: IDisplayComponent; const aEventName: string);
begin
    inherited Create();
    fSender := aSender;
    fEventName := aEventName;
end;

function TExecuteEventMessageInfo.GetMessageID: integer;
begin
    result := cMessageDisplayComponentEvent;
end;

{ TLibLoader }
class procedure TDisplayComponentEventHandlerManager.CreateInstance();
begin
    if not Assigned(uInstance) then
        uInstance := TDisplayComponentEventHandlerManager.Create();

    Inc(uRefCount);
end;

class procedure TDisplayComponentEventHandlerManager.DestroyInstance;
begin
    if uRefCount = 1 then
        FreeAndNil(uInstance);

    Dec(uRefCount);
end;

class function TDisplayComponentEventHandlerManager.Instance(): TDisplayComponentEventHandlerManager;
begin
    result := uInstance;
end;

constructor TDisplayComponentEventHandlerManager.Create();
begin
    inherited Create();
    fEventHandlers := TList<IDisplayComponentEventHandler>.Create();
end;

destructor TDisplayComponentEventHandlerManager.Destroy;
begin
    FreeAndNil(fEventHandlers);
    inherited;
end;

function TDisplayComponentEventHandlerManager.FindEventHandler(const aContextID: string)
    : IDisplayComponentEventHandler;
var
    xInterface: IInterface;
    xDisplayComponentEventHandler: IDisplayComponentEventHandler;
begin
    result := nil;
    for xInterface in fEventHandlers do
    begin
        Assert(Supports(xInterface, IDisplayComponentEventHandler, xDisplayComponentEventHandler));
        if not SameText(aContextID, xDisplayComponentEventHandler.ContextID) then
            CONTINUE;
        result := xDisplayComponentEventHandler;
        EXIT;
    end;

end;

procedure TDisplayComponentEventHandlerManager.AddEventHandler(const aContextID: string;
    const aHandler: IDisplayComponentEventHandler);
begin
    fEventHandlers.Add(aHandler);
end;

procedure TDisplayComponentEventHandlerManager.RemoveEventHandler(const aContextID: string);
var
    xInterface: IDisplayComponentEventHandler;
begin
    xInterface := self.FindEventHandler(aContextID);
    fEventHandlers.Remove(xInterface);
end;


initialization


TDisplayComponentEventHandlerManager.RefCount := 0;


finalization


// uLoadedLibs.Free;


end.
