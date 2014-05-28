{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  18.06.09 pk                                        TN4585.3    Uses changed
  30.07.09 pk                                        TN4585.5    Various Changes
  23.09.09 pk                                        TN4793      GetIsActive moved to protected
  12.10.09 pk  TransmitRunInfoInsert                 TN4812      New
  04.02.10 pk                                        TN4972      Changes for Restart
  19.10.10 pk                                        TN5305      changes needed for CoreClient/Server
  12.11.12 wl                                        TN6008   Log-Funktion angepasst
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit TCPIPCoreEventTransmitter;


interface


uses
    RemoteClasses,
    RemoteClient,
    CoreEventTransmitter,
    RemoteFunctionClasses,
    GeneralTypes,
    LogManager, MessagableExecutable,MessageQueue,
    ThreadClasses, TrackingSemaphore;

type

    TCoreEventTrasmitterMessageIDs = class
    public const
        cControlEventPendingMessageID = 9001;
        cWriteLogEventMessageID = 9002;
        cLinkProcessToRunIDEventMessageID = 9003;
        cDisplayComponentEventMessageID = 9004;
    end;

    TControlEventPendingMessageInfo = class(TMessageInfo)
    protected
        function GetMessageID: integer; override;
    end;

    TTransmitWriteLogEventMessageInfo = class(TMessageInfo)
    protected
        fLogText: string;
        fThreadID: integer;
        fDisplayType: integer;
        function GetMessageID: integer; override;
    public
        constructor Create(const aLogText: string; aThreadID: integer; aDisplayType: integer);
        property LogText: string read fLogText;
        property ThreadID: integer read fThreadID;
        property DisplayType: integer read fDisplayType;
    end;

    TTransmitLinkProcessToRunIDEventMessageInfo = class(TMessageInfo)
    protected
        fRunID: string;
        fProcessID: string;
        function GetMessageID: integer; override;
    public
        constructor Create(const aRunID, aProcessID: string);
        property RunID: string read fRunID;
        property ProcessID: string read fProcessID write fProcessID;
    end;

    TTransmitDisplayComponentEventMessageInfo = class(TMessageInfo)
    private
        fDisplayComponentName: string;
        fContextID: string;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aDisplayComponentName, aContextID: string);
        property DisplayComponentName: string read fDisplayComponentName;
        property ContextID: string read fContextID;
    end;

    TTCPIPCoreEventTransmitterHandler = class(TMessagableExecutable)
    private
        fConnection: TMsgClient;
        function GetRemoteFunctionCaller: TRemoteFunctionCaller;
        procedure HandleMessageControlEventPending(const aMessage: TControlEventPendingMessageInfo);
        procedure HandleMessageTransmitWriteLogEvent(const aMessage: TTransmitWriteLogEventMessageInfo);
        procedure HandleMessageTransmitLinkProcessToRunIDEvent
            (const aMessage: TTransmitLinkProcessToRunIDEventMessageInfo);
        procedure HandleMessageTransmitDisplayComponentEvent(const aMessage
            : TTransmitDisplayComponentEventMessageInfo);
        property RemoteFunctionCaller: TRemoteFunctionCaller read GetRemoteFunctionCaller;
    protected
        procedure SetOriginalMask(); override;
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); override;
    public
        constructor Create(const aConnection: TMsgClient);
        procedure TransmitControlEventPending();
        procedure TransmitRunInfoInsert(const aDisplayID: string; const aGroupNames: TStringArray;
            const aKey, aText: string; aInfoGroupBehaviour: integer);
        procedure TransmitWriteLogEvent(const aLogText: string; aThreadID: integer; aDisplayType: integer);
        procedure TransmitLoadDisplayComponentEvent(const aDisplayComponentName: string;
            const aContextID: string);
        procedure TransmitLinkProcessToRunIDEvent(const aRunID, aProcessID: string);
    end;

    TTCPIPCoreEventTransmitter = class(TCoreEventTransmitter)
    private
        fHandler: TTCPIPCoreEventTransmitterHandler;
        function GetRemoteFunctionCaller: TRemoteFunctionCaller;
    protected
        fConnection: TMsgClient;
        fCriticalSection: TTrackingSemaphore;
        function GetIsActive: boolean; override;
        property RemoteFunctionCaller: TRemoteFunctionCaller read GetRemoteFunctionCaller;
    public
        constructor Create();
        destructor Destroy(); override;
        function Connect(const aAddress: string; const aPort: integer): boolean; override;
        procedure Disconnect(); override;
        procedure ControlEventPending(); override;
        procedure TransmitRunInfoInsert(const aDisplayID: string; const aGroupNames: TStringArray;
            const aKey, aText: string; aInfoGroupBehaviour: integer); override;
        procedure TransmitWriteLogEvent(const aLogText: string; aThreadID: integer;
            aDisplayType: integer); override;
        procedure TransmitLoadDisplayComponentEvent(const aDisplayComponentName: string;
            const aContextID: string); override;
        procedure TransmitLinkProcessToRunIDEvent(const aRunID, aProcessID: string); override;
        property IsActive: boolean read GetIsActive;
    end;


implementation


uses
    SysUtils,
    Windows,
    forms,
    RemoteMessageConstants,
    AppTypes,
    XMLClientServer,
    RemoteMessageTypes,
    ThrdMan;

{ TTCPIPCoreEventTransmitter }

constructor TTCPIPCoreEventTransmitter.Create;
begin
    inherited Create();
    fConnection := TXMLMsgClient.Create('', 0);
    // fCriticalSection := TSemaphore.Create(1);

    fHandler := TTCPIPCoreEventTransmitterHandler.Create(fConnection);
    TThreadManagerSetup.Instance.CreateServiceThread(fHandler, 'TCPIPCoreEventTransmitterHandler');
    fHandler.Unpause();
end;

destructor TTCPIPCoreEventTransmitter.Destroy;
begin
    // fCriticalSection.Free;
    Disconnect();
    fConnection.Free;
    inherited;
end;

function TTCPIPCoreEventTransmitter.Connect(const aAddress: string; const aPort: integer): boolean;
begin
    fConnection.Address := aAddress;
    fConnection.Port := aPort;
    fConnection.Active := true;
    result := self.IsActive and self.RemoteFunctionCaller.CallBoolFunc(cCoreCallIDCheckConnection);

end;

function TTCPIPCoreEventTransmitter.GetIsActive: boolean;
begin
    result := fConnection.Active;
end;

procedure TTCPIPCoreEventTransmitter.Disconnect;
begin
    fConnection.CloseClient();
end;

procedure TTCPIPCoreEventTransmitter.ControlEventPending();
begin
    if not self.IsActive then
        EXIT;
    fHandler.TransmitControlEventPending();
end;

procedure TTCPIPCoreEventTransmitter.TransmitWriteLogEvent(const aLogText: string; aThreadID: integer;
    aDisplayType: integer);
begin
    if not self.IsActive then
        EXIT;
    fHandler.TransmitWriteLogEvent(aLogText, aThreadID, aDisplayType);

end;

procedure TTCPIPCoreEventTransmitter.TransmitLinkProcessToRunIDEvent(const aRunID, aProcessID: string);
begin
    if not self.IsActive then
        EXIT;
    fHandler.TransmitLinkProcessToRunIDEvent(aRunID, aProcessID);
end;

procedure TTCPIPCoreEventTransmitter.TransmitLoadDisplayComponentEvent(const aDisplayComponentName: string;
    const aContextID: string);
begin
    if not self.IsActive then
        EXIT;
    fHandler.TransmitLoadDisplayComponentEvent(aDisplayComponentName, aContextID);
end;

procedure TTCPIPCoreEventTransmitter.TransmitRunInfoInsert(const aDisplayID: string;
    const aGroupNames: TStringArray; const aKey, aText: string; aInfoGroupBehaviour: integer);
begin
    // fCriticalSection.Enter();
    // try
    // self.RemoteFunctionCaller.CallFunc<TRemoteResult>( TRemoteCallRunInfoInsertCoreStatusEvent.Create( aDisplayID, aGroupNames, aKey, aText, aInfoGroupBehaviour ) );
    // finally
    // fCriticalSection.Leave();
    // end;
end;

function TTCPIPCoreEventTransmitter.GetRemoteFunctionCaller: TRemoteFunctionCaller;
begin
    result := (fConnection as TXMLMsgClient).RemoteFunctionCaller;
end;

function TControlEventPendingMessageInfo.GetMessageID: integer;
begin
    result := TCoreEventTrasmitterMessageIDs.cControlEventPendingMessageID;
end;

{ TTransmitLinkProcessToRunIDEventMessageInfo }

constructor TTransmitLinkProcessToRunIDEventMessageInfo.Create(const aRunID, aProcessID: string);
begin
    inherited Create();
    fRunID := aRunID;
    fProcessID := aProcessID;
end;

function TTransmitLinkProcessToRunIDEventMessageInfo.GetMessageID: integer;
begin
    result := TCoreEventTrasmitterMessageIDs.cLinkProcessToRunIDEventMessageID;
end;

{ TTransmitDisplayComponentEventMessageInfo }

constructor TTransmitDisplayComponentEventMessageInfo.Create(const aDisplayComponentName, aContextID: string);
begin
    inherited Create();
    fDisplayComponentName := aDisplayComponentName;
    fContextID := aContextID;
end;

function TTransmitDisplayComponentEventMessageInfo.GetMessageID: integer;
begin
    result := TCoreEventTrasmitterMessageIDs.cDisplayComponentEventMessageID;
end;

{ TTCPIPCoreEventTransmitterHandler }
procedure TTCPIPCoreEventTransmitterHandler.TransmitControlEventPending();
begin
    RegisterMessageAndWait(TControlEventPendingMessageInfo.Create());
end;

procedure TTCPIPCoreEventTransmitterHandler.TransmitLinkProcessToRunIDEvent(const aRunID, aProcessID: string);
begin
    self.RegisterMessageAndLeave(TTransmitLinkProcessToRunIDEventMessageInfo.Create(aRunID, aProcessID));

end;

procedure TTCPIPCoreEventTransmitterHandler.TransmitLoadDisplayComponentEvent(const aDisplayComponentName,
    aContextID: string);
begin
    self.RegisterMessageAndLeave(TTransmitDisplayComponentEventMessageInfo.Create(aDisplayComponentName,
        aContextID));

end;

procedure TTCPIPCoreEventTransmitterHandler.TransmitRunInfoInsert(const aDisplayID: string;
    const aGroupNames: TStringArray; const aKey, aText: string; aInfoGroupBehaviour: integer);
begin
    inherited;

end;

procedure TTCPIPCoreEventTransmitterHandler.TransmitWriteLogEvent(const aLogText: string; aThreadID: integer;
    aDisplayType: integer);
begin
    self.RegisterMessageAndLeave(TTransmitWriteLogEventMessageInfo.Create(aLogText, aThreadID, aDisplayType));
end;

constructor TTCPIPCoreEventTransmitterHandler.Create(const aConnection: TMsgClient);
begin
    inherited Create();
    fConnection := aConnection;
end;

function TTCPIPCoreEventTransmitterHandler.GetRemoteFunctionCaller: TRemoteFunctionCaller;
begin
    result := (fConnection as TXMLMsgClient).RemoteFunctionCaller;
end;

procedure TTCPIPCoreEventTransmitterHandler.HandleMessageControlEventPending
    (const aMessage: TControlEventPendingMessageInfo);
begin
    self.RemoteFunctionCaller.CallProc(cCoreEventCallIDControlEventPending);
end;

procedure TTCPIPCoreEventTransmitterHandler.HandleMessageTransmitLinkProcessToRunIDEvent
    (const aMessage: TTransmitLinkProcessToRunIDEventMessageInfo);
begin
    self.RemoteFunctionCaller.CallProc(cCoreStatusEventCallIDLinkRunIDToProcess,
        [aMessage.RunID, aMessage.ProcessID]);
end;

procedure TTCPIPCoreEventTransmitterHandler.HandleMessageTransmitWriteLogEvent
    (const aMessage: TTransmitWriteLogEventMessageInfo);
begin
    self.RemoteFunctionCaller.CallProc(cCoreStatusEventCallIDWriteLog, [aMessage.LogText, aMessage.ThreadID,
        aMessage.DisplayType]);
end;

procedure TTCPIPCoreEventTransmitterHandler.HandleMessageTransmitDisplayComponentEvent
    (const aMessage: TTransmitDisplayComponentEventMessageInfo);
begin
    self.RemoteFunctionCaller.CallProc(cCoreStatusEventCallIDLoadDisplayComponent,
        [aMessage.DisplayComponentName, aMessage.ContextID]);
end;

procedure TTCPIPCoreEventTransmitterHandler.SetOriginalMask();
begin
    fMessageQueue.SetMask([INT_MESSAGE_EXECUTABLE_TERMINATE,
        TCoreEventTrasmitterMessageIDs.cControlEventPendingMessageID,
        TCoreEventTrasmitterMessageIDs.cLinkProcessToRunIDEventMessageID,
        TCoreEventTrasmitterMessageIDs.cDisplayComponentEventMessageID,
        // ...
        TCoreEventTrasmitterMessageIDs.cWriteLogEventMessageID]);
end;

procedure TTCPIPCoreEventTransmitterHandler.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
    try
        inherited;
        if aMessage.MessageInfo is TControlEventPendingMessageInfo then
        begin
            HandleMessageControlEventPending(aMessage.MessageInfo as TControlEventPendingMessageInfo);
        end
        else if aMessage.MessageInfo is TTransmitWriteLogEventMessageInfo then
        begin
            HandleMessageTransmitWriteLogEvent(aMessage.MessageInfo as TTransmitWriteLogEventMessageInfo);
        end
        else if aMessage.MessageInfo is TTransmitLinkProcessToRunIDEventMessageInfo then
        begin
            HandleMessageTransmitLinkProcessToRunIDEvent
                (aMessage.MessageInfo as TTransmitLinkProcessToRunIDEventMessageInfo);
        end
        else if aMessage.MessageInfo is TTransmitDisplayComponentEventMessageInfo then
        begin
            HandleMessageTransmitDisplayComponentEvent
                (aMessage.MessageInfo as TTransmitDisplayComponentEventMessageInfo);
        end
    except
        on E: Exception do
        begin
            raise; // gLogManager.Log( E.Message, false );
        end;
    end;
end;

{ TTransmitWriteLogEventEventMessageInfo }

constructor TTransmitWriteLogEventMessageInfo.Create(const aLogText: string; aThreadID: integer;
    aDisplayType: integer);
begin
    inherited Create();
    fLogText := aLogText;
    fThreadID := aThreadID;
    fDisplayType := aDisplayType;
end;

function TTransmitWriteLogEventMessageInfo.GetMessageID: integer;
begin
    result := TCoreEventTrasmitterMessageIDs.cWriteLogEventMessageID;
end;


end.
