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
  30.07.09 pk                                        TN4585.5    Various changes
  12.10.09 pk                                        TN4812      Handle Insert RunInfo event
  04.02.10 pk                                        TN4973      Compiler Warnings fixed
  29.05.12 wl  DoHandleCall                          TN5904   TCoreWriteLogEventMessageInfo mit ThreadID und DisplayType als Parameter
  ----------------------------------------------------------------------------------------------------------------------- }

unit TCPIPCoreEventHost;


interface


uses
    Classes,
    RemoteServer,
    RemoteClasses,
    CoreEventHost,
    ThreadUtils,
    ScktComp,
    Streamable,
    XMLClientServer,
    CoreEventMessageInfo;

type

    TEventHostMsgServer = class(TMsgServer)
    end;

    TCoreEventHostMsgHandler = class(TXMLMsgHandler)
    protected
        fOnEventHostEventPending: TCoreEventCallback;
        procedure DoHandleCall(); override;
        procedure DoOnEventHostEventPending(const aMessageInfo: TCoreEventMessageInfo);
    public
        constructor Create(aInterpreter: TMsgInterpreter);
        destructor Destroy(); override;
        property OnEventHostEventPending: TCoreEventCallback read fOnEventHostEventPending
            write fOnEventHostEventPending;
    end;

    TTCPIPCoreEventHost = class(TCoreEventHost)
    private
        class var uInstance: TTCPIPCoreEventHost;
    protected
        fMsgServer: TEventHostMsgServer;
        function DoOnCreateMsgHandler(aInterpreter: TMsgInterpreter; aParams: TFirstMesssageParams)
            : TMsgHandler;
        function DoOnCreateMsgInterpreter(const aSocket: TServerClientWinSocket): TMsgInterpreter;
        procedure DoOnEventHostEventPending(const aMessageInfo: TCoreEventMessageInfo);
    public
        constructor Create(const aPort: integer);
        destructor Destroy(); override;
        class procedure CreateInstance(const aPort: integer);
        class procedure DestroyInstance();
        class function Instance(): TTCPIPCoreEventHost;
    end;


implementation


uses
    SysUtils,
    RemoteMessageConstants,
    GeneralTypes,
    LogManager,
    RemoteFunctionClasses,
    RemoteMessageTypes;

{ TCoreEventHostMsgHandler }

constructor TCoreEventHostMsgHandler.Create(aInterpreter: TMsgInterpreter);

begin
    inherited Create(aInterpreter);

end;

destructor TCoreEventHostMsgHandler.Destroy;
begin
    inherited;
end;

procedure TCoreEventHostMsgHandler.DoOnEventHostEventPending(const aMessageInfo: TCoreEventMessageInfo);
begin
    if Assigned(self.OnEventHostEventPending) then
        self.OnEventHostEventPending(aMessageInfo);
end;

procedure TCoreEventHostMsgHandler.DoHandleCall();
var
    xInt1, xInt2: integer;
    xStr1, xStr2: string;
    // xBool1: boolean;
    xMessageInfo: TCoreEventMessageInfo;
    xRemoteCall: TRemoteCall;
begin
    inherited DoHandleCall();
    xMessageInfo := nil;

    xRemoteCall := fRemoteFunctionReceiver.AsRemoteCall;
    if Assigned(xRemoteCall) then
    begin
        if xRemoteCall is TRemoteCallRunInfoInsertCoreStatusEvent then
        begin
            with xRemoteCall as TRemoteCallRunInfoInsertCoreStatusEvent do
            begin
                xMessageInfo := TCoreRunInfoInsertEventMessageInfo.Create(DisplayID, GroupNames, Key, Text,
                    InfoGroupBehaviour);
            end;
        end;
    end
    else
    begin
        if fRemoteFunctionReceiver.CallNameMatches(cCoreEventCallIDControlEventPending) then
        begin
            xMessageInfo := TCoreControlEventMessageInfo.Create();
        end
        else if fRemoteFunctionReceiver.CallNameMatches(cCoreStatusEventCallIDWriteLog) then
        begin
            xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
            xInt1 := fRemoteFunctionReceiver.CallParams[1].AsInt;
            xint2 := fRemoteFunctionReceiver.CallParams[2].AsInt;
            xMessageInfo := TCoreWriteLogEventMessageInfo.Create(xStr1, xInt1, TDisplayLogInfoType(xInt2));
        end
        else if fRemoteFunctionReceiver.CallNameMatches(cCoreStatusEventCallIDLoadDisplayComponent) then
        begin
            xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
            xStr2 := fRemoteFunctionReceiver.CallParams[1].AsStr;
            xMessageInfo := TCoreLoadDisplayComponentEventMessageInfo.Create(xStr1, xStr2);
        end
        else if fRemoteFunctionReceiver.CallNameMatches(cCoreStatusEventCallIDLinkRunIDToProcess) then
        begin
            xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
            xStr2 := fRemoteFunctionReceiver.CallParams[1].AsStr;
            xMessageInfo := TCoreLinkRunIDToProcesstEventMessageInfo.Create(xStr1, xStr2);
        end;
    end;

    if Assigned(xMessageInfo) then
        self.DoOnEventHostEventPending(xMessageInfo);

end;

{ TTCPIPCoreEventHost }

constructor TTCPIPCoreEventHost.Create(const aPort: integer);
begin
    inherited Create();
    fMsgServer := TEventHostMsgServer.Create(aPort);
    fMsgServer.OnCreateMsgHandler := self.DoOnCreateMsgHandler;
    fMsgServer.OnCreateMsgInterpreter := self.DoOnCreateMsgInterpreter;
    fMsgServer.Active := true;
end;

destructor TTCPIPCoreEventHost.Destroy;
begin
    fMsgServer.Free;
    inherited;
end;

class procedure TTCPIPCoreEventHost.CreateInstance(const aPort: integer);
begin
    uInstance := TTCPIPCoreEventHost.Create(aPort);
end;

class procedure TTCPIPCoreEventHost.DestroyInstance;
begin
    uInstance.Free;
end;

class function TTCPIPCoreEventHost.Instance: TTCPIPCoreEventHost;
begin
    result := uInstance;
end;

procedure TTCPIPCoreEventHost.DoOnEventHostEventPending(const aMessageInfo: TCoreEventMessageInfo);
begin
    if Assigned(self.OnEventHostEventPending) then
        self.OnEventHostEventPending(aMessageInfo);
end;

function TTCPIPCoreEventHost.DoOnCreateMsgHandler(aInterpreter: TMsgInterpreter;
    aParams: TFirstMesssageParams): TMsgHandler;
var
    xMsgHandler: TCoreEventHostMsgHandler;
begin
    xMsgHandler := TCoreEventHostMsgHandler.Create(aInterpreter);
    xMsgHandler.OnEventHostEventPending := DoOnEventHostEventPending;
    result := xMsgHandler;
end;

function TTCPIPCoreEventHost.DoOnCreateMsgInterpreter(const aSocket: TServerClientWinSocket): TMsgInterpreter;
begin
    result := TXMLMsgInterpreter.Create(aSocket);
end;


end.
