{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  18.06.09 pk                                        TN4585.2    ProcessFinished New aIsError Parameter
  06.07.09 pk  StartMethod                           TN4585.4    Parameters changed
  06.07.09 pk  CreateObject, DestroyObject           TN4585.4    New
  30.07.09 pk                                        TN4585.5    Various Changes
  23.09.09 pk                                        TN4767      only allow one method to run
  12.10.09 pk  DoHandleCall                          TN4812      Handle GetErrorBoxAdvanced call
  16.12.09 pk  CreateObject                          TN4933      can now create TRemoteServerDatabase
  04.02.10 pk                                        TN4972      Changes for Restart
  17.08.10 wl  MessageBox                            TN5112   Parameter neu aufgeteilt
  19.10.10 pk                                        TN5305      changes needed for CoreClient/Server
  21.03.11 wl  AskRunStartPromptModal                TN5508   neue Parameter
  17.03.12 pk                                        TN5809.1  Changes for ZACoreClientX dll
  20.04.12 wl  SetStopPromptModalEventHandled        TN5946   entfernt
  12.11.12 wl                                        TN6008   Log-Funktion angepasst
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreServerTCPIP;


interface


uses
    RemoteServer,
    RemoteClasses,
    RemoteFunctionClasses,
    XMLClientServer,
    ScktComp;

type
    TCoreServerMsgServer = class(TMsgServer)
    end;

    TTCPIPCoreServer = class
    private
        class var uInstance: TTCPIPCoreServer;
        function GetServerPort: integer;
    protected
        fMsgServer: TCoreServerMsgServer;
        fOnWriteLog: TOnRemoteWriteLogCallback;
        function DoOnCreateMsgHandler(aInterpreter: TMsgInterpreter; aParams: TFirstMesssageParams)
            : TMsgHandler;
        function DoOnCreateMsgInterpreter(const aSocket: TServerClientWinSocket): TMsgInterpreter;
    public
        constructor Create(const aPort: integer);
        destructor Destroy(); override;
        property OnWriteLog: TOnRemoteWriteLogCallback read fOnWriteLog write fOnWriteLog;
        class procedure CreateInstance(const aPort: integer);
        class procedure DestroyInstance();
        class function Instance(): TTCPIPCoreServer;
        property ServerPort: integer read GetServerPort;
    end;

    TCoreServerMsgHandler = class(TXMLMsgHandler)
    private
        function IsStartMethodAllowed(): boolean;
        function CreateObject(const aClassName: string): integer;
        procedure DestroyObject(const aObjectID: integer);
    protected
        procedure DoHandleCloseConnectionCall(); override;
        procedure DoHandleCall(); override;
    public
        constructor Create(aInterpreter: TMsgInterpreter);
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    RemoteMessageConstants,
    RemoteMessageTypes,
    ServerCoreServerCalls,
    CoreClientInfo,
    CoreClientInfoManager,
    TCPIPCoreEventTransmitter,
    RemoteQuery,
    CoreServerEventManager,
    ThrdMan,
    DataProviderFactory;

{ TCoreServerMsgHandler }

constructor TCoreServerMsgHandler.Create(aInterpreter: TMsgInterpreter);

begin
    inherited Create(aInterpreter);
    fErrorNumber := 0;
end;

function TCoreServerMsgHandler.CreateObject(const aClassName: string): integer;
var
    xObject: TObject;
begin
    xObject := nil;

    if SameText(aClassName, TRemoteServerQuery.ClassName) then
        xObject := TRemoteServerQuery.Create(fRemoteFunctionReceiver)
    else if SameText(aClassName, TRemoteServerDatabase.ClassName) then
        xObject := TRemoteServerDatabase.Create(fRemoteFunctionReceiver);

    ASSERT(Assigned(xObject));
    result := TCoreClientInfoManager.Instance.AddObject(self.ClientID, xObject);
end;

procedure TCoreServerMsgHandler.DestroyObject(const aObjectID: integer);
begin
    TCoreClientInfoManager.Instance.RemoveObject(self.ClientID, aObjectID);
end;

procedure TCoreServerMsgHandler.DoHandleCloseConnectionCall();
begin
    TCoreClientInfoManager.Instance.RemoveByClientID(self.ClientID);
end;

procedure TCoreServerMsgHandler.DoHandleCall();
var
    xInt1, xInt2, xInt3: integer;
    xIntArray: TIntArray;
    xStr1, xStr2: string;
    xBool1, xBool2, xBool3, xBool4: boolean;
    xObj1: TObject;
    xObject: TObject;
    xStrArray: TStringArray;
    xRemoteCall: TRemoteCall;
begin
    inherited DoHandleCall();

    if fRemoteFunctionReceiver.CallHasObjectID then
    begin

        xObject := TCoreClientInfoManager.Instance.FindObjectByObjectID(self.ClientID,
            fRemoteFunctionReceiver.CallObjectID);
        if xObject is TRemoteServerQuery then
            (xObject as TRemoteServerQuery).HandleCall();
    end
    else
    begin
        xRemoteCall := fRemoteFunctionReceiver.AsRemoteCall;
        if Assigned(xRemoteCall) then
        begin
            if xRemoteCall is TRemoteCallControllerGetErrorBoxAdvanced then
            begin
                with xRemoteCall as TRemoteCallControllerGetErrorBoxAdvanced do
                begin
                    xBool1 := TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID
                        (self.ClientID).GetErrorBoxAdvancedEvent(xInt1, xObj1);
                    fRemoteFunctionReceiver.AssignRemoteResult
                        (TRemoteResultControllerGetErrorBoxAdvanced.Create(xInt1, xObj1, xBool1));
                end;
            end;
        end
        else
        begin
            if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDCreateObject) then
            begin
                xInt1 := CreateObject(fRemoteFunctionReceiver.CallParams[0].AsStr);
                fRemoteFunctionReceiver.ResultParams.Add(xInt1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDDestroyObject) then
            begin
                DestroyObject(fRemoteFunctionReceiver.CallParams[0].AsInt);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDRegisterController) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xBool1 := TCoreClientInfoManager.Instance.ObtainControllerPrivileges(self.ClientID, xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDUnRegisterController) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xBool1 := TCoreClientInfoManager.Instance.ReleaseControllerPrivileges(self.ClientID, xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches
                (cCoreControllerCallIDGetPendingControlEventID) then
            begin
                xInt1 := TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID(self.ClientID)
                    .GetPendingControlEventID();
                fRemoteFunctionReceiver.ResultParams.Add(xInt1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDIsEventHostRegistered) then
            begin
                xBool1 := TCoreClientInfoManager.Instance.IsEventHostRegistered(self.ClientID);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDRegisterEventHost) then
            begin
                xInt1 := fRemoteFunctionReceiver.CallParams[0].AsInt;
                TServerCoreServerCalls.Instance.RegisterEventHost(self.ClientID,
                    fInterpreter.Socket.RemoteAddress, xInt1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDUnRegisterEventHost) then
            begin
                TServerCoreServerCalls.Instance.UnRegisterEventHost(self.ClientID);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDAliasNameExists) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xBool1 := TDataProviderFactory.Instance.AliasNameExists(xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDGetAliasPath) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xStr2 := TDataProviderFactory.Instance.GetAliasPath(xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xStr2);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDCreateDataConnectionParams) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xObj1 := TDataProviderFactory.Instance.CreateDataConnectionParams(xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xObj1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDCreateConnectionParamsByAlias) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xObj1 := TDataProviderFactory.Instance.CreateConnectionParamsByAlias(xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xObj1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDGetMainDBAlias) then
            begin
                xStr1 := TDataProviderFactory.Instance.MainDBAlias;
                fRemoteFunctionReceiver.ResultParams.Add(xStr1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDGetAllAliasNames) then
            begin
                xStrArray := TDataProviderFactory.Instance.GetAllAliasNames();
                fRemoteFunctionReceiver.ResultParams.Add(xStrArray);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDGetNewClientRunID) then
            begin
                fRemoteFunctionReceiver.ResultParams.Add(TCoreClientInfoManager.Instance.NewClientRun());
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDAddClientToRun) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                TCoreClientInfoManager.Instance.AddClientToRun(self.ClientID, xStr1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDRemoveClientFromRun) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                TCoreClientInfoManager.Instance.RemoveClientFromRun(self.ClientID, xStr1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDGetClientRunIDsAtAddress) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xBool1 := fRemoteFunctionReceiver.CallParams[1].AsBool;
                xStrArray := TCoreClientInfoManager.Instance.GetClientRunIDsAtAddress(xStr1, xBool1);
                fRemoteFunctionReceiver.ResultParams.Add(xStrArray);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSetControlEventMask) then
            begin
                xIntArray := fRemoteFunctionReceiver.CallParams[0].AsIntArray;
                xBool1 := TCoreClientInfoManager.Instance.SetClientControlEventMask(self.ClientID, xIntArray);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDStartMethod) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xBool1 := fRemoteFunctionReceiver.CallParams[1].AsBool;
                xStr2 := fRemoteFunctionReceiver.CallParams[2].AsStr;
                // at this point we should generate a processdescription/context and pass this as the processID.  But for now
                // we will just pass the Methodname/Sourcedataname
                xBool2 := IsStartMethodAllowed();
                if xBool2 then
                begin
                    TCoreClientInfoManager.Instance.LinkProcessToRunID(xStr2, xStr1);
                    TCoreServerEventManager.Instance.TransmitLinkProcessToRunIDEvent(xStr2, xStr1);
                    xBool2 := TServerCoreServerCalls.Instance.StartMethod(xStr1, xBool1, xStr2);
                end;
                fRemoteFunctionReceiver.ResultParams.Add(xBool2);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSimulateMethod) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xBool1 := fRemoteFunctionReceiver.CallParams[1].AsBool;
                xStr2 := fRemoteFunctionReceiver.CallParams[2].AsStr;
                xBool2 := IsStartMethodAllowed();
                if xBool2 then
                begin
                    TCoreClientInfoManager.Instance.LinkProcessToRunID(xStr2, xStr1);
                    TCoreServerEventManager.Instance.TransmitLinkProcessToRunIDEvent(xStr2, xStr1);
                    xBool2 := TServerCoreServerCalls.Instance.SimulateMethod(xStr1, xBool1, xStr2);
                end;
                fRemoteFunctionReceiver.ResultParams.Add(xBool2);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDInterruptStart) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xBool1 := TServerCoreServerCalls.Instance.InterruptStart(xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDInterruptFinish) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xBool1 := TServerCoreServerCalls.Instance.InterruptFinish(xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSetGlobalError) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                TServerCoreServerCalls.Instance.SetGlobalError(xStr1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDUserStopInterrupt) then
            begin
                xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
                xBool1 := TServerCoreServerCalls.Instance.UserStopInterrupt(xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDAskStatus) then
            begin
                xInt1 := TServerCoreServerCalls.Instance.AskStatus();
                fRemoteFunctionReceiver.ResultParams.Add(xInt1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSetErrorBoxEventHandled) then
            begin
                xInt1 := fRemoteFunctionReceiver.CallParams[0].AsInt;
                TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID(self.ClientID)
                    .SetErrorBoxEventHandled(xInt1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDGetErrorBoxEvent) then
            begin
                xBool1 := TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID(self.ClientID)
                    .GetErrorBoxEvent(xInt1);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
                fRemoteFunctionReceiver.ResultParams.Add(xInt1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches
                (cCoreControllerCallIDSetMessageBoxEventHandled) then
            begin
                xInt1 := fRemoteFunctionReceiver.CallParams[0].AsInt;
                TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID(self.ClientID)
                    .SetMessageBoxEventHandled(xInt1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDGetMessageBoxEvent) then
            begin
                xBool1 := TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID(self.ClientID)
                    .GetMessageBoxEvent(xStr1, xStr2, xInt1, xInt2, xInt3);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
                fRemoteFunctionReceiver.ResultParams.Add(xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xStr2);
                fRemoteFunctionReceiver.ResultParams.Add(xInt1);
                fRemoteFunctionReceiver.ResultParams.Add(xInt2);
                fRemoteFunctionReceiver.ResultParams.Add(xInt3);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDGetAskRunStartEvent) then
            begin
                xBool1 := TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID(self.ClientID)
                    .GetAskRunStartEvent(xStr1, xBool2, xBool3, xBool4, xInt1);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
                fRemoteFunctionReceiver.ResultParams.Add(xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xBool2);
                fRemoteFunctionReceiver.ResultParams.Add(xBool3);
                fRemoteFunctionReceiver.ResultParams.Add(xBool4);
                fRemoteFunctionReceiver.ResultParams.Add(xInt1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches
                (cCoreControllerCallIDSetAskRunStartEventHandled) then
            begin
                xInt1 := fRemoteFunctionReceiver.CallParams[0].AsInt;
                xBool1 := fRemoteFunctionReceiver.CallParams[1].AsBool;
                xBool2 := fRemoteFunctionReceiver.CallParams[2].AsBool;
                xInt2 := fRemoteFunctionReceiver.CallParams[3].AsInt;
                TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID(self.ClientID)
                    .SetAskRunStartEventHandled(xInt1, xBool1, xBool2, xInt2);
            end
            {
              else if fRemoteFunctionReceiver.CallNameMatches( cCoreControllerCallIDGetLoadDisplayComponentEvent ) then begin
              xBool1 := TServerCoreServerCalls.Instance.GetLoadDisplayComponentEvent( xStr1, xStr2 );
              fRemoteFunctionReceiver.ResultParams.Add( xBool1 );
              fRemoteFunctionReceiver.ResultParams.Add( xStr1 );
              fRemoteFunctionReceiver.ResultParams.Add( xStr2 );
              end
              else if fRemoteFunctionReceiver.CallNameMatches( cCoreControllerCallIDSetLoadDisplayComponentEventHandled ) then begin
              xBool1 := fInterpreter.Stream.Buffer.ReadBool;
              TServerCoreServerCalls.Instance.SetLoadDisplayComponentEventHandled( xBool1 );
              end
            }
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDGetProcessStartedEvent) then
            begin
                xBool1 := TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID(self.ClientID)
                    .GetProcessStartedEvent(xStr1, xStr2);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
                fRemoteFunctionReceiver.ResultParams.Add(xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xStr2);
            end
            else if fRemoteFunctionReceiver.CallNameMatches
                (cCoreControllerCallIDSetProcessStartedEventHandled) then
            begin
                xBool1 := fRemoteFunctionReceiver.CallParams[0].AsBool;
                TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID(self.ClientID)
                    .SetProcessStartedEventHandled(xBool1);
            end
            else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDGetProcessFinishedEvent) then
            begin
                xBool1 := TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID(self.ClientID)
                    .GetProcessFinishedEvent(xStr1, xBool2);
                fRemoteFunctionReceiver.ResultParams.Add(xBool1);
                fRemoteFunctionReceiver.ResultParams.Add(xStr1);
                fRemoteFunctionReceiver.ResultParams.Add(xBool2);
            end
            else if fRemoteFunctionReceiver.CallNameMatches
                (cCoreControllerCallIDSetProcessFinishedEventHandled) then
            begin
                xBool1 := fRemoteFunctionReceiver.CallParams[0].AsBool;
                TCoreClientInfoManager.Instance.FindControllerEventManagerByClientID(self.ClientID)
                    .SetProcessFinishedEventHandled(xBool1);
            end
        end;
    end;;
end;

function TCoreServerMsgHandler.IsStartMethodAllowed(): boolean;
begin
    result := TCoreClientInfoManager.Instance.IsControllerClientID(self.ClientID) and
        (not ThrMan.Instance.SamThreadRunning(false));
end;

{ TTCPIPCoreServer }

constructor TTCPIPCoreServer.Create(const aPort: integer);
begin
    inherited Create();
    fMsgServer := TCoreServerMsgServer.Create(aPort);
    fMsgServer.OnCreateMsgHandler := self.DoOnCreateMsgHandler;
    fMsgServer.OnCreateMsgInterpreter := self.DoOnCreateMsgInterpreter;
    fMsgServer.Active := true;
end;

destructor TTCPIPCoreServer.Destroy;
begin
    fMsgServer.Free;
    inherited;
end;

class procedure TTCPIPCoreServer.CreateInstance(const aPort: integer);
begin
    uInstance := TTCPIPCoreServer.Create(aPort);
end;

class procedure TTCPIPCoreServer.DestroyInstance;
begin
    uInstance.Free;
end;

class function TTCPIPCoreServer.Instance: TTCPIPCoreServer;
begin
    result := uInstance;
end;

function TTCPIPCoreServer.DoOnCreateMsgHandler(aInterpreter: TMsgInterpreter; aParams: TFirstMesssageParams)
    : TMsgHandler;
var
    xMsgHandler: TCoreServerMsgHandler;
begin
    {
      if aParams.ServerType = INT_NETSERVERTYPE_SERVERCONTROL then begin
      result := TServerControlMsgHandler.Create( aInterpreter );
      end
      else begin
      result := TMsgHandler.Create( aInterpreter );
      end;
    }
    xMsgHandler := TCoreServerMsgHandler.Create(aInterpreter);
    xMsgHandler.OnWriteLog := self.OnWriteLog;
    TCoreClientInfoManager.Instance.Add(TCoreClientInfo.Create(xMsgHandler.ClientID));

    result := xMsgHandler;
end;

function TTCPIPCoreServer.DoOnCreateMsgInterpreter(const aSocket: TServerClientWinSocket): TMsgInterpreter;
begin
    result := TXMLMsgInterpreter.Create(aSocket);
end;

function TTCPIPCoreServer.GetServerPort: integer;
begin
    EXIT(fMsgServer.Port);
end;


end.
