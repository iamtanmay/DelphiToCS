{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  18.03.13 ts                                      TN6109   ehemals TCPIPProxyServer von ZAProxy übernommen
  23.04.13 wl                                      TN6135   uses geändert
  21.08.13 ts                                      TN6109   RequestStartableMethods und SimulateMethod
  11.03.14 tp  DoHandleCall: cCoreControllerCallIDRequestMethodLayouts, cCoreControllerCallIDRequestMethodIcons,
  cCoreControllerCallIDRequestBuildingBlockBools, cCoreControllerCallIDRequestEditableBools,
  cCoreControllerCallIDRequestMethodParameters, cCoreControllerCallIDRequestAllLayouts,
  cCoreControllerCallIDCreateMethod, cCoreControllerCallIDSQLWriteQuery, cCoreControllerCallIDSQLReadQuery,
  cCoreControllerCallIDVerifierQuery               TN6375   if statements erweiterung
  ----------------------------------------------------------------------------------------------------------- }

unit TCPIPRunnerServer;


interface


uses
    RunnerServerCalls,
    RemoteServer,
    RemoteClasses,
    RemoteFunctionClasses,
    SimpleStringClientServer,
    ScktComp;

type
    TRunnerServerMsgServer = class(TMsgServer)
    end;

    TSimpleStringRemoteMsgInterpreter = class(TMsgInterpreter)
    protected
        function CreateSocketStream(): TWinSocketStreamExt; override;
    end;

    TTCPIPRunnerServer = class
    private
        fServerCalls: TRunnerServerCalls; // Owner
    protected
        fMsgServer: TRunnerServerMsgServer;
        fOnWriteLog: TOnRemoteWriteLogCallback;
        function DoOnCreateMsgHandler(aInterpreter: TMsgInterpreter; aParams: TFirstMesssageParams)
            : TMsgHandler;
        function DoOnCreateMsgInterpreter(const aSocket: TServerClientWinSocket): TMsgInterpreter;
    public
        constructor Create(const aPort: integer);
        destructor Destroy(); override;
        property OnWriteLog: TOnRemoteWriteLogCallback read fOnWriteLog write fOnWriteLog;
    end;

    TRunnerServerMsgHandler = class(TSimpleStringRemoteFunctionMsgHandler)
    private
        fServerCalls: TRunnerServerCalls; // Referenz, kein Owner
    protected
        procedure DoHandleCloseConnectionCall(); override;
        procedure DoHandleCall(); override;
    public
        constructor Create(aInterpreter: TMsgInterpreter; aServerCalls: TRunnerServerCalls);
    end;


implementation


uses
    SysUtils,
    RemoteMessageConstants;

{ TRunnerServerMsgHandler }

constructor TRunnerServerMsgHandler.Create(aInterpreter: TMsgInterpreter; aServerCalls: TRunnerServerCalls);

begin
    inherited Create(aInterpreter);

    fServerCalls := aServerCalls;
end;

procedure TRunnerServerMsgHandler.DoHandleCloseConnectionCall();
begin

end;

procedure TRunnerServerMsgHandler.DoHandleCall();
var
    xInt1: integer;
    xIntArray: TArray<integer>;
    xStr1, xStr2: string;
    xBool1, xBool2: boolean;
begin
    inherited DoHandleCall();

    if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDRegisterController) then
    begin
        xBool1 := fServerCalls.RegisterController();
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreCallIDUnRegisterController) then
    begin
        xBool1 := fServerCalls.UnRegisterController();
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDGetPendingControlEventID) then
    begin
        xInt1 := fServerCalls.GetPendingControlEventID(self.ClientID);
        fRemoteFunctionReceiver.ResultParams.Add(xInt1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSetControlEventMask) then
    begin
        xIntArray := fRemoteFunctionReceiver.CallParams[0].AsIntArray;
        xBool1 := fServerCalls.SetControlEventMask(xIntArray);
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDStartMethod) then
    begin
        xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
        xBool1 := fServerCalls.StartMethod(xStr1, true);
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSimulateMethod) then
    begin
        xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
        xBool1 := fServerCalls.SimulateMethod(xStr1, true);
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDInterruptStart) then
    begin
        xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
        xBool1 := fServerCalls.InterruptStart(xStr1);
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDInterruptFinish) then
    begin
        xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
        xBool1 := fServerCalls.InterruptFinish(xStr1);
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSetGlobalError) then
    begin
        xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
        fServerCalls.SetGlobalError(xStr1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDUserStopInterrupt) then
    begin
        xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
        xBool1 := fServerCalls.UserStopInterrupt(xStr1);
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDAskStatus) then
    begin
        xInt1 := fServerCalls.AskStatus();
        fRemoteFunctionReceiver.ResultParams.Add(xInt1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDRequestStartableMethods) then
    begin
        xStr1 := fServerCalls.RequestStartableMethods();
        fRemoteFunctionReceiver.ResultParams.Add(xStr1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSetErrorBoxEventHandled) then
    begin
        xInt1 := fRemoteFunctionReceiver.CallParams[0].AsInt;
        fServerCalls.SetErrorBoxEventHandled(xInt1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDGetErrorBoxEvent) then
    begin
        xBool1 := fServerCalls.GetErrorBoxEvent(xInt1);
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
        fRemoteFunctionReceiver.ResultParams.Add(xInt1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSetMessageBoxEventHandled) then
    begin
        xInt1 := fRemoteFunctionReceiver.CallParams[0].AsInt;
        fServerCalls.SetMessageBoxEventHandled(xInt1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDGetMessageBoxEvent) then
    begin
        xBool1 := fServerCalls.GetMessageBoxEvent(xStr1, xStr2, xInt1);
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
        fRemoteFunctionReceiver.ResultParams.Add(xStr1);
        fRemoteFunctionReceiver.ResultParams.Add(xStr2);
        fRemoteFunctionReceiver.ResultParams.Add(xInt1);
    end
    {
      else if fRemoteFunctionReceiver.CallNameMatches( cCoreControllerCallIDGetLoadDisplayComponentEvent ) then begin
      xBool1 := fServerCalls.GetLoadDisplayComponentEvent( xStr1, xStr2 );
      fRemoteFunctionReceiver.ResultParams.Add( xBool1 );
      fRemoteFunctionReceiver.ResultParams.Add( xStr1 );
      fRemoteFunctionReceiver.ResultParams.Add( xStr2 );
      end
      else if fRemoteFunctionReceiver.CallNameMatches( cCoreControllerCallIDSetLoadDisplayComponentEventHandled ) then begin
      xBool1 := fInterpreter.Stream.Buffer.ReadBool;
      fServerCalls.SetLoadDisplayComponentEventHandled( xBool1 );
      end
    }
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDGetProcessStartedEvent) then
    begin
        xBool1 := fServerCalls.GetProcessStartedEvent(xStr1, xStr2);
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
        fRemoteFunctionReceiver.ResultParams.Add(xStr1);
        fRemoteFunctionReceiver.ResultParams.Add(xStr2);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSetProcessStartedEventHandled) then
    begin
        xBool1 := fRemoteFunctionReceiver.CallParams[0].AsBool;
        fServerCalls.SetProcessStartedEventHandled(xBool1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDGetProcessFinishedEvent) then
    begin
        xBool1 := fServerCalls.GetProcessFinishedEvent(xStr1, xBool2);
        fRemoteFunctionReceiver.ResultParams.Add(xBool1);
        fRemoteFunctionReceiver.ResultParams.Add(xStr1);
        fRemoteFunctionReceiver.ResultParams.Add(xBool2);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSetProcessFinishedEventHandled) then
    begin
        xBool1 := fRemoteFunctionReceiver.CallParams[0].AsBool;
        fServerCalls.SetProcessFinishedEventHandled(xBool1);
    end

    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDRequestMethodLayouts) then
    begin
        xStr1 := fServerCalls.RequestMethodLayouts();
        fRemoteFunctionReceiver.ResultParams.Add(xStr1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDRequestMethodIcons) then
    begin
        xStr1 := fServerCalls.RequestMethodIcons();
        fRemoteFunctionReceiver.ResultParams.Add(xStr1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDRequestBuildingBlockBools) then
    begin
        xStr1 := fServerCalls.RequestBuildingBlockBools();
        fRemoteFunctionReceiver.ResultParams.Add(xStr1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDRequestEditableBools) then
    begin
        xStr1 := fServerCalls.RequestEditableBools();
        fRemoteFunctionReceiver.ResultParams.Add(xStr1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDRequestMethodParameters) then
    begin
        xStr1 := fServerCalls.RequestMethodParameters();
        fRemoteFunctionReceiver.ResultParams.Add(xStr1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDRequestAllLayouts) then
    begin
        xStr1 := fServerCalls.RequestAllLayouts();
        fRemoteFunctionReceiver.ResultParams.Add(xStr1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDCreateMethod) then
    begin
        xBool1 := fRemoteFunctionReceiver.CallParams[0].AsBool;
        fServerCalls.SetProcessFinishedEventHandled(xBool1);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSQLWriteQuery) then
    begin
        xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
        fServerCalls.SQLWriteQuery(xStr1);
        fRemoteFunctionReceiver.ResultParams.Add(true);
    end
    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDSQLReadQuery) then
    begin
        xStr1 := fRemoteFunctionReceiver.CallParams[0].AsStr;
        xStr1 := fServerCalls.SQLReadQuery(xStr1);
        fRemoteFunctionReceiver.ResultParams.Add(xStr1);
    end

    else if fRemoteFunctionReceiver.CallNameMatches(cCoreControllerCallIDVerifierQuery) then
    begin
        xBool1 := fRemoteFunctionReceiver.CallParams[0].AsBool;
        fServerCalls.SetProcessFinishedEventHandled(xBool1);
    end;
end;

{ TTCPIPRunnerServer }

constructor TTCPIPRunnerServer.Create(const aPort: integer);
begin
    inherited Create();

    fServerCalls := TRunnerServerCalls.Create();

    fMsgServer := TRunnerServerMsgServer.Create(aPort);
    fMsgServer.OnCreateMsgHandler := self.DoOnCreateMsgHandler;
    fMsgServer.OnCreateMsgInterpreter := self.DoOnCreateMsgInterpreter;
    fMsgServer.Active := true;

end;

destructor TTCPIPRunnerServer.Destroy;
begin
    FreeAndNil(fMsgServer);
    FreeAndNil(fServerCalls);

    inherited;
end;

function TTCPIPRunnerServer.DoOnCreateMsgHandler(aInterpreter: TMsgInterpreter; aParams: TFirstMesssageParams)
    : TMsgHandler;
var
    xMsgHandler: TRunnerServerMsgHandler;
begin
    {
      if aParams.ServerType = INT_NETSERVERTYPE_SERVERCONTROL then begin
      result := TServerControlMsgHandler.Create( aInterpreter );
      end
      else begin
      result := TMsgHandler.Create( aInterpreter );
      end;
    }
    xMsgHandler := TRunnerServerMsgHandler.Create(aInterpreter, fServerCalls);
    xMsgHandler.OnWriteLog := self.OnWriteLog;
    // TCoreClientInfoManager.Instance.Add( TCoreClientInfo.Create( xMsgHandler.ClientID ) );

    result := xMsgHandler;
end;

function TTCPIPRunnerServer.DoOnCreateMsgInterpreter(const aSocket: TServerClientWinSocket): TMsgInterpreter;
begin
    result := TSimpleStringRemoteMsgInterpreter.Create(aSocket);
end;

{ TSimpleStringRemoteMsgInterpreter }

function TSimpleStringRemoteMsgInterpreter.CreateSocketStream: TWinSocketStreamExt;
begin
    result := TSimpleStringRemoteSocketStreamType.Create(fSocket, 2000, INT_SERVER_WAITFORDATA_TIMEOUT_MSECS);
end;


end.
