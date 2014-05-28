{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  18.06.09 pk  ProcessFinished                       TN4585.2    New aIsError Parameter
  06.07.09 pk  StartMethod                           TN4585.4    Parameters changed
  30.07.09 pk                                        TN4585.5    Various changes
  12.10.09 pk  GetErrorBoxAdvancedEvent              TN4812      New
  21.03.11 wl  AskRunStartPromptModal                TN5508   neue Parameter
  20.04.12 wl  SetStopPromptModalEventHandled        TN5946   entfernt
  ----------------------------------------------------------------------------------------------------------------------- }

unit TCPIPCoreControllerClientCalls;


interface


uses
    GeneralTypes,
    CoreControllerClientCalls,
    TCPIPCoreClient;

type
    TTCPIPCoreControllerClientCalls = class(TCoreControllerClientCalls)
    protected
        fCoreClient: TTCPIPCoreClient;
    public
        constructor Create(const aCoreClient: TTCPIPCoreClient);
        destructor Destroy(); override;
        function SetControlEventMask(const aIDs: TIntArray): boolean; override;
        function StartMethod(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientGroupID: string): boolean; override;
        function SimulateMethod(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientGroupID: string): boolean; override;
        function InterruptStart(const aInterruptText: string): boolean; override;
        function InterruptFinish(const aInterruptText: string): boolean; override;
        procedure SetGlobalError(const aErrorText: string); override;
        function UserStopInterrupt(const aInterruptText: string): boolean; override;
        function AskStatus(): integer; override;
        function GetPendingControlEventID: integer; override;
        function GetErrorBoxEvent(out oErrorID: integer): boolean; override;
        function GetErrorBoxAdvancedEvent(out oErrorID: integer; out oErrorInfo: TObject): boolean; override;
        procedure SetErrorBoxEventHandled(const aResultModal: integer); override;
        function GetMessageBoxEvent(out oText, oCaption: string; out oType: integer): boolean; override;
        procedure SetMessageBoxEventHandled(const aResultModal: integer); override;
        function GetAskRunStartEvent(out oMethodName: string; out oIsSimChangeAllowed: boolean;
            out oIsSim: boolean; out oSimulationAskWeight: boolean; out oSimulationSpeed_Percent: integer)
            : boolean; override;
        procedure SetAskRunStartEventHandled(const aResultModal: integer; const aResultIsSim: boolean;
            const aSimulationAskWeight: boolean; const aSimulationSpeed_Percent: integer); override;
        function GetLoadDisplayComponentEvent(out oDisplayComponentName: string; out oContextID: string)
            : boolean; override;
        procedure SetLoadDisplayComponentEventHandled(const aResultSuccess: boolean); override;
        function GetProcessStartedEvent(out oSourceDataName: string; out oProcessID: string)
            : boolean; override;
        procedure SetProcessStartedEventHandled(const aResultSuccess: boolean); override;
        function GetProcessFinishedEvent(out oProcessID: string; out oIsError: boolean): boolean; override;
        procedure SetProcessFinishedEventHandled(const aResultSuccess: boolean); override;
        class function Instance(): TTCPIPCoreControllerClientCalls;
    end;


implementation


uses
    RemoteClasses,
    RemoteMessageConstants,
    RemoteFunctionClasses,
    RemoteMessageTypes;

{ TTCPIPCoreControllerClientCalls }

constructor TTCPIPCoreControllerClientCalls.Create(const aCoreClient: TTCPIPCoreClient);
begin
    inherited Create();
    fCoreClient := aCoreClient;
end;

destructor TTCPIPCoreControllerClientCalls.Destroy;
begin
    inherited;
end;

class function TTCPIPCoreControllerClientCalls.Instance: TTCPIPCoreControllerClientCalls;
begin
    result := TCoreControllerClientCalls.Instance as TTCPIPCoreControllerClientCalls;
end;

function TTCPIPCoreControllerClientCalls.InterruptStart(const aInterruptText: string): boolean;
begin
    result := false;
    if not fCoreClient.IsActive then
        EXIT;
    result := fCoreClient.RemoteFunctionCaller.CallBoolFunc(cCoreControllerCallIDInterruptStart,
        [aInterruptText]);
end;

function TTCPIPCoreControllerClientCalls.InterruptFinish(const aInterruptText: string): boolean;
begin
    result := false;
    if not fCoreClient.IsActive then
        EXIT;
    result := fCoreClient.RemoteFunctionCaller.CallBoolFunc(cCoreControllerCallIDInterruptFinish,
        [aInterruptText]);
end;

function TTCPIPCoreControllerClientCalls.StartMethod(const aMethodName: string;
    const aDontShowMessages: boolean; const aClientGroupID: string): boolean;
begin
    result := false;
    if not fCoreClient.IsActive then
        EXIT;
    result := fCoreClient.RemoteFunctionCaller.CallBoolFunc(cCoreControllerCallIDStartMethod,
        [aMethodName, aDontShowMessages, aClientGroupID]);
end;

function TTCPIPCoreControllerClientCalls.SimulateMethod(const aMethodName: string;
    const aDontShowMessages: boolean; const aClientGroupID: string): boolean;
begin
    result := false;
    if not fCoreClient.IsActive then
        EXIT;
    result := fCoreClient.RemoteFunctionCaller.CallBoolFunc(cCoreControllerCallIDSimulateMethod,
        [aMethodName, aDontShowMessages, aClientGroupID]);
end;

procedure TTCPIPCoreControllerClientCalls.SetGlobalError(const aErrorText: string);
begin
    if not fCoreClient.IsActive then
        EXIT;
    fCoreClient.RemoteFunctionCaller.CallProc(cCoreControllerCallIDSetGlobalError, [aErrorText]);
end;

function TTCPIPCoreControllerClientCalls.UserStopInterrupt(const aInterruptText: string): boolean;
begin
    result := false;
    if not fCoreClient.IsActive then
        EXIT;
    result := fCoreClient.RemoteFunctionCaller.CallBoolFunc(cCoreControllerCallIDUserStopInterrupt,
        [aInterruptText]);
end;

function TTCPIPCoreControllerClientCalls.AskStatus: integer;
begin
    result := 0;
    if not fCoreClient.IsActive then
        EXIT;
    result := fCoreClient.RemoteFunctionCaller.CallIntFunc(cCoreControllerCallIDAskStatus);
end;

function TTCPIPCoreControllerClientCalls.GetPendingControlEventID: integer;
begin
    result := cControlEventIDNone;
    if not fCoreClient.IsActive then
        EXIT;
    result := fCoreClient.RemoteFunctionCaller.CallIntFunc(cCoreControllerCallIDGetPendingControlEventID);
end;

function TTCPIPCoreControllerClientCalls.GetErrorBoxEvent(out oErrorID: integer): boolean;
begin
    result := false;
    if not fCoreClient.IsActive then
        EXIT;
    fCoreClient.RemoteFunctionCaller.Call(cCoreControllerCallIDGetErrorBoxEvent, [], [@result, @oErrorID],
        [result, oErrorID]);
end;

function TTCPIPCoreControllerClientCalls.GetErrorBoxAdvancedEvent(out oErrorID: integer;
    out oErrorInfo: TObject): boolean;
var
    xRemoteResult: TRemoteResultControllerGetErrorBoxAdvanced;
begin
    result := false;
    if not fCoreClient.IsActive then
        EXIT;
    xRemoteResult := TRemoteResultControllerGetErrorBoxAdvanced
        (fCoreClient.RemoteFunctionCaller.CallFunc<TRemoteResultControllerGetErrorBoxAdvanced>
        (TRemoteCallControllerGetErrorBoxAdvanced.Create()));
    oErrorID := xRemoteResult.ErrorID;
    oErrorInfo := xRemoteResult.ErrorInfo;
    result := xRemoteResult.ResultSuccess;
end;

procedure TTCPIPCoreControllerClientCalls.SetErrorBoxEventHandled(const aResultModal: integer);
begin
    if not fCoreClient.IsActive then
        EXIT;
    fCoreClient.RemoteFunctionCaller.CallProc(cCoreControllerCallIDSetErrorBoxEventHandled, [aResultModal]);
end;

function TTCPIPCoreControllerClientCalls.GetMessageBoxEvent(out oText, oCaption: string;
    out oType: integer): boolean;
begin
    fCoreClient.RemoteFunctionCaller.Call(cCoreControllerCallIDGetMessageBoxEvent, [],
        [@result, @oText, @oCaption, @oType], [result, oText, oCaption, oType]);
end;

procedure TTCPIPCoreControllerClientCalls.SetMessageBoxEventHandled(const aResultModal: integer);
begin
    if not fCoreClient.IsActive then
        EXIT;
    fCoreClient.RemoteFunctionCaller.CallProc(cCoreControllerCallIDSetMessageBoxEventHandled, [aResultModal]);
end;

function TTCPIPCoreControllerClientCalls.GetAskRunStartEvent(out oMethodName: string;
    out oIsSimChangeAllowed, oIsSim: boolean; out oSimulationAskWeight: boolean;
    out oSimulationSpeed_Percent: integer): boolean;
begin
    fCoreClient.RemoteFunctionCaller.Call(cCoreControllerCallIDGetAskRunStartEvent, [],
        [@result, @oMethodName, @oIsSimChangeAllowed, @oIsSim],
        [result, oMethodName, oIsSimChangeAllowed, oIsSim]);
end;

procedure TTCPIPCoreControllerClientCalls.SetAskRunStartEventHandled(const aResultModal: integer;
    const aResultIsSim: boolean; const aSimulationAskWeight: boolean;
    const aSimulationSpeed_Percent: integer);
begin
    if not fCoreClient.IsActive then
        EXIT;
    fCoreClient.RemoteFunctionCaller.CallProc(cCoreControllerCallIDSetAskRunStartEventHandled,
        [aResultModal, aResultIsSim, aSimulationAskWeight, aSimulationSpeed_Percent]);
end;

function TTCPIPCoreControllerClientCalls.GetLoadDisplayComponentEvent(out oDisplayComponentName: string;
    out oContextID: string): boolean;
begin
    fCoreClient.RemoteFunctionCaller.Call(cCoreControllerCallIDGetLoadDisplayComponentEvent, [],
        [@result, @oDisplayComponentName, @oContextID], [result, oDisplayComponentName, oContextID]);
end;

procedure TTCPIPCoreControllerClientCalls.SetLoadDisplayComponentEventHandled(const aResultSuccess: boolean);
begin
    if not fCoreClient.IsActive then
        EXIT;
    fCoreClient.RemoteFunctionCaller.CallProc(cCoreControllerCallIDSetLoadDisplayComponentEventHandled,
        [aResultSuccess]);
end;

function TTCPIPCoreControllerClientCalls.GetProcessStartedEvent(out oSourceDataName: string;
    out oProcessID: string): boolean;
begin
    fCoreClient.RemoteFunctionCaller.Call(cCoreControllerCallIDGetProcessStartedEvent, [],
        [@result, @oSourceDataName, @oProcessID], [result, oSourceDataName, oProcessID]);
end;

procedure TTCPIPCoreControllerClientCalls.SetProcessStartedEventHandled(const aResultSuccess: boolean);
begin
    if not fCoreClient.IsActive then
        EXIT;
    fCoreClient.RemoteFunctionCaller.CallProc(cCoreControllerCallIDSetProcessStartedEventHandled,
        [aResultSuccess]);
end;

function TTCPIPCoreControllerClientCalls.GetProcessFinishedEvent(out oProcessID: string;
    out oIsError: boolean): boolean;
begin
    fCoreClient.RemoteFunctionCaller.Call(cCoreControllerCallIDGetProcessFinishedEvent, [],
        [@result, @oProcessID, @oIsError], [result, oProcessID, oIsError]);
end;

procedure TTCPIPCoreControllerClientCalls.SetProcessFinishedEventHandled(const aResultSuccess: boolean);
begin
    if not fCoreClient.IsActive then
        EXIT;
    fCoreClient.RemoteFunctionCaller.CallProc(cCoreControllerCallIDSetProcessFinishedEventHandled,
        [aResultSuccess]);
end;

function TTCPIPCoreControllerClientCalls.SetControlEventMask(const aIDs: TIntArray): boolean;
begin
    result := false;
    if not fCoreClient.IsActive then
        EXIT;
    fCoreClient.RemoteFunctionCaller.PrepareCall(cCoreControllerCallIDSetControlEventMask);
    fCoreClient.RemoteFunctionCaller.Params.Add(aIDs);
    fCoreClient.RemoteFunctionCaller.ExecuteCall();
    result := fCoreClient.RemoteFunctionCaller.FuncResult.FirstParam.AsBool;
end;


end.
