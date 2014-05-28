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
  30.07.09 pk                                        TN4585.5    Various Changes
  12.10.09 pk  TransmitRunInfoInsert                 TN4812      New
  17.08.10 wl  MessageBox                            TN5112   Parameter neu aufgeteilt
  21.03.11 wl  AskRunStartPromptModal                TN5508   neue Parameter
  01.03.12 wl                                        TN5822   uses geändert
  20.04.12 wl  StopPromptModal                       TN5946   entfernt
  12.11.12 wl                                        TN6008   Log-Funktion angepasst
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreServerEventManager;


interface


uses
    GeneralTypes,
    ThreadClasses,
    CoreClientInfo,
    CoreEventTransmitter,
    RemoteMessageConstants,
    ErrorInfo;

type
    TCoreServerEventManager = class
    private
        class var uInstance: TCoreServerEventManager;
        procedure RegisterMessageAndWait(aMessageInfo: TMessageInfo; aOwnsMessageInfo: boolean;
            aMaxWaitTime: cardinal);
    public
        constructor Create();
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstace();
        class function Instance(): TCoreServerEventManager;

        function ErrorBox(const aErrorID: integer; const aErrorInfo: TErrorInfo;
            const aDefaultResultModal: integer): integer;
        function MessageBox(const aText, aCaption: string; aButtons, aIcon, aDefaultButton: integer;
            const aDefaultResultModal: integer): integer;
        function AskRunStartPromptModal(const aMethodName: string; const aIsSimChangeAllowed: boolean;
            var vIsSim: boolean; var vSimulationAskWeight: boolean; var vSimulationSpeed_Percent: integer;
            const aDefaultResultModal: integer): integer;

        function ProcessStarted(const aSourceDataName: string; const aProcessID: string;
            const aDefaultResultSuccess: boolean): boolean;
        procedure ProcessFinished(const aProcessID: string; const aIsError: boolean;
            const aDefaultResultSuccess: boolean);

        procedure TransmitWriteLogEvent(const aLogText: string; const aThreadID: integer;
            const aDisplayType: integer);
        procedure TransmitLoadDisplayComponentEvent(const aDisplayComponentName: string;
            const aContextID: string);
        procedure TransmitLinkProcessToRunIDEvent(const aRunID, aProcessID: string);
        procedure TransmitRunInfoInsert(const aDisplayID: string; const aGroupNames: TStringArray;
            const aKey, aText: string; aInfoGroupBehaviour: integer);
    end;


implementation


uses
    Windows,
    CoreControllerEventManager,
    CoreClientInfoManager,
    CoreClientRun;

{ TCoreServerEventManager }

procedure TCoreServerEventManager.RegisterMessageAndWait(aMessageInfo: TMessageInfo;
    aOwnsMessageInfo: boolean; aMaxWaitTime: cardinal);
var
    xClientRun: TCoreClientRun;
begin
    xClientRun := TCoreClientInfoManager.Instance.FindRunForCurrentThread();
    if not Assigned(xClientRun) then
        EXIT;
    xClientRun.ControllerEventManager.RegisterMessageAndWait(aMessageInfo, aOwnsMessageInfo, aMaxWaitTime);
end;

function TCoreServerEventManager.MessageBox(const aText, aCaption: string;
    aButtons, aIcon, aDefaultButton: integer; const aDefaultResultModal: integer): integer;
var
    xMessageInfo: TMessageBoxMessageInfo;
begin
    xMessageInfo := TMessageBoxMessageInfo.Create(aText, aCaption, aButtons, aIcon, aDefaultButton,
        aDefaultResultModal);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultModal;
    finally
        xMessageInfo.Free;
    end;
end;

function TCoreServerEventManager.AskRunStartPromptModal(const aMethodName: string;
    const aIsSimChangeAllowed: boolean; var vIsSim: boolean; var vSimulationAskWeight: boolean;
    var vSimulationSpeed_Percent: integer; const aDefaultResultModal: integer): integer;
var
    xMessageInfo: TAskRunStartMessageInfo;
begin
    xMessageInfo := TAskRunStartMessageInfo.Create(aMethodName, aIsSimChangeAllowed, vIsSim,
        vSimulationAskWeight, vSimulationSpeed_Percent, aDefaultResultModal);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultModal;
        vIsSim := xMessageInfo.ResultIsSim;
        vSimulationAskWeight := xMessageInfo.SimulationAskWeight;
        vSimulationSpeed_Percent := xMessageInfo.SimulationSpeed_Percent;
    finally
        xMessageInfo.Free;
    end;
end;

function TCoreServerEventManager.ErrorBox(const aErrorID: integer; const aErrorInfo: TErrorInfo;
    const aDefaultResultModal: integer): integer;
var
    xMessageInfo: TErrorBoxMessageInfo;
begin
    xMessageInfo := TErrorBoxMessageInfo.Create(aErrorID, aErrorInfo, aDefaultResultModal);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultModal;
    finally
        xMessageInfo.Free;
    end;
end;

{
  function TCoreServerEventManager.LoadDisplayComponent( const aDisplayComponentName: string; const aContextID : string;
  const aDefaultResultSuccess : boolean ) : boolean;
  var
  xMessageInfo : TLoadDisplayComponentMessageInfo;
  begin
  xMessageInfo := TLoadDisplayComponentMessageInfo.Create( aDisplayComponentName, aContextID, aDefaultResultSuccess );
  try
  self.RegisterMessageAndWait( xMessageInfo, false, INFINITE );
  result := xMessageInfo.ResultSuccess;
  finally
  xMessageInfo.Free;
  end;
  end;
}
function TCoreServerEventManager.ProcessStarted(const aSourceDataName, aProcessID: string;
    const aDefaultResultSuccess: boolean): boolean;
var
    xMessageInfo: TProcessStartedMessageInfo;
begin
    xMessageInfo := TProcessStartedMessageInfo.Create(aSourceDataName, aProcessID, aDefaultResultSuccess);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultSuccess;
    finally
        xMessageInfo.Free;
    end;
end;

procedure TCoreServerEventManager.ProcessFinished(const aProcessID: string; const aIsError: boolean;
    const aDefaultResultSuccess: boolean);
var
    xMessageInfo: TProcessFinishedMessageInfo;
begin
    xMessageInfo := TProcessFinishedMessageInfo.Create(aProcessID, aIsError, aDefaultResultSuccess);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
    finally
        xMessageInfo.Free;
    end;
end;

constructor TCoreServerEventManager.Create;
begin
    inherited Create();
end;

destructor TCoreServerEventManager.Destroy;
begin
    inherited;
end;

class procedure TCoreServerEventManager.CreateInstance;
begin
    uInstance := TCoreServerEventManager.Create();
end;

class procedure TCoreServerEventManager.DestroyInstace;
begin
    uInstance.Free;
end;

class function TCoreServerEventManager.Instance: TCoreServerEventManager;
begin
    result := uInstance;
end;

procedure TCoreServerEventManager.TransmitWriteLogEvent(const aLogText: string; const aThreadID: integer;
    const aDisplayType: integer);
var
    xClientRun: TCoreClientRun;
begin
    xClientRun := TCoreClientInfoManager.Instance.FindRunForCurrentThread();
    if not Assigned(xClientRun) then
        EXIT;
    xClientRun.ControllerEventManager.TransmitWriteLogEvent(aLogText, aThreadID, aDisplayType);
end;

procedure TCoreServerEventManager.TransmitLoadDisplayComponentEvent(const aDisplayComponentName: string;
    const aContextID: string);
{ var
  xClientRun : TCoreClientRun;
  begin
  xClientRun := TCoreClientInfoManager.Instance.FindRunForCurrentThread();
  if not Assigned( xClientRun ) then EXIT;
  xClientRun.ControllerEventManager.TransmitLoadDisplayComponentEvent( aDisplayComponentName, aContextID );
}
var
    x: integer;
    xClientInfo: TCoreClientInfo;
    xEventTransmitter: TCoreEventTransmitter;

begin
    for x := 0 to TCoreClientInfoManager.Instance.ClientInfos.Count - 1 do
    begin
        xClientInfo := TCoreClientInfoManager.Instance.ClientInfos[x];
        if not xClientInfo.IsEventHost then
            CONTINUE;
        xEventTransmitter := xClientInfo.EventTransmitter;
        ASSERT(Assigned(xEventTransmitter));
        xEventTransmitter.TransmitLoadDisplayComponentEvent(aDisplayComponentName, aContextID);
    end;
end;

procedure TCoreServerEventManager.TransmitLinkProcessToRunIDEvent(const aRunID, aProcessID: string);
var
    x: integer;
    xClientInfo: TCoreClientInfo;
    xEventTransmitter: TCoreEventTransmitter;

begin
    for x := 0 to TCoreClientInfoManager.Instance.ClientInfos.Count - 1 do
    begin
        xClientInfo := TCoreClientInfoManager.Instance.ClientInfos[x];
        if not xClientInfo.IsEventHost then
            CONTINUE;
        xEventTransmitter := xClientInfo.EventTransmitter;
        ASSERT(Assigned(xEventTransmitter));
        xEventTransmitter.TransmitLinkProcessToRunIDEvent(aRunID, aProcessID);
    end;

end;

procedure TCoreServerEventManager.TransmitRunInfoInsert(const aDisplayID: string;
    const aGroupNames: TStringArray; const aKey, aText: string; aInfoGroupBehaviour: integer);
var
    xClientRun: TCoreClientRun;
begin
    xClientRun := TCoreClientInfoManager.Instance.FindRunForCurrentThread();
    if not Assigned(xClientRun) then
        EXIT;
    xClientRun.ControllerEventManager.TransmitRunInfoInsert(aDisplayID, aGroupNames, aKey, aText,
        aInfoGroupBehaviour);
end;


end.
