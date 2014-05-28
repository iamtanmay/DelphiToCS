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
  30.07.09 pk                                        TN4585.5    Various changes
  12.10.09 pk  HandleMessage                         TN4812      New
  02.02.11 wl  MessageBox                            TN5466   Parameter geändert
  21.03.11 wl  AskRunStartPromptModal                TN5508   neue Parameter
  20.04.12 wl  StopPromptModalEvent                  TN5946   entfernt
  29.05.12 wl  LogTextDisplay                        TN5904   mit ThreadID und DisplayType als Parameter
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreEventHandler;


interface


uses
    GeneralTypes,
    LogManager,
    CoreEventMessageInfo;

type
    TCoreEventHandler = class
    private
        class var uInstanceCoreEventHandler: TCoreEventHandler;
        function HandleErrorBoxEvent: integer;
        function HandleMessageBoxEvent: integer;
        procedure HandleProcessStartedEvent;
        procedure HandleProcessFinishedEvent;
        function HandleAskRunStartEvent: integer;
    protected
        // control events
        function ProcessStarted(const aSourceDataName: string; const aProcessID: string): boolean;
            virtual; abstract;
        procedure ProcessFinished(const aProcessID: string; const aIsError: boolean); virtual; abstract;
        // procedure ProcessPaused(const aProcessID: string); virtual; abstract;
        function ErrorBox(const aErrorID: integer; const aErrorInfo: TObject): integer; virtual; abstract;
        function MessageBox(const aText: string; const aCaption: string = ''; aButtons: integer = 0;
            aIcon: integer = 0; aDefaultButton: integer = 0): integer; virtual; abstract;
        function AskRunStartPromptModal(const aMethodName: string; const aIsSimChangeAllowed: boolean;
            var vIsSim: boolean; var vSimulationAskWeight: boolean; var vSimulationSpeed_Percent: integer)
            : integer; virtual; abstract;

        // status events
        procedure RunInfoInsert(const aDisplayID: string; const aGroupNames: TStringArray;
            const aKey, aText: string; aInfoGroupBehaviour: integer); virtual; abstract;
        procedure LoadDisplayComponentToMain(const aDisplayComponentName: string; const aContextID: string);
            virtual; abstract;
        procedure LogTextDisplay(const aLogText: string; aThreadID: integer;
            aDisplayType: TDisplayLogInfoType); virtual; abstract;
        procedure LinkRunIDToProcess(const aRunID: string); virtual; abstract;
    public
        constructor Create();
        procedure HandleMessage(const aMessageInfo: TCoreEventMessageInfo);
        class procedure SetInstance(const aInstance: TCoreEventHandler);
        class procedure DestroyInstace();
        class function Instance(): TCoreEventHandler;

        procedure PerformControlEvent();
    end;


implementation


uses
    CoreControllerClientCalls,
    RemoteMessageConstants;

{ TCoreEventHandler }

constructor TCoreEventHandler.Create;
begin
    inherited Create();
end;

class procedure TCoreEventHandler.SetInstance(const aInstance: TCoreEventHandler);
begin
    uInstanceCoreEventHandler := aInstance;
end;

class procedure TCoreEventHandler.DestroyInstace;
begin
    uInstanceCoreEventHandler.Free;
end;

class function TCoreEventHandler.Instance: TCoreEventHandler;
begin
    result := uInstanceCoreEventHandler;
end;

function TCoreEventHandler.HandleErrorBoxEvent(): integer;
var
    xErrorID: integer;
    xErrorInfo: TObject;
begin
    ASSERT(TCoreControllerClientCalls.Instance.GetErrorBoxAdvancedEvent(xErrorID, xErrorInfo));
    result := ErrorBox(xErrorID, xErrorInfo);
    TCoreControllerClientCalls.Instance.SetErrorBoxEventHandled(result);
end;

function TCoreEventHandler.HandleMessageBoxEvent(): integer;
var
    xText, xCaption: string;
    xType: integer;
begin
    ASSERT(TCoreControllerClientCalls.Instance.GetMessageBoxEvent(xText, xCaption, xType));
    result := MessageBox(xText, xCaption, xType);
    TCoreControllerClientCalls.Instance.SetMessageBoxEventHandled(result);
end;

function TCoreEventHandler.HandleAskRunStartEvent(): integer;
var
    xMethodName: string;
    xIsSimChangeAllowed, xIsSim: boolean;
    xSimulationAskWeight: boolean;
    xSimulationSpeed_Percent: integer;
begin
    ASSERT(TCoreControllerClientCalls.Instance.GetAskRunStartEvent(xMethodName, xIsSimChangeAllowed, xIsSim,
        xSimulationAskWeight, xSimulationSpeed_Percent));
    result := AskRunStartPromptModal(xMethodName, xIsSimChangeAllowed, xIsSim, xSimulationAskWeight,
        xSimulationSpeed_Percent);
    TCoreControllerClientCalls.Instance.SetAskRunStartEventHandled(result, xIsSim, xSimulationAskWeight,
        xSimulationSpeed_Percent);
end;

procedure TCoreEventHandler.HandleProcessStartedEvent;
var
    xSourceDataName, xProcessID: string;
begin
    ASSERT(TCoreControllerClientCalls.Instance.GetProcessStartedEvent(xSourceDataName, xProcessID));
    ProcessStarted(xSourceDataName, xProcessID);
    TCoreControllerClientCalls.Instance.SetProcessStartedEventHandled(true);
end;

procedure TCoreEventHandler.HandleProcessFinishedEvent;
var
    xProcessID: string;
    xIsError: boolean;
begin
    ASSERT(TCoreControllerClientCalls.Instance.GetProcessFinishedEvent(xProcessID, xIsError));
    ProcessFinished(xProcessID, xIsError);
    TCoreControllerClientCalls.Instance.SetProcessFinishedEventHandled(true);
end;

procedure TCoreEventHandler.PerformControlEvent;
var
    xControlEventID: integer;
begin
    xControlEventID := TCoreControllerClientCalls.Instance.GetPendingControlEventID();
    if xControlEventID = cControlEventIDNone then
        EXIT;

    if xControlEventID = cControlEventIDErrorBox then
    begin
        HandleErrorBoxEvent();
    end
    else if xControlEventID = cControlEventIDMessageBox then
    begin
        HandleMessageBoxEvent();
    end
    else if xControlEventID = cControlEventIDAskRunStart then
    begin
        HandleAskRunStartEvent();
    end
    else if xControlEventID = cControlEventIDProcessStarted then
    begin
        HandleProcessStartedEvent();
    end
    else if xControlEventID = cControlEventIDProcessFinished then
    begin
        HandleProcessFinishedEvent();
    end;
end;

procedure TCoreEventHandler.HandleMessage(const aMessageInfo: TCoreEventMessageInfo);
begin
    if aMessageInfo is TCoreControlEventMessageInfo then
    begin
        PerformControlEvent();
    end
    else if aMessageInfo is TCoreWriteLogEventMessageInfo then
    begin
        with (aMessageInfo as TCoreWriteLogEventMessageInfo) do
            LogTextDisplay(LogText, ThreadID, DisplayType);
    end
    else if aMessageInfo is TCoreLoadDisplayComponentEventMessageInfo then
    begin
        with (aMessageInfo as TCoreLoadDisplayComponentEventMessageInfo) do
            LoadDisplayComponentToMain(DisplayComponentName, ContextID);
    end
    else if aMessageInfo is TCoreLinkRunIDToProcesstEventMessageInfo then
    begin
        with (aMessageInfo as TCoreLinkRunIDToProcesstEventMessageInfo) do
        begin
            LinkRunIDToProcess(RunID);
            {
              if not SameText( RunID, fRunID ) then begin
              self.RegisterController( RunID );
              end;
            }
        end;
    end
    else if aMessageInfo is TCoreRunInfoInsertEventMessageInfo then
    begin
        with aMessageInfo as TCoreRunInfoInsertEventMessageInfo do
            RunInfoInsert(DisplayID, GroupNames, Key, Text, InfoGroupBehaviour);
    end;
end;


end.
