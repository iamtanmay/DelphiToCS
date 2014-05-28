{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  30.07.09 pk                                        TN4585.5    Initial Revision
  12.10.09 pk  TransmitRunInfoInsert                 TN4812      New
  04.11.09 pk                               	     TN4843   	Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.08.10 wl  MessageBox                            TN5112   Parameter neu aufgeteilt
  21.03.11 wl  AskRunStartPromptModal                TN5508   neue Parameter
  19.02.12 pk                                        TN5809     Bug fixes to CoreServer classes for V8
  20.04.12 wl  TStopPromptModalMessageInfo           TN5946   entfernt
  12.11.12 wl                                        TN6008   Log-Funktion angepasst
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreControllerEventManager;


interface


uses
    Generics.Collections,
    GeneralTypes,   MessageQueue,
    ThreadClasses,
    ErrorInfo,
    CoreEventTransmitter,
    LockHandle,
    CoreClientInfo;

type
    TErrorBoxMessageInfo = class(TMessageInfo)
    private
        fErrorID: integer;
        fErrorInfo: TErrorInfo;
        fResultModal: integer;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aErrorID: integer; const aErrorInfo: TErrorInfo;
            const aDefaultResultModal: integer);
        property ErrorID: integer read fErrorID;
        property ErrorInfo: TErrorInfo read fErrorInfo;
        property ResultModal: integer read fResultModal write fResultModal;
    end;

    TMessageBoxMessageInfo = class(TMessageInfo)
    private
        fText: string;
        fCaption: string;
        fButtons: integer;
        fIcon: integer;
        fDefaultButton: integer;
        fResultModal: integer;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aText, aCaption: string; aButtons, aIcon, aDefaultButton: integer;
            const aDefaultResultModal: integer);
        property Text: string read fText;
        property Caption: string read fCaption;
        property Buttons: integer read fButtons;
        property Icon: integer read fIcon;
        property DefaultButton: integer read fDefaultButton;
        property ResultModal: integer read fResultModal write fResultModal;
    end;

    TAskRunStartMessageInfo = class(TMessageInfo)
    private
        fMethName: string;
        fIsSimChangeAllowed: boolean;
        fResultIsSim: boolean;
        fResultModal: integer;
        fSimulationAskWeight: boolean;
        fSimulationSpeed_Percent: integer;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aMethName: string; const aIsSimChangeAllowed: boolean;
            const aResultIsSim: boolean; const aSimulationAskWeight: boolean;
            const aSimulationSpeed_Percent: integer; const aDefaultResultModal: integer);
        property MethName: string read fMethName;
        property IsSimChangeAllowed: boolean read fIsSimChangeAllowed;
        property ResultIsSim: boolean read fResultIsSim write fResultIsSim;
        property ResultModal: integer read fResultModal write fResultModal;
        property SimulationAskWeight: boolean read fSimulationAskWeight write fSimulationAskWeight;
        property SimulationSpeed_Percent: integer read fSimulationSpeed_Percent
            write fSimulationSpeed_Percent;
    end;

    {
      TLoadDisplayComponentMessageInfo = class( TMessageInfo )
      private
      fDisplayComponentName : string;
      fContextID : string;
      fResultSuccess : boolean;
      protected
      function GetMessageID: integer; override;
      public
      constructor Create( const aDisplayComponentName, aContextID: string; const aDefaultResultSuccess : boolean );
      property DisplayComponentName : string read fDisplayComponentName;
      property ContextID : string read fContextID;
      property ResultSuccess : boolean read fResultSuccess write fResultSuccess;
      end;
    }
    TProcessStartedMessageInfo = class(TMessageInfo)
    private
        fSourceDataName: string;
        fProcessID: string;
        fResultSuccess: boolean;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aSourceDataName, aProcessID: string; const aDefaultResultSuccess: boolean);
        property SourceDataName: string read fSourceDataName;
        property ProcessID: string read fProcessID;
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TProcessFinishedMessageInfo = class(TMessageInfo)
    private
        fProcessID: string;
        fIsError: boolean;
        fResultSuccess: boolean;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aProcessID: string; const aIsError: boolean;
            const aDefaultResultSuccess: boolean);
        property ProcessID: string read fProcessID;
        property IsError: boolean read fIsError;
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TCoreControllerEventManager = class
    private
        class var uInstance: TCoreControllerEventManager;
        procedure RegisterMessageWithLock(aMessageInfo: TMessageInfo; aLock: TLock;
            aOwnsMessageInfo: boolean);
        procedure RegisterMessageWithQueueItem(const aItem: TMessageQueueItem);
        procedure WaitOnLockTillMessageHandled(aLock: TLock; aMaxWaitTime: cardinal);
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
        procedure TransmitControlEventPending();
        function GetPendingControlEventMessageQueueItem: TMessageQueueItem;
        function ControlEventMaskContainsMessageID(const aMessageID: integer): boolean;
        procedure ControlEventMaskChanged();
    protected
        fControlEventMask: TList<integer>;
        fControlEventMessageQueue: TMaskedMessageQueue;
        fCoreClients: TCoreClientInfoList;
    public
        constructor Create();
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstace();
        class function Instance(): TCoreControllerEventManager;
        procedure RegisterMessageAndWait(aMessageInfo: TMessageInfo; aOwnsMessageInfo: boolean;
            aMaxWaitTime: cardinal);

        function SetControlEventMask(const aIDs: TIntArray): boolean;

        function GetPendingControlEventID(): integer;
        function IsControlEventPending: boolean;
        function GetPendingControlEventMessageInfo: TMessageInfo;

        function GetErrorBoxEvent(out oErrorID: integer): boolean;
        function GetErrorBoxAdvancedEvent(out oErrorID: integer; out oErrorInfo: TObject): boolean;
        function GetMessageBoxEvent(out oText, oCaption: string;
            out oButtons, oIcon, oDefaultButton: integer): boolean;
        function GetAskRunStartEvent(out oMethodName: string; out oIsSimChangeAllowed: boolean;
            out oIsSim: boolean; out oSimulationAskWeight: boolean;
            out oSimulationSpeed_Percent: integer): boolean;
        // function GetLoadDisplayComponentEvent(out oDisplayComponentName, oContextID: string ): boolean;
        function GetProcessStartedEvent(out oSourceDataName, oProcessID: string): boolean;
        function GetProcessFinishedEvent(out oProcessID: string; out oIsError: boolean): boolean;

        procedure SetErrorBoxEventHandled(const aResultModal: integer);
        procedure SetMessageBoxEventHandled(const aResultModal: integer);
        procedure SetAskRunStartEventHandled(const aResultModal: integer; const aResultIsSim: boolean;
            const aSimulationAskWeight: boolean; const aSimulationSpeed_Percent: integer);
        // procedure SetLoadDisplayComponentEventHandled( const aResultSuccess : boolean );
        procedure SetProcessStartedEventHandled(const aResultSuccess: boolean);
        procedure SetProcessFinishedEventHandled(const aResultSuccess: boolean);

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
        procedure TransmitRunInfoInsert(const aDisplayID: string; const aGroupNames: TStringArray;
            const aKey, aText: string; aInfoGroupBehaviour: integer);
        procedure TransmitWriteLogEvent(const aLogText: string; aThreadID: integer; aDisplayType: integer);
        procedure TransmitLoadDisplayComponentEvent(const aDisplayComponentName: string;
            const aContextID: string);
        property CoreClients: TCoreClientInfoList read fCoreClients write fCoreClients;
    end;


implementation


uses
    Windows,
    RemoteMessageConstants;

{ TCoreControllerEventManager }
procedure TCoreControllerEventManager.TransmitControlEventPending();
var
    x: integer;
    xClientInfo: TCoreClientInfo;
    xEventTransmitter: TCoreEventTransmitter;
    xControllerClientInfo: TCoreClientInfo;
begin
    xControllerClientInfo := nil;
    for x := 0 to fCoreClients.Count - 1 do
    begin
        xClientInfo := fCoreClients[x];
        if xClientInfo.IsController then
        begin
            xControllerClientInfo := xClientInfo;
            BREAK;
        end;
    end;
    if not Assigned(xControllerClientInfo) then
        EXIT;
    if not xControllerClientInfo.IsEventHost then
        EXIT;

    xEventTransmitter := xControllerClientInfo.EventTransmitter;
    ASSERT(Assigned(xEventTransmitter));
    xEventTransmitter.ControlEventPending();

end;

procedure TCoreControllerEventManager.RegisterMessageWithQueueItem(const aItem: TMessageQueueItem);
begin
    fControlEventMessageQueue.EnqueueMessage(aItem);
    TransmitControlEventPending();
end;

procedure TCoreControllerEventManager.RegisterMessageWithLock(aMessageInfo: TMessageInfo; aLock: TLock;
    aOwnsMessageInfo: boolean);
begin
    RegisterMessageWithQueueItem(TMessageQueueItem.Create(aMessageInfo, aLock, false, MessageProc,
        aOwnsMessageInfo));
end;

procedure TCoreControllerEventManager.WaitOnLockTillMessageHandled(aLock: TLock; aMaxWaitTime: cardinal);
begin
    aLock.WaitForLock(aMaxWaitTime);
end;

procedure TCoreControllerEventManager.RegisterMessageAndWait(aMessageInfo: TMessageInfo;
    aOwnsMessageInfo: boolean; aMaxWaitTime: cardinal);
var
    xLock: TLock;
begin
    if not ControlEventMaskContainsMessageID(aMessageInfo.MessageID) then
    begin
        EXIT;
    end;

    xLock := TSimpleLock.Create(true, false, false);
    try
        RegisterMessageWithLock(aMessageInfo, xLock, aOwnsMessageInfo);
        WaitOnLockTillMessageHandled(xLock, aMaxWaitTime);
    finally
        xLock.Free;
    end
end;

procedure TCoreControllerEventManager.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
end;

function TCoreControllerEventManager.MessageBox(const aText, aCaption: string;
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

function TCoreControllerEventManager.AskRunStartPromptModal(const aMethodName: string;
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
    finally
        xMessageInfo.Free;
    end;
end;

function TCoreControllerEventManager.ErrorBox(const aErrorID: integer; const aErrorInfo: TErrorInfo;
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
  function TCoreControllerEventManager.LoadDisplayComponent( const aDisplayComponentName: string; const aContextID : string;
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
function TCoreControllerEventManager.ProcessStarted(const aSourceDataName, aProcessID: string;
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

procedure TCoreControllerEventManager.ProcessFinished(const aProcessID: string; const aIsError: boolean;
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

constructor TCoreControllerEventManager.Create;
begin
    inherited Create();
    fControlEventMessageQueue := TMaskedMessageQueue.Create();
    fControlEventMessageQueue.SetMask([cControlEventIDErrorBox, cControlEventIDMessageBox,
        cControlEventIDAskRunStart, cControlEventIDProcessStarted, cControlEventIDProcessFinished]);
    fControlEventMask := TList<integer>.Create();

end;

destructor TCoreControllerEventManager.Destroy;
begin
    fControlEventMask.Free;
    fControlEventMessageQueue.Free;
    inherited;
end;

class procedure TCoreControllerEventManager.CreateInstance;
begin
    uInstance := TCoreControllerEventManager.Create();
end;

class procedure TCoreControllerEventManager.DestroyInstace;
begin
    uInstance.Free;
end;

class function TCoreControllerEventManager.Instance: TCoreControllerEventManager;
begin
    result := uInstance;
end;

function TCoreControllerEventManager.GetPendingControlEventMessageQueueItem: TMessageQueueItem;
begin
    result := fControlEventMessageQueue.Next;
end;

function TCoreControllerEventManager.GetPendingControlEventMessageInfo: TMessageInfo;
var
    xMessageQueueItem: TMessageQueueItem;
begin
    result := nil;
    xMessageQueueItem := GetPendingControlEventMessageQueueItem;
    if not Assigned(xMessageQueueItem) then
        EXIT;
    result := xMessageQueueItem.MessageInfo
end;

function TCoreControllerEventManager.IsControlEventPending: boolean;
begin
    result := Assigned(GetPendingControlEventMessageQueueItem());
end;

procedure TCoreControllerEventManager.ControlEventMaskChanged();
var
    x: integer;
    xMessageQueuItem: TMessageQueueItem;
begin
    for x := 0 to self.fControlEventMessageQueue.Count - 1 do
    begin
        xMessageQueuItem := fControlEventMessageQueue.GetItem(x);
        if not ControlEventMaskContainsMessageID(xMessageQueuItem.MessageInfo.MessageID) then
        begin
            fControlEventMessageQueue.HandleMessage(xMessageQueuItem);
        end;
    end;
end;

function TCoreControllerEventManager.SetControlEventMask(const aIDs: TIntArray): boolean;
var
    x: integer;
begin

    fControlEventMask.Clear();
    for x := 0 to high(aIDs) do
    begin
        fControlEventMask.Add(aIDs[x]);
    end;
    result := true;

    ControlEventMaskChanged();
end;

function TCoreControllerEventManager.GetPendingControlEventID(): integer;
var
    xMessageQueueItem: TMessageQueueItem;
begin

    result := cControlEventIDNone;
    // only the controller can ask for pending events
    // if not xClientRun.IsControllerClientID( aClientID ) then EXIT;

    xMessageQueueItem := GetPendingControlEventMessageQueueItem();
    if not Assigned(xMessageQueueItem) then
        EXIT;
    result := xMessageQueueItem.MessageInfo.MessageID;
end;

function TCoreControllerEventManager.GetErrorBoxEvent(out oErrorID: integer): boolean;
var
    xMessageInfo: TMessageInfo;
begin
    result := false;
    xMessageInfo := GetPendingControlEventMessageInfo();
    if not(xMessageInfo is TErrorBoxMessageInfo) then
        EXIT;
    with xMessageInfo as TErrorBoxMessageInfo do
    begin
        oErrorID := ErrorID;
    end;
    result := true;
end;

function TCoreControllerEventManager.GetErrorBoxAdvancedEvent(out oErrorID: integer;
    out oErrorInfo: TObject): boolean;
var
    xMessageInfo: TMessageInfo;
begin
    result := false;
    xMessageInfo := GetPendingControlEventMessageInfo();
    if not(xMessageInfo is TErrorBoxMessageInfo) then
        EXIT;
    with xMessageInfo as TErrorBoxMessageInfo do
    begin
        oErrorID := ErrorID;
        oErrorInfo := ErrorInfo;
    end;
    result := true;
end;

function TCoreControllerEventManager.GetMessageBoxEvent(out oText, oCaption: string;
    out oButtons, oIcon, oDefaultButton: integer): boolean;
var
    xMessageInfo: TMessageInfo;
begin
    result := false;
    xMessageInfo := GetPendingControlEventMessageInfo();
    if not(xMessageInfo is TMessageBoxMessageInfo) then
        EXIT;
    with xMessageInfo as TMessageBoxMessageInfo do
    begin
        oText := Text;
        oCaption := Caption;
        oButtons := Buttons;
        oIcon := Icon;
        oDefaultButton := DefaultButton;
    end;
    result := true;
end;

function TCoreControllerEventManager.GetAskRunStartEvent(out oMethodName: string;
    out oIsSimChangeAllowed: boolean; out oIsSim: boolean; out oSimulationAskWeight: boolean;
    out oSimulationSpeed_Percent: integer): boolean;
var
    xMessageInfo: TMessageInfo;
begin
    result := false;
    xMessageInfo := GetPendingControlEventMessageInfo();
    if not(xMessageInfo is TAskRunStartMessageInfo) then
        EXIT;
    with xMessageInfo as TAskRunStartMessageInfo do
    begin
        oMethodName := MethName;
        oIsSimChangeAllowed := IsSimChangeAllowed;
        oIsSim := ResultIsSim;
        oSimulationAskWeight := SimulationAskWeight;
        oSimulationSpeed_Percent := SimulationSpeed_Percent;
    end;
    result := true;
end;

{
  function TCoreControllerEventManager.GetLoadDisplayComponentEvent( out oDisplayComponentName, oContextID: string ): boolean;
  var
  xMessageInfo : TMessageInfo;
  begin
  result := false;
  xMessageInfo := GetPendingControlEventMessageInfo();
  if not ( xMessageInfo is TLoadDisplayComponentMessageInfo ) then EXIT;
  with xMessageInfo as TLoadDisplayComponentMessageInfo do begin
  oDisplayComponentName := DisplayComponentName;
  oContextID := ContextID;
  end;
  result := true;
  end;
}
function TCoreControllerEventManager.GetProcessStartedEvent(out oSourceDataName, oProcessID: string): boolean;
var
    xMessageInfo: TMessageInfo;
begin
    result := false;
    xMessageInfo := GetPendingControlEventMessageInfo();
    if not(xMessageInfo is TProcessStartedMessageInfo) then
        EXIT;
    with xMessageInfo as TProcessStartedMessageInfo do
    begin
        oSourceDataName := SourceDataName;
        oProcessID := ProcessID;
    end;
    result := true;
end;

function TCoreControllerEventManager.GetProcessFinishedEvent(out oProcessID: string;
    out oIsError: boolean): boolean;
var
    xMessageInfo: TMessageInfo;
begin
    result := false;
    xMessageInfo := GetPendingControlEventMessageInfo();
    if not(xMessageInfo is TProcessFinishedMessageInfo) then
        EXIT;
    with xMessageInfo as TProcessFinishedMessageInfo do
    begin
        oProcessID := ProcessID;
        oIsError := IsError;
    end;
    result := true;
end;

procedure TCoreControllerEventManager.SetErrorBoxEventHandled(const aResultModal: integer);
var
    xMessageQueueItem: TMessageQueueItem;
begin
    xMessageQueueItem := GetPendingControlEventMessageQueueItem();
    ASSERT(Assigned(xMessageQueueItem) and (xMessageQueueItem.MessageInfo is TErrorBoxMessageInfo));
    (xMessageQueueItem.MessageInfo as TErrorBoxMessageInfo).ResultModal := aResultModal;
    fControlEventMessageQueue.HandleMessage(xMessageQueueItem);
end;

procedure TCoreControllerEventManager.SetMessageBoxEventHandled(const aResultModal: integer);
var
    xMessageQueueItem: TMessageQueueItem;
begin
    xMessageQueueItem := GetPendingControlEventMessageQueueItem();
    ASSERT(Assigned(xMessageQueueItem) and (xMessageQueueItem.MessageInfo is TMessageBoxMessageInfo));
    (xMessageQueueItem.MessageInfo as TMessageBoxMessageInfo).ResultModal := aResultModal;
    fControlEventMessageQueue.HandleMessage(xMessageQueueItem);
end;

procedure TCoreControllerEventManager.SetAskRunStartEventHandled(const aResultModal: integer;
    const aResultIsSim: boolean; const aSimulationAskWeight: boolean;
    const aSimulationSpeed_Percent: integer);
var
    xMessageQueueItem: TMessageQueueItem;
begin
    xMessageQueueItem := GetPendingControlEventMessageQueueItem();
    ASSERT(Assigned(xMessageQueueItem) and (xMessageQueueItem.MessageInfo is TAskRunStartMessageInfo));
    (xMessageQueueItem.MessageInfo as TAskRunStartMessageInfo).ResultModal := aResultModal;
    (xMessageQueueItem.MessageInfo as TAskRunStartMessageInfo).ResultIsSim := aResultIsSim;
    (xMessageQueueItem.MessageInfo as TAskRunStartMessageInfo).SimulationAskWeight := aSimulationAskWeight;
    (xMessageQueueItem.MessageInfo as TAskRunStartMessageInfo).SimulationSpeed_Percent :=
        aSimulationSpeed_Percent;
    fControlEventMessageQueue.HandleMessage(xMessageQueueItem);
end;

{
  procedure TCoreControllerEventManager.SetLoadDisplayComponentEventHandled( const aResultSuccess : boolean );
  var
  xMessageQueueItem : TMessageQueueItem;
  begin
  xMessageQueueItem := GetPendingControlEventMessageQueueItem();
  ASSERT( Assigned( xMessageQueueItem ) and ( xMessageQueueItem.MessageInfo is TLoadDisplayComponentMessageInfo ) );
  ( xMessageQueueItem.MessageInfo as TLoadDisplayComponentMessageInfo ).ResultSuccess := aResultSuccess;
  fControlEventMessageQueue.HandleMessage( xMessageQueueItem );
  end;

}
procedure TCoreControllerEventManager.SetProcessStartedEventHandled(const aResultSuccess: boolean);
var
    xMessageQueueItem: TMessageQueueItem;
begin
    xMessageQueueItem := GetPendingControlEventMessageQueueItem();
    ASSERT(Assigned(xMessageQueueItem) and (xMessageQueueItem.MessageInfo is TProcessStartedMessageInfo));
    (xMessageQueueItem.MessageInfo as TProcessStartedMessageInfo).ResultSuccess := aResultSuccess;
    fControlEventMessageQueue.HandleMessage(xMessageQueueItem);
end;

procedure TCoreControllerEventManager.SetProcessFinishedEventHandled(const aResultSuccess: boolean);
var
    xMessageQueueItem: TMessageQueueItem;
begin
    xMessageQueueItem := GetPendingControlEventMessageQueueItem();
    ASSERT(Assigned(xMessageQueueItem) and (xMessageQueueItem.MessageInfo is TProcessFinishedMessageInfo));
    (xMessageQueueItem.MessageInfo as TProcessFinishedMessageInfo).ResultSuccess := aResultSuccess;
    fControlEventMessageQueue.HandleMessage(xMessageQueueItem);
end;

procedure TCoreControllerEventManager.TransmitWriteLogEvent(const aLogText: string; aThreadID: integer;
    aDisplayType: integer);
var
    x: integer;
    xClientInfo: TCoreClientInfo;
    xEventTransmitter: TCoreEventTransmitter;

begin
    for x := 0 to fCoreClients.Count - 1 do
    begin
        xClientInfo := fCoreClients[x];
        if not xClientInfo.IsEventHost then
            CONTINUE;
        xEventTransmitter := xClientInfo.EventTransmitter;
        ASSERT(Assigned(xEventTransmitter));
        xEventTransmitter.TransmitWriteLogEvent(aLogText, aThreadID, aDisplayType);
    end;

end;

procedure TCoreControllerEventManager.TransmitRunInfoInsert(const aDisplayID: string;
    const aGroupNames: TStringArray; const aKey, aText: string; aInfoGroupBehaviour: integer);
var
    x: integer;
    xClientInfo: TCoreClientInfo;
    xEventTransmitter: TCoreEventTransmitter;

begin
    for x := 0 to fCoreClients.Count - 1 do
    begin
        xClientInfo := fCoreClients[x];
        if not xClientInfo.IsEventHost then
            CONTINUE;
        xEventTransmitter := xClientInfo.EventTransmitter;
        ASSERT(Assigned(xEventTransmitter));
        xEventTransmitter.TransmitRunInfoInsert(aDisplayID, aGroupNames, aKey, aText, aInfoGroupBehaviour);
    end;
end;

procedure TCoreControllerEventManager.TransmitLoadDisplayComponentEvent(const aDisplayComponentName: string;
    const aContextID: string);
var
    x: integer;
    xClientInfo: TCoreClientInfo;
    xEventTransmitter: TCoreEventTransmitter;
begin
    for x := 0 to fCoreClients.Count - 1 do
    begin
        xClientInfo := fCoreClients[x];
        if not xClientInfo.IsEventHost then
            CONTINUE;
        xEventTransmitter := xClientInfo.EventTransmitter;
        ASSERT(Assigned(xEventTransmitter));
        xEventTransmitter.TransmitLoadDisplayComponentEvent(aDisplayComponentName, aContextID);
    end;
end;

function TCoreControllerEventManager.ControlEventMaskContainsMessageID(const aMessageID: integer): boolean;
begin
    result := fControlEventMask.IndexOf(aMessageID) >= 0;
end;

{ TErrorBoxMessageInfo }

constructor TErrorBoxMessageInfo.Create(const aErrorID: integer; const aErrorInfo: TErrorInfo;
    const aDefaultResultModal: integer);
begin
    inherited Create();
    fErrorID := aErrorID;
    fErrorInfo := aErrorInfo;
    fResultModal := aDefaultResultModal;
end;

function TErrorBoxMessageInfo.GetMessageID: integer;
begin
    result := cControlEventIDErrorBox;
end;
{ TMessageBoxMessageInfo }

constructor TMessageBoxMessageInfo.Create(const aText, aCaption: string;
    aButtons, aIcon, aDefaultButton: integer; const aDefaultResultModal: integer);
begin
    inherited Create();
    fText := aText;
    fCaption := aCaption;
    fButtons := aButtons;
    fIcon := aIcon;
    fDefaultButton := aDefaultButton;
    fResultModal := aDefaultResultModal;
end;

function TMessageBoxMessageInfo.GetMessageID: integer;
begin
    result := cControlEventIDMessageBox;
end;

constructor TAskRunStartMessageInfo.Create(const aMethName: string; const aIsSimChangeAllowed: boolean;
    const aResultIsSim: boolean; const aSimulationAskWeight: boolean; const aSimulationSpeed_Percent: integer;
    const aDefaultResultModal: integer);
begin
    inherited Create();
    fMethName := aMethName;
    fIsSimChangeAllowed := aIsSimChangeAllowed;
    fResultIsSim := aResultIsSim;
    fResultModal := aDefaultResultModal;
    fSimulationAskWeight := aSimulationAskWeight;
    fSimulationSpeed_Percent := aSimulationSpeed_Percent;
end;

function TAskRunStartMessageInfo.GetMessageID: integer;
begin
    result := cControlEventIDAskRunStart;
end;
// { TLoadDisplayComponentMessageInfo }
// constructor TLoadDisplayComponentMessageInfo.Create( const aDisplayComponentName, aContextID: string; const aDefaultResultSuccess : boolean );
// begin
// inherited Create();
// fDisplayComponentName := aDisplayComponentName;
// fContextID := aContextID;
// fResultSuccess := aDefaultResultSuccess;
// end;
//
// function TLoadDisplayComponentMessageInfo.GetMessageID: integer;
// begin
// result := cControlEventIDLoadDisplayComponent;
// end;

{ TProcessStartedMessageInfo }

constructor TProcessStartedMessageInfo.Create(const aSourceDataName, aProcessID: string;
    const aDefaultResultSuccess: boolean);
begin
    inherited Create();
    fSourceDataName := aSourceDataName;
    fProcessID := aProcessID;
    fResultSuccess := aDefaultResultSuccess;
end;

function TProcessStartedMessageInfo.GetMessageID: integer;
begin
    result := cControlEventIDProcessStarted;
end;

{ TProcessFinishedMessageInfo }

constructor TProcessFinishedMessageInfo.Create(const aProcessID: string; const aIsError: boolean;
    const aDefaultResultSuccess: boolean);
begin
    inherited Create();
    fProcessID := aProcessID;
    fIsError := aIsError;
    fResultSuccess := aDefaultResultSuccess;
end;

function TProcessFinishedMessageInfo.GetMessageID: integer;
begin
    result := cControlEventIDProcessFinished;
end;


end.
