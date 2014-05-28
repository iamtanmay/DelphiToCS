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
  30.07.09 pk                                        TN4585.5    Various Changes
  24.08.09 pk  StartMethod                           TN4735.3    now with try except
  14.12.11 wl  StartMethodIntern                     TN5765   StartMethod-Aufruf geändert
  19.02.12 pk                                        TN5809.1  Changes for ZACoreClientX dll
  30.07.13 wl                                        TN6160   an TSystemEvents angepasst
  30.08.13 wl                                        TN6236   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreServerCallHandler;


interface


uses
    GeneralTypes,
    MessagableExecutable,
    MessageQueue,
    ThreadClasses;

type
    TStartMethodMessageInfo = class(TMessageInfo)
    protected
        fMethName: string;
        fDontShowMessages: boolean;
        fClientGroupID: string;
        fResultSuccess: boolean;
        function GetMessageID: integer; override;
    public
        constructor Create(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientGroupID: string);
        property MethName: string read fMethName;
        property DontShowMessages: boolean read fDontShowMessages;
        property ClientGroupID: string read fClientGroupID;
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TSimulateMethodMessageInfo = class(TMessageInfo)
    protected
        fMethName: string;
        fDontShowMessages: boolean;
        fClientGroupID: string;
        fResultSuccess: boolean;
        function GetMessageID: integer; override;
    public
        constructor Create(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientGroupID: string);
        property MethName: string read fMethName;
        property DontShowMessages: boolean read fDontShowMessages;
        property ClientGroupID: string read fClientGroupID;
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TInterruptStartMessageInfo = class(TMessageInfo)
    protected
        fInterruptText: string;
        fResultSuccess: boolean;
        function GetMessageID: integer; override;
    public
        constructor Create(const aInterruptText: string);
        property InterruptText: string read fInterruptText;
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TInterruptFinishMessageInfo = class(TMessageInfo)
    protected
        fInterruptText: string;
        fResultSuccess: boolean;
        function GetMessageID: integer; override;
    public
        constructor Create(const aInterruptText: string);
        property InterruptText: string read fInterruptText;
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TUserStopInterruptMessageInfo = class(TMessageInfo)
    protected
        fInterruptText: string;
        fResultSuccess: boolean;
        function GetMessageID: integer; override;
    public
        constructor Create(const aInterruptText: string);
        property InterruptText: string read fInterruptText;
        property ResultSuccess: boolean read fResultSuccess write fResultSuccess;
    end;

    TSetGlobalErrorMessageInfo = class(TMessageInfo)
    private
        fErrorText: string;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aErrorText: string);
        property ErrorText: string read fErrorText;
    end;

    TRegisterEventHostMessageInfo = class(TMessageInfo)
    private
        fClientID: string;
        fAddress: string;
        fPort: integer;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aClientID: string; const aAddress: string; const aPort: integer);
        property ClientID: string read fClientID write fClientID;
        property Address: string read fAddress write fAddress;
        property Port: integer read fPort write fPort;
    end;

    TUnRegisterEventHostMessageInfo = class(TMessageInfo)
    private
        fClientID: string;
    protected
        function GetMessageID: integer; override;
    public
        constructor Create(const aClientID: string);
        property ClientID: string read fClientID write fClientID;
    end;

    TCoreServerCallHandler = class(TMessagableExecutable)
    private
        procedure RegisterEventHostIntern(const aClientID, aAddress: string; const aPort: integer);
        procedure UnRegisterEventHostIntern(const aClientID: string);
        function InterruptStartIntern(const aInterruptText: string): boolean;
        function InterruptFinishIntern(const aInterruptText: string): boolean;
        function StartMethodIntern(const aMethodName: string; const aDontShowMessages: boolean;
            const aSimulate: boolean; const aClientGroupID: string): boolean;
        procedure SetGlobalErrorIntern(const aErrorText: string);

        procedure HandleMessageStartMethod(const aMessage: TStartMethodMessageInfo);
        procedure HandleMessageSimulateMethod(const aMessage: TSimulateMethodMessageInfo);
        procedure HandleMessageInterruptStart(const aMessage: TInterruptStartMessageInfo);
        procedure HandleMessageInterruptFinish(const aMessage: TInterruptFinishMessageInfo);
        procedure HandleMessageSetGlobalError(const aMessage: TSetGlobalErrorMessageInfo);
        procedure HandleMessageRegisterEventHost(const aMessage: TRegisterEventHostMessageInfo);
        procedure HandleMessageUnRegisterEventHost(const aMessage: TUnRegisterEventHostMessageInfo);
    protected
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); override;
        procedure SetOriginalMask(); override;
    public
        function StartMethod(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientGroupID: string): boolean;
        function SimulateMethod(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientGroupID: string): boolean;
        function InterruptStart(const aInterruptText: string): boolean;
        function InterruptFinish(const aInterruptText: string): boolean;
        function UserStopInterrupt(const aInterruptText: string): boolean;
        procedure SetGlobalError(const aErrorText: string);
        procedure RegisterEventHost(const aClientID: string; const aAddress: string; const aPort: integer);
        procedure UnRegisterEventHost(const aClientID: string);
        function AskStatus(): integer;

    end;


implementation


uses
    Windows,
    SysUtils,
    MethodStarter,
    AppTypes,
    ThrdMan,
    ThreadAPI,
    ErrorManager,
    CommonTypes,
    CoreServerEventManager,
    CoreClientInfoManager,
    LogManager;

const
    cMessageStartMethod = 9001;
    cMessageSimulateMethod = 9002;

    cMessageInterruptStart = 9010;
    cMessageInterruptFinish = 9011;
    cMessageSetGlobalError = 9012;
    cMessageUserStopInterrupt = 9013;

    cMessageRegisterEventHost = 9020;
    cMessageUnRegisterEventHost = 9021;

    cAskStatusIdle = 1;
    cAskStatusActive = 2;
    cAskStatusUserInputPending = 3;

    { TStartMethodMessageInfo }

constructor TStartMethodMessageInfo.Create(const aMethodName: string; const aDontShowMessages: boolean;
    const aClientGroupID: string);
begin
    inherited Create();
    fMethName := aMethodName;
    fDontShowMessages := aDontShowMessages;
    fClientGroupID := aClientGroupID;
    fResultSuccess := false;
end;

function TStartMethodMessageInfo.GetMessageID: integer;
begin
    result := cMessageStartMethod;
end;

constructor TSimulateMethodMessageInfo.Create(const aMethodName: string; const aDontShowMessages: boolean;
    const aClientGroupID: string);
begin
    inherited Create();
    fMethName := aMethodName;
    fDontShowMessages := aDontShowMessages;
    fClientGroupID := aClientGroupID;
    fResultSuccess := false;
end;

function TSimulateMethodMessageInfo.GetMessageID: integer;
begin
    result := cMessageSimulateMethod;
end;
{ TInterruptStartMessageInfo }

constructor TInterruptStartMessageInfo.Create(const aInterruptText: string);
begin
    inherited Create();
    fInterruptText := aInterruptText;
    fResultSuccess := false;
end;

function TInterruptStartMessageInfo.GetMessageID: integer;
begin
    result := cMessageInterruptStart;
end;

{ TInterruptFinishMessageInfo }

constructor TInterruptFinishMessageInfo.Create(const aInterruptText: string);
begin
    inherited Create();
    fInterruptText := aInterruptText;
    fResultSuccess := false;
end;

function TInterruptFinishMessageInfo.GetMessageID: integer;
begin
    result := cMessageInterruptFinish;
end;

{ TUserStopInterruptMessageInfo }

constructor TUserStopInterruptMessageInfo.Create(const aInterruptText: string);
begin
    inherited Create();
    fInterruptText := aInterruptText;
    fResultSuccess := false;
end;

function TUserStopInterruptMessageInfo.GetMessageID: integer;
begin
    result := cMessageUserStopInterrupt;
end;

{ TSetGlobalErrorMessageInfo }

constructor TSetGlobalErrorMessageInfo.Create(const aErrorText: string);
begin
    inherited Create();
    fErrorText := aErrorText;
end;

function TSetGlobalErrorMessageInfo.GetMessageID: integer;
begin
    result := cMessageSetGlobalError;
end;

{ TCoreServerCallHandler }

function TCoreServerCallHandler.StartMethodIntern(const aMethodName: string; const aDontShowMessages: boolean;
    const aSimulate: boolean; const aClientGroupID: string): boolean;
var
    xModes: TSamplerThreadModes;
begin
    try
        xModes := [mdInitFirst];
        if aDontShowMessages then
            xModes := xModes + [mdNoMessages];

        result := TMethodStarter.MethodStart(aMethodName, xModes, aSimulate);
    except
        result := false;
    end;
end;

function TCoreServerCallHandler.InterruptStartIntern(const aInterruptText: string): boolean;
var
    xInterruptText: string;
begin
    xInterruptText := aInterruptText;
    if xInterruptText = '' then
    begin
        xInterruptText := 'Interrupt Start';
    end;

    result := TThreadManagerSetup.Instance.RequestInterruptStart(TThreadAPI.GetCurrentThreadID(),
        aInterruptText);
end;

function TCoreServerCallHandler.InterruptFinishIntern(const aInterruptText: string): boolean;
var
    xInterruptText: string;
begin
    xInterruptText := aInterruptText;
    if xInterruptText = '' then
    begin
        xInterruptText := 'Interrupt Finish';
    end;

    result := TThreadManagerSetup.Instance.RequestInterruptFinish(TThreadAPI.GetCurrentThreadID(),
        aInterruptText);
end;

procedure TCoreServerCallHandler.RegisterEventHostIntern(const aClientID, aAddress: string;
    const aPort: integer);
begin
    TCoreClientInfoManager.Instance.RegisterEventHost(aClientID, aAddress, aPort);
end;

procedure TCoreServerCallHandler.UnRegisterEventHostIntern(const aClientID: string);
begin
    TCoreClientInfoManager.Instance.UnRegisterEventHost(aClientID);
end;

procedure TCoreServerCallHandler.SetGlobalErrorIntern(const aErrorText: string);
begin
    gErrorManager.SetGlobalErr(ERR_USER, aErrorText);
end;

procedure TCoreServerCallHandler.HandleMessageStartMethod(const aMessage: TStartMethodMessageInfo);
begin
    aMessage.ResultSuccess := StartMethodIntern(aMessage.MethName, aMessage.DontShowMessages, false,
        aMessage.ClientGroupID);
end;

procedure TCoreServerCallHandler.HandleMessageSimulateMethod(const aMessage: TSimulateMethodMessageInfo);
begin
    aMessage.ResultSuccess := StartMethodIntern(aMessage.MethName, aMessage.DontShowMessages, true,
        aMessage.ClientGroupID);
end;

procedure TCoreServerCallHandler.HandleMessageInterruptStart(const aMessage: TInterruptStartMessageInfo);
begin
    aMessage.ResultSuccess := InterruptStartIntern(aMessage.InterruptText);
end;

procedure TCoreServerCallHandler.HandleMessageInterruptFinish(const aMessage: TInterruptFinishMessageInfo);
begin
    aMessage.ResultSuccess := InterruptFinishIntern(aMessage.InterruptText);
end;

procedure TCoreServerCallHandler.HandleMessageSetGlobalError(const aMessage: TSetGlobalErrorMessageInfo);
begin
    SetGlobalErrorIntern(aMessage.ErrorText);
end;

procedure TCoreServerCallHandler.HandleMessageRegisterEventHost
    (const aMessage: TRegisterEventHostMessageInfo);
begin
    RegisterEventHostIntern(aMessage.ClientID, aMessage.Address, aMessage.Port);

end;

procedure TCoreServerCallHandler.HandleMessageUnRegisterEventHost
    (const aMessage: TUnRegisterEventHostMessageInfo);
begin
    UnRegisterEventHostIntern(aMessage.ClientID);

end;

procedure TCoreServerCallHandler.SetOriginalMask();
begin
    fMessageQueue.SetMask([INT_MESSAGE_EXECUTABLE_TERMINATE, cMessageSetGlobalError, cMessageStartMethod,
        cMessageSimulateMethod, cMessageInterruptStart, cMessageInterruptFinish, cMessageUserStopInterrupt,
        cMessageRegisterEventHost, cMessageUnRegisterEventHost]);
end;

procedure TCoreServerCallHandler.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
    try
        inherited;
        if aMessage.MessageInfo is TStartMethodMessageInfo then
        begin
            HandleMessageStartMethod(aMessage.MessageInfo as TStartMethodMessageInfo);
        end
        else if aMessage.MessageInfo is TSimulateMethodMessageInfo then
        begin
            HandleMessageSimulateMethod(aMessage.MessageInfo as TSimulateMethodMessageInfo);
        end
        else if aMessage.MessageInfo is TInterruptStartMessageInfo then
        begin
            HandleMessageInterruptStart(aMessage.MessageInfo as TInterruptStartMessageInfo);
        end
        else if aMessage.MessageInfo is TInterruptFinishMessageInfo then
        begin
            HandleMessageInterruptFinish(aMessage.MessageInfo as TInterruptFinishMessageInfo);
        end
        else if aMessage.MessageInfo is TSetGlobalErrorMessageInfo then
        begin
            HandleMessageSetGlobalError(aMessage.MessageInfo as TSetGlobalErrorMessageInfo);
        end
        else if aMessage.MessageInfo is TRegisterEventHostMessageInfo then
        begin
            HandleMessageRegisterEventHost(aMessage.MessageInfo as TRegisterEventHostMessageInfo);
        end
        else if aMessage.MessageInfo is TUnRegisterEventHostMessageInfo then
        begin
            HandleMessageUnRegisterEventHost(aMessage.MessageInfo as TUnRegisterEventHostMessageInfo);
        end;
    except
        on E: Exception do
        begin
            gLogManager.Log(E.Message, false);
        end;
    end;
end;

{
  function TCoreServerCallHandler.StartMethod( const aClientID : integer; const aMethodName : string; const aDontShowMessages : boolean ) : boolean;
  var
  xMessageInfo : TStartMethodMessageInfo;
  begin
  xMessageInfo := TStartMethodMessageInfo.Create( aClientID, aMethodName, aDontShowMessages );
  try
  self.RegisterMessageAndWait( xMessageInfo, false, INFINITE );
  result := xMessageInfo.ResultSuccess;
  finally
  xMessageInfo.Free;
  end;
  end;
}

function TCoreServerCallHandler.StartMethod(const aMethodName: string; const aDontShowMessages: boolean;
    const aClientGroupID: string): boolean;
var
    xMessageInfo: TStartMethodMessageInfo;
begin
    xMessageInfo := TStartMethodMessageInfo.Create(aMethodName, aDontShowMessages, aClientGroupID);
    self.RegisterMessageAndLeave(xMessageInfo);
    result := true;
end;

function TCoreServerCallHandler.SimulateMethod(const aMethodName: string; const aDontShowMessages: boolean;
    const aClientGroupID: string): boolean;
var
    xMessageInfo: TSimulateMethodMessageInfo;
begin
    xMessageInfo := TSimulateMethodMessageInfo.Create(aMethodName, aDontShowMessages, aClientGroupID);
    self.RegisterMessageAndLeave(xMessageInfo);
    result := true;
end;

function TCoreServerCallHandler.InterruptStart(const aInterruptText: string): boolean;
var
    xMessageInfo: TInterruptStartMessageInfo;
begin
    xMessageInfo := TInterruptStartMessageInfo.Create(aInterruptText);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultSuccess;
    finally
        xMessageInfo.Free;
    end;
end;

function TCoreServerCallHandler.InterruptFinish(const aInterruptText: string): boolean;
var
    xMessageInfo: TInterruptFinishMessageInfo;
begin
    xMessageInfo := TInterruptFinishMessageInfo.Create(aInterruptText);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultSuccess;
    finally
        xMessageInfo.Free;
    end;
end;

function TCoreServerCallHandler.UserStopInterrupt(const aInterruptText: string): boolean;
var
    xMessageInfo: TUserStopInterruptMessageInfo;
begin
    xMessageInfo := TUserStopInterruptMessageInfo.Create(aInterruptText);
    try
        self.RegisterMessageAndWait(xMessageInfo, false, INFINITE);
        result := xMessageInfo.ResultSuccess;
    finally
        xMessageInfo.Free;
    end;
end;

procedure TCoreServerCallHandler.SetGlobalError(const aErrorText: string);
begin
    self.RegisterMessageAndWait(TSetGlobalErrorMessageInfo.Create(aErrorText), true, INFINITE);
end;

procedure TCoreServerCallHandler.RegisterEventHost(const aClientID, aAddress: string; const aPort: integer);
begin
    self.RegisterMessageAndLeave(TRegisterEventHostMessageInfo.Create(aClientID, aAddress, aPort));
end;

procedure TCoreServerCallHandler.UnRegisterEventHost(const aClientID: string);
begin
    self.RegisterMessageAndLeave(TUnRegisterEventHostMessageInfo.Create(aClientID));
end;

function TCoreServerCallHandler.AskStatus: integer;
begin
    result := cAskStatusIdle;
    if (not TThreadAPI.WaitTillAllProcessesExited(0)) then
    begin
        result := cAskStatusActive;
    end;
end;

{ TRegisterEventHostMessageInfo }

constructor TRegisterEventHostMessageInfo.Create(const aClientID, aAddress: string; const aPort: integer);
begin
    inherited Create();
    fClientID := aClientID;
    fAddress := aAddress;
    fPort := aPort;
end;

function TRegisterEventHostMessageInfo.GetMessageID: integer;
begin
    result := cMessageRegisterEventHost;
end;

{ TUnRegisterEventHostMessageInfo }

constructor TUnRegisterEventHostMessageInfo.Create(const aClientID: string);
begin
    inherited Create();
    fClientID := aClientID;
end;

function TUnRegisterEventHostMessageInfo.GetMessageID: integer;
begin
    result := cMessageUnRegisterEventHost;
end;


end.
