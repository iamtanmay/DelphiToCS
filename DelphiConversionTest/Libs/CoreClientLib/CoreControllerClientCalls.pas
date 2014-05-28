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
  20.05.10 wl  Instance                              TN5116   Instance-Funktionen geändert
  19.10.10 pk                                        TN5305    changes needed for CoreClient/Server-
  21.03.11 wl  AskRunStartPromptModal                TN5508   neue Parameter
  17.03.12 pk                                        TN5809.1    Changes for ZACoreClientX dll
  20.04.12 wl  SetStopPromptModalEventHandled        TN5946   entfernt
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreControllerClientCalls;


interface


uses
    GeneralTypes,
    RemoteFunctionClasses,
    DataConnectionParams;

type
    TCoreClient = class
    private
        fOnWriteLog: TOnRemoteWriteLogCallback;
        class var uInstance: TCoreClient;
    protected
        function GetRemoteFunctionCaller: TRemoteFunctionCaller; virtual; abstract;
        procedure SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback); virtual;
        function GetClientAddress(): string; virtual; abstract;
    public
        function Connect(const aAddress: string; const aPort: integer): boolean; virtual; abstract;
        procedure Disconnect(); virtual; abstract;
        function RegisterController(const aRunID: string): boolean; virtual; abstract;
        function UnRegisterController(const aRunID: string): boolean; virtual; abstract;
        procedure RegisterEventHost(const aPort: integer); virtual; abstract;
        procedure UnRegisterEventHost(); virtual; abstract;
        function IsEventHostRegistered(): boolean; virtual; abstract;

        function GetAllAliasNames(): TStringArray; virtual; abstract;
        function AliasNameExists(const aAlias: string): boolean; virtual; abstract;
        function GetAliasPath(const aAlias: string): string; virtual; abstract;
        function CreateDataConnectionParams(const aTypeName: string): TDataConnectionParams; virtual;
            abstract;
        function CreateConnectionParamsByAlias(const aAlias: string): TDataConnectionParams; virtual;
            abstract;
        function GetMainDBAlias(): string; virtual; abstract;

        function GetClientRunIDsAtAddress(const aAddress: string; const aMustBeController: boolean)
            : TStringArray; virtual; abstract;
        function GetNewClientRunID: string; virtual; abstract;
        procedure AddClientToRun(const aRunID: string); virtual; abstract;
        procedure RemoveClientFromRun(const aRunID: string); virtual; abstract;
        property RemoteFunctionCaller: TRemoteFunctionCaller read GetRemoteFunctionCaller;
        class procedure DestroyInstance();
        class property Instance: TCoreClient read uInstance write uInstance;
        property OnWriteLog: TOnRemoteWriteLogCallback read fOnWriteLog write SetOnWriteLog;
        property ClientAddress: string read GetClientAddress;
    end;

    TCoreControllerClientCalls = class
    private
        class var uInstance: TCoreControllerClientCalls;
    public
        function SetControlEventMask(const aIDs: TIntArray): boolean; virtual; abstract;
        function StartMethod(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientRunID: string): boolean; virtual; abstract;
        function SimulateMethod(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientRunID: string): boolean; virtual; abstract;
        function InterruptStart(const aInterruptText: string): boolean; virtual; abstract;
        function InterruptFinish(const aInterruptText: string): boolean; virtual; abstract;
        procedure SetGlobalError(const aErrorText: string); virtual; abstract;
        function UserStopInterrupt(const aInterruptText: string): boolean; virtual; abstract;
        function AskStatus(): integer; virtual; abstract;
        function GetPendingControlEventID: integer; virtual; abstract;
        function GetErrorBoxEvent(out oErrorID: integer): boolean; virtual; abstract;
        function GetErrorBoxAdvancedEvent(out oErrorID: integer; out oErrorInfo: TObject): boolean;
            virtual; abstract;
        procedure SetErrorBoxEventHandled(const aResultModal: integer); virtual; abstract;
        function GetMessageBoxEvent(out oText, oCaption: string; out oType: integer): boolean;
            virtual; abstract;
        procedure SetMessageBoxEventHandled(const aResultModal: integer); virtual; abstract;
        function GetAskRunStartEvent(out oMethodName: string; out oIsSimChangeAllowed: boolean;
            out oIsSim: boolean; out oSimulationAskWeight: boolean; out oSimulationSpeed_Percent: integer)
            : boolean; virtual; abstract;
        procedure SetAskRunStartEventHandled(const aResultModal: integer; const aResultIsSim: boolean;
            const aSimulationAskWeight: boolean; const aSimulationSpeed_Percent: integer); virtual; abstract;
        function GetLoadDisplayComponentEvent(out oDisplayComponentName: string; out oContextID: string)
            : boolean; virtual; abstract;
        procedure SetLoadDisplayComponentEventHandled(const aResultSuccess: boolean); virtual; abstract;
        function GetProcessStartedEvent(out oSourceDataName: string; out oProcessID: string): boolean;
            virtual; abstract;
        procedure SetProcessStartedEventHandled(const aResultSuccess: boolean); virtual; abstract;
        function GetProcessFinishedEvent(out oProcessID: string; out oIsError: boolean): boolean;
            virtual; abstract;
        procedure SetProcessFinishedEventHandled(const aResultSuccess: boolean); virtual; abstract;
        class procedure DestroyInstance();
        class property Instance: TCoreControllerClientCalls read uInstance write uInstance;
    end;


implementation


uses
    SysUtils;

{ TCoreClient }

class procedure TCoreClient.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

procedure TCoreClient.SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback);
begin
    fOnWriteLog := aValue;
end;

{ TCoreControllerClientCalls }

class procedure TCoreControllerClientCalls.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;


end.
