{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  18.06.09 pk                                        Tn4585.2    CheckResponse removed
  18.06.09 pk                                        TN4585.3    Uses changed
  06.07.09 pk                                        TN4585.4    Uses logmanager removed
  30.07.09 pk                                        TN4585.5    Various changes
  20.05.10 wl  Instance                              TN5116      Instance-Funktionen geändert
  19.10.10 pk                                        TN5305      changes needed for CoreClient/Server-
  19.02.12 pk                                        TN5809.1    Changes for ZACoreClientX dll
  ----------------------------------------------------------------------------------------------------------------------- }

unit TCPIPCoreClient;


interface


uses
    GeneralTypes,
    CoreControllerClientCalls,
    RemoteClasses,
    RemoteClient,
    RemoteMessageConstants,
    RemoteFunctionClasses,
    DataConnectionParams;

type
    TTCPIPCoreClient = class(TCoreClient)
    private
        function GetStream: TWinSocketStreamExt;
        function GetIsActive: boolean;
    protected
        fConnection: TMsgClient;
        fOnWriteLog: TOnRemoteWriteLogCallback;
        function GetRemoteFunctionCaller: TRemoteFunctionCaller; override;
        procedure SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback); override;
        function GetClientAddress(): string; override;
    public
        constructor Create();
        destructor Destroy(); override;
        function Connect(const aAddress: string; const aPort: integer): boolean; override;
        procedure Disconnect(); override;
        function GetError(out oErrorDescription: string): boolean;
        function RegisterController(const aRunID: string): boolean; override;
        function UnRegisterController(const aRunID: string): boolean; override;
        procedure RegisterEventHost(const aPort: integer); override;
        procedure UnRegisterEventHost(); override;
        function IsEventHostRegistered(): boolean; override;
        function GetAllAliasNames(): TStringArray; override;
        function AliasNameExists(const aAlias: string): boolean; override;
        function GetAliasPath(const aAlias: string): string; override;
        function CreateDataConnectionParams(const aTypeName: string): TDataConnectionParams; override;
        function CreateConnectionParamsByAlias(const aAlias: string): TDataConnectionParams; override;
        function GetMainDBAlias(): string; override;

        function GetClientRunIDsAtAddress(const aAddress: string; const aMustBeController: boolean)
            : TStringArray; override;
        function GetNewClientRunID: string; override;
        procedure AddClientToRun(const aRunID: string); override;
        procedure RemoveClientFromRun(const aRunID: string); override;
        property Stream: TWinSocketStreamExt read GetStream;
        property IsActive: boolean read GetIsActive;
    end;


implementation


uses
    SysUtils,
    Streamable,
    XMLClientServer;

{ TTCPIPCoreClient }

constructor TTCPIPCoreClient.Create;
begin
    inherited Create();
    fConnection := TXMLMsgClient.Create('', 0);
    (fConnection as TXMLMsgClient).OnWriteLog := fOnWriteLog;

    // gmSetRemoteWriteLogFunc( WriteLog );
end;

destructor TTCPIPCoreClient.Destroy;
begin
    Disconnect();
    fConnection.Free;
    // gmSetRemoteWriteLogFunc( nil );
    inherited;
end;

function TTCPIPCoreClient.Connect(const aAddress: string; const aPort: integer): boolean;
begin
    fConnection.Address := aAddress;
    fConnection.Port := aPort;
    try
        fConnection.Active := true;
    except
        on E: Exception do
        begin
            raise Exception.CreateFmt('Server %s, Port %d:' + #13#10 + '%s', [aAddress, aPort, e.Message]);
        end;
    end;
    result := self.IsActive and self.RemoteFunctionCaller.CallBoolFunc(cCoreCallIDCheckConnection);
end;

function TTCPIPCoreClient.AliasNameExists(const aAlias: string): boolean;
begin
    result := self.RemoteFunctionCaller.CallBoolFunc(cCoreCallIDAliasNameExists, [aAlias]);
end;

function TTCPIPCoreClient.GetAliasPath(const aAlias: string): string;
begin
    self.RemoteFunctionCaller.Call(cCoreCallIDGetAliasPath, [aAlias], [@result], [result]);
end;

function TTCPIPCoreClient.CreateDataConnectionParams(const aTypeName: string): TDataConnectionParams;
begin
    self.RemoteFunctionCaller.Call(cCoreCallIDCreateDataConnectionParams, [aTypeName], [@result], [result]);
end;

function TTCPIPCoreClient.CreateConnectionParamsByAlias(const aAlias: string): TDataConnectionParams;
begin
    self.RemoteFunctionCaller.Call(cCoreCallIDCreateConnectionParamsByAlias, [aAlias], [@result], [result]);
end;

function TTCPIPCoreClient.GetAllAliasNames: TStringArray;
var
    xStringList: TStreamableStringList;
begin
    self.RemoteFunctionCaller.Call(cCoreCallIDGetAllAliasNames, [], [@xStringList], [xStringList]);
    result := xStringList.ToStringArray;
end;

function TTCPIPCoreClient.GetClientAddress: string;
begin
    result := fConnection.Socket.LocalAddress;
end;

function TTCPIPCoreClient.GetClientRunIDsAtAddress(const aAddress: string; const aMustBeController: boolean)
    : TStringArray;
var
    xStringList: TStreamableStringList;
begin
    self.RemoteFunctionCaller.Call(cCoreCallIDGetClientRunIDsAtAddress, [aAddress, aMustBeController],
        [@xStringList], [xStringList]);
    result := xStringList.ToStringArray;
end;

function TTCPIPCoreClient.GetNewClientRunID(): string;
begin
    self.RemoteFunctionCaller.Call(cCoreCallIDGetNewClientRunID, [], [@result], [result]);
end;

procedure TTCPIPCoreClient.AddClientToRun(const aRunID: string);
begin
    self.RemoteFunctionCaller.CallProc(cCoreCallIDAddClientToRun, [aRunID]);
end;

procedure TTCPIPCoreClient.RemoveClientFromRun(const aRunID: string);
begin
    self.RemoteFunctionCaller.CallProc(cCoreCallIDRemoveClientFromRun, [aRunID]);
end;

function TTCPIPCoreClient.GetError(out oErrorDescription: string): boolean;
begin
    result := false;
    if not self.IsActive then
        EXIT;
    self.RemoteFunctionCaller.Call(cCoreCallIDGetError, [], [@result, @oErrorDescription],
        [result, oErrorDescription]);
end;

function TTCPIPCoreClient.GetStream: TWinSocketStreamExt;
begin
    result := fConnection.Stream;
end;

function TTCPIPCoreClient.GetIsActive: boolean;
begin
    result := fConnection.Active;
end;

function TTCPIPCoreClient.GetMainDBAlias: string;
begin
    self.RemoteFunctionCaller.Call(cCoreCallIDGetMainDBAlias, [], [@result], [result]);
end;

procedure TTCPIPCoreClient.Disconnect;
begin
    fConnection.CloseClient();
end;

function TTCPIPCoreClient.RegisterController(const aRunID: string): boolean;
begin
    result := false;
    if not self.IsActive then
        EXIT;
    result := self.RemoteFunctionCaller.CallBoolFunc(cCoreCallIDRegisterController, [aRunID]);
end;

function TTCPIPCoreClient.UnRegisterController(const aRunID: string): boolean;
begin
    result := false;
    if not self.IsActive then
        EXIT;
    result := self.RemoteFunctionCaller.CallBoolFunc(cCoreCallIDUnRegisterController, [aRunID]);
end;

procedure TTCPIPCoreClient.RegisterEventHost(const aPort: integer);
begin
    if not self.IsActive then
        EXIT;
    self.RemoteFunctionCaller.CallProc(cCoreCallIDRegisterEventHost, [aPort]);
end;

procedure TTCPIPCoreClient.UnRegisterEventHost();
begin
    if not self.IsActive then
        EXIT;
    self.RemoteFunctionCaller.CallProc(cCoreCallIDUnRegisterEventHost);
end;

function TTCPIPCoreClient.IsEventHostRegistered(): boolean;
begin
    result := false;
    if not self.IsActive then
        EXIT;
    result := self.RemoteFunctionCaller.CallBoolFunc(cCoreCallIDIsEventHostRegistered, []);
end;

function TTCPIPCoreClient.GetRemoteFunctionCaller: TRemoteFunctionCaller;
begin
    result := (fConnection as TXMLMsgClient).RemoteFunctionCaller;
end;

procedure TTCPIPCoreClient.SetOnWriteLog(const aValue: TOnRemoteWriteLogCallback);
begin
    fOnWriteLog := aValue;
    if Assigned(self.RemoteFunctionCaller) then
        self.RemoteFunctionCaller.OnWriteLog := fOnWriteLog;
end;


end.
