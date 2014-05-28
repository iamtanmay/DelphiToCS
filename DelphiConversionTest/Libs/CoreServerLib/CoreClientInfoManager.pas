{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  30.07.09 pk                                        TN4585.5    Initial Revision
  04.11.09 pk                               	    TN4843      Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.03.12 pk                                        TN5809.1  Changes for ZACoreClientX dll
  22.01.13 ts  FindRunForCurrentThread               TN6071    CoreServer muss Fehlermeldungen weitergeben
  22.01.13 ts  FindRunForCurrentThread               TN6071.1  CoreServer muss Fehlermeldungen weitergeben
  11.02.13 ts  FindRunForCurrentThread               TN6071.2  Warnung entfernt
  14.02.13 ts  FindRunForCurrentThread               TN6071.3  result = nil, dann verhält es sich wie vorher
  10.04.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreClientInfoManager;


interface


uses
    Generics.Collections,
    GeneralTypes,
    ThreadClasses,
    CoreControllerEventManager,
    CoreEventTransmitter,
    CoreClientInfo,
    CoreClientRun;

type
    TCoreClientInfoManager = class
    private
        fClientInfos: TCoreClientInfoList;
        fClientRuns: TCoreClientRunList;
        fMasterControllerClientID: string;
        procedure UnRegisterEventHosts();
        function IsControllerClient(const aClientInfo: TCoreClientInfo): boolean;
        function IsControllerClientOfRun(const aClientInfo: TCoreClientInfo; const aRunID: string): boolean;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Add(const aCoreClientInfo: TCoreClientInfo);
        function AddObject(const aClientID: string; const aObject: TObject): integer;
        function FindObjectByObjectID(const aClientID: string; const aObjectID: integer): TObject;
        procedure RemoveObject(const aClientID: string; const aObjectID: integer);
        procedure RemoveByClientID(const aClientID: string);
        function ObtainControllerPrivileges(const aClientID: string; const aRunID: string): boolean;
        function ReleaseControllerPrivileges(const aClientID: string; const aRunID: string): boolean;
        function IsEventHostRegistered(const aClientID: string): boolean;
        function RegisterEventHost(const aClientID, aAddress: string; const aPort: integer): boolean;
        procedure UnRegisterEventHost(const aClientID: string);
        function FindClientInfoByClientID(const aClientID: string): TCoreClientInfo;
        function IsControllerClientID(const aClientID: string): boolean;
        function GetClientRunIDsAtAddress(const aAddress: string; const aMustBeController: boolean)
            : TStringArray;
        procedure AddClientToRun(const aClientID: string; const aRunID: string);
        procedure RemoveClientFromRun(const aClientID: string; const aRunID: string);
        function NewClientRun(): string;
        procedure LinkProcessToRunID(const aRunID: string; const aProcessID: string);
        function FindRunForCurrentThread(): TCoreClientRun;
        function SetClientControlEventMask(const aClientID: string; const aIDs: TIntArray): boolean;
        function FindControllerEventManagerByClientID(const aClientID: string): TCoreControllerEventManager;
        property ClientInfos: TCoreClientInfoList read fClientInfos;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TCoreClientInfoManager;
    end;


implementation


uses
    SysUtils,
    UtilLib,    ThreadAPI,
    Streamable,
    TCPIPCoreEventTransmitter;

var
    uInstance: TCoreClientInfoManager = nil;
    { TCoreClientInfoManager }

constructor TCoreClientInfoManager.Create;
begin
    inherited Create();
    fClientInfos := TCoreClientInfoList.Create(true);
    fClientRuns := TCoreClientRunList.Create();
    fMasterControllerClientID := '';
end;

destructor TCoreClientInfoManager.Destroy;
begin
    UnRegisterEventHosts();
    fClientRuns.Free;
    fClientInfos.Free;
    inherited;
end;

class procedure TCoreClientInfoManager.CreateInstance();
begin
    uInstance := TCoreClientInfoManager.Create();
end;

class procedure TCoreClientInfoManager.DestroyInstance;
begin
    uInstance.Free;
end;

class function TCoreClientInfoManager.Instance: TCoreClientInfoManager;
begin
    result := uInstance;
end;

procedure TCoreClientInfoManager.Add(const aCoreClientInfo: TCoreClientInfo);
begin
    fClientInfos.Add(aCoreClientInfo);
end;

function TCoreClientInfoManager.AddObject(const aClientID: string; const aObject: TObject): integer;
var
    xClientInfo: TCoreClientInfo;
begin
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));
    result := xClientInfo.AddObject(aObject);
end;

procedure TCoreClientInfoManager.RemoveObject(const aClientID: string; const aObjectID: integer);
var
    xClientInfo: TCoreClientInfo;
begin
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));
    xClientInfo.RemoveObject(aObjectID);
end;

function TCoreClientInfoManager.SetClientControlEventMask(const aClientID: string;
    const aIDs: TIntArray): boolean;
var
    xClientInfo: TCoreClientInfo;
begin
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));
    xClientInfo.ControlEventIDs := aIDs;
    result := true;
end;

function TCoreClientInfoManager.FindObjectByObjectID(const aClientID: string;
    const aObjectID: integer): TObject;
var
    xClientInfo: TCoreClientInfo;
begin
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));
    result := xClientInfo.FindObjectByID(aObjectID)
end;

function TCoreClientInfoManager.IsControllerClientOfRun(const aClientInfo: TCoreClientInfo;
    const aRunID: string): boolean;
var
    xClientRun: TCoreClientRun;
begin
    xClientRun := self.fClientRuns.FindRunByRunID(aRunID);
    result := xClientRun.IsControllerClient(aClientInfo);
end;

function TCoreClientInfoManager.IsEventHostRegistered(const aClientID: string): boolean;
var
    xClientInfo: TCoreClientInfo;
begin
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));
    result := Assigned(xClientInfo.EventTransmitter) and xClientInfo.EventTransmitter.IsActive;
end;

function TCoreClientInfoManager.IsControllerClient(const aClientInfo: TCoreClientInfo): boolean;
var
    xRunID: string;
begin
    result := false;
    for xRunId in aClientInfo.RunIDs do
    begin
        result := IsControllerClientOfRun(aClientInfo, xRunID);
        if result then
            EXIT;
    end;

end;

function TCoreClientInfoManager.IsControllerClientID(const aClientID: string): boolean;
var
    xClientInfo: TCoreClientInfo;
begin
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));
    result := IsControllerClient(xClientInfo);
end;

procedure TCoreClientInfoManager.LinkProcessToRunID(const aRunID, aProcessID: string);
begin
    fClientRuns.LinkProcessToRunID(aRunID, aProcessID);
end;

function TCoreClientInfoManager.FindClientInfoByClientID(const aClientID: string): TCoreClientInfo;
begin
    result := fClientInfos.FindByClientID(aClientID);
end;

function TCoreClientInfoManager.FindControllerEventManagerByClientID(const aClientID: string)
    : TCoreControllerEventManager;
var
    xClientInfo: TCoreClientInfo;
    xClientRun: TCoreClientRun;
begin
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));

    ASSERT(xClientInfo.RunIDCount > 0);
    xClientRun := self.fClientRuns.FindRunByRunID(xClientInfo.RunIDs[0]);
    result := xClientRun.ControllerEventManager;
end;

function TCoreClientInfoManager.FindRunForCurrentThread: TCoreClientRun;
var
    xProcessID: string;
begin
    result := nil;
    xProcessID := '';
    try
        xProcessID := TThreadAPI.GetCurrentSourceDataName();
    except
        xProcessID := '';
    end;
    // if result = nil (happens because Interrupt Manager has no SourceDataName) the message has to be sent to the message queue,
    // otherwhise the Errorbox can not be handled
    if xProcessID = '' then
    begin
        xProcessID := TThreadAPI.GetCurrentThreadDescription;
        if (xProcessID = 'Interrupt Manager') or (xProcessID = 'Safe Module Communication Manager') or
            (xProcessID = 'Safe Application Close Manager') then
            result := fClientRuns[0];
    end
    else
        result := fClientRuns.FindRunByProcessID(xProcessID);
end;

function TCoreClientInfoManager.ObtainControllerPrivileges(const aClientID: string;
    const aRunID: string): boolean;
var
    xClientInfo: TCoreClientInfo;
    xClientRun: TCoreClientRun;
begin
    result := false;
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));
    xClientRun := self.fClientRuns.FindRunByRunID(aRunID);

    if xClientRun.ControllerExists() and (not xClientRun.IsControllerClient(xClientInfo)) then
        EXIT;
    xClientRun.ControllerClientID := xClientInfo.ClientID;

    result := true;
end;

function TCoreClientInfoManager.ReleaseControllerPrivileges(const aClientID: string;
    const aRunID: string): boolean;
var
    xClientInfo: TCoreClientInfo;
    xClientRun: TCoreClientRun;
begin
    result := false;
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));

    xClientRun := self.fClientRuns.FindRunByRunID(aRunID);
    if not Assigned(xClientRun) then
        EXIT;

    if not IsControllerClient(xClientInfo) then
        EXIT;

    xClientRun.ControllerClientID := '';

    result := true;
end;

function TCoreClientInfoManager.GetClientRunIDsAtAddress(const aAddress: string;
    const aMustBeController: boolean): TStringArray;
var
    xClientInfo: TCoreClientInfo;
    x: integer;
    xList: TList<string>;
    xRunID: string;
begin
    SetLength(result, 0);

    xList := TList<string>.Create();
    try
        for x := 0 to fClientInfos.Count - 1 do
        begin
            xClientInfo := fClientInfos[x];
            if (aAddress <> '') and (not SameText(aAddress, xClientInfo.Address)) then
                CONTINUE;
            if xClientInfo.RunIDCount = 0 then
                CONTINUE;
            if aMustBeController and (not self.IsControllerClientID(xClientInfo.ClientID)) then
                CONTINUE;

            for xRunID in xClientInfo.RunIDs do
            begin
                if xList.IndexOf(xRunID) < 0 then
                    xList.Add(xRunID);
            end;
        end;

        result := xList.ToArray;

    finally
        xList.Free;
    end;
end;

procedure TCoreClientInfoManager.AddClientToRun(const aClientID: string; const aRunID: string);
var
    xClientInfo: TCoreClientInfo;
begin
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));
    fClientRuns.AddClientToRun(xClientInfo, aRunID)
end;

procedure TCoreClientInfoManager.RemoveClientFromRun(const aClientID: string; const aRunID: string);
var
    xClientInfo: TCoreClientInfo;
begin
    xClientInfo := FindClientInfoByClientID(aClientID);
    if not Assigned(xClientInfo) then
        EXIT;

    fClientRuns.RemoveClientFromRun(aClientID, aRunID);
end;

function TCoreClientInfoManager.NewClientRun(): string;
var
    xClientRun: TCoreClientRun;
begin
    xClientRun := fClientRuns.AddNewRun();
    result := xClientRun.RunID;
end;

procedure TCoreClientInfoManager.UnRegisterEventHost(const aClientID: string);
var
    xClientInfo: TCoreClientInfo;
    xEventTransmitter: TCoreEventTransmitter;
begin
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));

    xEventTransmitter := xClientInfo.EventTransmitter;
    if not Assigned(xEventTransmitter) then
        EXIT;
    xEventTransmitter.Disconnect();
    xEventTransmitter.Free;

    xClientInfo.EventTransmitter := nil;
end;

procedure TCoreClientInfoManager.UnRegisterEventHosts();
var
    xClientInfo: TCoreClientInfo;
    x: integer;
begin
    for x := 0 to fClientInfos.Count - 1 do
    begin
        xClientInfo := fClientInfos[x];
        UnRegisterEventHost(xClientInfo.ClientID);
    end;
end;

function TCoreClientInfoManager.RegisterEventHost(const aClientID, aAddress: string;
    const aPort: integer): boolean;
var
    xClientInfo: TCoreClientInfo;
begin
    result := false;
    xClientInfo := FindClientInfoByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));
    if not Assigned(xClientInfo.EventTransmitter) then
    begin
        xClientInfo.EventTransmitter := TTCPIPCoreEventTransmitter.Create();
    end;

    if xClientInfo.EventTransmitter.IsActive then
        EXIT;

    try
        result := xClientInfo.EventTransmitter.Connect(aAddress, aPort);
    except

    end;
end;

procedure TCoreClientInfoManager.RemoveByClientID(const aClientID: string);
var
    xClientInfo: TCoreClientInfo;
    xRunID: string;
begin
    xClientInfo := FindClientInfoByClientID(aClientID);
    for xRunID in xClientInfo.RunIDs do
    begin
        ReleaseControllerPrivileges(aClientID, xRunID);
        fClientRuns.RemoveClientFromRun(aClientID, xRunID);
    end;
    UnRegisterEventHost(aClientID);
    fClientInfos.RemoveByClientID(aClientID);
end;


end.
