{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  30.07.09 pk                                        TN4585.5    Initial Revision
  01.03.12 wl                                        TN5822   uses geändert
  17.03.12 pk                                        TN5809.1  Changes for ZACoreClientX dll
  10.04.13 wl                                        TN6045   TStringKeyList umbenannt in TStringKeyObjectValueList
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreClientRun;


interface


uses
    Classes,
    GeneralTypes,
    ThreadClasses,
    CoreControllerEventManager,
    CoreEventTransmitter,
    CoreClientInfo;

type
    TCoreClientRun = class(TCoreClientInfoList)
    private
        fRunID: string;
        fProcessID: string;
        fControllerClientID: string;
        fControllerEventManager: TCoreControllerEventManager;
        procedure SetControllerClientID(const aClientID: string);
    public
        constructor Create(const aRunID: string);
        destructor Destroy(); override;
        function IsControllerClient(const aClientInfo: TCoreClientInfo): boolean;
        function IsControllerClientID(const aClientID: string): boolean;
        function ControllerExists: boolean;
        function FindControllerClient: TCoreClientInfo;
        property RunID: string read fRunID;
        property ProcessID: string read fProcessID write fProcessID;
        property ControllerClientID: string read fControllerClientID write SetControllerClientID;
        property ControllerEventManager: TCoreControllerEventManager read fControllerEventManager;
    end;

    TCoreClientRunList = class(TStringList)
    private
        function GenerateRunID(): string;
        function GetClientRunAt(aIndex: integer): TCoreClientRun;
        procedure RemoveRun(const aRunID: string);
    public
        constructor Create();
        destructor Destroy; override;
        procedure LinkProcessToRunID(const aRunID: string; const aProcessID: string);
        function AddNewRun(): TCoreClientRun;
        function FindRunByRunID(const aRunID: string): TCoreClientRun;
        function FindRunByProcessID(const aProcessID: string): TCoreClientRun;
        procedure AddClientToRun(const aClientInfo: TCoreClientInfo; const aRunID: string);
        procedure RemoveClientFromRun(const aClientID: string; const aRunID: string);
        property Runs[aIndex: integer]: TCoreClientRun read GetClientRunAt; default;
    end;


implementation


uses
    SysUtils,
    UtilLib,
    Streamable;
{ TCoreClientRun }

constructor TCoreClientRun.Create(const aRunID: string);
begin
    inherited Create(false);
    fRunID := aRunID;
    fProcessID := '';
    fControllerEventManager := TCoreControllerEventManager.Create();
    fControllerEventManager.CoreClients := self;
end;

destructor TCoreClientRun.Destroy();
begin
    fControllerEventManager.Free;
    inherited;
end;

function TCoreClientRun.FindControllerClient: TCoreClientInfo;
begin
    result := FindByClientID(fControllerClientID);
end;

function TCoreClientRun.ControllerExists(): boolean;
begin
    result := Assigned(FindControllerClient());
end;

function TCoreClientRun.IsControllerClient(const aClientInfo: TCoreClientInfo): boolean;
begin
    result := ControllerExists() and SameText(aClientInfo.ClientID, fControllerClientID);
end;

function TCoreClientRun.IsControllerClientID(const aClientID: string): boolean;
var
    xClientInfo: TCoreClientInfo;
begin
    xClientInfo := FindByClientID(aClientID);
    ASSERT(Assigned(xClientInfo));
    result := IsControllerClient(xClientInfo);
end;

procedure TCoreClientRun.SetControllerClientID(const aClientID: string);
var
    xOldControllerClientInfo, xNewControllerClientInfo: TCoreClientInfo;
begin
    xOldControllerClientInfo := FindControllerClient();
    if Assigned(xOldControllerClientInfo) then
        xOldControllerClientInfo.IsController := false;

    fControllerClientID := aClientID;
    if aClientID = '' then
        EXIT;

    xNewControllerClientInfo := self.FindByClientID(aClientID);
    ASSERT(Assigned(xNewControllerClientInfo));
    xNewControllerClientInfo.IsController := true;
    self.ControllerEventManager.SetControlEventMask(xNewControllerClientInfo.ControlEventIDs);

end;

{ TCoreClientRunList }

constructor TCoreClientRunList.Create;
begin
    inherited Create();
end;

destructor TCoreClientRunList.Destroy();
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
        self.Objects[x].Free;

    inherited;
end;

function TCoreClientRunList.GenerateRunID(): string;
var
    xID: integer;
begin
    xID := 1;
    while true do
    begin
        if self.IndexOf(IntToStr(xID)) < 0 then
        begin
            result := IntToStr(xID);
            EXIT;
        end;
        Inc(xID);
    end;
end;

function TCoreClientRunList.GetClientRunAt(aIndex: integer): TCoreClientRun;
begin
    result := self.Objects[aIndex] as TCoreClientRun;
end;

procedure TCoreClientRunList.LinkProcessToRunID(const aRunID, aProcessID: string);
var
    xRun: TCoreClientRun;
begin
    if aRunID = '' then
        EXIT;
    xRun := FindRunByRunID(aRunID);
    xRun.ProcessID := aProcessID;
end;

function TCoreClientRunList.AddNewRun(): TCoreClientRun;
var
    xRunID: string;
begin
    xRunID := GenerateRunID();
    result := TCoreClientRun.Create(xRunID);
    self.AddObject(xRunID, result);
end;

function TCoreClientRunList.FindRunByRunID(const aRunID: string): TCoreClientRun;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := self.IndexOf(aRunID);
    if xIndex < 0 then
        EXIT;
    result := self[xIndex];
end;

function TCoreClientRunList.FindRunByProcessID(const aProcessID: string): TCoreClientRun;
var
    xRun: TCoreClientRun;
    x: integer;
begin
    result := nil;

    for x := 0 to self.Count - 1 do
    begin
        xRun := self[x];
        if SameText(xRun.ProcessID, aProcessID) then
        begin
            result := xRun;
            EXIT;
        end;
    end;
end;

procedure TCoreClientRunList.AddClientToRun(const aClientInfo: TCoreClientInfo; const aRunID: string);
var
    xRun: TCoreClientRun;
begin
    if aRunID = '' then
        EXIT;
    if aClientInfo.HasRunID(aRunID) then
        EXIT;

    aClientInfo.AddRunID(aRunID);

    xRun := FindRunByRunID(aRunID);
    xRun.Add(aClientInfo);
end;

procedure TCoreClientRunList.RemoveRun(const aRunID: string);
var
    xIndex: integer;
begin
    xIndex := self.IndexOf(aRunID);
    self[xIndex].Free;
    self.Delete(xIndex);
end;

procedure TCoreClientRunList.RemoveClientFromRun(const aClientID: string; const aRunID: string);
var
    xClientInfo: TCoreClientInfo;
    xRun: TCoreClientRun;
begin
    if aRunID = '' then
        EXIT;
    xRun := FindRunByRunID(aRunID);
    xClientInfo := xRun.FindByClientID(aClientID);

    xClientInfo.RemoveRunID(aRunID);

    xRun.RemoveByClientID(xClientInfo.ClientID);
    // If no more clients in this Run delete the Run
    if xRun.Count = 0 then
        RemoveRun(xRun.RunID);
end;


end.
