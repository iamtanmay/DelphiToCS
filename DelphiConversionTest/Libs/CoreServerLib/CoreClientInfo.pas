{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  06.07.09 pk  TRemoteObjectItem                     TN4585.4    New
  30.07.09 pk  TCoreClientInfoManager                TN4585.5    moved to CoreClientInfoManager
  09.09.09 pk  TCoreClientInfoList.Destroy           TN4585.2    fList.free was called too early
  01.03.12 wl                                        TN5822   uses geändert
  17.03.12 pk                                        TN5809.1  Changes for ZACoreClientX dll
  11.04.13 wl                                        TN6045   uses Generics.Collections
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreClientInfo;


interface


uses
    Generics.Collections,
    GeneralTypes,
    ThreadClasses,
    CoreEventTransmitter;

type
    TRemoteObjectItem = class
    private
        fID: integer;
        fObj: TObject;
    public
        constructor Create(const aID: integer; const aObj: TObject);
        property ID: integer read fID;
        property Obj: TObject read fObj;
    end;

    TRemoteObjectList = class(TObjectList<TRemoteObjectItem>)
    private
        function GetItemAt(aIndex: integer): TRemoteObjectItem;
        function GenerateID: integer;
    public
        function FindByID(const aID: integer): TRemoteObjectItem;
        function Add(const aObject: TObject): integer;
        procedure Remove(const aID: integer);
        property RemoteObjectItems[aIndex: integer]: TRemoteObjectItem read GetItemAt; default;
    end;

    TCoreClientInfo = class
    strict private
        fClientID: string;
        fRunIDs: TList<string>;
        fAddress: string;
        fPort: integer;
        fIsController: boolean;
        fEventTransmitter: TCoreEventTransmitter;
        fObjects: TRemoteObjectList;
        fControlEventIDs: TIntArray;
        function GetIsEventHost: boolean;
        function GetRunIDs(): TStringArray;
        function GetRunIDCount(): integer;
    public
        constructor Create(const aClientID: string);
        destructor Destroy(); override;
        function AddObject(const aObject: TObject): integer;
        procedure RemoveObject(const aID: integer);
        function FindObjectByID(const aID: integer): TObject;
        function HasRunID(const aRunID: string): boolean;
        procedure AddRunID(const aRunID: string);
        procedure RemoveRunID(const aRunID: string);
        property ClientID: string read fClientID;
        property Port: integer read fPort;
        property Address: string read fAddress;
        property EventTransmitter: TCoreEventTransmitter read fEventTransmitter write fEventTransmitter;
        property IsEventHost: boolean read GetIsEventHost;
        property IsController: boolean read fIsController write fIsController;
        property RunIDs: TStringArray read GetRunIDs;
        property RunIDCount: integer read GetRunIDCount;
        // property RunIDs: TStringList read fRunIDs;
        property ControlEventIDs: TIntArray read fControlEventIDs write fControlEventIDs;
    end;

    TCoreClientInfoList = class
    private
        fList: TObjectList<TCoreClientInfo>;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TCoreClientInfo;
    public
        constructor Create(const aOwnsObjects: boolean);
        destructor Destroy(); override;

        procedure Add(const aCoreClientInfo: TCoreClientInfo);
        procedure RemoveByClientID(const aClientID: string);
        function FindByClientID(const aClientID: string): TCoreClientInfo;
        property Count: integer read GetCount;
        property Items[aIndex: integer]: TCoreClientInfo read GetItemAt; default;
    end;


implementation


uses
    SysUtils,
    Streamable;

{ TCoreClientInfoList }

procedure TCoreClientInfoList.Add(const aCoreClientInfo: TCoreClientInfo);
begin
    fList.Add(aCoreClientInfo);
end;

constructor TCoreClientInfoList.Create(const aOwnsObjects: boolean);
begin
    inherited Create();
    fList := TObjectList<TCoreClientInfo>.Create(aOwnsObjects);
end;

destructor TCoreClientInfoList.Destroy;
begin
    fList.Free;

    inherited;
end;

function TCoreClientInfoList.FindByClientID(const aClientID: string): TCoreClientInfo;
var
    x: integer;
begin
    for x := 0 to fList.Count - 1 do
    begin
        if (aClientID = fList[x].ClientID) then
            EXIT(fList[x]);
    end;

    EXIT(nil);
end;

function TCoreClientInfoList.GetCount: integer;
begin
    result := fList.Count;
end;

function TCoreClientInfoList.GetItemAt(aIndex: integer): TCoreClientInfo;
begin
    result := fList[aIndex];
end;

procedure TCoreClientInfoList.RemoveByClientID(const aClientID: string);
var
    xClientInfo: TCoreClientInfo;
begin
    xClientInfo := FindByClientID(aClientID);
    if Assigned(xClientInfo) then
        fList.Remove(xClientInfo);
end;

{ TCoreClientInfo }

constructor TCoreClientInfo.Create(const aClientID: string);
begin
    inherited Create();
    fObjects := TRemoteObjectList.Create(true);
    fClientID := aClientID;
    fRunIDs := TList<string>.Create();
end;

destructor TCoreClientInfo.Destroy;
begin
    fRunIDs.Free;
    fObjects.Free;
    inherited;
end;

function TCoreClientInfo.AddObject(const aObject: TObject): integer;
begin
    result := fObjects.Add(aObject);
end;

procedure TCoreClientInfo.RemoveObject(const aID: integer);
begin
    fObjects.Remove(aID);
end;

function TCoreClientInfo.FindObjectByID(const aID: integer): TObject;
begin
    result := fObjects.FindByID(aID);
end;

function TCoreClientInfo.HasRunID(const aRunID: string): boolean;
begin
    result := fRunIDs.IndexOf(aRunID) >= 0;
end;

procedure TCoreClientInfo.AddRunID(const aRunID: string);
begin
    fRunIDs.Add(aRunID)
end;

procedure TCoreClientInfo.RemoveRunID(const aRunID: string);
begin
    fRunIDs.Remove(aRunID);
end;

function TCoreClientInfo.GetIsEventHost: boolean;
begin
    result := Assigned(fEventTransmitter);
end;

function TCoreClientInfo.GetRunIDs(): TStringArray;
begin
    result := fRunIDs.ToArray();
end;

function TCoreClientInfo.GetRunIDCount(): integer;
begin
    result := fRunIDs.Count;
end;

{ TRemoteObjectList }
function TRemoteObjectList.FindByID(const aID: integer): TRemoteObjectItem;
var
    x: integer;
begin
    result := nil;
    for x := 0 to self.Count - 1 do
    begin
        if self[x].ID = aID then
        begin
            result := self[x];
            EXIT;
        end;
    end;
end;

function TRemoteObjectList.GenerateID: integer;
var
    xID: integer;
begin
    xID := 1;
    while true do
    begin
        if not Assigned(FindByID(xID)) then
        begin
            result := xID;
            EXIT;
        end;
        Inc(xID);
    end;
end;

function TRemoteObjectList.Add(const aObject: TObject): integer;
begin
    result := GenerateID();
    inherited Add(TRemoteObjectItem.Create(result, aObject));
end;

function TRemoteObjectList.GetItemAt(aIndex: integer): TRemoteObjectItem;
begin
    result := ( inherited Items[aIndex]) as TRemoteObjectItem;
end;

procedure TRemoteObjectList.Remove(const aID: integer);
var
    xObject: TRemoteObjectItem;
begin
    xObject := FindByID(aID);
    if Assigned(xObject) then
        inherited Remove(xObject);

end;
{ TRemoteObjectItem }

constructor TRemoteObjectItem.Create(const aID: integer; const aObj: TObject);
begin
    inherited Create();
    fID := aID;
    fObj := aObj;
end;


end.
