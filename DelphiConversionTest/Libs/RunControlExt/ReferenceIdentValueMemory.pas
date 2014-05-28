{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232      Initial Revision
  20.02.09 pk                                        TN4232      Changes for multithreaded trace
  30.03.09 pk  TReferenceIdentValueMemoryManager     TN4495      New: Clear
  30.03.09 pk  TReferenceIdentValueMemory            TN4495      New: Destroy - frees fObj
  04.11.09 pk                               	        TN4843      Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  16.11.09 pk                                        TN4843      Remove: do not free object already done in List object
  15.08.13 wl                                        TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit ReferenceIdentValueMemory;


interface


uses
    ListClasses,
    TrackingSemaphore;

type
    TReferenceIdentValueMemory = class
    private
        fID: integer;
        fObj: TObject;
    public
        constructor Create(const aID: integer; const aObj: TObject);
        destructor Destroy(); override;
        property ID: integer read fID;
        property Obj: TObject read fObj;
    end;

    TReferenceIdentValueMemoryList = class
    private
        fList: TIntegerKeyObjectValueList;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TReferenceIdentValueMemory;
        procedure RemoveByIndex(aIndex: integer);
    public
        constructor Create();
        destructor Destroy(); override;
        function IndexOf(const aID: integer): integer;
        procedure Add(const aID: integer; const aItem: TReferenceIdentValueMemory);
        procedure Clear();
        procedure Remove(const aID: integer);
        function Find(const aID: integer): TReferenceIdentValueMemory;
        property Count: integer read GetCount;
        property Items[aIndex: integer]: TReferenceIdentValueMemory read GetItemAt; default;
    end;

    TReferenceIdentValueMemoryManager = class
    private
        fCriticalSection: TTrackingSemaphore;
        fList: TReferenceIdentValueMemoryList;
        constructor Create();
        function GenerateNewID(): integer;
    public
        destructor Destroy(); override;
        function Add(const aObject: TObject): integer;
        procedure AddWithID(const aID: integer; const aObject: TObject);
        function Find(const aID: integer): TReferenceIdentValueMemory;
        procedure Remove(const aID: integer);
        procedure Clear();
        class procedure CreateInstance();
        class function Instance(): TReferenceIdentValueMemoryManager;
        class procedure DestroyInstance();

    end;


implementation


uses
    ThreadAPI;

var
    uReferenceIdentValueMemoryManagerInstance: TReferenceIdentValueMemoryManager;

    { TReferenceIdentValueMemoryManager }
class procedure TReferenceIdentValueMemoryManager.CreateInstance;
begin
    if Assigned(uReferenceIdentValueMemoryManagerInstance) then
        EXIT;
    uReferenceIdentValueMemoryManagerInstance := TReferenceIdentValueMemoryManager.Create();
end;

class procedure TReferenceIdentValueMemoryManager.DestroyInstance;
begin
    uReferenceIdentValueMemoryManagerInstance.Free;
end;

class function TReferenceIdentValueMemoryManager.Instance: TReferenceIdentValueMemoryManager;
begin
    result := uReferenceIdentValueMemoryManagerInstance;
end;

constructor TReferenceIdentValueMemoryManager.Create;
begin
    inherited Create();
    fList := TReferenceIdentValueMemoryList.Create();
    fCriticalSection := TThreadAPI.CreateSemaphore(1);
end;

destructor TReferenceIdentValueMemoryManager.Destroy;
begin
    Clear();
    fCriticalSection.Free;
    fList.Free;
    inherited;
end;

function TReferenceIdentValueMemoryManager.GenerateNewID(): integer;
var
    xID: integer;
begin
    xID := 1;
    while true do
    begin
        if fList.IndexOf(xID) < 0 then
        begin
            result := xID;
            EXIT;
        end;
        Inc(xID);
    end;
end;

procedure TReferenceIdentValueMemoryManager.AddWithID(const aID: integer; const aObject: TObject);
begin
    fList.Add(aID, TReferenceIdentValueMemory.Create(aID, aObject));
end;

function TReferenceIdentValueMemoryManager.Add(const aObject: TObject): integer;
begin
    fCriticalSection.Enter();
    try
        result := GenerateNewID();
        AddWithID(result, aObject);
    finally
        fCriticalSection.Leave();
    end;
end;

function TReferenceIdentValueMemoryManager.Find(const aID: integer): TReferenceIdentValueMemory;
begin
    fCriticalSection.Enter();
    try
        result := fList.Find(aID);
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TReferenceIdentValueMemoryManager.Remove(const aID: integer);
begin
    fCriticalSection.Enter();
    try
        fList.Remove(aID);
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TReferenceIdentValueMemoryManager.Clear;
begin
    fCriticalSection.Enter();
    try
        fList.Clear();
    finally
        fCriticalSection.Leave();
    end;
end;

{ TReferenceIdentValueMemoryList }

function TReferenceIdentValueMemoryList.GetCount: integer;
begin
    result := fList.Count;
end;

function TReferenceIdentValueMemoryList.GetItemAt(aIndex: integer): TReferenceIdentValueMemory;
begin
    result := fList.Objects[aIndex] as TReferenceIdentValueMemory;
end;

function TReferenceIdentValueMemoryList.IndexOf(const aID: integer): integer;
begin
    result := fList.IndexOf(aID);
end;

procedure TReferenceIdentValueMemoryList.Add(const aID: integer; const aItem: TReferenceIdentValueMemory);
begin
    fList.AddObject(aID, aItem);
end;

function TReferenceIdentValueMemoryList.Find(const aID: integer): TReferenceIdentValueMemory;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := fList.IndexOf(aID);
    if xIndex < 0 then
        EXIT;
    result := self[xIndex];
end;

constructor TReferenceIdentValueMemoryList.Create;
begin
    inherited;
    fList := TIntegerKeyObjectValueList.Create(true);
end;

destructor TReferenceIdentValueMemoryList.Destroy;
begin
    fList.Free;
    inherited;
end;

procedure TReferenceIdentValueMemoryList.RemoveByIndex(aIndex: integer);
begin
    fList.Delete(aIndex);
end;

procedure TReferenceIdentValueMemoryList.Remove(const aID: integer);
var
    xIndex: integer;
begin
    xIndex := fList.IndexOf(aID);
    RemoveByIndex(xIndex);
end;

procedure TReferenceIdentValueMemoryList.Clear;
var
    x: integer;
begin
    for x := fList.Count - 1 downto 0 do
    begin
        RemoveByIndex(x);
    end;
end;

{ TReferenceIdentValueMemory }

constructor TReferenceIdentValueMemory.Create(const aID: integer; const aObj: TObject);
begin
    inherited Create();
    fID := aID;
    fObj := aObj;
end;

destructor TReferenceIdentValueMemory.Destroy;
begin
    fObj.Free;
    inherited;
end;


end.
