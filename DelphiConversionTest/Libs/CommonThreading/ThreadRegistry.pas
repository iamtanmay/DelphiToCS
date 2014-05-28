{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  15.08.13 wl                                      TN6223   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit ThreadRegistry;


interface


uses
    SyncObjs,
    ThreadClasses,
    ListClasses,
    Executable;

type
    TThreadRegistryItem = class
    private
        fThreadImage: TThreadImage;
    public
        constructor Create();
        destructor Destroy(); override;
        property ThreadImage: TThreadImage read fThreadImage write fThreadImage;
    end;

    TThreadRegistryList = class
    private
        fList: TIntegerKeyObjectValueList;
        fCriticalSection: TCriticalSection;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TThreadRegistryItem;
        function IndexOf(const aID: cardinal): integer;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Add(aItem: TThreadRegistryItem);
        function FindByID(const aID: cardinal; aMustFind: boolean): TThreadRegistryItem;
        function FindTIDByOSThreadID(const aOSThreadID: cardinal): cardinal;
        procedure Delete(aIndex: integer);
        procedure RemoveByID(const aID: cardinal);
        procedure Clear();
        property Items[aIndex: integer]: TThreadRegistryItem read GetItemAt; default;
        property Count: integer read GetCount;
    end;

    TThreadRegistryEvent = procedure(aRegistryItem: TThreadRegistryItem) of object;

    TThreadRegistry = class
    private
        fThreadList: TThreadRegistryList;
        fOnAfterRegistered, fOnBeforeUnRegistered: TThreadRegistryEvent;
        function FindThreadRegistryItem(const aThreadID: cardinal): TThreadRegistryItem;
    public
        constructor Create();
        destructor Destroy(); override;

        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TThreadRegistry;
        class procedure SetInstance(const aValue: TThreadRegistry);

        function CreateRegistryItem(): TThreadRegistryItem; virtual;
        procedure RegisterAppThread(const aThreadImage: TThreadImage);
        procedure RegisterThread(aRegistryItem: TThreadRegistryItem);
        procedure UnRegisterThread(const aTID: cardinal);
        function CreateThreadWithRegistryItem(const aPID: cardinal; aRegistryItem: TThreadRegistryItem;
            const aDescription: string; const aCreateSuspended: boolean; aCanSuspendSafe: boolean;
            const aExecutable: IExecutable): TExecThread;
        function GenerateUniqueThreadDescription(const aDescription: string): string;
        function GetThreadInfoByDescription(const aDescription: string): TThreadImage;
        function FindThreadImageByThreadID(const aTID: cardinal): TThreadImage;
        function FindCurrentThreadID(): cardinal;

        property OnAfterRegistered: TThreadRegistryEvent read fOnAfterRegistered write fOnAfterRegistered;
        property OnBeforeUnRegistered: TThreadRegistryEvent read fOnBeforeUnRegistered
            write fOnBeforeUnRegistered;
    end;


implementation


uses
    SysUtils,
    ThreadUtils;

var
    uThreadRegistry: TThreadRegistry = nil;

    { TThreadRegistryList }

constructor TThreadRegistryList.Create();
begin
    inherited Create();
    fCriticalSection := TCriticalSection.Create();
    fList := TIntegerKeyObjectValueList.Create(false);
end;

destructor TThreadRegistryList.Destroy;
begin
    fList.Free;
    fCriticalSection.Free;
    inherited;
end;

procedure TThreadRegistryList.Add(aItem: TThreadRegistryItem);
begin
    fCriticalSection.Acquire;
    try
        fList.AddObject(aItem.ThreadImage.TCB.TID, aItem);
    finally
        fCriticalSection.Release;
    end;
end;

function TThreadRegistryList.GetCount: integer;
begin
    fCriticalSection.Acquire;
    try
        result := fList.Count;
    finally
        fCriticalSection.Release;
    end;
end;

function TThreadRegistryList.GetItemAt(aIndex: integer): TThreadRegistryItem;
begin
    fCriticalSection.Acquire;
    try
        result := fList.Objects[aIndex] as TThreadRegistryItem;
    finally
        fCriticalSection.Release;
    end;
end;

function TThreadRegistryList.IndexOf(const aID: cardinal): integer;
begin
    fCriticalSection.Acquire;
    try
        result := fList.IndexOf(aID);
    finally
        fCriticalSection.Release;
    end;
end;

function TThreadRegistryList.FindByID(const aID: cardinal; aMustFind: boolean): TThreadRegistryItem;
var
    xIndex: integer;
begin
    fCriticalSection.Acquire;
    try
        result := nil;
        xIndex := self.IndexOf(aID);
        if xIndex < 0 then
        begin
            if aMustFind then
                raise Exception.CreateFmt('ID %d could not be found', [aID]);
            EXIT;
        end;

        result := self[xIndex];
    finally
        fCriticalSection.Release;
    end;
end;

function TThreadRegistryList.FindTIDByOSThreadID(const aOSThreadID: cardinal): cardinal;
var
    x: integer;
    xItem: TThreadRegistryItem;
begin
    result := cThreadIDNone;

    fCriticalSection.Acquire;
    try
        for x := 0 to self.Count - 1 do
        begin
            xItem := self[x];
            if xItem.ThreadImage.TCB.OSThreadID = aOSThreadID then
            begin
                result := xItem.ThreadImage.TCB.TID;
                EXIT;
            end;
        end;
    finally
        fCriticalSection.Release;
    end;
end;

procedure TThreadRegistryList.Clear;
begin
    fCriticalSection.Acquire;
    try
        fList.Clear();
    finally
        fCriticalSection.Release;
    end;
end;

procedure TThreadRegistryList.Delete(aIndex: integer);
begin
    fCriticalSection.Acquire;
    try
        fList.Delete(aIndex);
    finally
        fCriticalSection.Release;
    end;
end;

procedure TThreadRegistryList.RemoveByID(const aID: cardinal);
var
    xIndex: integer;
begin
    fCriticalSection.Acquire;
    try
        xIndex := IndexOf(aID);
        Delete(xIndex);
    finally
        fCriticalSection.Release;
    end;
end;

{ TThreadRegistryItem }

constructor TThreadRegistryItem.Create();
begin
    inherited Create;
end;

destructor TThreadRegistryItem.Destroy();
begin
    inherited;
end;

{ TThreadRegistry }

class procedure TThreadRegistry.CreateInstance();
begin
    if Assigned(uThreadRegistry) then
        EXIT;
    SetInstance(TThreadRegistry.Create());
end;

class procedure TThreadRegistry.DestroyInstance();
begin
    uThreadRegistry.Free;
end;

class function TThreadRegistry.Instance(): TThreadRegistry;
begin
    result := uThreadRegistry;
end;

class procedure TThreadRegistry.SetInstance(const aValue: TThreadRegistry);
begin
    uThreadRegistry := aValue;
end;

constructor TThreadRegistry.Create();
begin
    inherited;
    fThreadList := TThreadRegistryList.Create();
end;

destructor TThreadRegistry.Destroy();
begin
    fThreadList.Free;
    inherited;
end;

procedure TThreadRegistry.RegisterAppThread(const aThreadImage: TThreadImage);
var
    xRegistryItem: TThreadRegistryItem;
begin

    xRegistryItem := TThreadRegistryItem.Create();
    xRegistryItem.ThreadImage := aThreadImage;

    self.RegisterThread(xRegistryItem);
end;

function TThreadRegistry.FindThreadRegistryItem(const aThreadID: cardinal): TThreadRegistryItem;
begin
    result := fThreadList.FindByID(aThreadID, false);
end;

function TThreadRegistry.FindThreadImageByThreadID(const aTID: cardinal): TThreadImage;
var
    xItem: TThreadRegistryItem;
begin
    result := nil;
    xItem := FindThreadRegistryItem(aTID);
    if not Assigned(xItem) then
        EXIT;
    // ASSERT( Assigned( xItem ), Format( 'ThreadID %d not found in registry', [ aThreadID ] ) );
    result := xItem.ThreadImage;
end;

function TThreadRegistry.FindCurrentThreadID(): cardinal;
var
    xOSCurrentThreadID: cardinal;
begin
    xOSCurrentThreadID := TPlatformSpecificOS.GetCurrentThreadID;
    result := fThreadList.FindTIDByOSThreadID(xOSCurrentThreadID);
end;

function TThreadRegistry.CreateRegistryItem(): TThreadRegistryItem;
begin
    result := TThreadRegistryItem.Create();
end;

procedure TThreadRegistry.RegisterThread(aRegistryItem: TThreadRegistryItem);
begin
    fThreadList.Add(aRegistryItem);
    if Assigned(fOnAfterRegistered) then
        fOnAfterRegistered(aRegistryItem);
end;

procedure TThreadRegistry.UnRegisterThread(const aTID: cardinal);
var
    xItem: TThreadRegistryItem;
begin
    xItem := self.FindThreadRegistryItem(aTID);
    if not Assigned(xItem) then
        Exit;
    if Assigned(fOnBeforeUnRegistered) then
        fOnBeforeUnRegistered(xItem);
    fThreadList.RemoveByID(aTID);
    xItem.Free;
end;

function TThreadRegistry.CreateThreadWithRegistryItem(const aPID: cardinal;
    aRegistryItem: TThreadRegistryItem; const aDescription: string; const aCreateSuspended: boolean;
    aCanSuspendSafe: boolean; const aExecutable: IExecutable): TExecThread;

begin
    aRegistryItem.ThreadImage.Description := aDescription;
    aRegistryItem.ThreadImage.TCB.PID := aPID;
    aRegistryItem.ThreadImage.TCB.Executable := aExecutable;

    result := TExecThread.Create(aRegistryItem.ThreadImage, aCreateSuspended, false, aExecutable,
        aCanSuspendSafe);

    aRegistryItem.ThreadImage.SetThread(result);
    RegisterThread(aRegistryItem);
end;

function TThreadRegistry.GetThreadInfoByDescription(const aDescription: string): TThreadImage;
var
    x: integer;
    xItem: TThreadRegistryItem;
begin
    result := nil;
    for x := 0 to fThreadList.Count - 1 do
    begin
        xItem := fThreadList[x];
        if SameText(xItem.ThreadImage.Description, aDescription) then
        begin
            result := xItem.fThreadImage;
            EXIT;
        end;
    end;
end;

function TThreadRegistry.GenerateUniqueThreadDescription(const aDescription: string): string;
var
    xID: integer;
begin
    xID := 0;
    result := aDescription;
    while true do
    begin
        if not Assigned(GetThreadInfoByDescription(result)) then
            EXIT;
        Inc(xID);
        ASSERT(xID < high(integer));
        result := Format('%s_%d', [aDescription, xID]);
    end;

end;


end.
