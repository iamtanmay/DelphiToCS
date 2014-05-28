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

unit ProcessRegistry;


interface


uses
    SyncObjs,
    Generics.Collections,
    ListClasses,
    Executable,
    ThreadClasses;

type
    TProcessThreadSysHandleIDItem = class
    private
        fSysHandleID: TSysHandleID;
        fTID: cardinal;
    public
        constructor Create(const aTID: TSysObjectID; const aSysHandleID: TSysHandleID);
        property SysHandleID: TSysHandleID read fSysHandleID;
        property TID: TSysObjectID read fTID;
    end;

    TProcessThreadSysHandleIDList = class(TObjectList<TProcessThreadSysHandleIDItem>)
    public
        function FindBySysHandleID(const aSysHandleID: TSysHandleID): TProcessThreadSysHandleIDItem;
        function FindByTID(const aTID: TSysObjectID): TProcessThreadSysHandleIDItem;
    end;

    TProcessThreadList = class(TIntegerKeyObjectValueList)

    end;

    TProcess = class
    protected
        fPCB: TProcessControlBlock;
        fAddressSpace: TAddressSpace;
        fThreads: TProcessThreadList;
        fThreadSysHandleIDs: TProcessThreadSysHandleIDList;
        fChildPIDs: TIntegerKeyObjectValueList;
        fPrimaryThreadTID: cardinal;
        fSourceDataName: string;
        fSysObjectID: TSysObjectID;
        fIsSimulated: boolean;
        fIsAppProcess: boolean;
        fIsSystemProcess: boolean;
        // the handle added on creating a thread dont use this internally
        fCloseUserSysHandleOnFinishInfo: TCloseUserSysHandleOnFinishInfo;

        function GetPrimaryThreadSysHandleID: TSysHandleID;
    public
        constructor Create(const aPID: integer);
        destructor Destroy(); override;

        procedure SetAddressSpace(const aAddressSpace: TAddressSpace);
        procedure SetSysObjectID(const aValue: TSysObjectID);
        procedure SetCloseUserSysHandleOnFinishInfo(const aCloseSysHandleOnFinish: boolean;
            const aProcessSysHandleID: TSysHandleID);

        property SourceDataName: string read fSourceDataName write fSourceDataName;
        property IsSimulated: boolean read fIsSimulated write fIsSimulated;
        property PCB: TProcessControlBlock read fPCB;
        procedure AddThread(const aTID: cardinal);
        procedure AddThreadSysHandle(const aTID: cardinal; const aSysHandleID: TSysHandleID);
        procedure RemoveThreadSysHandle(const aTID: cardinal);
        procedure AddChildPID(const aPID: integer);
        procedure RemoveThread(const aTID: cardinal);
        property Threads: TProcessThreadList read fThreads;
        property ThreadSysHandleIDs: TProcessThreadSysHandleIDList read fThreadSysHandleIDs;
        property PrimaryThreadTID: cardinal read fPrimaryThreadTID;
        property PrimaryThreadSysHandleID: TSysHandleID read GetPrimaryThreadSysHandleID;
        property AddressSpace: TAddressSpace read fAddressSpace;
        property IsAppProcess: boolean read fIsAppProcess write fIsAppProcess;
        property IsSystemProcess: boolean read fIsSystemProcess write fIsSystemProcess;
        property CloseUserSysHandleOnFinishInfo: TCloseUserSysHandleOnFinishInfo
            read fCloseUserSysHandleOnFinishInfo;
        property SysObjectID: TSysObjectID read fSysObjectID;
    end;

    TProcessList = class
    private
        function GetCount: integer;
        function GetProcessAt(aIndex: integer): TProcess;
    protected
        fList: TIntegerKeyObjectValueList;
        fCriticalSection: TCriticalSection;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Add(const aProcess: TProcess);
        procedure DeleteByID(const aPID: integer);
        function FindProcessByID(const aPID: integer): TProcess;
        property Count: integer read GetCount;
        property this[aIndex: integer]: TProcess read GetProcessAt; default;
    end;

    TProcessRegistry = class
    private
        function GetProcessCount: integer;
    protected
        fProcesses: TProcessList;
        function GeneratePID(): cardinal;
        function CreateProcess: TProcess;
        procedure RegisterProcess(const aProcess: TProcess);
        function GetProcessAt(aIndex: integer): TProcess;
    public
        constructor Create();
        destructor Destroy(); override;
        function CreateProcessAndRegister(): TProcess;
        procedure DestroyProcessAndUnRegister(const aPID: cardinal);
        function FindProcessByID(const aPID: cardinal): TProcess;
        property ProcessCount: integer read GetProcessCount;
        property Processes[aIndex: integer]: TProcess read GetProcessAt; default;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TProcessRegistry;
        class procedure SetInstance(const aValue: TProcessRegistry);
    end;


implementation


uses
    SysUtils;

var
    uProcessRegistry: TProcessRegistry = nil;

    { TProcessThreadSysHandleIDItem }

constructor TProcessThreadSysHandleIDItem.Create(const aTID: cardinal; const aSysHandleID: TSysHandleID);
begin
    inherited Create();
    fTID := aTID;
    fSysHandleID := aSysHandleID;
end;

{ TProcessThreadList }

function TProcessThreadSysHandleIDList.FindByTID(const aTID: cardinal): TProcessThreadSysHandleIDItem;
var
    xThreadItem: TProcessThreadSysHandleIDItem;
begin
    result := nil;
    for xThreadItem in self do
    begin
        if xThreadItem.TID = aTID then
        begin
            result := xThreadItem;
            EXIT;
        end;
    end;
end;

function TProcessThreadSysHandleIDList.FindBySysHandleID(const aSysHandleID: TSysHandleID)
    : TProcessThreadSysHandleIDItem;
var
    xThreadItem: TProcessThreadSysHandleIDItem;
begin
    result := nil;
    for xThreadItem in self do
    begin
        if xThreadItem.SysHandleID = aSysHandleID then
        begin
            result := xThreadItem;
            EXIT;
        end;
    end;
end;

{ TProcess }

constructor TProcess.Create(const aPID: integer);
begin
    inherited Create();

    fPrimaryThreadTID := cThreadIDNone;
    fPCB := TProcessControlBlock.Create(aPID);
    fAddressSpace := nil;
    fIsSimulated := false;
    fThreadSysHandleIDs := TProcessThreadSysHandleIDList.Create();
    fThreads := TProcessThreadList.Create();
    fChildPIDs := TIntegerKeyObjectValueList.Create();
    self.SetCloseUserSysHandleOnFinishInfo(false, cHandleIDNone);
    fIsAppProcess := false;
    fIsSystemProcess := false;
end;

destructor TProcess.Destroy;
begin
    FreeAndNil(fChildPIDs);
    FreeAndNil(fAddressSpace);
    FreeAndNil(fThreads);
    FreeAndNil(fThreadSysHandleIDs);
    FreeAndNil(fPCB);

    inherited;
end;

function TProcess.GetPrimaryThreadSysHandleID: TSysHandleID;
var
    xProcessThreadSysHandleIDItem: TProcessThreadSysHandleIDItem;
begin
    result := cHandleIDNone;
    xProcessThreadSysHandleIDItem := fThreadSysHandleIDs.FindByTID(fPrimaryThreadTID);
    if not Assigned(xProcessThreadSysHandleIDItem) then
        EXIT;
    result := xProcessThreadSysHandleIDItem.SysHandleID;
end;

procedure TProcess.AddThread(const aTID: cardinal);
begin
    if self.Threads.Count = 0 then
    begin
        fPrimaryThreadTID := aTID;
    end;

    fThreads.Add(aTID);
end;

procedure TProcess.AddThreadSysHandle(const aTID: cardinal; const aSysHandleID: TSysHandleID);
begin
    fThreadSysHandleIDs.Add(TProcessThreadSysHandleIDItem.Create(aTID, aSysHandleID));
end;

procedure TProcess.RemoveThreadSysHandle(const aTID: cardinal);
var
    xThreadItem: TProcessThreadSysHandleIDItem;
begin
    xThreadItem := fThreadSysHandleIDs.FindByTID(aTID);
    fThreadSysHandleIDs.Remove(xThreadItem);
end;

procedure TProcess.AddChildPID(const aPID: integer);
begin
    fChildPIDs.Add(aPID);
end;

procedure TProcess.RemoveThread(const aTID: cardinal);
begin
    fThreads.Remove(aTID);
end;

procedure TProcess.SetCloseUserSysHandleOnFinishInfo(const aCloseSysHandleOnFinish: boolean;
    const aProcessSysHandleID: TSysHandleID);
var
    xSysHandleID: TSysHandleID;
begin
    fCloseUserSysHandleOnFinishInfo.CloseSysHandleOnFinish := aCloseSysHandleOnFinish;

    xSysHandleID := aProcessSysHandleID;
    if not fCloseUserSysHandleOnFinishInfo.CloseSysHandleOnFinish then
        xSysHandleID := cHandleIDNone;

    fCloseUserSysHandleOnFinishInfo.SysHandleID := xSysHandleID;
end;

procedure TProcess.SetSysObjectID(const aValue: TSysObjectID);
begin
    fSysObjectID := aValue;
end;

procedure TProcess.SetAddressSpace(const aAddressSpace: TAddressSpace);
begin
    fAddressSpace := aAddressSpace;
end;

{ TProcessList }

constructor TProcessList.Create;
begin
    inherited;

    fList := TIntegerKeyObjectValueList.Create();
    fCriticalSection := TCriticalSection.Create();
end;

destructor TProcessList.Destroy;
begin
    fCriticalSection.Free;
    fList.Free;

    inherited;
end;

procedure TProcessList.Add(const aProcess: TProcess);
begin
    fCriticalSection.Acquire;
    try
        fList.AddObject(aProcess.PCB.PID, aProcess);
    finally
        fCriticalSection.Release;
    end;
end;

function TProcessList.FindProcessByID(const aPID: integer): TProcess;
var
    xIndex: integer;
begin
    result := nil;
    fCriticalSection.Acquire;
    try
        xIndex := fList.IndexOf(aPID);
        if xIndex < 0 then
            EXIT;
        result := self[xIndex];
    finally
        fCriticalSection.Release;
    end;
end;

function TProcessList.GetCount: integer;
begin
    fCriticalSection.Acquire;
    try
        result := fList.Count;
    finally
        fCriticalSection.Release;
    end;
end;

function TProcessList.GetProcessAt(aIndex: integer): TProcess;
begin
    fCriticalSection.Acquire;
    try
        result := fList.Objects[aIndex] as TProcess;
    finally
        fCriticalSection.Release;
    end;
end;

procedure TProcessList.DeleteByID(const aPID: integer);
var
    xIndex: integer;
begin
    fCriticalSection.Acquire;
    try
        xIndex := fList.IndexOf(aPID);
        self.fList.Delete(xIndex);
    finally
        fCriticalSection.Release;
    end;
end;

{ TProcessRegistry }

class procedure TProcessRegistry.CreateInstance;
begin
    uProcessRegistry := TProcessRegistry.Create();
end;

class procedure TProcessRegistry.DestroyInstance;
begin
    uProcessRegistry.Free;
end;

class function TProcessRegistry.Instance: TProcessRegistry;
begin
    result := uProcessRegistry;
end;

class procedure TProcessRegistry.SetInstance(const aValue: TProcessRegistry);
begin
    uProcessRegistry := aValue;
end;

constructor TProcessRegistry.Create;
begin
    inherited Create();
    fProcesses := TProcessList.Create();
end;

destructor TProcessRegistry.Destroy;
begin
    fProcesses.Free;
    inherited;
end;

procedure TProcessRegistry.RegisterProcess(const aProcess: TProcess);
begin
    fProcesses.Add(aProcess);
end;

function TProcessRegistry.CreateProcess: TProcess;
var
    xPID: integer;
begin
    xPID := self.GeneratePID();
    result := TProcess.Create(xPID);
end;

function TProcessRegistry.CreateProcessAndRegister: TProcess;
begin
    result := CreateProcess();
    self.RegisterProcess(result);
end;

procedure TProcessRegistry.DestroyProcessAndUnRegister(const aPID: cardinal);
begin
    fProcesses.DeleteByID(aPID);
end;

function TProcessRegistry.FindProcessByID(const aPID: cardinal): TProcess;
begin
    result := fProcesses.FindProcessByID(aPID);
end;

function TProcessRegistry.GeneratePID: cardinal;
var
    xPID: integer;
begin
    xPID := 1;
    while true do
    begin
        if not Assigned(FindProcessByID(xPID)) then
        begin
            result := xPID;
            EXIT;
        end;
        Inc(xPID);
    end;
end;

function TProcessRegistry.GetProcessCount: integer;
begin
    result := fProcesses.Count;
end;

function TProcessRegistry.GetProcessAt(aIndex: integer): TProcess;
begin
    result := fProcesses[aIndex];
end;


end.
