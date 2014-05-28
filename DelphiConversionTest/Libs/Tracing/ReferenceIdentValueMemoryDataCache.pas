{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232    Initial Revision
  20.02.09 pk                                        TN4232    Changes for multithreaded trace
  25.11.09 pk                                        TN4898    New: Flush function
  04.02.10 pk                                        TN4972    Various Changes
  25.06.10 wl  TDataExDSWrapperReferenceIdentValueMemoryObjData   TN5161    neu: SourceFileName, OrderBy
  15.08.13 wl                                                     TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit ReferenceIdentValueMemoryDataCache;


interface


uses
    XMLReaderWriter,
    Streamable,
    TrackingSemaphore;

type
    TReferenceIdentValueMemoryListReaderWriter = class(TXMLReaderWriter)
    end;

    TReferenceIdentValueMemoryObjData = class(TStreamable);

    TDataExDSWrapperReferenceIdentValueMemoryObjData = class(TReferenceIdentValueMemoryObjData)
    private
        fDefName: string;
        fFilter: string;
        fOrderBy: string;
        fSourceFileName: string;
        fCursorPos: integer;
        fIsOpen: boolean;
    published
        property DefName: string read fDefName write fDefName;
        property Filter: string read fFilter write fFilter;
        property OrderBy: string read fOrderBy write fOrderBy;
        property SourceFileName: string read fSourceFileName write fSourceFileName;
        property CursorPos: integer read fCursorPos write fCursorPos;
        property IsOpen: boolean read fIsOpen write fIsOpen;
    end;

    TReferenceIdentValueMemoryData = class(TStreamable)
    private
        fRefID: integer;
        fObj: TReferenceIdentValueMemoryObjData;
    public
        constructor Create(const aRefID: integer); reintroduce;
    published
        property RefID: integer read fRefID write fRefID;
        property Obj: TReferenceIdentValueMemoryObjData read fObj write fObj;
    end;

    TReferenceIdentValueMemoryListData = class(TStreamable)
    private
        fList: TStreamableObjectList;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TReferenceIdentValueMemoryData;
    public
        constructor Create(); override;
        destructor Destroy(); override;
        procedure Add(const aData: TReferenceIdentValueMemoryData);
        function FindByID(const aRefID: integer): TReferenceIdentValueMemoryData;
        procedure Remove(const aRefID: integer);
        property Count: integer read GetCount;
        property Idents[aIndex: integer]: TReferenceIdentValueMemoryData read GetItemAt; default;
    published
        property List: TStreamableObjectList read fList write fList;
    end;

    TReferenceIdentValueMemoryListDataCache = class
    private
        fCriticalSection: TTrackingSemaphore;
        fReferenceIdentValueMemoryListData: TReferenceIdentValueMemoryListData;
        fReaderWriter: TReferenceIdentValueMemoryListReaderWriter;
    public
        constructor Create(const aPathName: string);
        destructor Destroy(); override;
        function Find(const aRefID: integer): TReferenceIdentValueMemoryData;
        procedure Add(const aData: TReferenceIdentValueMemoryData);
        procedure Remove(const aRefID: integer);
        procedure DataChanged(const aData: TReferenceIdentValueMemoryData);
        procedure Init();
        procedure Clear();
        function read(): boolean;
        procedure Flush();
        property ReferenceIdentValueMemoryListData: TReferenceIdentValueMemoryListData
            read fReferenceIdentValueMemoryListData;
    end;


implementation


uses
    SysUtils,
    ThreadAPI;

const
    cAutoFlushBufferCycle = 0;

    { TReferenceIdentValueMemoryListData }
procedure TReferenceIdentValueMemoryListData.Add(const aData: TReferenceIdentValueMemoryData);
begin
    fList.Add(aData);
end;

constructor TReferenceIdentValueMemoryListData.Create;
begin
    inherited;
    fList := TStreamableObjectList.Create();
end;

destructor TReferenceIdentValueMemoryListData.Destroy;
begin
    fList.Free;
    inherited;
end;

function TReferenceIdentValueMemoryListData.FindByID(const aRefID: integer): TReferenceIdentValueMemoryData;
var
    x: integer;
begin
    result := nil;
    for x := 0 to fList.Count - 1 do
    begin
        if self[x].RefID = aRefID then
        begin
            result := self[x];
            EXIT;
        end;
    end;

end;

function TReferenceIdentValueMemoryListData.GetCount: integer;
begin
    result := fList.Count;
end;

function TReferenceIdentValueMemoryListData.GetItemAt(aIndex: integer): TReferenceIdentValueMemoryData;
begin
    result := fList[aIndex] as TReferenceIdentValueMemoryData;
end;

procedure TReferenceIdentValueMemoryListData.Remove(const aRefID: integer);
var
    xData: TReferenceIdentValueMemoryData;
begin
    xData := FindByID(aRefID);
    fList.Remove(xData);
end;

{ TReferenceIdentValueMemoryListDataCache }

constructor TReferenceIdentValueMemoryListDataCache.Create(const aPathName: string);
begin
    inherited Create();
    fReferenceIdentValueMemoryListData := nil;
    fReaderWriter := TReferenceIdentValueMemoryListReaderWriter.Create(aPathName, cAutoFlushBufferCycle);
    fCriticalSection := TThreadAPI.CreateSemaphore(1);
end;

destructor TReferenceIdentValueMemoryListDataCache.Destroy;
begin
    fCriticalSection.Free;
    fReaderWriter.Free;
    fReferenceIdentValueMemoryListData.Free;
    inherited;
end;

procedure TReferenceIdentValueMemoryListDataCache.Init();
begin
    fReferenceIdentValueMemoryListData := TReferenceIdentValueMemoryListData.Create();
    fReaderWriter.Activate();
    fReaderWriter.AddObjectToRootNode(fReferenceIdentValueMemoryListData);
end;

procedure TReferenceIdentValueMemoryListDataCache.Clear();
begin
    fReaderWriter.DisActivate();
end;

function TReferenceIdentValueMemoryListDataCache.Find(const aRefID: integer): TReferenceIdentValueMemoryData;
begin
    fCriticalSection.Enter();
    try
        result := fReferenceIdentValueMemoryListData.FindByID(aRefID);
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TReferenceIdentValueMemoryListDataCache.Flush;
begin
    fCriticalSection.Enter();
    try
        fReaderWriter.WriteToFile();
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TReferenceIdentValueMemoryListDataCache.Add(const aData: TReferenceIdentValueMemoryData);
begin
    fCriticalSection.Enter();
    try
        fReferenceIdentValueMemoryListData.Add(aData);
        // aData.ID := IntToStr( fReferenceIdentValueMemoryListData.List.Count - 1 );
        // fReaderWriter.AddObjectToItemsVirtualObject( aData, fReferenceIdentValueMemoryListData.List.Tag );
        fReaderWriter.DataChanged();
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TReferenceIdentValueMemoryListDataCache.DataChanged(const aData: TReferenceIdentValueMemoryData);
begin
    fCriticalSection.Enter();
    try
        fReaderWriter.DataChanged();
        // fReaderWriter.SetObjectToVirtualObject( aData, aData.Tag );
    finally
        fCriticalSection.Leave();
    end;
end;

function TReferenceIdentValueMemoryListDataCache.Read(): boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    fReferenceIdentValueMemoryListData :=
        fReaderWriter.CreateObjectFromRootNode<TReferenceIdentValueMemoryListData>();
    if not Assigned(fReferenceIdentValueMemoryListData) then
        EXIT;
    result := true
end;

procedure TReferenceIdentValueMemoryListDataCache.Remove(const aRefID: integer);
// var
// xData : TReferenceIdentValueMemoryData;
begin
    // xData := fReferenceIdentValueMemoryListData.FindByID( aRefID );
    fReaderWriter.DataChanged();
    // fReaderWriter.RemoveObjectFromItemsVirtualObject( xData, fReferenceIdentValueMemoryListData.List.Tag );
    fReferenceIdentValueMemoryListData.Remove(aRefID);
end;

{ TReferenceIdentValueMemoryData }

constructor TReferenceIdentValueMemoryData.Create(const aRefID: integer);
begin
    inherited Create();
    fRefID := aRefID;
end;

// initialization
// RegisterClasses( [ TReferenceIdentValueMemoryListData, TReferenceIdentValueMemoryData,
// TReferenceIdentValueMemoryObjData, TDataExDSWrapperReferenceIdentValueMemoryObjData ] );


end.
