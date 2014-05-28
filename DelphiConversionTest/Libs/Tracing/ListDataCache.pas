{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  24.02.09 pk                                        TN4232    Initial Revision
  25.11.09 pk                                        TN4898    New: Flush function
  04.02.10 pk                                        TN4972    Various Changes
  15.04.10 pk  Create                                TN5050    New constructor with just PathName parameter
  23.04.10 pk  fCriticalSection                      TN5050    Now thread-safe
  07.06.10 pk                                        TN5077    now overrides Init, Clear, etc from TTraceDataCache
  07.06.10 pk  SetPathName                           TN5077    New
  26.10.10 pk  TListDataCache.Remove                 TN5297    New
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  15.08.13 wl                               TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit ListDataCache;


interface


uses
    Classes,
    TraceDataCache,
    XMLReaderWriter,
    Streamable,
    TrackingSemaphore;

type
    // TListData< T_Data : TStreamable > = class( TStreamable ) // caused problems
    TListData = class(TStreamable)
    private
        fList: TStreamableObjectList;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TStreamable;
    public
        constructor Create(); override;
        destructor Destroy(); override;
        procedure Add(const aData: TStreamable);
        procedure Remove(const aData: TStreamable);
        property Count: integer read GetCount;
        property Items[aIndex: integer]: TStreamable read GetItemAt; default;
    published
        property List: TStreamableObjectList read fList write fList;
    end;

    TListDataCache<T_Data: TStreamable> = class(TTraceDataCache)
    private
        fListData: TListData;
        fReaderWriter: TXMLReaderWriter;
        fListID: string;
    protected
        fCriticalSection: TTrackingSemaphore;
        function DoFind(const aID: integer): T_Data; virtual;
        function GetCount: integer;
        function GetTypeID: string; override;
    public
        constructor Create(const aPathName: string; const aListID: string;
            const aFlushBufferCycle: integer); overload;
        constructor Create(const aPathName: string); overload;
        constructor Create(); overload;
        destructor Destroy(); override;
        procedure SetPathName(const aValue: string); override;
        function Find(const aID: integer): T_Data;
        procedure Add(const aData: T_Data);
        procedure Remove(const aData: T_Data);
        procedure DataChanged();
        procedure Init(); override;
        procedure Clear(); override;
        function read(): boolean; override;
        procedure Flush(); override;
        property ListData: TListData read fListData;
        property ListID: string read fListID;
        property Count: integer read GetCount;
    end;


implementation


uses
    SysUtils,
    ThreadAPI;

{ TListData }
constructor TListData.Create();
begin
    inherited Create();
    fList := TStreamableObjectList.Create();
end;

destructor TListData.Destroy;
begin
    fList.Free;
    inherited;
end;

function TListData.GetCount: integer;
begin
    result := fList.Count;
end;

procedure TListData.Add(const aData: TStreamable);
begin
    fList.Add(aData);
end;

procedure TListData.Remove(const aData: TStreamable);
begin
    fList.Remove(aData);
end;

function TListData.GetItemAt(aIndex: integer): TStreamable;
begin
    result := fList[aIndex] as TStreamable;
end;

{ TListDataCache<T_Data> }

constructor TListDataCache<T_Data>.Create(const aPathName: string; const aListID: string;
    const aFlushBufferCycle: integer);
begin
    inherited Create();
    fListID := aListID;
    fListData := TListData.Create();;
    fReaderWriter := TXMLReaderWriter.Create(aPathName, aFlushBufferCycle);
    fCriticalSection := TThreadAPI.CreateSemaphore(1);
end;

constructor TListDataCache<T_Data>.Create(const aPathName: string);
begin
    Create(aPathName, '', 0);
end;

constructor TListDataCache<T_Data>.Create();
begin
    Create('');
end;

destructor TListDataCache<T_Data>.Destroy;
begin
    fCriticalSection.Free;
    fReaderWriter.Free;
    fListData.Free;
    inherited;
end;

function TListDataCache<T_Data>.GetTypeID: string;
begin
    result := '';
end;

procedure TListDataCache<T_Data>.SetPathName(const aValue: string);
begin
    fReaderWriter.SetPathName(aValue);
end;

procedure TListDataCache<T_Data>.Init();
begin
    fReaderWriter.Activate();
    fReaderWriter.AddObjectToRootNode(fListData);
end;

procedure TListDataCache<T_Data>.Clear();
begin
    fCriticalSection.Enter();
    try
        fReaderWriter.DisActivate();
    finally
        fCriticalSection.Leave();
    end;
end;

function TListDataCache<T_Data>.DoFind(const aID: integer): T_Data;
begin
    result := default (T_Data);
    // result := fListData.Find( aID );
end;

function TListDataCache<T_Data>.Find(const aID: integer): T_Data;
begin
    fCriticalSection.Enter();
    try
        result := DoFind(aID);
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TListDataCache<T_Data>.Flush;
begin
    fCriticalSection.Enter();
    try
        fReaderWriter.WriteToFile();
    finally
        fCriticalSection.Leave();
    end;
end;

function TListDataCache<T_Data>.GetCount: integer;
begin
    fCriticalSection.Enter();
    try
        result := fListData.Count;
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TListDataCache<T_Data>.Add(const aData: T_Data);
begin
    fCriticalSection.Enter();
    try
        fListData.Add(aData);
        // aData.ID := IntToStr( fListData.List.Count - 1 );
        // aData.ParentID := fListID;
        fReaderWriter.DataChanged();
        // fReaderWriter.AddObjectToItemsVirtualObject( aData, fListData.List.Tag );
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TListDataCache<T_Data>.Remove(const aData: T_Data);
begin
    fCriticalSection.Enter();
    try
        fListData.Remove(aData);
        fReaderWriter.DataChanged();
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TListDataCache<T_Data>.DataChanged();
begin
    fReaderWriter.DataChanged();
    // fReaderWriter.SetObjectToVirtualObject( aData, aData.Tag );
end;

function TListDataCache<T_Data>.Read(): boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    FreeAndNil(fListData);
    fListData := fReaderWriter.CreateObjectFromRootNode<TListData>();
    if not Assigned(fListData) then
        EXIT;
    result := true
end;


// initialization
// RegisterClasses( [ TListData ] );
end.
