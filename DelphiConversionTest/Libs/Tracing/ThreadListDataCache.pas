{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  20.02.09 pk                                        TN4232    code from Callstackdatacache
  25.11.09 pk                                        TN4898      XMLReaderWriter.FlushToFile renamed to DataChanged
  04.02.10 pk                                        TN4972    Various Changes
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  15.08.13 wl                                        TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit ThreadListDataCache;


interface


uses
    XMLReaderWriter,
    Streamable,
    GeneralTypes,
    TrackingSemaphore;

type
    TThreadData = class(TStreamable)
    private
        fTraceName: string;
        fIsMainThread: boolean;
    published
        constructor Create(const aTraceName: string; const aIsMainThread: boolean); reintroduce;
        property TraceName: string read fTraceName write fTraceName;
        property IsMainThread: boolean read fIsMainThread write fIsMainThread;
    end;

    TThreadListData = class(TStreamable)
    private
        fList: TStreamableObjectList;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property List: TStreamableObjectList read fList write fList;
    end;

    TThreadListDataCache = class
    private
        fCriticalSection: TTrackingSemaphore;
        fThreads: TThreadListData;
        fReaderWriter: TXMLReaderWriter;
        function GetCount: integer;
        function GetThreadDataAt(aIndex: integer): TThreadData;
        function GetMainThread(): TThreadData;
    public
        constructor Create(const aPathName: string);
        destructor Destroy(); override;
        procedure Init();
        procedure Clear();
        function read(): boolean;
        procedure AddThread(aThreadData: TThreadData);
        procedure RemoveThread(const aTraceName: string; const aRemoveFromFile: boolean);
        function FindThread(const aTraceName: string): TThreadData;
        function GetTraceNamesForSecondaryThreads(): TStringArray;
        property MainThread: TThreadData read GetMainThread;
        property Count: integer read GetCount;
        property Threads[aIndex: integer]: TThreadData read GetThreadDataAt; default;
        // function FindThreadBySourceDataName( const aSourceDataName: string ): TThreadData;
    end;


implementation


uses
    SysUtils,
    Classes,
    ThreadAPI;

{ TThreadListData }

constructor TThreadListData.Create;
begin
    inherited;
    fList := TStreamableObjectList.Create();
end;

destructor TThreadListData.Destroy;
begin
    fList.Free;
    inherited;
end;

{ TThreadListDataCache }
constructor TThreadListDataCache.Create(const aPathName: string);
begin
    inherited Create();
    fReaderWriter := TXMLReaderWriter.Create(aPathName, 1);
    fThreads := nil;
    fCriticalSection := TThreadAPI.CreateSemaphore(1);
end;

destructor TThreadListDataCache.Destroy;
begin
    FreeAndNil(fCriticalSection);
    FreeAndNil(fThreads);
    FreeAndNil(fReaderWriter);
    inherited;
end;

function TThreadListDataCache.FindThread(const aTraceName: string): TThreadData;
var
    x: integer;
begin
    result := nil;
    fCriticalSection.Enter();
    try
        for x := 0 to fThreads.List.Count - 1 do
        begin
            if SameText((fThreads.List.Items[x] as TThreadData).TraceName, aTraceName) then
            begin
                result := fThreads.List.Items[x] as TThreadData;
                EXIT;
            end;
        end;
    finally
        fCriticalSection.Leave();
    end;
end;

{
  function TThreadListDataCache.FindThreadBySourceDataName(
  const aSourceDataName: string): TThreadData;
  var
  x : integer;
  begin
  result := nil;
  for x := 0 to fThreads.List.Count - 1 do begin
  if SameText( ( fThreads.List.Items[ x ] as TThreadData ).SourceDataName, aSourceDataName ) then begin
  result := fThreads.List.Items[ x ] as TThreadData;
  EXIT;
  end;
  end;
  end;
}
procedure TThreadListDataCache.AddThread(aThreadData: TThreadData);
begin
    fCriticalSection.Enter();
    try
        fThreads.List.Add(aThreadData);
        // aThreadData.ID := IntToStr( fThreads.List.Count - 1 );
        fReaderWriter.DataChanged();
        // fReaderWriter.AddObjectToItemsVirtualObject( aThreadData, fThreads.List.Tag );
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TThreadListDataCache.RemoveThread(const aTraceName: string; const aRemoveFromFile: boolean);
var
    xThread: TThreadData;
begin
    if not aRemoveFromFile then
        EXIT;

    fCriticalSection.Enter();
    try
        xThread := FindThread(aTraceName);
        fReaderWriter.DataChanged();
        // fReaderWriter.RemoveObjectFromItemsVirtualObject( xThread, fThreads.List.Tag );
        fThreads.List.Remove(xThread);
    finally
        fCriticalSection.Leave();
    end;
end;

function TThreadListDataCache.GetMainThread: TThreadData;
var
    x: integer;
    xThreadData: TThreadData;
begin
    result := nil;
    fCriticalSection.Enter();
    try
        for x := 0 to self.Count - 1 do
        begin
            xThreadData := self[x];
            if xThreadData.IsMainThread then
            begin
                result := xThreadData;
                EXIT;
            end;
        end;
    finally
        fCriticalSection.Leave();
    end;
end;

function TThreadListDataCache.GetTraceNamesForSecondaryThreads: TStringArray;
var
    x: integer;
    xCount: integer;
begin
    xCount := 0;

    fCriticalSection.Enter();
    try
        SetLength(result, fThreads.List.Count - 1);
        for x := 0 to fThreads.List.Count - 1 do
        begin
            if not(fThreads.List.Items[x] as TThreadData).IsMainThread then
            begin
                result[xCount] := (fThreads.List.Items[x] as TThreadData).TraceName;
                Inc(xCount);
            end;
        end;
    finally
        fCriticalSection.Leave();
    end;
end;

function TThreadListDataCache.Read(): boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    fThreads := fReaderWriter.CreateObjectFromRootNode<TThreadListData>();

    if not Assigned(fThreads) then
        EXIT;

    result := true;
end;

procedure TThreadListDataCache.Init;
begin
    read();
    if not Assigned(fThreads) then
    begin
        fThreads := TThreadListData.Create();
        fReaderWriter.AddObjectToRootNode(fThreads);
    end;
end;

function TThreadListDataCache.GetCount: integer;
begin
    result := fThreads.List.Count;
end;

function TThreadListDataCache.GetThreadDataAt(aIndex: integer): TThreadData;
begin
    result := fThreads.List[aIndex] as TThreadData;
end;

procedure TThreadListDataCache.Clear;
begin
    FreeAndNil(fThreads);
    fReaderWriter.DisActivate();
end;

{ TThreadData }

constructor TThreadData.Create(const aTraceName: string; const aIsMainThread: boolean);
begin
    inherited Create();
    fTraceName := aTraceName;
    fIsMainThread := aIsMainThread;
end;

// initialization
// RegisterClasses( [ TThreadData, TThreadListData ] );


end.
