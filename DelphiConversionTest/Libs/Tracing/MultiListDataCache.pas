{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  24.02.09 pk                                        TN4232    Initial Revision
  04.11.09 pk                               	        TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  25.11.09 pk                                        TN4898    New: Flush function
  04.02.10 pk                                        TN4972    Various Changes
  23.04.10 pk                                        TN5072    Changes to TMultiListDataCacheIterator
  26.10.10 pk  TMultiListDataCache.Remove            TN5297    New
  27.10.10 pk  Remove                                TN5297    call switchtocache to move to previous cache
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  10.04.13 wl                                        TN6045   uses Generics.Collections
  ----------------------------------------------------------------------------------------------------------------------- }

unit MultiListDataCache;


interface


uses
    Generics.Collections,
    XMLReaderWriter,
    Streamable,
    GeneralTypes,
    ListDataCache,
    TOCDataCache;

type
    TMultiListDataCacheIterator<T_Data: TStreamable> = class;

    TMultiListDataCache<T_Data: TStreamable> = class
    private
        fPathNamePrefix: string;

        fMaxItemsPerList: integer;
        fCurrentDataCache: TListDataCache<T_Data>;
        fTOCListDataCache: TTOCListDataCache;
        fDirtyDataCaches: TObjectList<TListDataCache<T_Data>>;
        fFlushBufferCycle: integer;
        procedure StartNewSubList();
        function GetPathNameForListID(const aListID: integer): string;

        function CreateCache(const aListID: integer): TListDataCache<T_Data>;
        procedure ChangeCurrentCache(const aNewDataCache: TListDataCache<T_Data>);
        procedure SwitchToCache(const aListID: integer);
        property TOCListDataCache: TTOCListDataCache read fTOCListDataCache;
        property CurrentDataCache: TListDataCache<T_Data>read fCurrentDataCache;
    public
        constructor Create(const aPathNamePrefix: string; const aMaxItemsPerList: integer;
            const aFlushBufferCycle: integer);
        destructor Destroy(); override;
        procedure Add(const aData: T_Data);
        procedure Remove(const aData: T_Data);
        procedure DataChanged();
        procedure Init();
        procedure Clear();
        function ReadTOC(): boolean;
        function read(): boolean;
        procedure Flush();
        function CreateForwardStartAtLastIterator(): TMultiListDataCacheIterator<T_Data>;
        function CreateForwardIterator(): TMultiListDataCacheIterator<T_Data>;
        function CreateBackwardIterator(): TMultiListDataCacheIterator<T_Data>;
    end;

    TMultiListDataCacheIteratorMode = (imForward, imBackward, imForwardStartAtLast);

    TMultiListDataCacheIterator<T_Data: TStreamable> = class(TEnumerator<T_Data>)
    private
        fMode: TMultiListDataCacheIteratorMode;
        fCurrentListIndex: integer;
        fCursorPos: integer;
        fCursorInitialized: boolean;
        fMultiListDataCache: TMultiListDataCache<T_Data>;

        procedure MoveToCache(const aIndex: integer);
        function GetCurrentCount(): integer;
        function GetTOCCount(): integer;
        procedure DoInitIterator();
        procedure InitIterator();
        procedure Last();
        procedure First;
        procedure Next();
        procedure Previous();
        function GetIsEof: boolean;
        function GetIsBof: boolean;
    protected
        function DoGetCurrent(): T_Data; override;
        function DoMoveNext: Boolean; override;
    public
        constructor Create(const aMultiListDataCache: TMultiListDataCache<T_Data>;
            const aMode: TMultiListDataCacheIteratorMode);
        procedure MovePrevious();
    end;


implementation


uses
    SysUtils,
    FileUtilities;

{ TMultiListDataCache<T_Data> }

constructor TMultiListDataCache<T_Data>.Create(const aPathNamePrefix: string; const aMaxItemsPerList: integer;
    const aFlushBufferCycle: integer);
begin
    inherited Create();

    fPathNamePrefix := aPathNamePrefix;
    fMaxItemsPerList := aMaxItemsPerList;
    fCurrentDataCache := nil;
    fTOCListDataCache := TTOCListDataCache.Create(aPathNamePrefix + 'TOC');
    fDirtyDataCaches := TObjectList < TListDataCache < T_Data >>.Create(false);
    fFlushBufferCycle := aFlushBufferCycle
end;

destructor TMultiListDataCache<T_Data>.Destroy;
begin
    FreeAndNil(fDirtyDataCaches);
    FreeAndNil(fCurrentDataCache);
    FreeAndNil(fTOCListDataCache);
    inherited;
end;

function TMultiListDataCache<T_Data>.GetPathNameForListID(const aListID: integer): string;
begin
    result := fPathNamePrefix + IntToStr(aListID);
end;

function TMultiListDataCache<T_Data>.CreateCache(const aListID: integer): TListDataCache<T_Data>;
var
    xPathName: string;
begin
    xPathName := GetPathNameForListID(aListID);
    result := TListDataCache<T_Data>.Create(xPathName, IntToStr(aListID), fFlushBufferCycle);
end;

function TMultiListDataCache<T_Data>.CreateForwardIterator: TMultiListDataCacheIterator<T_Data>;
begin
    result := TMultiListDataCacheIterator<T_Data>.Create(self, imForward);

end;

function TMultiListDataCache<T_Data>.CreateForwardStartAtLastIterator: TMultiListDataCacheIterator<T_Data>;
begin
    result := TMultiListDataCacheIterator<T_Data>.Create(self, imForwardStartAtLast);

end;

function TMultiListDataCache<T_Data>.CreateBackwardIterator: TMultiListDataCacheIterator<T_Data>;
begin
    result := TMultiListDataCacheIterator<T_Data>.Create(self, imBackward);
end;

procedure TMultiListDataCache<T_Data>.ChangeCurrentCache(const aNewDataCache: TListDataCache<T_Data>);
begin
    if Assigned(fCurrentDataCache) then
    begin
        fDirtyDataCaches.Add(fCurrentDataCache);
    end;

    fCurrentDataCache := aNewDataCache;
end;

procedure TMultiListDataCache<T_Data>.StartNewSubList();
var
    xPathName: string;
    xListID: integer;
    xNewDataCache: TListDataCache<T_Data>;
begin
    xListID := fTOCListDataCache.Count + 1;
    xPathName := GetPathNameForListID(xListID);
    xNewDataCache := CreateCache(xListID);
    xNewDataCache.Init();
    ChangeCurrentCache(xNewDataCache);

    fTOCListDataCache.AddTOC(TTOCData.Create(xPathName));
end;

procedure TMultiListDataCache<T_Data>.Init();
begin
    fTOCListDataCache.Init();
end;

procedure TMultiListDataCache<T_Data>.Clear();
begin
    if Assigned(fCurrentDataCache) then
        fCurrentDataCache.Clear();
end;

procedure TMultiListDataCache<T_Data>.Add(const aData: T_Data);
begin
    if fCurrentDataCache = nil then
        StartNewSubList();

    if fCurrentDataCache.ListData.Count = fMaxItemsPerList then
        StartNewSublist();

    fCurrentDataCache.Add(aData);
end;

procedure TMultiListDataCache<T_Data>.Remove(const aData: T_Data);
var
    xTOCData: TTOCData;
    xPathName: string;
    xCurrentListID: integer;
begin
    if not Assigned(fCurrentDataCache) then
        EXIT;

    fCurrentDataCache.Remove(aData);
    if fCurrentDataCache.Count = 0 then
    begin
        xCurrentListID := StrToInt(fCurrentDataCache.ListID);
        xPathName := GetPathNameForListID(xCurrentListID);
        xTOCData := fTOCListDataCache.FindTOCByPathName(xPathName);
        fTOCListDataCache.RemoveTOC(xTOCData);
        self.SwitchToCache(xCurrentListID - 1);
    end;
end;

procedure TMultiListDataCache<T_Data>.Flush;
var
    x: integer;
    xDataCache: TListDataCache<T_Data>;
begin
    for x := 0 to fDirtyDataCaches.Count - 1 do
    begin
        xDataCache := fDirtyDataCaches[x];
        xDataCache.Flush();
    end;
    fDirtyDataCaches.Clear();

    if Assigned(fCurrentDataCache) then
        fCurrentDataCache.Flush();
end;

procedure TMultiListDataCache<T_Data>.SwitchToCache(const aListID: integer);
var
    xDataCache: TListDataCache<T_Data>;
begin
    if Assigned(fCurrentDataCache) and SameText(fCurrentDataCache.ListID, IntToStr(aListID)) then
        EXIT;
    xDataCache := nil;
    if aListID > 0 then
    begin
        xDataCache := CreateCache(aListID);
        xDataCache.Read;
        ASSERT(Assigned(xDataCache));
    end;
    ChangeCurrentCache(xDataCache);
end;
// procedure TMultiListDataCache<T_Data>.SwitchToCacheForData( const aData : T_Data );
//
// begin
// if ( not Assigned( fCurrentDataCache ) ) or ( not SameText( fCurrentDataCache.ListID, aData.ParentID ) ) then begin
// SwitchToCache( StrToInt( aData.ParentID ) );
// end;
// end;

procedure TMultiListDataCache<T_Data>.DataChanged();
begin
    // SwitchToCacheForData( aData );
    ASSERT(Assigned(fCurrentDataCache));
    fCurrentDataCache.DataChanged();
end;

function TMultiListDataCache<T_Data>.ReadTOC(): boolean;
begin
    result := fTOCListDataCache.Read();
end;

function TMultiListDataCache<T_Data>.Read(): boolean;
var
    xFinalListID: integer;
begin
    result := ReadTOC;
    if (not result) or (fTOCListDataCache.Count = 0) then
        EXIT;

    // just read the final list
    FreeAndNil(fCurrentDataCache);
    xFinalListID := fTOCListDataCache.Count;
    ChangeCurrentCache(CreateCache(xFinalListID));
    result := fCurrentDataCache.Read;
end;

{ TMultiListDataCacheIterator<T_Data> }

constructor TMultiListDataCacheIterator<T_Data>.Create(const aMultiListDataCache: TMultiListDataCache<T_Data>;
    const aMode: TMultiListDataCacheIteratorMode);
begin
    inherited Create();
    fMode := aMode;

    fMultiListDataCache := aMultiListDataCache;
    fCursorInitialized := false;
    fCurrentListIndex := -1;
    fCursorPos := -1;
end;

function TMultiListDataCacheIterator<T_Data>.GetCurrentCount(): integer;
begin
    result := 0;
    if not Assigned(fMultiListDataCache.CurrentDataCache) then
        EXIT;

    result := fMultiListDataCache.CurrentDataCache.Count;
end;

function TMultiListDataCacheIterator<T_Data>.GetTOCCount(): integer;
begin
    result := fMultiListDataCache.TOCListDataCache.Count;
end;

function TMultiListDataCacheIterator<T_Data>.DoGetCurrent: T_Data;
begin
    result := default (T_Data);
    if (not Assigned(fMultiListDataCache.CurrentDataCache)) or (fCursorPos < 0) or
        (fCursorPos >= GetCurrentCount) then
        EXIT;
    result := T_Data(fMultiListDataCache.CurrentDataCache.ListData[fCursorPos]);
end;

procedure TMultiListDataCacheIterator<T_Data>.Previous;
var
    xPossibleCursorPos: integer;
begin
    if GetIsBOF then
        EXIT;

    xPossibleCursorPos := fCursorPos - 1;

    if xPossibleCursorPos >= 0 then
    begin
        fCursorPos := xPossibleCursorPos;
        EXIT;
    end;

    MoveToCache(fCurrentListIndex - 1);

    fCursorPos := GetCurrentCount - 1;

end;

procedure TMultiListDataCacheIterator<T_Data>.Last;
var
    xLastIndex: integer;
begin

    xLastIndex := GetTOCCount - 1;
    if xLastIndex < 0 then
        EXIT;
    MoveToCache(xLastIndex);
    fCursorPos := GetCurrentCount - 1;
end;

procedure TMultiListDataCacheIterator<T_Data>.DoInitIterator;
begin

    if (fMode = imBackward) or (fMode = imForwardStartAtLast) then
        self.Last
    else if fMode = imForward then
        self.First;

    fCursorInitialized := true;

end;

procedure TMultiListDataCacheIterator<T_Data>.InitIterator();
begin
    DoInitIterator();
end;

procedure TMultiListDataCacheIterator<T_Data>.MovePrevious;
begin
    if fMode = imBackward then
        self.Next
    else
        self.Previous;
end;

procedure TMultiListDataCacheIterator<T_Data>.MoveToCache(const aIndex: integer);
begin
    fCurrentListIndex := aIndex;
    fMultiListDataCache.SwitchToCache(fCurrentListIndex + 1);
    fCursorPos := -1;
end;

function TMultiListDataCacheIterator<T_Data>.GetIsEof(): boolean;
begin
    result := (fCurrentListIndex >= GetTOCCount - 1) and (fCursorPos >= GetCurrentCount - 1);
end;

function TMultiListDataCacheIterator<T_Data>.GetIsBof(): boolean;
begin
    result := (fCurrentListIndex = 0) and (fCursorPos <= 0);
end;

procedure TMultiListDataCacheIterator<T_Data>.Next;
var
    xPossibleCursorPos: integer;
begin
    if GetIsEof then
        EXIT;

    xPossibleCursorPos := fCursorPos + 1;

    if xPossibleCursorPos <= GetCurrentCount - 1 then
    begin
        fCursorPos := xPossibleCursorPos;
        EXIT;
    end;

    MoveToCache(fCurrentListIndex + 1);
    fCursorPos := 0;

end;

procedure TMultiListDataCacheIterator<T_Data>.First;
begin
    if GetTOCCount = 0 then
        EXIT;
    MoveToCache(0);

    if GetCurrentCount > 0 then
        fCursorPos := 0;
end;

function TMultiListDataCacheIterator<T_Data>.DoMoveNext: Boolean;
var
    xMoveNeeded: boolean;
begin
    if not fCursorInitialized then
    begin
        InitIterator();
        xMoveNeeded := false;
        fCursorInitialized := true;
        result := GetCurrentCount > 0;
        EXIT;
    end;

    if fMode = imBackward then
        result := not self.GetIsBof
    else
        result := not self.GetIsEof;

    if not result then
        EXIT;

    if fMode = imBackward then
        self.Previous
    else
        self.Next;
end;


end.
