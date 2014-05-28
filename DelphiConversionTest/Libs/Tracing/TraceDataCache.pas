{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  15.04.10 pk                                        TN5050    Initial Revision
  07.06.10 pk  TraceDataCache                        TN5077    New Init, Clear, Flush, SetPathName
  07.06.10 pk  TTraceDataLoader                      TN5077    New
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  10.04.13 wl                                        TN6045   uses Generics.Collections
  ----------------------------------------------------------------------------------------------------------------------- }

unit TraceDataCache;


interface


uses
    Generics.Collections;

type
    TTraceDataCache = class
    protected
        // function GetCount : integer; virtual; abstract;
        function GetTypeID: string; virtual; abstract;
    public
        procedure SetPathName(const aValue: string); virtual; abstract;
        procedure Init(); virtual; abstract;
        procedure Clear(); virtual; abstract;
        function read(): boolean; virtual; abstract;
        procedure Flush(); virtual; abstract;
        // property Count : integer read GetCount;
        property TypeID: string read GetTypeID;
    end;

    TTraceDataCacheList = class(TObjectList<TTraceDataCache>)
    public
        procedure Clear();
        procedure Flush();
        function Find(const aTypeID: string): TTraceDataCache;
    end;

    TTraceDataCacheCreatorTypeInfo = class
    public
        function CreateDataCache(): TTraceDataCache; virtual; abstract;
    end;

    TTraceDataCacheCreatorTypeInfoList = class(TObjectList<TTraceDataCacheCreatorTypeInfo>)

    end;

    TTraceDataLoader = class
    protected
        fDataCache: TTraceDataCache;
        procedure DoLoadDataToRun(); virtual; abstract;
        procedure DoStoreDataFromRun(); virtual; abstract;
    public
        constructor Create(const aDataCache: TTraceDataCache);

        procedure LoadDataToRun();
        procedure StoreDataFromRun();
    end;

    TTraceDataLoaderList = class(TObjectList<TTraceDataLoader>);

    TTraceDataLoaderCreatorTypeInfo = class
    protected
        function DoSupportsDataCache(const aDataCache: TTraceDataCache): boolean; virtual; abstract;
        function DoCreateDataLoader(const aDataCache: TTraceDataCache): TTraceDataLoader; virtual; abstract;
    public
        function SupportsDataCache(const aDataCache: TTraceDataCache): boolean;
        function CreateDataLoader(const aDataCache: TTraceDataCache): TTraceDataLoader;
    end;

    TTraceDataLoaderCreatorTypeInfoList = class(TObjectList<TTraceDataLoaderCreatorTypeInfo>)
    public
        function FindBySupportsDataCache(const aDataCache: TTraceDataCache): TTraceDataLoaderCreatorTypeInfo;
    end;


implementation


uses
    SysUtils;

{ TTraceDataCacheList }

procedure TTraceDataCacheList.Clear;
var
    xTraceDataCache: TTraceDataCache;
begin
    for xTraceDataCache in self do
    begin
        xTraceDataCache.Clear();
    end;

end;

function TTraceDataCacheList.Find(const aTypeID: string): TTraceDataCache;
var
    xTraceDataCache: TTraceDataCache;
begin
    result := nil;
    for xTraceDataCache in self do
    begin
        if not SameText(xTraceDataCache.TypeID, aTypeID) then
            CONTINUE;
        result := xTraceDataCache;
        EXIT;
    end;
end;

procedure TTraceDataCacheList.Flush;
var
    xTraceDataCache: TTraceDataCache;
begin
    for xTraceDataCache in self do
    begin
        xTraceDataCache.Flush();
    end;

end;

{ TTraceDataLoader }

constructor TTraceDataLoader.Create(const aDataCache: TTraceDataCache);
begin
    inherited Create();
    fDataCache := aDataCache;
end;

procedure TTraceDataLoader.LoadDataToRun;
begin
    DoLoadDataToRun();
end;

procedure TTraceDataLoader.StoreDataFromRun;
begin
    DoStoreDataFromRun();
end;

{ TTraceDataLoaderCreatorTypeInfo }

function TTraceDataLoaderCreatorTypeInfo.CreateDataLoader(const aDataCache: TTraceDataCache)
    : TTraceDataLoader;
begin
    result := DoCreateDataLoader(aDataCache);
end;

function TTraceDataLoaderCreatorTypeInfo.SupportsDataCache(const aDataCache: TTraceDataCache): boolean;
begin
    result := DoSupportsDataCache(aDataCache);
end;

{ TTraceDataLoaderCreatorTypeInfoList }

function TTraceDataLoaderCreatorTypeInfoList.FindBySupportsDataCache(const aDataCache: TTraceDataCache)
    : TTraceDataLoaderCreatorTypeInfo;
var
    xTypeInfo: TTraceDataLoaderCreatorTypeInfo;
begin
    result := nil;
    for xTypeInfo in self do
    begin
        if not xTypeInfo.SupportsDataCache(aDataCache) then
            CONTINUE;
        result := xTypeInfo;
        EXIT;
    end;

end;


end.
