unit LayoutCarrierTrace;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  07.06.10 pk                                        TN5077     initial revision
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    Streamable,
    ListDataCache,
    TraceDataCache,
    RunEffect,
    RunEffectData,
    RestartPreparationStep,
    ActionData,
    TraceManager,
    TraceThreadDetails,
    TraceProcessDetails,
    LayoutDataCache;

const
    cLayoutCarrierTypeID = 'LayoutCarrier';

type

    TLayoutCarrierListTraceDataCache = class(TListDataCache<TLayoutCarrierDataCacheItem>)
    protected
        function GetTypeID: string; override;
        function FindCarrierByCarrierName(const aCarrierName: string): TLayoutCarrierDataCacheItem;
        // public
        // procedure CarrierMoved( const aCarrierName : string; const aCarrierName : string; const aCarrierSlot : integer; const aRotation : integer );
    end;

    TLayoutCarrierDataCacheCreatorTypeInfo = class(TTraceDataCacheCreatorTypeInfo)
    public
        function CreateDataCache(): TTraceDataCache; override;
    end;

    TLayoutCarrierListDataLoader = class(TTraceDataLoader)
    protected
        procedure DoLoadDataToRun(); override;
        procedure DoStoreDataFromRun(); override;
    end;

    TLayoutCarrierDataLoaderCreatorTypeInfo = class(TTraceDataLoaderCreatorTypeInfo)
    protected
        function DoSupportsDataCache(const aDataCache: TTraceDataCache): boolean; override;
        function DoCreateDataLoader(const aDataCache: TTraceDataCache): TTraceDataLoader; override;
    end;

    TCarrierTypeChangedRunEffectData = class(TRunEffectData)
    private
        fCarrierName: string;
        fOldTypeName: string;
    public
        constructor Create(const aCarrierName: string; const aOldTypeName: string); reintroduce;
    published
        property CarrierName: string read fCarrierName write fCarrierName;
        property OldTypeName: string read fOldTypeName write fOldTypeName;
    end;

    TCarrierTypeChangedRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TCarrierTypeChangedRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TCarrierTypeChangedRunEffectData read GetRunEffectData;
    end;

    TCarrierTypeChangedRunEffectCreatorTypeInfo = class(TRunEffectCreatorTypeInfo)
    protected
        function DoCreateRunEffect(const aRunEffectData: TRunEffectData): TRunEffect; override;
        function DoSupportsData(const aRunEffectData: TRunEffectData): boolean; override;
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    LayoutManager,
    TypeMapTranslator;

{ TLayoutCarrierListTraceDataCache }

function TLayoutCarrierListTraceDataCache.FindCarrierByCarrierName(const aCarrierName: string)
    : TLayoutCarrierDataCacheItem;
var
    x: integer;
    xData: TLayoutCarrierDataCacheItem;
begin
    result := nil;
    for x := 0 to self.ListData.Count - 1 do
    begin
        xData := self.ListData[x] as TLayoutCarrierDataCacheItem;
        if not SameText(xData.CarrierName, aCarrierName) then
            CONTINUE;
        result := xData;
        EXIT;
    end;
end;

function TLayoutCarrierListTraceDataCache.GetTypeID: string;
begin
    result := cLayoutCarrierTypeID;
end;

{ TLayoutCarrierDataCacheCreatorTypeInfo }

function TLayoutCarrierDataCacheCreatorTypeInfo.CreateDataCache: TTraceDataCache;
begin
    result := TLayoutCarrierListTraceDataCache.Create();
end;
{ TLayoutCarrierListDataLoader }

procedure TLayoutCarrierListDataLoader.DoLoadDataToRun;
var
    x: integer;
    xTraceDataCache: TLayoutCarrierListTraceDataCache;
    xDataCacheItem, xDataCacheItemCopy: TLayoutCarrierDataCacheItem;
begin
    xTraceDataCache := fDataCache as TLayoutCarrierListTraceDataCache;
    TLayoutManager.Instance.CurrentLayout.DataCache.LayoutCarrierDataCache.Clear();
    for x := 0 to xTraceDataCache.Count - 1 do
    begin
        xDataCacheItem := xTraceDataCache.ListData[x] as TLayoutCarrierDataCacheItem;
        xDataCacheItemCopy := TObjectCopy<TLayoutCarrierDataCacheItem>.Copy(xDataCacheItem);
        TLayoutManager.Instance.CurrentLayout.DataCache.LayoutCarrierDataCache.Add(xDataCacheItemCopy)
    end;
end;

procedure TLayoutCarrierListDataLoader.DoStoreDataFromRun;
var
    xTraceDataCache: TLayoutCarrierListTraceDataCache;
    xDataCacheItem, xDataCacheItemCopy: TLayoutCarrierDataCacheItem;
begin
    xTraceDataCache := fDataCache as TLayoutCarrierListTraceDataCache;
    xTraceDataCache.ListData.List.Clear();

    TLayoutManager.Instance.CurrentLayout.StoreCarriers();

    for xDataCacheItem in TLayoutManager.Instance.CurrentLayout.DataCache.LayoutCarrierDataCache do
    begin
        xDataCacheItemCopy := TObjectCopy<TLayoutCarrierDataCacheItem>.Copy(xDataCacheItem);
        xTraceDataCache.Add(xDataCacheItemCopy);
    end;

    xTraceDataCache.DataChanged();

end;
{ TLayoutCarrierDataLoaderCreatorTypeInfo }

function TLayoutCarrierDataLoaderCreatorTypeInfo.DoCreateDataLoader(const aDataCache: TTraceDataCache)
    : TTraceDataLoader;
begin
    result := TLayoutCarrierListDataLoader.Create(aDataCache);
end;

function TLayoutCarrierDataLoaderCreatorTypeInfo.DoSupportsDataCache(const aDataCache
    : TTraceDataCache): boolean;
begin
    result := aDataCache is TLayoutCarrierListTraceDataCache;
end;
{ TCarrierTypeChangedRunEffectData }

constructor TCarrierTypeChangedRunEffectData.Create(const aCarrierName, aOldTypeName: string);
begin
    inherited Create();
    fCarrierName := aCarrierName;
    fOldTypeName := aOldTypeName;
end;

{ TCarrierTypeChangedRunEffectCreatorTypeInfo }

function TCarrierTypeChangedRunEffectCreatorTypeInfo.DoCreateRunEffect(const aRunEffectData: TRunEffectData)
    : TRunEffect;
begin
    result := TCarrierTypeChangedRunEffect.Create(aRunEffectData);
end;

function TCarrierTypeChangedRunEffectCreatorTypeInfo.DoSupportsData(const aRunEffectData
    : TRunEffectData): boolean;
begin
    result := aRunEffectData is TCarrierTypeChangedRunEffectData;
end;

{ TCarrierTypeChangedRunEffect }

procedure TCarrierTypeChangedRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xDataCache: TLayoutCarrierListTraceDataCache;
    xCarrierItem: TLayoutCarrierDataCacheItem;
begin

    xDataCache := aProcessDetails.FindDataCache(cLayoutCarrierTypeID) as TLayoutCarrierListTraceDataCache;

    xCarrierItem := xDataCache.FindCarrierByCarrierName(self.RunEffectData.CarrierName);
    ASSERT(Assigned(xCarrierItem));
    xCarrierItem.CarrierType := self.RunEffectData.OldTypeName;

    xDataCache.DataChanged;
end;

function TCarrierTypeChangedRunEffect.GetRunEffectData: TCarrierTypeChangedRunEffectData;
begin
    result := fRunEffectData as TCarrierTypeChangedRunEffectData;
end;


end.
