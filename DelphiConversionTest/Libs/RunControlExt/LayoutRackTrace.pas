unit LayoutRackTrace;


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
    cLayoutRackTypeID = 'LayoutRack';

type

    TLayoutRackListTraceDataCache = class(TListDataCache<TLayoutRackDataCacheItem>)
    protected
        function GetTypeID: string; override;
        function FindRackByRackName(const aRackName: string): TLayoutRackDataCacheItem;
    public
        procedure RackMoved(const aRackName: string; const aCarrierName: string; const aCarrierSlot: integer;
            const aRotation: integer);
    end;

    TLayoutRackDataCacheCreatorTypeInfo = class(TTraceDataCacheCreatorTypeInfo)
    public
        function CreateDataCache(): TTraceDataCache; override;
    end;

    TLayoutRackListDataLoader = class(TTraceDataLoader)
    protected
        procedure DoLoadDataToRun(); override;
        procedure DoStoreDataFromRun(); override;
    end;

    TLayoutRackDataLoaderCreatorTypeInfo = class(TTraceDataLoaderCreatorTypeInfo)
    protected
        function DoSupportsDataCache(const aDataCache: TTraceDataCache): boolean; override;
        function DoCreateDataLoader(const aDataCache: TTraceDataCache): TTraceDataLoader; override;
    end;

    TRackMoveRunEffectData = class(TRunEffectData)
    private
        fRackName: string;
        fOldCarrierName: string;
        fOldCarrierSlot: integer;
        fOldRotation: integer;
    public
        constructor Create(const aRackName: string; const aOldCarrierName: string;
            const aOldCarrierSlot: integer; const aOldRotation: integer); reintroduce;
    published
        property RackName: string read fRackName write fRackName;
        property OldCarrierName: string read fOldCarrierName write fOldCarrierName;
        property OldCarrierSlot: integer read fOldCarrierSlot write fOldCarrierSlot;
        property OldRotation: integer read fOldRotation write fOldRotation;
    end;

    TRackMoveRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TRackMoveRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TRackMoveRunEffectData read GetRunEffectData;
    end;

    TRackMoveRunEffectCreatorTypeInfo = class(TRunEffectCreatorTypeInfo)
    protected
        function DoCreateRunEffect(const aRunEffectData: TRunEffectData): TRunEffect; override;
        function DoSupportsData(const aRunEffectData: TRunEffectData): boolean; override;
    end;

    TRackMoveRestartPreparationStep = class(TRestartPreparationStep)
    private
        function GetRunEffectData: TRackMoveRunEffectData;
    protected
        procedure DoVisualize(); override;
    public
        property RunEffectData: TRackMoveRunEffectData read GetRunEffectData;
    end;

    TRackMoveRestartPreparationStepCreatorTypeInfo = class(TRestartPreparationStepCreatorTypeInfo)
    protected
        function DoCreateRestartPreparationStep(const aRunEffectData: TRunEffectData)
            : TRestartPreparationStep; override;
        function DoSupportsData(const aRunEffectData: TRunEffectData): boolean; override;
    end;

    TRackChangeIDRunEffectData = class(TRunEffectData)
    private
        fRackName: string;
        fOldRackID: string;
    public
        constructor Create(const aRackName: string; const aOldRackID: string); reintroduce;
    published
        property RackName: string read fRackName write fRackName;
        property OldRackID: string read fOldRackID write fOldRackID;
    end;

    TRackChangeIDRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TRackChangeIDRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TRackChangeIDRunEffectData read GetRunEffectData;
    end;

    TRackChangeIDRunEffectCreatorTypeInfo = class(TRunEffectCreatorTypeInfo)
    protected
        function DoCreateRunEffect(const aRunEffectData: TRunEffectData): TRunEffect; override;
        function DoSupportsData(const aRunEffectData: TRunEffectData): boolean; override;
    end;

    TRackTypeChangedRunEffectData = class(TRunEffectData)
    private
        fRackName: string;
        fOldTypeName: string;
    public
        constructor Create(const aRackName: string; const aOldTypeName: string); reintroduce;
    published
        property RackName: string read fRackName write fRackName;
        property OldTypeName: string read fOldTypeName write fOldTypeName;
    end;

    TRackTypeChangedRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TRackTypeChangedRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TRackTypeChangedRunEffectData read GetRunEffectData;
    end;

    TRackTypeChangedRunEffectCreatorTypeInfo = class(TRunEffectCreatorTypeInfo)
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

{ TLayoutRackListTraceDataCache }

function TLayoutRackListTraceDataCache.FindRackByRackName(const aRackName: string): TLayoutRackDataCacheItem;
var
    x: integer;
    xData: TLayoutRackDataCacheItem;
begin
    result := nil;
    for x := 0 to self.ListData.Count - 1 do
    begin
        xData := self.ListData[x] as TLayoutRackDataCacheItem;
        if not SameText(xData.RackName, aRackName) then
            CONTINUE;
        result := xData;
        EXIT;
    end;
end;

function TLayoutRackListTraceDataCache.GetTypeID: string;
begin
    result := cLayoutRackTypeID;
end;

procedure TLayoutRackListTraceDataCache.RackMoved(const aRackName, aCarrierName: string;
    const aCarrierSlot, aRotation: integer);
var
    xData: TLayoutRackDataCacheItem;
begin
    xData := self.FindRackByRackName(aRackName);
    ASSERT(Assigned(xData), 'LayoutRackData not found');
    xData.CarrierName := aCarrierName;
    xData.Slot := aCarrierSlot;
    xData.Rotation := aRotation;
    self.DataChanged();
end;

{ TLayoutRackDataCacheCreatorTypeInfo }

function TLayoutRackDataCacheCreatorTypeInfo.CreateDataCache: TTraceDataCache;
begin
    result := TLayoutRackListTraceDataCache.Create();
end;
{ TLayoutRackListDataLoader }

procedure TLayoutRackListDataLoader.DoLoadDataToRun;
var
    x: integer;
    xTraceDataCache: TLayoutRackListTraceDataCache;
    xDataCacheItem, xDataCacheItemCopy: TLayoutRackDataCacheItem;
begin
    xTraceDataCache := fDataCache as TLayoutRackListTraceDataCache;
    TLayoutManager.Instance.CurrentLayout.DataCache.LayoutRackDataCache.Clear();
    for x := 0 to xTraceDataCache.Count - 1 do
    begin
        xDataCacheItem := xTraceDataCache.ListData[x] as TLayoutRackDataCacheItem;
        xDataCacheItemCopy := TObjectCopy<TLayoutRackDataCacheItem>.Copy(xDataCacheItem);
        TLayoutManager.Instance.CurrentLayout.DataCache.LayoutRackDataCache.Add(xDataCacheItemCopy)
    end;
end;

procedure TLayoutRackListDataLoader.DoStoreDataFromRun;
var
    xTraceDataCache: TLayoutRackListTraceDataCache;
    xDataCacheItem, xDataCacheItemCopy: TLayoutRackDataCacheItem;
begin
    xTraceDataCache := fDataCache as TLayoutRackListTraceDataCache;
    xTraceDataCache.ListData.List.Clear();

    TLayoutManager.Instance.CurrentLayout.StoreRacks();
    for xDataCacheItem in TLayoutManager.Instance.CurrentLayout.DataCache.LayoutRackDataCache do
    begin
        xDataCacheItemCopy := TObjectCopy<TLayoutRackDataCacheItem>.Copy(xDataCacheItem);
        xTraceDataCache.Add(xDataCacheItemCopy);
    end;

    xTraceDataCache.DataChanged();

end;
{ TLayoutRackDataLoaderCreatorTypeInfo }

function TLayoutRackDataLoaderCreatorTypeInfo.DoCreateDataLoader(const aDataCache: TTraceDataCache)
    : TTraceDataLoader;
begin
    result := TLayoutRackListDataLoader.Create(aDataCache);
end;

function TLayoutRackDataLoaderCreatorTypeInfo.DoSupportsDataCache(const aDataCache: TTraceDataCache): boolean;
begin
    result := aDataCache is TLayoutRackListTraceDataCache;
end;

{ TRackMoveRunEffectData }

constructor TRackMoveRunEffectData.Create(const aRackName, aOldCarrierName: string;
    const aOldCarrierSlot, aOldRotation: integer);
begin
    inherited Create();
    fRackName := aRackName;
    fOldCarrierName := aOldCarrierName;
    fOldCarrierSlot := aOldCarrierSlot;
    fOldRotation := aOldRotation;
end;

{ TRackMoveRunEffect }

function TRackMoveRunEffect.GetRunEffectData: TRackMoveRunEffectData;
begin
    result := fRunEffectData as TRackMoveRunEffectData;
end;

procedure TRackMoveRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xDataCache: TLayoutRackListTraceDataCache;
begin

    xDataCache := aProcessDetails.FindDataCache(cLayoutRackTypeID) as TLayoutRackListTraceDataCache;

    xDataCache.RackMoved(self.RunEffectData.RackName, self.RunEffectData.OldCarrierName,
        self.RunEffectData.OldCarrierSlot, self.RunEffectData.OldRotation);

end;

{ TRackMoveRunEffectCreatorTypeInfo }

function TRackMoveRunEffectCreatorTypeInfo.DoCreateRunEffect(const aRunEffectData: TRunEffectData)
    : TRunEffect;
begin
    result := TRackMoveRunEffect.Create(aRunEffectData);
end;

function TRackMoveRunEffectCreatorTypeInfo.DoSupportsData(const aRunEffectData: TRunEffectData): boolean;
begin
    result := aRunEffectData is TRackMoveRunEffectData;
end;

{ TRackMoveRestartPreparationStep }

procedure TRackMoveRestartPreparationStep.DoVisualize;
var
    xText: string;
begin
    xText := TTypeSafeFormat.Format('Put rack {0} on Slot {1} of Carrier {2}',
        [self.RunEffectData.RackName, self.RunEffectData.OldCarrierSlot, self.RunEffectData.OldCarrierName]);

    if self.RunEffectData.OldRotation <> 0 then
    begin
        xText := TTypeSafeFormat.Format('{0}, with a Rotation of {1}',
            [xText, self.RunEffectData.OldRotation]);
    end;

    fVisualManager.AddText(xText);

end;

function TRackMoveRestartPreparationStep.GetRunEffectData: TRackMoveRunEffectData;
begin
    result := fRunEffectData as TRackMoveRunEffectData;
end;

{ TRackMoveRestartPreparationStepCreatorTypeInfo }

function TRackMoveRestartPreparationStepCreatorTypeInfo.DoCreateRestartPreparationStep
    (const aRunEffectData: TRunEffectData): TRestartPreparationStep;
begin
    result := TRackMoveRestartPreparationStep.Create(aRunEffectData);
end;

function TRackMoveRestartPreparationStepCreatorTypeInfo.DoSupportsData(const aRunEffectData
    : TRunEffectData): boolean;
begin
    result := aRunEffectData is TRackMoveRunEffectData;
end;

{ TRackChangeIDRunEffectData }

constructor TRackChangeIDRunEffectData.Create(const aRackName, aOldRackID: string);
begin
    inherited Create();
    fRackName := aRackName;
    fOldRackID := aOldRackID;
end;

{ TRackChangeIDRunEffectCreatorTypeInfo }

function TRackChangeIDRunEffectCreatorTypeInfo.DoCreateRunEffect(const aRunEffectData: TRunEffectData)
    : TRunEffect;
begin
    result := TRackChangeIDRunEffect.Create(aRunEffectData);
end;

function TRackChangeIDRunEffectCreatorTypeInfo.DoSupportsData(const aRunEffectData: TRunEffectData): boolean;
begin
    result := aRunEffectData is TRackChangeIDRunEffectData;
end;

{ TRackChangeIDRunEffect }

procedure TRackChangeIDRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xDataCache: TLayoutRackListTraceDataCache;
    xRackItem: TLayoutRackDataCacheItem;
begin

    xDataCache := aProcessDetails.FindDataCache(cLayoutRackTypeID) as TLayoutRackListTraceDataCache;

    xRackItem := xDataCache.FindRackByRackName(self.RunEffectData.RackName);
    ASSERT(Assigned(xRackItem));
    xRackItem.RackID := self.RunEffectData.OldRackID;

    xDataCache.DataChanged;
end;

function TRackChangeIDRunEffect.GetRunEffectData: TRackChangeIDRunEffectData;
begin
    result := fRunEffectData as TRackChangeIDRunEffectData;
end;

{ TRackTypeChangedRunEffectData }

constructor TRackTypeChangedRunEffectData.Create(const aRackName, aOldTypeName: string);
begin
    inherited Create();
    fRackName := aRackName;
    fOldTypeName := aOldTypeName;
end;

{ TRackTypeChangedRunEffectCreatorTypeInfo }

function TRackTypeChangedRunEffectCreatorTypeInfo.DoCreateRunEffect(const aRunEffectData: TRunEffectData)
    : TRunEffect;
begin
    result := TRackTypeChangedRunEffect.Create(aRunEffectData);
end;

function TRackTypeChangedRunEffectCreatorTypeInfo.DoSupportsData(const aRunEffectData
    : TRunEffectData): boolean;
begin
    result := aRunEffectData is TRackTypeChangedRunEffectData;
end;

{ TRackTypeChangedRunEffect }

procedure TRackTypeChangedRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xDataCache: TLayoutRackListTraceDataCache;
    xRackItem: TLayoutRackDataCacheItem;
begin

    xDataCache := aProcessDetails.FindDataCache(cLayoutRackTypeID) as TLayoutRackListTraceDataCache;

    xRackItem := xDataCache.FindRackByRackName(self.RunEffectData.RackName);
    ASSERT(Assigned(xRackItem));
    xRackItem.RackType := self.RunEffectData.OldTypeName;
    xDataCache.DataChanged;
end;

function TRackTypeChangedRunEffect.GetRunEffectData: TRackTypeChangedRunEffectData;
begin
    result := fRunEffectData as TRackTypeChangedRunEffectData;
end;


end.
