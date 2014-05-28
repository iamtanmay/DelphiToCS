{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Classes and math functions for Coordinate System Relation
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.06.08 pk                                TN4139  Initial Revision
  27.06.08 pk                                TN4139  uneeded functions removed
  09.07.08 pk  ClearCache                    TN4139  now public
  16.07.08 pk                                TN4139  changes needed for reading racks from runlayout
  21.07.08 pk                                TN4179  changes for reading carriers from runlayout
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  07.06.10 pk                                TN5077  changes for restarting rack movement
  21.10.10 pk  FindCacheItemByName           TN5307  Now requires LayoutID parameter
  13.03.12 wl  TLayoutDataCache.ReadByRun    TN5798   Carrier normal laden
  10.04.13 wl                                TN6045   uses geändert
  18.09.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit LayoutDataCache;


interface


uses
    Generics.Collections,
    Streamable,
    LayoutDataAdaptor,
    LayoutWorkspaceDataAdaptor,
    TipsetDataAdaptor;

type
    TLayoutElementDataCacheItem = class(TStreamable)
    protected
        function GetLayoutName: string; virtual; abstract;
    public
        property LayoutName: string read GetLayoutName;
    end;

    TLayoutRackDataCacheItem = class(TLayoutElementDataCacheItem)
    private
        fLayoutID: string;
        fRackName: string;
        fRackType: string;
        fRackID: string;
        fCarrierName: string;
        fSlot: integer;
        fRotation: double;
    protected
        function GetLayoutName: string; override;
    public
        procedure FromRec(const aRec: TLayoutRackRec);
        function ToRec(): TLayoutRackRec;
    published
        property LayoutID: string read fLayoutID write fLayoutID;
        property RackName: string read fRackName write fRackName;
        property RackType: string read fRackType write fRackType;
        property RackID: string read fRackID write fRackID;
        property CarrierName: string read fCarrierName write fCarrierName;
        property Slot: integer read fSlot write fSlot;
        property Rotation: double read fRotation write fRotation;
    end;

    TLayoutCarrierDataCacheItem = class(TLayoutElementDataCacheItem)
    private
        fLayoutID: string;
        fCarrierName: string;
        fWorkspaceID: integer;
        fCarr_X, fCarr_Y, fCarr_Z: double;
        fCarrierType: string;
    protected
        function GetLayoutName: string; override;
    public
        procedure FromRec(const aRec: TLayoutCarrierRec);
        function ToRec(): TLayoutCarrierRec;
    published
        property LayoutID: string read fLayoutID write fLayoutID;
        property CarrierName: string read fCarrierName write fCarrierName;
        property WorkspaceID: integer read fWorkspaceID write fWorkspaceID;
        property Carr_X: double read fCarr_X write fCarr_X;
        property Carr_Y: double read fCarr_Y write fCarr_Y;
        property Carr_Z: double read fCarr_Z write fCarr_Z;
        property CarrierType: string read fCarrierType write fCarrierType;
    end;

    TLayoutWorkspaceDataCacheItem = class(TLayoutElementDataCacheItem)
    private
        fRec: TLayoutWorkspaceRec;
    protected
        function GetLayoutName: string; override;
    public
        procedure FromRec(const aRec: TLayoutWorkspaceRec);
        function ToRec(): TLayoutWorkspaceRec;
    end;

    TLayoutElementDataCache<T: TLayoutElementDataCacheItem> = class(TObjectList<T>)
    protected
        procedure ClearCache();
        procedure read(); virtual; abstract;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Refresh();
    end;

    TLayoutWorkspaceDataCache = class(TLayoutElementDataCache<TLayoutWorkspaceDataCacheItem>)
    protected
        procedure read(); override;
    public
        constructor Create();
        procedure AddCacheItem(const aRec: TLayoutWorkspaceRec);
    end;

    TLayoutCarrierDataCache = class(TLayoutElementDataCache<TLayoutCarrierDataCacheItem>)
    protected
        procedure read(); override;
    public
        constructor Create();
        procedure AddCacheItem(const aRec: TLayoutCarrierRec);
        function FindCacheItemByName(const aLayoutID: string; const aName: string)
            : TLayoutCarrierDataCacheItem;
    end;

    TLayoutRackDataCache = class(TLayoutElementDataCache<TLayoutRackDataCacheItem>)
    protected
        procedure read(); override;
        procedure ReadRun(const aRunName: string);
    public
        constructor Create();
        function FindCacheItemByName(const aLayoutID: string; const aName: string): TLayoutRackDataCacheItem;
        procedure AddCacheItem(const aRec: TLayoutRackRec);
    end;

    TLayoutLinkDataCacheItem = class(TLayoutElementDataCacheItem)
    private
        fRec: TLayoutLinkRec;
    protected
        function GetLayoutName: string; override;
    public
        procedure FromRec(const aRec: TLayoutLinkRec);
        function ToRec(): TLayoutLinkRec;
    end;

    TLayoutLinkDataCache = class(TLayoutElementDataCache<TLayoutLinkDataCacheItem>)
    protected
        procedure read(); override;
    public
        constructor Create();
        procedure AddCacheItem(const aRec: TLayoutLinkRec);
    end;

    TLayoutTipsetDataCacheItem = class(TLayoutElementDataCacheItem)
    private
        fLayoutID: string;
        fTipNumber: integer;
        fTipTypeName: string;
        fPipDeviceName: string;
    protected
        function GetLayoutName: string; override;
    public
        procedure FromRec(const aRec: TTipsetRec);
        function ToRec(): TTipsetRec;
    published
        property LayoutID: string read fLayoutID write fLayoutID;
        property TipNumber: integer read fTipNumber write fTipNumber;
        property TipTypeName: string read fTipTypeName write fTipTypeName;
        property PipDeviceName: string read fPipDeviceName write fPipDeviceName;
    end;

    TLayoutTipsetDataCache = class(TLayoutElementDataCache<TLayoutTipsetDataCacheItem>)
    protected
        procedure read(); override;
    public
        constructor Create();
        procedure AddCacheItem(const aRec: TTipsetRec);
    end;

    TLayoutDataCache = class
    private
        fLayoutLinkDataCache: TLayoutLinkDataCache;
        fLayoutWorkspaceDataCache: TLayoutWorkspaceDataCache;
        fLayoutCarrierDataCache: TLayoutCarrierDataCache;
        fLayoutRackDataCache: TLayoutRackDataCache;
        fLayoutTipsetDataCache: TLayoutTipsetDataCache;
    public
        constructor Create();
        destructor Destroy; override;
        procedure read();
        procedure ReadByRun(const aRunName: string);
        procedure ClearCache();
        property LayoutLinkDataCache: TLayoutLinkDataCache read fLayoutLinkDataCache;
        property LayoutWorkspaceDataCache: TLayoutWorkspaceDataCache read fLayoutWorkspaceDataCache;
        property LayoutCarrierDataCache: TLayoutCarrierDataCache read fLayoutCarrierDataCache;
        property LayoutRackDataCache: TLayoutRackDataCache read fLayoutRackDataCache;
        property LayoutTipsetDataCache: TLayoutTipsetDataCache read fLayoutTipsetDataCache;
    end;


implementation


uses
    SysUtils;

{ TLayoutElementDataCache<T> }

constructor TLayoutElementDataCache<T>.Create;
begin
    inherited Create(false);
end;

procedure TLayoutElementDataCache<T>.ClearCache;
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
        self.Items[x].Free;
    self.Clear();
end;

procedure TLayoutElementDataCache<T>.Refresh;
begin
    ClearCache();
    read();
end;

destructor TLayoutElementDataCache<T>.Destroy;
begin
    self.ClearCache();
    inherited;
end;

{ TLayoutWorkspaceDataCacheItem }

procedure TLayoutWorkspaceDataCacheItem.FromRec(const aRec: TLayoutWorkspaceRec);
begin
    fRec := aRec;
end;

function TLayoutWorkspaceDataCacheItem.ToRec(): TLayoutWorkspaceRec;
begin
    result := fRec;
end;

function TLayoutWorkspaceDataCacheItem.GetLayoutName: string;
begin
    result := fRec.LayoutID;
end;

{ TLayoutWorkspaceDataCache }

constructor TLayoutWorkspaceDataCache.Create;
begin
    inherited Create();
end;

procedure TLayoutWorkspaceDataCache.AddCacheItem(const aRec: TLayoutWorkspaceRec);
var
    xItem: TLayoutWorkspaceDataCacheItem;
begin
    xItem := TLayoutWorkspaceDataCacheItem.Create();
    xItem.FromRec(aRec);
    self.Add(xItem);
end;

procedure TLayoutWorkspaceDataCache.Read;
var
    xLayoutWorkspaceDA: TLayoutWorkspaceDataAdaptor;
    xLayoutWorkspaceRecs: TLayoutWorkspaceRecArray;
    x: integer;
begin
    xLayoutWorkspaceDA := TLayoutWorkspaceDataAdaptor.Create();
    try
        xLayoutWorkspaceDA.ReadAllRecs(xLayoutWorkspaceRecs);
    finally
        xLayoutWorkspaceDA.Free;
    end;

    for x := 0 to high(xLayoutWorkspaceRecs) do
    begin
        self.AddCacheItem(xLayoutWorkspaceRecs[x]);
    end;
end;

{ TLayoutCarrierDataCacheItem }

procedure TLayoutCarrierDataCacheItem.FromRec(const aRec: TLayoutCarrierRec);
begin
    self.LayoutID := aRec.Layout;
    self.CarrierName := aRec.CarrierName;
    self.WorkspaceID := aRec.WorkspaceID;
    self.Carr_X := aRec.Carr_X;
    self.Carr_Y := aRec.Carr_Y;
    self.Carr_Z := aRec.Carr_Z;
    self.CarrierType := aRec.CarrierType;
end;

function TLayoutCarrierDataCacheItem.GetLayoutName: string;
begin
    result := fLayoutID;
end;

function TLayoutCarrierDataCacheItem.ToRec: TLayoutCarrierRec;
begin
    result.Layout := self.LayoutID;
    result.CarrierName := self.CarrierName;
    result.WorkspaceID := self.WorkspaceID;
    result.Carr_X := self.Carr_X;
    result.Carr_Y := self.Carr_Y;
    result.Carr_Z := self.Carr_Z;
    result.CarrierType := self.CarrierType;
end;

{ TLayoutCarrierDataCache }

constructor TLayoutCarrierDataCache.Create;
begin
    inherited Create();
end;

function TLayoutCarrierDataCache.FindCacheItemByName(const aLayoutID: string; const aName: string)
    : TLayoutCarrierDataCacheItem;
var
    xItem: TLayoutCarrierDataCacheItem;
begin

    result := nil;
    for xItem in self do
    begin
        if SameText(xItem.LayoutID, aLayoutID) and SameText(xItem.CarrierName, aName) then
        begin
            result := xItem;
            EXIT;
        end;
    end;
end;

procedure TLayoutCarrierDataCache.AddCacheItem(const aRec: TLayoutCarrierRec);
var
    xItem: TLayoutCarrierDataCacheItem;
begin
    xItem := TLayoutCarrierDataCacheItem.Create();
    xItem.FromRec(aRec);
    self.Add(xItem);
end;

procedure TLayoutCarrierDataCache.Read;
var
    xLayoutCarrierDA: TLayoutDataAdaptor;
    xLayoutCarrierRecs: TLayoutCarrierRecArray;
    x: integer;
begin
    xLayoutCarrierDA := TLayoutDataAdaptor.Create();
    try
        xLayoutCarrierDA.ReadCarrierRecs(xLayoutCarrierRecs);
    finally
        xLayoutCarrierDA.Free;
    end;

    for x := 0 to high(xLayoutCarrierRecs) do
    begin
        self.AddCacheItem(xLayoutCarrierRecs[x]);
    end;
end;

{ TLayoutRackDataCacheItem }

function TLayoutRackDataCache.FindCacheItemByName(const aLayoutID: string; const aName: string)
    : TLayoutRackDataCacheItem;
var
    xItem: TLayoutRackDataCacheItem;
begin

    result := nil;
    for xItem in self do
    begin
        if SameText(xItem.LayoutID, aLayoutID) and SameText(xItem.RackName, aName) then
        begin
            result := xItem;
            EXIT;
        end;
    end;
end;

procedure TLayoutRackDataCacheItem.FromRec(const aRec: TLayoutRackRec);
begin
    self.LayoutID := aRec.Layout;
    self.RackName := aRec.RackName;
    self.RackType := aRec.RackType;
    self.RackID := aRec.RackID;
    self.CarrierName := aRec.CarrierName;
    self.Slot := aRec.Slot;
    self.Rotation := aRec.Rotation;
end;

function TLayoutRackDataCacheItem.GetLayoutName: string;
begin
    result := fLayoutID;
end;

function TLayoutRackDataCacheItem.ToRec: TLayoutRackRec;
begin
    result.Layout := self.LayoutID;
    result.RackName := self.RackName;
    result.RackType := self.RackType;
    result.RackID := self.RackID;
    result.CarrierName := self.CarrierName;
    result.Slot := self.Slot;
    result.Rotation := self.Rotation;
end;

{ TLayoutRackDataCache }

constructor TLayoutRackDataCache.Create();
begin
    inherited Create();
end;

procedure TLayoutRackDataCache.AddCacheItem(const aRec: TLayoutRackRec);
var
    xItem: TLayoutRackDataCacheItem;
begin
    xItem := TLayoutRackDataCacheItem.Create();
    xItem.FromRec(aRec);
    self.Add(xItem);
end;

procedure TLayoutRackDataCache.Read;
var
    xLayoutRackDA: TLayoutDataAdaptor;
    xLayoutRackRecs: TLayoutRackRecArray;
    x: integer;
begin
    xLayoutRackDA := TLayoutDataAdaptor.Create();
    try
        xLayoutRackDA.ReadRackRecs(xLayoutRackRecs);
    finally
        xLayoutRackDA.Free;
    end;

    for x := 0 to high(xLayoutRackRecs) do
    begin
        self.AddCacheItem(xLayoutRackRecs[x]);
    end;
end;

procedure TLayoutRackDataCache.ReadRun(const aRunName: string);
var
    xLayoutRackDA: TLayoutDataAdaptor;
    xLayoutRackRecs: TLayoutRackRecArray;
    x: integer;
begin
    xLayoutRackDA := TLayoutDataAdaptor.Create();
    try
        xLayoutRackDA.ReadRackRecsByRun(aRunName, xLayoutRackRecs);
    finally
        xLayoutRackDA.Free;
    end;

    for x := 0 to high(xLayoutRackRecs) do
    begin
        self.AddCacheItem(xLayoutRackRecs[x]);
    end;
end;

{ TLayoutLinkDataCacheItem }

procedure TLayoutLinkDataCacheItem.FromRec(const aRec: TLayoutLinkRec);
begin
    fRec := aRec;
end;

function TLayoutLinkDataCacheItem.GetLayoutName: string;
begin
    result := fRec.Layout;
end;

function TLayoutLinkDataCacheItem.ToRec: TLayoutLinkRec;
begin
    result := fRec;
end;

{ TLayoutLinkDataCache }

constructor TLayoutLinkDataCache.Create;
begin
    inherited Create();
end;

procedure TLayoutLinkDataCache.AddCacheItem(const aRec: TLayoutLinkRec);
var
    xItem: TLayoutLinkDataCacheItem;
begin
    xItem := TLayoutLinkDataCacheItem.Create();
    xItem.FromRec(aRec);
    self.Add(xItem);
end;

procedure TLayoutLinkDataCache.Read;
var
    xRecs: TLayoutLinkRecArray;
    xLayoutLinkDA: TLayoutDataAdaptor;
    x: integer;
begin
    xLayoutLinkDA := TLayoutDataAdaptor.Create();
    try
        xLayoutLinkDA.ReadLinkedLayoutRecs(xRecs);
    finally
        xLayoutLinkDA.Free;
    end;

    for x := 0 to high(xRecs) do
    begin
        self.AddCacheItem(xRecs[x]);
    end;
end;

{ TLayoutDataCache }

constructor TLayoutDataCache.Create();
begin
    inherited Create();
    fLayoutLinkDataCache := TLayoutLinkDataCache.Create();
    fLayoutWorkspaceDataCache := TLayoutWorkspaceDataCache.Create();
    fLayoutCarrierDataCache := TLayoutCarrierDataCache.Create();
    fLayoutRackDataCache := TLayoutRackDataCache.Create();
    fLayoutTipsetDataCache := TLayoutTipsetDataCache.Create();
end;

destructor TLayoutDataCache.Destroy();
begin
    fLayoutTipsetDataCache.Free;
    fLayoutLinkDataCache.Free;
    fLayoutRackDataCache.Free;
    fLayoutCarrierDataCache.Free;
    fLayoutWorkspaceDataCache.Free;
    inherited;
end;

procedure TLayoutDataCache.Read;
begin
    ClearCache();
    fLayoutLinkDataCache.Read();
    fLayoutWorkspaceDataCache.Read();
    fLayoutCarrierDataCache.Read();
    fLayoutRackDataCache.Read();
    fLayoutTipsetDataCache.Read();
end;

procedure TLayoutDataCache.ReadByRun(const aRunName: string);
begin
    ClearCache();
    fLayoutLinkDataCache.Read();
    fLayoutWorkspaceDataCache.Read();
    fLayoutCarrierDataCache.Read();
    fLayoutRackDataCache.ReadRun(aRunName);
    fLayoutTipsetDataCache.Read();
end;

procedure TLayoutDataCache.ClearCache;
begin
    fLayoutLinkDataCache.ClearCache();
    fLayoutWorkspaceDataCache.ClearCache();
    fLayoutCarrierDataCache.ClearCache();
    fLayoutRackDataCache.ClearCache();
    fLayoutTipsetDataCache.ClearCache();
end;

{ TLayoutTipsetDataCacheItem }

procedure TLayoutTipsetDataCacheItem.FromRec(const aRec: TTipsetRec);
begin
    self.LayoutID := aRec.Tipset;
    self.TipNumber := aRec.TipIndex;
    self.TipTypeName := aRec.TipType;
    self.PipDeviceName := aRec.PipDeviceName;
end;

function TLayoutTipsetDataCacheItem.GetLayoutName: string;
begin
    result := self.LayoutID;
end;

function TLayoutTipsetDataCacheItem.ToRec: TTipsetRec;
begin
    result.Tipset := self.LayoutID;
    result.TipIndex := self.TipNumber;
    result.TipType := self.TipTypeName;
    result.PipDeviceName := self.PipDeviceName;
end;

{ TLayoutTipsetDataCache }

constructor TLayoutTipsetDataCache.Create;
begin
    inherited Create();
end;

procedure TLayoutTipsetDataCache.AddCacheItem(const aRec: TTipsetRec);
var
    xItem: TLayoutTipsetDataCacheItem;
begin
    xItem := TLayoutTipsetDataCacheItem.Create();
    xItem.FromRec(aRec);
    self.Add(xItem);
end;

procedure TLayoutTipsetDataCache.Read;
var
    xRecs: TTipsetRecArray;
    xTipsetDA: TTipsetDataAdaptor;
    x: integer;
begin
    xTipsetDA := TTipsetDataAdaptor.Create();
    try
        xTipsetDA.ReadRecs(xRecs);
    finally
        xTipsetDA.Free;
    end;

    for x := 0 to high(xRecs) do
    begin
        self.AddCacheItem(xRecs[x]);
    end;
end;


end.
