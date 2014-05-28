{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.06.08 pk                                TN4139  Initial Revision
  09.07.08 pk  ClearCache                    TN4139  now public
  16.07.08 wl  TWorkspaceTypeDataCacheItem   TN4164   neu: fDeviceRecs
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  10.04.13 wl                                TN6045   uses geändert
  18.09.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit LayoutElementTypeDataCache;


interface


uses
    Generics.Collections,
    RackDataAdaptor,
    CarrierDataAdaptor,
    WorkspaceDataAdaptor,
    WorkspaceDevicesDataAdaptor;

type
    // LayoutElementType
    TLayoutElementTypeDataCacheItem = class
    end;

    TLayoutElementTypeDataCache<T: TLayoutElementTypeDataCacheItem> = class(TObjectList<T>)
    protected
        procedure ClearCache();
        procedure read(); virtual; abstract;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Refresh();
    end;

    TWorkspaceTypeDataCacheItem = class(TLayoutElementTypeDataCacheItem)
    private
        fRec: TWorkspaceRec;
        fDeviceRecs: TWorkspaceDevicesRecArray;
    public
        constructor Create(const aRec: TWorkspaceRec; const aDeviceRecs: TWorkspaceDevicesRecArray);
        property Rec: TWorkspaceRec read fRec;
        property DeviceRecs: TWorkspaceDevicesRecArray read fDeviceRecs;
    end;

    TWorkspaceTypeDataCache = class(TLayoutElementTypeDataCache<TWorkspaceTypeDataCacheItem>)
    protected
        procedure read(); override;
    public
        constructor Create();
        procedure AddCacheItem(const aRec: TWorkspaceRec; const aDeviceRecs: TWorkspaceDevicesRecArray);
        function FindItemByType(const aType: integer): TWorkspaceTypeDataCacheItem;
    end;

    TCarrierTypeDataCacheItem = class(TLayoutElementTypeDataCacheItem)
    private
        fRec: TCarrierRec;
    public
        constructor Create(const aRec: TCarrierRec);
        property Rec: TCarrierRec read fRec write fRec;
    end;

    TCarrierTypeDataCache = class(TLayoutElementTypeDataCache<TCarrierTypeDataCacheItem>)
    protected
        procedure read(); override;
    public
        constructor Create();
        procedure AddCacheItem(const aRec: TCarrierRec);
        function FindItemByType(const aType: string): TCarrierTypeDataCacheItem;
    end;

    TRackTypeDataCacheItem = class(TLayoutElementTypeDataCacheItem)
    private
        fRec: TRackRec;
    public
        constructor Create(const aRec: TRackRec);
        property Rec: TRackRec read fRec write fRec;
    end;

    TRackTypeDataCache = class(TLayoutElementTypeDataCache<TRackTypeDataCacheItem>)
    protected
        procedure read(); override;
    public
        constructor Create();
        procedure AddCacheItem(const aRec: TRackRec);
        function FindItemByType(const aType: string): TRackTypeDataCacheItem;
    end;

    TLayoutTypeDataCache = class
    private
        fWorkspaceTypeDataCache: TWorkspaceTypeDataCache;
        fCarrierTypeDataCache: TCarrierTypeDataCache;
        fRackTypeDataCache: TRackTypeDataCache;
        procedure read();
    public
        constructor Create();
        destructor Destroy; override;
        procedure ClearCache();
        procedure Refresh;
        property WorkspaceTypeDataCache: TWorkspaceTypeDataCache read fWorkspaceTypeDataCache;
        property CarrierTypeDataCache: TCarrierTypeDataCache read fCarrierTypeDataCache;
        property RackTypeDataCache: TRackTypeDataCache read fRackTypeDataCache;
    end;


implementation


uses
    SysUtils;

{ TLayoutElementTypeDataCache<T> }

constructor TLayoutElementTypeDataCache<T>.Create;
begin
    inherited Create(true);
end;

destructor TLayoutElementTypeDataCache<T>.Destroy;
begin
    self.ClearCache();
    inherited;
end;

procedure TLayoutElementTypeDataCache<T>.ClearCache;
begin
    self.Clear();
end;

procedure TLayoutElementTypeDataCache<T>.Refresh;
begin
    ClearCache();
    read();
end;

{ TWorkspaceTypeDataCacheItem }

constructor TWorkspaceTypeDataCacheItem.Create(const aRec: TWorkspaceRec;
    const aDeviceRecs: TWorkspaceDevicesRecArray);
begin
    inherited Create();
    fRec := aRec;
    fDeviceRecs := aDeviceRecs
end;

{ TWorkspaceTypeDataCache }

constructor TWorkspaceTypeDataCache.Create;
begin
    inherited Create();
end;

procedure TWorkspaceTypeDataCache.AddCacheItem(const aRec: TWorkspaceRec;
    const aDeviceRecs: TWorkspaceDevicesRecArray);
begin
    self.Add(TWorkspaceTypeDataCacheItem.Create(aRec, aDeviceRecs));
end;

procedure TWorkspaceTypeDataCache.Read;
var
    xWorkspaceDA: TWorkspaceDataAdaptor;
    xWorkspaceDevicesDA: TWorkspaceDevicesDataAdaptor;
    xWorkspaceRecs: TWorkspaceRecArray;
    xDeviceRecs: TWorkspaceDevicesRecArray;
    x: integer;
begin
    xWorkspaceDA := TWorkspaceDataAdaptor.Create();
    try
        xWorkspaceDA.ReadRecs(xWorkspaceRecs);
    finally
        xWorkspaceDA.Free;
    end;

    xWorkspaceDevicesDA := TWorkspaceDevicesDataAdaptor.Create();
    try
        for x := 0 to high(xWorkspaceRecs) do
        begin
            xWorkspaceDevicesDA.ReadByWorkspaceID(xWorkspaceRecs[x].ID, xDeviceRecs);
            self.AddCacheItem(xWorkspaceRecs[x], xDeviceRecs);
        end;
    finally
        xWorkspaceDevicesDA.Free;
    end;
end;

function TWorkspaceTypeDataCache.FindItemByType(const aType: integer): TWorkspaceTypeDataCacheItem;
var
    x: integer;
    xItem: TWorkspaceTypeDataCacheItem;
begin
    result := nil;
    for x := 0 to self.Count - 1 do
    begin
        xItem := self[x];
        if xItem.Rec.ID = aType then
        begin
            result := xItem;
            EXIT;
        end;
    end;
end;

{ TCarrierTypeDataCacheItem }

constructor TCarrierTypeDataCacheItem.Create(const aRec: TCarrierRec);
begin
    inherited Create();
    fRec := aRec;
end;

{ TCarrierTypeDataCache }

constructor TCarrierTypeDataCache.Create;
begin
    inherited Create();
end;

procedure TCarrierTypeDataCache.AddCacheItem(const aRec: TCarrierRec);
begin
    self.Add(TCarrierTypeDataCacheItem.Create(aRec));
end;

procedure TCarrierTypeDataCache.Read;
var
    xCarrierDA: TCarrierDataAdaptor;
    xCarrierRecs: TCarrierRecArray;
    x: integer;
begin
    xCarrierDA := TCarrierDataAdaptor.Create();
    try
        xCarrierDA.ReadRecs(xCarrierRecs);
    finally
        xCarrierDA.Free;
    end;

    for x := 0 to high(xCarrierRecs) do
    begin
        self.AddCacheItem(xCarrierRecs[x]);
    end;
end;

function TCarrierTypeDataCache.FindItemByType(const aType: string): TCarrierTypeDataCacheItem;
var
    x: integer;
    xItem: TCarrierTypeDataCacheItem;
begin
    result := nil;
    for x := 0 to self.Count - 1 do
    begin
        xItem := self[x];
        if SameText(xItem.Rec.Name, aType) then
        begin
            result := xItem;
            EXIT;
        end;
    end;
end;

{ TRackTypeDataCacheItem }
constructor TRackTypeDataCacheItem.Create(const aRec: TRackRec);
begin
    inherited Create();
    fRec := aRec;
end;

{ TRackTypeDataCache }

constructor TRackTypeDataCache.Create;
begin
    inherited Create();
end;

procedure TRackTypeDataCache.AddCacheItem(const aRec: TRackRec);
begin
    self.Add(TRackTypeDataCacheItem.Create(aRec));
end;

procedure TRackTypeDataCache.Read;
var
    xRackDA: TRackDataAdaptor;
    xRackRecs: TRackRecArray;
    x: integer;
begin
    xRackDA := TRackDataAdaptor.Create();
    try
        xRackDA.ReadRecs(xRackRecs);
    finally
        xRackDA.Free;
    end;

    for x := 0 to high(xRackRecs) do
    begin
        self.AddCacheItem(xRackRecs[x]);
    end;
end;

function TRackTypeDataCache.FindItemByType(const aType: string): TRackTypeDataCacheItem;
var
    x: integer;
    xItem: TRackTypeDataCacheItem;
begin
    result := nil;
    for x := 0 to self.Count - 1 do
    begin
        xItem := self[x];
        if SameText(xItem.Rec.Name, aType) then
        begin
            result := xItem;
            EXIT;
        end;
    end;
end;

{ TLayoutTypeDataCache }

constructor TLayoutTypeDataCache.Create;
begin
    inherited Create();
    fWorkspaceTypeDataCache := TWorkspaceTypeDataCache.Create();
    fCarrierTypeDataCache := TCarrierTypeDataCache.Create();
    fRackTypeDataCache := TRackTypeDataCache.Create();
end;

destructor TLayoutTypeDataCache.Destroy();
begin
    fRackTypeDataCache.Free;
    fCarrierTypeDataCache.Free;
    fWorkspaceTypeDataCache.Free;
    inherited;
end;

procedure TLayoutTypeDataCache.Read;
begin
    fWorkspaceTypeDataCache.Read();
    fCarrierTypeDataCache.Read();
    fRackTypeDataCache.Read();
end;

procedure TLayoutTypeDataCache.ClearCache;
begin
    fWorkspaceTypeDataCache.ClearCache();
    fCarrierTypeDataCache.ClearCache();
    fRackTypeDataCache.ClearCache();
end;

procedure TLayoutTypeDataCache.Refresh();
begin
    ClearCache();
    read();
end;


end.
