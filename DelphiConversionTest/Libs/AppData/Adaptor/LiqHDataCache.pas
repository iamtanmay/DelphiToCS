{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data cache for liquid handling parameters
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.10.08 pk                                TN4265   Initial Revision
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  29.06.10 pk                                TN5143   New TLiqHDataCompleteCache
  20.01.11 wl  TLiqHDataCache.Create         TN5438   die Liste hat die Eigenschaft dupIgnore, dann ist sie weniger fehleranfällig
  06.04.11 wl  ReadAll                       TN5501   jetzt public
  06.04.11 wl  fIsRedi                       TN5501   neu
  14.04.11 wl  ReloadLiqHData                TN5501   lädt nur die Daten neu, dadurch bleiben bestehende Referenzen auf TLiqHCacheElement erhalten
  11.04.13 wl                                TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit LiqHDataCache;


interface


uses
    Generics.Collections,
    LiqHTypes,
    LiqHDataAdaptor;

type
    TLiqHDataCache = class
    private
        fDA: TLiqHDataAdaptor;
        fLiqHNames: TList<string>;
        fLiqHNamesLastRead: TDateTime;
        procedure RefreshLiqHNames();
        class var uInstance: TLiqHDataCache;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure AddLiqHName(const aName: string);
        procedure DeleteLiqHName(const aName: string);
        function GetLiqHNames(): TArray<string>;

        class function CreateInstance(): TLiqHDataCache;
        class procedure DestroyInstance();
        class property Instance: TLiqHDataCache read uInstance;
    end;

    TLiqHCacheElement = class
    private
        fRec: TLiqHandlingRec;
        fIsRedi: boolean;
        procedure SetRec(const Value: TLiqHandlingRec);
    public
        constructor Create(const aRec: TLiqHandlingRec);
        property Rec: TLiqHandlingRec read fRec write SetRec;
        property IsRedi: boolean read fIsRedi;
    end;

    TLiqHDataCompleteCache = class(TObjectDictionary<string, TLiqHCacheElement>)
    private
        fDA: TLiqHDataAdaptor;
        procedure ReadAll();
    public
        constructor Create();
        destructor Destroy(); override;
        procedure ReloadLiqHDataAll();
        procedure ReloadLiqHData(const aParamName: string);
        function FindByName(const aParamName: string): TLiqHCacheElement;
    end;


implementation


uses
    SysUtils,
    TipTypeDataAdaptor;

{ TLiqHDataCache }

constructor TLiqHDataCache.Create;
begin
    inherited Create();
    fDA := TLiqHDataAdaptor.Create();
    fLiqHNames := TList<string>.Create;
    fLiqHNamesLastRead := 0;
    RefreshLiqHNames();
end;

destructor TLiqHDataCache.Destroy;
begin
    FreeAndNil(fLiqHNames);
    FreeAndNil(fDA);
    inherited;
end;

class function TLiqHDataCache.CreateInstance(): TLiqHDataCache;
begin
    // create instance if instance does not exist
    if not Assigned(uInstance) then
        uInstance := TLiqHDataCache.Create();

    // return instance
    result := uInstance;
end;

class procedure TLiqHDataCache.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

function TLiqHDataCache.GetLiqHNames(): TArray<string>;
begin
    result := fLiqHNames.ToArray;
end;

procedure TLiqHDataCache.RefreshLiqHNames;
var
    xNames: TArray<string>;
begin
    fLiqHNames.Clear();
    xNames := fDA.ReadAllNames();
    fLiqHNames.AddRange(xNames);
end;

procedure TLiqHDataCache.AddLiqHName(const aName: string);
begin
    fLiqHNames.Add(aName);
end;

procedure TLiqHDataCache.DeleteLiqHName(const aName: string);
var
    xIndex: integer;
begin
    xIndex := fLiqHNames.IndexOf(aName);
    if xIndex < 0 then
        EXIT;
    fLiqHNames.Delete(xIndex);
end;

{ TLiqHCacheElement }

constructor TLiqHCacheElement.Create(const aRec: TLiqHandlingRec);
begin
    inherited Create;
    SetRec(aRec);
end;

procedure TLiqHCacheElement.SetRec(const Value: TLiqHandlingRec);
begin
    fRec := Value;
    fIsRedi := TTipTypeDataAdaptor.TipNameIsRedi(fRec.UsedTipType);
end;

{ TLiqHDataCompleteCache }

constructor TLiqHDataCompleteCache.Create;
begin
    inherited Create([doOwnsValues]);
    fDA := TLiqHDataAdaptor.Create();
    self.ReadAll();
end;

destructor TLiqHDataCompleteCache.Destroy;
begin
    FreeAndNil(fDA);
    inherited;
end;

function TLiqHDataCompleteCache.FindByName(const aParamName: string): TLiqHCacheElement;
begin
    if self.TryGetValue(aParamName, result) then
        EXIT
    else
        raise Exception.Create('Liquid handling param ' + aParamName + ' not found');
end;

procedure TLiqHDataCompleteCache.ReadAll;
var
    xRec: TLiqHandlingRec;
begin
    self.Clear;

    fDA.SelectAndOpenAll(true);
    try
        while not fDA.DataProvider.Eof do
        begin

            fDA.ReadRecFromDataset(fDA.DataProvider, xRec);
            self.Add(xRec.PARAMNAME, TLiqHCacheElement.Create(xRec));
            fDA.DataProvider.Next;
        end;
    finally
        fDA.Close();
    end;
end;

procedure TLiqHDataCompleteCache.ReloadLiqHDataAll;
var
    xElement: TLiqHCacheElement;
    xRec: TLiqHandlingRec;
begin
    // weniger brachial als ReadAll: Nur die Daten der LiquidHandlingParameter werden neu gelesen,
    // dadurch bleiben bestehende Referenzen auf TLiqHCacheElement erhalten

    for xElement in self.Values do
    begin
        if fDA.ReadRec(xElement.Rec.PARAMNAME, xRec) then
            xElement.Rec := xRec;
    end;
end;

procedure TLiqHDataCompleteCache.ReloadLiqHData(const aParamName: string);
var
    xElement: TLiqHCacheElement;
    xRec: TLiqHandlingRec;
begin
    if self.TryGetValue(aParamName, xElement) then
    begin
        if fDA.ReadRec(xElement.Rec.PARAMNAME, xRec) then
            xElement.Rec := xRec;
    end;
end;


end.
