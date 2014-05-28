{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  31.10.11 wl                                      TN5725   Initial Revision
  21.11.12 wl  ReadAll                             TN6021   ist jetzt public
  27.03.13 wl                                      TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------- }

unit SubstanceDataDataCache;


interface


uses
    Generics.Collections,
    GeneralTypes,
    SubstanceDataDataAdaptor;

type
    TSubstanceDataCacheElement = class
    private
        fRec: TSubstanceDataRec;
        procedure SetRec(const Value: TSubstanceDataRec);
    public
        constructor Create(const aRec: TSubstanceDataRec);
        property Rec: TSubstanceDataRec read fRec write SetRec;
    end;

    TSubstanceDataDataCompleteCache = class(TObjectDictionary<string, TSubstanceDataCacheElement>)
    private
        fDA: TSubstanceDataDataAdaptor;
    public
        constructor Create();
        destructor Destroy(); override;
        function FindByName(const aParamName: string): TSubstanceDataCacheElement;
        procedure ReadAll();
    end;


implementation


uses
    SysUtils,
    TipTypeDataAdaptor;

{ TSubstanceDataCacheElement }

constructor TSubstanceDataCacheElement.Create(const aRec: TSubstanceDataRec);
begin
    inherited Create;
    SetRec(aRec);
end;

procedure TSubstanceDataCacheElement.SetRec(const Value: TSubstanceDataRec);
begin
    fRec := Value;
end;

{ TSubstanceDataDataCompleteCache }

constructor TSubstanceDataDataCompleteCache.Create;
begin
    inherited Create([doOwnsValues]);
    fDA := TSubstanceDataDataAdaptor.Create();
    self.ReadAll();
end;

destructor TSubstanceDataDataCompleteCache.Destroy;
begin
    FreeAndNil(fDA);
    inherited;
end;

function TSubstanceDataDataCompleteCache.FindByName(const aParamName: string): TSubstanceDataCacheElement;
begin
    if not self.TryGetValue(aParamName, result) then
        EXIT(nil);
end;

procedure TSubstanceDataDataCompleteCache.ReadAll;
var
    xRec: TSubstanceDataRec;
begin
    self.Clear;

    fDA.SelectAndOpenAll(true);
    try
        while not fDA.DataProvider.Eof do
        begin
            xRec := fDA.ReadRecAtCursor(fDA.DataProvider);
            self.Add(xRec.SubstID, TSubstanceDataCacheElement.Create(xRec));
            fDA.DataProvider.Next;
        end;
    finally
        fDA.Close();
    end;
end;


end.
