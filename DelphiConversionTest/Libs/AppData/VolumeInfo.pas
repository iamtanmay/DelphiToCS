{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  15.02.13 wl                                      TN5914   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit VolumeInfo;


interface


uses
    AppTypes;

type
    TVolumeInfo = class
    strict private
        fRackID: string;
        fPos: integer;
        fAspType: TAspirateType;
        fVolumes: TArray<TSubstIDAndVol>;
        function GetTotalVolume: extended;
    public
        constructor Create(aVolumes: TArray<TSubstIDAndVol>; aAspType: TAspirateType); overload;
        constructor Create(aVolumes: TArray<TSubstIDAndVol>; aRackID: string; aPos: integer;
            aAspType: TAspirateType); overload;
        function ReduceVolumes(aVolume: extended): TArray<TSubstIDAndVol>;
        property TotalVolume: extended read GetTotalVolume;
        property Volumes: TArray<TSubstIDAndVol>read fVolumes;
        property RackID: string read fRackID;
        property Pos: integer read fPos;
        property AspType: TAspirateType read fAspType;
    end;


implementation


{ TVolumeInfo }

constructor TVolumeInfo.Create(aVolumes: TArray<TSubstIDAndVol>; aAspType: TAspirateType);
begin
    Create(aVolumes, '', 0, aAspType);
end;

constructor TVolumeInfo.Create(aVolumes: TArray<TSubstIDAndVol>; aRackID: string; aPos: integer;
    aAspType: TAspirateType);
begin
    inherited Create();
    fVolumes := aVolumes;
    fRackID := aRackID;
    fPos := aPos;
    fAspType := aAspType;
end;

function TVolumeInfo.GetTotalVolume: extended;
var
    x: integer;
begin
    result := 0.0;
    for x := 0 to high(fVolumes) do
    begin
        result := result + fVolumes[x].Volume;
    end;
end;

function TVolumeInfo.ReduceVolumes(aVolume: extended): TArray<TSubstIDAndVol>;
var
    x: integer;
    xTotalVolume, xPartVolume: extended;
begin
    SetLength(result, Length(fVolumes));

    if Length(fVolumes) = 0 then
        EXIT;

    if Length(fVolumes) = 1 then
    begin
        fVolumes[0].Volume := fVolumes[0].Volume - aVolume;

        // entnommenes Volumen zurückgeben
        result[0].SubstID := fVolumes[0].SubstID;
        result[0].Volume := aVolume;
        EXIT;
    end;

    // Mehrere Einträge: Von allen den gleichen Anteil abziehen
    xTotalVolume := self.GetTotalVolume;
    for x := 0 to high(fVolumes) do
    begin
        xPartVolume := aVolume * fVolumes[x].Volume / xTotalVolume;
        fVolumes[x].Volume := fVolumes[x].Volume - xPartVolume;

        // entnommenes Volumen zurückgeben
        result[x].Volume := xPartVolume;
        result[x].SubstID := fVolumes[x].SubstID;
    end;

end;


end.
