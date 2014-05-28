{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  11.02.13 wl                                      TN6078   Initial Revision
  11.02.13 wl  TVolumeTrackedPosition              TN6078   neu: Volumen von Storage-Positionen werden immer getrackt
  ----------------------------------------------------------------------------------------------------------- }

unit VolumeTracker;


interface


uses
    Generics.Collections,
    RackWell,
    AppTypes,
    LiqHTypes,
    MethodTypes;

type
    TFindRackPosByTubeIDEvent = function(const aTubeID: string): TRackPosition of object;
    TFindRackPosBySubstIDEvent = function(const aSubstID: string; aUseDeadVolume: TUseDeadVolume)
        : TArray<TRackPositionWithVol> of object;
    TFindAllStorageRackPosEvent = function(aUseDeadVolume: TUseDeadVolume)
        : TArray<TRackIDPositionWithVol> of object;

    TRackPositionWithVolAndWaste = record
        Rack: string;
        Pos: integer;
        Vol: extended;
        WasteVol: extended;
    end;

    TVolumeTrackedPosition = class
    strict private
        fRack: string;
        fPos: integer;
        fRestVol: extended;
        fTotalSourceVol: extended;
        procedure SetRestVol(const Value: extended);
    public
        constructor Create(const aRack: string; aPos: integer; aVol: extended);

        class function GetWasteVolByLHP(aCalculateWaste: boolean; aLHRec: TLiqHandlingRec;
            aVol, aPreviousTotalSourceVol: extended; out oTotalSourceVol: extended): extended;
        class procedure AddPos(var vPosition: TArray<TRackPositionWithVolAndWaste>; const aRack: string;
            aPos: integer; aVol, aWasteVol: extended);
        procedure TakeVolAndAddPos(var vPositions: TArray<TRackPositionWithVolAndWaste>;
            aUsedWasteVol, aUsedTotalSVol: extended; const aUsedVol: extended);
        procedure TakeVolAndAddPosExt(var vPositions: TArray<TRackPositionWithVolAndWaste>;
            aVol, aUsedWasteVol, aUsedTotalSVol: extended; out oMissingVolume: extended);

        property Rack: string read fRack;
        property Pos: integer read fPos;
        property RestVol: extended read fRestVol write SetRestVol;
        property TotalSourceVol: extended read fTotalSourceVol write fTotalSourceVol;
    end;

    TVolumeTrackedSubstance = class
    strict private
        fSubstID: string;
        fPosList: TObjectList<TVolumeTrackedPosition>;
        function FindSmallestSufficientPos(aCalculateWaste: boolean; const aID: string;
            const aVol, aMinVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType;
            const aLHRec: TLiqHandlingRec; out oUsedVol, oUsedWasteVol, oUsedTotalSVol: extended)
            : TVolumeTrackedPosition;
        procedure FindRackPosBySubstIDIntern(aCalculateWaste: boolean;
            var vPositions: TArray<TRackPositionWithVolAndWaste>; const aID: string;
            const aVol, aMinVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType;
            const aLHRec: TLiqHandlingRec; out oUsedVol: extended; out oMissingVolume: extended);
        function FindRackPosBySubstIDOnePos(aCalculateWaste: boolean; const aID: string;
            const aVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType;
            out oMissingVolume: extended; const aLHRec: TLiqHandlingRec)
            : TArray<TRackPositionWithVolAndWaste>;
        function FindRackPosBySubstIDSplitPos(aCalculateWaste: boolean; const aID: string;
            aVol, aSplitMinVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType;
            out oMissingVolume: extended; const aLHRec: TLiqHandlingRec)
            : TArray<TRackPositionWithVolAndWaste>;
    public
        constructor Create(aSubstID: string);
        destructor Destroy; override;

        procedure AddPosition(const aRack: string; aPos: integer; aVol: extended);
        function FindPosition(const aRackName: string; const aPos: integer): TVolumeTrackedPosition;
        function FindRackPosBySubstID(aCalculateWaste: boolean; const aID: string;
            const aVol, aSplitMinVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType;
            aSplitAllowed: boolean; out oMissingVolume: extended; const aLHRec: TLiqHandlingRec)
            : TArray<TRackPositionWithVolAndWaste>;

        property SubstID: string read fSubstID;
        property Data: TObjectList<TVolumeTrackedPosition>read fPosList;
    end;

    TVolumeTracker = class
    strict private
        fTrackedSubstances: TObjectList<TVolumeTrackedSubstance>;
        fOnFindRackPosByTubeID: TFindRackPosByTubeIDEvent;
        fOnFindRackPosBySubstID: TFindRackPosBySubstIDEvent;
        fOnFindAllStorageRackPos: TFindAllStorageRackPosEvent;
        fUseDeadVolume: TUseDeadVolume;
        fCalculateWaste: boolean;
        function CheckVolumeAndAddPos(aCalculateWaste: boolean; const aRack: string; aPos: integer;
            aVol, aBasicWasteVol: extended; const aLHRec: TLiqHandlingRec; out oMissingVolume: extended)
            : TArray<TRackPositionWithVolAndWaste>;
        function FindRackPosByTubeIDAsArray(aCheckVolume, aCalculateWaste: boolean; const aID: string;
            const aVol, aBasicWasteVol: extended; const aLHRec: TLiqHandlingRec; out oMissingVolume: extended)
            : TArray<TRackPositionWithVolAndWaste>;
        function FindTrackedPosition(const aRackName: string; const aPos: integer): TVolumeTrackedPosition;
    public
        constructor Create(aOnFindRackPosByTubeID: TFindRackPosByTubeIDEvent;
            aOnFindRackPosBySubstID: TFindRackPosBySubstIDEvent;
            aOnFindAllStorageRackPos: TFindAllStorageRackPosEvent);
        destructor Destroy; override;

        procedure ClearStoragePositions;
        procedure ReLoadAllStoragePositions;
        function FindRackPosBySubstOrTubeID(aCheckVolumeAtTubeID: boolean; const aID: string;
            const aVol, aSplitMinVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType;
            aSplitAllowed: boolean; out oMissingVolume: extended; const aLHRec: TLiqHandlingRec)
            : TArray<TRackPositionWithVolAndWaste>;
        function GetRackPositionVolArray(aRackNameType: TMethodRackNameType; const aRackName: string;
            const aPos: integer; const aVol, aSplitMinVol, aBasicWasteVol: extended; aSplitAllowed: boolean;
            out oMissingVolume: extended; aLHRec: TLiqHandlingRec): TArray<TRackPositionWithVolAndWaste>;
        procedure AddDestVol(const aRackName: string; const aPos: integer; const aVol: extended);

        property UseDeadVolume: TUseDeadVolume read fUseDeadVolume write fUseDeadVolume;
        property CalculateWaste: boolean read fCalculateWaste write fCalculateWaste;
    end;


implementation


uses
    SysUtils,
    MathUtils;

{ TVolumeTrackedPosition }

constructor TVolumeTrackedPosition.Create(const aRack: string; aPos: integer; aVol: extended);
begin
    inherited Create;
    fRack := aRack;
    fPos := aPos;
    fRestVol := aVol;
    fTotalSourceVol := 0;
end;

class function TVolumeTrackedPosition.GetWasteVolByLHP(aCalculateWaste: boolean; aLHRec: TLiqHandlingRec;
    aVol, aPreviousTotalSourceVol: extended; out oTotalSourceVol: extended): extended;
begin
    if (not aLHRec.Valid) or (not aCalculateWaste) then
    begin
        oTotalSourceVol := 0;
        EXIT(0);
    end;

    if aLHRec.SampleAspMultipip then
    begin
        // Multi pipetting: WasteVol wird nur für die erste Verteilung berechnet
        if (aPreviousTotalSourceVol > 0) and ((aPreviousTotalSourceVol + aVol <= aLHRec.SampleAspMultiMaxVol)
            or (aLHRec.SampleAspMultiMaxVol = 0)) then
        begin
            oTotalSourceVol := aPreviousTotalSourceVol + aVol;
            EXIT(0);
        end;
    end;

    result := aLHRec.SampleAspWasteVol + (aLHRec.ExtraGapCount * aLHRec.ExtraGapWasteVol);

    if (aLHRec.SampleAspWastePerCent > 0) then
        result := result + (aVol * aLHRec.SampleAspWastePerCent / 100);

    if aLHRec.SampleAspMultipip then
    begin
        // Multi pipetting, 1. Schritt: Daten merken für nächsten Schritt
        oTotalSourceVol := aVol + result;
    end
    else
        oTotalSourceVol := 0;
end;

procedure TVolumeTrackedPosition.SetRestVol(const Value: extended);
begin
    fRestVol := Value;
    if fRestVol < 0 then
        fRestVol := 0;
end;

class procedure TVolumeTrackedPosition.AddPos(var vPosition: TArray<TRackPositionWithVolAndWaste>;
    const aRack: string; aPos: integer; aVol, aWasteVol: extended);
begin
    SetLength(vPosition, Length(vPosition) + 1);
    vPosition[ high(vPosition)].Rack := aRack;
    vPosition[ high(vPosition)].Pos := aPos;
    vPosition[ high(vPosition)].Vol := aVol;
    vPosition[ high(vPosition)].WasteVol := aWasteVol;
end;

procedure TVolumeTrackedPosition.TakeVolAndAddPos(var vPositions: TArray<TRackPositionWithVolAndWaste>;
    aUsedWasteVol, aUsedTotalSVol: extended; const aUsedVol: extended);
begin
    // komplettes Volumen aus der Position entnehmen
    AddPos(vPositions, fRack, fPos, aUsedVol, aUsedWasteVol);
    self.RestVol := self.RestVol - aUsedVol - aUsedWasteVol;
    self.TotalSourceVol := aUsedTotalSVol;
end;

procedure TVolumeTrackedPosition.TakeVolAndAddPosExt(var vPositions: TArray<TRackPositionWithVolAndWaste>;
    aVol, aUsedWasteVol, aUsedTotalSVol: extended; out oMissingVolume: extended);
var
    xPossibleVol: extended;
    xUsedVol: extended;
begin
    // testen, ob das Volumen ausreicht
    xPossibleVol := self.RestVol - aUsedWasteVol;
    xUsedVol := TMathUtils.MinExtValue([aVol, xPossibleVol]);
    oMissingVolume := aVol - xUsedVol;

    // komplettes Volumen aus der Position entnehmen
    AddPos(vPositions, fRack, fPos, xUsedVol, aUsedWasteVol);
    self.RestVol := self.RestVol - xUsedVol - aUsedWasteVol;
    self.TotalSourceVol := aUsedTotalSVol;
end;

{ TVolumeTrackedSubstance }

constructor TVolumeTrackedSubstance.Create(aSubstID: string);
begin
    inherited Create;
    fSubstID := aSubstID;
    fPosList := TObjectList<TVolumeTrackedPosition>.Create;
end;

destructor TVolumeTrackedSubstance.Destroy;
begin
    FreeAndNil(fPosList);
    inherited;
end;

function TVolumeTrackedSubstance.FindPosition(const aRackName: string; const aPos: integer)
    : TVolumeTrackedPosition;
var
    x: integer;
begin
    for x := 0 to fPosList.Count - 1 do
    begin
        if (fPosList[x].Rack = aRackName) and (fPosList[x].Pos = aPos) then
            EXIT(fPosList[x]);
    end;
    EXIT(nil);
end;

procedure TVolumeTrackedSubstance.AddPosition(const aRack: string; aPos: integer; aVol: extended);
var
    xPosition: TVolumeTrackedPosition;
begin
    xPosition := TVolumeTrackedPosition.Create(aRack, aPos, aVol);
    fPosList.Add(xPosition);
end;

function TVolumeTrackedSubstance.FindSmallestSufficientPos(aCalculateWaste: boolean; const aID: string;
    const aVol, aMinVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType;
    const aLHRec: TLiqHandlingRec; out oUsedVol, oUsedWasteVol, oUsedTotalSVol: extended)
    : TVolumeTrackedPosition;
var
    x: integer;
    xPossibleVol, xPossibleWasteVol, xPossibleTotalSVol: extended;
    xUsedPosVol: extended;
begin
    result := nil;
    oUsedVol := 0;
    oUsedWasteVol := 0;
    oUsedTotalSVol := 0;
    xUsedPosVol := 0;

    for x := 0 to fPosList.Count - 1 do
    begin
        xPossibleWasteVol := aBasicWasteVol + fPosList[x].GetWasteVolByLHP(aCalculateWaste, aLHRec, aVol,
            fPosList[x].TotalSourceVol, xPossibleTotalSVol);
        xPossibleVol := fPosList[x].RestVol - xPossibleWasteVol;

        // Suche die Position mit dem kleinsten ausreichenden Volumen
        if (xPossibleVol > 0) and (xPossibleVol >= aMinVol) and
            ((xUsedPosVol = 0) or (xPossibleVol < xUsedPosVol)) then
        begin
            oUsedWasteVol := xPossibleWasteVol;
            oUsedTotalSVol := xPossibleTotalSVol;
            xUsedPosVol := xPossibleVol;
            oUsedVol := TMathUtils.MinExtValue([aVol, xPossibleVol]);
            result := fPosList[x];
        end;
    end;
end;

procedure TVolumeTrackedSubstance.FindRackPosBySubstIDIntern(aCalculateWaste: boolean;
    var vPositions: TArray<TRackPositionWithVolAndWaste>; const aID: string;
    const aVol, aMinVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType;
    const aLHRec: TLiqHandlingRec; out oUsedVol: extended; out oMissingVolume: extended);
var
    xTrackedPos: TVolumeTrackedPosition;
    xUsedWasteVol, xUsedTotalSVol: extended;
begin
    ASSERT(fPosList.Count > 0, 'VolumeTracker has to own one position');

    // Suche die Position mit dem kleinsten ausreichenden Volumen
    xTrackedPos := FindSmallestSufficientPos(aCalculateWaste, aID, aVol, aMinVol, aBasicWasteVol,
        aRackNameType, aLHRec, oUsedVol, xUsedWasteVol, xUsedTotalSVol);

    if Assigned(xTrackedPos) then
    begin
        // Position gefunden: Ermitteltes Volumen aus der Position entnehmen
        oMissingVolume := 0;
        xTrackedPos.TakeVolAndAddPos(vPositions, xUsedWasteVol, xUsedTotalSVol, oUsedVol);
        EXIT;
    end;

    // keine Position gefunden!
    // Letzte Position mit dem verlangten Volumen verwenden und als MissingVol vermerken
    xTrackedPos := fPosList[fPosList.Count - 1];

    if (length(vPositions) > 0) and (vPositions[ high(vPositions)].Rack = xTrackedPos.Rack) and
        (vPositions[ high(vPositions)].Pos = xTrackedPos.Pos) then
    begin
        // Spezialfall (nur bei SplitVol): Es gab schon eine Entnahme bei dieser Position
        oUsedVol := aVol;
        oMissingVolume := aVol - xTrackedPos.RestVol;
        vPositions[ high(vPositions)].Vol := vPositions[ high(vPositions)].Vol + oUsedVol;
        xTrackedPos.RestVol := xTrackedPos.RestVol - oUsedVol;
        xTrackedPos.TotalSourceVol := xTrackedPos.TotalSourceVol + aVol;
        EXIT;
    end
    else
    begin
        // komplettes Volumen aus der Position entnehmen
        xUsedWasteVol := aBasicWasteVol + xTrackedPos.GetWasteVolByLHP(aCalculateWaste, aLHRec, aVol,
            xTrackedPos.TotalSourceVol, xUsedTotalSVol);
        oMissingVolume := aVol + xUsedWasteVol - xTrackedPos.RestVol;
        oUsedVol := aVol;
        xTrackedPos.TakeVolAndAddPos(vPositions, xUsedWasteVol, xUsedTotalSVol, oUsedVol);
        EXIT;
    end;

end;

function TVolumeTrackedSubstance.FindRackPosBySubstIDOnePos(aCalculateWaste: boolean; const aID: string;
    const aVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType; out oMissingVolume: extended;
    const aLHRec: TLiqHandlingRec): TArray<TRackPositionWithVolAndWaste>;
var
    xUsedVol: extended;
begin
    SetLength(result, 0);
    FindRackPosBySubstIDIntern(aCalculateWaste, result, aID, aVol, aVol, aBasicWasteVol, aRackNameType,
        aLHRec, xUsedVol, oMissingVolume);
end;

function TVolumeTrackedSubstance.FindRackPosBySubstIDSplitPos(aCalculateWaste: boolean; const aID: string;
    aVol, aSplitMinVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType;
    out oMissingVolume: extended; const aLHRec: TLiqHandlingRec): TArray<TRackPositionWithVolAndWaste>;
var
    xUsedVol: extended;
begin
    SetLength(result, 0);

    // Solange Positionen suchen, bis das Volumen stimmt
    while (aVol > 0) do
    begin
        self.FindRackPosBySubstIDIntern(aCalculateWaste, result, aID, aVol, aSplitMinVol, aBasicWasteVol,
            aRackNameType, aLHRec, xUsedVol, oMissingVolume);

        if (oMissingVolume > 0) then
            EXIT;

        aVol := aVol - xUsedVol;
    end;
end;

function TVolumeTrackedSubstance.FindRackPosBySubstID(aCalculateWaste: boolean; const aID: string;
    const aVol, aSplitMinVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType;
    aSplitAllowed: boolean; out oMissingVolume: extended; const aLHRec: TLiqHandlingRec)
    : TArray<TRackPositionWithVolAndWaste>;
begin
    // im VolumeTracker muss mindestens eine Position stehen
    ASSERT(fPosList.Count > 0);

    oMissingVolume := 0;
    if (aVol <= 0) then
    begin
        // Spezialfall Vol = 0: Erstbeste Position verwenden, raus!
        SetLength(result, 0);
        TVolumeTrackedPosition.AddPos(result, fPosList[0].Rack, fPosList[0].Pos, aVol, 0);
        EXIT;
    end;

    if (aSplitAllowed) then
    begin
        // Suche eine oder mehrere Position mit dem kleinsten ausreichenden Volumen
        EXIT(FindRackPosBySubstIDSplitPos(aCalculateWaste, aID, aVol, aSplitMinVol, aBasicWasteVol,
            aRackNameType, oMissingVolume, aLHRec));
    end
    else
    begin
        // Suche >eine< Position mit dem kleinsten ausreichenden Volumen
        EXIT(FindRackPosBySubstIDOnePos(aCalculateWaste, aID, aVol, aBasicWasteVol, aRackNameType,
            oMissingVolume, aLHRec));
    end;
end;

{ TVolumeTracker }

constructor TVolumeTracker.Create(aOnFindRackPosByTubeID: TFindRackPosByTubeIDEvent;
    aOnFindRackPosBySubstID: TFindRackPosBySubstIDEvent;
    aOnFindAllStorageRackPos: TFindAllStorageRackPosEvent);
begin
    inherited Create;
    fTrackedSubstances := TObjectList<TVolumeTrackedSubstance>.Create;
    fOnFindRackPosByTubeID := aOnFindRackPosByTubeID;
    fOnFindRackPosBySubstID := aOnFindRackPosBySubstID;
    fOnFindAllStorageRackPos := aOnFindAllStorageRackPos;
    fUseDeadVolume := TUseDeadVolume.MinVol1;
end;

destructor TVolumeTracker.Destroy;
begin
    FreeAndNil(fTrackedSubstances);
    inherited;
end;

procedure TVolumeTracker.ClearStoragePositions;
begin
    fTrackedSubstances.Clear;
end;

function TVolumeTracker.FindRackPosBySubstOrTubeID(aCheckVolumeAtTubeID: boolean; const aID: string;
    const aVol, aSplitMinVol, aBasicWasteVol: extended; aRackNameType: TMethodRackNameType;
    aSplitAllowed: boolean; out oMissingVolume: extended; const aLHRec: TLiqHandlingRec)
    : TArray<TRackPositionWithVolAndWaste>;
var
    x: integer;
    xVolumeTracker: TVolumeTrackedSubstance;
    xStoragePositions: TArray<TRackPositionWithVol>;
begin
    xVolumeTracker := nil;

    // VolumeTracker suchen, der schon benutzt wurde
    for x := 0 to fTrackedSubstances.Count - 1 do
    begin
        if (fTrackedSubstances[x].SubstID = aID) then
        begin
            xVolumeTracker := fTrackedSubstances[x];
            BREAK;
        end;
    end;

    if not Assigned(xVolumeTracker) then
    begin
        // Neue Daten für SubstID in Layout suchen
        xStoragePositions := fOnFindRackPosBySubstID(aID, fUseDeadVolume);
        if (Length(xStoragePositions) = 0) then
        begin
            // nicht gefunden, ID könnte TubeID sein
            EXIT(FindRackPosByTubeIDAsArray(aCheckVolumeAtTubeID, fCalculateWaste, aID, aVol, aBasicWasteVol,
                aLHRec, oMissingVolume));
        end;

        xVolumeTracker := TVolumeTrackedSubstance.Create(aID);
        for x := 0 to high(xStoragePositions) do
            xVolumeTracker.AddPosition(xStoragePositions[x].Rack, xStoragePositions[x].Pos,
                xStoragePositions[x].Vol);
        fTrackedSubstances.Add(xVolumeTracker);
    end;

    // nach Storage-Positionen suchen, die genug Substanz enthalten
    EXIT(xVolumeTracker.FindRackPosBySubstID(fCalculateWaste, aID, aVol, aSplitMinVol, aBasicWasteVol,
        aRackNameType, aSplitAllowed, oMissingVolume, aLHRec));
end;

function TVolumeTracker.FindRackPosByTubeIDAsArray(aCheckVolume, aCalculateWaste: boolean; const aID: string;
    const aVol, aBasicWasteVol: extended; const aLHRec: TLiqHandlingRec; out oMissingVolume: extended)
    : TArray<TRackPositionWithVolAndWaste>;
var
    xRackPos: TRackPosition;
begin
    SetLength(result, 0);

    xRackPos := fOnFindRackPosByTubeID(aID);
    if (xRackPos.Rack = '') or (xRackPos.Pos <= 0) then
    begin
        EXIT;
    end;

    if (aCheckVolume) then
    begin
        EXIT(CheckVolumeAndAddPos(aCalculateWaste, xRackPos.Rack, xRackPos.Pos, aVol, aBasicWasteVol, aLHRec,
            oMissingVolume));
    end
    else
    begin
        // einfach die Position zurückgeben (ohne Volumenkontrolle)
        TVolumeTrackedPosition.AddPos(result, xRackPos.Rack, xRackPos.Pos, 0, 0);
        EXIT;
    end;
end;

function TVolumeTracker.CheckVolumeAndAddPos(aCalculateWaste: boolean; const aRack: string; aPos: integer;
    aVol, aBasicWasteVol: extended; const aLHRec: TLiqHandlingRec; out oMissingVolume: extended)
    : TArray<TRackPositionWithVolAndWaste>;
var
    xTrackedPos: TVolumeTrackedPosition;
    xUsedWasteVol, xPreviousTotalSVol, xUsedTotalSVol: extended;
begin
    oMissingVolume := 0;
    SetLength(result, 0);

    if (aVol > 0) then
    begin
        // handelt es sich um eine Storage-Position?
        xTrackedPos := FindTrackedPosition(aRack, aPos);
        if Assigned(xTrackedPos) then
            xPreviousTotalSVol := xTrackedPos.TotalSourceVol
        else
            xPreviousTotalSVol := 0;

        xUsedWasteVol := aBasicWasteVol + TVolumeTrackedPosition.GetWasteVolByLHP(aCalculateWaste, aLHRec,
            aVol, xPreviousTotalSVol, xUsedTotalSVol);

        if Assigned(xTrackedPos) then
        begin
            // Volumen aus der Position entnehmen
            xTrackedPos.TakeVolAndAddPosExt(result, aVol, xUsedWasteVol, xUsedTotalSVol, oMissingVolume);
            EXIT;
        end
        else
        begin
            // einfach die Position zurückgeben (ohne Volumenkontrolle)
            TVolumeTrackedPosition.AddPos(result, aRack, aPos, aVol, xUsedWasteVol);
            EXIT;
        end;
    end
    else
    begin
        // einfach die Position zurückgeben
        TVolumeTrackedPosition.AddPos(result, aRack, aPos, 0, 0);
        EXIT;
    end;
end;

procedure TVolumeTracker.ReLoadAllStoragePositions;
var
    x, xUsed: integer;
    xVolumeTracker: TVolumeTrackedSubstance;
    xStoragePositions: TArray<TRackIDPositionWithVol>;
begin
    // alle Volume tracker löschen
    fTrackedSubstances.Clear;

    // All Storage-Positionen suchen
    xStoragePositions := fOnFindAllStorageRackPos(fUseDeadVolume);

    // alle Volume tracker neu erzeugen
    for x := 0 to high(xStoragePositions) do
    begin
        xVolumeTracker := nil;

        // VolumeTracker suchen, der schon benutzt wurde
        for xUsed := 0 to fTrackedSubstances.Count - 1 do
        begin
            if (fTrackedSubstances[xUsed].SubstID = xStoragePositions[x].SubstID) then
            begin
                xVolumeTracker := fTrackedSubstances[xUsed];
                BREAK;
            end;
        end;

        if not Assigned(xVolumeTracker) then
        begin
            xVolumeTracker := TVolumeTrackedSubstance.Create(xStoragePositions[x].SubstID);
            fTrackedSubstances.Add(xVolumeTracker);
        end;

        xVolumeTracker.AddPosition(xStoragePositions[x].Rack, xStoragePositions[x].Pos,
            xStoragePositions[x].Vol);
    end;
end;

function TVolumeTracker.FindTrackedPosition(const aRackName: string; const aPos: integer)
    : TVolumeTrackedPosition;
var
    x: integer;
begin
    result := nil;
    for x := 0 to fTrackedSubstances.Count - 1 do
    begin
        result := fTrackedSubstances[x].FindPosition(aRackName, aPos);
        if Assigned(result) then
            EXIT;
    end;
end;

function TVolumeTracker.GetRackPositionVolArray(aRackNameType: TMethodRackNameType; const aRackName: string;
    const aPos: integer; const aVol, aSplitMinVol, aBasicWasteVol: extended; aSplitAllowed: boolean;
    out oMissingVolume: extended; aLHRec: TLiqHandlingRec): TArray<TRackPositionWithVolAndWaste>;
begin
    SetLength(result, 0);

    // Diese Funktion wird von PListCheck aufgerufen
    if (aPos = INT_RACKPOS_RACKNAME_IS_TUBEID) then
    begin
        // Position = -1 heißt, dass vRackName als TubeID verwendet wird!
        result := self.FindRackPosBySubstOrTubeID(true, aRackName, aVol, aSplitMinVol, aBasicWasteVol,
            aRackNameType, aSplitAllowed, oMissingVolume, aLHRec);
        if (Length(result) = 0) then
        begin
            oMissingVolume := aVol + aBasicWasteVol;
            TVolumeTrackedPosition.AddPos(result, aRackName, aPos, aVol, aBasicWasteVol);
        end;
    end
    else
    begin
        result := self.CheckVolumeAndAddPos(fCalculateWaste, aRackName, aPos, aVol, aBasicWasteVol, aLHRec,
            oMissingVolume);
    end;
end;

procedure TVolumeTracker.AddDestVol(const aRackName: string; const aPos: integer; const aVol: extended);
var
    xTrackedPos: TVolumeTrackedPosition;
begin
    if (aVol > 0) then
    begin
        // handelt es sich um eine Storage-Position?
        xTrackedPos := FindTrackedPosition(aRackName, aPos);

        // Rest-Volumen der Storage-Position wird aufgefüllt!
        if Assigned(xTrackedPos) then
            xTrackedPos.RestVol := xTrackedPos.RestVol + aVol;
    end;
end;


end.
