{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no  improvement/change
  -------- --  ----------------------------------  -------- ----------------------------------------------------------
  24.10.13 wl                                      TN6276   initial revision
  14.11.13 wl  DoTipScatteringStandard             TN6276   kann jetzt auch für Teile der Pipettierliste verwendet werden
  28.11.13 wl  AddStepListToTipList                TN6277   vor der Zuordnung zu TipLists werden die Positionen überprüft
  28.11.13 wl  DoTipScatteringStandardUsedTips     TN6277   TipScattering wird abgebrochen, wenn eine Position nicht erreichbar ist
  28.11.13 wl  FillTipStepLists                    TN6277   Wenn eine Position mit einem Tip nicht erreichbar ist, wwird für die anderen Tips probiert
  ----------------------------------------------------------------------------------------------------------------------- }

unit StepOptimizerDest;


interface


uses
    Generics.Collections,
    LiqHTypes,
    IntfPipDevice,
    BasicPipetteTypes,
    StepOptimizer,
    StepOptimizerClasses,
    BasicPipetteRunStep,
    AppTypes;

type
    TStepOptimizerUsedTipsData = record
        UsedTips: TIPMAP;
        NoOfSteps: integer;
    end;

    TDestStepOptimizer = class(TStepOptimizer)
    private
        fSourceSortCriteria: TArray<string>;
        fTipScattering: TMultiPipTipScatteringType;
        fStepShift: TMultiPipStepShiftType;
        fFirstExistingDestRack: string;

        function GetStepListsStatistic(aCriteriaStepLists: TListOfStepLists)
            : TArray<TStepOptimizerUsedTipsData>;

        class procedure SortLists(aLists: TListOfStepLists; aSortCriteria: TArray<string>);
        class function NumDispSteps(aList: TObjectList<TMultiPipStep>): integer;
        class function LeastFullListIndex(aActiveTipLists: TObjectList<TTipStepList>;
            aPossibleTips: TIPMAP): integer;
        function CreateSplitLargeStep(aLargeStep: TBasicPipetteRunStep; aMaxVol: double)
            : TObjectList<TBasicPipetteRunStep>;
        procedure FillStepListsByVolume(const aTip: integer; aResultList: TListOfStepLists;
            aSteps: TObjectList<TBasicPipetteRunStep>);
        class function NextListIndex(aActiveTipLists: TObjectList<TTipStepList>; aPossibleTips: TIPMAP;
            aLastIndex: integer): integer;
        procedure FillTipStepLists(aTipStepLists: TObjectList<TTipStepList>;
            aCriteriaStepLists: TListOfStepLists);
        procedure DoTipScatteringStandardUsedTips(aUsedTips: TIPMAP; aNoOfSteps: integer;
            aTipStepLists: TObjectList<TTipStepList>; aCriteriaStepLists: TListOfStepLists);
        procedure DoTipScatteringStandard(aTipStepLists: TObjectList<TTipStepList>;
            aCriteriaStepLists: TListOfStepLists);
        procedure FillTipListsWithList(aUsedTips: TIPMAP; aActiveTipLists: TObjectList<TTipStepList>;
            aStepList: TObjectList<TBasicPipetteRunStep>;
            const aTipRhythm, aSkipNumTips, aCurrentSpreadTipOffset: integer);
        function CreateMultiPipStepFromSteps(aSteps: TObjectList<TBasicPipetteRunStep>;
            aHasMaxVolume: boolean): TMultiPipStep;
        procedure AddStepListToTipList(aTipList: TTipStepList; aStepList: TObjectList<TBasicPipetteRunStep>);
        function GetPipDeviceTipMaxAspVolume(const aTip: integer; const aLiqHParamRec: TLiqHandlingData)
            : extended;
    public
        constructor Create(aPipDevice: IPipDevice; aOptions: TStepOptimizerOptions;
            aOnCopyStep: TBasicPipetteRunStepCopyEvent);

        procedure OptimizeStepsOfList(aDeviceSteps: TList<TBasicPipetteRunStep>;
            aFinalSteps: TList<TBasicPipetteRunStep>); override;
    end;


implementation


uses
    SysUtils,
    Math,
    LogManager,
    StepOptimizerUtils,
    TipmapUtils,
    TipSystem;

{ TDestStepOptimizer }

constructor TDestStepOptimizer.Create(aPipDevice: IPipDevice; aOptions: TStepOptimizerOptions;
    aOnCopyStep: TBasicPipetteRunStepCopyEvent);
begin
    inherited Create(aPipDevice, aOptions.DestSortCriteria, aOptions.SortDestByTips, aOptions.CheckSourcePos,
        aOptions.CheckDestPos, aOnCopyStep);

    fSourceSortCriteria := TStepOptimizerUtils.StringToCriteria(aOptions.SourceSortCriteria);
    fTipScattering := aOptions.TipScattering;
    fStepShift := aOptions.ShiftType;

    fFirstExistingDestRack := '';
end;

class procedure TDestStepOptimizer.SortLists(aLists: TListOfStepLists; aSortCriteria: TArray<string>);
var
    i: integer;
begin
    if Length(aSortCriteria) = 0 then
        EXIT;

    for i := 0 to aLists.Count - 1 do
    begin
        aLists[i].Sort(TStepComparer.Create(aSortCriteria));
    end;
end;

class function TDestStepOptimizer.NumDispSteps(aList: TObjectList<TMultiPipStep>): integer;
var
    i: integer;
    xStep: TMultiPipStep;
begin
    result := 0;
    for i := 0 to aList.Count - 1 do
    begin
        xStep := aList[i];
        result := result + xStep.DispSteps.Count;
    end;
end;

function TDestStepOptimizer.GetPipDeviceTipMaxAspVolume(const aTip: integer;
    const aLiqHParamRec: TLiqHandlingData): extended;
var
    xMaxVol: extended;
    xPipPumpIndex: integer;
begin
    // TODO: use Volcorr!

    xPipPumpIndex := TTipSystem.PipPumpNumberToPipPumpIndex(aLiqHParamRec.Ch1PumpNumber);
    xMaxVol := fPipDevice.Tips[aTip - 1].GetPipPumpMaxVolume(xPipPumpIndex);

    result := TTipSystemCalc.CalcMaxSampleAndWasteVol(xMaxVol, aLiqHParamRec.TransAirVol,
        aLiqHParamRec.SysAirAspVol, aLiqHParamRec.ExtraGapCount, aLiqHParamRec.ExtraGapAirVol,
        aLiqHParamRec.ExtraGapWasteVol, aLiqHParamRec.SampleAspWasteVol, aLiqHParamRec.SampleAspWastePerCent,
        aLiqHParamRec.SampleAspSpitBack, aLiqHParamRec.SampleAspSpitBackCount);

    // But it cant be more that MultiMaxVol
    if aLiqHParamRec.SampleAspMultipip and (aLiqHParamRec.SampleAspMultiMaxVol > 0) then
    begin
        result := Min(result, aLiqHParamRec.SampleAspMultiMaxVol);
    end;
end;

function TDestStepOptimizer.CreateSplitLargeStep(aLargeStep: TBasicPipetteRunStep; aMaxVol: double)
    : TObjectList<TBasicPipetteRunStep>;
var
    xVolRemaining: double;
    xStep: TBasicPipetteRunStep;
    xNextVol: double;
begin
    result := TObjectList<TBasicPipetteRunStep>.Create(false);

    xVolRemaining := aLargeStep.SourceVol;

    while true do
    begin
        if xVolRemaining = 0 then
            BREAK;
        xStep := self.CopyStep(aLargeStep);
        xNextVol := Math.Min(xVolRemaining, aMaxVol);
        xVolRemaining := xVolRemaining - xNextVol;

        xStep.SourceVol := xNextVol;
        xStep.DestVol := xNextVol;

        result.Add(xStep);
    end;
end;

// Separates the steps into several sublists such that in each sublist the sum of the
// volumes in the field aVolField is <= aMaxVol
// No steps are created or freed.  Steps are transferred from the aSteps list to one of the sublists
// The aSteps list will contain 0 steps at the end of this function
procedure TDestStepOptimizer.FillStepListsByVolume(const aTip: integer; aResultList: TListOfStepLists;
    aSteps: TObjectList<TBasicPipetteRunStep>);
var
    x: integer;
    xSubList: TObjectList<TBasicPipetteRunStep>;
    xVolSoFar: double;
    xIndex: integer;
    xStep: TBasicPipetteRunStep;
    xPossibleVol: double;
    xMaxVol: double;
    xNextVol: double;
    xLargeList: TObjectList<TBasicPipetteRunStep>;
begin
    while aSteps.Count > 0 do
    begin
        xVolSoFar := 0;
        xIndex := 0;
        xSubList := nil;
        while (xIndex < aSteps.Count) and (aSteps.Count > 0) do
        begin
            ASSERT(xIndex < aSteps.Count);
            xStep := aSteps[xIndex];

            // Get the max volume allowed for this set of steps
            xMaxVol := self.GetPipDeviceTipMaxAspVolume(aTip, xStep.LiqHRec);

            if xMaxVol > 0 then
            begin
                xNextVol := xStep.SourceVol;
                if xNextVol > xMaxVol then
                begin
                    xLargeList := CreateSplitLargeStep(xStep, xMaxVol);
                    try
                        aSteps.Delete(xIndex);
                        ASSERT(xIndex <= aSteps.Count);
                        for x := xLargeList.Count - 1 downto 0 do
                        begin
                            aSteps.Insert(xIndex, xLargeList[x]);
                        end;
                    finally
                        xLargeList.Free;
                    end;

                    CONTINUE;
                end;

                xPossibleVol := xVolSoFar + xNextVol;

                if xPossibleVol > xMaxVol then
                begin
                    // Volume has reached max, do not merge this steps
                    Inc(xIndex);
                    CONTINUE;
                end;

                xVolSoFar := xPossibleVol;
            end;

            if (xSubList = nil) then
            begin
                // create a new sublist and add the sublist to the result list
                xSubList := TObjectList<TBasicPipetteRunStep>.Create(false);
                aResultList.Add(xSubList);
            end;

            xSubList.Add(xStep);
            aSteps.Delete(xIndex);
        end;
    end;
end;

// Pre: All steps in the StepList must have same Liquid Handling
// Using the information in the steps of aSteps new steps are created.  The news steps are
// assigned to member variables of the TMultiPipStep that is returned
// All of the Dest volumes in the steps are summed and assigned to the sourcevol of the first step
// All of the Source volumes of the second to last steps are set to 0
// The first Dispense is done in the same step as the Asp step
// The steps in aSteps are freed, but the aSteps list itself is NOT freed.
function TDestStepOptimizer.CreateMultiPipStepFromSteps(aSteps: TObjectList<TBasicPipetteRunStep>;
    aHasMaxVolume: boolean): TMultiPipStep;
const
    INT_VOLUME_DONT_ASPIRATE = 0;
    INT_VOLUME_DONT_DISPENSE = 0;
var
    i: integer;
    xStep: TBasicPipetteRunStep;
    xDispStep: TBasicPipetteRunStep;
    xVolSoFar: double;
    xOk: boolean;
begin
    // -- create MultiPipStep
    result := TMultiPipStep.Create();

    ASSERT(aSteps.Count > 0);
    xStep := aSteps[0];
    if fFirstExistingDestRack = '' then
        fFirstExistingDestRack := xStep.DRack;

    xOk := aHasMaxVolume and xStep.LiqHRec.SampleAspMultipip;
    result.AspStep := self.CopyStep(xStep);
    // -- create a list of DispSteps using First till Last steps
    // But add up Sum up volumes of First till Last Steps
    xVolSoFar := 0;
    for i := 0 to aSteps.Count - 1 do
    begin
        xStep := aSteps[i];
        xDispStep := self.CopyStep(xStep);

        ASSERT(xDispStep.DestVol = xDispStep.SourceVol);
        xVolSoFar := xVolSoFar + xDispStep.SourceVol;

        // if this is not the last step set the TransAir
        if i < (aSteps.Count - 1) then
            xDispStep.DTransAirRetake := xStep.LiqHRec.TransAirRetakeBetweenDisp;

        if (i > 0) and (not xStep.LiqHRec.TransAirRetakeBetweenDisp) then
            xDispStep.DTransAirDisp := false;

        if xOk then
            xDispStep.SourceVol := INT_VOLUME_DONT_ASPIRATE;

        result.DispSteps.Add(xDispStep);
    end;

    // -- create the Asp Step using the first step
    if not xOk then
        result.AspStep.SourceVol := INT_VOLUME_DONT_ASPIRATE
    else
    begin
        result.AspStep.SourceVol := xVolSoFar;
        result.AspStep.DRack := '';
        result.AspStep.DPos := 0;
    end;
    result.AspStep.DestVol := INT_VOLUME_DONT_DISPENSE;
end;

// create multipipsteps from the steps in aStepList using the field aVolField as a criteria and
procedure TDestStepOptimizer.AddStepListToTipList(aTipList: TTipStepList;
    aStepList: TObjectList<TBasicPipetteRunStep>);
var
    xMaxVol: double;
    xStepList: TObjectList<TBasicPipetteRunStep>;
    xMultiPipStep: TMultiPipStep;
    xStepLists: TListOfStepLists;
    i: integer;
    xSPos, xDPos: integer;
begin
    if aStepList.Count = 0 then
        EXIT;

    for i := 0 to aStepList.Count - 1 do
    begin
        xSPos := aStepList[i].SPos;
        if not CheckRackPosition(fCheckSourcePos, aTipList.Tip, aStepList[i].SRack, xSPos) then
            raise EStepOptimizerException.Create('Source Position can not be reached');

        xDPos := aStepList[i].DPos;
        if not CheckRackPosition(fCheckDestPos, aTipList.Tip, aStepList[i].DRack, xDPos) then
            raise EStepOptimizerException.Create('Destination Position can not be reached');

        // falls Positionen geändert wurden (MultiTip-Rack), werden diese jetzt eingetragen
        aStepList[i].SPos := xSPos;
        aStepList[i].DPos := xDPos;
    end;

    xStepLists := TObjectList < TObjectList < TBasicPipetteRunStep >>.Create(true);
    try
        FillStepListsByVolume(aTipList.Tip, xStepLists, aStepList);

        for i := 0 to xStepLists.Count - 1 do
        begin
            xStepList := xStepLists[i];

            // Get the max volume allowed for this set of steps
            xMaxVol := self.GetPipDeviceTipMaxAspVolume(aTipList.Tip, xStepList[0].LiqHRec);
            xMultiPipStep := CreateMultiPipStepFromSteps(xStepList, xMaxVol > 0);

            aTipList.AddStep(xMultiPipStep);
        end;
    finally
        FreeAndNil(xStepLists);
    end;
end;

procedure TDestStepOptimizer.FillTipListsWithList(aUsedTips: TIPMAP;
    aActiveTipLists: TObjectList<TTipStepList>; aStepList: TObjectList<TBasicPipetteRunStep>;
    const aTipRhythm, aSkipNumTips, aCurrentSpreadTipOffset: integer);
var
    xTempList: TObjectList<TBasicPipetteRunStep>;
    xCurrentStepListSpreadTipOffset: integer;
    x: integer;
    xStepListIndex: integer;
begin
    xCurrentStepListSpreadTipOffset := 0;

    // for each tip find the steps that should be added to the list of steps for that tip
    for x := 0 to aActiveTipLists.Count - 1 do
    begin
        if not TTipMapUtils.TipSelected(aUsedTips, x) then
            CONTINUE;
        if (x mod aTipRhythm) <> aCurrentSpreadTipOffset then
            CONTINUE;

        xTempList := TObjectList<TBasicPipetteRunStep>.Create(false);
        try
            xStepListIndex := xCurrentStepListSpreadTipOffset;
            while true do
            begin
                if xStepListIndex >= aStepList.Count then
                    BREAK;

                xTempList.Add(aStepList[xStepListIndex]);
                Inc(xStepListIndex);
                xStepListIndex := xStepListIndex + aSkipNumTips;
            end;

            AddStepListToTipList(aActiveTipLists[x], xTempList);
            // wirft Exception, wenn Positionen nicht erreichbar
        finally
            FreeAndNil(xTempList);
        end;

        Inc(xCurrentStepListSpreadTipOffset);
    end;
end;

procedure TDestStepOptimizer.DoTipScatteringStandard(aTipStepLists: TObjectList<TTipStepList>;
    aCriteriaStepLists: TListOfStepLists);
var
    xStatistic: TArray<TStepOptimizerUsedTipsData>;
    x: integer;
    xCheckIndex: integer;
    xUsedTips: TIPMAP;
begin
    if (aCriteriaStepLists.Count = 0) then
        EXIT;

    xStatistic := GetStepListsStatistic(aCriteriaStepLists);

    for x := 0 to high(xStatistic) do
    begin
        xUsedTips := xStatistic[x].UsedTips;

        // Standard: Die UsedTips dürfen sich nicht überschneiden!
        // Erlaubt ist z.B. eine Kombination von UsedTips=3 (Tip 1 und 2) und UsedTips = 12 (Tip 3 und 4)
        for xCheckIndex := 0 to high(xStatistic) do
        begin
            if (x = xCheckIndex) then
                CONTINUE;

            // Prüfen, ob sich die aktuellen UsedTips mit anderen überschneiden
            if ((xStatistic[xCheckIndex].UsedTips and xUsedTips) > 0) then
            begin
                TLogManager.Instance.Log('TipScattering UsedTips ' + IntToStr(xUsedTips) +
                    ' not possible: Overlapping with UsedTips ' +
                    IntToStr(xStatistic[xCheckIndex].UsedTips), false);
                CONTINUE;
            end;
        end;

        DoTipScatteringStandardUsedTips(xUsedTips, xStatistic[x].NoOfSteps, aTipStepLists,
            aCriteriaStepLists);
    end;
end;

procedure TDestStepOptimizer.DoTipScatteringStandardUsedTips(aUsedTips: TIPMAP; aNoOfSteps: integer;
    aTipStepLists: TObjectList<TTipStepList>; aCriteriaStepLists: TListOfStepLists);
var
    xStepList: TObjectList<TBasicPipetteRunStep>;
    xSkipNumTips: integer;
    xTipRhythm: integer;
    x: integer;
    xNoOfUsedTips: integer;
    xCurrentTipCounter: integer;
begin
    // Es müssen weniger Source-Positionen als Tips sein
    xNoOfUsedTips := TTipmapUtils.GetNoOfUsedTips(aUsedTips, aTipStepLists.Count);
    if (aNoOfSteps >= xNoOfUsedTips) then
    begin
        TLogManager.Instance.Log('TipScattering UsedTips ' + IntToStr(aUsedTips) +
            ' not possible: Too much source positions', false);
        EXIT;
    end;

    TLogManager.Instance.Log('Do TipScattering UsedTips ' + IntToStr(aUsedTips), false);

    // TipRhythm:
    // if Rhythm is 1, then the steps for a single source will be spread across all tips  SRack Pos1 -> DRack Pos 1 to 4
    // if Rhythm is 2, then the steps for a single source will be spread across 2 tips    SRack Pos1 -> DRack Pos 1 to 2; SRack Pos2 -> DRack Pos 3 to 4
    xTipRhythm := aNoOfSteps;
    xSkipNumTips := ((xNoOfUsedTips - 1) div xTipRhythm);
    xCurrentTipCounter := 0;

    for x := 0 to aCriteriaStepLists.Count - 1 do
    begin
        xStepList := aCriteriaStepLists[x];
        if (xStepList[0].UsedTips <> aUsedTips) then
            CONTINUE;

        try
            // StepList zu den TipStepLists hinzufügen
            FillTipListsWithList(aUsedTips, aTipStepLists, xStepList, xTipRhythm, xSkipNumTips,
                (xCurrentTipCounter mod xTipRhythm));
        except
            on E: EStepOptimizerException do
            begin
                raise Exception.Create('TipScattering failed: ' + E.Message);
            end;
        end;

        inc(xCurrentTipCounter);
    end;

    for x := (aCriteriaStepLists.Count - 1) downto 0 do
    begin
        xStepList := aCriteriaStepLists[x];
        if (xStepList[0].UsedTips <> aUsedTips) then
            CONTINUE;

        // StepList löschen
        aCriteriaStepLists.Delete(x);
    end;
end;

function TDestStepOptimizer.GetStepListsStatistic(aCriteriaStepLists: TListOfStepLists)
    : TArray<TStepOptimizerUsedTipsData>;
var
    xStepList: TObjectList<TBasicPipetteRunStep>;
    xUsedTipsList: TList<TStepOptimizerUsedTipsData>;
    x: integer;
    xCurrentData: TStepOptimizerUsedTipsData;
begin
    xCurrentData.UsedTips := 0;
    xCurrentData.NoOfSteps := 1;

    xUsedTipsList := TList<TStepOptimizerUsedTipsData>.Create;
    try
        for x := 0 to aCriteriaStepLists.Count - 1 do
        begin
            xStepList := aCriteriaStepLists[x];
            if (xCurrentData.UsedTips <> xStepList[0].UsedTips) then
            begin
                if (x > 0) then
                    xUsedTipsList.Add(xCurrentData);
                xCurrentData.UsedTips := xStepList[0].UsedTips;
                xCurrentData.NoOfSteps := 1;
            end
            else
            begin
                inc(xCurrentData.NoOfSteps);
            end;
        end;

        xUsedTipsList.Add(xCurrentData);
        EXIT(xUsedTipsList.ToArray);
    finally
        FreeAndNil(xUsedTipsList)
    end;
end;

class function TDestStepOptimizer.LeastFullListIndex(aActiveTipLists: TObjectList<TTipStepList>;
    aPossibleTips: TIPMAP): integer;
var
    i: integer;
    xMin, xCount: integer;
begin
    xMin := high(integer);
    result := -1;
    for i := 0 to aActiveTipLists.Count - 1 do
    begin
        if not TTipMapUtils.TipSelected(aPossibleTips, i) then
            CONTINUE;

        xCount := NumDispSteps(aActiveTipLists[i]);
        if (xCount >= xMin) then
            CONTINUE;

        xMin := xCount;
        result := i;
    end;
end;

class function TDestStepOptimizer.NextListIndex(aActiveTipLists: TObjectList<TTipStepList>;
    aPossibleTips: TIPMAP; aLastIndex: integer): integer;
var
    xCounter: integer;
begin
    result := aLastIndex;
    xCounter := 0;
    repeat
        if (xCounter > aActiveTipLists.Count) then // Endlosschleife verhindern
            raise Exception.Create('TStepOptimizer_Liquid.NextListIndex: No possible tip found!');
        inc(xCounter);

        inc(result);
        if result = aActiveTipLists.Count then
            result := 0;

    until TTipMapUtils.TipSelected(aPossibleTips, result);
end;

procedure TDestStepOptimizer.FillTipStepLists(aTipStepLists: TObjectList<TTipStepList>;
    aCriteriaStepLists: TListOfStepLists);
var
    xTipListsIndex: integer;
    xStepList: TObjectList<TBasicPipetteRunStep>;
    x: integer;
    xPossibleTips: TIPMAP;
    xTipFound: boolean;
begin
    if (aCriteriaStepLists.Count = 0) then
        EXIT;

    // StepLists is a list of StepLists each list has steps with the same source
    // All steps have the same Liqparam
    xTipListsIndex := -1;
    for x := 0 to aCriteriaStepLists.Count - 1 do
    begin
        xStepList := aCriteriaStepLists[x];

        // UsedTips ist für alle Schritte einer StepList gleich
        xPossibleTips := xStepList[0].UsedTips;

        xTipFound := false;
        while (not xTipFound) do
        begin
            case fStepShift of
                ssoReduceSourceSteps:
                    xTipListsIndex := LeastFullListIndex(aTipStepLists, xPossibleTips);
                else
                    xTipListsIndex := self.NextListIndex(aTipStepLists, xPossibleTips, xTipListsIndex);
            end;

            try
                AddStepListToTipList(aTipStepLists[xTipListsIndex], xStepList);
                // wirft Exception, wenn Positionen nicht erreichbar
                xTipFound := true;
            except
                on E: EStepOptimizerException do
                begin
                    TTipMapUtils.UnselectTip(xPossibleTips, xTipListsIndex);
                    if xPossibleTips = 0 then
                        raise;
                end;
            end;
        end;
    end;
end;

procedure TDestStepOptimizer.OptimizeStepsOfList(aDeviceSteps: TList<TBasicPipetteRunStep>;
    aFinalSteps: TList<TBasicPipetteRunStep>);
var
    xCriteriaStepLists: TListOfStepLists;
    xTipStepLists: TObjectList<TTipStepList>;
begin
    xTipStepLists := self.CreateTipLists();
    try
        if (xTipStepLists.Count = 0) then
            EXIT;

        // seperate into lists with same source
        xCriteriaStepLists := TStepOptimizerUtils.CreateStepListsForCriteria(aDeviceSteps,
            fSourceSortCriteria, fSourceSortCriteria);
        try
            // sort all steps by destination
            TStepOptimizerUtils.LogLists(xCriteriaStepLists, 'List before sorting');
            SortLists(xCriteriaStepLists, fDestSortCriteria);
            TStepOptimizerUtils.LogLists(xCriteriaStepLists, 'List after sorting');

            // Assign Steps to tip lists
            if (fTipScattering = stoStandard) then
                DoTipScatteringStandard(xTipStepLists, xCriteriaStepLists);
            FillTipStepLists(xTipStepLists, xCriteriaStepLists);
            TStepOptimizerUtils.LogTipLists(xTipStepLists, 'Filled Tip Step List');
        finally
            xCriteriaStepLists.Free;
        end;

        // write the steps from the tip lists to the
        WriteTipListsToFinalStepList(xTipStepLists, aFinalSteps);
    finally
        xTipStepLists.Free;
    end;
end;


end.
