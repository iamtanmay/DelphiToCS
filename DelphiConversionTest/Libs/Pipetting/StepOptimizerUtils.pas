{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no  improvement/change
  -------- --  ----------------------------------  -------- ----------------------------------------------------------
  24.10.13 wl                                      TN6276   initial revision
  ----------------------------------------------------------------------------------------------------------------------- }

unit StepOptimizerUtils;


interface


uses
    Generics.Defaults,
    Generics.Collections,
    BasicPipetteRunStep,
    StepOptimizerClasses,
    BasicPipetteTypes;

type
    TStepComparer = class(TInterfacedObject, IComparer<TBasicPipetteRunStep>)
    strict private
        fCriteria: TArray<string>;
    public
        constructor Create(aCriteria: TArray<string>);
        function Compare(const aItem1, aItem2: TBasicPipetteRunStep): Integer;

        class function StepCompare(aLeft, aRight: TBasicPipetteRunStep; aCriteria: TArray<string>): integer;
    end;

    TStepOptimizerUtils = record
    private const
        cCriteriaDelimiter = ';';
    private
        class procedure LogList(aStepList: TList<TBasicPipetteRunStep>); static;
        class procedure LogTipList(aStepList: TTipStepList); static;
    public
        class procedure LogLists(aCriteriaStepLists: TObjectList<TObjectList<TBasicPipetteRunStep>>;
            const aFirstText: string); static;
        class procedure LogTipLists(aTipStepLists: TList<TTipStepList>; const aFirstText: string); static;
        class function DefineOptions(aTipScattering: TMultiPipTipScatteringType;
            aStepShift: TMultiPipStepShiftType; const aSourceSortCriteria, aDestSortCriteria: string;
            aCombineDifferentLiqParams: boolean; aCheckSourcePos, aCheckDestPos: boolean)
            : TStepOptimizerOptions; static;
        class function StringToCriteria(const aText: string): TArray<string>; static;
        class function CreateStepListsForCriteria(aSteps: TList<TBasicPipetteRunStep>;
            const aSortCriteria, aCompareCriteria: TArray<string>): TListOfStepLists; static;
    end;


implementation


uses     LogManager,
    SysUtils,
    ArrayUtils;

{ TStepComparer }

constructor TStepComparer.Create(aCriteria: TArray<string>);
begin
    inherited Create;
    fCriteria := aCriteria;
end;

class function TStepComparer.StepCompare(aLeft, aRight: TBasicPipetteRunStep;
    aCriteria: TArray<string>): integer;
var
    i: integer;
    xLeftFields, xRightFields: TArray<Variant>;
begin
    xLeftFields := aLeft.GetFieldValsAsArray(aCriteria);
    xRightFields := aRight.GetFieldValsAsArray(aCriteria);

    for i := 0 to high(xLeftFields) do
    begin
        if (xLeftFields[i] > xRightFields[i]) then
            EXIT(1);

        if xLeftFields[i] < xRightFields[i] then
            EXIT(-1);
    end;

    EXIT(0);
end;

function TStepComparer.Compare(const aItem1, aItem2: TBasicPipetteRunStep): Integer;
begin
    result := StepCompare(aItem1, aItem2, fCriteria);
end;

{ TStepOptimizerUtils }

class procedure TStepOptimizerUtils.LogList(aStepList: TList<TBasicPipetteRunStep>);
var
    x: integer;
    xText: string;
begin
    xText := ' ';
    for x := 0 to aStepList.Count - 1 do
    begin
        xText := xText + IntToStr(aStepList[x].SPos) + ',';
    end;
    TLogManager.Instance.Log(xText, false);
end;

class procedure TStepOptimizerUtils.LogLists(aCriteriaStepLists
    : TObjectList<TObjectList<TBasicPipetteRunStep>>; const aFirstText: string);
var
    x: integer;
begin
    TLogManager.Instance.Log(aFirstText, false);
    TLogManager.Instance.Log(' CriteriaStepLists - BEGIN', false);
    for x := 0 to aCriteriaStepLists.Count - 1 do
    begin
        LogList(aCriteriaStepLists[x]);
    end;
    TLogManager.Instance.Log(' CriteriaStepLists - END', false);
end;

class procedure TStepOptimizerUtils.LogTipList(aStepList: TTipStepList);
var
    x: integer;
    xText: string;
begin
    xText := ' Tip ' + IntToStr(aStepList.Tip) + ': ';
    for x := 0 to aStepList.Count - 1 do
    begin
        xText := xText + IntToStr(aStepList[x].AspStep.SPos) + ',';
    end;
    TLogManager.Instance.Log(xText, false);
end;

class procedure TStepOptimizerUtils.LogTipLists(aTipStepLists: TList<TTipStepList>; const aFirstText: string);
var
    x: integer;
begin
    TLogManager.Instance.Log(aFirstText, false);
    TLogManager.Instance.Log(' TipStepLists - BEGIN', false);
    for x := 0 to aTipStepLists.Count - 1 do
    begin
        LogTipList(aTipStepLists[x]);
    end;
    TLogManager.Instance.Log(' TipStepLists - END', false);
end;

class function TStepOptimizerUtils.DefineOptions(aTipScattering: TMultiPipTipScatteringType;
    aStepShift: TMultiPipStepShiftType; const aSourceSortCriteria, aDestSortCriteria: string;
    aCombineDifferentLiqParams: boolean; aCheckSourcePos, aCheckDestPos: boolean): TStepOptimizerOptions;
begin
    result.SourceSortCriteria := aSourceSortCriteria;
    result.DestSortCriteria := aDestSortCriteria;
    result.TipScattering := aTipScattering;
    result.ShiftType := aStepShift;
    result.CheckSourcePos := aCheckSourcePos;
    result.CheckDestPos := aCheckDestPos;

    // Es ist nicht klar, was diese Option genau bewirkt: Bitte testen!
    result.SortDestByTips := false;

    // Kein Kriterium: Nimm den Default
    if aSourceSortCriteria = '' then
        result.SourceSortCriteria := cSourceSortCriteriaDefault;
    if aDestSortCriteria = '' then
        result.DestSortCriteria := cDestSortCriteriaDefault;

    // NONE: Es wird bewusst kein Kriterium verwendet
    if SameText(aSourceSortCriteria, cSourceSortCriteriaNone) then
        result.SourceSortCriteria := '';
    if SameText(aDestSortCriteria, cDestSortCriteriaNone) then
        result.DestSortCriteria := '';

    if (aCombineDifferentLiqParams) then
        // Zunächst werden alle Schritte auch bei nicht kombinierbaren Parametern zusammengefasst.
        // Später erfolgt aber noch eine Prüfung mit TLiqHandlingParamComparer.
        result.BasicSortCriteria := 'PIPDEVICE'
    else
        result.BasicSortCriteria := 'LIQPARAM;PIPDEVICE';

    // Wenn UsedTips nicht gleich sind können Asp- und Disp-Schritte nicht direkt zusammengefasst werden
    result.SourceSortCriteria := 'USEDTIPS;' + result.SourceSortCriteria;
end;

class function TStepOptimizerUtils.StringToCriteria(const aText: string): TArray<string>;
begin
    // alles als Großbuchstaben:
    EXIT(TArrayUtils.StringToStringArray(UpperCase(aText), cCriteriaDelimiter));
end;

class function TStepOptimizerUtils.CreateStepListsForCriteria(aSteps: TList<TBasicPipetteRunStep>;
    const aSortCriteria, aCompareCriteria: TArray<string>): TListOfStepLists;
var
    xStep: TBasicPipetteRunStep;
    xPreviousStep: TBasicPipetteRunStep;
    i: integer;
    xSubList: TObjectList<TBasicPipetteRunStep>;
begin
    if aSortCriteria <> nil then
        aSteps.Sort(TStepComparer.Create(aSortCriteria));

    result := TObjectList < TObjectList < TBasicPipetteRunStep >>.Create(true);
    xSubList := nil;
    xPreviousStep := nil;
    // Consecutive elements where the aCompareCriteria fields are equal are put in the same sublist.
    for i := 0 to aSteps.Count - 1 do
    begin
        xStep := aSteps[i];
        // if the current step and the previous step do not match in the aCompareCritera, set previous to nil
        if (xPreviousStep <> nil) and (TStepComparer.StepCompare(xStep, xPreviousStep, aCompareCriteria)
            <> 0) then
        begin
            xPreviousStep := nil;
        end;
        // Create a new sublist and add it to the result
        if (xPreviousStep = nil) then
        begin
            xSubList := TObjectList<TBasicPipetteRunStep>.Create(false);
            result.Add(xSubList);
            xPreviousStep := xStep;
        end;
        // Add the current step to the current sublist
        xSubList.Add(xStep);
    end;
end;


end.
