{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : The TStepOptimizer reads steps from the RunStep List, rearranges the steps,
  optimizes the list of steps by merging some steps, and writes the steps back
  to a Result Step List.
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no improvement/change
  -------- --  ------------------------------------- -------- --------------------------------------------------------------------
  04.08.10 pk                                        TN4996   Code from MultiPip.dll
  23.04.10 pk  FillTipListsOptimizedDestBasic        TN4996   New DestBasic Strategy
  11.06.10 pk  FillTipListsOptimizedDestBasic        TN5140   Also handles the case where number of steps is less than number of tips
  18.06.10 pk  FillTipListsOptimizedDestBasic        TN5154   Avoid division by zero: set TipRhythm to TipCount
  23.06.10 pk  FillTipListsOptimizedDestBasic        TN5164   correct organization of tips when num of steplists = num tips
  25.06.10 pk  DoOptimizeDilSteps                    TN5170   multipip of system liquid steps corrected
  12.08.10 wl                                        TN5227   an Änderungen in TipSystem angepasst
  12.08.10 wl  GetPipDeviceTipMaxAspVolume           TN5228   berücksichtigt jetzt auch Ch1PumpNumber
  25.08.10 wl  fBasicSortCriteria                    TN5244   Die Reihenfolge 'LIQPARAM;PIPDEVICE;USEDTIPS' kann jetzt von außen verändert werden
  06.04.11 wl                                        TN5501   benutzt TMultiPipLiqHCacheElement nicht mehr
  14.04.11 wl  CopyPipetteStep                       TN5501   TLiqHCacheElement ist kein TStreamable und muss deshalb extra kopiert werden
  18.05.11 ts  WriteTipListsToFinalStepList          TN5584   new: fSortDestByTips - Destination kann nach Tips sortiert werden (Abgabe komplett Tip 1,dann 2...)
  19.07.11 wl  CreateMultiPipStepFromSteps           TN5630   DTransAirRetake boolean statt double
  27.09.11 wl                                        TN5698   verwendet TLiqHandlingData für TPipetteRunStep
  12.12.11 wl                                        TN5764   uses PipetteRunStep entfernt, war zu speziell
  12.12.11 wl  fOnCreateStep,fOnCopyStep,fOnCopyPipetteStep  TN5764   als Events, werden von PipetteRunStep oder PipetteListReworkRunStep überschrieben
  25.04.12 wl                                        TN5878   uses geändert
  11.04.13 wl  GetColumnForPos                       TN6128   ersetzt Methoden, mit denen TRackInfo erzeugt, aber nie zerstört wurde
  03.09.13 wl  FillTipListsOptimizedDestBasic        TN6240   Bugfix: Schritte für Tip 1 wurden übersprungen
  03.09.13 wl                                        TN6241   umstrukturiert
  21.10.13 wl                                        TN6276   stark überarbeitet
  28.11.13 wl                                        TN6277   neu: CheckSource & CheckDest
  ----------------------------------------------------------------------------------------------------------------------- }

unit StepOptimizerManager;


interface


uses
    Generics.Collections,
    AppTypes,
    RunStep,
    IntfPipDevice,
    StepOptimizer,
    StepOptimizerClasses,
    BasicPipetteRunStep,
    BasicPipetteTypes;

type
    TCreateDestStepOptimizerEvent = function(aPipDevice: IPipDevice): TStepOptimizer of object;

    TStepOptimizerManager = class
    private
        fOptions: TStepOptimizerOptions;
        fOnCopyStep: TBasicPipetteRunStepCopyEvent;
        function CreateDestStepOptimizer(aPipDevice: IPipDevice): TStepOptimizer;
        function CreateDiluentStepOptimizer(aPipDevice: IPipDevice): TStepOptimizer;
        procedure OptimizeSteps(aOnCreate: TCreateDestStepOptimizerEvent;
            aInitialSteps, aFinalSteps: TList<TBasicPipetteRunStep>; const aLogText: string);
        function CopyPipetteStep(const aStep: TBasicPipetteRunStep): TBasicPipetteRunStep;
        procedure WriteFinalStepsToRun(const aRunSteps: TRunStepList;
            aFinalSteps: TList<TBasicPipetteRunStep>);
        class procedure ExtractStepListsForDilution(aSteps, aDilSteps: TList<TBasicPipetteRunStep>);
        function CreateInitialStepList(const aRunSteps: TRunStepList): TList<TBasicPipetteRunStep>;
        procedure OptimizeCopiedSteps(aInitialSteps, aFinalSteps: TList<TBasicPipetteRunStep>);
        procedure OptimizeRun(const aRunSteps: TRunStepList);
    public
        constructor Create(aOptions: TStepOptimizerOptions; aOnCopyStep: TBasicPipetteRunStepCopyEvent);
        destructor Destroy(); override;

        class procedure UseStepOptimizer(const aRunSteps: TRunStepList; aOptions: TStepOptimizerOptions;
            aOnCopyStep: TBasicPipetteRunStepCopyEvent); static;
    end;


implementation


uses
    SysUtils,
    Variants,
    PipDeviceManager,
    StepOptimizerDest,
    StepOptimizerDiluent,
    StepOptimizerUtils,
    Rack,
    TypeMapTranslator,
    LogManager,
    TipMapUtils,
    TipSystem;

{ TStepOptimizerManager }

constructor TStepOptimizerManager.Create(aOptions: TStepOptimizerOptions;
    aOnCopyStep: TBasicPipetteRunStepCopyEvent);
begin
    inherited Create;

    fOptions := aOptions;
    fOnCopyStep := aOnCopyStep;
end;

destructor TStepOptimizerManager.Destroy;
begin
    inherited;
end;

procedure TStepOptimizerManager.OptimizeSteps(aOnCreate: TCreateDestStepOptimizerEvent;
    aInitialSteps, aFinalSteps: TList<TBasicPipetteRunStep>; const aLogText: string);
var
    xBasicSortCriteria: TArray<string>;
    i: integer;
    xDeviceSteps: TList<TBasicPipetteRunStep>;
    xPipDeviceName: string;
    xPipDevice: IPipDevice;
    xDeviceStepLists: TListOfStepLists;
    xOptimizer: TStepOptimizer;
begin
    if aInitialSteps.Count = 0 then
        EXIT;

    xBasicSortCriteria := TStepOptimizerUtils.StringToCriteria(fOptions.BasicSortCriteria);
    xDeviceStepLists := TStepOptimizerUtils.CreateStepListsForCriteria(aInitialSteps, xBasicSortCriteria,
        xBasicSortCriteria);
    try
        for i := 0 to xDeviceStepLists.Count - 1 do
        begin
            xDeviceSteps := xDeviceStepLists[i];
            if xDeviceSteps.Count = 0 then
                EXIT;

            // Pip Device muss für alle Schritte gleich sein, kann also aus dem ersten Schritt gelesen werden
            xPipDeviceName := (xDeviceSteps[0] as TBasicPipetteRunStep).PipDeviceName;
            xPipDevice := gPipDeviceManager.FindPipDevice_ByName(xPipDeviceName);

            TLogManager.Instance.Log(aLogText + ', ' + xPipDeviceName + ' - BEGIN', false);
            xOptimizer := aOnCreate(xPipDevice);
            try
                xOptimizer.OptimizeStepsOfList(xDeviceSteps, aFinalSteps);
            finally
                FreeAndNil(xOptimizer);
            end;
            TLogManager.Instance.Log(aLogText + ', ' + xPipDeviceName + ' - END', false);
        end;
    finally
        // FreeAndNil( xDeviceStepLists);
    end;
end;

function TStepOptimizerManager.CreateDestStepOptimizer(aPipDevice: IPipDevice): TStepOptimizer;
begin
    EXIT(TDestStepOptimizer.Create(aPipDevice, fOptions, fOnCopyStep));
end;

function TStepOptimizerManager.CreateDiluentStepOptimizer(aPipDevice: IPipDevice): TStepOptimizer;
begin
    EXIT(TDiluentStepOptimizer.Create(aPipDevice, fOptions.SortDestByTips, fOptions.CheckSourcePos,
        fOptions.CheckDestPos, fOnCopyStep));
end;

function TStepOptimizerManager.CopyPipetteStep(const aStep: TBasicPipetteRunStep): TBasicPipetteRunStep;
begin
    EXIT(fOnCopyStep(aStep));
end;

function TStepOptimizerManager.CreateInitialStepList(const aRunSteps: TRunStepList)
    : TList<TBasicPipetteRunStep>;
var
    xRunStep: TBasicPipetteRunStep;
    xStep: TBasicPipetteRunStep;
    x: integer;
begin
    result := TObjectList<TBasicPipetteRunStep>.Create(true);

    for x := 0 to aRunSteps.Count - 1 do
    begin
        xRunStep := aRunSteps[x] as TBasicPipetteRunStep;
        xStep := CopyPipetteStep(xRunStep);
        result.Add(xStep);
    end;
end;

class procedure TStepOptimizerManager.ExtractStepListsForDilution(aSteps,
    aDilSteps: TList<TBasicPipetteRunStep>);
var
    xStep: TBasicPipetteRunStep;
    xObject: TBasicPipetteRunStep;
    i: integer;
begin
    // Remove the steps with DilVol > 0 from aSteps and add them to the list being returned
    i := 0;
    while (aSteps.Count > 0) and (i < aSteps.Count) do
    begin
        xStep := aSteps[i];
        if xStep.DilVol > 0 then
        begin
            xObject := aSteps[i];
            aSteps.Extract(xObject);
            aDilSteps.Add(xStep);
        end
        else
            Inc(i);
    end;
end;

procedure TStepOptimizerManager.WriteFinalStepsToRun(const aRunSteps: TRunStepList;
    aFinalSteps: TList<TBasicPipetteRunStep>);
var
    xRunStep: TBasicPipetteRunStep;
    xStep: TBasicPipetteRunStep;
    i: integer;
begin
    // remove Run Steps
    aRunSteps.Clear();

    // fill run step list with changed steps
    for i := 0 to aFinalSteps.Count - 1 do
    begin
        xStep := aFinalSteps[i];
        xRunStep := CopyPipetteStep(xStep);
        // the tip number in the rest of the software is a 0-offset Tipindex
        xRunStep.Tip := xRunStep.Tip - 1;
        aRunSteps.AddStep(xRunStep);
    end;
end;

procedure TStepOptimizerManager.OptimizeCopiedSteps(aInitialSteps, aFinalSteps: TList<TBasicPipetteRunStep>);
var
    xDilSteps: TList<TBasicPipetteRunStep>;
begin
    // Optimize xSteps and write steps to fFinalSteps
    xDilSteps := TObjectList<TBasicPipetteRunStep>.Create(true);
    try
        // extract steps with sysliq
        ExtractStepListsForDilution(aInitialSteps, xDilSteps);

        // do normal steps
        OptimizeSteps(CreateDestStepOptimizer, aInitialSteps, aFinalSteps, 'Sample Step Optimization');

        // do sysliq steps
        OptimizeSteps(CreateDiluentStepOptimizer, xDilSteps, aFinalSteps, 'Diluent Step Optimization');
    finally
        FreeAndNil(xDilSteps);
    end;
end;

procedure TStepOptimizerManager.OptimizeRun(const aRunSteps: TRunStepList);
var
    xInitialSteps, xFinalSteps: TList<TBasicPipetteRunStep>;
begin
    xFinalSteps := TObjectList<TBasicPipetteRunStep>.Create(true);
    try
        // copy the steps from the run
        xInitialSteps := CreateInitialStepList(aRunSteps);
        try
            //
            OptimizeCopiedSteps(xInitialSteps, xFinalSteps)
        finally
            FreeAndNil(xInitialSteps);
        end;

        // write fFinalSteps to run
        WriteFinalStepsToRun(aRunSteps, xFinalSteps);
    finally
        FreeAndNil(xFinalSteps);
    end;
end;

class procedure TStepOptimizerManager.UseStepOptimizer(const aRunSteps: TRunStepList;
    aOptions: TStepOptimizerOptions; aOnCopyStep: TBasicPipetteRunStepCopyEvent);
var
    x: integer;
    xOptimizer: TStepOptimizerManager;
    xLogText: string;
begin
    if aRunSteps.Count = 0 then
        EXIT;

    // Ist bei allen Liquid Handling Parametern Multipipetting eingestellt?
    for x := 0 to aRunSteps.Count - 1 do
    begin
        if ((aRunSteps[x] as TBasicPipetteRunStep).LiqHRec.SampleAspMultipip = false) then
        begin
            TLogManager.Instance.Log((aRunSteps[x] as TBasicPipetteRunStep).LiqHRec.ParamName +
                ': No Multipipetting set! Using single pipetting.', true);
            EXIT;
        end;
    end;

    xLogText := 'Pipette Step Optimization';
    TLogManager.Instance.Log(xLogText + ' - BEGIN', false);

    xOptimizer := TStepOptimizerManager.Create(aOptions, aOnCopyStep);
    try
        xOptimizer.OptimizeRun(aRunSteps);
    finally
        xOptimizer.Free;
    end;

    TLogManager.Instance.Log(xLogText + ' - END', false);
end;


end.
