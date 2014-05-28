{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no  improvement/change
  -------- --  ----------------------------------  -------- ----------------------------------------------------------
  03.09.13 wl                                      TN6241   initial revision
  03.09.13 wl  DoOptimizeSteps                     TN6241   viele neue Log-Zeilen
  21.10.13 wl                                      TN6276   stark überarbeitet
  28.11.13 wl  CheckRackPosition                   TN6277   Prüft Erreichbarkeit der Position
  28.11.13 wl  CheckRackPosition                   TN6313   Schreibt Position um bei MultiTipRacks
  ----------------------------------------------------------------------------------------------------------------------- }

unit StepOptimizer;


interface


uses
    SysUtils,
    Generics.Collections,
    StepOptimizerClasses,
    BasicPipetteRunStep,
    BasicPipetteTypes,
    IntfArmDevice,
    IntfPipDevice,
    Rack;

type
    EStepOptimizerException = class(Exception);

    TStepOptimizer = class
    private
        fOnCopyStep: TBasicPipetteRunStepCopyEvent;
        fSortDestByTips: boolean;
        class function IsPosReachable(aUsedArm: IArmDevice; aTipIndex: integer; aRack: TRack;
            aPos: integer): boolean;
    protected
        fPipDevice: IPipDevice;
        fDestSortCriteria: TArray<string>;
        fCheckSourcePos: boolean;
        fCheckDestPos: boolean;
        function CheckRackPosition(aCheck: boolean; aTipIndex: integer; aRackName: string;
            var vPos: integer): boolean;
        function CreateTipLists(): TObjectList<TTipStepList>;
        function CopyStep(const aStep: TBasicPipetteRunStep): TBasicPipetteRunStep;
        procedure WriteTipListsToFinalStepList(const aTipLists: TObjectList<TTipStepList>;
            aFinalSteps: TList<TBasicPipetteRunStep>);
    public
        constructor Create(aPipDevice: IPipDevice; const aDestSortCriteria: string;
            aSortDestByTips, aCheckSourcePos, aCheckDestPos: boolean;
            aOnCopyStep: TBasicPipetteRunStepCopyEvent);
        destructor Destroy(); override;

        procedure OptimizeStepsOfList(aDeviceSteps: TList<TBasicPipetteRunStep>;
            aFinalSteps: TList<TBasicPipetteRunStep>); virtual; abstract;
    end;


implementation


uses
    Math,
    StepOptimizerUtils,
    MotorStepCalculator,
    ArrayUtils,
    ObjModul,
    LayoutManager,
    LogManager;

{ TStepOptimizer }

constructor TStepOptimizer.Create(aPipDevice: IPipDevice; const aDestSortCriteria: string;
    aSortDestByTips, aCheckSourcePos, aCheckDestPos: boolean; aOnCopyStep: TBasicPipetteRunStepCopyEvent);
begin
    inherited Create();

    fPipDevice := aPipDevice;
    // Options
    fDestSortCriteria := TStepOptimizerUtils.StringToCriteria(aDestSortCriteria);
    fSortDestByTips := aSortDestByTips;
    fCheckDestPos := aCheckDestPos;
    fCheckSourcePos := aCheckSourcePos;

    fOnCopyStep := aOnCopyStep;
end;

destructor TStepOptimizer.Destroy();
begin
    // FreeAndNil(fDeviceStepLists);
    inherited;
end;

function TStepOptimizer.CreateTipLists(): TObjectList<TTipStepList>;
var
    i: integer;
    xTipSteps: TTipStepList;
begin
    result := TObjectList<TTipStepList>.Create(true);
    for i := 0 to fPipDevice.TipCount - 1 do
    begin
        xTipSteps := TTipStepList.Create(i + 1);
        result.Add(xTipSteps);
    end;
end;

function TStepOptimizer.CopyStep(const aStep: TBasicPipetteRunStep): TBasicPipetteRunStep;
begin
    EXIT(self.fOnCopyStep(aStep));
end;

procedure TStepOptimizer.WriteTipListsToFinalStepList(const aTipLists: TObjectList<TTipStepList>;
    aFinalSteps: TList<TBasicPipetteRunStep>);
var
    i: integer;
    xAtleastOneStepFound: boolean;
    xMultiPipStep: TMultiPipStep;
    xTipSteps: TTipStepList;
    xStep: TBasicPipetteRunStep;
    xTempDestStepList: TObjectList<TBasicPipetteRunStep>;
    xNumDisps: integer;
begin
    // for each step in each TipStepList, write the asp step and disp steps to the final run
    while true do
    begin
        xAtleastOneStepFound := false;

        // Write AspStep
        for i := 0 to aTipLists.Count - 1 do
        begin
            xTipSteps := aTipLists[i];
            if xTipSteps.Count = 0 then
                CONTINUE;
            xMultiPipStep := xTipSteps[0];
            if not xMultiPipStep.DoPip then
            begin
                CONTINUE;
            end;
            xAtleastOneStepFound := true;
            if Assigned(xMultiPipStep.AspStep) and
                ((xMultiPipStep.AspStep.SourceVol > 0) or (xMultiPipStep.AspStep.DilVol > 0)) then
                aFinalSteps.Add(xMultiPipStep.AspStep)
        end;

        // if no steps found then exit
        if not xAtleastOneStepFound then
            EXIT;

        // Write DispSteps
        xNumDisps := 0;
        xTempDestStepList := TObjectList<TBasicPipetteRunStep>.Create(false);
        while true do
        begin
            xAtleastOneStepFound := false;
            for i := 0 to aTipLists.Count - 1 do
            begin
                xTipSteps := aTipLists[i];
                if xTipSteps.Count = 0 then
                    CONTINUE;
                xMultiPipStep := xTipSteps[0];
                if not Assigned(xMultiPipStep.DispSteps) or (xMultiPipStep.DispSteps.Count <= xNumDisps) then
                    CONTINUE;
                xAtleastOneStepFound := true;
                xStep := xMultiPipStep.DispSteps[xNumDisps];
                xTempDestStepList.Add(xStep);
            end;
            if not xAtleastOneStepFound then
                BREAK;
            Inc(xNumDisps);
        end;

        if fSortDestByTips then
            xTempDestStepList.Sort(TStepComparer.Create(fDestSortCriteria));

        for i := 0 to xTempDestStepList.Count - 1 do
        begin
            xStep := xTempDestStepList[i];
            aFinalSteps.Add(xStep);
        end;

        xTempDestStepList.Destroy;

        // remove prev steps for each list
        for i := 0 to aTipLists.Count - 1 do
        begin
            xTipSteps := aTipLists[i];
            if xTipSteps.Count = 0 then
                CONTINUE;
            xTipSteps.Delete(0);
        end;

    end;
end;

class function TStepOptimizer.IsPosReachable(aUsedArm: IArmDevice; aTipIndex: integer; aRack: TRack;
    aPos: integer): boolean;
var
    xPipPosCheck: TPipPosCheck;
begin
    xPipPosCheck := TMotorStepCalculatorFactory.CreatePipPosCheck(aUsedArm);
    try
        xPipPosCheck.SetXYByRackPos(aTipIndex, aRack, aPos);
        result := xPipPosCheck.IsPosReachable(aTipIndex);
    finally
        FreeAndNil(xPipPosCheck);
    end;
end;

function TStepOptimizer.CheckRackPosition(aCheck: boolean; aTipIndex: integer; aRackName: string;
    var vPos: integer): boolean;
var
    xRack: TRack;
    xArm: IArmDevice;
begin
    xRack := TLayoutManager.Instance.CurrentLayout.FindRackByName(aRackName, true);

    // Spezialfall MultiTip-Rack: Change Position for Multi-Tip-Rack
    if (xRack.IsMultiTipRack()) then
    begin
        vPos := aTipIndex;
    end;

    if (aCheck) then
    begin
        xArm := gModules.FindArmByPipDevice(fPipDevice);
        EXIT(IsPosReachable(xArm, aTipIndex, xRack, vPos));
    end
    else
        EXIT(true); // kein Check gewünscht (Rack steht z.B. noch nicht an der richtigen Position)
end;


end.
