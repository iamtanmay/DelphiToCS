{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no  improvement/change
  -------- --  ----------------------------------  -------- ----------------------------------------------------------
  24.10.13 wl                                      TN6276   initial revision
  28.11.13 wl                                      TN6277   neu: CheckSource & CheckDest
  ----------------------------------------------------------------------------------------------------------------------- }

unit StepOptimizerDiluent;


interface


uses
    Generics.Collections,
    IntfPipDevice,
    BasicPipetteRunStep,
    StepOptimizer,
    StepOptimizerClasses;

type
    TDiluentStepOptimizer = class(TStepOptimizer)
    private
        class function GetColumnForPos(const aRackName: string; aPos: integer): integer;
        class function GetColForStep(aMultiPipStep: TMultiPipStep): integer;
        class function CreateDummyMultiPipStep(): TMultiPipStep;
        function CreateMultiPipSteps(aSteps: TList<TBasicPipetteRunStep>): TObjectList<TMultiPipStep>;
        procedure FillTipListsOptimizedPos(aActiveTipLists: TList<TTipStepList>;
            aSteps: TList<TBasicPipetteRunStep>);
    public
        constructor Create(aPipDevice: IPipDevice; aSortDestByTips, aCheckSourcePos, aCheckDestPos: boolean;
            aOnCopyStep: TBasicPipetteRunStepCopyEvent);

        procedure OptimizeStepsOfList(aDeviceSteps: TList<TBasicPipetteRunStep>;
            aFinalSteps: TList<TBasicPipetteRunStep>); override;
    end;


implementation


uses
    SysUtils,
    StepOptimizerUtils,
    LayoutManager,
    Rack;

{ TDiluentStepOptimizer }

constructor TDiluentStepOptimizer.Create(aPipDevice: IPipDevice;
    aSortDestByTips, aCheckSourcePos, aCheckDestPos: boolean; aOnCopyStep: TBasicPipetteRunStepCopyEvent);
begin
    inherited Create(aPipDevice, 'DESTRACK;DESTPOS', aSortDestByTips, aCheckSourcePos, aCheckDestPos,
        aOnCopyStep);
end;

class function TDiluentStepOptimizer.GetColumnForPos(const aRackName: string; aPos: integer): integer;
var
    xRack: TRack;
begin
    xRack := TLayoutManager.Instance.CurrentLayout.FindRackByName(aRackName, true);

    result := (aPos - 1) div xRack.Structure.Rows;
end;

class function TDiluentStepOptimizer.CreateDummyMultiPipStep(): TMultiPipStep;
begin
    result := TMultiPipStep.Create();
    result.DoPip := false;
end;

class function TDiluentStepOptimizer.GetColForStep(aMultiPipStep: TMultiPipStep): integer;
var
    xDestRackName: string;
    xDestRackPos: integer;
begin
    xDestRackName := aMultiPipStep.AspStep.DRack;
    xDestRackPos := aMultiPipStep.AspStep.DPos;
    result := GetColumnForPos(xDestRackName, xDestRackPos);
end;

// Create a list of multipip steps from a list of steps
function TDiluentStepOptimizer.CreateMultiPipSteps(aSteps: TList<TBasicPipetteRunStep>)
    : TObjectList<TMultiPipStep>;
var
    xStep: TBasicPipetteRunStep;
    xMultiPipStep: TMultiPipStep;
    x: integer;
begin
    result := TObjectList<TMultiPipStep>.Create(true);
    for x := 0 to aSteps.Count - 1 do
    begin
        xStep := aSteps[x];
        xMultiPipStep := TMultiPipStep.Create();
        xMultiPipStep.AspStep := self.CopyStep(xStep);
        result.Add(xMultiPipStep);
    end;
end;

procedure TDiluentStepOptimizer.FillTipListsOptimizedPos(aActiveTipLists: TList<TTipStepList>;
    aSteps: TList<TBasicPipetteRunStep>);
var
    xTipList: TTipStepList;
    xMultiPipStep: TMultiPipStep;
    xMultiPipSteps: TObjectList<TMultiPipStep>;
    xActiveTipListsIndex: integer;
    xPrevColumn: integer;
    xCurColumn: integer;
    xPrevRackName: string;
    xCurRackName: string;
    xGotoFirstTip: boolean;
begin
    xMultiPipSteps := CreateMultiPipSteps(aSteps);
    try
        xActiveTipListsIndex := -1;
        xPrevColumn := -1;
        while xMultiPipSteps.Count > 0 do
        begin
            xMultiPipStep := xMultiPipSteps[0];
            xCurColumn := GetColForStep(xMultiPipStep);
            xCurRackName := xMultiPipStep.AspStep.DRack;

            xGotoFirstTip := (xPrevRackName <> '') and (xPrevRackName <> xCurRackName);
            xGotoFirstTip := xGotoFirstTip or (xPrevColumn > -1) and (xCurColumn <> xPrevColumn);

            xTipList := nil;
            if xGotoFirstTip then
            begin
                while true do
                begin
                    xActiveTipListsIndex := (xActiveTipListsIndex + 1) mod aActiveTipLists.Count;
                    xTipList := aActiveTipLists[xActiveTipListsIndex];
                    if (xActiveTipListsIndex = 0) then
                        BREAK;
                    xTipList.AddStep(self.CreateDummyMultiPipStep());
                end;
            end
            else
            begin
                xActiveTipListsIndex := (xActiveTipListsIndex + 1) mod aActiveTipLists.Count;
                xTipList := aActiveTipLists[xActiveTipListsIndex];
            end;
            ASSERT(Assigned(xTipList));
            xTipList.AddStep(xMultiPipStep);
            xMultiPipSteps.Delete(0);

            xPrevColumn := xCurColumn;
            xPrevRackName := xCurRackName;
        end;
    finally
        xMultiPipSteps.Free;
    end;
end;

procedure TDiluentStepOptimizer.OptimizeStepsOfList(aDeviceSteps: TList<TBasicPipetteRunStep>;
    aFinalSteps: TList<TBasicPipetteRunStep>);
var
    xTipStepLists: TObjectList<TTipStepList>;
begin
    aDeviceSteps.Sort(TStepComparer.Create(fDestSortCriteria));

    xTipStepLists := self.CreateTipLists();
    try
        // sort all steps by destination
        FillTipListsOptimizedPos(xTipStepLists, aDeviceSteps);
        WriteTipListsToFinalStepList(xTipStepLists, aFinalSteps);
    finally
        xTipStepLists.Free;
    end;
end;


end.
