{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  20.09.08 pk                                         TN4215    Initial Revision
  04.02.10 pk                                         TN4972    Changes for Restart
  14.08.13 wl  AddStepsToBatch                        TN6218   CurrentBatchIndex wird an DoBatchEnded() gegeben und hochgezählt
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunStepBatcher;


interface


uses
    RunStep;

type

    TRunStepBatcher = class
    protected
        fCompositeRunStep: TCompositeRunstep;
        procedure RaiseBreak(const aReason: string);
        procedure CheckStepTypeCompatible(const aStep: TRunStep);
        procedure AddStepToBatch(const aStep: TRunStep);
        procedure DoBeforeAddStepToBatch(const aStep: TRunStep); virtual;
        procedure IsStepCompatible(const aStep: TRunStep); virtual;
        procedure DoBatchEnded(const aCurrentBatchIndex: integer); virtual;
        procedure DoErrorOccured(const aMessage: string; const aStep: TRunStep); virtual;
        procedure BatchEnded(const aCurrentBatchIndex: integer);
        procedure ErrorOccured(const aMessage: string; const aStep: TRunStep);
    public
        constructor Create(const aCompositeRunStep: TCompositeRunstep);
        procedure AddStepsToBatch(const aStepListIterator: TRunStepListIterator);
    end;


implementation


uses
    SysUtils,
    ErrorManager;

type
    ERunStepBatchBreak = class(Exception);

constructor TRunStepBatcher.Create(const aCompositeRunStep: TCompositeRunstep);
begin
    inherited Create();
    fCompositeRunStep := aCompositeRunStep;
end;

procedure TRunStepBatcher.RaiseBreak(const aReason: string);
begin
    raise ERunStepBatchBreak.Create(aReason);
end;

procedure TRunStepBatcher.CheckStepTypeCompatible(const aStep: TRunStep);
var
    xFirstStep: TRunStep;
begin
    if fCompositeRunStep.Count = 0 then
        EXIT;
    xFirstStep := fCompositeRunStep[0];
    if not(xFirstStep.ClassType = aStep.ClassType) then
        RaiseBreak('Different Step Type');
end;

procedure TRunStepBatcher.DoBeforeAddStepToBatch(const aStep: TRunStep);
begin
end;

procedure TRunStepBatcher.DoBatchEnded;
begin
end;

procedure TRunStepBatcher.AddStepToBatch(const aStep: TRunStep);
begin
    DoBeforeAddStepToBatch(aStep);
    fCompositeRunStep.AddStep(aStep);
end;

procedure TRunStepBatcher.IsStepCompatible(const aStep: TRunStep);
begin
    CheckStepTypeCompatible(aStep);
end;

procedure TRunStepBatcher.BatchEnded(const aCurrentBatchIndex: integer);
begin
    DoBatchEnded(aCurrentBatchIndex);
end;

procedure TRunStepBatcher.DoErrorOccured(const aMessage: string; const aStep: TRunStep);
begin
    raise Exception.CreateFmt('Batch Steps:  %s', [aMessage]);
end;

procedure TRunStepBatcher.ErrorOccured(const aMessage: string; const aStep: TRunStep);
begin
    DoErrorOccured(aMessage, aStep);
end;

procedure TRunStepBatcher.AddStepsToBatch(const aStepListIterator: TRunStepListIterator);
var
    xCurrentStep: TRunStep;
begin
    xCurrentStep := nil;
    try
        while true do
        begin
            if (gErrorManager.IsGlobalErr) then
            begin
                Exit;
            end;

            try
                // if this is the first record being read and it is invalid then Exception. Otherwise it means EOF so break
                if aStepListIterator.IsEOF then
                begin
                    RaiseBreak('Invalid Record');
                end;

                xCurrentStep := aStepListIterator.CurrentStep;
                IsStepCompatible(xCurrentStep);
            except
                on ERunStepBatchBreak do
                begin
                    if fCompositeRunStep.IsEmpty() then
                        raise; // RE-Raise the exception if no Steps were successfully found!
                    BREAK;
                end;
                else
                    raise;
            end;
            AddStepToBatch(xCurrentStep);
            aStepListIterator.MoveNext();
        end;

        BatchEnded(aStepListIterator.CurrentBatchIndex);
        aStepListIterator.CurrentBatchIndex := aStepListIterator.CurrentBatchIndex + 1;

    except
        on E: Exception do
        begin
            ErrorOccured(E.Message, xCurrentStep);
        end;
    end;
end;


end.
