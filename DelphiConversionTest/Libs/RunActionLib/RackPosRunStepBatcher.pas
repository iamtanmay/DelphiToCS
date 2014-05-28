{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  20.09.08 pk                                         TN4215    Initial Revision
  02.02.11 wl  IsStepCompatible                       TN5791   Prüfung auf gleichen Racknamen entfernt
  ----------------------------------------------------------------------------------------------------------------------- }

unit RackPosRunStepBatcher;


interface


uses
    RunStep,
    IntfPipDevice,
    AppTypes,
    RunStepBatcher,
    TipRunStepBatcher;

type

    TRackPosRunStepBatcher = class(TTipRunStepBatcher)
    protected
        function GetCompositeRunStep: TMultiRackPosRunStep;
        procedure DoBeforeAddStepToBatch(const aStep: TRunStep); override;
        procedure IsStepCompatible(const aStep: TRunStep); override;
        property CompositeRunStep: TMultiRackPosRunStep read GetCompositeRunStep;
    public
        constructor Create(const aCompositeRunStep: TMultiRackPosRunStep);
    end;


implementation


uses
    SysUtils;

{ TRackPosRunStepBatcher }

constructor TRackPosRunStepBatcher.Create(const aCompositeRunStep: TMultiRackPosRunStep);
begin
    inherited Create(aCompositeRunStep);
end;

procedure TRackPosRunStepBatcher.DoBeforeAddStepToBatch(const aStep: TRunStep);
var
    xStep: TMultiRackPosSubRunStep;
begin
    inherited;

    ASSERT(aStep is TMultiRackPosSubRunStep);
    xStep := aStep as TMultiRackPosSubRunStep;

    self.CompositeRunStep.SetRackNameAt(xStep.Tip, xStep.RackName);
    self.CompositeRunStep.SetPositionAt(xStep.Tip, xStep.Position);
end;

function TRackPosRunStepBatcher.GetCompositeRunStep: TMultiRackPosRunStep;
begin
    result := fCompositeRunStep as TMultiRackPosRunStep;
end;

procedure TRackPosRunStepBatcher.IsStepCompatible(const aStep: TRunStep);
begin
    inherited;
end;


end.
