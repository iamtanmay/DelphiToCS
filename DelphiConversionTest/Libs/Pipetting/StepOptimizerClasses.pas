{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : This unit contains various List classes which are used by the StepOptimizer unit
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  04.08.10 pk                               TN4996   Code from MultiPip.dll
  12.08.10 wl  TMultiPipLiqHCacheElement    TN5228   neu Ch1PumpNumber
  06.04.11 wl  TMultiPipLiqHCacheElement    TN5501   entfernt (nicht mehr notwendig)
  14.04.11 wl  TStep                        TN5501   TLiqHCacheElement ist Teil von TPipetteRunStep
  12.12.11 wl  TBasicMultiPipStep           TN5764   = TStep, ist nur noch abstrakte Klasse
  25.04.12 wl                               TN5878   uses geändert
  11.04.13 wl                               TN6128   aufgeräumt und modernisiert
  11.04.13 wl  TRackInfo                    TN6128   entfernt
  03.09.13 wl  TMultiPipStepUtils           TN6241   Logging-Funktionen für Multipip
  24.10.13 wl  TMultiPipStepUtils           TN6276   --> StepOptimizerUtils
  ----------------------------------------------------------------------------------------------------------------------- }

unit StepOptimizerClasses;


interface


uses
    Generics.Collections,
    BasicPipetteRunStep;

type
    TListOfStepLists = TObjectList<TObjectList<TBasicPipetteRunStep>>;

    TMultiPipStep = class
    strict private
        fDoPip: boolean;
        fAspStep: TBasicPipetteRunStep;
        fDispSteps: TObjectList<TBasicPipetteRunStep>;
    public
        constructor Create();
        destructor Destroy(); override;

        procedure SetTipForSteps(aTip: integer);
        property AspStep: TBasicPipetteRunStep read fAspStep write fAspStep;
        property DispSteps: TObjectList<TBasicPipetteRunStep>read fDispSteps;
        property DoPip: boolean read fDoPip write fDoPip;
    end;

    TTipStepList = class(TObjectList<TMultiPipStep>)
    strict private
        fTip: integer;
    public
        constructor Create(const aTip: integer);

        procedure AddStep(aStep: TMultiPipStep);
        property Tip: integer read fTip;
    end;


implementation


uses
    SysUtils,
    ArrayUtils;

{ TMultiPipStep }

constructor TMultiPipStep.Create();
begin
    inherited Create;
    fDoPip := true;
    fAspStep := nil;
    fDispSteps := TObjectList<TBasicPipetteRunStep>.Create(true);
end;

destructor TMultiPipStep.Destroy();
begin
    fAspStep.Free;
    fDispSteps.Free;
    inherited;
end;

procedure TMultiPipStep.SetTipForSteps(aTip: integer);
var
    i: integer;
begin
    fAspStep.Tip := aTip;
    if not Assigned(fDispSteps) then
        EXIT;
    for i := 0 to fDispSteps.Count - 1 do
    begin
        fDispSteps[i].Tip := aTip;
    end;
end;

{ TTipStepList }

constructor TTipStepList.Create(const aTip: integer);
begin
    inherited Create(false); // Achtung!! Ist das richtig?
    fTip := aTip;
end;

procedure TTipStepList.AddStep(aStep: TMultiPipStep);
begin
    if aStep.DoPip then
        aStep.SetTipForSteps(fTip);

    Add(aStep);
end;


end.
