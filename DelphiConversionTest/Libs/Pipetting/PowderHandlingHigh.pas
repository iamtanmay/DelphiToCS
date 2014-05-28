{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Powder Handling
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  12.09.09 wl  gmAspiratePowder,gmDispensePowder  TN4740  von PowderHandling hierher
  19.10.11 wl                               TN5723   Aspirate,DispensePowder1 werden mit ColorChar aufgerufen
  28.09.11 wl                               TN5725   gmCalculatePipSteps jetzt ohne Parameter
  02.02.11 wl  alle Funktionen              TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  -------------------------------------------------------------------------------------------------- }

unit PowderHandlingHigh;


interface


uses
    MethodStepSettingRunStart,
    RackTypes,
    AppTypes,
    GeneralTypes,
    IntfArmDevice;

procedure gmAspiratePowder(aUsedArm: IArmDevice; aArmTips: TIPMAP; const aRP: TArray<TXRackPosition>;
    var vVarixMotorSteps: integer; aRunRec: TAspirateEvRunRec);
procedure gmDispensePowder(aUsedArm: IArmDevice; aArmTips: TIPMAP; const aRP: TArray<TXRackPosition>;
    aRunRec: TDispenseEvRunRec);


implementation


uses
    Windows,
    SysUtils,
    Classes,
    Math,
    Rack,
    IntfPipDevice,
    SamGlobe,
    EventManager,
    CommonTypes,
    LogManager,
    ErrorManager,
    RunFlow,
    PowderHandling,
    ObjModul,
    ArrayUtils,
    SamHigh;

type
    TPowderStepData = record
        Vol: TDoubleArray;
        Delay: integer;
        DisableZErr: boolean;
        SingleRetract: boolean;
        SubMerge_mm: TPosMM;
        SwitchPlace: integer;
        SwitchModule: string;
        PerformingSteps: double; // fraglich, ob das im Powder Handling Sinn ergibt
        PerformDelay: integer; // "
        ZInsertSpeed, ZInsertMoveType, ZRetractSpeed: integer; // nur bei Aspirate (von Position zu Wiper)
        EmptyVarRedi: boolean; // nur Disp
        DelayBetweenAsp: integer; // nur Asp
        XYShifting: boolean;
        SingleTip: boolean;
    end;

    TRediShakerStruct = record
        ShakeTime: Integer;
        Rack: TRack;
        TipMap: TIPMAP;
        Positions: TIntArray;
    end;

var
    gRediShakerStruct: TRediShakerStruct; // formerly in TExtendedLiqThread

function gmInitPowderStepData(aPipDevice: IPipDevice): TPowderStepData;
begin
    result.Vol := aPipDevice.GetNullDoubleArray;
    result.Delay := 0;
    result.DisableZErr := false;
    result.SingleRetract := false;
    result.SubMerge_mm := 0;
    result.SwitchPlace := 0;
    result.SwitchModule := '';
    result.PerformingSteps := 0;
    result.PerformDelay := 0;
    result.ZInsertSpeed := 0;
    result.ZInsertMoveType := 0;
    result.ZRetractSpeed := 0;
    result.EmptyVarRedi := false;
    result.SingleTip := false;
end;

// --------------------------------------------------------------------------------------------------
procedure gmRediShakerOn(aTipMap: TIPMAP; const aPos: TIntArray; aShakeTime: Integer; aRack: TRack);
// --------------------------------------------------------------------------------------------------
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;
    if not Assigned(aRack) or (aShakeTime <= 0) then
    begin
        gRediShakerStruct.ShakeTime := 0;
        EXIT;
    end;

    gLogManager.Log('Powder Shaker On for Rack: ' + aRack.Name, false);
    TPowderHandling.SwitchRackDevice(aTipMap, aRack, aPos, { dptSwitchShaker, } true);
    gRediShakerStruct.ShakeTime := aShakeTime;
    gRediShakerStruct.TipMap := aTipMap;
    gRediShakerStruct.Positions := Copy(aPos);
    gRediShakerStruct.Rack := aRack;
end;

// --------------------------------------------------------------------------------------------------
procedure gmRediShakerOff;
// --------------------------------------------------------------------------------------------------
begin
    if not(gShakeTimeAdjustable) then
        EXIT;
    if (gRediShakerStruct.ShakeTime <= 0) then
        EXIT;

    gRunFlow.AppSleep(gRediShakerStruct.ShakeTime);
    gLogManager.Log('Powder Shaker Off for Rack: ' + gRediShakerStruct.Rack.Name, false);
    TPowderHandling.SwitchRackDevice(gRediShakerStruct.TipMap, gRediShakerStruct.Rack,
        gRediShakerStruct.Positions, { dptSwitchShaker, } false);
end;

// --------------------------------------------------------------------------------------------------
procedure gmAspiratePowder(aUsedArm: IArmDevice; aArmTips: TIPMAP; const aRP: TArray<TXRackPosition>;
    var vVarixMotorSteps: integer; aRunRec: TAspirateEvRunRec);
// --------------------------------------------------------------------------------------------------
var
    cnt, x, xNoOfAspirations, xCurrentDelay: integer;
    xData: TPowderStepData;
    xShakeDelay: Integer;
    xNoCalculation: boolean;
    xEvBeforeAsp, xEvBeforePickLq, xEvAfterPickLq, xEvAfterAsp: TRunstCall;
    xPipDevice: IPipDevice;
begin
    xPipDevice := aUsedArm.PipDevice;
    xNoCalculation := false;
    xShakeDelay := 0;
    xData := gmInitPowderStepData(aUsedArm.PipDevice);
    xNoOfAspirations := 1;
    for cnt := 0 to xPipDevice.TipCount - 1 do
    begin
        if (aArmTips and (1 shl cnt)) > 0 then
        begin

            if (xPipDevice.Tips[cnt].St_LhPtr.SampleAspErrFlag <> 0) then
                xData.DisableZErr := true;
            xData.Delay :=
                MaxIntValue([xData.Delay, Round(xPipDevice.Tips[cnt].St_LhPtr.SampleAspDelay * 1000)]);
            xData.SubMerge_mm := xPipDevice.Tips[cnt].St_LhPtr.SampleAspSubmerge;
            xData.Vol[cnt] := xPipDevice.Tips[cnt].St_SVol;
            if (xPipDevice.Tips[cnt].St_LhPtr.SampleAspTipSingleRetract) then
                xData.SingleRetract := true;
            xData.SwitchModule := xPipDevice.Tips[cnt].St_LhPtr.AspSwitchModule;
            xData.SwitchPlace := xPipDevice.Tips[cnt].St_LhPtr.AspSwitchPos;
            xShakeDelay :=
                MaxIntValue([xShakeDelay, Round(xPipDevice.Tips[cnt].St_LhPtr.SysAirAspDelay * 1000)]);
            xData.ZInsertSpeed := xPipDevice.Tips[cnt].St_LhPtr.SampleAspScanSpeed;
            xData.ZInsertMoveType := xPipDevice.Tips[cnt].St_LhPtr.SampleAspInsertMoveType;
            xData.ZRetractSpeed := xPipDevice.Tips[cnt].St_LhPtr.SampleAspRetrSpeed;
            if ((xPipDevice.Tips[cnt].St_LhPtr.SampleAspLiqDet and INT_LQMODES_NO_CALCULATION) <> 0) then
                xNoCalculation := true;
            xNoOfAspirations :=
                MaxIntValue([xNoOfAspirations, xPipDevice.Tips[cnt].St_LhPtr.SampleAspSpitBackCount]);
            xData.DelayBetweenAsp :=
                MaxIntValue([xData.Delay, Round(xPipDevice.Tips[cnt].St_LhPtr.SampleAspCh2WashDelay * 1000)]);
            xData.XYShifting := xPipDevice.Tips[cnt].St_LhPtr.SampleAspXYShifting;
            xData.SingleTip :=
                ((xPipDevice.Tips[cnt].St_LhPtr.SampleAspLiqDet and INT_LQMODES_LQ_SINGLE_TIP) <> 0);
        end;
    end;
    // ------------------------------------------------------------------------------ Starte Aufnahme
    gLogManager.Log('Aspirate Powder - ' + TXRackPositionUtils.RackPositionsToBracketText(aRP) + ' Vol-' +
        TArrayUtils.ArrayToBracketText(xData.Vol) + ' Delay[' + IntToStr(xData.Delay) + ']', true);
    // ----------------------------------------------- Im Falle von variablen Volumen Motor ansteuern
    TPowderHandling.SetVarRediVolumes(xPipDevice, aArmTips, xData.Vol, vVarixMotorSteps);

    xEvBeforeAsp := TEventManager.Instance.CreateEventIfNecessary(aRunRec.BeforeAsp);
    xEvBeforePickLq := TEventManager.Instance.CreateEventIfNecessary(aRunRec.BeforePickLq);
    xEvAfterPickLq := TEventManager.Instance.CreateEventIfNecessary(aRunRec.AfterPickLq);
    xEvAfterAsp := TEventManager.Instance.CreateEventIfNecessary(aRunRec.AfterAsp);

    if Assigned(xEvBeforeAsp) then
        xEvBeforeAsp.Execute('before aspirating powder');

    for x := 0 to xNoOfAspirations - 1 do
    begin

        if (x < xNoOfAspirations - 1) then // "Delay between" oder "Delay after"
            xCurrentDelay := xData.DelayBetweenAsp
        else
            xCurrentDelay := xData.Delay;

        TPowderHandling.AspiratePowder1(aUsedArm, aArmTips, aRP, xData.Vol, xCurrentDelay, xData.SubMerge_mm,
            xData.SwitchModule, gmParseSwitchPlace(xData.SwitchPlace), xData.SingleRetract, xEvBeforeAsp,
            xEvBeforePickLq, xEvAfterPickLq, xEvAfterAsp, xData.ZInsertSpeed, xData.ZInsertMoveType,
            xData.ZRetractSpeed, xNoCalculation, xData.DisableZErr, xData.XYShifting,
            (x = xNoOfAspirations - 1), xData.SingleTip);
    end;

    if Assigned(xEvAfterAsp) then
        xEvAfterAsp.Execute('after aspirating powder');

    FreeAndNil(xEvBeforeAsp);
    FreeAndNil(xEvBeforePickLq);
    FreeAndNil(xEvAfterPickLq);
    FreeAndNil(xEvAfterAsp);

    gmRediShakerOn(aArmTips, TXRackPositionUtils.ExtractPositionsFromArray(aRP), xShakeDelay,
        TXRackPositionUtils.ExtractSingleRackFromArray(aRP));
end;

// --------------------------------------------------------------------------------------------------
procedure gmDispensePowder(aUsedArm: IArmDevice; aArmTips: TIPMAP; const aRP: TArray<TXRackPosition>;
    aRunRec: TDispenseEvRunRec);
// --------------------------------------------------------------------------------------------------
var
    cnt: integer;
    xData: TPowderStepData;
    xEvBeforeDisp, xEvBeforeDispLq, xEvAfterDispLq, xEvAfterDisp: TRunstCall;
    xPipDevice: IPipDevice;
begin
    xPipDevice := aUsedArm.PipDevice;

    xData := gmInitPowderStepData(xPipDevice);
    for cnt := 0 to xPipDevice.TipCount - 1 do
    begin
        if (aArmTips and (1 shl cnt)) > 0 then
        begin

            if (xPipDevice.Tips[cnt].St_LhPtr.DispErrFlag > 0) then
                xData.DisableZErr := true;
            xData.Delay := MaxIntValue([xData.Delay, Round(xPipDevice.Tips[cnt].St_LhPtr.DispDelay * 1000)]);
            xData.Submerge_mm := xPipDevice.Tips[cnt].St_LhPtr.DispSubmerge;
            xData.PerFormingSteps := xPipDevice.Tips[cnt].St_LhPtr.DispStepVolume;
            xData.PerFormDelay := round(xPipDevice.Tips[cnt].St_LhPtr.DispStepDelay * 1000);
            xData.Vol[cnt] := xPipDevice.Tips[cnt].St_DVol;
            if (xPipDevice.Tips[cnt].St_LhPtr.DispTipSingleRetract) then
                xData.SingleRetract := true;

            xData.SwitchModule := xPipDevice.Tips[cnt].St_LhPtr.DspSwitchModule;
            xData.SwitchPlace := xPipDevice.Tips[cnt].St_LhPtr.DspSwitchPos;
            xData.ZInsertSpeed := xPipDevice.Tips[cnt].St_LhPtr.DispScanSpeed;
            xData.ZInsertMoveType := xPipDevice.Tips[cnt].St_LhPtr.DispInsertMoveType;
            xData.ZRetractSpeed := xPipDevice.Tips[cnt].St_LhPtr.DispRetrSpeed;
            xData.EmptyVarRedi := xPipDevice.Tips[cnt].St_LhPtr.DispEmptyVarRedi;
            xData.XYShifting := xPipDevice.Tips[cnt].St_LhPtr.DispXYShifting;
            xData.SingleTip :=
                ((xPipDevice.Tips[cnt].St_LhPtr.DispLiqDet and INT_LQMODES_LQ_SINGLE_TIP) <> 0);
        end;
    end;

    gLogManager.Log('Dispense Powder - ' + TXRackPositionUtils.RackPositionsToBracketText(aRP) + ' Vol-' +
        TArrayUtils.ArrayToBracketText(xData.Vol) + ' Delay[' + IntToStr(xData.Delay) + ']', true);

    gmRediShakerOff;
    xEvBeforeDisp := TEventManager.Instance.CreateEventIfNecessary(aRunRec.BeforeDispense);
    xEvBeforeDispLq := TEventManager.Instance.CreateEventIfNecessary(aRunRec.BeforeDispLq);
    xEvAfterDispLq := TEventManager.Instance.CreateEventIfNecessary(aRunRec.AfterDispLq);
    xEvAfterDisp := TEventManager.Instance.CreateEventIfNecessary(aRunRec.AfterDispense);
    TPowderHandling.DispensePowder1(aUsedArm, aArmTips, aRP, xData.Vol, xData.Delay, xData.SubMerge_mm,
        xData.PerformingSteps, xData.PerFormDelay, xData.SwitchModule, gmParseSwitchPlace(xData.SwitchPlace),
        xData.SingleRetract, xEvBeforeDisp, xEvBeforeDispLq, xEvAfterDispLq, xEvAfterDisp, xData.ZInsertSpeed,
        xData.ZInsertMoveType, xData.DisableZErr, xData.EmptyVarRedi, xData.XYShifting, xData.SingleTip);
    FreeAndNil(xEvBeforeDisp);
    FreeAndNil(xEvBeforeDispLq);
    FreeAndNil(xEvAfterDispLq);
    FreeAndNil(xEvAfterDisp);
end;


end.
