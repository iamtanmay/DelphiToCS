{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  08.03.07 wl                               TN3620   uses geändert
  29.01.08 wl                               TN3980   uses geändert
  15.05.08 wl                               TN4100    TXYStepList property changed
  11.07.08 wl                               TN4164   gModules.FindByName statt gModules.FindSwitch
  12.09.09 wl                               TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  24.11.09 ts  gmDoTipPressure              TN4771   xXYSteps created in gmCalculatePipSteps will be freed
  28.09.11 wl                               TN5725   gmCalculatePipSteps jetzt ohne Parameter
  02.02.11 wl  alle Funktionen              TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  23.05.13 wl                               TN6153   geänderte Parameter bei MoveXY
  -------------------------------------------------------------------------------------------------- }

unit TipPressureHandling;


interface


uses
    AppTypes,
    IntfArmDevice,
    RackTypes;

procedure gmTipPressure(aUsedArm: IArmDevice; aTips: TIPMAP; const aRP: TArray<TXRackPosition>);


implementation


uses
    Windows,
    SysUtils,
    Math,
    LogManager,
    ErrorManager,
    CommonTypes,
    Rack,
    AppSettings,
    SamHigh,
    ObjModul,
    RackWell,
    LiqHTypes,
    IntfPipDevice,
    IntfSensorDevice,
    IntfSwitchDevice,
    IntfMotorDevice,
    IntfMotorDriver,
    OperationFactory,
    OperationAxisMove,
    MotionSystemPipMove,
    TipMapUtils,
    GeneralTypes;

procedure gmDoTipPressure(aUsedArm: IArmDevice; aSyrMap: TIPMAP; const aRP: TArray<TXRackPosition>;
    aErrFlag, aTipSingleRetract: boolean; aDispDelay: single;
    aZScanMode, aZScanSpeed, aZRetractSpeed: integer);
var
    c: integer;
    xResult: integer;
    xIniAccess: IWinLissyIniAccess;
    xSensorDev: ISensorDevice;
    iSwitch: ISwitchDevice;
    RequestTime, StartDelay, EndDelay, MoveZDelay: Integer;
    xStartTime, xTimeSoFar, xMaxTime: DWORD; // must be DWORD
    xTipsCompleted: TIPMAP;
    xPipDevice: IPipDevice;
    xXYSteps: TXYStepList;
    xZSteps: TPipStepZPosArray;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xScanSpeeds: TIntArray;
begin
    xIniAccess := gCommonDll.CreateAppIni;
    RequestTime := xIniAccess.ReadInteger('PosPressCtrl', 'RequestTime');
    StartDelay := xIniAccess.ReadInteger('PosPressCtrl', 'StartDelay');
    MoveZDelay := xIniAccess.ReadInteger('PosPressCtrl', 'MoveZDelay');
    EndDelay := xIniAccess.ReadInteger('PosPressCtrl', 'EndDelay');
    if (aZScanSpeed <= 0) then
        aZScanSpeed := xIniAccess.ReadInteger('PosPressCtrl', 'ScanSpeed');
    if (aZRetractSpeed <= 0) then
        aZRetractSpeed := xIniAccess.ReadInteger('PosPressCtrl', 'RetractSpeed');

    xPipDevice := aUsedArm.PipDevice;
    xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);

    if (aZScanSpeed = 0) then
        xScanSpeeds := xZMotion.GetScanSpeeds(aSyrMap, 0);

    // xPStep := aUsedArm.CalculatePipSteps(aSyrMap, ahRack, aPosition, 'B'); // y = Array of Positions
    gmCalculatePipSteps(aUsedArm, aSyrMap, aRP, xXYSteps, xZSteps);
    xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);
    for c := 0 to xXYSteps.Count - 1 do
    begin
        if (xXYSteps[c].MotorMap > 0) then
        begin
            xXYOp.MoveXY(xXYSteps[c], aRP, xZSteps[c], []);

            if (aErrFlag) then
                xZMotion.DisableZErrors(xZSteps[c].SyrMap);

            xZMotion.MoveZ_MultiSpeed(xZSteps[c].SyrMap, xZSteps[c].Z.ZDisp, m_EXECUTE, AT_MOVE_ABS,
                xScanSpeeds, 0, false);
            Sleep(MoveZDelay);
            gLogManager.Log('Pressure on', true);
        end;
        if (not gErrorManager.IsGlobalErr) then
        begin
            if (((aSyrMap shr c) and 1) = 1) and (not gErrorManager.IsGlobalErr) then
            begin
                if gModules.Find_ByName('POSPRESS' + IntToStr(c + 1), ISwitchDevice, iSwitch) then
                    // ?? old 'SwitchValve'+IntToStr(c+1)
                    iSwitch.SwitchOn(false);
            end;
        end;
    end;
    FreeAndNil(xXYOp);

    Sleep(StartDelay);

    xMaxTime := Round(aDispDelay * 1000);

    xTipsCompleted := 0;
    xStartTime := GetTickCount();
    // Break out of loop if time is higher than xMaxTime or if pressure detection
    // is completed for all tips in SyrMap
    while (true) do
    begin
        for c := 0 to xPipDevice.TipCount - 1 do
        begin

            if (gErrorManager.IsGlobalErr) then
                BREAK;
            // if tip c is not in aSyrMap or if the pressure detection has already been completed keep going
            if (not TTipMapUtils.TipSelected(aSyrMap, c)) or TTipMapUtils.TipSelected(xTipsCompleted, c) then
                CONTINUE;

            aRP[c].Rack.PaintTubePos(aRP[c].Pos, TRackWellDisplayType.Other);
            xResult := 0;
            xSensorDev := gModules.FindSensor(c + 1);
            if (xSensorDev <> nil) then
            begin
                if (not xSensorDev.ValueIsDefault) then
                begin
                    gLogManager.Log('Tip ' + IntToStr(c + 1) + ': Pressure detected', true);
                    xResult := 1;
                end
                else
                    gLogManager.Log('Tip ' + IntToStr(c + 1) + ': No pressure detected', true);

            end
            else
                gLogManager.Log('Device LineIn' + IntToStr(c + 1) + ' is not present!', true);

            if (xResult = 0) then
            begin
                TTipMapUtils.SelectTip(xTipsCompleted, c);
                if gModules.Find_ByName('POSPRESS' + IntToStr(c + 1), ISwitchDevice, iSwitch) then
                    // ?? old 'SwitchValve'+IntToStr(c+1)
                    iSwitch.SwitchOff(false);
                if (EndDelay = 0) then
                    xZMotion.MoveSingleZ_Retract(c, xZSteps[c].Z.ZTravel[c], m_EXECUTE,
                        aZRetractSpeed, false);
            end;

        end;

        xTimeSoFar := GetTickCount() - xStartTime;

        if xTipsCompleted = aSyrMap then
        begin
            gLogManager.LogF('Pressure detection completed after %d seconds',
                [Round(xTimeSoFar / 1000)], true);
            BREAK;
        end;

        if xTimeSoFar >= (xMaxTime) then
        begin
            gLogManager.LogF
                ('Aborting pressure detection - Time so far: %d seconds >= Disp Delay: %d seconds',
                [Round(xTimeSofar / 1000), Round(xMaxTime / 1000)], true);
            BREAK;
        end;
        Sleep(RequestTime);
    end;

    for c := 0 to xXYSteps.Count - 1 do
    begin
        if (((aSyrMap shr c) and 1) = 1) then
        begin
            if gModules.Find_ByName('POSPRESS' + IntToStr(c + 1), ISwitchDevice, iSwitch) then
                // ?? old 'SwitchValve'+IntToStr(c+1)
                iSwitch.SwitchOff(false);
            if (EndDelay = 0) then
                xZMotion.MoveSingleZ_Retract(c, xZSteps[c].Z.ZTravel[c], m_EXECUTE, aZRetractSpeed, false);
        end;
    end;
    Sleep(EndDelay);
    for c := 0 to xPipDevice.TipCount - 1 do
        if (EndDelay <> 0) then
        begin
            xZMotion.MoveSingleZ_Retract(c, xZSteps[c].Z.ZTravel[c], m_EXECUTE, aZRetractSpeed, false);
            if (aTipSingleRetract) then
                Sleep(300);
        end;
    if (aErrFlag) then
        xZMotion.ResetAllDisabledZErrors();

    FreeAndNil(xZMotion);
    FreeAndNil(xXYSteps);
end;

procedure gmTipPressure(aUsedArm: IArmDevice; aTips: TIPMAP; const aRP: TArray<TXRackPosition>);
var
    cnt: integer;
    xLiqH: TLiqHandlingRec;
begin
    for cnt := 0 to aUsedArm.PipDevice.TipCount - 1 do
    begin
        if TTipMapUtils.TipSelected(aTips, cnt) then
        begin
            xLiqH := aUsedArm.PipDevice.Tips[cnt].St_LhPtr;
        end;
    end;
    gLogManager.Log('Tip Pressure - ' + TXRackPositionUtils.RackPositionsToBracketText(aRP), true);

    gmDoTipPressure(aUsedArm, aTips, aRP, (xLiqH.DispErrFlag = 1), xLiqH.DispTipSingleRetract,
        xLiqH.DispDelay, xLiqH.DispScanMode, xLiqH.DispScanSpeed, 0);
end;


end.
