{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : These functions are used by the action 'WASHP'
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.07.07 wl  WashAtRackPositions          TN3792    AspTime wurde bisher zweimal benutzt: Jetz aufgeteilt in InsertDelay & AspDispDelay
  23.07.07 wl  WashAtRackPositions          TN3792    CleanDelay ersetzt Setting 'DelayBefore2ndBlowN2'
  23.07.07 wl  WashAtRackPositions          TN3792    Neu: CleanPrecision! Wurde bisher bei allen Steps außer dem letzten benutzt
  23.07.07 wl  WashAtWashStation            TN3792    N2-Time in Wash-Station nicht mehr 2 Sec hardcodiert, sondern variabel
  23.07.07 wl  WashAtWashStation            TN3792    WashStation_SwitchOffCh3 ersetzt Setting 'SwitchOffN2C3BeforeWashInnerCh'
  23.07.07 wl  StartWashProg                TN3750    Zwangswash am Schluß entfernt
  23.07.07 wl  WashAtRackPositions          TN3754    1st Blow N2-Time entfernt: Einmal N2-Time vor dem Rausfahren reicht
  25.07.07 wl                               TN3792    DefineUsedArm --> TWashProgAction
  25.07.07 wl  WashAtRackPositions          TN3792    Retract am Schluß geht auf Rack-ZTravel, nicht Global-ZTravel
  08.08.07 wl                               TN3802    WashSt_Ch3Off wird wieder aus Settings gelesen
  08.08.07 wl                               TN3802    N2Time aufgeteilt in 2 Felder (vor und nach "Wash needle")
  08.08.07 wl                               TN3802    uses geändert
  09.11.07 pk                               TN3924    Steps changed to mm
  29.01.08 wl                               TN3980   uses geändert
  15.05.08 wl                               TN4100    TXYStepList property changed
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  12.09.09 wl                               TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  10.12.09 pk  WashAtRackPositions          TN4929   Resin Height added using AddZHeight
  11.12.09 pk  WashAtRackPositions          TN4167   New DispHeightPos: Choose from Z-Scan or Z-Disp height for dispensing
  09.03.10 pk  WashAtRackPositions          TN5016   move to Z-Scan before blowing N2 the second time
  03.11.10 pk  WashAtRackPositions          TN5025   AV occured when using non-existing array positions in xZSteps[ xStep ].Z array.  Now check tipmap
  19.10.11 wl                               TN5723   PaintTubes ersetzt
  20.09.11 wl                               TN5723   gmCalculatePipStep jetzt ohne Parameter
  28.09.11 wl                               TN5725   gmCalculatePipSteps jetzt ohne Parameter
  02.02.11 wl  alle Funktionen              TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  10.08.12 wl  FlushSyringeCh1,Ch2          TN5947   von LiquidHandlingLow hierher
  07.01.13 ts  WashAtRackPositions          TN6065   neu: xCleanHeightArr, to clean also at ZDisp
  23.05.13 wl                               TN6153   geänderte Parameter bei MoveXY
  21.08.13 wl                               TN6249   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit WashProgramHandling;


interface


uses
    WashProgDataAdaptor,
    GeneralTypes,
    RackTypes,
    MotionSystemPipMove,
    Rack,
    IntfPipDevice,
    IntfArmDevice;

type
    TWashProgProcess = class
    private
        fUsedArm: IArmDevice;
        fRP: TArray<TXRackPosition>;
        //
        fVacuumPumpName: string;

        class function GetVacuumPumpName(): string;
        procedure TurnDevice(const aName: string; aOn: boolean);
        procedure WashAtRackPositions(const aWashRec: TWashprogRec);
        procedure WashAtWaste(aInnerWashVol, aN2Time: double; aSwitchOffCh3: boolean);
        procedure WashAtWash(aOuterWashVol: double);
        procedure WashAtWashStation(aInnerWashVol, aOuterWashVol, aN2Time: double);
        class procedure FlushSyringeCh1(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aRack: TRack; aTips: integer; aVolume: extended);
        class procedure FlushSyringeCh2(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aRack: TRack; aTips: integer; aVolume: extended; aDispSpeed: integer);
    public
        constructor Create(aUsedArm: IArmDevice; aRP: TArray<TXRackPosition>);

        procedure StartWashProg(const aWashRec: TWashprogRec);
    end;


implementation


uses
    SysUtils,
    ErrorManager,
    LogManager,
    RunFlow,
    ObjModul,
    TipMapUtils,
    CommonTypes,
    LiquidHandling,
    LiquidHandlingLow,
    SamGlobe,
    RackWell,
    IntfSwitchDevice,
    IntfMotorDevice,
    IntfMotorDriver,
    AppSettings,
    LiquidManager,
    AppTypes,
    ArrayUtils,
    SamHigh,
    OperationFactory,
    OperationAxisMove,
    Carrier;

const
    STR_WASHPROG_VACUUMPUMP_DEFAULT = 'VACUUMPUMP';
    STR_WASHPROG_VACUUMPUMP_SPECIAL = 'WASHPROGVACUUMPUMP';
    STR_WASHPROG_FULLPRESSURE = 'FULLPRESSURE';

    STR_WASHPROG_VALVE_N2_CHANNEL3 = 'N2CANNEL3';
    STR_WASHPROG_VALVE_VACC_N2 = 'VACC_N2';
    STR_WASHPROG_VALVE_DILUTOR_GAS = 'DILUTOR_GAS';
    // false: Schalten auf Dilutorkanal; true: Schalten auf Gaskanal

    { TWashProgProcess }
constructor TWashProgProcess.Create(aUsedArm: IArmDevice; aRP: TArray<TXRackPosition>);
begin
    inherited Create;

    fUsedArm := aUsedArm;
    fRP := Copy(aRP);

    fVacuumPumpName := GetVacuumPumpName;
end;

procedure TWashProgProcess.TurnDevice(const aName: string; aOn: boolean);
var
    xDevice: ISwitchDevice;
begin
    if not gModules.Find_ByNameCut(aName, ISwitchDevice, xDevice) then
        EXIT;
    if (aOn) then
        xDevice.SwitchOn(false)
    else
        xDevice.SwitchOff(false);
end;

class function TWashProgProcess.GetVacuumPumpName(): string;
var
    xDevice: ISwitchDevice;
begin
    if gModules.Find_ByName(STR_WASHPROG_VACUUMPUMP_SPECIAL, ISwitchDevice, xDevice) then
        result := STR_WASHPROG_VACUUMPUMP_SPECIAL
    else
        result := STR_WASHPROG_VACUUMPUMP_DEFAULT;
end;

class procedure TWashProgProcess.FlushSyringeCh1(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aRack: TRack; aTips: integer; aVolume: extended);
var
    x: integer;
    xVol: TDoubleArray;
begin
    xVol := aPipDevice.GetNullDoubleArray;
    for x := 0 to aPipDevice.TipCount - 1 do
        if (((aTips shr x) and 1) = 1) then
            xVol[x] := aVolume;
    TLiquidHandlingLow.FlushSyringe(aPipDevice, aZMotion, aRack, xVol, aPipDevice.GetNullDoubleArray, 0,
        TLiquidChannels.GetDefaultChannel1Array(aPipDevice));
end;

class procedure TWashProgProcess.FlushSyringeCh2(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aRack: TRack; aTips: integer; aVolume: extended;
    aDispSpeed: integer);
var
    x: integer;
    xVol: TDoubleArray;
begin
    xVol := aPipDevice.GetNullDoubleArray;
    for x := 0 to aPipDevice.TipCount - 1 do
        if (((aTips shr x) and 1) = 1) then
            xVol[x] := aVolume;
    TLiquidHandlingLow.FlushSyringe(aPipDevice, aZMotion, aRack, aPipDevice.GetNullDoubleArray, xVol,
        aDispSpeed, TLiquidChannels.GetDefaultChannel1Array(aPipDevice));
end;

procedure TWashProgProcess.WashAtRackPositions(const aWashRec: TWashprogRec);
var
    x, xStep: integer;
    xXYSteps: TXYStepList;
    xZSteps: TPipStepZPosArray;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xZPosArray: TDoubleArray;
    xDispHeightArr, xCleanHeightArr: TDoubleArray;
    xDispHeightLog, xCleanHeightLog: string;
    xSingleRack: TRack;
begin
    for x := 0 to high(fRP) do
        fRP[x].Rack.PaintTubePos(fRP[x].Pos, TRackWellDisplayType.Other);

    // Rackpositionen berechnen
    gmCalculatePipSteps(fUsedArm, fUsedArm.PipDevice.UseTips, fRP, xXYSteps, xZSteps);
    try
        gLogManager.Log('Wash Program: Wash Positions - ' + TXRackPositionUtils.RackPositionsToBracketText
            (fRP), true);

        // Main Loop: Alles an der Rackposition
        for xStep := 0 to xXYSteps.Count - 1 do
        begin // Dieser Loop sollte im Normalfall nur einmal durchlaufen werden

            if (xZSteps[xStep].SYRMAP <= 0) then
                CONTINUE;

            self.TurnDevice(STR_WASHPROG_VALVE_VACC_N2, false); // Vacc on
            self.TurnDevice(STR_WASHPROG_VALVE_DILUTOR_GAS, true); // Vacc on
            self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, false); // kein N2 für Fahrt über Arbeitsfläche

            // xy Position anfahren
            xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(fUsedArm);
            try
                xXYOp.MoveXY(xXYSteps[xStep], fRP, xZSteps[xStep], []);
            finally
                FreeAndNil(xXYOp);
            end;

            if (gErrorManager.IsGlobalErr) then
                EXIT;

            // Herunterfahren bis ZMax & Aspirate
            gLogManager.LogF('Wash Program: Tracking to Z-Max - ResinHeight %g',
                [aWashRec.Resin_Height], false);

            xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(fUsedArm.MotionDevice);
            try
                xZMotion.MoveZ(xZSteps[xStep].SYRMAP, xZSteps[xStep].Z.ZScan, m_EXECUTE);
                if (aWashRec.InertGas) then
                    self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, true);

                // Höhenberechnung ZMax - ResinHeight
                xZPosArray := xZMotion.Motors.GetNullDoubleArray;
                for x := 0 to xZMotion.Motors.MaxIndex do
                begin
                    if not TTipmapUtils.TipSelected(xZSteps[xStep].SYRMAP, x) then
                        CONTINUE;

                    xZPosArray[x] := AddZHeight(xZSteps[xStep].Z.ZMax[x], aWashRec.Resin_Height);
                end;

                xZMotion.MoveZ(xZSteps[xStep].SYRMAP, xZPosArray, m_EXECUTE, AT_MOVE_ABS,
                    aWashRec.InsertSpeed);
                gRunFlow.AppSleep(Round(aWashRec.InsertDelay * 1000));

                if (gErrorManager.IsGlobalErr) then
                    EXIT;
                xSingleRack := TXRackPositionUtils.ExtractSingleRackFromArray(fRP);
                if not Assigned(xSingleRack) then
                    EXIT;

                // Dispensieren und gleichzeitig mit dem Vacuum Flüssigkeit aufnehmen
                gLogManager.LogF('Wash Program: Washing - Aspirate and dispense volume %g',
                    [aWashRec.AspDispVol], false);
                if (aWashRec.AspDispVol > 0) then
                begin
                    FlushSyringeCh2(fUsedArm.PipDevice, xZMotion, xSingleRack, xZSteps[xStep].SYRMAP,
                        aWashRec.AspDispVol, aWashRec.AspDispSpeed);
                    gRunFlow.AppSleep(Round(aWashRec.AspDispDelay * 1000));
                end;

                xDispHeightArr := xZMotion.Motors.GetNullDoubleArray;
                for x := 0 to xZMotion.Motors.MaxIndex do
                begin
                    if not TTipmapUtils.TipSelected(xZSteps[xStep].SYRMAP, x) then
                        CONTINUE;
                    if aWashRec.DispHeightPos = INT_DISPHEIGHTPOS_ZDISP then
                    begin
                        xDispHeightArr[x] := xZSteps[xStep].Z.ZDisp[x];
                        xDispHeightLog := 'Z-Disp';
                    end
                    else
                    begin
                        xDispHeightArr[x] := xZSteps[xStep].Z.ZScan[x];
                        xDispHeightLog := 'Z-Scan';
                    end
                end;

                // Nadeln auf ZScan fahren
                gLogManager.Log('Wash Program: Move to ' + xDispHeightLog + ' position', false);
                xZMotion.MoveZ(xZSteps[xStep].SYRMAP, xDispHeightArr, m_EXECUTE, AT_MOVE_ABS, 0, 0,
                    aWashRec.SingleRetract);
                self.TurnDevice(STR_WASHPROG_VALVE_DILUTOR_GAS, true); // Vacc onValveSyrN2_On_N2;

                // Nadeln ausblasen --1
                if (aWashRec.N2Time1 > 0) then
                begin
                    gLogManager.LogF('Wash Program: Blow N2 - Time %g', [aWashRec.N2Time1], true);
                    self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, false);
                    self.TurnDevice(STR_WASHPROG_VALVE_VACC_N2, true); // N2 on
                    gRunFlow.AppSleep(Round(aWashRec.N2Time1 * 1000));
                    self.TurnDevice(STR_WASHPROG_VALVE_VACC_N2, false); // Vacc on
                    if (aWashRec.InertGas) then
                        self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, true);
                end;

                if (gErrorManager.IsGlobalErr) then
                    EXIT;

                // Cleaning: Disp at Z-Scan
                if (aWashRec.Clean_Volume > 0) then
                begin
                    gLogManager.Log(Format('Wash Program: Cleaning - Dispense volume %g',
                        [aWashRec.Clean_Volume]), false);

                    if (aWashRec.CleanMode = 1) then
                    begin
                        self.TurnDevice(STR_WASHPROG_VALVE_DILUTOR_GAS, false);
                        // Vacc off, wenn dieser Waschschritt nicht der letzte ist, um ein definiertes Volumen abzugeben
                        self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, false);
                        // N2 aus, damit kein Überdruck entsteht
                    end;
                    FlushSyringeCh2(fUsedArm.PipDevice, xZMotion, xSingleRack, xZSteps[xStep].SYRMAP,
                        aWashRec.Clean_Volume, aWashRec.CleanDispSpeed);
                    if (aWashRec.CleanMode = 1) then
                    begin
                        if (aWashRec.InertGas) then
                            self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, true); // Gas wieder ein !
                        self.TurnDevice(STR_WASHPROG_VALVE_DILUTOR_GAS, true); // Vacc wieder einschalten !
                    end;
                    gRunFlow.AppSleep(Round(aWashRec.CleanDelay * 1000));
                end;

                if (gErrorManager.IsGlobalErr) then
                    EXIT;

                // Druckausgleich mit Kanal 3
                if (aWashRec.N2Time2 > 0) then
                begin
                    gLogManager.LogF('Wash Program: Blow N2 - Time %g sec', [aWashRec.N2Time2], true);

                    // Nadeln auf ZScan fahren - Hardcodiert - Laut Heyduck muss das Zweite mal beim ZScan passieren weil
                    // sonst wird der Dreck im röhrchen auf der Nadel gepustet
                    // Denis Duval braucht aber die Möglichkeit auch bei ZDispense zu pusten, da ZScan zu hoch ist!

                    xCleanHeightArr := xZMotion.Motors.GetNullDoubleArray;
                    for x := 0 to xZMotion.Motors.MaxIndex do
                    begin
                        if not TTipmapUtils.TipSelected(xZSteps[xStep].SYRMAP, x) then
                            CONTINUE;
                        if aWashRec.CleanHeight = INT_DISPHEIGHTPOS_ZDISP then
                        begin
                            xCleanHeightArr[x] := xZSteps[xStep].Z.ZDisp[x];
                            xCleanHeightLog := 'Z-Disp';
                        end
                        else
                        begin
                            xCleanHeightArr[x] := xZSteps[xStep].Z.ZScan[x];
                            xCleanHeightLog := 'Z-Scan';
                        end
                    end;

                    gLogManager.Log('Wash Program: Move to ' + xCleanHeightLog + ' position', false);
                    xZMotion.MoveZ(xZSteps[xStep].SYRMAP, xCleanHeightArr, m_EXECUTE, AT_MOVE_ABS, 0, 0,
                        aWashRec.SingleRetract);

                    self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, false);
                    self.TurnDevice(STR_WASHPROG_VALVE_VACC_N2, true); // N2 on
                    gRunFlow.AppSleep(Round(aWashRec.N2Time2 * 1000));
                    self.TurnDevice(STR_WASHPROG_VALVE_VACC_N2, false); // Vacc on
                    if (aWashRec.InertGas) then
                        self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, true);
                end;

                // Retract auf Rack-ZTravel
                xZMotion.MoveZ(xZSteps[xStep].SYRMAP, xZSteps[xStep].Z.ZTravel, m_EXECUTE, AT_MOVE_ABS, 0, 0,
                    aWashRec.SingleRetract);
            finally
                FreeAndNil(xZMotion);
            end;
        end;

    finally
        FreeAndNil(xXYSteps);
    end;
end;

procedure TWashProgProcess.WashAtWaste(aInnerWashVol, aN2Time: double; aSwitchOffCh3: boolean);
var
    xRack: TRack;
    xXYStep: TXYStep;
    xZStep: TPipStepZPos;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
begin
    if (aInnerWashVol <= 0) then
        EXIT;

    self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, false); // kein N2 für Fahrt über Arbeitsfläche
    // --------------------------------------------- Ventile auf Vakuum schalten um Tropfen aufzuziehen
    self.TurnDevice(STR_WASHPROG_VALVE_DILUTOR_GAS, true); // Schalten auf Gaskanal
    self.TurnDevice(STR_WASHPROG_VALVE_VACC_N2, false);

    // Zur Waste-Position gehen
    gLogManager.Log('Wash Program: Move to WASTE position', false);
    xRack := TLiquidHandlingLow.FindWasteRack(fUsedArm.PipDevice);

    xXYStep := gmCalculatePipStep(fUsedArm, fUsedArm.PipDevice.UseTips,
        gmGetRackPosArrayForTips(fUsedArm.PipDevice, xRack), xZStep);
    if not Assigned(xXYStep) then
        EXIT;

    try
        xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(fUsedArm);
        try
            xXYOp.MoveXY(xXYStep, gmGetRackPosArrayForTips(fUsedArm.PipDevice, xRack), xZStep, []);
        finally
            FreeAndNil(xXYOp);
        end;

        self.TurnDevice(STR_WASHPROG_VALVE_DILUTOR_GAS, false); // Schalten auf Diluterkanal

        xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(fUsedArm.MotionDevice);
        try
            xZMotion.MoveZ(xZStep.SyrMap, xZStep.Z.ZMax, m_EXECUTE);
            self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, true); // Kanal3 ausblasen !

            // inneren Kanal waschen
            gLogManager.LogF('Wash Program: Wash inner channel - volume %g', [aInnerWashVol], true);
            FlushSyringeCh1(fUsedArm.PipDevice, xZMotion, xRack, xZStep.SyrMap, aInnerWashVol);
        finally
            FreeAndNil(xZMotion);
        end;

        // Setting:
        if (aSwitchOffCh3) then
            self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, false);

        // Mit N2 blasen ! Sonst würde das Waschvolumen im Schlauch bleiben
        gLogManager.LogF('Wash Program: Blow N2 at WASTE - Time %g sec', [aN2Time], true);
        self.TurnDevice(STR_WASHPROG_VALVE_DILUTOR_GAS, true);
        self.TurnDevice(STR_WASHPROG_VALVE_VACC_N2, true);
        if not gErrorManager.IsGlobalErr() then
            gRunFlow.AppSleep(Round(aN2Time * 1000)); // um Spritzen zu vermeiden
        self.TurnDevice(STR_WASHPROG_VALVE_DILUTOR_GAS, false);
        self.TurnDevice(STR_WASHPROG_VALVE_VACC_N2, false);
    finally
        FreeAndNil(xXYStep);
    end;
end;

procedure TWashProgProcess.WashAtWash(aOuterWashVol: double);
var
    xRack: TRack;
    xXYStep: TXYStep;
    xZStep: TPipStepZPos;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
begin
    if (aOuterWashVol <= 0) then
        EXIT;

    // Zur Wash-Position gehen
    gLogManager.Log('Wash Program: Move to WASH position', false);
    self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, false); // N2 aus, damit es nicht spritzt

    xRack := TLiquidHandlingLow.FindWashRack(fUsedArm.PipDevice);

    xXYStep := gmCalculatePipStep(fUsedArm, fUsedArm.PipDevice.UseTips,
        gmGetRackPosArrayForTips(fUsedArm.PipDevice, xRack), xZStep);
    if not Assigned(xXYStep) then
        EXIT;
    try
        xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(fUsedArm);
        try
            xXYOp.MoveXY(xXYStep, gmGetRackPosArrayForTips(fUsedArm.PipDevice, xRack), xZStep, []);
        finally
            FreeAndNil(xXYOp);
        end;

        xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(fUsedArm.MotionDevice);
        try
            xZMotion.MoveZ(xZStep.SyrMap, xZStep.Z.ZMax, m_EXECUTE);

            // äußeren Kanal waschen
            gLogManager.LogF('Wash Program: Wash outer channel - volume %g', [aOuterWashVol], true);
            FlushSyringeCh2(fUsedArm.PipDevice, xZMotion, xRack, xZStep.SyrMap, aOuterWashVol, 0);
        finally
            FreeAndNil(xZMotion);
        end;
    finally
        FreeAndNil(xXYStep);
    end;
end;

procedure TWashProgProcess.WashAtWashStation(aInnerWashVol, aOuterWashVol, aN2Time: double);
var
    xIniAccess: IWinlissyIniAccess;
    xSwitchOffCh3: boolean;
begin
    xIniAccess := gCommonDll.CreateAppIni;
    xSwitchOffCh3 := xIniAccess.ReadBool('Washprogram', 'SwitchOffN2C3BeforeWashInnerCh');

    // Wash inner channel
    self.WashAtWaste(aInnerWashVol, aN2Time, xSwitchOffCh3);

    // Wash outer channel
    self.WashAtWash(aOuterWashVol);

    // Dry needles
    if (aInnerWashVol > 0) or (aOuterWashVol > 0) then
        TLiquidHandlingLow.Dry(fUsedArm, fUsedArm.PipDevice.UseTips);
end;

procedure TWashProgProcess.StartWashProg(const aWashRec: TWashprogRec);
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    gLogManager.LogF('Wash Program [%s]: Start', [aWashRec.WPName], true);

    // Vacuumpumpe auf vollen Druck schalten
    self.TurnDevice(fVacuumPumpName, true);
    self.TurnDevice(STR_WASHPROG_FULLPRESSURE, true); // Vollen N2 Druck für das Waschprogramm

    // Execute Wash program at rack positions
    self.WashAtRackPositions(aWashRec);

    // Execute Wash program at Washstation
    self.WashAtWashstation(aWashRec.InnerWashVolume, aWashRec.OuterWashVolume, aWashRec.InnerWashN2Time);

    // Vacuumpumpe wieder ausschalten
    self.TurnDevice(STR_WASHPROG_FULLPRESSURE, false);
    self.TurnDevice(fVacuumPumpName, false);

    // Switch off all valves
    self.TurnDevice(STR_WASHPROG_VALVE_DILUTOR_GAS, false); // Vacc on
    self.TurnDevice(STR_WASHPROG_VALVE_N2_CHANNEL3, false); // kein N2 für Fahrt über Arbeitsfläche
    self.TurnDevice(STR_WASHPROG_VALVE_VACC_N2, false); // Vacc on

    // Zwangswash am Schluß entfernt
    // xUsedArm.MoveZ(aUsedArm.OKTips, aUsedArm.ZTravelArr_Steps, m_EXECUTE, AT_MOVE_ABS, 0, 0, xWashRec.SingleRetract);
    // if (gSaveSolvent=0) then gmWash( aUsedArm, gmSArrWrite(2000, aUsedArm.TipCount), gmSArrWrite(2000, aUsedArm.TipCount),false)
    // else gmWash( aUsedArm, gmSArrWrite(2000, aUsedArm.TipCount), gmSArrWrite(0,0),false);
end;


end.
