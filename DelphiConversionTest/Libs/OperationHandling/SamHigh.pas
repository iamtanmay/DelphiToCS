unit SamHigh;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Schrott-Unit, in die Dinge gepackt werden, die man nicht unterbringen kann
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  12.08.03 wl                               TN1526   initial version
  22.09.03 wl                               TN1526   alle Arrays werden als var übergeben (wie in C)
  23.09.03 wl  gmIsZTravel,gmDetectLiquid,..TN1580   alle Routinen übersetzt, die mit Liquid Handling zu tun haben
  20.10.03 wl  gmIsZTravel                  TN1580   Funktion korrigiert
  04.12.03 wl  gmIsZTravel                  TN1580   Funktion nochmals korrigiert
  16.12.03 wl  alle Funktionen              TN1672   Robot-Move-Aufrufe ersetzt durch gPipArm-Methoden
  19.12.03 wl                               TN1672   MoveSingleZ - Parameter geändert
  21.01.04 wl  gmMoveAspPos2                TN1637   Fehlermeldung läßt sich jetzt mit 'Abort' wegklicken
  21.01.04 wl  gmDetectLiq                  TN1672   Aufruf von gPipArm.LiquidTips
  02.02.04 wl  gmMoveAspPos2,gmMoveDspPos2  TN1738   Negativer Submerge kann übergeben werden, da MPOS statt word
  03.02.04 wl  gmMoveAspPos2,gmMoveDspPos2  TN1708   einer von 4 ScanModes (0,1,2,3) kann mit übergeben werden
  12.03.03 wl  gmIsZTravel                  TN1812   --> DevicesArms
  05.04.04 wl  gmDetectLiquid,gmDetectLiq   TN1708   ZScanMode und ZScanSpeed als Parameter
  05.04.04 wl  gmMoveAspPos2,gmMoveDspPos2  TN1708   ZScanMode, ZScanSpeed und ZRetractSpeed als Parameter
  05.04.04 wl  alle Funktionen!             TN1788   aUsedArm wird als Parameter übergeben, gPipArm wird nicht mehr verwendet
  05.04.04 wl  gmGetVol                     TN1788   von AppInterface hierher verschoben
  05.04.04 wl  gmMoveAspPos2,gmMoveDspPos2  TN1788   Setzen von OKTips im Fehlerfall deaktiviert, da diese Fälle nie eintreten können
  06.04.04 pk  gmMoveAspPos2                TN1839   All tips should not be moved to lowest detected tip pos if TubeType has the RTF_SINGLE_TUBE flag set
  06.04.04 pk  gmMoveAspPos2                TN1839   when xErrMode = 1 IsVol[] was only set for first tip due to incorrect bitshift operation
  20.04.04 wl                               TN1788   TipIntegerArray statt TipWordArray
  20.04.04 wl  gmDispenseVirtual            TN1788   von DilutionManager hierher
  10.05.04 wl  gmGetVol                     TN1788   log "TubeBoden" in "tube bottom" geändert
  10.05.04 wl  gmGetVol                     TN1788   vereinfachte Berechnung durch Benutzung von Rack.WellSurface_mm2
  11.05.04 wl  gmDispenseVirtual            TN1788   SamDspLiquid ersetzt
  13.05.04 wl  gmMoveAspPos2                TN1788   Parameter aAspPos wurde nicht benutzt -> entfernt
  13.05.04 wl  gmGetZPosByVolCalc           TN1788   entspricht GetAPosition aus AppInterface
  13.05.04 wl  gmGetZPosByVolCalc           TN1788   benutzt GetPipVolume
  13.05.04 wl  gmMoveDspPos2                TN1919   wenn LevelTracking eingeschaltet ist, wird die Startposition berechnet (wenn möglich)
  13.05.04 wl  gmGetZPosByVolCalc           TN1919   kein Volumen -> result = ZMax / keine RackID -> result = aZPosIfNoRackID
  29.06.04 wl                               TN1985   FLastRackName entfernt (hatte hier keinen Sinn)
  25.08.04 wl  gmMoveAspPos2                TN2105   auch ohne LiquidDetection wird ZInsertSpeed (= Scanspeed) benutzt
  25.08.04 wl  gmMoveDspPos2                TN2105   auch ohne LiquidDetection wird ZInsertSpeed (= Scanspeed) benutzt
  01.09.04 pk                               TN2114   gmMoveAspPos2 : Use TipCount - 1
  08.09.04 wl  gmSetTipError,gmRackGetZPos,gmSwitchModulePort  TN2121   von AppInterface hierher verschoben
  08.09.04 wl  gmStoreAsp,gmStoreDsp        TN2121   ruft gleichnamige Funktionen in DevicesTips auf
  11.09.04 wl  gmDetectLiquid,gmMoveToLowestTip TN2123  Z-Parameter: MPosArray statt MPos
  11.09.04 wl  gmGetVol,gmRackGetZPos       TN2123   statt UseTip_ZOffset_mm wird TipZOffset_mm mit Index verwendet
  11.09.04 wl  gmMoveAspPos2,gmMoveDspPos2  TN2123   aZScan, aZMax, und aZDisp werden als MPosArray-Parameter übergeben
  11.09.04 wl  gmMoveAspPos2,gmMoveDspPos2  TN2123   geänderte Aufrufe von MoveZ (MposArray statt MPos)
  13.10.04 wl  gmMoveAspPos2,gmMoveDspPos2,gmDetectLiq,gmGetVol,gmMoveToLowestTip  TN2151 --> LiquidHandling
  13.10.04 wl  gmDetectLiquid               TN2151   --> LiquidHandlingLow
  13.10.04 wl  gmSwitchModulePort           TN2151   vereinfacht
  14.10.04 wl  gmGetZPosByVolCalc           TN2178   neuer Parameter aDoNotCalc: result ist immer DefaultWert
  21.02.05 wl  gmPipDelay                   TN1960   gCommManager.AppSleep statt global_AppInterface-Methode
  27.04.05 pk  gmRackGetZPos,gmGetZPosByVolCalc TN2400 : receive ZTube as parameter instead of calculating ZTube - Slanted Racks
  04.05.05 wl  gmDispenseVirtual            TN2410   --> TFillManualAction
  03.06.05 wl  gmSetTipError                TN2436   Sam.nTips durch MAX_TIPS ersetzt (kommt aufs gleiche raus)
  18.07.05 thr gmGetZPosByVolCalc           TN2411   Zusätzlicher Parameter IsMultitip;
  09.11.05 wl  gmGetRackZPosByVolCalc,gmRackGetZPos  TN2728   benutzt TArmZMovementOperator
  17.11.05 wl                               TN2771    TOperation statt TOperator
  18.11.05 wl  gmRackGetZPos                TN2764    hat aTip als Parameter
  11.01.06 pk  gmMaxLiquid                  TN2781.0  var aVol changed to const aVol
  19.04.06 wl                               TN3051  benutzt TipFloatArray und extended
  24.08.06 wl  gmCalculatePipStep,gmCalculatePipSteps  TN3271   neuer Parameter: ColorChar
  18.09.06 pk  gmStoreAsp                   TN3227.1 find CurrentSysName and use it as argument to Tip.StoreAsp
  03.12.06 wl  gmStoreAsp                   TN3243   Benutzung von TReagentIsOutErrorInfo
  04.12.06 wl  gmSetSingleTipError,..       TN3445   neue Funktionen für TipLiqError (.Code entfernt - wurde nicht benutzt)
  07.12.06 wl                               TN3243    uses SamErr entfernt
  20.12.06 wl  gmStoreAsp                   TN3490    CurrentSysLiquid wird hier nicht mehr ermittelt (Access Violation bei Powder)
  19.01.07 wl  gmSwitchModulePort           TN3473    bei Swich-Namen mit * wird das * durch alle in der Tipmap enthaltenen Tip-Nummern ersetzt und alle gaschaltet
  24.01.07 wl  gmSwitchModulePort           TN3473    Relays_Restore und Relays_execute entfernt (nicht mehr gleichzeitig)
  14.02.07 wl  gmCalculatePipStep           TN3147    Result ist jetzt immer nil (und nicht irgendetwas), wenn Position nicht existiert
  14.02.07 wl  gmCalculateXYStepBySinglePos TN3147    Result ist jetzt immer nil (und nicht irgendetwas), wenn Position nicht existiert
  09.11.07 pk                               TN3924    Steps changed to mm
  02.06.08 wl  gmSwitchModulePort           TN4131    SwitchDevice wird mit normaler Find-Funktion gesucht (SwitchNameCut wie früher nicht mehr nötig)
  03.07.08 wl                               TN4157
  16.07.08 pk  gmSetTipError                TN4157    moved to MotionSystemPipMove
  12.12.08 wl                               TN4363    ISwitchDevice.SwitchOnOrOff ersetzt
  12.12.08 wl  ResetLiqError                TN4363    entfernt
  08.09.09 pk                               TN4753    uses ErrorMessage replaced by ErrorInfo
  12.09.09 wl                               TN4740    TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  12.08.10 wl  gmStoreAsp                   TN5227    PumpIndex kann mit übergeben werden (wenn nötig)
  23.03.11 wl  gmCombinedArmsMoveToZTravel        TN5515   neu: enthält nur das Hochfahren der anderen Arme!
  29.03.11 wl                                     TN5524   TPipStepXYZCalculatorFactory
  29.03.11 wl                                     TN5524   OperationPip -> MotorStepCalculator
  12.10.11 ts  gmCalculatePipStep/-s              TN5711   if aAddTipOffset is true, Offsets from TipType will be added
  20.09.11 wl  gmStoreDsp                         TN5723   ruft TRack.SetPosVolumes auf
  28.10.11 wl  gmCalculatePipStep                 TN5725   TRack.PaintTubePositions entfernt
  28.10.11 wl  gmStoreAsp                         TN5725   ruft jetzt auch TRack.SetPosVolumes auf
  03.11.11 wl  gmStoreDsp, gmStoreAsp             TN5725   --> SubstanceLoading
  02.02.11 wl  alle Funktionen                    TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  07.09.12 ts                                     TN5973   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Rack
  28.09.12 ts  gmCalculatePipStep/-s              TN5987   new: MoveVarispan, Möglichkeit Varispan-Bewegung abzuschalten (für XYMOV-Action)
  15.02.13 wl  gmResetWashFlags                   TN6089   von SubstanceHandling hierher
  21.08.13 wl  gmIsPosReachableBySinglePos        TN6231   --> LayEvalu
  21.08.13 wl  gmIsPosReachable                   TN6231   --> BasicPipetteRunStepBatcher
  03.03.14 tp  gmMoveToZTravel                    TN6347  Varispan korrigierung implementiert mit aUsedArm overloaded Parameter
  -------------------------------------------------------------------------------------------------- }


interface


uses
    CommonTypes,
    GeneralTypes,
    AppTypes,
    Rack,
    RackTypes,
    TipSystem,
    IntfPipDevice,
    IntfMotorDevice,
    IntfArmDevice,
    IntfMultiZMotorDevice;

function gmParseSwitchPlace(aSwitchPlace: integer): TSwitchPlace;
procedure gmSwitchModulePort(aPipDevice: IPipDevice; aTips: TIPMAP; aSwitchName: string; aOn: boolean);

procedure gmCombinedArmsMoveToZTravel(aUsedArm: IArmDevice);
procedure gmMoveToZTravel(aUsedArm: IArmDevice; aZSpeed: integer = 0; aZRamp: integer = 0);

// Achtung! TXYStepList wird nie aus dem Speicher entfernt - könnte ein Speicherfresser sein!

procedure gmCalculatePipSteps(aUsedArm: IArmDevice; aSyrMap: TIPMAP; aRP: TArray<TXRackPosition>;
    var vXYResultList: TXYStepList; out oZResultArr: TPipStepZPosArray; aMoveVarispan: boolean = true);

function gmCalculatePipStep(aUsedArm: IArmDevice; aSyrMap: TIPMAP; aRP: TArray<TXRackPosition>;
    out oZResult: TPipStepZPos): TXYStep;

function gmCalculateXYStepBySinglePos(aUsedArm: IArmDevice; aIndex: integer; aX, aY: TPosMM): TXYStep;

function gmGetRackPosArrayForTips(aPipDevice: IPipDevice; aRack: TRack): TArray<TXRackPosition>;
procedure gmResetWashFlags(aPipDevice: IPipDevice; aTips: TIPMAP);


implementation


uses
    Windows,
    SysUtils,
    Controls,
    SamGlobe,
    ColorUtilities,
    ErrorManager,
    IntfSwitchDevice,
    ObjModul,
    OperationAxisMove,
    OperationFactory,
    MotorStepCalculator,
    LiquidManager,
    RunFlow,
    TipMapUtils;

function gmParseSwitchPlace(aSwitchPlace: integer): TSwitchPlace;
begin
    case (aSwitchPlace) of
        1:
            result := spSwitchOutside;
        2:
            result := spSwitchInside;
        else
            result := spDoNotSwitch;
    end;
end;

procedure SwitchOnOrOff(const aSwitchName: string; aOn: boolean);
var
    xDevice: ISwitchDevice;
begin
    if not gModules.Find_ByName(aSwitchName, ISwitchDevice, xDevice) then
        EXIT;
    if aOn then
        xDevice.SwitchOn(false)
    else
        xDevice.SwitchOff(false);
end;

procedure gmSwitchModulePort(aPipDevice: IPipDevice; aTips: TIPMAP; aSwitchName: string; aOn: boolean);
var
    xTipSwitchName: string;
    x: integer;
begin
    if (aSwitchName = '') then
        EXIT;
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    if (aSwitchName[Length(aSwitchName)] = STR_SWITCH_WILDCARD) then
    begin

        for x := 0 to aPipDevice.TipCount - 1 do
            if (((aTips shr x) and 1) = 1) then
            begin

                xTipSwitchName := Copy(aSwitchName, 1, Length(aSwitchName) - 1) + IntToStr(x + 1);
                SwitchOnOrOff(xTipSwitchName, aOn);
            end;
    end
    else
    begin
        // WL: Früher wurde hier mit FindSwitchByNameCut auch nach Wortanfängen gesucht. Das ist m.E. nicht nötig!
        SwitchOnOrOff(aSwitchName, aOn);
    end;
end;

procedure gmCombinedArmsMoveToZTravel(aUsedArm: IArmDevice);
var
    xZTravelOp: TZAxisTravelMoveOperation;
begin
    xZTravelOp := TOperationFactory.CreateZAxisTravelMoveOp(aUsedArm.MotionDevice, aUsedArm.ZTravelManager);
    xZTravelOp.CombinedArmsMoveToZTravel();
    FreeAndNil(xZTravelOp);
end;

procedure gmMoveToZTravel(aUsedArm: IArmDevice; aZSpeed, aZRamp: integer);
var
    xZTravelOp: TZAxisTravelMoveOperation;
begin
    xZTravelOp := TOperationFactory.CreateZAxisTravelMoveOp(aUsedArm.MotionDevice, aUsedArm.ZTravelManager,
        aUsedArm);
    xZTravelOp.MoveToZTravelAllTips(aZSpeed, aZRamp);
    FreeAndNil(xZTravelOp);
end;

function gmGetRackPosArrayForTips(aPipDevice: IPipDevice; aRack: TRack): TArray<TXRackPosition>;
var
    xPos: TArray<integer>;
    x: integer;
begin
    xPos := aPipDevice.PosArrayForTips;
    SetLength(result, Length(xPos));

    for x := 0 to high(xPos) do
    begin
        result[x].Rack := aRack;
        result[x].Pos := xPos[x];
    end;
end;

procedure gmCalculatePipSteps(aUsedArm: IArmDevice; aSyrMap: TIPMAP; aRP: TArray<TXRackPosition>;
    var vXYResultList: TXYStepList; out oZResultArr: TPipStepZPosArray; aMoveVarispan: boolean);
var
    xPipStepCalculator: TMotorXYZStepCalculator;
begin
    xPipStepCalculator := TMotorStepCalculatorFactory.CreateMotorXYZStepCalc(aUsedArm, aSyrMap);
    try
        vXYResultList := TXYStepList.Create;

        xPipStepCalculator.SetXYByRackPos(aRP);
        xPipStepCalculator.CalculateXYStepList(vXYResultList, true, aMoveVarispan);
        oZResultArr := xPipStepCalculator.CalcStepsZPosFromRackPos
            (xPipStepCalculator.GetMotorMapsFromSteps(vXYResultList), aRP);
    finally
        FreeAndNil(xPipStepCalculator);
    end;
end;

function gmCalculatePipStep(aUsedArm: IArmDevice; aSyrMap: TIPMAP; aRP: TArray<TXRackPosition>;
    out oZResult: TPipStepZPos): TXYStep;
var
    xPipStepCalculator: TMotorXYZStepCalculator;
begin
    xPipStepCalculator := TMotorStepCalculatorFactory.CreateMotorXYZStepCalc(aUsedArm, aSyrMap);
    try
        xPipStepCalculator.SetXYByRackPos(aRP);
        result := xPipStepCalculator.CalculateXYStep(true);
        if not Assigned(result) or (result.MotorMap <= 0) then
            EXIT;
        oZResult := xPipStepCalculator.CalcStepZPosFromRackPos(result.MotorMap, aRP);
    finally
        FreeAndNil(xPipStepCalculator);
    end;
end;

function gmCalculateXYStepBySinglePos(aUsedArm: IArmDevice; aIndex: integer; aX, aY: TPosMM): TXYStep;
var
    xPipStepCalculator: TMotorXYZStepCalculator;
    xTipMap: TIPMAP;
begin
    xTipMap := TTipMapUtils.EmptyTipMap();
    TTipMapUtils.SelectTip(xTipMap, aIndex);
    xPipStepCalculator := TMotorStepCalculatorFactory.CreateMotorXYZStepCalc(aUsedArm, xTipMap);
    try
        xPipStepCalculator.SetXYBySinglePos(aX, aY);
        result := xPipStepCalculator.CalculateXYStep(true);
        if Assigned(result) or (result.MotorMap <= 0) then
            EXIT;
    finally
        FreeAndNil(xPipStepCalculator);
    end;
end;

procedure gmResetWashFlags(aPipDevice: IPipDevice; aTips: TIPMAP);
var
    x: integer;
begin
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if TTipMapUtils.TipSelected(aTips, x) then
            aPipDevice.Tips[x].Wm_Clean;
    end;
end;


end.
