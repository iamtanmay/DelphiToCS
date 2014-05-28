{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : motion system to move the tips for pipetting
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                  track-no improvement/change
  -------- --  -------------------------------------   -------- ------------------------------------
  07.03.07 wl                                          TN3620    MotorSystem classes moved from OperationAxisMove to here
  07.03.07 wl  ZMotorsHaveBlockMoveOption              TN3620    ersetzt IsSias bei BlockMove (LiquidHandlingDiTi)
  08.03.07 wl  TZAxisPipMoveMotorMotionSystem.DoZMovement  TN3439   Disable errors --> TZAxisPipMoveOperation.DoZMovement
  12.03.07 pk                                          TN3628    use general TZAxisTravelMotionSystem objects instead of MotorMotionSystem
  11.04.07 pk  MoveToLowestTip                         TN3660   move all tips to the lowest detected z position
  19.04.07 wl  LiquidTips                              TN3675   3 neue Parameter für Simulationsmodus: Detektierte Höhe ist ein Random-Wert zwischen Start- und Endwert
  19.04.07 wl  DetectLiq                               TN3675   im Simulationsmodus keine Fehlermeldung
  07.08.07 wl                                          TN3811.3  TPosinfoAdapter.Create ohne Parameter
  09.11.07 pk                                          TN3924   Steps changed to mm
  09.11.07 pk                                          TN3923   class references to TErrorMessageFactory changed to gErrorMessageFactory
  13.11.07 wl  TZAxisPipMoveMotorMotionSystem.PowderDetection   TN3844    neu für Powder detection
  13.11.07 wl  TZAxisPipMoveMotorMotionSystem.PowderDetection   TN3844.1  funktioniert jetzt (wahrscheinlich)
  29.01.08 wl                                                   TN3980   uses geändert
  21.02.08 wl                                                   TN4009    Submerge wird jetzt überall negativ gerechnet
  21.02.08 wl                                                   TN4009    Abfrage, ob Z > ZMax in allen Funktionen mit DistanceToMin-Methode
  25.04.08 pk  CalcZPosDifference                               TN4086    New
  25.04.08 pk  LiquidTips                                       TN4086    Simulation Changed
  19.05.08 pk                                                   TN4086    GetRackZPosByVolume add level height by using AddZHeight
  04.06.08 pk  CalcTrackingSpeedByDistanceAndTime               TN4135    Speed of ZMotor for level tracking now calculated here
  11.06.08 wl  TZAxisPipMoveMotorMotionSystem.LiquidTips        TN4143    Timeout auch für ZP01
  20.06.08 pk                                                   TN4139    TWorkbench replaced by TLayout
  03.07.08 wl                                                   TN4157
  16.07.08 pk  SetTipLiqDetError, etc                           TN4157    New
  22.09.08 pk                                                   TN4157    for logging Tip+1
  27.11.08 wl  TZAxisPipMoveMotorMotionSystem.DoZMovement        TN4287    do not move deeper than Z-Max
  27.11.08 wl  TZAxisPipMoveMotorMotionSystem.GetMoveModeFromInt TN3950    neu: zur Übersetzung von ZInsertMoveType
  27.11.08 wl  TZAxisPipMoveMotorMotionSystem.DoZMovement        TN3626    new parameter: ZMoveType
  21.07.09 pk  GetRackZPosByDetectedLevel                        TN4667    New
  21.07.09 pk  MoveAspPos2, MoveDspPos2                          TN4667    calls GetRackZPosByDetectedLevel
  31.07.09 wl  MoveAsp2,MoveDisp2,MoveAspPowder,MoveDspPowder    TN3950  neu: ZInsertMoveType
  31.07.09 wl  ResetDisabledZErrors                              TN3950    Bug fixed, calles ResetDisabledZErrors instead of DisableZErrors
  31.07.09 wl  MoveAsp2,MoveDisp2,MoveAspPowder,MoveDspPowder    TN4018  neu: SingleTip ohne Liquid Detection
  05.08.09 wl  DoZMovement                                       TN4707   berechnet jetzt auch die Global-ZTravel-Werte
  10.08.09 wl                                                    TN4702   Strings werden jetzt direkt geladen
  26.08.09 wl  MoveZ_WithOffset                                  TN4730   Berechnung korrigiert (wird nur für Mix-Z-Offset benutzt)
  08.09.09 pk                                                    TN4753   uses ErrorMessage replaced by ErrorInfo
  12.09.09 wl                                                    TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  02.10.09 pk  MoveZ_WithOffset                                  TN4802   Copy Dest array instead of writing values directly to it
  02.10.09 pk  DetectLiq                                         TN4802   set vIsVol only for tips defined in tipmap
  06.10.09 pk  MoveDspPos2                                       TN4806   Level Calculation works again
  24.11.09 pk  MoveRetractDistance                               TN4900   moved up instead of down, now implemented with AddZHeight
  07.05.10 pk  InverseDetectBasic                                TN5093   New for ZPOSR Action
  07.05.10 pk  ReadCurrentPositions                              TN5093   New for ZPOSR Action
  07.05.10 pk  DoZDetect                                         TN5094   New for ZTIPD Action
  12.05.10 pk  RecordDetectedVolumes                             TN5094   New for ZTIPD Action
  23.08.10 wl  RecordDetectedVolumes                             TN5214   LiqFlags werden mit übergeben
  23.08.10 wl  MoveAspPos2                                       TN5214   liest und speichert die LiqFlags bei "Store detected volumes"
  26.08.10 ts  GetZPositionsFromZPositionType                    TN5249   wenn Z von PreviousStep undefiniert (0) ist, dann muss GlobalZTravel anstelle von 0 genommen werden (Koordinatensystem hat sich gedreht/gespiegelt)
  27.08.10 wl  RediZRetractSpeed                                 TN5250   entfernt
  30.11.10 wl  GetZPositionsFromZPositionType                    TN5374   Änderungen von TN5249 wieder rückgängig gemacht
  23.03.11 wl                                                    TN5515   MoveToZTravel: Methodenname geändert
  11.04.11 wl  CopyArray                                         TN5549   ist jetzt generisch
  02.09.11 ts  MoveAspPos2/MoveDspPos2                           TN5680   wenn ZErrorDisabled, dann Single Execute - wie ZPOSM
  13.09.11 ts                                                    TN5680   wenn ZErrorDisabled, dann Funktionsweise auch in Retract-function
  20.09.11 wl                                                    TN5723   CalcVolumeOfRackPos ohne SubstID
  03.11.11 wl  ReadDetectedVol, RecordVolumes                    TN5725   --> SubstanceLoading
  05.12.11 wl  MoveAspPos2                                       TN5758   wenn aCalculateIsVol = true: Berechnetes Volumen bei IsVol eintragen
  13.01.12 ts  IModuleMotorExecute                               TN5769   SingleExecuteWait (für ZP01 notwendig, sonst wird bei SingleExecute nicht gewartet)
  02.02.11 wl  alle Funktionen                                   TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  24.02.12 wl  TipTouch                                          TN5818   Neue Variante: Nicht Scannen, sondern auf berechnete Höhe + Submerge fahren
  28.02.12 wl  TipTouch                                          TN5818.1 Bugfix
  14.03.12 wl  GetZPositionsFromZPositionType                    TN5489   UsedTips wird reduziert, wenn es keine PrevZPos gibt
  04.05.12 wl  MoveAspPos2                                       TN5883   Beim Aufruf von RecordDetectedVolumes wurde statt Z-Höhen schon die Volumen übergeben
  07.05.12 wl  MoveDspPos2                                       TN5888   Bei vIsVol wurde über die Array-Grenze hinaus geschrieben
  07.05.12 wl  MoveAspPosPowder                                  TN5888   Bei aVol wurde über die Array-Grenze hinaus gelesen
  18.07.12 ts  MoveAspPos2                                       TN5921   SingleExecuteWait eingefügt, wenn ZError disabled
  23.07.12 wl  TipTouch                                          TN5922   vRetractMoveDone wird auf false gesetzt, weil die Nadeln ja noch mal runtergefahren werden
  06.08.12 wl  MoveAspPos2                                       TN5921   Compilierbar gemacht
  28.08.12 ts                                                    TN5943   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Tube
  07.05.13 ts  TipTouch                                          TN6118   Neues Feld: DispTipTouchScanStoreVol, Detektiertes Volumen kann in Posinfo gespeichert werden
  05.06.13 wl  GetZPositionsFromZPositionType                    TN6164   Exception, wenn PreviousArmPosition = nil und nicht GlobalZTravel
  06.06.13 wl                                                    TN6154   Motor-Disable wird jetzt immer mit ExecuteDisabledErrorsAndCheck ausgeführt
  04.09.13 wl  MoveAspPos2,MoveDspPos2                           TN6243   Workaround für Sampler.dll: Wenn Z-Motor.MapAllToFist, wird der Move-Befehl nur einmal geschickt
  27.11.13 wl                                                    TN6313   RTF_SINGLE_TUBE ersetzt durch aRack.IsMultiTipRack()
  ---------------------------------------------------------------------------------------------------------------------- }

unit MotionSystemPipMove;


interface


uses
    AppTypes,
    GeneralTypes,
    CommonTypes,
    Rack,
    RackTypes,
    ArmPositionInfo,
    IntfMotorDriver,
    IntfMultiZMotorDevice,
    MotionSystemTravel,
    IntfBalanceDevice;

type
    TDispAtPosType = (dapDispAtZMax, dapDispAtZDisp);

    TZAxisPipMoveMotorMotionSystem = class
    private
        function GetMoveModeFromInt(aMoveType: integer): ACTION_TYPE;
    protected
        fMotors: IMultiZMotorDevice;
        procedure MoveToLowestTip(aCTips, aMTips: TIPMAP);
        function HandleLiqDetError(aTipmap: integer; aNotEnough: boolean;
            const aDetectedVol, aExpectedVol: TDoubleArray): integer;
        function GetZPositionsFromZPositionType(const aPreviousArmPosition: TArmPositionInfo;
            var vUsedTips: TIPMAP; aZPosition: TRackZPositionType; aZSubmerge_mm: TPosMM): TDoubleArray;
    public
        constructor Create(aMotors: IMultiZMotorDevice);
        procedure MoveSingleZ(aTipIndex: integer; aDest: TPosMM; aExec: EXEC_MODE;
            aMode: ACTION_TYPE = AT_MOVE_ABS; aSpeed: integer = 0; aRamp: integer = 0);

        function Submerge(aMotorMap: TIPMAP; const aZMax: TDoubleArray; aSubMerge_mm: TPosMM): TIPMAP;
        // Z-Movement
        procedure DoZMovement(const aPreviousArmPosition: TArmPositionInfo; aUsedTips: TIPMAP;
            aZPosition: TRackZPositionType; aZSubmerge_mm: TPosMM; aZSpeed, aZRamp: integer;
            aZMoveType: integer);

        function DoZDetect(const aPreviousArmPosition: TArmPositionInfo; aUsedTips: TIPMAP;
            const aSingleTip: boolean; const aInverseDetect: boolean; aEndZPosition: TRackZPositionType;
            aEndZSubmerge_mm: TPosMM; const aZScanMode: integer; const aZScanSpeed, aZScanRamp: integer;
            const aStoreDifferenceAsVolume: boolean): TIPMAP;

        procedure PowderDetection(aPrevZStep: TPipStepZPos; aUsedTips: integer; aStepLength_mm: TPosMM;
            aZSpeed, aZRamp: integer; aZSubmerge_mm: TPosMM; aStopWeightAbs, aStopWeightRel: double;
            aBalance: IBalanceDevice);

        procedure DetectLiquidBasic(aMotors: TIPMAP; const aZMax: TDoubleArray; aSingleTip: boolean;
            aZScanMode, aZScanSpeed: integer);
        procedure DetectLiquid(aMotorMap: TIPMAP; const aZStart: TDoubleArray; const aZMax: TDoubleArray;
            aSingleTip: boolean; aZScanMode, aZScanSpeed: integer);
        function DetectLiq(aTipmap: TIPMAP; const aRacks: TArray<TRack>; aSubMerge_mm: TPosMM;
            const aVol: TDoubleArray; var vIsVol: TDoubleArray; aSingleTip: boolean;
            aZScanMode, aZScanSpeed: integer; const aZScan, aZMax, aZTube: TDoubleArray): TIPMAP;

        function InverseDetectBasic(aMotorMap: TIPMAP; const aEndPos: TDoubleArray; aSingleTip: boolean;
            aZScanMode, aZScanSpeed: integer): TIPMAP;

        function GetScanSpeeds(aMotorMap: TIPMAP; aSpeed: integer): TIntArray;
        procedure MoveSingleZ_Retract(aMotor: integer; aDest: TPosMM; aExec: EXEC_MODE;
            aRetractSpeed: integer; aRedi: boolean);
        function MoveRetractDistance(aMotorMap: TIPMAP; aZRetractSpeed: integer; aZRetractDistance_mm: TPosMM;
            aSingleRetract: boolean; aDispense, aDisableZErr: boolean): boolean;

        { // WL: wird überhaupt nicht benutzt
          function MoveRetractAndDetectClot( aMotorMap : TIPMAP; const aRack : TRack; const aPos: TIntArray;
          const aZMax, aZTube : TDoubleArray;
          aZRetractSpeed: integer; aZRetractDistance_mm: TPosMM;
          aSingleRetract: boolean; aZScanMode : integer): boolean;
        }
        procedure TipTouch(aMotorMap: TIPMAP; aRetractMotorMap: TIPMAP; const aRP: TArray<TXRackPosition>;
            aTipTouchDelay: integer; aTipTouchScan: boolean; aTipTouchZScanMode: integer;
            aTipTouchSingle: boolean; aZRetractSpeed: integer; aZRetractDistance_mm: TPosMM;
            const aZMax, aZTube, aZSubmerge: TDoubleArray; aZInsertSpeed: integer; aSingleRetract: boolean;
            aDispense: boolean; var vRetractMoveDone, aDisableZErr: boolean; aTipTouchScanStoreVol: boolean);

        function MoveAspPos2(aZTravelMotion: TZAxisTravelMotionSystem; aTipmap: TIPMAP; aOKTips: TIPMAP;
            const aRP: TArray<TXRackPosition>; aLQMode: integer; const aDetVol: TDoubleArray;
            aSubMerge_mm: TPosMM; var vIsVol: TDoubleArray; aCalculateIsVol: boolean;
            aRecordDetectionVolume: boolean; aZScanMode, aZInsertSpeed, aZInsertMoveType: integer;
            const aZScan, aZMax, aZTube: TDoubleArray; aDisableZError: boolean): TIPMAP;
        function MoveDspPos2(aZTravelMotion: TZAxisTravelMotionSystem; aTipmap: TIPMAP; aOKTips: TIPMAP;
            const aRP: TArray<TXRackPosition>; aLQMode: integer; aDspPos: TDispAtPosType;
            const aDetVol: TDoubleArray; var vIsVol: TDoubleArray; aSubMerge_mm: TPosMM;
            aZScanMode, aZInsertSpeed, aZInsertMoveType, aZRetractSpeed: integer;
            const aZScan, aZDisp, aZMax, aZTube: TDoubleArray; aDisableZError: boolean): TIPMAP;

        procedure MoveAspPosPowder(aTipmap: TIPMAP; aOKTips: TIPMAP; const aRP: TArray<TXRackPosition>;
            const aVol: TDoubleArray; aSubMerge_mm: TPosMM; aZInsertSpeed, aZInsertMoveType: integer;
            const aZScan, aZMax, aZTube: TDoubleArray; aNoCalculation: boolean; aSingleTip: boolean);

        function MoveDspPosPowder(aTipmap: TIPMAP; aOKTips: TIPMAP; const aRP: TArray<TXRackPosition>;
            aDspPos: TDispAtPosType; aSubMerge_mm: TPosMM;
            aZInsertSpeed, aZInsertMoveType, aZRetractSpeed: integer; aRedi: boolean;
            const aZScan, aZDisp, aZMax: TDoubleArray; aSingleTip: boolean): TIPMAP;

        procedure MoveZ(aMotorMap: TIPMAP; const aDest: TDoubleArray; aExec: EXEC_MODE;
            aMode: ACTION_TYPE = AT_MOVE_ABS; aSpeed: integer = 0; aRamp: integer = 0;
            aSingle: boolean = false);
        procedure MoveZ_MultiSpeed(aMotorMap: TIPMAP; const aDest: TDoubleArray; aExec: EXEC_MODE;
            aMode: ACTION_TYPE; const aSpeed: TIntArray; aRamp: integer; aSingle: boolean);
        procedure MoveZ_WithOffset(aMotorMap: TIPMAP; const aDest: TDoubleArray; aOffset_mm: TPosMM;
            aExec: EXEC_MODE; aMode: ACTION_TYPE = AT_MOVE_ABS; aSpeed: integer = 0; aRamp: integer = 0;
            aSingle: boolean = false);

        // diti
        procedure BlockMoveZ(aMotorMap: TIPMAP; const aDest: TDoubleArray; aSpeed: integer);

        // Z-Position Calculation
        function CalcZPosDifference(aTip: integer; aZHeight1, aZHeight2: TPosMM): TPosMM;
        function GetRackZPosByVolume(aTip: integer; aRack: TRack; aVol: extended;
            aZMax, aZTube: TPosMM): TPosMM;
        function GetRackZPosByVolCalc(aTip: integer; aHRack: TRack; aPos: integer; aZPosIfNoRackID: TPosMM;
            aDoNotCalc: boolean; aZMax, aZTube: TPosMM): TPosMM;
        function GetVolByRackZPos(aTip: integer; aHRack: TRack; aZ, aZTube: TPosMM): extended;
        function GetRackZPosByDetectedLevel(aTip: integer; aRack: TRack; aPos: integer;
            aZPosIfNoRackID: TPosMM; aZMax, aZTube: TPosMM): TPosMM;
        // TipPressure
        procedure MoveSingleZ_Scan(aMotor: integer; aDest: TPosMM; aExec: EXEC_MODE;
            aZScanMode, aZScanSpeed, aZScanRamp: integer);

        procedure MoveZ_WashRetract(aMotorMap: TIPMAP; const aDest: TDoubleArray; aWashRetractSpeed: integer);

        procedure MoveToTransportAirPos(aMotorMap: TIPMAP; const aTransAirVol: TDoubleArray;
            aZRetractSpeed: integer; aSingleRetract: boolean; const aTransAirZPos: TDoubleArray;
            aRetractMoveDone: boolean);

        function LiquidTips(const aMotorMap: TIPMAP; out oDetectedPos: TDoubleArray; aShouldDetect: boolean;
            const aStartPos, aEndPos: TDoubleArray): TIPMAP;

        procedure RecordDetectedVolumes(const aMotorMap: TIPMAP; const aZDetected, aZTube: TDoubleArray;
            const aRP: TArray<TXRackPosition>; const aStoreDifferenceAsVolume: boolean;
            const aLiqFlags: TTipLiqErrorTypeArray);

        function ReadCurrentPos(aIndex: integer): TPosMM;
        function ReadCurrentPositions(const aMotorMap: TIPMAP): TDoubleArray;
        procedure DisableZErrors(aMotorMap: TIPMAP);
        procedure ResetDisabledZErrors(aMotorMap: TIPMAP);
        procedure ResetAllDisabledZErrors();
        function ZErrorMotors(aMotorMap: TIPMAP): TIPMAP;
        function GetOKTips_ZP02(aMotorMap: TIPMAP; const aMaxPos: TDoubleArray): TIPMAP;

        function ZMotorsHaveBlockMoveOption(): boolean;
        function CalcTrackingSpeedByDistanceAndTime(aIndex: integer; aDistance: TPosMM;
            aTimeInSec: double): integer;

        procedure ResetTipLiqDetError(aTipmap: TIPMAP);
        function GetTipLiqDetError(aTipmap: TIPMAP): TTipLiqErrorTypeArray;

        function ExecuteWithDisabledZErrors(aDisabledMotors: TIPMAP): TIPMAP;

        procedure SingleExecute();
        procedure Execute();
        property Motors: IMultiZMotorDevice read fMotors;
    end;


implementation


uses
    SysUtils,
    Windows,
    Math,
    LogManager,
    SubstanceLoading,
    AppSettings,
    ErrorManager,
    RunFlow,
    Layout,
    ErrorMessageFactory,
    TipMapUtils,
    ErrorInfo,
    Carrier,
    ArrayUtils,
    LayoutManager;

{ TZAxisPipMoveMotorMotionSystem }

function TZAxisPipMoveMotorMotionSystem.GetMoveModeFromInt(aMoveType: integer): ACTION_TYPE;
begin
    case (aMoveType) of
        1:
            result := AT_BLOCK_MOVE1;
        2:
            result := AT_BLOCK_MOVE2;
        3:
            result := AT_BLOCK_MOVE3;
        else
            result := AT_MOVE_ABS;
    end;
end;

function TZAxisPipMoveMotorMotionSystem.ZMotorsHaveBlockMoveOption(): boolean;
begin
    result := fMotors.HaveBlockMoveOption;
end;

constructor TZAxisPipMoveMotorMotionSystem.Create(aMotors: IMultiZMotorDevice);
begin
    inherited Create();
    fMotors := aMotors;
end;

procedure TZAxisPipMoveMotorMotionSystem.MoveSingleZ(aTipIndex: integer; aDest: TPosMM; aExec: EXEC_MODE;
    aMode: ACTION_TYPE; aSpeed, aRamp: integer);
begin
    fMotors.MoveSingleZ(aTipIndex, aDest, aExec, aMode, aSpeed, aRamp);
end;

procedure TZAxisPipMoveMotorMotionSystem.Execute();
begin
    fMotors.Execute;
end;

procedure TZAxisPipMoveMotorMotionSystem.SingleExecute;
begin
    fMotors.SingleExecute;
end;

function TZAxisPipMoveMotorMotionSystem.ExecuteWithDisabledZErrors(aDisabledMotors: TIPMAP): TIPMAP;
begin
    EXIT(fMotors.ExecuteDisabledErrorsAndCheck(aDisabledMotors));
end;

function TZAxisPipMoveMotorMotionSystem.CalcZPosDifference(aTip: integer;
    aZHeight1, aZHeight2: TPosMM): TPosMM;
begin
    // This calculation simply subtracts aZHeight2 from aZHeight1 (i.e. aZHeight1 - aZHeight2) and should give a
    // positive value if aZHeight2 is physically higher than aZHeight1
    result := fMotors.DistanceToMin(aTip, aZHeight1) - fMotors.DistanceToMin(aTip, aZHeight2)
end;

function TZAxisPipMoveMotorMotionSystem.GetRackZPosByVolume(aTip: integer; aRack: TRack; aVol: extended;
    aZMax, aZTube: TPosMM): TPosMM;
var
    xTubeLevelHeight: TPosMM;
begin
    xTubeLevelHeight := (aVol / aRack.WellSurface_mm2);
    result := AddZHeight(aZTube, xTubeLevelHeight);

    // if result is physically lower than zmax set result to zmax
    if CalcZPosDifference(aTip, result, aZMax) > 0 then
        result := aZMax;

    gLogManager.LogF('Calculate Height: Tip: %d, Input Volume: %f --> ZPos: %f',
        [aTip + 1, aVol, result], false);
end;

function TZAxisPipMoveMotorMotionSystem.GetRackZPosByDetectedLevel(aTip: integer; aRack: TRack; aPos: integer;
    aZPosIfNoRackID: TPosMM; aZMax, aZTube: TPosMM): TPosMM;
var
    xVolume: extended;
begin
    result := aZPosIfNoRackID;
    if (aRack = nil) or (aRack.RackID = '') then
    begin
        gLogManager.LogF('No RackID - Go to position %f', [aZPosIfNoRackID], false);
        EXIT;
    end;

    if not TSubstanceLoading.ReadDetectedVol(aRack, aPos, xVolume) then
        EXIT;

    gLogManager.LogF('Volume in RackID %s, Position %d --> %f uL found',
        [aRack.RackID, aPos, xVolume], false);

    result := GetRackZPosByVolume(aTip, aRack, xVolume, aZMax, aZTube);
end;

function TZAxisPipMoveMotorMotionSystem.GetRackZPosByVolCalc(aTip: integer; aHRack: TRack; aPos: integer;
    aZPosIfNoRackID: TPosMM; aDoNotCalc: boolean; aZMax, aZTube: TPosMM): TPosMM;
var
    xVolume: extended;
begin
    if (aHRack = nil) or (aHRack.RackID = '') or (aDoNotCalc) then
    begin
        gLogManager.LogF('No Position Calculation - Go to position %f', [aZPosIfNoRackID], false);
        result := aZPosIfNoRackID;
        EXIT;
    end;

    xVolume := aHRack.CalcVolumeOfRackPos(aPos);
    gLogManager.LogF('Substance Volume in RackID %s, Position %d --> %f uL found',
        [aHRack.RackID, aPos, xVolume], false);

    result := GetRackZPosByVolume(aTip, aHRack, xVolume, aZMax, aZTube);
end;

function TZAxisPipMoveMotorMotionSystem.GetVolByRackZPos(aTip: integer; aHRack: TRack; aZ, aZTube: TPosMM)
    : extended;
var
    xVol: extended;
    xHeight: TPosMM;

begin
    xHeight := CalcZPosDifference(aTip, aZTube, aZ);
    xVol := xHeight * aHRack.WellSurface_mm2;
    if xVol < 0 then
        xVol := 0;
    result := Round(10 * xVol) / 10;;

    gLogManager.LogF('Calculate Volume: Tip: %d, Tube bottom: %f, Input ZHeight: %f --> Volume: %d',
        [aTip + 1, aZTube, aZ, round(xVol)], false);
end;

function TZAxisPipMoveMotorMotionSystem.GetScanSpeeds(aMotorMap: TIPMAP; aSpeed: integer): TIntArray;
begin
    result := fMotors.GetScanSpeeds(aMotorMap, aSpeed);
end;

function TZAxisPipMoveMotorMotionSystem.GetZPositionsFromZPositionType(const aPreviousArmPosition
    : TArmPositionInfo; var vUsedTips: TIPMAP; aZPosition: TRackZPositionType; aZSubmerge_mm: TPosMM)
    : TDoubleArray;
var
    xPrevRP: TArray<TXRackPosition>;
    xPrevZStep: TPipStepZPos;
    x: integer;
begin
    // im Zweifelsfall: auf GlobalZTravel bleiben
    result := fMotors.CalcZTravel(vUsedTips);

    for x := 0 to fMotors.MaxIndex do
    begin
        if not TTipMapUtils.TipSelected(vUsedTips, x) then
            CONTINUE;

        // Spezialfall GlobalZTravel: Keine Previous Position notwendig
        if (aZPosition = zptGlobalZTravel) then
        begin
            // Add Submerge
            result[x] := AddZHeight(result[x], -aZSubmerge_mm);
            CONTINUE;
        end;

        // PreviousArmPosition muss definiert sein
        if not Assigned(aPreviousArmPosition) then
            raise Exception.Create('Z-Movement must bo done at a defined rack position!');
        xPrevRP := aPreviousArmPosition.RP;
        xPrevZStep := aPreviousArmPosition.ZStep;

        // vorhergehende Position ist undefiniert
        if not TTipMapUtils.TipSelected(xPrevZStep.SyrMap, x) then
        begin
            TLogManager.Instance.Log('Tip ' + IntToStr(x + 1) +
                ' has an undefined position. It will not be used!', true);
            TTipMapUtils.UnselectTip(vUsedTips, x);
            CONTINUE;
        end;

        case (aZPosition) of
            zptRackZTravel:
                result[x] := xPrevZStep.Z.ZTravel[x];
            zptZScan:
                result[x] := xPrevZStep.Z.ZScan[x];
            zptZDispense:
                result[x] := xPrevZStep.Z.ZDisp[x];
            zptZMaximum:
                result[x] := xPrevZStep.Z.ZMax[x];
            zptZMoveOffset:
                result[x] := xPrevZStep.Z.MOffset[x];
            zptCalculated:
                begin
                    result[x] := GetRackZPosByVolCalc(x, xPrevRP[x].Rack, xPrevRP[x].Pos,
                        xPrevZStep.Z.ZMax[x], false, xPrevZStep.Z.ZMax[x], xPrevZStep.Z.ZTube[x]);
                end;
            else
                raise Exception.Create('ZPosMovement: Position undefined');
        end;

        // Add Submerge:
        result[x] := AddZHeight(result[x], -aZSubmerge_mm);

        // result cannot be deeper than Z-Max
        if (fMotors.DistanceToMin(x, result[x]) > fMotors.DistanceToMin(x, xPrevZStep.Z.ZMax[x])) then
        begin
            gLogManager.LogF('Tip ' + IntToStr(x + 1) +
                ': Z-Position %f is below Z-Max %f, Z-Position will be set to Z-Max',
                [result[x], xPrevZStep.Z.ZMax[x]], true);
            result[x] := xPrevZStep.Z.ZMax[x];
        end;
    end;
end;

procedure TZAxisPipMoveMotorMotionSystem.DoZMovement(const aPreviousArmPosition: TArmPositionInfo;
    aUsedTips: TIPMAP; aZPosition: TRackZPositionType; aZSubmerge_mm: TPosMM; aZSpeed, aZRamp: integer;
    aZMoveType: integer);
var
    xDest: TDoubleArray;
begin
    xDest := GetZPositionsFromZPositionType(aPreviousArmPosition, aUsedTips, aZPosition, aZSubmerge_mm);

    // send move command (no execute)
    self.MoveZ(aUsedTips, xDest, m_NO_EXEC, GetMoveModeFromInt(aZMoveType), aZSpeed, aZRamp);
end;

function TZAxisPipMoveMotorMotionSystem.DoZDetect(const aPreviousArmPosition: TArmPositionInfo;
    aUsedTips: TIPMAP; const aSingleTip: boolean; const aInverseDetect: boolean;
    aEndZPosition: TRackZPositionType; aEndZSubmerge_mm: TPosMM; const aZScanMode: integer;
    const aZScanSpeed, aZScanRamp: integer; const aStoreDifferenceAsVolume: boolean): TIPMAP;
var
    xDetectedPos: TDoubleArray;
    xCurrentZPositions: TDoubleArray;
    xDest: TDoubleArray;
begin
    // PreviousArmPosition muss definiert sein
    if not Assigned(aPreviousArmPosition) then
        raise Exception.Create('Z-Movement must bo done at a defined rack position!');

    xDest := GetZPositionsFromZPositionType(aPreviousArmPosition, aUsedTips, aEndZPosition, aEndZSubmerge_mm);

    if aInverseDetect then
    begin
        result := self.InverseDetectBasic(aUsedTips, xDest, aSingleTip, aZScanMode, aZScanSpeed);
    end
    else
    begin
        xCurrentZPositions := self.ReadCurrentPositions(aUsedTips);
        self.DetectLiquid(aUsedTips, xCurrentZPositions, xDest, aSingleTip, aZScanMode, aZScanSpeed);
        result := self.LiquidTips(aUsedTips, xDetectedPos, true, xCurrentZPositions, xDest);
        RecordDetectedVolumes(aUsedTips, xDetectedPos, aPreviousArmPosition.ZStep.Z.ZTube,
            aPreviousArmPosition.RP, aStoreDifferenceAsVolume, nil);
    end;
end;

procedure TZAxisPipMoveMotorMotionSystem.RecordDetectedVolumes(const aMotorMap: TIPMAP;
    const aZDetected, aZTube: TDoubleArray; const aRP: TArray<TXRackPosition>;
    const aStoreDifferenceAsVolume: boolean; const aLiqFlags: TTipLiqErrorTypeArray);
var
    x: integer;
    xLevel: TPosMM;
    xVolumes, xCalcVolumes: TDoubleArray;
begin
    xVolumes := fMotors.GetNullDoubleArray();
    xCalcVolumes := fMotors.GetNullDoubleArray();
    for x := 0 to fMotors.MaxIndex do
    begin
        if not TTipMapUtils.TipSelected(aMotorMap, x) then
            CONTINUE;
        xLevel := aZDetected[x];
        gLogManager.LogF('Tip %d, detected level %f', [x + 1, xLevel], false);
        xVolumes[x] := self.GetVolByRackZPos(x, aRP[x].Rack, xLevel, aZTube[x]);

        if (aStoreDifferenceAsVolume) then
            xCalcVolumes[x] := aRP[x].Rack.CalcVolumeOfRackPos(aRP[x].Pos);
    end;
    TSubstanceLoading.Instance.RecordVolumes(aRP, aMotorMap, xVolumes, xCalcVolumes, aStoreDifferenceAsVolume,
        aLiqFlags);
end;

procedure TZAxisPipMoveMotorMotionSystem.PowderDetection(aPrevZStep: TPipStepZPos; aUsedTips: integer;
    aStepLength_mm: TPosMM; aZSpeed, aZRamp: integer; aZSubmerge_mm: TPosMM;
    aStopWeightAbs, aStopWeightRel: double; aBalance: IBalanceDevice);
var
    x: integer;
    xZPos_mm: TPosMM;
    xWeight, xLastWeight, xDifference, xStartWeight: double;
    xWeighStep: integer;
begin
    for x := 0 to fMotors.MaxIndex do
    begin
        if not TTipMapUtils.TipSelected(aUsedTips, x) then
            CONTINUE;

        xStartWeight := aBalance.SimpleReadWeight_mg();
        xZPos_mm := self.ReadCurrentPos(x);

        gLogManager.Log('Powder detection - START', true);
        xWeighStep := 0;
        xLastWeight := 0;
        while (not gErrorManager.IsGlobalErr()) do
        begin

            xZPos_mm := xZPos_mm + aStepLength_mm;

            // Z-Max darf nicht überschritten werden!
            if (fMotors.DistanceToMin(x, xZPos_mm) > fMotors.DistanceToMin(x, aPrevZStep.Z.ZMax[x])) then
                BREAK;

            // send move command (no execute)
            self.MoveSingleZ(x, xZPos_mm, m_EXECUTE, AT_MOVE_ABS, aZSpeed, aZRamp);
            // jede Aktion wird einzeln ausgeführt

            // Wiegewert ablesen
            xWeight := aBalance.SimpleReadWeight_mg();
            inc(xWeighStep);

            if (xWeighStep > 1) then
            begin
                xDifference := xWeight - xLastWeight;
                gLogManager.LogF('Weight difference %f mg', [xDifference], false);
                if (xDifference > aStopWeightRel) then
                begin
                    gLogManager.LogF('Powder detected: Weight difference %f mg > %f mg',
                        [xDifference, aStopWeightRel], true);
                    BREAK;
                end;
                if (xWeight - xStartWeight > aStopWeightAbs) then
                begin
                    gLogManager.LogF('Powder detected: Weight %f mg higher than start value %f mg',
                        [xWeight, xStartWeight], true);
                    BREAK;
                end;
            end;
            xLastWeight := xWeight;
        end;
        gLogManager.Log('Powder detection - END', true);

        // Move to Pos + Submerge (außer wenn Z-Max bereits erreicht wurde)
        if (aZSubmerge_mm <> 0) then
        begin
            gLogManager.Log('Powder detection - Go to submerge position', true);
            xZPos_mm := AddZHeight(xZPos_mm, -aZSubmerge_mm);

            if (fMotors.DistanceToMin(x, xZPos_mm) <= fMotors.DistanceToMin(x, aPrevZStep.Z.ZMax[x])) then
            begin
                self.MoveSingleZ(x, xZPos_mm, m_EXECUTE, AT_MOVE_ABS, aZSpeed, aZRamp);
                // jede Aktion wird einzeln ausgeführt
            end;
        end;
    end;
end;

procedure TZAxisPipMoveMotorMotionSystem.DetectLiquidBasic(aMotors: TIPMAP; const aZMax: TDoubleArray;
    aSingleTip: boolean; aZScanMode, aZScanSpeed: integer);
var
    x: integer;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT; // return on Error

    for x := 0 to fMotors.MaxIndex do
        if (((aMotors shr x) and 1) <> 0) then
        begin
            MoveSingleZ_Scan(x, aZMax[x], m_NO_EXEC, aZScanMode, aZScanSpeed, 0);
            if (aSingleTip) then
                self.Execute;
        end;

    self.Execute;
end;

function TZAxisPipMoveMotorMotionSystem.InverseDetectBasic(aMotorMap: TIPMAP; const aEndPos: TDoubleArray;
    aSingleTip: boolean; aZScanMode, aZScanSpeed: integer): TIPMAP;
const
    cZInterval = 2;
var
    x: integer;
    xStartingPos, xCurrentPos, xCurrentEndPos, xDetectedPos: TDoubleArray;
    xWetTips: TIPMAP;
    xMotorsToDetect: TIPMAP;
    xTryCount: integer;
    xDryTips: TIPMAP;
    xShouldDetect: boolean;
begin
    result := TTipMapUtils.EmptyTipMap;

    if gErrorManager.IsGlobalErr() then
        EXIT; // return on Error
    Randomize();

    xDetectedPos := fMotors.GetNullDoubleArray;
    xCurrentPos := fMotors.GetNullDoubleArray;
    xCurrentEndPos := fMotors.GetNullDoubleArray;

    xStartingPos := self.ReadCurrentPositions(aMotorMap);

    xMotorsToDetect := aMotorMap;
    xDryTips := 0;

    xTryCount := 1;

    while true do
    begin

        if (gErrorManager.IsGlobalErr) then
            EXIT;

        for x := 0 to fMotors.MaxIndex do
        begin
            if not TTipMapUtils.TipSelected(xMotorsToDetect, x) then
                CONTINUE;
            xCurrentPos[x] := fMotors.ReadCurrentPos(x);
            if xCurrentPos[x] >= aEndPos[x] then
            begin
                TTipMapUtils.UnselectTip(xMotorsToDetect, x);
            end;

            xCurrentEndPos[x] := AddZHeight(xCurrentPos[x], cZInterval);
            if xCurrentEndPos[x] >= aEndPos[x] then
            begin
                xCurrentEndPos[x] := aEndPos[x];
            end;
        end;
        if xMotorsToDetect = 0 then
            BREAK;

        gLogManager.Instance.Log(Format('Detect Dryness Try: %d', [xTryCount]), false);

        // DetectLiquidBasic( xMotorsToDetect, xCurrentEndPos, aSingleTip, aZScanMode, aZScanSpeed );
        self.MoveZ(xMotorsToDetect, xCurrentEndPos, m_EXECUTE);
        xShouldDetect := RandomRange(0, 2) = 1;
        xWetTips := self.LiquidTips(xMotorsToDetect, xDetectedPos, xShouldDetect, xCurrentPos, xCurrentPos);

        for x := 0 to fMotors.MaxIndex do
        begin
            if not TTipMapUtils.TipSelected(xMotorsToDetect, x) then
                CONTINUE;
            if not TTipMapUtils.TipSelected(xWetTips, x) then
            begin
                TTipMapUtils.SelectTip(xDryTips, x);
                TTipMapUtils.UnselectTip(xMotorsToDetect, x);
            end;
        end;

        if xMotorsToDetect = 0 then
            BREAK;

        inc(xTryCount);
    end;

    // at the end do a detection to move down to correct level
    self.DetectLiquidBasic(xDryTips, xStartingPos, aSingleTip, aZScanMode, aZScanSpeed);
    // self.LiquidTips( xDryTips, xDetectedPos, false, xCurrentPos, xCurrentEndPos );
    result := xDryTips;

end;

procedure TZAxisPipMoveMotorMotionSystem.DetectLiquid(aMotorMap: TIPMAP; const aZStart: TDoubleArray;
    const aZMax: TDoubleArray; aSingleTip: boolean; aZScanMode, aZScanSpeed: integer);
begin
    if gErrorManager.IsGlobalErr() then
        EXIT; // return on Error

    MoveZ(aMotorMap, aZStart, m_EXECUTE); // Move to start of scan

    self.DetectLiquidBasic(aMotorMap, aZMax, aSingleTip, aZScanMode, aZScanSpeed);
end;

procedure TZAxisPipMoveMotorMotionSystem.MoveSingleZ_Retract(aMotor: integer; aDest: TPosMM; aExec: EXEC_MODE;
    aRetractSpeed: integer; aRedi: boolean);
begin
    fMotors.MoveSingleZ_Retract(aMotor, aDest, aExec, aRetractSpeed);
end;

function TZAxisPipMoveMotorMotionSystem.MoveRetractDistance(aMotorMap: TIPMAP; aZRetractSpeed: integer;
    aZRetractDistance_mm: TPosMM; aSingleRetract: boolean; aDispense, aDisableZErr: boolean): boolean;
var
    x: integer;
    xZPos: TPosMM;
begin
    result := false;
    if (gErrorManager.IsGlobalErr) then
        EXIT;
    if (aZRetractDistance_mm <= 0) then
        EXIT;

    // if RetractDistance_mm is defined: Relative movement for that distance
    for x := 0 to fMotors.MaxIndex do
    begin
        if not TTipMapUtils.TipSelected(aMotorMap, x) then
            CONTINUE;

        // Kein Retract wenn bei Systemverteilung weitere Hübe notwendig sind!!!
        // if (aDispense) and (fUsedArm.Tips[x].St_SaveDilVol > 0) then CONTINUE;

        // calculate absolute position to go
        xZPos := AddZHeight(fMotors.ReadCurrentPos(x), aZRetractDistance_mm);

        // make sure we dont go past min
        fMotors.SetToMinPosIfIsBeyondMin(x, xZPos);

        MoveSingleZ_Retract(x, xZPos, m_NO_EXEC, aZRetractSpeed, false);

        if (aSingleRetract) then
        begin
            gRunFlow.AppSleep(500); // Single Retract: wait 500 msec
            if (aDisableZErr) then
            begin
                fMotors.ExecuteDisabledErrorsAndCheck(aMotorMap);
                // notwendig? Am Schluss wird noch einmal DisableErrors ausgeführt
                fMotors.DisableErrors(aMotorMap);
            end
            else
            begin
                self.Execute;
            end;
        end;
    end;
    if (aDisableZErr) then
    begin
        fMotors.ExecuteDisabledErrorsAndCheck(aMotorMap);
        // notwendig? Am Schluss wird noch einmal DisableErrors ausgeführt
        fMotors.DisableErrors(aMotorMap);
    end
    else
    begin
        self.Execute;
    end;
    result := true;
end;

{
  function TZAxisPipMoveMotorMotionSystem.MoveRetractAndDetectClot( aMotorMap : TIPMAP; const aRack : TRack; const aPos: TIntArray;
  const aZMax, aZTube : TDoubleArray;
  aZRetractSpeed: integer; aZRetractDistance_mm: TPosMM; aSingleRetract: boolean; aZScanMode : integer ): boolean;
  var x: integer;
  xZPos : TDoubleArray;
  begin
  result := false;
  if (gErrorManager.IsGlobalErr) then EXIT;
  if (aZRetractDistance_mm <= 0) then EXIT;

  xZPos := fMotors.GetNullDoubleArray();

  for x := 0 to fMotors.MaxIndex do begin
  if not TTipMapUtils.TipSelected( aMotorMap, x ) then CONTINUE;
  xZPos[x] := self.GetRackZPosByDetectedLevel( x, aRack, aPos[x], aZMax[x], aZMax[x], aZTube[x] );
  end;
  self.MoveZ( aMotorMap, xZPos, m_EXECUTE );

  // if RetractDistance_mm is defined: Relative movement for that distance
  for x := 0 to fMotors.MaxIndex do begin
  if not TTipMapUtils.TipSelected( aMotorMap, x ) then CONTINUE;
  xZPos[x] := AddZHeight( fMotors.ReadCurrentPos( x ), aZRetractDistance_mm );
  fMotors.SetToMinPosIfIsBeyondMin( x, xZPos[x] );
  end;

  InverseDetectBasic( aMotorMap, xZPos, true, aZScanMode, aZRetractSpeed );

  result := true;
  end;
}
procedure TZAxisPipMoveMotorMotionSystem.TipTouch(aMotorMap: TIPMAP; aRetractMotorMap: TIPMAP;
    const aRP: TArray<TXRackPosition>; aTipTouchDelay: integer; aTipTouchScan: boolean;
    aTipTouchZScanMode: integer; aTipTouchSingle: boolean; aZRetractSpeed: integer;
    aZRetractDistance_mm: TPosMM; const aZMax, aZTube, aZSubmerge: TDoubleArray; aZInsertSpeed: integer;
    aSingleRetract: boolean; aDispense: boolean; var vRetractMoveDone, aDisableZErr: boolean;
    aTipTouchScanStoreVol: boolean);
var
    xDetectArray, xZPosArr, xCurrentZPositions: TArray<double>;
    x: integer;
begin
    if not vRetractMoveDone then
        self.MoveRetractDistance(aMotorMap, aZRetractSpeed, aZRetractDistance_mm, aSingleRetract, false,
            aDisableZErr);

    // zurücksetzen von RetractMoveDone, weil jetzt noch mal runtergefahren wird
    vRetractMoveDone := false;

    if aTipTouchScan then
    begin
        if aTipTouchScanStoreVol then
        begin
            xCurrentZPositions := self.ReadCurrentPositions(aMotorMap);
            self.DetectLiquid(aMotorMap, xCurrentZPositions, aZMax, aTipTouchSingle, aTipTouchZScanMode,
                aZInsertSpeed);
            self.LiquidTips(aMotorMap, xDetectArray, true, xCurrentZPositions, aZMax);
            RecordDetectedVolumes(aMotorMap, xDetectArray, aZTube, aRP, false, nil);
        end
        else
        begin
            xDetectArray := TArrayUtils.CopyArray<double>(aZMax);
            self.DetectLiquidBasic(aMotorMap, xDetectArray, aTipTouchSingle, aTipTouchZScanMode,
                aZInsertSpeed);
        end;
    end
    else
    begin
        xZPosArr := fMotors.GetNullDoubleArray();
        for x := 0 to fMotors.MaxIndex do
        begin
            if not TTipMapUtils.TipSelected(aMotorMap, x) then
                CONTINUE;

            xZPosArr[x] := GetRackZPosByVolCalc(x, aRP[x].Rack, aRP[x].Pos, aZMax[x], false, aZMax[x],
                aZTube[x]);

            // Add Submerge:
            xZPosArr[x] := AddZHeight(xZPosArr[x], -aZSubmerge[x]);

            if (fMotors.DistanceToMin(x, xZPosArr[x]) > fMotors.DistanceToMin(x, aZMax[x])) then
            begin
                xZPosArr[x] := aZMax[x];
                gLogManager.LogF('ZPOSM: Z-Position %f > Z-Max %f, Z-Position will be set to Z-Max',
                    [xZPosArr[x], aZMax[x]], false);
            end;
        end;
        self.MoveZ(aMotorMap, xZPosArr, m_NO_EXEC, AT_MOVE_ABS, 0, 0, aTipTouchSingle);
    end;

    if (aTipTouchDelay > 0) then
    begin
        self.Execute(); // Unsinn??? wl
        gRunFlow.AppSleep(aTipTouchDelay);
    end;
end;

// --------------------------------------------------------------------------------------------------
// WORD DetectLiq(TIPMAP TipMap,HRACK hRack,integer Sub,float *Vol,float *IsVol)
//
// This function will pick liquid from the defined rack using the
// corrected tuberarea for rectangular of diecret position racks
// The Tube Area depends on the Racktype:
// Rtype=0: Rectangular Rack, one Tube used Tip ==>
// ValidTubeArea = Tubearea of one Tube
// Rtype>0: Rectangular Rack, one Tube for all used Tips
// ValidTubeArea = Tubearea of one Tube  / No of used tips
//
// --------------------------------------------------------------------------------------------------
function TZAxisPipMoveMotorMotionSystem.DetectLiq(aTipmap: TIPMAP; const aRacks: TArray<TRack>;
    aSubMerge_mm: TPosMM; const aVol: TDoubleArray; var vIsVol: TDoubleArray; aSingleTip: boolean;
    aZScanMode, aZScanSpeed: integer; const aZScan, aZMax, aZTube: TDoubleArray): TIPMAP;
// --------------------------------------------------------------------------------------------------
var
    xZMax: TDoubleArray;
    x: integer;
    xIntVolume: TIntArray;
begin
    // bis auf Z-Max runterfahren ( Volume = 0 )
    xZMax := fMotors.GetNullDoubleArray;
    for x := 0 to fMotors.MaxIndex do
        if (((aTipmap shr x) and 1) <> 0) then
        begin
            xZMax[x] := self.GetRackZPosByVolume(x, aRacks[x], 0, aZMax[x], aZTube[x]) + aSubMerge_mm;
        end;

    { if ((aHRack.Structure.TubeTyp and RTF_SINGLE_TUBE) <> 0)  then begin // single tip tubes
      for x := 0 to fMotors.MaxIndex do if (((aTipMap shr x) and 1) <> 0) then begin
      xZMax[x] := self.GetRackZPosByVolume( x, aHRack, aVol[x], aZMax[x], aZTube[x] ) - fMotors.GetStepsFromUnit( x, aSubmerge_mm );
      end;
      end
      else begin	// all tips in same liquid
      xv := 0;

      // all in same tube ==> calculate sum of all liquids to take
      for x := 0 to fMotors.MaxIndex do if (((aTipMap shr x) and 1) <> 0) then xv := xv + aVol[x];
      for x := 0 to fMotors.MaxIndex do if (((aTipMap shr x) and 1) <> 0) then begin
      xZMax[x] := self.GetRackZPosByVolume( x, ahRack, xv, aZMax[x], aZTube[x] ) - fMotors.GetStepsFromUnit( x, aSubmerge_mm );
      end;
      end; }

    self.DetectLiquid(aTipmap, aZScan, xZMax, aSingleTip, aZScanMode, aZScanSpeed);
    aTipmap := self.LiquidTips(aTipmap, xZMax, true, aZScan, xZMax);

    xIntVolume := fMotors.GetNullIntArray();

    for x := 0 to fMotors.MaxIndex do
    begin
        if TTipMapUtils.TipSelected(aTipmap, x) then
        begin
            vIsVol[x] := self.GetVolByRackZPos(x, aRacks[x], xZMax[x], aZTube[x]);

            // MultiTipTubes sind hier nicht (?) berücksichtigt
            if (not gRunFlow.SimulationMode) and (vIsVol[x] < aVol[x]) then
                // Detektiert, aber zu wenig Volumen: wird auch als Fehler gerechnet
                TTipMapUtils.UnselectTip(aTipmap, x);

            xIntVolume[x] := round(vIsVol[x]); // nur für Log als integer
        end;
    end;

    gLogManager.Log('Detected Volumes: ' + TArrayUtils.ArrayToBracketText(xIntVolume), true);

    result := aTipmap;
end;

function TZAxisPipMoveMotorMotionSystem.Submerge(aMotorMap: TIPMAP; const aZMax: TDoubleArray;
    aSubMerge_mm: TPosMM): TIPMAP;
var
    x: integer;
    xDest: TPosMM;
    xZPos: TDoubleArray;
begin
    gLogManager.LogF('Execute Submerge %f; TTips: %d', [aSubMerge_mm, aMotorMap], false);
    // ------------------------------------------------------------- Submerge
    xZPos := fMotors.GetNullDoubleArray;
    for x := 0 to fMotors.MaxIndex do
    begin
        if not TTipMapUtils.TipSelected(aMotorMap, x) then
            CONTINUE;

        xZPos[x] := -aSubMerge_mm;
        xDest := AddZHeight(fMotors.ReadCurrentPos(x), xZPos[x]);

        // nicht über Z-Max !
        if (self.CalcZPosDifference(x, xDest, aZMax[x]) >= 0) then
        begin
            xDest := aZMax[x];
            aMotorMap := aMotorMap and not(1 shl x); // kein Tracking !
        end;
        self.MoveSingleZ(x, xDest, m_NO_EXEC, AT_MOVE_ABS, 0);
    end;

    self.Execute;
    result := aMotorMap;
end;

// --------------------------------------------------------------------------------------------------
// MoveToLowestTip(WORD CTips,WORD MTips,WORD Zmin);                       _/
// _/
// This function will check the positions of all in CTips defined Z Motors _/
// and will move all with MTips defined Motors to the same height as the   _/
// lowest tip of Check Tips.                                               _/
// _/
// CTips    selector for the tips to check                            _/
// MTips    selector for the move tips                                _/
// Zmin     minimum Z value to move to                                _/
// --------------------------------------------------------------------------------------------------
procedure TZAxisPipMoveMotorMotionSystem.MoveToLowestTip(aCTips, aMTips: TIPMAP);
// --------------------------------------------------------------------------------------------------
var
    x, xTip: integer;
    xZPos: TDoubleArray;
    xPos, xZMin: TPosMM;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    xZMin := 0;

    // all tip in same liquid, at least one tip has detected liquid
    for xTip := 0 to fMotors.MaxIndex do
    begin
        if (((aCTips shr xTip) and 1) <> 0) then
        begin // we should check the tips
            xPos := self.ReadCurrentPos(xTip);
            if (fMotors.DistanceToMin(xTip, xPos) > fMotors.DistanceToMin(xTip, xZMin)) then
                xZMin := xPos; // tip is lower ?
        end;
    end;

    if xZMin <= 0 then
        EXIT;

    // all tips to same level
    xZPos := fMotors.GetNullDoubleArray;
    for x := 0 to fMotors.MaxIndex do
    begin
        if not TTipMapUtils.TipSelected(aMTips, x) then
            CONTINUE;
        xZPos[x] := xZMin;
    end;

    self.MoveZ(aMTips, xZPos, m_EXECUTE);
end;

function TZAxisPipMoveMotorMotionSystem.HandleLiqDetError(aTipmap: integer; aNotEnough: boolean;
    const aDetectedVol, aExpectedVol: TDoubleArray): integer;
var
    xErrorInfo: TErrorInfo;
    x: integer;
    xTipmapText, xErrText, xSign: string;
begin
    if (aNotEnough) then
    begin
        xErrText := TLanguageString.Read('Not enough Liquid found at liquid detection!',
            'Bei der Flüssigkeitsermittlung wurde nicht genug Flüssigkeit entdeckt!');
        xSign := '<';
    end
    else
    begin
        xErrText := TLanguageString.Read('Liquid found at liquid detection (empty tube expected)!',
            'Bei der Flüssigkeitsermittlung wurde Flüssigkeit entdeckt (leeres Tube erwartet)!');
        xSign := '>';
    end;

    xErrorInfo := TErrorInfo.Create();
    try
        xTipmapText := '';
        for x := 0 to high(aDetectedVol) do
        begin
            if (((aTipmap shr x) and 1) = 1) then
                xTipmapText := xTipmapText + 'Tip ' + IntToStr(x + 1) + ', ';
        end;
        xTipmapText := Copy(xTipmapText, 1, Length(xTipmapText) - 2);

        xErrorInfo.Init(xTipmapText + ': ' + xErrText, TLanguageString.Read('Liquid detection error',
            'Liquid-Detection-Fehler'), eibAbortRetryIgnore);
        xErrorInfo.AddText(fMotors.Name);
        xErrorInfo.AddText(xTipmapText);
        xErrorInfo.AddText(xErrText);
        xErrorInfo.AddText('');
        for x := 0 to high(aDetectedVol) do
        begin
            if (((aTipmap shr x) and 1) = 1) then
                xErrorInfo.AdditionalInfo.Add(TLanguageString.
                    Read('Tip {0}: Detected volume {1} {2}, expected volume {3}',
                    'Nadel {0}: Detektiertes Volumen {1} {2}, erwartetes Volumen {3}',
                    [x + 1, aDetectedVol[x], xSign, aExpectedVol[x]]));
        end;
        xErrorInfo.IgnoreHint := 'Go to Z-Max and aspirate as much as possible';
        xErrorInfo.RetryHint := 'Detect again with these tips';

        result := gErrorMessageFactory.ErrBox(xErrorInfo);
    finally
        FreeAndNil(xErrorInfo);
    end;
end;

function TZAxisPipMoveMotorMotionSystem.MoveAspPos2(aZTravelMotion: TZAxisTravelMotionSystem; aTipmap: TIPMAP;
    aOKTips: TIPMAP; const aRP: TArray<TXRackPosition>; aLQMode: integer; const aDetVol: TDoubleArray;
    aSubMerge_mm: TPosMM; var vIsVol: TDoubleArray; aCalculateIsVol: boolean; aRecordDetectionVolume: boolean;
    aZScanMode, aZInsertSpeed, aZInsertMoveType: integer; const aZScan, aZMax, aZTube: TDoubleArray;
    aDisableZError: boolean): TIPMAP;
var
    xTTips, xTipLq: TIPMAP;
    xErrMode, xTip: integer;
    xAPosition: TPosMM;
    xZHeights: TDoubleArray;
    xv: single;
    xSingleRack: TRack;
begin
    // xSubMerge := aSub;
    // if (aSub > 32000) then xSubMerge := aSub - 65535;  pk 07.03.06 what is this supposed to do?

    aTipmap := aTipmap and aOKTips;

    // nur Tips die ein Volumen haben werden bewegt
    for xTip := 0 to fMotors.MaxIndex do
    begin
        if not TTipMapUtils.TipSelected(aTipmap, xTip) then
            CONTINUE;
        if (aDetVol[xTip] = 0) then
            TTipMapUtils.UnselectTip(aTipmap, xTip);
    end;

    ResetTipLiqDetError(aTipmap);
    xTTips := aTipmap;
    for xTip := 0 to fMotors.MaxIndex do
    begin
        if not TTipMapUtils.TipSelected(aTipmap, xTip) then
            CONTINUE;
        if ((aLQMode and INT_LQMODES_LQ_SCAN) <> 0) then
        begin // Used for slow retract / clot detection
            vIsVol[xTip] := self.GetVolByRackZPos(xTip, aRP[xTip].Rack, aZMax[xTip], aZTube[xTip]);
        end
        else
        begin
            if (aRP[xTip].Rack.RackID = '') or (not aCalculateIsVol) then
            begin // Berechnung nicht berücksichtigen: Wir tun so als wäre das Gefäß bis zum Rand voll
                vIsVol[xTip] := self.GetVolByRackZPos(xTip, aRP[xTip].Rack, aZScan[xTip], aZTube[xTip]);
            end
            else // Mit Berechnung:
                vIsVol[xTip] := aRP[xTip].Rack.CalcVolumeOfRackPos(aRP[xTip].Pos);
        end;
    end;

    if (gErrorManager.IsGlobalErr) then
        EXIT(0);

    if ((aLQMode and (INT_LQMODES_LQ_SCAN or INT_LQMODES_CD_ON)) <> 0) then
    begin // Liquid or clot detection

        // we have to scan for liquid
        xTipLq := aTipmap and aOKTips; // Set Tips for liquid detection

        // reset IsVol for these positions
        for xTip := 0 to fMotors.MaxIndex do
        begin
            if not TTipMapUtils.TipSelected(aTipmap, xTip) then
                CONTINUE;
            vIsVol[xTip] := 0;
        end;

        if ((aLQMode and INT_LQMODES_LQ_PIP_ISVOL) <> 0) then
            xTipLq := xTipLq and (not self.DetectLiq(xTipLq, TXRackPositionUtils.ExtractRacksFromArray(aRP),
                0, fMotors.GetNullDoubleArray(), vIsVol, ((aLQMode and INT_LQMODES_LQ_SINGLE_TIP) <> 0),
                aZScanMode, aZInsertSpeed, aZScan, aZMax, aZTube))
        else
            xTipLq := xTipLq and (not self.DetectLiq(xTipLq, TXRackPositionUtils.ExtractRacksFromArray(aRP),
                0, aDetVol, vIsVol, ((aLQMode and INT_LQMODES_LQ_SINGLE_TIP) <> 0), aZScanMode, aZInsertSpeed,
                aZScan, aZMax, aZTube));

        // Fehlerbehandlung Liquid detection
        while (not gErrorManager.IsGlobalErr) and ((xTipLq and aOKTips) <> 0) do
        begin

            // there are any tips with not enough liquid
            if ((aLQMode and INT_LQMODES_LQ_ERROR_GOTO) <> 0) then
            begin
                xErrMode := IDIGNORE // Error Mode: Ignore (Goto ZMax and Aspirate)
            end
            else if ((aLQMode and INT_LQMODES_LQ_DISP_ERROR) <> 0) then
            begin

                aZTravelMotion.MoveToZTravel(xTipLq, 0, 0);
                xErrMode := HandleLiqDetError(xTipLq, true, vIsVol, aDetVol);
            end
            else
                xErrMode := IDABORT; // dieser Fall ist eigentlich gar nicht möglich, deshalb Abbruch!!!

            // Error Handling
            case (xErrMode) of
                IDABORT:
                    begin
                        gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
                    end;
                IDIGNORE:
                    begin // Ignore: goto Z-Max and aspirate, deactivate tracking
                        self.MoveZ(xTipLq, aZMax, m_EXECUTE);

                        for xTip := 0 to fMotors.MaxIndex do
                        begin
                            if TTipMapUtils.TipSelected(xTipLq, xTip) then
                            begin
                                if (vIsVol[xTip] > 0) then
                                    fMotors[xTip].LiqErrorType := letLowLiquid
                                else
                                    fMotors[xTip].LiqErrorType := letNoLiquid;

                                // Ignore: IsVol wird auf ZMax gesetzt
                                vIsVol[xTip] := self.GetVolByRackZPos(xTip, aRP[xTip].Rack, aZMax[xTip],
                                    aZTube[xTip]);
                            end;
                        end;

                        xTTips := xTTips and not xTipLq;
                        xTipLq := 0;
                    end;
                IDRETRY:
                    begin // Retry
                        ResetTipLiqDetError(xTipLq); // !!! evtl vermerken daß da was war (keine Warnung)
                        if ((aLQMode and INT_LQMODES_CD_ON) <> 0) then
                            xTipLq := aTipmap and aOKTips; // retry with all if CD is ON

                        xTipLq := xTipLq and not self.DetectLiq(xTipLq,
                            TXRackPositionUtils.ExtractRacksFromArray(aRP), 0, aDetVol, vIsVol,
                            ((aLQMode and INT_LQMODES_LQ_SINGLE_TIP) <> 0), aZScanMode, aZInsertSpeed, aZScan,
                            aZMax, aZTube);
                    end;
            end;
        end; // while (TipLq)

        // Z- Höhen auslesen, wenn Detectiert wurde und Recording Modus aktiv !
        if (aRecordDetectionVolume) and (not gErrorManager.IsGlobalErr) then
        begin
            self.LiquidTips(aTipmap, xZHeights, true, aZScan, aZMax);
            self.RecordDetectedVolumes(aTipmap, xZHeights, aZTube, aRP, false,
                fMotors.GetTipLiqDetError(aTipmap));
        end;
    end;

    xTTips := xTTips and aOKTips; // das muss wahrscheinlich gar nicht sein!
    aTipmap := aTipmap and aOKTips;

    // ---------------------------------------------------------------------- Detection
    if ((aLQMode and INT_LQMODES_LQ_SCAN) <> 0) then
    begin
        { TODO : Funktioniert nur mit einem Rack }
        xSingleRack := TXRackPositionUtils.ExtractSingleRackFromArray(aRP);

        // if all tip in same liquid and at least one tip has detected
        if (xTTips <> 0) and Assigned(xSingleRack) and (xSingleRack.IsMultiTipRack()) then
        begin // not single tip tubes
            xv := 1E38;
            self.MoveToLowestTip(xTTips, aTipmap);
            for xTip := 0 to fMotors.MaxIndex do
            begin
                if (((xTTips shr xTip) and 1) <> 0) and (xv > vIsVol[xTip]) then
                    xv := vIsVol[xTip];
            end;
            for xTip := 0 to fMotors.MaxIndex do
            begin
                if (((aTipmap shr xTip) and 1) <> 0) then
                    vIsVol[xTip] := xv;
            end;
        end;

        xTTips := self.Submerge(xTTips, aZMax, aSubMerge_mm);
    end
    // ------------------------------------------------------------------ No Detection
    else
    begin
        for xTip := 0 to fMotors.MaxIndex do
            if (((aTipmap shr xTip) and 1) <> 0) then
            begin
                if ((aLQMode and INT_LQMODES_USE_DETECTED_VAL) <> 0) then
                begin
                    xAPosition := self.GetRackZPosByDetectedLevel(xTip, aRP[xTip].Rack, aRP[xTip].Pos,
                        aZMax[xTip], aZMax[xTip], aZTube[xTip]);
                end
                else
                begin
                    xAPosition := self.GetRackZPosByVolCalc(xTip, aRP[xTip].Rack, aRP[xTip].Pos, aZMax[xTip],
                        ((aLQMode and INT_LQMODES_NO_CALCULATION) <> 0), aZMax[xTip], aZTube[xTip]);
                end;

                // Add Submerge
                xAPosition := AddZHeight(xAPosition, -aSubMerge_mm);

                // Check range: Z value must not be higher than Z-Scan and not be deeper than Z-Max
                if (fMotors.DistanceToMin(xTip, xAPosition) >= fMotors.DistanceToMin(xTip, aZMax[xTip])) then
                begin
                    xAPosition := aZMax[xTip];
                    xTTips := xTTips and not(1 shl xTip);
                end
                else if (fMotors.DistanceToMin(xTip, xAPosition) < fMotors.DistanceToMin(xTip,
                    aZScan[xTip])) then
                begin
                    xAPosition := aZScan[xTip];
                end;

                gLogManager.LogF('Aspirate Position=%f SubMerge=%f Tip=%d Position=%d ',
                    [xAPosition, aSubMerge_mm, xTip, aRP[xTip].Pos], false);
                self.MoveSingleZ(xTip, xAPosition, m_NO_EXEC, GetMoveModeFromInt(aZInsertMoveType),
                    aZInsertSpeed);
                ResetTipLiqDetError(aTipmap);

                if (fMotors.MapAllToFirst) then
                begin
                    TLogManager.Instance.Log('MapAllToFirst Option: Break up Z-Motor-Movement', false);
                    BREAK;
                end;
            end;
        if (aDisableZError) then
        begin
            fMotors.ExecuteDisabledErrorsAndCheck(aTipmap);
            // notwendig? Am Schluss wird noch einmal DisableErrors ausgeführt
            fMotors.DisableErrors(aTipmap);
        end
        else
        begin
            self.Execute;
        end;
    end;
    if ((aLQMode and INT_LQMODES_TRACKING) = 0) then
        xTTips := 0; // Set tracking tips
    EXIT(xTTips); // return the tips witch have to track
end;

// --------------------------------------------------------------------------------------------------
function TZAxisPipMoveMotorMotionSystem.MoveDspPos2(aZTravelMotion: TZAxisTravelMotionSystem; aTipmap: TIPMAP;
    aOKTips: TIPMAP; const aRP: TArray<TXRackPosition>; aLQMode: integer; aDspPos: TDispAtPosType;
    const aDetVol: TDoubleArray; var vIsVol: TDoubleArray; aSubMerge_mm: TPosMM;
    aZScanMode, aZInsertSpeed, aZInsertMoveType, aZRetractSpeed: integer;
    const aZScan, aZDisp, aZMax, aZTube: TDoubleArray; aDisableZError: boolean): TIPMAP;
// --------------------------------------------------------------------------------------------------
var
    xTTips, xTipLq: TIPMAP;
    xErrMode, xTip: integer;
    xRetractPos, xDPosition: TPosMM;
begin
    // opt. Scan for liquid (only if LD or CD)
    // opt. LQ Error Handling
    // move to aspiration position
    // !!! Liquid position will be saved

    if (gErrorManager.IsGlobalErr) then
        EXIT(0);

    aTipmap := aTipmap and aOKTips;

    if ((aLQMode and INT_LQMODES_TRACKING) <> 0) then // Set tracking tips
        xTTips := aTipmap
    else
        xTTips := 0;

    for xTip := 0 to fMotors.MaxIndex do
    begin
        if not TTipMapUtils.TipSelected(aTipmap, xTip) then
            CONTINUE;
        if ((aLQMode and INT_LQMODES_LQ_SCAN) <> 0) then
        begin // Used for slow retract / clot detection
            vIsVol[xTip] := self.GetVolByRackZPos(xTip, aRP[xTip].Rack, aZMax[xTip], aZTube[xTip]);
        end
        else
            vIsVol[xTip] := self.GetVolByRackZPos(xTip, aRP[xTip].Rack, aZScan[xTip], aZTube[xTip]);
    end;

    if ((aLQMode and INT_LQMODES_LQ_CHECK_EMPTY) <> 0) then
    begin // We want to display an error if we find liquid

        xTipLq := aTipmap and aOKTips; // Set Tips for liquid detection

        while ((xTipLq <> 0) and (not gErrorManager.IsGlobalErr)) do
        begin

            xTipLq := self.DetectLiq(xTipLq, TXRackPositionUtils.ExtractRacksFromArray(aRP), 0, aDetVol,
                vIsVol, ((aLQMode and INT_LQMODES_LQ_SINGLE_TIP) <> 0), aZScanMode, aZInsertSpeed, aZScan,
                aZMax, aZTube);
            if (xTipLq <> 0) then
            begin
                if ((aLQMode and INT_LQMODES_LQ_DISP_ERROR) <> 0) then
                begin

                    aZTravelMotion.MoveToZTravel(xTipLq, 0, 0);
                    case HandleLiqDetError(xTipLq, false, vIsVol, aDetVol) of
                        IDABORT:
                            xErrMode := 0; // Abort on Error
                        IDIGNORE:
                            xErrMode := 1; // Ignore (Goto ZMax)
                        IDRETRY:
                            xErrMode := 2; // Retry scan
                        else
                            xErrMode := 3; // Skip Sample
                    end;
                end
                else
                begin
                    if ((aLQMode and INT_LQMODES_LQ_ERROR_GOTO) <> 0) then
                        xErrMode := 1 // Error Mode: Ignore (Goto ZMax and Aspirate)
                    else
                        xErrMode := 3; // Error Mode: skip  (Skip Sample)
                end;

                // Error Handling
                case (xErrMode) of
                    1:
                        begin // Ignore: go on as if it was empty
                            xTipLq := 0;
                        end;

                    2:
                        begin // Retry
                            ResetTipLiqDetError(xTipLq); // !!! evtl vermerken daß da was war (keine Warnung)
                            if ((aLQMode and INT_LQMODES_CD_ON) <> 0) then
                                xTipLq := aTipmap and aOKTips; // retry with all if CD is ON
                        end;

                    0:
                        begin // Cancel
                            // SetGlobalErr(SE_TUBE_FULL or (xTipLq shl 16));
                            gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
                        end;

                    3:
                        begin
                        end; // Skip

                end;
                { else begin       // Dieser Fall konnte gar nicht eintreffen
                  SetTipLiqDetError(xTipLq, TE_DISP_LIQ_ERROR);
                  aPipDevice.OKTips := not xTipLq;
                  xTipLq := 0;
                  aUsedArm.MoveZ(xTipLq, xrd.ZTravel, m_EXECUTE); // Move bad Tips out
                  end; }
            end;
        end;

        // MoveTo Z Dispense
        aTipmap := aTipmap and aOKTips;
        self.MoveZ(aTipmap, aZDisp, m_EXECUTE);
    end
    // ------------------------------------------------------------- Liquid Detection
    else if ((aLQMode and INT_LQMODES_LQ_SCAN) <> 0) then
    begin

        xTipLq := aTipmap and aOKTips; // Set Tips for liquid detection
        while ((xTipLq <> 0) and (not gErrorManager.IsGlobalErr)) do
        begin

            // reset IsVol for these positions
            for xTip := 0 to high(vIsVol) do
                vIsVol[xTip] := 0;

            xTipLq := xTipLq and not self.DetectLiq(xTipLq, TXRackPositionUtils.ExtractRacksFromArray(aRP), 0,
                fMotors.GetNullDoubleArray(), vIsVol, ((aLQMode and INT_LQMODES_LQ_SINGLE_TIP) <> 0),
                aZScanMode, aZInsertSpeed, aZScan, aZMax, aZTube);
            // --------------------------------------------------------- Move Submerge steps
            for xTip := 0 to fMotors.MaxIndex do
                if (((xTipLq shr xTip) and 1) <> 0) then
                begin
                    xRetractPos := AddZHeight(fMotors.ReadCurrentPos(xTip), -aSubMerge_mm);
                    if (fMotors.DistanceToMin(xTip, xRetractPos) <= fMotors.DistanceToMin(xTip,
                        aZMax[xTip])) then
                        self.MoveSingleZ_Retract(xTip, xRetractPos, m_NO_EXEC, aZRetractSpeed, false);
                end;
            self.Execute;

            if (xTipLq <> 0) then
            begin

                // there are any tips with not enough liquid
                if ((aLQMode and INT_LQMODES_LQ_DISP_ERROR) <> 0) then
                begin
                    aZTravelMotion.MoveToZTravel(xTipLq, 0, 0);
                    case HandleLiqDetError(xTipLq, true, vIsVol, aDetVol) of
                        IDABORT:
                            xErrMode := 0; // Abort on Error
                        IDIGNORE:
                            xErrMode := 1; // Ignore (Goto ZMax)
                        IDRETRY:
                            xErrMode := 2; // Retry scan
                        else
                            xErrMode := 3; // Skip Sample
                    end;
                end
                else
                begin
                    if ((aLQMode and INT_LQMODES_LQ_ERROR_GOTO) <> 0) then
                        xErrMode := 1 // Error Mode: Ignore (Goto ZMax and Aspirate)
                    else
                        xErrMode := 3; // Error Mode: skip  (Skip Sample)
                end;

                // Error Handling
                case (xErrMode) of
                    1:
                        begin // Ignore: goto Z-Max and aspirate, deactivate tracking
                            // SetTipLiqDetError(xTipLq, TE_DISP_LIQ_WARNING);

                            if (aDspPos = dapDispAtZMax) then
                                self.MoveZ(xTipLq, aZMax, m_EXECUTE)
                            else
                                self.MoveZ(xTipLq, aZDisp, m_EXECUTE);

                            xTTips := xTTips and not xTipLq;
                            xTipLq := 0;
                        end;

                    2:
                        begin // Retry
                            ResetTipLiqDetError(xTipLq); // !!! evtl vermerken daß da was war (keine Warnung)
                            if ((aLQMode and INT_LQMODES_CD_ON) <> 0) then
                                xTipLq := aTipmap and aOKTips; // retry with all if CD is ON
                        end;

                    0:
                        begin // Cancel
                            // SetGlobalErr(SE_NO_LIQUID or (xTipLq shl 16));
                            gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
                        end;

                    3:
                        begin
                        end; // Skip

                    { else begin   // Dieser Fall konnte gar nicht eintreffen
                      SetTipLiqDetError(xTipLq, TE_DISP_LIQ_ERROR);
                      aPipDevice.OKTips := not xTipLq;
                      xTipLq := 0;
                      aUsedArm.MoveZ(xTipLq, xrd.ZTravel, m_EXECUTE); // Move bad Tips out
                      end; }
                end;

            end; // if (TipLq)

        end; // while (TipLq)
    end
    else
    begin
        for xTip := 0 to fMotors.MaxIndex do
        begin
            if (((aTipmap shr xTip) and 1) <> 0) then
            begin

                xDPosition := aZDisp[xTip]; // default: nimm ZDisp

                if ((aLQMode and INT_LQMODES_USE_DETECTED_VAL) <> 0) then
                begin
                    xDPosition := self.GetRackZPosByDetectedLevel(xTip, aRP[xTip].Rack, aRP[xTip].Pos,
                        aZMax[xTip], aZMax[xTip], aZTube[xTip]);
                end
                else if (aDspPos = dapDispAtZMax) or ((aLQMode and INT_LQMODES_TRACKING) <> 0) then
                begin // Level Tracking ist gesetzt
                    // Berechnung der Höhe mit dem gespeicherten Volumen
                    xDPosition := self.GetRackZPosByVolCalc(xTip, aRP[xTip].Rack, aRP[xTip].Pos, aZDisp[xTip],
                        ((aLQMode and INT_LQMODES_NO_CALCULATION) <> 0), aZMax[xTip], aZTube[xTip]);
                end;

                // Berechnetes Z soll nicht "höher" als ZDisp sein
                if (fMotors.DistanceToMin(xTip, xDPosition) < fMotors.DistanceToMin(xTip, aZDisp[xTip])) then
                    xDPosition := aZDisp[xTip];

                // Submerge hinzurechnen
                xDPosition := AddZHeight(xDPosition, -aSubMerge_mm);

                // zur Sicherheit: extreme Werte vermeiden
                if (fMotors.DistanceToMin(xTip, xDPosition) > fMotors.DistanceToMin(xTip, aZMax[xTip])) then
                begin
                    xDPosition := aZMax[xTip];
                    gLogManager.LogF
                        ('Z-Disp Position %f is deeper than Z-Max %f, Z Position will be set to Z-Max',
                        [xDPosition, aZMax[xTip]], false);
                end;

                fMotors.SetToMinPosIfIsBeyondMin(xTip, xDPosition);
                gLogManager.LogF
                    ('MoveDspPos2: Tip %d, Position %d, Z-Disp Position: %f, including SubMerge %f,',
                    [xTip + 1, aRP[xTip].Pos, xDPosition, aSubMerge_mm], false);

                self.MoveSingleZ(xTip, xDPosition, m_NO_EXEC, GetMoveModeFromInt(aZInsertMoveType),
                    aZInsertSpeed);

                if (fMotors.MapAllToFirst) then
                begin
                    TLogManager.Instance.Log('MapAllToFirst Option: Break up Z-Motor-Movement', false);
                    BREAK;
                end;
            end;
        end;

        if (aDisableZError) then
        begin
            fMotors.ExecuteDisabledErrorsAndCheck(aTipmap);
            // notwendig? Am Schluss wird noch einmal DisableErrors ausgeführt
            fMotors.DisableErrors(aTipmap);
        end
        else
        begin
            self.Execute;
        end;
    end;
    EXIT(xTTips);
end;

// --------------------------------------------------------------------------------------------------
procedure TZAxisPipMoveMotorMotionSystem.MoveAspPosPowder(aTipmap: TIPMAP; aOKTips: TIPMAP;
    const aRP: TArray<TXRackPosition>; const aVol: TDoubleArray; aSubMerge_mm: TPosMM;
    aZInsertSpeed, aZInsertMoveType: integer; const aZScan, aZMax, aZTube: TDoubleArray;
    aNoCalculation: boolean; aSingleTip: boolean);
// --------------------------------------------------------------------------------------------------
var
    xTip: integer;
    xAPosition: TPosMM;
begin
    // Powder Handling: Kein Tracking, kein Liquid Detection/Clot detection

    // if (aSub > 32000) then xSubMerge := aSub - 65535; // pk 07.03.06 commented out Hä???

    aTipmap := aTipmap and aOKTips;

    // nur Tips die ein Volumen haben werden bewegt
    for xTip := 0 to high(aVol) do
        if (aVol[xTip] = 0) then
            aTipmap := aTipmap and not(1 shl xTip); // = SetUnused

    ResetTipLiqDetError(aTipmap);
    if gErrorManager.IsGlobalErr() then
        EXIT;

    aTipmap := aTipmap and aOKTips;

    // ------------------------------------------------------------------ No Detection
    for xTip := 0 to fMotors.MaxIndex do
        if (((aTipmap shr xTip) and 1) <> 0) then
        begin
            xAPosition := self.GetRackZPosByVolCalc(xTip, aRP[xTip].Rack, aRP[xTip].Pos, aZMax[xTip],
                aNoCalculation, aZMax[xTip], aZTube[xTip]);
            xAPosition := AddZHeight(xAPosition, -aSubMerge_mm);
            if (fMotors.DistanceToMin(xTip, xAPosition) >= fMotors.DistanceToMin(xTip, aZMax[xTip])) then
            begin
                xAPosition := aZMax[xTip];
                gLogManager.LogF('Z-Asp Position %f > Z-Max %f, Z-Asp Position will be set to Z-Max',
                    [xAPosition, aZMax[xTip]], false);
            end
            else if (fMotors.DistanceToMin(xTip, xAPosition) < fMotors.DistanceToMin(xTip, aZScan[xTip])) then
            begin
                xAPosition := aZScan[xTip];
                gLogManager.LogF('Z-Asp Position %f < Z-Scan %f, Z-Asp Position will be set to Z-Scan',
                    [xAPosition, aZMax[xTip]], false);
            end;

            gLogManager.LogF('Tip %d, Position %d, Z-Asp Position %f, including SubMerge %f ',
                [xTip + 1, aRP[xTip].Pos, xAPosition, aSubMerge_mm], false);
            self.MoveSingleZ(xTip, xAPosition, m_NO_EXEC, GetMoveModeFromInt(aZInsertMoveType),
                aZInsertSpeed);
            ResetTipLiqDetError(aTipmap);

            // Single Tip: Z-Motoren werden einzeln gefahren
            if (aSingleTip) then
                self.Execute;
        end;
    self.Execute();
end;

// --------------------------------------------------------------------------------------------------
function TZAxisPipMoveMotorMotionSystem.MoveDspPosPowder(aTipmap: TIPMAP; aOKTips: TIPMAP;
    const aRP: TArray<TXRackPosition>; aDspPos: TDispAtPosType; aSubMerge_mm: TPosMM;
    aZInsertSpeed, aZInsertMoveType, aZRetractSpeed: integer; aRedi: boolean;
    const aZScan, aZDisp, aZMax: TDoubleArray; aSingleTip: boolean): TIPMAP;
// --------------------------------------------------------------------------------------------------
var
    xTip: integer;
    xDPosition: TPosMM;
begin
    // Powder Handling: Kein Tracking, kein Liquid Detection/Clot detection

    result := 0;
    if gErrorManager.IsGlobalErr() then
        EXIT;

    aTipmap := aTipmap and aOKTips;

    for xTip := 0 to fMotors.MaxIndex do
        if (((aTipmap shr xTip) and 1) <> 0) then
        begin

            xDPosition := aZDisp[xTip]; // kein Level Tracking: nimm ZDisp

            // Submerge hinzurechnen
            xDPosition := AddZHeight(xDPosition, -aSubMerge_mm);

            // zur Sicherheit: extreme Werte vermeiden
            if (fMotors.DistanceToMin(xTip, xDPosition) > fMotors.DistanceToMin(xTip, aZMax[xTip])) then
            begin
                xDPosition := aZMax[xTip];
                gLogManager.LogF('Z-Disp Position %f > Z-Max %f, Z-Disp Position will be set to Z-Max',
                    [xDPosition, aZMax[xTip]], false);
            end;

            fMotors.SetToMinPosIfIsBeyondMin(xTip, xDPosition);

            gLogManager.LogF('MoveDspPos2: Tip %d, Position %d, Z-Disp Position: %f, including SubMerge %f,',
                [xTip + 1, aRP[xTip].Pos, xDPosition, aSubMerge_mm], false);
            self.MoveSingleZ(xTip, xDPosition, m_NO_EXEC, GetMoveModeFromInt(aZInsertMoveType),
                aZInsertSpeed);

            // Single Tip: Z-Motoren werden einzeln gefahren
            if (aSingleTip) then
                self.Execute;
        end;
    self.Execute;
end;

procedure TZAxisPipMoveMotorMotionSystem.MoveSingleZ_Scan(aMotor: integer; aDest: TPosMM; aExec: EXEC_MODE;
    aZScanMode, aZScanSpeed, aZScanRamp: integer);
begin
    fMotors.MoveSingleZ_Scan(aMotor, aDest, aExec, aZScanMode, aZScanSpeed, aZScanRamp);

end;

procedure TZAxisPipMoveMotorMotionSystem.MoveZ(aMotorMap: TIPMAP; const aDest: TDoubleArray; aExec: EXEC_MODE;
    aMode: ACTION_TYPE; aSpeed, aRamp: integer; aSingle: boolean);
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT; // return on Error

    fMotors.MoveZ(aMotorMap, aDest, aExec, aMode, aSpeed, aRamp, aSingle);
end;

procedure TZAxisPipMoveMotorMotionSystem.MoveZ_WithOffset(aMotorMap: TIPMAP; const aDest: TDoubleArray;
    aOffset_mm: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed, aRamp: integer; aSingle: boolean);
var
    x: integer;
    xDest: TArray<double>;
begin
    xDest := TArrayUtils.CopyArray<double>(aDest);
    if (gErrorManager.IsGlobalErr) then
        EXIT; // return on Error
    for x := 0 to fMotors.MaxIndex do
    begin
        if not Assigned(fMotors.GetMotor(x)) then
            CONTINUE;
        if not TTipMapUtils.TipSelected(aMotorMap, x) then
            CONTINUE;

        xDest[x] := AddZHeight(aDest[x], aOffset_mm);
    end;

    fMotors.MoveZ(aMotorMap, xDest, aExec, aMode, aSpeed, aRamp, aSingle);
end;

procedure TZAxisPipMoveMotorMotionSystem.BlockMoveZ(aMotorMap: TIPMAP; const aDest: TDoubleArray;
    aSpeed: integer);
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT; // return on Error
    fMotors.BlockMoveZ(aMotorMap, aDest, aSpeed);
end;

procedure TZAxisPipMoveMotorMotionSystem.MoveZ_MultiSpeed(aMotorMap: TIPMAP; const aDest: TDoubleArray;
    aExec: EXEC_MODE; aMode: ACTION_TYPE; const aSpeed: TIntArray; aRamp: integer; aSingle: boolean);
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT; // return on Error
    fMotors.MoveZ_MultiSpeed(aMotorMap, aDest, aExec, aMode, aSpeed, aRamp, aSingle);
end;

procedure TZAxisPipMoveMotorMotionSystem.MoveZ_WashRetract(aMotorMap: TIPMAP; const aDest: TDoubleArray;
    aWashRetractSpeed: integer);
begin
    fMotors.MoveZ_WashRetract(aMotorMap, aDest, m_EXECUTE, aWashRetractSpeed);
end;

procedure TZAxisPipMoveMotorMotionSystem.MoveToTransportAirPos(aMotorMap: TIPMAP;
    const aTransAirVol: TDoubleArray; aZRetractSpeed: integer; aSingleRetract: boolean;
    const aTransAirZPos: TDoubleArray; aRetractMoveDone: boolean);
var
    x: integer;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    for x := 0 to fMotors.MaxIndex do
    begin
        if (((aMotorMap shr x) and 1) = 0) then
            CONTINUE;

        if (aRetractMoveDone) then // Retract-Movement wurde bereits ausgeführt: fahre normalen Speed
            MoveSingleZ(x, aTransAirZPos[x], m_NO_EXEC)
        else
            MoveSingleZ_Retract(x, aTransAirZPos[x], m_NO_EXEC, aZRetractSpeed, false);

        if (aSingleRetract) then
        begin
            gRunFlow.AppSleep(500); // Single Retract: wait 500 msec
            self.Execute;
        end;
    end;
    self.Execute;
    if (gErrorManager.IsGlobalErr) then
        EXIT;
end;

function TZAxisPipMoveMotorMotionSystem.ReadCurrentPos(aIndex: integer): TPosMM;
begin
    result := fMotors.ReadCurrentPos(aIndex);
end;

function TZAxisPipMoveMotorMotionSystem.ReadCurrentPositions(const aMotorMap: TIPMAP): TDoubleArray;
var
    x: integer;
begin
    result := fMotors.GetNullDoubleArray;
    for x := 0 to fMotors.MaxIndex do
    begin
        if not TTipMapUtils.TipSelected(aMotorMap, x) then
            CONTINUE;
        result[x] := fMotors.ReadCurrentPos(x);
    end;
end;

procedure TZAxisPipMoveMotorMotionSystem.DisableZErrors(aMotorMap: TIPMAP);
begin
    fMotors.DisableErrors(aMotorMap);
end;

procedure TZAxisPipMoveMotorMotionSystem.ResetDisabledZErrors(aMotorMap: TIPMAP);
begin
    fMotors.ResetDisabledErrors(aMotorMap);
end;

procedure TZAxisPipMoveMotorMotionSystem.ResetAllDisabledZErrors();
begin
    fMotors.ResetDisabledErrors(fMotors.GetMotorMap);
end;

function TZAxisPipMoveMotorMotionSystem.ZErrorMotors(aMotorMap: TIPMAP): TIPMAP;
begin
    result := fMotors.GetZErrorMap(aMotorMap);
end;

function TZAxisPipMoveMotorMotionSystem.GetOKTips_ZP02(aMotorMap: TIPMAP;
    const aMaxPos: TDoubleArray): TIPMAP;
begin
    result := fMotors.GetOKTips_ZP02(aMotorMap, aMaxPos);
end;

function TZAxisPipMoveMotorMotionSystem.LiquidTips(const aMotorMap: TIPMAP; out oDetectedPos: TDoubleArray;
    aShouldDetect: boolean; const aStartPos, aEndPos: TDoubleArray): TIPMAP;
var
    xTo: extended;
    x: integer;
    xTempMotorMap: TIPMAP;
begin
    result := TTipMapUtils.EmptyTipMap;
    oDetectedPos := fMotors.GetNullDoubleArray;

    xTo := Now + 2000; // be sure to leave at least after 2 seconds (ZP02 only)

    xTempMotorMap := aMotorMap;

    // Besonderheit für Simulationsmodus: Es soll möglichst kein Fehler hochkommen, die "detektierte"
    // Höhe ist ein Mittelwert aus StartPos und EndPos
    if gRunFlow.SimulationMode then
    begin
        Randomize;
        for x := 0 to fMotors.MaxIndex do
        begin

            if not TTipMapUtils.TipSelected(aMotorMap, x) then
                CONTINUE;

            if (aShouldDetect) then
                TTipMapUtils.SelectTip(result, x);

            oDetectedPos[x] := RandomRange(round(aStartPos[x]), round(aEndPos[x]));
        end;

        EXIT;
    end;

    while (xTempMotorMap <> 0) and (not gErrorManager.IsGlobalErr) do
    begin
        // Besonderheit für Simulationsmodus: Es soll möglichst kein Fehler hochkommen, die "detektierte"
        // Höhe ist ein Mittelwert aus StartPos und EndPos

        result := result or fMotors.DetectedMotors(xTempMotorMap, oDetectedPos);

        if { ( TAppSettings.IsSias ) and } (Now > xTo) then
        begin
            gLogManager.LogF('LiquidTips timeout error: %d , %d', [xTempMotorMap, result], true);
            EXIT;
        end;
    end;

end;

function TZAxisPipMoveMotorMotionSystem.CalcTrackingSpeedByDistanceAndTime(aIndex: integer; aDistance: TPosMM;
    aTimeInSec: double): integer;
begin
    result := Abs(fMotors.CalcSpeedByDistanceAndTime(aIndex, aDistance, aTimeInSec));
end;

procedure TZAxisPipMoveMotorMotionSystem.ResetTipLiqDetError(aTipmap: TIPMAP);
begin
    fMotors.SetTipLiqDetError(aTipmap, letOK);
end;

function TZAxisPipMoveMotorMotionSystem.GetTipLiqDetError(aTipmap: TIPMAP): TTipLiqErrorTypeArray;
begin
    result := fMotors.GetTipLiqDetError(aTipmap);
end;


end.
