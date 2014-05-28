{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : An Operator uses Devices.
  All devices needed for the operation must be parameters of the constructor.
  An oparator is created just for a special operation and destroyed after it.
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                      track-no  improvement/change
  -------- --  ------------------------------------------  --------  ----------------------------------------------
  09.11.05 wl                                              TN2728    initial version
  09.11.05 wl  TArmZMovementOperator                       TN2728    Operator, der alle Funktionen beinhaltet um Nadeln in Z zu bewegen
  09.11.05 wl  TArmZMovementOperator.GetRackZPosByVolCalc  TN2728    von SamHigh hierher
  09.11.05 wl  TArmZMovementOperator.TipTouch,MoveRetractDistance   TN2728    von LiquidHandling hierher
  09.11.05 wl  TArmZMovementOperator.DetectLiquid          TN2728    von LiquidHandlingLow hierher
  17.11.05 wl                                              TN2771    TOperation statt TOperator
  17.11.05 wl  TRack/TubeBCReadingOperation                TN2771    neu
  18.11.05 wl  TArmZMovementOperation.GetRackZPosByVolume  TN2764    benutzt SetToMinPosIfIsBeyondMin, damit wieder richtig gerechnet wird
  18.11.05 wl  TArmZMovementOperation.MoveRetractDistance  TN2764    benutzt SetToMinPosIfIsBeyondMin, damit wieder richtig gerechnet wird
  02.02.06 wl  TArmZMovementOperation.GetRackZPosByVolCalc TN2923    CalcVolumeOfRackPos --> ObjWorkb (uses geändert)
  25.03.06 pk  TZAxisPipMoveOperation.DoSingleZMovement    TN2958    case for zptRackZTravel removed
  29.03.06 wl  TZAxisPipMoveMotorMotionSystem.DoSingleZMovement  TN2966    Parameter aDisableError wird in Version 7.1.0 noch nicht verwendet
  29.03.06 wl  TXYZTravelAxisPipMoveOperation.MoveXY       TN3005    Neue Parameter: XSpeed,xRamp,YSpeed,YRamp
  29.03.06 wl  TXMultiYAxisMoveMotorMotionSystem.MoveXY    TN3005    Neue Parameter: XSpeed,xRamp,YSpeed,YRamp
  29.03.06 wl  TZAxisPipMoveOperation.DoSingleZMovement    TN3006    Neuer Parameter: ZRamp
  29.03.06 wl  TZAxisPipMoveMotorMotionSystem.MoveSingleZ  TN3006    Neuer Parameter: ZRamp
  29.03.06 wl  TZAxisPipMoveMotorMotionSystem.DoSingleZMovement  TN3006    Neuer Parameter: ZRamp
  31.03.06 pk  TXMultiYAxisMoveMotorMotionSystem.MoveXY    TN2958    Bug : The option xyoNeverUseRackZTravel was ignored
  31.03.06 pk  TXMultiYAxisMoveMotorMotionSystem.CreateRoute TN2958  Bug : current value was given in steps instead of mm
  03.04.06 pk  Init                                        TN2997   Init now has parameter aInitID
  11.04.06 wl  TZAxisPipMoveMotorMotionSystem.DoSingleZMovement  TN2966    DisableError ist jetzt aktiv
  18.04.06 pk  TXMultiYAxisMoveMotorMotionSystem           TN2958   new functions to implement Moffset
  20.04.06 pk  DoZMovement                                 TN3054   function DoSingleZMovement adapted for more than one tip
  21.04.06 wl                                              TN3051    alle Volume- und VolSteps-Angaben sind extended
  25.04.06 pk  TXMultiYAxisMoveMotorMotionSystem.CreateRoute TN2958  recalculate the Y move to avoid moving y motors
  27.04.06 pk  TZAxisPipMoveMotorMotionSystem              TN2958   Changes for ZRetractSpeed and ZScanSpeed
  02.08.06 pk  TZAxisPipMoveMotorMotionSystem              TN3225   MoveZ_WashRetract functionality moved to MultiZMotorDevice
  05.09.06 pk  THomeMoveOperation                          TN3278   New: move to home position to avoid collision
  14.09.06 pk  TXMultiYAxisMoveMotorMotionSystem           TN3306   Aspiration Shifting corrected
  10.11.06 wl  TZAxisPipMoveMotorMotionSystem.MoveZ_WithOffset  TN3400   Es wird abgefragt, ob Motor existiert, sonst gibt es eine Assertion
  04.12.06 wl  TZAxisPipMoveMotorMotionSystem.HandleLiqDetError TN3243   spezielle Fehlerbehandlung für Liquid Detection
  04.12.06 wl  TZAxisPipMoveMotorMotionSystem.DetectLiq         TN3445   es wird bis nach Z-Max detektiert. Volumen zu klein gilt auch als Fehler
  04.12.06 wl  TZAxisPipMoveMotorMotionSystem.MoveAspPos2       TN3445   überarbeitet, aber keine Änderungen im Ablauf
  06.12.06 wl  TXYZRAxisInitMoveMotorMotionSystem               TN3452   fInitXBeforeY: if true, x-motor will be initialized just after the z-motor
  07.12.06 wl                                                   TN3243    uses SamErr entfernt
  12.12.06 wl  GetRackZTravel                                   TN3467   statt CreateFirstPosition wird CreateRotationCorner benutzt (ändert nichts)
  19.01.07 wl  THomeMoveOperation.MoveToHome                    TN3510   nach dem Fahren des R-Motors wird Execute ausgeführt
  07.03.07 wl  T..MotorSystem                                   TN3620   --> MotorSystemHelper,MotorSystemPipMove,MotorSystemTravel,MotorSystemInit
  08.03.07 wl  TZAxisPipMoveOperation.DoZMovement               TN3439   ResetDisabledErrors jetzt erst nach dem Execute
  12.03.07 pk  TZAxisTravelMoveOperation.fMotionSystem          TN3628   type changed to MotionSystem instead of MotorMotionSystem
  09.11.07 pk                                                   TN3924   Steps changed to mm
  13.11.07 wl  TZAxisPipMoveWithBalanceOperation                TN3844    liest StopWeight aus Settings und startet TZAxisPipMoveMotorMotionSystem.PowderDetection
  20.06.08 pk                                                   TN4139    WB global object replaced by LayoutManager
  27.11.08 wl  TZAxisPipMoveOperation.DoZMovement               TN3626    new parameter: ZMoveType
  05.08.09 wl  TZAxisPipMoveOperation.DoZMovement               TN4707   benutzt auch bei Global-ZTravel die gleiche Methode
  12.09.09 wl                                                   TN4740   MPOS durch integer ersetzt
  07.05.10 pk  ReadCurrentPositions                             TN5093   New for ZPOSR Action
  07.05.10 pk  DoZDetect                                        TN5094   New for ZTIPD Action
  12.05.10 pk  DoZDetect                                        TN5094   New aStoreDifferenceAsVolume parameter
  22.07.10 wl  TZAxisPipMoveOperation.DoZMovement               TN5193   benutzt Funktion ZErrorMotors, um die Motorfehler als Rückgabewert auszulesen
  26.01.11 wl  TXYZTravelAxisPipMoveOperation                   TN5448   MoveZTravelIfNoXYMove entfernt
  23.03.11 wl  TZAxisTravelMoveOperation                        TN5515    Methodennamen geändert
  23.03.11 wl  TZAxisTravelMoveOperation                        TN5515   CombinedArmsMoveToZTravel ist jetzt getrennt von MoveToZTravel
  25.03.11 wl  TInitOperation                                   TN5519   an TGripInitMotorMotionSystem angepasst
  20.05.11 ts  MoveXYWithOffset                                 TN5589   wenn ein Offset eingetragen ist, muss die Bewegung immer ausgeführt werden!! (auch wenn x-,y-Werte stimmen)
  03.11.11 wl                                                   TN5725   verwendet SubstanceLoading
  13.01.12 ts  DoZMovement                                      TN5769   SingleExecuteWait eingefügt (für ZP01 notwendig, sonst wird bei SingleExecute nicht gewartet)
  02.02.11 wl  alle Funktionen                                  TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  03.02.11 wl  DetermineInternMove                              TN5791   Inhalt --> TZAxisRackTravelMotorMotionSystem.PrepareInternMovement
  28.08.12 ts                                                   TN5943   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Tube
  07.09.12 ts                                                   TN5973   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Rack
  27.02.13 wl  THomeMoveOperation.MoveToHome                    TN6066   neuer parameter Direction
  23.05.13 wl  TInitOperation.Init                              TN6153   ResetLastRack von SamIntf.gmInitArm hierher
  23.05.13 wl  TXYZTravelAxisPipMoveOperation.MoveXYSimple      TN6153   neu: Ohne InternRackMovement (für Layouter)
  23.05.13 wl  TXYZTravelAxisPipMoveOperation.MoveXYWithOffset  TN6153   erzeugt TArmPostionInfo, die in MotionSystem als PreviousPos gespeichert wird
  23.05.13 wl  TXYZTravelAxisPipMoveOperation                   TN6153   verwendet GetPreviousPosition/SetPreviousPosition
  05.06.13 wl  TZAxisPipMoveOperation.GetPreviousArmPosition    TN6164   Keine Exception, wenn result = nil
  06.06.13 wl  TZAxisPipMoveOperation.DoZMovement               TN6154   Motor-Disable wird mit ExecuteDisabledErrorsAndCheck ausgeführt
  06.06.13 wl  GetTubeXYOffset                                  TN6154   --> MotionSystemTravel
  07.06.13 wl  TXYZTravelAxisPipMoveOperation.MoveXYWithOffset  TN6164   Position speichern, auch wennn XY-Move nicht nötig war
  27.09.13 wl  TXYZTravelAxisPipMoveOperation.Destroy           TN6263   Speicherleck beseitigt
  30.09.13 wl  TXYZTravelAxisPipMoveOp...DetermineInternMove    TN6263   PreviousArmPosition wird vor der Übergabe geprüft
  -------------------------------------------------------------------------------------------------- }

unit OperationAxisMove;


interface


uses
    AppTypes,
    CommonTypes,
    Rack,
    RackTypes,
    Driver,
    IntfMotorDevice,
    GeneralTypes,
    IntfGripDevice,
    MotionSystemTravel,
    MotionSystemPipMove,
    MotionSystemInit,
    IntfZTravelManager,
    IntfArmDevice,
    IntfBalanceDevice,
    ArmPositionInfo;

type
    TZAxisTravelMoveOperation = class
    protected
        fMotionSystem: TZAxisTravelMotionSystem;
    public
        constructor Create(aMotionSystem: TZAxisTravelMotionSystem);
        procedure CombinedArmsMoveToZTravel();
        procedure MoveToZTravelAllTips(aZSpeed: integer = 0; aZRamp: integer = 0);
    end;

    TZAxisPipMoveOperation = class
    protected
        fZMotion: TZAxisPipMoveMotorMotionSystem;
        fZTravelMotion: TZAxisTravelMotorMotionSystem;
        fOnGetPreviousPosition: TGetArmPositionEvent;
        function GetPreviousArmPosition: TArmPositionInfo;
    public
        constructor Create(aZMotion: TZAxisPipMoveMotorMotionSystem;
            aZTravelMotion: TZAxisTravelMotorMotionSystem; aOnGetPreviousPosition: TGetArmPositionEvent);

        function DoZMovement(const aMotorMap: TIPMAP; aZPosition: TRackZPositionType; aZSubmerge_mm: TPosMM;
            aZSpeed, aZRamp: integer; aDisableError: boolean; aZMoveType: integer): TIPMAP;
        function DoZDetect(const aMotorMap: TIPMAP; const aSingleTip: boolean; const aInverseDetect: boolean;
            const aEndZPosition: TRackZPositionType; const aEndZSubmerge_mm: TPosMM;
            const aZScanMode: integer; const aZScanSpeed, aZScanRamp: integer;
            const aStoreDifferenceAsVolume: boolean): TIPMAP;
        function ReadCurrentPositions(const aMotorMap: TIPMAP): TDoubleArray;
    end;

    TZAxisPipMoveWithBalanceOperation = class(TZAxisPipMoveOperation)
    private
        fBalance: IBalanceDevice;
    public
        constructor Create(aZMotion: TZAxisPipMoveMotorMotionSystem;
            aZTravelMotion: TZAxisTravelMotorMotionSystem; aOnGetPreviousPosition: TGetArmPositionEvent;
            aBalance: IBalanceDevice);
        procedure PowderDetection(const aPrevZStep: TPipStepZPos; aUsedTips: integer; aStepLength_mm: TPosMM;
            aZSpeed, aZRamp: integer; aZSubmerge_mm: TPosMM);
    end;

    TXYZTravelAxisPipMoveOperation = class
    private
        fXYMotion: TXYAxisMotorMotionSystem;
        fZTravelMotion: TZAxisTravelMotorMotionSystem;
        fOnGetPreviousPosition: TGetArmPositionEvent;
        fOnSetPreviousPosition: TSetArmPositionEvent;
        class function CreateArmPositionInfo(aUsedTips: TIPMAP; const aRP: TArray<TXRackPosition>;
            const aZStep: TPipStepZPos): TArmPositionInfo;
        function DetermineInternMove(aCurrentPosition: TArmPositionInfo; aInternMoveAllowed: boolean)
            : boolean;
    public
        constructor Create(aXYMotion: TXYAxisMotorMotionSystem; aZTravelMotion: TZAxisTravelMotorMotionSystem;
            aOnGetPreviousPosition: TGetArmPositionEvent; aOnSetPreviousPosition: TSetArmPositionEvent);
        destructor Destroy; override;

        procedure MoveXYSimple(aXYStep: TXYStep);
        procedure MoveXY(aXYStep: TXYStep; const aRP: TArray<TXRackPosition>; const aZStep: TPipStepZPos;
            aOptions: TMoveXYMovementOptions; aXSpeed: integer = 0; aXRamp: integer = 0; aYSpeed: integer = 0;
            aYRamp: integer = 0);
        procedure MoveXYWithOffset(aXYStep: TXYStep; aXOffset, aYOffset: TPosMM;
            const aRP: TArray<TXRackPosition>; const aZStep: TPipStepZPos; aOptions: TMoveXYMovementOptions;
            aXSpeed: integer = 0; aXRamp: integer = 0; aYSpeed: integer = 0; aYRamp: integer = 0);

        procedure MoveX(aXYStep: TXYStep; aOffset: TPosMM; aExec: EXEC_MODE; aSpeed: integer = 0);
        procedure MoveY(aXYStep: TXYStep; aOffset: TPosMM; aExec: EXEC_MODE; aSpeed: integer = 0);
        class function GetXYAspShift(const aRP: TArray<TXRackPosition>; aMotorMap: TIPMAP;
            out oAspShiftXOffset, oAspShiftYOffset: TPosMM): boolean;
    end;

    // Home Move Operation
    THomeMoveOperation = class
    private
        fXYMotion: TXYAxisMotorMotionSystem;
        fZTravelMotion: TZAxisTravelMotorMotionSystem;
        fGripDevice: IGripDevice;
        fRMotorDevice: IRMotorDevice;
    public
        constructor Create(aXYMotion: TXYAxisMotorMotionSystem; aZTravelMotion: TZAxisTravelMotorMotionSystem;
            aGripDevice: IGripDevice; aRMotorDevice: IRMotorDevice);
        procedure MoveToHome(aDirection: TXMotorDirection);
    end;

    // Init operation
    TInitOperation = class
    strict private
        fMotion: TInitMotionSystem;
        fGripper: IGripDevice;
        fOnSetPreviousPosition: TSetArmPositionEvent;
        procedure ResetPreviousPosition;
    public
        constructor Create(aMotion: TInitMotionSystem; aGripper: IGripDevice;
            aOnSetPreviousPosition: TSetArmPositionEvent);
        procedure Init(aInitID: TDevInitID);
    end;


implementation


uses
    SysUtils,
    LogManager,
    SubstanceLoading,
    AppSettings,
    PosinfoDataAdaptor,
    TipMapUtils,
    LayoutManager;

{ TZAxisTravelMoveOperation }

constructor TZAxisTravelMoveOperation.Create(aMotionSystem: TZAxisTravelMotionSystem);
begin
    inherited Create();
    fMotionSystem := aMotionSystem;
end;

procedure TZAxisTravelMoveOperation.CombinedArmsMoveToZTravel;
begin
    fMotionSystem.CombinedArmsMoveToZTravel();
end;

procedure TZAxisTravelMoveOperation.MoveToZTravelAllTips(aZSpeed, aZRamp: integer);
begin
    fMotionSystem.MoveToZTravelAllTips(aZSpeed, aZRamp);
end;

{ TZAxisPipMoveOperation }

constructor TZAxisPipMoveOperation.Create(aZMotion: TZAxisPipMoveMotorMotionSystem;
    aZTravelMotion: TZAxisTravelMotorMotionSystem; aOnGetPreviousPosition: TGetArmPositionEvent);
begin
    inherited Create();

    fZMotion := aZMotion;
    fZTravelMotion := aZTravelMotion;
    fOnGetPreviousPosition := aOnGetPreviousPosition;
end;

function TZAxisPipMoveOperation.GetPreviousArmPosition: TArmPositionInfo;
begin
    result := TArmPositionInfo.GetArmPosition(fOnGetPreviousPosition);
end;

function TZAxisPipMoveOperation.ReadCurrentPositions(const aMotorMap: TIPMAP): TDoubleArray;
begin
    result := fZMotion.ReadCurrentPositions(aMotorMap);
end;

function TZAxisPipMoveOperation.DoZDetect(const aMotorMap: TIPMAP; const aSingleTip: boolean;
    const aInverseDetect: boolean; const aEndZPosition: TRackZPositionType; const aEndZSubmerge_mm: TPosMM;
    const aZScanMode: integer; const aZScanSpeed, aZScanRamp: integer;
    const aStoreDifferenceAsVolume: boolean): TIPMAP;
var
    xPreviousArmPosition: TArmPositionInfo;
begin
    xPreviousArmPosition := self.GetPreviousArmPosition();
    result := fZMotion.DoZDetect(xPreviousArmPosition, aMotorMap, aSingleTip, aInverseDetect, aEndZPosition,
        aEndZSubmerge_mm, aZScanMode, aZScanSpeed, aZScanRamp, aStoreDifferenceAsVolume);
end;

function TZAxisPipMoveOperation.DoZMovement(const aMotorMap: TIPMAP; aZPosition: TRackZPositionType;
    aZSubmerge_mm: TPosMM; aZSpeed, aZRamp: integer; aDisableError: boolean; aZMoveType: integer): TIPMAP;
var
    xPreviousArmPosition: TArmPositionInfo;
begin
    result := 0;
    if (aZPosition = zptOther) then
        EXIT;

    xPreviousArmPosition := self.GetPreviousArmPosition();
    fZMotion.DoZMovement(xPreviousArmPosition, aMotorMap, aZPosition, aZSubmerge_mm, aZSpeed, aZRamp,
        aZMoveType);

    // Execute
    if (aDisableError) then
    begin
        fZMotion.ExecuteWithDisabledZErrors(aMotorMap);
    end
    else
    begin
        fZMotion.Execute;
    end;
end;

{ TArmZMovementOperationWithBalance }

constructor TZAxisPipMoveWithBalanceOperation.Create(aZMotion: TZAxisPipMoveMotorMotionSystem;
    aZTravelMotion: TZAxisTravelMotorMotionSystem; aOnGetPreviousPosition: TGetArmPositionEvent;
    aBalance: IBalanceDevice);
begin
    inherited Create(aZMotion, aZTravelMotion, aOnGetPreviousPosition);

    fBalance := aBalance;
end;

procedure TZAxisPipMoveWithBalanceOperation.PowderDetection(const aPrevZStep: TPipStepZPos;
    aUsedTips: integer; aStepLength_mm: TPosMM; aZSpeed, aZRamp: integer; aZSubmerge_mm: TPosMM);
var
    xStopWeightAbs, xStopWeightRel: double;
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateRobotIni;
    xStopWeightAbs := xIniAccess.ReadInteger('PowderDetection', 'AbsoluteWeight');
    xStopWeightRel := xIniAccess.ReadInteger('PowderDetection', 'RelativeWeight');

    fZMotion.PowderDetection(aPrevZStep, aUsedTips, aStepLength_mm, aZSpeed, aZRamp, aZSubmerge_mm,
        xStopWeightAbs, xStopWeightRel, fBalance);
end;

{ TXYAxisPipMoveOperation }

constructor TXYZTravelAxisPipMoveOperation.Create(aXYMotion: TXYAxisMotorMotionSystem;
    aZTravelMotion: TZAxisTravelMotorMotionSystem; aOnGetPreviousPosition: TGetArmPositionEvent;
    aOnSetPreviousPosition: TSetArmPositionEvent);
begin
    inherited Create();
    fXYMotion := aXYMotion;
    fZTravelMotion := aZTravelMotion;
    fOnGetPreviousPosition := aOnGetPreviousPosition;
    fOnSetPreviousPosition := aOnSetPreviousPosition;
end;

destructor TXYZTravelAxisPipMoveOperation.Destroy;
begin
    FreeAndNil(fZTravelMotion);
    FreeAndNil(fXYMotion);

    inherited;
end;

class function TXYZTravelAxisPipMoveOperation.CreateArmPositionInfo(aUsedTips: TIPMAP;
    const aRP: TArray<TXRackPosition>; const aZStep: TPipStepZPos): TArmPositionInfo;
begin
    if (aUsedTips > 0) and (Length(aRP) > 0) then
        EXIT(TArmPositionInfo.Create(aUsedTips, aRP, aZStep));

    EXIT(nil);
end;

function TXYZTravelAxisPipMoveOperation.DetermineInternMove(aCurrentPosition: TArmPositionInfo;
    aInternMoveAllowed: boolean): boolean;
var
    xPreviousPosition: TCustomArmPositionInfo;
begin
    if not aInternMoveAllowed then
        EXIT(false);

    if not(fZTravelMotion is TZAxisRackTravelMotorMotionSystem) then
        EXIT(false);

    if not Assigned(fOnGetPreviousPosition) then
        EXIT(false);

    xPreviousPosition := fOnGetPreviousPosition;
    if not(xPreviousPosition is TArmPositionInfo) then
        EXIT(false);

    EXIT((fZTravelMotion as TZAxisRackTravelMotorMotionSystem).PrepareInternMovement(aCurrentPosition,
        xPreviousPosition as TArmPositionInfo));
end;

procedure TXYZTravelAxisPipMoveOperation.MoveXY(aXYStep: TXYStep; const aRP: TArray<TXRackPosition>;
    const aZStep: TPipStepZPos; aOptions: TMoveXYMovementOptions; aXSpeed, aXRamp, aYSpeed, aYRamp: integer);
begin
    MoveXYWithOffset(aXYStep, 0, 0, aRP, aZStep, aOptions, aXSpeed, aXRamp, aYSpeed, aYRamp);
end;

procedure TXYZTravelAxisPipMoveOperation.MoveXYWithOffset(aXYStep: TXYStep; aXOffset, aYOffset: TPosMM;
    const aRP: TArray<TXRackPosition>; const aZStep: TPipStepZPos; aOptions: TMoveXYMovementOptions;
    aXSpeed, aXRamp, aYSpeed, aYRamp: integer);
var
    xIsInternMove: boolean;
    xCurrentPosition: TArmPositionInfo;
begin
    xCurrentPosition := CreateArmPositionInfo(aXYStep.Y.MotorMap, aRP, aZStep);

    if (fXYMotion.IsMoveNeeded(aXYStep) or (aXOffset <> 0) or (aYOffset <> 0)) then
    begin
        xIsInternMove := DetermineInternMove(xCurrentPosition, (not(xyoNeverUseRackZTravel in aOptions)));

        fZTravelMotion.MoveToZTravelAllTips(0, 0);

        fXYMotion.MoveXY(aXYStep, aXOffset, aYOffset, aOptions, xIsInternMove, aXSpeed, aXRamp,
            aYSpeed, aYRamp);
    end;

    // Position speichern, auch wennn XY-Move nicht nötig war
    fXYMotion.StoreXYPosition(xCurrentPosition);
end;

procedure TXYZTravelAxisPipMoveOperation.MoveXYSimple(aXYStep: TXYStep);
begin
    (fZTravelMotion as TZAxisRackTravelMotorMotionSystem).ResetInternMovement;

    fZTravelMotion.MoveToZTravelAllTips(0, 0);

    fXYMotion.MoveXY(aXYStep, 0, 0, [], false, 0, 0, 0, 0);
end;

procedure TXYZTravelAxisPipMoveOperation.MoveX(aXYStep: TXYStep; aOffset: TPosMM; aExec: EXEC_MODE;
    aSpeed: integer);
begin
    fXYMotion.MoveX(aXYStep, aOffset, aExec, aSpeed);
end;

procedure TXYZTravelAxisPipMoveOperation.MoveY(aXYStep: TXYStep; aOffset: TPosMM; aExec: EXEC_MODE;
    aSpeed: integer);
begin
    fXYMotion.MoveY(aXYStep, aOffset, aExec, aSpeed);
end;

class function TXYZTravelAxisPipMoveOperation.GetXYAspShift(const aRP: TArray<TXRackPosition>;
    aMotorMap: TIPMAP; out oAspShiftXOffset, oAspShiftYOffset: TPosMM): boolean;
var
    xLastStep: integer;
    xPosinfoDA: TPosinfoDataAdaptor;
    x: integer;
begin
    result := false;
    oAspShiftXOffset := 0;
    oAspShiftYOffset := 0;

    for x := 0 to high(aRP) do
    begin
        if not TTipMapUtils.TipSelected(aMotorMap, x) then
            CONTINUE;

        if (aRP[x].Rack.Structure.Shift_NoOfSteps <= 0) or (aRP[x].Rack.Structure.Shift_Radius_mm <= 0) then
            EXIT;

        result := true;

        xPosinfoDA := TPosinfoDataAdaptor.Create();
        try
            xLastStep := TSubstanceLoading.GetNextStep(xPosinfoDA, aRP[x].Rack.RackID, aRP[x].Pos);
        finally
            FreeAndNil(xPosinfoDA);
        end;

        TXYAxisMotorMotionSystem.GetTubeXYOffset(oAspShiftXOffset, oAspShiftYOffset, xLastStep,
            aRP[x].Rack.RackStructure.Shift_NoOfSteps, aRP[x].Rack.RackStructure.Shift_Radius_mm);
        gLogManager.LogF('Get Aspirate Shifting Values: WellPos=%d, StepNumber=%d, X-Offset=%f, Y-Offset=%f',
            [aRP[x].Pos, xLastStep, oAspShiftXOffset, oAspShiftYOffset], false);

        // aspiration shifting with more than one tips is NOT implemented yet. So EXIT after one tip has been found
        EXIT;
    end;
end;

{ THomeMoveOperation }

constructor THomeMoveOperation.Create(aXYMotion: TXYAxisMotorMotionSystem;
    aZTravelMotion: TZAxisTravelMotorMotionSystem; aGripDevice: IGripDevice; aRMotorDevice: IRMotorDevice);
begin
    inherited Create();
    fXYMotion := aXYMotion;
    fZTravelMotion := aZTravelMotion;
    fGripDevice := aGripDevice;
    fRMotorDevice := aRMotorDevice;
end;

procedure THomeMoveOperation.MoveToHome(aDirection: TXMotorDirection);
begin
    // wichtig: Bei einem Home move müssen beide Seiten des Arms hochfahren!!
    fZTravelMotion.CombinedArmsMoveToZTravel();
    fZTravelMotion.MoveToZTravelAllTips(0, 0);

    if Assigned(fGripDevice) then
    begin
        if not fGripDevice.HasTool then
            fGripDevice.CloseFull();
    end;
    if Assigned(fRMotorDevice) then
        fRMotorDevice.Move(fRMotorDevice.MinPos, m_EXECUTE);
    fXYMotion.MoveXYHome(aDirection);
end;

{ TInitOperation }

constructor TInitOperation.Create(aMotion: TInitMotionSystem; aGripper: IGripDevice;
    aOnSetPreviousPosition: TSetArmPositionEvent);
begin
    inherited Create();
    fMotion := aMotion;
    fGripper := aGripper;
    fOnSetPreviousPosition := aOnSetPreviousPosition;
end;

procedure TInitOperation.Init(aInitID: TDevInitID);
begin
    self.fMotion.Init1(aInitID);

    if Assigned(fGripper) then
        fGripper.Init(aInitID);

    self.fMotion.Init2(aInitID);

    ResetPreviousPosition;
end;

procedure TInitOperation.ResetPreviousPosition;
begin
    if Assigned(fOnSetPreviousPosition) then
        fOnSetPreviousPosition(nil);
end;


end.
