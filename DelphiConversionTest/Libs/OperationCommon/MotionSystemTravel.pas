{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : motion system to travel in xy direction
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                  track-no improvement/change
  -------- --  -------------------------------------   -------- --------------------------------------------------------
  07.03.07 wl                                          TN3620    MotorSystem classes moved from OperationAxisMove to here
  12.03.07 pk  TZAxisTravelLocationMotionSystem        TN3628    New
  11.04.07 pk  TZAxisRackTravelMotorMotionSystem       TN3657    MoveToZTravel_NoCombined_All : the MotorToWorldSteps function was removed
  09.11.07 pk                                          TN3924    Steps changed to mm
  29.01.08 wl                                          TN3980   uses geändert
  06.05.08 wl  TZAxisRackTravelMotorMotionSystem.GetRackZTravel   TN4093  an neues Koordinatensystem angepasst
  19.05.08 wl  TXYAxisMotorMotionSystem.ShiftBy        TN4114    X wird in mm berechnet
  02.07.08 pk  GetRackZTravel                          TN4139    calls CalcRackZTop instead of CreateRotationCorner
  11.09.08 wl  GetRackZTravel                          TN4026    RackZTravel wird von der höchsten Position aus berechnent (wie in TPipStepXYZCalculator.FillTubeDataZ)
  13.01.09 wl  TXYAxisMotorMotionSystem.MoveXY         TN4386    TXYRoute uses default property
  12.09.09 wl                                          TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  12.09.09 wl                                          TN4740   default-Werte aus interface entfernt
  17.12.09 pk  TXYAxisMotorMotionSystem                TN4915    New: calls movedone after execute
  23.03.11 wl  TZAxisTravelMotionSystem                TN5515   MoveToZTravel: Methodennamen geändert
  23.03.11 wl  TZAxisTravelMotionSystem                TN5515   CombinedArmsMoveToZTravel ist jetzt getrennt von anderen Funktionen
  29.03.11 wl  TXYAxisMotorMotionSystem.MoveHXY        TN5524   --> MotionSystemTube, -Rack
  29.03.11 wl  MoveToZTravelAllTips                    TN5516   Z-Bewegungen --> MultiZMotor
  29.03.11 wl  MoveToZTravel                           TN5516   benutzt MultiZMotors.MoveUpIfNecessary
  29.03.11 wl  MoveToAtLeastZTravel                    TN5516   benutzt MultiZMotors.MoveUpIfNecessary
  19.05.11 ts  TXYAxisMotorMotionSystem.Execute        TN5587   Y-Motor kann auch fahren, wenn andere ExecGroup
  17.08.11 ts  MoveToAtleastZTravelAllTips             TN5664   Min instead of Max according to the change of coordinate system
  15.11.11 wl  TXYZRMotorMotionSystem                  TN5736   entspricht bisherigen TXYZRMotorMotionDevice
  03.02.11 wl  PrepareInternMovement                   TN5791   neue Funktion, die das interne Rack-Movement ermittelt
  28.08.12 ts                                          TN5943   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Tube
  16.01.13 wl                                          TN6068   ArmGroupID statt GroupID
  27.02.13 wl  TXYAxisMotorMotionSystem.MoveXYHome     TN6066   X-Position ist abhängig von Direction
  25.04.13 ts  TXYAxisMotorMotionSystem.IsMoveNeeded   TN6141   Logging of "XY-Movement not necessary" corrected
  23.05.13 wl  TZAxisRackTravelMo...PrepareInternMovement  TN6153   verwendet TArmPosition zum Vergleichen von Current/Previous step
  23.05.13 wl  TXYAxisMotorMotionSystem.MoveXY             TN6153   bei jedem Aufruf wird die Position als PreviousArmPosition gespeichert (oder nil)
  23.05.13 wl  TZAxisRackTravelMo..ResetInternMovement     TN6153   neu, entfernt alte Racks aus Array
  05.06.13 wl                                              TN6164   TArmPositionInfo: Bezeichnung "Previous" entfernt
  06.06.13 wl  TXYAxisMotorMotionSystem.GetTubeXYOffset    TN6154   von OperationAxisMove hierher
  07.06.13 wl  TXYAxisMotorMotionSystem.StoreXYPosition    TN6164   kann jetzt unabhängig von MoveXY aufgerufen werden
  12.07.13 ts  TXYZRMotorMotionSystem.MoveR                TN6201   Tolerance for position (RMotor)
  09.08.13 ts  TXYZRMotorMotionSystem.ReadR                TN6201.1 Tolerance beim Lesen an Grenzen (Max,Min) beachten
  27.09.13 wl  TZAxisRackTravelMotorMotionSystem.Create    TN6263   Arrays werden sicherer initialisiert
  30.09.13 wl  TZAxisRackTravelMo...PrepareInternMovement  TN6263   Sporadisch auftretende Access Violation wird jetzt unterdrückt
  21.11.13 ts  Execute                                     TN6309   if UseSingleExecute then the execution is splitted into SingleExecute and SingleExecuteWait
  14.01.14 ts  TXYAxisMotorMotionSystem.Execute            TN6342   SetNextWaiting für X zurückgesetzt, wenn Y-UseOwnExecute, ZP01-Y wartet sonst nicht
  10.02.14 ts  TXYAxisMotorMotionSystem.Execute            TN6354   TN6309 durch TN6342 unwirksam, fix
  03.03.14 tp  MoveZ, MoveToZTravel Uses geaendert, MoveZ,
  MoveToZTravel                                            TN6374   Wegen Geschwindigkeit andergung von Varispan Korrigierung
  ---------------------------------------------------------------------------------------------------------------------- }

unit MotionSystemTravel;


interface


uses
    AppTypes,
    CommonTypes,
    Rack,
    RackTypes,
    IntfMotorDriver,
    IntfMotorDevice,
    IntfGripDevice,
    ArmPositionInfo,
    IntfModuleExecute,
    XYRouteFinder,
    IntfMultiZMotorDevice,
    IntfMultiYMotorDevice,
    IntfArmDevice,
    IntfZTravelManager,
    GeneralTypes;

const
    cRMotorTolerance = 2;

type
    TZAxisTravelMotionSystem = class
    public
        procedure CombinedArmsMoveToZTravel(); virtual;
        procedure MoveToZTravelAllTips(aZSpeed, aZRamp: integer); virtual;
        procedure MoveToZTravel(aMotorMap: TIPMAP; aZSpeed, aZRamp: integer;
            aSingleRetract: boolean = false); virtual;
        procedure MoveToAtleastZTravelAllTips(aDest: TPosMM); virtual;
    end;

    TZAxisTravelMotorMotionSystem = class(TZAxisTravelMotionSystem)
    strict protected
        fMotors: IMultiZMotorDevice;
        fGripDevice: IGripDevice;
    public
        constructor Create(aMotors: IMultiZMotorDevice);
        procedure CombinedArmsMoveToZTravel(); override;
        procedure MoveToZTravelAllTips(aZSpeed, aZRamp: integer); override;
        procedure MoveToZTravel(aMotorMap: TIPMAP; aZSpeed, aZRamp: integer;
            aSingleRetract: boolean = false); override;
        procedure MoveToAtleastZTravelAllTips(aDest: TPosMM); override;
        property Gripper: IGripDevice read fGripDevice write fGripDevice;
    end;

    TZAxisTravelLocationMotionSystem = class(TZAxisTravelMotionSystem)
    end;

    TZAxisRackTravelMotorMotionSystem = class(TZAxisTravelMotorMotionSystem)
    strict private
        fInternMoveRacks: TArray<TRack>;
        fTipZOffsets: TDoubleArray;
        class function GetRackZTravel(aRack: TRack; aZOffsetMM: TPosMM): TPosMM;
    public
        constructor Create(aMotors: IMultiZMotorDevice; const aTipZOffsets: TDoubleArray);
        procedure MoveToZTravel(aMotorMap: TIPMAP; aZSpeed, aZRamp: integer;
            aSingleRetract: boolean = false); override;
        procedure ResetInternMovement();
        function PrepareInternMovement(aCurrentPosition, aPreviousPosition: TArmPositionInfo): boolean;
    end;

    TXYAxisMotionSystem = class
    end;

    TXYAxisMotorMotionSystem = class(TXYAxisMotionSystem)
    strict private
        fX: IXMotorDevice;
        fY: IMultiYMotorDevice;
        fXYRange: TXYRangeData;
        fRotation: TPosMM;
        fY1Offset, fY2Offset: TPosMM;
        fOnSetPreviousPosition: TSetArmPositionEvent;
        procedure Execute;
        procedure RotateXYRange(var vXYRange: TXYRangeData; aRotation: TPosMM);
        function GetXYRange(aOffset1, aOffset2: TPosMM): TXYRangeData;
        function CreateRoute(aXYStep: TXYStep; aUseCorridor: boolean): TXYRoute;
    public
        constructor Create(aXMotor: IXMotorDevice; aYMotors: IMultiYMotorDevice; aXYRange: TXYRangeData;
            aOnSetPreviousPosition: TSetArmPositionEvent);
        function ShiftBy(aXYStep: TXYStep; aXOffset, aYOffset: TPosMM; out oNewStep: boolean): TXYStep;
        function IsMoveNeeded(aXYStep: TXYStep): boolean;
        procedure MoveXY(aXYStep: TXYStep; aXOffset, aYOffset: TPosMM; aOptions: TMoveXYMovementOptions;
            aIsInternMove: boolean; aXSpeed: integer = 0; aXRamp: integer = 0; aYSpeed: integer = 0;
            aYRamp: integer = 0);
        procedure StoreXYPosition(aPreviousArmPosition: TArmPositionInfo);
        procedure MoveXYHome(aDirection: TXMotorDirection);
        procedure MoveX(aXYStep: TXYStep; aOffset: TPosMM; aExec: EXEC_MODE; aSpeed: integer = 0);
        procedure MoveY(aXYStep: TXYStep; aOffset: TPosMM; aExec: EXEC_MODE; aSpeed: integer = 0);
        procedure SetupRangeSettings(aRPos, aGPos: TPosMM);
        class procedure GetTubeXYOffset(var vX, vY: TPosMM; aCurStepNumber, aTotalNumberSteps: integer;
            aRadius: TPosMM);
    end;

    // This Object is just a wrapper for the XYZR motor combination used in Rack/Tube/BC Gripping operations!
    TXYZRMotorMotionSystem = class
    strict private
        fX: IXMotorDevice;
        fY: IYMotorDevice;
        fZ: IZMotorDevice;
        fR: IRMotorDevice;
        fGripDevice: IGripDevice;
        fExecIntf: IModuleExecute;
        function GetXMotor: IXMotorDevice;
        function GetYMotor: IYMotorDevice;
        function GetZMotor: IZMotorDevice;
        function GetRMotor: IRMotorDevice;
        procedure PrepareExecIntfForMotor(aMotor: IMotorDevice);
        procedure PrepareExecIntf();
    public
        constructor Create(aX: IXMotorDevice; aY: IYMotorDevice; aZ: IZMotorDevice; aR: IRMotorDevice);
        destructor Destroy; override;

        procedure Execute();
        procedure MoveX(aDest: TPosMM; aExec: EXEC_MODE); overload;
        procedure MoveX(aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer;
            aRamp: integer); overload;
        procedure MoveY(aDest: TPosMM; aExec: EXEC_MODE); overload;
        procedure MoveY(aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer;
            aRamp: integer); overload;
        procedure MoveZ(aDest: TPosMM; aExec: EXEC_MODE); overload;
        procedure MoveZ(aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer;
            aRamp: integer); overload;
        procedure MoveR(aDest: TPosMM; aExec: EXEC_MODE); overload;
        procedure MoveR(aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer;
            aRamp: integer); overload;
        function ReadR(): TPosMM;

        property XMotor: IXMotorDevice read GetXMotor;
        property YMotor: IYMotorDevice read GetYMotor;
        property ZMotor: IZMotorDevice read GetZMotor;
        property RMotor: IRMotorDevice read GetRMotor;
        property Gripper: IGripDevice read fGripDevice write fGripDevice;
    end;


implementation


uses
    SysUtils,
    Math,
    LogManager,
    DeviceManager,
    ErrorManager,
    GeometricClasses,
    IntfMotionDevice,
    IntfMotorBasedMotionDevice,
    TipMapUtils,
    Carrier;

{ TZAxisTravelMotionSystem }

procedure TZAxisTravelMotionSystem.MoveToAtleastZTravelAllTips(aDest: TPosMM);
begin
end;

procedure TZAxisTravelMotionSystem.CombinedArmsMoveToZTravel;
begin
end;

procedure TZAxisTravelMotionSystem.MoveToZTravel(aMotorMap: TIPMAP; aZSpeed, aZRamp: integer;
    aSingleRetract: boolean);
begin
end;

procedure TZAxisTravelMotionSystem.MoveToZTravelAllTips;
begin
end;

{ TZAxisTravelMotorMotionSystem }

constructor TZAxisTravelMotorMotionSystem.Create(aMotors: IMultiZMotorDevice);
begin
    inherited Create();
    fMotors := aMotors;
end;

procedure TZAxisTravelMotorMotionSystem.CombinedArmsMoveToZTravel;
var
    x: integer;
    xOtherZMotors: IMultiZMotorDevice;
    xMultiZMotorDevices: TArray<IMultiZMotorDevice>;
begin
    xMultiZMotorDevices := gDeviceManager.FindModules<IMultiZMotorDevice>();
    for x := 0 to high(xMultiZMotorDevices) do
    begin
        xOtherZMotors := xMultiZMotorDevices[x];

        // Suche nach einem kombinierten Arm für den angegebenen Arm
        if (not(xOtherZMotors.AnyMotorsActive)) or (fMotors = xOtherZMotors) or (fMotors.ArmGroupID < 0) or
            (fMotors.ArmGroupID <> xOtherZMotors.ArmGroupID) then
            CONTINUE;

        // Kombinierter Arm wurde gefunden: hochfahren
        xOtherZMotors.MoveUpIfNecessaryAllTips();
    end;
end;

procedure TZAxisTravelMotorMotionSystem.MoveToZTravelAllTips(aZSpeed, aZRamp: integer);
begin
    MoveToZTravel(fMotors.GetMotorMap, aZSpeed, aZRamp);
end;

procedure TZAxisTravelMotorMotionSystem.MoveToZTravel(aMotorMap: TIPMAP; aZSpeed, aZRamp: integer;
    aSingleRetract: boolean);
var
    xGripperDestSteps: integer;
    xDirection: integer;
    xFactor: integer;
    xGripperDest: TPosMM;
    xDest: TPosMM;
    xCurrentPos, xCurrentGripperPos: TPosMM;
begin

    xDest := fMotors.CalcZTravel(aMotorMap)[0];
    // Update the Gripper in case of Varispan correction
    if (fMotors.FirstMotor.VarispanCorrectionEnabled) and (fGripDevice <> nil) then
    begin
        xCurrentPos := fMotors.FirstMotor.GetCurrentPos;
        fGripDevice.EnableVSpanCorrection(true);
        fGripDevice.SetVSpanFactor(fMotors.FirstMotor.VarispanFactor);
        fGripDevice.SetZPos(fMotors.FirstMotor.GetStepsFromUnit(fMotors.FirstMotor.WorldToLocal(xDest)));
        xFactor := fMotors.FirstMotor.VarispanFactor;
        xDirection := Round((xDest - xCurrentPos) / // fMotors.FirstMotor.GetCurrentPos()) /
            Abs(xDest - fMotors.FirstMotor.GetCurrentPos()));
        xGripperDestSteps := Round(Abs(xDest - fMotors.FirstMotor.GetCurrentPos()));
        xGripperDestSteps := fMotors.FirstMotor.GetStepsFromUnit(xGripperDestSteps);
        xGripperDestSteps := Round(xGripperDestSteps / xFactor);
        xCurrentGripperPos := fGripDevice.GetPosition(true);
        xGripperDestSteps := xDirection * xGripperDestSteps + fGripDevice.GetStepsFromUnits
            (xCurrentGripperPos); // fGripDevice.GetPosition(true));
        xGripperDest := fGripDevice.GetUnitsFromSteps(xGripperDestSteps);
        fGripDevice.SetToMinPosIfIsBeyondMin(xGripperDest);
        fGripDevice.SetToMaxPosIfIsBeyondMax(xGripperDest);
        fGripDevice.Move(xGripperDest, xDirection, m_NO_EXEC, fGripDevice.GetVariSpanCorrectionSpeed());
    end;

    fMotors.MoveUpIfNecessary(aMotorMap, fMotors.CalcZTravel(aMotorMap), aSingleRetract, aZSpeed, aZRamp);
end;

procedure TZAxisTravelMotorMotionSystem.MoveToAtleastZTravelAllTips(aDest: TPosMM);
var
    xMotorMap: TIPMAP;
    x: integer;
    xDest: TDoubleArray;
begin
    xMotorMap := fMotors.GetMotorMap;
    xDest := fMotors.CalcZTravel(xMotorMap);
    for x := 0 to fMotors.MaxIndex do
    begin
        if TTipMapUtils.TipSelected(xMotorMap, x) then
        begin
            xDest[x] := Min(xDest[x], aDest);
        end;
    end;

    fMotors.MoveUpIfNecessary(fMotors.GetMotorMap, xDest, false, 0, 0);
end;

{ TZAxisRackTravelMotorMotionSystem }

constructor TZAxisRackTravelMotorMotionSystem.Create(aMotors: IMultiZMotorDevice;
    const aTipZOffsets: TDoubleArray);
var
    x: Integer;
begin
    inherited Create(aMotors);
    SetLength(fTipZOffsets, aMotors.MaxIndex + 1);
    SetLength(fInternMoveRacks, aMotors.MaxIndex + 1);
    for x := 0 to high(fTipZOffsets) do
    begin
        fInternMoveRacks[x] := nil;
        if (x <= high(aTipZOffsets)) then
            fTipZOffsets[x] := aTipZOffsets[x]
        else
            fTipZOffsets[x] := 0;
    end;
end;

class function TZAxisRackTravelMotorMotionSystem.GetRackZTravel(aRack: TRack; aZOffsetMM: TPosMM): TPosMM;
var
    xHighestZPos_mm: TPosMM;
begin
    xHighestZPos_mm := aRack.CalcHighestZPos();
    result := AddZHeight(AddZHeight(xHighestZPos_mm, aZOffsetMM), -aRack.Structure.ZTravel_mm);
end;

function TZAxisRackTravelMotorMotionSystem.PrepareInternMovement(aCurrentPosition,
    aPreviousPosition: TArmPositionInfo): boolean;
var
    x: integer;
    xAnyInternMove: boolean;
begin
    xAnyInternMove := false;

    for x := 0 to high(fInternMoveRacks) do
    begin
        fInternMoveRacks[x] := nil;

        // Ist die Info definiert
        if not Assigned(aCurrentPosition) or not Assigned(aPreviousPosition) then
            CONTINUE;

        // Ist dieser Tip in beiden Schritten enthalten
        if not TTipMapUtils.TipSelected(aCurrentPosition.UsedTips and aPreviousPosition.UsedTips, x) then
            CONTINUE;

        // Sind die Positionen definiert
        if (x > high(aCurrentPosition.RP)) or (x > high(aPreviousPosition.RP)) then
            CONTINUE;

        // Sind die Racks definiert
        if not Assigned(aCurrentPosition.RP[x].Rack) or not Assigned(aPreviousPosition.RP[x].Rack) then
            CONTINUE;

        try
            // Beim Zugriff auf das vorhergegangene Rack kommt es sporadisch zu einer Access Violation
            TLogManager.Instance.Log(aPreviousPosition.RP[x].Rack.name, false);
        except
            TLogManager.Instance.Log('Suppressed Access Violation -> No intern rack movement', false);
            EXIT(false);
        end;

        // Sind die Racks identisch
        if (aCurrentPosition.RP[x].Rack.name <> aPreviousPosition.RP[x].Rack.name) then
            CONTINUE;

        xAnyInternMove := true;
        fInternMoveRacks[x] := aCurrentPosition.RP[x].Rack;
        gLogManager.LogF('Intern rack movement - rack: %s, tip: %d',
            [aCurrentPosition.RP[x].Rack.Name, x + 1], false);
    end;
    EXIT(xAnyInternMove);
end;

procedure TZAxisRackTravelMotorMotionSystem.ResetInternMovement;
var
    x: integer;
begin
    for x := 0 to high(fInternMoveRacks) do
        fInternMoveRacks[x] := nil;
end;

procedure TZAxisRackTravelMotorMotionSystem.MoveToZTravel(aMotorMap: TIPMAP; aZSpeed, aZRamp: integer;
    aSingleRetract: boolean);
var
    x: integer;
    xZTravelArr: TDoubleArray;
begin
    // default: move to ztravel
    xZTravelArr := fMotors.CalcZTravel(aMotorMap);

    // for tips in InternMoveMotorMap move to rackztravel
    for x := 0 to fMotors.MaxIndex do
    begin
        if Assigned(fInternMoveRacks[x]) then
            xZTravelArr[x] := GetRackZTravel(fInternMoveRacks[x], fTipZOffsets[x]);
    end;

    fMotors.MoveUpIfNecessary(aMotorMap, xZTravelArr, aSingleRetract, aZSpeed, aZRamp);
end;

{ TXYAxisMotorMotionSystem }

constructor TXYAxisMotorMotionSystem.Create(aXMotor: IXMotorDevice; aYMotors: IMultiYMotorDevice;
    aXYRange: TXYRangeData; aOnSetPreviousPosition: TSetArmPositionEvent);
begin
    inherited Create();
    fX := aXMotor;
    fY := aYMotors;
    fXYRange := aXYRange;
    fOnSetPreviousPosition := aOnSetPreviousPosition;
    fY1Offset := 0;
    fY2Offset := 0;
    fRotation := 0;
end;

procedure TXYAxisMotorMotionSystem.RotateXYRange(var vXYRange: TXYRangeData; aRotation: TPosMM);
var
    xTempValue: TPosMM;
begin
    // Change current XY range by current rotation motor position
    case Round(aRotation) of
        0:
            ; // nichts tun
        90:
            begin
                // 90° drehen
                xTempValue := vXYRange.XRel1;
                vXYRange.XRel1 := vXYRange.YRel2;
                vXYRange.YRel2 := vXYRange.XRel2;
                vXYRange.XRel2 := vXYRange.YRel1;
                vXYRange.YRel1 := xTempValue;
            end;
        180:
            begin
                // 180° drehen
                xTempValue := vXYRange.XRel1;
                vXYRange.XRel1 := vXYRange.XRel2;
                vXYRange.XRel2 := xTempValue;
                xTempValue := vXYRange.YRel1;
                vXYRange.YRel1 := vXYRange.YRel2;
                vXYRange.YRel2 := xTempValue;
            end;
        270:
            begin
                // 270° drehen
                xTempValue := vXYRange.XRel1;
                vXYRange.XRel1 := vXYRange.YRel1;
                vXYRange.YRel1 := vXYRange.XRel2;
                vXYRange.XRel2 := vXYRange.YRel2;
                vXYRange.YRel2 := xTempValue;
            end;
        else
            begin
                // alle Richtungen Höchstwert überallhin
                vXYRange.XRel1 := MaxValue([vXYRange.XRel1, vXYRange.XRel2, vXYRange.YRel1, vXYRange.YRel2]);
                vXYRange.XRel2 := vXYRange.XRel1;
                vXYRange.YRel1 := vXYRange.XRel1;
                vXYRange.YRel2 := vXYRange.XRel1;
            end;
    end;

end;

class procedure TXYAxisMotorMotionSystem.GetTubeXYOffset(var vX, vY: TPosMM;
    aCurStepNumber, aTotalNumberSteps: integer; aRadius: TPosMM);
var
    xAngle: extended;
begin
    xAngle := 2 * Pi / aTotalNumberSteps; // Berechnen des Schritt-Winkels im Bogenmass
    xAngle := (aCurStepNumber mod aTotalNumberSteps) * xAngle; // Berechnen des individuellen Winkels im BM
    vY := vY + Round(Cos(xAngle) * aRadius); // Y-Mass in Steps
    vX := vX + Round(Sin(xAngle) * aRadius); // X-Mass in Steps
end;

function TXYAxisMotorMotionSystem.GetXYRange(aOffset1, aOffset2: TPosMM): TXYRangeData;
begin
    result := fXYRange;
    result.YRel1 := result.YRel1 + fY1Offset + aOffset1;
    result.YRel2 := result.YRel2 + fY2Offset + aOffset2;

    RotateXYRange(result, fRotation);
end;

procedure TXYAxisMotorMotionSystem.SetupRangeSettings(aRPos, aGPos: TPosMM);
begin
    fY1Offset := +(aGPos / 2);
    fY2Offset := fY1Offset;

    fRotation := aRPos;
end;

function TXYAxisMotorMotionSystem.CreateRoute(aXYStep: TXYStep; aUseCorridor: boolean): TXYRoute;
var
    xRouteFinder: TXYRouteFinder;
    xCurrentX: TPosMM;
    xCurrentRefY: TPosMM;
    xXYRange: TXYRangeData;
    xDestX, xDestYRef: TPosMM;
    xCurRange1, xCurRange2, xDestRange1, xDestRange2, xMaxRange1, xMaxRange2: TPosMM;
    xFinalYStep: TMultiYStep;
begin
    result := nil;

    // DeterminRange
    fY.GetCurrentTipRange(xCurRange1, xCurRange2);
    fY.GetTipRangeOf(aXYStep.Y, xDestRange1, xDestRange2);
    xMaxRange1 := Max(xCurRange1, xDestRange1);
    xMaxRange2 := Max(xCurRange2, xDestRange2);
    xXYRange := GetXYRange(xMaxRange1, xMaxRange2);
    // Get current ref pos
    xCurrentX := fX.CurrentPos;
    xCurrentRefY := fY.RefReadCurrentPos;
    // Get destination ref pos
    xDestYRef := fY.GetRefPosOf(aXYStep.Y);
    xDestX := aXYStep.x;
    xRouteFinder := TXYRouteFinder.Create(fX.MinPos, fY.RefMinPos, fX.MaxPos, fY.RefMaxPos);

    if xRouteFinder.AnyRelevantItem(xXYRange, xCurrentX, xCurrentRefY, xDestX, xDestYRef, aUseCorridor) then
    begin

        // in Y zusammenfahren!
        fY.MinimizeTipRange(xCurrentRefY);
        fY.GetCurrentTipRange(xCurRange1, xCurRange2);
        if (xCurRange1 <> xMaxRange1) or (xCurRange2 <> xMaxRange2) then
            xXYRange := GetXYRange(xCurRange1, xCurRange2);

        result := TXYRoute.Create(xCurrentX, xCurrentRefY);
        xRouteFinder.CreateRoute(result, xXYRange, xDestX, xDestYRef, aUseCorridor);

        // we will recalculate the aXYStep.Y so that we do not unecessarily move motors which dont have to be moved
        // This occurs when we do minimizetiprange which brings all the motors close together, but the aXYStep.Y still
        // thinks that uneeded motors should be at the original position so it moves the motors to their original position
        // after the barrier move
        if result.Count > 1 then
        begin
            // get the final step of the route
            xFinalYStep := fY.GetStepFromRefMove(result[result.Count - 1].Y);
            // recalculate the y move
            fY.VerifyStepBounds(aXYStep.Y, xFinalYStep);
            FreeAndNil(xFinalYStep);
        end;
    end;

    xRouteFinder.Free;
end;

function TXYAxisMotorMotionSystem.IsMoveNeeded(aXYStep: TXYStep): boolean;
begin
    result := false;
    // Check if movement is necessary
    if fX.IsMoveRequired(aXYStep.x) or fY.IsMoveRequired(aXYStep.Y) then
        result := true
    else
        gLogManager.Log('XY-Movement not necessary', false);
end;

procedure TXYAxisMotorMotionSystem.MoveX(aXYStep: TXYStep; aOffset: TPosMM; aExec: EXEC_MODE;
    aSpeed: integer = 0);
var
    xStep: TXYStep;
    xNewStep: boolean;
begin
    xStep := ShiftBy(aXYStep, aOffset, 0, xNewStep);
    try
        fX.Move(xStep.x, aExec, AT_MOVE_ABS, aSpeed);
    finally
        if xNewStep then
            FreeAndNil(xStep);
    end;
end;

procedure TXYAxisMotorMotionSystem.MoveY(aXYStep: TXYStep; aOffset: TPosMM; aExec: EXEC_MODE;
    aSpeed: integer = 0);
var
    xStep: TXYStep;
    xNewStep: boolean;
begin
    xStep := ShiftBy(aXYStep, 0, aOffset, xNewStep);
    try
        fY.MoveY(xStep.Y, aExec, AT_MOVE_ABS, aSpeed, 0);
    finally
        if xNewStep then
            FreeAndNil(xStep);
    end;
end;

function TXYAxisMotorMotionSystem.ShiftBy(aXYStep: TXYStep; aXOffset, aYOffset: TPosMM;
    out oNewStep: boolean): TXYStep;
begin
    if (aXOffset = 0) and (aYOffset = 0) then
    begin
        oNewStep := false;
        result := aXYStep;
        EXIT;
    end;

    result := aXYStep.Clone();
    oNewStep := true;
    if aXOffset <> 0 then
        result.x := result.x + aXOffset;

    if aYOffset <> 0 then
        fY.ShiftBy(result.Y, aYOffset);
end;

procedure TXYAxisMotorMotionSystem.Execute();
begin
    // either the x or the y motor should do execute because both should be in the same execution group anyways
    if Assigned(fX) then
    begin
        if fY.UseOwnExecute then
            fX.GetExecIntf.SingleExecute
        else
            fX.GetExecIntf.Execute();
    end
    else if Assigned(fY) then
    begin
        fY.Execute();
    end;

    if fY.UseOwnExecute then
    begin
        fY.Execute;
        fX.GetExecIntf.SingleExecuteWait;
        if Assigned(fX) then // wichtig, da sonst nicht auf den YMotor gewartet wird (ZP01)
            fX.SetNextWaiting(false);
    end;

    if Assigned(fX) then
        fX.MoveDone(); // inform XMotor conflict manager that move is done
end;

procedure TXYAxisMotorMotionSystem.MoveXY(aXYStep: TXYStep; aXOffset, aYOffset: TPosMM;
    aOptions: TMoveXYMovementOptions; aIsInternMove: boolean; aXSpeed, aXRamp, aYSpeed, aYRamp: integer);
var
    x: integer;
    xXYRoute: TXYRoute;
    xStep: TXYStep;
    xNewStep: boolean;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT; // return on Error

    // Get new step with offset
    xStep := self.ShiftBy(aXYStep, aXOffset, aYOffset, xNewStep);

    try
        if (not aIsInternMove) // no barrier check for rack internal movement
            and (fX <> nil) and (fY.AnyMotorsActive()) then
        begin

            xXYRoute := self.CreateRoute(xStep, (xyoUseCorridor in aOptions));
            if (xXYRoute is TXYRoute) then
            begin
                for x := 1 to xXYRoute.Count - 1 do
                begin
                    if (gErrorManager.IsGlobalErr) then
                        break;
                    fX.Move(xXYRoute[x].x, m_NO_EXEC, AT_MOVE_ABS, aXSpeed, aXRamp);
                    fY.MoveRelativeToRef(xXYRoute[x].Y, m_NO_EXEC, AT_MOVE_ABS, aYSpeed, aYRamp);
                    Execute();
                end;
                FreeAndNil(xXYRoute);
            end;
        end;

        fX.Move(xStep.x, m_NO_EXEC, AT_MOVE_ABS, aXSpeed, aXRamp);
        if Assigned(fX) then
            fX.SetNextWaiting(true);
        fY.MoveY(xStep.Y, m_NO_EXEC, AT_MOVE_ABS, aYSpeed, aYRamp);
        Execute();

        if Assigned(fX) then
            fX.SetNextWaiting(false);
    finally
        if xNewStep then
            FreeAndNil(xStep);
    end;

    // nach jeder XY-Bewegung wird die Position gelöscht
    if Assigned(fOnSetPreviousPosition) then
        fOnSetPreviousPosition(nil);
end;

procedure TXYAxisMotorMotionSystem.StoreXYPosition(aPreviousArmPosition: TArmPositionInfo);
begin
    // nach jeder XY-Bewegung wird die Position gespeichert (Für ZPOSM-Action & Intern rack move)
    if Assigned(fOnSetPreviousPosition) then
        fOnSetPreviousPosition(aPreviousArmPosition);
end;

procedure TXYAxisMotorMotionSystem.MoveXYHome(aDirection: TXMotorDirection);
var
    xXYStep: TXYStep;
begin
    xXYStep := TXYStep.Create(fX.GetConflictHomePos(aDirection), fY.GetHomeStep());
    try
        MoveXY(xXYStep, 0, 0, [], false);
    finally
        FreeAndNil(xXYStep);
    end;
end;

{ TXYZRMotorMotionSystem }

constructor TXYZRMotorMotionSystem.Create(aX: IXMotorDevice; aY: IYMotorDevice; aZ: IZMotorDevice;
    aR: IRMotorDevice);
begin
    inherited Create();

    fX := aX;
    fY := aY;
    fZ := aZ;
    fR := aR;
end;

destructor TXYZRMotorMotionSystem.Destroy();
begin
    inherited;
end;

procedure TXYZRMotorMotionSystem.PrepareExecIntfForMotor(aMotor: IMotorDevice);
begin
    if not Assigned(aMotor) then
        EXIT;
    if Assigned(fExecIntf) then
        ASSERT(aMotor.GetExecIntf = fExecIntf)
    else
        fExecIntf := aMotor.GetExecIntf;
end;

procedure TXYZRMotorMotionSystem.PrepareExecIntf;
begin
    if fExecIntf <> nil then
        EXIT;
    PrepareExecIntfForMotor(fX);
    PrepareExecIntfForMotor(fY);
    PrepareExecIntfForMotor(fZ);
    PrepareExecIntfForMotor(fR);
    ASSERT(Assigned(fExecIntf));
end;

procedure TXYZRMotorMotionSystem.Execute;
begin
    PrepareExecIntf();
    fExecIntf.Execute();
end;

procedure TXYZRMotorMotionSystem.MoveX(aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE;
    aSpeed, aRamp: integer);
begin
    if Assigned(fX) then
        fX.Move(aDest, aExec, aMode, aSpeed, aRamp);
end;

procedure TXYZRMotorMotionSystem.MoveX(aDest: TPosMM; aExec: EXEC_MODE);
begin
    self.MoveX(aDest, aExec, AT_MOVE_ABS, 0, 0);
end;

procedure TXYZRMotorMotionSystem.MoveY(aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE;
    aSpeed, aRamp: integer);
begin
    if Assigned(fY) then
        fY.Move(aDest, aExec, aMode, aSpeed, aRamp);
end;

procedure TXYZRMotorMotionSystem.MoveY(aDest: TPosMM; aExec: EXEC_MODE);
begin
    self.MoveY(aDest, aExec, AT_MOVE_ABS, 0, 0);
end;

procedure TXYZRMotorMotionSystem.MoveZ(aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer;
    aRamp: integer);
var
    xGripperDestSteps: integer;
    xDirection: integer;
    xFactor: integer;
    xGripperDest: TPosMM;
begin
    // Update the Gripper in case of Varispan correction
    if (fZ.VarispanCorrectionEnabled) then
    begin
        fGripDevice.EnableVSpanCorrection(true);
        fGripDevice.SetVSpanFactor(fZ.VarispanFactor);
        fGripDevice.SetZPos(fZ.GetStepsFromUnit(fZ.WorldToLocal(aDest)));
        xFactor := fZ.VarispanFactor;
        xDirection := Round((aDest - fZ.GetCurrentPos()) / Abs(aDest - fZ.GetCurrentPos()));
        xGripperDestSteps := Round(Abs(aDest - fZ.GetCurrentPos()));
        xGripperDestSteps := fZ.GetStepsFromUnit(xGripperDestSteps);
        xGripperDestSteps := Round(xGripperDestSteps / xFactor);
        xGripperDestSteps := xDirection * xGripperDestSteps + fGripDevice.GetStepsFromUnits
            (fGripDevice.GetPosition(true));
        xGripperDest := fGripDevice.GetUnitsFromSteps(xGripperDestSteps);
        fGripDevice.SetToMinPosIfIsBeyondMin(xGripperDest);
        fGripDevice.SetToMaxPosIfIsBeyondMax(xGripperDest);
        fGripDevice.Move(xGripperDest, xDirection, m_NO_EXEC, fGripDevice.GetVariSpanCorrectionSpeed());
    end;

    if Assigned(fZ) then
        fZ.Move(aDest, aExec, aMode, aSpeed, aRamp);
end;

procedure TXYZRMotorMotionSystem.MoveZ(aDest: TPosMM; aExec: EXEC_MODE);
begin
    self.MoveZ(aDest, aExec, AT_MOVE_ABS, 0, 0);
end;

procedure TXYZRMotorMotionSystem.MoveR(aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer;
    aRamp: integer);
begin
    if Assigned(fR) then
    begin
        if fR.IsMoveRequired(aDest, cRMotorTolerance) then // Tolerance for RMotor
            fR.Move(aDest, aExec, aMode, aSpeed, aRamp);
    end;
end;

procedure TXYZRMotorMotionSystem.MoveR(aDest: TPosMM; aExec: EXEC_MODE);
begin
    self.MoveR(aDest, aExec, AT_MOVE_ABS, 0, 0);
end;

function TXYZRMotorMotionSystem.ReadR: TPosMM;
begin
    result := 0;
    if Assigned(fR) then
        result := fR.ReadCurrentPos();
    if ((result - cRMotorTolerance) <= fR.MinPos) then
        result := fR.MinPos
    else if ((result + cRMotorTolerance) >= fR.MaxPos) then
        result := fR.MaxPos;
end;

function TXYZRMotorMotionSystem.GetXMotor: IXMotorDevice;
begin
    result := fX;
end;

function TXYZRMotorMotionSystem.GetYMotor: IYMotorDevice;
begin
    result := fY;
end;

function TXYZRMotorMotionSystem.GetZMotor: IZMotorDevice;
begin
    result := fZ;
end;

function TXYZRMotorMotionSystem.GetRMotor: IRMotorDevice;
begin
    result := fR;
end;


end.
