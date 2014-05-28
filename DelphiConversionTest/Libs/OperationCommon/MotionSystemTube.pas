{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : motion system to move tubes
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                  track-no improvement/change
  -------- --  -------------------------------------   -------- ------------------------------------
  07.03.07 wl                                          TN3620    MotorSystem classes moved from OperationTube to here
  14.06.07 wl  TTubeMoveMotorMotionSystem.MoveTubeFromPos2    TN3728  R-Motor wird nur auf 0 gefahren wenn KeepRotation = false
  09.11.07 pk                                          TN3924    Steps changed to mm
  07.01.08 pk                                          TN3971    ZHeights are added via AddZHeight function
  29.01.08 wl                                          TN3980   uses geändert
  20.06.08 pk                                          TN4139   WB global object replaced by LayoutManager
  20.06.08 pk                                          TN4139   Rack no longer has typed link to Carrier use TLayout.GetCarrierOfRack
  02.07.08 pk  GetTubeDataXY                           TN4139   calls GetTubePos with Pos instead of Row and Col
  07.07.08 pk                                          TN4139   WE_X changed to PosX
  01.08.08 pk  GetMoveTubeData                         TN3924   do not convert HT_RStart_degree.  It should already be in degrees
  29.10.08 wl  GetMoveTubeData                         TN4290   zu Tool.ZDrop wird Rack.Structure.TubePutOffset_mm addiert
  29.10.08 wl  MoveTubeToPos2, MoveTubeFromPos1        TN4290   zu Tool.ZDrop wird Rack.Structure.TubePutOffset_mm addiert (sieht falsch aus)
  10.12.08 wl  TTubeMoveLocationMotionSystem.MoveTubeFromPos1  TN4353   statt einzelner X- und Z-Moves wird MoveFromGripPosition aufgerufen
  10.12.08 wl  TTubeMoveLocationMotionSystem.MoveTubeToPos2    TN4353   statt einzelner X- und Z-Moves wird MoveToGripPosition aufgerufen
  13.03.09 wl  TTubeMoveMotorMotionSystem.GetMoveTubeData      TN4460   neu: Zusätzlicher Z-Offset
  13.08.09 pk  GetMoveTubeData                                 TN4722   Use CalcCarrierPosition function instead of Carrier.PosX, .PosY
  08.09.09 pk                                                  TN4753   uses ErrorMessage replaced by ErrorInfo
  12.09.09 wl                                                  TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  23.03.11 wl                                                  TN5515   MoveToZTravel: Methodenname geändert
  29.03.11 wl  TRackMoveMotorMotionSystem.MoveHXY              TN5524   von MotionSystemTravel hierher
  29.03.11 wl                                                  TN5524   OperationPip -> MotorStepCalculator
  29.03.11 wl  TTubeMoveMotorMotionSystem.GetMoveTubeData      TN5524   Logzeile nicht mehr im Display!
  05.05.11 ts                                                  TN5552   new: TubeGripOffset_mm
  12.10.11 ts  MoveHXY                                         TN5711   if aAddTipOffset is true, Offsets from TipType will be added
  19.10.11 wl  TTubeMoveMotorMotionSystem.GetMoveTubeData      TN5723  PaintTubePos entfernt
  15.11.11 wl                                                  TN5736   verwendet TXYZRMotorMotionSystem
  28.08.12 ts                                                  TN5943   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Tube
  06.06.13 wl  MoveTubeToPos2, MoveTubeToPos                   TN6154   neuer Parameter IsGetTube
  06.06.13 wl  TTubeMoveMotorMotionSystem.DoSafeMove           TN6154   neu: Für GetTube gibt es einen Retry, wenn der Z-Motor in den Fehler kommt
  -------------------------------------------------------------------------------------------------- }

unit MotionSystemTube;


interface


uses
    AppTypes,
    CommonTypes,
    RackTypes,
    IntfMotorDevice,
    MotionSystemTravel,
    IntfGripDevice,
    IntfMotionDevice,
    IntfMotorBasedMotionDevice,
    IntfArmDevice,
    Rack,
    Carrier,
    MotorStepCalculator;

type
    TMoveTubeData = record
        hXDest, hYDest, hZDest, XStart, YStart, RStart, HZStart: TPosMM;
        hYStartBeforeX: boolean;
        KeepRotation: boolean;
    end;

    TMoveTubeMode = (mtmNoStartPosBeforeGetTube, mtmNoStartPosAfterGetTube, mtmNoStartPosBeforePutTube,
        mtmNoStartPosAfterPutTube);
    TMoveTubeModes = set of TMoveTubeMode;

    TTubeMoveMotionSystem = class
    public
        procedure MoveToTubeXY(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM); virtual; abstract;

        procedure MoveTubeFromPos1(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM); virtual; abstract;
        procedure MoveTubeFromPos2(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM; aZSpeed, aZRamp: integer);
            virtual; abstract;
        procedure MoveTubeToPos1(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM); virtual; abstract;
        procedure MoveTubeToPos2(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos, aUseSafeMove, aIsGetTube: boolean; aToolData: TTubeToolData; aZOffset: TPosMM;
            aZSpeed, aZRamp: integer); virtual; abstract;
        procedure MoveYShakeOnce(aDest: TPosMM); virtual; abstract;
        procedure MoveZShakeOnce(aDest: TPosMM; aSlow: boolean); virtual; abstract;
    end;

    TTubeMoveMotorMotionSystem = class(TTubeMoveMotionSystem)
    strict private
        fMotors: TXYZRMotorMotionSystem;
        fXYMotion: TXYAxisMotorMotionSystem;
        fZTravelMotion: TZAxisTravelMotorMotionSystem;
        fPipStepCalculator: TMotorXYStepCalculator;
        procedure MoveHXY(aHX, aHY: TPosMM; aOptions: TMoveXYMovementOptions; aAddTipOffset: boolean);
        function GetMoveTubeData(aRackPos: TXRackPosition; aUsePutOffset: boolean; aToolData: TTubeToolData;
            aZOffset: TPosMM): TMoveTubeData;
        procedure DoSafeMove(aZDest: TPosMM; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos, aUseSafeMove, aIsGetTube: boolean; aToolData: TTubeToolData; aZOffset: TPosMM;
            aZSpeed, aZRamp: integer);
    public
        constructor Create(aMotors: TXYZRMotorMotionSystem; aXYMotion: TXYAxisMotorMotionSystem;
            aZTravelMotion: TZAxisTravelMotorMotionSystem; aPipStepCalculator: TMotorXYStepCalculator);
        destructor Destroy; override;

        procedure MoveToTubeXY(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM); override;

        procedure MoveTubeFromPos1(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM); override;
        procedure MoveTubeFromPos2(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM;
            aZSpeed, aZRamp: integer); override;
        procedure MoveTubeToPos1(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM); override;
        procedure MoveTubeToPos2(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos, aUseSafeMove, aIsGetTube: boolean; aToolData: TTubeToolData; aZOffset: TPosMM;
            aZSpeed, aZRamp: integer); override;

        procedure MoveYShakeOnce(aDest: TPosMM); override;
        procedure MoveZShakeOnce(aDest: TPosMM; aSlow: boolean); override;

    end;

    TTubeMoveLocationMotionSystem = class(TTubeMoveMotionSystem)
    strict private
        fMotionDevices: ILocationBasedMotionDevice;
    public
        constructor Create(aMotionDevices: ILocationBasedMotionDevice);
        procedure MoveToTubeXY(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM); override;

        procedure MoveTubeFromPos1(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM); override;
        procedure MoveTubeFromPos2(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM;
            aZSpeed, aZRamp: integer); override;
        procedure MoveTubeToPos1(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM); override;
        procedure MoveTubeToPos2(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aUseStartPos, aUseSafeMove, aIsGetTube: boolean; aToolData: TTubeToolData; aZOffset: TPosMM;
            aZSpeed, aZRamp: integer); override;

        procedure MoveYShakeOnce(aDest: TPosMM); override;
        procedure MoveZShakeOnce(aDest: TPosMM; aSlow: boolean); override;
    end;


implementation


uses
    SysUtils,
    Controls,
    LogManager,
    ErrorManager,
    ErrorMessageFactory,
    GeometricClasses,
    IntfMotorDriver,
    IntfLocationBasedAllAxesDriver,
    SamGlobe,
    ErrorInfo,
    Layout,
    MatrixMath;

{ TTubeMoveMotorMotionSystem }

constructor TTubeMoveMotorMotionSystem.Create(aMotors: TXYZRMotorMotionSystem;
    aXYMotion: TXYAxisMotorMotionSystem; aZTravelMotion: TZAxisTravelMotorMotionSystem;
    aPipStepCalculator: TMotorXYStepCalculator);
begin
    inherited Create();
    fMotors := aMotors;
    fXYMotion := aXYMotion;
    fZTravelMotion := aZTravelMotion;
    fPipStepCalculator := aPipStepCalculator;
end;

destructor TTubeMoveMotorMotionSystem.Destroy;
begin
    FreeAndNil(fMotors);
    inherited;
end;

procedure TTubeMoveMotorMotionSystem.MoveToTubeXY(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM);
var
    xMTD: TMoveTubeData;
begin
    // --------------------------------------- berechnen der Tube X+Y Position
    xMTD := self.GetMoveTubeData(aRackPos, aUsePutOffset, aToolData, aZOffset);
    // ------------------------------ Gehe zur Startposition (benutzt XStart-,YStart-,HStart-Offset)
    if (not gErrorManager.IsGlobalErr) and (aUseStartPos) then
    begin
        MoveHXY(xMTD.XStart, xMTD.YStart, [], false);
    end;
end;

procedure TTubeMoveMotorMotionSystem.MoveHXY(aHX, aHY: TPosMM; aOptions: TMoveXYMovementOptions;
    aAddTipOffset: boolean);
var
    xXYStep: TXYStep;
begin
    if gErrorManager.IsGlobalErr then
        EXIT; // return on Error

    fPipStepCalculator.SetXYBySinglePos(aHX, aHY);
    xXYStep := fPipStepCalculator.CalculateXYStep(aAddTipOffset);
    try
        if not fXYMotion.IsMoveNeeded(xXYStep) then
            EXIT;

        fZTravelMotion.MoveToZTravelAllTips(0, 0);

        fXYMotion.MoveXY(xXYStep, 0, 0, aOptions, false);
    finally
        FreeAndNil(xXYStep);
    end;
end;

procedure TTubeMoveMotorMotionSystem.MoveTubeToPos1(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM);
var
    xMTD: TMoveTubeData;
begin
    // --------------------------------------- berechnen der Tube X+Y Position
    xMTD := self.GetMoveTubeData(aRackPos, aUsePutOffset, aToolData, aZOffset);
    // ------------------------------ Gehe zur Startposition (benutzt XStart-,YStart-,HStart-Offset)
    if (not gErrorManager.IsGlobalErr) and (aUseStartPos) then
    begin
        MoveHXY(xMTD.XStart, xMTD.YStart, [], false);
    end;
    // ------------------------------------------------------------------------- rotation = HRStart
    if (not gErrorManager.IsGlobalErr) and (aRotation + xMTD.RStart > 0) then
    begin
        fMotors.MoveR(aRotation + xMTD.RStart, m_EXECUTE);
    end;

    if (not gErrorManager.IsGlobalErr) and (aUseStartPos) and (xMTD.HZStart <> xMTD.hZDest) then
    begin
        fZTravelMotion.MoveToAtleastZTravelAllTips(xMTD.HZStart);
    end;
end;

procedure TTubeMoveMotorMotionSystem.MoveTubeToPos2(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aUseStartPos, aUseSafeMove, aIsGetTube: boolean; aToolData: TTubeToolData;
    aZOffset: TPosMM; aZSpeed, aZRamp: integer);
var
    xMTD: TMoveTubeData;
begin
    // --------------------------------------- berechnen der Tube X+Y Position
    xMTD := self.GetMoveTubeData(aRackPos, aUsePutOffset, aToolData, aZOffset);
    // --------------------------------------------------------- Fahre den Handler zur Greifposition
    if (not gErrorManager.IsGlobalErr) then
    begin
        if (not aUseStartPos) then
        begin
            fMotors.MoveX(xMTD.hXDest, m_NO_EXEC);
            fMotors.MoveY(xMTD.hYDest, m_EXECUTE);
        end
        else
        begin
            if (xMTD.hYStartBeforeX) then
            begin
                // vom Startpunkt aus zuerst Y, dann X fahren
                if (xMTD.hYDest <> xMTD.YStart) then
                    fMotors.MoveY(xMTD.hYDest, m_EXECUTE);
                if (xMTD.hXDest <> xMTD.XStart) then
                    fMotors.MoveX(xMTD.hXDest, m_EXECUTE);
            end
            else
            begin
                // vom Startpunkt aus zuerst X, dann Y fahren
                if (xMTD.hXDest <> xMTD.XStart) then
                    fMotors.MoveX(xMTD.hXDest, m_EXECUTE);
                if (xMTD.hYDest <> xMTD.YStart) then
                    fMotors.MoveY(xMTD.hYDest, m_EXECUTE);
            end;
        end;
    end;
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    if (aUseSafeMove) then
    begin
        DoSafeMove(xMTD.hZDest, aUsePutOffset, aRotation, aUseStartPos, aUseSafeMove, aIsGetTube, aToolData,
            aZOffset, aZSpeed, aZRamp);
    end
    else
    begin
        fMotors.MoveZ(xMTD.hZDest, m_EXECUTE, AT_MOVE_ABS, aZSpeed, aZRamp);
    end;
end;

procedure TTubeMoveMotorMotionSystem.DoSafeMove(aZDest: TPosMM; aUsePutOffset: boolean; aRotation: TPosMM;
    aUseStartPos, aUseSafeMove, aIsGetTube: boolean; aToolData: TTubeToolData; aZOffset: TPosMM;
    aZSpeed, aZRamp: integer);
var
    xZDestSafeMove, xRadius: TPosMM;
    xShiftXOffset, xShiftYOffset: TPosMM;
    xLastShiftXOffset, xLastShiftYOffset: TPosMM;
    xStep, xTry: integer;
    xButton: integer;
begin
    // da der Offset zwischenzeitig negativ eingetragen werden musste, wird ABS() verwendet
    xZDestSafeMove := aZDest + Abs(aToolData.SafeMoveOffset);
    fMotors.MoveZ(xZDestSafeMove, m_EXECUTE);

    xButton := mrRetry;
    while (aIsGetTube) and (aToolData.GetTubeXYShiftStepCount > 0) and (xButton = mrRetry) do
    begin
        fMotors.MoveZ(aZDest, m_NO_EXEC, AT_MOVE_ABS, aToolData.SafeMoveSpeed, 0);
        if not fMotors.ZMotor.ExecuteDisabledErrorAndCheck() then
            EXIT;
        xLastShiftXOffset := 0;
        xLastShiftYOffset := 0;

        // nochmal probieren mit geänderter XY-Position
        for xTry := 1 to 2 do
        begin
            if (xTry = 1) then
                xRadius := aToolData.GetTubeXYShiftRadius1
            else
                xRadius := aToolData.GetTubeXYShiftRadius2;

            if (xRadius > 0) then
            begin
                xShiftXOffset := 0;
                xShiftYOffset := 0;
                for xStep := 1 to aToolData.GetTubeXYShiftStepCount do
                begin
                    xLastShiftXOffset := xShiftXOffset;
                    xLastShiftYOffset := xShiftYOffset;
                    TXYAxisMotorMotionSystem.GetTubeXYOffset(xShiftXOffset, xShiftYOffset, xStep,
                        aToolData.GetTubeXYShiftStepCount, xRadius);
                    TLogManager.Instance.Log('Retry Get Tube: Shift Offset X ' + FloatToStr(xShiftXOffset) +
                        ' mm, Y ' + FloatToStr(xShiftYOffset) + ' mm', false);

                    fMotors.MoveZ(xZDestSafeMove, m_NO_EXEC);
                    fMotors.ZMotor.ExecuteDisabledErrorAndCheck();
                    fMotors.MoveX(xShiftXOffset - xLastShiftXOffset, m_NO_EXEC, AT_MOVE_REL, 0, 0);
                    fMotors.MoveY(xShiftYOffset - xLastShiftYOffset, m_EXECUTE, AT_MOVE_REL, 0, 0);

                    fMotors.MoveZ(aZDest, m_NO_EXEC, AT_MOVE_ABS, aToolData.SafeMoveSpeed, 0);
                    if not fMotors.ZMotor.ExecuteDisabledErrorAndCheck() then
                        EXIT;
                end;
            end;
        end;

        // ein letztes Mal zurück auf die Ursprungsposition
        fMotors.MoveZ(xZDestSafeMove, m_NO_EXEC);
        fMotors.ZMotor.ExecuteDisabledErrorAndCheck();
        fMotors.MoveX(-xLastShiftXOffset, m_NO_EXEC, AT_MOVE_REL, 0, 0);
        fMotors.MoveY(-xLastShiftYOffset, m_EXECUTE, AT_MOVE_REL, 0, 0);

        xButton := gErrorMessageFactory.ErrBoxSimple
            ('Get Tube: Wrong Tube position - can not get tube with gripper!', 'Get Tube', eibAbortRetry);
    end;

    fMotors.MoveZ(aZDest, m_EXECUTE, AT_MOVE_ABS, aToolData.SafeMoveSpeed, 0);
end;

procedure TTubeMoveMotorMotionSystem.MoveTubeFromPos1(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM);
var
    xMTD: TMoveTubeData;
begin
    // --------------------------------------- berechnen der Tube X+Y Position
    xMTD := self.GetMoveTubeData(aRackPos, aUsePutOffset, aToolData, aZOffset);
    // ------------------------------ Gehe zur Startposition (benutzt XStart-,YStart-,HStart-Offset)
    if (not gErrorManager.IsGlobalErr) and (aUseStartPos) and (xMTD.HZStart <> xMTD.hZDest) then
    begin
        fZTravelMotion.MoveToAtleastZTravelAllTips(xMTD.HZStart);
    end;
    if (not gErrorManager.IsGlobalErr) and (aUseStartPos) then
    begin
        if (xMTD.hYStartBeforeX) then
        begin
            // zum Startpunkt zuerst X, dann Y fahren
            if (xMTD.hXDest <> xMTD.XStart) then
                fMotors.MoveX(xMTD.XStart, m_EXECUTE);
            if (xMTD.hYDest <> xMTD.YStart) then
                fMotors.MoveY(xMTD.YStart, m_EXECUTE);
        end
        else
        begin
            // zum Startpunkt zuerst Y, dann X fahren
            if (xMTD.hYDest <> xMTD.YStart) then
                fMotors.MoveY(xMTD.YStart, m_EXECUTE);
            if (xMTD.hXDest <> xMTD.XStart) then
                fMotors.MoveX(xMTD.XStart, m_EXECUTE);
        end;
    end;
end;

procedure TTubeMoveMotorMotionSystem.MoveTubeFromPos2(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM;
    aZSpeed, aZRamp: integer);
var
    xMTD: TMoveTubeData;
begin
    // --------------------------------------- berechnen der Tube X+Y Position
    xMTD := self.GetMoveTubeData(aRackPos, aUsePutOffset, aToolData, aZOffset);
    // ------------------------------------------------------------------------------ move to z-travel
    if (not gErrorManager.IsGlobalErr) then
    begin
        fZTravelMotion.MoveToZTravelAllTips(aZSpeed, aZRamp);
    end;
    // ---------------------------------------------------------------------------------- rotation = 0
    if (not gErrorManager.IsGlobalErr) and (aRotation + xMTD.RStart > 0) and (not xMTD.KeepRotation) then
    begin
        fMotors.MoveR(0, m_EXECUTE);
    end;
end;

procedure TTubeMoveMotorMotionSystem.MoveYShakeOnce(aDest: TPosMM);
var
    xSpeed: integer;
begin
    xSpeed := 0;
    if Assigned(fMotors.YMotor) then
        xSpeed := fMotors.YMotor.DefaultSpeed + 1000;
    fMotors.MoveY(aDest, m_EXECUTE, AT_MOVE_REL, xSpeed, 0);
end;

procedure TTubeMoveMotorMotionSystem.MoveZShakeOnce(aDest: TPosMM; aSlow: boolean);
var
    xSpeed: integer;
begin
    if aSlow then
    begin
        xSpeed := 0;
        if Assigned(fMotors.ZMotor) then
            xSpeed := fMotors.ZMotor.DefaultSpeed div 2;
        fMotors.MoveZ(aDest, m_EXECUTE, AT_MOVE_REL, xSpeed, 0)
    end
    else
        fMotors.MoveZ(aDest, m_EXECUTE, AT_MOVE_REL, 0, 0);
end;

function TTubeMoveMotorMotionSystem.GetMoveTubeData(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aToolData: TTubeToolData; aZOffset: TPosMM): TMoveTubeData;
var
    xTubeZPosition: TGeom3D_Position;
    xMoveStruct: TTubeMoveStruct;
    xXPosTube, xYPosTube, xHZDest_mm: TPosMM;
    xToolInHandler: boolean;
    xCarrier: TCarrier;
    xCarrierPos: TPoint4d;
begin
    xToolInHandler := true; { TODO : toolInhandler? }

    if (gErrorManager.IsGlobalErr) then
        exit;
    if (aRackPos.Rack = nil) then
        exit;
    xCarrier := TLayout.GetCarrierOfRack(aRackPos.Rack);
    if not Assigned(xCarrier) then
        exit;

    TMotorStepCalculator.GetTubeDataXY(aRackPos.Rack, aRackPos.Pos, xXPosTube, xYPosTube);
    result.hXDest := xXPosTube;
    result.hYDest := xYPosTube;

    result.hYDest := fMotors.YMotor.GetTip1YPos(result.hYDest, xToolInHandler);
    // -------------------------------------------  Koordinaten des Tubes bestimmen (relativ zu Tip 1),
    aRackPos.Rack.PosShifted := false;
    result.hXDest := result.hXDest + aToolData.XOffset;
    result.hYDest := fMotors.YMotor.TH_GetRealYPosition(result.hYDest, aToolData.YOffset);
    aRackPos.Rack.PosShifted := true;

    xTubeZPosition := aRackPos.Rack.CreateTubePos(aRackPos.Pos);
    xHZDest_mm := AddZHeight(xTubeZPosition.Z, -aToolData.ZOffset);
    xTubeZPosition.Free;

    xMoveStruct := xCarrier.GetTubeMoveStruct();
    result.hYStartBeforeX := xMoveStruct.HT_YStartBeforeX;
    result.KeepRotation := xMoveStruct.HT_KeepRotation;

    xMoveStruct.HT_ZStart_mm := AddZHeight(xHZDest_mm, -xMoveStruct.HT_ZStart_mm);
    if (aUsePutOffset) then
    begin
        xHZDest_mm := AddZHeight(xHZDest_mm, -aToolData.ZDrop);
        xHZDest_mm := AddZHeight(xHZDest_mm, -aRackPos.Rack.Structure.TubePutOffset_mm);
    end;
    xHZDest_mm := AddZHeight(xHZDest_mm, -aRackPos.Rack.Structure.TubeGripOffset_mm);
    xHZDest_mm := AddZHeight(xHZDest_mm, -aZOffset);

    result.hZDest := xHZDest_mm;

    { TODO : Comparison of zstart and HZDest }
    // if (xMoveStruct.HT_ZStart_mm > xHZDest_mm) then
    // result.HZStart := result.HZDest
    // else
    result.HZStart := xMoveStruct.HT_ZStart_mm;

    xCarrierPos := xCarrier.CalcCarrierPosition();

    if (xMoveStruct.HT_XStart_mm <> 0) then
    begin
        result.XStart := xMoveStruct.HT_XStart_mm;
        if (gUseCorrectCarrierOffset) then
            result.XStart := result.XStart + xCarrierPos.X + aToolData.XOffset
        else
            result.XStart := result.XStart + result.hXDest;
    end
    else
        result.XStart := result.hXDest;

    if (xMoveStruct.HT_YStart_mm <> 0) then
    begin
        result.YStart := xMoveStruct.HT_YStart_mm;
        if (gUseCorrectCarrierOffset) then
            result.YStart := result.YStart + fMotors.YMotor.TH_GetRealYPosition(xCarrierPos.Y,
                aToolData.YOffset)
        else
            result.YStart := result.YStart + result.hYDest;
    end
    else
        result.YStart := result.hYDest;

    result.RStart := xMoveStruct.HT_RStart_degree;

    gLogManager.Log(Format('Move Tube Data: Start Offset Position(%f,%f,%f,%f), Target Position(%f,%f,%f)',
        [result.XStart, result.YStart, result.HZStart, result.RStart, result.hXDest, result.hYDest,
        result.hZDest]), false);
end;

{ TTubeMoveLocationMotionSystem }

constructor TTubeMoveLocationMotionSystem.Create(aMotionDevices: ILocationBasedMotionDevice);
begin
    inherited Create();
    fMotionDevices := aMotionDevices;
end;

procedure TTubeMoveLocationMotionSystem.MoveToTubeXY(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM);
begin
    // NOT YET IMPLEMENTED
end;

procedure TTubeMoveLocationMotionSystem.MoveTubeToPos1(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM);
begin
    if not Assigned(aRackPos.Rack) then
        exit;

    // pfad von Ready-Position zu Rackapproach-Position  (entspricht Startposition)
    fMotionDevices.DeviceApproachPath(aRackPos.Rack.Name, pposAll, false);
end;

procedure TTubeMoveLocationMotionSystem.MoveTubeToPos2(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aUseStartPos, aUseSafeMove, aIsGetTube: boolean; aToolData: TTubeToolData;
    aZOffset: TPosMM; aZSpeed, aZRamp: integer);
var
    xZUp: double;
    xXForward: single;
    xCarrier: TCarrier;
begin
    if not Assigned(aRackPos.Rack) then
        exit;
    xCarrier := TLayout.GetCarrierOfRack(aRackPos.Rack);
    if not Assigned(xCarrier) then
        exit;

    // bewege zu Position "Pos" im rack
    fMotionDevices.DeviceMoveToTubePos(aRackPos.Rack.Name, aRackPos.Pos);

    // if XStart <> 0 then move forward in X direction
    xXForward := xCarrier.GetTubeMoveStruct().HT_XStart_mm;

    // move down in Z
    xZUp := Abs(aRackPos.Rack.Structure.zTravel_mm - aRackPos.Rack.Structure.ZDisp_mm);
    if (aUsePutOffset) then
    begin
        xZUp := xZUp + aToolData.ZDrop;
        xZUp := xZUp + aRackPos.Rack.Structure.TubePutOffset_mm;
    end;
    xZUp := xZUp + aRackPos.Rack.Structure.TubeGripOffset_mm;

    // if xXForward <> 0 then
    // fMotionDevices.DeviceMoveX(  );
    fMotionDevices.MoveToGripPosition(aRackPos.Rack.Name, aRackPos.Pos, xXForward, xZUp, true);
end;

procedure TTubeMoveLocationMotionSystem.MoveTubeFromPos1(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM);
var
    xZUp: double;
    xXForward: single;
    xCarrier: TCarrier;
begin
    if not Assigned(aRackPos.Rack) then
        exit;
    xCarrier := TLayout.GetCarrierOfRack(aRackPos.Rack);
    if not Assigned(xCarrier) then
        exit;

    // move up in Z (ZDown)
    xZUp := Abs(aRackPos.Rack.Structure.zTravel_mm - aRackPos.Rack.Structure.ZDisp_mm);
    if (aUsePutOffset) then
    begin
        xZUp := xZUp + aToolData.ZDrop;
        xZUp := xZUp + aRackPos.Rack.Structure.TubePutOffset_mm;
    end;
    xZUp := xZUp + aRackPos.Rack.Structure.TubeGripOffset_mm;

    // if XStart <> 0 then move backward in X direction
    xXForward := xCarrier.GetTubeMoveStruct().HT_XStart_mm;
    // if xXForward <> 0 then
    // fMotionDevices.DeviceMoveX(  );

    fMotionDevices.MoveFromGripPosition(aRackPos.Rack.Name, aRackPos.Pos, xXForward, xZUp, false);

    // zur letzten pfad von Ready-Position zu Rackapproach-Position  (entspricht Startposition)
    fMotionDevices.DeviceApproachPath(aRackPos.Rack.Name, pposLastPosOnly, true);
end;

procedure TTubeMoveLocationMotionSystem.MoveTubeFromPos2(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aUseStartPos: boolean; aToolData: TTubeToolData; aZOffset: TPosMM;
    aZSpeed, aZRamp: integer);
begin
    if not Assigned(aRackPos.Rack) then
        exit;

    // zur letzten pfad von Ready-Position zu Rackapproach-Position  (entspricht Startposition)
    fMotionDevices.DeviceApproachPath(aRackPos.Rack.Name, pposAllWithoutLastPos, true);

end;

procedure TTubeMoveLocationMotionSystem.MoveYShakeOnce(aDest: TPosMM);
begin
end;

procedure TTubeMoveLocationMotionSystem.MoveZShakeOnce(aDest: TPosMM; aSlow: boolean);
begin
end;


end.
