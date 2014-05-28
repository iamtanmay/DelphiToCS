{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : motion system to move racks
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                               track-no improvement/change
  -------- --  ---------------------------------------------------- -------- ------------------------------------
  07.03.07 wl                                                       TN3620   MotorSystem classes moved from OperationRack to here
  12.03.07 pk                                                       TN3629   new RackMoveOptions
  22.06.07 pk  TRackMoveMotorMotionSystem.MovePlateToPos1           TN3731   Now also move combined arms to ztravel instead of just moving self to ztravel
  12.07.07 wl  TRackMoveMotorMotionSystem.GetSlotGroupData_Steps    TN3782   nur wenn fHandlerXYIndependetFromRotation = true, hat HRTake-Wert keinen Einfluß mehr auf X- und Y-Take
  12.07.07 wl  TRackMoveMotorMotionSystem.GetSlotGroupData_Steps    TN3782   fChangeXYAtRotation = false abgeschafft
  09.11.07 pk                                                       TN3924   Steps changed to mm
  07.01.08 pk                                                       TN3971   ZHeights are added via AddZHeight function
  29.01.08 wl                                                       TN3980   uses geändert
  20.06.08 pk                                                       TN4139    WB global object replaced by LayoutManager
  20.06.08 pk                                                       TN4139   Rack no longer has typed link to Carrier use TLayout.GetCarrierOfRack
  02.07.08 pk  GetSlotGroupData_Steps                               TN4139   renamed to GetSlotGroupData. Also, CreateRackPosition: paramter removed
  07.07.08 pk  GetSlotGroupData                                     TN4139   various changes
  10.12.08 wl  TRackMoveLocationMotionSystem.MovePlateFromPos       TN4353   statt einzelner X- und Z-Moves wird MoveFromGripPosition aufgerufen
  10.12.08 wl  TRackMoveLocationMotionSystem.Intern_MovePlateToPos2 TN4353   statt einzelner X- und Z-Moves wird MoveToGripPosition aufgerufen
  12.09.09 wl                                                       TN4740   default-Werte aus interface entfernt
  27.08.10 wl  TRackMoveMotorMotionSystem.HandlerXYIndependetFromRotation  TN5250   entfernt
  23.03.11 wl                                                       TN5515   MoveToZTravel: Methodenname geändert
  29.03.11 wl  TRackMoveMotorMotionSystem.MoveHXY                   TN5524   von MotionSystemTravel hierher
  29.03.11 wl                                                       TN5524   OperationPip -> MotorStepCalculator
  12.10.11 ts  MoveHXY                                              TN5711   if aAddTipOffset is true, Offsets from TipType will be added
  15.11.11 wl                                                            TN5736   verwendet TXYZRMotorMotionSystem
  15.11.11 wl  TRackMoveMotorMotionSystem.MovePlateToPos2Put_AfterRetake TN5736   XY-Bewgungen werden nur noch bei Start-Offset oder Retake ausgeführt
  28.08.12 ts                                                            TN5943   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Tube
  07.09.12 ts                                                            TN5973   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Rack
  01.07.13 wl  MovePlateToPos1, MovePlateToPos2Put_AfterRetake           TN6192   neue Parameter aXSpeed, aXRamp, aYSpeed, aYRamp
  -------------------------------------------------------------------------------------------------- }

unit MotionSystemRack;


interface


uses
    AppTypes,
    CommonTypes,
    Rack,
    MotionSystemTravel,
    IntfMotionDevice,
    IntfMotorBasedMotionDevice,
    MotorStepCalculator;

type
    SLOTGROUPDATA = record
        // --------------------------------- Beschreibung des Greifers: (mit Referenzpunktkorrektur Slot)
        PlateHXTake: TPosMM; // Greiferoffset: X		    (absolut aus Racktyp und aktuellem Slot)
        PlateHYTake: TPosMM; // Greiferoffset: Y		    (absolut aus Racktyp und aktuellem Slot)
        PlateHZTake: TPosMM; // Greiferoffset: Z        	(absolut aus Racktyp und aktuellem Slot)
        PlateHRTake: TPosMM; // GreiferRotation
        // Umgreifoffsets: Retake offset for all slots of this slot group
        hXReTakeOfs, // =  XReTakeOfs
        hYReTakeOfs, // =  YReTakeOfs
        hRReTakeOfs, hZReTakeOfs: TPosMM;
        // How to move away
        hXPreStartOfs, // =  XStartOfs
        hXStartOfs, // =  XStartOfs
        hYStartOfs, // =  XStartOfs
        hZStartOfs, // =  XStartOfs
        hRStartOfs: TPosMM; // =  0 für ZP01
        HZPutOfs: TPosMM; // Put-Offset
    end;

    TRackMoveMotionSystem = class
    public
        // Rack-Handling
        procedure MovePlateToPos1(aXSpeed, aXRamp, aYSpeed, aYRamp: integer); virtual; abstract;
        procedure MovePlateToPos2Get(aMoveOptions: TRackMoveOptions); virtual; abstract;
        function RetakeNeeded(): boolean; virtual; abstract;
        procedure MovePlateToPos2Put_BeforeRetake(aMoveOptions: TRackMoveOptions; aZSpeed, aZRamp: integer);
            virtual; abstract;
        procedure MovePlateToPos2Put_ToRetake(); virtual; abstract;
        procedure MovePlateToPos2Put_FromRetake1(); virtual; abstract;
        procedure MovePlateToPos2Put_FromRetake2(); virtual; abstract;
        procedure MovePlateToPos2Put_AfterRetake(aXSpeed, aXRamp, aYSpeed, aYRamp, aZSpeed, aZRamp: integer);
            virtual; abstract;

        procedure MovePlateFromPos(aMovePlateType: TMovePlateType; aMoveOptions: TRackMoveOptions;
            aZSpeed: integer = 0; aZRamp: integer = 0); virtual; abstract;
        procedure MovePlateToPos2Get_ToRetake(); virtual; abstract;
        procedure MovePlateToPos2Get_FromRetake(); virtual; abstract;
    end;

    TRackMoveMotorMotionSystem = class(TRackMoveMotionSystem)
    private
        fMotors: TXYZRMotorMotionSystem;
        fXYMotion: TXYAxisMotorMotionSystem;
        fZTravelMotion: TZAxisTravelMotorMotionSystem;
        fRack: TRack;
        fIsSGDInitialized: boolean;
        fSGD: SLOTGROUPDATA;
        fPipStepCalculator: TMotorXYStepCalculator;
        function GetSlotGroupData(aRack: TRack): SLOTGROUPDATA;
        procedure InitSGD();
        procedure MoveHXY(aHX, aHY: TPosMM; aOptions: TMoveXYMovementOptions; aAddTipOffset: boolean;
            aXSpeed, aXRamp, aYSpeed, aYRamp: integer);
        procedure MoveZRelativeToCarrier(aHeightRelToCarrier: TPosMM; aZSpeed: integer = 0;
            aZRamp: integer = 0);
    public
        constructor Create(aRack: TRack; aMotors: TXYZRMotorMotionSystem; aXYMotion: TXYAxisMotorMotionSystem;
            aZTravelMotion: TZAxisTravelMotorMotionSystem; aPipStepCalculator: TMotorXYStepCalculator);
        destructor Destroy; override;
        procedure MovePlateFromPos(aMovePlateType: TMovePlateType; aMoveOptions: TRackMoveOptions;
            aZSpeed: integer = 0; aZRamp: integer = 0); override;

        procedure MovePlateToPos1(aXSpeed, aXRamp, aYSpeed, aYRamp: integer); override;

        function RetakeNeeded(): boolean; override;
        procedure MovePlateToPos2Put_BeforeRetake(aMoveOptions: TRackMoveOptions;
            aZSpeed, aZRamp: integer); override;
        procedure MovePlateToPos2Put_ToRetake(); override;
        procedure MovePlateToPos2Put_FromRetake1(); override;
        procedure MovePlateToPos2Put_FromRetake2(); override;
        procedure MovePlateToPos2Put_AfterRetake(aXSpeed, aXRamp, aYSpeed, aYRamp, aZSpeed,
            aZRamp: integer); override;

        procedure MovePlateToPos2Get(aMoveOptions: TRackMoveOptions); override;
        procedure MovePlateToPos2Get_ToRetake(); override;
        procedure MovePlateToPos2Get_FromRetake(); override;
    end;

    TRackMoveLocationMotionSystem = class(TRackMoveMotionSystem)
    private
        fRack: TRack;
        fMotionDevices: ILocationBasedMotionDevice;
        procedure Intern_MovePlateToPos2(aMovePlateType: TMovePlateType; aMoveOptions: TRackMoveOptions);
    public
        constructor Create(aRack: TRack; aMotionDevices: ILocationBasedMotionDevice);
        procedure MovePlateFromPos(aMovePlateType: TMovePlateType; aMoveOptions: TRackMoveOptions;
            aZSpeed: integer = 0; aZRamp: integer = 0); override;

        procedure MovePlateToPos1(aXSpeed, aXRamp, aYSpeed, aYRamp: integer); override;

        function RetakeNeeded(): boolean; override;
        procedure MovePlateToPos2Put_BeforeRetake(aMoveOptions: TRackMoveOptions;
            aZSpeed, aZRamp: integer); override;
        procedure MovePlateToPos2Put_ToRetake(); override;
        procedure MovePlateToPos2Put_FromRetake1(); override;
        procedure MovePlateToPos2Put_FromRetake2(); override;
        procedure MovePlateToPos2Put_AfterRetake(aXSpeed, aXRamp, aYSpeed, aYRamp, aZSpeed,
            aZRamp: integer); override;

        procedure MovePlateToPos2Get(aMoveOptions: TRackMoveOptions); override;
        procedure MovePlateToPos2Get_ToRetake(); override;
        procedure MovePlateToPos2Get_FromRetake(); override;
    end;


implementation


uses
    SysUtils,
    ErrorManager,
    IntfMotorDriver,
    IntfLocationBasedAllAxesDriver,
    AppSettings,
    GeometricClasses,
    IntfMotorDevice,
    Carrier,
    CarrierSlot,
    Layout;

{ TRackMoveMotorMotionSystem }

constructor TRackMoveMotorMotionSystem.Create(aRack: TRack; aMotors: TXYZRMotorMotionSystem;
    aXYMotion: TXYAxisMotorMotionSystem; aZTravelMotion: TZAxisTravelMotorMotionSystem;
    aPipStepCalculator: TMotorXYStepCalculator);
begin
    inherited Create();
    fMotors := aMotors;
    fXYMotion := aXYMotion;
    fZTravelMotion := aZTravelMotion;
    fRack := aRack;
    fIsSGDInitialized := false;
    fPipStepCalculator := aPipStepCalculator;
end;

procedure TRackMoveMotorMotionSystem.InitSGD();
begin
    if fIsSGDInitialized then
        EXIT;
    fSGD := GetSlotGroupData(fRack); // Get Slot Group data
    fIsSGDInitialized := true;
end;

procedure TRackMoveMotorMotionSystem.MoveHXY(aHX, aHY: TPosMM; aOptions: TMoveXYMovementOptions;
    aAddTipOffset: boolean; aXSpeed, aXRamp, aYSpeed, aYRamp: integer);
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

        fXYMotion.MoveXY(xXYStep, 0, 0, aOptions, false, aXSpeed, aXRamp, aYSpeed, aYRamp);
    finally
        FreeAndNil(xXYStep);
    end;
end;

procedure TRackMoveMotorMotionSystem.MoveZRelativeToCarrier(aHeightRelToCarrier: TPosMM;
    aZSpeed, aZRamp: integer);
var
    xDest: TPosMM;
begin
    xDest := AddZHeight(fSGD.PlateHZTake, -aHeightRelToCarrier);
    fMotors.MoveZ(xDest, m_EXECUTE, AT_MOVE_ABS, aZSpeed, aZRamp);
end;

procedure TRackMoveMotorMotionSystem.MovePlateToPos1(aXSpeed, aXRamp, aYSpeed, aYRamp: integer);
var
    xIniAccess: IWinlissyIniAccess;
    xMoveXYBeforeRotation: boolean;
begin
    xIniAccess := gCommonDll.CreateRobotIni;
    xMoveXYBeforeRotation := xIniAccess.ReadBool('MoveRack', 'MoveXYBeforeRotation');

    InitSGD();

    // Kombinierten Arm hochfahren (Funktion wird von MOVER, READB, TOOLG und TOOLR aufgerufen)
    fZTravelMotion.CombinedArmsMoveToZTravel();

    // auf Z-travel fahren
    fZTravelMotion.MoveToZTravelAllTips(0, 0);

    if (not xMoveXYBeforeRotation) then // Rotate before moving to (pre-)start position
        fMotors.MoveR(fSGD.PlateHRTake + fSGD.hRStartOfs, m_EXECUTE);

    // Move to pre-start position
    MoveHXY(fSGD.PlateHXTake + fSGD.hXStartOfs + fSGD.hXPreStartOfs, fSGD.PlateHYTake + fSGD.hYStartOfs, [],
        false, aXSpeed, aXRamp, aYSpeed, aYRamp);

    if (xMoveXYBeforeRotation) then // Rotate at (pre-)start position
        fMotors.MoveR(fSGD.PlateHRTake + fSGD.hRStartOfs, m_EXECUTE);

    // Move to start position (X, Y)
    if (fSGD.hXPreStartOfs <> 0) then
        fMotors.MoveX(fSGD.PlateHXTake + fSGD.hXStartOfs, m_NO_EXEC, AT_MOVE_ABS, aXSpeed, aXRamp);
end;

procedure TRackMoveMotorMotionSystem.MovePlateToPos2Get(aMoveOptions: TRackMoveOptions);
begin
    InitSGD();
    // Move gripper down to start offset
    MoveZRelativeToCarrier(fSGD.hZStartOfs);

    // Movement from start position to plate position (R - Y - X - Z)
    if (fSGD.hRStartOfs <> 0) then
        fMotors.MoveR(fSGD.PlateHRTake, m_EXECUTE);
    if (fSGD.hYStartOfs <> 0) then
        fMotors.MoveY(fSGD.PlateHYTake, m_EXECUTE);
    if (fSGD.hXStartOfs <> 0) then
        fMotors.MoveX(fSGD.PlateHXTake, m_EXECUTE);
    if (fSGD.hZStartOfs <> 0) then
        fMotors.MoveZ(fSGD.PlateHZTake, m_EXECUTE);
end;

function TRackMoveMotorMotionSystem.RetakeNeeded(): boolean;
begin
    InitSGD();
    result := (fSGD.hXReTakeOfs <> 0) or (fSGD.hYReTakeOfs <> 0) or (fSGD.hRReTakeOfs <> 0);
end;

procedure TRackMoveMotorMotionSystem.MovePlateToPos2Put_ToRetake();
begin
    InitSGD();
    // Put Plate at retake position (TakeOfs-ReTakeOfs)
    fMotors.MoveY(fSGD.PlateHYTake, m_EXECUTE);
    fMotors.MoveX(fSGD.PlateHXTake, m_EXECUTE);
    MoveZRelativeToCarrier(fSGD.hZReTakeOfs + fSGD.HZPutOfs);
end;

procedure TRackMoveMotorMotionSystem.MovePlateToPos2Put_FromRetake1();
begin
    InitSGD();
    // get Plate from retake position again
    MoveZRelativeToCarrier(fSGD.hZStartOfs + fSGD.HZPutOfs);
    fMotors.MoveY(fSGD.PlateHYTake + fSGD.hYReTakeOfs, m_EXECUTE);
    fMotors.MoveX(fSGD.PlateHXTake + fSGD.hXReTakeOfs, m_EXECUTE);
    MoveZRelativeToCarrier(fSGD.hZReTakeOfs + fSGD.HZPutOfs);
end;

procedure TRackMoveMotorMotionSystem.MovePlateToPos2Put_FromRetake2();
begin
    InitSGD();
    MoveZRelativeToCarrier(fSGD.hZStartOfs + fSGD.HZPutOfs);
end;

procedure TRackMoveMotorMotionSystem.MovePlateToPos2Put_BeforeRetake(aMoveOptions: TRackMoveOptions;
    aZSpeed, aZRamp: integer);
begin
    InitSGD();
    // Move gripper down to start offset
    MoveZRelativeToCarrier(fSGD.hZStartOfs + fSGD.HZPutOfs, aZSpeed, aZRamp);

    // Movement to plate position (Rotation)
    fMotors.MoveR(fSGD.PlateHRTake, m_EXECUTE);

end;

procedure TRackMoveMotorMotionSystem.MovePlateToPos2Put_AfterRetake(aXSpeed, aXRamp, aYSpeed, aYRamp, aZSpeed,
    aZRamp: integer);
begin
    InitSGD();
    // Movement to plate position (Y - X - Z)
    if (fSGD.hYReTakeOfs <> 0) or (fSGD.hYStartOfs <> 0) then
        fMotors.MoveY(fSGD.PlateHYTake, m_EXECUTE, AT_MOVE_ABS, aYSpeed, aYRamp);
    if (fSGD.hXReTakeOfs <> 0) or (fSGD.hXStartOfs <> 0) then
        fMotors.MoveX(fSGD.PlateHXTake, m_EXECUTE, AT_MOVE_ABS, aXSpeed, aXRamp);
    MoveZRelativeToCarrier(fSGD.HZPutOfs, aZSpeed, aZRamp);
end;

procedure TRackMoveMotorMotionSystem.MovePlateToPos2Get_ToRetake();
begin
    InitSGD();
    // Put Plate at retake position (TakeOfs-ReTakeOfs)
    MoveZRelativeToCarrier(fSGD.hZStartOfs);
    fMotors.MoveY(fSGD.PlateHYTake + fSGD.hYReTakeOfs, m_EXECUTE);
    fMotors.MoveX(fSGD.PlateHXTake + fSGD.hXReTakeOfs, m_EXECUTE);
    MoveZRelativeToCarrier(fSGD.hZReTakeOfs);
end;

procedure TRackMoveMotorMotionSystem.MovePlateToPos2Get_FromRetake();
begin
    InitSGD();
    // get Plate from retake position again
    MoveZRelativeToCarrier(fSGD.hZStartOfs);
    fMotors.MoveY(fSGD.PlateHYTake, m_EXECUTE);
    fMotors.MoveX(fSGD.PlateHXTake, m_EXECUTE);
    MoveZRelativeToCarrier(fSGD.hZReTakeOfs);
end;

procedure TRackMoveMotorMotionSystem.MovePlateFromPos(aMovePlateType: TMovePlateType;
    aMoveOptions: TRackMoveOptions; aZSpeed, aZRamp: integer);
begin
    InitSGD();
    if (aMovePlateType = mpGet) then
    begin // Move to Z-Start (if <> 0)
        if (fSGD.hZStartOfs <> 0) then
            MoveZRelativeToCarrier(fSGD.hZStartOfs, aZSpeed, aZRamp);
    end;

    if (aMovePlateType = mpPut) then
    begin // Move to Z-Start (with Z-Put-Offset)
        MoveZRelativeToCarrier(fSGD.hZStartOfs + fSGD.HZPutOfs, aZSpeed, aZRamp);
    end;

    // Move Back to Startup position
    if (fSGD.hXStartOfs <> 0) then
        fMotors.MoveX(fSGD.PlateHXTake + fSGD.hXStartOfs, m_EXECUTE);
    if (fSGD.hYStartOfs <> 0) then
        fMotors.MoveY(fSGD.PlateHYTake + fSGD.hYStartOfs, m_EXECUTE);
    if (fSGD.hRStartOfs <> 0) then
        fMotors.MoveR(fSGD.PlateHRTake + fSGD.hRStartOfs, m_EXECUTE);

    // Move to pre-start position
    if (fSGD.hXPreStartOfs <> 0) then
        fMotors.MoveX(fSGD.PlateHXTake + fSGD.hXStartOfs + fSGD.hXPreStartOfs, m_EXECUTE);
end;

destructor TRackMoveMotorMotionSystem.Destroy;
begin
    FreeAndNil(fMotors);
    inherited;
end;

function TRackMoveMotorMotionSystem.GetSlotGroupData(aRack: TRack): SLOTGROUPDATA;
var
    xRackPosition: TGeom3D_Position;
    xMoveStruct: TRackMoveStruct;
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(aRack);
    if not Assigned(xCarrier) then
        EXIT;

    xRackPosition := aRack.CreateRackPosition(aRack.Structure.H_XTake_mm, aRack.Structure.H_YTake_mm,
        -aRack.Structure.H_ZTake_mm);
    try
        result.PlateHXTake := xRackPosition.X; // xRackPosition.X + xHXTake_mm;
        result.PlateHYTake := xRackPosition.Y; // xRackPosition.Y + xHYTake_mm;
        result.PlateHZTake := xRackPosition.Z; // AddZHeight( xRackPosition.Z, -aRack.Structure.H_ZTake_mm );
    finally
        FreeAndNil(xRackPosition);
    end;
    if Assigned(fMotors.RMotor) then
        result.PlateHRTake := aRack.HRTake_degree { TODO : GetROffset }{ + FIntf.HArm_GetROffset }
    else
        result.PlateHRTake := 0;

    xMoveStruct := xCarrier.GetRackMoveStruct();

    // Umgreifoffsets
    result.hXReTakeOfs := xMoveStruct.HR_XReTake_mm;
    result.hYReTakeOfs := xMoveStruct.HR_YReTake_mm;
    result.hZReTakeOfs := xMoveStruct.HR_ZReTake_mm;
    if Assigned(fMotors.RMotor) then
        result.hRReTakeOfs := xMoveStruct.HR_RReTake_degree
    else
        result.hRReTakeOfs := 0;
    result.PlateHXTake := result.PlateHXTake + result.hXReTakeOfs;
    result.PlateHYTake := result.PlateHYTake + result.hYReTakeOfs;

    // How to move away
    result.hXPreStartOfs := xMoveStruct.HR_XPreStart_mm;
    result.hXStartOfs := xMoveStruct.HR_XStart_mm;
    result.hYStartOfs := xMoveStruct.HR_YStart_mm;
    result.hZStartOfs := xMoveStruct.HR_ZStart_mm;
    if Assigned(fMotors.RMotor) then
        result.hRStartOfs := xMoveStruct.HR_RStart_degree
    else
        result.hRStartOfs := 0;
    result.HZPutOfs := xMoveStruct.HR_ZPut_mm; // Abstellen des Racks
end;

{ TRackMoveLocationMotionSystem }

constructor TRackMoveLocationMotionSystem.Create(aRack: TRack; aMotionDevices: ILocationBasedMotionDevice);
begin
    inherited Create;
    fRack := aRack;
    fMotionDevices := aMotionDevices;
end;

procedure TRackMoveLocationMotionSystem.MovePlateFromPos(aMovePlateType: TMovePlateType;
    aMoveOptions: TRackMoveOptions; aZSpeed, aZRamp: integer);
// move away from carrier
var
    xXForward, xZUp: single;
    xMoveStruct: TRackMoveStruct;
    xDoXBeforeZ: boolean;
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(fRack);
    if not Assigned(xCarrier) then
        EXIT;

    xMoveStruct := xCarrier.GetRackMoveStruct();

    xDoXBeforeZ := ((aMovePlateType = mpGet) and aMoveOptions.GetMoveFromDoXBeforeZ) or
        ((aMovePlateType = mpPut) and aMoveOptions.PutMoveFromDoXBeforeZ);

    // move up in Z
    xZUp := xMoveStruct.HR_ZStart_mm;
    if aMovePlateType = mpPut then
        xZUp := xZUp + xMoveStruct.HR_ZPut_mm;
    // if XStart <> 0 then move backward in X direction
    xXForward := xMoveStruct.HR_XStart_mm;

    fMotionDevices.MoveFromGripPosition(xCarrier.Name, (fRack.Slot as TCarrierSlot).SlotNr, xXForward, xZUp,
        xDoXBeforeZ);

    fMotionDevices.DeviceApproachPath(xCarrier.Name, pPosAll, true);
end;

procedure TRackMoveLocationMotionSystem.MovePlateToPos1(aXSpeed, aXRamp, aYSpeed, aYRamp: integer);
var
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(fRack);
    if not Assigned(xCarrier) then
        EXIT;
    // move through approach path in the direction of the carrier
    fMotionDevices.DeviceApproachPath(xCarrier.Name, pPosAll, false);
end;

procedure TRackMoveLocationMotionSystem.Intern_MovePlateToPos2(aMovePlateType: TMovePlateType;
    aMoveOptions: TRackMoveOptions);
// move towards carrier
var
    xXForward, xZUp: single;
    xMoveStruct: TRackMoveStruct;
    xDoXBeforeZ: boolean;
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(fRack);
    if not Assigned(xCarrier) then
        EXIT;

    if not Assigned(fRack.Slot) then
        EXIT;
    // move to rack position.  This could also be a position which is not directly above the rack
    fMotionDevices.DeviceMoveToCarrierSlot(xCarrier.Name, (fRack.Slot as TCarrierSlot).SlotNr);

    xMoveStruct := xCarrier.GetRackMoveStruct();

    // if XStart <> 0 then move forward in X direction
    xXForward := xMoveStruct.HR_XStart_mm;
    xZUp := xMoveStruct.HR_ZStart_mm;
    // H_ZPut_mm should be a negative value
    if aMovePlateType = mpPut then
        xZUp := xZUp + xMoveStruct.HR_ZPut_mm;

    xDoXBeforeZ := ((aMovePlateType = mpGet) and aMoveOptions.GetMoveToDoXBeforeZ) or
        ((aMovePlateType = mpPut) and aMoveOptions.PutMoveToDoXBeforeZ);

    // move down in Z ()
    // if putting a plate then don't go down less than if you were getting a plate
    fMotionDevices.MoveToGripPosition(xCarrier.Name, (fRack.Slot as TCarrierSlot).SlotNr, xXForward, xZUp,
        xDoXBeforeZ);
end;

procedure TRackMoveLocationMotionSystem.MovePlateToPos2Get(aMoveOptions: TRackMoveOptions);
begin
    self.Intern_MovePlateToPos2(mpGet, aMoveOptions);
end;

procedure TRackMoveLocationMotionSystem.MovePlateToPos2Get_FromRetake;
begin
    inherited;

end;

procedure TRackMoveLocationMotionSystem.MovePlateToPos2Get_ToRetake;
begin
    inherited;

end;

procedure TRackMoveLocationMotionSystem.MovePlateToPos2Put_AfterRetake(aXSpeed, aXRamp, aYSpeed, aYRamp,
    aZSpeed, aZRamp: integer);
begin
    inherited;

end;

procedure TRackMoveLocationMotionSystem.MovePlateToPos2Put_BeforeRetake(aMoveOptions: TRackMoveOptions;
    aZSpeed, aZRamp: integer);
begin
    // No retake implemented yet, so we will do everything in BeforeRetake
    self.Intern_MovePlateToPos2(mpPut, aMoveOptions);
end;

procedure TRackMoveLocationMotionSystem.MovePlateToPos2Put_FromRetake1;
begin
    inherited;

end;

procedure TRackMoveLocationMotionSystem.MovePlateToPos2Put_FromRetake2;
begin
    inherited;

end;

procedure TRackMoveLocationMotionSystem.MovePlateToPos2Put_ToRetake;
begin
    inherited;

end;

function TRackMoveLocationMotionSystem.RetakeNeeded: boolean;
begin
    result := false;
end;


end.
