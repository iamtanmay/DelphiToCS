unit OperationRack;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : All Rack Operations - This is the link between workbench object and devices for rack gripping and rack movement
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                      track-no  improvement/change
  -------- --  ------------------------------------------  --------  ----------------------------------------------
  31.03.06 pk   GetSlotGroupData_Steps                      TN2958   Bug : Workbench Z-Steps were converted to steps
  31.03.06 pk                                               TN3009   Gripping values changed from steps to mm
  24.08.06 wl  GetSlotGroupData_Steps                       TN3269   statt eigener, doppelter Berechnungen wird aRack.CreateRackPosition aufgerufen
  15.09.06 pk  IsPlateMovable                               TN3073   check H_VIsUndefined
  29.11.06 wl  TRackMoveMotorMotionSystem.GetSlotGroupData_Steps  TN3408   wenn RackRotation 90° oder 270°, wird um einen anderen Mittelpunkt gedreht
  05.12.06 wl  TRackMoveMotorMotionSystem.GetSlotGroupData_Steps  TN3449   Retake-Werte ändern die Rackposition (z.B. für Pipettieren) nicht mehr
  19.01.07 wl  TRackMoveMotorMotionSystem.MovePlateToPos1         TN3509   gedreht wird erst bei Erreichen der Startposition
  31.01.07 wl  SLOTGROUPDATA.hRReTakeOfs                            TN3532   neu: wird aber noch nicht benutzt
  31.01.07 wl  TRackMoveLocationMotionSystem.MovePlateFromPos       TN3532  benutzt Carrier.GetRackMoveStruct()
  31.01.07 wl  TRackMoveLocationMotionSystem.Intern_MovePlateToPos2 TN3532  benutzt Carrier.GetRackMoveStruct()
  31.01.07 wl  TRackMoveMotorMotionSystem.GetSlotGroupData_Steps    TN3532  benutzt Carrier.GetRackMoveStruct()
  01.02.07 wl  TRackMoveMotorMotionSystem.MovePlateToPos1           TN3537  wenn MoveXYBeforeRotation = true, wird erst bei Erreichen der Startposition gedreht
  31.01.07 wl  TRackMoveMotorMotionSystem.GetSlotGroupData_Steps    TN3568  Bug: access violation when no R-Motor exists
  07.03.07 wl  TRackMoveMotionSystem                                TN3620   --> MotionSystemRack
  12.03.07 pk                                                       TN3629    new RackMoveOptions
  29.02.08 wl  TRackMoveOperation.Create                            TN4032   needs ZTravelManager
  29.02.08 wl  TRackMoveOperation.GetPlate                          TN4032   set ZTravel-Value to RackMoveZTravel
  29.02.08 wl  TRackMoveOperation.PutPlate                          TN4032   reset ZTravel-Value
  20.06.08 pk                                                       TN4139   Rack no longer has typed link to Carrier use TLayout.GetCarrierOfRack
  01.07.08 pk                                                       TN4139   Assertion for checking rack has carrier
  25.09.08 wl                                                       TN4242    TRunstCall ersetzt TDllCall
  07.09.12 ts                                                       TN5973   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Rack
  10.09.12 ts  TRackMoveOperation.GetPlate                          TN5977   new: RackMoveZTravelOffset, add to RackMoveZTravel
  01.07.13 wl  TRackMoveOperation.PutPlate                          TN6192   neue Parameter aXSpeed, aXRamp, aYSpeed, aYRamp
  -------------------------------------------------------------------------------------------------- }


interface


uses
    AppTypes,
    CommonTypes,
    Rack,
    EventManager,
    IntfGripDevice,
    MotionSystemRack,
    IntfZTravelManager;

type
    TRackMoveOperation = class
    protected
        fGripper: IGripDevice;
        fMotion: TRackMoveMotionSystem;
        fRack: TRack;
        fZTravelManager: IGripZTravelManager;
    public
        constructor Create(aMotion: TRackMoveMotionSystem; aGripper: IGripDevice; aRack: TRack;
            aZTravelManager: IGripZTravelManager);
        function PlateCheck(aVOpen, aVClose: TPosMM): boolean;
        function IsPlateMovable(): boolean;

        function GetPlate(aCheckPlate: boolean; aEvBeforeGet, aEvAfterGet: TRunstCall;
            aMoveOptions: TRackMoveOptions; aZSpeedRackUp: integer = 0; aZRampRackUp: integer = 0): boolean;
        function PutPlate(aCheckPlate: boolean; aEvBeforePut, aEvAfterPut: TRunstCall;
            aMoveOptions: TRackMoveOptions; aZSpeedRackDown: integer = 0; aZRampRackDown: integer = 0;
            aXSpeedRack: integer = 0; aXRampRack: integer = 0; aYSpeedRack: integer = 0;
            aYRampRack: integer = 0): boolean;
        class function GetDefaultRackMoveOptions(): TRackMoveOptions;
    end;


implementation


uses
    LogManager,
    ErrorManager,
    IntfMotionDevice,
    Layout,
    Carrier,
    CarrierSlot;

{ TRackMoveOperation }

constructor TRackMoveOperation.Create(aMotion: TRackMoveMotionSystem; aGripper: IGripDevice; aRack: TRack;
    aZTravelManager: IGripZTravelManager);
begin
    inherited Create;
    fMotion := aMotion;
    fGripper := aGripper;
    fRack := aRack;
    fZTravelManager := aZTravelManager;
end;

class function TRackMoveOperation.GetDefaultRackMoveOptions(): TRackMoveOptions;
begin
    result.GetMoveToDoXBeforeZ := true;
    result.PutMoveToDoXBeforeZ := true;
    result.GetMoveFromDoXBeforeZ := false;
    result.PutMoveFromDoXBeforeZ := false;
end;

function TRackMoveOperation.PlateCheck(aVOpen, aVClose: TPosMM): boolean;
begin
    result := true;

    if Assigned(fGripper) and (fGripper.VCheck > 0) then
    begin
        gLogManager.Log('Plate check', false);
        fGripper.CloseGripper(aVClose - fGripper.VCheck, true);
        // pk Removed for now - fGripper.Execute;
        if fGripper.IsGripError then
        begin
            gLogManager.Log('Plate check - plate ok', false);
            fGripper.CloseGripper(aVClose, true);
        end
        else
        begin
            gLogManager.Log('Plate check - no plate', false);
            result := false;
            fGripper.OpenGripper(aVOpen, false);
        end;
    end
    else
        gLogManager.Log('No Plate check -> plate OK assumed', false);
end;

function TRackMoveOperation.PutPlate(aCheckPlate: boolean; aEvBeforePut, aEvAfterPut: TRunstCall;
    aMoveOptions: TRackMoveOptions; aZSpeedRackDown, aZRampRackDown: integer;
    aXSpeedRack, aXRampRack, aYSpeedRack, aYRampRack: integer): boolean;
var
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(fRack);
    ASSERT(Assigned(xCarrier), 'Rack has no carrier');

    result := true;
    if (gErrorManager.IsGlobalErr) then
        exit;
    ASSERT(Assigned(fRack), 'Rack object is NULL');

    gLogManager.LogF('Put Plate %s to %s [%d]', [fRack.Name, xCarrier.Name,
        (fRack.Slot as TCarrierSlot).SlotNr + 1], true);

    // approach the carrier
    fMotion.MovePlateToPos1(aXSpeedRack, aXRampRack, aYSpeedRack, aYRampRack);

    // move to drop position ( with retake )
    fMotion.MovePlateToPos2Put_BeforeRetake(aMoveOptions, aZSpeedRackDown, aZRampRackDown);

    if fMotion.RetakeNeeded() then
    begin
        fMotion.MovePlateToPos2Put_ToRetake();
        fGripper.OpenGripper(fRack.Structure.H_VOpen_mm, true);
        fMotion.MovePlateToPos2Put_FromRetake1();
        fGripper.CloseGripper(fRack.Structure.H_VClose_mm, true);
        fMotion.MovePlateToPos2Put_FromRetake2();
    end;

    fMotion.MovePlateToPos2Put_AfterRetake(aXSpeedRack, aXRampRack, aYSpeedRack, aYRampRack, aZSpeedRackDown,
        aZRampRackDown);

    // Event "before put"
    if Assigned(aEvBeforePut) then
        aEvBeforePut.Execute('before put plate');

    // plate check
    if (aCheckPlate) then
        result := PlateCheck(fRack.Structure.H_VOpen_mm, fRack.Structure.H_VClose_mm);

    // open gripper
    fGripper.OpenGripper(fRack.Structure.H_VOpen_mm, true);

    // set ZTravel to default
    fZTravelManager.Reset;

    // Event "after put"
    if Assigned(aEvAfterPut) then
        aEvAfterPut.Execute('after put plate');

    // move away from carrier
    fMotion.MovePlateFromPos(mpPut, aMoveOptions);
end;

function TRackMoveOperation.GetPlate(aCheckPlate: boolean; aEvBeforeGet, aEvAfterGet: TRunstCall;
    aMoveOptions: TRackMoveOptions; aZSpeedRackUp, aZRampRackUp: integer): boolean;
var
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(fRack);
    ASSERT(Assigned(xCarrier), 'Rack has no carrier');

    result := true;
    if (gErrorManager.IsGlobalErr) then
        exit;
    ASSERT(Assigned(fRack), 'Rack object is NULL');

    gLogManager.LogF('Get Plate %s from %s [%d]', [fRack.Name, xCarrier.Name,
        (fRack.Slot as TCarrierSlot).SlotNr + 1], true);

    // approach the carrier
    fMotion.MovePlateToPos1(0, 0, 0, 0);

    // open gripper
    fGripper.OpenGripper(fRack.Structure.H_VOpen_mm, true);

    // move to gripping position
    fMotion.MovePlateToPos2Get(aMoveOptions);

    // Event "before get"
    if Assigned(aEvBeforeGet) then
        aEvBeforeGet.Execute('before get plate');

    // close the gripper
    fGripper.CloseGripper(fRack.Structure.H_VClose_mm, true);

    // plate check
    if (aCheckPlate) then
        result := PlateCheck(fRack.Structure.H_VOpen_mm, fRack.Structure.H_VClose_mm);

    // retake plate after get
    if fMotion.RetakeNeeded() then
    begin
        fMotion.MovePlateToPos2Get_ToRetake();
        fGripper.OpenGripper(fRack.Structure.H_VOpen_mm, true);
        fMotion.MovePlateToPos2Get_FromRetake();
        fGripper.CloseGripper(fRack.Structure.H_VClose_mm, true);
    end;

    // set ZTravel to RackMoveZTravel
    fZTravelManager.SetToRackMove(fRack.Structure.RackMoveZTravelOffset);

    // Event "after get"
    if Assigned(aEvAfterGet) then
        aEvAfterGet.Execute('after get plate');

    // move away from carrier
    fMotion.MovePlateFromPos(mpGet, aMoveOptions, aZSpeedRackUp, aZRampRackUp);
end;

function TRackMoveOperation.IsPlateMovable(): boolean;
// Ist Rack überhaupt greifbar
begin
    result := (not fRack.Structure.H_VIsUndefined) and fGripper.IsPosWithinRange(fRack.Structure.H_VClose_mm);
end;


end.
