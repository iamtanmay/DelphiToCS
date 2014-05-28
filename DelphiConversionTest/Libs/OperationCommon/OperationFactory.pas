{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : creates operations using devices and workbench objects
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  ---------------------------       -------- -----------------------------------------------
  18.04.06 pk  CreateZAxisPipMoveMotorMotionSys  TN2958    create using rackmovemanager
  05.09.06 pk  CreateHomeMoveOp                  TN3278    new
  15.09.06 pk  CreateRackBCReadingOp             TN3309    create op even if motionsystem is nil
  23.10.06 pk  CreateHomeMoveOp                  TN3345    Use ZTravelMoveMotorMotionSys instead of RackTravelMoveMotorMotionSys
  27.10.06 pk  CreateTubeBCReadingAndMoveOp      TN3386    New
  06.12.06 wl  CreateInitOp                      TN3452    reads property aUsedArm.InitXBeforeY for the init operation
  19.01.07 pk  CreateRackBCReadingOp             TN3511    use aUsedArm only if aRackMovable=true
  07.03.07 wl                                    TN3620    benutzt neue MotionSystem-units
  12.03.07 pk  CreateZAxisTravelMotionSys        TN3628    can now create a TZAxisTravelMotionSystem for the LocationbasedMotion as well
  12.04.07 pk  CreateBCReadingMotionSys          TN3664    can now create a TBCReadingLocationMotionSystem
  12.07.07 wl  CreateRackMoveOp                  TN3782    geänderter Aufruf von TRackMoveOperation.Create
  13.11.07 wl  CreateZAxisPipMoveWithBalanceOp   TN3844    neu für Powder detection
  29.02.08 wl  CreateRackMoveOp                  TN4032    needs ZTravelManager
  22.04.08 pk                                    TN4080    use IContainerBCReaderDevice, ITubeBCReaderDevice, IRackBCReaderDevice
  10.07.08 hd  CreateToolMoveOp				    TN4163    can now create a TToolMoveOperation for the ILocationBasedMotionDevice
  27.08.10 wl  TRackMoveMotorMotionSystem.HandlerXYIndependetFromRotation  TN5250   entfernt
  26.01.11 wl  TXYZTravelAxisPipMoveOperation    TN5448   MoveZTravelIfNoXYMove entfernt
  25.03.11 wl  CreateInitOp                      TN5519   an TGripInitMotorMotionSystem angepasst
  29.03.11 wl  CreatePipStepXYZCalc              TN5524   --> OperationPip
  29.03.11 wl                                                       TN5524   OperationPip -> MotorStepCalculator
  15.11.11 wl  CreateXYZRMotorMotionDevice       TN5736   war vorher in TMotorBasedMotionDevice
  25.02.13 wl  CreateInitOp                      TN6097   Grip- und Piparm vereinheitlicht
  23.05.13 wl                                    TN6153   verwendet GetPreviousPosition/SetPreviousPosition-Events (statt RackMoveManager)
  27.09.13 wl  CreateRackTravelMoveMotorMotionSys TN6263  prüft, ob PipDevice existiert
  03.03.14 tp  CreateZTravelMoveMotorMotionSys,
  CreateXYZRMotorMotionDevice                    TN6347  Varispan korrigierung implementiert mit Gripper := aUsedArm.GripDevice
  -------------------------------------------------------------------------------------------------- }

unit OperationFactory;


interface


uses
    AppTypes,
    Rack,
    RackTypes,
    IntfMotorDevice,
    IntfGripDevice,
    IntfBalanceDevice,
    IntfPipDevice,
    IntfMotionDevice,
    IntfArmDevice,
    IntfZTravelManager,
    IntfContainerBCReaderDevice,
    MotionSystemBCReading,
    MotorStepCalculator,
    MotionSystemTube,
    MotionSystemTravel,
    MotionSystemPipMove,
    OperationBCReading,
    OperationRack,
    IntfMotorBasedMotionDevice,
    OperationTube,
    OperationAxisMove,
    OperationTool;

type
    TOperationFactory = class
    private
        // Motion system
        class function CreateXYZRMotorMotionDevice(aUsedArm: IArmDevice;
            aMotionDevice: IMotorBasedMotionDevice): TXYZRMotorMotionSystem;
        class function CreateZTravelMoveMotorMotionSys(aUsedArm: IArmDevice): TZAxisTravelMotorMotionSystem;
        class function CreateRackTravelMoveMotorMotionSys(aUsedArm: IArmDevice)
            : TZAxisRackTravelMotorMotionSystem;
        class function CreateXYAxisMoveMotorMotionSys(aUsedArm: IArmDevice): TXYAxisMotorMotionSystem;
        class function CreateBCReadingMotionSys(aUsedArm: IArmDevice): TBCReadingMotionSystem;
        class function CreateTubeMoveMotionSys(aUsedArm: IArmDevice): TTubeMoveMotionSystem;
    public
        class function CreateZAxisTravelMotionSys(aMotionDevice: IMotionDevice)
            : TZAxisTravelMotionSystem; overload;
        class function CreateZAxisTravelMotionSys(aMotionDevice: IMotionDevice; aUsedArm: IArmDevice)
            : TZAxisTravelMotionSystem; overload;
        class function CreateZAxisPipMoveMotorMotionSys(aMotionDevice: IMotionDevice)
            : TZAxisPipMoveMotorMotionSystem;

        // Operation
        class function CreateZAxisTravelMoveOp(aMotionDevice: IMotionDevice; aZTravelManager: IZTravelManager)
            : TZAxisTravelMoveOperation; overload;
        class function CreateZAxisTravelMoveOp(aMotionDevice: IMotionDevice; aZTravelManager: IZTravelManager;
            aUsedArm: IArmDevice): TZAxisTravelMoveOperation; overload;
        class function CreateZAxisPipMoveOp(aUsedArm: IArmDevice): TZAxisPipMoveOperation;
        class function CreateZAxisPipMoveWithBalanceOp(aUsedArm: IArmDevice; aBalance: IBalanceDevice)
            : TZAxisPipMoveWithBalanceOperation;
        class function CreateRackMoveOp(aUsedArm: IArmDevice; aRack: TRack): TRackMoveOperation;
        class function CreateRackBCReadingOp(aUsedArm: IArmDevice; aReadingDev: IContainerBCReaderDevice;
            aRackMovable: boolean): TRackBCReadingOperation;
        class function CreateTubeBCReadingOp(aUsedArm: IArmDevice; aReadingDev: IContainerBCReaderDevice)
            : TTubeBCReadingOperation;
        class function CreateTubeBCReadingAndMoveOp(aUsedArm: IArmDevice;
            aReadingDev: IContainerBCReaderDevice; aDestPos: TXRackPosition): TTubeBCReadingAndMoveOperation;
        class function CreateXYZTravelPipMoveOp(aUsedArm: IArmDevice): TXYZTravelAxisPipMoveOperation;
        class function CreateToolMoveOp(aUsedArm: IArmDevice): TToolMoveOperation;
        class function CreateTubeMoveOp(aUsedArm: IArmDevice): TTubeMoveOperation;
        class function CreateInitOp(aUsedArm: IArmDevice): TInitOperation;
        class function CreateHomeMoveOp(aUsedArm: IArmDevice): THomeMoveOperation;
    end;


implementation


uses
    SysUtils,
    MotionSystemInit,
    MotionSystemRack,
    ArmPositionInfo;

{ TOperationFactory }

class function TOperationFactory.CreateZAxisTravelMotionSys(aMotionDevice: IMotionDevice)
    : TZAxisTravelMotionSystem;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
    xLocationBasedMD: ILocationBasedMotionDevice;
begin
    result := nil;

    if Supports(aMotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := TZAxisTravelMotorMotionSystem.Create(xMotorBasedMD.ZMotors)
    end
    else if SysUtils.Supports(aMotionDevice, ILocationBasedMotionDevice, xLocationBasedMD) then
    begin
        result := TZAxisTravelLocationMotionSystem.Create();
    end;
end;

class function TOperationFactory.CreateZAxisTravelMotionSys(aMotionDevice: IMotionDevice;
    aUsedArm: IArmDevice): TZAxisTravelMotionSystem;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
    xLocationBasedMD: ILocationBasedMotionDevice;
    xTZAxisTravelMotorMotionSystem: TZAxisTravelMotorMotionSystem;
begin
    result := nil;

    if Supports(aMotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        xTZAxisTravelMotorMotionSystem := TZAxisTravelMotorMotionSystem.Create(xMotorBasedMD.ZMotors);
        xTZAxisTravelMotorMotionSystem.Gripper := aUsedArm.GripDevice;
        result := xTZAxisTravelMotorMotionSystem;
    end
    else if SysUtils.Supports(aMotionDevice, ILocationBasedMotionDevice, xLocationBasedMD) then
    begin
        result := TZAxisTravelLocationMotionSystem.Create();
    end;
end;

class function TOperationFactory.CreateZAxisTravelMoveOp(aMotionDevice: IMotionDevice;
    aZTravelManager: IZTravelManager): TZAxisTravelMoveOperation;
begin
    result := TZAxisTravelMoveOperation.Create(CreateZAxisTravelMotionSys((aMotionDevice)))
end;

class function TOperationFactory.CreateZAxisTravelMoveOp(aMotionDevice: IMotionDevice;
    aZTravelManager: IZTravelManager; aUsedArm: IArmDevice): TZAxisTravelMoveOperation;
begin
    result := TZAxisTravelMoveOperation.Create(CreateZAxisTravelMotionSys(aMotionDevice, aUsedArm))
end;

class function TOperationFactory.CreateRackTravelMoveMotorMotionSys(aUsedArm: IArmDevice)
    : TZAxisRackTravelMotorMotionSystem;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
    xTipOffsets: TArray<double>;
begin
    result := nil;

    if not Assigned(aUsedArm.PipDevice) then
        EXIT;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        xTipOffsets := aUsedArm.PipDevice.GetTip_ZOffsets_mm();
        result := TZAxisRackTravelMotorMotionSystem.Create(xMotorBasedMD.ZMotors, xTipOffsets);
    end;
end;

class function TOperationFactory.CreateZTravelMoveMotorMotionSys(aUsedArm: IArmDevice)
    : TZAxisTravelMotorMotionSystem;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
    xTZAxisTravelMotorMotionSystem: TZAxisTravelMotorMotionSystem;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        xTZAxisTravelMotorMotionSystem := TZAxisTravelMotorMotionSystem.Create(xMotorBasedMD.ZMotors);
        xTZAxisTravelMotorMotionSystem.Gripper := aUsedArm.GripDevice;
        result := xTZAxisTravelMotorMotionSystem;
    end;
end;

class function TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aMotionDevice: IMotionDevice)
    : TZAxisPipMoveMotorMotionSystem;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
begin
    result := nil;

    if Supports(aMotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := TZAxisPipMoveMotorMotionSystem.Create(xMotorBasedMD.ZMotors);
    end;
end;

class function TOperationFactory.CreateTubeMoveMotionSys(aUsedArm: IArmDevice): TTubeMoveMotionSystem;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
    xLocationBasedMD: ILocationBasedMotionDevice;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := TTubeMoveMotorMotionSystem.Create(CreateXYZRMotorMotionDevice(aUsedArm, xMotorBasedMD),
            CreateXYAxisMoveMotorMotionSys(aUsedArm), CreateZTravelMoveMotorMotionSys(aUsedArm),
            TMotorStepCalculatorFactory.CreateMotorXYZStepCalc(aUsedArm, 1))
    end
    else if Supports(aUsedArm.MotionDevice, ILocationBasedMotionDevice, xLocationBasedMD) then
    begin
        result := TTubeMoveLocationMotionSystem.Create(xLocationBasedMD);
    end;
end;

class function TOperationFactory.CreateZAxisPipMoveOp(aUsedArm: IArmDevice): TZAxisPipMoveOperation;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := TZAxisPipMoveOperation.Create(CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice),
            CreateZTravelMoveMotorMotionSys(aUsedArm), aUsedArm.PositionMemory.OnGetPreviousPosition);
    end;
end;

class function TOperationFactory.CreateZAxisPipMoveWithBalanceOp(aUsedArm: IArmDevice;
    aBalance: IBalanceDevice): TZAxisPipMoveWithBalanceOperation;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := TZAxisPipMoveWithBalanceOperation.Create
            (CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice),
            CreateZTravelMoveMotorMotionSys(aUsedArm), aUsedArm.PositionMemory.OnGetPreviousPosition,
            aBalance);
    end;
end;

class function TOperationFactory.CreateRackMoveOp(aUsedArm: IArmDevice; aRack: TRack): TRackMoveOperation;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
    xLocationBasedMD: ILocationBasedMotionDevice;
    xZTravelManager: IGripZTravelManager;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        ASSERT(Supports(aUsedArm.ZTravelManager, IGripZTravelManager, xZTravelManager));
        result := TRackMoveOperation.Create(TRackMoveMotorMotionSystem.Create(aRack,
            CreateXYZRMotorMotionDevice(aUsedArm, xMotorBasedMD), CreateXYAxisMoveMotorMotionSys(aUsedArm),
            CreateZTravelMoveMotorMotionSys(aUsedArm),
            TMotorStepCalculatorFactory.CreateMotorXYZStepCalc(aUsedArm, 1)), aUsedArm.GripDevice, aRack,
            xZTravelManager)
    end
    else if Supports(aUsedArm.MotionDevice, ILocationBasedMotionDevice, xLocationBasedMD) then
    begin
        ASSERT(Supports(aUsedArm.ZTravelManager, IGripZTravelManager, xZTravelManager));
        result := TRackMoveOperation.Create(TRackMoveLocationMotionSystem.Create(aRack, xLocationBasedMD),
            aUsedArm.GripDevice, aRack, xZTravelManager);
    end;
end;

class function TOperationFactory.CreateBCReadingMotionSys(aUsedArm: IArmDevice): TBCReadingMotionSystem;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
    xLocationBasedMD: ILocationBasedMotionDevice;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := TBCReadingMotorMotionSystem.Create(CreateXYZRMotorMotionDevice(aUsedArm, xMotorBasedMD),
            CreateZTravelMoveMotorMotionSys(aUsedArm))
    end
    else if Supports(aUsedArm.MotionDevice, ILocationBasedMotionDevice, xLocationBasedMD) then
    begin
        result := TBCReadingLocationMotionSystem.Create(xLocationBasedMD);
    end;
end;

class function TOperationFactory.CreateRackBCReadingOp(aUsedArm: IArmDevice;
    aReadingDev: IContainerBCReaderDevice; aRackMovable: boolean): TRackBCReadingOperation;
var
    xMotionSystem: TBCReadingMotionSystem;
    xGripDevice: IGripDevice;
begin
    xMotionSystem := nil;
    xGripDevice := nil;

    if aRackMovable then
    begin
        xGripDevice := aUsedArm.GripDevice;
        xMotionSystem := self.CreateBCReadingMotionSys(aUsedArm);
    end;

    result := TRackBCReadingOperation.Create(xMotionSystem, xGripDevice, aReadingDev, aRackMovable);
end;

class function TOperationFactory.CreateTubeBCReadingOp(aUsedArm: IArmDevice;
    aReadingDev: IContainerBCReaderDevice): TTubeBCReadingOperation;
var
    xMotionSystem: TBCReadingMotionSystem;
begin
    xMotionSystem := CreateBCReadingMotionSys(aUsedArm);
    result := TTubeBCReadingOperation.Create(xMotionSystem, aUsedArm.GripDevice, aReadingDev);
end;

class function TOperationFactory.CreateTubeBCReadingAndMoveOp(aUsedArm: IArmDevice;
    aReadingDev: IContainerBCReaderDevice; aDestPos: TXRackPosition): TTubeBCReadingAndMoveOperation;
var
    xMotionSystem: TBCReadingMotionSystem;
begin
    xMotionSystem := CreateBCReadingMotionSys(aUsedArm);
    result := TTubeBCReadingAndMoveOperation.Create(xMotionSystem, CreateTubeMoveMotionSys(aUsedArm),
        aUsedArm.GripDevice, aReadingDev, aDestPos);
end;

class function TOperationFactory.CreateXYAxisMoveMotorMotionSys(aUsedArm: IArmDevice)
    : TXYAxisMotorMotionSystem;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := TXYAxisMotorMotionSystem.Create(xMotorBasedMD.XMotor, xMotorBasedMD.YMotors,
            aUsedArm.XYRange, aUsedArm.PositionMemory.OnSetPreviousPosition);
    end;
end;

class function TOperationFactory.CreateXYZRMotorMotionDevice(aUsedArm: IArmDevice;
    aMotionDevice: IMotorBasedMotionDevice): TXYZRMotorMotionSystem;
var
    xTXYZRMotorMotionSystem: TXYZRMotorMotionSystem;
begin
    xTXYZRMotorMotionSystem := TXYZRMotorMotionSystem.Create(aMotionDevice.XMotor, aMotionDevice.YMotor,
        aMotionDevice.ZMotor, aMotionDevice.RMotor);
    xTXYZRMotorMotionSystem.Gripper := aUsedArm.GripDevice;
    result := xTXYZRMotorMotionSystem;
end;

class function TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm: IArmDevice)
    : TXYZTravelAxisPipMoveOperation;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := TXYZTravelAxisPipMoveOperation.Create(CreateXYAxisMoveMotorMotionSys(aUsedArm),
            CreateRackTravelMoveMotorMotionSys(aUsedArm), aUsedArm.PositionMemory.OnGetPreviousPosition,
            aUsedArm.PositionMemory.OnSetPreviousPosition);
    end;
end;

class function TOperationFactory.CreateToolMoveOp(aUsedArm: IArmDevice): TToolMoveOperation;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
    xLocationBasedMD: ILocationBasedMotionDevice;
    xZTravelManager: IGripZTravelManager;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        ASSERT(Supports(aUsedArm.ZTravelManager, IGripZTravelManager, xZTravelManager));
        result := TToolMoveOperation.Create(aUsedArm, xMotorBasedMD, aUsedArm.GripDevice, xZTravelManager);
    end
    else if Supports(aUsedArm.MotionDevice, ILocationBasedMotionDevice, xLocationBasedMD) then
    begin
        ASSERT(Supports(aUsedArm.ZTravelManager, IGripZTravelManager, xZTravelManager));
        result := TToolMoveOperation.Create(aUsedArm, xLocationBasedMD, aUsedArm.GripDevice, xZTravelManager);
    end;
end;

class function TOperationFactory.CreateTubeMoveOp(aUsedArm: IArmDevice): TTubeMoveOperation;
var
    xZTravelManager: IGripZTravelManager;
begin
    ASSERT(Supports(aUsedArm.ZTravelManager, IGripZTravelManager, xZTravelManager));
    result := TTubeMoveOperation.Create(CreateTubeMoveMotionSys(aUsedArm), aUsedArm.GripDevice,
        xZTravelManager);
end;

class function TOperationFactory.CreateHomeMoveOp(aUsedArm: IArmDevice): THomeMoveOperation;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := THomeMoveOperation.Create(CreateXYAxisMoveMotorMotionSys(aUsedArm),
            CreateZTravelMoveMotorMotionSys(aUsedArm), aUsedArm.GripDevice, xMotorBasedMD.RMotor);
    end;
end;

class function TOperationFactory.CreateInitOp(aUsedArm: IArmDevice): TInitOperation;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
    xLocationBasedMD: ILocationBasedMotionDevice;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := TInitOperation.Create(TInitMotorMotionSystem.Create(xMotorBasedMD.XMotor,
            xMotorBasedMD.YMotors, xMotorBasedMD.RMotor, aUsedArm.InitXBeforeY), aUsedArm.GripDevice,
            aUsedArm.PositionMemory.OnSetPreviousPosition)
    end
    else if Supports(aUsedArm.MotionDevice, ILocationBasedMotionDevice, xLocationBasedMD) then
    begin
        result := TInitOperation.Create(TInitLocationMotionSystem.Create(xLocationBasedMD),
            aUsedArm.GripDevice, aUsedArm.PositionMemory.OnSetPreviousPosition)
    end;
end;


end.
