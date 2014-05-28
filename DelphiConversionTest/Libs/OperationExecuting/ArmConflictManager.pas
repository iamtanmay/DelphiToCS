unit ArmConflictManager;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Manager for resolving device conflicts such as XMotor conflict
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  21.06.05 pk                                TN2464.3   New
  05.10.05 pk                                TN2653     Check if other XMotor is assigned
  24.04.06 pk                                TN3057     use XMotor.Conflictbuffer
  25.04.06 pk                                TN3057     New use ConflictbufferPlus, ConflictbufferMinus
  05.09.06 pk  MoveAway                      TN3278     use HomeMove Op
  09.11.07 pk  CheckXConflict                TN3924     ReadCurrentPos function name changed
  29.01.08 wl                                TN3980   uses geändert
  03.07.08 wl                                TN4157
  19.11.08 pk ResolveXConflict               TN4280     Implementation for xcMoveAwayCheckID
  19.11.08 pk UnRegisterUseXMotorAll         TN4280     called by each thread
  24.07.09 pk                                TN4677     Conflict was not detected when both XMotors moved at same time in 2 threads
  30.11.09 pk                                TN4915     Only the thread that is moving the motor can ask its position,
  when another threads asks position of an already moving motor, it causes problems in Canio
  08.03.10 pk ClearMotorUsageUsedByIDAll     TN5014     New
  10.03.11 wl  IsAnyXConflict                TN5496   statt OtherXMotor.ReadCurrentPos wird OtherXMotor.ReadCurrentPosThreadSafe aufgerufen
  19.05.11 ts ResolveXConflict               TN5575   in Simulation muss TRUE zurückgegeben werden, sonst wird die Ressource blockiert
  -------------------------------------------------------------------------------------------------- }


interface


uses
    AppTypes,
    Module,
    CommonTypes,
    IntfMotorDriver,
    ThreadClasses,
    ObjModul,
    IntfMotorDevice,
    IntfArmDevice;

type
    TXConflictResolveMode = (xcNone, xcMoveAway, xcWait, xcMoveAwayCheckID);

    TDevicesConflictManager = class
    protected
        fArmManager: TArmDeviceList;
        fXConflictResolveMode: TXConflictResolveMode;
        fXConflictResolveHardMode: TXConflictResolveMode;
        fXConflictBufferMin_MM: TPosMM;
        fCriticalSection: TSemaphore;
        procedure SetXConflictResolveMode(aMode: TXConflictResolveMode);
        function ResolveXConflict(aUsedXMotor: IXMotorDevice; aOtherArm: IArmDevice;
            aOtherXMotor: IXMotorDevice): boolean;
        function IsXConflict(const aUsedXMotor, aOtherXMotor: IXMotorDevice; const aOtherXMotorPos: TPosMM;
            const aCurrent, aDest: TPosMM; out oOtherXMotorRequiredPos: TPosMM): boolean;
        function IsAnyXConflict(const aUsedXMotor: IXMotorDevice; const aUsedByID: cardinal;
            const aCurrent, aDest: TPosMM; out oOtherArm: IArmDevice; out oOtherXMotor: IXMotorDevice;
            out oOtherXMotorRequiredPos: TPosMM): boolean;

        procedure CheckXConflict(aUsedXMotor: IModule; aDest: TPosMM; aIsInitMove: boolean);

        function GetXMotor(aUsedArm: IArmDevice; out oXMotor: IXMotorDevice): boolean;
        // procedure CheckZTravelConflict( aUsedZMotors : TObject );
        // function GetZMotors( aUsedArm : IArmDevice; out oZMotors : TMultiZMotorDevice ) : boolean;
        procedure MoveAway(aUsedArm: IArmDevice);
        function CanUseXMotor(const aXMotorDevice: IXMotorDevice): boolean;
        procedure RegisterUseXMotor(const aXMotorDevice: IXMotorDevice; const aUsedByID: cardinal;
            const aDest: TPosMM; const aIsInitMove: boolean);
        procedure XMoveDone(aUsedXMotor: IModule; aCurrentPos: TPosMM);
    public
        constructor Create(aArmManager: TArmDeviceList);
        destructor Destroy(); override;
        procedure InitMode();
        procedure UnRegisterUseXMotorAll();
        procedure ClearMotorUsageUsedByIDAll();
        procedure AssignEvents();
        property XConflicResolveMode: TXConflictResolveMode read fXConflictResolveMode
            write SetXConflictResolveMode;
    end;


implementation


uses
    SysUtils,
    Math,
    LogManager,
    AppSettings,
    OperationFactory,
    OperationAxisMove,
    SamHigh,
    IntfMotorBasedMotionDevice,
    ErrorManager,
    RunFlow;

const
    ENUM_XCONFLICT_RESOLVE_MODE_DEF = xcMoveAway;

    { TDevicesConflictManager }

procedure TDevicesConflictManager.MoveAway(aUsedArm: IArmDevice);
var
    xHomeMoveOp: THomeMoveOperation;
begin
    xHomeMoveOp := TOperationFactory.CreateHomeMoveOp(aUsedArm);
    xHomeMoveOp.MoveToHome();
    xHomeMoveOp.Free;
end;

constructor TDevicesConflictManager.Create(aArmManager: TArmDeviceList);
var
    xIniAccess: IWinlissyIniAccess;
begin
    inherited Create;
    fArmManager := aArmManager;

    xIniAccess := gCommonDll.CreateRobotIni; // "ROBOT"
    fXConflictResolveHardMode := xcNone;
    fCriticalSection := TThreadAPI.CreateSemaphore(1);
    InitMode();
    fXConflictResolveHardMode := TXConflictResolveMode(xIniAccess.ReadInteger('XConflictResolve',
        'HardMode'));
    if fXConflictResolveHardMode <> xcNone then
        SetXConflictResolveMode(fXConflictResolveHardMode);
    fXConflictBufferMin_MM := xIniAccess.ReadFloat('XConflictResolve', 'ConflictBuffer');
end;

destructor TDevicesConflictManager.Destroy();
begin
    fCriticalSection.Free;
    inherited;
end;

procedure TDevicesConflictManager.SetXConflictResolveMode(aMode: TXConflictResolveMode);
begin
    if fXConflictResolveHardMode <> xcNone then
        EXIT;
    fXConflictResolveMode := aMode;
    ClearMotorUsageUsedByIDAll();
end;

procedure TDevicesConflictManager.ClearMotorUsageUsedByIDAll();
var
    x: integer;
    xXMotor: IXMotorDevice;
begin
    for x := 0 to fArmManager.Count - 1 do
    begin
        if not GetXMotor(fArmManager.Arms[x], xXMotor) then
            CONTINUE;
        xXMotor.MotorUsage.ClearUsedByID();
    end;
end;

procedure TDevicesConflictManager.InitMode();
begin
    SetXConflictResolveMode(ENUM_XCONFLICT_RESOLVE_MODE_DEF);
end;

function TDevicesConflictManager.GetXMotor(aUsedArm: IArmDevice; out oXMotor: IXMotorDevice): boolean;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
begin
    result := false;
    oXMotor := nil;

    if not SysUtils.Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
        EXIT;

    oXMotor := xMotorBasedMD.XMotor;
    result := Assigned(oXMotor);
end;

procedure TDevicesConflictManager.AssignEvents();
var
    i: integer;
    xXMotor: IXMotorDevice;
begin
    for i := 0 to gArmManager.Count - 1 do
    begin
        if GetXMotor(gArmManager.Arms[i], xXMotor) then
        begin
            xXMotor.OnBeforeMove := CheckXConflict;
            xXMotor.OnAfterMove := XMoveDone;
        end;
    end;
end;

function TDevicesConflictManager.CanUseXMotor(const aXMotorDevice: IXMotorDevice): boolean;
var
    xCurrentID: cardinal;
begin
    xCurrentID := TThreadAPI.GetCurrentThreadID();

    fCriticalSection.Enter();
    try
        result := aXMotorDevice.MotorUsage.IsUsageAllowed(xCurrentID);
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TDevicesConflictManager.RegisterUseXMotor(const aXMotorDevice: IXMotorDevice;
    const aUsedByID: cardinal; const aDest: TPosMM; const aIsInitMove: boolean);
begin
    fCriticalSection.Enter();
    try
        aXMotorDevice.MotorUsage.RegisterPendingMove(aUsedByID, aDest, aIsInitMove);
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TDevicesConflictManager.UnRegisterUseXMotorAll();
var
    xCurrentID: cardinal;
    x: integer;
    xXMotor: IXMotorDevice;
begin
    xCurrentID := TThreadAPI.GetCurrentThreadID();

    fCriticalSection.Enter();
    try
        for x := 0 to fArmManager.Count - 1 do
        begin
            if not GetXMotor(fArmManager.Arms[x], xXMotor) then
                CONTINUE;
            if xXMotor.MotorUsage.IsLastUsedBy(xCurrentID) then
            begin
                xXMotor.MotorUsage.ClearPending();
                xXMotor.MotorUsage.ClearUsedByID();
            end;
        end;
    finally
        fCriticalSection.Leave();
    end;
end;

function TDevicesConflictManager.ResolveXConflict(aUsedXMotor: IXMotorDevice; aOtherArm: IArmDevice;
    aOtherXMotor: IXMotorDevice): boolean;
var
    xCanUseXMotor: boolean;
begin
    if gRunFlow.SimulationMode then
        EXIT(true);

    result := false;
    if (fXConflictResolveMode = xcMoveAway) or (fXConflictResolveMode = xcMoveAwayCheckID) then
    begin
        xCanUseXMotor := true;
        if (fXConflictResolveMode = xcMoveAwayCheckID) then
        begin
            xCanUseXMotor := CanUseXMotor(aOtherXMotor);
        end;

        if xCanUseXMotor then
        begin
            gLogManager.LogF('XMotor [%s] : Movement blocked by other arm [%s]. Moving away other arm.',
                [aUsedXMotor.Name, aOtherArm.Name], false);
            self.MoveAway(aOtherArm);
            result := true;
        end;
    end;
end;

function TDevicesConflictManager.IsXConflict(const aUsedXMotor, aOtherXMotor: IXMotorDevice;
    const aOtherXMotorPos: TPosMM; const aCurrent, aDest: TPosMM;
    out oOtherXMotorRequiredPos: TPosMM): boolean;
const
    DBL_MOVE_NECESSARY_MARGIN_MM = 0.2; // in mm
var
    xXConflictBuffer_MM: TPosMM;
begin
    result := false;

    // if the distance between destination and the current position is very small dont check for conflict
    // Note: this obviously covers the case where aDest = aCurrent
    if Abs(aDest - aCurrent) <= DBL_MOVE_NECESSARY_MARGIN_MM then
        EXIT;

    xXConflictBuffer_MM := fXConflictBufferMin_MM;

    if aDest > aCurrent then
    begin // Moving away from worldX = 0
        if (aCurrent > aOtherXMotorPos) then
            EXIT; // other arm is closer to worldx=0 than self - no collision possible
        xXConflictBuffer_MM := Max(xXConflictBuffer_MM, aUsedXMotor.ConflictBufferPlus_MM +
            aOtherXMotor.ConflictBufferMinus_MM);
        oOtherXMotorRequiredPos := aDest + xXConflictBuffer_MM;
        if aOtherXMotorPos >= oOtherXMotorRequiredPos then
            EXIT;
    end
    else
    begin // Moving towards worldX = 0
        if (aOtherXMotorPos > aCurrent) then
            EXIT; // other arm is farther from worldx=0 than self - no collision possible
        xXConflictBuffer_MM := Max(xXConflictBuffer_MM, aUsedXMotor.ConflictBufferMinus_MM +
            aOtherXMotor.ConflictBufferPlus_MM);
        oOtherXMotorRequiredPos := aDest - xXConflictBuffer_MM;
        if aOtherXMotorPos <= oOtherXMotorRequiredPos then
            EXIT;
    end;

    result := true;
end;

function TDevicesConflictManager.IsAnyXConflict(const aUsedXMotor: IXMotorDevice; const aUsedByID: cardinal;
    const aCurrent, aDest: TPosMM; out oOtherArm: IArmDevice; out oOtherXMotor: IXMotorDevice;
    out oOtherXMotorRequiredPos: TPosMM): boolean;
var
    x: integer;
    xIsXConflict: boolean;
    xOtherXMotorCurrentPos: TPosMM;
begin
    result := false;

    for x := 0 to fArmManager.Count - 1 do
    begin
        oOtherArm := fArmManager.Arms[x];
        if not GetXMotor(oOtherArm, oOtherXMotor) then
            CONTINUE;

        // if OtherXMotor is not enabled or OtherXMotor is really the same XMotor
        if (not oOtherXMotor.Active) or (aUsedXMotor.MotorID = oOtherXMotor.MotorID) or
            (aUsedXMotor.TrackID < 0) or (aUsedXMotor.TrackID <> oOtherXMotor.TrackID) then
            CONTINUE;

        // we assume that we are already in a thread-safe function, otherwise we would need a critical section before using the MotorUsage properties

        // Case 1: other motor is initializing
        xIsXConflict := oOtherXMotor.MotorUsage.IsInitMovePending;
        // if the other motor is intializing, we have no idea where it is so assume worst case

        // Case 2: other motor is blocking the way at its current position
        if (not xIsXConflict) then
        begin
            // if another thread is using the other xmotor this thread is not even allowed to read position of other motor (for ZP02 Motors)
            if oOtherXMotor.MotorUsage.IsUsageAllowed(aUsedByID) then
                xOtherXMotorCurrentPos := oOtherXMotor.ReadCurrentPosThreadSafe
            else
                xOtherXMotorCurrentPos := oOtherXMotor.MotorUsage.CurrentPos;

            xIsXConflict := self.IsXConflict(aUsedXMotor, oOtherXMotor, xOtherXMotorCurrentPos, aCurrent,
                aDest, oOtherXMotorRequiredPos);
        end;

        // Case 3: other motor will move to a position that will block the way
        if (not xIsXConflict) and (oOtherXMotor.MotorUsage.IsNonInitMovePending) then
        begin
            xIsXConflict := self.IsXConflict(aUsedXMotor, oOtherXMotor,
                oOtherXMotor.MotorUsage.PendingDestPos, aCurrent, aDest, oOtherXMotorRequiredPos);
        end;

        if xIsXConflict then
        begin
            result := true;
            EXIT;
        end;
    end;
end;

procedure TDevicesConflictManager.XMoveDone(aUsedXMotor: IModule; aCurrentPos: TPosMM);
var
    xUsedXMotor: IXMotorDevice;
begin
    Assert(SysUtils.Supports(aUsedXMotor, IXMotorDevice, xUsedXMotor), 'No X-Motor');
    xUsedXMotor.MotorUsage.PendingMoveDone(aCurrentPos);
end;

procedure TDevicesConflictManager.CheckXConflict(aUsedXMotor: IModule; aDest: TPosMM; aIsInitMove: boolean);
var
    xCurrentPos: TPosMM;
    xOtherArm: IArmDevice;
    xUsedXMotor, xOtherXMotor, xPreviousConflictXMotor: IXMotorDevice;
    xOtherXMotorRequiredPos: TPosMM;
    xConflictFound: boolean;
    xOtherXMotorLastUsedByID: cardinal;
    xCurrentID: cardinal;
begin
    Assert(SysUtils.Supports(aUsedXMotor, IXMotorDevice, xUsedXMotor), 'No X-Motor');
    xPreviousConflictXMotor := nil;

    xCurrentID := TThreadAPI.GetCurrentThreadID();

    xCurrentPos := xUsedXMotor.ReadCurrentPos;
    while true do
    begin
        if gErrorManager.IsGlobalErr() then
            EXIT;

        fCriticalSection.Enter();
        try
            xConflictFound := false;
            if not aIsInitMove then
            begin
                xConflictFound := IsAnyXConflict(xUsedXMotor, xCurrentID, xCurrentPos, aDest, xOtherArm,
                    xOtherXMotor, xOtherXMotorRequiredPos);
            end;

            if not xConflictFound then
            begin
                RegisterUseXMotor(xUsedXMotor, xCurrentID, aDest, aIsInitMove);
                EXIT;
            end;

            if ResolveXConflict(xUsedXMotor, xOtherArm, xOtherXMotor) then
                EXIT;

            xOtherXMotorLastUsedByID := xOtherXMotor.MotorUsage.LastUsedByID;
        finally
            fCriticalSection.Leave();
        end;

        if xOtherXMotor <> xPreviousConflictXMotor then
            gLogManager.LogF
                ('XMotor [%s] : Movement blocked by other arm [%s] which was previously used by thread [%d]. Waiting...',
                [xUsedXMotor.Name, xOtherArm.Name, xOtherXMotorLastUsedByID], true);

        xPreviousConflictXMotor := xOtherXMotor;

        gRunFlow.AppSleep(500);
    end;
end;


end.
