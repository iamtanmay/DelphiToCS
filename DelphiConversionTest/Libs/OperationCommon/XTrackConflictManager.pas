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
  08.03.10 pk  ClearMotorUsageUsedByIDAll    TN5014     New
  10.03.11 wl  IsAnyXConflict                TN5496   statt OtherXMotor.ReadCurrentPos wird OtherXMotor.ReadCurrentPosThreadSafe aufgerufen
  19.05.11 ts  ResolveXConflict              TN5575   in Simulation muss TRUE zurückgegeben werden, sonst wird die Ressource blockiert
  27.02.13 wl                                TN6066   massive changes
  27.02.13 wl                                TN6066   ArmConflictManager aufgeteilt in XTrackConflictUnit, XTrackConflictManager und DevicesConflictManager
  24.04.13 ts  TXTrackConflictManager.Create TN6138   InitMode entfernt, Default aus Settings
  15.08.13 wl                                TN6223   uses geändert
  20.08.13 wl  CheckXConflict                TN6231   wenn RunFlow.IsRecoveryMovement gesetzt ist, gibt es bei blockierten Arm eine Exception
  -------------------------------------------------------------------------------------------------- }

unit XTrackConflictManager;


interface


uses
    Module,
    Generics.Collections,
    CommonTypes,
    IntfMotorDriver,
    TrackingSemaphore,
    ObjModul,
    IntfMotorDevice,
    XTrackConflictUnit,
    IntfArmDevice;

type
    TXConflictResolveMode = (xcNone, xcMoveAway, xcWait, xcMoveAwayCheckID);

    TXTrackConflictManager = class
    private const
        DBL_MOVE_NECESSARY_MARGIN_MM = 0.2; // in mm
    strict private
        fConflictUnits: TObjectList<TXTrackConflictUnit>;
        fTrackID: integer;
        fXConflictResolveMode: TXConflictResolveMode;
        fXConflictResolveHardMode: TXConflictResolveMode;
        fXConflictBufferMin_MM: TPosMM;
        fCriticalSection: TTrackingSemaphore;
        function GetDircetion(const aCurrent, aDest: TPosMM): TXMotorDirection;
        function ResolveXConflict(const aUsedConflictUnit, aOtherConflictUnit: TXTrackConflictUnit;
            aDirection: TXMotorDirection): boolean;
        function IsXConflict(aUsedXMotor, aOtherXMotor: IXMotorDevice; const aOtherXMotorPos: TPosMM;
            const aCurrent, aDest: TPosMM; out oOtherXMotorRequiredPos: TPosMM;
            aDirection: TXMotorDirection): boolean;
        function IsXConflictRight(aUsedXMotor, aOtherXMotor: IXMotorDevice; const aOtherXMotorPos: TPosMM;
            const aCurrent, aDest: TPosMM; out oOtherXMotorRequiredPos: TPosMM): boolean;
        function IsXConflictLeft(aUsedXMotor, aOtherXMotor: IXMotorDevice; const aOtherXMotorPos: TPosMM;
            const aCurrent, aDest: TPosMM; out oOtherXMotorRequiredPos: TPosMM): boolean;
        function IsAnyXConflict(aUsedConflictUnit: TXTrackConflictUnit; aUsedXMotor: IXMotorDevice;
            const aUsedByID: cardinal; const aCurrent, aDest: TPosMM;
            out oOtherConflictUnit: TXTrackConflictUnit; out oOtherXMotorRequiredPos: TPosMM;
            const aDirection: TXMotorDirection): boolean;

        class function CheckCurrentPosition(aUsedXMotor: IXMotorDevice; const aUsedByID: cardinal): TPosMM;
        procedure CheckXConflict(aSender: TObject; aUsedXMotor: IXMotorDevice; aDest: TPosMM;
            aIsInitMove: boolean);

        function CanUseXMotor(const aConflictUnit: TXTrackConflictUnit): boolean;
        procedure RegisterUseXMotor(const aConflictUnit: TXTrackConflictUnit; const aUsedByID: cardinal;
            const aDest: TPosMM; const aIsInitMove: boolean);
        function FindOrAddConflictUnit(aXMotorID: integer): TXTrackConflictUnit;
    public
        constructor Create(aTrackID: integer);
        destructor Destroy(); override;
        procedure UnRegisterUseXMotorAll();
        procedure ClearMotorUsageUsedByIDAll();
        procedure SetXConflictResolveMode(aMode: TXConflictResolveMode);
        procedure InitConflictUnits();
        procedure AddArm(aArmDevice: IArmDevice);
        property TrackID: integer read fTrackID;
    end;


implementation


uses
    SysUtils,
    Math,
    ThreadAPI,
    LogManager,
    AppSettings,
    IntfMotorBasedMotionDevice,
    ErrorManager,
    RunFlow;

{ TXTrackConflictManager }

constructor TXTrackConflictManager.Create(aTrackID: integer);
var
    xIniAccess: IWinlissyIniAccess;
begin
    inherited Create;

    fConflictUnits := TObjectList<TXTrackConflictUnit>.Create(true);
    fTrackID := aTrackID;

    xIniAccess := gCommonDll.CreateRobotIni; // "ROBOT"
    fXConflictResolveHardMode := xcNone;
    fCriticalSection := TThreadAPI.CreateSemaphore(1);
    fXConflictResolveHardMode := TXConflictResolveMode(xIniAccess.ReadInteger('XConflictResolve',
        'HardMode'));
    fXConflictResolveMode := fXConflictResolveHardMode;
    ClearMotorUsageUsedByIDAll();
    fXConflictBufferMin_MM := xIniAccess.ReadFloat('XConflictResolve', 'ConflictBuffer');
end;

destructor TXTrackConflictManager.Destroy();
begin
    FreeAndNil(fConflictUnits);
    FreeAndNil(fCriticalSection);
    inherited;
end;

procedure TXTrackConflictManager.InitConflictUnits;
var
    x: integer;
begin
    for x := 0 to fConflictUnits.Count - 1 do
        fConflictUnits[x].InitConflictUnit();
end;

procedure TXTrackConflictManager.SetXConflictResolveMode(aMode: TXConflictResolveMode);
begin
    if fXConflictResolveHardMode <> xcNone then
        EXIT;
    fXConflictResolveMode := aMode;
    ClearMotorUsageUsedByIDAll();
end;

procedure TXTrackConflictManager.ClearMotorUsageUsedByIDAll();
var
    x: integer;
begin
    for x := 0 to fConflictUnits.Count - 1 do
        fConflictUnits[x].ClearMotorUsageUsedByIDAll();
end;

procedure TXTrackConflictManager.AddArm(aArmDevice: IArmDevice);
var
    xXMotor: IXMotorDevice;
    xConflictUnit: TXTrackConflictUnit;
begin
    ASSERT(TModuleFinder.GetXMotor(aArmDevice, xXMotor));

    // ConflictUnit finden
    xConflictUnit := FindOrAddConflictUnit(xXMotor.MotorID);
    xConflictUnit.AddArm(aArmDevice);
end;

function TXTrackConflictManager.FindOrAddConflictUnit(aXMotorID: integer): TXTrackConflictUnit;
var
    x: integer;
    xNewConflictUnit: TXTrackConflictUnit;
begin
    // gibt es schon eine ConflictUnit mit dieser Track-ID?
    for x := 0 to fConflictUnits.Count - 1 do
    begin
        if (fConflictUnits[x].XMotorID = aXMotorID) then
        begin
            EXIT(fConflictUnits[x]);
        end;
    end;

    xNewConflictUnit := TXTrackConflictUnit.Create(aXMotorID, self.CheckXConflict);
    fConflictUnits.Add(xNewConflictUnit);
    EXIT(xNewConflictUnit);
end;

function TXTrackConflictManager.CanUseXMotor(const aConflictUnit: TXTrackConflictUnit): boolean;
begin
    fCriticalSection.Enter();
    try
        EXIT(aConflictUnit.CanUseXMotor())
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TXTrackConflictManager.RegisterUseXMotor(const aConflictUnit: TXTrackConflictUnit;
    const aUsedByID: cardinal; const aDest: TPosMM; const aIsInitMove: boolean);
begin
    fCriticalSection.Enter();
    try
        aConflictUnit.RegisterUseXMotor(aUsedByID, aDest, aIsInitMove);
    finally
        fCriticalSection.Leave();
    end;
end;

procedure TXTrackConflictManager.UnRegisterUseXMotorAll();
var
    xCurrentID: cardinal;
    x: integer;
begin
    xCurrentID := TThreadAPI.GetCurrentThreadID();

    fCriticalSection.Enter();
    try
        for x := 0 to fConflictUnits.Count - 1 do
            fConflictUnits[x].UnRegisterUseXMotorAll(xCurrentID);
    finally
        fCriticalSection.Leave();
    end;
end;

function TXTrackConflictManager.ResolveXConflict(const aUsedConflictUnit, aOtherConflictUnit
    : TXTrackConflictUnit; aDirection: TXMotorDirection): boolean;
var
    xCanUseXMotor: boolean;
begin
    if (fXConflictResolveMode = xcMoveAway) or (fXConflictResolveMode = xcMoveAwayCheckID) then
    begin
        xCanUseXMotor := true;
        if (fXConflictResolveMode = xcMoveAwayCheckID) then
        begin
            xCanUseXMotor := CanUseXMotor(aOtherConflictUnit);
        end;

        if xCanUseXMotor then
        begin
            gLogManager.LogF('XMotor [%s] : Movement blocked by other arm [%s]. Moving away other arm.',
                [aUsedConflictUnit.Name, aOtherConflictUnit.Name], false);
            aOtherConflictUnit.MoveAway(aDirection);
            EXIT(true);
        end;
    end;
    EXIT(false);
end;

function TXTrackConflictManager.GetDircetion(const aCurrent, aDest: TPosMM): TXMotorDirection;
begin
    if (aDest > aCurrent) then
        EXIT(xmhRight)
    else
        EXIT(xmhLeft);
end;

function TXTrackConflictManager.IsXConflict(aUsedXMotor, aOtherXMotor: IXMotorDevice;
    const aOtherXMotorPos, aCurrent, aDest: TPosMM; out oOtherXMotorRequiredPos: TPosMM;
    aDirection: TXMotorDirection): boolean;
begin
    case (aDirection) of
        xmhRight:
            EXIT(IsXConflictRight(aUsedXMotor, aOtherXMotor, aOtherXMotorPos, aCurrent, aDest,
                oOtherXMotorRequiredPos));

        xmhLeft:
            EXIT(IsXConflictLeft(aUsedXMotor, aOtherXMotor, aOtherXMotorPos, aCurrent, aDest,
                oOtherXMotorRequiredPos));
        else
            raise Exception.Create('Error Message');
    end;
end;

function TXTrackConflictManager.IsXConflictRight(aUsedXMotor, aOtherXMotor: IXMotorDevice;
    const aOtherXMotorPos: TPosMM; const aCurrent, aDest: TPosMM;
    out oOtherXMotorRequiredPos: TPosMM): boolean;
var
    xXConflictBuffer_MM: TPosMM;
begin
    ASSERT(aDest > aCurrent);

    if (aCurrent > aOtherXMotorPos) then
        EXIT(false); // other arm is closer to worldx=0 than self - no collision possible

    xXConflictBuffer_MM := Math.Max(fXConflictBufferMin_MM, aUsedXMotor.ConflictBufferPlus +
        aOtherXMotor.ConflictBufferMinus);

    oOtherXMotorRequiredPos := aDest + xXConflictBuffer_MM;
    EXIT(aOtherXMotorPos < oOtherXMotorRequiredPos);
end;

function TXTrackConflictManager.IsXConflictLeft(aUsedXMotor, aOtherXMotor: IXMotorDevice;
    const aOtherXMotorPos: TPosMM; const aCurrent, aDest: TPosMM;
    out oOtherXMotorRequiredPos: TPosMM): boolean;
var
    xXConflictBuffer_MM: TPosMM;
begin
    ASSERT(aDest < aCurrent);

    if (aOtherXMotorPos > aCurrent) then
        EXIT(false); // other arm is farther from worldx=0 than self - no collision possible

    xXConflictBuffer_MM := Math.Max(fXConflictBufferMin_MM, aUsedXMotor.ConflictBufferMinus +
        aOtherXMotor.ConflictBufferPlus);

    oOtherXMotorRequiredPos := aDest - xXConflictBuffer_MM;
    EXIT(aOtherXMotorPos > oOtherXMotorRequiredPos);
end;

class function TXTrackConflictManager.CheckCurrentPosition(aUsedXMotor: IXMotorDevice;
    const aUsedByID: cardinal): TPosMM;
begin
    // if another thread is using the other xmotor this thread is not even allowed to read position of other motor (for ZP02 Motors)
    if aUsedXMotor.MotorUsage.IsUsageAllowed(aUsedByID) then
        EXIT(aUsedXMotor.ReadCurrentPosThreadSafe)
    else
        EXIT(aUsedXMotor.CurrentPos);
end;

function TXTrackConflictManager.IsAnyXConflict(aUsedConflictUnit: TXTrackConflictUnit;
    aUsedXMotor: IXMotorDevice; const aUsedByID: cardinal; const aCurrent, aDest: TPosMM;
    out oOtherConflictUnit: TXTrackConflictUnit; out oOtherXMotorRequiredPos: TPosMM;
    const aDirection: TXMotorDirection): boolean;
var
    x: integer;
    xOtherXMotorCurrentPos: TPosMM;
    xOtherXMotor: IXMotorDevice;
begin
    // if the distance between destination and the current position is very small dont check for conflict
    // Note: this obviously covers the case where aDest = aCurrent
    if Abs(aDest - aCurrent) <= DBL_MOVE_NECESSARY_MARGIN_MM then
        EXIT(false);

    for x := 0 to fConflictUnits.Count - 1 do
    begin
        oOtherConflictUnit := fConflictUnits[x];
        xOtherXMotor := oOtherConflictUnit.FirstXMotor;

        // if other XMotor is really the same XMotor
        if (aUsedConflictUnit.XMotorID = oOtherConflictUnit.XMotorID) then
            CONTINUE;

        // Case 1: other motor is initializing
        if oOtherConflictUnit.IsInitMovePending then
            EXIT(true); // if the other motor is intializing, we have no idea where it is so assume worst case

        // Case 2: other motor is blocking the way at its current position
        xOtherXMotorCurrentPos := CheckCurrentPosition(oOtherConflictUnit.FirstXMotor, aUsedByID);
        if self.IsXConflict(aUsedXMotor, oOtherConflictUnit.FirstXMotor, xOtherXMotorCurrentPos, aCurrent,
            aDest, oOtherXMotorRequiredPos, aDirection) then
            EXIT(true);

        // Case 3: other motor will move to a position that will block the way
        if (oOtherConflictUnit.IsNonInitMovePending()) then
        begin
            if self.IsXConflict(aUsedXMotor, oOtherConflictUnit.FirstXMotor,
                oOtherConflictUnit.PendingDestPos, aCurrent, aDest, oOtherXMotorRequiredPos, aDirection) then
                EXIT(true);
        end;
    end;

    EXIT(false);
end;

procedure TXTrackConflictManager.CheckXConflict(aSender: TObject; aUsedXMotor: IXMotorDevice; aDest: TPosMM;
    aIsInitMove: boolean);
var
    xUsedConflictUnit: TXTrackConflictUnit;
    xCurrentPos: TPosMM;
    xOtherConflictUnit, xPreviousConflictUnit: TXTrackConflictUnit;
    xOtherXMotorRequiredPos: TPosMM;
    xConflictFound: boolean;
    xOtherXMotorLastUsedByID: cardinal;
    xCurrentID: cardinal;
    xDirection: TXMotorDirection;
    xErrorText: string;
begin
    Assert((aSender is TXTrackConflictUnit), 'No Conflict Unit');
    xUsedConflictUnit := (aSender as TXTrackConflictUnit);
    xPreviousConflictUnit := nil;

    xCurrentID := TThreadAPI.GetCurrentThreadID();

    xCurrentPos := aUsedXMotor.ReadCurrentPos();
    xDirection := self.GetDircetion(xCurrentPos, aDest);

    while true do
    begin
        if gErrorManager.IsGlobalErr() then
            EXIT;

        fCriticalSection.Enter();
        try
            xConflictFound := false;
            if not aIsInitMove then
            begin
                xConflictFound := IsAnyXConflict(xUsedConflictUnit, aUsedXMotor, xCurrentID, xCurrentPos,
                    aDest, xOtherConflictUnit, xOtherXMotorRequiredPos, xDirection);
            end;

            if not xConflictFound then
            begin
                RegisterUseXMotor(xUsedConflictUnit, xCurrentID, aDest, aIsInitMove);
                EXIT;
            end;

            if ResolveXConflict(xUsedConflictUnit, xOtherConflictUnit, xDirection) then
                EXIT;

            xOtherXMotorLastUsedByID := xOtherConflictUnit.LastUsedByID;
        finally
            fCriticalSection.Leave();
        end;

        if xOtherConflictUnit <> xPreviousConflictUnit then
        begin
            xErrorText := 'XMotor of ' + xUsedConflictUnit.Name + ': Movement blocked by other arm [' +
                xOtherConflictUnit.Name + ']';
            if (xOtherXMotorLastUsedByID > 0) then
                xErrorText := xErrorText + ' which was previously used by thread ' +
                    IntToStr(xOtherXMotorLastUsedByID);

            if (TRunFlow.Instance.IsRecoveryMovement) then
                raise ERecoveryMovementException.Create(xErrorText)
            else
                gLogManager.Log(xErrorText + ' Waiting ...', true);
        end;

        xPreviousConflictUnit := xOtherConflictUnit;

        gRunFlow.AppSleep(500);
    end;
end;


end.
