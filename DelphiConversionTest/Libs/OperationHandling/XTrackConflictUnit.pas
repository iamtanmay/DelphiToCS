{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  27.02.13 wl                                      TN6066   Initial Revision
  27.02.13 wl  AlignConflictBuffers                TN6100   Bei kombinierten Armen werden die ConflictBuffer-Einstellungen vereinheitlicht
  15.08.13 wl                                      TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------- }

unit XTrackConflictUnit;


interface


uses
    Generics.Collections,
    CommonTypes,
    Module,
    IntfMotorDriver,
    IntfMotorDevice,
    IntfArmDevice;

type
    TMoveXTrackConflictUnitEvent = procedure(aSender: TObject; aUsedXMotor: IXMotorDevice; aDest: double;
        aIsInitMove: boolean) of object;

    // Eine Conflict-Unit entspricht einem physikalischen Arm auf einer Schiene. Dieser Physikalische Arm kann
    // aber in den Devices als mehrere Arme definiert werden.
    TXTrackConflictUnit = class
    strict private
        fFirstArm: IArmDevice;
        fXMotors: TList<IXMotorDevice>;
        fXMotorID: integer;
        fOnBeforeMove: TMoveXTrackConflictUnitEvent;
        fMotorUsage: IMotorUsage;
        fArmMinusPos, fArmPlusPos: TPosMM;
        procedure XMoveDone(aUsedXMotor: IModule; aCurrentPos: TPosMM);
        procedure CheckXConflict(aUsedXMotor: IModule; aDest: TPosMM; aIsInitMove: boolean);

        procedure AssignEvents();
        function GetPendingDestPos: TPosMM;
        function GetLastUsedByID: cardinal;
        procedure GetArmBounds(aXMotor: IXMotorDevice; out oArmMinusPos, oArmPlusPos: TPosMM);
        procedure AlignConflictBuffers;
        function GetFirstXMotor: IXMotorDevice;
        function GetName: string;
    public
        constructor Create(aXMotorID: integer; aOnBeforeMove: TMoveXTrackConflictUnitEvent);
        destructor Destroy; override;
        procedure ClearMotorUsageUsedByIDAll();
        procedure UnRegisterUseXMotorAll(const aCurrentThreadID: cardinal);
        procedure InitConflictUnit();
        function IsInitMovePending: boolean;
        function IsNonInitMovePending(): boolean;
        procedure RegisterUseXMotor(const aUsedByID: cardinal; const aDest: TPosMM;
            const aIsInitMove: boolean);
        function CanUseXMotor(): boolean;
        procedure MoveAway(aDirection: TXMotorDirection);
        procedure AddArm(aArmDevice: IArmDevice);
        function GetConflictBufferMinus(aUsedXMotor: IXMotorDevice): TPosMM;
        function GetConflictBufferPlus(aUsedXMotor: IXMotorDevice): TPosMM;

        property name: string read GetName;
        property XMotorID: integer read fXMotorID;
        property PendingDestPos: TPosMM read GetPendingDestPos;
        property LastUsedByID: cardinal read GetLastUsedByID;
        property FirstXMotor: IXMotorDevice read GetFirstXMotor;
    end;


implementation


uses
    OperationFactory,
    OperationAxisMove,
    ObjModul,
    LogManager,
    ThreadAPI,
    SysUtils;

{ TXTrackConflictUnit }

constructor TXTrackConflictUnit.Create(aXMotorID: integer; aOnBeforeMove: TMoveXTrackConflictUnitEvent);
begin
    inherited Create;

    fXMotors := TList<IXMotorDevice>.Create;
    fXMotorID := aXMotorID;
    fOnBeforeMove := aOnBeforeMove;
    fFirstArm := nil;
end;

destructor TXTrackConflictUnit.Destroy;
begin
    FreeAndNil(fXMotors);
    inherited;
end;

procedure TXTrackConflictUnit.GetArmBounds(aXMotor: IXMotorDevice; out oArmMinusPos, oArmPlusPos: TPosMM);
begin
    oArmMinusPos := aXMotor.WorldOffset - aXMotor.ConflictBufferMinus;
    oArmPlusPos := aXMotor.ConflictBufferPlus + aXMotor.WorldOffset;
end;

function TXTrackConflictUnit.GetConflictBufferMinus(aUsedXMotor: IXMotorDevice): TPosMM;
begin
    EXIT(aUsedXMotor.ConflictBufferMinus);
end;

function TXTrackConflictUnit.GetConflictBufferPlus(aUsedXMotor: IXMotorDevice): TPosMM;
begin
    EXIT(aUsedXMotor.ConflictBufferMinus);
end;

function TXTrackConflictUnit.GetFirstXMotor: IXMotorDevice;
begin
    EXIT(fXMotors[0]);
end;

function TXTrackConflictUnit.GetLastUsedByID: cardinal;
begin
    EXIT(fMotorUsage.LastUsedByID);
end;

function TXTrackConflictUnit.GetName: string;
begin
    EXIT(fFirstArm.Name);
end;

procedure TXTrackConflictUnit.AlignConflictBuffers;
var
    x: integer;
    xCurrentArmMinusPos, xCurrentArmPlusPos: TPosMM;
begin
    if FXMotors.Count <= 1 then
        EXIT; // nur für kombinierte Arme

    // Ausmaße des Gesamt-Arms bestimmen
    for x := 0 to fXMotors.Count - 1 do
    begin
        GetArmBounds(fXMotors[x], xCurrentArmMinusPos, xCurrentArmPlusPos);
        if (xCurrentArmMinusPos < fArmMinusPos) then
            fArmMinusPos := xCurrentArmMinusPos;
        if (xCurrentArmPlusPos > fArmPlusPos) then
            fArmPlusPos := xCurrentArmPlusPos;
    end;

    // ConflictBuffer für jeden X-Motor neu bestimmen
    for x := 0 to fXMotors.Count - 1 do
    begin
        fXMotors[x].ConflictBufferMinus := fXMotors[x].WorldOffset - fArmMinusPos;
        fXMotors[x].ConflictBufferPlus := fArmPlusPos - fXMotors[x].WorldOffset;

        TLogManager.Instance.Log('Align conflict buffers [' + fXMotors[x].name + '] ConflictBufferMinus: ' +
            FloatToStr(fXMotors[x].ConflictBufferMinus) + ' mm, ConflictBufferPlus: ' +
            FloatToStr(fXMotors[x].ConflictBufferPlus) + ' mm', false);
    end;
end;

procedure TXTrackConflictUnit.InitConflictUnit;
begin
    // Events für die X-Motoren bestimmen
    AssignEvents();

    // Arm-Umfang bestimmen
    GetArmBounds(fXMotors[0], fArmMinusPos, fArmPlusPos);

    // Bei mehreren kombinierten Armen: Einstellungen angleichen
    AlignConflictBuffers;
end;

function TXTrackConflictUnit.IsInitMovePending: boolean;
begin
    EXIT(fMotorUsage.IsInitMovePending);
end;

function TXTrackConflictUnit.GetPendingDestPos: TPosMM;
begin
    EXIT(fMotorUsage.PendingDestPos);
end;

function TXTrackConflictUnit.IsNonInitMovePending(): boolean;
begin
    EXIT(fMotorUsage.IsNonInitMovePending);
end;

procedure TXTrackConflictUnit.RegisterUseXMotor(const aUsedByID: cardinal; const aDest: TPosMM;
    const aIsInitMove: boolean);
begin
    fMotorUsage.RegisterPendingMove(aUsedByID, aDest, aIsInitMove);
end;

procedure TXTrackConflictUnit.AddArm(aArmDevice: IArmDevice);
var
    xXMotor: IXMotorDevice;
begin
    ASSERT(TModuleFinder.GetXMotor(aArmDevice, xXMotor));

    // MotorUsage muss gleich sein (und damit auch der Motor-Driver)
    if Assigned(fMotorUsage) and (xXMotor.MotorUsage <> fMotorUsage) then
        raise Exception.Create('There is a x-Motor with the same ID but different drivers');

    // X-Motor, MotorUsage und Arm hinzufügen
    fXMotors.Add(xXMotor);
    if not Assigned(fMotorUsage) then
        fMotorUsage := xXMotor.MotorUsage;
    if not Assigned(fFirstArm) then
        fFirstArm := aArmDevice;
end;

procedure TXTrackConflictUnit.AssignEvents();
var
    x: integer;
begin
    for x := 0 to fXMotors.Count - 1 do
    begin
        fXMotors[x].OnBeforeMove := CheckXConflict;
        fXMotors[x].OnAfterMove := XMoveDone;
    end;
end;

function TXTrackConflictUnit.CanUseXMotor: boolean;
var
    xCurrentID: cardinal;
begin
    xCurrentID := TThreadAPI.GetCurrentThreadID();
    EXIT(fMotorUsage.IsUsageAllowed(xCurrentID));
end;

procedure TXTrackConflictUnit.CheckXConflict(aUsedXMotor: IModule; aDest: TPosMM; aIsInitMove: boolean);
begin
    if Assigned(fOnBeforeMove) then
        fOnBeforeMove(self, aUsedXMotor as IXMotorDevice, aDest, aIsInitMove);
end;

procedure TXTrackConflictUnit.ClearMotorUsageUsedByIDAll;
begin
    fMotorUsage.ClearUsedByID();
end;

procedure TXTrackConflictUnit.UnRegisterUseXMotorAll(const aCurrentThreadID: cardinal);
begin
    if fMotorUsage.IsLastUsedBy(aCurrentThreadID) then
    begin
        fMotorUsage.ClearPending();
        fMotorUsage.ClearUsedByID();
    end;
end;

procedure TXTrackConflictUnit.XMoveDone(aUsedXMotor: IModule; aCurrentPos: TPosMM);
var
    xUsedXMotor: IXMotorDevice;
begin
    Assert(SysUtils.Supports(aUsedXMotor, IXMotorDevice, xUsedXMotor), 'No X-Motor');
    xUsedXMotor.MotorUsage.PendingMoveDone(aCurrentPos);
end;

procedure TXTrackConflictUnit.MoveAway(aDirection: TXMotorDirection);
var
    xHomeMoveOp: THomeMoveOperation;
begin
    xHomeMoveOp := TOperationFactory.CreateHomeMoveOp(fFirstArm);
    xHomeMoveOp.MoveToHome(aDirection);
    xHomeMoveOp.Free;
end;


end.
