{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : motion system to read barcodes
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                  track-no improvement/change
  -------- --  -------------------------------------   -------- ------------------------------------
  07.03.07 wl                                          TN3620    MotorSystem classes moved from OperationBCReading to here
  12.04.07 pk  TBCReadingLocationMotionSystem          TN3664    New
  13.04.07 pk  TBCReadingLocationMotionSystem          TN3666    MovePos changed to MoveToBCPos. MoveZTravel changed to MoveFromBCPos
  13.04.07 pk  TBCReadingLocationMotionSystem          TN3666    MoveToBCPos: now also goes through approach path
  18.04.07 pk  TBCReadingLocationMotionSystem          TN3666    MoveTowardsBCPos: move approach path, MoveToBCPos: just move to final position
  18.04.07 pk  TBCReadingMotorMotionSystem             TN3666    ResetRotation: Movetoztravel removed. it will now be called in MoveAwayBCPos
  09.11.07 pk                                          TN3924    Steps to mm
  29.01.08 wl                                          TN3980   uses geändert
  12.09.09 wl                                          TN4740   MPOS durch integer ersetzt
  12.09.09 wl                                          TN4740   default-Werte aus interface entfernt
  17.12.09 pk  MoveToBCPos                             TN4908    check BCPos.Defined instead of checking >= 0
  30.06.10 pk  MoveInDimension                         TN5179    override was missing
  23.03.11 wl  MoveAwayBCPos                           TN5515   Methodenname geändert
  15.11.11 wl                                          TN5736   verwendet TXYZRMotorMotionSystem
  28.06.12 wl  MoveToBCPos                             TN5527   Aufruf von MoveDone für Y-Motor macht keinen Sinn - entfernt
  28.08.12 ts                                          TN5943   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Tube
  -------------------------------------------------------------------------------------------------- }

unit MotionSystemBCReading;


interface


uses
    AppTypes,
    CommonTypes,
    IntfMotorDriver,
    IntfMotionDevice,
    IntfMotorBasedMotionDevice,
    MotionSystemTravel;

type
    TBCReadingMotionSystem = class
        // procedure Execute(); virtual;
        procedure MoveTowardsBCPos(aBCPos: BCRPOSITION); virtual;
        procedure MoveToBCPos(aBCPos: BCRPOSITION; aMoveXYOnlyIfBothDefined: boolean;
            aMoveR: boolean); virtual;
        procedure MoveAwayBCPos(aBCPos: BCRPOSITION); virtual;
        procedure MoveInDimension(aMotorDimension: TMotorDimension; aPos: TPosMM; aExec: EXEC_MODE;
            aMode: ACTION_TYPE; aSpeed: integer; aRamp: integer; aClampAbsValue: boolean); virtual;
        function ReadRotation(out oPos: TPosMM): boolean; virtual;
        procedure ResetRotation(aPos: TPosMM); virtual;
    end;

    TBCReadingMotorMotionSystem = class(TBCReadingMotionSystem)
    strict private
        fMotors: TXYZRMotorMotionSystem;
        fZTravelMotion: TZAxisTravelMotorMotionSystem;
        // procedure Execute();
    public
        constructor Create(aMotors: TXYZRMotorMotionSystem; aZTravelMotion: TZAxisTravelMotorMotionSystem);
        destructor Destroy; override;
        procedure MoveToBCPos(aBCPos: BCRPOSITION; aMoveXYOnlyIfBothDefined: boolean;
            aMoveR: boolean); override;
        procedure MoveAwayBCPos(aBCPos: BCRPOSITION); override;
        procedure MoveInDimension(aMotorDimension: TMotorDimension; aPos: TPosMM; aExec: EXEC_MODE;
            aMode: ACTION_TYPE; aSpeed: integer; aRamp: integer; aClampAbsValue: boolean); override;
        function ReadRotation(out oPos: TPosMM): boolean; override;
        procedure ResetRotation(aPos: TPosMM); override;
    end;

    TBCReadingLocationMotionSystem = class(TBCReadingMotionSystem)
    strict private
        fMotionDevices: ILocationBasedMotionDevice;
    public
        constructor Create(aMotionDevices: ILocationBasedMotionDevice);

        procedure MoveTowardsBCPos(aBCPos: BCRPOSITION); override;
        procedure MoveToBCPos(aBCPos: BCRPOSITION; aMoveXYOnlyIfBothDefined: boolean;
            aMoveR: boolean); override;
        procedure MoveAwayBCPos(aBCPos: BCRPOSITION); override;
        procedure MoveInDimension(aMotorDimension: TMotorDimension; aPos: TPosMM; aExec: EXEC_MODE;
            aMode: ACTION_TYPE; aSpeed: integer; aRamp: integer; aClampAbsValue: boolean); override;
    end;


implementation


uses
    SysUtils,
    IntfMotorDevice,
    IntfLocationBasedAllAxesDriver;

{ TBCReadingMotionSystem }

procedure TBCReadingMotionSystem.MoveInDimension(aMotorDimension: TMotorDimension; aPos: TPosMM;
    aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed, aRamp: integer; aClampAbsValue: boolean);
begin

end;

procedure TBCReadingMotionSystem.MoveTowardsBCPos(aBCPos: BCRPOSITION);
begin
end;

procedure TBCReadingMotionSystem.MoveToBCPos(aBCPos: BCRPOSITION; aMoveXYOnlyIfBothDefined, aMoveR: boolean);
begin

end;

procedure TBCReadingMotionSystem.MoveAwayBCPos(aBCPos: BCRPOSITION);
begin

end;

function TBCReadingMotionSystem.ReadRotation(out oPos: TPosMM): boolean;
begin
    result := true;
end;

procedure TBCReadingMotionSystem.ResetRotation(aPos: TPosMM);
begin

end;

{ TBCReadingMotorMotionSystem }

constructor TBCReadingMotorMotionSystem.Create(aMotors: TXYZRMotorMotionSystem;
    aZTravelMotion: TZAxisTravelMotorMotionSystem);
begin
    inherited Create();
    fMotors := aMotors;
    fZTravelMotion := aZTravelMotion;
end;

destructor TBCReadingMotorMotionSystem.Destroy;
begin
    FreeAndNil(fMotors);
    inherited;
end;

procedure TBCReadingMotorMotionSystem.MoveAwayBCPos(aBCPos: BCRPOSITION);
begin
    fZTravelMotion.MoveToZTravelAllTips(0, 0);
end;

procedure TBCReadingMotorMotionSystem.MoveToBCPos(aBCPos: BCRPOSITION; aMoveXYOnlyIfBothDefined: boolean;
    aMoveR: boolean);
begin
    if (not aMoveXYOnlyIfBothDefined) or ((aBCPos.XDefined) and (aBCPos.YDefined)) then
    begin
        if aBCPos.XDefined then
        begin
            fMotors.MoveX(aBCPos.XPos, m_NO_EXEC);
        end;

        if aBCPos.YDefined then
        begin
            fMotors.MoveY(aBCPos.YPos, m_NO_EXEC);
        end;

        if (aBCPos.XDefined) or (aBCPos.YDefined) then
        begin
            fMotors.Execute();

            if aBCPos.XDefined and Assigned(fMotors.XMotor) then
                fMotors.XMotor.MoveDone();
        end;
    end;

    if aMoveR and aBCPos.RDefined then
        fMotors.MoveR(aBCPos.RPos, m_EXECUTE);

    if aBCPos.ZDefined then
        fMotors.MoveZ(aBCPos.ZPos, m_EXECUTE);
end;

function TBCReadingMotorMotionSystem.ReadRotation(out oPos: TPosMM): boolean;
begin
    result := true;
    oPos := fMotors.ReadR();
end;

procedure TBCReadingMotorMotionSystem.ResetRotation(aPos: TPosMM);
begin
    if (not Assigned(fMotors.RMotor)) or (aPos = fMotors.ReadR()) then
        EXIT;
    fMotors.MoveR(aPos, m_EXECUTE);
end;

procedure TBCReadingMotorMotionSystem.MoveInDimension(aMotorDimension: TMotorDimension; aPos: TPosMM;
    aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer; aRamp: integer; aClampAbsValue: boolean);

var
    xClampValue: boolean;

    function ClampValue(aPos: TPosMM; aMotor: IMotorDevice): TPosMM;
    begin
        result := aPos;
        if (not Assigned(aMotor)) or (not xClampValue) then
            EXIT;
        aMotor.SetToMinOrMaxPosIfBeyondMinOrMax(result);
    end;

begin
    xClampValue := ((aClampAbsValue) and (aMode = AT_MOVE_ABS));
    case aMotorDimension of
        mdZ:
            begin
                // aPos := ClampValue( aPos, fMotors.Z );
                fMotors.MoveZ(aPos, aExec, aMode, aSpeed, 0);
            end;
        mdY:
            begin
                // aPos := ClampValue( aPos, fMotors.Y );
                fMotors.MoveY(aPos, aExec, aMode, aSpeed, aRamp);
            end;
        mdX:
            begin
                aPos := ClampValue(aPos, fMotors.XMotor);
                fMotors.MoveX(aPos, aExec, aMode, aSpeed, 0);
            end;
    end;
end;

{ TBCReadingLocationMotionSystem }

constructor TBCReadingLocationMotionSystem.Create(aMotionDevices: ILocationBasedMotionDevice);
begin
    inherited Create();
    fMotionDevices := aMotionDevices;
end;

procedure TBCReadingLocationMotionSystem.MoveInDimension(aMotorDimension: TMotorDimension; aPos: TPosMM;
    aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed, aRamp: integer; aClampAbsValue: boolean);
begin
    fMotionDevices.MoveInDimension(aMotorDimension, aPos, aExec, aMode, aSpeed, aRamp, aClampAbsValue);
end;

procedure TBCReadingLocationMotionSystem.MoveTowardsBCPos(aBCPos: BCRPOSITION);
begin
    if aBCPos.PosName = '' then
        raise Exception.Create('Invalid barcode position');

    // Before V7.2.0 there was no approach path to barcode pos
    if fMotionDevices.DeviceGetApproachLocationExists(aBCPos.PosName) then
        fMotionDevices.DeviceApproachPath(aBCPos.PosName, pposAll, false);
end;

procedure TBCReadingLocationMotionSystem.MoveToBCPos(aBCPos: BCRPOSITION;
    aMoveXYOnlyIfBothDefined, aMoveR: boolean);
begin
    if aBCPos.PosName = '' then
        raise Exception.Create('Invalid barcode position');
    fMotionDevices.DeviceMoveToPos(aBCPos.PosName, INT_FIRST_POSITION);
end;

procedure TBCReadingLocationMotionSystem.MoveAwayBCPos(aBCPos: BCRPOSITION);
begin
    if aBCPos.PosName = '' then
        raise Exception.Create('Invalid barcode position');

    // Before V7.2.0 there was no approach path to barcode pos
    if fMotionDevices.DeviceGetApproachLocationExists(aBCPos.PosName) then
        fMotionDevices.DeviceApproachPath(aBCPos.PosName, pposAll, true);
end;


end.
