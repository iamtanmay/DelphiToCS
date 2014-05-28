{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : motion system to initialize motors
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                  track-no improvement/change
  -------- --  -------------------------------------   -------- ------------------------------------
  07.03.07 wl                                          TN3620    MotorSystem classes moved from OperationAxisMove to here
  07.01.08 pk  TPipInitMotorMotionSystem.Init          TN3864    ZMotor Init commented out for now.
  29.01.08 wl                                          TN3980   uses geändert
  19.10.09 pk                                          TN4820   new InitID parameter
  25.03.11 wl  TGripInitMotorMotionSystem              TN5519   Initialisiert MultiYDevice statt nur Y-Motor
  25.02.13 wl  TInitMotorMotionSystem                  TN6097   Grip- und Piparm vereinheitlicht, MoveToInitOffset vor dem Initialisieren deaktiviert
  -------------------------------------------------------------------------------------------------- }

unit MotionSystemInit;


interface


uses
    IntfMotorDevice,
    Driver,
    IntfMultiYMotorDevice,
    IntfMultiZMotorDevice,
    IntfMotionDevice,
    IntfMotorBasedMotionDevice;

type
    TInitMotionSystem = class
    public
        procedure Init1(aInitID: TDevInitID); virtual; abstract;
        procedure Init2(aInitID: TDevInitID); virtual; abstract;
    end;

    TInitLocationMotionSystem = class(TInitMotionSystem)
    strict private
        fMotions: ILocationBasedMotionDevice;
    public
        constructor Create(aMotions: ILocationBasedMotionDevice);
        procedure Init1(aInitID: TDevInitID); override;
        procedure Init2(aInitID: TDevInitID); override;
    end;

    TInitMotorMotionSystem = class(TInitMotionSystem)
    strict private
        fX: IXMotorDevice;
        fY: IMultiYMotorDevice;
        fR: IRMotorDevice;
        fInitXBeforeY: boolean;
        fMoveToXInitOffsetBeforeInit: boolean;
        fMoveToYInitOffsetBeforeInit: boolean;
    public
        constructor Create(aX: IXMotorDevice; aY: IMultiYMotorDevice; aR: IRMotorDevice;
            aInitXBeforeY: boolean);
        procedure Init1(aInitID: TDevInitID); override;
        procedure Init2(aInitID: TDevInitID); override;
    end;


implementation


uses
    AppTypes,
    IntfMotorDriver;

{ TInitLocationMotionSystem }

constructor TInitLocationMotionSystem.Create(aMotions: ILocationBasedMotionDevice);
begin
    inherited Create();
    fMotions := aMotions;
end;

procedure TInitLocationMotionSystem.Init1(aInitID: TDevInitID);
begin
    fMotions.Init(aInitID);
end;

procedure TInitLocationMotionSystem.Init2(aInitID: TDevInitID);
begin
    // do nothing
end;

{ TGripInitMotorMotionSystem }

constructor TInitMotorMotionSystem.Create(aX: IXMotorDevice; aY: IMultiYMotorDevice; aR: IRMotorDevice;
    aInitXBeforeY: boolean);
begin
    inherited Create();
    fX := aX;
    fY := aY;
    fR := aR;
    fInitXBeforeY := aInitXBeforeY;

    // Man könnte das wieder als Parameter einführen (wenn es denn nötig sein sollte)
    fMoveToXInitOffsetBeforeInit := false;
    fMoveToYInitOffsetBeforeInit := false;
end;

procedure TInitMotorMotionSystem.Init1(aInitID: TDevInitID);
begin
    if Assigned(fX) and (fInitXBeforeY) then
        fX.Init(aInitID);

    if Assigned(fY) and (fMoveToYInitOffsetBeforeInit) then
        fY.MoveToInitOffset(m_Execute, AT_MOVE_ABS);

    if Assigned(fX) and (fMoveToXInitOffsetBeforeInit) then
        fX.MoveToInitOffset(m_EXECUTE, AT_MOVE_ABS);
end;

procedure TInitMotorMotionSystem.Init2(aInitID: TDevInitID);
begin
    if Assigned(fR) then
    begin
        fR.Init(aInitID);
        fR.MoveToInitOffset(m_Execute, AT_MOVE_ABS);
    end;

    if Assigned(fY) then
    begin
        fY.Init(aInitID);
        if (not fMoveToYInitOffsetBeforeInit) then
            fY.MoveToInitOffset(m_Execute, AT_MOVE_ABS);
    end;

    if Assigned(fX) and (not fInitXBeforeY) then
    begin
        fX.Init(aInitID);
        if (not fMoveToXInitOffsetBeforeInit) then
            fX.MoveToInitOffset(m_Execute, AT_MOVE_ABS);
    end;
end;


end.
