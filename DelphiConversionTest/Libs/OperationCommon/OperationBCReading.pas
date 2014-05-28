{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Functions used by Operation units and by Actions to read barcodes
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  08.09.06 pk   ResetRotation               TN3294    No access violation if R motor is not assigned
  27.10.06 pk   TTubeBCReadingAndMoveOp     TN3368    New: Read barcode and move to position at the same time
  19.01.07 pk   TRackBCReadingOperation     TN3511    Create: accepts new parameter aRackMovable
  07.03.07 wl   TBCReadingMotionSystem      TN3620   --> MotionSystemBCReading
  12.04.07 pk   TBCReadingOperation         TN3664   fMotion changed to TBCReadingMotionSystem
  13.04.07 pk   ReadTubeBarcode             TN3666   MoveFromBCPos called at end instead of moveztravel
  13.04.07 pk   ReadBCandTurn               TN3666   MoveZ before aTurnDevice.Restore removed!!! Didn't see the point of this
  18.04.07 pk   ReadBcAndScan               TN3666   MoveTowardsBCPos called at beginning and MoveAwayBCPos called at end of function
  18.04.07 pk   ReadBCandTurn               TN3666   MoveAwayBCPos called at end of function instead of at end of ReadTubeBarcode
  18.04.07 pk   ReacBCandScan               TN3666   try finally so that MoveAwayBCPos is always called
  09.11.07 pk                               TN3924   Steps changed to mm
  29.01.08 wl                               TN3980   uses geändert
  22.04.08 pk                               TN4080   use ITubeBCReaderDevice, IRackBCReaderDevice
  13.03.09 wl  TTubeBCReadingAndMoveOperation.DoAfterFirstTriggerOn  TN4460   geänderte Parameter bei fTubeMotion.MoveToTubeXY
  12.09.09 wl                                                        TN4740   MPOS durch integer ersetzt
  17.12.09 pk   ReadTubeBarcode             TN4908   check BCPos.XDefined and YDefined instead of checking >= 0
  15.07.10 pk   ReacBCandScan               TN5194   New ScanSpeed, ScanRamp
  15.04.11 ts   ReadBCandTurn               TN5555   new Setting TurnAfterTriggerOn -> TurnDevice wird gleich zu Beginn mit gedreht
  19.04.11 ts   ReadBCandTurn               TN5555   new Setting ReturnBeforeGetBC  -> TurnDevice wird zurückgedreht, bevor GetLastRead ausgeführt wird
  19.04.11 ts   ReadBCandTurn               TN5555   cnt muss kleiner/gleich NoOfTurns sein
  09.09.11 ts   ReadTubeBarcode             TN5687   new Setting RetryRead -> Anzahl der Retries, TubeBCReader an Arm ohne TurnDevice
  29.03.12 wl   ReadTubeBarcode             TN5845   xReadingDev.SetGraphicsColor deaktiviert
  -------------------------------------------------------------------------------------------------- }

unit OperationBCReading;


interface


uses
    AppTypes,
    CommonTypes,
    RackTypes,
    Rack,
    IntfGripDevice,
    IntfMotorDevice,
    IntfBCTurntableDevice,
    IntfContainerBCReaderDevice,
    MotionSystemTube,
    MotionSystemBCReading;

type
    TBCReadingOperation = class
    protected
        fGripper: IGripDevice;
        fReadingDev: IContainerBCReaderDevice;
        fMotion: TBCReadingMotionSystem;
    public
        constructor Create(aMotion: TBCReadingMotionSystem; aGripper: IGripDevice;
            aReadingDev: IContainerBCReaderDevice);
        function ReadBcAndScan(aBCPos: BCRPOSITION; aCodeFilter, aRackTypeName: string): string;
    end;

    TRackBCReadingOperation = class(TBCReadingOperation)
    protected
        fRackMovable: boolean;
    public
        constructor Create(aMotion: TBCReadingMotionSystem; aGripper: IGripDevice;
            aReadingDev: IContainerBCReaderDevice; aRackMovable: boolean);
        function ReadRackBarcode(): string;
    end;

    TTubeBCReadingOperation = class(TBCReadingOperation)
    protected
        function VSpanSafetyStart(out oPos: integer; aTool: TTubeToolData): boolean;
        procedure VSpanSafetyFinish(aPos: integer);
        procedure DoAfterFirstTriggerOn(aTool: TTubeToolData); virtual;
    public
        function ReadBCandTurn(aTurnDevice: IBCTurntableDevice; aBCPos: BCRPOSITION; aTool: TTubeToolData;
            aCodeFilter, aRackTypeName: string; aNoOfTurns: integer;
            aTurnAfterTriggerOn, aReturnBeforeGetBC: boolean): string;
        function ReadTubeBarcode(aTool: TTubeToolData; aRackTypeName: string): string;
    end;

    TTubeBCReadingAndMoveOperation = class(TTubeBCReadingOperation)
    protected
        fTubeMotion: TTubeMoveMotionSystem;
        fDestPos: TXRackPosition;
        procedure DoAfterFirstTriggerOn(aTool: TTubeToolData); override;
    public
        constructor Create(aMotion: TBCReadingMotionSystem; aTubeMotion: TTubeMoveMotionSystem;
            aGripper: IGripDevice; aReadingDev: IContainerBCReaderDevice; aDestPos: TXRackPosition);
    end;


implementation


uses
    SysUtils,
    Controls,
    Graphics,
    ErrorManager,
    IntfMotorDriver,
    IntfMotionDevice;

{ TRackBCReadingOperation }

constructor TBCReadingOperation.Create(aMotion: TBCReadingMotionSystem; aGripper: IGripDevice;
    aReadingDev: IContainerBCReaderDevice);
begin
    inherited Create();
    fMotion := aMotion;
    fGripper := aGripper;
    fReadingDev := aReadingDev;
end;

function TBCReadingOperation.ReadBcAndScan(aBCPos: BCRPOSITION; aCodeFilter, aRackTypeName: string): string;
var
    xDelta, xNextDeltaRelativeToOrig, xPos, xScanSteps, xRange, xOrigPos: integer;
    xLastRotation: TPosMM;
    i, xNumTries, xNegative: integer;
    xMoveType: ACTION_TYPE;
    xCalcRelative: boolean;
    xDim: TMotorDimension;
    xSpeed, xRamp: integer;
begin
    result := '';
    fMotion.MoveTowardsBCPos(aBCPos);
    fMotion.ReadRotation(xLastRotation);
    try
        // if the position name is not empty then calculate a relative position
        xCalcRelative := aBCPos.PosName <> '';

        for xDim := mdZ downto mdX do
        begin // 0 = Z, 1 = Y, 2 = X

            if gErrorManager.IsGlobalErr() then
                Exit;
            fMotion.MoveToBCPos(aBCPos, false, true);
            result := fReadingDev.BCReader.LongRead(aCodeFilter);
            if (result <> '') or gErrorManager.IsGlobalErr() then
                Exit;

            // Scan in scan range (Z,Y,X)
            case xDim of
                mdZ:
                    begin // Z-Scan
                        xScanSteps := FReadingDev.BCScanRange.ZScanSteps;
                        xRange := FReadingDev.BCScanRange.ZScanRange;
                        xOrigPos := aBCPos.ZPos;
                        xSpeed := fReadingDev.ZScanSpeed;
                        xRamp := fReadingDev.ZScanRamp;
                    end;
                mdY:
                    begin // Y-Scan
                        xScanSteps := FReadingDev.BCScanRange.YScanSteps;
                        xRange := FReadingDev.BCScanRange.YScanRange;
                        xOrigPos := aBCPos.YPos;
                        xSpeed := fReadingDev.YScanSpeed;
                        xRamp := fReadingDev.YScanRamp;
                    end;
                else
                    begin // X-Scan
                        xScanSteps := FReadingDev.BCScanRange.XScanSteps;
                        xRange := FReadingDev.BCScanRange.XScanRange;
                        xOrigPos := aBCPos.XPos;
                        xSpeed := fReadingDev.XScanSpeed;
                        xRamp := fReadingDev.XScanRamp;
                    end;
            end;

            if ((xRange <= 0) or (xScanSteps <= 0)) then
                CONTINUE;
            xNumTries := (xRange div xScanSteps) * 2;
            xNegative := -1;
            xDelta := 0;
            for i := 0 to xNumTries - 1 do
            begin
                // ----------------------------------------------------- nächste bewegung vor der Kamera setzen

                // change direction for the next delta
                xNegative := (xNegative * -1);
                // calculate the next delta relative to zero
                xNextDeltaRelativeToOrig := (xNegative * ((i div 2) + 1) * xScanSteps);

                if xCalcRelative then
                begin
                    // here we'll have a relative movement
                    xDelta := xDelta * -1; // undo the last delta.  Now we are working relative to zero
                    xDelta := xDelta + xNextDeltaRelativeToOrig;
                    xPos := xDelta;
                    xMoveType := AT_MOVE_REL;
                end
                else
                begin
                    // here we'll have an absolute movement
                    xDelta := xNextDeltaRelativeToOrig;
                    xPos := xOrigPos + xDelta;
                    xMoveType := AT_MOVE_ABS;
                end;

                // ------------------------------------------------------------------------------ move to new position
                fMotion.MoveInDimension(xDim, xPos, m_EXECUTE, xMoveType, xSpeed, xRamp, true);
                // ------------------------------------------------------------------------------ barcode lesen
                result := fReadingDev.BCReader.LongRead(aCodeFilter);
                if (result <> '') or gErrorManager.IsGlobalErr() then
                    EXIT;
            end;
        end;

    finally
        // is the order here important? moveztravel and then resetrotation? why not reset rotation and then moveztravel?
        fMotion.MoveAwayBCPos(aBCPos);
        fMotion.ResetRotation(xLastRotation);
    end;
end;

{ TTubeBCReadingOperation }

function TTubeBCReadingOperation.VSpanSafetyStart(out oPos: integer; aTool: TTubeToolData): boolean;
// var
// xMotorGrip : IMotorGripDevice;
begin
    result := false;
    { TODO : either declare IMotorGripDevice in devicecommon or create a new function VSpanSafetyNeeded() in IGripDevice }
    // if not ( fGripper is IMotorGripDevice ) then EXIT;
    // xMotorGrip := ( fGripper as IMotorGripDevice );
    // if ( xMotorGrip.GMotor is TVSpanGMotorDevice ) then begin
    // //-------------------------------------------------------------------------------- Vspan sichern
    // result := true;
    // oPos := xMotorGrip.ReadCurrentGPos_Steps;
    // //--------------------------------------------------------------------------------- Vspan öffnen
    // fGripper.OpenGripper( aTool.VOpen + aTool.VDrop, true);
    // end;
end;

procedure TTubeBCReadingOperation.VSpanSafetyFinish(aPos: integer);
begin
    fGripper.CloseGripper(aPos, false);
end;

procedure TTubeBCReadingOperation.DoAfterFirstTriggerOn(aTool: TTubeToolData);
begin
end;

function TTubeBCReadingOperation.ReadBCandTurn(aTurnDevice: IBCTurntableDevice; aBCPos: BCRPOSITION;
    aTool: TTubeToolData; aCodeFilter, aRackTypeName: string; aNoOfTurns: integer;
    aTurnAfterTriggerOn, aReturnBeforeGetBC: boolean): string;
var
    cnt: integer;
    xIsVSpan: boolean;
    xVSpanPos: integer;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;
    if (aTurnDevice = nil) then
        EXIT;
    fMotion.MoveTowardsBCPos(aBCPos);
    fMotion.MoveToBCPos(aBCPos, true, false);
    // ----------------------------------------------------------------------------- TrigOn und lesen
    result := '';
    if not aTurnAfterTriggerOn then
    begin
        fReadingDev.BCReader.TriggerOn;
        DoAfterFirstTriggerOn(aTool);
        result := fReadingDev.BCReader.GetBarcode(aCodeFilter);
    end;
    if result = '' then
    begin
        xIsVSpan := VSpanSafetyStart(xVSpanPos, aTool);
        // --------------------------------------------------------------------------------- motor drehen
        cnt := 1;
        while (not gErrorManager.IsGlobalErr) and (result = '') and (cnt <= aNoOfTurns) do
        begin
            fReadingDev.BCReader.TriggerOn;
            aTurnDevice.Turn(cnt);
            if aReturnBeforeGetBC then
            begin
                aTurnDevice.Return;
                result := fReadingDev.BCReader.GetBarcode(aCodeFilter);
            end
            else
            begin
                result := fReadingDev.BCReader.GetBarcode(aCodeFilter);
                aTurnDevice.Return;
            end;
            inc(cnt);
        end;
        // ----------------------------------------------------------------- Vspan wieder schliessen
        if xIsVSpan then
            VSpanSafetyFinish(xVSpanPos);
    end;
    aTurnDevice.Restore;
    // move to ztravel or move backwards through approach path
    fMotion.MoveAwayBCPos(aBCPos);
end;

function TTubeBCReadingOperation.ReadTubeBarcode(aTool: TTubeToolData; aRackTypeName: string): string;
var
    xBCTubePosition: BCRPOSITION;
    xCodeFilter: string;
    xReadingDev: ITubeBCReaderDevice;
    xRetry: integer;
begin
    Supports(fReadingDev, ITubeBCReaderDevice, xReadingDev);
    xRetry := 0;
    xBCTubePosition := xReadingDev.GetBCTubePosition(aTool.Name, aRackTypeName);
    xCodeFilter := xReadingDev.GetCodeFilter(aTool.Name, aRackTypeName);
    // xReadingDev.SetGraphicsColor(clRed);

    if (xReadingDev.TurnDevice <> nil) then
    begin
        // Turntable exists
        result := ReadBCandTurn(xReadingDev.TurnDevice, xBCTubePosition, aTool, xCodeFilter, aRackTypeName,
            xReadingDev.NoOfTurns, xReadingDev.TurnAfterTriggerOn, xReadingDev.ReturnBeforeGetBC)
    end
    else
    begin
        if xBCTubePosition.XDefined and xBCTubePosition.YDefined then
        begin
            // Read BC and scan
            result := self.ReadBcAndScan(xBCTubePosition, xCodeFilter, aRackTypeName)
        end
        else
        begin
            while ((xRetry <= xReadingDev.RetryRead) and (result = '')) do
            begin
                // Barcode-Reader befindet sich am Handler - einfach nur lesen
                result := xReadingDev.BCReader.LongRead(xCodeFilter);
                Inc(xRetry);
            end;
            // move to ztravel or move backwards through approach path. I am not sure if this movetoztravel is needed here!?
            fMotion.MoveAwayBCPos(xBCTubePosition);
        end;
    end;

    xReadingDev.BCReader.ResetWithRead(xCodeFilter, result); // Barcodeleser ausschalten
    // why do we do resetwithread, why not just reset?
    // xReadingDev.SetGraphicsColor(clMaroon);

end;

{ TRackBCReadingOperation }

constructor TRackBCReadingOperation.Create(aMotion: TBCReadingMotionSystem; aGripper: IGripDevice;
    aReadingDev: IContainerBCReaderDevice; aRackMovable: boolean);
begin
    inherited Create(aMotion, aGripper, aReadingDev);
    fRackMovable := aRackMovable;
end;

function TRackBCReadingOperation.ReadRackBarcode(): string;
var
    xCodeFilter: string;
    xReadingDev: IRackBCReaderDevice;
begin
    Supports(fReadingDev, IRackBCReaderDevice, xReadingDev);

    result := '';
    if (fRackMovable) then
    begin
        // xReadingDev.SetGraphicsColor(clRed);

        xCodeFilter := xReadingDev.GetCodeFilter;

        // -------------------- Rack vor der Kamera verschieben (z-, y-, x- Richtung)
        result := self.ReadBcAndScan(xReadingDev.BCPosition, xCodeFilter, '');

        xReadingDev.BCReader.Reset;
        // xReadingDev.SetGraphicsColor(clMaroon);
    end;

    result := trim(result);

    if (xReadingDev.BCDelLastPos > 0) then
        result := Copy(result, 1, Length(result) - xReadingDev.BCDelLastPos);
end;

{ TTubeBCReadingAndMoveOperation }

constructor TTubeBCReadingAndMoveOperation.Create(aMotion: TBCReadingMotionSystem;
    aTubeMotion: TTubeMoveMotionSystem; aGripper: IGripDevice; aReadingDev: IContainerBCReaderDevice;
    aDestPos: TXRackPosition);
// This is only useful if the barcode reader is mounted on the arm.
// The barcodereading will be done during the movement to the next XY position
begin
    inherited Create(aMotion, aGripper, aReadingDev);
    fTubeMotion := aTubeMotion;
    fDestPos := aDestPos;
end;

procedure TTubeBCReadingAndMoveOperation.DoAfterFirstTriggerOn(aTool: TTubeToolData);
begin
    fTubeMotion.MoveToTubeXY(fDestPos, false, 0, true, aTool, 0);
end;


end.
