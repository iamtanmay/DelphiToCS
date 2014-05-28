{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : disposable tip handling functions
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  22.04.04 wl                               TN1788   initial version
  22.04.04 wl  gmDropAllDispTips            TN1788   von LiquidHandling hierher verschoben
  22.04.04 wl  gmPickDispTips,gmDropDispTips TN1788   von LiquidHandlingLow hierher verschoben
  28.04.04 wl  gmDropAllDispTips            TN1788   --> SubstanceHandling
  10.05.04 pk  gmGetDiTiPositions           TN1889.1 Use pass XRackPosition instead of RackName to gmDoRackMove
  22.06.04 wl  gmDropDispTips               TN2001   Vor dem Abwerfen muss aUsedArm.Tips.DitiLoaded zurückgesetzt werden sonst wird die Station falsch angefahren
  01.07.04 wl                               TN1963   Anpassung an Änderungen in TPipStep
  07.07.04 wl  alle Methoden                TN2019   Parameter: RobotArmDevice statt nur PipArmDevice
  06.08.04 wl  TDropPos,TCheckPos           TN2074   ersetzen Robot.DropPos, .DropXOfs und .CheckPos
  08.09.04 wl                               TN2121   gmSetTipError statt global_AppIntf.SetTipError
  11.09.04 wl  TDropPos,TCheckPos           TN2123   werden ersetzt durch TPipSingleStep
  11.09.04 wl  gmDropDispTips               TN2123   ermittelt Positionen für DropRack mit CalcSingelStepRack
  11.09.04 wl  alle Funktionen              TN2123   geänderte Aufrufe von MoveZ (MposArray statt MPos)
  22.09.04 wl  gmDropDispTips               TN2147   aUsedArm.DropDispTips muß vor .CalcSingleStepRack sein -> sonst falsche Z-Berechnung
  02.02.05 wl  gmFetchTips                  TN2297.6 Wenn tip am Handler -> kein BlockMove
  21.02.05 wl  alle Funktionen              TN1960   gCommManager.ErrBox & AppSleep statt global_AppInterface-Methode
  29.03.05 pk  gmFetchTips                  TN2346   Call SingleExecute because DoubleExecute ignores DisableZError after second execute
  03.06.05 wl  gmGetDiTiPositions           TN2436   aUsedArm.TipCount wird an Unterfunktion übergeben
  09.11.05 wl                               TN2728   Aufrufe von MoveXY mit geänderten Parametern
  17.11.05 wl  gmGetDiTiPositions           TN2771   Funktion auskommentiert, die einfach so Diti-Racks transportiert
  18.04.06 pk                               TN2958   Call StorePreviousXYMoveInfo
  18.04.06 pk  gmExecuteDropTips            TN2958   code which was commented out is active again
  27.04.06 pk                               TN2958   Robot.DTScanSpeed, DTScanRamp replaced by gDiTiScanSpeed and gDiTiScanRamp
  24.08.06 wl                               TN3271    bei den Aufrufen von gmCalculatePipSteps wird wieder PaintTubes übergeben
  04.12.06 wl                               TN3243   alle ErrBox-Aufrufe durch gmHandleDispTipError ersetzt
  07.12.06 wl                               TN3243    uses SamErr entfernt
  14.02.07 wl  gmDropDispTips               TN3147    geänderter Aufruf von gmCalculatePipStep, alles in try-finally-Blöcken
  19.02.07 wl                               TN3675    LiquidTips wird mit 3 neuen Parametern aufgerufen
  07.03.07 wl                               TN3620    ZMotorsHaveBlockMoveOption ersetzt Robot.IsSias bei BlockMove
  09.11.07 pk                               TN3924    Steps changed to mm
  09.11.07 pk                               TN3923    class references to TErrorMessageFactory changed to gErrorMessageFactory
  29.01.08 wl                               TN3980   uses geändert
  15.05.08 wl                               TN4100    TXYStepList property changed
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  27.11.08 wl  gmFetchTips                  TN3950    AT_BLOCK_MOVE heißt jetzt AT_BLOCK_MOVE2
  31.07.09 wl                               TN4018    geänderter Aufruf von DetectLiquid
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  08.09.09 pk                               TN4753    uses ErrorMessage replaced by ErrorInfo
  12.09.09 wl                               TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  21.09.09 pk  gmHandleDispTipError         TN4740   MaxTips replaced by TipCount
  24.11.09 ts  gmFetchTips                  TN4771   xXYSteps created in gmCalculatePipSteps will be freed
  29.03.11 wl                               TN5524   uses OperationPip entfernt
  20.09.11 wl                               TN5723   gmCalculatePipStep jetzt ohne Parameter
  28.09.11 wl                               TN5725   gmCalculatePipSteps jetzt ohne Parameter
  15.11.11 wl  gmPickDispTips,gmDropDispTips  TN5738    neu: Wenn eine Get/PutTipMethode angegeben ist, wird diese ausgeführt
  02.02.11 wl  alle Funktionen              TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  01.03.12 wl                               TN5822   uses geändert
  23.05.13 wl  gmPickDispTips               TN6153   SetPreviosZStep und StorePreviousXYMoveInfo werden nach MoveXY nicht mehr benötigt
  23.05.13 wl  gmExecuteDropTips            TN6153   das Drop-Rack wird beim Anfahren in XY automatisch gespeichert
  23.05.13 wl                               TN6153   geänderte Parameter bei MoveXY
  21.08.13 wl                               TN6231   uses geändert
  23.08.13 wl  DropAllDispTips              TN6233   von DeviceInitHandling hierher
  05.12.13 ts  gmFetchTips                  TN6321   GT_SINGLE_TIP enabled for fetching tips one by one
  09.01.14 ts  gmExecuteDropTips            TN6338   if there is a y-offset, it will be used
  -------------------------------------------------------------------------------------------------- }

unit LiquidHandlingDiti;


interface


uses
    AppTypes,
    IntfArmDevice;

type
    TLiquidHandlingDiTi = record
        class procedure DropAllDispTips; static;
    end;

procedure gmPickDispTips(aUsedArm: IArmDevice; aTips: TIPMAP);
procedure gmDropDispTips(aUsedArm: IArmDevice; aTips: TIPMAP; aDropMode: word; aUseCorridor: boolean);


implementation


uses
    Windows,
    Forms,
    Math,
    SysUtils,
    Classes,
    LogManager,
    ErrorManager,
    EventManager,
    SamGlobe,
    SamHigh,
    LiquidHandlingLow,
    CommonTypes,
    IntfMotorDriver,
    IntfPipDevice,
    IntfMotorDevice,
    RackWell,
    ObjModul,
    PlateHandlingLow,
    RackTypes,
    GUIManager,
    GeneralTypes,
    MotionSystemPipMove,
    OperationAxisMove,
    ErrorMessageFactory,
    ErrorInfo,
    MethodTypes,
    OperationFactory,
    RunFlow,
    TipMapUtils,
    LayoutManager,
    Layout,
    PeripheryManager,
    Rack,
    Carrier,
    DitiObserver;

const
    INT_DT_SCANMODE = 0;
    INT_DT_SCANSPEED = 0;

    // --------------------------------------------------------------------------------------------------
function gmHandleDispTipError(const aPipDevice: IPipDevice; aTipmap: integer; const aText, aCaption: string;
    aButtons: TErrorInfoButtons): integer;
// --------------------------------------------------------------------------------------------------
var
    xErrorInfo: TErrorInfo;
    x: integer;
    xTipmapText: string;
    xDelim: string;
begin
    xErrorInfo := TErrorInfo.Create();
    try
        xTipmapText := '';
        xDelim := '';
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if TTipMapUtils.TipSelected(aTipmap, x) then
            begin
                xTipmapText := xTipmapText + xDelim + 'Tip ' + IntToStr(x + 1);
                xDelim := ', ';
            end;
        end;

        xErrorInfo.Init(xTipmapText + ': ' + aText, aCaption, aButtons);
        xErrorInfo.AdditionalInfo.Add(xTipmapText);
        xErrorInfo.AdditionalInfo.Add(aText);

        result := gErrorMessageFactory.ErrBox(xErrorInfo);
    finally
        FreeAndNil(xErrorInfo);
    end;
end;

// --------------------------------------------------------------------------------------------------
// DROP DISPOSABLE TIPS
// --------------------------------------------------------------------------------------------------
procedure gmExecuteDropTips(aUsedArm: IArmDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
    aXYOp: TXYZTravelAxisPipMoveOperation; aTips: TIPMAP; aDropMode: integer; aUseCorridor: boolean;
    aCheckPosZ, aDropPosZ: TPipStepZPos; aCheckPosXYStep, aDropPosXYStep: TXYStep;
    aCheckPosRack, aDropPosRack: TRack);
// --------------------------------------------------------------------------------------------------
var
    xCheckPosDetLiqZ, xLPos: TDoubleArray;
    x, xRetry: integer;
    xErrTips: TIPMAP;
    xPipDevice: IPipDevice;
begin

    if (gErrorManager.IsGlobalErr) then
        exit;

    xPipDevice := aUsedArm.PipDevice;
    xCheckPosDetLiqZ := xPipDevice.GetNullDoubleArray;

    // Robot.WriteLog(Format('DropTips(%02hX,%02hX)', [aTips, aDropMode]), LOG_DEBUG, nil);
    if aCheckPosZ.SyrMap > 0 then
    begin
        for x := 0 to xPipDevice.TipCount - 1 do
            xCheckPosDetLiqZ[x] := aCheckPosZ.Z.ZMax[x]; // DL 27.02.98 Checken auf der CheckPosition
    end;

    if ((aDropMode and DT_REMOVE_LIQ) <> 0) then
    begin

        TLiquidHandlingLow.EmptyDilutor(aUsedArm, aTips); // vorher: EmptyTips(TipMap, 1); wl 24.09.03
    end;
    // DL 27.01.98 Fahren zur Drop Position verschoben, denn im Falle von check_before muß erst zur CheckPos gefahren werden

    // ***** Check for the existenz of all SyrMap we wnat to drop
    if ((aDropMode and DT_CHECK_BEFORE) <> 0) and Assigned(aCheckPosXYStep) and
        (not gErrorManager.IsGlobalErr) then
    begin
        aXYOp.MoveXY(aCheckPosXYStep, gmGetRackPosArrayForTips(xPipDevice, aCheckPosRack), aCheckPosZ,
            [xyoUseCorridor]); // DL 27.01.98 fahren zur checkpos

        // we have to check for loosed tips
        aTips := aTips and xPipDevice.OKTips;
        xErrTips := aTips;
        while (xErrTips <> 0) and (not gErrorManager.IsGlobalErr) do
        begin

            xRetry := 2; // Hardcodiert!
            while (xErrTips <> 0) and (xRetry > 0) do
            begin
                dec(xRetry);

                // check for available tips
                aZMotion.DetectLiquid(xErrTips, aCheckPosZ.Z.ZTravel, xCheckPosDetLiqZ, false,
                    INT_DT_SCANMODE, INT_DT_SCANSPEED); // DL 27.01.98  Detect Liq auf Checkpos ausgetauscht
                xErrTips := xErrTips and not aZMotion.LiquidTips(xErrTips, xLPos, true, aCheckPosZ.Z.ZTravel,
                    xCheckPosDetLiqZ);
            end;

            if (xErrTips <> 0) and ((aDropMode and DT_DISP_BEFORE) <> 0) then
            begin
                case gmHandleDispTipError(xPipDevice, xErrTips, TLanguageString.
                    Read('Drop Tips: Tips lost while pipetting!',
                    'Abwerfen der Spitzen: Während des Pipettierens wurden Spitzen verloren!'),
                    'Drop Tips error', eibAbortRetryIgnore) of
                    IDRETRY:
                        begin
                        end; // Retry check
                    IDABORT:
                        gErrorManager.SetGlobalErr(ERR_USER, '');
                    // mo 6.4.98 SE_TIPS_LOST|((long)ErrTips<<16);
                    else
                        xErrTips := 0; // assume the tip is available
                end;
            end
            else
                xErrTips := 0;
        end;
    end;

    while (aTips <> 0) and (not gErrorManager.IsGlobalErr) do
    begin // ***** Now remove tips
        aTips := aTips and xPipDevice.OKTips;
        xErrTips := aTips;
        if Assigned(aDropPosXYStep) then
        begin
            aXYOp.MoveXYWithOffset(aDropPosXYStep, aDropPosRack.Structure.MOffsetUpX_mm,
                aDropPosRack.Structure.MOffsetUpY_mm, gmGetRackPosArrayForTips(xPipDevice, aDropPosRack),
                aDropPosZ, [xyoUseCorridor]);
            // DL 27.01.98 fahren zur DropPos

            while (xErrTips <> 0) and (not gErrorManager.IsGlobalErr) do
            begin
                xRetry := 5; // Hardcodiert!
                while (xErrTips <> 0) and (xRetry > 0) do
                begin

                    dec(xRetry);
                    aZMotion.MoveZ(xErrTips, aDropPosZ.Z.ZMax, m_EXECUTE); // Move down to Start
                    if aDropPosRack.Structure.MOffsetUpX_mm <> 0 then
                        aXYOp.MoveX(aDropPosXYStep, 0, m_EXECUTE);
                    if aDropPosRack.Structure.MOffsetUpY_mm <> 0 then
                        aXYOp.MoveY(aDropPosXYStep, -aDropPosRack.Structure.MOffsetUpY_mm, m_EXECUTE);
                    if (aZMotion.ZMotorsHaveBlockMoveOption) then
                    begin
                        aZMotion.DisableZErrors(xErrTips);
                        // Disable Z-Robot.Move Errors of started Robot.Moves
                        aZMotion.MoveZ(xErrTips, aDropPosZ.Z.ZTravel, m_EXECUTE); // Move up to Start
                        xErrTips := xErrTips and aZMotion.ZErrorMotors(xErrTips);
                        // delete motors without error
                        if aDropPosRack.Structure.MOffsetUpX_mm <> 0 then
                            aXYOp.MoveX(aDropPosXYStep, aDropPosRack.Structure.MOffsetUpX_mm, m_EXECUTE);
                        if aDropPosRack.Structure.MOffsetUpY_mm <> 0 then
                            aXYOp.MoveY(aDropPosXYStep, aDropPosRack.Structure.MOffsetUpY_mm, m_EXECUTE);
                    end
                    else
                    begin
                        aZMotion.MoveZ(xErrTips, aDropPosZ.Z.ZTravel, m_EXECUTE); // Move up to Start
                        aZMotion.DisableZErrors(xErrTips); // Disable Z-Move Errors of started moves
                        if aDropPosRack.Structure.MOffsetUpX_mm <> 0 then
                            aXYOp.MoveX(aDropPosXYStep, aDropPosRack.Structure.MOffsetUpX_mm, m_EXECUTE);
                        if aDropPosRack.Structure.MOffsetUpY_mm <> 0 then
                            aXYOp.MoveY(aDropPosXYStep, aDropPosRack.Structure.MOffsetUpY_mm, m_EXECUTE);
                        xErrTips := xErrTips and aZMotion.ZErrorMotors(xErrTips);
                        // delete motors without error
                    end;

                end;

                if (xErrTips <> 0) and ((aDropMode and DT_DISP_ERROR) <> 0) then
                begin
                    // Display Drop Tip Error
                    case gmHandleDispTipError(xPipDevice, xErrTips, TLanguageString.
                        Read('Error at drop Tips: Unremovable tips!',
                        'Fehler beim Abwerfen der Pipettierspitzen: Spitzen sind nicht zu entfernen!'),
                        'Drop Tips error', eibAbortRetryIgnore) of
                        IDRETRY:
                            begin
                            end; // Retry dropping
                        IDABORT:
                            gErrorManager.SetGlobalErr(ERR_USER, '');
                        // mo 6.4.98 SE_DROP_ERROR|((long)ErrTips<<16);
                        else
                            xErrTips := 0; // Ignore (assume tip is removed)
                    end;
                end
                else
                    xErrTips := 0; // ignore possible error
            end;
        end;

        if ((aDropMode and DT_CHECK_AFTER) <> 0) and Assigned(aCheckPosXYStep) and
            (not gErrorManager.IsGlobalErr) then
        begin

            aXYOp.MoveXY(aCheckPosXYStep, gmGetRackPosArrayForTips(xPipDevice, aCheckPosRack), aCheckPosZ,
                [xyoUseCorridor]);
            // DL 27.01.98 Fahren zur CheckPosition
            aTips := aTips and aUsedArm.PipDevice.OKTips;
            xErrTips := aTips;

            xRetry := 2; // Hardcodiert!
            while (xErrTips <> 0) and (xRetry > 0) do
            begin
                dec(xRetry);
                // check for available tips
                aZMotion.DetectLiquid(xErrTips, aCheckPosZ.Z.ZTravel, xCheckPosDetLiqZ, false,
                    INT_DT_SCANMODE, INT_DT_SCANSPEED); // DL 27.01.98  DetectLiquid auf der CheckPosition
                xErrTips := xErrTips and aZMotion.LiquidTips(xErrTips, xLPos, true, aCheckPosZ.Z.ZTravel,
                    xCheckPosDetLiqZ);
            end;

            if (xErrTips <> 0) and ((aDropMode and DT_DISP_AFTER) <> 0) then
            begin
                // Display Error
                case gmHandleDispTipError(xPipDevice, xErrTips, TLanguageString.
                    Read('Drop Tips: The tips haven´t been removed!',
                    'Abwerfen der Spitzen: Die Spitzen wurden nicht entfernt!'), 'Drop Tips error',
                    eibAbortRetryIgnore) of
                    IDRETRY:
                        ; // Retry dropping
                    IDABORT:
                        gErrorManager.SetGlobalErr(ERR_USER, '');
                    // mo 6.4.98 SE_NOT_REMOVED|((long)ErrTips<<16);
                    else
                        xErrTips := 0; // Ignore (assume tip is removed)
                end;
            end
            else
                xErrTips := 0;
        end;
        aTips := aTips and xErrTips;
    end;

end;

procedure gmDropDispTips(aUsedArm: IArmDevice; aTips: TIPMAP; aDropMode: word; aUseCorridor: boolean);
var
    xDropRack: TRack;
    xDitiTypeExists: boolean;
    xCheckPosZDummy, xDropPosZ: TPipStepZPos;
    xDropPosXY: TXYStep;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xMethodParams: TKeyArgValueList;
    xMethod: string;
    xExactDropTipmap: TIPMAP;
begin
    xExactDropTipmap := aTips and aUsedArm.PipDevice.DropTipmap;

    // set back diti flag in Tip-Device
    aUsedArm.PipDevice.DropDispTips(aTips);

    // Drop Tips Method
    xMethod := aUsedArm.PipDevice.Tips[aUsedArm.PipDevice.FirstUsedTipIndex].MethodPutTip;
    if (xMethod <> '') then
    begin
        if (xExactDropTipmap > 0) then
        begin
            xMethodParams := TEventManager.CreateParamsForTipMethods(aUsedArm.PipDevice.name,
                xExactDropTipmap, aUsedArm.PipDevice.FirstUsedTipName);
            try
                TEventManager.Instance.ExecuteRunStartWithNameAndParams('Drop Tips Method', xMethod,
                    xMethodParams);
            finally
                xMethodParams.Free;
            end;
        end;
        EXIT;
    end;

    // is there a specific drop rack defined?
    xDropRack := TLayoutManager.Instance.CurrentLayout.GetDropRack(aUsedArm.PipDevice.FirstUsedTipDitiType,
        xDitiTypeExists);
    if (xDropRack = nil) or (not xDitiTypeExists) then
    begin
        gGUIManager.MessageBox(TLanguageString.Read('No drop rack defined for DiTi type DITI{0}',
            'Für den Wechselspitzentyp DITI{0} wurde kein Drop-Rack definiert',
            [aUsedArm.PipDevice.FirstUsedTipDitiType]), TLanguageString.Read('Drop tips can not be done',
            'Tips abwerfen nicht durchführbar'), 0);
        gErrorManager.SetGlobalErr(1, '');
        exit;
    end;

    // xCheckPosDummy := gmGetEmptyPipSingleStep;
    xCheckPosZDummy.SyrMap := 0;

    gLogManager.Log('Drop disposable tips - Rack-[' + xDropRack.Name + ']', true);

    // get drop (and check) position data
    xDropPosXY := gmCalculatePipStep(aUsedArm, aTips, gmGetRackPosArrayForTips(aUsedArm.PipDevice, xDropRack),
        xDropPosZ);
    if not Assigned(xDropPosXY) then
        EXIT;
    try
        // ASSERT(aTips = xFirstMotorMap, 'Drop station could not be reached with all tips');

        xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);
        xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);

        try
            // call drop tips function
            gmExecuteDropTips(aUsedArm, xZMotion, xXYOp, aTips, aDropMode, aUseCorridor, xCheckPosZDummy,
                xDropPosZ, nil, xDropPosXY, nil, xDropRack);

            // verhindern, dass nachfolgender Rack-Move als internes Movement ausgewertet wird
            // aUsedArm.RackMoveManager.ResetLastRack;
        finally
            FreeAndNil(xZMotion);
            FreeAndNil(xXYOp);
        end;
    finally
        FreeAndNil(xDropPosXY);
    end;
end;

class procedure TLiquidHandlingDiTi.DropAllDispTips;
var
    iList: TArray<string>;
    i: integer;
    xArm: IArmDevice;
    xPipDevice: IPipDevice;
    xSearchIndex: integer;
begin
    xSearchIndex := 0;

    while TPeripheryManager.Instance.FindNextModule(IArmDevice, xSearchIndex, xArm) do
    begin

        if not Assigned(xArm.PipDevice) then
            CONTINUE;
        if Assigned(xArm.GripDevice) then
            CONTINUE; // DiTi-Tips am Gripper bei Init nicht abwerfen

        xPipDevice := xArm.PipDevice;
        iList := xPipDevice.GetTipNames([DispTip]);

        for i := 0 to Length(iList) - 1 do
        begin
            if gErrorManager.IsGlobalErr() then
                EXIT;

            gLogManager.LogF('Arm %s - Drop Disposable Tips of Type: %s ', [xArm.Name, iList[i]], false);
            xPipDevice.SetUseTips(iList[i], 0, false);
            gmDropDispTips(xArm, xPipDevice.UseTips, DT_DISP_ERROR, false);
            xPipDevice.DropDispTips(xPipDevice.UseTips);
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------
// PICK DISPOSABLE TIPS
// --------------------------------------------------------------------------------------------------
function gmGetDiTiPositions(aUsedArm: IArmDevice; aTips: TipMap): TDiTiPositions;
var
    xOldRack: TRack; // war mal für automatischen Rackwechsel gedacht
begin
    gLogManager.Log('SamGetTipPositions', false);

    xOldRack := nil;
    result := TLayoutManager.Instance.CurrentLayout.GetDiTiPositions(aTips, aUsedArm.PipDevice.TipCount,
        aUsedArm.PipDevice.FirstUsedTipDitiType, gRunFlow.SimulationMode, xOldRack);

    if (xOldRack <> nil) then
    begin // Racks müssen getauscht werden
        // deaktiviert
    end;
end;

procedure gmDitiMoveZ_Scan(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem; aMotors: TIPMAP;
    const aDest: TDoubleArray);
var
    x: integer;
begin
    for x := 0 to aPipDevice.TipCount - 1 do
        if (((aMotors shr x) and 1) <> 0) then
            aZMotion.MoveSingleZ_Scan(x, aDest[x], m_NO_EXEC, 0, 0, 0); // Welcher Scanmode/Speed??

    aZMotion.Execute;
end;

procedure gmFetchTips(aUsedArm: IArmDevice; aTips: TIPMAP; aFilter, aGetMode: integer;
    aCheckPosZ, aDropPosZ: TPipStepZPos; aCheckPosXYStep, aDropPosXYStep: TXYStep;
    aCheckPosRack, aDropPosRack: TRack);
// Vor dem Aufruf muß auf die Sampler ZTravel Höhe gegangen werden,
// denn innehalb dieser Funktion wird die Rack Reisehöhe benutzt // DL 27.01.98
var
    xLPos: TDoubleArray;
    xTipsToGet, xErrTips, xBadTips, xTipsOk, xRemainingTips: TIPMAP;
    xMode, xRetry, xStep, xTipIndex: integer;
    xDTPos: TDiTiPositions;

    xXYSteps: TXYStepList;
    xPipDevice: IPipDevice;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xZSteps: TPipStepZPosArray;
begin
    xPipDevice := aUsedArm.PipDevice;
    if not(aUsedArm.PipDevice.FirstUsedTipType = DispTip) then
        exit;

    xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);
    xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);

    while (aTips <> 0) and (not gErrorManager.IsGlobalErr) do
    begin
        aTips := aTips and xPipDevice.OKTips;
        xTipsToGet := aTips;
        xRemainingTips := xTipsToGet;
        while ((xTipsToGet <> 0) or (xRemainingTips <> 0)) and (not gErrorManager.IsGlobalErr) do
        begin

            if ((aGetMode and GT_SINGLE_TIP) <> 0) then
            begin
                xTipIndex := TTipMapUtils.GetFirstSelectedTipIndex(xRemainingTips, xPipDevice.TipCount);
                xTipsToGet := TTipMapUtils.EmptyTipMap();
                TTipMapUtils.SelectTip(xTipsToGet, xTipIndex);
                TTipMapUtils.UnselectTip(xRemainingTips, xTipIndex);
            end
            else
                xRemainingTips := 0;

            xDTPos := gmGetDiTiPositions(aUsedArm, xTipsToGet); // Get available tip

            if (xDTPos.DiTiMap <= 0) then
                CONTINUE;

            // Now we get the available tips at the given positions
            TXRackPositionUtils.PaintTubePositions(xDTPos.DiTiRP, TRackWellDisplayType.default);
            gmCalculatePipSteps(aUsedArm, xDTPos.DiTiMap, xDTPos.DiTiRP, xXYSteps, xZSteps);

            for xStep := 0 to xXYSteps.Count - 1 do
            begin

                xZSteps[xStep].SyrMap := xZSteps[xStep].SyrMap and xPipDevice.OKTips;
                if (xZSteps[xStep].SyrMap <= 0) then
                    CONTINUE;

                // Move to step position
                xXYOp.MoveXY(xXYSteps[xStep], xDTPos.DiTiRP, xZSteps[xStep], []);

                // Get Tips from that position
                xBadTips := xZSteps[xStep].SyrMap;

                while (xBadTips <> 0) and (not gErrorManager.IsGlobalErr) do
                begin

                    xZMotion.MoveZ(xBadTips, xZSteps[xStep].Z.ZScan, m_EXECUTE);

                    if (xZMotion.ZMotorsHaveBlockMoveOption) then
                    begin
                        // aUsedArm.Execute Block Robot.Move (with disabled errors)
                        xZMotion.DisableZErrors(xBadTips);
                        // Disable Z-Robot.Move Errors of get tip Robot.Move
                        xZMotion.MoveZ(xBadTips, xZSteps[xStep].Z.ZMax, m_EXECUTE, AT_BLOCK_MOVE2,
                            gDiTiScanSpeed, gDiTiScanRamp); // Get Tip Move

                        // Get Tips
                        xTipsOk := xZMotion.GetOKTips_ZP02(xBadTips, xZSteps[xStep].Z.ZMax);

                        // Robot.Move back to start position
                        xZMotion.DisableZErrors(xBadTips);
                        xZMotion.MoveZ(xBadTips, xZSteps[xStep].Z.ZScan, m_EXECUTE, AT_MOVE_ABS,
                            gDiTiScanSpeed, gDiTiScanRamp); // out Robot.Move

                        xBadTips := xBadTips and not(xTipsOk); // we did not reach ZMax ==> Tip available
                    end
                    else
                    begin

                        // Get Tip Move
                        xZMotion.BlockMoveZ(xBadTips, xZSteps[xStep].Z.ZDisp, gDiTiScanSpeed);

                        xZMotion.DisableZErrors(xBadTips); // Disable Z-Move Errors of get tip move
                        xZMotion.MoveZ(xBadTips, xZSteps[xStep].Z.ZScan, m_NO_EXEC, AT_MOVE_ABS,
                            gDiTiScanSpeed); // out move
                        xZMotion.SingleExecute();
                        xBadTips := xBadTips and xZMotion.ZErrorMotors(xBadTips);
                        // delete motors without error
                        xZMotion.DisableZErrors(xBadTips); // Disable Z-Move Errors of out move
                    end;

                    if (xBadTips <> 0) then
                    begin
                        xZMotion.MoveZ(xBadTips, xZSteps[xStep].Z.ZTravel, m_EXECUTE, AT_MOVE_ABS,
                            gDiTiScanSpeed); // travel move
                        // unnötig: aUsedArm.DisableZErrors(xBadTips);	   // Disable Z-Move Errors of travel move

                        if ((aGetMode and GT_DISP_ERROR) <> 0) then
                            xMode := gmHandleDispTipError(xPipDevice, xBadTips,
                                TLanguageString.Read('Fetch Tips: No disposable tips got with Motors!',
                                'Aufnehmen der Spitzen: Es wurden keine Pipettierspitzen aufgenommen!'),
                                'Get tips error', eibAbortRetryIgnore)
                        else if ((aGetMode and GT_SKIP_ERROR) <> 0) then
                            xMode := 0 // Auto Skip
                        else if ((aGetMode and GT_RETRY_ERROR) <> 0) then
                            xMode := IDRETRY // Auto Retry
                        else
                            xMode := IDIGNORE; // Auto Ignore

                        case (xMode) of
                            IDIGNORE:
                                begin
                                    xBadTips := 0; // Ignore (assume tip is available)
                                    // gmSetTipError(xBadTips, TE_GET_TIPS_WARNING); // remember this error
                                end;
                            IDRETRY:
                                begin
                                end; // Retry getting at same position
                            IDABORT:
                                begin
                                    gErrorManager.SetGlobalErr(ERR_USER, '');
                                    // mo 6.4.98 GlobalError=SE_GET_ERROR|((long)BadTips<<16);
                                    // gmSetTipError(xBadTips, TE_GET_TIPS_ERROR);	// Tip Error
                                end;
                            else
                                begin
                                    xZSteps[xStep].SyrMap := xZSteps[xStep].SyrMap and not xBadTips;
                                    xBadTips := 0; // not taken, no retry
                                end;

                        end; // end case

                    end;

                end; // end while

                xTipsToGet := xTipsToGet and not xZSteps[xStep].SyrMap;
                // keep the bad tips to retry at another position

            end; // for xStep := 0 to xXYSteps.Count -1
            FreeAndNil(xXYSteps);
        end; // while(Tips to Get)

        // Now we have all needed tips for "TipMap" ==>
        // **** Check for tips are available and OK
        xBadTips := 0;
        if ((aGetMode and GT_CHECK_AFTER) <> 0) then
        begin
            if aCheckPosXYStep <> nil then
                xXYOp.MoveXY(aCheckPosXYStep, gmGetRackPosArrayForTips(xPipDevice, aCheckPosRack),
                    aCheckPosZ, []);

            aTips := aTips and xPipDevice.OKTips;
            xErrTips := aTips;

            while (xErrTips <> 0) and (not gErrorManager.IsGlobalErr) do
            begin

                xRetry := 2;

                while (xErrTips <> 0) and (xRetry > 0) do
                begin
                    dec(xRetry);

                    if (xZMotion.ZMotorsHaveBlockMoveOption) then
                    begin
                        // check for available tips (ignore any Errors)
                        xZMotion.DisableZErrors(xErrTips); // disable Z errors at move to ZStart
                        xZMotion.MoveZ(xErrTips, aCheckPosZ.Z.ZTravel, m_EXECUTE); // Move to start
                        xBadTips := xBadTips or xZMotion.ZErrorMotors(xErrTips);
                        // we assume blocked motors have bad tips

                        // scan into check position
                        xZMotion.DisableZErrors(xErrTips); // disable Z errors at scan move
                        gmDitiMoveZ_Scan(xPipDevice, xZMotion, xErrTips, aCheckPosZ.Z.ZMax); // Scan Move

                        // we assume blocked motors have bad tips
                        xBadTips := xBadTips or xZMotion.ZErrorMotors(xErrTips);

                        // ErrTips=all tips with no liquid found
                        xErrTips := xErrTips and not xZMotion.LiquidTips(xErrTips, xLPos, true,
                            aCheckPosZ.Z.ZTravel, aCheckPosZ.Z.ZMax);
                    end
                    else
                    begin
                        // check for available tips (ignore any Errors)
                        xZMotion.MoveZ(xErrTips, aCheckPosZ.Z.ZTravel, m_EXECUTE); // Move to start
                        xZMotion.DisableZErrors(xErrTips);

                        // Move to scan Position, recognize errors, and ignore scan errors
                        gmDitiMoveZ_Scan(xPipDevice, xZMotion, xErrTips, aCheckPosZ.Z.ZMax); // Scan Move

                        // we assume tips are bad if we got an error at start move
                        xBadTips := xZMotion.ZErrorMotors(xErrTips);
                        xZMotion.DisableZErrors(255);

                        // ErrTips=all tips with no liquid found
                        xErrTips := xErrTips and not xZMotion.LiquidTips(xErrTips, xLPos, true,
                            aCheckPosZ.Z.ZTravel, aCheckPosZ.Z.ZMax);

                        // Bad Tips= all tips with move errors
                        xBadTips := xBadTips or xZMotion.ZErrorMotors(xErrTips);
                    end;

                    xErrTips := xErrTips and not xBadTips; // BadTips not also Error Tips

                end;

                // ErrTips = Tips without tips, BadTips: Tips bad tips
                if (xBadTips <> 0) then
                begin
                    gmExecuteDropTips(aUsedArm, xZMotion, xXYOp, xBadTips, 0, false, aCheckPosZ, aDropPosZ,
                        aCheckPosXYStep, aDropPosXYStep, aCheckPosRack, aDropPosRack);
                    xErrTips := xErrTips or xBadTips;
                end; // remove bad Tips ==> tips also empty

                if (xErrTips <> 0) then
                begin

                    if ((aGetMode and GT_DISP_AFTER) <> 0) then
                        xMode := gmHandleDispTipError(xPipDevice, xErrTips,
                            TLanguageString.
                            Read('Fetch Tips: No disposable tips found at tips check position!',
                            'Aufnehmen der Spitzen: Bei der Kontrolle wurden keine Pipettierspitzen gefunden!'),
                            'Check tips error', eibAbortRetryIgnore)
                    else if ((aGetMode and GT_SKIP_AFTER) <> 0) then
                        xMode := 0 // Auto Skip
                    else if ((aGetMode and GT_RETRY_AFTER) <> 0) then
                        xMode := IDRETRY // Auto Retry
                    else
                        xMode := IDIGNORE; // Auto Ignore

                    case (xMode) of
                        IDIGNORE:
                            begin // Remember this Error and ignore for further actions
                                // gmSetTipError(xBadTips, TE_GET_TIPS_WARNING);
                                xErrTips := 0;
                                aTips := 0;
                            end;
                        IDRETRY:
                            begin // Retry getting tips (at other positions)
                                aTips := xErrTips; // DL 27.01.98  nur Tips holen, die nicht detektiert wurden
                                gmExecuteDropTips(aUsedArm, xZMotion, xXYOp, aTips, 0, false, aCheckPosZ,
                                    aDropPosZ, aCheckPosXYStep, aDropPosXYStep, aCheckPosRack, aDropPosRack);
                                xErrTips := 0;
                            end;
                        IDABORT:
                            gErrorManager.SetGlobalErr(ERR_USER, ''); // Abort Run (only from User)
                        else
                            begin // Remember this Error and skip tip usage for further actions
                                // gmSetTipError(xBadTips, TE_GET_TIPS_ERROR); // remember this error
                                // Robot.OKTips := xPipDevice.OKTips and not xErrTips;
                                xErrTips := 0;
                                aTips := 0; // DL 27.01.98  keine Tips mehr holen
                            end;
                    end;
                end
                else
                    aTips := aTips and xErrTips;
                // DL 27.01.98 kein Fehler: TipMap muß = 0 gesetzt werden, sonst werden immer wieder neue Spitzen geholt
            end;

        end
        else
            aTips := aTips and xTipsToGet;
    end;
end;

procedure gmPickDispTips(aUsedArm: IArmDevice; aTips: TIPMAP);
var
    xCheckPosZDummy, xDropPosZDummy: TPipStepZPos;
    xMethodParams: TKeyArgValueList;
    xGetTipMethod: string;
begin
    if (aUsedArm.PipDevice.FirstUsedTipType = DispTip) and
        ((aTips and aUsedArm.PipDevice.DropTipMap) <> aTips) then
    begin

        xCheckPosZDummy.SyrMap := 0;
        xDropPosZDummy.SyrMap := 0;
        xGetTipMethod := aUsedArm.PipDevice.Tips[aUsedArm.PipDevice.FirstUsedTipIndex].MethodGetTip;
        if (xGetTipMethod <> '') then
        begin
            xMethodParams := TEventManager.CreateParamsForTipMethods(aUsedArm.PipDevice.name, aTips,
                aUsedArm.PipDevice.FirstUsedTipName);
            try
                TEventManager.Instance.ExecuteRunStartWithNameAndParams('Get Tips Method', xGetTipMethod,
                    xMethodParams);
            finally
                xMethodParams.Free;
            end;
        end
        else
        begin
            gmFetchTips(aUsedArm, aTips, 0, GTErrorMode, xCheckPosZDummy, xDropPosZDummy, nil, nil, nil, nil);
        end;

        // Tips im PipDevice eintragen
        aUsedArm.PipDevice.FetchDispTips(aUsedArm.PipDevice.DropTipMap or aTips);
    end;
end;


end.
