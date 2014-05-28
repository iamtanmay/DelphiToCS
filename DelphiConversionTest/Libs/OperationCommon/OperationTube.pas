{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Functions used by Operation units and by Actions to work transport tubes
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.03.06 pk                               TN3009   Gripping values changed from steps to mm
  30.05.06 wl  TTubeMoveOperation.MoveYShake  TN3027    Y-Wert wird jetzt richtig berechnet
  27.10.06 pk  MoveToTubeXY                 TN3386   New
  03.11.06 pk  MoveToTubeXY                          for TTubeMoveLocationMotionSystem to avoid comiler warning
  03.12.06 wl                               TN3243   alle ErrBox-Aufrufe durch ErrBoxSimple oder ErrBoxSerialIntf ersetzt
  31.01.07 wl  TTubeMoveMotorMotionSystem.MoveTubeToPos1      TN3532  Rotation-Wert = TMoveTubeData.RStart + aRotation (z.B. State1Rotaion bei Decapper)
  31.01.07 wl  TTubeMoveMotorMotionSystem.MoveTubeFromPos2    TN3532  Rotation-Wert = 0, wenn TMoveTubeData.RStart + aRotation vorher größer als 0 waren
  31.01.07 wl  TTubeMoveMotorMotionSystem.GetMoveTubeData     TN3532  benutzt Carrier.GetTubeMoveStruct()
  31.01.07 wl  TTubeMoveLocationMotionSystem.MoveTubeFromPos1 TN3532  benutzt GetTubeMoveStruct().HT_XStart_mm statt einfsch nur H_XStart
  31.01.07 wl  TTubeMoveLocationMotionSystem.MoveTubeToPos2   TN3532  benutzt GetTubeMoveStruct().HT_XStart_mm statt einfsch nur H_XStart
  14.02.07 pk  GetMoveTubeData                                TN3568  Bug: access violation when no R-Motor exists
  07.03.07 wl  TTubeMoveMotionSystem                          TN3620   --> MotionSystemTube
  09.11.07 pk                                                 TN3924   Steps changed to mm
  29.02.08 wl  SetZTravelTubes                                TN4032   Method name changed
  11.09.08 wl  TTubeMoveOperation.MoveZShake                  TN4174  Bugfix: statt ShakeZOffset wurde ShakeHeight benutzt
  25.09.08 wl                                                 TN4242    TRunstCall ersetzt TDllCall
  11.03.09 wl  TTubeMoveOperation.GetTube1                    TN4461    Anachronismus entfernt: Prüfung Tool diameter/Tube diameter
  13.03.09 wl  GetTubeMoveTo,GetTubeGrip                      TN4460    GetTube1 aufgeteilt in 2 Funktionen
  13.03.09 wl  PutTubeRelease,PutTubeMoveFrom                 TN4460    PutTube2 aufgeteilt in 2 Funktionen
  13.03.09 wl  MoveTubeToPosition,MoveTubeFromPosition        TN4460    neu: für neue Actions
  10.08.09 wl                                                 TN4702   Strings werden jetzt direkt geladen
  08.09.09 pk                                                 TN4753      uses ErrorMessage replaced by ErrorInfo
  19.10.11 wl  GetTube,PutTube                                TN5723   PaintTubePos hinzugefügt
  28.10.11 wl                                                 TN5725   an TRack.PaintTubePositions angepasst
  28.08.12 ts                                                 TN5943   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Tube
  06.06.13 wl                                                 TN6154   MoveTubeToPos: neuer Parameter IsGetTube
  29.10.13 wl  GetTube,PutTube                                TN6288.1 Logging verbessert
  -------------------------------------------------------------------------------------------------- }

unit OperationTube;


interface


uses
    AppTypes,
    CommonTypes,
    RackTypes,
    MotionSystemTravel,
    IntfSensorDevice,
    IntfGripDevice,
    MotionSystemTube,
    IntfZTravelManager,
    EventManager,
    Rack,
    Carrier;

type
    TTubeMoveOperation = class
    protected
        fGripper: IGripDevice;
        fMotion: TTubeMoveMotionSystem;
        fZTravelManager: IGripZTravelManager;
        procedure CalcGripOpen(var vGripOpen: TPosMM);
        procedure CalcGripClose(var vGripClose: TPosMM; aTubeY2: TPosMM);
        procedure MoveYShake();
        procedure MoveZShake();
        procedure ShakeAfterGetTube(aToolData: TTubeToolData);
        procedure ShakeAfterPutTube(aToolData: TTubeToolData);
        procedure CheckTubeWithGripSensor();
        procedure SetZTravelTool;
        procedure SetZTravelTubes(aTubeZ: TPosMM);
        procedure MoveTubeFromPos(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: integer;
            aUseStartPos: boolean; aTubeSensor: ISensorDevice; aTubeMustBeFound: boolean;
            aZSpeed, aZRamp: integer);
        procedure MoveTubeToPos(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: integer;
            aUseStartPos, aUseSafeMove, aIsGetTube: boolean; aTubeSensor: ISensorDevice;
            aTubeMustBeFound: boolean; aZSpeed, aZRamp: integer);
        procedure CheckTubeWithSensor(aTubeSensor: ISensorDevice; aTubeMustBeFound: boolean);
    public
        constructor Create(aMotion: TTubeMoveMotionSystem; aGripper: IGripDevice;
            aZTravelManager: IGripZTravelManager);

        procedure GetTube(aRackPos: TXRackPosition; aTOptions: TTubeOptions; aMTModes: TMoveTubeModes;
            aTubeSensor: ISensorDevice; aRotation: integer; aEvBeforeGet, aEvAfterGet: TRunstCall;
            aZSpeed, aZRamp: integer);
        procedure GetTubeMoveTo(aRackPos: TXRackPosition; aTOptions: TTubeOptions; aMTModes: TMoveTubeModes;
            aTubeSensor: ISensorDevice; aRotation: integer; aZSpeed, aZRamp: integer);
        procedure GetTubeGrip(aRackPos: TXRackPosition; aTOptions: TTubeOptions; aMTModes: TMoveTubeModes;
            aTubeSensor: ISensorDevice; aRotation: integer; aEvBeforeGet, aEvAfterGet: TRunstCall);
        procedure GetTubeMoveFrom(aRackPos: TXRackPosition; aTOptions: TTubeOptions; aMTModes: TMoveTubeModes;
            aTubeSensor: ISensorDevice; aRotation, aZSpeed, aZRamp: integer);

        procedure PutTube(aRackPos: TXRackPosition; aTOptions: TTubeOptions; aMTModes: TMoveTubeModes;
            aTubeSensor: ISensorDevice; aRotation: integer; aEvBeforePut, aEvAfterPut: TRunstCall;
            aZSpeed, aZRamp: integer);
        procedure PutTubeMoveTo(aRackPos: TXRackPosition; aTOptions: TTubeOptions; aMTModes: TMoveTubeModes;
            aTubeSensor: ISensorDevice; aRotation, aZSpeed, aZRamp: integer);
        procedure PutTubeRelease(aRackPos: TXRackPosition; aTOptions: TTubeOptions; aMTModes: TMoveTubeModes;
            aTubeSensor: ISensorDevice; aRotation: integer; aEvBeforePut, aEvAfterPut: TRunstCall);
        procedure PutTubeMoveFrom(aRackPos: TXRackPosition; aTOptions: TTubeOptions; aMTModes: TMoveTubeModes;
            aTubeSensor: ISensorDevice; aRotation, aZSpeed, aZRamp: integer);

        procedure MoveTubeFromPosition(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aRoute: TMoveFromPositionRoute; aAddZOffset: TPosMM);
        procedure MoveTubeToPosition(aRackPos: TXRackPosition; aUsePutOffset: boolean; aRotation: TPosMM;
            aRoute: TMoveToPositionRoute; aAddZOffset: TPosMM; aUseSafeMove, aIsGetTube: boolean;
            aZSpeed, aZRamp: integer);
    end;


implementation


uses
    SysUtils,
    Controls,
    ErrorMessageFactory,
    LogManager,
    RackWell,
    ErrorManager,
    RunFlow,
    GeometricClasses,
    SamGlobe,
    ErrorInfo,
    GeneralTypes;

{ TTubeMoveOperation }

constructor TTubeMoveOperation.Create(aMotion: TTubeMoveMotionSystem; aGripper: IGripDevice;
    aZTravelManager: IGripZTravelManager);
begin
    inherited Create();
    fMotion := aMotion;
    fGripper := aGripper;
    fZTravelManager := aZTravelManager;
end;

procedure TTubeMoveOperation.SetZTravelTool;
begin
    fZTravelManager.SetToTool;
end;

procedure TTubeMoveOperation.SetZTravelTubes(aTubeZ: TPosMM);
begin
    fZTravelManager.SetToTubeMove(aTubeZ);
end;

procedure TTubeMoveOperation.CalcGripOpen(var vGripOpen: TPosMM);
begin
    if vGripOpen = 0 then
        vGripOpen :=
            fGripper.Tool.VOpen { - Round((fGripper.Tool.VOpen - xCloseVarispan) * FToolVarispanFactor ) };
end;

procedure TTubeMoveOperation.CalcGripClose(var vGripClose: TPosMM; aTubeY2: TPosMM);
begin
    if vGripClose = 0 then
        vGripClose := fGripper.Tool.VOpen - (fGripper.Tool.TubeDY * 2) + (aTubeY2 * 2);
end;

procedure TTubeMoveOperation.CheckTubeWithSensor(aTubeSensor: ISensorDevice; aTubeMustBeFound: boolean);
var
    xButton: integer;
    xTubeInGripper: boolean;
    xSensorVal: TSensorValue;
begin
    xButton := mrRetry;
    while (aTubeSensor <> nil) and (not gErrorManager.IsGlobalErr) and (xButton = mrRetry) do
    begin
        xButton := mrNone;

        xSensorVal := aTubeSensor.AskValue();
        case xSensorVal of
            svDefault:
                xTubeInGripper := false;
            svNotDefault:
                xTubeInGripper := true;
            else
                xTubeInGripper := aTubeMustBeFound;
        end;

        // Error window if error is detected
        if (aTubeMustBeFound) and (not xTubeInGripper) then
            xButton := gErrorMessageFactory.ErrBoxSimple('Tube Movement: No Tube in gripper!',
                'Tube Sensor Error', eibAbortRetryIgnore);
        if (not aTubeMustBeFound) and (xTubeInGripper) then
            xButton := gErrorMessageFactory.ErrBoxSimple('Tube Movement: Tube is still in gripper!',
                'Tube Sensor Error', eibAbortRetryIgnore);

        if (xButton = mrNone) then
            gLogManager.Log('Sensor: Tube Check OK', true);

        if (xButton = mrAbort) then
            gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
    end;
end;

procedure TTubeMoveOperation.CheckTubeWithGripSensor();
begin
    if not fGripper.IsGripped() then
    begin
        gErrorMessageFactory.ErrBoxSimple(TLanguageString.Read('Gripper did not grip the object',
            'Greifer konnte das Objekt nicht greifen'), 'Tube Gripper check', eibAbortRetryIgnore);
        gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
        EXIT;
    end;
end;

procedure TTubeMoveOperation.MoveTubeToPos(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: integer; aUseStartPos, aUseSafeMove, aIsGetTube: boolean; aTubeSensor: ISensorDevice;
    aTubeMustBeFound: boolean; aZSpeed, aZRamp: integer);
begin
    // --------------------------------- Gehe zur Startposition (benutzt XStart-,YStart-,HStart-Offset)
    fMotion.MoveTubeToPos1(aRackPos, aUsePutOffset, aRotation, aUseStartPos, fGripper.Tool, 0);
    // ------------------------------------------------------------------- check tube with tube sensor
    if (gErrorManager.IsGlobalErr) and (aUseStartPos) and (aTubeSensor <> nil) then
        CheckTubeWithSensor(aTubeSensor, aTubeMustBeFound);
    // ------------------------------------------------------------ Fahre den Handler zur Greifposition
    fMotion.MoveTubeToPos2(aRackPos, aUsePutOffset, aRotation, aUseStartPos, aUseSafeMove, aIsGetTube,
        fGripper.Tool, 0, aZSpeed, aZRamp);
end;

procedure TTubeMoveOperation.MoveTubeFromPos(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: integer; aUseStartPos: boolean; aTubeSensor: ISensorDevice; aTubeMustBeFound: boolean;
    aZSpeed, aZRamp: integer);
begin
    // ------------------------------ Gehe zur Startposition (benutzt XStart-,YStart-,HStart-Offset)
    fMotion.MoveTubeFromPos1(aRackPos, aUsePutOffset, aRotation, aUseStartPos, fGripper.Tool, 0);
    // ----------------------------------------------------------------- check tube with tube sensor
    if (not gErrorManager.IsGlobalErr) and (aUseStartPos) and (aTubeSensor <> nil) then
        CheckTubeWithSensor(aTubeSensor, aTubeMustBeFound);
    // -------------------------------------------------------------- move to z-travel, rotation = 0
    fMotion.MoveTubeFromPos2(aRackPos, aUsePutOffset, aRotation, aUseStartPos, fGripper.Tool, 0,
        aZSpeed, aZRamp);

    CheckTubeWithGripSensor();
end;

procedure TTubeMoveOperation.MoveTubeToPosition(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aRoute: TMoveToPositionRoute; aAddZOffset: TPosMM; aUseSafeMove, aIsGetTube: boolean;
    aZSpeed, aZRamp: integer);
var
    xUseStartPos: boolean;
begin
    xUseStartPos := (aRoute <> mtpSkipStartPos);

    // Gehe zur Startposition (benutzt XStart-,YStart-,HStart-Offset)
    if (aRoute <> mtpToStartPosOnly) then
        fMotion.MoveTubeToPos1(aRackPos, aUsePutOffset, aRotation, xUseStartPos, fGripper.Tool, aAddZOffset);

    // Fahre den Handler zur Greifposition
    if (aRoute <> mtpFromStartPosToDestPos) then
        fMotion.MoveTubeToPos2(aRackPos, aUsePutOffset, aRotation, xUseStartPos, aUseSafeMove, aIsGetTube,
            fGripper.Tool, aAddZOffset, aZSpeed, aZRamp);
end;

procedure TTubeMoveOperation.MoveTubeFromPosition(aRackPos: TXRackPosition; aUsePutOffset: boolean;
    aRotation: TPosMM; aRoute: TMoveFromPositionRoute; aAddZOffset: TPosMM);
var
    xUseStartPos: boolean;
begin
    xUseStartPos := (aRoute <> mfpSkipStartPos);

    // Gehe zur Startposition (benutzt XStart-,YStart-,HStart-Offset)
    if (aRoute <> mfpToStartPosOnly) then
        fMotion.MoveTubeFromPos1(aRackPos, aUsePutOffset, aRotation, xUseStartPos, fGripper.Tool,
            aAddZOffset);

    // move to z-travel, rotation = 0
    if (aRoute <> mfpFromStartPosToApproach) then
        fMotion.MoveTubeFromPos2(aRackPos, aUsePutOffset, aRotation, xUseStartPos, fGripper.Tool,
            aAddZOffset, 0, 0);
end;

procedure TTubeMoveOperation.GetTube(aRackPos: TXRackPosition; aTOptions: TTubeOptions;
    aMTModes: TMoveTubeModes; aTubeSensor: ISensorDevice; aRotation: integer;
    aEvBeforeGet, aEvAfterGet: TRunstCall; aZSpeed, aZRamp: integer);
begin
    aRackPos.Rack.PaintTubePos(aRackPos.Pos, TRackWellDisplayType.TubeMove);

    if (not gErrorManager.IsGlobalErr) then
        GetTubeMoveTo(aRackPos, aTOptions, aMTModes, aTubeSensor, aRotation, 0, 0); // hinfahren
    if (not gErrorManager.IsGlobalErr) then
        GetTubeGrip(aRackPos, aTOptions, aMTModes, aTubeSensor, aRotation, aEvBeforeGet, aEvAfterGet);
    // zugreifen
    if (not gErrorManager.IsGlobalErr) then
        GetTubeMoveFrom(aRackPos, aTOptions, aMTModes, aTubeSensor, aRotation, aZSpeed, aZRamp);
    // mit dem Tube hochfahren
end;

procedure TTubeMoveOperation.PutTube(aRackPos: TXRackPosition; aTOptions: TTubeOptions;
    aMTModes: TMoveTubeModes; aTubeSensor: ISensorDevice; aRotation: integer;
    aEvBeforePut, aEvAfterPut: TRunstCall; aZSpeed, aZRamp: integer);
begin
    aRackPos.Rack.PaintTubePos(aRackPos.Pos, TRackWellDisplayType.TubeMove);

    if (not gErrorManager.IsGlobalErr) then
        PutTubeMoveTo(aRackPos, aTOptions, aMTModes, aTubeSensor, aRotation, aZSpeed, aZRamp);
    // mit dem Tube hinfahren
    if (not gErrorManager.IsGlobalErr) then
        PutTubeRelease(aRackPos, aTOptions, aMTModes, aTubeSensor, aRotation, aEvBeforePut, aEvAfterPut);
    // loslassen
    if (not gErrorManager.IsGlobalErr) then
        PutTubeMoveFrom(aRackPos, aTOptions, aMTModes, aTubeSensor, aRotation, 0, 0); // hochfahren
end;

procedure TTubeMoveOperation.GetTubeMoveTo(aRackPos: TXRackPosition; aTOptions: TTubeOptions;
    aMTModes: TMoveTubeModes; aTubeSensor: ISensorDevice; aRotation: integer; aZSpeed, aZRamp: integer);
var
    xOpenVarispan: TPosMM;
begin
    // fGripper.ResetDisabledErrors;
    if (not Assigned(aRackPos.Rack)) or (gErrorManager.IsGlobalErr) then
        exit;

    TLogManager.Instance.Log('Get Tube - Move to: ' + aRackPos.Rack.name + ', ' +
        IntToStr(aRackPos.Pos), true);

    // Greifer öffnen
    xOpenVarispan := aRackPos.Rack.RackStructure.TubeGetOpen_mm;
    CalcGripOpen(xOpenVarispan);
    fGripper.OpenGripper(xOpenVarispan, true);

    // Tube ZHöhe vom Tool Z-Travel Abziehen => TubeZTravel
    self.SetZTravelTubes(aRackPos.Rack.RackStructure.TubeZ_mm);

    // Fahre das Tool zur Start- und Greif-Position
    self.MoveTubeToPos(aRackPos, false, aRotation, (not(mtmNoStartPosBeforeGetTube in aMTModes)),
        (optTubeSafeMovement in aTOptions), true, aTubeSensor, false, aZSpeed, aZRamp);
end;

procedure TTubeMoveOperation.GetTubeGrip(aRackPos: TXRackPosition; aTOptions: TTubeOptions;
    aMTModes: TMoveTubeModes; aTubeSensor: ISensorDevice; aRotation: integer;
    aEvBeforeGet, aEvAfterGet: TRunstCall); // hinfahren und zugreifen
var
    xCloseVarispan: TPosMM;
begin
    // fGripper.ResetDisabledErrors;
    if (not Assigned(aRackPos.Rack)) or (gErrorManager.IsGlobalErr) then
        exit;

    TLogManager.Instance.Log('Get Tube - Grip: ' + aRackPos.Rack.name + ', ' + IntToStr(aRackPos.Pos), false);

    if Assigned(aEvBeforeGet) then
        aEvBeforeGet.Execute('before get tube');
    // ---------------------------------------------------------------------------- Schließe das Tool
    if (not gErrorManager.IsGlobalErr) then
    begin
        // gCommManager.LogF('Rack [%s] Position [%d] Get tube (open/close varispan in mm): %f/%f ZTravel steps: %d' ,
        // [aRackPos.Rack.Name,aRackPos.Pos, xOpenVarispan, xCloseVarispan, fMotion.GetWorldZTravel_Steps ], true );
        xCloseVarispan := aRackPos.Rack.RackStructure.TubeGetClose_mm;
        CalcGripClose(xCloseVarispan, aRackPos.Rack.RackStructure.TubeY2_mm);
        fGripper.CloseGripper(xCloseVarispan, false);
    end;
    // ----------------------------------------------------------------------- warte nach dem Greifen
    if (not gErrorManager.IsGlobalErr) and not(optTubeDoNotWait in aTOptions) then
        gRunFlow.Appsleep(800);

    if Assigned(aEvAfterGet) then
        aEvAfterGet.Execute('after get tube');
end;

procedure TTubeMoveOperation.GetTubeMoveFrom(aRackPos: TXRackPosition; aTOptions: TTubeOptions;
    aMTModes: TMoveTubeModes; aTubeSensor: ISensorDevice; aRotation, aZSpeed, aZRamp: integer);
// mit dem Tube hochfahren
begin
    // fGripper.ResetDisabledErrors;
    if (not Assigned(aRackPos.Rack)) or (gErrorManager.IsGlobalErr) then
        exit;

    TLogManager.Instance.Log('Get Tube - Move from: ' + aRackPos.Rack.name + ', ' +
        IntToStr(aRackPos.Pos), false);

    // --------------------------------------------------------------- benachbarte Tubes abschütteln
    if (optTubeShaking in aTOptions) then
        self.ShakeAfterGetTube(fGripper.Tool);
    // ----------------------------------------------------- Fahre das Tool zurück zur Start-Position
    self.MoveTubeFromPos(aRackPos, false, aRotation, (not(mtmNoStartPosAfterGetTube in aMTModes)),
        aTubeSensor, true, aZSpeed, aZRamp);
end;

procedure TTubeMoveOperation.PutTubeMoveTo(aRackPos: TXRackPosition; aTOptions: TTubeOptions;
    aMTModes: TMoveTubeModes; aTubeSensor: ISensorDevice; aRotation, aZSpeed, aZRamp: integer);
// mit dem Tube hinfahren
begin
    // fGripper.ResetDisabledErrors;
    if (not Assigned(aRackPos.Rack)) or (gErrorManager.IsGlobalErr) then
        exit;

    TLogManager.Instance.Log('Put Tube - Move to: ' + aRackPos.Rack.name + ', ' +
        IntToStr(aRackPos.Pos), true);

    // ------------------------------------------------- Fahre das Tool zur Start- und Greif-Position
    self.MoveTubeToPos(aRackPos, true, aRotation, (not(mtmNoStartPosBeforePutTube in aMTModes)),
        (optTubeSafeMovement in aTOptions), false, aTubeSensor, true, aZSpeed, aZRamp);
end;

procedure TTubeMoveOperation.PutTubeRelease(aRackPos: TXRackPosition; aTOptions: TTubeOptions;
    aMTModes: TMoveTubeModes; aTubeSensor: ISensorDevice; aRotation: integer; // loslassen und hochfahren
    aEvBeforePut, aEvAfterPut: TRunstCall);
var
    xOpenVarispan: TPosMM;
begin
    // fGripper.ResetDisabledErrors;
    if (not Assigned(aRackPos.Rack)) or (gErrorManager.IsGlobalErr) then
        exit;

    TLogManager.Instance.Log('Put Tube - Release: ' + aRackPos.Rack.name + ', ' +
        IntToStr(aRackPos.Pos), false);

    if Assigned(aEvBeforePut) then
        aEvBeforePut.Execute('before put tube');

    // ------------------------------------------------- Varispan berechnen
    xOpenVarispan := aRackPos.Rack.RackStructure.TubePutOpen_mm; // fester Wert aus Rackdaten
    CalcGripOpen(xOpenVarispan);
    xOpenVarispan := xOpenVarispan + fGripper.Tool.VDrop { +20 };
    // ------------------------------------------------------------------------------- Öffne das Tool
    if not gErrorManager.IsGlobalErr then
    begin
        // gCommManager.LogF('Rack [%s] Position [%d] Put tube (open grip in mm): %f ZTravel steps: %d' ,
        // [aRackPos.Rack.Name,aRackPos.Pos, xOpenVarispan, fMotion.GetWorldZTravel_Steps ], true );
        fGripper.OpenGripper(xOpenVarispan, true);
    end;
    // ------------------------------------------------------------------------ warte nach dem Öffnen
    if (not gErrorManager.IsGlobalErr) and not(optTubeDoNotWait in aTOptions) then
        gRunFlow.AppSleep(500);
    // ----------------------------------------------------------------------------- Tube abschüttlen
    if (optTubeShaking in aTOptions) then
        self.ShakeAfterPutTube(fGripper.Tool);

    // Event after put tube
    if Assigned(aEvAfterPut) then
        aEvAfterPut.Execute('after put tube');
end;

procedure TTubeMoveOperation.PutTubeMoveFrom(aRackPos: TXRackPosition; aTOptions: TTubeOptions;
    aMTModes: TMoveTubeModes; aTubeSensor: ISensorDevice; aRotation, aZSpeed, aZRamp: integer); // hochfahren
begin
    TLogManager.Instance.Log('Put Tube - Move from: ' + aRackPos.Rack.name + ', ' +
        IntToStr(aRackPos.Pos), false);

    // -------------------------------------------------------------------- Z-Travel auf Tool ZTravel
    self.SetZTravelTool;
    // ----------------------------------------------------- Fahre das Tool zurück zur Start-Position
    self.MoveTubeFromPos(aRackPos, true, aRotation, (not(mtmNoStartPosAfterPutTube in aMTModes)), aTubeSensor,
        false, aZSpeed, aZRamp);
end;

procedure TTubeMoveOperation.MoveYShake();
var
    xShakeOffset: TPosMM;
begin
    xShakeOffset := fGripper.Tool.ShakeYOffset;
    fMotion.MoveYShakeOnce(-(xShakeOffset / 2));
    fMotion.MoveYShakeOnce(xShakeOffset);
    fMotion.MoveYShakeOnce(-(xShakeOffset / 2));
end;

procedure TTubeMoveOperation.MoveZShake();
var
    xShakeOffset: TPosMM;
begin
    // langsam hochfahren
    fMotion.MoveZShakeOnce(-fGripper.Tool.ShakeHeight, true);

    // runter und hoch fahren
    xShakeOffset := fGripper.Tool.ShakeZOffset;
    fMotion.MoveZShakeOnce(xShakeOffset, false);
    fMotion.MoveZShakeOnce(-xShakeOffset, false);
end;

procedure TTubeMoveOperation.ShakeAfterGetTube(aToolData: TTubeToolData);
begin
    if (gErrorManager.IsGlobalErr) then
        exit;
    MoveZShake();
    MoveYShake();
end;

procedure TTubeMoveOperation.ShakeAfterPutTube(aToolData: TTubeToolData);
begin
    // ----------------------------------------------------------------------------- Tube abschüttlen
    MoveYShake();
end;


end.
