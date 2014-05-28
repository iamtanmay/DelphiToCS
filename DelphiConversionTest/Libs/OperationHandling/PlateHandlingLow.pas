{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl), Payman Kamali (pk)
  Description  : The only platehandling functions required by the Layouter
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.02.04 pk                                TN1719   initial version
  12.03.04 wl  gmRackMoveGet/Put             TN1812   GetPlate/PutPlate-Aufruf geändert: TRack wird übereben
  05.04.04 wl  gmRackMoveGet/Put             TN1788   gGrpArm.MoveToZTravel statt _MoveSamZTravel
  05.04.04 wl  gmRackMovable                 TN1788   GetRackData entfernt
  10.05.04 pk                               TN1889.1  All functions now use a TRack parameter instead of a rackname(string) parameter
  08.06.04 wl  gmRackMovable                 TN1963   Bedingung: GripperDevice muss existieren (vorher: HZMotor)
  15.06.04 wl  gmRackMoveGet/Put             TN1963   benutzt gmGetGripperArm statt gGrpArm
  28.06.04 pk  gmDoRackMove1                 TN2009.7 CreateVortRegThread from DevicesVortex
  26.07.04 pk  gmRackMovable                 TN1953   call isplatemoveable
  03.08.04 wl  gmDoRackMove1,gmRackMoveGet/Put  TN2063.1  neuer Parameter: aEventList:TSamplerEventlist
  08.11.04 wl  alle Funktionen               TN2213   Parameter: einzelne TDllCallExt statt TEventList
  08.03.05 pk                                TN2337   TDLLCallExt changed to TDLLCall
  17.11.05 wl                                TN2771   alle Funktionen die gGrpArm benutzt haben, haben Parameter aGripperArm
  23.02.06 thr gmCheckbalanceOpen            TN2941   Uses gModules.FindBalanceByCarrierName
  29.08.06 thr gmDoRackMove1                 TN3264   thrman für Vortexer aufrufen
  03.12.06 wl                                TN3243   alle ErrBox-Aufrufe durch ErrBoxSimple oder ErrBoxSerialIntf ersetzt
  07.12.06 wl                               TN3243    uses SamErr entfernt
  18.01.07 wl  gmDoRackMove1                 TN3507   einige Änderungen, da nicht mehr auf ObjModulA zugegriffen wird
  12.03.07 pk                                TN3629    new RackMoveOptions
  09.11.07 pk                               TN3923    class references to TErrorMessageFactory changed to gErrorMessageFactory
  31.01.08 wl  gmDoRackMove1                 TN4003   Automatismus entfernt, der nach dem Bewegen auf einen anderen Votexer diesen wieder anschaltet
  31.01.08 wl  gmDoRackMove1                 TN4003   Sicherheits-Automatismus, der Vortexer abschaltet, bevor Platte geholt wird, bleibt
  17.03.08 wl                                TN4043    uses IntfMixerDevice
  20.06.08 pk  gmRackMovePut,gmDoRackMove1   TN4139   Rack.SetSlot changed to Layout.MoveRack
  25.09.08 wl                                TN4242    TRunstCall ersetzt TDllCall
  13.02.09 wl  gmDoRackMove1                 TN4429    neu: InBetweeen-Event vor dem Zurückbringen
  10.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  08.09.09 pk                                TN4753    uses ErrorMessage replaced by ErrorInfo
  07.06.10 pk                                TN5077    Various changes needed for restart of rack moves
  07.09.12 ts                                TN5973    unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Rack
  01.07.13 wl  gmRackMovePut,gmDoRackMove1   TN6192   neue Parameter aXSpeedRack, aXRampRack, aYSpeedRack, aYRampRack
  21.08.13 wl                                TN6231   VortexerDisplay-Funktionen entfernt
  -------------------------------------------------------------------------------------------------- }

unit PlateHandlingLow;


interface


uses
    AppTypes,
    Rack,
    EventManager,
    IntfArmDevice;

procedure gmDoRackMove1(aUsedArm: IArmDevice; aRack: TRack; aDestination: TSlotStruct;
    aMoveOptions: TRackMoveOptions; aEvBeforeGet, aEvAfterGet, aEvInBetween, aEvBeforePut,
    aEvAfterPut: TRunstCall; aZSpeedRackUp, aZRampRackUp, aZSpeedRackDown, aZRampRackDown: integer;
    aXSpeedRack, aXRampRack, aYSpeedRack, aYRampRack: integer);
function gmRackMoveGet(aUsedArm: IArmDevice; aRack: TRack; aMoveZTravel: boolean; aPlateCheck: word;
    aMoveOptions: TRackMoveOptions; aEvBeforeGet: TRunstCall = nil; aEvAfterGet: TRunstCall = nil;
    aZSpeedRackUp: integer = 0; aZRampRackUp: integer = 0): boolean;
function gmRackMovePut(aUsedArm: IArmDevice; aRack: TRack; aNewSlot, aOldSlot: TSlotStruct;
    aMoveOptions: TRackMoveOptions; aEvBeforePut: TRunstCall = nil; aEvAfterPut: TRunstCall = nil;
    aZSpeedRackDown: integer = 0; aZRampRackDown: integer = 0; aXSpeedRack: integer = 0;
    aXRampRack: integer = 0; aYSpeedRack: integer = 0; aYRampRack: integer = 0): boolean;
function gmRackMovable(aUsedArm: IArmDevice; aRack: TRack): boolean;
function gmGetCurrentSlotStruct(const aRack: TRack): TSlotStruct;
function gmRackMovePutHandleError(const aRack: TRack; const aNewSlot: TSlotStruct): integer;
procedure gmAddRackMoveRunEffect(const aRack: TRack; const aNewSlot, aOldSlot: TSlotStruct);

procedure gmCheckBalanceOpen(aRack: TRack; aDestination: TSlotStruct);


implementation


uses
    windows,
    Forms,
    Sysutils,
    Controls,
    GeneralTypes,
    LogManager,
    ErrorManager,
    SamGlobe,
    ObjModul,
    ErrorInfo,
    IntfBalanceDevice,
    Device,
    AppSettings,
    OperationRack,
    OperationFactory,
    SamHigh,
    IntfMixerDevice,
    ErrorMessageFactory,
    LayoutManager,
    Layout,
    Carrier,
    CarrierSlot,
    TraceManager;

function gmIsBCMRack(aRack: TRack): boolean;
const
    STR_RACKNAME_BCM_SUFFIX = '.BCM';
begin
    result := Pos(STR_RACKNAME_BCM_SUFFIX, aRack.Name) > 0;
end;

// --------------------------------------------------------------------------------------------------
function gmRackMovable(aUsedArm: IArmDevice; aRack: TRack): boolean;
// --------------------------------------------------------------------------------------------------
// Kann das Rack mit dem Handler gegriffen werden ??
// --------------------------------------------------------------------------------------------------
var
    xRackOp: TRackMoveOperation;
begin
    result := false;
    if aRack = nil then
        Exit;
    if not Assigned(aUsedArm) then
        EXIT; // Handler existiert

    xRackOp := TOperationFactory.CreateRackMoveOp(aUsedArm, aRack);
    try
        if not xRackOp.IsPlateMovable() then
            EXIT;
    finally
        xRackOp.Free;
    end;
    if (gmIsBCMRack(aRack)) then
        EXIT; // Racks mit '.BCM' im Namen werden nicht gegriffen

    result := true;
end;

// --------------------------------------------------------------------------------------------------
function gmRackMoveGet(aUsedArm: IArmDevice; aRack: TRack; aMoveZTravel: boolean; aPlateCheck: word;
    aMoveOptions: TRackMoveOptions; aEvBeforeGet, aEvAfterGet: TRunstCall;
    aZSpeedRackUp, aZRampRackUp: integer): boolean;
// --------------------------------------------------------------------------------------------------
// Der Rückgabewert ist FALSE, wenn beim PlateCheck keine Platte gefunden wurde
// --------------------------------------------------------------------------------------------------
var
    xRackOp: TRackMoveOperation;
begin
    result := true;
    if (gErrorManager.IsGlobalErr) then
        exit;
    if not gmRackMovable(aUsedArm, aRack) then
        exit;

    // --------------------------------------------------------------- Rack greifen
    xRackOp := TOperationFactory.CreateRackMoveOp(aUsedArm, aRack);
    try
        xRackOp.GetPlate((aPlateCheck <> 0), aEvBeforeGet, aEvAfterGet, aMoveOptions, aZSpeedRackUp,
            aZRampRackUp);
    finally
        xRackOp.Free;
    end;
    gLogManager.Log('Get Plate [' + aRack.Name + ']', true);

    if (not gErrorManager.IsGlobalErr) and (aMoveZTravel) then
        gmMoveToZTravel(aUsedArm, aZSpeedRackUp, aZRampRackUp);
    Application.ProcessMessages;
end;

// --------------------------------------------------------------------------------------------------
procedure gmCheckBalanceOpen(aRack: TRack; aDestination: TSlotStruct);
// --------------------------------------------------------------------------------------------------
// If either the source, or destination carrier's name is the same as the Balance Carrier's name
// open the balance door
// --------------------------------------------------------------------------------------------------
var
    xBalanceDev: IBalanceDevice;
    xCarrier: TCarrier;
begin
    xCarrier := TLayout.GetCarrierOfRack(aRack);
    if not Assigned(xCarrier) then
        EXIT;

    xBalanceDev := gModules.FindBalanceByCarrierName(aDestination.CarrierName);
    if Assigned(xBalanceDev) then
        xBalanceDev.OpenDoor;

    if aDestination.CarrierName <> xCarrier.Name then
    begin
        xBalanceDev := gModules.FindBalanceByCarrierName(xCarrier.Name);
        if Assigned(xBalanceDev) then
            xBalanceDev.OpenDoor;
    end;
end;

procedure gmAddRackMoveRunEffect(const aRack: TRack; const aNewSlot, aOldSlot: TSlotStruct);
begin
    if SameText(aNewSlot.CarrierName, aOldSlot.CarrierName) and (aNewSlot.SlotNr = aOldSlot.SlotNr) and
        (aNewSlot.Rotation = aOldSlot.Rotation) then
    begin
        EXIT;
    end;

    TTraceManager.Instance.RackMoved(aRack.Name, aOldSlot.CarrierName, aOldSlot.SlotNr,
        TRack.GetRotationDegree(aOldSlot.Rotation));
end;

function gmGetCurrentSlotStruct(const aRack: TRack): TSlotStruct;
var
    xOldRotation: TRotationValue;
    xOldSlot: TCarrierSlot;
    xOldCarrier: TCarrier;
begin
    xOldSlot := aRack.Slot as TCarrierSlot;
    xOldRotation := aRack.RackRotation;

    xOldCarrier := xOldSlot.Carrier as TCarrier;
    result.SlotNr := xOldSlot.SlotNr;
    result.CarrierName := xOldCarrier.Name;
    result.Rotation := xOldRotation;
end;

function gmRackMovePutHandleError(const aRack: TRack; const aNewSlot: TSlotStruct): integer;
begin
    // 01.06.10 pk I removed the Retry option because I realized it would be really dangerous.  A lot of functions would have to be reimplemented
    result := gErrorMessageFactory.ErrBoxSimple(TLanguageString.
        Read('Error while putting rack{0} to carrier {1}, Slot {2}',
        'Fehler beim Abstellen von Racks {0} auf Carrier {1}, Slot {2}', [aRack.Name, aNewSlot.CarrierName,
        aNewSlot.SlotNr]), 'Wrong state detected', eibAbortIgnore);

    if (result = mrAbort) then
        gErrorManager.SetGlobalErr(ERR_USER, '');

end;

// --------------------------------------------------------------------------------------------------
function gmRackMovePut(aUsedArm: IArmDevice; aRack: TRack; aNewSlot, aOldSlot: TSlotStruct;
    aMoveOptions: TRackMoveOptions; aEvBeforePut, aEvAfterPut: TRunstCall;
    aZSpeedRackDown, aZRampRackDown: integer; aXSpeedRack, aXRampRack, aYSpeedRack,
    aYRampRack: integer): boolean;
// --------------------------------------------------------------------------------------------------
// - Der Rückgabewert ist nur FALSE, wenn ein Fehler auftrat und dieser mit
// 'RETRY' quittiert wurde.
// - Racks, die nicht 'movable' sind, werden trotzdem in der Layout.DB versetzt
// --------------------------------------------------------------------------------------------------
var
    xRackOp: TRackMoveOperation;
begin
    result := true;
    if (gErrorManager.IsGlobalErr) then
        Exit;

    // we have to do already set the destination slot here because the putplate function needs it
    TLayoutManager.Instance.CurrentLayout.MoveRack(aRack, aNewSlot.CarrierName, aNewSlot.SlotNr,
        aNewSlot.Rotation);

    // ------------------------ Platte zurück- oder auf neuen Carrierplatz fahren
    if not gmRackMovable(aUsedArm, aRack) then
        exit;
    xRackOp := TOperationFactory.CreateRackMoveOp(aUsedArm, aRack);
    try
        result := xRackOp.PutPlate((gPlateCheckPut <> 0), aEvBeforePut, aEvAfterPut, aMoveOptions,
            aZSpeedRackDown, aZRampRackDown, aXSpeedRack, aXRampRack, aYSpeedRack, aYRampRack);
    finally
        xRackOp.Free;
    end;

    gLogManager.Log(Format('Put Plate [%s] to Carrier %s Slot %d', [aRack.Name, aNewSlot.CarrierName,
        aNewSlot.SlotNr]), true);

    if (not gErrorManager.IsGlobalErr) then
        gmMoveToZTravel(aUsedArm);

end;

// --------------------------------------------------------------------------------------------------
procedure gmDoRackMove1(aUsedArm: IArmDevice; aRack: TRack; aDestination: TSlotStruct;
    aMoveOptions: TRackMoveOptions; aEvBeforeGet, aEvAfterGet, aEvInBetween, aEvBeforePut,
    aEvAfterPut: TRunstCall; aZSpeedRackUp, aZRampRackUp, aZSpeedRackDown, aZRampRackDown: integer;
    aXSpeedRack, aXRampRack, aYSpeedRack, aYRampRack: integer);
// --------------------------------------------------------------------------------------------------
// Move-Rack-Schleife, die bei jedem Fehler-Retry wiederholt wird
// --------------------------------------------------------------------------------------------------
var
    xDevice: IMixerDevice;
    xCarrier: TCarrier;
    xIsRackGetSuccess, xIsRackPutSuccess: boolean;
    xOldSlot: TSlotStruct;
    xErrorResult: integer;
begin
    gmCheckBalanceOpen(aRack, aDestination);

    { iSpeed := 0;
      iOnPulse := 0;
      iOffPulse := 0; }
    if gErrorManager.IsGlobalErr() then
        EXIT;

    // Ziel-Slot überprüfen
    xDevice := gModules.FindVortexer(aDestination.CarrierName);
    if Assigned(xDevice) then
    begin
        xDevice.WaitForAnswer();
        if gErrorManager.IsGlobalErr() then
            EXIT;

        // Ist der Destination-Carrier-Vortexer angeschaltet? -> sicherheitshalber abschalten!
        if (xDevice.Speed > 0) then
            xDevice.ResetSpeedAndOpenFixation();
    end;

    // Source-Slot überprüfen
    xCarrier := TLayout.GetCarrierOfRack(aRack);
    xDevice := gModules.FindVortexer(xCarrier.Name);
    if Assigned(xDevice) then
    begin
        xDevice.WaitForAnswer();
        if gErrorManager.IsGlobalErr() then
            EXIT;

        // wenn Source-Carrier-Vortexer läuft -> abschalten!
        { iSpeed := xDevice.Speed;
          iOnPulse := xDevice.OnPulse;
          iOffPulse := xDevice.OffPulse; }
        if (xDevice.Speed > 0) then
            xDevice.ResetSpeedAndOpenFixation();
    end;

    xOldSlot := gmGetCurrentSlotStruct(aRack);

    // ------------------------------------------------------------------ Platte Greifen und abstellen

    xIsRackGetSuccess := false;

    while true do
    begin
        if (gErrorManager.IsGlobalErr) then
            BREAK;

        xErrorResult := mrOK;
        // --------------------------------------------- Rack greifen und Plate Check
        xIsRackGetSuccess := gmRackMoveGet(aUsedArm, aRack, true, gPlateCheckGet, aMoveOptions, aEvBeforeGet,
            aEvAfterGet, aZSpeedRackUp, aZRampRackUp);
        if not xIsRackGetSuccess then
        begin

            xErrorResult := gErrorMessageFactory.ErrBoxSimple(TLanguageString.
                Read('Cannot find rack {0} in position {1}', 'Rack {0} nicht auf Position {1} gefunden.',
                [aRack.Name, aDestination.CarrierName]), 'Wrong state detected', eibAbortRetryIgnore);

            // --------------------------------------------------------- Fehlerauswertung
            if xErrorResult = mrRetry then
            begin
                CONTINUE;
            end;

            if (xErrorResult = mrAbort) then
            begin
                gErrorManager.SetGlobalErr(ERR_USER, '');
                BREAK;
            end;

            if (gErrorManager.IsGlobalErr) or (xErrorResult = mrIgnore) then
            begin
                // if ignore or error make sure we still move the rack in the layout because
                TLayoutManager.Instance.CurrentLayout.MoveRack(aRack, aDestination.CarrierName,
                    aDestination.SlotNr, aDestination.Rotation);
            end;

        end;

        // Event ausführen: In between
        if Assigned(aEvInBetween) then
            aEvInBetween.Execute('between get and put Rack');

        // ----------------------------------------- Rack an die Zielposition stellen
        if (not gErrorManager.IsGlobalErr) and (xErrorResult = mrOK) then
        begin
            xIsRackPutSuccess := gmRackMovePut(aUsedArm, aRack, aDestination, xOldSlot, aMoveOptions,
                aEvBeforePut, aEvAfterPut, aZSpeedRackDown, aZRampRackDown, aXSpeedRack, aXRampRack,
                aYSpeedRack, aYRampRack);

            if (not gErrorManager.IsGlobalErr) and (not xIsRackPutSuccess) then
            begin

                xErrorResult := gmRackMovePutHandleError(aRack, aDestination);
                // 01.06.10 pk retry removed for now.  a retry would try to move back to the source carrier and to a get, that would be dangerous if the rack was already in the gripper
                ASSERT(xErrorResult <> mrRetry, 'Retry not implemented');
            end;
            BREAK;
        end;

    end;

    if xIsRackGetSuccess then
    begin
        gmAddRackMoveRunEffect(aRack, aDestination, xOldSlot);
    end;

    // Vortexer des Zielcarriers anschalten
    { if ( iSpeed > 0 ) then begin
      xDevice := gModules.FindVortexer(aDestination.CarrierName);
      if Assigned( xDevice ) then begin
      xDevice.WaitForAnswer();
      if gErrorManager.IsGlobalErr() then EXIT;

      xDevice.SetPulseSpeed( iSpeed, iONPulse, iOFFPulse, false, false );
      end;
      end; }  // Automatismus, der nicht unbedigt gewollt ist!!

end;


end.
