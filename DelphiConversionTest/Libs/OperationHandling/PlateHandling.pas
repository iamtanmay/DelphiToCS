{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Collection of all plate handling methods
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.08.03 wl                               TN1543   initial version
  30.08.03 wl  gmCheckRack                  TN1543   aus TBasicLiqThread.ExecuteAction extrahiert
  30.08.03 wl  alle anderen Funktionen      TN1543   von BasicThr hierher verschoben
  30.08.03 wl  gmCheckRack,gmDoRackMove1,gmRackMovePut  TN1543 SamErrBox statt MessageBox im Fehlerfall
  18.12.03 wl                               TN1672   uses geändert
  04.02.04 pk                               TN1719   functions moved into this unit from old Thread units( ExtLiq, BasicLig, etc)
  04.02.04 pk                               TN1719   Function required by Layouter -> PlateHandlingLow
  11.02.04 pk  gmReadWriteRackID            TN1719   bug fixed gmReadWriteRackID
  19.02.04 pk  gmReadWriteRackID            TN1719   result was not returned correctly -> fixed
  23.02.04 pk  gmAskRackID                  TN1719   is able to call SystemEvents again
  19.04.04 wl  gmGetRack                    TN1788   --> ObjWorkb
  29.04.04 wl  gmAskRackID                  TN1887   wenn gMTP_ShowNoRackButton = false, wird "No Rack"-Button nicht angezeigt
  10.05.04 pk                               TN1889.1  All functions now use a TRack parameter instead of a rackname(string) parameter
  25.05.04 pk  gmDoRackMove                 TN1889.1  gmCheckNewSlot was commented out by mistake
  22.06.04 pk  gmReadAllBarcodesInLayout    TN2002    uses WB object instead of Layout Table
  28.06.04 pk  gmReadWriteRackID            TN2009.5  uses SyncSetRackColor
  03.08.04 wl  gmCheckRack,gmReadRackID,gmReadWriteRackID,gmDoRackMove,gmReadAndCompareRackID  TN2063.1  neuer Parameter: aEventList:TSamplerEventlist
  03.08.04 wl  gmReadRackID                 TN2063.1  Events eingefügt (STR_EV_BEFORE_READBC, _AFTER_READBC)
  04.08.04 pk  gmReadWriteRackID            TN2080   if roAllowNonUniqueBC in RackOptions dont check for unique rack barcode
  08.11.04 wl  alle Funktionen              TN2213   Parameter: einzelne TDllCallExt statt TEventList
  11.03.05 pk  gmAskRackID                  TN2339.2 Call BarcodeDlg via GUIManager
  15.06.05 wl  gmCheckRack,gmReadWriteRackID  TN2465    durch TMethodGUIParser.RackActionParseSlot wird auch die neue Syntax unterstützt
  17.11.05 wl                               TN2771   alle Funktionen die gGrpArm benutzt haben, haben Parameter aGripperArm
  30.11.05 wl                               TN2815   Ressource-Nummer geändert
  31.10.06 pk  gmAskRackID, gmCheckRack     TN3391   SystemEvent calls moved to GUIManager
  03.12.06 wl                               TN3243   alle ErrBox-Aufrufe durch ErrBoxSimple oder ErrBoxSerialIntf ersetzt
  07.12.06 wl                               TN3243    uses SamErr entfernt
  12.03.07 pk                               TN3629    new RackMoveOptions
  09.11.07 pk                               TN3923    class references to TErrorMessageFactory changed to gErrorMessageFactory
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  20.06.08 pk                               TN4139    gmCheckReadRackIDInCurrentLayout: deactivated
  02.07.08 pk                               TN4139    calls CurrentLayout.ChangeRackID  instead of Rack.ChangeRackID
  11.07.08 wl                                         TN4164   TActionModules wird ohne "gModules as" aufgerufen
  02.09.08 pk                               TN4215    DllCallExt changed to DllCall
  25.09.08 wl                               TN4242    TRunstCall ersetzt TDllCall
  13.02.09 wl  gmReadWriteRackID            TN4429    neu: InBetweeen-Event nach dem Lesen, vor dem Zurückbringen
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  08.09.09 pk                               TN4753    uses ErrorMessage replaced by ErrorInfo
  15.12.09 ts  gmAskRackID/ReadWriteRackID  TN4945    Barcode_PromptInput reimplemented, ask for BC if roReadBCManually in RackOptions
  07.06.10 pk                               TN5077    Various changes needed for restart of rack moves
  28.10.10 wl  gmAskRackID                  TN5312    TRack.Highlight wird benutzt um Rack zu zeigen
  28.10.10 wl                               TN5312    alle Aufrufe von TRack.SetRackColor entfernt
  25.01.11 wl  gmCheckNewSlot               TN3984    Call ErrBoxSimple before setting global error
  09.04.11 wl  gmReadBarcodeRack            TN5545    sucht nach aUsedArm.RackBCReaderDevice
  07.09.12 ts                               TN5973    unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Rack
  01.10.12 wl                               TN5973    SetStatusBar durch Log ersetzt
  01.07.13 wl  gmDoRackMove                 TN6192    neue Parameter aXSpeedRack, aXRampRack, aYSpeedRack, aYRampRack
  30.07.13 wl                               TN6160   uses geändert
  18.09.13 wl  BCRackLeadZero,ShowNoRackButton  TN6045  diese Werte werden nicht mehr aus den Settings gelesen, existieren aber als optionale Parameter
  -------------------------------------------------------------------------------------------------- }

unit PlateHandling;


interface


uses
    AppTypes,
    Rack,
    EventManager,
    IntfArmDevice;

function gmMakeBCResult(aBCString: string; aBCRackLeadZero: integer = 0): string;
procedure gmCheckRack(aGripperArm: IArmDevice; aRack: TRack; aEvBeforeGet, aEvAfterGet, aEvBeforePut,
    aEvAfterPut: TRunstCall);
function gmReadRackID(aGripperArm: IArmDevice; aRack: TRack; aDestSlot: TSlotStruct;
    aReadModes: TReadBarcodeModes; aEvBeforeGet, aEvAfterGet, aEvInBetween, aEvBeforePut, aEvAfterPut,
    aBCBeforeRead, aBCAfterRead: TRunstCall): string;
function gmReadWriteRackID(aGripperArm: IArmDevice; aRack: TRack; aDestSlot: TSlotStruct;
    aReadModes: TReadBarcodeModes; aRackOptions: TRackOptions; aEvBeforeGet, aEvAfterGet, aEvInBetween,
    aEvBeforePut, aEvAfterPut, aBCBeforeRead, aBCAfterRead: TRunstCall): string; overload;
function gmReadWriteRackID(aGripperArm: IArmDevice; aRack: TRack; aReadModes: TReadBarcodeModes)
    : string; overload;

procedure gmDoRackMove(aGripperArm: IArmDevice; aRack: TRack; aSlot: TSlotStruct;
    aMoveOptions: TRackMoveOptions; aEvBeforeGet, aEvAfterGet, aEvInBetween, aEvBeforePut,
    aEvAfterPut: TRunstCall; aZSpeedRackUp: integer = 0; aZRampRackUp: integer = 0;
    aZSpeedRackDown: integer = 0; aZRampRackDown: integer = 0; aXSpeedRack: integer = 0;
    aXRampRack: integer = 0; aYSpeedRack: integer = 0; aYRampRack: integer = 0);
procedure gmDoRackMoveAndPaint(aGripperArm: IArmDevice; aRack: TRack; aSlot: TSlotStruct;
    aMoveOptions: TRackMoveOptions; aEvBeforeGet, aEvAfterGet, aEvInBetween, aEvBeforePut,
    aEvAfterPut: TRunstCall; aZSpeedRackUp, aZRampRackUp, aZSpeedRackDown, aZRampRackDown: integer;
    aXSpeedRack, aXRampRack, aYSpeedRack, aYRampRack: integer);

procedure gmReadAndCompareRackID(aGripperArm: IArmDevice; aRack: TRack; aDestSlot: TSlotStruct;
    aReadModes: TReadBarcodeModes; aRackOptions: TRackOptions; aEvBeforeGet, aEvAfterGet, aEvInBetween,
    aEvBeforePut, aEvAfterPut, aBCBeforeRead, aBCAfterRead: TRunstCall);
function gmAskRackID(aCaption: string; aRack: TRack; aShowNoRackButton: boolean = false): string;
function gmReadAllBarcodesInLayout(aGripperArm: IArmDevice; aRackNamePrefix: string;
    aReadModes: TReadBarcodeModes; var aNumBarcodeReads: integer): boolean; overload;
function gmReadAllBarcodesInLayout(aGripperArm: IArmDevice; aRackNamePrefix: string;
    aReadModes: TReadBarcodeModes): boolean; overload;


implementation


uses
    windows,
    Forms,
    Sysutils,
    Controls,
    LogManager,
    ErrorManager,
    GeneralTypes,
    GUIManager,
    LayoutManager,
    ObjModul,
    SamGlobe,
    ThrdMan,
    Device,
    AppSettings,
    DataProvider,
    PlateHandlingLow,
    ErrorInfo,
    ErrorMessageFactory,
    LayoutDataAdaptor,
    IntfContainerBCReaderDevice,
    DeviceManager,
    OperationBCReading,
    OperationRack,
    OperationFactory,
    GUIManagerSetup,
    TraceManager;

// --------------------------------------------------------------------------------------------------
procedure gmReadAndCompareRackID(aGripperArm: IArmDevice; aRack: TRack; aDestSlot: TSlotStruct;
    aReadModes: TReadBarcodeModes; aRackOptions: TRackOptions; aEvBeforeGet, aEvAfterGet, aEvInBetween,
    aEvBeforePut, aEvAfterPut, aBCBeforeRead, aBCAfterRead: TRunstCall);
// --------------------------------------------------------------------------------------------------
// Read the barcode of rack 'aRackName' and compare it to the barcode already in the DB
// if the barcode was not the same, bring an error message box, if the user clicks CANCEL set the
// error pointer, otherwise re-read the barcode.
// This function stops when cancel is pressed or the barcode read is the same as the one already in the DB
// --------------------------------------------------------------------------------------------------
var
    xMsgResult: integer;
    xUnmatchedBarcodeError: string;
begin
    aReadModes := aReadModes + [mdCheckBC];
    while (not gErrorManager.IsGlobalErr) do
    begin
        xUnmatchedBarcodeError := gmReadWriteRackID(aGripperArm, aRack, aDestSlot, aReadModes, aRackOptions,
            aEvBeforeGet, aEvAfterGet, aEvInBetween, aEvBeforePut, aEvAfterPut, aBCBeforeRead, aBCAfterRead);
        if xUnmatchedBarcodeError = '' then
            Exit;
        xMsgResult := gGUIManager.MessageBox(xUnmatchedBarcodeError, TLanguageString.
            Read('Unexpected rack barcode', 'Unerwarteter Rack-Barcode'), MB_RETRYCANCEL);
        if xMsgResult = mrCancel then
            gErrorManager.SetGlobalErr(ERR_USER, '');
    end;
end;

// --------------------------------------------------------------------------------------------------
function gmAskRackID(aCaption: string; aRack: TRack; aShowNoRackButton: boolean): string;
// --------------------------------------------------------------------------------------------------
var
    xModRes: integer;
    xRackID: string;
    xLabelCaption, xNoRackBtnCaption: string;
begin
    aRack.Highlight := true;
    try
        xRackID := '';
        xLabelCaption := TLanguageString.Read('Please enter ID for rack: {0}',
            'Bitte ID für Rack >{0}< eingeben!', [aRack.Name]);

        if (aShowNoRackButton) then
            xNoRackBtnCaption := TLanguageString.Read('No Rack', 'Kein Rack')
        else
            xNoRackBtnCaption := '';

        // The promptinput dialog can change the xRackID variable
        xModRes := (gGUIManager as TGUIManagerSetup).Barcode_PromptInput(xRackID, aCaption, xLabelCaption,
            xNoRackBtnCaption);

        case xModRes of
            40:
                xRackID := cBCNoRack;
            mrNone:
                xRackID := '';
            mrAbort:
                begin
                    xRackID := '';
                    gErrorManager.SetGlobalErr(ERR_USER, '');
                end;
        end;

        result := xRackID;
    finally
        aRack.Highlight := false;
    end;
end;

procedure gmCheckNewSlot(aRack: TRack; var aSlot: TSlotStruct);
var
    xError: string;
begin
    xError := TLayoutManager.Instance.CurrentLayout.CheckSlot(aSlot, aRack);
    if (xError <> '') then
    begin
        gErrorMessageFactory.ErrBoxSimple(xError, 'Carrier Slot error', eibAbort);
        gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure gmCheckRack(aGripperArm: IArmDevice; aRack: TRack; aEvBeforeGet, aEvAfterGet, aEvBeforePut,
    aEvAfterPut: TRunstCall);
// --------------------------------------------------------------------------------------------------
var
    ErrorResult: integer;
    xSlot, xOldSlot: TSlotStruct;
begin
    ErrorResult := 0;
    // aRack.SetRackColor( $00FF0080 );
    xOldSlot := gmGetCurrentSlotStruct(aRack);

    repeat
        if (gErrorManager.IsGlobalErr) then
            Break;

        if gmRackMoveGet(aGripperArm, aRack, false, 1, TRackMoveOperation.GetDefaultRackMoveOptions(),
            aEvBeforeGet, aEvAfterGet) then
            Break;

        if (gErrorManager.IsGlobalErr) then
            Break;
        { TODO -oPK : no access to methodguiparser }
        // xSlot := TMethodGUIParser.RackActionParseSlotIfNotEmpty('');
        gmCheckNewSlot(aRack, xSlot);
        gmRackMovePut(aGripperArm, aRack, xSlot, xOldSlot, TRackMoveOperation.GetDefaultRackMoveOptions(),
            aEvBeforePut, aEvAfterPut);

        if (gErrorManager.IsGlobalErr) then
            Break;

        ErrorResult := gErrorMessageFactory.ErrBoxSimple(TLanguageString.
            Read('Cannot find rack {0} in position', 'Rack {0} nicht auf Position gefunden.', [aRack.Name]),
            'Wrong state detected', eibAbortRetryIgnore);
        if (ErrorResult = mrAbort) then
            gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
    until (ErrorResult <> mrRetry) or (gErrorManager.IsGlobalErr);
    // aRack.SetRackColor( 0 );
end;

// --------------------------------------------------------------------------------------------------
function gmMakeBCResult(aBCString: string; aBCRackLeadZero: integer): string;
// --------------------------------------------------------------------------------------------------
// Umwandlung des gelesenen BC in das Ergebnis und Einfügen der führenden Nullen
// --------------------------------------------------------------------------------------------------
begin
    if (aBCString = cBCNoRack) then
    begin
        result := cBCNoRack;
        exit;
    end;
    if (aBCString = '') then
    begin
        result := cBCError;
        exit;
    end;
    if (aBCString = 'Error') then
    begin
        result := 'NOCODE';
        exit;
    end; // analog: Audat Balluff-Lesen
    result := aBCString;
    while (Length(result) < aBCRackLeadZero) do
        result := '0' + result;
end;

// --------------------------------------------------------------------------------------------------
procedure gmDoRackMoveAndPaint(aGripperArm: IArmDevice; aRack: TRack; aSlot: TSlotStruct;
    aMoveOptions: TRackMoveOptions; aEvBeforeGet, aEvAfterGet, aEvInBetween, aEvBeforePut,
    aEvAfterPut: TRunstCall; aZSpeedRackUp, aZRampRackUp, aZSpeedRackDown, aZRampRackDown: integer;
    aXSpeedRack, aXRampRack, aYSpeedRack, aYRampRack: integer);
// --------------------------------------------------------------------------------------------------
begin
    gmDoRackMove(aGripperArm, aRack, aSlot, aMoveOptions, aEvBeforeGet, aEvAfterGet, aEvInBetween,
        aEvBeforePut, aEvAfterPut, aZSpeedRackUp, aZRampRackUp, aZSpeedRackDown, aZRampRackDown, aXSpeedRack,
        aXRampRack, aYSpeedRack, aYRampRack);
end;

// --------------------------------------------------------------------------------------------------
procedure gmDoRackMove(aGripperArm: IArmDevice; aRack: TRack; aSlot: TSlotStruct;
    aMoveOptions: TRackMoveOptions; aEvBeforeGet, aEvAfterGet, aEvInBetween, aEvBeforePut,
    aEvAfterPut: TRunstCall; aZSpeedRackUp, aZRampRackUp, aZSpeedRackDown, aZRampRackDown: integer;
    aXSpeedRack, aXRampRack, aYSpeedRack, aYRampRack: integer);
// --------------------------------------------------------------------------------------------------
begin
    gmCheckNewSlot(aRack, aSlot);
    gmDoRackMove1(aGripperArm, aRack, aSlot, aMoveOptions, aEvBeforeGet, aEvAfterGet, aEvInBetween,
        aEvBeforePut, aEvAfterPut, aZSpeedRackUp, aZRampRackUp, aZSpeedRackDown, aZRampRackDown, aXSpeedRack,
        aXRampRack, aYSpeedRack, aYRampRack);
end;

function gmReadBarcodeRack(aUsedArm: IArmDevice; aExtReader: boolean; aRackName: string;
    aRackMovable: boolean): string;
var
    xRackBCReaderDevice: IRackBCReaderDevice;
    xOperation: TRackBCReadingOperation;
begin
    result := '';
    xRackBCReaderDevice := nil;
    if (aExtReader) then
    begin
        ASSERT(false, 'Not implemented');
        // self.Find_ByClass( xDevice, TRackRunstBCReaderDevice );
        // if not ( xDevice is TRackRunstBCReaderDevice ) then EXIT;

        // result := ( xDevice as TRackRunstBCReaderDevice ).ReadRackBarcode( aRackName );
    end
    else
    begin
        xRackBCReaderDevice := aUsedArm.RackBCReaderDevice;
        if (xRackBCReaderDevice = nil) then
        begin
            gErrorMessageFactory.ErrBoxSimple('No Rack Barcode Reader defined!', 'Rack Barcode Reading',
                eibAbort);
            gErrorManager.SetGlobalErr(ERR_USER);
            EXIT;
        end;

        xOperation := TOperationFactory.CreateRackBCReadingOp(aUsedArm, xRackBCReaderDevice, aRackMovable);
        try
            result := xOperation.ReadRackBarcode()
        finally
            xOperation.Free;
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------
function gmReadRackID(aGripperArm: IArmDevice; aRack: TRack; aDestSlot: TSlotStruct;
    aReadModes: TReadBarcodeModes; aEvBeforeGet, aEvAfterGet, aEvInBetween, aEvBeforePut, aEvAfterPut,
    aBCBeforeRead, aBCAfterRead: TRunstCall): string;
// --------------------------------------------------------------------------------------------------
// Nimmt Rack, liest den Barcode und stellt es zurück bzw. an das vorgegebene Ziel
// Returns cBCNoRack if the Destination slot was not found or if RackMoveGet returns false
// --------------------------------------------------------------------------------------------------
var
    xIsRackGetSuccess: boolean;
    xBarcode: string;
    xOldSlot: TSlotStruct;
    xIsRackPutSuccess: boolean;
    xErrorResult: integer;
begin
    result := cBCError;
    if (mdNoRackMove in aReadModes) and not(mdExternBCReader in aReadModes) then
        exit;
    if (gErrorManager.IsGlobalErr) or (mdInsertOnly in aReadModes) then
        Exit;

    xOldSlot := gmGetCurrentSlotStruct(aRack);
    gmCheckNewSlot(aRack, aDestSlot); // aDestSlot could be changed here

    xIsRackGetSuccess := false;

    if (not gErrorManager.IsGlobalErr) then
        gmCheckBalanceOpen(aRack, aDestSlot); // Make sure the balance is open
    // -------------------------------------------------------------------------------- Rack holen
    if (not gErrorManager.IsGlobalErr) then
    begin
        xIsRackGetSuccess := gmRackMoveGet(aGripperArm, aRack, true, gPlateCheckGet,
            TRackMoveOperation.GetDefaultRackMoveOptions(), aEvBeforeGet, aEvAfterGet);
    end;

    if (gErrorManager.IsGlobalErr) or (not xIsRackGetSuccess) then
    begin
        result := cBCNoRack;
        Exit;
    end;

    // Event ausführen: EV_BREADB
    if Assigned(aBCBeforeRead) then
        aBCBeforeRead.Execute('before reading rack barcode');

    // Barcode lesen
    xBarcode := gmReadBarcodeRack(aGripperArm, (mdExternBCReader in aReadModes), aRack.Name,
        gmRackMovable(aGripperArm, aRack));
    result := gmMakeBCResult(xBarcode);

    // Event ausführen: EV_AREADB
    if Assigned(aBCAfterRead) then
        aBCAfterRead.Execute('after reading rack barcode');

    // Event ausführen: In between
    if Assigned(aEvInBetween) then
        aEvInBetween.Execute('between get and put tube');

    // Rack zurückstellen
    xIsRackPutSuccess := gmRackMovePut(aGripperArm, aRack, aDestSlot, xOldSlot,
        TRackMoveOperation.GetDefaultRackMoveOptions(), aEvBeforePut, aEvAfterPut);

    if (not gErrorManager.IsGlobalErr) and (not xIsRackPutSuccess) then
    begin
        xErrorResult := gmRackMovePutHandleError(aRack, aDestSlot);
        if (xErrorResult = mrRetry) then
        begin
            TLayoutManager.Instance.CurrentLayout.MoveRack(aRack, xOldSlot.CarrierName, xOldSlot.SlotNr,
                xOldSlot.Rotation);

            // 01.06.10 pk : I dont understand this part? if the rack movement causes an error and the user clicks Ignore the rack is moved again?
            // it doesn't make sense. If anybody understands write a comment here.
            gmDoRackMove(aGripperArm, aRack, aDestSlot, TRackMoveOperation.GetDefaultRackMoveOptions(),
                aEvBeforeGet, aEvAfterGet, aEvInBetween, aEvBeforePut, aEvAfterPut);
        end;
    end;

    // if we were able to get the rack successfully we will assume that something has been done
    if xIsRackGetSuccess then
    begin
        gmAddRackMoveRunEffect(aRack, aDestSlot, xOldSlot);
    end;

end;

// --------------------------------------------------------------------------------------------------
function gmCompareBarcode(aRack: TRack; aNewBarcode: string): string;
// --------------------------------------------------------------------------------------------------
// if the new barcode is the same as the one already in the DB return empty string
// otherwise write an error message to the log file and return an error string
// --------------------------------------------------------------------------------------------------
var
    xPreviousBarcode: string;
begin
    result := '';
    xPreviousBarcode := aRack.RackID;
    // --------------------------------------------------------- Vergleichen mit vorhandenem Wert
    if (xPreviousBarcode = aNewBarcode) then
        Exit;

    gLogManager.Log(Format('Check Barcode [' + aRack.Name + ']: Expected: %s; found: %s',
        [xPreviousBarcode, aNewBarcode]), true);

    result := TLanguageString.Read('Wrong barcode Rack {0}!  Expected: {1}; found: {2}.',
        'Falscher Barcode Rack {0}!  Erwartet: {1}; gefunden: {2}.',
        [aRack.Name, xPreviousBarcode, aNewBarcode]);
end;

// --------------------------------------------------------------------------------------------------
function gmReadWriteRackID(aGripperArm: IArmDevice; aRack: TRack; aReadModes: TReadBarcodeModes): string;
// --------------------------------------------------------------------------------------------------
// Use this function if the DestSlot is the same as the source slot
// --------------------------------------------------------------------------------------------------
var
    xSlotStruct: TSlotStruct;
begin
    { TODO -oPK : no access to methodguiparser }
    // xSlotStruct := TMethodGUIParser.RackActionParseSlotIfNotEmpty('');
    result := gmReadWriteRackID(aGripperArm, aRack, xSlotStruct, aReadModes, [], nil, nil, nil, nil, nil,
        nil, nil);
end;

// --------------------------------------------------------------------------------------------------
function gmReadWriteRackID(aGripperArm: IArmDevice; aRack: TRack; aDestSlot: TSlotStruct;
    aReadModes: TReadBarcodeModes; aRackOptions: TRackOptions; aEvBeforeGet, aEvAfterGet, aEvInBetween,
    aEvBeforePut, aEvAfterPut, aBCBeforeRead, aBCAfterRead: TRunstCall): string;
// --------------------------------------------------------------------------------------------------
// Lesen der RackId eines Racks und schreiben in den aktuellen RUN in Layout.db
// --------------------------------------------------------------------------------------------------
var
    OtherRack: string;
    xRackID, xDlgCaption: string;
    xManualBarcode: string;
begin

    result := '';

    // PK: I dont think this is the right place to do this check!?
    ASSERT((TLayoutManager.Instance.CurrentLayout.LayoutName <> '') and
        (TLayoutManager.Instance.CurrentLayout.LayoutRunName <> ''), TLanguageString.
        Read('No Run-Layout loaded!', 'Kein Run-Layout geladen!')); // ----- Run-Layout muß vorhanden sein

    if (not gErrorManager.IsGlobalErr) then
    begin
        // --------------------------------------------------- Bei Check muß das Rack einen Barcode haben
        if (mdCheckBC in aReadModes) then
        begin
            xRackID := aRack.RackID;
            if (xRackID = '') or (xRackID = cBCNoRack) or (xRackID = cBCError) or (xRackID = 'NOCODE') or
                (POS(cBCRandom, xRackID) = 1) then
            begin
                gLogManager.Log('Check Barcode [' + aRack.Name + '] not necessary.', true);
                exit;
            end;
        end;
        // ------------------------- für Audat: mit '.BCB' im Racknamen wird die Balluff.Runst angesprochen
        if (Pos('.BCB', aRack.Name) <> 0) then
            aReadModes := aReadModes + [mdExternBCReader];

        // ------------------------------------------------------------------ Starte Barcode-Lese-Vorgang
        gLogManager.Log(TLanguageString.Read('Read barcode of rack {0}', 'Barcode Lesen von Rack {0}',
            [aRack.Name]), true);

        if (roReadBCManually in aRackOptions) then
            aReadModes := aReadModes + [mdInsertOnly];

        // ------------------------------------------------------------------------- Barcode normal lesen
        result := gmReadRackID(aGripperArm, aRack, aDestSlot, aReadModes, aEvBeforeGet, aEvAfterGet,
            aEvInBetween, aEvBeforePut, aEvAfterPut, aBCBeforeRead, aBCAfterRead);

        // -------------------------------- Barcode manuell eingeben, wenn leer oder gleich einem anderen
        if (not gErrorManager.IsGlobalErr) and (result <> cBCNoRack) and (not(mdCheckBC in aReadModes)) and
            (((mdShowErrMsg in aReadModes) and (g24hMode = 0)) or (mdInsertOnly in aReadModes)) then
        begin

            while true do
            begin
                OtherRack := '';
                if not(roAllowNonUniqueBC in aRackOptions) then
                    OtherRack := TLayoutManager.Instance.BCEqualToOther(aRack.Name, result);

                if (gErrorManager.IsGlobalErr) then
                    Break;
                if (result <> cBCError) and (POS(cBCRandom, result) <> 1) and (OtherRack = '') then
                    Break;

                if (result = cBCError) or (Pos(cBCRandom, result) = 1) then
                    xDlgCaption := TLanguageString.Read('No barcode read!', 'Es wurde kein Barcode gelesen!')
                else
                    xDlgCaption := TLanguageString.Read('{0} is stored for rack {1} !',
                        '{0} bereits vorhanden für Rack {1} !', [result, OtherRack]);

                xManualBarcode := gmAskRackID(xDlgCaption, aRack);
                result := gmMakeBCResult(xManualBarcode);
            end;
        end;
        // ---------------------- Random-Barcode wird erzeugt, wenn fehlerhafter Lesevorgang im 24-h-Mode
        if (not gErrorManager.IsGlobalErr) and (g24hMode > 0) and (result = cBCError) then
        begin
            Randomize;
            result := cBCRandom + IntToStr(Random(99999999));
            while (TLayoutManager.Instance.BCEqualToOther(aRack.Name, result) <> '') do
                result := cBCRandom + IntToStr(Random(99999999));
        end;
        // ----------------------------------- Eintragen in Datenbank / vergleichen mit Datenbank-Eintrag
        if (not gErrorManager.IsGlobalErr) then
        begin
            if (mdCheckBC in aReadModes) then
            begin
                result := gmCompareBarcode(aRack, result);
            end
            else
            begin
                // Save old RackID
                TTraceManager.Instance.RackChangedID(aRack.Name, aRack.RackID);
                // --------------------------------------------------------------- Schreiben in die Datenbank
                TLayoutManager.Instance.CurrentLayout.ChangeRackID(aRack, result);

                gLogManager.Log('Barcode read [' + aRack.Name + ']: ' + result, true);
            end;
        end
        else
            result := '';
    end;
end;

function gmReadAllBarcodesInLayout(aGripperArm: IArmDevice; aRackNamePrefix: string;
    aReadModes: TReadBarcodeModes; var aNumBarcodeReads: integer): boolean;
var
    xRackID: string;
    xRack: TRack;
    i: integer;
begin
    result := true;
    aNumBarcodeReads := 0;
    gLogManager.Log(TLanguageString.Read('Read rack barcodes', 'Rack-Barcodes lesen'), true);
    for i := 0 to TLayoutManager.Instance.CurrentLayout.NoOfRacks - 1 do
    begin
        if gErrorManager.IsGlobalErr then
            BREAK;
        xRack := TLayoutManager.Instance.CurrentLayout.Racks[i];
        if (xRack.RackID <> '') and (xRack.RackID <> cBCError) then
            CONTINUE;
        if Pos(aRackNamePrefix, Uppercase(xRack.Name)) <> 1 then
            CONTINUE;
        Inc(aNumBarcodeReads);
        xRackID := gmReadWriteRackID(aGripperArm, xRack, aReadModes);
        if (xRackID = cBCNoRack) then
            result := false;
    end;
end;

function gmReadAllBarcodesInLayout(aGripperArm: IArmDevice; aRackNamePrefix: string;
    aReadModes: TReadBarcodeModes): boolean;
var
    xDummy: integer;
begin
    result := gmReadAllBarcodesInLayout(aGripperArm, aRackNamePrefix, aReadModes, xDummy);
end;


end.
