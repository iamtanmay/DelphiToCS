{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : These are functions used by Actions which require PlateHandling and TubeHandling
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.02.04 pk                               TN1719   New
  05.02.04 wl  ReadWriteTubeID              TN1711   Tube wird zur Abfrage violett und nach erfolgter Eingabe grün gezeichnet
  05.02.04 wl  ReadWriteTubeID              TN1739   der Rückgabewert ist von Anfang an definiert
  11.02.04 pk  gmTubeAction                 TN1719   dont set balance type if SetBalanceType parameter is false
  02.03.04 wl  gmTubeAction                 TN1773   new parameter: EventList for tube events
  02.03.04 wl  gmReadWriteTubeID            TN1773   new parameter: EventList for tube events
  12.03.04 mo  gmReadWriteTubeID            TN1807   if not (optUseRackIDPos in aTubeParams.TOptions) ....
  05.04.04 wl                               TN1788   statt gTool wird gGrpArm.Tool verwendet
  04.05.04 wl  gmReadWriteTubeID            TN1899   gmAskTubeId-Schleife wird bei "Abort" beendet
  10.05.04 pk                               TN1889.2 All function parameters changed to use TXTubeParamters instead of TTubeParametes
  10.05.04 pk  gmReadWriteTubeID            TN1889.2 Returns TXRackPosition instead of TRackIDPosition
  11.05.04 wl  gmReadWriteTubeID            TN1889.2 Format-String mit S.Rack.Name statt S.Rack
  13.05.04 pk  gmReadWriteTubeID            TN1920   If no barcodereader check if entered tube already exists if gTubeBarcodesAreUnique is true
  08.06.04 wl  alle Funktionen              TN1963   geänderte Parameter der TubeHandling-Funktionen
  17.06.04 wl  gmReadWriteTubeID,gmTubeAction  TN1981   --> TubeHandling
  04.08.04 pk                               TN2080   gmReadWriteRackID takes RackOptions parameter
  01.11.04 wl  gnGetRealBarcodes            TN2181   Anpassung an Änderungen von TubeHandling-Funktionen
  08.11.04 wl  gmPrepareBalance             TN2213   Aufruf von GetBalancePos geändert
  07.12.04 wl                               TN2246.4  uses ObjectsRun statt ObjWorkbExt
  27.04.05 pk  gmWeighPosition              TN2246   use PosinfoDataAdaptor
  17.11.05 wl  gmGetRealBarcodes            TN2771    mit Parameter aGripperArm
  17.11.05 wl  gmPrepareBalance             TN2771    gmBalanceTakeTube wird immer mit dem erstbesten Gripper-Arm durchgeführt
  23.11.05 thr gmWeighPosition              TN2799    Die SubstID wird aus der Posinfo gelesen
  28.11.05 thr gmWeighPosition              TN2799    Der "Tarawert" der SubstID wird aus der Posinfo gelesen
  05.01.06 pk  gmPrepareBalance             TN2877    use FindFirstGripArm
  08.03.06 thr gmWeighPosition              TN2941    Uses BalanceName
  08.03.06 thr gmPrepareBalance             TN2941    Init all balances
  03.04.06 thr gmWeighPosition              TN3007    Additional Parameters
  03.04.06 pk  gmPrepareBalance             TN2997    new parameter : aInitID
  12.04.06 pk                               TN2997    uses interfacecomm instead of device
  07.09.06 pk                               TN3292    use of PosinfodataAdaptor instance removed
  07.03.07 wl                               TN3620    uses geändert
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  11.07.08 wl                               TN4164   TActionModules wird ohne "gModules as" aufgerufen
  31.07.08 pk                               TN4139   reference to ZARunnerLayout removed
  11.09.08 wl  gmWeighPosition              TN4176    reads 'UseLastSubstID' from settings - if false the ID will always be "Destination"
  17.12.08 wl  TWeighPositionOperation      TN4359    ersetzt gmWeighPosition
  17.12.08 wl  TWeighPositionOperation      TN4359    Berechnung und Speichern des neuen Tara --> TBalanceDevice.CalcAndStoreNewTare
  17.12.08 wl  TWeighPositionOperation      TN4365    neu: StoreAsTare
  13.01.09 wl  GetRealBarcodes              TN4312    TRackPositions ist jetzt dynamisches Array
  13.02.09 wl  gmGetRealBarcodes            TN4429    Parameter von gmReadWriteRackID geändert
  10.08.09 wl                               TN4702    Strings werden jetzt direkt geladen
  16.10.09 pk  TBalanceOperation            TN4810    New
  16.10.09 pk  TTareBalanceOperation        TN4810    New
  28.05.10 wl                               TN5116   uses geändert
  19.10.10 wl  gmGetRealBarcodes            TN5288    Erst werden die Racks abgefragt, dann die Tubes
  17.11.11 wl                               TN5729   RecCnt entfernt
  -------------------------------------------------------------------------------------------------- }

unit ContainerHandling;


interface


uses
    IntfBalanceDevice,
    AppTypes,
    RackTypes,
    IntfArmDevice,
    Driver;

type
    TBalanceOperation = class
    protected
        fBalanceDev: IBalanceDevice;
    public
        constructor Create(const aBalanceName: string);
        procedure WaitForBalance(const aWeightStoreKeyName: string; const aIsTimeoutStoreKeyName: string);
    end;

    TWeighPositionOperation = class(TBalanceOperation)
    private
        fRackIDPosition: TRackIDPosition;
        fNumValues: integer;
        fDeviation: double;
        fTarget: double;
        fSubstance: string;
        fStoreAsTare: boolean;
    public
        constructor Create(aRack: TXRackPosition; const aBalanceName: string; aNumValues: integer;
            aDeviation, aTarget: double; aStoreAsTare: boolean);
        procedure WeighPosition();

    end;

    TTareBalanceOperation = class(TBalanceOperation)
    private
        fNumValues: integer;
        fDeviation: double;
    public
        constructor Create(const aBalanceName: string; aNumValues: integer; aDeviation: double);
        procedure Tare();
    end;

procedure gmPrepareBalance(aInitID: TDevInitID);
procedure gmWaitForBalance();

procedure gmGetRealBarcodes(aTubesReadAgain: boolean; aGripperArm: IArmDevice; const aToolName: string);


implementation


uses
    windows,
    SysUtils,
    AppSettings,
    ErrorManager,
    LogManager,
    CommonTypes,
    ObjModul,
    LayoutManager,
    GUIManager,
    GeneralTypes,
    PlateHandling,
    ToolHandling,
    TubeHandling,
    SamGlobe,
    PosInfoDataAdaptor,
    SamIntf,
    Rack,
    MotionSystemTube,
    EventManager;

constructor TBalanceOperation.Create(const aBalanceName: string);
begin
    inherited Create();
    if aBalanceName <> '' then
        fBalanceDev := gModules.FindBalanceByName(aBalanceName)
    else
        fBalanceDev := gModules.FindBalance;
end;

procedure TBalanceOperation.WaitForBalance(const aWeightStoreKeyName: string;
    const aIsTimeoutStoreKeyName: string);
var
    xWeight: double;
    xIsTimeout: boolean;
    xIsTimeoutAsStr: string;
    xStoreResults: boolean;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;
    if not Assigned(fBalanceDev) then
        EXIT;

    gLogManager.LogF('Wait for balance %s', [fBalanceDev.Name], true);
    xStoreResults := fBalanceDev.Wait(xWeight, xIsTimeout);

    if xStoreResults then
    begin
        if (aWeightStoreKeyName <> '') then
            TEventManager.Instance.StoreParserIdentRWValue(aWeightStoreKeyName, FloatToStr(xWeight));

        if (aIsTimeoutStoreKeyName <> '') then
        begin
            xIsTimeoutAsStr := '0';
            if xIsTimeout then
                xIsTimeoutAsStr := '1';
            TEventManager.Instance.StoreParserIdentRWValue(aIsTimeoutStoreKeyName, xIsTimeoutAsStr);
        end;
    end;

    fBalanceDev.OpenDoor;
end;

{ TWeighPositionOperation }

constructor TWeighPositionOperation.Create(aRack: TXRackPosition; const aBalanceName: string;
    aNumValues: integer; aDeviation, aTarget: double; aStoreAsTare: boolean);
const
    cDefNameDestination = 'Destination';
begin
    inherited Create(aBalanceName);

    fRackIDPosition := gmXRackPosToRackIDPos(aRack);
    fNumValues := aNumValues;
    fDeviation := aDeviation;
    fTarget := aTarget;
    fStoreAsTare := aStoreAsTare;
    fSubstance := cDefNameDestination;
end;

procedure TWeighPositionOperation.WeighPosition();
var
    xPosinfoDA: TPosinfoDataAdaptor;
    xIniAccess: IWinLissyIniAccess;
    xUseLastSubstID: boolean;
begin
    if not Assigned(fBalanceDev) then
        Exit;

    xIniAccess := gCommonDll.CreateRobotIni;
    xUseLastSubstID := xIniAccess.ReadBool('WGHP', 'UseLastSubstID');

    if (xUseLastSubstID) then
    begin
        xPosinfoDA := TPosInfoDataAdaptor.Create();
        try
            xPosinfoDA.GetLastSubstID(fSubstance, fRackIDPosition.RackID, fRackIDPosition.Pos);
        finally
            xPosinfoDA.Free;
        end;
    end;

    // fBalanceDev.OnAfterStoreWeight := self.CalcAndStoreNetWeight;
    fBalanceDev.StartWeight(fRackIDPosition.RackID, fRackIDPosition.Pos, fSubstance, fNumValues, fDeviation,
        fTarget, fStoreAsTare);

end;

constructor TTareBalanceOperation.Create(const aBalanceName: string; aNumValues: integer; aDeviation: double);
begin
    inherited Create(aBalanceName);

    fNumValues := aNumValues;
    fDeviation := aDeviation;
end;

procedure TTareBalanceOperation.Tare();
begin
    if not Assigned(fBalanceDev) then
        Exit;
    if (gErrorManager.IsGlobalErr) then
        Exit;

    fBalanceDev.StartTare(fNumValues, fDeviation);
end;

// --------------------------------------------------------------------------------------------------
procedure gmPrepareBalance(aInitID: TDevInitID);
// --------------------------------------------------------------------------------------------------
var
    xGripperArm: IArmDevice;
    xDummy, xOccupiedPos, xOriginOccupiedPos: TXRackPosition;
    xBalanceArray: TBalanceArray;
    xBalanceDev: IBalanceDevice;
    i: integer;
    xDA: TPosinfoDataAdaptor;
begin
    xDA := TPosinfoDataAdaptor.Create();
    try

        gModules.FindAllBalances(xBalanceArray);
        for i := low(xBalanceArray) to high(xBalanceArray) do
        begin
            xBalanceDev := xBalanceArray[i];
            if (xBalanceDev <> nil) then
            begin
                // Vorsicht!! Nur alte Version: Rackname=BalanceName - Tubes abräumen.
                if Assigned(TLayoutManager.Instance.CurrentLayout.FindRackByName(xBalanceDev.Name)) then
                begin
                    TLayoutManager.Instance.CurrentLayout.GetBalancePos(xDummy, xOccupiedPos,
                        xOriginOccupiedPos);
                    if (xOccupiedPos.Pos <> 0) then
                    begin
                        if (gMoveTubesAfterRestart = 1) then
                        begin
                            gGUIManager.MessageBox(TLanguageString.
                                Read('Robot removes tubes from the balance!',
                                'Tubes werden von der Waage genommen!'),
                                TLanguageString.Read('Tubes On Balance', 'Tubes auf der Waage'), 0);
                            while (xOccupiedPos.Pos <> 0) and (not gErrorManager.IsGlobalErr) do
                            begin
                                xGripperArm := gModules.FindFirstGripArm();
                                ASSERT(Assigned(xGripperArm), 'No grip arm');
                                TTubeHandling.GeneralBalanceTakeTube(xGripperArm, xOccupiedPos,
                                    xOriginOccupiedPos, [mtmNoStartPosBeforeGetTube], gMoveTubeFromBalance);
                            end;
                        end
                        else
                            gGUIManager.MessageBox(TLanguageString.Read('Please take tubes from the balance!',
                                'Bitte Tubes von der Waage nehmen'), TLanguageString.Read('Tubes On Balance',
                                'Tubes auf der Waage'), 0);
                        { TODO : Not implemented yet }
                        // if not gmQU(nil, DMRack.QuerySamintf,Format('DELETE FROM Posinfo WHERE (RACKID="%s") AND (POS=%d)',
                        // [xOriginOccupiedPos.Rack.RackID,xOriginOccupiedPos.Pos])) then SetGlobalErr(ERR_APPLICATION);
                        // if not gmQU(nil,DMRack.QuerySamintf,Format('DELETE FROM Posinfo WHERE (RACKID="%s") AND (POS=%d)',
                        // [xOccupiedPos.Rack.RackID,xOccupiedPos.Pos])) then SetGlobalErr(ERR_APPLICATION);
                    end;
                end;
                // Ende Tubes abräumen
                if (not gErrorManager.IsGlobalErr) then
                    xBalanceDev.Init(aInitID);
            end;
        end;

    finally
        xDA.Free;
    end;

end;

// --------------------------------------------------------------------------------------------------
procedure gmWaitForBalance;
// --------------------------------------------------------------------------------------------------
var
    xBalanceDev: IBalanceDevice;
begin
    xBalanceDev := gModules.FindBalance();
    if not Assigned(xBalanceDev) then
        Exit;
    xBalanceDev.WaitForBalanceThread();
end;

// --------------------------------------------------------------------------------------------------
procedure gmGetRealBarcodes(aTubesReadAgain: boolean; aGripperArm: IArmDevice; const aToolName: string);
// --------------------------------------------------------------------------------------------------
// Rückgabewert: wenn FALSE, ist eines der gelesenen Racks nicht vorhanden
// --------------------------------------------------------------------------------------------------
var
    x, xTube: integer;
    xPositions: TRackPositions;
    xRack: TRack;
begin
    // replace virtual rack ID's
    for x := 0 to TLayoutManager.Instance.CurrentLayout.NoOfRacks - 1 do
    begin
        if (gErrorManager.IsGlobalErr) then
            EXIT;
        xRack := TLayoutManager.Instance.CurrentLayout.Racks[x];

        if (Pos(cBCRandom, xRack.RackID) = 1) then
        begin
            gmReadWriteRackID(nil, xRack, [mdShowErrMsg, mdInsertOnly]);
        end;
    end;

    // replace virtual tube ID's
    for x := 0 to TLayoutManager.Instance.CurrentLayout.NoOfRacks - 1 do
    begin
        xRack := TLayoutManager.Instance.CurrentLayout.Racks[x];

        xPositions := xRack.FindRandomTubeBC();
        for xTube := 0 to high(xPositions) do
        begin
            if (gErrorManager.IsGlobalErr) then
                EXIT;
            TTubeHandling.GeneralCheckTubeID(TLayoutManager.Instance.CurrentLayout.FindXRackPos
                (xPositions[xTube].Rack, xPositions[xTube].Pos, true), aTubesReadAgain, aGripperArm,
                aToolName);
        end;
    end;
end;


end.
