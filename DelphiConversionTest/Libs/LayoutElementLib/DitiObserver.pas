{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Class that contains information about drop- and fetch-racks of one DiTi type
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  09.05.03 wl                               TN1490   initial version
  30.08.03 wl  PaintFetchRacks              TN1560   die Funktion, überzählige DiTi-Positionen zu löschen, führte bisher zum Fehler
  30.08.03 wl  GetTipPositions              TN1543   wenn sich das Rack noch im Stacker (DiTiStorage) befindet, wird ein leeres DiTi-RAck mit übergeben
  05.04.04 wl  FillFetchRacks,PaintFetchRacks TN1788 statt GetTubeData wird nur PaintRackPos verwendet
  19.04.04 wl  GetDiTiPositions             TN1788   ehm. GetTipPositions benutzt TDiTiPositions aud RackTypes.pas
  27.04.05 pk  FillFetchRacks,PaintFetchRacks TN2398   use TPosinfoDataAdaptor
  03.06.05 wl  GetDiTiPositions               TN2436   statt Sam.nTips zu benutzen wird TipCount übergeben
  20.06.07 wl  TDiTiObserver.PaintFetchRacks  TN3735   Query wird nicht mehr ReadOnly geöffnet, da es sonst zu Fehler kommen kann
  07.08.07 wl                               TN3811.3 TPosinfoAdapter.Create ohne Parameter
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  20.06.08 pk                               TN4139   Rack no longer has typed link to Carrier use TLayout.GetCarrierOfRack
  13.07.09 pk  FillFetchRacks               TN4585.4 selectandopen removed
  12.09.09 wl                               TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  10.05.10 wl  FillFetchRacks               TN5105   Log eingefügt
  24.02.11 wl                               TN5431   an Änderungen von TRack angepasst
  28.10.11 wl                               TN5728   verwendet DisposableTipDataAdaptor statt PosinfoTipDataAdaptor
  02.02.11 wl  TDiTiPositions               TN5791   enthält TXRackPositions statt TRack und TIntArray für Positionen
  -------------------------------------------------------------------------------------------------- }

unit DitiObserver;


interface


uses
    Rack,
    AppTypes,
    RackTypes,
    GeneralTypes;

type
    TDiTiPositions = record
        DiTiRP: TArray<TXRackPosition>;
        DiTiMap: TipMap;
    end;

    TDiTiPositionsUtils = class
    public
        class function GetEmptyDiTiPositions(aTipCount: integer): TDiTiPositions;
    end;

    TDiTiObserver = class // Überwachung einer Sorte von Ditis
    private
        FFetchRacks: TArray<TRack>;
        FDropRack: TRack;
        FDitiType: string;
        FNextFetchRackIndex: integer;
        FNextFetchRackPos: integer;
        function GetNoOfFetchRacks: integer;
        function FindNextFetchPos: boolean;
        class procedure DeleteFetchPositions(aFetchRack: TRack; aLastPos: integer);
    public
        // constructor
        constructor Create(aDitiType: string);
        // public methods
        procedure AddFetchRack(aFetchRack: TRack);
        function GetDiTiPositions(aTipMap: TipMap; aTipCount: integer; aSimulated: boolean;
            var aEmptyRack: TRack): TDiTiPositions;
        procedure FillFetchRacks;
        procedure PaintFetchRacks;
        // properties
        property DitiType: string read FDitiType;
        property DropRack: TRack read FDropRack write FDropRack;
        property NoOfFetchRacks: integer read GetNoOfFetchRacks;
    end;


implementation


uses
    SysUtils,
    Math,
    DisposableTipDataAdaptor,
    SamGlobe,
    CommonTypes,
    Layout,
    Carrier,
    RackWell,
    LogManager,
    TipMapUtils,
    ArrayUtils;

{ TDiTiObserver }

constructor TDiTiObserver.Create(aDitiType: string);
begin
    inherited Create;

    FDitiType := aDitiType;
    FNextFetchRackPos := 0;
end;

procedure TDiTiObserver.AddFetchRack(aFetchRack: TRack);
var
    xLength: integer;
begin
    xLength := high(FFetchRacks);
    SetLength(FFetchRacks, xLength + 2);
    FFetchRacks[xLength + 1] := aFetchRack;
end;

procedure TDiTiObserver.PaintFetchRacks;
var
    x, xPos: integer;
    xDataAdaptor: TDisposableTipDataAdaptor;
begin

    xDataAdaptor := TDisposableTipDataAdaptor.Create();
    try

        for x := 0 to high(FFetchRacks) do
        begin
            // find postions in DisposableTip.db
            xDataAdaptor.SelectAndOpenByRackID(FFetchRacks[x].RackID, false);
            while not(xDataAdaptor.DataProvider.Eof) do
            begin
                xPos := xDataAdaptor.ReadPos;
                if (xPos > 0) and
                    (xPos <= (FFetchRacks[x].Structure.Cols * FFetchRacks[x].Structure.Rows)) then
                begin
                    FFetchRacks[x].PaintTubePos(xPos, TRackWellDisplayType.Disposables);
                    xDataAdaptor.DataProvider.Next;
                end
                else
                begin
                    xDataAdaptor.DataProvider.Delete; // überzählige Positionen werden gelöscht!
                end;
            end;
            xDataAdaptor.Close;
        end;

    finally
        FreeAndNil(xDataAdaptor);
    end;
end;

function TDiTiObserver.GetNoOfFetchRacks: integer;
begin
    result := high(FFetchRacks) + 1;
end;

procedure TDitiObserver.FillFetchRacks;
var
    xPos, x: integer;
    xDataAdaptor: TDisposableTipDataAdaptor;
begin
    xDataAdaptor := TDisposableTipDataAdaptor.Create();
    try
        for x := 0 to high(FFetchRacks) do
        begin

            TLogManager.Instance.Log('Fill disp tips rack ' + FFetchRacks[x].Name + ' ID ' +
                FFetchRacks[x].RackID, true);

            // DisposableTip-Daten des Racks neu füllen
            for xPos := 1 to (FFetchRacks[x].Structure.Cols * FFetchRacks[x].Structure.Rows) do
            begin
                xDataAdaptor.InternInsertIfNotFound(FFetchRacks[x].RackId, xPos);
                FFetchRacks[x].PaintTubePos(xPos, TRackWellDisplayType.Disposables);
            end;

            // First position = next fetch position
            FNextFetchRackPos := 1;
            FNextFetchRackIndex := 0;
        end;
    finally
        FreeAndNil(xDataAdaptor);
    end;
end;

class procedure TDiTiObserver.DeleteFetchPositions(aFetchRack: TRack; aLastPos: integer);
var
    xPos: integer;
    xDataAdaptor: TDisposableTipDataAdaptor;
begin

    // un-paint all positions
    for xPos := 1 to (aLastPos) do
    begin
        aFetchRack.PaintTubePos(xPos, TRackWellDisplayType.default);
    end;

    xDataAdaptor := TDisposableTipDataAdaptor.Create();
    try
        xDataAdaptor.DeletePositions(aFetchRack.RackID, aLastPos);
    finally
        FreeAndNil(xDataAdaptor);
    end;
end;

function TDiTiObserver.FindNextFetchPos;
var
    x: integer;
    xDataAdaptor: TDisposableTipDataAdaptor;
begin
    result := false;
    FNextFetchRackPos := 0;

    xDataAdaptor := TDisposableTipDataAdaptor.Create();
    try
        for x := 0 to high(FFetchRacks) do
        begin
            if xDataAdaptor.GetPosFromRackID(FFetchRacks[x].RackID, FNextFetchRackPos) then
            begin
                FNextFetchRackIndex := x;
                result := true;
                exit;
            end;
        end;
    finally
        FreeAndNil(xDataAdaptor);
    end;
end;

function TDiTiObserver.GetDiTiPositions(aTipMap: TipMap; aTipCount: integer; aSimulated: boolean;
    var aEmptyRack: TRack): TDiTiPositions;
var
    x: integer;
    xCarrier: TCarrier;
begin
    result := TDiTiPositionsUtils.GetEmptyDiTiPositions(aTipCount);

    // find next diti fetch position
    if (not FindNextFetchPos) then
    begin
        FillFetchRacks; // automatisches Wiederauffüllen des Dips-Tip-Racks

        if (not FindNextFetchPos) then
        begin
            gLogManager.Log('ERROR: No disposable tip rack found!', true);
            raise Exception.Create('No disposable tip rack found.');
        end;
    end;

    // if rack is in Diti-Storage-Carrier: put last rack into free slot, get new diti rack from carrier
    xCarrier := TLayout.GetCarrierOfRack(FFetchRacks[FNextFetchRackIndex]);
    if TCarrier.IsDitiStorage(xCarrier.Name) then
    begin

        // find last Diti rack to put away
        aEmptyRack := nil;
        for x := 0 to FNextFetchRackIndex - 1 do
        begin
            xCarrier := TLayout.GetCarrierOfRack(FFetchRacks[x]);
            if not TCarrier.IsDitiStorage(xCarrier.Name) then
            begin
                aEmptyRack := FFetchRacks[x];
                BREAK;
            end;
        end;
        if (aEmptyRack = nil) then
            raise Exception.Create('No slot found for next DiTi rack from DiTi storage');
    end;

    // define result-TipMap and the positions that can be fetched
    for x := 0 to aTipCount - 1 do
    begin
        if (FNextFetchRackPos > (FFetchRacks[FNextFetchRackIndex].Structure.Cols *
            FFetchRacks[FNextFetchRackIndex].Structure.Rows)) then
            break; // bei letzter Position abbrechen, weil nur ein Rackhandle übergeben werden kann

        if (((aTipMap shr x) and 1) = 1) then
        begin
            result.DiTiRP[x].Rack := FFetchRacks[FNextFetchRackIndex]; // define result rack
            result.DiTiRP[x].Pos := FNextFetchRackPos; // gefundene Wechselspitzenposition  zuweisen
            result.DiTiMap := result.DiTiMap + Round(Power(2, x)); // Ergebnis-TipMap setzen;
            inc(FNextFetchRackPos);
        end
        else if (gDiTiIgnoreRestTips) then
            inc(FNextFetchRackPos); // lösche alle Positionen im 4er/8er-Block die nicht benötigt werden
    end;

    // delete positions in DisposableTip.db (if not simulated)
    if (not aSimulated) then
        DeleteFetchPositions(FFetchRacks[FNextFetchRackIndex], FNextFetchRackPos - 1);

    gLogManager.Log('Get disposable tips - ' + TXRackPositionUtils.RackPositionsToBracketText
        (result.DiTiRP), true);
end;

{ TDiTiPositionsUtils }

class function TDiTiPositionsUtils.GetEmptyDiTiPositions(aTipCount: integer): TDiTiPositions;
begin
    result.DiTiRP := TXRackPositionUtils.GetEmptyXRackPositions(aTipCount);
    result.DiTiMap := 0;
end;


end.
