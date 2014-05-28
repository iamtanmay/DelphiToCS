{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk), Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  08.09.08 pk                               TN4215
  17.09.08 wl                               TN4224   TRunLayout = class( TLayoutWithDevices )
  20.09.08 pk  SetBalanceType               TN4215   override
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.09.10 pk  DoMouseDown                  TN5042   functions that were in RunMain now accessible only via GUIManager
  08.02.11 wl                               TN5475   Zugriffe auf TLayoutDataAdaptor.Instance geändert
  20.09.11 wl                               TN5723   an Änderungen angepasst
  03.11.11 wl  DoCreateRackWell             TN5725   jetzt mit WellNr
  17.11.11 wl  TRunRack,TRunRackWell        TN5729   entfernt (wurde nur für PositionInfo gebraucht)
  ---------------------------------------------------------------------------------------------------------------------- }

unit RunLayout;


interface


uses
    Rack,
    Layout,
    LayoutElementCallbackTypes,
    Tipset,
    RackTypes,
    LayoutWithDevices;

type
    TRunLayout = class(TLayoutWithDevices)
    protected
        procedure UpdateRackRunData(aRack: TRack); override;
        function FindAllBalancePositions: TArray<TXRackPosition>;
        function FindBalanceRack: TRack;
        procedure UpdateCarrierRunType(const aCarrierName, aNewCarrierType: string); override;
        procedure UpdateRackRunType(const aRackName, aNewRackType: string); override;
    public
        procedure ChangeRackID(aRack: TRack; const aNewID: string); override;
        procedure GetBalancePos(out oFreePos, oOccupiedPos, oOriginOccupiedPos: TXRackPosition); override;
        function CheckBalances: boolean;
        function BalanceBlocked(aBalanceName: string): boolean;
        procedure SetBalanceType(const aSourceRackName: string); override;
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    RunGlobals,
    IntfBalanceDevice,
    ObjModul,
    Carrier,
    AppTypes,
    ErrorManager,
    RackDataAdaptor,
    CarrierSlot,
    LayoutDataAdaptor,
    SamGlobe,
    PosInfoDataAdaptor,
    PeripheryManager,
    GUIManagerRun;

{ TRunLayout }

function TRunLayout.FindBalanceRack(): TRack;
var
    xBalance: IBalanceDevice;
begin
    result := nil;
    xBalance := gModules.FindBalance;
    if (xBalance = nil) then
        EXIT;

    result := FindRackByName(xBalance.Name, false);
end;

function TRunLayout.FindAllBalancePositions(): TArray<TXRackPosition>;
var
    x: integer;
    xBalanceRack: TRack;
begin
    xBalanceRack := FindBalanceRack();
    if not Assigned(xBalanceRack) then
        EXIT;

    SetLength(result, xBalanceRack.TubeCount);

    for x := 0 to high(result) do
        result[x] := gmMakeXRackPos(xBalanceRack, x + 1);
end;

procedure TRunLayout.GetBalancePos(out oFreePos, oOccupiedPos, oOriginOccupiedPos: TXRackPosition);
var
    x: integer;
    xPos: TXRackPosition;
    iOriginPos: TRackIDPosition;
    xDataAdaptor: TPosinfoDataAdaptor;
    xBalPositions: TArray<TXRackPosition>;
begin
    xDataAdaptor := TPosinfoDataAdaptor.Create();
    try
        oFreePos.Pos := 0;
        oOccupiedPos.Pos := 0;
        oOriginOccupiedPos.Pos := 0;
        oFreePos.Rack := nil;
        oOccupiedPos.Rack := nil;
        oOriginOccupiedPos.Rack := nil;

        xBalPositions := FindAllBalancePositions();

        for x := 0 to high(xBalPositions) do
        begin
            xPos := xBalPositions[x];
            iOriginPos := xDataAdaptor.GetTubeOrigin(gmXRackPosToRackIDPos(xPos), LayoutRunName);

            // if no origin ( balance position is unoccupied ) and freepos has not been set yet, then set freepos
            if (iOriginPos.Pos = 0) and (oFreePos.Pos = 0) then
            begin
                oFreePos := xPos;
            end;
            // if there is an origin (this balance pos is occupied) and Occupied pos has not been set yet, then set occupied and orignoccupied
            if (iOriginPos.Pos <> 0) and (oOccupiedPos.Pos = 0) then
            begin
                oOccupiedPos := xPos;
                oOriginOccupiedPos := self.FindXRackPos(iOriginPos);
            end;
        end;
    finally
        xDataAdaptor.Free;
    end;
end;

function TRunLayout.CheckBalances: boolean;
// Result: Stehen Tubes auf der Waage
var
    FreePos, OccupiedPos, OriginOccupiedPos: TXRackPosition;
begin
    result := false;
    GetBalancePos(FreePos, OccupiedPos, OriginOccupiedPos);
    if (OccupiedPos.Pos <> 0) and (not gErrorManager.IsGlobalErr) then
        result := true;
end;

procedure TRunLayout.SetBalanceType(const aSourceRackName: string);
var
    xTypeName, xSourceRackType: string;
    xBalanceRack: TRack;
    xSourceRack: TRack;
    xDA: TRackDataAdaptor;
    xIsTypeFound: boolean;
    xRackTypeRec: TRackRec;
begin
    xBalanceRack := FindBalanceRack();
    if not Assigned(xBalanceRack) then
        EXIT;

    xTypeName := '';
    xSourceRack := self.FindRackByName(aSourceRackName, true);
    xSourceRackType := xSourceRack.TypeName;
    xDA := TRackDataAdaptor.Create();
    try
        xIsTypeFound := xDA.ReadRack('bl_' + xSourceRackType, xRackTypeRec);
        if not xIsTypeFound then
        begin
            xIsTypeFound := xDA.ReadRack('BL_' + xSourceRackType, xRackTypeRec);
        end;

        if xIsTypeFound then
        begin
            xBalanceRack.SetType(xRackTypeRec);
        end
        else
        begin
            xDA.ReadRack(xBalanceRack.OrigignalTypeName, xRackTypeRec);
            xBalanceRack.SetType(xRackTypeRec);
        end;
    finally
        xDA.Free;
    end;
end;

function TRunLayout.BalanceBlocked(aBalanceName: string): boolean;
var
    xBalanceCarrier: TCarrier;
    x: integer;
    xSlot: TCarrierSlot;
    xRack: TRack;
begin
    result := false;

    xBalanceCarrier := FindCarrierByName(aBalanceName);
    if not(xBalanceCarrier is TCarrier) then
        EXIT;

    for x := 1 to xBalanceCarrier.SlotCount do
    begin
        xSlot := xBalanceCarrier.GetSlotBySlotNr(x);
        if not Assigned(xSlot.Rack) then
            CONTINUE;
        xRack := xSlot.Rack as TRack;
        if (xRack.Structure.BlockBalanceDoor) then
        begin
            result := true;
            BREAK;
        end;
    end;
end;

procedure TRunLayout.ChangeRackID(aRack: TRack; const aNewID: string);
var
    xOldRackID: string;
    xLayoutDA: TLayoutDataAdaptor;
begin
    xLayoutDA := TLayoutDataAdaptor.Create;
    try
        if not xLayoutDA.RackExists(fRunName, fLayoutName, aRack.Name) then
            EXIT;
        // ------------------------------------------------------------------- Schreiben in die Datenbank
        xLayoutDA.WriteRackID(fRunName, fLayoutName, aRack.Name, aNewID, xOldRackID);
    finally
        xLayoutDA.Free;
    end;

    // ---------------------------------------------- Sämtliche POSINFO-Einträge werden umgeschrieben
    if ((g24hMode > 0) or (gChangePosinfo)) and (xOldRackID <> '') then
        TPosinfoDataAdaptor.InstChangeRackID(xOldRackId, aNewID);

    // call inherited to write aNewID to RackID field of rack
    inherited;
end;

procedure TRunLayout.UpdateCarrierRunType(const aCarrierName, aNewCarrierType: string);
var
    xDA: TLayoutDataAdaptor;
begin
    xDA := TLayoutDataAdaptor.Create();
    try
        xDA.UpdateCarrierRunType(fRunName, aCarrierName, aNewCarrierType);
    finally
        xDA.Free;
    end;
end;

procedure TRunLayout.UpdateRackRunType(const aRackName, aNewRackType: string);
var
    xDA: TLayoutDataAdaptor;
begin
    xDA := TLayoutDataAdaptor.Create();
    try
        xDA.UpdateRackRunType(fRunName, aRackName, aNewRackType);
    finally
        xDA.Free;
    end;
end;

procedure TRunLayout.UpdateRackRunData(aRack: TRack);
var
    xDA: TLayoutDataAdaptor;
    xSlot: TCarrierSlot;
begin
    xDA := TLayoutDataAdaptor.Create();
    try
        if not xDA.RackExists(fRunName, '', aRack.Name) then
            raise Exception.Create(TLanguageString.Read('Rack {0} not found in {1}!',
                'Rack {0} wurde in {1} nicht gefunden!', [aRack.Name, 'Layout']));

        ASSERT(aRack.Slot is TCarrierSlot, 'Rack has no slot');
        xSlot := aRack.Slot as TCarrierSlot;
        xDA.WriteSlot(fRunName, '', aRack.Name, (xSlot.Carrier as TCarrier).Name, xSlot.SlotNr,
            TRack.GetRotationDegree(aRack.RackRotation));

    finally
        xDA.Free;
    end;
end;


end.
