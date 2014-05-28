{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : creates operations using devices and workbench objects
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                  track-no improvement/change
  -------- --  -------------------------------------   -------- --------------------------------------------------------
  OperationPip.pas:
  18.04.06 pk  CalculateXYStepList                     TN2958    Bug fixed. Use xstep tipmap for calculating ysteps
  24.08.06 wl  SetXYByRackPos                          TN3271    Positionen werden mit aRack.PaintTubePos eingefärbt
  14.02.07 wl  TPipStepXYCalculator.CalculateXYStep    TN3147    Result ist jetzt immer nil (und nicht irgendetwas), wenn Position nicht existiert
  07.03.07 wl                                          TN3620    uses geändert
  09.11.07 pk                                          TN3924    Steps changed to mm
  07.01.08 pk  CalcZProperty                           TN3971    use AddZHeight to add ZHeights
  15.05.08 wl                                          TN4100    TXYStepList property changed
  02.07.08 pk  FillTubeDataZ                           TN4139    CalcHighestZPos
  03.07.08 wl                                          TN4157
  17.09.08 wl                                          TN4224   uses TipsetExt entfernt
  12.09.09 wl                                          TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  12.09.09 wl  CalcStepZPosFromRackPos                 TN4740   Arrays werden zunächst mit SetLength initialisiert
  29.03.11 wl  TPipStepXYZCalculatorFactory            TN5524   PipStepCalculation wird damit erzeugt
  ----------------------------
  MotorStepCalculator.pas:
  29.03.11 wl                                          TN5524   unit umbenannt
  12.10.11 ts  CalculateXYStep/-List                   TN5711   if aAddTipOffset is true, Offsets from TipType will be added
  20.09.11 wl  SetXYByRackPos                          TN5723   Kein PaintPosition mehr
  02.02.11 wl  TMotorXYStepCalculator                  TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  28.09.12 ts                                          TN5987   new: MoveVarispan, Möglichkeit Varispan-Bewegung abzuschalten (für XYMOV-Action)
  ---------------------------------------------------------------------------------------------------------------------- }

unit MotorStepCalculator;


interface


uses
    AppTypes,
    CommonTypes,
    RackTypes,
    Rack,
    GeneralTypes,
    IntfMotorDevice,
    IntfMultiYMotorDevice,
    IntfMultiZMotorDevice,
    IntfPipDevice,
    IntfArmDevice;

type
    TMotorStepCalculator = record
    public
        class procedure GetTubeDataXY(aRack: TRack; aPos: integer; out oXPosTube, oYPosTube: TPosMM); static;
    end;

    TMotorXYStepCalculator = class
    protected
        fX: IXMotorDevice;
        fY: IMultiYMotorDevice;
        fMotorMap: TIPMAP;
        fPipDevice: IPipDevice;
        fTubeX, fTubeY: TDoubleArray;

        procedure CalcXSteps(aMotorMap: TIPMAP; const aX: TDoubleArray; aResultList: TXStepList);
        procedure AddTipOffset;
    public
        constructor Create(aMotorMap: TIPMAP; aPipDevice: IPipDevice; aXMotor: IXMotorDevice;
            aYMotors: IMultiYMotorDevice);

        procedure CalculateXYStepList(aResultList: TXYStepList; aAddTipOffset: boolean;
            aMoveVarispan: boolean = true);
        function CalculateXYStep(aAddTipOffset: boolean): TXYStep;
        function GetMotorMapsFromSteps(aXYSteps: TXYStepList): TMotorMapArray;
        procedure SetXYByRackPos(aRP: TArray<TXRackPosition>);
        procedure SetXYBySinglePos(aX, aY: TPosMM);
    end;

    TMotorXYZStepCalculator = class(TMotorXYStepCalculator)
    protected
        fZ: IMultiZMotorDevice;
        procedure FillTubeDataZ(aIndex: integer; aRack: TRack; aPos: integer;
            var vZPositions: TPipZPositions);
    public
        constructor Create(aMotorMap: TIPMAP; aPipDevice: IPipDevice; aXMotor: IXMotorDevice;
            aYMotors: IMultiYMotorDevice; aZMotors: IMultiZMotorDevice);
        function CalcStepZPosFromRackPos(aMotorMap: TIPMAP; aRP: TArray<TXRackPosition>): TPipStepZPos;
        function CalcStepsZPosFromRackPos(aMotorMaps: TMotorMapArray; aRP: TArray<TXRackPosition>)
            : TPipStepZPosArray;
    end;

    TPipPosCheck = class
    private
        fX: IXMotorDevice;
        fY: IMultiYMotorDevice;
        fPipDevice: IPipDevice;
        fTubeX, fTubeY: TPosMM;
        procedure AddTipOffset(aTipIndex: integer);
    public
        constructor Create(aXMotor: IXMotorDevice; aYMotors: IMultiYMotorDevice; aPipDevice: IPipDevice);
        function IsPosReachable(aTipIndex: integer): boolean;
        procedure SetXYByRackPos(aTipIndex: integer; aRack: TRack; aPos: integer);
        procedure SetXYBySinglePos(aTipIndex: integer; aX, aY: TPosMM);
    end;

    TMotorStepCalculatorFactory = record
    public
        class function CreateMotorXYZStepCalc(aUsedArm: IArmDevice; aMotorMap: TIPMAP)
            : TMotorXYZStepCalculator; static;
        class function CreatePipPosCheck(aUsedArm: IArmDevice): TPipPosCheck; static;
    end;


implementation


uses
    SysUtils,
    SamGlobe,
    PosinfoDataAdaptor,
    AppSettings,
    GeometricClasses,
    TipMapUtils,
    Carrier,
    ArrayUtils,
    IntfMotorBasedMotionDevice;

{ TMotorStepCalculator }

class procedure TMotorStepCalculator.GetTubeDataXY(aRack: TRack; aPos: integer;
    out oXPosTube, oYPosTube: TPosMM);
var
    xTubePos: TGeom3D_Position;
begin

    // Tube-Daten für Position errechnen
    xTubePos := aRack.CreateTubePos(aPos);

    oXPosTube := xTubePos.X;
    oYPosTube := xTubePos.Y;

    xTubePos.Free;
end;

{ TPipPosCheck }

constructor TPipPosCheck.Create(aXMotor: IXMotorDevice; aYMotors: IMultiYMotorDevice; aPipDevice: IPipDevice);
begin
    inherited Create();
    fX := aXMotor;
    fY := aYMotors;
    fPipDevice := aPipDevice;
end;

procedure TPipPosCheck.AddTipOffset(aTipIndex: integer);
begin
    fTubeX := fTubeX - fPipDevice.GetTipXOffset_mm(aTipIndex);
    fTubeY := fTubeY - fPipDevice.GetTipYOffset_mm(aTipIndex);
end;

procedure TPipPosCheck.SetXYByRackPos(aTipIndex: integer; aRack: TRack; aPos: integer);
begin
    TMotorStepCalculator.GetTubeDataXY(aRack, aPos, fTubeX, fTubeY);
end;

procedure TPipPosCheck.SetXYBySinglePos(aTipIndex: integer; aX, aY: TPosMM);
begin
    fTubeX := aX;
    fTubeY := aY
end;

function TPipPosCheck.IsPosReachable(aTipIndex: integer): boolean;
begin
    result := false;
    AddTipOffset(aTipIndex);
    if not fY.IsPosReachable(aTipIndex, fTubeY) then
        EXIT;

    if Assigned(fX) then
    begin
        if fX.IsBeyondMinOrMax(fTubeX) then
            EXIT;
    end;
    result := true;
end;

{ TMotorXYStepCalculator }

constructor TMotorXYStepCalculator.Create(aMotorMap: TIPMAP; aPipDevice: IPipDevice; aXMotor: IXMotorDevice;
    aYMotors: IMultiYMotorDevice);
begin
    inherited Create();
    fTubeX := TArrayUtils.GetNullDoubleArray(MAX_TIPS);
    fTubeY := TArrayUtils.GetNullDoubleArray(MAX_TIPS);
    fMotorMap := aMotorMap;
    fPipDevice := aPipDevice;
    fX := aXMotor;
    fY := aYMotors;
end;

procedure TMotorXYStepCalculator.SetXYByRackPos(aRP: TArray<TXRackPosition>);
var
    x: integer;
begin
    for x := 0 to high(aRP) do
    begin
        if TTipMapUtils.TipSelected(fMotorMap, x) then
        begin
            TMotorStepCalculator.GetTubeDataXY(aRP[x].Rack, aRP[x].Pos, fTubeX[x], fTubeY[x]);
        end;
    end;
end;

procedure TMotorXYStepCalculator.SetXYBySinglePos(aX, aY: TPosMM);
var
    x: integer;
begin
    for x := 0 to high(fTubeY) do
    begin
        if TTipMapUtils.TipSelected(fMotorMap, x) then
        begin
            fTubeX[x] := aX;
            fTubeY[x] := aY;
        end;
    end;
end;

procedure TMotorXYStepCalculator.AddTipOffset();
var
    x: integer;
begin
    for x := 0 to high(fTubeY) do
    begin
        if TTipMapUtils.TipSelected(fMotorMap, x) then
        begin
            fTubeX[x] := fTubeX[x] - fPipDevice.GetTipXOffset_mm(x);
            fTubeY[x] := fTubeY[x] - fPipDevice.GetTipYOffset_mm(x);
        end;
    end;
end;

procedure TMotorXYStepCalculator.CalcXSteps(aMotorMap: TIPMAP; const aX: TDoubleArray;
    aResultList: TXStepList);
var
    i, j: integer;
    xCurrentStep: TXStep;
begin
    xCurrentStep := nil;
    for i := 0 to high(aX) do
    begin
        if not TTipMapUtils.TipSelected(aMotorMap, i) then
            CONTINUE;

        if Assigned(xCurrentStep) and (aX[i] <> xCurrentStep.X) then
        begin
            xCurrentStep := nil;
            for j := 0 to aResultList.Count - 1 do
            begin
                if (aX[i] <> aResultList[j].X) then
                    CONTINUE;
                xCurrentStep := aResultList[j];
            end;
        end;

        if (not Assigned(xCurrentStep)) then
        begin
            xCurrentStep := TXStep.Create(aX[i]);
            aResultList.Add(xCurrentStep);
        end;
        xCurrentStep.AddToMotorMap(i);
    end;
end;

procedure TMotorXYStepCalculator.CalculateXYStepList(aResultList: TXYStepList; aAddTipOffset: boolean;
    aMoveVarispan: boolean);
var
    i, j: integer;
    xXSteps: TXStepList;
    xYSteps: TMultiYStepList;
begin
    if aAddTipOffset then
        AddTipOffset();
    xXSteps := TXStepList.Create();
    CalcXSteps(fMotorMap, fTubeX, xXSteps);

    for i := 0 to xXSteps.Count - 1 do
    begin
        xYSteps := TMultiYStepList.Create(false);
        fY.CalculateSteps(xXSteps[i].MotorMap, fTubeY, xYSteps, aMoveVarispan);
        for j := 0 to xYSteps.Count - 1 do
        begin
            aResultList.Add(TXYStep.Create(xXSteps[i].X, xYSteps[j]));
        end;
        FreeAndNil(xYSteps);
    end;

    FreeAndNil(xXSteps);
end;

function TMotorXYStepCalculator.CalculateXYStep(aAddTipOffset: boolean): TXYStep;
var
    xList: TXYStepList;
begin
    result := nil;
    xList := TXYStepList.Create;
    try
        CalculateXYStepList(xList, aAddTipOffset, true);
        if (xList.Count > 0) then
        begin
            result := xList[0];
            xList.Extract(result);
        end;
    finally
        FreeAndNil(xList);
    end;
end;

function TMotorXYStepCalculator.GetMotorMapsFromSteps(aXYSteps: TXYStepList): TMotorMapArray;
var
    xStepIndex: integer;
begin
    SetLength(result, aXYSteps.Count);
    for xStepIndex := 0 to high(result) do
    begin
        result[xStepIndex] := aXYSteps[xStepIndex].MotorMap;
    end;
end;

constructor TMotorXYZStepCalculator.Create(aMotorMap: TIPMAP; aPipDevice: IPipDevice; aXMotor: IXMotorDevice;
    aYMotors: IMultiYMotorDevice; aZMotors: IMultiZMotorDevice);
begin
    inherited Create(aMotorMap, aPipDevice, aXMotor, aYMotors);
    fZ := aZMotors;
end;

procedure TMotorXYZStepCalculator.FillTubeDataZ(aIndex: integer; aRack: TRack; aPos: integer;
    var vZPositions: TPipZPositions);
var
    xAddZOffset_mm: TPosMM;
    xHighestZPos_mm: TPosMM;
    xZPosTube: TPosMM;
    xTubePosition: TGeom3D_Position;
    function CalcZProperty(aHeightInMM, aPropInMM: double): TPosMM;
    begin
        result := aHeightInMM;
        result := AddZHeight(result, fPipDevice.GetTipZOffset_mm(aIndex));
        result := AddZHeight(result, -aPropInMM);
    end;

begin
    xHighestZPos_mm := aRack.CalcHighestZPos();
    xTubePosition := aRack.CreateTubePos(aPos);
    xZPosTube := xTubePosition.Z;
    FreeAndNil(xTubePosition);

    // Einrechnen der Wash-Z-Offsets: wirken sich nur auf ZDisp und ZMax aus!
    xAddZOffset_mm := 0;
    if TRackDefinitions.RackIsSpecialRack(srtDry, aRack.Name, fPipDevice.RackNamePrefix) then
        xAddZOffset_mm := fPipDevice.GetTipDryZOffset_mm(aIndex);
    if TRackDefinitions.RackIsWashOrRediWash(aRack.Name, fPipDevice.RackNamePrefix) then
        xAddZOffset_mm := fPipDevice.GetTipWashZOffset_mm(aIndex);
    if TRackDefinitions.RackIsSpecialRack(srtWaste, aRack.Name, fPipDevice.RackNamePrefix) then
        xAddZOffset_mm := fPipDevice.GetTipWasteZOffset_mm(aIndex);

    // Berechnung von Rack-Z-Travel: Rack-Z-Travel muss immer auf den höchsten Z-Wert des Racks berechnet werden
    vZPositions.ZTravel[aIndex] := CalcZProperty(xHighestZPos_mm, aRack.Structure.ZTravel_mm);

    // If Z-Travel Is beyond MinPos (0 Steps) set to MinPos
    fZ.SetToMinPosIfIsBeyondMin(aIndex, vZPositions.ZTravel[aIndex]);

    // Berechnung der übrigen Z-Werte: Bei (Slope <> 0) abhängig von der Position
    vZPositions.ZScan[aIndex] := CalcZProperty(xZPosTube, aRack.Structure.ZScan_mm);
    // Z-Scan    (absolut aus Racktyp und aktuellem Slot)
    vZPositions.ZDisp[aIndex] := CalcZProperty(xZPosTube, aRack.Structure.ZDisp_mm + xAddZOffset_mm);
    // Z-Disp    (absolut aus Racktyp und aktuellem Slot)
    vZPositions.ZMax[aIndex] := CalcZProperty(xZPosTube, aRack.Structure.ZMax_mm + xAddZOffset_mm);
    // Z-Max     (absolut aus Racktyp und aktuellem Slot)
    vZPositions.ZTube[aIndex] := CalcZProperty(xZPosTube, aRack.Structure.TubeZ_mm); // Tube-Höhe
    vZPositions.MOffset[aIndex] := CalcZProperty(xZPosTube, aRack.Structure.MOffsetZ_mm); // Z-Move-Offset

end;

function TMotorXYZStepCalculator.CalcStepZPosFromRackPos(aMotorMap: TIPMAP; aRP: TArray<TXRackPosition>)
    : TPipStepZPos;
var
    xMotorIndex: integer;
begin
    result.SyrMap := TTipMapUtils.EmptyTipMap;
    SetLength(result.Z.ZTravel, Length(aRP));
    SetLength(result.Z.ZScan, Length(aRP));
    SetLength(result.Z.ZDisp, Length(aRP));
    SetLength(result.Z.ZMax, Length(aRP));
    SetLength(result.Z.ZTube, Length(aRP));
    SetLength(result.Z.MOffset, Length(aRP));

    for xMotorIndex := 0 to high(aRP) do
    begin
        if not TTipMapUtils.TipSelected(aMotorMap, xMotorIndex) then
            CONTINUE;
        TTipMapUtils.SelectTip(result.SyrMap, xMotorIndex);
        FillTubeDataZ(xMotorIndex, aRP[xMotorIndex].Rack, aRP[xMotorIndex].Pos, result.Z);
    end;
end;

function TMotorXYZStepCalculator.CalcStepsZPosFromRackPos(aMotorMaps: TMotorMapArray;
    aRP: TArray<TXRackPosition>): TPipStepZPosArray;
var
    xStepIndex: integer;
begin
    SetLength(result, Length(aMotorMaps));
    for xStepIndex := 0 to high(aMotorMaps) do
    begin
        result[xStepIndex] := CalcStepZPosFromRackPos(aMotorMaps[xStepIndex], aRP);
    end;
end;

{ TMotorXYZStepCalculatorFactory }

class function TMotorStepCalculatorFactory.CreatePipPosCheck(aUsedArm: IArmDevice): TPipPosCheck;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := TPipPosCheck.Create(xMotorBasedMD.XMotor, xMotorBasedMD.YMotors, aUsedArm.PipDevice);
    end;
end;

class function TMotorStepCalculatorFactory.CreateMotorXYZStepCalc(aUsedArm: IArmDevice; aMotorMap: TIPMAP)
    : TMotorXYZStepCalculator;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
begin
    result := nil;

    if Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
    begin
        result := TMotorXYZStepCalculator.Create(aMotorMap, aUsedArm.PipDevice, xMotorBasedMD.XMotor,
            xMotorBasedMD.YMotors, xMotorBasedMD.ZMotors);
    end;
end;


end.
