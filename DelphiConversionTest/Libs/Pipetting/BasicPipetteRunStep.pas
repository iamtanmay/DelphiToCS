{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  25.04.12 wl                                        TN5878   initial revision
  10.12.12 wl  IsFinalPip                            TN6049   entfernt
  11.04.13 wl                                        TN6128   TCriteriaArray ersetzt durch TArray<string>
  30.04.13 wl                                        TN6139.2 Neue Option WashAfterLastDispense
  12.06.13 wl                                        TN6172   Neue Optionen PipetteListName und PipetteListSeq (Array)
  23.10.13 wl  TBasicRunStepWrapper                  TN6276   neu
  ----------------------------------------------------------------------------------------------------------------------- }

unit BasicPipetteRunStep;


interface


uses
    RackTypes,
    Streamable,
    IntfArmDevice,
    RunStep,
    MethodTypes,
    LiqHTypes,
    AppTypes,
    MethodStepSettingRunStart,
    GeneralTypes,
    RunStepBuilderHelper,
    BasicPipetteTypes,
    RunStepBuilder,
    LiqHDataCache;

type
    TBasicPipetteRunStep = class(TMultiTipSubRunStep)
    private
        function GetIsMultiPip: boolean;
        function GetFieldVal(aFieldName: string): variant;
    protected
        fTipLiqDetErrorType: TTipLiqErrorType;
        fSRack: string;
        fSPos: integer;
        fDRack: string;
        fDPos: integer;
        fSourceVol: double;
        fDestVol: double;
        fDiluent: integer;
        fDilRack: string;
        fDilPos: integer;
        fDilVol: double;
        fDTransAirDisp: boolean;
        fDTransAirRetake: boolean;
        fUsedTips: TIPMAP;
        fUsedTipType: string;
        fDilSteps: integer;
        fAspS: TAspirateEvRunRec;
        fAspD: TAspirateEvRunRec;
        fDisp: TDispenseEvRunRec;
        fLiqHRec: TLiqHandlingData;
        fIsRedi: boolean;
        fRunSourcePosX: double;
        fRunSourcePosY: double;
        fRunDestPosX: double;
        fRunDestPosY: double;
        fRunDilPosX: double;
        fRunDilPosY: double;
        fWashAfterLastDispense: boolean;
        fPipetteListName: string;
        fPipetteListSeq: integer;

        function DoGetDescription(): string; override;
        function GetRestartAtStepAllowed: boolean; override;
    public
        constructor Create(); override;
        destructor Destroy(); override;

        class function GetEmptyDilRackPositions(aTipCount: integer): TDilRackPositions;
        function StepContainsAnyVolume(): boolean;
        property DilSteps: integer read fDilSteps write fDilSteps;
        property IsMultiPip: boolean read GetIsMultiPip;
        function GetFieldValsAsArray(aCriteria: TArray<string>): TArray<Variant>;
    published
        property SRack: string read fSRack write fSRack;
        property SPos: integer read fSPos write fSPos;
        property DRack: string read fDRack write fDRack;
        property DPos: integer read fDPos write fDPos;
        property SourceVol: double read fSourceVol write fSourceVol;
        property DestVol: double read fDestVol write fDestVol;
        property Diluent: integer read fDiluent write fDiluent;
        property DilRack: string read fDilRack write fDilRack;
        property DilPos: integer read fDilPos write fDilPos;
        property DilVol: double read fDilVol write fDilVol;
        property LiqHRec: TLiqHandlingData read fLiqHRec write fLiqHRec;
        property IsRedi: boolean read fIsRedi write fIsRedi;
        property DTransAirDisp: boolean read fDTransAirDisp write fDTransAirDisp;
        property DTransAirRetake: boolean read fDTransAirRetake write fDTransAirRetake;
        property UsedTips: TIPMAP read fUsedTips write fUsedTips;
        property UsedTipType: string read fUsedTipType write fUsedTipType;
        property AspS: TAspirateEvRunRec read fAspS write fAspS;
        property AspD: TAspirateEvRunRec read fAspD write fAspD;
        property Disp: TDispenseEvRunRec read fDisp write fDisp;
        property RunSourcePosX: double read fRunSourcePosX write fRunSourcePosX;
        property RunSourcePosY: double read fRunSourcePosY write fRunSourcePosY;
        property RunDestPosX: double read fRunDestPosX write fRunDestPosX;
        property RunDestPosY: double read fRunDestPosY write fRunDestPosY;
        property RunDilPosX: double read fRunDilPosX write fRunDilPosX;
        property RunDilPosY: double read fRunDilPosY write fRunDilPosY;
        property WashAfterLastDispense: boolean read fWashAfterLastDispense write fWashAfterLastDispense;
        property PipetteListName: string read fPipetteListName write fPipetteListName;
        property PipetteListSeq: integer read fPipetteListSeq write fPipetteListSeq;
    end;

    TBasicPipetteRunStepCopyEvent = function(const aStep: TBasicPipetteRunStep)
        : TBasicPipetteRunStep of object;

    TBasicPipetteRunStepBuilder = class(TRunStepByMethodStepBuilder)
    protected
        procedure AddXYPosToRunSteps(aRunStep: TBasicPipetteRunStep; aHelper: TRunStepBuilderHelper);
        class procedure GetUsedTips(const aLiqHElement: TLiqHCacheElement; aHelper: TRunStepBuilderHelper;
            const aDesiredTips: TIPMAP; var vPipDeviceName: string; out oAllowedTips: TIPMAP;
            out oUsedTipType: string);
    end;

    TCombinedBasicPipetteRunStep = class(TCompositeRunStep)
    private
        fUsedArm: IArmDevice;
        fRackPositions: TPipetteRackPositions;
        fIsMultiPip: boolean;
        fUsedTipType: string;
        fUsedTips: TIPMAP;

        fAspS: TAspirateEvRunRec;
        fAspD: TAspirateEvRunRec;
        fDisp: TDispenseEvRunRec;

        function GetPipStepAt(aIndex: integer): TBasicPipetteRunStep;

        function GetHigestTipNumber: integer;
        function VolumeExists(const aVolType: TPipetteVolType): boolean;
        function VolsToTipVolArray(const aVolType: TPipetteVolType): TDoubleArray;
        function VolsToStr(const aVolType: TPipetteVolType): string;
        function PosStepArrayToStr(const aVolType: TPipetteVolType): string;
        function RacksToStr(const aVolType: TPipetteVolType): string;
        function RackPositionsToStr(const aVolType: TPipetteVolType): string;
        function GetWashAfterLastDispense: boolean;
    protected
        function DoGetDescription: string; override;

        function GetRestartAtStepAllowed: boolean; override;
    public
        constructor Create(); override;

        procedure SetRackPositions(const aValue: TPipetteRackPositions; aRunStepCount: integer);

        property UsedArm: IArmDevice read fUsedArm write fUsedArm;
        property UsedTipType: string read fUsedTipType write fUsedTipType;
        property UsedTips: TIPMAP read fUsedTips write fUsedTips;
        property RackPositions: TPipetteRackPositions read fRackPositions;
        property IsMultiPip: boolean read fIsMultiPip write fIsMultiPip;
        property AspS: TAspirateEvRunRec read fAspS write fAspS;
        property AspD: TAspirateEvRunRec read fAspD write fAspD;
        property Disp: TDispenseEvRunRec read fDisp write fDisp;
        property WashAfterLastDispense: boolean read GetWashAfterLastDispense;

        property this[aIndex: integer]: TBasicPipetteRunStep read GetPipStepAt; default;
    end;


implementation


uses
    Classes,
    Variants,
    SysUtils,
    ArrayUtils,
    IntfPipDevice,
    PipDeviceManager,
    TipMapUtils;

{ TBasicPipetteRunStep }

constructor TBasicPipetteRunStep.Create;
begin
    inherited Create();
    PipDeviceName := '';
    SRack := '';
    SPos := 0;
    DRack := '';
    DPos := 0;
    SourceVol := 0;
    DestVol := 0;
    Diluent := 0;
    DilRack := '';
    DilPos := 0;
    DilVol := 0;
    fLiqHRec := TLiqHandlingData.Create;
    fDTransAirDisp := true;
    fDTransAirRetake := false;
    fDilSteps := 0;
    fTip := -1;
    fAspS := TAspirateEvRunRec.Create();
    fAspD := TAspirateEvRunRec.Create();
    fDisp := TDispenseEvRunRec.Create();
    fRunSourcePosX := 0;
    fRunSourcePosY := 0;
    fRunDestPosX := 0;
    fRunDestPosY := 0;
    fRunDilPosX := 0;
    fRunDilPosY := 0;
    fWashAfterLastDispense := false;
end;

destructor TBasicPipetteRunStep.Destroy();
begin
    FreeAndNil(fLiqHRec);
    FreeAndNil(fAspS);
    FreeAndNil(fAspD);
    FreeAndNil(fDisp);
    inherited;
end;

function TBasicPipetteRunStep.DoGetDescription: string;
// var
// xDilDescription: string;
begin
    // Braucht man das ??

    // xDilDescription := '';

    { if DilVol > 0 then
      begin
      xDilDescription := ', Dilution Vol '+DilVol;
      if not SameText(DilRack, 'SYSTEM') then
      xDilDescription :=xDilDescription +', Dilution Rack ' + DilRack+', Dilution Pos '+IntToStr( DilPos);
      end;
      result := TTypeSafeFormat.Format
    }// ('Device {0}, UsedTips {1}, Source Rack {2}, Pos {3}, Dest Rack {4}, Pos {5}, Vol {6} Liq Param {7}',
    // [PipDeviceName, UsedTips, SRack, SPos, DRack, DPos, DestVol, fLiqHRec.PARAMNAME]);

    // result := result + xDilDescription;

    // if (fDTransAirRetake) then
    // result := result + ', Retake Transport Air';
    EXIT('');
end;

class function TBasicPipetteRunStep.GetEmptyDilRackPositions(aTipCount: integer): TDilRackPositions;
begin
    result.DilRackType := drNormal;
    result.Positions := TXRackPositionUtils.GetEmptyXRackPositions(aTipCount);
end;

function TBasicPipetteRunStep.GetIsMultiPip: boolean;
begin
    result := fLiqHRec.SampleAspMultipip;
end;

function TBasicPipetteRunStep.GetRestartAtStepAllowed: boolean;
begin
    result := true;
    // usually if sourcevol = 0 it means that this is a disp step
    if self.IsMultiPip then
    begin
        if (self.DestVol > 0) and (self.SourceVol <= 0) then
        begin
            result := false;
        end;
    end;
end;

function TBasicPipetteRunStep.StepContainsAnyVolume: boolean;
begin
    result := (self.SourceVol > 0) or (self.DestVol > 0) or (self.DilVol > 0);
end;

function TBasicPipetteRunStep.GetFieldVal(aFieldName: string): variant;
begin
    if aFieldName = 'LIQPARAM' then
        EXIT(self.LiqHRec.PARAMNAME);
    if aFieldName = 'PIPDEVICE' then
        EXIT(self.PipDeviceName);
    if aFieldName = 'USEDTIPS' then
        EXIT(self.UsedTips);
    if aFieldName = 'SOURCERACK' then
        EXIT(self.SRack);
    if aFieldName = 'SOURCEPOS' then
        EXIT(self.SPos);
    if aFieldName = 'SOURCEVOL' then
        EXIT(self.SourceVol);
    if aFieldName = 'DESTRACK' then
        EXIT(self.DRack);
    if aFieldName = 'DESTPOS' then
        EXIT(self.DPos);
    if aFieldName = 'DESTVOL' then
        EXIT(self.DestVol);
    if aFieldName = 'DILVOL' then
        EXIT(self.DilVol);
    if aFieldName = 'RUNSOURCEPOSX' then
        EXIT(self.RunSourcePosX);
    if aFieldName = 'RUNSOURCEPOSY' then
        EXIT(self.RunSourcePosy);
    if aFieldName = 'RUNDESTPOSX' then
        EXIT(self.RunDestPosX);
    if aFieldName = 'RUNDESTPOSY' then
        EXIT(self.RunDestPosY);
    if aFieldName = 'RUNDILPOSX' then
        EXIT(self.RunDilPosX);
    if aFieldName = 'RUNDILPOSY' then
        EXIT(self.RunDilPosY);

    raise Exception.CreateFmt('Field [%s] not found', [aFieldName]);
end;

function TBasicPipetteRunStep.GetFieldValsAsArray(aCriteria: TArray<string>): TArray<Variant>;
var
    i: integer;
begin
    SetLength(result, high(aCriteria) + 1);
    for i := 0 to high(aCriteria) do
    begin
        result[i] := self.GetFieldVal(aCriteria[i]);
    end;
end;

{ TBasicPipetteRunStepBuilder }

procedure TBasicPipetteRunStepBuilder.AddXYPosToRunSteps(aRunStep: TBasicPipetteRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xPosX, xPosY: double;
begin
    if aHelper.GetXYByRackPosition(aRunStep.SRack, aRunStep.SPos, xPosX, xPosY) then
    begin
        aRunStep.RunSourcePosX := xPosX;
        aRunStep.RunSourcePosY := -xPosY;
    end;
    if aHelper.GetXYByRackPosition(aRunStep.DRack, aRunStep.DPos, xPosX, xPosY) then
    begin
        aRunStep.RunDestPosX := xPosX;
        aRunStep.RunDestPosY := -xPosY;
    end;
    if aHelper.GetXYByRackPosition(aRunStep.DilRack, aRunStep.DilPos, xPosX, xPosY) then
    begin
        aRunStep.RunDilPosX := xPosX;
        aRunStep.RunDilPosY := -xPosY;
    end;
end;

class procedure TBasicPipetteRunStepBuilder.GetUsedTips(const aLiqHElement: TLiqHCacheElement;
    aHelper: TRunStepBuilderHelper; const aDesiredTips: TIPMAP; var vPipDeviceName: string;
    out oAllowedTips: TIPMAP; out oUsedTipType: string);
var
    xDesiredDeviceName: string;
    xDesiredTips: TIPMAP;
    xPipDevice: IPipDevice;
begin
    if (vPipDeviceName <> '') then
    begin
        xDesiredDeviceName := vPipDeviceName;
        xDesiredTips := aDesiredTips;
        oUsedTipType := '';
    end
    else
    begin
        xDesiredDeviceName := aLiqHElement.Rec.UsedPipDevice;
        xDesiredTips := aLiqHElement.Rec.UsedTips;
        oUsedTipType := aLiqHElement.Rec.UsedTipType;
    end;

    xPipDevice := gPipDeviceManager.FindPipDevice_ByName(xDesiredDeviceName);
    if (not Assigned(xPipDevice)) then
        raise Exception.Create(TLanguageString.Read('PipDevice {0} could not be found!',
            'Das PipDevice {0} konnte nicht gefunden werden!', [xDesiredDeviceName]));

    oAllowedTips := xPipDevice.DetermineUsableTips(xDesiredTips);
    if (oAllowedTips = 0) then
        raise Exception.Create(TLanguageString.Read('The tips could not be found! (Tip numbers: {0})',
            'Die Pipettiernadeln konnten nicht gefunden werden. (Tipnummern: {0})',
            [TTipmapUtils.TipMapToStr(xDesiredTips)]));

    vPipDeviceName := xPipDevice.Name;
end;

{ TCombinedPipetteRunStep }

constructor TCombinedBasicPipetteRunStep.Create;
begin
    inherited Create(false);
    fUsedArm := nil;
    fUsedTipType := '';
    fUsedTips := TTipMapUtils.EmptyTipMap;
end;

function TCombinedBasicPipetteRunStep.GetPipStepAt(aIndex: integer): TBasicPipetteRunStep;
begin
    result := ( inherited GetStepAt(aIndex)) as TBasicPipetteRunStep
end;

function TCombinedBasicPipetteRunStep.GetRestartAtStepAllowed: boolean;
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
    begin
        if (not self[x].RestartAtStepAllowed) then
            EXIT(false);
    end;
    EXIT(true);
end;

function TCombinedBasicPipetteRunStep.GetWashAfterLastDispense: boolean;
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
    begin
        if (self[x].WashAfterLastDispense) then
            EXIT(true);
    end;
    EXIT(false);
end;

procedure TCombinedBasicPipetteRunStep.SetRackPositions(const aValue: TPipetteRackPositions;
    aRunStepCount: integer);
var
    xRunStepIndex: integer;
begin
    fRackPositions.Source := TXRackPositionUtils.GetEmptyXRackPositions(aRunStepCount);
    fRackPositions.Dest := TXRackPositionUtils.GetEmptyXRackPositions(aRunStepCount);
    fRackPositions.Dil.Positions := TXRackPositionUtils.GetEmptyXRackPositions(aRunStepCount);

    for xRunStepIndex := 0 to aRunStepCount - 1 do
    begin
        fRackPositions.Source[xRunStepIndex].Rack := aValue.Source[xRunStepIndex].Rack;
        fRackPositions.Source[xRunStepIndex].Pos := aValue.Source[xRunStepIndex].Pos;
        fRackPositions.Dest[xRunStepIndex].Rack := aValue.Dest[xRunStepIndex].Rack;
        fRackPositions.Dest[xRunStepIndex].Pos := aValue.Dest[xRunStepIndex].Pos;
        fRackPositions.Dil.Diluent := aValue.Dil.Diluent;
        fRackPositions.Dil.DilRackType := aValue.Dil.DilRackType;
        fRackPositions.Dil.Positions[xRunStepIndex].Rack := aValue.Dil.Positions[xRunStepIndex].Rack;
        fRackPositions.Dil.Positions[xRunStepIndex].Pos := aValue.Dil.Positions[xRunStepIndex].Pos;
    end;
end;

function TCombinedBasicPipetteRunStep.GetHigestTipNumber(): integer;
// we dont wanna use any pipdevice objects here, we have to get the information by only using the steps

var
    x: integer;
    xCurrentTip: integeR;
    xHighestTipIndex: integer;
begin
    xHighestTipIndex := -1;
    for x := 0 to self.Count - 1 do
    begin
        xCurrentTip := self[x].Tip;
        if xCurrentTip > xHighestTipIndex then
            xHighestTipIndex := xCurrentTip;
    end;

    result := xHighestTipIndex + 1;
end;

function TCombinedBasicPipetteRunStep.PosStepArrayToStr(const aVolType: TPipetteVolType): string;
var
    x: integer;
    xPosArray: TIntArray;
    xPos: integer;
    xTipCount: integer;
begin
    result := '';
    xTipCount := self.GetHigestTipNumber();
    xPosArray := TArrayUtils.GetNullIntArray(xTipCount);

    for x := 0 to self.Count - 1 do
    begin
        xPos := 0;
        case aVolType of
            pvtSource:
                xPos := self[x].SPos;
            pvtDest:
                xPos := self[x].DPos;
            pvtDil:
                xPos := self[x].DilPos;
        end;
        xPosArray[self[x].Tip] := xPos;
    end;
    EXIT(TArrayUtils.ArrayToBracketText(xPosArray));
end;

function TCombinedBasicPipetteRunStep.RacksToStr(const aVolType: TPipetteVolType): string;
var
    x: integer;
    xRack, xLastRack: string;
    xRackArray: TArray<string>;
    xTipCount: integer;
    xDifferentRacks: boolean;
begin
    result := '';
    xTipCount := self.GetHigestTipNumber();
    xLastRack := '';
    xDifferentRacks := false;
    SetLength(xRackArray, xTipCount);

    for x := 0 to self.Count - 1 do
    begin
        xRack := '';
        case aVolType of
            pvtSource:
                xRack := self[x].SRack;
            pvtDest:
                xRack := self[x].DRack;
            pvtDil:
                xRack := self[x].DilRack;
        end;
        xRackArray[self[x].Tip] := xRack;

        if (xRack <> '') then
        begin
            // werden verschiedene Racks benutzt?
            if (xLastRack <> '') and (xLastRack <> xRack) then
                xDifferentRacks := true;

            xLastRack := xRack;
        end;
    end;
    if xDifferentRacks then
        EXIT(TArrayUtils.ArrayToBracketText(xRackArray))
    else
        EXIT('[' + xLastRack + ']');
end;

function TCombinedBasicPipetteRunStep.RackPositionsToStr(const aVolType: TPipetteVolType): string;
begin
    EXIT(self.RacksToStr(aVolType) + ' Pos-' + self.PosStepArrayToStr(aVolType));
end;

function TCombinedBasicPipetteRunStep.VolsToTipVolArray(const aVolType: TPipetteVolType): TDoubleArray;
var
    x: integer;
    xVol: extended;
    xTipCount: integer;
begin
    xTipCount := self.GetHigestTipNumber();
    result := TArrayUtils.GetNullDoubleArray(xTipCount);

    for x := 0 to self.Count - 1 do
    begin
        xVol := 0;
        case aVolType of
            pvtSource:
                xVol := self[x].SourceVol;
            pvtDest:
                xVol := self[x].DestVol;
            pvtDil:
                xVol := self[x].DilVol;
        end;
        result[self[x].Tip] := xVol;
    end;
end;

function TCombinedBasicPipetteRunStep.VolumeExists(const aVolType: TPipetteVolType): boolean;
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
    begin
        case aVolType of
            pvtSource:
                if (self[x].SourceVol > 0) then
                    EXIT(true);
            pvtDest:
                if (self[x].DestVol > 0) then
                    EXIT(true);
            pvtDil:
                if (self[x].DilVol > 0) then
                    EXIT(true);
        end;
    end;
    EXIT(false)
end;

function TCombinedBasicPipetteRunStep.VolsToStr(const aVolType: TPipetteVolType): string;
var
    xTipArray: TDoubleArray;
begin
    xTipArray := VolsToTipVolArray(aVolType);

    result := TArrayUtils.ArrayToBracketText(xTipArray);
end;

function TCombinedBasicPipetteRunStep.DoGetDescription: string;
var
    xFirstStep: TBasicPipetteRunStep;
    xDestVolExists, xDilVolExists: boolean;
begin
    result := '';
    ASSERT(self.Count > 0);
    xFirstStep := self[0];
    ASSERT(Assigned(xFirstStep));
    result := 'Device ' + xFirstStep.PipDeviceName + ' Param ' + xFirstStep.LiqHRec.PARAMNAME;

    if VolumeExists(pvtSource) then
    begin
        result := result + ', Source: ' + self.RackPositionsToStr(pvtSource) + ', Vol-' +
            VolsToStr(pvtSource);
    end;

    xDestVolExists := VolumeExists(pvtDest);
    xDilVolExists := VolumeExists(pvtDil);
    if xDilVolExists then
    begin
        if SameText(xFirstStep.DilRack, 'SYSTEM') then
            result := result + ', System Vol-' + VolsToStr(pvtDil)
        else
            result := result + ', Diluent: ' + self.RackPositionsToStr(pvtDil) + ', DilVol-' +
                VolsToStr(pvtDil);
    end;
    if xDilVolExists or xDestVolExists then
    begin
        result := result + ' --> ' + self.RackPositionsToStr(pvtDest);
    end;
    if xDestVolExists then
    begin
        result := result + ', Vol-' + VolsToStr(pvtDest);
    end;
end;


end.
