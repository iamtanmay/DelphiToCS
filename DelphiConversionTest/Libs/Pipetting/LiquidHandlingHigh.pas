{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : High-Level Pipetting steps: Aspirate Diluent, Aspirate Sample and Dispense
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  15.04.04 wl                               TN1788   initial version
  19.04.04 wl  TLiquidStepData              TN1788   entspricht TDLLParameters
  19.04.04 wl  gmDoLiquidHandling           TN1788   neu: extrahiert aus DoPipette (DilutionManager)
  19.04.04 wl  gmMaxVal,gmSetSysAir,gmPickSysAir,gmAspirateDiluentVolume       TN1788   von DilutionManager hierher verschoben
  19.04.04 wl  gmAspirateSampleVolume,gmDispenseVolume,gmLiquidDetectionOnly   TN1788   von DilutionManager hierher verschoben
  20.04.04 wl  alle Funktionen              TN1788   Umstellung von TipStatus auf aUsedArm.Tips
  22.04.04 wl                               TN1788   uses LiquidHandlingDiTi
  23.04.04 wl  gmAspirateSampleVolume       TN1788   Log für Waste-Vol wieder aktiviert
  10.05.04 pk  gmAspirateDiluentVolume      TN1889.2 Use FindXRackPos to get the rackname and position of DilRack
  10.05.04 pk  gmAspirateDiluentVolume      TN1889   compiler warning avoided
  13.05.04 wl  gmAspirateDiluentVolume      TN1788   Aufruf von gmAspirate ohne 0(aAspPos)
  13.05.04 wl  gmAspirateSampleVolume       TN1788   Aufruf von gmAspirate ohne 0(aAspPos)
  15.05.04 pk  gmAspirateDiluentVolume      TN1889.1 uses changed version of TDilRackPositions
  17.05.04 wl  gmDoLiquidHandling           TN1933   aUseVolControl als neuer Parameter
  15.06.04 wl  gmGetAspSpeed,gmGetDispSpeed TN1963   von SamCmd hierher verschoben
  17.06.04 wl  gmDispenseVolume             TN1975   DispRetractSpeed = LH.SampleAspRetrSpeed (für Mixen)
  17.06.04 wl  gmLiquidDetectionOnly        TN1975   WashRetractSpeed wird immer auf 0 gesetzt
  07.07.04 wl                               TN2019   alle Aufrufe von gmPickDispTips: RobotArmDevice statt nur PipArmDevice
  21.07.04 pk                               TN2049   Changes needed due to changes in LiqHandling Record
  25.08.04 wl                               TN2105   ZScanspeed heißt jetzt ZInsertspeed
  08.09.04 wl                               TN2121   gmSetTipError statt global_AppIntf.SetTipError
  08.09.04 wl  gmAspirateDiluentVolume      TN2121   ruft gmPickSys auf, das enthält: gmLogText, gmPickLiquid, AppSleep, ChangeActualVol
  13.10.04 wl  gmAspirateSampleVolume       TN2179   DisableZErr wird jetzt auch wirklich ausgeführt
  13.10.04 wl                               TN2179   DisableZError: wird in gmAspirate,gmDispense durchgeführt
  13.10.04 wl  TLiquidStepData              TN2151   neu: ZRetractDistance_mm
  08.11.04 wl  DoPipette                    TN2213   TPipetteActionRunRec als Paramter
  08.11.04 wl  gmAspirateDiluent/SampleVolume TN2213   TAspirateActionRunRec als Paramter, erzeugt TExtDllCall für Events
  08.11.04 wl  gmDispenseVolume             TN2213   TDispenseActionRunRec als Paramter, erzeugt TExtDllCall für Events
  17.11.04 mo  gmDispenseVolume             TN2232  Wenn Dispensevolumen = 0 dann werden die Zielpositionen nicht angefahren
  01.02.05 wl  gmAspirateSampleVolume       TN2297.4 Log 'Aspirate..' erst nach Aufnahme der DiTis
  16.02.05 wl  gmDispenseVolume             TN2269   Update der Füllstände entfernt
  04.03.05 pk                               TN2330.1  uses LiqHDataAdaptor
  22.03.05 pk  gmAspirateDiluentVolume      TN2330.1 gmWriteMixRec replaced by TLiqHDataAdaptor.WriteMixRec
  11.04.05 pk                               TN2373   references to TRunRecord removed
  27.04.05 pk  gmDoLiquidHandling           TN2400   Better Logging
  28.04.05 wl  gmDispenseVolume             TN2383   Hack entfernt (TN2232) - Ersetzt durch gmNoDispenseVolume (LiquidHandling.pas)
  15.06.05 pk                               TN2464   logic of SumSVol and SumDilVol replaced by aAspSampleTips and aAspDilTips
  20.06.05 tbh gmPickSysAir                 TN2385   Aufrufe angepasst
  20.06.05 tbh gmAspirateDiluentVolume      TN2385   Verwendung von Dilutormotor-Schritten bei Volumenkorrektur
  20.06.05 tbh gmAspirateSampleVolume       TN2385   Verwendung von Dilutormotor-Schritten bei Volumenkorrektur
  20.06.05 tbh gmDispenseVolume             TN2385   Verwendung von Dilutormotor-Schritten bei Volumenkorrektur
  20.06.05 tbh gmDoLiquidHandling           TN2385   Aufrufe angepasst
  22.06.05 tbh gmSetSysAir                  TN2464.2 aUsedArm.Tips[cnt].St_LhPtr kein Pointer mehr
  22.06.05 tbh gmAspirateDiluentVolume      TN2464.2 aUsedArm.Tips[cnt].St_LhPtr kein Pointer mehr
  22.06.05 tbh gmAspirateSampleVolume       TN2464.2 aUsedArm.Tips[cnt].St_LhPtr kein Pointer mehr
  22.06.05 tbh gmDispenseVolume             TN2464.2 aUsedArm.Tips[cnt].St_LhPtr kein Pointer mehr
  22.06.05 tbh gmLiquidDetectionOnly        TN2464.2 aUsedArm.Tips[cnt].St_LhPtr kein Pointer mehr
  22.06.05 tbh gmAspirateSampleVolume       TN2385   Volumenkorrektur nur für benötigte Tips
  23.06.05 tbh gmAspirateSampleVolume       TN2385   Volumenkorrektur nur für existierende Pumpen
  06.07.05 pk                               TN2492   New : TotalSpitVol and SpitCount for multiple spit backs
  08.08.05 pk                               TN2527   New : TipTouch and TipTouchDelay
  26.08.05 pk  gmDoLiquidHandling           TN2566   Dispense only if DispTips > 0
  18.10.05 wl  gmSetSysAir                  TN2697.2 aUsedArm.Tips[cnt].St_ActSysAirVol kann keine negativen Werte mehr speichern
  11.01.05 pk  gmAspirateSampleVolume       TN2871.0 Set up extra gap parameters
  11.01.05 pk  gmAspirateSampleVolume       TN2871.2 St_ActWasteVol used instead of global gActualWastVol
  19.04.06 wl                               TN3051    benutzt TipFloatArray und extended für alle Vol, Speed und VolSteps
  04.09.06 pk  gmAspirateSampleVolume       TN3281   Set AirGapTaken only when count > 0
  05.09.06 pk  gmAspirate..                 TN3193   new aAspType parameter: distinguish between asp diluent and asp sample
  06.09.06 pk  gmLiquidDetectionOnly        TN3285   kann jetzt auch mit WashMethod benutzt werden
  26.09.06 wl  gmLiquidDetectionOnly        TN3326   Parameter geändert
  04.12.06 wl  gmLiquidDetectionOnly        TN3445   Parameter geändert
  19.01.07 wl  alle Funktionen              TN3473  statt TSwichDevice wird nur der Name weitergegeben
  08.03.07 wl                               TN3620   uses geändert
  18.04.07 wl  TLiquidStepData              TN3658   LIQPAR_MIXREC heißt jetzt TLiqHandlingMixRec
  30.08.07 pk                               TN3840.2 Uses LiqHTypes
  27.11.07 wl  gmAspirateSampleVolume,gmAspirateDiluentVolume  TN3901   Logs enthalten keine Volumina mehr
  27.11.07 wl  gmAspirateSampleVolume       TN3901   xSampleVol,xWasteVol,xExtraGapWasteSpeed werden initialisiert
  27.11.07 wl  gmAspirateSampleVolume       TN3897   Add. Waste Vol. wird nur noch einmal aufgenommen
  27.11.07 wl  gmAspirateSampleVolume       TN3897   WasteVol und WastePercent können jetzt sogar gleichzeitig genutzt werden
  09.01.08 wl  gmDispenseVolume             TN3972    Perivol entfernt
  18.03.08 wl  gmAspirateSampleVolume       TN4007   alle xExtraGap-Arrays werden mit 0 beschrieben sonst steht Mist drin
  28.03.08 wl  gmAspirateSampleVolume       TN4053   das Waste-Volumen wird auch bei Volumenkorrektur zu den Steps gerechnet
  20.06.08 pk                               TN4139   uses changed
  24.06.08 wl                               TN4143   uses posTools entfernt: Einige Änderungen
  03.07.08 wl                               TN4157
  16.07.08 pk                               TN4157   ZMotion.SetTipLiqDetError instead of gmSetTipError
  02.09.08 pk                               TN4215    DllCallExt changed to DllCall
  25.09.08 wl                               TN4242    TRunstCall ersetzt TDllCall
  27.11.08 wl  gmAspirateSampleVolume       TN4277   xStepsForVolCh1 was set to Max Pump Steps.
  31.07.09 wl  gmAspirateDiluentVolume      TN4049   an änderungen in WriteMixRec angepasst
  31.07.09 wl  gmAspirateDiluentVolume      TN3950   liest und übergibt neues Feld: SampleAspInsertMoveType
  31.07.09 wl  gmAspirateSampleVolume       TN3950   liest und übergibt neues Feld: DilAspInsertMoveType
  31.07.09 wl  gmDispenseVolume             TN3950   liest und übergibt neues Feld: DispInsertMoveType
  12.09.09 wl                               TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  13.04.10 wl                               TN5044   uses geändert
  19.04.10 pk                               TN5062   SendDDE removed
  23.04.10 pk                               TN5015   remove SamDDE
  29.06.10 pk                               TN5173   various changes to allow pipetting sample with second pump
  15.07.10 pk  gmAspirateDiluentVolume      TN5196   WaitAfterPickSys removed (for Stratec pumps)
  12.08.10 wl                               TN5227   benutzt TLiquidChannels
  12.08.10 wl  gmLogAspirate,gmLogDispense  TN5227   neu: sichtbare Logzeile zeigt jetzt auch die Pumpennummer
  26.08.10 ts  Aspirate                     TN5248   new: MoveToSysAirPosSpeed
  22.09.10 wl  gmAspirateSampleVolume       TN5275   ExtraGapAirPos wird übergeben
  19.07.11 wl  gmDispenseVolume             TN5630   wenn TransAirRetakeAfterDisp = true wird Transair nach Dispense wieder aufgenommen
  31.10.11 wl                               TN5731   UseVolCorr ist immer true
  03.11.11 wl                               TN5725   PaintTubes entfernt
  05.12.11 wl                               TN5758   WriteMixRec: Geänderte Parameter
  02.02.11 wl                               TN5791   verwendet TArray<TXRackPosition> statt TXRackPositions
  24.02.12 wl                               TN5818   Neue Felder für TipTouch werden übergeben
  25.04.12 wl                               TN5878   uses geändert
  04.09.12 wl  gmAspirateSampleVolume       TN5972   Neues Feld SpitBackAtAspPos wird übergeben
  02.01.13 wl  gmAspirateSampleVolume       TN6064   SampleAspMixFirstOnly ist jetzt TLiqHandlingRec zugeordnet
  02.01.13 wl  gmDispenseVolume             TN6064   Neues Feld: DispMixAspSubmerge
  30.04.13 wl                               TN6139.2 TCombinedActivePipetteRunStep umbenannt
  07.05.13 ts  gmDispenseVolume             TN6118   DispTipTouchScanStoreVol, Detektiertes Volumen kann in Posinfo gespeichert werden
  12.06.13 wl  gmDoLiquidHandling           TN6172   wenn (PipetteListName<>''), wird der aktuelle Status in die PipetteList geschrieben
  21.08.13 wl                               TN6231   uses geändert
  25.09.13 wl                               TN6259   Verbessertes Logging
  27.11.13 wl  gmAspirateDiluentVolume      TN6313   ChangePosForMultiTipRack hier entfernt (war zu spät)
  -------------------------------------------------------------------------------------------------- }

unit LiquidHandlingHigh;


interface


uses
    AppTypes,
    Rack,
    IntfArmDevice,
    RackTypes,
    MethodTypes,
    BasicPipetteTypes,
    PipetteListDataAdaptor,
    BasicPipetteRunAction;

procedure gmDoLiquidHandling(aUsedArm: IArmDevice; const aSource, aDest: TArray<TXRackPosition>;
    aDil: TDilRackPositions; aLastSysAirAspPos: integer; aDispTips, aAspSampleTips, aAspDilTips: TipMap;
    aPipRunStep: TCombinedActivePipetteRunStep; aDA: TPipetteListDataAdaptor; const aPipListName: string;
    const aPipListSeq: TArray<integer>);


implementation


uses
    SysUtils,
    Math,
    LiqHDataAdaptor,
    LiqHTypes,
    GeneralTypes,
    SamGlobe,
    LiquidHandling,
    LiquidHandlingLow,
    ObjModul,
    CommonTypes,
    EventManager,
    MethodStepSettingRunStart,
    LiquidHandlingDiti,
    ThrdMan,
    SamHigh,
    ArrayUtils,
    IntfPipDevice,
    TipSystem,
    OperationFactory,
    MotionSystemPipMove,
    LogManager,
    TipMapUtils,
    ErrorManager,
    IntfMotorBasedMotionDevice;

type
    TLiquidStepData = record
        Vol: TDoubleArray;
        TotalVol: TDoubleArray;
        Speed: TDoubleArray;
        TransAirVol: TDoubleArray;
        TransAirSpeed: TDoubleArray;
        SysAirVol: TDoubleArray;
        SysAirSpeed: TDoubleArray;
        SpitCount: integer;
        SpitVolume: TDoubleArray;
        TotalSpitVol: TDoubleArray;
        SpitSpeed: TDoubleArray;
        DAirVol: TDoubleArray;
        Ch2DispenseVol: TDoubleArray;
        Ch2TipWashVol: TDoubleArray;
        Ch2TipWashSpeed: TDoubleArray;
        Delay: integer;
        TransAirDelay: integer;
        Ch2TipWashDelay: integer;
        SysAirDelay: integer;
        LiqDet: integer;
        SysAirPos: integer;
        TransAirPos: integer;
        DspTransAirPos: integer;
        DisableZErr: boolean;
        SingleRetract: boolean;
        SubMerge_mm: TPosMM;
        SwitchPos: integer;
        SwitchModule: string;
        PerformingSteps: double;
        PerformDelay: integer;
        RecordDetectionVolume: integer;
        AspirationMethodRetractZ: integer; // Aspiration Method for small Volumes
        SampleAspRetractPos_mm: TPosMM;
        MixRec: TLiqHandlingMixRec;
        ZScanMode: integer;
        ZInsertSpeed: integer;
        ZInsertMoveType: integer;
        ZRetractSpeed: integer;
        ZRetractDistance_mm: TPosMM;
        TipTouch: boolean;
        TipTouchDelay: integer;
        TipTouchScan: boolean;
        TipTouchScanMode: integer;
        TipTouchSingle: boolean;
        TipTouchSubmerge: TArray<double>;
        TipTouchScanStoreVol: boolean;
        MoveToSysAirPosSpeed: integer;
    end;

function gmInitLiquidStepData(aPipDevice: IPipDevice): TLiquidStepData;
begin
    result.Vol := aPipDevice.GetNullDoubleArray;
    result.TotalVol := aPipDevice.GetNullDoubleArray;
    result.Speed := aPipDevice.GetNullDoubleArray;
    result.TransAirVol := aPipDevice.GetNullDoubleArray;
    result.TransAirSpeed := aPipDevice.GetNullDoubleArray;
    result.SysAirVol := aPipDevice.GetNullDoubleArray;
    result.SysAirSpeed := aPipDevice.GetNullDoubleArray;
    result.SpitCount := 0;
    result.SpitVolume := aPipDevice.GetNullDoubleArray;
    result.TotalSpitVol := aPipDevice.GetNullDoubleArray;
    result.SpitSpeed := aPipDevice.GetNullDoubleArray;
    result.DAirVol := aPipDevice.GetNullDoubleArray;
    result.Ch2DispenseVol := aPipDevice.GetNullDoubleArray;
    result.Ch2TipWashVol := aPipDevice.GetNullDoubleArray;
    result.Ch2TipWashSpeed := aPipDevice.GetNullDoubleArray;
    result.Delay := 0;
    result.TransAirDelay := 0;
    result.Ch2TipWashDelay := 0;
    result.SysAirDelay := 0;
    result.LiqDet := 0;
    result.SysAirPos := 0;
    result.TransAirPos := 0;
    result.DspTransAirPos := 0;
    result.DisableZErr := false;
    result.SingleRetract := false;
    result.SubMerge_mm := 0.0;
    result.SwitchPos := 0;
    result.SwitchModule := '';
    result.PerformingSteps := 0.0;
    result.PerformDelay := 0;
    result.RecordDetectionVolume := 0;
    result.AspirationMethodRetractZ := 0; // Aspiration Method for small Volumes
    result.SampleAspRetractPos_mm := 0.0;
    // result.MixRec                   : TLiqHandlingMixRec;
    result.ZScanMode := 0;
    result.ZInsertSpeed := 0;
    result.ZInsertMoveType := 0;
    result.ZRetractSpeed := 0;
    result.ZRetractDistance_mm := 0.0;
    result.TipTouch := false;
    result.TipTouchScan := false;
    result.TipTouchDelay := 0;
    result.TipTouchScanMode := 0;
    result.TipTouchSingle := false;
    result.TipTouchSubmerge := aPipDevice.GetNullDoubleArray;
    result.MoveToSysAirPosSpeed := 0;
end;

function gmParseTransAirPosition(aAirPos: integer): TPickAirPosition;
const
    cTransAirPosZTravel = 0;
    cTransAirPosZScan = 1;
    cTransAirPosZDisp = 2;
begin
    case (aAirPos) of
        cTransAirPosZScan:
            EXIT(papZScan);
        cTransAirPosZDisp:
            EXIT(papZDisp);
        else
            EXIT(papZTravel);
    end;
end;

function gmParseSysAirPosition(aAirPos: integer): TPickAirPosition;
const
    cSysAirPosWashStation = 0;
    cSysAirPosZTravel = 1;
    cSysAirPosZScan = 2;
    cSysAirPosZDisp = 3;
begin
    case (aAirPos) of
        cSysAirPosZTravel:
            EXIT(papZTravel);
        cSysAirPosZScan:
            EXIT(papZScan);
        cSysAirPosZDisp:
            EXIT(papZDisp);
        else
            EXIT(papUndefined);
    end;
end;

function gmParseExtraGapAirPosition(aAirPos: integer): TPickAirPosition;
const
    cExtraGapAirPosZTravel = 0;
    cExtraGapAirPosZScan = 1;
    cExtraGapAirPosZDisp = 2;
begin
    case (aAirPos) of
        cExtraGapAirPosZScan:
            EXIT(papZScan);
        cExtraGapAirPosZDisp:
            EXIT(papZDisp);
        else
            EXIT(papZTravel);
    end;
end;

// --------------------------------------------------------------------------------------------------
function gmGetAspSpeed(Calculate: Boolean; Volume, Speed: extended): extended;
// --------------------------------------------------------------------------------------------------
begin
    if Calculate then
    begin
        Speed := Volume * AspSpeedCalcFactor / 100;
        if (Speed > AspSpeedMax) then
            Speed := AspSpeedMax;
    end;
    result := Speed;
    if result <= 0 then
        result := AspSpeedDefault;
end;

// --------------------------------------------------------------------------------------------------
function gmGetDispSpeed(Calculate: Boolean; Volume, Speed: extended): extended;
// --------------------------------------------------------------------------------------------------
begin
    if Calculate then
    begin
        Speed := DispSpeedDefault;
        if (Volume <= 200) then
            Speed := 350 * DispSpeedCalcFactor / 100;
        if (Volume <= 100) then
            Speed := 300 * DispSpeedCalcFactor / 100;
        if (Volume <= 50) then
            Speed := 400 * DispSpeedCalcFactor / 100;
        if (Volume <= 10) then
            Speed := 500 * DispSpeedCalcFactor / 100;
    end;
    result := Speed;
    if result <= 0 then
        result := DispSpeedDefault;
end;

// --------------------------------------------------------------------------------------------------
function gmSetSysAir(aPipDevice: IPipDevice; aArmTips: TIPMAP; out oSysAirVol, oSysAirSpeed: TDoubleArray;
    out oSysAirDelay: integer): boolean;
// --------------------------------------------------------------------------------------------------
// Kalkuliert das aktuelle SysAirVol & SysAirSpeed & SysAirDelay
// Rückgabewert : True wenn SysAir aufgenommen werden soll
// --------------------------------------------------------------------------------------------------
var
    cnt: integer;
begin
    result := false;
    oSysAirDelay := 0;
    oSysAirVol := aPipDevice.GetNullDoubleArray;
    oSysAirSpeed := aPipDevice.GetNullDoubleArray;

    for cnt := 0 to aPipDevice.TipCount - 1 do
    begin

        if ((aArmTips and (1 shl cnt)) <= 0) then
            CONTINUE;

        // Volumen berechnen (bisheriges SysAirVolumen wird abgezogen)
        oSysAirVol[cnt] :=
            MaxValue([0, aPipDevice.Tips[cnt].St_LhPtr.SysAirAspVol - aPipDevice.Tips[cnt].St_ActSysAirVol]);
        if (oSysAirVol[cnt] = 0) then
            CONTINUE;

        result := true;
        oSysAirSpeed[cnt] := gmGetAspSpeed(aPipDevice.Tips[cnt].St_LhPtr.SysAirAspCalc, oSysAirVol[cnt],
            aPipDevice.Tips[cnt].St_LhPtr.SysAirAspSpeed);
        oSysAirDelay :=
            MaxIntValue([oSysAirDelay, Round(aPipDevice.Tips[cnt].St_LhPtr.SysAirAspDelay * 1000)]);
        aPipDevice.Tips[cnt].St_ActSysAirVol := oSysAirVol[cnt];
    end;
end;

function DetermineCh1PumpIndices(const aPipDevice: IPipDevice; const aUsedTips: TIPMAP): TIntArray;
var
    x: integer;
    xPumpIndex: integer;
begin
    result := TLiquidChannels.GetDefaultChannel1Array(aPipDevice);

    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if not TTipmapUtils.TipSelected(aUsedTips, x) then
            CONTINUE;
        xPumpIndex := aPipDevice.Tips[x].St_GetChannel1PumpIndex;
        if xPumpIndex < 0 then
            CONTINUE;
        result[x] := xPumpIndex;
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure gmPickSysAir(var DllPar: TLiquidStepData; aUsedArm: IArmDevice; aArmTips: TipMap);
// --------------------------------------------------------------------------------------------------
// Nimmt die SystemLuftblase auf
// --------------------------------------------------------------------------------------------------
var
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xCh1PumpIndices: TIntArray;
begin
    xCh1PumpIndices := DetermineCh1PumpIndices(aUsedArm.PipDevice, aArmTips);
    if gmSetSysAir(aUsedArm.PipDevice, aArmTips, Dllpar.Vol, Dllpar.Speed, Dllpar.Delay) then
    begin
        gLogManager.Log('Pick System Air - Vol-' + TLiquidHandlingLow.GetChannelVolumesBracketText(Dllpar.Vol,
            xCh1PumpIndices) + ' Speed-' + TArrayUtils.ArrayToBracketText(Dllpar.Speed) + ' Delay-[' +
            InttoStr(Dllpar.Delay) + ']', true);

        xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
        try
            TLiquidHandlingLow.PickAir(aUsedArm.PipDevice, xZMotion, aArmTips, xCh1PumpIndices, Dllpar.Vol,
                Dllpar.Speed, Dllpar.Delay, true);
        finally
            FreeAndNil(xZMotion);
        end;
    end;
end;

procedure gmLogAspirate(const aCaption: string; const aRP: TArray<TXRackPosition>; aTips: integer;
    aDoSpitBack: boolean; const aTotalSpitVol: TDoubleArray; aSpitCount: integer; aLiqDet: integer;
    aVol, aSpeed: TDoubleArray; aPumpIndices: TIntArray; aDelay: integer;
    aExtraGapLastWasteVol: TDoubleArray);
var
    xText: string;
begin
    xText := aCaption + ': ' + TXRackPositionUtils.RackPositionsToBracketText(aRP) + ' Vol-' +
        TLiquidHandlingLow.GetChannelVolumesBracketText(aVol, aPumpIndices) + ' Speed-' +
        TArrayUtils.ArrayToBracketText(aSpeed) + ' LiqDet-[' + IntToStr(aLiqDet) + ']' + ' Delay-[' +
        IntToStr(aDelay) + ']';

    if (TTipmapUtils.MaxLiquid(aTips, aExtraGapLastWasteVol) > 0) then
        xText := xText + ' Add.waste vol-' + TArrayUtils.ArrayToBracketText(aExtraGapLastWasteVol);

    if aDoSpitBack then
        xText := xText + ' Spitback vol-' + TArrayUtils.ArrayToBracketText(aTotalSpitVol) + ', ' +
            IntToStr(aSpitCount) + ' times';

    gLogManager.Log(xText, true);
end;

procedure gmLogDispense(const aRP: TArray<TXRackPosition>; aTips: integer; aLiqDet: integer;
    aVol, aSpeed: TDoubleArray; aPumpIndices: TIntArray; aDelay: integer;
    aDAirVol, aCh2DispenseVol: TDoubleArray);
var
    xText: string;
begin
    xText := 'Dispense: ' + TXRackPositionUtils.RackPositionsToBracketText(aRP) + ' Vol-' +
        TLiquidHandlingLow.GetChannelVolumesBracketText(aVol, aPumpIndices);

    if (TTipmapUtils.MaxLiquid(aTips, aDAirVol) > 0) then
        xText := xText + ' including AirGapVol-' + TArrayUtils.ArrayToBracketText(aDAirVol);

    if (TTipmapUtils.MaxLiquid(aTips, aCh2DispenseVol) > 0) then
        xText := xText + ' Vol(Ch2) -' + TArrayUtils.ArrayToBracketText(aCh2DispenseVol);

    xText := xText + ' Speed-' + TArrayUtils.ArrayToBracketText(aSpeed) + ' LiqDet-[' + IntToStr(aLiqDet) +
        ']' + ' Delay-[' + IntToStr(aDelay) + ']';

    gLogManager.Log(xText, true);
end;

// --------------------------------------------------------------------------------------------------
procedure gmAspirateDiluentVolume(var DllPar: TLiquidStepData; aUsedArm: IArmDevice; aTips: TIPMAP;
    aRP: TDilRackPositions; aRunRec: TAspirateEvRunRec);
// --------------------------------------------------------------------------------------------------
var
    iUseDispTipWithoutTip: boolean;
    cnt: integer;
    xDoSpitBack: boolean;
    xEvBeforeAsp, xEvBeforePickLq, xEvAfterPickLq, xEvAfterAsp: TRunstCall;
    xStepsForVolCh1, xStepsForVolCh2: TDoubleArray;
    xVolCh1, xVolCh2: TDoubleArray;
    xPipDevice: IPipDevice;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xCh1PumpIndices: TIntArray;
    xVol: double;
    xVolCorrCurves: TArray<string>;
begin
    xPipDevice := aUsedArm.PipDevice;

    iUseDispTipWithoutTip := false;

    xCh1PumpIndices := DetermineCh1PumpIndices(aUsedArm.PipDevice, aTips);
    xVolCh1 := xPipDevice.GetNullDoubleArray;
    xVolCh2 := xPipDevice.GetNullDoubleArray;

    // --------- Liquidhandling -----------------------------
    with DllPar do
    begin

        if (aRP.DilRackType <> drSystem) then
        begin
            Delay := 0;
            LiqDet := 0;
            xDoSpitBack := false;
            TransAirPos := 0;
            DLLPar.SingleRetract := false;
            DLLPar.DisableZErr := false;
            SubMerge_mm := 0;

            DLLPar.TipTouch := false;
            for cnt := 0 to xPipDevice.TipCount - 1 do
            begin
                Speed[cnt] := 0;
                TransAirVol[cnt] := 0;
                TransAirSpeed[cnt] := 0;
                if (aTips and (1 shl cnt)) > 0 then
                begin
                    // ----Parameter zum Speichern des detectierten Volumens und pip. von kleinen Volumina zuweisen
                    DllPar.RecordDetectionVolume := ord(xPipDevice.Tips[cnt].St_LhPtr.RecordDetectionVolume);
                    DllPar.AspirationMethodRetractZ :=
                        ord(xPipDevice.Tips[cnt].St_LhPtr.AspirationMethodRetractZ);
                    DllPar.SampleAspRetractPos_mm := xPipDevice.Tips[cnt].St_LhPtr.SampleAspRetractPos_mm;
                    // --------------------------------------------------------------------------------------------

                    ASSERT(Assigned(aRP.Positions[cnt].Rack));
                    Speed[cnt] := gmGetAspSpeed(xPipDevice.Tips[cnt].St_LhPtr.DilAspCalc,
                        xPipDevice.Tips[cnt].St_DilVol, xPipDevice.Tips[cnt].St_LhPtr.DilAspSpeed);
                    Delay := MaxIntValue([Delay, Round(xPipDevice.Tips[cnt].St_LhPtr.DilAspDelay * 1000)]);
                    LiqDet := MaxIntValue([LiqDet, xPipDevice.Tips[cnt].St_LhPtr.DilAspLiqDet]);
                    // !!! ZPosition bei fester Pos
                    if (xPipDevice.Tips[cnt].St_LhPtr.DilAspErrFlag > 0) then
                        DllPar.DisableZErr := true;
                    SubMerge_mm := xPipDevice.Tips[cnt].St_LhPtr.DilAspSubmerge;

                    SpitCount := xPipDevice.Tips[cnt].St_SpitBackCount;
                    SpitVolume[cnt] := xPipDevice.Tips[cnt].St_LhPtr.SampleAspSpitBack;
                    TotalSpitVol[cnt] := DLLPar.SpitVolume[cnt] * SpitCount;

                    SwitchModule := xPipDevice.Tips[cnt].St_LhPtr.DilAspSwitchModule;
                    SwitchPos := xPipDevice.Tips[cnt].St_LhPtr.DilAspSwitchPos;
                    if TotalSpitVol[cnt] > 0 then
                        xDoSpitBack := true;

                    // if aUsedArm.Tips[cnt].St_DilVol + aUsedArm.Tips[cnt].St_SpitVolume < aUsedArm.Tips[cnt].SyrVolume[0]
                    // then Vol[cnt] := xPipDevice.Tips[cnt].St_DilVol + xPipDevice.Tips[cnt].St_SpitVolume;

                    // SpitVolume wurde nie auf einen anderen Wert als 0 gesetzt - daher deaktiviert!
                    if xPipDevice.Tips[cnt].St_DilVol < xPipDevice.Tips[cnt].GetPipPumpMaxVolume
                        (xCh1PumpIndices[cnt]) then
                        xVolCh1[cnt] := xPipDevice.Tips[cnt].St_DilVol;

                    if (xPipDevice.Tips[cnt].St_LhPtr.DilAspTipSingleRetract) then
                        SingleRetract := true;
                    if xPipDevice.Tips[cnt].St_SVol = 0 then
                    begin // ---------------------------------- wenn keine Probe dann TransAir
                        TransAirVol[cnt] := xPipDevice.Tips[cnt].St_LhPtr.TransAirVol;
                        TransAirPos := xPipDevice.Tips[cnt].St_LhPtr.TransAirPos;
                        TransAirSpeed[cnt] := gmGetAspSpeed(xPipDevice.Tips[cnt].St_LhPtr.TransAirAspCalc,
                            TransAirVol[cnt], xPipDevice.Tips[cnt].St_LhPtr.TransAirSpeed);
                    end;
                    SysAirPos := xPipDevice.Tips[cnt].St_LhPtr.SysAirAspPos;
                    if xPipDevice.Tips[cnt].St_LhPtr.UseDispTipWithoutTip then
                        iUseDispTipWithoutTip := true;
                    ZScanMode := xPipDevice.Tips[cnt].St_LhPtr.DilAspScanMode;
                    ZInsertSpeed := xPipDevice.Tips[cnt].St_LhPtr.DilAspScanSpeed;
                    ZInsertMoveType := xPipDevice.Tips[cnt].St_LhPtr.DilAspInsertMoveType;
                    ZRetractSpeed := xPipDevice.Tips[cnt].St_LhPtr.DilAspRetrSpeed;
                    ZRetractDistance_mm := xPipDevice.Tips[cnt].St_LhPtr.DilAspRetrDistance;
                    MoveToSysAirPosSpeed := xPipDevice.Tips[cnt].St_LhPtr.MoveToSysAirPosSpeed;
                end;
            end; // end for cnt:=0 to xPipDevice.TipCount-1
            // ---------------------------------------------------------------------------- System Airgap errechnen
            gmSetSysAir(xPipDevice, aTips, DLLPar.SysAirVol, DLLPar.SysAirSpeed, DLLPar.SysAirDelay);

            if (not iUseDispTipWithoutTip) then
                gmPickDispTips(aUsedArm, aTips);

            // Volume correction
            xVolCorrCurves := TLiquidHandling.GetVolCorrCurves(xPipDevice);
            xStepsForVolCh1 := TLiquidHandling.CalcDilutorSteps(xPipDevice, aTips, xVolCh1, xCh1PumpIndices,
                xVolCorrCurves);

            xEvBeforeAsp := TEventManager.Instance.CreateEventIfNecessary(aRunRec.BeforeAsp);
            xEvBeforePickLq := TEventManager.Instance.CreateEventIfNecessary(aRunRec.BeforePickLq);
            xEvAfterPickLq := TEventManager.Instance.CreateEventIfNecessary(aRunRec.AfterPickLq);
            xEvAfterAsp := TEventManager.Instance.CreateEventIfNecessary(aRunRec.AfterAsp);
            // -----------------------------------------------------------------------------

            gmLogAspirate('Aspirate Diluent', aRP.Positions, aTips, xDoSpitBack, DLLPar.TotalSpitVol,
                DllPar.SpitCount, DLLPar.LiqDet, DLLPar.Vol, DLLPar.Speed, xCh1PumpIndices, DLLPar.Delay,
                aUsedArm.PipDevice.GetNullDoubleArray);

            TLiquidHandling.Aspirate(aUsedArm, aTips, aRP.Positions, xCh1PumpIndices, SysAirVol, SysAirSpeed,
                xVolCh1, xStepsForVolCh1, aUsedArm.PipDevice.GetNullDoubleArray,
                aUsedArm.PipDevice.GetNullDoubleArray, Speed, aUsedArm.PipDevice.GetNullDoubleArray,
                gmParseSysAirPosition(SysAirPos), gmParseTransAirPosition(TransAirPos), Delay, SpitVolume,
                SpitCount, aUsedArm.PipDevice.GetNullDoubleArray, false,
                aUsedArm.PipDevice.GetNullDoubleArray,
                // Ch2WashVolume
                aUsedArm.PipDevice.GetNullDoubleArray, // Ch2WashSpeed
                0, // Ch2WashDelay
                SysAirDelay, 0 { DDelay } , LiqDet, SubMerge_mm { Sub } ,
                TLiqHDataAdaptor.WriteMixRec(0, 0, 0, 0, false, 0, 0, 0) { Mix } , false, SwitchModule,
                gmParseSwitchPlace(SwitchPos), SingleRetract, (RecordDetectionVolume <> 0),
                AspirationMethodRetractZ, DLLPar.SampleAspRetractPos_mm, gDispCh1and2together, xEvBeforeAsp,
                xEvBeforePickLq, xEvAfterPickLq, xEvAfterAsp, DllPar.DisableZErr, ZScanMode, ZInsertSpeed,
                ZInsertMoveType, ZRetractSpeed, ZRetractDistance_mm, TipTouch, TipTouchScan, TipTouchDelay,
                TipTouchScanMode, TipTouchSingle, TipTouchSubmerge, aUsedArm.PipDevice.GetNullIntArray,
                aUsedArm.PipDevice.GetNullDoubleArray, aUsedArm.PipDevice.GetNullDoubleArray, 0, papUndefined,
                aUsedArm.PipDevice.GetNullDoubleArray, aUsedArm.PipDevice.GetNullDoubleArray, 0,
                aUsedArm.PipDevice.GetNullDoubleArray, asptSystemRackLiquid, MoveToSysAirPosSpeed);
            // ---------------------------------------------------------------------------------------
            FreeAndNil(xEvBeforeAsp);
            FreeAndNil(xEvBeforePickLq);
            FreeAndNil(xEvAfterPickLq);
            FreeAndNil(xEvAfterAsp);
        end
        else
        begin
            // ------------------------------------------------------------------ Aus dem System aufnehmen
            Delay := 10;
            for cnt := 0 to xPipDevice.TipCount - 1 do
            begin
                Speed[cnt] := 0;
                if not TTipmapUtils.TipSelected(aTips, cnt) then
                    CONTINUE;
                xVol := xPipDevice.Tips[cnt].St_DilVol;
                if xPipDevice.Tips[cnt].St_DilVolWithCh2 then
                    xVolCh2[cnt] := xVol
                else
                    xVolCh1[cnt] := xVol;

                Speed[cnt] := gmGetAspSpeed(xPipDevice.Tips[cnt].St_LhPtr.DilAspCalc, xVol,
                    xPipDevice.Tips[cnt].St_LhPtr.DilAspSpeed);
                Delay := MaxIntValue([Delay, Round(xPipDevice.Tips[cnt].St_LhPtr.DilAspDelay)]);

            end;

            // Volume correction
            xVolCorrCurves := TLiquidHandling.GetVolCorrCurves(xPipDevice);
            xStepsForVolCh1 := TLiquidHandling.CalcDilutorSteps(xPipDevice, aTips, xVolCh1, xCh1PumpIndices,
                xVolCorrCurves);
            xStepsForVolCh2 := TLiquidHandling.CalcDilutorSteps(xPipDevice, aTips, xVolCh2,
                TLiquidChannels.GetDefaultChannel2Array(xPipDevice), xVolCorrCurves);

            xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
            TLiquidHandling.PickSys(aUsedArm.PipDevice, xZMotion, aTips, xCh1PumpIndices, xVolCh1, Speed,
                xVolCh2, Speed, xStepsForVolCh1, xStepsForVolCh2, Delay, true);
            FreeAndNil(xZMotion);
        end; // else

    end; // end with DLLPar
end;

// --------------------------------------------------------------------------------------------------
procedure gmAspirateSampleVolume(var DllPar: TLiquidStepData; aUsedArm: IArmDevice; aTips: TIPMAP;
    const aRP: TArray<TXRackPosition>; aRunRec: TAspirateEvRunRec);
// --------------------------------------------------------------------------------------------------
var
    iUseDispTipWithoutTip: boolean;
    cnt: integer;
    xDoSpitBack: boolean;
    xSpitBackAtAspPos: boolean;
    xEvBeforeAsp, xEvBeforePickLq, xEvAfterPickLq, xEvAfterAsp: TRunstCall;
    xSampleVol: TDoubleArray;
    xStepsForVolCh1, xStepsForVolCh2, xTempIntArray1, xTempIntArray2: TDoubleArray;
    xExtraGapCount: TIntArray;
    xExtraGapAirVol: TDoubleArray;
    xExtraGapAirSpeed: TDoubleArray;
    xExtraGapAirDelay: integer;
    xExtraGapAirPos: TPickAirPosition;
    xExtraGapWasteVol: TDoubleArray;
    xExtraGapWasteSpeed: TDoubleArray;
    xExtraGapWasteDelay: integer;
    xPipDevice: IPipDevice;
    xExtraGapLastWasteVol: TDoubleArray;
    xCh1PumpIndices: TIntArray;
    xVolCorrCurves: TArray<string>;
    xMixFirstOnly: boolean;
begin
    xPipDevice := aUsedArm.PipDevice;
    iUseDispTipWithoutTip := false;
    // --------------------------------------------------------------------------------- Liquidhandling
    DLLPar.Delay := 0;
    DLLPar.TransAirDelay := 0;
    DLLPar.LiqDet := 0;
    DLLPar.SubMerge_mm := 0;
    DLLPar.TransAirPos := 0;
    DLLPar.SingleRetract := false;
    DLLPar.DisableZErr := false;
    DLLPar.TipTouch := false;
    xExtraGapAirDelay := 0;
    xExtraGapWasteDelay := 0;
    xExtraGapAirPos := papUndefined;

    xDoSpitBack := false;
    xSpitBackAtAspPos := false;
    xSampleVol := xPipDevice.GetNullDoubleArray;
    xExtraGapCount := xPipDevice.GetNullIntArray;
    xExtraGapAirVol := xPipDevice.GetNullDoubleArray;
    xExtraGapAirSpeed := xPipDevice.GetNullDoubleArray;
    xExtraGapWasteVol := xPipDevice.GetNullDoubleArray;
    xExtraGapWasteSpeed := xPipDevice.GetNullDoubleArray;
    xExtraGapLastWasteVol := xPipDevice.GetNullDoubleArray;
    xMixFirstOnly := false;

    xCh1PumpIndices := DetermineCh1PumpIndices(aUsedArm.PipDevice, aTips);
    for cnt := 0 to xPipDevice.TipCount - 1 do
    begin
        DLLPar.Speed[cnt] := 0;
        DLLPar.TransAirVol[cnt] := 0;
        DLLPar.TransAirSpeed[cnt] := 0;
        DLLPar.SpitSpeed[cnt] := 0;

        if (aTips and (1 shl cnt)) > 0 then
        begin
            // ----Parameter zum Speichern des detectierten Volumens und pip. von kleinen Volumina zuweisen
            DllPar.RecordDetectionVolume := ord(xPipDevice.Tips[cnt].St_LhPtr.RecordDetectionVolume);
            DllPar.AspirationMethodRetractZ := ord(xPipDevice.Tips[cnt].St_LhPtr.AspirationMethodRetractZ);
            DllPar.SampleAspRetractPos_mm := xPipDevice.Tips[cnt].St_LhPtr.SampleAspRetractPos_mm;
            // --------------------------------------------------------------------------------------------
            DLLPar.Speed[cnt] := gmGetAspSpeed(xPipDevice.Tips[cnt].St_LhPtr.SampleAspCalc,
                xPipDevice.Tips[cnt].St_SVol, xPipDevice.Tips[cnt].St_LhPtr.SampleAspSpeed);
            DLLPar.LiqDet := MaxIntValue([DLLPar.LiqDet, xPipDevice.Tips[cnt].St_LhPtr.SampleAspLiqDet]);
            if (xPipDevice.Tips[cnt].St_LhPtr.SampleAspErrFlag <> 0) then
                DllPar.DisableZErr := true;
            DLLPar.Delay :=
                MaxIntValue([DLLPar.Delay, Round(xPipDevice.Tips[cnt].St_LhPtr.SampleAspDelay * 1000)]);
            DLLPar.TransAirDelay :=
                MaxIntValue([DLLPar.TransAirDelay, Round(xPipDevice.Tips[cnt].St_LhPtr.TransAirAspDelay
                * 1000)]);
            DLLPar.SubMerge_mm := xPipDevice.Tips[cnt].St_LhPtr.SampleAspSubmerge;
            // --------------------------------------------------------------------------------- SpitVolume
            DLLPar.SpitCount := xPipDevice.Tips[cnt].St_SpitBackCount;
            DLLPar.SpitVolume[cnt] := xPipDevice.Tips[cnt].St_LhPtr.SampleAspSpitBack;
            DLLPar.TotalSpitVol[cnt] := DLLPar.SpitVolume[cnt] * DLLPar.SpitCount;

            // --------------------------------------------------------------------------------- ExtraGap
            if not xPipDevice.Tips[cnt].St_ActExtraGapTaken then
            begin
                xExtraGapCount[cnt] := xPipDevice.Tips[cnt].St_LhPtr.ExtraGapCount;
                xExtraGapAirVol[cnt] := xPipDevice.Tips[cnt].St_LhPtr.ExtraGapAirVol;
                xExtraGapAirDelay :=
                    MaxIntValue([xExtraGapAirDelay, Round(xPipDevice.Tips[cnt].St_LhPtr.ExtraGapAirDelay
                    * 1000)]);
                xExtraGapAirPos := gmParseExtraGapAirPosition(xPipDevice.Tips[cnt].St_LhPtr.ExtraGapAirPos);
                xExtraGapAirSpeed[cnt] := xPipDevice.Tips[cnt].St_LhPtr.ExtraGapAirSpeed;
                xExtraGapAirSpeed[cnt] := gmGetAspSpeed(xPipDevice.Tips[cnt].St_LhPtr.ExtraGapAirCalc,
                    xExtraGapAirVol[cnt], xExtraGapAirSpeed[cnt]);
                xExtraGapWasteVol[cnt] := xPipDevice.Tips[cnt].St_LhPtr.ExtraGapWasteVol;
                xExtraGapWasteDelay :=
                    MaxIntValue([xExtraGapWasteDelay, Round(xPipDevice.Tips[cnt].St_LhPtr.ExtraGapWasteDelay
                    * 1000)]);
                xExtraGapWasteSpeed[cnt] := xPipDevice.Tips[cnt].St_LhPtr.ExtraGapWasteSpeed;
                xExtraGapWasteSpeed[cnt] := gmGetAspSpeed(xPipDevice.Tips[cnt].St_LhPtr.ExtraGapWasteCalc,
                    xExtraGapWasteVol[cnt], xExtraGapWasteSpeed[cnt]);
                xExtraGapLastWasteVol[cnt] := xPipDevice.Tips[cnt].St_LhPtr.SampleAspWasteVol +
                    (xPipDevice.Tips[cnt].St_SVol * xPipDevice.Tips[cnt]
                    .St_LHPtr.SampleAspWastePerCent / 100);
                xPipDevice.Tips[cnt].St_ActExtraGapTaken := true;
            end
            else
            begin
                xExtraGapCount[cnt] := 0;
            end;

            // Is this needed? because we already accounted for a spitback in St_CalculatePipVolume
            if (xPipDevice.Tips[cnt].St_SVol + DLLPar.TotalSpitVol[cnt] > xPipDevice.Tips[cnt]
                .GetPipPumpMaxVolume(xCh1PumpIndices[cnt])) then
            begin
                DLLPar.TotalSpitVol[cnt] := 0;
                DLLPar.SpitVolume[cnt] := 0; // Maxvol !!
            end;

            if (DLLPar.TotalSpitVol[cnt] > 0) then
                xDoSpitBack := true;
            DLLPar.Vol[cnt] := xPipDevice.Tips[cnt].St_SVol + DLLPar.TotalSpitVol[cnt];
            DLLPar.SpitSpeed[cnt] := gmGetDispSpeed(xPipDevice.Tips[cnt].St_LhPtr.SampleAspSpitBackCalc,
                DLLPar.Vol[cnt], xPipDevice.Tips[cnt].St_LhPtr.SampleAspSpitBackSpeed);
            xSpitBackAtAspPos := xPipDevice.Tips[cnt].St_LhPtr.SampleAspSpitBackAtAspPos;
            // ------------------------------------------------------------------------------ TipWaschVolumen Kanal2
            DllPar.Ch2TipWashVol[cnt] := xPipDevice.Tips[cnt].St_LhPtr.SampleAspCh2WashVol;
            DllPar.Ch2TipWashSpeed[cnt] := gmGetDispSpeed(xPipDevice.Tips[cnt].St_LhPtr.SampleAspCh2WashCalc,
                DllPar.Ch2TipWashVol[cnt], xPipDevice.Tips[cnt].St_LhPtr.SampleAspCh2WashSpeed);
            DllPar.Ch2TipWashDelay :=
                MaxIntValue([DLLPar.Ch2TipWashDelay, Round(xPipDevice.Tips[cnt].St_LhPtr.SampleAspCh2WashDelay
                * 1000)]);
            // --------------------------------------------------------------------------------------------
            if (xPipDevice.Tips[cnt].St_LHPtr.SampleAspTipSingleRetract) then
                DLLPar.SingleRetract := true;
            DLLPar.MixRec := xPipDevice.Tips[cnt].St_LhPtr.SampleAspMix;
            xMixFirstOnly := xPipDevice.Tips[cnt].St_LhPtr.SampleAspMixFirstOnly;
            DLLPar.SwitchModule := xPipDevice.Tips[cnt].St_LhPtr.AspSwitchModule;
            DLLPar.SwitchPos := xPipDevice.Tips[cnt].St_LhPtr.AspSwitchPos;
            DLLPar.TransAirVol[cnt] := xPipDevice.Tips[cnt].St_LhPtr.TransAirVol;
            DLLPar.TransAirPos := xPipDevice.Tips[cnt].St_LhPtr.TransAirPos;
            DLLPar.TransAirSpeed[cnt] := gmGetAspSpeed(xPipDevice.Tips[cnt].St_LhPtr.TransAirAspCalc,
                DLLPar.Vol[cnt], xPipDevice.Tips[cnt].St_LhPtr.TransAirSpeed);
            DLLPar.SysAirPos := xPipDevice.Tips[cnt].St_LhPtr.SysAirAspPos;
            if xPipDevice.Tips[cnt].St_LhPtr.UseDispTipWithoutTip then
                iUseDispTipWithoutTip := true;
            DLLPar.ZScanMode := xPipDevice.Tips[cnt].St_LhPtr.SampleAspScanMode;
            DLLPar.ZInsertSpeed := xPipDevice.Tips[cnt].St_LhPtr.SampleAspScanSpeed;
            DLLPar.ZInsertMoveType := xPipDevice.Tips[cnt].St_LhPtr.SampleAspInsertMoveType;
            DLLPar.ZRetractSpeed := xPipDevice.Tips[cnt].St_LhPtr.SampleAspRetrSpeed;
            DLLPar.ZRetractDistance_mm := xPipDevice.Tips[cnt].St_LhPtr.SampleAspRetrDistance;

            DLLPar.TipTouch := xPipDevice.Tips[cnt].St_LhPtr.SampleAspTipTouch;
            DLLPar.TipTouchScan := xPipDevice.Tips[cnt].St_LhPtr.SampleAspTipTouchScan;
            DLLPar.TipTouchDelay := Round(xPipDevice.Tips[cnt].St_LhPtr.SampleAspTipTouchDelay * 1000);
            DLLPar.TipTouchScanMode := xPipDevice.Tips[cnt].St_LhPtr.SampleAspTipTouchScanMode;
            DLLPar.TipTouchSingle := xPipDevice.Tips[cnt].St_LhPtr.SampleAspTipTouchSingle;
            DLLPar.TipTouchSubmerge[cnt] := xPipDevice.Tips[cnt].St_LhPtr.SampleAspTipTouchSubmerge;
            DLLPar.MoveToSysAirPosSpeed := xPipDevice.Tips[cnt].St_LhPtr.MoveToSysAirPosSpeed;
            // ----------------------------------------------------------------------- für Volumenkorrektur
            xSampleVol[cnt] := DLLPar.Vol[cnt] - DLLPar.TotalSpitVol[cnt];
        end;
    end;

    // Volume correction
    xVolCorrCurves := TLiquidHandling.GetVolCorrCurves(xPipDevice);
    xStepsForVolCh1 := TLiquidHandling.CalcDilutorSteps(xPipDevice, aTips, xSampleVol, xCh1PumpIndices,
        xVolCorrCurves);
    xTempIntArray1 := TLiquidHandling.CalcDilutorSteps(xPipDevice, aTips, DLLPar.TotalSpitVol,
        xCh1PumpIndices, nil);
    xTempIntArray2 := TLiquidHandling.CalcDilutorSteps(xPipDevice, aTips, xExtraGapLastWasteVol,
        xCh1PumpIndices, xVolCorrCurves);
    for cnt := 0 to xPipDevice.TipCount - 1 do
        if ((aTips and (1 shl cnt)) > 0) and (xPipDevice.Tips[cnt].GetPipPump(xCh1PumpIndices[cnt]) <>
            nil) then
        begin
            if (xPipDevice.Tips[cnt].St_SVolSteps > 0) then
                xStepsForVolCh1[cnt] := xPipDevice.Tips[cnt].St_SVolSteps;
            xStepsForVolCh1[cnt] := xStepsForVolCh1[cnt] + xTempIntArray1[cnt] + xTempIntArray2[cnt];

            // Das sollte nie passieren: Alles zusammen übersteigt das Maximum des Dilutors
            if (xStepsForVolCh1[cnt] > xPipDevice.Tips[cnt].GetPipPump(xCh1PumpIndices[cnt])
                .MaxMotorStep) then
            begin
                xStepsForVolCh1[cnt] := xPipDevice.Tips[cnt].GetPipPump(xCh1PumpIndices[cnt]).MaxMotorStep;
                gLogManager.LogF
                    ('Aspirate with volume correction - Dilutor %d Volume Steps are reset to max. Dilutor steps!',
                    [cnt + 1], true);
            end;
        end;
    xStepsForVolCh2 := TLiquidHandling.CalcDilutorSteps(xPipDevice, aTips, DllPar.Ch2TipWashVol,
        TLiquidChannels.GetDefaultChannel2Array(xPipDevice), xVolCorrCurves);

    // ---------------------------------------------------------------------------- System Airgap errechnen
    gmSetSysAir(xPipDevice, aTips, DLLPar.SysAirVol, DLLPar.SysAirSpeed, DLLPar.SysAirDelay);

    if (not iUseDispTipWithoutTip) then
        gmPickDispTips(aUsedArm, aTips);

    // //---------------------------------------------------------------------------- Dde Advanced Status
    // ThrMan.SendDDEStatus('_Aspirate - ' + TXRackPositionUtils.RackPositionsToBracketText(aRP));

    // --------------------------------------------------------------------------- Aspirate durchführen
    xEvBeforeAsp := TEventManager.Instance.CreateEventIfNecessary(aRunRec.BeforeAsp);
    xEvBeforePickLq := TEventManager.Instance.CreateEventIfNecessary(aRunRec.BeforePickLq);
    xEvAfterPickLq := TEventManager.Instance.CreateEventIfNecessary(aRunRec.AfterPickLq);
    xEvAfterAsp := TEventManager.Instance.CreateEventIfNecessary(aRunRec.AfterAsp);
    try

        gmLogAspirate('Aspirate Sample', aRP, aTips, xDoSpitBack, DLLPar.TotalSpitVol, DllPar.SpitCount,
            DLLPar.LiqDet, DLLPar.Vol, DLLPar.Speed, xCh1PumpIndices, DLLPar.Delay, xExtraGapLastWasteVol);

        TLiquidHandling.Aspirate(aUsedArm, aTips, aRP, xCh1PumpIndices, DLLPar.SysAirVol, DLLPar.SysAirSpeed,
            DLLPar.Vol, xStepsForVolCh1, xStepsForVolCh2, DLLPar.TransAirVol, DLLPar.Speed,
            DLLPar.TransAirSpeed, gmParseSysAirPosition(DLLPar.SysAirPos),
            gmParseTransAirPosition(DLLPar.TransAirPos), DLLPar.Delay, DLLPar.SpitVolume, DLLPar.SpitCount,
            DLLPar.SpitSpeed, xSpitBackAtAspPos, DLLPar.Ch2TipWashVol, DLLPar.Ch2TipWashSpeed,
            DLLPar.Ch2TipWashDelay, DLLPar.SysAirDelay, DLLPar.TransAirDelay, DLLPar.LiqDet,
            DLLPar.SubMerge_mm, DLLPar.MixRec, xMixFirstOnly, DLLPar.SwitchModule,
            gmParseSwitchPlace(DLLPar.SwitchPos), DLLPar.SingleRetract, (DLLPar.RecordDetectionVolume <> 0),
            DLLPar.AspirationMethodRetractZ, DLLPar.SampleAspRetractPos_mm, gDispCh1and2together,
            xEvBeforeAsp, xEvBeforePickLq, xEvAfterPickLq, xEvAfterAsp, DllPar.DisableZErr, DLLPar.ZScanMode,
            DLLPar.ZInsertSpeed, DLLPar.ZInsertMoveType, DLLPar.ZRetractSpeed, DLLPar.ZRetractDistance_mm,
            DLLPar.TipTouch, DLLPar.TipTouchScan, DLLPar.TipTouchDelay, DLLPar.TipTouchScanMode,
            DLLPar.TipTouchSingle, DLLPar.TipTouchSubmerge, xExtraGapCount, xExtraGapAirVol,
            xExtraGapAirSpeed, xExtraGapAirDelay, xExtraGapAirPos, xExtraGapWasteVol, xExtraGapWasteSpeed,
            xExtraGapWasteDelay, xExtraGapLastWasteVol, asptLiquid, DLLPar.MoveToSysAirPosSpeed);
    finally
        FreeAndNil(xEvBeforeAsp);
        FreeAndNil(xEvBeforePickLq);
        FreeAndNil(xEvAfterPickLq);
        FreeAndNil(xEvAfterAsp);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure gmDispenseVolume(var DllPar: TLiquidStepData; aUsedArm: IArmDevice; aTips: TIPMAP;
    const aRP: TArray<TXRackPosition>; aRunRec: TDispenseEvRunRec);
// --------------------------------------------------------------------------------------------------
var
    cnt: integer;
    xEvBeforeDisp, xEvBeforeDispLq, xEvAfterDispLq, xEvAfterDisp: TRunstCall;
    xPipDevice: IPipDevice;
    xCh1PumpIndices: TIntArray;
    xMixAspSubmerge: double;
begin
    xPipDevice := aUsedArm.PipDevice;
    xCh1PumpIndices := DetermineCh1PumpIndices(aUsedArm.PipDevice, aTips);

    with DllPar do
    begin

        DLLPar.SingleRetract := false;
        DLLPar.DisableZErr := false;
        DLLPar.TipTouch := false;
        DLLPar.TipTouchScanStoreVol := false;

        TransAirPos := 0;
        PerFormingSteps := 0;
        PerFormDelay := 0;
        Submerge_mm := 0;
        Delay := 0;
        LiqDet := 0;
        xMixAspSubmerge := 0;
        // aUsedArm.ResetDisabledErrors; not needed!?
        for cnt := 0 to xPipDevice.TipCount - 1 do
        begin
            Speed[cnt] := 0;
            TransAirVol[cnt] := 0;
            TransAirSpeed[cnt] := 0;
            Ch2DispenseVol[cnt] := 0;
            if ((aTips and (1 shl cnt)) > 0) then
            begin
                Speed[cnt] := gmGetDispSpeed(xPipDevice.Tips[cnt].St_LhPtr.DispCalc,
                    xPipDevice.Tips[cnt].St_DVol - xPipDevice.Tips[cnt].St_LhPtr.SysAirDispVol -
                    xPipDevice.Tips[cnt].St_LhPtr.TransAirVol, xPipDevice.Tips[cnt].St_LhPtr.DispSpeed);
                LiqDet := MaxIntValue([LiqDet, xPipDevice.Tips[cnt].St_LhPtr.DispLiqDet]);
                // !!! ZPosition bei fester Pos
                if (xPipDevice.Tips[cnt].St_LhPtr.DispErrFlag > 0) then
                    DllPar.DisableZErr := true;
                Delay := MaxIntValue([Delay, Round(xPipDevice.Tips[cnt].St_LhPtr.DispDelay * 1000)]);
                Submerge_mm := xPipDevice.Tips[cnt].St_LhPtr.DispSubmerge;
                PerFormingSteps := xPipDevice.Tips[cnt].St_LhPtr.DispStepVolume;
                PerFormDelay := round(xPipDevice.Tips[cnt].St_LhPtr.DispStepDelay * 1000);
                MixRec := xPipDevice.Tips[cnt].St_LhPtr.DispMix;
                xMixAspSubmerge := xPipDevice.Tips[cnt].St_LhPtr.DispMixAspSubmerge;
                DAirVol[cnt] := xPipDevice.Tips[cnt].St_DAirVol;
                Vol[cnt] := xPipDevice.Tips[cnt].St_DVol;
                TotalVol[cnt] := xPipDevice.Tips[cnt].St_TotalVol;

                if xPipDevice.Tips[cnt].St_LhPtr.DilAspChannel2 then
                begin
                    Ch2DispenseVol[cnt] := xPipDevice.Tips[cnt].St_DilVol;
                end;

                xPipDevice.Tips[cnt].St_ActSysAirVol := xPipDevice.Tips[cnt].St_LhPtr.SysAirAspVol -
                    xPipDevice.Tips[cnt].St_LhPtr.SysAirDispVol;
                if (xPipDevice.Tips[cnt].St_LhPtr.DispTipSingleRetract) then
                    SingleRetract := true;
                SwitchModule := xPipDevice.Tips[cnt].St_LhPtr.DspSwitchModule;
                SwitchPos := xPipDevice.Tips[cnt].St_LhPtr.DspSwitchPos;

                // -------------------------------------------------------- Transair bei Multipip Dispense
                if (xPipDevice.Tips[cnt].St_DTransAirRetake) then
                begin
                    TransAirVol[cnt] := xPipDevice.Tips[cnt].St_LhPtr.TransAirVol;
                    TransAirSpeed[cnt] := gmGetAspSpeed(xPipDevice.Tips[cnt].St_LhPtr.TransAirAspCalc,
                        TransAirVol[cnt], xPipDevice.Tips[cnt].St_LhPtr.TransAirSpeed);
                    TransAirPos := xPipDevice.Tips[cnt].St_LhPtr.TransAirPos;
                end;
                DLLPar.ZScanMode := xPipDevice.Tips[cnt].St_LhPtr.DispScanMode;
                DLLPar.ZInsertSpeed := xPipDevice.Tips[cnt].St_LhPtr.DispScanSpeed;
                DLLPar.ZInsertMoveType := xPipDevice.Tips[cnt].St_LhPtr.DispInsertMoveType;
                DLLPar.ZRetractSpeed := xPipDevice.Tips[cnt].St_LhPtr.DispRetrSpeed; // für Mixen
                DLLPar.ZRetractDistance_mm := xPipDevice.Tips[cnt].St_LhPtr.DispRetrDistance;

                DLLPar.TipTouch := xPipDevice.Tips[cnt].St_LhPtr.DispTipTouch;
                DLLPar.TipTouchScan := xPipDevice.Tips[cnt].St_LhPtr.DispTipTouchScan;
                DLLPar.TipTouchDelay := Round(xPipDevice.Tips[cnt].St_LhPtr.DispTipTouchDelay * 1000);
                DLLPar.TipTouchScanMode := xPipDevice.Tips[cnt].St_LhPtr.DispTipTouchScanMode;
                DLLPar.TipTouchSingle := xPipDevice.Tips[cnt].St_LhPtr.DispTipTouchSingle;
                DLLPar.TipTouchSubmerge[cnt] := xPipDevice.Tips[cnt].St_LhPtr.DispTipTouchSubmerge;
                DLLPar.TipTouchScanStoreVol := xPipDevice.Tips[cnt].St_LhPtr.DispTipTouchScanStoreVol;
            end;
        end;

        if not gDispSubmergeActive then
            Submerge_mm := 0;

        // //---------------------------------------------------------------------------- Dde Advanced Statu
        // ThrMan.SendDDEStatus('_Dispense - ' + TXRackPositionUtils.RackPositionsToBracketText(aRP));

        xEvBeforeDisp := TEventManager.Instance.CreateEventIfNecessary(aRunRec.BeforeDispense);
        xEvBeforeDispLq := TEventManager.Instance.CreateEventIfNecessary(aRunRec.BeforeDispLq);
        xEvAfterDispLq := TEventManager.Instance.CreateEventIfNecessary(aRunRec.AfterDispLq);
        xEvAfterDisp := TEventManager.Instance.CreateEventIfNecessary(aRunRec.AfterDispense);

        gmLogDispense(aRP, aTips, DLLPar.LiqDet, DLLPar.Vol, DLLPar.Speed, xCh1PumpIndices, DLLPar.Delay,
            DAirVol, Ch2DispenseVol);

        TLiquidHandling.Dispense(aUsedArm, aTips, aRP, xCh1PumpIndices, Vol, Ch2DispenseVol, TotalVol,
            DAirVol, Speed, Delay, TransAirVol, TransAirSpeed, gmParseTransAirPosition(TransAirPos), MixRec,
            xMixAspSubmerge, LiqDet, Submerge_mm, PerFormingSteps, PerFormDelay, SwitchModule,
            gmParseSwitchPlace(SwitchPos), SingleRetract, gDispCh1and2together, xEvBeforeDisp,
            xEvBeforeDispLq, xEvAfterDispLq, xEvAfterDisp, DllPar.DisableZErr, DLLPar.ZScanMode,
            DLLPar.ZInsertSpeed, DLLPar.ZInsertMoveType, DLLPar.ZRetractSpeed, DLLPar.ZRetractDistance_mm,
            DLLPar.TipTouch, DLLPar.TipTouchScan, DLLPar.TipTouchDelay, DLLPar.TipTouchScanMode,
            DLLPar.TipTouchSingle, DLLPar.TipTouchSubmerge, DLLPar.TipTouchScanStoreVol);

        FreeAndNil(xEvBeforeDisp);
        FreeAndNil(xEvBeforeDispLq);
        FreeAndNil(xEvAfterDispLq);
        FreeAndNil(xEvAfterDisp);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure gmDoLiquidHandling(aUsedArm: IArmDevice; const aSource, aDest: TArray<TXRackPosition>;
    aDil: TDilRackPositions; aLastSysAirAspPos: integer; aDispTips, aAspSampleTips, aAspDilTips: TipMap;
    aPipRunStep: TCombinedActivePipetteRunStep; aDA: TPipetteListDataAdaptor; const aPipListName: string;
    const aPipListSeq: TArray<integer>);
// --------------------------------------------------------------------------------------------------
var
    xDllPar: TLiquidStepData;
begin
    if (aDispTips or aAspSampleTips or aAspDilTips <= 0) then
        Exit;

    aDA.UpdateStatus(aPipListName, aPipListSeq, plsStarted);
    // 29.06.10 pk It would be nice if we could get rid of DLLPar. It is very dangerous, especially because it is used as a var
    // I think that there is no need to use it as a var.  If you see any purpose in this DLLPar record, write a comment, otherwise I will
    // replace it!!!
    // 19.08.10 wl Du hast meine volle Unterstützung! DLLPar stammt aus der Zeit, als Aspirate und Dispense noch Dll-Funktionen waren.
    xDLLPar := gmInitLiquidStepData(aUsedArm.PipDevice);

    // -------------------------------------------------------------- Systemluftblase aufnehmen
    if (((aAspSampleTips > 0) { and (aSource.Rack <> nil) } ) or (aAspDilTips > 0)) and
        (not gErrorManager.IsGlobalErr()) and (aLastSysAirAspPos = 0) then
    begin
        gLogManager.Log('Pick System Air - START', true);
        gmPickSysAir(xDllPar, aUsedArm, aAspSampleTips or aAspDilTips);
        gLogManager.Log('Pick System Air - END', true);
    end;
    // -------------------------------------------------------------- Lösungsmittel (Diluent) aufnehmen
    if (aAspDilTips > 0) and (not gErrorManager.IsGlobalErr()) then
    begin
        gLogManager.Log('Aspirate Diluent - START', true);
        gmAspirateDiluentVolume(xDllPar, aUsedArm, aAspDilTips, aDil, aPipRunStep.AspD);
        aDA.UpdateStatus(aPipListName, aPipListSeq, plsDilAspDone);
        gLogManager.Log('Aspirate Diluent - END', true);
    end;
    // -------------------------------------------------------------- Aspirieren + Mixen
    if (aAspSampleTips > 0) and (not gErrorManager.IsGlobalErr()) { and (aSource.Rack <> nil) } then
    begin
        gLogManager.Log('Aspirate Sample - START', true);
        gmAspirateSampleVolume(xDllPar, aUsedArm, aAspSampleTips, aSource, aPipRunStep.AspS);
        aDA.UpdateStatus(aPipListName, aPipListSeq, plsSampleAspDone);
        gLogManager.Log('Aspirate Sample - END', true);
    end;
    // -------------------------------------------------------------- Dispensieren + Mixen
    if (aDispTips > 0) and (not gErrorManager.IsGlobalErr()) { and (aDest.Rack <> nil) } then
    begin
        gLogManager.Log('Dispense - START', true);
        gmDispenseVolume(xDllPar, aUsedArm, aDispTips, aDest, aPipRunStep.Disp);
        gLogManager.Log('Dispense - END', true);
    end;

    aDA.UpdateStatus(aPipListName, aPipListSeq, plsDispDoneAndEnded);
end;


end.
