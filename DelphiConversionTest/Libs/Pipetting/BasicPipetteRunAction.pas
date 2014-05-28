{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Used by ASP,DISP,PIPET,Aspirate,Dispense,Pipette,(PListRework)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.02.04 pk                                TN1719  New
  24.02.04 pk  TWashManager                  TN1719  Bug fixed.  WorkType is now correctly saved to tip array
  02.03.04 wl  TDilutionManager.Dilution               TN1773   EventList is createted and filled - instead of SetSamplerEvent()
  02.03.04 wl  TDilutionManager.AspirateDiluentVolume  TN1773   new parameter: EventList for tube events
  02.03.04 wl  TDilutionManager.AspirateSampleVolume   TN1773   new parameter: EventList for tube events
  02.03.04 wl  TDilutionManager.DispenseVolume         TN1773   new parameter: EventList for tube events
  02.03.04 wl  TDilutionManager.DoPipette              TN1773   new parameter: EventList for tube events
  04.03.04 pk  TDilutionManager.Dilution               TN1766   Don't do anything with the tool if worktype=wtVirtual
  10.03.04 wl  TWashManager.SaveJobToTip               TN1797   Don't set tip status "dirty" if worktype=wtVirtual
  11.03.04 wl  TWashManager.Wash                       TN1800   PutHandlerTool auskommentiert
  05.04.04 wl  TWashManager.FWashRetrSpeed             TN1708   Parameter wird gespeichert und beim nächsten Waschen benutzt
  05.04.04 wl  AspirateDiluentVolume,AspirateSampleVolume,DispenseVolume  TN1708   ZScanMode, ZScanSpeed und ZRetractSpeed als Parameter
  05.04.04 wl  CalculatePipVolume,PickSysAir,AspirateDiluentVolume,AspirateSampleVolume,DispenseVolume  TN1788 aUsedArm als Parameter
  05.04.04 wl  TDilutionManager.DoPipette              TN1788   MegaSuperRedi komplett entfernt
  06.04.04 pk  DoPipette                               TN1844   aEventlist passed as parameter to gmDispensePowder, gmAspiratePowder instead of nil
  19.04.04 wl  TDilutionManager                        TN1788   Umstellung von TipStatus auf aUsedArm.Tips
  19.04.04 wl  TWashManager                            TN1788   Umstellung von TipArray auf aUsedArm.Tips
  19.04.04 wl  TDilutionManager.DoPipette              TN1788   LiquidHandling-Teile nach gmDoLiquidHandling (LiquidHandlingHigh) ausgelagert
  19.04.04 wl  MaxVal,SetSysAir,PickSysAir,AspirateDiluentVolume       TN1788   --> LiquidHandlingHigh
  19.04.04 wl  AspirateSampleVolume,DispenseVolume,LiquidDetectionOnly TN1788   --> LiquidHandlingHigh
  19.04.04 wl  TDilutionManager.CalculatePipVolume,StorePipPosVolumeToTipStatus  TN1788   --> DevicesTips
  19.04.04 wl  TWashManager.IsSampleAlreadyMixed,SaveJobToTip          TN1788   --> DevicesTips
  20.04.04 wl  TWashManager                            TN1788   --> SubstanceHandling
  22.04.04 wl  TDilutionManager.Dilution               TN1788   Log vor jeder Ausführung
  28.04.04 wl  Dilution                                TN1788   benutzt gmGetArm_ByTip & gmFindRobotArmToUse (SubstanceHandling)
  03.05.04 wl  Dilution                                TN1788   aUsedArm.Name statt .ArmName
  10.05.04 pk  DoPipette                               TN1902   when calling gmDoCalli passes LiqH.WashFlag as a Var parameter
  10.05.04 pk  Dilution                                TN1889   uses FindXRackPos to find racks
  10.05.04 wl  Dilution                                TN1788   nach dem Waschen muß UsedArm erneut gesetzt werden
  10.05.04 wl  Create                                  TN1788   löscht TipStatus komplett (wie in früheren Versionen)
  13.05.04 pk  Dilution                                TN1889   allow exception to be raised if source or dest rack not found
  15.05.04 pk  Dilution                                TN1889.1 Uses source, dest, dil rackpositions from new parameter: aDilutionRackPositions
  17.05.04 wl  Dilution                                TN1788   benutzt lokales xUseVolControl statt gUseVolControl
  17.06.04 pk  Dilution                                TN1976   uses Detectiononly constant
  17.06.04 pk  Dilution                                TN1995   Use Diluent from TDilutionRackPositions instead of from TJobArray
  07.07.04 tbh Dilution                                TN2022   tips are selected correctly for distribution of system liquid
  21.07.04 pk  StorePipPosVol                          TN2049   New: given a run record call St_StorePipPosVolume
  14.10.04 wl  Dilution                                TN2178   LQModes: Bezeichner mit INT_LQMODES_
  04.11.04 wl                                          TN2213   uses MethodTypes
  08.11.04 wl  DoPipette                               TN2213   TPipetteActionRunRec als Paramter
  08.11.04 wl  Dilution                                TN2213   TPipetteActionRunRec wird hier beschrieben
  17.11.04 pk                                          TN2231    uses RunDataAdaptorExt
  16.02.05 wl  Dilution                                TN2269   geänderter Aufruf von gmSwitchDiluentAndWash - unschön - muß anders werden!!!!!
  23.02.05 wl  Dilution                                TN2269   Fehler bei Diluent korrigiert
  11.04.05 pk  DoCalli                                 TN2373   extracted from DoPipette - attempt to reduce references to TRunRecord
  20.04.05 wl  Dilution                                TN2377   CalliMode ist Parameter und wird nicht mehr hier ermittelt
  20.04.05 wl  DoSimpleCalli                           TN2377   neu für 'MASSP' (Simple Calli) Action
  04.05.05 wl  Dilution                                TN2410   wtVirtual-Abarbeitung --> TFillManualAction
  14.06.05 pk  Dilution                                TN2464.2 Some code moved to GeneratePipSeq
  14.06.05 pk  Dilution                                TN2464.2 use PeriVol from aPipRunRec
  14.06.05 pk  Dilution                                TN2464.2 use worktype from aPipRunRec
  14.06.05 pk  Dilution                                TN2464.2 Logic of SumDilVol and SumAspVol replaced by xAspDilTips, xAspSampleTips
  14.06.05 pk  Dilution                                TN2464.2 Call to DoPipette moved. Now done at the end of the for loop
  20.06.05 tbh Dilution                                TN2385   für Volumenkorrektur muss Korrektur-Kurve aktiviert sein
  20.06.05 tbh StorePipPosVol                          TN2385   speichert Dilutorschritte für Aufnahme bei Multi-Dispense
  21.06.05 pk  Dilution                                TN2464.3 do St_ClearTipStatus(false) only for aUsedArm and NOT for all arms
  21.06.05 pk  Dilution                                TN2464.3 LiqH is no longer a pointer
  08.08.05 pk  Dilution                                TN2524   if gPipDifferentOptionsTogether=true then all steps to be combined even if they have different options
  26.08.05 pk  Dilution                                TN2566   Do not call gmSwitschDiluentAndWash if Diluent undefined -> for ASP and DSP actions
  26.08.05 pk  Dilution                                TN2566   Keep track of DispTips ( tips for which DestVol > 0 ) and pass DispTips to LiquidHandling function
  30.11.05 wl  gmSetCalliWRTipName                     TN2818    entfernt
  11.01.06 pk                                          TN2871.0  St_ClearTipStatus : ResetAll param removed
  20.01.06 pk  DoCalli                                 TN2891    aOptions changed from string to TCalliTubeRunRec
  20.01.06 pk                                          TN2862    Now supports Aps and Disp events for Calli
  25.04.06 wl  Dilution                                TN3053   AspSplitMinVolPercent wird an St_CalculatePipVolume weitergereicht
  11.05.06 wl  Dilution                                TN3096    nach einem Systemflüssigkeitswechsel müssen die UseTips erneut bestimmt werden
  14.06.06 wl  Dilution                                TN3155    xDispTips wird auch gesetzt wenn nur mit Kanal 2 aufgenommen wurde
  07.09.06 wl  DoSimpleCalli                           TN3287    entfernt - MASSP braucht den DilutionManager nicht mehr
  07.09.06 wl  DoCalli                                 TN3288    entfernt
  07.09.06 wl  Dilution                                TN3288    alle Bezüge auf Calli entfernt
  18.09.06 pk  Dilution                                TN3227.1  call switchdiulentandwash with usetips instead of OKTips
  26.09.06 wl                                          TN3326    geänderter Aufruf von gmTakeAndPutTool
  18.10.06 wl  DoPipette                               TN3362    Aufruf von AspiratePowder geändert
  07.12.06 wl                                          TN3243    uses TipPressureHandling
  30.08.07 pk                                          TN3840.2  Uses LiqHTypes
  09.01.08 wl                                          TN3972    DetectionOnly & TipPressure bis auf weiteres deaktiviert
  20.06.08 pk                                          TN4139    Uses changed
  03.07.08 wl                                          TN4157
  09.07.08 pk                                          TN4157    calls pipdevice.setusetips directly instead of gmsetusedtips
  08.09.08 pk                                          TN4215    all functions made into class functions
  20.09.08 pk TPipetteWashManager                      TN4215    from TWashManager
  31.07.09 wl  PrepareSampleAspMicCycles               TN4049    entfernt: Prfung auf SampleAspMixFirstOnly findet jetzt in gmAspirate statt
  31.07.09 wl  TPipetteWashManager.JobToWashJob        TN4049    entfernt: Übergabe SampleAspMixFirstOnly
  12.09.09 wl                                          TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  16.12.09 pk  Dilution                                TN4950   use the UsedTips and UsedTipType of the RunStep instead of the liquidhandling param
  21.06.10 wl                                          TN5159   geänderter Aufruf von St_CalculatePipVolume
  29.06.10 pk  Dilution                                TN5143   changes for using different LiqParams together
  06.04.11 wl                                          TN5501    Liquid-Handling-Daten brauchen nicht mehr neu geladen werden
  19.07.11 wl  StorePipPosVol                          TN5630   DTransAirRetake boolean statt double
  27.09.11 wl                                          TN5698   verwendet TLiqHandlingData für TPipetteRunStep
  31.10.11 wl  UseVolCorrCurve                         TN5731   UseVolCorr wird nicht mehr zentral festgelegt
  14.12.11 wl                                          TN5765   uses geändert
  02.02.11 wl                                          TN5791   verwendet TArray<TXRackPosition> statt TXRackPositions
  10.12.12 wl  IsFinalPip                              TN6049   entfernt
  25.04.13 wl  MustWashBeforeExecute                   TN6139   entfernt
  25.04.13 wl  TipsNeedingWash                         TN6139.1 wenn SourceVol+DilVil = 0, wird nicht geprüft, denn es handelt sich um einen reinen Dsipense-Schritt
  25.04.13 wl  SaveJobs                                TN6139.1 wenn SourceVol+DilVil = 0, wird WashJob nicht gespeichert
  30.04.13 wl  TCombinedActivePipetteRunStep           TN6139.2 umbenannt (um zu verdeutlichen, dass diese nur von ASP,DISP und PIPET verwendet wird)
  30.04.13 wl  TLastWashReoperationTask                TN6139.2 neu: Ersetzt das grundsätzliche Waschen im ActionHandler
  12.06.13 wl  Dilution                                TN6172   verwendet PipetteListName und PipetteListSeq (Array)
  06.08.13 wl  Dilution                                TN6210   Abbruchkriterium für Verdünnungsreihen --> BasicPipetteRunStepBatcher
  21.08.13 wl                                          TN6231   an Änderungen in ToolHandling angepasst
  -------------------------------------------------------------------------------------------------- }

unit BasicPipetteRunAction;


interface


uses
    RunAction,
    RunStep,
    AppTypes,
    IntfArmDevice,
    RackTypes,
    MethodTypes,
    RunStepBatcher,
    LiqHTypes,
    IntfPipDevice,
    TipSystem,
    BasicPipetteRunStep,
    BlockRunAction,
    BasicPipetteTypes;

type
    TCombinedActivePipetteRunStep = class(TCombinedBasicPipetteRunStep);

    TBasicPipetteRunActionCreator = class(TBlockRunActionCreator)
    protected
        function CreateRunStepBatcher(const aMultiRunStep: TCompositeRunStep): TRunStepBatcher; override;
        function CreateMultiRunStep(): TCompositeRunStep; override;
    end;

    TLastWashReoperationTask = class(TRunActionReoperationTask)
    strict private
        fUsedArm: IArmDevice;
    public
        constructor Create(aUsedArm: IArmDevice);
        procedure Execute(aCurrentRunStep: TRunStep); override;
    end;

    TBasicPipetteRunAction = class(TRunAction)
    private
        // ToolHandling-Funktionen
        class procedure PutCombinedArmTool(aUsedArm: IArmDevice);
        class procedure TakeAndPutTool(aUsedArm: IArmDevice);
        // DilutionManager-Funktionen
        class function JobToWashJob(const aStep: TBasicPipetteRunStep): TWashJob;
        class function TipsNeedingWash(const aPipDevice: IPipDevice;
            const aPipRunStep: TCombinedActivePipetteRunStep): TIPMAP;
        class procedure WashIfNeeded(const aUsedArm: IArmDevice;
            const aPipRunStep: TCombinedActivePipetteRunStep);
        class procedure SaveJobs(const aPipRunStep: TCombinedActivePipetteRunStep; aWorkType: TWorkType);
        class function PipWorkTypeName(aWorkType: TWorkType): string;
        class procedure DoPipette(aUsedArm: IArmDevice; aWorkType: TWorkType;
            const aSource, aDest: TArray<TXRackPosition>; const aDil: TDilRackPositions;
            aLastSysAirAspPos: integer; aTips, aAspSampleTips, aAspDilTips, aDispTips: TipMap;
            const aPipRunStep: TCombinedActivePipetteRunStep; const aPipListName: string;
            const aPipListSeq: TArray<integer>);
        class procedure StorePipPosVol(const aPipDevice: IPipDevice; aArmTip: integer;
            aPipStep: TBasicPipetteRunStep; aDilAspCh2: boolean);
        class procedure Dilution(const aPipRunStep: TCombinedActivePipetteRunStep);

        function GetRunStep: TCombinedActivePipetteRunStep;
        property RunStep: TCombinedActivePipetteRunStep read GetRunStep;
    public
        procedure ExecFirst(); override;
        function CreateReoperationTask: TRunActionReoperationTask; override;
    end;


implementation


uses
    SysUtils,
    ErrorManager,
    LogManager,
    CommonTypes,
    ObjModul,
    PipetteListDataAdaptor,
    LiquidHandlingHigh,
    PowderHandlingHigh,
    ToolHandling,
    WashHandling,
    TipMapUtils,
    PipDeviceManager,
    BasicPipetteRunStepBatcher;

{ TBasicPipetteRunActionCreator }

function TBasicPipetteRunActionCreator.CreateMultiRunStep: TCompositeRunStep;
begin
    result := TCombinedActivePipetteRunStep.Create();
end;

function TBasicPipetteRunActionCreator.CreateRunStepBatcher(const aMultiRunStep: TCompositeRunStep)
    : TRunStepBatcher;
begin
    result := TBasicPipetteRunStepBatcher.Create(aMultiRunStep as TCombinedActivePipetteRunStep);
end;

{ TBasicPipetteRunAction }

class procedure TBasicPipetteRunAction.PutCombinedArmTool(aUsedArm: IArmDevice);
begin
    { TODO : combined arm }
    {
      // Tool am CombinedArm immer ablegen! ( Tool am ConcurrentArm (--> SIAS) nicht ablegen! )
      if (aUsedArm.CombinedArm is TGripperArmDevice) then begin
      (aUsedArm.CombinedArm as TGripperArmDevice).PutHandlerTool;
      end;
    }
end;

class procedure TBasicPipetteRunAction.TakeAndPutTool(aUsedArm: IArmDevice);
// var xGrpArm: TGripperArmDevice;
// var
// xPipToolName : string;
begin
    if not Assigned(aUsedArm.GripDevice) then
        exit; // Tool-Handling nur für Gripper-Arm

    // Pipettier-Tool aufnehmen
    { TODO -oPK -cDEVICE : Removalblepipdevice }
    { if (aUsedArm.PipDevice is TRemovablePipDevice) then begin
      xPipToolName := ((aUsedArm.PipDevice as TRemovablePipDevice).PossiblePipToolName );
      gmGetHandlerTool(aUsedArm, xPipToolName);
      end
      else begin
      // andere Tools ablegen
      PutHandlerTool( aUsedArm );
      end;
    }
end;

class function TBasicPipetteRunAction.JobToWashJob(const aStep: TBasicPipetteRunStep): TWashJob;
begin
    with result do
    begin
        IsPipAction := true; // set to always true for now!
        SRack := aStep.SRack;
        SPos := aStep.SPos;
        SourceVol := aStep.SourceVol;
        if (aStep.DilVol > 0) and (aStep.DilRack <> 'SYSTEM') then
            DilRackVol := aStep.DilVol
        else
            DilRackVol := 0;
        DispMixVol := aStep.LiqHRec.DispMix.Vol;
        SampleAspMixVol := aStep.LiqHRec.SampleAspMix.Vol;
        Wash := aStep.LiqHRec.Wash.ReadData;
        UsedTipType := aStep.LiqHRec.UsedTipType;
        DirtyBatchID := TWashHandling.GetCurrentBatchID();
    end;
end;

class procedure TBasicPipetteRunAction.SaveJobs(const aPipRunStep: TCombinedActivePipetteRunStep;
    aWorkType: TWorkType);
var
    i: integer;
    xPipDevice: IPipDevice;
    xWashJob: TWashJob;
begin
    for i := 0 to aPipRunStep.Count - 1 do
    begin
        xPipDevice := gPipDeviceManager.FindPipDevice_ByName(aPipRunStep[i].PipDeviceName);
        TWashHandling.VerifyTipNumber(xPipDevice, aPipRunStep[i].Tip);

        xWashJob := JobToWashJob(aPipRunStep[i]);

        // Wenn keine Flüssigkeit aufgenommen wurde, wird der letzte Zustand beibehalten
        if (xWashJob.SourceVol > 0) or (xWashJob.DilRackVol > 0) then
            xPipDevice.Tips[aPipRunStep[i].Tip].Wm_SaveJobToTip(xWashJob, aWorkType);
    end;
end;

class function TBasicPipetteRunAction.TipsNeedingWash(const aPipDevice: IPipDevice;
    const aPipRunStep: TCombinedActivePipetteRunStep): TIPMAP;
var
    x: integer;
    xTip: TTipSystem;
    xJobArmTipIndex: integer;
    xJobPipDevice: IPipDevice;
    xWashJob: TWashJob;
begin
    // Which tips are needed in the next step but are dirty?

    result := 0; // No tips will be washed

    for x := 0 to aPipRunStep.Count - 1 do
    begin
        xJobPipDevice := gPipDeviceManager.FindPipDevice_ByName(aPipRunStep[x].PipDeviceName);
        xJobArmTipIndex := aPipRunStep[x].Tip;
        ASSERT((aPipDevice = xJobPipDevice), 'TotalTip does not exist for expected arm');

        xTip := aPipDevice.Tips[xJobArmTipIndex];
        ASSERT(Assigned(xTip), Format('No tip found at TotalTip index [%d]', [xJobArmTipIndex]));

        xWashJob := JobToWashJob(aPipRunStep[x]);

        // Wenn andere Flüssigkeit aufgenommen wurde als vorher, soll gewaschen werden
        if ((xWashJob.SourceVol > 0) or (xWashJob.DilRackVol > 0)) and (xTip.Wm_TipNeedsWash(xWashJob)) then
            TTipMapUtils.SelectTip(result, xJobArmTipIndex);
    end;
end;

class procedure TBasicPipetteRunAction.WashIfNeeded(const aUsedArm: IArmDevice;
    const aPipRunStep: TCombinedActivePipetteRunStep);
var
    xTipsNeedingWash: TIPMAP;
begin
    if (aUsedArm = nil) then
        EXIT;

    // den aktuellen Arm nur waschen, wenn sich etwas geändert hat
    xTipsNeedingWash := TipsNeedingWash(aUsedArm.PipDevice, aPipRunStep);

    TWashHandling.ExecuteWash(aUsedArm, xTipsNeedingWash);
end;

class procedure TBasicPipetteRunAction.DoPipette(aUsedArm: IArmDevice; aWorkType: TWorkType;
    const aSource, aDest: TArray<TXRackPosition>; const aDil: TDilRackPositions; aLastSysAirAspPos: integer;
    aTips, aAspSampleTips, aAspDilTips, aDispTips: TipMap; const aPipRunStep: TCombinedActivePipetteRunStep;
    const aPipListName: string; const aPipListSeq: TArray<integer>);
var
    xVarixMotorSteps: integer;
    xDA: TPipetteListDataAdaptor;
begin
    if (aTips or aAspSampleTips or aAspDilTips <= 0) then
        Exit;

    xDA := TPipetteListDataAdaptor.Create;
    try

        case aWorkType of
            wtLiquid:
                begin // =============================== Standard Pipettierroutinen ===============

                    gmDoLiquidHandling(aUsedArm, aSource, aDest, aDil, aLastSysAirAspPos, aDispTips,
                        aAspSampleTips, aAspDilTips, aPipRunStep, xDA, aPipListName, aPipListSeq)
                end;
            wtPowder:
                begin // =========================================== Redi  ===========================
                    if (not gErrorManager.IsGlobalErr) and (aAspSampleTips > 0) then
                    begin
                        xVarixMotorSteps := -1;
                        gmAspiratePowder(aUsedArm, aAspSampleTips, aSource, xVarixMotorSteps,
                            aPipRunStep.AspS);
                    end;
                    if (not gErrorManager.IsGlobalErr) then
                        gmDispensePowder(aUsedArm, aTips, aDest, aPipRunStep.Disp);
                end;
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

class function TBasicPipetteRunAction.PipWorkTypeName(aWorkType: TWorkType): string;
begin
    case aWorkType of
        wtLiquid:
            EXIT('Liquid handling');
        wtPowder:
            EXIT('Powder handling');
        else
            EXIT('');
    end;
end;

function TBasicPipetteRunAction.CreateReoperationTask: TRunActionReoperationTask;
begin
    if (self.RunStep.WashAfterLastDispense) then
        EXIT(TLastWashReoperationTask.Create(self.RunStep.UsedArm))
    else
        EXIT(nil);
end;

class procedure TBasicPipetteRunAction.Dilution(const aPipRunStep: TCombinedActivePipetteRunStep);
// --------------------------------------------------------------------------------------------------
// WARNING: SRack, SPos, DRack, DPos, DilRack, DPos, Dil
// MUST BE ACCESSED VIA aDilutionRackPositions and NOT aJobArray!
// WARNING: LIQH MUST BE ACCESSED VIA aLiqH and NOT aJobArray!
// --------------------------------------------------------------------------------------------------
var
    xSource, xDest: TArray<TXRackPosition>;
    xDil: TDilRackPositions;
    SampleCnt, xCurrentSample: integer;
    xArmTip, xActArmTip: integer;
    xAspDilTips, xAspSampleTips, xUsedArmTips, xErgArmTips, xDispTips: TipMap;
    AuftCnt: integer;
    xJobIndex: integer;
    xWorkType: TWorkType;
    // xFinalJob : TBasicPipetteRunStep;
    xDilIndex: integer;
    xPipDevice: IPipDevice;
    xPipListName: string;
    xPipListSeq: TArray<integer>;
begin
    xPipDevice := aPipRunStep.UsedArm.PipDevice;

    // -- 1 -- UseTips zum 1. Mal bestimmen!!!
    xPipDevice.SetUseTips(aPipRunStep.UsedTipType, aPipRunStep.UsedTips, true);

    WashIfNeeded(aPipRunStep.UsedArm, aPipRunStep);

    // -- 2 -- nach dem Waschen: unbedingt noch mal UseTips bestimmen!!!
    xPipDevice.SetUseTips(aPipRunStep.UsedTipType, aPipRunStep.UsedTips, true);

    // Worktype aus dem Tip lesen !!!
    if (xPipDevice.FirstUsedTipType in [RediTip, VarRediTip]) then
        xWorkType := wtPowder
    else
        xWorkType := wtLiquid;

    xSource := TXRackPositionUtils.GetEmptyXRackPositions(xPipDevice.TipCount);
    xDest := TXRackPositionUtils.GetEmptyXRackPositions(xPipDevice.TipCount);
    xDil := TBasicPipetteRunStep.GetEmptyDilRackPositions(xPipDevice.TipCount);
    xCurrentSample := 0;
    xUsedArmTips := 0;
    xPipListName := '';
    xPipListSeq := xPipDevice.GetNullIntArray();

    // ---------------- Lösungsmittel einstellen ------------------------------
    // 26.08.05 pk : A dilindex less than 0 means -> do not switch diluent!
    xDilIndex := aPipRunStep.RackPositions.Dil.Diluent - 1; // run diluent is offset by one
    if (xDilIndex) >= 0 then
        TWashHandling.SwitchDiluentAndWash(aPipRunStep.UsedArm, xPipDevice.UseTips, xDilIndex);

    xPipDevice.St_ClearTipStatus();

    // -- 3 -- nach dem Lösemittelwechsel-Flush: unbedingt noch mal UseTips bestimmen!!!
    xPipDevice.SetUseTips(aPipRunStep.UsedTipType, aPipRunStep.UsedTips, true);

    // ==================================================================================================
    while (xCurrentSample <= (aPipRunStep.Count - 1)) do
    begin // =============== Main Loop
        // ==================================================================================================
        // ---------------------------------------------------------------------- Tips zuweisen
        // This loop will increment SampleCnt until one of the conditions are met
        // It will also set xFirstArmTip which is the arm tip when SampleCnt = 0
        // and will set  xActArmTip which is the last arm tip before the loop breaks
        if (gErrorManager.IsGlobalErr) then
            BREAK;
        SampleCnt := 0;
        xUsedArmTips := 0;
        xJobIndex := -1;
        while true do
        begin

            // ----- Ende der Liste erreicht ---------------------------------------------------------
            xJobIndex := xCurrentSample + SampleCnt;
            xActArmTip := aPipRunStep[xJobIndex].Tip;
            TTipMapUtils.SelectTip(xUsedArmTips, xActArmTip);

            // ----- Letzter Tip erreicht ------------------------------------------------------------
            if (xActArmTip = xPipDevice.TipCount) then
                BREAK;

            // Next step does not exist
            if (xJobIndex + 1) >= aPipRunStep.Count then
                BREAK;

            // ----- nächster Tip -------------------------------------------------
            SampleCnt := SampleCnt + 1;
        end; // ----------------------------------------------------------------- End While

        ASSERT(xJobIndex >= 0);
        // xFinalJob     := aPipRunStep[ xJobIndex ];


        // Purpose : Split up steps that have Volume > dilutor Max
        // this is done in St_CalculatePipVolume
        // For each tip that must pipet Volume > dilutor Max Save the remaining volume ( i.e. remaining := Volume - dilutor max )
        // that could not be pipetted in the first round and pipett in the next round
        // Keep doing rounds until there is no remaining volume for any tip

        xErgArmTips := 0; // is updated when the remaining volume for a tip = 0
        xArmTip := 0;

        while (xErgArmTips <> xUsedArmTips) and (not gErrorManager.IsGlobalErr) do
        begin
            xAspDilTips := 0;
            xAspSampleTips := 0;
            xDispTips := 0;
            for AuftCnt := 0 to SampleCnt do
            begin
                xJobIndex := xCurrentSample + AuftCnt;
                xArmTip := aPipRunStep[xJobIndex].Tip;
                // --------------------------------------------------------------------- Liquidhandling lesen
                // 17.06.05 pk : St_LhPtr is no longer a pointer
                xPipDevice.Tips[xArmTip].St_LhPtr := aPipRunStep[xJobIndex].LiqHRec.ReadData;

                // ---------------------------------------------------------------------- Racknamen zuweisen
                xSource[xArmTip].Rack := aPipRunStep.RackPositions.Source[xJobIndex].Rack;
                xSource[xArmTip].Pos := aPipRunStep.RackPositions.Source[xJobIndex].Pos;

                xDest[xArmTip].Rack := aPipRunStep.RackPositions.Dest[xJobIndex].Rack;
                xDest[xArmTip].Pos := aPipRunStep.RackPositions.Dest[xJobIndex].Pos;

                xDil.DilRackType := aPipRunStep.RackPositions.Dil.DilRackType;
                if xDil.DilRackType = drNormal then
                begin
                    xDil.Positions[xArmTip].Rack := aPipRunStep.RackPositions.Dil.Positions[xJobIndex].Rack;
                    xDil.Positions[xArmTip].Pos := aPipRunStep.RackPositions.Dil.Positions[xJobIndex].Pos;
                end;

                // PipList-Daten zuweisen
                if (xPipListName = '') then
                    xPipListName := aPipRunStep[xJobIndex].PipetteListName;
                xPipListSeq[xArmTip] := aPipRunStep[xJobIndex].PipetteListSeq;

                // ------------------------------------------------------------------- Gesamtvolumen speichern
                StorePipPosVol(xPipDevice, xArmTip, aPipRunStep[xJobIndex],
                    aPipRunStep[xJobIndex].LiqHRec.DilAspChannel2);

                // --------------------------------------------------------- Pipettier - Volumen zuweisen
                xPipDevice.Tips[xArmTip].St_CalculatePipVolume(aPipRunStep.IsMultiPip,
                    aPipRunStep[xJobIndex].LiqHRec.AspSplitMinVolPercent);

                // ---------------------------------------------------------------------- AspTip, DilTip, DispTip - Bit Masken zuweisen
                if (xPipDevice.Tips[xArmTip].St_SVol > 0) then
                    TTipMapUtils.SelectTip(xAspSampleTips, xArmTip);

                if (xPipDevice.Tips[xArmTip].St_DilVol > 0) then
                    TTipMapUtils.SelectTip(xAspDilTips, xArmTip);

                if (xPipDevice.Tips[xArmTip].St_DVol > 0) or
                    ((xPipDevice.Tips[xArmTip].St_DilVol > 0) and
                    (xPipDevice.Tips[xArmTip].St_DilVolWithCh2)) then
                    TTipMapUtils.SelectTip(xDispTips, xArmTip);
                // ---------------------------------------------------------------------- ErgTip - Bit Masken zuweisen
                if (xPipDevice.Tips[xArmTip].St_SaveSVol + xPipDevice.Tips[xArmTip].St_SaveDilVol) = 0 then
                    TTipMapUtils.SelectTip(xErgArmTips, xArmTip);

            end; // ------------------------------------------------------------------ Ende FOR AuftCnt := 0 to SampleCnt

            gLogManager.LogF('-- %s Step -- (Used Arm: %s)', [self.PipWorkTypeName(xWorkType),
                xPipDevice.Name], true);

            // Pipettier-Tool aufnehmen/ablegen
            PutCombinedArmTool(aPipRunStep.UsedArm);
            TakeAndPutTool(aPipRunStep.UsedArm);

            DoPipette(aPipRunStep.UsedArm, xWorkType, xSource, xDest, xDil,
                xPipDevice.Tips[xArmTip].St_LhPtr.SysAirAspPos, xUsedArmTips, xAspSampleTips, xAspDilTips,
                xDispTips, aPipRunStep, xPipListName, xPipListSeq);

        end; // -------------------------------------------------------------------- Ende while (ErgTips <> UsedTips)

        // ------------------------------------------------------------ nächste Probe
        xCurrentSample := xCurrentSample + SampleCnt + 1;

    end; // ========= Ende while (xCurrentSample <= NoOfSamples) Main Loop

    SaveJobs(aPipRunStep, xWorkType);
end;

class procedure TBasicPipetteRunAction.StorePipPosVol(const aPipDevice: IPipDevice; aArmTip: integer;
    aPipStep: TBasicPipetteRunStep; aDilAspCh2: boolean);
begin
    aPipDevice.Tips[aArmTip].St_StorePipPosVolume(aPipStep.SourceVol, aPipStep.DestVol, aPipStep.DilVol,
        aPipStep.DTransAirDisp, aPipStep.DTransAirRetake, aPipStep.DilSteps, aDilAspCh2)
end;

procedure TBasicPipetteRunAction.ExecFirst;
begin
    Dilution(self.RunStep);
end;

function TBasicPipetteRunAction.GetRunStep: TCombinedActivePipetteRunStep;
begin
    result := fRunStep as TCombinedActivePipetteRunStep;
end;

{ TLastWashReoperationTask }

constructor TLastWashReoperationTask.Create(aUsedArm: IArmDevice);
begin
    inherited Create;
    fUsedArm := aUsedArm;
end;

procedure TLastWashReoperationTask.Execute(aCurrentRunStep: TRunStep);
begin
    // Handelt es sich um einen ASP-,DSP-,PIPET-,Aspirate-,Dispense- oder Pipette-Schritt?
    if (aCurrentRunStep is TCombinedActivePipetteRunStep) then
    begin
        if ((aCurrentRunStep as TCombinedActivePipetteRunStep).UsedArm = fUsedArm) then
        begin
            fCanBeDeleted := true;
            EXIT; // Gleicher Arm: Das Waschen wird vom nächsten Schritt übernommen
        end;
    end;

    fCanBeDeleted := true;
    TWashHandling.WashDirtyTips(fUsedArm);
end;


end.
