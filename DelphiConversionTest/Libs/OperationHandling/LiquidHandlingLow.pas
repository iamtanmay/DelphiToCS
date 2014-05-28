{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : low level liquid handling functions
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  02.03.04 wl                               TN1773   initial version
  12.03.04 wl  gmFetchTips                  TN1812   MoveXY-Befehl: statt ZTravel-Wert wird aktuelles Rack übereben
  05.04.04 wl  gmEmptyDilutor,CahngeValvePOs1To4  TN1788   von SamCmd hierher
  05.04.04 wl  gmGetTipPositions            TN1788   von AppInterface hierher
  05.04.04 wl  alle Funktionen!             TN1788   aUsedArm wird immer als Parameter übergeben und immer benutzt (wenn irgend möglich)
  05.04.04 wl  gmFetchTips,gmDropTips       TN1788   aUsedArm muß TPipArmDevice sein
  20.04.04 wl  gmGetDiTiPositions           TN1788   benutzt jetzt TDitiPositions (RackTypes)
  22.04.04 wl  gmFetchTips,gmDropTips       TN1788   --> LiquidHandlingDiTi
  22.04.04 wl  gmPickLiquid,gmDispLiquid,gmCalcRestVol,gmFlushSyringe   TN1788 von LiquidHandling hierher
  22.04.04 wl  gmFlushSyringe               TN1788.8 Logging in Log-Diplay bei jeden Wash/Flush-Vorgang
  10.05.04 pk  gmMoveToWashOrWasteRack      TN1889.1 Use Wb.FindByRackName
  10.05.04 wl  gmCalcRestVol                TN1788   Bugfix: falscher Dilutor
  10.05.04 wl  gmEmptyDilutors              TN1788   Bugfix: falscher Dilutor
  14.05.04 wl  gmPickLiquid,gmDispLiquid    TN1922.1 durch Abs()-Funktion werden nagative Speeds unmöglich
  01.07.04 wl                               TN1963   Anpassung an Änderungen in TPipStep
  11.09.04 wl  gmMoveToWashOrWasteRack      TN2123   ohne Movement -> TRoboticArm.CalcSingleStepRack
  11.09.04 wl  gmEmptyDilutor               TN2123   Benutzt aUsedArm.CalcSingleStepRack statt gmMoveToWashOrWasteRack
  13.10.04 wl  gmDetectLiquid               TN2151   aus SamHigh hierher verschoben
  03.02.05 wl  gmDetectLiquid               TN2297.7 Beschränkung auf TPipArm aufgehoben
  16.02.05 wl  gmDispPeripump               TN2269   von LiquidHandling.pas hierher, Update der Füllstände immer auf SystemLiquid 1
  16.02.05 wl  gmFlushPeripump              TN2269   ruft gmDispPeripump auf
  16.02.05 wl  gmFlushSyringe               TN2269   Update der Füllstände entfernt
  18.04.05 wl  gmFlushSyringe               TN2379   Application.ProcessMessages entfernt
  19.04.05 wl  gmFlushSyringe               TN2380.1 Anpassung an Änderung von TPipPumpDevice.Pick
  20.06.05 tbh gmPickLiquid                 TN2385   angepasst zur Verwendung Dilutormotor-Schritte bei Volumenkorrektur
  20.06.05 tbh gmDispLiquid                 TN2385   angepasst zur Verwendung Dilutormotor-Schritte bei Volumenkorrektur
  20.06.05 tbh gmFlushSyringe               TN2385   Aufrufe aktualisiert
  23.06.05 wl  gmFlushSyringe               TN2463.1 Wenn Rack = nil (Layout enthält kein WASTE-Rack) wird kein Flush durchgeführt
  08.08.05 pk  gmDetectLiquidBasic          TN2527   Just detect - used for tip touch
  16.08.05 pk                               TN2550   uses InterfacePipPump
  22.09.05 pk  gmFindWashRackForArm         TN2622   New : tries to find rack with name WASH_<ArmDeviceName>
  22.09.05 pk  gmEmptyDilutor               TN2622   calls gmFindWastRackForArm
  14.10.05 wl  Wash/WasteRack-Funktionen    TN2672   Rack-Namenskonventionen --> RackTypes
  14.10.05 wl  gmFindWash/WasteRack         TN2672   benutzen WB.FindSpecialRack
  08.11.05 wl  gmFindWasteRack              TN2740   statt WASTE-Rack wurde immer WASH-Rack benutzt
  09.11.05 wl  gmDetectLiquidBasic          TN2728    --> Operator
  09.11.05 wl  gmDetectLiquid               TN2728    benutzt TArmZMovementOperator
  17.11.05 wl                               TN2771    TOperation statt TOperator
  19.04.06 wl                               TN3051    benutzt TipFloatArray und extended für alle Vol, Speed und VolSteps
  23.05.06 pk  gmDispLiquid                 TN3114    Use absolute move instead of relative
  24.08.06 wl                               TN3271    bei den Aufrufen von gmCalculatePipSteps wird wieder PaintTubes übergeben
  12.09.06 thr                              TN3272   Anpassung wegen geändertem Pippump-Interface
  23.10.06 wl  gmPickLiquid                 TN3375   ruft PipDevice.WaitFor auf, Pick-Aufruf mit Wait = false
  23.10.06 wl  gmDispLiquid                 TN3375   ruft PipDevice.WaitFor auf, Disp-Aufruf mit Wait = false
  14.02.07 wl  gmMoveToWasteRack            TN3147    geänderter Aufruf von gmCalculatePipStep, alles in try-finally-Blöcken
  08.03.07 wl  gmFlushSyringeOrPeripump     TN3620    gModules.FindPeriPump statt ModuleExist(PeriPump)
  08.03.07 wl  gmDispPeriPump               TN3623    benutzt TPeriPumpDevice
  09.11.07 pk                               TN3924    Steps changed to mm
  29.01.08 wl                               TN3980   uses geändert
  25.04.08 wl  gmDispLiquid                 TN4050    Parameter von var in const geändert
  25.04.08 wl  gmDispPeriPump               TN4063    Volume parameter const istead of var
  25.04.08 pk  gmPickLiquid                 TN4086    Use absolute move for level tracking
  04.06.08 pk  gmPickLiquid                 TN4135    Speed of ZMotor for level tracking was calculated as mm/secs instead of steps/sec
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  18.12.08 wl  gmDispLiquid                 TN4318    aExecute: Exceute am Schluss kann weggelassen werden; Valves werden aber in jedem Fall gedreht!
  28.07.09 wl  gmDispLiquid                 TN4682    Tracking-Weg und -Speed werden jetzt richtig berechnet
  28.08.09 pk                               TN4753    uses changed
  12.09.09 wl  TLiquidHandlingLow           TN4740    alle Funktionen werden zu Klassen-Methoden
  12.09.09 wl                               TN4740    TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  10.12.09 ts  DispLiquid                   TN4921    use aVInxCh1/2 instead of const V_SYR_TIP
  10.12.09 pk  DispLiquid                   TN4931    Do not dispense with channel 2 if volume for channel 2 = 0
  15.12.09 ts  PickLiquid                   TN4947    if NoAspBeforeDispDil-setting of PipPump is true and liquid is V_SYR_SYS no picking should be done
  15.12.09 pk  PickLiquid                   TN4947    Bug fixed (always skipped system liquid aspriation)
  29.01.10 pk  Pick/DispLiquid              TN4966    New aTurnValves boolean parameter, if false dont turn valves
  29.01.10 pk  TurnValves                   TN4966    New
  29.06.10 pk                               TN5173    various changes to allow pipetting sample with second pump
  15.07.10 pk  Pick/DispLiquid              TN5196    Pumps executed first and then ZMotors
  21.07.10 pk  FlushSyringe                 TN5185    Now with correct Disp Speeds for Channel 2
  27.07.10 ts  Pick/DispLiquid              TN5012    Änderungen, damit bei PUMPA/D-Actions PipDevice ohne Arm genutzt werden kann
  12.08.10 wl  FlushSyringes                TN5227    an TipSystem angepasst
  12.08.10 wl  TLiquidChannels              TN5227    kapselt cChannel1Index,cChannel2Index, erzeugt PipPumpArrays
  12.08.10 wl  TurnValves                   TN5227    Channel spielt keine Rolle mehr, entfernt
  27.01.11 wl  DispLiquid                   TN5449    Dispense time wird ins log geschrieben
  29.03.11 wl                               TN5524   uses OperationPip entfernt
  05.04.11 wl                               TN5535   ExecutePipPumps jetzt überall mit TipMap
  11.04.11 wl  CopyArray                    TN5549   ist jetzt generisch
  20.09.11 wl                               TN5723   gmCalculatePipStep jetzt ohne Parameter
  02.02.12 wl  alle Funktionen              TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  16.02.12 wl  FlushCalcRestVol             TN5807   Spezialfall Flush: Hier zählt das echte Dilutorvolumen, egal was im TipType steht
  15.03.12 wl  PickLiquid,DispLiquid        TN5837   WaitFor für den 2.Kanal nachgetragen
  29.03.12 wl  alle Funktionen              TN5791   Restarbeiten Rck-übergreifendes Pipettieren
  18.06.12 wl  DispPeriPump                 TN5899   Bei Liquids-Methoden muss jetzt gRunFlow.SimulationMode mit übergeben werden
  08.08.12 wl  DispPeriPump                 TN5947   überarbeitet: PeriPump muss jetzt im PipPumpDevice eingetragen sein
  10.08.12 wl  FlushSyringeCh1,Ch2          TN5947 --> WashProgramHandling
  10.08.12 wl  FlushSyringe,FlushSyringeOrPeriPump  TN5947 aPumpIndices: Pumpennummer der ersten Pumpe kann bestimmt werden
  15.02.13 wl  DispPeriPump                 TN5914   and Änderungen in SubstanceHandling angepasst
  24.04.13 wl                               TN6137   TLiquids.Instance statt gLiquids
  23.05.13 wl                               TN6153   geänderte Parameter bei MoveXY
  21.08.13 wl  FlushAfterInit,Dry,PickAir   TN6249   von LiquidHandling hierher
  11.09.13 wl  PickLiquid,DispLiquid        TN6249   neuer Parameter aInWashOrWaste (Array)
  17.09.13 wl  FlushSyringe                 TN6252   PumpIndex wird jetzt auch wirklich an Unterfunktionen weitergegeben
  17.09.13 wl  EmptyAllDilutors             TN6252.1 Vor EmptyDilutor werden immer auch alle Ventile in die richtige Position gebracht
  17.09.13 wl  EmptyDilutor                 TN6252.1 verwendet EmptyAllDilutors
  17.09.13 wl  FlushCalcRestVol             TN6252.2 kann auch für unterschiedliche PumpIndices berechnen
  25.09.13 wl  GetChannelVolumesBracketText TN6259   Neu: Schreibt Volumen in der Form [100,200(Ch2),300(Ch3),400(Ch4)]
  25.09.13 wl                               TN6259   Verbessertes Logging
  30.09.13 wl  FlushSyringeOrPeripump       TN6260   Volumes-Parameter werden nicht mehr verändert
  20.11.13 ts  EmptyAllDilutors             TN6307   the valves should not be moved if it is an init of the pump
  -------------------------------------------------------------------------------------------------- }

unit LiquidHandlingLow;


interface


uses
    AppTypes,
    CommonTypes,
    Rack,
    RackTypes,
    IntfPipPumpDriver,
    IntfMotorDriver,
    IntfPipDevice,
    GeneralTypes,
    IntfPeriPumpDevice,
    IntfArmDevice,
    IntfSwitchDevice,
    MotionSystemPipMove;

type
    TLiquidChannels = record
    public const
        cChannel1Index = 0;
        cChannel2Index = 1;
    public
        class function GetPumpIndexArray(const aPipDevice: IPipDevice; const aChannelIndex: integer)
            : TIntArray; static;
        class function GetDefaultChannel1Array(const aPipDevice: IPipDevice): TIntArray; static;
        class function GetDefaultChannel2Array(const aPipDevice: IPipDevice): TIntArray; static;
    end;

    TDrySwitchDeviceType = (dsdN2Channel, dsdVacuumPump, dsdDryValve);
    TEmptyDilutorType = (edtInit, edtDispAll);

    TLiquidHandlingLow = class
    private
        class procedure DispPeriPump(aPipDevice: IPipDevice; aPeriPump: IPeriPumpDevice; aSyrMap: TIPMAP;
            const aVol: TDoubleArray; const aRP: TArray<TXRackPosition>);
        class function FlushCalcRestVol(aPipDevice: IPipDevice; const aPumpIndices: TArray<integer>;
            out oVol: TDoubleArray; var vRestVol: TDoubleArray): extended;
    public
        class procedure TurnValves(aPipDevice: IPipDevice; aSyrMap: TIPMAP; aVInx: TValvePos;
            const aPumpIndices: TIntArray; aExecute: boolean);
        class procedure PickLiquid(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aSyrMap: TIPMAP; const aCh1PumpIndices: TIntArray; const aVol, aVolSpeed: TDoubleArray;
            aVInx: TValvePos; const aCh2Vol, aCh2Speed: TDoubleArray; aCh2VInx: TValvePos;
            const aVolStepsCh1, aVolStepsCh2: TDoubleArray; const aTWay: TDoubleArray;
            const aInWashOrWaste: TArray<boolean>; aTurnValves, aExec, aUseVolCorrect: boolean);
        class procedure DispLiquid(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            const aSyrMap: TIPMAP; const aCh1PumpIndices: TIntArray; const aVolCh1, aSpeedCh1: TDoubleArray;
            aVInxCh1: TValvePos; aVolCh2, aSpeedCh2: TDoubleArray; aVInxCh2: TValvePos;
            const aVolStepsCh1, aVolStepsCh2: TDoubleArray; const aTWay: TDoubleArray;
            const aInWashOrWaste: TArray<boolean>; aTurnValves, aExec, aUseVolControl: boolean);

        class function EmptyDilutor(aUsedArm: IArmDevice; aTips: TIPMAP): TRack;
        class procedure EmptyAllDilutors(aPipDevice: IPipDevice; aTips: TIPMAP; aType: TEmptyDilutorType;
            aInitID: TDateTime);

        class function MoveToWasteRack(aUsedArm: IArmDevice; aTips: TIPMAP): TRack;
        class procedure DispSeveralPeriPumps(aPipDevice: IPipDevice; const aRP: TArray<TXRackPosition>;
            const aVolume: TDoubleArray);

        class procedure FlushSyringe(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aRack: TRack; aCh1Vol, aCh2Vol: TDoubleArray; aDispSpeed: integer; const aPumpIndices: TIntArray);
        class procedure FlushSyringeOrPeripump(aPipDevice: IPipDevice;
            aZMotion: TZAxisPipMoveMotorMotionSystem; aRack: TRack; const aCh1Vol, aCh2Vol: TDoubleArray;
            aUsePeripump: boolean; aDispSpeed: integer; const aPumpIndices: TIntArray);

        class function FindWashRack(aPipDevice: IPipDevice): TRack;
        class function FindWasteRack(aPipDevice: IPipDevice): TRack;

        class function GetInWashOrWasteArray(aPipDevice: IPipDevice; aRack: TRack): TArray<boolean>;
        class function GetChannelVolumesBracketText(const aVol: TDoubleArray;
            const aPumpIndices: TIntArray): string;

        // Flush after init
        class function FindDrySwitchDevice(aDeviceType: TDrySwitchDeviceType; aPipDevice: IPipDevice)
            : ISwitchDevice;
        class function InitSyringes(aUsedArm: IArmDevice; aTips: TIPMAP; aInitID: TDateTime): TRack;
        class procedure FlushAfterInit(aUsedArm: IArmDevice; aRack: TRack; aTips: TIPMAP);
        class procedure PickAir(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
            aTipMap: TIPMAP; const aCh1PumpIndices: TIntArray; const aVol, aSpeed: TDoubleArray;
            aDelay: integer; aUseVolCorrect: boolean);
        class procedure Dry(aUsedArm: IArmDevice; aTips: TIPMAP);
    end;


implementation


uses
    Forms,
    Math,
    SysUtils,
    Classes,
    Generics.Collections,
    TimeSpan,
    LogManager,
    IntfMotorDevice,
    LayoutManager,
    SamGlobe,
    SamHigh,
    RunFlow,
    ErrorManager,
    SubstanceLoading,
    IntfPipPumpDevice,
    MotionSystemTravel,
    ObjModul,
    Liquids,
    OperationFactory,
    OperationAxisMove,
    Device,
    TipMapUtils,
    Carrier,
    ArrayUtils,
    TipSystem;

{ TLiquidChannels }

class function TLiquidChannels.GetDefaultChannel1Array(const aPipDevice: IPipDevice): TIntArray;
begin
    result := GetPumpIndexArray(aPipDevice, cChannel1Index)
end;

class function TLiquidChannels.GetDefaultChannel2Array(const aPipDevice: IPipDevice): TIntArray;
begin
    result := GetPumpIndexArray(aPipDevice, cChannel2Index)
end;

class function TLiquidChannels.GetPumpIndexArray(const aPipDevice: IPipDevice; const aChannelIndex: integer)
    : TIntArray;
begin
    EXIT(TArrayUtils.GetDefinedArray<integer>(aChannelIndex, aPipDevice.TipCount));
end;

{ TLiquidHandlingLow }

class function TLiquidHandlingLow.FindWashRack(aPipDevice: IPipDevice): TRack;
begin
    result := TLayoutManager.Instance.CurrentLayout.FindSpecialRack(srtWash, aPipDevice.RackNamePrefix, true);
end;

class function TLiquidHandlingLow.FindWasteRack(aPipDevice: IPipDevice): TRack;
begin
    result := TLayoutManager.Instance.CurrentLayout.FindSpecialRack(srtWaste,
        aPipDevice.RackNamePrefix, true);
end;

class procedure TLiquidHandlingLow.TurnValves(aPipDevice: IPipDevice; aSyrMap: TIPMAP; aVInx: TValvePos;
    const aPumpIndices: TIntArray; aExecute: boolean);
var
    x: integer;
    xDilutorDev: IPipPumpDevice;
begin
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if not TTipMapUtils.TipSelected(aSyrMap, x) then
            CONTINUE;

        xDilutorDev := aPipDevice.Tips[x].GetPipPump(aPumpIndices[x]);

        if Assigned(xDilutorDev) then
            xDilutorDev.TurnValve(aVInx);
    end;

    if aExecute then
        aPipDevice.ExecutePipPumps(aSyrMap);
end;

class procedure TLiquidHandlingLow.PickLiquid(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aSyrMap: TIPMAP; const aCh1PumpIndices: TIntArray;
    const aVol, aVolSpeed: TDoubleArray; aVInx: TValvePos; const aCh2Vol, aCh2Speed: TDoubleArray;
    aCh2VInx: TValvePos; const aVolStepsCh1, aVolStepsCh2: TDoubleArray; const aTWay: TDoubleArray;
    const aInWashOrWaste: TArray<boolean>; aTurnValves, aExec, aUseVolCorrect: boolean);
var
    x: integer;
    xDilutorDev: IPipPumpDevice;
    xZMotorMap: TIPMAP;
    xSpeed: TIntArray;
    xDest: TDoubleArray;
    xVol, xCh2Vol: TArray<double>;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;

    xVol := TArrayUtils.CopyArray<double>(aVol);
    xCh2Vol := TArrayUtils.CopyArray<double>(aCh2Vol);

    // if aspirating system liquid and NoAspBeforeDispDil is true set volume to 0 to skip aspiration
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if not TTipMapUtils.TipSelected(aSyrMap, x) then
            CONTINUE;
        // channel 1:
        if (aVInx = V_SYR_SYS) then
        begin
            xDilutorDev := aPipDevice.Tips[x].GetPipPump(aCh1PumpIndices[x]);
            if Assigned(xDilutorDev) and (xDilutorDev.NoAspBeforeDispDil) then
            begin
                xVol[x] := 0;
            end;
        end;
        // channel 2:
        if (aCh2VInx = V_SYR_SYS) then
        begin
            xDilutorDev := aPipDevice.Tips[x].GetPipPump(1);
            if Assigned(xDilutorDev) and (xDilutorDev.NoAspBeforeDispDil) then
            begin
                xCh2Vol[x] := 0;
            end;
        end;
    end;

    // --------------------------------------------------------------------------- Turn Valves
    if aTurnValves then
    begin
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if TTipMapUtils.TipSelected(aSyrMap, x) and (xVol[x] > 0) then
            begin
                xDilutorDev := aPipDevice.Tips[x].GetPipPump(aCh1PumpIndices[x]);
                if Assigned(xDilutorDev) then
                    xDilutorDev.TurnValve(aVInx);
            end;
            if TTipMapUtils.TipSelected(aSyrMap, x) and (xCh2Vol[x] > 0) then
            begin
                xDilutorDev := aPipDevice.Tips[x].GetPipPump(1);
                if Assigned(xDilutorDev) then
                    xDilutorDev.TurnValve(aCh2VInx);
            end;
        end;
        aPipDevice.ExecutePipPumps(aSyrMap);
    end;

    // --------------------------------------------------------------------------- Pick up Liquid
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if not TTipMapUtils.TipSelected(aSyrMap, x) then
            CONTINUE;

        if (xVol[x] > 0) then
        begin // Channel1
            xDilutorDev := aPipDevice.Tips[x].GetPipPump(aCh1PumpIndices[x]);
            if Assigned(xDilutorDev) then
            begin
                if (aUseVolCorrect) then
                    xDilutorDev.PickSteps(aVolStepsCh1[x], xVol[x], aVolSpeed[x], aInWashOrWaste[x], false)
                else
                    xDilutorDev.Pick(xVol[x], aVolSpeed[x], aInWashOrWaste[x], false);
            end;
        end;
        if (xCh2Vol[x] > 0) then
        begin // Channel2
            xDilutorDev := aPipDevice.Tips[x].GetPipPump(1);
            if Assigned(xDilutorDev) then
            begin
                if (aUseVolCorrect) then
                    xDilutorDev.PickSteps(aVolStepsCh2[x], xCh2Vol[x], aCh2Speed[x], aInWashOrWaste[x], false)
                else
                    xDilutorDev.Pick(xCh2Vol[x], aCh2Speed[x], aInWashOrWaste[x], false);
            end;
        end;
    end;

    xZMotorMap := TTipMapUtils.EmptyTipMap;
    xDest := aPipDevice.GetNullDoubleArray;
    xSpeed := aPipDevice.GetNullIntArray;
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if TTipMapUtils.TipSelected(aSyrMap, x) and (xVol[x] > 0) and (aTWay[x] > 0) then
        begin // Channel1
            TTipMapUtils.SelectTip(xZMotorMap, x);
            xSpeed[x] := aZMotion.CalcTrackingSpeedByDistanceAndTime(x, aTWay[x], (xVol[x] / aVolSpeed[x]));
            xDest[x] := AddZHeight(aZMotion.ReadCurrentPos(x), -aTWay[x]);
        end;
    end;

    if xZMotorMap > 0 then
    begin
        aZMotion.MoveZ_MultiSpeed(xZMotorMap, xDest, m_NO_EXEC, AT_MOVE_ABS, xSpeed, 0, false);
    end;

    // Picking and Tracking Execute
    if aExec then
    begin
        aPipDevice.ExecutePipPumps(aSyrMap);
        if xZMotorMap > 0 then
            aZMotion.Execute();

        // Speziell für EDOS oder andere DLL-Interfaces: WaitFor- statt Execute-Mechanismus
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if TTipMapUtils.TipSelected(aSyrMap, x) and (xVol[x] > 0) then
            begin // Channel1
                xDilutorDev := aPipDevice.Tips[x].GetPipPump(aCh1PumpIndices[x]);
                if Assigned(xDilutorDev) then
                    xDilutorDev.WaitFor();
            end;
            if TTipMapUtils.TipSelected(aSyrMap, x) and (xCh2Vol[x] > 0) then
            begin // Channel2
                xDilutorDev := aPipDevice.Tips[x].GetPipPump(1);
                if Assigned(xDilutorDev) then
                    xDilutorDev.WaitFor();
            end;
        end;
    end;

    // Sonderfall: TWay ist negativ, wenn SampleAspRetractPos gesetzt ist (Retract from Z-Max)
    xZMotorMap := TTipMapUtils.EmptyTipMap;
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if TTipMapUtils.TipSelected(aSyrMap, x) and (xVol[x] > 0) and (aTWay[x] < 0) then
        begin // Channel1
            TTipMapUtils.SelectTip(xZMotorMap, x);
            xSpeed[x] := aZMotion.CalcTrackingSpeedByDistanceAndTime(x, aTWay[x], (xVol[x] / aVolSpeed[x]));
            xDest[x] := AddZHeight(aZMotion.ReadCurrentPos(x), -aTWay[x]);
        end;
    end;
    if (xZMotorMap > 0) then
    begin
        aZMotion.MoveZ_MultiSpeed(xZMotorMap, xDest, m_NO_EXEC, AT_MOVE_ABS, xSpeed, 0, false);
        if aExec then
            aZMotion.Execute();
    end;
end;

class function TLiquidHandlingLow.GetInWashOrWasteArray(aPipDevice: IPipDevice; aRack: TRack)
    : TArray<boolean>;
var
    xIsWashOrWaste: boolean;
begin
    xIsWashOrWaste := false;
    if Assigned(aRack) then
        xIsWashOrWaste := TRackDefinitions.RackIsWashOrWaste(aRack.name, aPipDevice.RackNamePrefix);

    EXIT(TArrayUtils.GetDefinedArray<boolean>(xIsWashOrWaste, aPipDevice.TipCount));
end;

class procedure TLiquidHandlingLow.DispLiquid(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; const aSyrMap: TIPMAP; const aCh1PumpIndices: TIntArray;
    const aVolCh1, aSpeedCh1: TDoubleArray; aVInxCh1: TValvePos; aVolCh2, aSpeedCh2: TDoubleArray;
    aVInxCh2: TValvePos; const aVolStepsCh1, aVolStepsCh2: TDoubleArray; const aTWay: TDoubleArray;
    const aInWashOrWaste: TArray<boolean>; aTurnValves, aExec, aUseVolControl: boolean);
var
    x: integer;
    xDilutorDev: IPipPumpDevice;
    xSpeed: TIntArray;
    xDest: TDoubleArray;
    xZMotorMap: TIPMAP;
    xTime: TDateTime;
    xTimespan: TTimeSpan;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;

    if aTurnValves then
    begin

        // Turn Valves
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if (((aSyrMap shr x) and 1) <> 0) and (aVolCh1[x] > 0) then
            begin // Channel 1
                xDilutorDev := aPipDevice.Tips[x].GetPipPump(aCh1PumpIndices[x]);
                if Assigned(xDilutorDev) then
                    xDilutorDev.TurnValve(aVInxCh1);
            end;
            if (((aSyrMap shr x) and 1) <> 0) and (aVolCh2[x] > 0) then
            begin // Channel 2
                xDilutorDev := aPipDevice.Tips[x].GetPipPump(1);
                if Assigned(xDilutorDev) then
                    xDilutorDev.TurnValve(aVInxCh2);
            end;
        end;
        aPipDevice.ExecutePipPumps(aSyrMap);
    end;

    // --------------------------------------------------------------------------- Dispense with Ch1 and ch2
    xZMotorMap := TTipMapUtils.EmptyTipMap;
    xDest := aPipDevice.GetNullDoubleArray;
    xSpeed := aPipDevice.GetNullIntArray;
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if (((aSyrMap shr x) and 1) <> 0) and ((aVolCh1[x] > 0) or (aVolCh2[x] > 0)) and (aTWay[x] > 0) then
        begin
            TTipMapUtils.SelectTip(xZMotorMap, x);
            xSpeed[x] := aZMotion.CalcTrackingSpeedByDistanceAndTime(x, aTWay[x],
                (aVolCh1[x] / aSpeedCh1[x]));
            xDest[x] := AddZHeight(aZMotion.ReadCurrentPos(x), aTWay[x]);
        end;
    end;

    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if not TTipMapUtils.TipSelected(aSyrMap, x) then
            CONTINUE;

        if (aVolCh1[x] > 0) then
        begin
            xDilutorDev := aPipDevice.Tips[x].GetPipPump(aCh1PumpIndices[x]);
            if Assigned(xDilutorDev) then
            begin
                if (aUseVolControl) then
                    xDilutorDev.DispSteps(aVolStepsCh1[x], aVolCh1[x], aSpeedCh1[x], 0,
                        aInWashOrWaste[x], false)
                else
                    xDilutorDev.Disp(aVolCh1[x], aSpeedCh1[x], 0, aInWashOrWaste[x], false);
            end;
        end;

        if (aVolCh2[x] > 0) then
        begin
            xDilutorDev := aPipDevice.Tips[x].GetPipPump(1);
            if Assigned(xDilutorDev) then
            begin
                if (aUseVolControl) then
                    xDilutorDev.DispSteps(aVolStepsCh2[x], aVolCh2[x], aSpeedCh2[x], 0,
                        aInWashOrWaste[x], false)
                else
                    xDilutorDev.Disp(aVolCh2[x], aSpeedCh2[x], 0, aInWashOrWaste[x], false);
            end;
        end;

    end;

    if xZMotorMap > 0 then
    begin
        aZMotion.MoveZ_MultiSpeed(xZMotorMap, xDest, m_NO_EXEC, AT_MOVE_ABS, xSpeed, 0, false);
    end;

    if aExec then
    begin
        xTime := Now;
        aPipDevice.ExecutePipPumps(aSyrMap);

        if xZMotorMap > 0 then
        begin
            if aExec then
                aZMotion.Execute();
        end;

        // Speziell für EDOS oder andere DLL-Interfaces: WaitFor- statt Execute-Mechanismus
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if (((aSyrMap shr x) and 1) <> 0) then
            begin // Channel1
                xDilutorDev := aPipDevice.Tips[x].GetPipPump(aCh1PumpIndices[x]);
                if Assigned(xDilutorDev) then
                    xDilutorDev.WaitFor();
                // Channel2
                if (aVolCh2[x] > 0) then
                begin
                    xDilutorDev := aPipDevice.Tips[x].GetPipPump(1);
                    if Assigned(xDilutorDev) then
                        xDilutorDev.WaitFor();
                end;
            end;
        end;

        xTimespan := TTimeSpan.Subtract(Now, xTime);
        TLogManager.Instance.Log('Dispense time: ' + FloatToStr(xTimespan.TotalMilliseconds) +
            ' msec', false);
    end;
end;

class procedure TLiquidHandlingLow.EmptyAllDilutors(aPipDevice: IPipDevice; aTips: TIPMAP;
    aType: TEmptyDilutorType; aInitID: TDateTime);
var
    x, xPumpIndex: integer;
    xDilutorDev: IPipPumpDevice;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;

    if (aType <> edtInit) then
    begin
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if not TTipMapUtils.TipSelected(aTips, x) then
                CONTINUE;

            // Sichergehen, dass die Ventile aller an diesem Tip vorhandenen Pumpen gedreht sind
            for xPumpIndex := 0 to aPipDevice.Tips[x].NumberOfPumps - 1 do
            begin
                xDilutorDev := aPipDevice.Tips[x].GetPipPump(xPumpIndex);
                if Assigned(xDilutorDev) then
                    xDilutorDev.TurnValve(V_SYR_TIP);
            end;
        end;
        aPipDevice.ExecutePipPumps(aTips);
    end;

    // Alle vorhandenen Pumpen leeren (Je Pumpen-Index ein Exceute)
    xPumpIndex := 0;
    while not gErrorManager.IsGlobalErr() do
    begin
        xDilutorDev := nil;
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if not TTipMapUtils.TipSelected(aTips, x) then
                CONTINUE;

            xDilutorDev := aPipDevice.Tips[x].GetPipPump(xPumpIndex);
            if not Assigned(xDilutorDev) then
                CONTINUE;

            if (aType = edtInit) then
                xDilutorDev.InitPipPump(aInitID, m_NO_EXEC)
            else
                xDilutorDev.DispAll(true, true);
        end;
        if not Assigned(xDilutorDev) then
            EXIT;

        aPipDevice.ExecutePipPumps(aTips);
        inc(xPumpIndex);
    end;
end;

class function TLiquidHandlingLow.MoveToWasteRack(aUsedArm: IArmDevice; aTips: TIPMAP): TRack;
var
    xXYStep: TXYStep;
    xZStep: TPipStepZPos;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xRP: TArray<TXRackPosition>;
begin
    result := FindWasteRack(aUsedArm.PipDevice);
    // Normales Waschen in Wash-Rack
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    xRP := gmGetRackPosArrayForTips(aUsedArm.PipDevice, result);
    xXYStep := gmCalculatePipStep(aUsedArm, aTips, xRP, xZStep);
    if not Assigned(xXYStep) then
        EXIT;

    try
        xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);
        try
            xXYOp.MoveXY(xXYStep, xRP, xZStep, []);
        finally
            FreeAndNil(xXYOp);
        end;

        xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
        try
            xZMotion.MoveZ(xZStep.SyrMap, xZStep.Z.ZMax, m_EXECUTE);
        finally
            FreeAndNil(xZMotion);
        end;
    finally
        FreeAndNil(xXYStep);
    end;
end;

class function TLiquidHandlingLow.EmptyDilutor(aUsedArm: IArmDevice; aTips: TIPMAP): TRack;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT(nil);

    result := FindWasteRack(aUsedArm.PipDevice);
    MoveToWasteRack(aUsedArm, aTips);

    EmptyAllDilutors(aUsedArm.PipDevice, aTips, edtDispAll, 0);
end;

class procedure TLiquidHandlingLow.DispSeveralPeriPumps(aPipDevice: IPipDevice;
    const aRP: TArray<TXRackPosition>; const aVolume: TDoubleArray);
var
    x, xPeriIndex: integer;
    xDilutorDev: IPipPumpDevice;
    xPeriPumps: TList<IPeriPumpDevice>;
    xUsedTipsList: TList<TIPMAP>;
    xTipMap: integer;
begin
    if gErrorManager.IsGlobalErr() then
        exit; // Return on Error

    xPeriPumps := TList<IPeriPumpDevice>.Create;
    xUsedTipsList := TList<TIPMAP>.Create;
    try
        for x := 0 to aPipDevice.TipCount - 1 do
        begin
            if not TTipMapUtils.TipSelected(aPipDevice.UseTips, x) then
                CONTINUE;
            xDilutorDev := aPipDevice.Tips[x].GetPipPump(0);
            if not Assigned(xDilutorDev) then
                CONTINUE;
            if not Assigned(xDilutorDev.PeriPump) then
                CONTINUE;

            // Es könnte sein, dass verschiedene PeriPump-Devices angeschlossen sind:
            xPeriIndex := xPeriPumps.IndexOf(xDilutorDev.PeriPump);
            if (xPeriIndex < 0) then
            begin
                xPeriPumps.Add(xDilutorDev.PeriPump);
                xTipMap := 0;
                TTipMapUtils.SelectTip(xTipMap, x);
                xUsedTipsList.Add(xTipMap);
            end
            else
            begin
                xTipMap := xUsedTipsList[xPeriIndex];
                TTipMapUtils.SelectTip(xTipMap, x);
                xUsedTipsList[xPeriIndex] := xTipMap;
            end;
        end;

        // Dispense für eine PeriPumpe durchführen
        for xPeriIndex := 0 to xPeriPumps.Count - 1 do
        begin
            DispPeriPump(aPipDevice, xPeriPumps[xPeriIndex], xUsedTipsList[xPeriIndex], aVolume, aRP);
        end;
    finally
        FreeAndNil(xUsedTipsList);
        FreeAndNil(xPeriPumps);
    end;
end;

class procedure TLiquidHandlingLow.DispPeriPump(aPipDevice: IPipDevice; aPeriPump: IPeriPumpDevice;
    aSyrMap: TIPMAP; const aVol: TDoubleArray; const aRP: TArray<TXRackPosition>);
var
    x: integer;
    xDilutorDev: IPipPumpDevice;
    xMaxVolume, xSumVolume: extended;
begin
    xMaxVolume := 0;
    xSumVolume := 0;

    // Turn valves first ( not used valves turn to system liquid)
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        xDilutorDev := aPipDevice.Tips[x].GetPipPump(0);
        if not Assigned(xDilutorDev) then
            CONTINUE;

        if (TTipMapUtils.TipSelected(aSyrMap, x) and (aVol[x] > 0)) then
        begin
            xDilutorDev.TurnValve(V_PER_TIP);
            xMaxVolume := MaxValue([xMaxVolume, aVol[x]]);
            xSumVolume := xSumVolume + xMaxVolume;

            // Update der Füllstände
            TLiquids.Instance.SystemLiquids[0].TakeVolume(xMaxVolume, gRunFlow.SimulationMode);
            if Assigned(aRP[x].Rack) then
            begin
                if TRackDefinitions.RackIsWashOrWaste(aRP[x].Rack.Name, aPipDevice.RackNamePrefix) then
                begin
                    // Füllstand Waste ändern
                    TLiquids.Instance.WasteLiquid.AddVolume(xMaxVolume, gRunFlow.SimulationMode)
                end
                else
                begin
                    // Füllstand der Position ändern
                    TSubstanceLoading.Instance.StoreLiquidPumpAsp(xDilutorDev, nil, 0, aVol[x],
                        asptSystemLiquid);
                    TSubstanceLoading.Instance.StoreDsp(xDilutorDev.Volumes, aRP[x].Rack, aRP[x].Pos, aVol[x],
                        dsptLiquid);
                end;
            end;
        end
        else
            xDilutorDev.TurnValve(V_PER_SYS);
    end;
    aPipDevice.ExecutePipPumps(aSyrMap);

    if (xMaxVolume <= 0) then
        EXIT;

    // flush peripump
    aPeriPump.Dispense(xMaxVolume);

    // Turn valves back
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        xDilutorDev := aPipDevice.Tips[x].GetPipPump(0);
        if not Assigned(xDilutorDev) then
            CONTINUE;

        xDilutorDev.TurnValve(V_SYR_TIP);
    end;
    aPipDevice.ExecutePipPumps(aSyrMap);
end;

class function TLiquidHandlingLow.FlushCalcRestVol(aPipDevice: IPipDevice;
    const aPumpIndices: TArray<integer>; out oVol: TDoubleArray; var vRestVol: TDoubleArray): extended;
var
    x: integer;
    xPipPump: IPipPumpDevice;
begin
    oVol := aPipDevice.GetNullDoubleArray;
    result := 0;

    for x := 0 to (aPipDevice.TipCount - 1) do
    begin
        oVol[x] := vRestVol[x];

        // Spezialfall Flush: Hier zählt das echte Dilutorvolumen, egal was im TipType steht
        xPipPump := aPipDevice.Tips[x].GetPipPump(aPumpIndices[x]);
        if Assigned(xPipPump) and (xPipPump.MaxVolume_ul < oVol[x]) then
            oVol[x] := xPipPump.MaxVolume_ul;

        vRestVol[x] := vRestVol[x] - oVol[x];
        if (vRestVol[x] < 0) then
            vRestVol[x] := 0;
        result := result + oVol[x];
    end;
end;

class function TLiquidHandlingLow.GetChannelVolumesBracketText(const aVol: TDoubleArray;
    const aPumpIndices: TIntArray): string;
var
    xDelim: string;
    x: integer;
begin
    result := '';
    xDelim := '';
    for x := 0 to high(aVol) do
    begin
        if (x = 1) then
            xDelim := TFormatUtils.CurrentListSeparator;
        result := result + xDelim + FloatToStr(aVol[x]);
        if (aVol[x] > 0) and (aPumpIndices[x] > 0) then
            result := result + '(Ch' + IntToStr(aPumpIndices[x] + 1) + ')'
    end;

    result := '[' + result + ']'
end;

class procedure TLiquidHandlingLow.FlushSyringe(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aRack: TRack; aCh1Vol, aCh2Vol: TDoubleArray;
    aDispSpeed: integer; const aPumpIndices: TIntArray);
var
    i: integer;
    xAspSpeed1, xAspSpeed2, xDispSpeed1, xDispSpeed2, xVol1, xVol2: TDoubleArray;
    xTWay: TDoubleArray;
    xCh2PumpArray: TIntArray;
    xTips: TIPMAP;
    xLogText: string;
begin
    if not Assigned(aRack) then
        EXIT;

    xTips := 0;
    xTWay := aPipDevice.GetNullDoubleArray;
    xAspSpeed1 := aPipDevice.GetNullDoubleArray;
    xAspSpeed2 := aPipDevice.GetNullDoubleArray;
    xDispSpeed1 := aPipDevice.GetNullDoubleArray;
    xDispSpeed2 := aPipDevice.GetNullDoubleArray;
    xCh2PumpArray := TLiquidChannels.GetDefaultChannel2Array(aPipDevice);

    for i := 0 to aPipDevice.TipCount - 1 do
    begin

        if (aCh1Vol[i] > 0) or (aCh2Vol[i] > 0) then
            TTipMapUtils.SelectTip(xTips, i);

        // Define Asp Speeds (Dilutor 1 und 2)
        xAspSpeed1[i] := aPipDevice.Tips[i].GetPipPumpCurrentAspSpeed(aPumpIndices[i]);
        xAspSpeed2[i] := aPipDevice.Tips[i].GetPipPumpCurrentAspSpeed(xCh2PumpArray[i]);

        // Define Disp Speed
        if (aDispSpeed = 0) then
        begin
            xDispSpeed1[i] := aPipDevice.Tips[i].GetPipPumpDispSpeed(aPumpIndices[i]);
            xDispSpeed2[i] := aPipDevice.Tips[i].GetPipPumpDispSpeed(xCh2PumpArray[i]);
        end
        else
        begin
            xDispSpeed1[i] := aDispSpeed;
            xDispSpeed2[i] := aDispSpeed;
        end;
    end;

    // logText
    xLogText := 'Wash - Rack-[' + aRack.Name + ']';

    if (Math.Sum(aCh1Vol) > 0) then
        xLogText := xLogText + ' - Vol' + GetChannelVolumesBracketText(aCh1Vol, aPumpIndices);

    if (aDispSpeed > 0) then
        xLogText := xLogText + ' - DispSpeed ' + IntToStr(aDispSpeed);

    if (Math.Sum(aCh2Vol) > 0) then
        xLogText := xLogText + ' - Vol(Ch2)' + TArrayUtils.ArrayToBracketText(aCh2Vol);

    gLogManager.Log(xLogText, true);

    // execute flush!
    while ((FlushCalcRestVol(aPipDevice, aPumpIndices, xVol1, aCh1Vol) + FlushCalcRestVol(aPipDevice,
        xCh2PumpArray, xVol2, aCh2Vol)) > 0) do
    begin

        if (not gErrorManager.IsGlobalErr) then
            PickLiquid(aPipDevice, aZMotion, xTips, aPumpIndices, xVol1, xAspSpeed1, V_SYR_SYS, xVol2,
                xAspSpeed2, V_SYR_SYS, aPipDevice.GetNullDoubleArray, aPipDevice.GetNullDoubleArray, xTWay,
                GetInWashOrWasteArray(aPIpDevice, aRack), true, true, false);

        if (not gErrorManager.IsGlobalErr) then
            DispLiquid(aPipDevice, aZMotion, xTips, aPumpIndices, xVol1, xDispSpeed1, V_SYR_TIP, xVol2,
                xDispSpeed2, V_SYR_TIP, aPipDevice.GetNullDoubleArray, aPipDevice.GetNullDoubleArray, xTWay,
                GetInWashOrWasteArray(aPIpDevice, aRack), true, true, false);
    end;
end;

class procedure TLiquidHandlingLow.FlushSyringeOrPeripump(aPipDevice: IPipDevice;
    aZMotion: TZAxisPipMoveMotorMotionSystem; aRack: TRack; const aCh1Vol, aCh2Vol: TDoubleArray;
    aUsePeripump: boolean; aDispSpeed: integer; const aPumpIndices: TIntArray);
var
    xRP: TArray<TXRackPosition>;
    xCh1Vol, xCh2Vol: TDoubleArray;
begin
    if aUsePeripump then
    begin
        gLogManager.Log('Wash with peripump: Rack - ' + aRack.Name + ' Vol' + TArrayUtils.ArrayToBracketText
            (aCh1Vol), true);
        xRP := gmGetRackPosArrayForTips(aPipDevice, aRack);
        DispSeveralPeriPumps(aPipDevice, xRP, aCh1Vol);
    end
    else
    begin
        xCh1Vol := Copy(aCh1Vol); // Kopie verwenden, da Werte geändert werden
        xCh2Vol := Copy(aCh2Vol);
        FlushSyringe(aPipDevice, aZMotion, aRack, xCh1Vol, xCh2Vol, aDispSpeed, aPumpIndices);
    end;
end;

class function TLiquidHandlingLow.FindDrySwitchDevice(aDeviceType: TDrySwitchDeviceType;
    aPipDevice: IPipDevice): ISwitchDevice;
const
    STR_DEVICENAME_DRYVALVE_SWITCH = 'DRYVALVE';
    STR_DEVICENAME_VACUUMPUMP_SWITCH = 'VACUUMPUMP';
    STR_DEVICENAME_N2CHANNEL_SWITCH = 'N2CANNEL3';
var
    xDeviceName: string;
    xDryRack: TRack;
begin
    case aDeviceType of
        dsdN2Channel:
            xDeviceName := STR_DEVICENAME_N2CHANNEL_SWITCH;
        dsdVacuumPump:
            xDeviceName := STR_DEVICENAME_VACUUMPUMP_SWITCH;
        dsdDryValve:
            xDeviceName := STR_DEVICENAME_DRYVALVE_SWITCH;
    end;

    if Assigned(aPipDevice) then
    begin
        // find the dry rack
        xDryRack := TLayoutManager.Instance.CurrentLayout.FindSpecialRack(srtDry,
            aPipDevice.RackNamePrefix, false);
        if Assigned(xDryRack) then
        begin
            // find a device thas has a devicename that begins with xDeviceName and
            // that has a rackname setting which is identical to the rackname of the dry rack
            if gModules.Find_ByRackNameAndClass(xDeviceName, xDryRack.Name, ISwitchDevice, result) then
                EXIT;
        end;
    end;

    // otherwise just look for a device that has a devicename that begins with xDeviceName
    gModules.Find_ByNameCut(xDeviceName, ISwitchDevice, result);
end;

class function TLiquidHandlingLow.InitSyringes(aUsedArm: IArmDevice; aTips: TIPMAP;
    aInitID: TDateTime): TRack;
begin
    result := nil;

    if (aTips = 0) or (gErrorManager.IsGlobalErr) then
        EXIT;

    result := MoveToWasteRack(aUsedArm, aTips);

    EmptyAllDilutors(aUsedArm.PipDevice, aTips, edtInit, aInitID);
end;

class procedure TLiquidHandlingLow.PickAir(aPipDevice: IPipDevice; aZMotion: TZAxisPipMoveMotorMotionSystem;
    aTipMap: TIPMAP; const aCh1PumpIndices: TIntArray; const aVol, aSpeed: TDoubleArray; aDelay: integer;
    aUseVolCorrect: boolean);
var
    xTWay, xAirSteps: TDoubleArray;
    x: integer;
begin
    if (gErrorManager.IsGlobalErr()) then
        EXIT;

    xAirSteps := aPipDevice.GetNullDoubleArray;
    xTWay := aPipDevice.GetNullDoubleArray; // No Tracking
    aTipMap := aTipMap and aPipDevice.OKTips;

    if (TTipMapUtils.MaxLiquid(aTipMap, aVol) <= 0) then
        EXIT;

    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if (aPipDevice.Tips[x].GetPipPump(aCh1PumpIndices[x]) <> nil) then
            xAirSteps[x] := aPipDevice.Tips[x].GetPipPump(aCh1PumpIndices[x]).VolumeULToVolumeSteps(aVol[x]);
    end;
    gLogManager.Log('Pick air volume ' + TLiquidHandlingLow.GetChannelVolumesBracketText(aVol,
        aCh1PumpIndices) + ', speed ' + TArrayUtils.ArrayToBracketText(aSpeed), false);
    PickLiquid(aPipDevice, aZMotion, aTipMap, aCh1PumpIndices, aVol, aSpeed, V_SYR_TIP,
        aPipDevice.GetNullDoubleArray, aPipDevice.GetNullDoubleArray, V_SYR_TIP, xAirSteps,
        aPipDevice.GetNullDoubleArray, xTWay, TArrayUtils.GetFalseArray(aPipDevice.TipCount), true, true,
        aUseVolCorrect); // Take Airgap
    gRunFlow.AppSleep(aDelay);

    for x := 0 to aPipDevice.TipCount - 1 do
        if (((aTipMap shr x) and 1) <> 0) then
        begin
            TSubstanceLoading.Instance.StoreLiquidAsp(aPipDevice, nil, 0, x, aVol[x], asptAir,
                aCh1PumpIndices[x]);
            // Update Tube liquid list
        end;
end;

class procedure TLiquidHandlingLow.FlushAfterInit(aUsedArm: IArmDevice; aRack: TRack; aTips: TIPMAP);
var
    i: integer;
    xSpeed, xVol: TDoubleArray;
    xPipPumpDev: IPipPumpDevice;
    xIsWashOrWaste: boolean;
    xPipDevice: IPipDevice;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xCh1PumpIndices: TIntArray;
begin
    if (gErrorManager.IsGlobalErr()) then
        EXIT;
    if (gFlushVolAfterInit + gAirVolAfterInit <= 0) then
        EXIT;

    xPipDevice := aUsedArm.PipDevice;
    xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
    xCh1PumpIndices := TLiquidChannels.GetDefaultChannel1Array(xPipDevice);

    // Flush im Waste
    xVol := xPipDevice.GetNullDoubleArray;
    for i := 0 to xPipDevice.TipCount - 1 do
        if (((aTips shr i) and 1) = 1) then
            xVol[i] := gFlushVolAfterInit;
    FlushSyringe(xPipDevice, xZMotion, aRack, xVol, xPipDevice.GetNullDoubleArray, 0, xCh1PumpIndices);
    gRunFlow.AppSleep(200);
    xZMotion.MoveRetractDistance(aTips, 0, gRetractStepsAfterInit, false, false, false);

    // Pick air and dispense
    if (gAirVolAfterInit > 0) then
    begin
        xVol := TArrayUtils.GetDefinedArray<double>(gAirVolAfterInit, xPipDevice.TipCount);
        xSpeed := TArrayUtils.GetDefinedArray<double>(200, xPipDevice.TipCount);
        PickAir(xPipDevice, xZMotion, aTips, xCh1PumpIndices, xVol, xSpeed, 100, false);

        xIsWashOrWaste := TRackDefinitions.RackIsWashOrWaste(aRack.Name, aUsedArm.Name);
        for i := 0 to xPipDevice.TipCount - 1 do
        begin
            if (((aTips shr i) and 1) = 1) then
            begin
                xPipPumpDev := xPipDevice.Tips[i].GetPipPump(xCh1PumpIndices[i]);
                if not Assigned(xPipPumpDev) then
                    CONTINUE;
                xPipPumpDev.Disp(gAirVolAfterInit, DispSpeedDefault, 0, xIsWashOrWaste, true);
            end;
        end;
        xPipDevice.ExecutePipPumps(aTips);

        gRunFlow.AppSleep(200);
    end;
end;

class procedure TLiquidHandlingLow.Dry(aUsedArm: IArmDevice; aTips: TIPMAP);
var
    iDevN2, iDevDry: ISwitchDevice;
    xDryRack: TRack;
    xPipDevice: IPipDevice;
    xXYStep: TXYStep;
    xZStep: TPipStepZPos;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xZTravelMotion: TZAxisTravelMotionSystem;
begin
    if aTips = 0 then
        EXIT;

    xPipDevice := aUsedArm.PipDevice;
    xDryRack := TLayoutManager.Instance.CurrentLayout.FindSpecialRack(srtDry,
        xPipDevice.RackNamePrefix, false);
    if not Assigned(xDryRack) then
        EXIT;
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    // Dry-Rack gefunden
    xZTravelMotion := TOperationFactory.CreateZAxisTravelMotionSys(aUsedArm.MotionDevice);
    try
        xZTravelMotion.MoveToZTravelAllTips(0, 0);

        xXYStep := gmCalculatePipStep(aUsedArm, aTips, gmGetRackPosArrayForTips(xPipDevice,
            xDryRack), xZStep);
        if not Assigned(xXYStep) then
            EXIT;

        try
            xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);
            try
                xXYOp.MoveXY(xXYStep, gmGetRackPosArrayForTips(aUsedArm.PipDevice, xDryRack), xZStep, []);
            finally
                FreeAndNil(xXYOp);
            end;

            xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
            try
                xZMotion.MoveZ(xZStep.SyrMap, xZStep.Z.ZMax, m_EXECUTE);
            finally
                FreeAndNil(xZMotion);
            end;
            if (gErrorManager.IsGlobalErr) then
                EXIT;

            // Die Vacuumpumpe muß durch die übergeordnete Funktion eingeschaltet werden
            iDevDry := FindDrySwitchDevice(dsdDryValve, xPipDevice);
            if Assigned(iDevDry) then
                iDevDry.SwitchOn(false);
            iDevN2 := FindDrySwitchDevice(dsdN2Channel, xPipDevice);
            if Assigned(iDevN2) then
            begin
                iDevN2.SwitchOn(false);
                gRunFlow.AppSleep(500);
                iDevN2.SwitchOff(false);
            end;
            gRunFlow.AppSleep(gWashDryDelay);

            if (gErrorManager.IsGlobalErr) then
                EXIT;

            xZTravelMotion.MoveToZTravelAllTips(0, 0);

            if Assigned(iDevDry) then
                iDevDry.SwitchOff(false);
        finally
            FreeAndNil(xXYStep);
        end;
    finally
        FreeAndNil(xZTravelMotion);
    end;
end;


end.
