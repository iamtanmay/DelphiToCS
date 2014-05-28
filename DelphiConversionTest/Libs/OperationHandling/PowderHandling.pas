{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : EXAKTE Übersetzungen der Sampler.dll-Powder-Funktionen
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  12.08.03 wl                               TN1526   initial version
  14.08.03 wl  gmGetPRTip, gmReturnPRTip    TN1526   neu: Pickup-Redi-Funktionen als Delphi-Quelltext
  17.09.03 wl  gmAspiratePowder             TN1526   Zusatz "_ZP02" entfernt, da von beiden Plattformen genutzt
  17.09.03 wl  gmAspiratePowder,gmDispensePowder  TN1526   Bei aUseGripperArm wird Robot.MoveToXY_H_WithVarispan aufgerufen
  18.09.03 wl  gmAspiratePowder             TN1526   kleinere Korrekturen
  22.09.03 wl                               TN1526   alle Arrays werden als var übergeben (wie in C)
  10.12.03 wl  gmAPowder,gmDPowder          TN1672   von AppInterface hierher verschoben
  10.12.03 wl  gmRediWash                   TN1672   von SamCmd hierher verschoben
  10.12.03 wl  alle Funktionen              TN1672   Robot-Move-Aufrufe ersetzt durch gPipArm/gGrpArm-Methoden
  16.12.03 wl  alle Funktionen              TN1672   der benutze Arm wird als Parameter übergeben!
  17.12.03 wl  gmRediSaveZUpMove            TN1672   von SamCmd hierher
  18.12.03 wl                               TN1672   uses geändert
  19.12.03 wl                               TN1672   MoveSingleZ - Parameter geändert
  04.02.04 pk                               TN1719   functions moved into this unit from old Thread units( ExtLiq, BasicLig, etc)
  04.02.04 pk                               TN1719   Function required by Layouter -> PowderHandlingLow
  02.03.04 wl  gmAspiratePowder,gmDispensePowder  TN1773   uses new parameter aEventList for Events (EV_BASP,EV_BPICKLQ,..)
  10.03.04 wl  gmRediShakerOff              TN1801   Übergabe von Pointer führte zu Access Violation
  12.03.04 wl                               TN1812   alle MoveXY-Befehle: statt ZTravel-Wert wird aktuelles Rack übereben
  05.04.04 wl  TDllParam                    TN1708   neu: ZScanMode, ZScanSpeed, ZRetractSpeed
  05.04.04 wl  gmAspiratePowder,gmDispensePowder  TN1708   ZScanMode, ZScanSpeed und ZRetractSpeed als Parameter
  05.04.04 wl  alle Funktionen!             TN1788   aUsedArm wird immer als Parameter übergeben und immer benutzt (wenn irgend möglich)
  05.04.04 wl  gmWash,gmAPowder,gmDPoweder  TN1788   Hardcore-Umschreibungen entfernt
  19.04.04 wl  TPowderStepData              TN1788   entspricht TDLLParameters
  20.04.04 wl  TTipStatus,gmClearTipStatus  TN1788   --> DevicesTips
  20.04.04 wl  gmGetRack                    TN1788   --> ObjWorkb
  20.04.04 wl  gmDispenseVirtual            TN1788   --> SamHigh
  20.04.04 wl  gmAspiratePowder,gmDispensePowder  TN1788   Umstellung von TipStatus auf aUsedArm.Tips
  20.04.04 wl  gmRediWash                   TN1788   Log mit Rackname
  27.04.04 wl  gmAspiratePowder,gmMoveToWiperRack  TN1881   Berechnung der Wiper-Positionen immer ohne Offsets, da AspShift nicht mehr in Positionen eingerechnet werden
  27.04.04 wl  gmAspiratePowder             TN1881   X/YAspShift wird beim Anfahren der Position mit eingerechnet
  27.04.04 wl  gmAspiratePowder             TN1882   Vor dem Anfahren des Wipers wird erst die XY-Position ohne Asp-Shift angefahren
  27.04.04 wl  gmAspiratePowder             TN1798   Wiper wird jetzt auch mit ZP02-Greiferarm richtig herum angefahren
  04.05.04 wl  gmAspiratePowder             TN1898   ResetLastRack statt MovetoZTravel (gmMoveToWiperRack) -> keine auf-ab-Bewegung
  10.05.04 pk  gmRediWash                   TN1889.1 New overloaded: When no Rack param then use default RWash rack
  10.05.04 pk  gmMoveToWiperRack            TN1889.1 call FindByRackName
  13.05.04 wl  gmAspiratePowder1            TN1788   Parameter aAspPos wurde nicht benutzt -> entfernt
  15.06.04 wl                               TN1963   alle Aufrufe von gmGetAspSpeed & gmGetDispSpeed entfernt (Unsinn für Powder Handling)
  15.06.04 wl  gmAspiratePowder1            TN1987   XY-Move vor MoveToWiper muß auch mit Wiper-Speed erfolgen
  17.06.04 wl  gmAspiratePowder1            TN1987.1 XY-Move vor MoveToWiper nur wenn gMoveToRealPosBeforeWiper gesetzt ist
  17.06.04 wl  gmDispensePowder             TN1975   DispRetrSpeed entfernt bzw. durch 0 ersetzt
  01.07.04 wl                               TN1963   Anpassung an Änderungen in TPipStep
  01.07.04 wl  gmAspiratePowder1            TN1963   GetRealXValue für Wiper -> ist bereits in X.MOffset enthalten
  07.07.04 wl  gmAspiratePowder             TN2019   Aufnahme von PR-Tips: RobotArmDevice statt nur PipArmDevice
  25.08.04 wl                               TN2105   ZScanspeed heißt jetzt ZInsertspeed (ScanMode & LiqDet entfernt)
  01.09.04 pk                               TN2114   gmMoveToWiperRack, gmRediWash : Use TipCount - 1
  08.09.04 wl                               TN2121   gmSwitchModulePort statt global_AppIntf.SwitchModulePort
  08.09.04 wl  gmPickPowder,gmDispPowder    TN2121   ruft gmStoreAsp/Dsp auf, mit dem Zusatz "Powder"
  11.09.04 wl  gmMoveToWiperRack,gmAspiratePowder1,gmDispensePowder1,gmRediWash  TN2123   Z-Werte werden mit CalculatePipStaps ermittelt (statt GetRackData)
  11.09.04 wl  gmMoveToWiperRack,gmAspiratePowder1,gmDispensePowder1,gmRediWash  TN2123   geänderte Aufrufe von MoveZ (MposArray statt MPos)
  13.10.04 wl  gmMoveAspPosPowder           TN2151   vereinfachte Version von gmMoveAspPos (SamHigh)
  13.10.04 wl  gmMoveDspPosPowder           TN2151   vereinfachte Version von gmMoveDspPos (SamHigh)
  13.10.04 wl  TPowderStepData              TN2151   sinnlose Parameter entfernt
  14.10.04 wl  gmAspiratePowder             TN2178   xNoCalculation wird aus Feld SampleAspLiqDet gelesen!
  18.10.04 wl  gmAspiratePowder1,gmDispensePowder1  TN2177   bei MoveXY wird nicht aSyrMap sondern die Syrmap des Einzelschrittes übergeben - dadurch fahren unbenutzte Tips auf global-ZTravel
  08.11.04 wl  gmAspirate,gmDispensePowder  TN2213   Parameter TAspirate/DispenseActionRunRec statt TEventList
  08.11.04 wl  gmAspirate,gmDispensePowder1 TN2213   Events werden als TExtDllCall übergeben
  07.12.04 wl                               TN2246.4  uses ObjectsRun statt ObjWorkbExt
  21.02.05 wl  alle Funktionen              TN1960   gCommManager.AppSleep statt global_AppInterface-Methode
  11.04.05 pk                               TN2373   references to TRunRecord removed
  20.04.05 wl                               TN2377   uses CalliW entfernt
  27.04.05 pk  gmMoveAspPosPowder           TN2400   new ZTube array for gmGetZPosByVolCalc
  22.05.05 wl  gmCalli...                   TN2377   Calli-Funktionen hierher verschoben
  21.06.05 pk                               TN2464.3  St_LhPtr : No longer a pointer - Too dangerous, caused access violations
  18.07.05 thr gmGetZPosByVolCalc           TN2411   Parameter added
  19.09.05 pk  gmMoveDspPosPowder           TN2610   Use the MotorDevice to calculate min position
  14.10.05 wl                               TN2672   RediWash-Rack wird immer über WB.FindSpecialRack gesucht
  28.10.05 pk  gmAspiratePowder1            TN2424   Aspiration Shifting : do Y shift only for tips which are being aspirated with
  09.11.05 wl                               TN2728   Aufrufe von MoveXY mit geänderten Parametern
  14.11.05 wl  gmAspiratePowder1            TN2756   Berechnung des AspShift-Arrays (das gar nicht mehr benutzt wurde) entfernt
  14.11.05 wl  gmRediShakerOff/On           TN2762   Exit if there is no Shaker rack defined
  18.11.05 wl  gmMoveDspPosPowder           TN2764   benutzt SetToMinPosIfIsBeyondMin
  30.11.05 wl  gmAspiratePowder             TN2818   Abfrage auf RemRediTip entfernt
  20.01.06 pk  gmCalli                      TN2862   Now supports Aps and Disp events
  31.03.06 thr gmCalliTare                  TN3007   neuer Parameter in StartTare eingefügt
  03.04.06 pk  gmAspiratePowder1            TN2958   use format string with %s instead of %d
  18.04.06 pk                               TN2958   Call StorePreviousXYMoveInfo
  18.04.06 pk  gmAspiratePowder1            TN2958   MOffset reimplemented
  19.04.06 wl                               TN3051    benutzt TipFloatArray und extended für alle Volumen
  24.05.06 wl  gmDispensePowder             TN3119   Der neue Parameter "DispEmptyVarRedi" wird ausgewertet
  24.05.06 wl  gmDPowder                    TN3119   Wenn EmptyVarRedi = true, wird der VarRediMotor auf 0 gefahren
  05.07.06 wl  gmDPowder                    TN3119.2  Wenn EmptyVarRedi = true, wird der VarRediMotor wirklich auf 0 gefahren
  05.07.06 wl  gmDPowder                    TN3119.3  Delays neu geordnet: Jetzt wird wirklich genau die Delay-Zeit gewartet!!
  24.08.06 wl                               TN3271    bei den Aufrufen von gmCalculatePipSteps wird wieder PaintTubes übergeben
  07.09.06 wl  gmCalli..-Funktionen         TN3287    --> CalliSimpleHandling
  12.09.06 wl  gmAspiratePowder(1)          TN3286    GetTipMethodName kann jetzt übergeben werden
  14.09.06 pk                               TN3306   Aspiration shifting corrected
  19.09.06 wl  gmAspiratePowder1            TN3286    GetTipMethodName wird mit hardcodierten Tip-Variablen aufgerufen
  26.09.06 wl  gmAspiratePowder1            TN3326    GetTipMethodName wieder entfernt
  04.10.06 wl  gmAspiratePowder             TN3317   Aspirate kann mehrfach ausgeführt werden (steht in SampleAspSpitBackCount)
  04.10.06 wl  gmAspiratePowder             TN3317   Delay zwischen mehreren Aspirates (steht in SampleAspCh2WashDelay)
  04.10.06 wl  gmAspiratePowder             TN3317   Neu: XYShifting wird nur benutzt, wenn in SampleAspXYShifting so festgelegt
  04.10.06 wl  gmDispensePowder             TN3317   Neu: XYShifting wird nur benutzt, wenn in DispXYShifting so festgelegt
  05.10.06 wl  gmPickPowder                 TN3317   nur beim letzten Aspirate-Schritt wird das Volumen in die Posinfo geschrieben, sonst null
  06.10.06 wl  gmAspiratePowder1            TN3317   ResetLastRack am Schluß entfernt, weil zwischen den Aspirates immer auf Global Z-Travel gefahren wurde
  18.10.06 wl  gmSetVarRediVolumes          TN3362    VarixMotorSteps ist var, wird auf den wirklich eingestellten Wert gesetzt
  18.10.06 wl  gmAspiratePowder             TN3362    VarixMotorSteps ist var, wird auf den wirklich eingestellten Wert gesetzt
  19.01.07 wl  gmAspiratePowder1,gmDispensePowder1 TN3473  an gmSwitchModulePort wird jetzt Swich-Name, PipDevice und Tipmap übergeben
  19.01.07 wl  gmAspiratePowder,gmDispensePowder   TN3473  statt TSwichDevice wird nur der Name weitergegeben
  09.02.07 wl  gmDispensePowder                    TN3560  statt DispXYShifting wurde fälschlicherweise SampleAspXYShifting gelesen
  19.02.07 wl  gmAspiratePowder             TN3584   EV_BASP und EV_AASP werden auch bei mehreren Aspirates nur einmal ausgeführt
  02.03.07 pk  gmSetVarRediVolumes          TN3613   Calls TVarRediMotorDevice functions
  08.03.07 wl                               TN3620   uses geändert
  12.03.07 pk                               TN3628   create TZAxisTravelMotionSystem instead of MotorMotionSystem
  02.11.07 wl  gmRediShakerOn               TN3896   wenn aShakeTime <= 0, wird auch gRediShakerStruct.ShakeTime = 0 gesetzt und nicht gaschaltet
  02.11.07 wl  gmRediShakerOff              TN3896   wenn gRediShakerStruct.ShakeTime = 0 => nichts tun
  09.01.08 wl                               TN3972   interne Änderungen
  07.02.08 wl  gmSwitchOn/OffAllShakers     TN4009   von ObjModul hierher
  07.02.08 wl  gmSwitchOffAllShakers        TN4009   von ObjModul hierher
  05.05.08 wl  gmDispensePowder1            TN4064    Step Performing: "Event before/after disp liquid" is done at each dispense step
  15.05.08 wl                               TN4100    TXYStepList property changed
  19.05.08 pk                               TN4083   always Check if RediDev is assigned. otherwise Access violation
  20.06.08 pk                               TN4139   WB global object replaced by LayoutManager
  24.06.08 wl  gmSwitchRackDevice           TN4143   TDeviceType entfernt
  02.09.08 pk                               TN4215    DllCallExt changed to DllCall
  25.09.08 wl                               TN4242    TRunstCall ersetzt TDllCall
  31.07.09 wl  gmAspiratePowder             TN3950   liest und übergibt neues Feld: SampleAspInsertMoveType
  31.07.09 wl  gmDispensePowder             TN3950   liest und übergibt neues Feld: DispInsertMoveType
  31.07.09 wl  gmAspiratePowder1            TN3950   übergibt SampleAspInsertMoveType an MoveAspPosPowder
  31.07.09 wl  gmDispensePowder1            TN3950   übergibt DispInsertMoveType an MoveDspPosPowder
  31.07.09 wl  gmAspiratePowder1,gmDispensePowder1  TN4018  neu: Parameter SingleTip
  12.09.09 wl  gmAspiratePowder,gmDispensePowder  TN4740  --> PowderHandlingHigh
  12.09.09 wl  TPowderHandling              TN4740    alle Funktionen werden zu Klassen-Methoden
  12.09.09 wl                               TN4740    TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  24.11.09 ts  Aspirate/DispensePowder1     TN4771   xXYSteps created in gmCalculatePipSteps will be freed
  23.03.11 wl                               TN5515   MoveToZTravel: Methodenname geändert
  29.03.11 wl                               TN5524   uses OperationPip entfernt
  05.04.11 wl                               TN5535   ExecutePipPumps entfernt (hat hier nichts zu suchen)
  19.10.11 wl  Aspirate,DispensePowder1     TN5723   mit Parameter aColorChar
  28.09.11 wl                               TN5725   gmCalculatePipSteps jetzt ohne Parameter
  03.11.11 wl                               TN5725   verwendet TSubstanceLoading.StoreDisp/StoreAsp
  02.02.11 wl  alle Funktionen              TN5791   verwendet TArray<TXRackPosition> statt TRack und Pos-Array
  28.08.12 ts                               TN5943   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Tube
  15.02.13 wl                               TN5914   and Änderungen in SubstanceLoading angepasst
  15.02.13 wl  RediWash                     TN6089   gmResetWashFlags wird mit aufgerufen
  23.05.13 wl                               TN6153   geänderte Parameter bei MoveXY
  -------------------------------------------------------------------------------------------------- }

unit PowderHandling;


interface


uses
    MethodStepSettingRunStart,
    AppTypes,
    Rack,
    RackTypes,
    MethodTypes,
    IntfPipDevice,
    IntfArmDevice,
    GeneralTypes,
    CommonTypes,
    LogManager,
    ErrorManager,
    RunFlow,
    SamHigh,
    ObjModul,
    LayoutManager,
    AppSettings,
    MotionSystemPipMove,
    MotionSystemTravel,
    EventManager,
    OperationAxisMove,
    TipSystem,
    IntfMotorDevice,
    TipMapUtils,
    ArrayUtils,
    OperationFactory;

type
    TPowderHandling = class
    private
        class procedure APowder(aPipDevice: IPipDevice; aTips: TIPMAP; const aRP: TArray<TXRackPosition>;
            const aVol: TDoubleArray);
        class procedure DPowder(aPipDevice: IPipDevice; aTips: TIPMAP; const aRP: TArray<TXRackPosition>;
            const aVol: TDoubleArray; aDelay: integer; aEmptyVarRedi: boolean);
        class procedure SwitchOffAllShakers;
        class procedure PickPowder(aPipDevice: IPipDevice; aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>;
            const aVol: TDoubleArray; aDelay: integer; aIsLastAspirate: boolean);
        class procedure DispPowder(aPipDevice: IPipDevice; aTipMap: TIPMAP; const aRP: TArray<TXRackPosition>;
            const aVol: TDoubleArray; aDelay: integer; aRecordInDatabase: boolean; aEmptyVarRedi: boolean);
        class procedure SwitchRediBlower(aPipDevice: IPipDevice; aTips: TIPMAP; aOn: boolean);
        class procedure MoveToWiperRack(aUsedArm: IArmDevice; aTips: TIPMAP; aHRack: TRack;
            const aPos: TIntArray);
        class procedure GetRWashPipSteps(aUsedArm: IArmDevice; aTips: TIPMAP; aRediWashRack: TRack;
            out oPos: TIntArray; out oXYSteps: TXYStepList; out oZSteps: TPipStepZPosArray);
        class procedure RediWashWithRack(aUsedArm: IArmDevice; aTips: TIPMAP; aHRack: TRack);
    public
        class procedure SetVarRediVolumes(aPipDevice: IPipDevice; aTips: TIPMAP; const aVolumes: TDoubleArray;
            var vVarixMotorSteps: integer);

        class procedure RediWash(aUsedArm: IArmDevice; aTips: TIPMAP);
        class procedure AdjustRackZMaxToResinHeight(aRackNum: integer; aResinHeight: integer);

        class procedure SwitchRediPump(aPipDevice: IPipDevice; aTips: TIPMAP; aOn: boolean);
        class function RestAtRediWash(aUsedArm: IArmDevice; aTips: TIPMAP;
            aGoDownAndSwitchOffPump: boolean): TIPMAP;
        class procedure SwitchOnAllShakers(aTime: integer);

        class procedure AspiratePowder1(aUsedArm: IArmDevice; aSyrMap: TIPMAP;
            const aRP: TArray<TXRackPosition>;
            // array of Rack Positions
            const aVol: TDoubleArray; // aspiration volumes
            aDelay: integer; // aspiration delay
            aSubmerge_mm: TPosMM; // Subtract mm
            aSwitchDeviceName: string; // Port to Switch
            aSwitchPlace: TSwitchPlace; // switch position
            aSingleRetract: boolean; aEvBeforeAsp, aEvBeforePickLq, aEvAfterPickLq, aEvAfterAsp: TRunstCall;
            aZInsertSpeed, aZInsertMoveType, aZRetractSpeed: integer; aNoCalculation: boolean;
            aDisableZErr: boolean; aXYShifting: boolean; aIsLastAspirate: boolean; aSingleTip: boolean);
        class procedure DispensePowder1(aUsedArm: IArmDevice; aSyrMap: TIPMAP;
            const aRP: TArray<TXRackPosition>;
            // array of Rack Positions
            const aVol: TDoubleArray; // array of volumes
            aDelay: integer; // aspiration delay
            aSubmerge_mm: TPosMM; // Subtract steps
            aPerformingSteps: double; aPerformDelay: integer; aSwitchDeviceName: string; // Port to Switch
            aSwitchPlace: TSwitchPlace; // switch position
            aSingleRetract: boolean; aEvBeforeDisp, aEvBeforeDispLq, aEvAfterDispLq, aEvAfterDisp: TRunstCall;
            aZInsertSpeed, aZInsertMoveType: integer; aDisableZErr: boolean; aEmptyVarRedi: boolean;
            aXYShifting: boolean; aSingleTip: boolean);
        class function SwitchRackDevice(aTips: TIPMAP; aHRack: TRack; const aPos: TIntArray;
            aSwitchOn: boolean): integer;
    end;


implementation


uses
    Windows,
    SysUtils,
    RackWell,
    SubstanceLoading,
    Classes,
    Math,
    SamGlobe;

{ TPowderHandling }

class procedure TPowderHandling.SetVarRediVolumes(aPipDevice: IPipDevice; aTips: TIPMAP;
    const aVolumes: TDoubleArray; var vVarixMotorSteps: integer);
var
    i: integer;
begin
    if not(aPipDevice.FirstUsedTipType = VarRediTip) then
        EXIT;

    for i := 0 to aPipDevice.TipCount - 1 do
    begin
        if not TTipMapUtils.TipSelected(aTips, i) then
            CONTINUE;
        if not Assigned(aPipDevice.Tips[i].RediDev) then
            CONTINUE;
        aPipDevice.Tips[i].RediDev.MotorSetVol(aVolumes[i], vVarixMotorSteps);
    end;
end;

class procedure TPowderHandling.SwitchOnAllShakers(aTime: integer);
// var i:integer;
begin
    {
      gCommManager.Relays_RestoreNextStatus;
      for i:=0 to FDeviceList.Count-1 do
      if (FDeviceList.Items[i] is TSwitchDevice)
      and (FDeviceList.Items[i].DType=dptSwitchShaker) then begin
      if (GlobalErr<>ERR_NONE) then exit;
      (FDeviceList.Items[i] as TSwitchDevice).SwitchOn(true, aTime);
      gCommManager.Relays_ExecuteNextStatus;
      (FDeviceList.Items[i] as TSwitchDevice).SwitchOff(true);
      end;
      gCommManager.Relays_ExecuteNextStatus;
    }
end;

class procedure TPowderHandling.SwitchOffAllShakers;
// var i:integer;
begin
    { TODO -oPK -cDEVICE : DType removed }
    {
      for i:=0 to FDeviceList.Count-1 do
      if (FDeviceList.Items[i].DType=dptSwitchShaker)
      and (FDeviceList.Items[i] is TSwitchDevice) then
      (FDeviceList.Items[i] as TSwitchDevice).SwitchOff(true);
      gCommManager.Relays_ExecuteNextStatus;
    }
end;

class procedure TPowderHandling.APowder(aPipDevice: IPipDevice; aTips: TIPMAP;
    const aRP: TArray<TXRackPosition>; const aVol: TDoubleArray);
var
    i: integer;
begin
    gLogManager.Log('Aspirate Powder', false);
    // ----------------------------------------------------------------------- alle Schüttler abstellen
    // { TODO : gCommManager.Relays_xxx does not exist }
    // gCommManager.Relays_RestoreNextStatus;
    SwitchOffAllShakers;
    // --------------------------------------------------------------------------------- evtl. Pumpe an
    for i := 0 to aPipDevice.TipCount - 1 do
        if (((aTips shr i) and 1) = 1) then
        begin
            if not Assigned(aPipDevice.Tips[i].RediDev) then
                CONTINUE;
            aPipDevice.Tips[i].RediDev.PumpSwitchOnOrOff(true, true, 500);
            // Etwas Warten nach Anschalten der Pumpen
        end;
    // { TODO : gCommManager.Relays_xxx does not exist }
    // gCommManager.Relays_ExecuteNextStatus;
    // ----------------------------------------------------------------------------- evtl. Kanal öffnen
    for i := 0 to aPipDevice.TipCount - 1 do
        if (((aTips shr i) and 1) = 1) then
        begin
            if not Assigned(aPipDevice.Tips[i].RediDev) then
                CONTINUE;
            aPipDevice.Tips[i].RediDev.FeedingSwitchOnOrOff(true, true);
        end;
    // -------------------------------------------------------------------------- Blower aus, Vacuum an
    for i := 0 to aPipDevice.TipCount - 1 do
        if (((aTips shr i) and 1) = 1) then
        begin
            if not Assigned(aPipDevice.Tips[i].RediDev) then
                CONTINUE;
            aPipDevice.Tips[i].RediDev.BlowerSwitchOnOrOff(false, true);
            aPipDevice.Tips[i].RediDev.VacuumSwitchOnOrOff(true, true);
        end;
    // { TODO : gCommManager.Relays_xxx does not exist }
    // gCommManager.Relays_ExecuteNextStatus;
end;

class procedure TPowderHandling.DPowder(aPipDevice: IPipDevice; aTips: TIPMAP;
    const aRP: TArray<TXRackPosition>; const aVol: TDoubleArray; aDelay: integer; aEmptyVarRedi: boolean);
var
    i: integer;
    xTimeMarker: TDateTime;
    xCurrentDelayMSecs: int64;
    xVarixMotorSteps: integer;
begin
    // WL, 4.7.2006: hier war bisher ein Delay von der Länge trunc( aDelay / 2 ). Ich halte das für
    // Zeiverschwendung, zur Sicherheit habe ich es durch ein festes Delay von 100 msec ersetzt.
    // Besser wäre natürlich, dieses Delay konfigurierbar zu machen oder ganz zu entfernen.
    gRunFlow.AppSleep(100);

    // alle Schüttler abstellen
    if not(gShakeTimeAdjustable) then
    begin
        // { TODO : gCommManager.Relays_xxx does not exist }
        // gCommManager.Relays_RestoreNextStatus;
        SwitchOffAllShakers;
    end;

    // Start-Time-Marker setzen (für Delay)
    gLogManager.Log('Dispense Powder Delay: ' + IntToStr(aDelay), false);
    xTimeMarker := Now;

    // Vacuum aus, Blower an
    // { TODO : gCommManager.Relays_xxx does not exist }
    // gCommManager.Relays_RestoreNextStatus;
    for i := 0 to aPipDevice.TipCount - 1 do
        if (((aTips shr i) and 1) = 1) then
        begin
            if not Assigned(aPipDevice.Tips[i].RediDev) then
                CONTINUE;
            aPipDevice.Tips[i].RediDev.VacuumSwitchOnOrOff(false, true);
            aPipDevice.Tips[i].RediDev.BlowerSwitchOnOrOff(true, true);
        end;
    // { TODO : gCommManager.Relays_xxx does not exist }
    // gCommManager.Relays_ExecuteNextStatus;

    // wenn EmptyVarRedi = true wird der Motor auf 0 gefahren
    if (aEmptyVarRedi) then
    begin
        xVarixMotorSteps := -1;
        SetVarRediVolumes(aPipDevice, aTips, aPipDevice.GetNullDoubleArray, xVarixMotorSteps);
    end;

    // Delay: Verbliebene Zeit warten
    xCurrentDelayMSecs := aDelay + Round((xTimeMarker - Now()) * MSecsPerDay);
    gRunFlow.AppSleep(xCurrentDelayMSecs);

    // -------------------------------------------------------------------------- Blower aus, Vacuum an
    // { TODO : gCommManager.Relays_xxx does not exist }
    // gCommManager.Relays_RestoreNextStatus;
    for i := 0 to aPipDevice.TipCount - 1 do
        if (((aTips shr i) and 1) = 1) then
        begin
            if not Assigned(aPipDevice.Tips[i].RediDev) then
                CONTINUE;
            aPipDevice.Tips[i].RediDev.BlowerSwitchOnOrOff(false, true);
            aPipDevice.Tips[i].RediDev.VacuumSwitchOnOrOff(true, true);
        end;
    // { TODO : gCommManager.Relays_xxx does not exist }
    // gCommManager.Relays_ExecuteNextStatus;
end;

class procedure TPowderHandling.PickPowder(aPipDevice: IPipDevice; aTipMap: TIPMAP;
    const aRP: TArray<TXRackPosition>; const aVol: TDoubleArray; aDelay: integer; aIsLastAspirate: boolean);
var
    x: integer;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;
    aTipMap := aTipMap and aPipDevice.OKTips;
    TPowderHandling.APowder(aPipDevice, aTipMap, aRP, aVol);

    if (aDelay > 0) then
    begin
        gRunFlow.AppSleep(aDelay);
    end;

    for x := 0 to aPipDevice.TipCount - 1 do
        if (((aTipMap shr x) and 1) <> 0) then
        begin
            if (aIsLastAspirate) then
                TSubstanceLoading.Instance.StorePowderOrSampleAsp(aPipDevice.Tips[x].RediDev.Volumes,
                    aRP[x].Rack, aRP[x].Pos, aVol[x], asptPowder)
                // Update Tube liquid list
            else
                TSubstanceLoading.Instance.StorePowderOrSampleAsp(aPipDevice.Tips[x].RediDev.Volumes,
                    aRP[x].Rack, aRP[x].Pos, 0, asptPowder);
            // Update Tube liquid list
        end;
end;

class procedure TPowderHandling.DispPowder(aPipDevice: IPipDevice; aTipMap: TIPMAP;
    const aRP: TArray<TXRackPosition>; const aVol: TDoubleArray; aDelay: integer; aRecordInDatabase: boolean;
    aEmptyVarRedi: boolean);
var
    xSampleVol: TDoubleArray;
    x: integer;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;

    xSampleVol := aPipDevice.GetNullDoubleArray;
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        xSampleVol[x] := aVol[x]; // ProbenVolumen sichern für POSINFO
    end;

    aTipMap := aTipMap and aPipDevice.OKTips;

    DPowder(aPipDevice, aTipMap, aRP, aVol, aDelay, aEmptyVarRedi);

    for x := 0 to aPipDevice.TipCount - 1 do
        if (((aTipMap shr x) and 1) <> 0) and (aRecordInDatabase) then
        begin
            TSubstanceLoading.Instance.StoreDsp(aPipDevice.Tips[x].RediDev.Volumes, aRP[x].Rack, aRP[x].Pos,
                xSampleVol[x], dsptPowder);
            // Update Tube liquid list POSINFO
        end;
end;

class procedure TPowderHandling.MoveToWiperRack(aUsedArm: IArmDevice; aTips: TIPMAP; aHRack: TRack;
    const aPos: TIntArray);
var
    x, Step, xZMotor: integer;
    xIniAccess: IWinLissyIniAccess;
    xRackTypes: TStringArray;
    xRackName: string;
    // iperYArray: MPosArray;
    xWiperRack: TRack;
    xXYSteps: TXYStepList;
    xZSteps: TPipStepZPosArray;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xZTravelMotion: TZAxisTravelMotionSystem;
    xRP: TArray<TXRackPosition>;
begin
    xIniAccess := gCommonDll.CreateAppIni;
    xRackTypes := xIniAccess.ReadAllowedSection('Wiper', '');

    xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
    xZTravelMotion := TOperationFactory.CreateZAxisTravelMotionSys(aUsedArm.MotionDevice);
    xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);

    for x := 0 to Length(xRackTypes) - 1 do
        if (xRackTypes[x] = aHRack.TypeName) then
        begin
            xRackName := xIniAccess.ReadString('Wiper', xRackTypes[x]);
            // prüfe ob WiperRack existiert
            xWiperRack := TLayoutManager.Instance.CurrentLayout.FindRackByName(xRackName);
            if not Assigned(xWiperRack) then
                CONTINUE;

            SetLength(xRP, Length(aPos));
            for Step := 0 to high(xRP) do
            begin
                xRP[Step].Rack := xWiperRack;
                xRP[Step].Pos := aPos[Step];
            end;

            // berechne Koordinaten WiperRack
            gmCalculatePipSteps(aUsedArm, aTips, xRP, xXYSteps, xZSteps);
            for Step := 0 to xXYSteps.Count - 1 do
            begin
                if xXYSteps[Step].MotorMap <= 0 then
                    CONTINUE;

                // gLogManager.Log(Format('Move To Wiper: %s - X: %d to %d - Y: %d to %d',[xRackName,
                // xXYSteps.Steps[ Step ].X, xPStep[Step].X.Pos + xPStep[Step].X.MOffset,
                // xPStep[Step].Y.Pos[0], xPStep[Step].Y.Pos[0] + xPStep[Step].Y.MOffset]), LogDisplay);

                // führe Wipe-Bewegungen aus
                xZTravelMotion.MoveToZTravelAllTips(0, 0);

                xXYOp.MoveXY(xXYSteps[Step], gmGetRackPosArrayForTips(aUsedArm.PipDevice, xWiperRack),
                    xZSteps[Step], []);
                for xZMotor := 0 to xZMotion.Motors.MaxIndex do
                    xZMotion.MoveSingleZ(aTips, xZSteps[Step].Z.MOffset[xZMotor], m_EXECUTE);

                xXYOp.MoveX(xXYSteps[Step], xWiperRack.Structure.MOffsetUpX_mm, m_NO_EXEC, gWiperSpeedX);
                xXYOp.MoveY(xXYSteps[Step], xWiperRack.Structure.MOffsetUpY_mm, m_EXECUTE, gWiperSpeedY);

                xZTravelMotion.MoveToZTravelAllTips(0, 0);
            end;

            FreeAndNil(xXYSteps);

        end;
end;

// --------------------------------------------------------------------------------------------------
class procedure TPowderHandling.AspiratePowder1(aUsedArm: IArmDevice; aSyrMap: TIPMAP;
    const aRP: TArray<TXRackPosition>; // array of Rack Positions
    const aVol: TDoubleArray; // aspiration volumes
    aDelay: integer; // aspiration delay
    aSubmerge_mm: TPosMM; // Subtract mm
    aSwitchDeviceName: string; // Port to Switch
    aSwitchPlace: TSwitchPlace; // switch position
    aSingleRetract: boolean; aEvBeforeAsp, aEvBeforePickLq, aEvAfterPickLq, aEvAfterAsp: TRunstCall;
    aZInsertSpeed, aZInsertMoveType, aZRetractSpeed: integer; aNoCalculation: boolean; aDisableZErr: boolean;
    aXYShifting: boolean; aIsLastAspirate: boolean; aSingleTip: boolean);
// --------------------------------------------------------------------------------------------------
var
    xStep: integer;
    x: integer;
    // xTip: integer;
    xDisabledErrorMap: TIPMAP;
    xXYSteps: TXYStepList;
    xZSteps: TPipStepZPosArray;
    xPipDevice: IPipDevice;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xAspShiftXOffset, xAspShiftYOffset: TPosMM;
    xSingleRack: TRack;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;

    xPipDevice := aUsedArm.PipDevice;
    xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
    xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);

    aSyrMap := aSyrMap and xPipDevice.OKTips;

    xDisabledErrorMap := aSyrMap;
    if (aDisableZErr) then
        xZMotion.DisableZErrors(xDisabledErrorMap);

    // ahRack.PaintTubePositions(aPos,TRackWellDisplayType.Aspirate);
    gmCalculatePipSteps(aUsedArm, aSyrMap, aRP, xXYSteps, xZSteps);

    for xStep := 0 to xXYSteps.Count - 1 do
    begin
        xZSteps[xStep].SyrMap := xZSteps[xStep].SyrMap and xPipDevice.OKTips;

        if (xZSteps[xStep].SyrMap <= 0) then
            CONTINUE;

        if (TTipMapUtils.MaxLiquid(xZSteps[xStep].SyrMap, aVol) > 0) then
        begin

            if (aXYShifting) then
            begin
                xXYOp.GetXYAspShift(aRP, xXYSteps[xStep].MotorMap, xAspShiftXOffset, xAspShiftYOffset);
                xXYOp.MoveXYWithOffset(xXYSteps[xStep], xAspShiftXOffset, xAspShiftYOffset, aRP,
                    xZSteps[xStep], []);
            end
            else
            begin
                xXYOp.MoveXYWithOffset(xXYSteps[xStep], 0, 0, aRP, xZSteps[xStep], []);
            end;

            // Module Port über dem Röhrchen einschalten
            if (aSwitchPlace = spSwitchOutside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, true);

            // Aspiration Position in Z anfahren
            xZMotion.MoveAspPosPowder(xZSteps[xStep].SyrMap, xPipDevice.OKTips, aRP, aVol, aSubmerge_mm,
                aZInsertSpeed, aZInsertMoveType, xZSteps[xStep].Z.ZScan, xZSteps[xStep].Z.ZMax,
                xZSteps[xStep].Z.ZTube, aNoCalculation, aSingleTip);

            // Module Port im Röhrchen einschalten
            if (aSwitchPlace = spSwitchInside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, true);

            // Event before PickLiquid
            if Assigned(aEvBeforePickLq) then
                aEvBeforePickLq.Execute('before sucking powder');

            // PickLiquid
            PickPowder(xPipDevice, xZSteps[xStep].SyrMap, aRP, aVol, aDelay, aIsLastAspirate);

            // Event after PickLiquid
            if Assigned(aEvAfterPickLq) then
                aEvAfterPickLq.Execute('after sucking powder');

            // Module Port im Röhrchen ausschalten
            if (aSwitchPlace = spSwitchInside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, false);

            xSingleRack := TXRackPositionUtils.ExtractSingleRackFromArray(aRP);
            if (xSingleRack.Structure.MOffsetUpX_mm + xSingleRack.Structure.MOffsetUpY_mm <> 0) then
            begin

                // Z Offset für wiper anfahren
                for x := 0 to xPipDevice.TipCount - 1 do
                begin
                    if TTipMapUtils.TipSelected(xZSteps[xStep].SyrMap, x) then
                    begin
                        xZMotion.MoveSingleZ_Retract(x, xZSteps[xStep].Z.MOffset[x], m_NO_EXEC,
                            aZRetractSpeed, true);
                        if (aSingleRetract) then
                            xZMotion.Execute;
                    end;
                end;
                if (not aSingleRetract) then
                    xZMotion.Execute;

                // wirkliche Position (ohne x+y Offset) für Wiper anfahren
                if (gMoveToRealPosBeforeWiper) then
                begin
                    xXYOp.MoveX(xXYSteps[xStep], 0, m_NO_EXEC, gWiperSpeedX);
                    xXYOp.MoveY(xXYSteps[xStep], 0, m_EXECUTE, gWiperSpeedY);
                end;

                // gLogManager.LogF('AspiratePowder: MoveToWiper: X:%d - Y:%d - Z:%d MOffsetXUp:%d rd.MOffsetYUp:%d!',
                // [xPStep[xStep].X.Pos, xPStep[xStep].Y.Pos[0], xZSteps[xStep].Z.MOffset[0], xZSteps[xStep].X.MOffset, xPStep[xStep].Y.MOffset], false);

                xXYOp.MoveX(xXYSteps[xStep], xSingleRack.Structure.MOffsetUpX_mm, m_NO_EXEC, gWiperSpeedX);
                xXYOp.MoveY(xXYSteps[xStep], xSingleRack.Structure.MOffsetUpY_mm, m_EXECUTE, gWiperSpeedY);
                xZMotion.MoveZ(aSyrMap, xZSteps[xStep].Z.ZTravel, m_EXECUTE);
            end
            else
            begin
                // kein Retract (mit RetractSpeed)
                // fahre zu Wiper auf externen Wiper-Rack
                MoveToWiperRack(aUsedArm, aSyrMap, xSingleRack,
                    TXRackPositionUtils.ExtractPositionsFromArray(aRP));

                // nach dem Aspirate wird immer auf Z-Travel gefahren (muß das sein? WL)
                // aUsedArm.RackMoveManager.ResetLastRack;
            end;
        end;

        // Module Port über dem Röhrchen ausschalten
        if (aSwitchPlace = spSwitchOutside) then
            gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, false);

    end; // for xStep := 0 to xXYSteps.Count - 1

    if (aDisableZErr) then
        xZMotion.ResetDisabledZErrors(xDisabledErrorMap);
    FreeAndNil(xXYSteps);
end;

// --------------------------------------------------------------------------------------------------
class procedure TPowderHandling.DispensePowder1(aUsedArm: IArmDevice; aSyrMap: TIPMAP;
    const aRP: TArray<TXRackPosition>; // array of Rack Positions
    const aVol: TDoubleArray; // array of volumes
    aDelay: integer; // array of aspiration delays
    aSubmerge_mm: TPosMM; // Subtract steps
    aPerformingSteps: double; aPerformDelay: integer; aSwitchDeviceName: string; // Port to Switch
    aSwitchPlace: TSwitchPlace; // switch position
    aSingleRetract: boolean; aEvBeforeDisp, aEvBeforeDispLq, aEvAfterDispLq, aEvAfterDisp: TRunstCall;
    aZInsertSpeed, aZInsertMoveType: integer; aDisableZErr: boolean; aEmptyVarRedi: boolean;
    aXYShifting: boolean; aSingleTip: boolean);
// --------------------------------------------------------------------------------------------------
var
    x, xStep: integer;
    xPerStepVol: TDoubleArray;
    xVTips: TIPMAP;
    xSumVol: extended;
    xDisabledErrorMap: TIPMAP;
    xXYSteps: TXYStepList;
    xZSteps: TPipStepZPosArray;
    xPipDevice: IPipDevice;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xAspShiftXOffset, xAspShiftYOffset: TPosMM;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;

    xPipDevice := aUsedArm.PipDevice;
    xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
    xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);

    // wenn (Vol+PeriVol = 0) Tip aus Syrmap entfernen
    xVTips := 0;
    for x := 0 to xPipDevice.TipCount - 1 do
        if (aVol[x] > 0) then
            xVTips := xVTips or (1 shl x);

    aSyrMap := aSyrMap and xVTips;

    xDisabledErrorMap := aSyrMap;
    if (aDisableZErr) then
        xZMotion.DisableZErrors(xDisabledErrorMap);

    // ahRack.PaintTubePositions(aPos,TRackWellDisplayType.Dispense);
    gmCalculatePipSteps(aUsedArm, aSyrMap, aRP, xXYSteps, xZSteps);

    if Assigned(aEvBeforeDisp) then
        aEvBeforeDisp.Execute('before dispensing powder');

    for xStep := 0 to xXYSteps.Count - 1 do
    begin
        xZSteps[xStep].SyrMap := xZSteps[xStep].SyrMap and xPipDevice.OKTips;

        if (xZSteps[xStep].SyrMap <= 0) then
            CONTINUE;

        if (TTipMapUtils.MaxLiquid(xZSteps[xStep].SyrMap, aVol) > 0) then
        begin

            // Move to Dispense Position (Move along Corridor)
            if (aXYShifting) then
            begin
                xXYOp.GetXYAspShift(aRP, xXYSteps[xStep].MotorMap, xAspShiftXOffset, xAspShiftYOffset);
                xXYOp.MoveXYWithOffset(xXYSteps[xStep], xAspShiftXOffset, xAspShiftYOffset, aRP,
                    xZSteps[xStep], [xyoUseCorridor]);
            end
            else
            begin
                xXYOp.MoveXYWithOffset(xXYSteps[xStep], 0, 0, aRP, xZSteps[xStep], [xyoUseCorridor]);
            end;

            // Module Port über dem Röhrchen einschalten
            if (aSwitchPlace = spSwitchOutside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, true);

            xZMotion.MoveDspPosPowder(xZSteps[xStep].SyrMap, xPipDevice.OKTips, aRP, dapDispAtZDisp,
                aSubmerge_mm, aZInsertSpeed, aZInsertMoveType, 0, true, xZSteps[xStep].Z.ZScan,
                xZSteps[xStep].Z.ZDisp, xZSteps[xStep].Z.ZMax, aSingleTip);

            // Module Port im Röhrchen einschalten
            if (aSwitchPlace = spSwitchInside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, true);

            // ------------------------------------------------------------------------ Dispense
            if (aPerformingSteps <= 1) then
            begin
                if Assigned(aEvBeforeDispLq) then
                    aEvBeforeDispLq.Execute('before dropping powder');

                DispPowder(xPipDevice, xZSteps[xStep].SyrMap, aRP, aVol, aDelay, true, aEmptyVarRedi);

                if Assigned(aEvAfterDispLq) then
                    aEvAfterDispLq.Execute('after dropping powder');
            end
            else
            begin // In Schritten Dispensieren
                while (not gErrorManager.IsGlobalErr()) do
                begin
                    xSumVol := 0;
                    xPerStepVol := xPipDevice.GetNullDoubleArray;
                    for x := 0 to xPipDevice.TipCount - 1 do
                    begin
                        if (((xZSteps[xStep].SyrMap shr x) and 1) = 0) or (aVol[x] <= 0) then
                            CONTINUE;
                        xPerStepVol[x] := MinValue([aPerformingSteps, aVol[x]]);
                        aVol[x] := aVol[x] - xPerStepVol[x];
                        xSumVol := xSumVol + xPerStepVol[x];
                    end;
                    if (xSumVol <= 0) then
                        BREAK;

                    if Assigned(aEvBeforeDispLq) then
                        aEvBeforeDispLq.Execute('before dropping powder');

                    DispPowder(xPipDevice, xZSteps[xStep].SyrMap, aRP, xPerStepVol, aDelay, true,
                        aEmptyVarRedi);
                    gRunFlow.AppSleep(aPerformDelay);

                    if Assigned(aEvAfterDispLq) then
                        aEvAfterDispLq.Execute('after dropping powder');
                end;
            end;

            // Module Port im Röhrchen ausschalten
            if (aSwitchPlace = spSwitchInside) then
                gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, false);

            // ----------------------------------------------------------------------- Move out of the liquid evey single Needle though (Spetum)
            if (aSingleRetract) and (not gErrorManager.IsGlobalErr) then
            begin
                for x := 0 to xPipDevice.TipCount - 1 do
                begin
                    if (((xZSteps[xStep].SyrMap shr x) and 1) <> 0) then
                    begin
                        xZMotion.MoveSingleZ(x, xZSteps[xStep].Z.ZTravel[x], m_NO_EXEC);
                        gLogManager.Log('(Dispense2: Single Retract) goto ZTravel', false);
                    end; // keine TransAir
                    xZMotion.Execute;
                end;
            end;

        end;

        // Module Port über dem Röhrchen ausschalten
        if (aSwitchPlace = spSwitchOutside) then
            gmSwitchModulePort(xPipDevice, xZSteps[xStep].SyrMap, aSwitchDeviceName, false);
    end;
    if Assigned(aEvAfterDisp) then
        aEvAfterDisp.Execute('after dispensing powder');

    if (aDisableZErr) then
        xZMotion.ResetDisabledZErrors(xDisabledErrorMap);
    FreeAndNil(xXYSteps);
end;

class procedure TPowderHandling.GetRWashPipSteps(aUsedArm: IArmDevice; aTips: TIPMAP; aRediWashRack: TRack;
    out oPos: TIntArray; out oXYSteps: TXYStepList; out oZSteps: TPipStepZPosArray);
var
    x, xPosition: integer;
    xRP: TArray<TXRackPosition>;
begin
    SetLength(xRP, aUsedArm.PipDevice.TipCount);
    xPosition := 0;
    for x := 0 to aUsedArm.PipDevice.TipCount - 1 do
        if (((aTips shr x) and 1) = 1) then
        begin
            inc(xPosition);
            xRP[x].Rack := aRediWashRack;
            xRP[x].Pos := xPosition;
            if (x = aRediWashRack.TubeCount) then
                xPosition := 0;
        end;

    gmCalculatePipSteps(aUsedArm, aTips, xRP, oXYSteps, oZSteps);

    oPos := TXRackPositionUtils.ExtractPositionsFromArray(xRP);
end;

class procedure TPowderHandling.RediWash(aUsedArm: IArmDevice; aTips: TIPMAP);
var
    xRediWashRack: TRack;
begin
    xRediWashRack := TLayoutManager.Instance.CurrentLayout.FindSpecialRack(srtRediWash,
        aUsedArm.PipDevice.RackNamePrefix, false);
    if Assigned(xRediWashRack) then
        RediWashWithRack(aUsedArm, aTips, xRediWashRack);

    // Tips-Status zurücksetzen
    gmResetWashFlags(aUsedArm.PipDevice, aUsedArm.PipDevice.UseTips);
end;

class procedure TPowderHandling.SwitchRediPump(aPipDevice: IPipDevice; aTips: TIPMAP; aOn: boolean);
var
    x: integer;
begin
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if (((aTips shr x) and 1) <> 1) then
            CONTINUE;
        if not Assigned(aPipDevice.Tips[x].RediDev) then
            CONTINUE;
        aPipDevice.Tips[x].RediDev.PumpSwitchOnOrOff(aOn, false);
    end;
end;

class procedure TPowderHandling.SwitchRediBlower(aPipDevice: IPipDevice; aTips: TIPMAP; aOn: boolean);
var
    x: integer;
begin
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if (((aTips shr x) and 1) <> 1) then
            CONTINUE;
        if not Assigned(aPipDevice.Tips[x].RediDev) then
            CONTINUE;
        aPipDevice.Tips[x].RediDev.BlowerSwitchOnOrOff(aOn, false);
        aPipDevice.Tips[x].RediDev.VacuumSwitchOnOrOff(not aOn, false); // Vacuum immer umgekehrt schalten
    end;
end;

class function TPowderHandling.RestAtRediWash(aUsedArm: IArmDevice; aTips: TIPMAP;
    aGoDownAndSwitchOffPump: boolean): TIPMAP;
var
    xRediWashRack: TRack;
    x: integer;
    xPos: TIntArray;
    xPipDevice: IPipDevice;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xXYSteps: TXYStepList;
    xZSteps: TPipStepZPosArray;
begin
    result := 0;
    if gErrorManager.IsGlobalErr() then
        EXIT;

    xPipDevice := aUsedArm.PipDevice;

    if (aTips <= 0) then
        EXIT;
    xRediWashRack := TLayoutManager.Instance.CurrentLayout.FindSpecialRack(srtRediWash, aUsedArm.Name, false);
    if not Assigned(xRediWashRack) then
        EXIT;

    gLogManager.Log('Rest at Redi Wash-[' + xRediWashRack.Name + '] Tips ' + IntToStr(aTips), true);

    xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
    xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);

    GetRWashPipSteps(aUsedArm, aTips, xRediWashRack, xPos, xXYSteps, xZSteps);
    try
        for x := 0 to xXYSteps.Count - 1 do
        begin
            if (xXYSteps[x].MotorMap <= 0) then
                CONTINUE;

            // Positionen anfahren
            xXYOp.MoveXY(xXYSteps[x], gmGetRackPosArrayForTips(aUsedArm.PipDevice, xRediWashRack),
                xZSteps[x], []);

            // in Waschstation fahren und Pumpe abschalten
            if (aGoDownAndSwitchOffPump) then
            begin
                xZMotion.MoveZ(xZSteps[x].SyrMap, xZSteps[x].Z.ZDisp, m_EXECUTE);
                SwitchRediPump(xPipDevice, xZSteps[x].SyrMap, false);
                result := xZSteps[x].SyrMap;
            end;

            EXIT; // Es wird einmal eine Position angefahren
        end;
    finally
        FreeAndNil(xZMotion);
        FreeAndNil(xXYOp);
        FreeAndNil(xXYSteps);
    end;
end;

class function TPowderHandling.SwitchRackDevice(aTips: TIPMAP; aHRack: TRack; const aPos: TIntArray;
    aSwitchOn: boolean): integer;
// --------------------------------------------------------------------------------------------------
// Rückgabewert: Delay
// --------------------------------------------------------------------------------------------------
// var iTip,i:integer;
// iDPart:ISwitchDevice;
// iRName:string;
// iRow,iCol:integer;
begin
    { TODO -oPK -cDEVICE : DType removed }
    result := 0;
    { gCommManager.Relays_RestoreNextStatus;
      iDPart := nil;
      for iTip:=0 to MAX_TIPS-1 do if (((Tips shr iTip) and 1) = 1) then begin
      iRName := ahRack.Name;
      for i:=0 to FDeviceList.Count-1 do begin
      if (FDeviceList.Items[i] is TLeafDevice) then begin
      if ((FDeviceList.Items[i] as TLeafDevice).DType=iDevType) and ((FDeviceList.Items[i] as TLeafDevice).RackName=iRName) then begin
      gmGetCoord( iCol, iRow, ahRack.Structure.Rows, Pos[ iTip ] );
      if  (FDeviceList.Items[i] is TSwitchDevice)
      and (((FDeviceList.Items[i] as TLeafDevice).Col=0) or ((FDeviceList.Items[i] as TLeafDevice).Col=iCol+1))
      and (((FDeviceList.Items[i] as TLeafDevice).Row=0) or ((FDeviceList.Items[i] as TLeafDevice).Row=iRow+1))
      then iDPart := FDeviceList.Items[i] as TSwitchDevice;
      end;
      end;
      end;
      if (iDPart <> nil) then begin
      if iSwitchOn then iDPart.SwitchOn(true) else iDPart.SwitchOff(true);
      result:=iDPart.Delay;
      end;
      end;
      gCommManager.Relays_ExecuteNextStatus;
    }
end;

class procedure TPowderHandling.RediWashWithRack(aUsedArm: IArmDevice; aTips: TIPMAP; aHRack: TRack);
var
    x, iDelay: integer;
    xPos: TIntArray;
    xPipDevice: IPipDevice;
    xXYOp: TXYZTravelAxisPipMoveOperation;
    xZMotion: TZAxisPipMoveMotorMotionSystem;
    xXYSteps: TXYStepList;
    xZSteps: TPipStepZPosArray;
    xZTravelMotion: TZAxisTravelMotionSystem;
begin
    if gErrorManager.IsGlobalErr() then
        EXIT;
    if not Assigned(aHRack) then
        EXIT;
    if (aTips <= 0) then
        EXIT;

    gLogManager.Log('Wash - Rack-[' + aHRack.Name + '] Tips ' + IntToStr(aTips), true);

    xPipDevice := aUsedArm.PipDevice;

    xZMotion := TOperationFactory.CreateZAxisPipMoveMotorMotionSys(aUsedArm.MotionDevice);
    xXYOp := TOperationFactory.CreateXYZTravelPipMoveOp(aUsedArm);

    GetRWashPipSteps(aUsedArm, aTips, aHRack, xPos, xXYSteps, xZSteps);
    try
        for x := 0 to xXYSteps.Count - 1 do
        begin
            if (xXYSteps[x].MotorMap <= 0) then
                CONTINUE;

            // Positionen anfahren
            xXYOp.MoveXY(xXYSteps[x], gmGetRackPosArrayForTips(aUsedArm.PipDevice, aHRack), xZSteps[x],
                [xyoUseCorridor]);

            xZMotion.MoveZ(xZSteps[x].SyrMap, xZSteps[x].Z.ZDisp, m_EXECUTE);
            if gErrorManager.IsGlobalErr() then
                EXIT;

            // Vacuum anschalten
            iDelay := SwitchRackDevice(xZSteps[x].SyrMap, aHRack, xPos, { dptVacuum, } true);
            SwitchRediBlower(xPipDevice, xZSteps[x].SyrMap, true);

            // SwitchTipDevices(iTipBlower);   // Schalten von Druck alle x mal vorgesehen

            if (iDelay > 0) then
                gRunFlow.AppSleep(iDelay);

            SwitchRackDevice(xZSteps[x].SyrMap, aHRack, xPos, { dptVacuum, } false);
            SwitchRediBlower(xPipDevice, xZSteps[x].SyrMap, false);
        end;
    finally
        FreeAndNil(xZMotion);
        FreeAndNil(xXYOp);
        FreeAndNil(xXYSteps);
    end;

    xZTravelMotion := TOperationFactory.CreateZAxisTravelMotionSys(aUsedArm.MotionDevice);
    xZTravelMotion.MoveToZTravelAllTips(0, 0);
    FreeAndNil(xZTravelMotion);
end;

class procedure TPowderHandling.AdjustRackZMaxToResinHeight(aRackNum: integer; aResinHeight: integer);
var
    xRack: TRack;
begin
    if aResinHeight = 0 then
        EXIT;
    xRack := TLayoutManager.Instance.CurrentLayout.FindRackByRackID(IntToStr(aRackNum));
    if not Assigned(xRack) then
        EXIT;
    xRack.ChangeZMax(aResinHeight);
    gLogManager.Log('Set Zmax of Rack "' + IntToStr(aRackNum) + '" to ' +
        FloatToStr(xRack.RackStructure.ZMax_mm) + 'mm !', true);
end;


end.
