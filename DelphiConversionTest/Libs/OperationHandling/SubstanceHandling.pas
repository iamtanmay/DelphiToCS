{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : These are functions used by Actions which require both Powder and LiquidHandling
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.02.04 pk                               TN1719   New
  05.02.04 wl  gmGoToZTravel                TN1734   geänderter Parameter bei gPipArm.MoveZ
  12.03.04 pk  gmWash                       TN1810   dont turn on vacuumpump for default and dips tips
  12.03.04 wl  gmWash                       TN1812   GetPlate/PutPlate-Aufruf geändert: TRack wird übereben
  12.03.04 wl                               TN1812   alle MoveXY-Befehle: statt ZTravel-Wert wird aktuelles Rack übereben
  12.03.04 wl                               TN1812   alle Aufrufe von gmIsZTravel entfernt
  12.03.04 wl  gmWashAndBlow                TN1812   bei den MoveXY-Funktionen wird das Rack nicht übergeben
  19.03.04 wl  gmSetUseTips                 TN1788   benutzt global_CalliWPRTipName statt WB.Arm.UseTipType.AName
  05.04.04 wl  alle Funktionen!             TN1788   aUsedArm wird immer als Parameter übergeben und immer benutzt (wenn irgend möglich)
  05.04.04 wl  gmConvert/WritebackWashPositions  TN1788   jetzt nicht mehr notwendig
  05.04.04 wl  gmFindArmToUse               TN1788   statt gmSetUseTips, UsedArm wird hier ermittelt
  20.04.04 wl  TWashManager                 TN1788   von DilutionManager hierher
  20.04.04 wl  TWashManager.WashIfNeeded    TN1788.7 ConcurrentArm wird nicht mit gewaschen!!! (-> SIAS)
  20.04.04 wl  gmExecuteFlush               TN1788   ruft aUsedArm.SetUseTips auf
  20.04.04 wl  gmSwitchDiluent              TN1788   ruft gPipArm.SetUseTips auf
  20.04.04 wl  TWashManager.ExecuteWash     TN1788   ruft aUsedArm.SetUseTips auf
  22.04.04 wl                               TN1788   uses LiquidHandlingDiTi
  28.04.04 wl  gmDropAllDispTips            TN1788   von LiquidHandlingDiti hierher
  28.04.04 wl  gmInitializeSyringes         TN1788   von LiquidHandling hierher
  28.04.04 wl  gmGetArm_ByTip,gmFindRobotArmToUse,gmGetArm  TN1788  Funktionen, die TipManager-Methoden kapseln: result = TRobotArmDevice
  29.04.04 wl  TWashManager.IsWashNeeded    TN1883   Wenn die schmutzigen Tips im nächsten Job nicht enthalten sind, wird zwingend gewaschen!!
  05.05.04 wl  gmInitializeSyringes         TN1788   AddDilutors werden erst nach den anderern Dilutoren initialisiert (nicht mehrmals)
  10.05.04 pk  gmSOPHAS_Wash,gmExecuteWashProg TN1889.1 Use TRack param instead of Rackname (string) param
  10.05.04 wl  gmClearTipStatus_AllArms     TN1788   von DilutionManager hierher, mit Parameter "Airgaps auch löschen"
  10.05.04 wl  TWashManager.IsWashNeeded    TN1788   enthält wieder Abfrage auf WashFlag
  18.05.04 tbh gmWash                       TN1939   Dry-Station wird nur geschaltet wenn ein Waschvolumen gesetzt ist
  18.05.04 wl  TWashManager.ExecuteWash     TN1926   Wenn kein Waschvolumen -> nicht waschen
  18.05.04 wl  gmWash                       TN1927   Relays_Reset wird für REDI nicht ausgeführt
  08.06.04 wl  gmExecuteFlush               TN1951   bei Flush (Flush-Fenster,Flush-Action) werden REDI-Nadeln nicht mehr gewaschen
  16.06.04 wl  gmExecuteFlush,gmSOPHAS_Wash TN1963   unsinnige Beschränkungen für ZP02 entfernt
  17.06.04 wl  gmExecuteFlush               TN1951.1 Redi-Waschen nur wenn gFlushWithRediTips gesetzt ist
  17.06.04 wl  TWashManager.PrepareWash     TN1975   WashRetractSpeed wird immer auf 0 gesetzt
  24.06.04 tbh gmWash                       TN2006   Nach Waschmakro wird keine DRY-Station angefahren
  01.07.04 wl  gmAllArmsInitAddSyringes     TN1963   Add-Dilutors aller Greiferarme werden initialisiert
  01.07.04 wl                               TN1963   Anpassung an Änderungen in TPipStep
  06.07.04 wl  gmFindArmToUse               TN2019   geänderter Aufruf von PRRackManager.PreselectPRTips
  07.07.04 wl  gmWash                       TN2019   Aufruf von gmDropDispTips: RobotArmDevice statt nur PipArmDevice
  09.07.04 wl  gmDoWashMacro                TN2040   von LiquidHandling.pas hierher verschoben
  09.07.04 wl  gmWash                       TN2040   Waschmakro auch für REDI ermöglicht
  09.07.04 wl  gmWash                       TN2040   Aufruf von gmDry --> gnDoLiquidWash (LiquidHandling)
  22.07.04 pk                               TN2049   use TWashJob instead of Run record
  22.07.04 tbh gmSOPHAS_Wash                TN2037   benutzte Tips werden auch als UseTips gesetzt
  22.07.04 tbh WashAllDirtyTips                 TN1956   Waschen bei aLastWash nur wenn WashFlag gesetzt ist
  22.07.04 tbh TWashManager.ArmRequiresWashing  TN1956   New: Checks if wash flag is set
  25.08.04 wl  gmDoWashMacro                    TN2106   führt am Schluß ResetLastRack UND MoveToZTravel durch!
  26.08.04 tbh gmWash                           TN2107   Waschen schaltet nicht mehr Relais-Boards aus
  11.09.04 wl                                   TN2123   Geänderte property von TRoboticArm: FirstUsedTipName,-Type statt FirstUsedTip
  11.09.04 wl  gmWashAndBlow                    TN2123   Benutzt aUsedArm.CalcSingleStepRack statt gmMoveToWashOrWasteRack
  11.09.04 wl  gmWashAndBlow,gmSOPHAS_Wash      TN2123   geänderte Aufrufe von MoveZ (MposArray statt MPos)
  11.09.04 wl  gmSOPHAS_Wash                    TN2123   Z-Werte werden mit CalculatePipStaps ermittelt (statt GetRackData)
  07.10.04 tbh gmSOPHAS_Wash                    TN2073   Es werden nur Tips verwendet, die im Tipset vorhanden sind
  07.10.04 tbh gmSOPHAS_Wash                    TN2073   Falls nicht alle Tips verwendet werden können, Tip 1 setzen
  08.11.04 wl  gmDoWashMacro                    TN2213   benutzt TDllCallExt.ExecuteCommandMacro (= gmExecuteCommandMacro)
  11.11.04 pk  TipsNeedingWash                  TN2228   replaces the function IsWashNeeded
  11.11.04 pk  ExecuteWash                      TN2228   takes parameter "RequestedTips"
  11.11.04 pk  ExecuteWash                      TN2228   exludes tips which are disallowed
  17.11.04 pk                                   TN2231   uses RunDataAdaptorExt
  22.11.04 pk  TipsNeedingWash                  TN2237   New param : IgnoreWashFlag
  25.11.04 wl  gmSwitchDiluent                  TN2242   ALLE TOOLS werden vor dem Wechsel der Systemflüssigkeit & Waschen zurückgebracht!
  08.12.04 wl  gmSwitchDiluent                  TN2257   Tools werden jetzt nur beim Wechsel der Sytemflüssigkeit weggebracht, nicht bei jeder Abfrage des 6-Wege-Ventils
  09.12.04 wl                                   TN2246.4  uses ObjectsRun
  21.01.05 pk  PrepareWash                      TN2286   Use MaxVol for each volume stored in LastJob
  27.01.05 wl  gmDropAllDispTips                TN2297.1  DiTis am Handler sollen/können nicht beim Init abgeworfen werden
  31.01.05 wl  gmInitializeSyringes             TN2297.5  gmDry mit ( xArm.UseTips and xArm.DryAtInitTips )
  01.02.05 wl  PrepareWash                      TN2297.3 liest DryAfterWash aus Liquid Handling Parameter
  01.02.05 wl  ExecuteWash                      TN2297.3 übergibt DryAfterWash
  01.02.05 wl  gmWash                           TN2297.3 DryAfterWash als Parameter (Default = true)
  16.02.05 wl  gmSwitchDiluentAndWash           TN2269   wird jetzt mit aUsedArm und Tips als Parameter aufgerufen
  16.02.05 wl  gmSwitchDiluentAndWash           TN2269   ruft gDeviceMain.ChangeLiquid auf - wenn Liquid nicht gesetzt werden kann -> SetGlobalErr
  16.02.05 wl  gmExecuteFlush                   TN2269   ruft gDeviceMain.ChangeLiquid auf - wenn Liquid nicht gesetzt werden kann -> SetGlobalErr
  21.02.05 wl  alle Funktionen                  TN1960   gCommManager.AppSleep statt global_AppInterface-Methode
  22.02.05 wl  gmWash,PrepareWash,ExecuteWash   TN2323   an geändertes gmDoLiquidWash angepasst
  07.03.05 pk  PrepareWash                      TN2332   Code moved to TTipSystem.Wm_DetermineWashVols
  07.03.05 pk  TipsNeedingWash                  TN2332   Code moved to TTipSystem.Wm_TipNeedsWash
  15.04.05 wl  gmExecuteFlush                   TN2380.1  am Schluß werden auch die Add.Dilutors gespült (wenn AddDilutorTipMap > 0)
  18.04.05 wl  gmExecuteFlush                   TN2379    Erweitert um TipMap: nur Tips aus der Tipmap werden gespült
  02.05.05 wl  TWashManager.WashIfNeeded        TN2382    Bugfix: Beim Waschen des CombinedArm wurden die Nadeln des falschen Arms übergeben
  15.06.05 pk  gmDefineArmAndWorktype           TN2464.1  Removed - code for Calli moved to Dilutionmanager. gmFindToUse is now used instead
  21.06.05 pk  ArmRequiresWashing               TN2464.3  use St_IsClear to determine if tips need washing
  01.08.05 wl  TWashManager.JobToWashJob        TN2503    IsPipetteOrCalliOrManualFillAction statt IsPipetteAction (ManualFill ist andieser Stelle aber Unsinn)
  16.08.05 wl  gmWash,gmExecuteFlush            TN2554    Wenn PeriPump nicht existiert -> UsePeripump = false
  19.09.05 pk  gmInitializeSyringes             TN2605    Do for loop for all arms - Needed for machine with 2 pip arms
  14.10.05 wl  gmWashAndBlow                    TN2672    benutzt gmFindWashRack und gmFindWasteRack
  28.10.05 pk  gmExecuteFlush                   TN2717    instead of GetTipNames, call GetTipNamesForTipMap with usable tips as argument
  08.11.05 wl  gmReadWashConfig                 TN2745    --> WashprogDataAdaptor
  09.11.05 wl  gmSOPHAS_Wash                    TN2728    Aufruf von MoveXY mit geänderten Parametern
  30.11.05 wl                                   TN2818    Benutzung von RemRediTip entfernt
  11.01.06 pk  gmInitTipStatus_AllArms          TN2781.0  ResetAll param removed
  11.01.06 pk  TWashManager.ExecuteWash         TN2781.2  gmWash now Always called, even if usedtips = 0
  11.01.06 pk  gmWash                           TN2781.2  gActualWasteVol removed. Now call gmResetWashFlags
  08.02.06 thr gmInitializeSyringes             TN2925    additional parameter InitID
  14.03.06 wl  gmWashAndBlow                    TN2964    'SwitchOffN2C3BeforeWashInnerCh' wird gelesen, wenn true, wird N2CANNEL3 früher abgeschaltet
  15.03.06 wl  gmSOPHAS_Wash                    TN2965    'DelayBefore2ndBlowN2' ist ein neues Delay vor "2nd Blow N2"
  25.03.06 pk  gmInitializeSyringes             TN3000    GetInitTipNames gets tip name which do not have DoNotInit=true
  06.04.06 pk  gmFindArmByPipDevice             TN3023    moved to TModules
  19.04.06 wl                                   TN3051    benutzt TipFloatArray und extended für alle Vol, Speed und VolSteps
  25.04.06 pk  gmSwitchDiluentAndWash           TN3067    pass aTips to ChangeLiquid instead of OKTips
  24.08.06 wl                                   TN3271    bei den Aufrufen von gmCalculatePipSteps wird wieder PaintTubes übergeben
  07.09.06 wl  WashCombinedArm                  TN3287    war 1. Teil von WashIfNeeded
  07.09.06 wl  WashIfNeeded                     TN3287    1. Teil -> WashCombinedArm
  07.09.06 wl  WashIfNeeded                     TN3287    1. Teil -> WashCombinedArm, WasDisallowedTips entfernt
  12.09.06 wl  gmWash,PrepareWash               TN3285    neuer Parameter: WashMethodName
  12.09.06 wl  gmWash,PrepareWash               TN3286    neuer Parameter: DropMethodName
  18.09.06 pk                                   TN3227.1  references to DeviceMain replaced by SysLiqManager
  19.09.06 pk                                   TN3312    call gmFindDrySwitchDevice to find vacuumpump switch
  19.09.06 wl  gmWash                           TN3286.2  DropMethodName & WashMethodName wird mit hardcodierten Tip-Variablen aufgerufen
  26.09.06 wl  gmWash                           TN3326    DropMethodName wieder entfernt
  27.09.06 wl  gmSOPHAS_Wash                    TN3340    wenn ein Switch namens "WASHPROGVACUUMPUMP" existiert, wird dieser statt VACUUMPUMP benutzt
  27.10.06 pk  gmSwitchDiluentAndWash           TN3227    use GetTipNamesForTipMap instead of just GetTipNames, avoid washing same tips more than once
  03.11.06 pk  gmSOPHAS_Wash                              completely commented out to avoid compiler messages
  20.11.06 wl  gmWash                           TN3418    Das unsinnige MoveToZTravel am Anfang entfernt
  14.02.07 wl  gmWashAndBlow                    TN3147    geänderter Aufruf von gmCalculatePipStep, alles in try-finally-Blöcken
  06.03.07 wl  gmWash                           TN3620    Abfrage ModuleExist(PeriPump) entfernt
  08.03.07 wl  gmExecuteFlush                   TN3620    gModules.FindPeriPump statt ModuleExist(PeriPump)
  12.03.07 pk                                   TN3628    create TZAxisTravelMotionSystem instead of MotorMotionSystem
  23.03.07 pk  ArmRequiresWashing               TN3546    removed
  23.07.07 wl  gmWashAndBlow,...                TN3792    Alle WashProgram-Funktionen --> WashProgramHandling
  27.07.07 wl  GetDirtyTipsNeedingAutoWash      TN3779    PipDevice.TipCount statt PipDevice.Count (sonst Access Violation)
  27.08.07 wl  mehrere Funktionen               TN3811.4  Integer( TBasicTipType ) statt TBasicTipTypes (set)
  27.11.07 wl  ExecuteWash                      TN3897    Parameter LastWash entfernt
  27.11.07 wl  PrepareWash                      TN3897    entfernt: wenn letzter Schritt und noch Waste in Dilutor..
  09.01.08 wl                                   TN3972    interne Änderungen
  20.06.08 pk                                   TN4139    WB global object replaced by LayoutManager
  03.07.08 wl                                   TN4157
  09.07.08 pk  gmFindArmToUse                   TN4157    now takes pipdevicename
  16.07.08 wl  gmInitializeSyringes             TN4164    Search for Used Arms changed
  02.09.08 pk                                   TN4215    all methods changed to class methods
  19.09.08 pk  JobToWashJob                     TN4215    moved to DilutionManager
  23.09.08 wl  gmExecuteFlush                   TN4238    statt Diluent (1-basiert) wird DilIndex (0-basiert) benutzt
  25.09.08 wl  TWashManager                     TN4242    WashMacro gibt es nicht mehr, nur noch WashMethod
  15.10.08 pk  GetParamsForTipMethods           TN4258    returns TKeyValueParamArray instead of string
  06.11.08 pk                                   TN4280    ThreadManager changed to ThreadAPI
  06.04.09 pk  gmInitializeSyringes             TN4503    Assertion replaced by if IsCurrentLayoutEmpty
  31.07.09 wl  IsSampleAlreadyMixed             TN4049    entfernt: Prfung auf SampleAspMixFirstOnly findet jetzt in gmAspirate statt
  10.08.09 wl                                   TN4702    Strings werden jetzt direkt geladen
  12.09.09 wl                                   TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  21.09.09 pk  ExecuteWash                      TN4740    MaxTips replaced by TipCount
  04.11.09 pk                                   TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  02.11.09 pk  gmDropAllDispTips                TN4837    caused access violation because no pipdevice was assigned
  12.11.09 pk  gmInitializeSyringes             TN4859    RefreshLayoutDeviceList before DropAllDispTips
  13.11.09 pk  gmSwitchDiluentAndWash           TN4843    FreeAndNil of a stringarray caused AV
  17.12.09 pk  gmWash                           TN4951    Copy the volumes array before setting values to 0
  04.15.10 pk  gmInitializeSyringes             TN5050    calls StoreTips
  07.05.10 pk  GetParamsForTipMethods           TN5092    Creates a TKeyArgValueList
  09.06.10 pk  gmInitializeSyringes             TN5077    Call StoreTips even for Arms that have a grip device
  25.06.10 pk  gmInitialize..., gmExecuteFlush  TN5163    Can now initialize different Tips with different tiptypenames at same time
  12.08.10 wl  gmExecuteFlush                   TN5227   an TipSystem angepasst
  11.04.11 wl  CopyArray                        TN5549   ist jetzt generisch
  27.04.11 wl  ExecuteWash                      TN5562   liest 'DropTipsBeforewashMethod'
  27.04.11 wl  Wash                             TN5562   wenn aDropTipsBeforewashMethod=false, werden die Spitzen vor der Waschmethode nicht abgeworfen
  14.09.11 wl  TWashManager.ExecuteWash         TN5672   Keine Compiler-Warnung mehr
  19.10.11 wl  TWashManager.Wash                TN5722   Es wird nicht mehr die komplette DropTipMap abgeworfen, wenn weniger Nadeln gewaschen werden
  15.11.11 wl  gmWash                           TN5738   WashMethod, neuer Parameter: _$PipDevice
  14.12.11 wl                                   TN5765   uses geändert
  16.02.12 wl  gmExecuteFlush                   TN5807   Cycle entspricht wieder der Spritzengröße, nicht TipType-MaxVol
  01.03.12 wl                                   TN5822   uses geändert
  10.08.12 wl  TWashManager.Wash                TN5947   aPumpIndices: Pumpennummer der ersten Pumpe kann bestimmt werden
  10.08.12 wl  PrepareWash                      TN5947   neuer Parameter UsePeripump
  13.11.12 wl  PrepareWash                      TN6016   wenn es keine Peripumpe gibt, bleibt UsePeripump = false
  15.02.13 wl  gmResetWashFlags                 TN6089   --> SamHigh
  15.02.13 wl  TWashManager.Wash                TN6089   gmResetWashFlags wird grundsätzlich nur noch für Wasch-Methoden aufgerufen
  21.08.13 wl                                   TN6249   uses geändert
  25.09.13 wl  SwitchDiluentAndWash             TN6258   Übernimmt die zu waschenden Tips aus der ChangeLiquid-Methode
  26.09.13 wl  Wash                             TN6250   Schalten der Vacuumpumpe --> LiquidHandling
  30.09.13 wl  Wash                             TN6260   Cycles als neuer Parameter
  -------------------------------------------------------------------------------------------------- }

unit SubstanceHandling;


interface


uses
    AppTypes,
    CommonTypes,
    IntfArmDevice,
    Rack,
    LiquidHandling,
    IntfPipDevice,
    TipSystem,
    IntfPeriPumpDevice,
    GeneralTypes;

type
    TWashManager = class
    private
        class procedure PrepareWash(aPipDevice: IPipDevice; aPeripump: IPeriPumpDevice;
            out oWashVolCh1, oWashVolCh2: TDoubleArray; out oWashRetrSpeed: integer;
            out oWashMethodName: string; out oDryAfterWash: TDryAfterWashType; out oUsePeripump: boolean);
        class function GetDirtyTipsNeedingAutoWash(aPipDevice: IPipDevice): TIPMAP;
    public
        class procedure VerifyTipNumber(aPipDevice: IPipDevice; aArmTipIndex: integer);
        class function GetCurrentBatchID(): integer;
        class procedure InitTipArray();
        class procedure ExecuteWash(aUsedArm: IArmDevice; aRequestedTips: TIPMAP);
        class procedure WashAllDirtyTips();
        class procedure WashCombinedArm(aUsedArm: IArmDevice);
        class procedure Wash(aUsedArm: IArmDevice; const aVolume, aVolume2: TDoubleArray; aCycles: integer;
            aUsePeripump: boolean; aWashRetractSpeed: integer; const aWashMethodName: string;
            aDryAfterWash: TDryAfterWashType; aDropTipsBeforewashMethod: boolean;
            const aPumpIndices: TIntArray);
    end;

procedure gmSwitchDiluentAndWash(aUsedArm: IArmDevice; aTips: TIPMAP; aDilIndex: integer);
procedure gmInitializeSyringes(aInitID: TDateTime);
procedure gmDropAllDispTips;

procedure gmFindArmToUse(out oUsedArm: IArmDevice; var vUsedTips: TipMap; const aPipDeviceName: string;
    const aUsedTipType: string);

procedure gmInitTipStatus_AllArms();


implementation


uses
    Windows,
    SysUtils,
    Math,
    LogManager,
    ErrorManager,
    TipMapUtils,
    ThreadClasses,
    Streamable,
    PowderHandling,
    LiquidHandlingDiti,
    ToolHandling,
    LayoutManager,
    SamIntf,
    SamGlobe,
    LiquidManager,
    PipDeviceManager,
    ObjModul,
    SamHigh,
    Device,
    IntfSwitchDevice,
    AppSettings,
    LiquidHandlingLow,
    EventManager,
    ArrayUtils,
    MethodGUIParsing,
    RackTypes,
    MotionSystemPipMove,
    MotionSystemTravel,
    OperationAxisMove,
    OperationFactory,
    MethodTypes,
    PeripheryManager,
    TipTypeDataAdaptor;

procedure gmInitTipStatus_AllArms();
var
    x: integer;
begin
    for x := 0 to gPipDeviceManager.Count - 1 do
        (gPipDeviceManager[x]).St_InitTipStatus();
end;

procedure gmInitializeSyringes(aInitID: TDateTime);
var
    xDevPump: ISwitchDevice;
    i, xSearchIndex: integer;
    xTipUsageArray: TTipUsageRecArray;
    xArm: IArmDevice;
    xRack: TRack;
begin
    // ---------------------------------------------------------------- Init Syringes (in Wastestation)
    TPeripheryManager.Instance.RefreshLayoutDeviceList;
    gmDropAllDispTips; // -- Drop Tips
    if TLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;

    xSearchIndex := 0;

    while TPeripheryManager.Instance.FindNextModule(IArmDevice, xSearchIndex, xArm) do
    begin

        if not Assigned(xArm.PipDevice) then
            CONTINUE;

        xArm.PipDevice.StoreTips();

        if Assigned(xArm.GripDevice) then
            CONTINUE; // Arms that have a GripDevice are not initialized

        xTipUsageArray := xArm.PipDevice.GetInitTipUsage([DefaultTip, DispTip]);

        if (Length(xTipUsageArray) = 0) then
            CONTINUE;

        gLogManager.LogF('%s - Init Syringes', [xArm.Name], true);

        xDevPump := TLiquidHandlingLow.FindDrySwitchDevice(dsdVacuumPump, xArm.PipDevice);
        if Assigned(xDevPump) then
            xDevPump.SwitchOn(false);

        for i := 0 to Length(xTipUsageArray) - 1 do
        begin
            xArm.PipDevice.SetUseTips(xTipUsageArray[i].TipTypeName, xTipUsageArray[i].TipMap, false);
            xRack := TLiquidHandlingLow.InitSyringes(xArm, xArm.PipDevice.UseTips, aInitID);
            TLiquidHandlingLow.FlushAfterInit(xArm, xRack, xArm.PipDevice.UseTips);
            gmResetWashFlags(xArm.PipDevice, xArm.PipDevice.UseTips);
            // --------------------------------------------------------------- Sophas: Nadeln in Drystation
            TLiquidHandlingLow.Dry(xArm, (xArm.PipDevice.UseTips and xArm.PipDevice.DryAfterFlushTips));
        end;

        if Assigned(xDevPump) then
            xDevPump.SwitchOff(false);
    end;
end;

procedure gmDropAllDispTips;
var
    iList: TStringArray;
    i: integer;
    xArm: IArmDevice;
    xPipDevice: IPipDevice;
    xSearchIndex: integer;
begin
    xSearchIndex := 0;

    while TPeripheryManager.Instance.FindNextModule(IArmDevice, xSearchIndex, xArm) do
    begin

        if not Assigned(xArm.PipDevice) then
            CONTINUE;
        if Assigned(xArm.GripDevice) then
            CONTINUE; // DiTi-Tips am Gripper bei Init nicht abwerfen

        xPipDevice := xArm.PipDevice;
        iList := xPipDevice.GetTipNames([DispTip]);

        for i := 0 to Length(iList) - 1 do
        begin
            gLogManager.LogF('Arm %s - Drop Disposable Tips of Type: %s ', [xArm.Name, iList[i]], false);
            xPipDevice.SetUseTips(iList[i], 0, false);
            gmDropDispTips(xArm, xPipDevice.UseTips, DT_DISP_ERROR, false);
            xPipDevice.DropDispTips(xPipDevice.UseTips);
        end;

    end;

end;

class procedure TWashManager.Wash(aUsedArm: IArmDevice; const aVolume, aVolume2: TDoubleArray;
    aCycles: integer; aUsePeripump: boolean; aWashRetractSpeed: integer; const aWashMethodName: string;
    aDryAfterWash: TDryAfterWashType; aDropTipsBeforewashMethod: boolean; const aPumpIndices: TIntArray);
var
    i: integer;
    iTips, xDropTips: TIPMAP;
    xPipDevice: IPipDevice;
    xVolume, xVolume2: TArray<double>;
    xWashMethodParams: TKeyArgValueList;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;

    xPipDevice := aUsedArm.PipDevice;

    if (xPipDevice.UseTips = 0) then
        EXIT; // UseTips müssen bereit bestimmt sein

    xVolume := TArrayUtils.CopyArray<double>(aVolume);
    xVolume2 := TArrayUtils.CopyArray<double>(aVolume2);
    // ------------------------------------------------------------------- Volumen-Array zurechtstutzen
    for i := 0 to xPipDevice.TipCount - 1 do
    begin
        if not TTipMapUtils.TipSelected(xPipDevice.UseTips, i) then
        begin
            xVolume[i] := 0;
            xVolume2[i] := 0;
        end;
    end;
    // ----------------------------------------------------------------- gegebenenfalls TipMap anpassen
    iTips := xPipDevice.UseTips;
    // ------------------------------------------------------- Disp. Tips abwerfen (mit Corridor-Move)
    xDropTips := iTips and xPipDevice.DropTipMap;
    if (xPipDevice.FirstUsedTipType = DispTip) and (xDropTips > 0) and
        ((aWashMethodName = '') or aDropTipsBeforeWashMethod) then
        gmDropDispTips(aUsedArm, xDropTips, DTErrorMode, true);

    if (aWashMethodName <> '') then
    begin
        xWashMethodParams := TEventManager.CreateParamsForTipMethods(xPipDevice.name, xPipDevice.UseTips,
            xPipDevice.FirstUsedTipName);
        try
            TEventManager.Instance.ExecuteRunStartWithNameAndParams('Wash Method', aWashMethodName,
                xWashMethodParams);

            // Flags zurücksetzen (wir wissen ja nicht was passiert ist)
            gmResetWashFlags(xPipDevice, xPipDevice.UseTips);
        finally
            xWashMethodParams.Free;
        end;
        EXIT;
    end;

    // ------------------------------------------------------------------------------- normales Waschen
    if (xPipDevice.FirstUsedTipType in [DispTip, DefaultTip]) then
    begin
        // normales Waschen der Nadeln (in WASTE- und WASH-Rack)
        TLiquidHandling.LiquidWash(aUsedArm, xVolume, xVolume2, aCycles, aUsePeripump, true,
            aWashRetractSpeed, aDryAfterWash, aPumpIndices); // mit Corridor-Move
    end;
    // ----------------------------------------------------------------------------------- Redi-Waschen
    if (aUsedArm.PipDevice.FirstUsedTipType in [RediTip, VarRediTip]) then
    begin
        TPowderHandling.RediWash(aUsedArm, aUsedArm.PipDevice.UseTips);
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure gmSwitchDiluentAndWash(aUsedArm: IArmDevice; aTips: TIPMAP; aDilIndex: integer);
// --------------------------------------------------------------------------------------------------
var
    i: integer;
    xTipUsageArray: TTipUsageRecArray;
    xUsableTips: TIPMAP;
    xPipDevice: IPipDevice;
begin
    xPipDevice := aUsedArm.PipDevice;
    // Must the system liquid be changed?
    xUsableTips := gSysLiqManager.ChangeLiquid(xPipDevice, aTips, aDilIndex);
    if (xUsableTips = 0) then
        EXIT;

    xTipUsageArray := xPipDevice.GetTipUsageForTipMap(xUsableTips, [DefaultTip, DispTip]);
    if (Length(xTipUsageArray) <= 0) then
        EXIT; // There are no liquid tips!

    // Flush Syringes (in Waschstation)
    gLogManager.Log('Wash syringes after changing diluent - Tipmap: ' + IntToStr(xUsableTips), true);

    // Tools wegbringen
    gmArmBringBackTool(aUsedArm, false);

    for i := 0 to Length(xTipUsageArray) - 1 do
    begin
        xPipDevice.SetUseTips(xTipUsageArray[i].TipTypeName, xTipUsageArray[i].TipMap, false);
        TWashManager.Wash(aUsedArm, TArrayUtils.GetDefinedDoubleArray(gFlushVol, aUsedArm.PipDevice.TipCount),
            TArrayUtils.GetDefinedDoubleArray(gFlushVol, aUsedArm.PipDevice.TipCount), 1, false, 0, '',
            dawUseTipParameter, false, TLiquidChannels.GetDefaultChannel1Array(aUsedArm.PipDevice))
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure gmFindPipDeviceToUse(out oPipDevice: IPipDevice; var vUsedTips: TipMap;
    const aPipDeviceName: string; const aUsedTipType: string);

// --------------------------------------------------------------------------------------------------
begin
    // oPipDevice := gPipDeviceManager.FindArmToUse(aUsedTipType, vUsedTips, true);
    oPipDevice := gPipDeviceManager.FindPipDevice_ByName(aPipDeviceName);
    oPipDevice.SetUseTips(aUsedTipType, vUsedTips, true);
    vUsedTips := oPipDevice.UseTips;

    gLogManager.LogF('Tipmap %d - Total tipmap %d - Tip Type: %s',
        [oPipDevice.UseTips, vUsedTips, aUsedTipType], false);
end;

// --------------------------------------------------------------------------------------------------
procedure gmFindArmToUse(out oUsedArm: IArmDevice; var vUsedTips: TipMap; const aPipDeviceName: string;
    const aUsedTipType: string);

// --------------------------------------------------------------------------------------------------
var
    xPipDevice: IPipDevice;
begin
    gmFindPipDeviceToUse(xPipDevice, vUsedTips, aPipDeviceName, aUsedTipType);
    oUsedArm := gModules.FindArmByPipDevice(xPipDevice);

end;
/// /--------------------------------------------------------------------------------------------------
// procedure gmSetUsedTips( var vUsedTips : TipMap; const aPipDeviceName : string; const aUsedTipType : string );
//
/// /--------------------------------------------------------------------------------------------------
// var
// xDummy : IPipDevice;
// begin
// gmFindPipDeviceToUse( xDummy, vUsedTips, aPipDeviceName, aUsedTipType );
// end;


// ==================================================================================================
// TWashManager
// ==================================================================================================

class function TWashManager.GetCurrentBatchID(): integer;
begin
    result := TThreadAPI.GetCurrentThreadID();
end;

// --------------------------------------------------------------------------------------------------
class procedure TWashManager.PrepareWash(aPipDevice: IPipDevice; aPeripump: IPeriPumpDevice;
    out oWashVolCh1, oWashVolCh2: TDoubleArray; out oWashRetrSpeed: integer; out oWashMethodName: string;
    out oDryAfterWash: TDryAfterWashType; out oUsePeripump: boolean);
// --------------------------------------------------------------------------------------------------
var
    x: integer;
begin
    oWashVolCh1 := aPipDevice.GetNullDoubleArray;
    oWashVolCh2 := aPipDevice.GetNullDoubleArray;
    oWashRetrSpeed := 0;
    oWashMethodName := '';
    oDryAfterWash := dawDoNotDryAfterWash;
    oUsePeripump := false;

    for x := 0 to aPipDevice.TipCount - 1 do
    begin

        oWashVolCh1[x] := 0;
        oWashVolCh2[x] := 0;
        if (not aPipDevice.Tips[x].Wm_Dirty) then
            Continue;

        oWashVolCh1[x] := aPipDevice.Tips[x].Wm_WashVolCh1;
        oWashVolCh2[x] := aPipDevice.Tips[x].Wm_WashVolCh2;

        // Bestimmen von WashRetrSpeed
        oWashRetrSpeed := 0; // aPipDevice.Tips[x].Wm_LastJob.LiqH.WashRetrSpeed;

        // Wash method Name
        oWashMethodName := aPipDevice.Tips[x].Wm_WashMethodName;

        if (aPipDevice.Tips[x].Wm_DryAfterWash) then
            oDryAfterWash := dawDoDryAfterWash;
        if (aPipDevice.Tips[x].Wm_UsePeripump) and Assigned(aPeripump) then
            oUsePeripump := true;
    end;
end;

// --------------------------------------------------------------------------------------------------
class procedure TWashManager.ExecuteWash(aUsedArm: IArmDevice; aRequestedTips: TIPMAP);
// --------------------------------------------------------------------------------------------------
var
    i, xWashRetrSpeed: integer;
    iMaxVol: extended;
    xWashVolCh1, xWashVolCh2: TDoubleArray;
    xWashMethodName: string;
    xTipsToWash: TIPMAP;
    xDryAfterWash: TDryAfterWashType;
    xUsePeripump: boolean;
    xPipDevice: IPipDevice;
    xIniAccess: IWinLissyIniAccess;
    xWashOnlyUsedTips, xDropTipsBeforewashMethod: boolean;
begin
    if (gErrorManager.IsGlobalErr) then
        Exit;

    if (aRequestedTips = 0) then
        Exit;

    xTipsToWash := 0;

    xPipDevice := aUsedArm.PipDevice;

    for i := 0 to xPipDevice.TipCount - 1 do
    begin
        if (not TTipMapUtils.TipSelected(aRequestedTips, i)) then
            CONTINUE;
        TTipMapUtils.SelectTip(xTipsToWash, i);
    end;
    if xTipsToWash = 0 then
        EXIT;

    xDropTipsBeforewashMethod := false;
    try
        PrepareWash(xPipDevice, gModules.FindPeripump(), xWashVolCh1, xWashVolCh2, xWashRetrSpeed,
            xWashMethodName, xDryAfterWash, xUsePeripump);

        // UseTips bestimmen
        xPipDevice.SetUseTips('', xTipsToWash, false);
        if (xPipDevice.UseTips <> xTipsToWash) then // sollte nicht vorkommen!
            gLogManager.Log('Error: Tips (' + IntToStr(xPipDevice.UseTips) +
                ') can not be washed together', true);

        // Wenn kein Waschmakro und kein Waschvolumen -> exit
        if (Math.MaxValue(xWashVolCh1) <= 0) and (Math.MaxValue(xWashVolCh2) <= 0) then
            exit;

        xIniAccess := TAppSettings.CreateAppIni;
        xWashOnlyUsedTips := xIniAccess.ReadBool('Pipetting', 'WashOnlyUsedTips');
        xDropTipsBeforewashMethod := xIniAccess.ReadBool('Pipetting', 'DropTipsBeforewashMethod');

        // ------------------------------------------------------------------------- Waschen wie früher
        if (not xWashOnlyUsedTips) then
        begin
            // ---------------------------------------- alle weiteren Tips vom gleichen Typ heraussuchen
            xPipDevice.SetUseTips(xPipDevice.FirstUsedTipName, 0, false);

            if xPipDevice.UseTips = 0 then
                Exit;
            // ------------------------------- höchstes Volumen heraussuchen und für alle Tips verwenden
            iMaxVol := 0;
            for i := 0 to xPipDevice.TipCount - 1 do
                if TTipMapUtils.TipSelected(xPipDevice.UseTips, i) then
                    iMaxVol := Max(iMaxVol, xWashVolCh1[i]);
            for i := 0 to xPipDevice.TipCount - 1 do
                if TTipMapUtils.TipSelected(xPipDevice.UseTips, i) then
                    xWashVolCh1[i] := iMaxVol;
        end;

    finally
        // ------------------------------------------------------------------------ Waschen durchführen
        // ALWAYS call gmWash - because it calls wm_clean to reset the flags
        TWashManager.Wash(aUsedArm, xWashVolCh1, xWashVolCh2, 1, xUsePeripump, xWashRetrSpeed,
            xWashMethodName, xDryAfterWash, xDropTipsBeforewashMethod,
            TLiquidChannels.GetDefaultChannel1Array(aUsedArm.PipDevice));
    end;
end;

class procedure TWashManager.InitTipArray();
var
    xArm, xTip: integer;
begin
    for xArm := 0 to gPipDeviceManager.Count - 1 do
    begin
        for xTip := 0 to gPipDeviceManager[xArm].TipCount - 1 do
        begin

            gPipDeviceManager[xArm].Tips[xTip].Wm_Clean;
        end;
    end;
end;

class procedure TWashManager.VerifyTipNumber(aPipDevice: IPipDevice; aArmTipIndex: integer);
begin
    ASSERT(aArmTipIndex < aPipDevice.TipCount);
end;

class function TWashManager.GetDirtyTipsNeedingAutoWash(aPipDevice: IPipDevice): TIPMAP;
var
    x: integer;
begin
    result := TTipMapUtils.EmptyTipMap();
    for x := 0 to aPipDevice.TipCount - 1 do
    begin
        if aPipDevice.Tips[x].Wm_IsDirtyNeedingAutoWash(GetCurrentBatchID()) then
            TTipMapUtils.SelectTip(result, x);
    end;
end;
// 23.03.07 pk: If Process X made the tip dirty, PREVENT Process Y from washing it.

// --------------------------------------------------------------------------------------------------
class procedure TWashManager.WashAllDirtyTips();
// --------------------------------------------------------------------------------------------------
var
    x: integer;
    xUsedArm: IArmDevice;
    xTipsNeedingWash: TIPMAP;
begin
    // alle schmutzigen Tips an allen Armen waschen!!
    for x := 0 to gArmManager.Count - 1 do
    begin
        xUsedArm := gArmManager.Arms[x];
        if not Assigned(xUsedArm.PipDevice) then
            CONTINUE;
        xTipsNeedingWash := GetDirtyTipsNeedingAutoWash(xUsedArm.PipDevice);
        ExecuteWash(xUsedArm, xTipsNeedingWash);
    end;
end;

class procedure TWashManager.WashCombinedArm(aUsedArm: IArmDevice);
begin
    if (aUsedArm = nil) then
        EXIT;

    { TODO : combined arm }
    {
      // CombinedArm immer waschen! ( ConcurrentArm (--> SIAS) nicht waschen! )
      if (aUsedArm.CombinedArm <> nil) then
      ExecuteWash( aUsedArm.CombinedArm, false, aUsedArm.CombinedArm.DirtyTips );
    }
end;


end.
