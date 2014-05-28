{ --------------------------------------------------------------------------------------------------
  Ebene 5 (WinLissy)
  --------------------------------------------------------------------------------------------------
  Globale Variablen, Typen und Funktionen des Projekts SAMPLER/WINLISSY
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  dbMTools:
  15.05.98 wl                         neue Unit, die Methoden-spezizfische Datenbankmethoden enthält
  (zum Teil aus dbtools herausgenommen)
  15.05.98 wl  gmRunCreate            = CreateRunDB (aus dbMTools verschoben)
  gmMethodGet            = GetMethod (aus dbMTools verschoben)
  gmMethodDelete         = DeleteMethod (aus dbMTools verschoben)
  gmMethodPrint          kann jetzt auch vom Hauptfenster aus aufgerufen werden
  23.06.98 wl                         uses SamWash
  10.07.98 wl  gmRunShow              --> dbTools
  gmMethodGet            jetzt funktion: liefert MethodName als Rückgabewert
  gmMethodDelete         statt PMethodName wird MethodName mit angegeben
  gmMethodPrint            dito
  gmRunCreate              dito
  gmMethodStart          neue Methode zum Starten einer Methode ohne das Main-Fenster
  13.07.98 wl  gmMethodStart          Aufruf von gmRunStart mit Layout
  gmMethodCreate         entfernt --> main
  gmMethodEditShow         dito
  15.07.98 wl  gmRunCreate            dmSampler.RUN durch eigene RUNDB ersetzt
  gmMethodStart          Übergabe von RunModes; Bezug auf dmRack.RUN gelöscht
  gmMethodStart          Fehler bei Übergabe beseitigt
  29.07.98 wl  gmRunCreate            RUNDB.Free
  30.07.98 wl  gmRunCreate            LoadLayout mit RUNName , davor DeleteLayout
  16.09.98 mo  gmRunCreate            Bei einer Action wird auch die SystemFlüssigkeit in Run eingetragen
  16.09.98 wl  gmMethodGet            gelöscht (ersetzt durch gmGetDBResult in GetNames)
  gmNewGlobalMethod      wieder verworfen (besser: global.ChangeGlobalName)
  uses GetEntry,LayOForm entfernt
  gmRunCreate            WB.Load statt LoadLayout / WB.RackA statt RackA / WB.R statt R
  gmRunCreate            RunDB-Alias = 'SamRun', GenerateMultipipSeq mit 'SamRun'
  gmMethodStart          PDBPath --> dmRack.RunPath; Verwenden von global.ChangeGlobalName
  21.09.98 wl  gmRunCreate            result:=false (result auf jeden Fall definiert)
  24.09.98 wl  gmMethodStart          Aufruf von RunStart mit nil als HandlePtr
  25.09.98 wl  gmMethodPrint          MethEditFormOpen>0 durch MethEditForm<>nil ersetzt
  gmMethodStart          auch ScheduleEditForm wird vorher geschlossen
  28.09.98 wl  gmMethodStart          RunCreate-Aufruf erst in global.RunStart
  gmMethodRunCreate      wird von global.RunCreate aufgerufen (kürzer)
  29.09.98 wl  gmMethodStart          Aufruf von global.ChangeGlobalName mir Reload=false
  02.11.98 wl  gmMethodRunCreate      Bei Perivol oder Detection_Only in den Options darf Volumen 0 sein
  Variable Err entfernt (redundant mit ErrStr)
  04.11.98 wl  GetReplicates          aus postools: hier als lokale Funktion
  25.01.99 wl  gmMethodRunCreate      LiqPar.FieldByName('SampleAspMultipip') statt LiqParSampleA...
  02.02.99 wl  gmMethodRunCreate      Fehler beim Build werden wie´der angezeigt
  09.02.99 wl  gmMethodRunCreate      PLastTip ersetzt durch Sam.nTips; PFirstTip durch 1
  11.02.99 wl  gmMethodRunCreate      bei Move-Actions wird das Zielrack geprüft
  17.02.99 wl  gmMethodRunCreate      gVirtualRackMove wird gesetzt
  Fehlermeldungen (Build failed) mit Zeilenangabe
  25.02.99 wl  gmMethodRunCreate      Action 'Photo' überprüft Photometer-Stellplatz
  08.03.99 wl  gmMethodRunCreate      ResetAllRacks am Schluß, wenn MetheditForm nicht offen
  26.03.99 wl  gmMethodRunCreate      Verwendung von TMethodRec (Funktionalität unverändert)
  21.04.99 wl  gmMethodRunCreate,gmMethodDelete  neuer Aufruf von gmDeleteRunDB
  08.06.99 wl  gmMethodRunCreate      neuer Aufruf von gLiquids.GetDilNo
  29.07.99 wl                         uses ThrdMan statt BasicThr
  10.08.99 wz                         --> Strings in Ressourcen
  22.09.99 wl  gmMethodRunCreate      berücksichtigt 'MANUAL' als DilRack
  08.10.99 wl  gmMethodRunCreate      neue Action 'ChkBC', benötigt Sourcerack
  11.10.99 wl  gmMethodRunCreate      'ChkB' und 'ChkE' statt 'ChkBC'
  11.11.99 mo                          neu Aufruf von Parser.. Funktionen aus SamPars.dll
  24.11.99 mo  gmMethodRunCreate      Loop Funktionen überarbeietet
  29.11.99 wl  gmMethodStart          geänderter Funktionsaufruf von global.RunStart
  14.12.99 mo  gmMethodRunCreate      neu function GetRunSeqNumber() erzeugt automatische sequenz nummern 1,2,3...
  14.12.99 mo                         wenn gRunCreateSeqAuto=true
  15.12.99 mo  gmMethodRunCreate      Loop Funktion korrigiert
  12.01.00 wl  alle Funktionen        LogText() durch gmLogText() ersetzt (für Delphi 5)
  25.02.00 mo  gmMethodRunCreate      Sampler.nTips durch WB.Arm.NoOfTips ersetzt
  25.02.00 mo  gmMethodRunCreate      Multipipetting aktiviert
  20.04.00 mo  gmMethodRunCreate      mit Esc Button kann ein build gestoppt werden ( wgn Gefahr von enlosen Loops )
  20.04.00 mo  gmMethodRunCreate      WB.CheckSlot wird übergangen wenn im Sourcerack eine Variable eingetragen ist
  20.04.00 mo  gmMethodRunCreate      gmResMsgBox Parameter korrigiert
  09.05.00 wl                         Aufruf gmDeleteRun geändert
  11.05.00 wl  gmPrepareAction        Funktion aus Winlissy- und Sophas-Funktion zusammengesetzt
  11.05.00 wl  CreateSophRunDB,CreateReagentDB,CreateTransfer  übernommen aus Sophas: CreatRun.pas
  11.05.00 wl  gmRunDoneDlg,gmScriptStart          aus ScheUtil hierher
  26.05.00 wl                         uses geändert
  --------------------------------------------------------------------------------------------------
  LisGlobe.pas:
  05.06.00 wl                         Name geändert (dbMTools -> LisGlobe)
  05.06.00 wl  InitProject,SetDLLMenuList  aus globals.pas hierher
  05.06.00 wl  InitProject            Initialisierung von gSeqInc,gVirtualRackMove -> ScheUtil
  06.06.00 wl  gmPrepareAction        benutzt gmGetUserFunction; iRackName wird jetzt auch definiert
  07.06.00 wl  GetSourceRackName      vereinfacht
  07.06.00 wl  gmPrepareAction        Aufruf StartSamplerThread ohne '.DB'
  07.06.00 wl  CreateReagentDB        wenn Diluent=0 -> DILUENT = aktueller Port
  14.06.00 wl  PrepareAction          --> Schedule
  06.07.00 wl  CreateTransfer         Bestimmung von DestRack debuggt
  06.07.00 wl  CreateTransfer         ValvePos = Aktuelles Diluent, nicht mehr 0 wie in Sophas
  19.07.00 wl  ReadPosInfoDB          gekürzt und Fehler bei nicht vorhandenen Substanzen beseitigt
  21.07.00 wl  gmAskClose             TEditRunForm statt TSamRunForm, zusätzlicher SetBounds-Befehl
  03.08.00 wl                         --> String in Ressourcen (45...)
  29.08.00 wl  CreateSophRunDB        wenn Diluent<0 nimm aktuelle Systemflüssigkeit
  29.08.00 wl  CreateSophRunDB        DestPos = BlockStart-Position + PRODUCTNO
  05.09.00 wl  CreateSophRunDB,CreateReagentDB  gmGetPosFromSubstID (dbTools) ersetzt ReadPosinfoDB und GetSourceRackName
  05.09.00 wl  CreateSophRunDB        SetRange immer über alle Datensätze, dafür prüft die if-Abfrage jetzt
  Level, Rackgröße, Start- und Endposition
  02.12.00 tbh gmCherryPStart         neu: Start eines CherryPicking-Runs
  02.12.00 tbh gmRunStart             CherryPicking-Runs eingearbeitet und RackPlacement eingebaut
  02.12.00 tbh gmStartSamplerThread   um Aufruf eines CherryPicking-Threads ergänzt
  07.12.00 tbh gmRunStart             Aufruf von RackPlacement bei CherryPicking nur für SR und DR-Racks
  18.12.00 tbh InitWinLissyProjects   externe Dlls werden initialisiert
  18.12.00 tbh gmCherryPStart         wird kein SchedHandle übergeben
  22.12.00 tbh gmCherryPStart         Samplerthread-Aufruf geändert
  26.01.01 tbh gmRunStart             wird kein RackPlaceList mehr für SAMI aufgerufen
  30.01.01 tbh gmRunDoneDlg           Dialog kann auch ohne 'View'- und 'Delete'-Button geöffnet werden
  11.10.01 mo  gmScriptStart          TN1067 ifndef SIAS TExtScheduleForm...
  18.10.01 mo                         TN1067 Sophas Methoden nach SchedExt.pas
  27.11.01 tbh gmConnectExtDatabases  TN1051 neu: Funktion zum Verbinden mit CALLIWEIGH-Datenbank
  27.11.01 tbh InitProject            TN1051 Aufruf von gmConnectExtDatabases
  04.12.01 tbh gmConnectExtDatabases  TN1123 Abfrage auf FileExist entfernt
  03.05.02 tbh gmEndSlaveMode         TN1052.1 neu: beendet SlaveMode
  21.06.02 tbh InitProject            TN1234 MasterObject wird erzeugt
  21.06.02 pk  gmScriptStart          TN1052 Prevent Unsaved Script from being started in Normal mode
  21.06.02 pk  gmScriptStart          TN1052 If script is scheduled, don't check for duplicated step & stepcnt
  25.06.02 tbh uses                   TN1234 ObjMaster eingebunden
  12.07.02 pk  gmScriptStart          TN1052 unscheduled scripts cannot be run unless under Simulation mode
  02.09.02 pk  gmScriptStart          TN1052 Launch of new Thread Manager Session when gUseScheduler =2
  02.09.02 pk                         TN1052 all functions that launch a thread return the Thread reference instead of a boolean
  13.09.02 wl                         TN1283 Merge mit SIAS
  26.09.02 wl                         TN1283 Benutzt jetzt global_Alias & global_RunAlias
  10.10.02 wl                         TN1293.2 Ini Access - uses geändert
  18.10.02 wl  InitProject            TN1293.2 komplett au IniAccess umgestellt
  18.10.02 mo gmStartSamplerThread    TN1301 ifdef SIAS -> ifdef XAP
  18.10.02 mo InitProject             TN1301 ifdef SIAS -> ifdef XAP
  23.10.02 wl  InitProject            TN1293.1 neu: [MasterMode] Active-Eintrag muß existieren
  09.11.02 tbh gmScriptStart          TN1337 Prüfung auf Global.IsStartable nur wenn Scheduler aktiv
  14.11.02 wl  FormCreate             TN1328.1 uses AppSettings, ThrdManExt
  12.12.02 wl                         TN1345 uses geändert
  27.12.02 wl                         TN1293.5 uses und WinlissyIniAccess geändert
  11.02.03 wl                         TN1345 XAP-Compiler-Direktiven entfernt
  26.02.03 wl  gmStartSamplerThread   TN1334.3 vor dem Start des Run-Threads wird ein LogRun() abgesetzt
  27.02.03 wl  gmStartSamplerThread   TN1332.2 vor dem Start des (Method-)Run-Threads wird der ProcessLog geöffnet
  12.03.03 wl                         TN1293.5 uses info entfernt
  21.03.03 wl  gmStartSamplerThread   TN1332.2 vor dem Start des (Method-)Run-Threads wird das ZipArchiv geöffnet
  31.03.03 mo  gmMethodStart          TN1383.8 TGlobal.GlobalEvents Aufruf
  31.03.03 mo  gmRunDoneDlg           TN1383.8 TGlobal.GlobalEvents Aufruf
  31.03.03 mo  gmScriptStart          TN1383.8 TGlobal.GlobalEvents Aufruf
  03.04.03 tbh gmMethodStart          TN1383.6 / TN1383.7 Aufruf evStartMethodOrScript -> gmRunStart
  03.04.03 tbh gmRunStart             TN1383.6 / TN1383.7 evStartMethodOrScript wird ausgelöst
  03.04.03 tbh gmRunDoneDlg           TN1383.6 / TN1383.7 löst evApplicationRequest
  07.04.03 wl                         TN1383.8 GlobalEvents. statt global.GlobalEvents.
  10.04.03 wl  InitProject            TN1439.7 CFR21 Compliant -> No Master Mode
  16.04.03 wl  gmStartSamplerThread   TN1332.4 lädt das Layout nachdem ProcessLog gestartet wurde
  05.05.03 tbh gmScriptStart          TN1482.2 Skriptname muss global gesetzt sein, bevor auf Reschedulen geprüft wird
  21.05.03 tbh gmScriptStart          TN1487   Überarbeiten des Skripts wenn Skripts rackbasierend
  17.06.03 wl                         TN1501   uses SchedExt entfernt
  21.07.03 wl  App_GetCurrentScriptName  TN1515 neu: Übergibt aktuellen Skriptnamen an DLL
  21.07.03 wl  App_GetCurrentMethodName  TN1515 neu: Übergibt aktuellen Methodennamen an DLL
  21.07.03 wl  App_GetCurrentRunName     TN1515 neu: Übergibt aktuellen Runnamen an DLL
  21.07.03 wl  InitProject               TN1515 Überschreiben der Dummy-Funktionen aus SamCmd.pas
  21.07.03 wl  App_GetCurrent...         TN1515 jetzt mit Abfrage Assigned(global)
  24.07.03 wl  App_GetCurrentRun         TN1515 bekommt Run-Namen von TSamplerThrMan (Objects.pas)
  24.07.03 tbh App_GetCurrentRun         TN1515 Aufruf geändert
  29.07.03 wl  App_..-Methoden           TN1515   DLLInterface 4: komplett von PChar auf WideString umgestellt
  20.08.03 pk                            TN1555.0 new gmScriptStartSched, called from gmScriptStart
  03.09.03 wl                            TN1568   uses DLLLoading.pas
  30.10.03 wl  InitProject               TN1641   Einlesen der Actions und Options --> MethEdit
  30.10.03 wl  gmMethodPrint             TN1641   --> ScheUtil
  08.12.03 pk  gmRunStart                TN1697   Calls SendDDEMessage with enumerated type instead of a string
  09.12.03 pk  gmScriptStartSched        TN1697   Give error message if sophas licence is not available
  04.02.04 pk                            TN1719   functions return TThread instead of TMoveThread where possible
  04.02.04 pk  gmStartSamplerThread      TN1719   Creates TActionHandlerThread instead of TMoveThread
  23.02.04 pk                            TN1719   gGlobalEvents renamed to gSystemEvents
  23.02.04 pk  gmRunDoneDlg              TN1758   Call evEndMethodOrScript instead of evApplicationRequest
  26.02.04 wl                            TN1574   uses SamLiqH entfernt
  02.03.04 pk  gmStartSamplerThread      TN1719   new way of creating TLiveRunAction
  11.03.04 wl  gmMethodAction            TN1803   entfernt (wird nicht mehr benutzt)
  20.04.04 wl                            TN1788   uses SamThrd entfernt
  07.06.04 pk                            TN1964   App_GetCurrentScriptName, App_GetCurrentMethodName use c calling convention and return TInterfaceStr type
  28.06.04 pk  gmStartSamplerThread      TN2009.5 CP: Create TActionHandlerThread instead of TCPThread
  28.06.04 pk                            TN2009.7 gmScriptStartSched uses CreateVortRegThread from DevicesVortex
  29.06.04 pk                            TN2009.8 Uses ActionLow
  21.07.04 pk  gmStartSamplerThread      TN2049   Create a Real Time Run Session.
  06.01.05 wl  gmEndSlaveMode            TN2246.4  --> SlaveObj
  06.01.05 wl                            TN2246.4  alle Bezüge auf MainForm,MethEdit,ScheEdit durch gRunForm-Methoden ersetzt
  07.01.05 pk  gmStartSamplerThread      TN2281.0 if method has single priority and is not scheduled use the normal thread
  17.01.05 wl  gmAskClose                TN2246.4  benutzt gmCreateRunForm
  15.02.05 pk  gmSessionStart            TN2315    New : Starts a Session
  28.02.05 pk  gmRunCheck                TN2314.1  code extracted from gmRunStart
  28.02.05 pk  gmCreateSamplerThread     TN2314.1  code extracted from gmStartSamplerThread
  11.03.05 pk  gmRunDoneDlg              TN2339.2  call methdone via GUIManager
  16.03.05 pk  App_GetCurrentMethodName  TN2352.0  returns session name if session is currently loaded
  17.03.05 pk  gmCreateSamplerThread     TN2352.1  gmCreateSamplerThread,gmCreateScriptThread: Create thread via Thread Registry
  23.03.05 pk  gmCreateScriptThread      TN2314.1  --> ObjThreadFactory
  31.03.05 pk  gmStartSessionThread      TN2362    New : Create and start session thread
  31.03.05 pk  gmScriptCheck             TN2362    code from Schedule.pas
  31.03.05 pk  gmCreateSamplerThread     TN2362    Changed to become compatible with TExecHandler
  06.04.05 pk  gmStartSessionThread      TN2362    Create TSessionExecHandler
  19.04.05 pk  gmStartSessionThread      TN2362    Do not ReSchedule if not necessary
  22.04.05 pk  gmAskBeforeStart          TN2393    code exctacted from gmRunCheck
  22.04.05 pk  gmStartSessionThread      TN2393    call gmAskBeforeStart
  05.07.05 pk  gmCreateSamplerThread     TN2490    ExecHandler create requires NoMessages boolean
  11.07.05 wl  App_..-Funktionen         TN2498.1  --> ObjectsRun
  22.08.05 wl                            TN2558.8  uses ScheUtil entfernt, ScheUtil-Methoden werden mit TDMScript. oder global. aufgerufen
  08.09.05 wl                            TN2574    Umbau der ZARunner-Oberfläche
  07.10.05 wl  gmPaintPositionsBeforeStart  TN2637   zeichnet beim Restart die bereits pipettierten Positionen
  07.10.05 wl  gmCheckRun                TN2637    benutzt TRunDataAdaptorExt.CheckWorkupState statt gmCheckRunBeforeStart
  12.10.05 pk  gmStartSessionThread                pass gSchedShiftTime as argument
  11.07.05 pk                            TN2737    Dynamic Scheduling gmSessionQueryTime, etc
  08.11.05 wl  gmScriptStart,gmScriptStartSched,gmScriptCheck  TN2745 entfernt
  08.11.05 wl  gmStartSamplerThread      TN2745    kein SchedHandlePointer als Parameter
  14.11.05 pk  gmSessionQueryReschedTime TN2758    New : get the time at which a reschedule is possible
  24.11.05 pk                            TN2805    ObjSampl replaced by ZARunnerObjects
  24.11.05 pk  gmRunCheck                TN2805    error message for rwsNoTable changed
  10.04.06 pk  gmCreateSamplerThread     TN3031    InitAtBegin is now read from methodsettings
  11.04.06 pk  gmRunDoneDlg              TN2910    evApplicationRequest removed
  18.04.06 pk                            TN2958    CreateThread calls with description
  18.07.06 pk  gmSessionInvalidate       TN3209    New : reset status of Session
  07.12.06 pk  gmCreateSamplerThread     TN3455    freeonterminate=true, suspendsafe=true
  07.12.06 pk  gmStartSessionThread      TN3455    suspendsafe=true
  07.12.06 wl  InitProject               TN3409    geänderte CFR21-Abfrage
  07.12.06 wl                            TN3243    uses SamCmd entfernt
  09.11.07 pk  gmPaintPositionsBef...    TN3922    Dataset changed to DataProvider
  09.01.08 wl                            TN3972    MasterModus entfernt
  20.06.08 pk                            TN4139    WB global object replaced by LayoutManager
  03.07.08 wl                                         TN4157
  30.07.08 pk  gmRunCheck                TN4139    call LoadRun instead of Reload
  31.07.08 pk  gmRunCheck                TN3965    No calls RunCreate with IsDesignTimeBuild = true
  01.08.08 pk  gmRunCheck                TN4139    Massive changes
  08.09.08 pk                            TN4215    Massive Changes
  20.09.08 pk  gmStartMethodThread       TN4215    changes in RunNameGenerator
  25.09.08 pk                            TN4241    GUIManagerRun removed
  15.10.08 pk  gmMethodRunCheck          TN4258    InstBuild called with nil value for Parameter
  06.11.08 pk                            TN4279    various changes
  06.11.08 pk                            TN4280    various changes
  10.11.08 pk                            TN4280    warnings removed
  17.11.08 pk  StartMethodThread         TN4280    CreateProcess requires new SourceDataName param
  03.12.08 pk  gmMethodRunCheck          TN4279    All racks well placed message was not shown
  17.12.08 pk                            TN4372    Various changes for Simulation
  18.12.08 pk  gmMethodStart             TN4311    call GuiManager.StartNewRun
  08.01.09 pk  gmMethodRunCheck          TN4381    calls EditMethodParams
  09.01.09 pk  gmCompile                 TN4279    change cursor to hourglass
  17.02.09 pk                            TN4232    various changes
  20.02.09 pk  gmCheckIsRestart          TN4232   Changes for multithreaded trace
  24.02.09 pk  gmCheckIsRestart          TN4232   Various changes
  25.02.09 pk  gmCheckIsRestart          TN4232   show message instead of exception when prog counter file cannot be read
  31.03.09 pk  gmAskBeforeStart          TN4496   All racks placed correctly not show if simulation.
  07.04.09 pk                            TN4503   Various changes
  08.04.09 pk  gmAskBeforeStart          TN4512   Calls AskRunStart_PromptModal
  14.04.09 pk  gmAskBeforeStart          TN4512   Check value of CanSimulateInRealMode
  09.06.09 pk                            TN4585.1 references of GUIManagerSetup changed to GUIManagerRun
  10.06.09 pk                            TN4600   TCompilerMessageList changed to TCompilerResult
  06.07.09 pk                            TN4585.4 references to gGuiManager changed to TRunGUIManager
  03.08.09 wl                            TN4702   uses MethodBuildPage entfernt
  10.08.09 wl                            TN4702   Strings werden jetzt direkt geladen
  24.08.09 pk  gmMethodStart             TN4735.5 write log before method thread is started
  27.08.09 pk                            TN4753   uses changed
  28.09.09 pk  gmMethodCheck             TN4753   always call UnregisterCurrentLayout
  04.11.09 pk                            TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.12.09 pk                            TN4920   use RunFlow.AppSimulationMode instead of RunFlow.SimulationMode
  04.02.10 pk                            TN4972   Changes for Restart
  09.03.10 pk  gmMethodStart             TN4920   user RunFlow.AppSimluationMode instead of RunFlow.SimulationMode
  04.09.10 pk                            TN5042   functions that were in RunMain now accessible only via GUIManager
  13.04.10 wl                            TN5044   uses FileUtilities
  23.04.10 pk                            TN5072   Changes to TMultiListDataCacheIterator
  07.05.10 pk  gmMethodStart             TN5092   MethodParams is TKeyArgValueList instead of array
  19.05.10 pk  gmCheckIsRestart          TN5114   AskRestart_PromptModal
  28.05.10 wl  gmRunDoneDlg              TN5116   DeleteRunLayout ersetzt gmDeleteRunDB
  07.06.10 ts  gmAskBeforeStart          TN5124   AskForRackPlacement reimplemented
  07.06.10 pk  gmMethodCheck             TN5077   moved to TExecHandler
  08.06.10 pk  gmMethodStart             TN5077   Simulation Mode reimplemented
  11.06.10 pk  gmStartMethodThread       TN5138   New IsSystemProcess Param
  18.06.10 wl                            TN5116   Moss, Cherry Picking und Run Table-Funktionen entfernt
  26.10.10 pk  gmCheckIsRestart          TN5297   Changes for ActionData segment concept
  08.03.11 wl  gmCheckIsRestart          TN5494   Statt Fehlermeldung wird einfach eine Log-Zeile geschrieben
  20.10.11 wl  gmConnectExtDatabases     TN5723   --> ExtDllFunc
  20.10.11 wl  RunDoneDlg                TN5723   --> ExecHandler
  14.12.11 wl  TMethodStarter            TN5765   neue Klasse
  27.12.11 wl                            TN5768   an geändertes TRunGlobals angepasst
  20.04.12 wl                            TN5946   TSourceDataType entfernt
  07.08.12 wl  StartMethodThread         TN5946   InitAtMethodEnd wird hier nicht mehr bestimmt
  07.08.12 wl  MethodStart               TN5946   RunNameAllowed entfernt
  -------------------------------------------------------------------------------------------------- }

unit LisGlobe;


interface


uses
    MethodCompiledFile,
    ExecHandler,
    AppTypes;

type
    TMethodStarter = record
    private
        class function StartMethodThread(const aMethodName: string; aProcessDescription: string;
            const aCompiledFile: TMethodCompiledFile; const aRunModes: TSamplerThreadModes;
            const aIsSimulated: boolean; const aRestartParams: TExecHandlerRestartParams): boolean; static;
        class function Compile(const aMainMethodName: string; const aCompiledFile: TMethodCompiledFile)
            : boolean; static;
        class function CheckIsRestart(const aMethodName: string; const aCompiledFile: TMethodCompiledFile;
            out oRestartParams: TExecHandlerRestartParams): boolean; static;
    public
        class function MethodStart(const aMethodName: string; aRunModes: TSamplerThreadModes;
            const aIsSimulated: boolean): boolean; static;
    end;


implementation


uses
    Controls,
    SysUtils,
    ThreadClasses,
    GUIManager,
    RunGlobals,
    GUIManagerRun,
    MethodSettings,
    MethodCompile,
    LogManager,
    RunFlow,
    ProcessTraceDataCache,
    TraceManager,
    RunTraceManager,
    ProcessInfoDataCache,
    ActionIDDataCache,
    ActionDataCache,
    MemoryClasses,
    TraceThreadDetails;

// --------------------------------------------------------------------------------------------------
class function TMethodStarter.StartMethodThread(const aMethodName: string; aProcessDescription: string;
    const aCompiledFile: TMethodCompiledFile; const aRunModes: TSamplerThreadModes;
    const aIsSimulated: boolean; const aRestartParams: TExecHandlerRestartParams): boolean;
// --------------------------------------------------------------------------------------------------
var
    xExecHandler: TExecHandler;
    xInitAtBegin, xInitAtEnd, xNoMessages: boolean;
    xAddressSpace: TAddressSpace;
begin
    xInitAtBegin := mdInitFirst in aRunModes;
    xInitAtEnd := mdInitAtEnd in aRunModes;
    xNoMessages := mdNoMessages in aRunModes;

    if Pos('NOINIT', Uppercase(aMethodName)) > 0 then
        xInitAtBegin := false;

    if TMethodSettings.LoadInitOption(aMethodName) = mioNoInit then
    begin
        xInitAtBegin := false;
        xInitAtEnd := false;
    end;

    xAddressSpace := TMethodAddressSpace.Create();
    xAddressSpace.Load(aCompiledFile);

    xExecHandler := TExecHandler.Create(xInitAtBegin, xInitAtEnd, true, xNoMessages, aIsSimulated,
        aRestartParams);

    TThreadAPI.CreateProcess(aProcessDescription, xExecHandler, xAddressSpace, aMethodName, false,
        aIsSimulated);

    result := true;
end;

class function TMethodStarter.Compile(const aMainMethodName: string;
    const aCompiledFile: TMethodCompiledFile): boolean;
var
    xCompilerResult: TCompilerResult;
    xOldCursor: TCursor;
begin
    result := true;
    xOldCursor := gGUIManager.SetCursor(crHourGlass);
    try
        xCompilerResult := TCompilerResult.Create();
        try
            TMethodCompile.InstCompile(aMainMethodName, aCompiledFile.CompiledCode.Code,
                aCompiledFile.CompiledCode.EvalTable, xCompilerResult, true);
            if xCompilerResult.ErrorMessages.Count = 0 then
                EXIT;
            raise Exception.Create(Format('Submethod: %s' + #13#10 + 'Line: %d' + #13#10 + '%s',
                [xCompilerResult.ErrorMessages[0].MName, xCompilerResult.ErrorMessages[0].LineIndex + 1,
                xCompilerResult.ErrorMessages[0].Text]));
        finally
            xCompilerResult.Free;
        end;
    finally
        gGUIManager.SetCursor(xOldCursor)
    end;
end;

class function TMethodStarter.CheckIsRestart(const aMethodName: string;
    const aCompiledFile: TMethodCompiledFile; out oRestartParams: TExecHandlerRestartParams): boolean;
var
    xRelativeMemAddress: TRelativeMemAddress;
    xThreadDetails: TThreadDetailsItem;
    xMainThreadTraceName: string;
    xContinueAtID: TActionID;
    xProcessInfoDataCache: TProcessInfoDataCache;
    xTraceName: string;
    xProcessListDataCache: TProcessTraceListDataCache;
    xContinueAtSegmentIndex: integer;
begin
    result := true;
    oRestartParams.IsRestart := false;
    oRestartParams.TraceName := '';
    if not TRunTraceManager.Instance.IsEnabled then
        EXIT;

    xProcessListDataCache := TRunTraceManager.CreateProcesListDataCache();
    try
        xProcessListDataCache.Init();
        xProcessInfoDataCache := TRunTraceManager.CreateProcessInfoDataCacheBySourceDataName
            (xProcessListDataCache, aMethodName);
        if not Assigned(xProcessInfoDataCache) then
            EXIT;
        try
            xTraceName := xProcessInfoDataCache.ProcessInfoData.TraceName;

            xRelativeMemAddress := TRelativeMemAddress.Create();
            try
                xMainThreadTraceName := TRunTraceManager.GetMainThreadTraceNameForProcess(xTraceName);
                oRestartParams.IsRestart := xMainThreadTraceName <> '';
                if not oRestartParams.IsRestart then
                    EXIT;
                // if the trace stopped at the last line in the main method no restart

                xThreadDetails := TRunTraceManager.Instance.CreateThreadDetailsForThreadTrace(xTraceName,
                    xMainThreadTraceName);
                try
                    if not xThreadDetails.ProgramCounterDataCache.Read() then
                    begin
                        TLogManager.Instance.Log
                            ('Restart not possible: Program-Counter file could not be read.', true);
                        oRestartParams.IsRestart := false;
                        EXIT;
                    end;
                    xRelativeMemAddress.LabelName :=
                        xThreadDetails.ProgramCounterDataCache.ProgramCounterData.LabelName;
                    xRelativeMemAddress.RelativeAddress :=
                        xThreadDetails.ProgramCounterDataCache.ProgramCounterData.RelativeAddress;
                    oRestartParams.IsRestart := not xProcessInfoDataCache.ProcessInfoData.IsCompleted;
                    if not oRestartParams.IsRestart then
                        EXIT;

                    xThreadDetails.ActionListDataCache.Read;

                    // xContinueAtID = -1 : continue at next line
                    // xContinueAtID = 0 : new run

                    result := TGUIManagerRun.Instance.AskRestart_PromptModal(aMethodName,
                        xProcessInfoDataCache.ProcessInfoData.DateLastStopped,
                        xThreadDetails.ActionListDataCache, TRunTraceManager.Instance.RestartOnlyAtMarks,
                        xContinueAtID, xContinueAtSegmentIndex) = mrOK;
                    if not result then
                        EXIT;
                    oRestartParams.IsRestart := xContinueAtID <> 0;
                    oRestartParams.ContinueAtActionID := xContinueAtID;
                    oRestartParams.ContinueAtSegmentIndex := xContinueAtSegmentIndex;
                    if not oRestartParams.IsRestart then
                        EXIT;

                    // TRunTraceManager.Instance.RewindThreadTraceToActionID( xTraceName, xMainThreadTraceName, xContinueAtID );

                    // xProcessInfoDataCache.ChangeIsRestart( true );
                    oRestartParams.TraceName := xTraceName;

                finally
                    xThreadDetails.Free;
                end;
            finally
                xRelativeMemAddress.Free;
            end;

        finally
            if not oRestartParams.IsRestart then
            begin
                TRunTraceManager.RemoveProcessTrace(xProcessListDataCache, xTraceName);
            end;
            xProcessInfoDataCache.Free;
        end;
    finally
        xProcessListDataCache.Free;
    end;

end;

class function TMethodStarter.MethodStart(const aMethodName: string; aRunModes: TSamplerThreadModes;
    const aIsSimulated: boolean): boolean;
var
    xCompiledFile: TMethodCompiledFile;
    xIsSimulated: boolean;
    xLogText: string;
    xRestartParams: TExecHandlerRestartParams;
    xProcessDescription: string;
begin
    result := false;

    if (aMethodName <> TRunGlobals.Instance.PMethodName) then
    begin
        if not TRunGlobals.Instance.ChangeGlobalName(aMethodName, '', false, false) then
            EXIT;
    end;

    if (aMethodName = '') then
        EXIT;

    xCompiledFile := TMethodCompiledFile.Create();
    result := Compile(aMethodName, xCompiledFile);
    if not result then
        EXIT;

    result := CheckIsRestart(aMethodName, xCompiledFile, xRestartParams);
    if not result then
        EXIT;

    xProcessDescription := xRestartParams.TraceName;
    if not xRestartParams.IsRestart then
    begin
        xProcessDescription := TRunTraceManager.MakeTraceName(aMethodName);
    end;

    TRunTraceManager.Instance.InitProcessList();

    if not result then
        EXIT;

    xIsSimulated := gRunFlow.AppSimulationMode or aIsSimulated;
    TGUIManagerRun.Instance.SimulationMode_Enable(xIsSimulated);
    xLogText := Format('Starting Method %s', [aMethodName]);
    gLogManager.Log(xLogText, false);

    result := StartMethodThread(aMethodName, xProcessDescription, xCompiledFile, aRunModes, xIsSimulated,
        xRestartParams);
end;


end.
