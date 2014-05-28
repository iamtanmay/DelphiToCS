{ --------------------------------------------------------------------------------------------------
  Ebene 2 (Sam-Interface)
  --------------------------------------------------------------------------------------------------
  grundlegende Threads (Error-Handling) und der Thread-Manager zu deren Verwaltung
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  TEscThread:
  05.12.97 mo   Execute               Sleep(10) + Application.Processmessages eingebaut
  09.04.98 mo   Execute               neu : Messagebeep ausgeben wenn  GlobalErrBeep=true
  29.06.98 mo   Execute               neu : setzen des DDe Status  Aufruf von  MainForm.SetStatus;
  Jedes Projekt muss in Main.pas die Methode "MainForm.SetStatus" enthalten
  23.07.98 wl   Execute               MainForm.SetStatus --> global.SendDDERunStatus
  24.07.98 wl   PressStopButton       Aufruf: global.UserInterrupt
  11.09.98 wl  SendDDERunStatus       aus TSampler-Object / wird synchronisiert aufgerufen
  21.10.98 wl  SendDDERunStatus       ruft Funktion in global auf
  20.11.98 mo  SendDDERunStatus       zur virtual; gemacht
  TThreadManager:
  11.11.98 wl                                   neu: TThreadManager
  12.11.98 wl  StopSamplerThread      DDEStatus 'Ready' wird immer gesendet
  01.02.99 wl  ThreadIsStarted        StopButton kann sich von MainStopButton unterscheiden
  StartFlushThread       neu: für TFlushThread
  TSamErrThread:
  23.06.98 wl                           erste lauffähige Version
  23.04.99 wl  Execute                  gActualEvent wird um die Fehlermeldung ergänzt
  --------------------------------------------------------------------------------------------------
  Datum    op  Objekt          Methode          Änderung / Neuerung
  -------- --  --------------  -------------    ----------------------------------------------------
  19.05.99 wl  TThreadManager  ThreadIsStarted/Stopped   benutzt TBasicThread
  TThreadManager  ThreadIsStopped  wenn [mdNoDDEReady] in RunModes, sende kein 'Ready'
  04.06.99 mo  SendDDEStatus   Application.ProcessMessages + HandleMessage eingefügt
  mo  TEscThread.Execute; Application.BringToFront;  Application.RestoreTopMosts; --> Fenster nach vorne
  09.06.99 mo  SendDDEStatus,SendDDERunStatus   Application.ProcessMessages + HandleMessage wieder entfernt
  29.07.99 wl  TSamErrThread                    aus SamErThr-Unit
  TMoveThread                      neu: Vorfahr von TBasicThread (für Setup)
  18.08.99 wz                                   --> String in Ressourcen
  26.08.99 wl  TEscThread                       TBeepOptions zum Verändern des GlobalErrBeep
  TEscThread      Execute          erweitert für DLL-Funktion (Beep über Opto-Karte)
  TThreadManager  Create           Lesen der BeepOptions aus sampler.ini
  06.01.00 mo  TThreadManager                   Neu: FDDEStatusItem2 für AdvancedDdeStatus
  TThreadManager.SendDDEStatus     Wenn Ddetext ein '_' am Anfang enthält -> DdeAdvancedStatus
  11.01.00 wl  TEscThread.Execute               RestoreTopMosts wieder aktiviert, kommt nur noch beim 10. Piep
  12.01.00 wl  alle Funktionen                  LogText() durch gmLogText() ersetzt (für Delphi 5)
  17.01.00 wl                                   mdExportAfterRun abgeschafft
  27.01.00 wl  TEscThread.Execute               bei ConfirmIgnore=1 kein RestoreTopMosts
  14.02.00 wl  TMoveThread.SyncMessageBox,ResSyncBox,ResFSyncBox  (aus TBasicThread)
  07.04.00 wl  TThreadManager.UserInterrupt     Methode ist virtual
  28.04.00 wl  TMoveThread.PutHandlerTool,GetHandlerTool,ToolError,ResetError      (aus BasicThr)
  TMoveThread.Execute,InitAll,JustStarted,MethodWorkedUp,EndSamplerT. (aus BasicThr)
  TThreadManager                   neu: ParallelThrCnt (für parallele Sophas-Threads)
  TMoveThread.EnterProcTime        Zeit von Start bis Ende eines Threads wird in Proctime.DB eingetragen (aus Sophas)
  11.05.00 wl  TMoveThread                      SchedHPtr^.InUse:=false wird erst später gesetzt
  22.05.00 wl  TMoveThread.JustStarted/EndSamplerThread  schaltet FWB.MenuActive während des Threads aus
  25.05.00 wl  TMoveThread.Create               jetzt public!
  25.05.00 wl  TMoveThread                      FWB darf auch nil sein
  26.05.00 wl                         uses geändert
  30.05.00 wl                                   TSamplerThreadModes --> SamGlobe
  TMoveThread.InitAll              DropAllDTips --> BasicLTh
  06.06.00 wl  TMoveThread.MethodWorkedUp       bei gLicence.Sophas wird immer ein Eintrag in Proctime.DB gemacht
  06.06.00 wl  TMoveThread.EnterProcTime        leichte Korrekturen
  20.06.00 wl  TThreadManager.ApplicationEnd    jetzt mit _ShutDownSampler
  18.07.00 wl  TMoveThread.EndSamplerThread     Bei Schedule-Threads verschwindet der Stop-Button nicht
  27.07.00 wl  TMoveThread.ResetError           Init nur wenn GlobalErrorPtr^=0 / YesNo statt OKCancel
  31.07.00 wl  TSamErrThread.ShowSamErr         Anzeige der Fehlercodes --> SamErr
  09.08.00 wl  TMoveThread.InitAll              erweitert um Balance, Vortexer, Stirrer, BC-Reader, VarRedi
  10.08.00 wl                                   --> String in Ressourcen
  17.08.00 wl  TMoveThread.InitAll              VarRedi- und Balance-Init zurück nach --> BasicLTh
  17.08.00 wl  TMoveThread.Execute              Nach dem Abarbeiten Tool zurückbringen
  18.08.00 wl  TThreadManager.ToolInHandler     Abfrage, ob sich ein Tool im Handler befindet
  29.08.00 wl  TMoveThread.GetHandlerTool       wenn gToolZTavel existiert, wird dieser Wert gesetzt
  29.08.00 wl  TMoveThread.PutHandlerTool       Sam.hZTavel wird zurückgesetzt
  30.08.00 wl  TMoveThread.ResetError           ResetAllModules wird in jedem Fall ausgeführt
  30.08.00 wl  TMoveThread.ResetError           Beim Greifer öffnen werden auch die Sam-Werte zurückgesetzt
  30.08.00 wl  TMoveThread.GetHandlerTool       gToolZTravel muß kleiner als Sam.hZMax sein!
  30.08.00 wl  TMoveThread.PutHandlerTool       Die Sam-werte werden erstzurückgesetzt, wenn PutPlate ausgeführt wurde
  06.09.00 wl  TMoveThread                      FStopbutton jetzt protected statt private
  13.03.01 tbh TVortReqThread                   neu: Thread zur Abfrage von Thermostaten und Vortexern
  30.03.01 tbh TVortReqThread                   neu: FDisplayList speichert Vortexerdaten für synchron. Anzeige
  30.03.01 tbh TVortReqThread.UpdateDisplay     neu: aktualisiert Vortexerinfofenster
  30.03.01 tbh TVortReqThread.UpdateDisplayValue  neu: aktualisiert Wert in FDisplayList
  11.05.01 tbh TVortReqThread                   Vortexerabfragen nur wenn nicht terminated
  11.05.01 tbh TVortReqThread                   Anzeigefarbe wird wieder auf schwarz gesetzt
  14.05.01 tbh TVortReqThread.FRequestFinished  neu: speichert ob letzte Abfrage beendet wurde
  01.06.01 tbh TVortReqThread.CreateDeviceLists Aufruf von GetDevicePartList aktualisiert
  01.06.01 tbh TVortReqThread.CreateDeviceLists Hinzunahme von IKA Vortexern
  06.06.01 tbh TMoveThread.GetHandlerTool       um Greiferdefinitionen erweitert
  08.06.01 mo  TMoveThread.EnterProcTime        in Proctime.db wird immer eingetragen
  28.06.01 mo  TMoveThread.PutHandlerTool       neu: BringBackVOffset zum Wegstellen eines Tools
  28.07.01 tbh TMoveThread.ToolError            Ressource angepasst
  28.07.01 tbh TMoveThread.ResetError           jetzt auch ResetExternModules
  16.08.01 mo  TMoveThread.GetHandlerTool       TN1015 Varispan öffnen beim 1. Anfahren auch wenn kein echtes Tool im Handler
  27.08.01 tbh TVortReqThread.Execute           TN1020 Timerkomponente statt Systemzeitauslesen
  27.08.01 tbh TVortReqThread.VortTimerEvent    TN1020 neu: kapselt Abfragefunktion (Timerfunktion)
  29.08.01 tbh TVortReqThread.Execute           TN1020 Timerkomponente am Ende freigegeben
  05.09.01 tbh TVortReqThread.FStopped          TN1031 neu: zeigt ob Thread gestopped oder nicht
  05.09.01 tbh TVortReqThread.ReadVortInfo      TN1031 kein Auslesen mehr wenn Thread gestoppt
  05.09.01 tbh TThreadManager.StopVortReqThread TN1031 neu: Funktion zum Stoppen des Vortexer-Abfrage-Threads
  05.09.01 tbh TThreadManager.ResumeVortReqThread  TN1031 neu: Funktion zum Fortsetzen des Vortexer-Abfrage-Threads
  12.09.01 tbh TVortReqThread.EventRunning      TN1040 neu: setzt ob ein TimerEvent läuft (immer nur eins)
  12.09.01 tbh TThreadManager.StopVortReqThread TN1040 keine Abfrage mehr auf FRequestFinished (Deadlock)
  12.09.01 tbh TThreadManager.ResumeVortReqThread  TN1040 FRequestFinished wird true gesetzt
  13.09.01 tbh TVortReqThread.Execute           TN1040 FRequestFinished und FStopped werden initialisiert
  13.09.01 tbh TVortReqThread.VortTimerEvent    TN1040 Abfrage auf GlobalErrorPtr jetzt erst im TimerEvent
  08.10.01 mo  TMoveThread                      TN1067 ifndef SIAS Handler Tool Funktionen
  11.10.01 mo  TMoveThread                      TN1067 TWorkbench => TWorkbenchExt
  17.10.01 mo                                   TN1067 Änderungen für SIAS
  21.10.01 mo  TMoveThread.InitAll              TN1002 neu FExtTubeBCReader.init
  26.04.02 tbh TEscThread.Execute               TN1052.1 ist SlaveModus aktiv wird Statusfile erzeugt
  26.04.02 tbh TThreadManager.UserInterrupt     TN1052.1 SlaveModus wird beendet
  03.05.02 tbh TThreadManager.UserInterrupt     TN1052.1 Aufruf zum Beenden des SlaveModus verändert
  16.05.02 tbh TEscThread.Execute               TN1052.1 es wird kein Statusfile mehr erzeugt
  04.06.02 tbh uses                             TN1225 SlaveMode mit Kompileranweisungen für Layouter deaktiviert
  04.06.02 tbh TThreadManager.UserInterrupt     TN1225 SlaveMode mit Kompileranweisungen für Layouter deaktiviert
  24.06.02 pk  TMoveThread.JustStarted          TN1052 Changes for Scheduler
  02.09.02 pk  TMoveThread.Execute              TN1052 Signalling functions for new thread manager added.
  02.09.02 pk  TMoveThread.EnterProcTime        TN1052 Increase Counter field, and calculate average using Counter Field.
  02.09.02 pk  TMoveThread                      TN1052 Now inherits from TSchedActionThread
  02.09.02 pk  TMoveThread.Create               TN1052 Thread created in suspended mode. It resumes automatically only when ResumeOnCreate=true
  02.09.02 pk  TThreadManagerExt1               TN1052 New class inherits from TThreadManager
  05.09.02 wl  uses                             TN1282 LAYOUTER: TMoveThread inherits from TThread, not TSchedActionThread
  09.09.02 mo  TMoveThread.Create               TN1282 Database session name without priority for WinLissy
  13.09.02 wl                                   TN1283 Merge mit SIAS
  26.09.02 wl                                   TN1283 Benutzt jetzt global_Alias & global_RunAlias
  10.10.02 wl                                   TN1293.2 Ini Access - uses geändert
  16.10.02 mo PutHandlerTool,GetHandlerTool     TN1301 ifdef SIAS ->XAP
  18.10.02 wl                                   TN1293.1 benutzt TAppSettings
  21.10.02 wl TThreadManager.Create             TN1293.1 benutzt TWinlissyIniAccess
  14.11.02 wl  TThreadManagerExt1               TN1328.1 --> ThrdManExt
  12.12.02 wl  Tool-Funktionen                  TN1345 Robot.SetHandlerValues statt direktes schreiben im Sam-Record
  12.12.02 wl  TMoveThread                      TN1345 TWorkbenchExt => TWorkbench
  27.12.02 wl                                   TN1293.5 uses und WinlissyIniAccess geändert
  28.01.03 wl                                   TN1293.5 geänderte gModules-Methodenaufrufe
  11.02.03 wl                                   TN1345 XAP-Compiler-Direktiven entfernt
  20.02.03 wl  TSamErrThread                    TN1334.3 --> SamErr
  20.02.03 wl  TThreadManager.UserInterrupt     TN1334.3 --> LayGlobe (weil nur noch im Layouter benutzt)
  05.03.03 mo  TMoveThread.Execute              TN1438 Exception Behandlung geändert
  07.04.03 wl  TThreadManager.evCloseErrorMessage  TN1383.8 Für Layouter als Dummy-Funktion
  07.04.03 wl  TThreadManager.evShowErrorMessage  TN1383.8 Für Layouter als Dummy-Funktion
  07.04.03 wl  TMoveThread.Create               TN1439.4 das System Data Password wird auch für diese Session gesetzt
  07.04.03 wl                                   TN1345   uses VortexerRequestThread
  07.04.03 wl  TEscThread                       TN1345   --> EscapeThread
  07.04.03 wl  TVortReqThread                   TN1345   --> VortexerRequestThread
  13.06.03 wl  TMoveThread.InitAll              TN1501.1 BC-Reader-Init --> ObjModulA
  09.07.03 wl  GetHandlerTool, GetRack          TN1539   vernünftige Fehlermeldung wenn Tool nicht da
  17.07.03 wl  ResetError                       TN1539   zeigt zum Schluß den Grund, warum der Run abgebrochen wurde
  30.07.03 wl  InitAll                          TN1501.2 Relay-Methoden in gCommManager statt in gModules
  03.08.03 wl                                   TN1536   uses DevicesVortex
  03.08.03 wl  ThrMan.VortReqThread             TN1536   --> DevicesVortex
  21.08.03 pk  ThrMan.                          TN1555.0 TerminateVortReqThread, CreateVortReqThread New. Used by Schedule unit.
  18.09.03 wl  TMoveThread.InitAll              TN1564   Fehler bei InitMotors abschalten --> RoboticInterface
  22.09.03 pk  TMoveThread.InitAll              TN1556.4 Calls DoJustAfterInitMotors just after initializing the motors
  22.09.03 pk  TMoveThread.IsSafe               TN1556.4 Check for global error
  01.10.03 pk  TMoveThread.InitAll              TN1608   Removed call to gCommManager.Relays_Reset because it was already called in ResetAllModules
  01.10.03 pk  TMoveThread.InitAll              TN1608   Make call to ResetDecappers
  03.11.03 wl                                   TN1493.6 SendCmd: string statt pchar
  06.11.03 pk  TMoveThread.ResetError           TN1654   Set the global error without displaying a message in the status box
  01.12.03 wl  TMoveThread.InitAll              TN1686   Reihenfolge geändert: erst Init Motors, dann Decapper auf Grundposition
  08.12.03 pk  ThrMan                           TN1696   New functions for easier access to AnimationThread and DDE Messages
  08.12.03 pk  TMoveThread.Create               TN1696   Resume thread only if CreateThreadsSuspended is false
  19.12.03 wl                                   TN1672   _Move und _MoveHandler ersetzt durch gGrpArm-Methoden
  04.02.04 pk  TSamplerThrMan                   TN1719 ThreadIsStarted/Stopped changed to accept TThread instead of TRunThread
  04.02.04 pk  TAnimateThread                   TN1719 Changed to support new way of Thread Synchronization
  04.02.04 pk                                   TN1719 Tool functions moved to Toolhandling
  11.02.04 pk  InitStandard                     TN1744 ResetModules called at begining.  InitBCReaders called before InitAllModules
  12.02.04 pk  InitStandard                     TN1745 Most of the function was moved to ObjModulA
  19.02.04 pk  SendDDEStatus, SendDDERunStatus  TN1753 ExceptionHanlding for when DDE object has been destroyed but pointer not set to nil
  26.02.04 pk  TThreadManager.ShutDownDDE()     TN1753 sets DDE objects to nil
  05.04.04 wl  TMoveThread.InitStandard         TN1788   gGrpArm.ResetToolData statt gmInitTool
  19.04.04 wl  TMoveThread.Get/PutHandlerTool   TN1788   entfernt
  22.04.04 wl  TThreadManager.SendDDEStatus     TN1788.8 DDEStatus wird nicht mehr im LogDisplay angezeigt
  08.06.04 wl  TThreadManager.ApplicationEnd    TN1963   Robot.ShutDownSampler statt _ShutDownSampler
  24.06.04 wl                                   TN2007   uses Variants (nur Delphi 6 und 7)
  28.06.04 pk  TMoveThread                      TN2009.7 Removed
  01.07.04 wl  TThreadManager.ToolInHandler     TN1963   entfernt (hat hier nichts zu suchen)
  21.07.04 pk                                   TN2049   uses SchedActionThread removed
  06.01.05 wl  evShow/CloseErrorMessage         TN2246.4 mit direkten Aufruf von gSystemEvents-Methode
  06.01.05 wl  ThreadIsStarted,ThreadIsStopped  TN2246.4 Aufruf der neuen gSystemEvents-Methoden ThreadStarted,ThreadStopped
  09.03.05 pk  SendDDEStatus                    TN2339.1 Application.Messagebox changed to gmSyncMessageBox
  11.03.05 pk  TAnimateThread.VCL               TN2339.2 changed from proc to func
  18.03.05 pk  TAnimateThread                   TN2352.1 changed from thread to TExecutable object
  18.03.05 pk  fThreadRegistry                  TN2352.1 New : Manage all TThread threads started by the application
  31.03.05 pk  ThreadIsStarted, ThreadIsStopped TN2362   made compatible with TExecHandler
  06.04.05 pk  OnThreadException                TN2352.1 Use RegistryItem description as messagebox caption
  19.04.05 pk  fExecHandler                     TN2392   fSamplerThread changed to fExecHandler
  03.06.05 wl  TThreadManager.ApplicationEnd    TN2436    ShutDownSampler umbenannt
  10.10.05 wl  TThreadManager.SendDDEStatus     TN2637.2  benutzt Konstanten aus AppTypes
  07.11.05 pk  TThreadManager.RequestStop       TN2735    Tells the EscapeThread to do a UserInterrupt
  07.11.05 pk  TThreadManager.ChangeSuspendable TN2736    New : prevent a thread from being suspended
  08.11.05 wl                                   TN2745   uses DatabaseConstants entfernt
  22.12.05 pk                                   TN2875   Various changes for Interrupt handling
  06.04.06 pk  CreateThreadWithRegistryItem     TN3001   New
  29.08.06 thr ThreadManager                    TN3264   Neu - Vortexer thread
  10.10.06 thr ThreadManager                    TN3351   Vortinfo durch VortinfoBase ersetzt
  31.10.06 pk  RequestInterrupt                 TN3991   New waitlock parameter
  09.11.06 pk  RequestInterrupt                 TN3399   Interrupts can now have arguements and results
  13.11.06 pk  HandleInterrupt                  TN3401   Call handleinterrupt function instead of doing nothing
  21.11.06 pk  LogCreateThread                  TN3424   New
  21.11.06 pk  CreateInterruptMonitor           TN3424   now uses RequestInterrupt function of Threadman to register interrupt
  23.11.06 pk  DoOnError                        TN3424   moved to public
  01.12.06 pk  TInterruptManager                TN3441   New. From functions in EscapeThread
  07.12.06 pk                                   TN3445   TInterruptManager moved to InterruptManager.pas
  07.12.06 pk  TThreadRegistry                  TN3455   moved to threadclasses
  26.01.07 pk  ApplicationEnd                   TN3525.4 removed
  26.01.07 pk  Destroy                          TN3525.4 New : end escapemanager
  22.02.07 pk  TAnimatedImage                   TN3583   New: Terminate
  22.02.07 pk  TThreadManager.Destroy           TN3583   Try to free all objects that were created in Create
  03.07.08 wl                                   TN4157
  21.07.08 pk  LogCreateThread                  TN3864   reactivated
  02.09.08 pk                                   TN4215   various changes
  06.11.08 pk  TThreadingProcessHandlerSetup    TN4280   new
  10.11.08 pk  TThreadingProcessHandlerSetup    TN4280   New: DoOnLogDetermineContextDescription
  17.11.08 wl  FMainStopBtn                     TN4312   TBitBtn statt TButton
  19.11.08 pk  TThreadingProcessHandlerSetup    TN4280   code moved back to TThreadManagerSetup
  24.11.08 pk  TAnimatedImage                   TN4280   now inherits from MessagableExecutable
  02.12.08 pk  TSafeAppCloseManager             TN4335   New
  06.01.09 pk  TSafeModuleCommunicationManager  TN4372   New - Used for functions which must be executed in a thread that never dies
  16.01.09 pk  HandleMessageGeneral             TN4372   now sets ContractorThreadID
  07.04.09 pk                                   TN4503   SystemEvents.ThreadStarted/Stopped removed
  09.06.09 pk  RequestInterruptStart/Finish     TN4585.1 New
  10.08.09 wl                                   TN4702   Strings werden jetzt direkt geladen
  28.09.09 pk  Create                           TN4753   parameters changed
  09.02.10 pk  Suspend/ResumeVortexerDisplay    TN4973   code commented out for now
  15.02.10 pk  Destroy                          TN4985   Reset
  18.02.10 pk  Destroy                          TN4985.1 call terminate for each executable
  18.02.10 pk  TSafeModuleCommunicationManager  TN4985.1 RequestGeneral: now also possible without waiting
  09.03.10 pk                                   TN5015   ThreadmanagerSetup is no longer inherited from ThreadManager
  23.04.10 pk                                   TN5015   remove SamDDE
  17.05.10 wl                                   TN5111   Änderungen TLogManager.Instance
  11.06.10 pk  LaunchServiceProcess             TN5138   New IsSystemProcess Param
  27.07.10 wl                                   TN5123   ShowRunTable entfernt
  19.10.10 pk                                   TN5305   changes needed for CoreClient/Server
  14.09.11 wl  TThreadAPICallbackImplementor    TN5672   --> ThreadAPICallbackImplementor
  15.09.11 wl                                   TN5694   DDE-Methoden wieder aktiviert
  19.01.12 wl  SuspendExec                      TN5759.2 neuer Parameter MaxWaitTime, Default = INFINITE
  19.01.12 wl  RequestInterrupt                 TN5759.2  neue property IgnoreInterruptStartError
  20.04.12 wl  RequestStopRunInterrupt          TN5858   nur noch abstract
  04.05.12 wl                                   TN5858   Animation endgültig endfernt
  09.05.12 wl  fUserLogRun                      TN5858   entfernt
  29.05.12 wl  DetermineContextDescription      TN5904   entfernt
  27.03.13 wl                                   TN6045   uses geändert
  30.07.13 wl  InterruptBeepStart,-End          TN6160   kapseln TEscapeManager.GlobalErrBeep
  15.08.13 wl                                   TN6223   uses geändert
  22.08.13 wl  Suspend/ResumeVortexerDisplay    TN6231   VortexerDisplay-Funktionen entfernt
  -------------------------------------------------------------------------------------------------- }

unit ThrdMan;


interface


uses
    Windows,
    SysUtils,
    Controls,
    DdeMan,
    Classes,
    ExtCtrls,
    Buttons,
    GeneralTypes,
    ThreadClasses,
    ThreadUtils,
    AppTypes,
    EscapeManager,
    InterruptManager,
    InterruptMonitor,
    ExecHandlerMinimal,
    ClockClass,
    LockHandle,
    MessagableExecutable,
    MessageQueue,
    Executable,
    ThreadingProcessHandler;

type
    TSafeAppCloseMessageInfo = class(TMessageInfo)
    private
        fMaxWaitTime: cardinal;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aMaxWaitTime: cardinal);
        property MaxWaitTime: cardinal read fMaxWaitTime;
    end;

    TSafeAppCloseManager = class(TMessagableExecutable)
    private
        fOnSafeAppClose: TNotifyEvent;
        procedure HandleMessageAppClose(const aMessage: TSafeAppCloseMessageInfo);
    protected
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); override;
        procedure SetOriginalMask(); override;
    public
        procedure RequestSafeAppClose(const aMaxWaitTime: cardinal);
        property OnSafeAppClose: TNotifyEvent read fOnSafeAppClose write fOnSafeAppClose;
    end;

    TModuleCommunicationGeneralMessageInfo = class; // forward declaration
    TModuleCommunicationGeneralMessageCallback = procedure(const aMessage
        : TModuleCommunicationGeneralMessageInfo) of object;

    TModuleCommunicationGeneralMessageInfo = class(TMessageInfo)
    protected
        fContractorThreadID: cardinal;
        fWait: boolean;
        fCallback: TModuleCommunicationGeneralMessageCallback;
        function GetMessageID(): integer; override;
    public
        constructor Create(const aCallback: TModuleCommunicationGeneralMessageCallback;
            const aContractorThreadID: cardinal);
        property ContractorThreadID: cardinal read fContractorThreadID;
        property Callback: TModuleCommunicationGeneralMessageCallback read fCallback;
        property Wait: boolean read fWait write fWait;
    end;

    TSafeModuleCommunicationManager = class(TMessagableExecutable)
    private
        procedure HandleMessageGeneral(const aMessage: TModuleCommunicationGeneralMessageInfo);
    protected
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); override;
        procedure SetOriginalMask(); override;
    public
        procedure RequestGeneral(const aMessage: TModuleCommunicationGeneralMessageInfo);
    end;

    TServicesProcessCreateThreadMessageInfo = class(TMessageInfo)
    private
        fExecutable: TMessagableExecutable;
        fDescription: string;
    protected
        function GetMessageID(): integer; override;
    public
        constructor Create(const aExecutable: TMessagableExecutable; const aDescription: string);
        property Executable: TMessagableExecutable read fExecutable;
        property Description: string read fDescription;
    end;

    IServicesProcessHandler = interface(IProcessHandler)
        ['{CB279DC9-C442-42CC-B1E0-50123BCA16E5}']
        procedure CreateThread(const aExecutable: TMessagableExecutable; const aDescription: string);
    end;

    TServicesProcessHandler = class(TProcessHandler, IServicesProcessHandler, IProcessHandler)
    protected
        procedure Initialize; override;
        procedure Finalize; override;
        procedure MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem); override;
        function GetAdditionalMask(): TArray<integer>; override;
    public
        procedure CreateThread(const aExecutable: TMessagableExecutable; const aDescription: string);
    end;

    // -------------------------------------------------------------------------------- Thread-Manager
    TThreadManagerSetup = class
    private
        class var uInstance: TThreadManagerSetup;
        function GetEscapeManager: TEscapeManager;
        function GetInterruptManager: TInterruptManager;
        procedure DoHandleInterrupt(aObject: TObject);
        procedure DoInterruptManagerStatusChanged(aSender: TObject; aStatus: TInterruptManagerStatus);
        procedure SetMainStopBtn(const aValue: TBitBtn);
    protected
        // ------------------------------------------------------- Grundeinstellungen
        fMainStopBtn: TBitBtn; // STOP-Button
        FStopBtnPressed: Boolean;

        // --------------------------------------------------- Alle laufenden Threads
        FDDEStatusItem: TDdeServerItem;
        FDDEStatusItem2: TDdeServerItem;
        fClock: TClock;

        fServicesProcessHandler: IServicesProcessHandler;

        fInterruptManager: TInterruptManager;
        fEscManager: TEscapeManager;
        fSafeAppCloseManager: TSafeAppCloseManager;
        fSafeModuleCommunicationManager: TSafeModuleCommunicationManager;

        procedure SuspendExec(const aMaxWaitTime: cardinal = INFINITE);
        procedure ResumeExec();

        procedure Log(const aText: string); // override;

        procedure CreateServicesProcessHandler(); virtual;

        procedure Finalize;
        procedure Initialize; virtual;
        procedure HandleInterrupt(aInterruptItem: TInterruptRequest); virtual;

        function DoInterruptStart(const aInterruptorThreadID: cardinal; const aInterruptText: string;
            const aInterruptMaxWaitTime: cardinal): boolean; virtual;
        function DoInterruptFinish(const aInterruptorThreadID: cardinal; const aInterruptText: string)
            : boolean; virtual;

        property InterruptManager: TInterruptManager read GetInterruptManager;
        property EscManager: TEscapeManager read GetEscapeManager;
    public
        constructor Create();
        destructor Destroy(); override;
        class procedure SetInstance(const aValue: TThreadManagerSetup);
        class procedure DestroyInstance();

        class property Instance: TThreadManagerSetup read uInstance;
        // ----------------------------------------------------- Read-Only Properties
        property StopBtnPressed: boolean read FStopBtnPressed;
        property MainStopBtn: TBitBtn read fMainStopBtn write SetMainStopBtn;
        property Clock: TClock read fClock;

        // DDE functions
        procedure SendDDEStatus(DDEText: string); overload;
        procedure SendDDEStatus(aDDEStatus: TDDEStatus); overload;
        procedure StartupDDE(aDDEStatusItem: TDdeServerItem; aDDEStatusItem2: TDdeServerItem);
        procedure ShutDownDDE();
        procedure SendDDERunStatus;

        function SamThreadRunning(ShowMsg: Boolean): Boolean;
        procedure ThreadIsStarted(); virtual;
        procedure ThreadIsStopped(); virtual;

        // Interrupt
        procedure UserInterrupt(); virtual;
        procedure RequestStopRunInterrupt(const aInterruptText: string; aWaitTillHandled: boolean);
            virtual; abstract;
        function RequestInterrupt(const aInterruptText: string; aInterruptRoutine: TInterruptRoutineEvent;
            aIRArgs: TInterruptRoutineArgs; var vIRResult: TInterruptRoutineResult; aWaitTillHandled: boolean;
            aWaitTillHandledLock: TLock; const aIgnoreInterruptStartError: boolean = false): boolean;

        function RequestInterruptStart(const aInterruptorThreadID: cardinal;
            const aInterruptText: string): boolean;
        function RequestInterruptFinish(const aInterruptorThreadID: cardinal;
            const aInterruptText: string): boolean;

        function CreateInterruptMonitor(const aInterruptText: string; aPollDelay: integer;
            aInterruptRoutine: TInterruptRoutineEvent; aOnCheck: TInterruptCheckEvent): TInterruptMonitor;

        // App Close
        procedure RequestSafeAppClose(const aMaxWaitTime: cardinal);
        procedure SetOnSafeAppClose(const aValue: TNotifyEvent);

        // Safe Module Communication
        procedure RequestSafeModuleCommunication(const aMessage: TModuleCommunicationGeneralMessageInfo);

        function ElapsedClockTime(): integer;

        procedure DoOnError(aSender: TObject);

        procedure LaunchServiceProcess(); // override;
        procedure CreateServiceThread(const aExecutable: TMessagableExecutable; const aDescription: string);

        procedure InterruptBeepStart();
        procedure InterruptBeepEnd();
    end;

    TInterruptMonitorExt = class(TInterruptMonitor)
    protected
        procedure StartMonitorThread(); override;
    end;

function ThrMan(): TThreadManagerSetup; // 21.08.08 pk depracated global variable should be removed


implementation


uses
    Variants,
    CommonTypes,
    AppSettings,
    ErrorManager,
    LogManager,
    ThreadAPI,
    GUIManager,
    ErrorMessageFactory,
    RunFlow;

const
    cLogEndOfSection =
        '---------------------------------------------------------------------------------------------';

    cMessageAppCloseManagerClose = 9001;
    cMessageModuleCommunicationGeneral = 9001;
    cMessageServicesProcessCreateThread = 9001;

    { TSafeAppCloseMessageInfo }

constructor TSafeAppCloseMessageInfo.Create(const aMaxWaitTime: cardinal);
begin
    inherited Create();
    fMaxWaitTime := aMaxWaitTime;
end;

function TSafeAppCloseMessageInfo.GetMessageID: integer;
begin
    result := cMessageAppCloseManagerClose;
end;

{ TSafeAppCloseManager }

procedure TSafeAppCloseManager.HandleMessageAppClose(const aMessage: TSafeAppCloseMessageInfo);
begin
    if not Assigned(fOnSafeAppClose) then
        EXIT;

    // Request a "Safe" close by setting a flag
    gRunFlow.IsSafeExitRequested := true;

    // open all locks
    TThreadAPI.SetError();

    // Make sure the ErrorBox is closed
    TErrorMessageFactory.ErrBoxClose();

    // If a Sampler thread is running, wait for it to finish
    TThreadAPI.WaitTillAllProcessesExited(aMessage.MaxWaitTime);

    fOnSafeAppClose(self);

end;

procedure TSafeAppCloseManager.SetOriginalMask();
begin
    fMessageQueue.SetMask([INT_MESSAGE_EXECUTABLE_TERMINATE, INT_MESSAGE_EXECUTABLE_PAUSE,
        cMessageAppCloseManagerClose]);
end;

procedure TSafeAppCloseManager.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
    inherited;
    if aMessage.MessageInfo is TSafeAppCloseMessageInfo then
    begin
        HandleMessageAppClose(aMessage.MessageInfo as TSafeAppCloseMessageInfo);
    end;
end;

procedure TSafeAppCloseManager.RequestSafeAppClose(const aMaxWaitTime: cardinal);
begin
    if not Assigned(fOnSafeAppClose) then
        EXIT;
    self.RegisterMessageAndLeave(TSafeAppCloseMessageInfo.Create(aMaxWaitTime));
end;

{ TModuleCommunicationGeneralMessageInfo }

constructor TModuleCommunicationGeneralMessageInfo.Create(const aCallback
    : TModuleCommunicationGeneralMessageCallback; const aContractorThreadID: cardinal);
begin
    inherited Create();
    fCallback := aCallback;
    fContractorThreadID := aContractorThreadID;
    fWait := true;
end;

function TModuleCommunicationGeneralMessageInfo.GetMessageID: integer;
begin
    result := cMessageModuleCommunicationGeneral;
end;

{ TSafeModuleCommunicationManager }

procedure TSafeModuleCommunicationManager.HandleMessageGeneral
    (const aMessage: TModuleCommunicationGeneralMessageInfo);
begin
    TThreadAPI.SetCurrentContractorThreadID(aMessage.ContractorThreadID);
    try
        aMessage.Callback(aMessage);
    finally
        TThreadAPI.ResetCurrentContractorThreadID();
    end;
end;

procedure TSafeModuleCommunicationManager.SetOriginalMask();
begin
    fMessageQueue.SetMask([INT_MESSAGE_EXECUTABLE_TERMINATE, INT_MESSAGE_EXECUTABLE_PAUSE,
        cMessageModuleCommunicationGeneral]);
end;

procedure TSafeModuleCommunicationManager.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
    inherited;
    if aMessage.MessageInfo is TModuleCommunicationGeneralMessageInfo then
    begin
        HandleMessageGeneral(aMessage.MessageInfo as TModuleCommunicationGeneralMessageInfo);
    end;
end;

procedure TSafeModuleCommunicationManager.RequestGeneral(const aMessage
    : TModuleCommunicationGeneralMessageInfo);
begin

    // This function will make sure that aMessage.Callback is called in the context of a thread that will stay alive until
    // the Application is closed.  For example, this could be used for creating/destroying COM/ActiveX objects
    if aMessage.Wait then
        self.RegisterMessageAndWait(aMessage, true, INFINITE)
    else
        self.RegisterMessageAndLeave(aMessage);
end;

{ TServicesProcessHandler }

procedure TServicesProcessHandler.Initialize;
begin
    inherited;
    TThreadManagerSetup.Instance.Initialize();
end;

procedure HandleMessageServicesProcessCreateThread(const aMessage: TServicesProcessCreateThreadMessageInfo);
begin
    TThreadAPI.CreateThread(aMessage.Description, false, true, false, aMessage.Executable);
end;

procedure TServicesProcessHandler.MessageProc(var vAccept: boolean; aMessage: TMessageQueueItem);
begin
    inherited;
    if aMessage.MessageInfo is TServicesProcessCreateThreadMessageInfo then
    begin
        HandleMessageServicesProcessCreateThread
            (aMessage.MessageInfo as TServicesProcessCreateThreadMessageInfo);
    end
end;

procedure TServicesProcessHandler.Finalize;
begin
    TThreadManagerSetup.Instance.Finalize();
    inherited;
end;

function TServicesProcessHandler.GetAdditionalMask(): TArray<integer>;
begin
    result := fMessageQueue.MaskArgsToMaskArray([cMessageServicesProcessCreateThread])
end;

procedure TServicesProcessHandler.CreateThread(const aExecutable: TMessagableExecutable;
    const aDescription: string);
begin
    RegisterMessageAndWait(TServicesProcessCreateThreadMessageInfo.Create(aExecutable, aDescription), false,
        INFINITE);
end;

{ TThreadManagerSetup }

constructor TThreadManagerSetup.Create();
var
    xIniAccess: IWinlissyIniAccess;
    iBeep: TBeepOptions;
begin
    inherited Create;

    xIniAccess := gCommonDll.CreateAppIni;

    // ----------------------------------------------------------------------------------- [Beeper]
    iBeep.DLLName := xIniAccess.ReadString('Beep', 'DLLName');
    iBeep.DLLFunc := xIniAccess.ReadString('Beep', 'DLLFunction');
    iBeep.DLLOffFunc := xIniAccess.ReadString('Beep', 'DLLOffFunction');
    iBeep.Time := xIniAccess.ReadInteger('Beep', 'Time');
    iBeep.Sound := xIniAccess.ReadInteger('Beep', 'Sound'); // = Windows: Kritischer Abbruch
    if (iBeep.Time < 5) then
        iBeep.Time := 5;

    FStopBtnPressed := false;

    // --------------------------------------------------------- Interrupt Manager
    fInterruptManager := TInterruptManager.Create;
    fInterruptManager.OnInterruptStart := DoInterruptStart;
    fInterruptManager.OnInterruptFinish := DoInterruptFinish;
    fInterruptManager.OnHandleInterrupt := DoHandleInterrupt;
    fInterruptManager.OnStatusChanged := DoInterruptManagerStatusChanged;

    // --------------------------------------------------------- Escape Manager
    fEscManager := TEscapeManager.Create(iBeep);

    fSafeAppCloseManager := TSafeAppCloseManager.Create();

    fSafeModuleCommunicationManager := TSafeModuleCommunicationManager.Create();

    gErrorManager.OnError := DoOnError;
end;

destructor TThreadManagerSetup.Destroy();
begin
    gErrorManager.OnError := nil;
    fSafeModuleCommunicationManager.Terminate();
    fSafeModuleCommunicationManager := nil;

    fSafeAppCloseManager.Terminate();
    fSafeAppCloseManager := nil;

    fEscManager.Terminate();
    fEscManager := nil;

    fInterruptManager.Terminate();
    fInterruptManager := nil;

    inherited;
end;

class procedure TThreadManagerSetup.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

procedure TThreadManagerSetup.InterruptBeepEnd;
begin
    fEscManager.InterruptBeepEnd;
end;

procedure TThreadManagerSetup.InterruptBeepStart;
begin
    fEscManager.InterruptBeepStart;
end;

class procedure TThreadManagerSetup.SetInstance(const aValue: TThreadManagerSetup);
begin
    uInstance := aValue;
end;

function ThrMan(): TThreadManagerSetup;
begin
    result := TThreadManagerSetup.Instance;
end;

procedure TThreadManagerSetup.HandleInterrupt(aInterruptItem: TInterruptRequest);
begin
    aInterruptItem.HandleInterrupt();
end;

procedure TThreadManagerSetup.DoHandleInterrupt(aObject: TObject);
begin
    HandleInterrupt(aObject as TInterruptRequest);
end;

procedure TThreadManagerSetup.DoInterruptManagerStatusChanged(aSender: TObject;
    aStatus: TInterruptManagerStatus);
begin
    if aStatus = imsActivated then
    begin
        TThreadAPI.SuspendAllProcesses();
    end
    else if aStatus = imsIdle then
    begin
        TThreadAPI.ResumeAllProcesses();
    end;
end;

procedure TThreadManagerSetup.Initialize();

begin
    TThreadAPI.CreateThread('Interrupt Manager', false, true, false, fInterruptManager);
    fInterruptManager.Unpause();

    TThreadAPI.CreateThread('Escape Manager', false, true, false, fEscManager);

    TThreadAPI.CreateThread('Safe Application Close Manager', false, true, false, fSafeAppCloseManager);
    fSafeAppCloseManager.Unpause();

    TThreadAPI.CreateThread('Safe Module Communication Manager', false, true, false,
        fSafeModuleCommunicationManager);
    fSafeModuleCommunicationManager.Unpause();
end;

procedure TThreadManagerSetup.Finalize();
begin

end;

procedure TThreadManagerSetup.DoOnError(aSender: TObject);
begin
    { TODO -oPK : general register error message for process main threads }
    {
      if not Assigned( fExecHandler ) then EXIT;
      fExecHandler.ErrorSet();
    }

    TThreadAPI.SetError();
end;

function TThreadManagerSetup.ElapsedClockTime(): integer;
begin
    result := -1;
    if (not Assigned(ThrMan.Clock)) or (ThrMan.Clock.ClockState = csStopped) then
        EXIT;
    result := self.fClock.ElapsedTime();
end;

procedure TThreadManagerSetup.ThreadIsStarted();
begin
    TLogManager.Instance.Log('Main thread started', true);
    TLogManager.Instance.Log(cLogEndOfSection, true);

    SendDDEStatus(ddeRunning);
end;

procedure TThreadManagerSetup.ThreadIsStopped();
begin
    // //------------------------------------------------------- DDE-Status = 'Ready'
    SendDDEStatus(ddeReady);

    TLogManager.Instance.Log('Main thread ended', true);
    TLogManager.Instance.Log(cLogEndOfSection, true);
end;

// ------------------------------------------------------------------------------
function TThreadManagerSetup.SamThreadRunning(ShowMsg: Boolean): boolean;
// ------------------------------------------------------------------------------
begin
    // more than just main process running?
    result := not TThreadAPI.WaitTillAllProcessesExited(0);

    if (result and ShowMsg) then
    begin
        MessageBeep(MB_ICONHAND);
        gGUIManager.MessageBox(TLanguageString.Read('One or more processes are active',
            'Ein oder mehrere Prozesse sind aktiv'), TLanguageString.Read('Processes Active',
            'Prozesse Aktiv'), MB_ICONSTOP);
    end;
end;

procedure TThreadManagerSetup.StartupDDE(aDDEStatusItem: TDdeServerItem; aDDEStatusItem2: TDdeServerItem);
begin
    FDDEStatusItem := aDDEStatusItem;
    FDDEStatusItem2 := aDDEStatusItem2;
end;

procedure TThreadManagerSetup.ShutDownDDE();
begin
    FDDEStatusItem := nil;
    FDDEStatusItem2 := nil;
end;

procedure TThreadManagerSetup.SendDDEStatus(aDDEStatus: TDDEStatus);
var
    xText: string;
begin
    case aDDEStatus of
        ddeRunning:
            xText := STR_DDE_STATUS_RUNNING;
        ddeStopButton:
            xText := STR_DDE_STATUS_STOPPED;
        ddeReady:
            xText := STR_DDE_STATUS_READY;
        ddeError:
            xText := STR_DDE_STATUS_ERROR;
        ddeMethodDone:
            xText := STR_DDE_STATUS_DONE;
    end;
    SendDDEStatus(xText);
end;

procedure TThreadManagerSetup.SendDDEStatus(DDEText: string);
begin
    try
        if (FDDEStatusItem = nil) then
            Exit;
        if pos('_', DDEText) = 1 then
            FDDEStatusItem2.Text := DDEText
        else
            FDDEStatusItem.Text := DDEText;
        TLogManager.Instance.Log('SendDDEStatus :' + DDEText, false);
    except
        on E: Exception do
        begin
            gGUIManager.MessageBox('TThreadManagerSetup.SendDDEStatus -> ' + E.Message, 'Error',
                MB_ICONSTOP + MB_OK);
        end;
    end;
end;

procedure TThreadManagerSetup.SendDDERunStatus;
begin
    if (FDDEStatusItem = nil) then
        Exit;
    if (FDDEStatusItem.Text = '') then
        Exit;
    { if ErrMessageActive then
      FDDEStatusItem.Text := STR_DDE_STATUS_ERROR; // Errormeldung an DDE Client
      if (FDDEStatusItem.Text = STR_DDE_STATUS_ERROR) and (not ErrMessageActive) then
      begin }
    FDDEStatusItem.Text := STR_DDE_STATUS_RUNNING;
    // Application.HandleMessage; Application.ProcessMessages;
    // end;
end;

function TThreadManagerSetup.RequestInterruptStart(const aInterruptorThreadID: cardinal;
    const aInterruptText: string): boolean;
begin
    result := self.InterruptManager.RequestInterruptStart(aInterruptorThreadID, aInterruptText);
end;

function TThreadManagerSetup.RequestInterruptFinish(const aInterruptorThreadID: cardinal;
    const aInterruptText: string): boolean;
begin
    result := self.InterruptManager.RequestInterruptFinish(aInterruptorThreadID, aInterruptText);
end;

function TThreadManagerSetup.RequestInterrupt(const aInterruptText: string;
    aInterruptRoutine: TInterruptRoutineEvent; aIRArgs: TInterruptRoutineArgs;
    var vIRResult: TInterruptRoutineResult; aWaitTillHandled: boolean; aWaitTillHandledLock: TLock;
    const aIgnoreInterruptStartError: boolean = false): boolean;
begin
    result := self.InterruptManager.RequestInterrupt(TThreadAPI.GetCurrentThreadID(), aInterruptText,
        aInterruptRoutine, aIRArgs, vIRResult, aWaitTillHandled, aWaitTillHandledLock,
        aIgnoreInterruptStartError);
    // wait here until interrupt is handled
    TThreadAPI.WaitIfInterrupt();
end;

procedure TThreadManagerSetup.UserInterrupt();
begin
    RequestStopRunInterrupt('STOP button pressed', false);
end;

procedure TThreadManagerSetup.SuspendExec(const aMaxWaitTime: cardinal);
begin
    TThreadAPI.WaitTillAllProcessesSuspended(aMaxWaitTime);
end;

procedure TThreadManagerSetup.ResumeExec();
begin
end;

function TThreadManagerSetup.CreateInterruptMonitor(const aInterruptText: string; aPollDelay: integer;
    aInterruptRoutine: TInterruptRoutineEvent; aOnCheck: TInterruptCheckEvent): TInterruptMonitor;
begin
    result := TInterruptMonitorExt.Create(aPollDelay);
    result.InterruptText := aInterruptText;
    result.OnInterruptRoutine := aInterruptRoutine;
    result.OnInterruptCheck := aOnCheck;
    result.OnInterruptRequest := RequestInterrupt;
    result.StartMonitor();
end;

procedure TInterruptMonitorExt.StartMonitorThread();
begin
    TThreadAPI.CreateThread('Interrupt Monitor', false, true, false, self);
end;

procedure TThreadManagerSetup.LaunchServiceProcess;
begin
    self.CreateServicesProcessHandler();
    TThreadAPI.CreateProcess('Services', fServicesProcessHandler, nil, '', true, false);
end;

procedure TThreadManagerSetup.Log(const aText: string);
begin
    TLogManager.Instance.Log(aText, false);
end;

procedure TThreadManagerSetup.SetMainStopBtn(const aValue: TBitBtn);
begin
    fMainStopBtn := aValue;
    if (fMainStopBtn <> nil) then
        fMainStopBtn.Visible := false;
end;

function TThreadManagerSetup.GetEscapeManager: TEscapeManager;
begin
    result := self.fEscManager;
end;

function TThreadManagerSetup.GetInterruptManager: TInterruptManager;
begin
    result := self.fInterruptManager;

end;

procedure TThreadManagerSetup.CreateServicesProcessHandler();
begin
    fServicesProcessHandler := TServicesProcessHandler.Create();
end;

procedure TThreadManagerSetup.CreateServiceThread(const aExecutable: TMessagableExecutable;
    const aDescription: string);
begin
    self.fServicesProcessHandler.CreateThread(aExecutable, aDescription);
end;

procedure TThreadManagerSetup.RequestSafeAppClose(const aMaxWaitTime: cardinal);
begin
    fSafeAppCloseManager.RequestSafeAppClose(aMaxWaitTime);
end;

procedure TThreadManagerSetup.SetOnSafeAppClose(const aValue: TNotifyEvent);
begin
    fSafeAppCloseManager.OnSafeAppClose := aValue;
end;

procedure TThreadManagerSetup.RequestSafeModuleCommunication(const aMessage
    : TModuleCommunicationGeneralMessageInfo);
begin
    fSafeModuleCommunicationManager.RequestGeneral(aMessage);
end;

function TThreadManagerSetup.DoInterruptFinish(const aInterruptorThreadID: cardinal;
    const aInterruptText: string): boolean;
begin
    result := true;
end;

function TThreadManagerSetup.DoInterruptStart(const aInterruptorThreadID: cardinal;
    const aInterruptText: string; const aInterruptMaxWaitTime: cardinal): boolean;
begin
    result := true;
end;

{ TServicesProcessCreateThreadMessageInfo }

constructor TServicesProcessCreateThreadMessageInfo.Create(const aExecutable: TMessagableExecutable;
    const aDescription: string);
begin
    inherited Create();
    fExecutable := aExecutable;
    fDescription := aDescription;
end;

function TServicesProcessCreateThreadMessageInfo.GetMessageID: integer;
begin
    result := cMessageServicesProcessCreateThread;
end;


end.
