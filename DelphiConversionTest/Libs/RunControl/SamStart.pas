{ --------------------------------------------------------------------------------------------------
  Ebene 2 (Sam-Interface)
  --------------------------------------------------------------------------------------------------
  Funktionen, die beim Starten aller Sampler-Applikationen benötigt werden
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  17.11.98 wl                         neue Unit zur Trennung SamGlobe von SamIntf
  30.11.98 wl  ReadBCIni              liest BCRackDelLastPos zum Löschen der letzten Ziffer(n)
  08.12.98 wl  InitSamGlobals         Titel, Version, Build, Sampler.dll --> LogFile
  ReadMachineIniFile     'PlateCheck': 'PutPlate','GetPlate' wird eingelesen
  02.02.99 wl  InitSamGlobals         gNewBCReading:=false
  04.02.99 wl  ReadMachineIniFile     BorderWidthLeft,Top,Right,Bottom statt X,Y
  11.02.99 wl  InitSamGlobals         Interflag entfernt
  15.02.99 wl  ReadMachineIniFile     Liest für externes Barcodelesen: ReadE_DLLName,-Func,-Param
  25.02.99 wl  InitSamGlobals         gPhotometer... wird bestimmt
  08.03.99 mo  InitSamGlobals         neu: gMultiCheckXYPos lesen
  09.03.99 mo                         neu: gSwitchPortOnRestart lesen
  17.03.99 wl  ReadSamplerIniFile     neu: gConfirmIgnore lesen
  23.04.99 wl  MoveErrorFile          Archivierte Error-Files heißen Error001.dat, Error002.dat ...
  ArchiveFiles           (aus SamIntf)
  03.05.99 wl  ReadSamplerIniFile     liest Applikations-Sprache gLanguage (Englisch=0,Deutsch=1,...)
  alle Funktionen        Ressourcen aus Samintf.res werden verwendet
  19.05.99 wl  MoveErrorFile,ArchiveFiles  ForceDirectories statt MkDirEx (für ".\Logfiles")
  30.06.99 mo  ReadSamplerIniFile     neu: gFlushVolAfterInit + gAirVolAfterInit +gRetractStepsAfterInit lesen
  13.07.99 wl  ReadSamplerIniFile     Corridor_XMoveOffset (aus globals)
  29.07.99 wl  ReadSamplerIniFile     Cycles für Flush-Fenster werden gelesen
  SaveSamGlobals         Cycles für Flush-Fenster werden gespeichert
  18.08.99 wz                         --> String in Ressourcen
  23.08.99 wl  ReadMachineIniFile     neu in LiquidSpeeds: Asp-/DispSpeedCalcFactor
  24.08.99 wl  ReadMachineIniFile     WorkspaceX,WorkspaceY,PipettXZero,PipettYZero entfernt (nutzlos)
  31.08.99 wl  alle Funktionen        Ressourcen und Nummern z.T. korrigiert
  ReadMachineIniFile     TipTypes werden gelesen (wenn vorhanden)
  ReadDeviceIniFile      Devices, Deviceparts werden aus 'device.ini' gelesen
  07.09.99 wl  ReadMachineIniFile     gTipTypes einlesen korrigiert
  09.09.99 wl  ReadMachineIniFile     DTOffset_mm wird mit eingelesen
  10.09.99 wl  InitSamGlobals         gLicence.Redi:=false
  20.09.99 wl  InitRelayBoard         Conrad-Relay Board initialisieren (wenn vorhanden)
  21.09.99 wl  ReadDeviceIniFile      Festlegung des Device-Typs; bei dptShaker RackName, Row und Col
  ReadDeviceIniFile      Standard-Werte: SwitchOn=1, SwitchOff=0
  23.09.99 wl  ReadMachineIniFile     MaxTipVolume/DispTipOffset entfernt
  28.09.99 mo  InitRelayBoard         Abbruch bei Fehler
  29.09.99 wl  ReadMachineIniFile     liest Abstands-Werte für Handler-Tip
  02.11.99 wl  ReadMachineIniFile     Daten für festen Tube-Greifer (gTubeToolData) einlesen
  05.11.99 wl  ReadSamplerIniFile     liest gBalance-Variablen ein
  16.11.99 wl  ReadSamplerIniFile     liest gImport-Variablen
  SaveSamGlobals         speichert File- und Tabellen-Namen in Sampler.ini
  29.11.99 wl  ReadMachineIniFile     hZTravelTubes wird gelesen
  17.12.99 wl  SaveSamGlobals         BayerAXC.DLL wird geschlossen (wenn aufgerufen)
  12.01.00 wl  alle Funktionen        LogText() durch gmLogText() ersetzt (für Delphi 5)
  InitSamGlobals         DecodeTime und Setzen von CurrentHour entfernt
  13.01.00 wl  ReadSamplerIniFile     liest 'Display','24h_Mode'
  17.01.00 wl  ReadSamplerIniFile     liest [ExportRunFile]-Daten (vorher im TExportRun-Objekt)
  11.02.00 wl  ReadSamplerIniFile     liest 'Display','ChangePosinfo'
  11.02.00 wl  ReadDeviceIniFile      Motor.. als Devicepart für Var-Redi-Motor
  11.02.00 wl  ReadMachineIniFile     neu MinVolume als 7.Angabe bei Tip Types
  23.02.00 mo  ReadMachineIniFile     neu: gDiTiIgnoreRestTips
  07.03.00 wl  ReadDeviceIniFile      RackName, Row und Column können explizit in der ini angegeben werden
  22.03.00 mo  ReadSamplerIniFile     neu gWashDryDelay lesen
  23.03.00 wl  ReadMachineIniFile     Standardwert für REDI-Tips: MinVolume = MaxVolume
  05.04.00 wl  ReadSamplerIniFile     [Calli] und [Balance] (Left/Top) werden gelesen
  12.04.00 wl  InitRelayBoard         TCompulab.Create (aus Sophas)
  ReadDeviceIniFile      bei 'COOL' ist das Device ein Thermostat
  13.04.00 wl  ReadDeviceIniFile      --> ObjGlobe
  15.04.00 wl  InitRelayBoard         --> ObjGlobe
  28.04.00 wl                          uses ObjModul statt SamGlobe
  05.05.00 wl  ReadSamplerIniFile     [Pipetting]-Values einlesen (...AfterInit)  --> SamWash
  17.05.00 wl  SaveSamGlobals         bei CRSAX.DLL wird CRS_Kill ausgeführt
  26.05.00 wl  InitSamGlobals         in 2 Funktionen zerlegt: wird von InitSamModule aufgerufen
  31.05.00 wl  ReadMachineIniFile     gConradBoardPort wird jetzt hier gelesen (nicht in TModules)
  31.05.00 wl  ReadSamplerIniFile     [6WayValve]/[Pipetting]-Daten zum Teil aus SamWash hierher
  19.06.00 wl  ReadSamplerIniFile     neu: [Pipetting] WashOnlyUsedTips
  05.07.00 mo  ReadSamplerIniFile     neu: [Pipetting] DispSubmergeActive
  17.07.00 wl  InitSamGlobals2        gmMoveErrorFile und gmArchiveFiles müssen extra aufgerufen werden
  19.07.00 wl  ReadSamplerIniFile     neu: [ScriptReset] AskForKeepContents
  16.08.00 mo  ReadMachineIniFile     neu: [Options] WaitAfterPickSys
  17.08.00 wl  ReadMachineIniFile     Tool-Name (für Handler-Tip) als 8.Angabe bei Tip Types
  17.08.00 wl  ReadMachineIniFile     Stanadrwerte DeviceName,ToolName=''
  29.08.00 wl  ReadSamplerIniFile     [ScriptReset] AskForKeepContents ist Default = false
  29.08.00 wl  ReadMachineIniFile     neu: [Module] ToolZTravel & RediZRetrSpeed
  06.09.00 wl  ReadSamplerIniFile     neu: Abfragezeit für ReadTemp, setzt Infofenster an/aus
  10.10.00 tbh ReadSamplerIniFile     neu: [Pipetting] SaveSolvent setzt unnütze Waschschritte (2. Kanal) aus
  20.10.00 tbh ReadSamplerIniFile     neu: [Liquids] gSysLiqMinVol setzt Grenze wenn Volumenanzeige gelb zeigt
  21.11.00 tbh ReadMachineIniFile     neu: gTubeToolData.SaveMoveOffset und ...SaveMoveSpeed
  22.11.00 tbh ReadSamplerIniFile     neu: [DatabaseToolDll] Name= ausgelesen für gDbToolDllName
  23.11.00 tbh ReadSamplerIniFile     neu: [Balance] MoveTubeToBalance/MoveTubeFromBalance
  28.11.00 tbh ReadSamplerIniFile     neu: [Balance] MoveTubesAfterRestart setzt gMoveTubesAfterRestart
  02.12.00 tbh ReadSamplerIniFile     neu: [Diplay] AskForRackPlacement setzt gAskForRackPlacement
  02.12.00 tbh ReadSamplerIniFile     neu: [Device] Name setzt gSamDeviceName (weißt Namen des Samplers zu)
  08.12.00 tbh InitSamGlobals2        externe Dlls werden initialisiert
  18.12.00 tbh InitSamGlobals2        Initialisierung externer Dlls -> globals.pas
  22.12.00 tbh ReadSamplerIniFile     neu: [DatabaseToolDll] RequestWriteBackData=1 Nachfrage ob Rückschreiben in DB=aktiv
  19.01.01 mo  ReadMachineIniFile     nie mehr Ärger beim auslesen der XYZStepsPerMM wegen Dezimaltrennzeichen
  19.01.01 mo  ReadSamplerIniFile     Exportverzeichnis wird angelegt wenn es nicht existiert
  13.03.01 tbh ReadSamplerIniFile     neu: [Schedule] VortRequestTime= Abfragezeit bei mehreren Zugriffen auf Vortexer
  13.03.01 tbh ReadSamplerIniFile     neu: [Schedule] TangoRequestTime= Abfragezeit bei mehreren Zugriffen auf Tango
  13.03.01 tbh ReadSamplerIniFile     neu: [Schedule] LogVortInfo= setzt ob Temperatur und Speed von Vortexern gelogt werden
  13.03.01 tbh ReadSamplerIniFile     neu: [Schedule] FixAfterVortexing= setzt ob nach Schütteln Vortexer neu fixieren
  22.03.01 mo  ReadSamplerIniFile     neu: SAMI Anzeige Racks vor Barcodelesen
  18.05.01 tbh ReadSamplerIniFile     neu: [ScriptReset] ResetScriptAfterDDEStart setzt ScriptReset nach DDE-Start
  21.05.01 tbh ReadSamplerIniFile     neu: [Balance] CloseDoorAfterInit schließt Waagentür nach Init
  06.06.01 tbh ReadSamplerIniFile     neu: [Pipetting] EnableVolumeControl aktiviert Volumenkontrolle
  06.06.01 tbh ReadMachineIniFile     gTipTypes lesen jetzt auch XOffset, YOffset eines TipTypes ein
  08.06.01 tbh ReadMachineIniFile     XOffset, YOffset an Ende der TipTypes angehängt
  28.06.01 mo  ReadMachineIniFile     neu: [TakeTubes] ToolBringBackVOffset zum Wegstellen eines Tools
  17.07.01 tbh ReadSamplerIniFile     neu: [Calli] DestOnBalance definiert ob Zieltube für Calli auf Waage steht
  17.07.01 tbh ReadSamplerIniFile     neu: [Calli] UseRackIDPos definiert ob statt Barcode RackID_Pos eingetragen wird
  18.07.01 tbh ReadSamplerIniFile     neu: [Calli] gTargetApproach setzt Teiler aus Volumenberechnung von DoCalli
  14.08.01 mo  ReadMachineIniFile     TN1004 neu: [Module] DilutorAspSpeedPerCent=1 wertet Dilutor.AspSpeed als % Wert von [6WayValve] AspSpeed
  15.08.01 mo  ReadMachineIniFile     TN1011 neu: [Options] HToolYCorrect=0 => Y Korrektur bei Handler Bewegungen abschaltbar
  28.08.01 tbh InitSamGlobals2        TN1008 Delimiter für MoveRack-Option Slot per Ini einstellbar
  28.08.01 tbh ReadSamplerIniFile     TN1008 neu: [Run] MoveRackSlotDelimiter setzt Delimiter für MoveRack-Option Slot
  05.09.01 tbh ReadSamplerIniFile     TN1032 neu: [Schedule] OpenFixationAtInit setzt ob bei Init Vortexer geöffnet werden
  06.09.01 tbh ReadSamplerIniFile     TN1030 neu: [Schedule] ReleaseVortexersAtEnd setzt ob Programmende CAT-Vortexer/Heizer stoppt
  06.09.01 tbh ReadSamplerIniFile     TN1030 neu: [Schedule] NoVortexerStopAtInit setzt ob Init CAT-Vortexer/Heizer stoppt
  10.09.01 mo  ReadSamplerIniFile     TN1035 neu: [DLLFiles] cDllFiles= <Liste aller C/C++ Dlls>
  21.09.01 mo  ReadSamplerIniFile     TN1060 neu: [DLLFiles] DetachDLL= <Liste aller DLL's bei denen Detach aufgerufen wird>
  21.09.01 mo  ReadSamplerIniFile     TN1060 neu: [Display]  MethEditActionOnly=1 nur Seq,Action,Option wird in Methedit angezeigt
  21.09.01 mo  SaveSamGlobals         TN1060 Aufruf von Dll->Detach
  28.09.01 tbh ReadMachineIniFile     TN1050 neu: neuer TipTyp 'REMREDI' für Pickup Redi Tips
  14.10.01 mo                         TN1067 Merge mit SIAS Änderungen
  17.10.01 mo  InitSamGlobals2        TN1067 Archive werden jetzt hier angelegt
  06.11.01 mo  ReadSamplerIniFile     TN1084/TN1057/TN1085 neue Parameter in Sampler.ini
  26.11.01 tbh ReadSamplerIniFile     TN1112 neu: [Pipetting] ChangeTipTypes=1 aktiviert Überschreiben von TipTypes
  26.11.01 mo  ReadMachineIniFile     TN1110 Parameter hzTravelTubes entfernt
  26.11.01 tbh ReadSamplerIniFile     TN1051 neu: [Calli] WeighLogicDbName= <Pfad\Datenbank für CW>
  27.11.01 tbh ReadSamplerIniFile     TN1051 neu: [Calli] WeighLogicDbUser= <User der CW-Datenbank>
  27.11.01 tbh ReadSamplerIniFile     TN1051 neu: [Calli] WeighLogicDbPassword= <Passwort für CW-Datenbank>
  29.11.01 tbh ReadSamplerIniFile     TN1121 neu: [Pipetting] ChkLHPByGeneratePipSeq=1 aktiviert LHP-Kontrolle in RunTrd.GeneratePipSeq
  30.11.01 mo  ReadSamplerIniFile     TN1105 neu [ExportRunFile] xlsDelCSV=1 DateTimeStampStr='yyyymmdd-hhnnsszzz'
  05.12.01 mo  ReadMachineIniFile     TN1136 neu: gMinZTravelTubes Begrenzung des hZTravel für TubeMovements
  28.12.01 tbh ReadSamplerIniFile     TN1051 Standards für WeighLogicDbUser/WeighLogicDbPassword geändert
  28.12.01 tbh ReadMachineIniFile     TN1116 Standard für ToolZTravel geändert
  03.01.02 tbh ReadMachineIniFile     TN1161 neu: [Module] PipToolZTravel setzt Reisehöhe für Pipettiertools an Tip 5
  03.01.02 tbh ReadMachineIniFile     TN1161 neu: [Module] MinZTravelPipTool setzt Mindest-Reisehöhe für Pipettiertools an Tip 5
  23.01.02 tbh ReadSamplerIniFile     TN1166 neu: [Pipetting] WashRemRediTips setzt ob PicFix-Spitzen gewaschen werden
  23.01.02 tbh ReadSamplerIniFile     TN1135 neu: [Balance] UseSeveralBalanceRacks setzt ob WaagenRacktyp angepasst wird
  22.02.02 tbh ReadMachineIniFile     TN1052.2 neu [AddDiluters] setzt zusätzliche Dilutoren (2.Comport)
  26.04.02 tbh ReadSamplerIniFile     TN1052.1 gClientModeActive wird true wenn [ClientMode] in Sampler.ini
  03.05.02 tbh ReadSamplerIniFile     TN1052.1 gClientModeActive in gSlaveModeActive umbenannt
  11.06.02 tbh ReadMachineIniFile     TN1208 neu: [OPTIONS] MaxSBError setzt maximale Anzahl an Retries für 2. X-Main
  12.07.02 tbh ReadSamplerIniFile     TN1243 neu: [ScriptReset] ArchiveScript setzt ob Skript archiviert werden soll
  12.07.02 tbh ReadSamplerIniFile     TN1243 neu: [ScriptReset] ArchiveScriptPath setzt den Skript-Archivierungspfad
  12.07.02 tbh ReadSamplerIniFile     TN1243 neu: [ScriptReset] AutoResetScript setzt ob Skript automatisch am Ende zurückgesetzt wird
  18.07.02 mo                         TN1109 IniFileNamen werden aus Konstante gelesen
  19.07.02 pk  ReadSamplerIniFile     TN1052 new: gUseScheduler turn Scheduler on/off
  19.07.02 tbh ReadSamplerIniFile     TN1109 Name SystemLiquidValve-Sektionsname wird aus Konstante gelesen
  19.07.02 tbh gmUpdateSamplerIni     TN1109 neu: passt Sampler.ini an actuelle Version an
  19.07.02 tbh InitSamGlobals2        TN1109 ruft jetzt gmUpdateSamplerIni auf
  02.09.02 pk  ReadSamplerIniFile     TN1052 Changed to Integer: 0:No Scheduler, 1:Scheduler + old thread manager, 2:Scheduler + new thread manager
  09.09.02 mo  ReadMachineIniFile     TN1280 neu: gRunTableExclusive
  13.09.02 wl                         TN1283 Merge mit SIAS
  26.09.02 wl                         TN1283 Kein fester Inifile Name mehr
  09.10.02 wl  gmReadSamRecord        TN1293.2  Funktion zum Einlesen des SAM-Records
  16.10.02 wl  ReadMachineIniFile     TN1300 Sam.WashTip wird nicht für SIAS ausgewertet
  16.10.02 wl  ReadMachineIniFile     TN1293.2 komplett mit IniAccess
  16.10.02 wl  ReadMachineIniFile     TN1293.2 debuggt
  18.10.02 wl  ReadAppIni             TN1293.1 komplett mit IniAccess
  18.10.02 wl  ReadAppIni             TN1293.1 komplett mit IniAccess
  22.10.02 wl  ChangePeriSpeedKV4     TN1293.1 verwendet WinlissyIniAccess
  22.10.02 wl  SaveSamGlobals         TN1293.1 verwendet WinlissyIniAccess
  23.10.02 wl  gmUpdateSamplerIni     TN1293.1 'COMMPORT' wird durch 'ComPort' ersetzt
  23.10.02 wl  ReadAppIni             TN1293.1 neu: [SlaveMode] Active-Eintrag muß existieren
  25.10.02 wl  InitSamGlobals2        TN1293.1 Aufruf UpdateSamplerIni in globals
  14.11.02 wl  ReadAppIni             TN1328.1 Lesen von UseScheduler --> globals
  14.11.02 wl  ReadRobotIni,ReadAppIni  TN1293.1 Application.Terminate bei Fehler (wie früher)
  22.11.02 wl  ConvertTubeToolSection TN1293.1 konvertiert Tool-Daten in neue Sektion [TubeToolData]
  26.11.02 wl  ReadRobotIni           TN1283   Dist_RefPos_Tip1 wurde 2 mal beschrieben -> entfernt
  26.11.02 wl  ReadCfgIni             TN1293.1 --> RoboticInterfaceZP01
  05.12.02 wl  ReadRobotIni           TN1345   HXSTEPS_PER_MM, HYSTEPS_PER_MM werden beim Aufstarten gesetzt
  10.12.02 wl  ReadRobotIni           TN1345   ifdef sias  durct TAppSettings.ISSias  ersetzt
  10.12.02 wl  ReadRobotIni           TN1345   MaxPorts, RelayBoard5, DilutorType --> RoboticInterfaceZP01
  12.12.02 wl  SaveSamGlobals         TN1345   Entladen von DLL's --> TActionModules.ShutDown
  20.12.02 wl                           TN1293.5 uses und WinlissyIniAccess geändert
  09.01.03 wl  gmUpdateSamplerIni       TN1293.5 --> ZACommon.dll
  09.01.03 wl  ChangePeriSpeedKV4       TN1293.5 --> ZACommon.dll
  22.01.03 tbh ReadAppIni             TN1193 neu: [Redi] ShakeTimeAdjustable setzt ob Redischüttlerzeit einstellbar
  12.02.03 wl  ReadAppIni             TN1293.5 Einlesen gLanguage entfernt
  15.02.03 tbh gmArchiveFiles         TN1434 Date formats assigned for english OS
  04.03.03 wl  ReadRobotIni           TN1345 ifdef sias kann wegfallen da bei ZP02 Sam.Washtip = 1
  04.03.03 wl  ReadRobotIni           TN1345 einlesen von 'OPTIONS','MaxSBError' --> RoboticInterfaceZP01
  07.03.03 tbh ReadRobotIni           TN1443 einlesen von Schüttelparametern
  17.03.03 wl  ReadAppIni             TN1332.2 Einlesen gLog-Variablen --> ZaCommon.dll (Logging)
  17.03.03 wl  gmMoveErrorFile,gmArchiveFiles TN1332.2 --> ZaCommon.dll (Logging)
  10.04.03 wl  ReadAppIni             TN1439.7 CFR21 Compliant -> No Slave Mode
  10.04.03 wl  ReadAppIni             TN1332.4 CFR21 Compliant -> gResetScriptAfterDDEStart, gAutoResetScript := true
  13.06.03 wl  ReadRobotIni           TN1501.1 Einlesen der BC-Reader-Daten --> ObjModulA
  25.06.03 wl  ReadAppIni             TN1501  gMoveTubeToBalance, gMoveTubeFromBalance: TTubeOptions statt integer
  08.07.03 wl  gConradBoardPort       TN1501   --> CommunicationManager
  23.07.03 wl  ReadAppIni             TN1501   Einlesen der gBalance..-Variablen --> gModules.CreateBalance
  17.09.03 wl  ReadRobotIni           TN1526   Bestimmung von Dist_RefPos_Tip1 --> Rack
  18.09.03 wl  ReadAppIni             TN1597   g24hMode jetzt als integer (statt boolean)
  18.09.03 wl  ReadRobotIni           TN1526   gWiperSpeedX,-Y wird jetzt aus Area "ROBOT" eingelesen
  25.09.03 wl  ReadRobotIni           TN1580   benutzt const-Werte für Diti-Get,-Drop
  30.10.03 tbh ReadRobotIni           TN1635   Einlesen von gUseCorrectCarrierOffset
  23.12.03 wl  ReadRobotIni           TN1712   gTubeRetakeRotation wird gelesen
  20.01.04 wl  ReadRobotIni           TN1672   ZP01: HVSTEPS_PER_MM = YSTEPS_PER_MM (wichtig für Motor-Device)
  19.02.04 pk  InitSamGlobals2        TN1753   Setup gCalledDlls to ignore adds of duplicate dll names
  19.02.04 pk  SaveSamGlobals         TN1753   Delete dlls from memory, call gCalledDlls.Free <- TActionModules.ShutDown
  23.02.04 pk  IsApplicationInUse     TN1753   from Sampler.pas and Layouter.pas
  23.02.04 pk  ReadAppIni             TN1760   gHideVoluemsWindow wird gelesen
  03.03.04 pk  EndApplicationUse      TN1753   Close Mutex Handle
  04.03.04 pk  ReadAppIni             TN1789   gDontPutBackToolForActions wird gelesen
  08.03.04 pk  TAppStartup            TN1646   New
  02.04.04 wl  ReadRobotIni           TN1788   Einlesen von gPipToolZTravel,gMinZTravelPipTool,gRediZRetrSpeed --> DevivesGrpArm
  02.04.04 wl  ReadRobotIni           TN1788   Einlesen von HTip1...  --> DevicesGrpArm
  02.04.04 wl  ReadRobotIni           TN1788   Einlesen von 'AddDilutors' --> RoboticInterfaceZP01
  29.04.04 wl  ReadRobotIni           TN1887   liest gMTP_ShowNoRackButton
  14.05.04 pk  ReadRobotIni           TN1920   read gTubeBarcodesAreUnique
  28.05.04 wl  ReadAppIni             TN1843   Die Tabellenpfade für den Import wurden falsch eingelesen
  08.06.04 wl  ReadRobotIni           TN1963   Enlesen von gToolZTravel,gMinZTravelTubes --> DevicesXBasedMotorSystem/DevicesGrpArms
  08.06.04 pk  ReadAppIni             TN1974.1 read gRunStatusFlagEditMode
  17.06.04 wl  ReadAppIni             TN1951.1 liest gFlushWithRediTips
  17.06.04 wl  ReadAppIni             TN1987.1 liest gMoveToRealPosBeforeWiper
  24.06.04 wl                         TN2007   FileCtrl-Aufrufe durch Utility-Aufrufe ersetzt
  05.07.04 pk  ReadAppIni             TN2018   uses TMethodGUIParser to parse tube options
  07.07.04 pk  ReadAppIni             TN2018   uses Postools to parse tube options
  21.07.04 pk  ReadAppIni             TN2049   read gSchedDatasourceType
  22.09.04 tbh IsApplicationInUse     TN2146   keinen Mutex erzeugen wenn entsprechendes File vorhanden
  10.11.04 wl  SaveSamGlobals         TN2213   benutzt TDllCall.DirectExecute statt gmDLL_LoadEventFunction
  15.11.04 wl  TAppStartup.IsApplicationInUse  TN2229  bei existierender NoMtx.dll können genau 10 Instanzen gestartet werden
  15.11.04 wl  TAppStartup.GetNumberOfOtherOpenApplications  TN2229  kann anhand der Mutexe Anzahl der gestarteten Applikationen ermitteln
  15.11.04 wl  TAppStartup.GetNumberOfOtherOpenApplications  TN2229  hat noch ein Problem: Instanzen setzen den Mutex erst zurück, wenn alle Instanzen geschlossen sind
  16.11.04 wl  SaveSamGlobals         TN2229   do not call EndApplicationUse
  04.03.05 pk  InitSamGlobals2        TN2330.1 call TAppSettings.SetAliases
  20.04.05 pk  ReadAppIni             TN2395   Read 'Scheduling', 'MinTime'
  19.05.05 wl  ReadAppIni             TN2414   gCapParkStrategy: für das Entnehmen von Deckeln aus CapParkRack
  25.05.05 wl  ReadAppIni,SaveSamGlobals  TN2427   gFlushCycles entfernt
  15.06.05 pk  ReadRobotIni           TN2464   read DistX_Pip2_Tip1
  21.06.05 pk  ReadRobotIni           TN2464.3  Dist..._H_Tip1 --> DevicesGrpArm
  21.06.05 pk  ReadRobotIni           TN2464.3  New ; HZSTEPS_PER_ZSTEPS - Handler Z-Steps per Z-Steps ( this factor =1 for ZP02 motors, but different for ZP01 )
  08.08.05 pk  ReadAppIni             TN2524    Read gPipDifferentOptionsTogether
  25.08.05 wl  ReadAppIni             TN2558.8  gSchedDataSourceType entfernt
  07.11.05 pk  ReadAppIni             TN2737    Read gSchedShiftTime
  07.11.05 pk  GetNextAvailableMutexName TN2737  New
  08.11.05 wl  ReadAppIni             TN2745   ExportScript,AutoResetScript,ResetScriptAfterDDEStart entfernt
  30.11.05 wl  ReadAppIni             TN2815   gReservedNames entfernt
  30.11.05 wl  ReadAppIni             TN2818   gWashRemRediTips entfernt
  06.01.06 wl  ReadRobotIni           TN2718   liest gUseCorrectLevelHeights
  06.02.06 wl  ReadRobotIni           TN2928   liest gNoMoveIfRackAtDestination
  27.04.06 pk  ReadRobotInit          TN2958   read gDiTiScanSpeed, gDiTiScanRamp
  13.06.06 pk  ReadAppIni             TN3154   read gRunVarUsePriority
  06.09.06 wl  ReadAppIni             TN3288   Nicht benötigte Calli-Options entfernt
  26.09.06 wl  ReadRobotIni           TN3326   Lesen der [TipTypes] entfernt
  07.12.06 wl  ReadAppIni             TN3409    geänderte CFR21-Abfrage
  12.12.06 wl  ReadRobotIni           TN3468   BorderFactor abgeschafft
  22.02.07 pk  SaveSamGlobals         TN3583   free gImport objects
  25.07.07 wl  ReadAppIni             TN3792   gSaveSolvent entfernt
  31.08.07 wl  InitSamGlobals2        TN3811.4 TDataAdaptor.SetAlias --> AppInstanceDataAdaptorCommon
  04.09.07 wl  gmInit/ShutdownApplication   TN3811.4 von AppSettings hierher
  12.11.07 pk                         TN3924   Steps changed to mm
  09.01.08 wl  gSwitchPortsOnRestart      TN3972    entfernt
  09.01.08 wl                             TN3972    Master/Slave-Krempel  entfernt
  07.01.08 pk  ReadRobotIni           TN3971   Read gWorkbenchZ
  30.01.08 wl  gOpenFixationAtInit    TN4003   --> CatMixConnection
  30.01.08 wl  gNoVortexerStopAtInit  TN4003   entfernt
  30.01.08 wl  gFixAfterVortexing     TN4003   --> IntfShakerDevice
  20.06.08 pk  gGraphicsType          TN4139   New
  27.06.08 pk  gCarrierReverseY       TN4139   New
  30.06.08 pk  ReadRobotIni           TN4151   ZTravel settings removed from Robot, Module
  04.08.08 pk  ReadRobotIni           TN4139   Remove VisualFactor Zoom, BorderLeft, etc.
  17.06.09 wl  gmReadGlobalsIniFile   TN4612   von ObjSampl hierher verschoben
  10.08.09 wl                         TN4702   Strings werden jetzt direkt geladen
  31.08.09 pk  gmReadGlobalIniFile    TN4753   removed
  15.12.09 ts  gmRackOptionsFromInt   TN4945   new
  13.04.10 wl                         TN5044   uses FileUtilities
  07.06.10 pk  gmRackOptionsFromInt   TN5077   moved to RackBarcodeRunStep
  23.06.10 pk                         TN5163   New gUseDifferentTipTypesTogether
  29.06.10 pk                         TN5143   New gUseDifferentLiqParamsTogether
  15.07.10 pk                         TN5196   WaitAfterPickSys removed
  27.07.10 wl                         TN5123   unbenutzte Settings entfernt
  27.04.11 wl  ReadAppIni             TN5562   gWashOnlyUsedTips entfernt
  15.09.11 wl  ReadAppIni             TN5694   ShowTempRequestTime,LogVortInfo entfernt
  28.10.11 wl  ReadAppIni             TN5725   gTubeColors : 4 Werte, die TubeDispColor ersetzen
  03.11.11 wl  ReadAppIni             TN5725   gWritePosinfo --> SubstanceLoading
  18.06.12 wl  ReadAppIni             TN5899  gEnableVolumeControl, gSysLiqMinVol entfernt
  25.07.12 ts  ReadAppIni             TN5944   EndOfLineChar for each line can be set in the settings (CR+LF)
  10.08.12 wl  ReadAppIni             TN5947   UsePeriAtWash entfernt
  06.06.13 wl  ReadRobotIni           TN6154   Lesen von TubeToolData --> ToolHandling
  -------------------------------------------------------------------------------------------------- }

unit SamStart;


interface


uses
    Windows,
    AppTypes,
    CommonTypes;

type
    TAppStartup = class
    private
        class procedure CreateAppMutex();
        class function MutexExists(aIndex: integer; out oName: string): boolean;
    public
        class function IsApplicationInUse(): boolean;
        class procedure EndApplicationUse();
        class function GetNextAvailableMutexName(out oName: string): boolean;
        class function GetNumberOfOtherOpenApplications(): integer;
    end;

procedure InitSamGlobals2;
procedure SaveSamGlobals;

function gmTubeOptionsFromInt(aOptionInt: integer): TTubeOptions;


implementation


uses
    Dialogs,
    Generics.Collections,
    SysUtils,
    Forms,
    Classes,
    FileUtilities,
    AppSettings,
    SamGlobe,
    DataAdaptor,
    DialogUtils,
    GeneralTypes;

// --------------------------------------------------------------------------------------------------
procedure ReadRobotIni;
// --------------------------------------------------------------------------------------------------
var
    xIniAccess: IWinlissyIniAccess;
begin

    xIniAccess := gCommonDll.CreateRobotIni;
    with xIniAccess do
    begin
        try
            gHToolYCorrect := ReadInteger('OPTIONS', 'HToolYCorrect');

            AspSpeedDefault := ReadInteger('LiquidSpeed', 'DefaultAspSpeed');
            DispSpeedDefault := ReadInteger('LiquidSpeed', 'DefaultDispSpeed');
            AspSpeedMax := ReadInteger('LiquidSpeed', 'MaxAspSpeed');
            AspSpeedCalcFactor := ReadInteger('LiquidSpeed', 'AspSpeedCalcFactor');
            DispSpeedCalcFactor := ReadInteger('LiquidSpeed', 'DispSpeedCalcFactor');

            gDilutorAspSpeedPerCent := ReadInteger('Module', 'DilutorAspSpeedPerCent');
            gWiperSpeedX := ReadInteger('Module', 'WiperSpeedX');
            gWiperSpeedY := ReadInteger('Module', 'WiperSpeedY');

            WRITE_LOG_INFO := ReadInteger('WriteLogs', 'WRITE_LOG_INFO');
            WRITE_LOG_ERROR := ReadInteger('WriteLogs', 'WRITE_LOG_ERROR');
            WRITE_LOG_DEBUG := ReadInteger('WriteLogs', 'WRITE_LOG_DEBUG');
            WRITE_LOG_SEND := ReadInteger('WriteLogs', 'WRITE_LOG_SEND');
            WRITE_LOG_NO_NEW := ReadInteger('WriteLogs', 'WRITE_LOG_NO_NEW');

            if ReadBool('DISPOSAL_TIPS', 'DT_CHECK_BEFORE') then
                DTErrorMode := DTErrorMode + DT_CHECK_BEFORE;
            if ReadBool('DISPOSAL_TIPS', 'DT_DISP_BEFORE') then
                DTErrorMode := DTErrorMode + DT_DISP_BEFORE;
            if ReadBool('DISPOSAL_TIPS', 'DT_DISP_ERROR') then
                DTErrorMode := DTErrorMode + DT_DISP_ERROR;
            if ReadBool('DISPOSAL_TIPS', 'DT_CHECK_AFTER') then
                DTErrorMode := DTErrorMode + DT_CHECK_AFTER;
            if ReadBool('DISPOSAL_TIPS', 'DT_DISP_AFTER') then
                DTErrorMode := DTErrorMode + DT_DISP_AFTER;
            if ReadBool('DISPOSAL_TIPS', 'DT_REMOVE_LIQ') then
                DTErrorMode := DTErrorMode + DT_REMOVE_LIQ;

            if ReadBool('DISPOSAL_TIPS', 'GT_DISP_ERROR') then
                GTErrorMode := GTErrorMode + GT_DISP_ERROR;
            if ReadBool('DISPOSAL_TIPS', 'GT_SKIP_ERROR') then
                GTErrorMode := GTErrorMode + GT_SKIP_ERROR;
            if ReadBool('DISPOSAL_TIPS', 'GT_RETRY_ERROR') then
                GTErrorMode := GTErrorMode + GT_RETRY_ERROR;
            if ReadBool('DISPOSAL_TIPS', 'GT_CHECK_AFTER') then
                GTErrorMode := GTErrorMode + GT_CHECK_AFTER;
            if ReadBool('DISPOSAL_TIPS', 'GT_DISP_AFTER') then
                GTErrorMode := GTErrorMode + GT_DISP_AFTER;
            if ReadBool('DISPOSAL_TIPS', 'GT_SKIP_AFTER') then
                GTErrorMode := GTErrorMode + GT_SKIP_AFTER;
            if ReadBool('DISPOSAL_TIPS', 'GT_RETRY_AFTER') then
                GTErrorMode := GTErrorMode + GT_RETRY_AFTER;
            if ReadBool('DISPOSAL_TIPS', 'GT_SINGLE_TIP') then
                GTErrorMode := GTErrorMode + GT_SINGLE_TIP;

            gDiTiIgnoreRestTips := ReadBool('DISPOSAL_TIPS', 'IgnoreRestTips');
            gDiTiScanSpeed := ReadInteger('DISPOSAL_TIPS', 'GT_ScanSpeed');
            gDiTiScanRamp := ReadInteger('DISPOSAL_TIPS', 'GT_ScanRamp');

            gPlateCheckPut := ReadInteger('PlateCheck', 'PutPlate');
            gPlateCheckGet := ReadInteger('PlateCheck', 'GetPlate');

            gUseCorrectCarrierOffset := ReadBool('TakeTubes', 'UseCorrectCarrierOffsets');

            gUseCorrectLevelHeights := ReadBool('Stacker', 'UseCorrectLevelHeights');
            gNoMoveIfRackAtDestination := ReadBool('MoveRack', 'NoMoveIfRackAtDestination');

            gTubeRetakeRotation := ReadInteger(STR_ISEC_BARCODE, 'TubeRetakeRotation');
            gTubeBarcodesAreUnique := ReadBool(STR_ISEC_BARCODE, 'TubeBarcodesAreUnique');

            gWorkbenchZ := ReadFloat('Module', 'WorkbenchZ');
            gCarrierReverseY := ReadBool('Carrier', 'ReverseY');
        except
            TDialogUtils.MessageBox(TLanguageString.Read('Error reading parameter from settings!',
                'Fehler beim Lesen der Parameter aus den Settings!'), TLanguageString.Read('Start-Up Error',
                'Start Fehler'), MB_ICONSTOP);
            Application.Terminate;
        end;
    end;
end;

// --------------------------------------------------------------------------------------------------
function gmTubeOptionsFromInt(aOptionInt: integer): TTubeOptions;
// --------------------------------------------------------------------------------------------------
var
    i: integer;
begin
    result := [];
    if aOptionInt = -1 then
    begin
        result := [optUseRackidPos];
        exit
    end;
    for i := 1 to Integer( high(TTubeOption)) do
    begin
        if (aOptionInt and ARR_POWER_OF_TWO[i]) = ARR_POWER_OF_TWO[i] then
            result := result + [TTubeOption(i)];
    end;
end;

// --------------------------------------------------------------------------------------------------
procedure ReadAppIni;
// --------------------------------------------------------------------------------------------------
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := gCommonDll.CreateAppIni;
    with xIniAccess do
    begin
        try
            // ---------------------------------------------------------------------------------- [Display]
            gConfirmIgnore := ReadBool('Display', 'ConfirmIgnore');
            g24hMode := ReadInteger('Display', '24h-Mode');
            gChangePosinfo := ReadBool('Display', 'ChangePosinfo');
            gAskForRackPlacement := ReadInteger('Display', 'AskForRackPlacement');
            gTubeColors.Aspirate := ReadInteger('Display', 'TubeAspColor');
            gTubeColors.Dispense := ReadInteger('Display', 'TubeDispColor');
            gTubeColors.TubeMove := ReadInteger('Display', 'TubeMoveColor');
            gTubeColors.Other := ReadInteger('Display', 'TubeOtherColor');
            gHideVolumesWindow := ReadBool('Display', 'HideVolumesWindow');
            // -------------------------------------------------------------------------------- [Pipetting]
            gWashDryDelay := ReadInteger('Pipetting', 'WashDryDelay');
            gDispSubmergeActive := ReadBool('Pipetting', 'DispSubmergeActive');
            gChangeTipTypes := ReadBool('Pipetting', 'ChangeTipTypes');
            gUseDifferentTipTypesTogether := ReadBool('Pipetting', 'UseDifferentTipTypesTogether');
            gUseDifferentLiqParamsTogether := ReadBool('Pipetting', 'UseDifferentLiqParamsTogether');
            // ----------------------------------------------------------------------------- Wash-Variablen
            gFlushVolAfterInit := ReadInteger('Pipetting', 'FlushVolAfterInit');
            gAirVolAfterInit := ReadInteger('Pipetting', 'AirVolAfterInit');
            gRetractStepsAfterInit := ReadInteger('Pipetting', 'RetractStepsAfterInit');
            gFlushWithRediTips := ReadBool('Pipetting', 'FlushWithRediTips');
            // -------------------------------------------------------------------------------- [6WayValve]
            gFlushVol := ReadInteger(STR_ISEC_SYSLIQVALVE, 'FlushVol');
            // ------------------------------------------------------------------------------------- [Redi]
            gShakeTimeAdjustable := ReadBool('Redi', 'ShakeTimeAdjustable');
            gMoveToRealPosBeforeWiper := ReadBool('Redi', 'MoveToRealPosBeforeWiper');
            // ---------------------------------------------------------------------------------- [Balance]
            gMoveTubeToBalance := gmTubeOptionsFromInt(ReadInteger('Balance', 'MoveTubeToBalance'));
            gMoveTubeFromBalance := gmTubeOptionsFromInt(ReadInteger('Balance', 'MoveTubeFromBalance'));
            gMoveTubesAfterRestart := ReadInteger('Balance', 'MoveTubesAfterRestart');
            gUseSeveralBalanceRacks := ReadBool('Balance', 'UseSeveralBalanceRacks');
            // ------------------------------------------------------------------------------------ [Calli]
            gUseRackIDPos := ReadBool('Calli', 'UseRackIDPos');
            gWeighLogicDbName := UPPERCASE(ReadString('Calli', 'WeighLogicDbName'));
            gWeighLogicDbUser := UPPERCASE(ReadString('Calli', 'WeighLogicDbUser'));
            gWeighLogicDbPassword := ReadString('Calli', 'WeighLogicDbPassword');
            // -------------------------------------------------------------------------- [DatabaseToolDll]
            gDbToolDllName := ReadString('DatabaseToolDll', 'Name');
            gRequestWriteBackData := ReadInteger('DatabaseToolDll', 'RequestWriteBackData');
            gShowPlatesBeforeBCRead := ReadInteger('DatabaseToolDll', 'ShowPlatesBeforeBCRead');
            // --------------------------------------------------------------------------------- [DLLFiles]
            gCDllFiles := ReadString('DLLFiles', 'cDLLFiles');
            gDetachDll := UpperCase(ReadString('DLLFiles', 'DetachDLL'));
            // ----------------------------------------------------------------------------------- [Device]
            gSamDeviceName := ReadString('Device', 'Name');
            // -------------------------------------------------------------------------------------- [Run]
            gMoveRackSlotDelimiter := ReadString('Run', 'MoveRackSlotDelimiter');
            // ---------------------------------------------------------------------------- [ExportRunFile]
            gExpDefs.Path := ReadString('ExportRunFile', 'Path');
            gExpDefs.Separator := ReadString('ExportRunFile', 'Separator');
            gExpDefs.Numbering := ReadBool('ExportRunFile', 'Numbering');
            gExpDefs.DateTimeStamp := ReadBool('ExportRunFile', 'DateTimeStamp');
            gExpDefs.SeprAtEndOfLine := ReadBool('ExportRunFile', 'SeprAtEndOfLine');
            gExpDefs.Extension := ReadString('ExportRunFile', 'Extension');
            gExpDefs.xlsDelCSV := ReadBool('ExportRunFile', 'xlsDelCSV');
            gExpDefs.DateTimeStampStr := ReadString('ExportRunFile', 'DateTimeStampStr');
            gExpDefs.EndOfLineChar := ReadString('ExportRunFile', 'EndOfLineChar');
            if not SysUtils.DirectoryExists(gExpDefs.Path) then
                TFileUtilities.ForceDirectories(gExpDefs.Path);
            // ------------------------------------------------------------------------------ [ScriptReset]
            gAskForKeepContents := ReadBool('ScriptReset', 'AskForKeepContents');
            // ------------------------------------------------------------------------------ [CapParkRack]
            gCapParkStrategy := xIniAccess.ReadString('CapParkRack', 'CapParkStrategy');
            // --------------------------------------------------------------------------------- [Scheduling]
            gSchedMinTime := ReadInteger('Scheduling', 'MinTime');
            // --------------------------------------------------------------------------------- [Graphics]
            gGraphicsType := ReadString('Graphics', 'GraphicsType');
            // --------------------------------------------------------------------------------[ActionDependent]
            gDontPutBackToolForActions := ReadString(STR_ISEC_ACTIONDEPENDENT, 'DontPutBackTool');
        except
            TDialogUtils.MessageBox(TLanguageString.Read('Error reading parameter from settings!',
                'Fehler beim Lesen der Parameter aus den Settings!'), TLanguageString.Read('Start-Up Error',
                'Start Fehler'), MB_ICONSTOP);
            Application.Terminate;
        end;
    end;
end;

// ==================================================================================================
// InitSamGlobals: Wird beim Start jeder Zinsser-Applikation aufgerufen
// --------------------------------------------------------------------------------------------------
procedure InitSamGlobals2;
// --------------------------------------------------------------------------------------------------
begin
    // ------------------------------------------------- Lesen der Konfigurations-Daten aus ini-Dateien
    ReadRobotIni;
    ReadAppIni;

    gCalledDLLs := TList<string>.Create;
end;

// ==================================================================================================
// SaveSamGlobals: Wird beim Beenden jeder Zinsser-Applikation aufgerufen
// --------------------------------------------------------------------------------------------------
procedure SaveSamGlobals;
// --------------------------------------------------------------------------------------------------
var
    i: integer;
begin
    // ---------------------------------------------------------- Im Speicher verbliebene DLL's löschen
    for i := 0 to gCalledDLLs.Count - 1 do
    begin
        { TODO : No access to DLLCall here! }
        // if (UpperCase(gCalledDLLs[i])='BAYERAXC') then TDllCall.DirectExecute('BAYERAXC','Kill','');
        // if (Pos(UpperCase(gCalledDLLs[i]),gDetachDll)>0) then TDllCall.DirectExecute(gCalledDLLs[i],'Detach','');
    end;
    gCalledDLLs.Free;

end;

var
    gHandleAppUse: THandle;
    gNoOfAllowedInst: integer;

function gmGetMutexName(aIndex: integer): string;
begin
    result := 'ZA_' + IntToStr(aIndex) + '_LISSY_ROBOT';
end;

class function TAppStartup.MutexExists(aIndex: integer; out oName: string): boolean;
var
    xMutex: THandle;
begin
    oName := gmGetMutexName(aIndex);
    xMutex := OpenMutex(MUTEX_ALL_ACCESS, true, PChar(oName));
    result := (xMutex <> 0);
end;

class function TAppStartup.GetNumberOfOtherOpenApplications(): integer;
var
    x: integer;
    xName: string;
begin
    result := -1;
    for x := 0 to gNoOfAllowedInst - 1 do
    begin

        if MutexExists(x, xName) then
            inc(result);
    end;

    ASSERT((result <> -1), 'Application has no Mutex!');
end;

class function TAppStartup.GetNextAvailableMutexName(out oName: string): boolean;
const
    STR_STOP_CREATE_MUTEX_FILE = 'nomtx.dll';
var
    x: integer;
begin
    result := true;

    if (FileExists(STR_STOP_CREATE_MUTEX_FILE)) then
        gNoOfAllowedInst := 10 // 10 erlaubte Instanzen
    else
        gNoOfAllowedInst := 1; // nur eine erlaubte Instanz

    for x := 0 to gNoOfAllowedInst - 1 do
    begin
        if MutexExists(x, oName) then
            CONTINUE;
        EXIT;
    end;
    result := false;

end;

class procedure TAppStartup.CreateAppMutex;
var
    xName: string;
begin
    gHandleAppUse := 0;
    if not GetNextAvailableMutexName(xName) then
        EXIT;
    gHandleAppUse := CreateMutex(nil, true, PChar(xName));
end;

class function TAppStartup.IsApplicationInUse(): boolean;
begin
    result := false;
    CreateAppMutex;
    if (gHandleAppUse <> 0) then
        EXIT;

    MessageBeep(MB_ICONHAND);
    MessageDlg('Application in use !', Dialogs.mtError, [mbOK], 0);
    result := true;
end;

// --------------------------------------------------------------------------------------------------
class procedure TAppStartup.EndApplicationUse();
// --------------------------------------------------------------------------------------------------
begin
    if (gHandleAppUse <> 0) then
        CloseHandle(gHandleAppUse);
end;


end.
