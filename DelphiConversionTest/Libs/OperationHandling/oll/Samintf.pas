unit Samintf;
{ --------------------------------------------------------------------------------------------------
  Ebene 2 (Sam-Interface)
  --------------------------------------------------------------------------------------------------
  Sampler Interface zur SAMPLER.DLL
  --------------------------------------------------------------------------------------------------
  Datum     op  function/procedure     Änderung / Neuerung
  --------  --  -------------------    -------------------------------------------------------------
  07.10.97  mo  SamErrBox              auf MessageBox umgestellt
  SamWriteLogStr         Schreiben Errstr entfernt
  InitIntf               in try..execpt block gesetzt
  21.11.97  dl  FlushPeripump          Abfrage V_Per_Sys disabled
  24.11.97  dl  SamGetZPos             Berechnung mit der RackStruktur (property RackStructure)
  28.11.97  dl  SamGetZPos             Berechnung von ZMax (RackData) auf TubeZ_mm (RackStruktur)
  SamGetVol              dito + Umstellung auf RackStrukur mm - Werte
  _SetBCReader           Interface zur dll
  _GetBarcode            Interface zur dll
  _ReadSingleBarcode     Interface zur dll
  _ReadBarcodeStrip      Interface zur dll
  //_ReadSamTrack
  04.12.97  dl  InitIntf               DLL - Versionsüberprüfung / bei falscher Version -> Ausstieg
  Deklaration            Const DLLVersion = '1.3' --> diese Versionsnummer wird mit der DLL - Nummer
  DLLVersionPtr^ verglichen
  Versionsnummer ist zur Laufzeit im About Formular zu sehen.
  05.12.97  dl  SamGetZpos             Z-Berechnug aus dem Volumen: wird bei ZMax abgeschnitten.
  SamGetaposition        Bug (bei leerer Rackid wird in der Posinfo nach ID '' gesucht) entfernt.

  08.12.97  dl  Pickliquid             nach samglobe.pas verschoben
  DispLiquid             dito
  FlushSyringe           dito
  EmptyDilutor           dito
  GetDilutorAmount       DITO
  GetAspSpeed            dito
  GetDispSpeed           dito
  ModuleExist            dito

  Wash                   nach globals(Sampler Software) / Sophglob (Sophas Software)
  08.01.98  mo  AppSleep               Abfrage auf GlobalErrPtr eingebaut
  08.01.98  mo  SamAppSleep            in interface bereich übernommen
  14.01.98  dl  SamWriteLogStr         LOG Infos sind ausschaltbar --> machine.ini / samglobe.pas
  17.02.98  dl  SamAspLiquid           Abfrage eingefügt on RackId Leerstring ist;
  SamDspLiquid           Abfrage eingefügt on RackId Leerstring ist;
  02.03.98  mo  SamDspLiquid           POSINFO.DB : Feldname POSITION in POS geändert
  SamAspLiquid                    - " -
  17.03.98  mo  SamevBeforePickLiquid  neu
  SamevAfterPickLiquid	neu
  SamevBeforeDispLiquid	neu
  SamevAfterDispLiquid	neu
  InitIntf               Prüfung auf Dll-Version jetzt direkt nach dem Laden der DLL
  Neuer recordTyp SamplerEvent
  CheckForSamplerEvent   neu:  Prüfung ob ein Event ausgeführt werden muss
  SetSamplerEvent        neu: Hinzufügen eines Events
  uses                   neu: Postools
  18.03.98 mo   SamEvBeforePickLiquid  Aufruf Eventfunction eingebaut
  20.03.98 mo   DestroySamplerEvents   Abfrage auf PSamplerEven <>nil
  24.03.98 dl   TSamplerEvent          Erweiterung mit FuncParam : TFuncParam;
  LoadEventFunction(ToolDLL, ToolFunc : SamEventStr; ToolParameter : TFuncParam);
  09.04.98 mo                          Neue Variablen :  GlobalErrBeep + ErrMessageActive
  SamErrBox              Setzen von  GlobalErrBeep + ErrMessageActive für Warnton
  02.05.98 mo                          TEventFunction Übergabe Fensterhandle
  05.05.98 wl   LoadEventFunction      Übernahme in den interface-Abschnitt -> public
  13.05.98 mo                          Alle zugriffe auf ComDebug entfernt
  14.05.98 mo   LiquidDetection        Neue Funktion : nur detektieren und in Posinfo schreiben
  15.05.98 mo                          TEventFunction : neu Übergabe GlobalErrptr
  03.06.98 wl  SamErrBox               Erneuerte Funktion mit Einbindung von SamErrDlg
  neu: uses SamErr
  04.06.98 wl  ErMsg                   ab sofort in veränderter Form in unit SamErr
  HexStr                  --> gmHexStr: zusätzlicher Parameter für Startbyte
  08.06.98 wl  CreateErrorFile         Kopien der Logfiles werden nach dem Datum benannt
  15.06.98 wl  CreateErrorFile         Fehler behoben: Dateikürzel immer 3 Zeichen lang
  18.06.98 wl  CreateErrorFile         Kopien der Logfiles werden im Pfad pLogFilePath gespeichert
  Ist der Pfad nicht vorhanden, wird er erzeugt
  Sollte dies nicht gelingen, bricht das Programm ab (Terminate)
  InitIntf                (!!!) Aufruf von InitSamGlobals ganz nach vorn gesetzt (heikel?!?)
  MkDirEx                 neu: Funktion zum Erstellen mehrerer geschachtelter Directories
  InitIntf                Aufruf von ArchiveErrorFile
  19.06.98 wl  ArchiveErrorFile        neue Funktion: Je nach Einstellung (machine.ini) werden
  alte Logfiles gelöscht oder in ein Archivverzeichnis kopiert
  StrToDateExt            Wandelt String in TDateTime um. Wenn String kein Datum ist, ist result:=false
  23.06.98 wl  SamErrBox               Aufruf von SamErr erfolgt über neuen Thread: SamErThr
  gmWriteSamModules       neu: von SamGlobe nach SamIntf verschoben
  gmWriteSamModule            dito
  InitIntf                neue Reihenfolge: 1.)InitSamGlobals; 2.)gmWriteSamModules;
  3.)ArchiveErrorFile; 4.)CreateErrorFile;
  InitSampler      (!)     --> von globals nach Samintf
  SamplerActive    (!)     --> von globals nach Samintf
  == Version 3.3.0
  26.06.98 wl  gmWriteSamModules       Pointer auf Moduladresse wird übergeben
  gmWriteSamModule         ""
  gmDeactivateSamModules  setzt Moduladressen auf 64000
  gmActivateSamModules    setzt Moduladressen zurück
  07.07.98 wl  LoadEventFunction       ruft procedure oder function mit Rückgabewert (string) auf
  10.07.98 wl  gmCheckHardlock         globale Hardlock-Abfrage (aus Main verschoben)
  13.07.98 wl  InitIntf                Initialisiert Hardlock
  gmHardlockLogout        Hardlock_Logout
  gm(De)activateSamModules global statt gSampler
  21.07.98 wl  TurnBCReaderOff         aus Barcode: Einbinden von Barcode in SamGlobe nicht mehr nötig
  LoadEventFunction       Rückgabewert: Inhalt des Adresse vom Funktionsparameter
  22.07.98 wl  gmActivateSamModules    --> dbTools
  gmDeactivateSamModules  --> dbTools
  02.09.98 wl  CreateErrorFile         Error.dat wird nicht im aktuellen Pfad, sondern in PAppPath gesucht
  SamWriteLogStr           dito
  16.09.98 wl  gmWriteSamModules,gmWriteSamModule  --> ObjGlobe (TSamModules)
  InitIntf                gmWriteSamModules entfernt (--> SamGlobe: InitSamGlobals)
  neu: WB (globales Workbench-Objekt), LogDisply
  uses ObjWorkb statt LayOForm
  SamplerActive           entfernt (--> ObjSampl)
  alle Funktionen         Layout.LogDisplay -> LogDisplay
  18.09.98  mo InitIntf                Abfrage auf DllVersion prüft nur auf die ersten drei Zeichen
  ------                  _GetPlate(.., neuer Param CheckPlate)
  _MovePlate(...) entfernt
  _Dispense2(..., neue Param AirVol, PeriVol)
  18.09.98 mo  SamEvBeforeExecute      Neu: Event wird immer vor Execute aufgerufen.
  08.10.98 wl  InitSampler             --> TBasicThread.InitAll (kein Init ohne Thread mehr möglich)
  09.10.98 wl  TurnBCReaderOff         --> SamGlobe: ResetAllModules / TBasicThread.InitAll
  15.10.98 wl  InitIntf                wird von InitSamGlobals aufgerufen, nicht umgekehrt
  03.11.98 mo  var                     _GetPlate(_hRack : HRack; CheckPlate : Word) Checkplate von Boolean in Word geändert
  04.11.98 HC  _ReadPlate              Funktion für Photometer hinzugefügt ! - Beispiel in Update Text !
  04.11.98 wl  SetSamplerEvent         gmReadSubStr statt ReadField
  09.11.98 wl  SamAspLiquid,SamDispLiquid   Abfrage auf gWritePosinfo
  11.11.98 wl                          neu: var gStatusBar
  17.11.98 wl                          var gSamModules,gExportRun aus SamGlobe
  EmptyDilutor,FlushSyringe,GetDilutorAmount,GetAspSpeedm,GetDispSpeed,CalcRestVol,
  ResetAllModules,ModuleExist   aus SamGlobe hierher verschoben
  20.11.98 wl                          uses Utility2
  02.12.98 wl  ResetAllModules         schaltet Rosys-Shaker aus
  08.12.98 wl  _PutPlate               mit Parameter CheckPlate
  const DLLVersion = 1.5
  25.01.99 wl  _Aspirate2              übergibt Mix:LIQPAR_MIXREC statt MixCycl,MixVol,MixSpeed
  _Dispense2              dito
  26.01.99 wl                          const DLLVersion = 1.6
  01.02.99 mo  SamSetTipError          Tip Error wird jetzt ausgewertet
  01.02.99 wl  ResetAllModules         Shaker wird nach ResetModules ausgeschaltet
  02.02.99 wl  SamErrBox               Automatisches Retry bei BC-Receive Error (einmal je BC)
  09.02.99 wl  SArrText,WArrText,FlushSyringe  MAX_TIPS durch Sam.nTips ersetzt
  01.03.99 mo                          Alle Abfragen auf Hardlock entfernt
  31.03.99 hc  _washplate              zum Starten des Waschers eingefügt !
  31.03.99 hc                           DLLVersion     = '1.7'
  14.04.99 hc  _corridormove           Der Dispensebefehl ruft diese Funktion auf. Dadurch wird jetzt eine Anfahrt über die Korridore ermöglicht
  14.04.99 hc  _XPos / _YPos           Liefert die aktuellen Kordinaten des Samplers zurrück !
  21.04 99 hc  Aspirate2              Drei neue Parameter werden übergeben
  int   RecordDetectionVolume, // write Detection Volume in Database !
  int   AspirationMethodRetractZ, // Aspiration Method for small Volumes
  SINGLE SampleAspRetractPos
  23.04.99 hc  SamRecordVolumes        Diese Funktion schreibt die beim Aspirate detectierten Volumina in die Posinfo.db
  23.04.99 hc SamAspLiquid             Posinfo als Step wird jetzt kein negativer Wert mehr zugelassen !
  26.04.99 wl  ArchiveErrorFile,CreateErrorFile  --> SamStart
  27.04.99 hc  procedure _DispLiq      wird nich mehr inportiert
  DLLVersion     = '2.0'
  12.07.99 wl  SamCorridorMove         Übergabeparameter ohne aktuelle Position
  13.07.99 wl  _XPos / _YPos           entfernt (überflüssig wg. _GetPos)
  29.07.99 wl                          uses ThrdMan
  23.08.99 wl  GetAspSpeed,GetDispSpeed  Asp-/DispSpeedCalcFactor wird mit eingerechnet
  13.09.99 mo                          DllVersion 3.0
  13.09.99 wl  GetDilutorAmount        Zahl 7 durch MAX_DILUTOR ersetzt
  14.09.99 wl  SamAPowder,SamDPowder   neu: für Aufnahme und Abgabe von Resin
  16.09.99 wl  _InitSyringes,_Init     gelöscht bzw. verschoben in TBasicThread.InitAll/InitSyringes
  _ReadFromPort,_WritePort  gelöscht (gibt es gar nicht)
  _InitMotor,_InitModule  PChar statt CharPtr
  20.09.99 wl  SamAPowder,SamDPowder   funktionsfähig gemacht
  SwitchOn,SwitchOff      verzweigen auf Sampler-port oder Conrad-Karte
  21.09.99 wl  SamPowderShakerOn       Extra-Funktion zum Anschalten der Shaker
  FlushSyringe,EmptyDilutor  --> SamWash
  24.09.99 wl                          uses dbTools entfernt
  27.09.99 wl  SamGetTubeData          Infos aus RackStructure.Shift_Radius_mm, .Shift_NoOfSteps
  SamAPowder,SamDPowder   Delays entfernt
  28.09.99 wl  GetAspSpeed,GetDispSpeed,ResetAllModules,LiquidDetection,SwitchOn,SwitchOff --> SamCmd
  28.09.99 mo  SamErrBox               ins interface + gSamErrLogText:=StrPas(PInfo);
  29.09.99 wl  SamAPowder,SamDPowder   Hardcore für MesseSuperMegaRedi
  30.09.99 wl  _ZTravel                eingefügt aus DLL
  30.09.99 mo  SamAPowder              Schüttler werden ausgeschaltet
  30.11.99 mo  SamParser Funktionen    zugriff auf SamPars.dll
  09.12.99 mo  SendMsg                 wird aus Sampler.dll importiert
  09.12.99 mo  AinfoPtr                für Ainfo Struktur Retry Funktion ( ausgeklammert )
  17.12.99 wl  LoadEventFunction       bei jedem DLL-Aufruf wird der Name in gCalledDLLs notiert
  29.12.99 mo  AinfoPtr                für Ainfo Struktur Retry Funktion aktiviert
  04.01.00 mo  var                     _wait_for_washer, WashPlate, ReadPlate ";" hinzugefügt (Delphi5)
  12.01.00 wl  LogText                 --> Utility2
  SamWriteLogStr          Ausführung in gmWriteLog (Utility2)
  17.01.00 wl                          Definition gExportDefs --> SamGlobe (gExpDefs)
  21.02.00 wl  SamAPowder,SamDPowder   LogText erscheint nicht mehr auf LogDisplay
  22.02.00 mo                          DLLVersion 3.1
  22.02.00 mo                          _MoveToXY( wieder mit Sam.ZTravel);
  07.03.00 wl  SamPowderShakerOn       Abfrage auf RackName von DevicePart statt von Device
  08.03.00 wl  SamPowderShakerOn       ruft gmSwitchRackDevice (aus SamCmd) auf
  12.04.00 wl  SamAPowder,SamDPowder   gmSwitchon/-Off um ExecuteWait erweitert
  SamSwitchNitro          neu: aus SOPHAS: Globals.pas
  neu: gTango : THuberTango
  14.04.00 wl                          gTango --> ObjGlobe
  15.04.00 wl  alle Funktionen         gModules... statt ConradRelayBoard...
  18.04.00 wl  SamPowderShakerOn       ruft gModules.SwitchRackDevice (ObjGlobe) auf
  28.04.00 wl                          uses ObjModul statt SamGlobe
  22.05.00 wl  gmCorridorMove          aus ObjWorkb hierher
  26.05.00 wl  Reduzierung der Funktionen auf Sampler.dll-Funktionen:
  26.05.00 wl  ModuleExist,LoadEventFunction,gmSetStatusBar  --> posTools
  26.05.00 wl  Parser-Funktionen (cdecl) --> dbTools
  26.05.00 wl  variablen, typen        --> SamGlobe
  04.07.00 wl  _Aspirate,_Dispense,_AspiratePowder,_DispensePowder  MODULENAME statt PortNumber
  04.07.00 wl                          DLLVersion 3.2
  05.07.00 mo  SamSwitchModulePort     neue Funktion zum Schalten von Ports
  05.07.00 wl  SamSwitchModulePort     mit Leben gefüllt
  07.07.00 mo  _Aspirate               neue Parameter für SysAirgap
  07.07.00 wl                          DLLVersion 3.3
  14.07.00 mo _Aspirate                neue Parameter für Kanal2 Tipwash zusammen mit Spitback
  14.07.00 mo _PickLiquid _DispLiquid  neue Parameter für Kanal2
  14.07.00 mo _PickLiq                 entfernt
  14.07.00 mo                          DLLVersion 3.4
  07.08.00 wl  SamAPowder,SamDPowder   benutzt neue Funktion gModules.SwitchOffAllShakers
  08.08.00 wl                          DLLVersion 3.5 (geänderter SAMPLER-record)
  11.08.00 mo  _DLLVersion             neue Funktion aus Sampler.dll (compiliert mit Visual C++)
  11.08.00 mo  InitIntf                Abfrage DllVersion geändert
  11.08.00 mo                          DLLVersion 4.0 Visual C++
  14.08.00 mo SamWriteLogStr           Logtype in WORD umgewandelt
  14.08.00 mo SamWriteLogStr           in interface Abschnitt
  14.08.00 mo SamGetVol                Logtext ist jetzt vom Typ LOG_DEBUG
  16.08.00 mo _PickLiquid              Neuer Parameter Exec
  16.08.00 mo _DetectLiquid, _DetectLiq Neuer Parameter LQMode
  16.08.00 mo _Dispense                Neuer Parameter Ch2Vol
  16.08.00 mo SamSetTipError           in interface Abschnitt
  18.08.00 wl  SamToolInHandler        Abfrage auf Tool In Handler  ==> DLLVersion 4.2
  29.08.00 wl                          Alle nicht verwendeten Funktionen auskommentiert
  24.10.00 tbh SamAspLiquid            Volumenkontrolle für Tubes auf Workbench eingeführt
  31.10.00 tbh SamAspLiquid            Volumenkontrolle korrigiert
  03.11.00 mo  _TTLResult              neu: Abfrage TTLPORT = DLL Version 4.3
  10.11.00 mo  _Dispense               neuer Parameter TotalVol = DLL Version 4.4
  13.11.00 tbh _Dispense/_Aspirate...  DLLParameter: Delays jetzt Typ Single
  05.03.01 tbh SamAspLiquid            Eintrag zu Volumengrenze in Sampler.ini trennt RackID und Pos mit '_'
  05.03.01 tbh SamAspLiquid            Bei Abbruch der Volumenkontrollfehlermeldung wird GlobalErrorPointer gesetzt
  22.03.01 tbh SamAspLiquid            Volumengrenze auch für ganzes Rack einstellbar
  10.04.01 tbh SamAspLiquid            Volumengrenze auch für ganzes Rack korrigiert
  25.04.01 mo  SamGetAPosition         Volumenberechnung korrigiert : nur Records mit Step >=0 gehen in die Rechnung ein
  25.04.01 mo  SamAspLiquid            Volumenberechnung korrigiert + IniFile.Free
  06.06.01 tbh SamAspLiquid            Volumenkontrolle über Ini-Eintrag abschaltbar
  13.10.01 mo                          TN1067 Merge mit SIAS Änderungen
  16.10.01 mo  _MoveSamZTravel         TN1067 SIAS Änderung
  20.10.01 tbh _GetPRTip               TN1050 neu: Procedur zum Aufnehmen von PickUp Removable Redi Tips
  20.10.01 tbh _ReturnPRTip            TN1050 neu: Procedur zum Zurückbringen von PickUp Removable Redi Tips
  20.10.01 tbh SamFixPRTips            TN1050 neu: Procedur zum Fixieren der Tips beim _ReturnPRTip
  29.10.01 mo  Dll Version             = 4.5
  05.11.01 tbh _GetPRTip/_ReturnPRTip  TN1050 Positionsparameter in TipWordArray geändert
  05.11.01 tbh SamFixPRTips            TN1050 Verknüpfung auf _FixPRTips gesetzt
  13.11.01 tbh _GetPRTip/_ReturnPRTip  TN1050 Parameter um CheckRack und CheckPos ergänzt
  27.11.01 mo  SetSamplerEvent         TN1058.1 Im Simulation Mode werden keine Events ausgeführt
  29.12.01 tbh SamDspLiquid/SamAspLiquid  TN1156 Pipettierschritte erhalten in der Posinfo keinen Step=0 mehr
  03.01.02 tbh SamAPowder              TN1158 es kann zusätzlich ein Port zum Umschalten auf Zuleitung geschaltet werden
  06.03.02 tbh SamMoveToWiper          TN1190 neu: ermöglicht das Abstreifen auf speziellem Wippenrack
  24.05.02 mo  SamErrBox               TN1208 Automatisches Retry bei Zusätzlichen Dilutoren auf zweiter Schnittstelle
  11.06.02 tbh SamErrBox               TN1208 Automatisches Retry bis FehlerCounter >= gMaxSBError
  11.06.02 tbh InitIntf                TN1208 FehlerCounter-Zeiger für Adressen auf 2. X-Main-Board wird gesetzt
  23.08.02 mo  ExecuteEvent            TN1252 neu ExecuteEvent = allgemeiner EventHandler
  23.08.02 mo  DllVersion              TN1252 = V 4.6.0
  11.09.02 wl                          TN1283 Merge mit SIAS
  23.09.02 wl  SamErrBox               TN1283 Fehlerbehandlung 2.Dilutorgruppe ausgeklammert
  27.09.02 wl  InitIntf                TN1294  DllHandle wird auf 0 statt NULL abgefragt
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  22.10.02 wl  SamAspLiquid            TN1293.1 verwendet WinlissyIniAccess
  22.10.02 wl  SamMoveToWiper          TN1293.1 verwendet WinlissyIniAccess
  06.11.02 wl  SamWriteLogStr          TN1293.1 PAppPath ersetzt
  12.11.02 wl  GetXPositionForHandlerConflict  TN1338 neue Routine zur Berechnung des maximalen x-Wertes
  14.11.02 wl                         TN1347 alle Messe-Redi- und TipConvert-Funktionen für SIAS deaktiviert (Sias ID 222)
  25.11.02 wl  GetTips, DropTips       TN1363   Berechnung für Handler Conflict eingefügt
  29.11.02 wl                          TN1369   SIAS_IO: Funtionsaufrufe von EmptyTip, TurnValve und Waste korrigiert
  --------------------------------------------------------------------------------------------------
  11.12.02 wl                          TN1345   komplett erneuert - jetzt "Kompatibilitäts-Hülle" für TRoboticInterface
  11.12.02 wl  Funktionen mit _        TN1345   --> RoboticInterfaceZP01/ZP02
  11.12.02 wl  Sam-Funktionen          TN1345   --> AppInterface
  11.12.02 wl  Robot                   TN1345   Ersatz für Robot von Sias und Sam-Variable
  11.12.02 wl  InitIntf                TN1345   erzeugt jetzt auch TModules-Object
  27.12.02 wl                          TN1293.5 uses und WinlissyIniAccess geändert
  18.02.03 wl  InitIntf                TN1345   PSamEvent:=nil --> TAppInterface.Create
  12.03.03 wl                          TN1293.5 uses posTools
  28.03.03 wl  InitIntf                TN1293.7 führt am Start gmInitAppFunctions aus
  08.04.03 wl                          TN1293.5 unbenutzte Funktionen entfernt
  02.06.03 wl                          TN1485.4 TAppSettings.IsSias ersetzt die ifdefs
  08.07.03 wl  InitIntf                TN1536   Umstellung auf gCommManager (Erzeugung von ConradBord und Robot)
  08.07.03 wl  _SetBCReader,_ReadSingleBarcode  TN1501.1 entfernt
  17.07.03 wl  InitIntf                TN1536   LogDisplay wird übergeben
  29.07.03 wl  _TTLResult              TN1501.3 --> RoboticInterfaceZP01
  30.07.03 wl  InitIntf                TN1536   ConradRelay wird hier nicht mehr erzeugt
  03.09.03 wl  _AspiratePowder,_DispensePowder,_GetPRTip,_ReturnPRTip  TN1526   entfernt
  03.09.03 wl  InitIntf                TN1568   benutzt DLLLoading.pas
  17.09.03 wl  InitIntf                TN1528   global_DistX_H_Tip1 - entfernt, da 0
  25.09.03 wl                          TN1580  wenn ini appdata.tmp-Datei NewMethods,UseAspDisp = 1 gestzt ist, werden die neuen Funktionen benutzt
  07.10.03 wl  PickDTips               TN1581.1 von BasicLTh hierher verschoben
  15.10.03 wl  _Disp                   TN1624   benutzt das TPipPumpDevice
  20.10.03 wl                          TN1580   bei UseAspDisp = 1 wird für jede Funktion ein Log-Eintrag geschrieben
  03.11.03 wl                          TN1493.6 SendCmd: string statt pchar
  10.11.03 wl  _Aspirate               TN1656.2 New Methods: SampleAspRetractPos integer statt single (führte zu Fehler)
  12.11.03 wl  CalculatePipSteps_IndividualY, MoveToXY_IndividualY  TN1659.1 entfernt
  10.12.03 wl  alle Asp/Disp-Funktionen  TN1672 Verzweigung für alte/neue Implementierung entfernt (UseAspDisp = 1)
  16.12.03 wl  gmCorridorMove          TN1672   entfernt
  18.12.03 wl                          TN1672   uses geändert
  19.12.03 wl  _Move,_Disp,_SendCmd    TN1672   entfernt
  21.01.04 wl  _TipOK                  TN1672   wird an TRobotArmDevice.IsTipOK weitergeleitet
  04.02.04 pk                          TN1719   Functions required by Layouter -> SamintfLow
  19.02.04 pk  ShutDownIntf            TN1753   Shuts down everything that was initialized in InitIntf
  02.03.04 wl  _Aspirate,_Dispense,_PickLiquid,_PickAir,_DispLiquid,SetSamplerEvent TN1773   removed
  02.03.04 wl  PickDTips               TN1773   --> LiquidHandlingLow
  12.03.04 wl  _InitMotors             TN1812   für beide Arme: ResetLastRack
  12.03.04 wl  _GetPlate,_PutPlate     TN1812   --> DeviceGrpArm
  18.03.04 wl  _CheckState             TN1765.1 entfernt
  05.04.04 wl  _GetPos,_MoveZ,_MoveHandler,_InitModule,_TipOK  TN1788 entfernt
  23.04.04 wl  InitIntf                TN1788.8 statt 'LogDisplay' steht im LogDisplay Title, Version und Build
  11.05.04 wl  SamDspLiquid            TN1788   entfernt
  08.06.04 wl  _GetError,_GetState,_ShutdownSampler  TN1963   entfernt
  08.06.04 wl  gmAllArms..-Methoden    TN1963   = _InitMotors, _Execute, _MoveToZTravel
  08.06.04 wl  gmGetGripperArm         TN1963   neu: result = GripperArmDevice
  08.09.04 wl  SamSetTipError          TN2121   entfernt
  16.02.05 wl  ShutDownIntf            TN2269   ruft gCommManager.ShutDown statt gModules.ShutDown auf
  21.02.05 wl  SamAppSleep             TN1960   hat jetzt 2 Parameter: der zweite ist die minimale Wartezeit im Simulationsmodus
  21.02.05 wl  InitIntf                TN1960   TAppInterface wird nicht mehr erzeugt
  28.02.05 wl  InitIntf                TN1960   initInterface von Create abgetrennt, damit gCommManager schon existiert
  03.06.05 wl  gmAllArmsSafety         TN2436   neu: für Safety-Funktion bei direkter Command-Eingabe
  15.06.05 pk  gmAllArmsMoveZTravel    TN2464   call MoveZTravel for all arms
  15.06.05 pk  gmAllArmsExecute        TN2464   call Execute for all arms
  11.07.05 wl  InitIntf                TN2498.1 neu: Instanz von TAppInterface wird übergeben
  17.11.05 wl  gGrpArm,gmGetGripperArm TN2771   entfernt - direkten Bezug auf einen Gripperarm darf es nicht mehr geben
  17.11.05 wl  SamErrBox,...           TN2771   überflüssige Funktionen entfernt
  03.04.06 pk  gmAllArmsInitMotorsWithID   TN2997   new parameter : aInitID
  01.12.06 pk  InitIntf                TN3441   TActionModules.Create moved to Main so that it is created after threadmanager
  05.12.06 wl  InitIntf                TN3448    Robot.WaitForWasher entfernt
  07.12.06 wl                          TN3243    uses SamCmd entfernt
  24.01.07 wl  InitIntf                TN3520    Version und build und Sampler.dll version werden in Appdata.tmp section [Versions] eingetragen
  26.01.07 pk  ShutDownIntf            TN3525.4  SaveSamGlobals moved here from thrman
  24.02.07 pk  gLogAndGlobalErrorIntf  TN3598   Keep a reference to the robotic interface for logging and global error checks
  27.02.07 pk  InitIntf                TN3525.2 Application title no longer logged in this function
  02.03.07 pk  InitIntf                TN3612   reference to global var Sam removed
  06.03.07 wl  InitIntf                TN3620    Robot.DLLName und DLLVersion werden hier nicht mehr geschrieben
  07.03.07 wl  Robot                   TN3620    entfernt
  08.03.07 wl  InitIntf                TN3620    RoboticInterface wird nicht mehr grundsätzlich erzeugt
  12.03.07 pk  gmAllArmsInitMotorsWithID TN3628  only init ZMotors if motion device is TMotorBasedMotionDevice
  01.08.07 wl  InitIntf                TN3811.2  LogDisplay entfernt
  07.01.08 pk  gmAllArmsInitMotorsWithID TN3864  Init ZMotors for all arms before initializing anything any other motors
  29.01.08 wl                            TN3980   uses geändert
  25.04.08 wl  gmAllArmsExecute          TN4051    entfernt
  10.07.08 hd  gmAllArmsInitMotorsWithID TN4163  declared in the interface part
  10.07.08 hd                            TN4163  uses Drivers added to the interface section
  23.03.11 wl  gmAllArmsMoveZTravel      TN5515   Funktionsname geändert
  17.01.13 wl                            TN6068   uses geändert
  23.05.13 wl  gmInitArm                 TN6153  ResetLastRack in die TInitOperation verlegt
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Driver;

procedure gmAllArmsInitMotors();
procedure gmAllArmsMoveZTravel;
procedure gmAllArmsInitMotorsWithID(aInitID: TDevInitID);


implementation


uses
    SysUtils,
    SamHigh,
    OperationFactory,
    OperationAxisMove,
    IntfMotorBasedMotionDevice,
    IntfArmDevice,
    DeviceManager;

procedure gmInitArm(aUsedArm: IArmDevice; aInitID: TDevInitID);
var
    xInitOp: TInitOperation;
begin
    xInitOp := TOperationFactory.CreateInitOp(aUsedArm);
    try
        xInitOp.Init(aInitID);
    finally
        xInitOp.Free;
    end;
end;

procedure gmAllArmsInitMotorsWithID(aInitID: TDevInitID);
var
    x: integer;
    xMotorBasedMD: IMotorBasedMotionDevice;
    xArms: TArray<IArmDevice>;
begin
    xArms := gDeviceManager.FindModules<IArmDevice>();
    for x := 0 to high(xArms) do
    begin
        if not SysUtils.Supports(xArms[x].MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
            CONTINUE;
        xMotorBasedMD.ZMotors.Init(aInitID);
    end;

    for x := 0 to high(xArms) do
    begin
        gmInitArm((xArms[x]), aInitID);
    end;
end;

procedure gmAllArmsInitMotors();
begin
    gmAllArmsInitMotorsWithID(0)
end;

procedure gmAllArmsMoveZTravel;
var
    x: integer;
    xArms: TArray<IArmDevice>;
begin
    xArms := gDeviceManager.FindModules<IArmDevice>();
    for x := 0 to high(xArms) do
        if not Assigned(xArms[x].GripDevice) then
            gmMoveToZTravel(xArms[x]);
end;


end.
