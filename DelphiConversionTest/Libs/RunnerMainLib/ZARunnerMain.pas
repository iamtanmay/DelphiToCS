{ --------------------------------------------------------------------------------------------------
  Ebene 5 (WinLissy)
  --------------------------------------------------------------------------------------------------
  Sampler Main Unit (MDI-Form)
  --------------------------------------------------------------------------------------------------
  Datum     op  function/procedure   Änderung / Neuerung
  --------  --  -------------------  ---------------------------------------------------------------
  11.11.97  mo  StartSamplerClick     Loadlayout vor    CreateRunDB gesetzt
  ( Zugriffsverletzung wenn layout nicht geladen )
  13.11.97  mo  InitSamplerClick      Neue Menufunktion Init
  StopBtnClick          _ResetModules nach Abbruch hinzugefügt
  20.11.97  mo  Hardlock              Hardlockabfrage eingebunden
  25.11.97  mo  Hardlock              Hardlock in compiler directive eingebunden
  26.11.97  dl  FormCreate            Abfrage für RESIN DISPENSING
  StartSamplerClick     Abfrage für RESIN DISPENSING
  StopBtnClick          Abfrage für RESIN DISPENSING
  unit ResDisp          uses Teil interface, Thread, um die Methode abzuarbeiten
  unit RESINPAR         uses Teil implementation, Formular für Redi Parameter
  Menüpunkt             ResinParameter1: TMenuItem;
  // nur sichtbar, wenn ResinDispense = true
  ResinParameter1Click  Formular für REDI Parameter
  03.12.97 mo                         Version für Hoechst mit Hardlock compiliert
  04.12.97 dl  // $DEFINE Hardlock    //Hardlock disabled
  09.12.97 mo                         Version 3.01 als Kundenversion kompiliert mit Hardlock
  Hardlockfunktion ist disabled wenn "C:\temp\$af9cde1.tmp" existiert
  Mit Versionslabel KW 3.01 in PVCS eingebucht
  12.12.97 mo                         unit report aus Uses klausel entfernt.
  15.12.97 mo  StartSamplerClick()    Bei Restart nach Multipipetting werden alle Records mit Sourcevol=0 auf Pipflag 'NO' gesetzt
  15.12.97 mo  MethodebearbeitenClick Prüfen ob Methodenname zulässig ist
  02.01.98 mo  AnimateThread          unit AnimateThread hinzugefügt nur aktiv wenn in Sampler.ini [Display] Animation=1
  Label1.visible nur wenn in Sampler.ini [Display] Animation=1
  06.01.98 mo                         TResinThread und Thread entfernt - alle Sampler-Aufrufe laufen über die Instanz SamplerThread
  Menupunkt IndexRebuild unter Tools entfernt
  Reset alle Module entfernt und nach SamThread verschoben
  07.01.98 mo                         Neuer Menueintrag Run
  13.01.98 dl  StartSamplerClick      bei Restart <if (FieldByName ('Action').AsString = '') then> vor tubepos lesen eingefügt
  StartSamplerClick      and (FieldByName('DilVol').AsFloat = 0) in while schleife bei MPip restart eingefügt
  20.01.98 hc  FormClose              Hier wird bei vorhandenen Omnifit 6 Wegeventil ein Comreset ausgegeben
  24.03.98 dl  Formcreate             Menue Tool: in der Sampler.ini unter [Tool] definierte Funktion wird aufgerufen aus einer DLL
  Menue Tool: FillDisposal Racks
  02.04.98 wl  MainMenu1              Änderung der Menülogik: Verteilen der "Sampler"-Menüpunkte in "Method" und "Tools"
  mnuFileExit            = Beenden1
  mnuMethodStart         = StartSampler  (jetzt in Menu "Method")
  mnuMethodSelect        = Auswhlen1
  mnuMethodEdit          = MethodeBearbeiten
  mnuMethodDelete        = MethodeLoeschen
  mnuMethodImport        = Importieren1
  mnuToolsMove           = Movement1
  mnuToolsInit           = InitSampler
  mnuToolsFlush          = Splen1        (jetzt in Menu "Tools")
  mnuToolsTool           = ToolMenue
  mnuHelpAbout           = ber1
  05.04.98 wl  mnuToolsLiqHClick      Menüpunkt zum Aufruf des Liquid-Parameter-Fensters
  05.05.98 wl  FormCreate             Erzeugen beliebig vieler Menüpunkte zum Aufruf von DLL-Funktionen
  ExecuteDLLMenu         (Anzeigen und) ausführen der erzeugten Menüpunkte
  05.05.98 wl  mnuMethodSelect        Einbinden der neuen Unit 'GetMeth' als universaler Einstieg in die Methodenbearbeitung
  03.06.98 wl  mnuMethodSelectClick   vorheriges Schließen des Metheditfensters
  mnuMethodSelectClick   GetMeth wird hier erzeugt
  = Build 980603a
  03.06.98 wl  mnuMethodStartClick    nach dem Laden des Layots wird der Füllstand der
  Disposable Tip Racks angezeigt (wenn vorhanden)
  mnuToolsInitClick      statt:  _InitMotors;  jetzt:  InitSampler;
  mnuMethodDeleteClick   wieder da: Methoden löschen ohne Methedit
  mnuMethodPrintClick    neu: Methode drucken ohne MethEdit
  08.06.98 wl  mnuToolsModulesClick   zeigt eine Liste mit allen gespeicherten Modulen + Adresse
  16.06.98 mo                         Referenz auf comdebug.pas entfernt
  18.06.98 wl  FormCreate             InitGlobals hinter InitIntf gesetzt
  23.06.98 wl                         uses SamWash
  typDLLMenu --> TDLLMenu
  var Restart --> nach globals verschoben
  29.06.98 mo  mnuMethodStartClick   DDE Funktionen hinzugefügt
  FormCreate            DDE Funktionen hinzugefügt
  FormShow              Neu wgn DDE
  SetStatus             Neu wird von EscThread aufgerufen
  30.06.98 wl  mnuToolsSimulationClick schaltet Simulationsmodus an und aus
  (26.06.)    mnuToolsModulesClick   zeigt Simulationsmodus mit an
  30.06.98 wl  mnuViewVolumesClick    zeigt VolForm
  09.07.98 wl  mnuMethodStartClick    Aufruf von gmRunCreate mit Methode
  mnuMethodStartClick    Disposable Tip Racks anzeigen --> nach LoadLayout verschoben
  09.07.98 wl  mnuMethodStartClick    neu: nur noch Aufruf von gmMethodStart (in dbMTools)
  ShutDownSampler        gelöscht
  AnimateThread          --> ANIMATE
  Mainform.SamplerThread --> TSampler.SamplerThread
  FormClose              EscThread              --> TSampler.EscapeThread
  10.07.98 wl  mnuMethodStartClick    Hardlock-Abfrage       --> SAMINTF
  sbFlushClick           gmGetLayout liefert Ergebnis
  mnuRunShowClick        Aufruf entsprechend dbMTools geändert
  mnuMethodEditClick     Aufruf mit Methodenname
  mnuMethodDeleteClick    dito
  mnuMethodPrintClick     dito
  13.07.98 wl  FormCreate             ERzeugen des Sampler-Objekts TMethodSampler
  Sampler1Click          gelöscht (Objekt Sampler1 gibt es nicht mehr)
  PMethodName,PLayoutName = property des Objektes TMainForm
  ChangeMethodName       Überprüft die Existenz einer Methode und ändert PMethodName,PLayoutName
  NewMethodName          Übernimmt einen neuen (oder bekannten) Namen als PMethodName
  NewLayoutName          Ändert PLayoutName, PMethodName :=''
  ChangeValues           wird von den 3 eben genannten Funktionen im Anschluß aufgerufen
  MethNameAllowed        Ist ein neuer Name überhaupt erlaubt ?
  14.07.98 wl  mnuMethodSelectClick   Programmtext vereinfacht, debugt
  15.07.98 wl  mnuMethodStartClick    Starten mit RunModes: [] oder [ExportAfterRun]
  FormShow               Erzeugen von SamDDE --> Projekt-Quelltext
  StopBtnClick           Aufruf von TMethodSampler.SendDDEStatus
  22.07.98 wl  StopBtnClick           global.ShowAnimation
  23.07.98 wl  SetStatus              entfernt: Ersatz: global.SendDDERunStatus (s. Globals)
  FormShow               global.StartEcsThread
  entfernt: uses SamDDE,Stop,TEscThrd,Delay,Rack,PosTools,dbRack
  StopBtnClick           ruft global.UserInterrupt auf
  24.07.98 wl                         var StopBtnPresses --> ObjSampl
  27.07.98 wl  mnuScriptStartClick    neuer Menüpunkt zum starten eines Scripts
  mnuScriptEditClick     neuer Menüpunkt zum editieren eines Scripts
  28.07.98 wl  ChangeMethodName       ChangeMethodName und NewMethodName zusammengefaßt
  FLayoutName --> ObjSampl
  NewLayoutName          --> ObjSampl:ChangeLayoutName
  NoMethodName           löscht aktuelle Methode
  29.07.98 wl  mnuToolsLayoutClick    lädt ein Layout (löscht aktuelle Methode oder Script)
  div. Funktionen :      global.SamThreadRunning statt SamplerActive
  Rackdefinition1Click,Layoutdefinition1Click  weg
  05.08.98 wl  alle Funktionen        Layout.Workbench durch WB ersetzt
  MainLogDisplay: aus LayOForm hierher verschoben
  mnuViewLog             Beeinflußt MainLogDisplay
  19.08.98 wl                         Offizielle Umbenennung in Zinsser WinLissy
  25.08.98 wl  FormCreate             mnuScript wird nur gezeigt, wenn UseScript in sampler.ini
  26.08.98 wl  mnuMethodEditClick,mnuMethodStartClick,mnuRunShowClick,
  mnuToolsLayout         Aufruf von gmGetDBResult
  10.09.98 wl  mnuToolsModulesClick   Aufruf von SamModules.ShowList
  mnuViewLogClick        verbessert
  22.09.98 wl  mnuScriptEditClick     Script-Edit-Aufruf funktioniert jetzt
  23.09.98 wl  mnuViewLogClick        LogDisplay kann jetzt gut ein- und ausgeschaltet werden
  mnuScriptStartClick    Startet das aktuelle Script
  28.09.98 wl  mnuMethodBuildClick    neu
  mnuRunShowClick        verkürzt (beinhaltet gmRunShow) aus dbTools
  neuer Button sbScript
  29.09.98 wl  alle Funktionen        Aufruf von global.ChangeGlobalName mir Reload=false
  30.09.98 wl  mnuScriptSelectClick   Datenbank-Feld: 'NAME' statt 'SYNTHNAME'
  01.10.98 wl  mnu1Click              Blockiert alle Menüs gleichermaßen
  ReadBarcodes1Click     Aufruf um ein false (Bayer) ergänzt
  mnuScriptSelectClick   ruft GetMeth auf
  05.10.98 wl  alle Funktionen        Abfrage auf ShowRunForm statt ShowRunFormOpen
  08.10.98 wl  FormCreate             TSamplerThrMan statt TMethodSampler
  19.10.98 wl  mnuToolsPeriCalClick   Aufruf des Peripump-Calibration-Fensters
  20.10.98 wl  FormShow               entfernt, EscThread wird bereits beim Create gestartet
  mnuToolsInitClick,mnuToolsFlushClick  Layout wird vorher geladen
  03.11.98 wl                         Run Select/Start deaktiviert
  04.11.98 wl  FormCreate             PeriCal-Menu nur bei aktiv, wenn Peripump vorhanden
  06.11.98 wl                         Menüpunkte in Method und Script neu gestaltet
  11.11.98 wl  alle Methoden          global.SamThreadRunning -> ThrMan.SamThreadRunning
  12.11.98 wl  FormCreate             Erzeugen von ThrMan in SamDDE verschoben
  17.11.98 wl                         uses SamStart
  19.11.98 wl  FormClose              WB.ApplicationEnd --> LayOForm
  19.01.99 wl  ResinParameter1Click   Referenz vom Menüpunkt zur Funktion nachgetragen
  29.01.99 wl  mnuRunShowClick        Run-Select-Fenster wieder wie früher
  01.02.99 wl  alle Funktionen        Thread-Aufruf nicht über ThrMan
  02.02.99 wl  mnuMethodDeleteClick   nach dem Delete wird auch das Layout gelöscht
  22.02.99 mo                         sbScript.Hint = Select Script
  FormCreate             UserLogin über User.dll
  23.02.99 mo  FormCreate             UserLogin wieder entfernt
  25.02.99 mo  FormCreate             Abfrage auf Seriennummer über UserGetSerialNo() in User.dll
  01.03.99 mo                         Alle Abfragen auf Hardlock entfernt
  UserLogout             neue funktion aus User.dll loggt den Hardlock aus
  das Login wird in UserGetSerialNo() ausgeführt
  03.03.99 mo                         neu Userverwaltungsfunktionen im Menue
  08.03.99 wl                         ShowPosForm eingebaut (Menüpunkt unsichtbar)
  Sampler.ini-Pfad wie überall
  09.03.99 wl  FormClose              geändert: wenn Action=caFree, wird auch nicht gemacht
  13.04.99 wl  mnuToolsSimulationClick  gLiquids... statt VolForm...
  FormCreate             gLiquids:=TDisplayLiquids.CreateLiq (statt InitIntf6WayValve)
  31.05.99 wl  FormClose              Valve1.RESET entfernt
  29.07.99 wl                         uses geändert
  10.08.99 wz                         --> Strings in Ressourcen
  07.09.99 wl  ReadBC1Click           Funktion entfernt
  10.09.99 wl  ResinParameter1Click   weg -> möglicherweise in Setup ähnliches Fenster
  13.10.99 wl  FormShow               Abfrage der aktuellen 6-Wege-Ventilposition erst nach Senden des DDE-Status
  mnuRunTableClick       Zeigt Methoden- bzw. Script-abhängiges Run-List-Fenster
  01.11.99 wl  FormCreate             Schalter für "DLLVERSION3" eingebaut
  15.11.99 wl  mnuFileImportClick     neuer Menüpunkt: integrierte Import-Funktion
  04.01.00 mo  FormCreate             lblJob.Caption := gOemText.JobTitle; Anzeige Oem Text
  12.01.00 wl  alle Funktionen        LogText() durch gmLogText() ersetzt (für Delphi 5)
  17.01.00 wl  mnuMethodStartClick    mdExportAfterRun abgeschafft
  11.02.00 wl  mnuToolsSerControl     Menüpunkt für Serial Port Control, bei gSerComControl Visible
  29.03.00 wl  FormCreate             gmBalanceCreate -> aufgeteilt in ..InitDLL und ..Create
  05.04.00 wl  FormShow               Left und Top für Balance-Anzeige werden als Variable übergeben
  13.04.00 wl  TestForm1Click         neues Test-Fenster für Schalter
  15.04.00 wl                         Label zeigt 'SIMULATION MODE' während der Simulation
  08.05.00 wl  mnuScriptSelectClick   global.ScriptName und nicht global.PMethodName als Überschrift
  11.05.00 wl  mnuToolsSchedulerClick neu: TimeScheduler-Button
  11.05.00 wl  FormCreate             Bestimmung der Lizenzrechte --> ObjSampl
  26.05.00 wl                         TestFenster, Barcodelesetest --> LAYOUTER
  26.05.00 wl  FormCreate             InitSamModulesWithDLL entspricht InitSamGlobals
  05.06.00 wl  FormCreate             Überprüfung der Seriennummer aus ObjSampl hierher
  05.06.00 wl  FormCreate             Aufruf von InitWinLissyProjects für Seriennummer
  06.06.00 wl  mnuToolsSchedulerClick verbesserter Aufruf
  16.06.00 wl  FormShow               Zeigt bei gLicence.Sophas ein Vortexer-Infofenster
  20.06.00 wl  FormCreate             mnuToolsFillDTR ist erst sichtbar, wenn Layout mit DiTi geladen ist
  20.06.00 wl  FormShow               gmBalanceCreate --> CreateBalDigit in ObjModulA
  20.06.00 wl  FormClose              _ShutDownSampler --> ThrMan
  21.06.00 wl  mnuScriptSaveAsClick   neu: Script speichern unter ...
  21.06.00 wl  sbBuildClick           neu: Build-Button
  04.07.00 wl  FormShow               Abfrage auf gBalances[0] ersetzt (sonst Exception)
  10.07.00 wl                         neue Button-Anordnung
  17.07.00 wl                         uses geändert
  17.07.00 wl  FormShow               Vortexer-Info-Fenster wird noch nicht angezeigt!!
  18.07.00 wl  FormClose              wenn ScheduleForm<>nil kommt 'Sampler in Betrieb'
  18.07.00 wl  mnuMethodEditClick,mnuMethodStartClick  Auswahl der Methode abgeschafft
  18.07.00 wl  alle Funktionen        Abfragen auf SamThreadRunning durch (ScheduleForm=nil) ergänzt
  18.07.00 wl  FormCreate             Time-Scheduler-Button und Menuitem deaktiviert
  21.07.00 wl  mnuRunTableClick       TEditRunForm statt TSamRunForm, zusätzlicher SetBounds-Befehl
  27.07.00 wl  FormCreate             Buttens werden anders gesetzt, wenn nicht UseScript
  31.08.00 wl  FormShow               Vortexer-Info-Fenster wird wieder angezeigt!!
  01.09.00 wl  FormClose              führt beim Schließen CheckLastScript aus
  01.09.00 wl  mnuScriptEditClick     neuer Aufruf von ScheEdit
  06.09.00 wl  FormShow               Vortinfo wird nur bei (gShowTempRequestTime<>0) angezeigt!
  02.12.00 tbh                        neue Menüeinträge und Button für CherryPicking sowie Dll-Funktionen
  08.12.00 tbh                        Dll-Funktionsaufrufe für CherryPicking in ExternDllFunc
  18.12.00 tbh FormCreate             CP-Elemente nur bei SAMI-Lizenz sichtbar
  18.12.00 tbh mnuCherryPSelectClick/mnuCherryPStartClick  diverses
  22.12.00 tbh CherryP1Click          neu: Menü CherryP kann nicht mehr bei laufendem Thread geöffnet werden
  22.12.00 tbh mnuCherryPSelectClick  Zugriff auf IBToolsDatenbank wird überprüft
  22.12.00 tbh mnuCherryPStartClick   Restart eines CPRuns kann nicht mehr abgebrochen werden
  30.01.01 tbh mnuCherryPSelectClick  Wird mein Run gewählt wird globaler Runname gelöscht
  30.01.01 tbh mnuCherryPStartClick   Es wird auf jeden Fall die Auswahl an CPRuns aufgerufen
  05.02.01 tbh mnuCherryPSelectClick  Header von SAMI-Runs auf Konstante umgestellt
  05.02.01 tbh Create                 SAMI-Button paßt sich Konfiguration an
  05.03.01 tbh mnuScriptSaveAsClick   Überschrift des Skripteditors passt sich neuem Skriptnamen an
  13.03.01 tbh mnuViewVortexerClick   neuer Button für die Anzeige der Thermostate bzw. Vortexer
  16.05.01 tbh mnuScriptResetClick    Aufruf gmScriptReset angepasst
  05.07.01 tbh FormShow               Initialisierung 6-Wege-Ventil deaktiviert
  17.10.01 mo                         TN1067 Änderungen für SIAS
  27.11.01 tbh mnuCherryPSelectClick  TN1051 Aufruf von IBLogin angepasst
  03.05.02 tbh mnuToolsSlaveModeClick TN1052.1 Aufruf des SlaveModus
  03.05.02 tbh EndSlaveMBtnClick      TN1052.1 Button-Funktion zum Beenden des SlaveModus
  05.05.02 mo  mnuToolsSlaveModeClick TN1052.1 neu cSlaveModeLayoutPrefix anstatt 'CM_'
  16.05.02 tbh EndSlaveMBtnClick      TN1210 Meldung beim Beenden wird richtig umgebrochen
  25.06.02 pk  mnuScriptSaveAsClick   TN1052 Prevent the Unsaved Script from begin saved
  12.07.02 tbh mnuScriptStart         TN1243
  12.07.02 pk  mnuScriptSaveAsClick   TN1052 Reallow Unsaved to be saved
  19.07.02 pk  mnuScriptNewClick      Call AskForNewScriptName function to get Valid Script Name
  03.09.02 pk  mnuScriptStartClick    TN1052 ScheduleEditForm is created using SamMainLinkAll
  03.09.02 pk  mnuScriptEditClick     TN1052 ScheduleEditForm is created using SamMainLinkAll
  13.09.02 wl                         TN1283 Merge mit SIAS
  26.09.02 wl                         TN1283 Kein fester Inifile Name mehr
  10.10.02 wl                         TN1293.2 Ini Access - uses geändert
  11.10.02 wl  FormCreate             TN1293.2 AppSettings werden gesetzt
  15.10.02 mo  FormShow               TN1301 ifndef SIAS -> ifndef XAP
  18.10.02 wl  FormCreate             TN1293.1 Aufruf von InitSamGlobals1 --> Projekt-Quelltext
  18.10.02 wl                         TN1293.1 benutzt TAppSettings
  18.10.02 wl  ExecuteDLLMenu         TN1293.1 geändert: gDLLMenuItems
  18.10.02 mo                         TN1301 ifdef SIAS -> XAP
  23.10.02 wl  UserLogout             TN1293.1 verwendet TWinlissyIniAccess
  25.10.02 wl                         TN1293.1 verwendet TAppSettings.DataPath
  28.10.02 mo mnuToolsInitClick       TN1300.1 Init korrigiert
  05.11.02 wl                         TN1328  XAP benutzt LnkSamplerMainLinkAll
  12.12.02 wl  FormCreate             TN1345 Initialisierung -> InitIntf
  27.12.02 wl                         TN1293.5 uses und WinlissyIniAccess geändert
  04.01.03 wl                         TN1334.1 alle User & Licence-Methoden: gCommonDll statt user.dll
  22.01.03 mo FormCreate              TN1405 Einlesen der Tool MenuPunkte korrigiert
  31.01.03 wl  UserSetAccess          TN1334.1 aus Objects hierher & verbessert
  11.02.03 wl  FormDestroy            TN1334.3 gCommonDll.Free
  11.02.03 wl  UserSetAccess          TN1334.3 mehr Restriktionen
  20.02.03 mo  uses                   TN1348 + ObjMossInterface
  20.02.03 mo  mnuMethodStartClick    TN1348 Start von Moss
  28.02.03 mo mnuMethodStartClick     TN1348 korrigiert
  28.02.03 mo ExecuteDLLMenu          TN1405.1 Dll Aufruf korrigiert
  06.03.03 mo                         TN1442   Namensänderungen für Calli+
  11.03.03 wl  mnuCommandEditor       TN1293.7 neuer Menueintrag
  12.03.03 wl  FormCreate             TN1293.7 Command editor menu item is shown if table exists
  27.03.03 wl  UserSetAccess          TN1439.4 Prepare-Mode wird in Statusleiste angezeigt
  03.04.03 tbh FormDestroy            TN1383.6 / TN1383.7 Auslösen des 'Terminate Application' Events
  07.04.03 wl                         TN1383.8 GlobalEvents. statt global.GlobalEvents.
  16.04.03 wl  FormCreate             TN1332.4 benutzt StopStartupLogging
  09.05.03 wl  mnuToolsFillDTRClick   TN1490   --> Objects
  02.06.03 wl                         TN1485.4 Zusammenfassing von MoveCode und SiasMoveCode
  04.06.03 wl  FormCreate             TN1493.12 Menüpunkt Simulation bei ZP02 unsichtbar
  14.07.03 wl  mnuToolsSimulation     TN1535.1 entfernt - Simulation kann nicht mehr zurückgesetzt werden
  17.07.03 wl  FormCreate             TN1536   geändertes InitIntf
  23.07.03 wl  FormShow               TN1536   neuer Aufruf der Balance-Anzeige
  21.10.03 wl  mnuToolsMoveClick      TN1633   Movement-Fenster wird modal aufgerufen und danach der Speicher freigegeben
  22.10.03 wl  mnuMethodStartClick    TN1632   statt den globalName auf '' zu setzen wird nur ein WB.Load durchgeführt
  30.10.03 wl  mnuMultiDelete,mnuMultiCopy  TN1641   Menüpunkte entfernt
  31.10.03 wl  mnuMethodSaveAsClick   TN1641   benutzt bereinigte Funktion gmMethodSaveAs aus ScheUtil
  18.12.03 wl                         TN1672   uses geändert
  04.02.04 pk  SyncShowStopButton     TN1719   Thread-synchronized way to set visibility of Stop Button
  05.02.04 wl  mnuRunTableClick       TN1732   bei MOSS werden die aktuellen MOSS-Runs zur Auswahl gestellt
  05.02.04 wl  LoadMossLayout         TN1732   mit Parameter aCheckTubes
  17.02.04 pk  FormClose              TN1747   ThrMan.ApplicationEnd -> FormDestroy to avoid crash
  19.02.04 pk  FormDestroy            TN1753   gCommonDll.Free -> AppSettings.gmShutDownApplication
  23.02.04 pk  FormCreate             TN1753   Sampler startup code moved from FormCreate to ApplicationStartup
  23.02.04 pk  FormDestroy            TN1753   Sampler shutdown code moved from FormDestroy to ApplicationShutdown
  23.02.04 pk  FormShow               TN1753   Sampler startup code moved from FormShow to ApplicationStartup
  23.02.04 pk                         TN1753   TLayout.Create assigned to global Layout variable
  23.02.04 pk  ApplicationStartup     TN1760   Create Volume form if gHideVolumesWindow is false
  26.02.04 wl  mnuToolsLiqHClick      TN1574   Geänderter Aufruf von LiqPar
  02.04.04 wl  mnuToolsInitClick      TN1788   Setzen von OKTips entfernt (ist sowieso immer 255)
  27.04.04 pk  mnuFileImportWizard    TN1880   New Import Wizard Menu
  08.06.04 wl  mnuToolsZTravelClick   TN1963   benutzt gmAllArmsMoveZTravel
  23.06.04 pk  mnuMethodRunTableClick TN2002   Moss: do not call CreateRunLayout. Just Load Main Moss RunLayout
  23.06.04 pk  MossStartRun           TN2002   Call MossInterface.StartRun
  23.06.04 pk  MossInterfaceInst      TN2002   Create MossInterface if not already created
  24.06.04 pk  mnuToolsInitClick      TN2009   TActionThread changed to TInitAction running in TActionHandlerThread
  24.06.04 pk                         TN2009.3 Uses Threads, Threads2 removed
  29.06.04 pk                         TN2009.8 Uses ActionLow
  05.08.04 wl  mnuMethodEditClick     TN2008.1  Creates TMDIMethEditForm (as MDI Child)
  05.08.04 wl  mnuMethodNewClick      TN2008.1  Creates TMDIMethEditForm (as MDI Child)
  18.08.04 wl  mnuMethodBuildClick    TN2102    Der Build-Button tut jetzt dasselbe wie der Build-Button des MethEdit-Fensters
  19.08.04 wl  mnuMethodSaveAsClick   TN1780    ruft global.MethodSaveAs auf
  23.09.04 wl  ApplicationStartup     TN2149    Erzeugen von VolForm --> SAMPLER.DPR
  19.10.04 wl  UserSetAccess          TN2185.2  mnuImportWizard nur für System-Admin, FillDTRacks nur für User
  02.11.04 pk  FormClose              TN2208    Advanced handling of form close methods
  08.11.04 wl  ExecuteDLLMenu         TN2213    benutzen TDllCall.DirectExecute statt gmLoadEventFunction
  02.12.04 wl  mnuToolsLayout         TN2254.2  ruft global.ChangeLayout auf (d.h. kann auch für Methoden neues Layout laden)
  02.12.04 wl  mnuToolsInit/Flush     TN2254.2  ruft global.ChangeLayout auf wenn kein Layout geladen (d.h. kann auch für Methoden neues Layout laden)
  02.12.04 wl  mnuMethodNewClick      TN2254.2  ruft global.CreateNewMethod auf
  15.12.04 wl                         TN2266    BorderStyle = bsSizable
  06.01.05 wl  sbEndSlaveMBtnClick    TN2246.4  --> SlaveObj.gmEndSlaveModeWithRequest
  06.01.05 wl  mnuMethodPrintClick    TN2246.4  Aufruf von global.PrintMethod statt gmMethodPrint
  06.01.05 wl                         TN2246.4  entspricht Main.pas
  14.01.05 pk  mnuMethodBuildClick    TN2281.0  call global.GUIRunCreate instead of .RunCreate
  17.01.05 wl  ShowRun,mnuViewRunClick TN2246.4  benutzt gmCreateRunForm
  27.01.05 pk  mnuMethodRunClick       TN2300    Searches run for RunStart action and gives list of runs to choose from
  15.02.05 pk  mnuSessionSelect       TN2315     New
  16.02.05 wl  ApplicationStartup      TN2269    statt gLiquids wird hier gDeviceMain erzeugt ( beinhaltet TLiquids.Create )
  21.02.05 wl  trbSimulation           TN1960    Regler für die Länge der Wartezeiten während der Simulation
  28.02.05 pk  mnuSession              TN2315    Run, Build, Delete, RunTable added
  02.03.05 pk                          TN2328    ShowSchedChart deactivated
  07.04.05 pk  TMainForm.FormCreate    TN2375.1  code moved to TRunMain
  07.04.05 pk  ScheTime                TN2375.1  References to ScheTime.pas removed
  19.04.05 pk                          TN2393    session functions moved to TRunMain
  22.04.05 pk  mnuSessionReschedule    TN2393    Session Reschedule
  07.06.05 pk  mnuToolsParserConv      TN2449    New
  07.06.05 pk  ApplicationStartup      TN2449    set mnuToolsParserConv.Visible
  11.07.05 wl  ApplicationStartup      TN2498.1  für InitIntf wird eine Instanz von TAppInterface erzeugt und übergeben
  22.08.05 wl                          TN2558.8  uses ScheUtil entfernt, ScheUtil-Methoden werden mit TDMScript. oder global. aufgerufen
  16.09.05 wl                          TN2574    Umbau der ZARunner-Oberfläche
  19.09.05 pk                          TN2604    Modules RichEdit text box caused crash when closing app. Replaced by memo box
  28.10.05 pk  SyncChangeSysVol        TN2716    New : synchronized call to ChangeSysVol and ChangeWasteVol
  28.10.05 pk  ApplicationStartup      TN2716    gLiquids.SetEvents : events set to Sync functions to avoid software hang
  07.11.05 pk  StopBtnClick            TN2735    call ThrMan.RequestStop instead of directly calling UserInterrupt
  08.11.05 wl                          TN2745    alle Bezüge auf ScheduleForm entfernt
  08.11.05 wl                          TN2745    Scripte können nicht mehr gestartet werden
  17.11.05 wl                          TN2771    SamerrBox durch gCommManager.ErrBox ersetzt
  24.11.05 pk  mnuCherryPSelectClick   TN2805    references to dbrack removed
  22.12.05 pk  ApplicationStartup      TN2875    Now calls CreateThreadManager
  05.01.06 pk  ApplicationStartup      TN2877    create gResourceManager
  10.04.06 pk  mnuToolsInitClick       TN3031    Pass true as fullinit argument
  05.07.06 wl  cbSimInputWeight        TN3182    Neu: wenn checked, müssen alle Wägeergebnisse manuell eingegeben werden
  18.07.06 pk  ApplicationStartup      TN3206    Enter weights manually option only shown in simulation
  25.08.06 thr ApplicationStartup      TN3264    Abfrage Vortexerinfo geändert
  18.09.06 pk                          TN3227.1  gDeviceMain changed to gSysLiqManager
  10.10.06 thr                         TN3351    Vortinfo durch VortinfoBase ersetzt
  27.11.06 wl  TSafeCloseThread.Execute  TN3243    calls TErrorMessageFactory.ErrBoxClose()
  01.12.06 pk  ApplicationStartup      TN3441   cretae thread manager before GUIManager and ActionModules
  01.12.06 pk  StopBtn                 TN3441   cancel property set to false. Avoid user interrupt being done twice
  03.12.06 wl  ChangeSysVol            TN3243   Benutzung von TSysLiquidIsOutErrorInfo
  07.12.06 pk  ApplicationStartup      TN3455   create resourcemanager
  07.12.06 pk  mnuToolsMoveZTravelClick TN3455  run moveztravel inside of a thread
  07.12.06 pk  mnuToolsMoveClick        TN3455  set to invisible. it does not work.
  07.12.06 wl                           TN3243    uses SamErr entfernt
  19.12.06 wl  UserSetAccess            TN3409    User management implementiert
  02.01.07 pk  grdDelays                TN3477    New grid for displaying delay action information
  03.01.07 pk  grdDelays                TN3477    moved to DelayInfo frame
  26.01.07 pk  ApplicationStartup       TN3525.3 create ErrorManager
  26.01.07 pk  ApplicationShutDown      TN3525.4 destroy everything that was created in startup
  22.02.07 pk  ApplicationStartup       TN3583   Create threadmanager before initintf
  22.02.07 pk  ApplicationShutDown      TN3583   global.DestroyWorkbench
  27.02.07 pk  ApplicationStartup       TN325.2  log application title
  06.03.07 wl  ApplicationStartup       TN3620   Robot.PeriPump_Exists statt ModuleExist
  07.03.07 wl  mnuToolsPeriCal          TN3623   entfernt
  01.08.07 wl  DisplayLogText           TN3811.2 überschreibt OnDisplayLogText in LogManager
  07.08.07 wl                           TN3811.3  TPosinfoAdapter.Create ohne Parameter
  09.11.07 pk                           TN3923    class references to TErrorMessageFactory changed to gErrorMessageFactory
  09.11.07 pk                           TN3922    references to Dataset changed to DataProvider
  27.11.07 wl  ApplicationStartup       TN3855    Menu item "Parser Conversion" entfernt
  27.11.07 wl  CreateLiquidLabels       TN3710    Ab dem 7.Kanal wird eine neue Spalte angefangen, eine dritte ab dem 15.Kanal
  27.11.07 wl  ApplicationStartup       TN3906    Lädt Höhe und Breite von Panels
  27.11.07 wl  ApplicationShutdown      TN3906    Speichert Höhe und Breite von Panels
  09.01.08 wl                           TN3972    Button für Slave-Mode entfernt
  20.06.08 pk                           TN4139    create instance of LayoutManager
  11.07.08 wl   FormCreate              TN4164    TModules heißt jetzt TModuleFinder, TActionModules wird extra initialisiert
  20.09.08 pk                           TN4215    Flush and Init reactivated
  23.09.08 wl                           TN4236    uses CommandDataAdaptor entfernt
  25.09.08 pk                           TN4241    various changes due to RunDialogs package
  17.11.08 wl                           TN4311    enthält RunInformation.pas
  18.11.08 pk ApplicationShutdown       TN4280    ResourceManager.Free removed
  02.12.08 pk WMExternalClose           TN4335    New: handles the WM_EXTERNALCLOSE message received from another application (such as AppCloser.exe)
  03.12.08 pk                           TN4341    New look for top Panel
  03.12.08 pk mnuMethodStartClick       TN4279    New: can start the selected method againg (this is not the same as a restart)
  08.12.08 wl                           TN4311    fraRunInformation jetzt mit TreeList
  08.12.08 wl                           TN4343    neue Settings: UseLiquids und DefaultMethod
  12.12.08 pk                           TN4311    fraRunInformation changed to FrmRunInformation
  17.12.08 pk                           TN4372    changes for starting runs in simulation mode
  18.12.08 pk MethodStart               TN4311    call to RunInformation.StartNewRun moved to LisGlobe
  18.02.09 pk                           TN4232    create instance of RunTraceManager
  06.03.09 pk ExternalClose             TN4335    write log when function called
  06.04.09 pk                           TN4503    MASSIVE CHANGES: Changes for Display Components
  07.04.09 pk pnlStart                  TN4503    New
  16.04.09 ts DataHistory               TN4477    New: DATAHISTORY, Methodhistory
  09.06.09 pk ApplicationStartup        TN4585.1  Call TRunDialogsManager.CreateInstance
  15.06.09 ts mnuMethodHistClick        TN4602    MethodHistory ist von 1 bis 10 durchnummeriert
  16.06.09 wl                           TN4606    uses ShowRun entfernt
  10.08.09 wl                           TN4702   GetMethDlg wird nicht mehr erzeugt
  11.08.09 wl                           TN4702   GetNames wird über TDialogUtils aufgerufen
  12.08.09 pk ApplicationStartup        TN4716    call SetLayoutSceneGraphics
  21.08.09 wl                           TN4702   Strings werden jetzt direkt geladen
  27.08.09 pk                           TN4753   Various changes
  31.08.09 pk                           TN4753   create instance of AppInstancePeripheryManagementExt
  24.09.09 pk ApplicationShutdown       TN4753   call UnloadAnyLoadedDisplayComponents before destroying AppInstanceDispComponent
  28.09.09 pk ApplicationStartup        TN4753   Creates AppInstanceRunnerLib instance
  04.11.09 pk                           TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  12.11.09 pk                           TN4845   XPManifest Removed
  04.09.10 pk                           TN5042   RunMain Changed to MainFormControls
  13.04.10 wl                           TN5044   uses geändert
  23.04.10 pk FormCreate                TN5073   set window state to maximized
  20.05.10 wl                           TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  27.05.10 wl                           TN5116   benutzt jetzt auch EdExtern
  04.06.10 wl                           TN5116   ZARunnerMain und ZARunClientMain vereinheitlicht
  18.06.10 wl                           TN5116   Buttons überarbeitet
  22.07.10 wl                           TN5192   neu: init-Button
  30.07.10 wl                           TN5209   Tool-Buttons zeigen Hints
  02.08.11 wl                           TN5645   Ressourcen geändert, User management verbessert
  13.01.12 ts                           TN5779   mnuFillDTR eingefügt
  20.04.12 wl  FormShow                 TN5858   Initialisierung von RunMainControls vereinfacht
  04.05.12 wl  FormShow                 TN5858   Animation endgültig endfernt
  09.05.12 wl  UserSetAccess            TN5458   Bei useLiquids = 0 wird kein Flush-Button angezeigt
  04.12.12 wl  ffrmMethodSelection      TN5960   Methodenauswahl als neues Hauptfenster-Element
  10.12.12 wl                           TN6045   neu: BuildingBlockEditor
  20.02.13 wl                           TN6055   uses geändert
  13.03.13 wl                           TN5960   ViewAllItems statt MethodSelection
  15.03.13 wl  FormCreate               TN5960   ruft ExpandMethodNode auf
  30.08.13 wl  FormShow                 TN6236   TreeView wird erst hier erzeugt (statt bei Create)
  18.09.13 wl  actToolsFlushExecute     TN6252.3 Aufruf von TEdExtern.Instance.ShowFlushDialog();
  24.01.14 tp  actToolsCalibrateExecute TN6341   Added Calibrate action, menu button and execute function
  03.04.14 ts  FormShow                 TN6387   new ntMethodEditableInRunner
  04.04.14 tp  FormCreate               TN6375   ViewItemWorkflowRunner mit int fBuildingBlockEditorMode Created statt bool fuer neu Building Block Editor
  08.04.14 ts                           TN6392   Setting (EnableCalibration) for Calibration
  08.04.14 ts                           TN6393   new MenuItem Edit table to edit tables from ZARunner
  -------------------------------------------------------------------------------------------------- }

unit ZARunnerMain;


interface


uses
    Windows,
    Messages,
    StdCtrls,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    Menus,
    DB,
    ExtCtrls,
    ComCtrls,
    Buttons,
    ImgList,
    Grids,
    cxStyles,
    cxCustomData,
    cxGraphics,
    cxFilter,
    cxData,
    cxDataStorage,
    cxEdit,
    cxGridCustomTableView,
    cxGridTableView,
    cxControls,
    cxGridCustomView,
    cxClasses,
    cxGridLevel,
    cxGrid,
    cxEditRepositoryItems,
    cxDropDownEdit,
    cxButtonEdit,
    ActnList,
    PlatformDefaultStyleActnCtrls,
    ActnMan,
    ToolWin,

    DelayInfo,
    ActionImages,
    ThreadUtils,
    RunInformation,
    RunMainControls,
    GeneralTypes,
    MainRun,
    AppTypes,
    BuildingBlockEditFunctions,
    StringLoader,
    DockableForm;

const
    WM_EXTERNALCLOSE = WM_APP + 10;

type
    TRunnerMainStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TMainForm = class(TForm)
        MainMenu1: TMainMenu;
        mnuFile: TMenuItem;
        mnuFileExit: TMenuItem;
        mnuMethod: TMenuItem;
        mnuMethodSelect: TMenuItem;
        mnuTools: TMenuItem;
        mnuToolsFlush: TMenuItem;
        mnuToolsInit: TMenuItem;
        mnuHelp: TMenuItem;
        mnuHelpAbout: TMenuItem;
        mnuUser: TMenuItem;
        mnuUserLogon: TMenuItem;
        mnuUserChangepassword: TMenuItem;
        N1: TMenuItem;
        mnuStartCurrentMethod: TMenuItem;
        StatusBar1: TStatusBar;
        pnlMain: TPanel;
        ActionManager1: TActionManager;
        actStartCurrent: TAction;
        actStartWithSelect: TAction;
        MainImageList: TImageList;
        actToolsFlush: TAction;
        ImagesButtons: TImageList;
        actToolsInit: TAction;
        mnuFillDTR: TMenuItem;
        pnlMethodSelection: TPanel;
        pnlDockRunMain: TPanel;
        ToolBar1: TToolBar;
        tbtnRestart: TToolButton;
        tbtnFlush: TToolButton;
        ToolButton1: TToolButton;
        Splitter1: TSplitter;
        actEditCut: TAction;
        actEditCopy: TAction;
        actEditPaste: TAction;
        mnuToolsCalibrate: TMenuItem;
        Action1: TAction;
        N2: TMenuItem;
        mnuEditTables: TMenuItem;
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure mnuFileExitClick(Sender: TObject);
        procedure mnuUserLogonClick(Sender: TObject);
        procedure mnuUserChangepasswordClick(Sender: TObject);
        procedure mnuHelpAboutClick(Sender: TObject);
        procedure actStartCurrentExecute(Sender: TObject);
        procedure actStartWithSelectExecute(Sender: TObject);
        procedure actToolsFlushExecute(Sender: TObject);
        procedure actToolsInitExecute(Sender: TObject);
        procedure actToolsCalibrateExecute(Sender: TObject);
        procedure actEditCutExecute(Sender: TObject);
        procedure actEditCopyExecute(Sender: TObject);
        procedure actEditPasteExecute(Sender: TObject);
        procedure mnuFillDTRClick(Sender: TObject);
        procedure Calibrate1Click(Sender: TObject);
        procedure mnuToolsCalibrateExecute(Sender: TObject);
    private
        fStarted: boolean;
        fExternalClose: boolean;
        fUseLiquids: boolean;
        fDefaultMethod: string;
        fMethodHistory: TStringArray; // array für Methodennamen aus DATAHISTORY
        ffrmMainRun: TfrmMainRun;
        fDllMenuItems: TDLLMenuItems;
        fTableEditMenuItems: TStringArray;
        fStringLoader: TRunnerMainStringLoader;
        fEditFunctions: TBuildingBlockEditFunctions;
        fBuildingBlockEditorMode: integer;

        procedure CreateDllMenuItems();
        procedure CreateEditTableMenuItems();
        procedure UserSetAccess;
        //
        procedure mnuMethodHistClick(Sender: TObject);
        procedure ExternalClose(const aMaxWaitTime: cardinal);
        procedure WMExternalClose(var vMsg: TMessage); message WM_EXTERNALCLOSE;
        procedure ExecuteDLLMenu(Sender: TObject);
        procedure ExecuteTableEdit(Sender: TObject);

        // Procedure für Menü-Klick
        procedure DoEditAction(aSender: TObject; aEditType: TEditActionType);
        procedure ExecuteMethodHistMenu(Sender: TObject);
        // Methode in DATAHISTORY schreiben
        // procedure WriteMethodToDataHistoryTable(aMethodName: string);
    public
        procedure PostClose(aSender: TObject);
        property EditFunctions: TBuildingBlockEditFunctions read fEditFunctions;
        procedure AfterSaveStatusChanged(Sender: TObject);
    end;

var
    MainForm: TMainForm;


implementation


{$R *.DFM}

uses
    ViewItem,
    ViewItemsWorkflow,
    ViewItemWorkflowRunner,
    CommonTypes,
    AppSettings,
    ControlUtils,
    GUIManager,
    EdExtern,
    LayoutManager,
    About,
    LogManager,
    DataHistoryDataAdaptor;

{ TRunnerMainStringLoader }

procedure TRunnerMainStringLoader.AddAllItems;
begin
    AddSingle(100, '&File', '&Datei');
    AddSingle(110, 'E&xit', '&Beenden');
    AddSingle(120, '&Help', '&Hilfe');
    AddSingle(130, '&About', '&Info');
    AddDouble(42090, 'C&ut', 'Cut|Move marked objekt into the clipboard', '&Ausschneiden',
        'Ausschneiden|Markiertes Objekt in die Zwischenablage verschieben');
    AddDouble(42100, '&Copy', 'Copy|Copy marked objekt into the clipboard', '&Kopieren',
        'Kopieren|Markiertes Objekt in die Zwischenablage kopieren');
    AddDouble(42110, '&Paste', 'Paste|Paste content of the clipboard', '&Einfügen',
        'Einfügen|Inhalt der Zwischenablage einfügen');
    AddSingle(62400, 'Start &method once again', '&Methode noch einmal starten');
    AddSingle(62450, 'Reschedule', 'Neu schedulen');
    AddSingle(62460, 'View Schedule Chart', 'Öffne Scheduleanzeige');
    AddSingle(62470, 'Sessio&n', 'Sessio&n');
    AddSingle(62500, '&Method', '&Methode');
    AddDouble(62520, 'Se&lect', 'Recently started methods', '&Auswahl', 'Zuletzt gestartete Methoden');
    AddSingle(62530, '&Start', '&Start');
    AddSingle(62580, '&Build', '&Run-Tabelle erstellen');
    AddSingle(62670, 'Select &Layout', '&Layout auswählen');
    AddSingle(62680, '&Flush System', '&System spülen');
    AddSingle(62700, '&Initialize', '&Initialisieren');
    AddSingle(62720, 'Fill &Disposal Tip Rack(s)', '&Wegwerfspitzen-Rack füllen');
    AddSingle(62770, '&Logon', '&Anmelden');
    AddSingle(62780, 'Change &Password', '&Passwort ändern');
    AddSingle(62820, '&Tools', 'E&xtras');
    AddSingle(62830, '&User', '&Benutzer');
    AddDouble(62850, '&Start', 'Select method and start', '&Start', 'Methode wählen und starten');
    AddDouble(62860, 'Start &method once again', 'Start method once again', '&Methode noch einmal starten',
        'Methode noch einmal starten');
    AddDouble(62870, '&Flush', 'Flush System', 'Spülen', 'System spülen');
    AddDouble(62880, '&Init', 'Init System', 'Initialisieren', 'System initialisieren');
    AddSingle(62890, 'What is my pipetting job today?', 'Was soll ich heute pipettieren?');
    AddSingle(62910, 'E&dit Run Table ', 'R&un-Tabelle bearbeiten');
    AddSingle(62920, 'Calibration', 'Kalibrierung');
    AddSingle(62930, 'Edit table', 'Editiere Tabelle');
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
    xIniAccess: IWinlissyIniAccess;
    xViewItemsWorkflowRunner: TViewItemsWorkflowRunner;
begin
    fStarted := true;
    fExternalClose := false;
    self.WindowState := wsMaximized;

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TRunnerMainStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    CreateDllMenuItems();
    CreateEditTableMenuItems();

    fEditFunctions := TBuildingBlockEditFunctions.Create;

    // self.mnuMethodHistClick(nil);  // ausgeblendet weil es nicht funktioniert

    xIniAccess := TAppSettings.CreateAppIni;
    fUseLiquids := xIniAccess.ReadBool('Display', 'UseLiquids');
    fDefaultMethod := xIniAccess.ReadString('Display', 'DefaultMethod');
    fBuildingBlockEditorMode := xIniAccess.ReadInteger('Display', 'BuildingBlockEditorMode');

    mnuToolsCalibrate.Visible := xIniAccess.ReadInteger('Display', 'EnableCalibration') = 1;

    xViewItemsWorkflowRunner := TViewItemsWorkflowRunner.Create(TfrmActionImages.Create(self),
        fBuildingBlockEditorMode);
    TViewItemsWorkflow.SetInstance(xViewItemsWorkflowRunner);

    // MainRun-Fenster komplett einfügen
    ffrmMainRun := TfrmMainRun.Create(self);
    ffrmMainRun.BorderStyle := bsNone;
    ffrmMainRun.Align := alClient;
    ffrmMainRun.Parent := pnlDockRunMain;
    ffrmMainRun.Visible := true;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
    FreeAndNil(fStringLoader);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
    if fStarted then
    begin
        // MethodSelection-Fenster komplett einfügen
        TViewItemsWorkflow.Instance.CreateOverviewInstance('Runner Items',
            [ntMethod, ntMethodEditableInRunner], 'Runner', self.pnlMethodSelection, true);

        TEdExtern.Instance.SetOnSafeAppClose(self.PostClose);

        TRunMainControls.Instance.SetMainFormControls(self, fDefaultMethod, fUseLiquids,
            self.actStartWithSelect, self.actStartCurrent, nil, self.actToolsFlush, self.actToolsInit,
            self.Action1, ffrmMainRun, mnuFile, mnuTools, mnuUser, mnuMethod, mnuFillDTR, pnlMethodSelection);

        TEdExtern.Instance.ResetGlobalName;
    end;

    UserSetAccess;
    fStarted := false;
end;

procedure TMainForm.DoEditAction(aSender: TObject; aEditType: TEditActionType);
// Sender is a TAction
begin
    { if Assigned(fCurrentEditor) then
      begin
      if fCurrentEditor.EditAction(frmEdMain.ActiveControl, aEditType) then
      EXIT;
      end;

      // Here we are assuming that the active control is actually the control for which we want to execute the edit Action
      // we will Execute the action onto the ActiveControl.
      if aSender is TAction then
      frmEdMain.ActiveControl.ExecuteAction(aSender as TAction); }
end;

procedure TMainForm.actEditCutExecute(Sender: TObject);
begin
    DoEditAction(Sender, eaCut);
end;

procedure TMainForm.actEditCopyExecute(Sender: TObject);
begin
    DoEditAction(Sender, eaCopy);
end;

procedure TMainForm.actEditPasteExecute(Sender: TObject);
begin
    DoEditAction(Sender, eaPaste);
end;

procedure TMainForm.actStartCurrentExecute(Sender: TObject);
begin
    TEdExtern.Instance.StartCurrentMethod;
end;

procedure TMainForm.actStartWithSelectExecute(Sender: TObject);
begin
    TEdExtern.Instance.StartMethod(true, fDefaultMethod);
end;

procedure TMainForm.actToolsFlushExecute(Sender: TObject);
begin
    if TEdExtern.Instance.ThreadIsRunning(false) then
        EXIT;

    TEdExtern.Instance.ShowFlushDialog();
end;

procedure TMainForm.actToolsInitExecute(Sender: TObject);
begin
    if TEdExtern.Instance.ThreadIsRunning(false) then
        EXIT;

    TEdExtern.Instance.InitSystem();
end;

procedure TMainForm.actToolsCalibrateExecute(Sender: TObject);
begin
    if TEdExtern.Instance.ThreadIsRunning(false) then
        EXIT;

    TEdExtern.Instance.ShowCalibrateDialog();
end;

// This is the calibrate button. That is enabled or disabled depending on user license
procedure TMainForm.Calibrate1Click(Sender: TObject);
begin
    // Show the Calibrate Dialog
    TEdExtern.Instance.ShowCalibrateDialog();

    // Calls the Calibrate function to launch a temporary method to calibrate the user's pumps
    // TEdExtern.Instance.Calibrate();
end;

procedure TMainForm.CreateDllMenuItems();
var
    xNewItem: TMenuItem;
    x: integer;
begin
    TEdExtern.ReadDllMenuItems(fDllMenuItems);

    // Erzeugen beliebig vieler Menüpunkte zum Aufruf von DLL-Funktionen
    for x := 0 to high(fDLLMenuItems) do
    begin
        xNewItem := TMenuItem.Create(self);
        xNewItem.Caption := fDLLMenuItems[x].MenuCaption;
        xNewItem.Name := fDLLMenuItems[x].MenuName;
        xNewItem.OnClick := ExecuteDLLMenu;
        mnuTools.Add(xNewItem);
    end;
end;

procedure TMainForm.CreateEditTableMenuItems;
var
    xNewItem: TMenuItem;
    x: integer;
begin
    TEdExtern.ReadEditTableMenuItems(fTableEditMenuItems);
    if Length(fTableEditMenuItems) <= 0 then
        mnuEditTables.Visible := false;

    // Erzeugen beliebig vieler Menüpunkte zum Aufruf von DLL-Funktionen
    for x := 0 to high(fTableEditMenuItems) do
    begin
        xNewItem := TMenuItem.Create(self);
        xNewItem.Caption := fTableEditMenuItems[x];
        xNewItem.Name := fTableEditMenuItems[x];
        xNewItem.OnClick := ExecuteTableEdit;
        mnuEditTables.Add(xNewItem);
    end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    // we have to do this here. if we do it in applicationshutdown the background panel has alread been destroyed and causes AV.
    TLayoutManager.Instance.UnregisterCurrentLayout();
    TLayoutManager.Instance.DestroyDefaultSceneGraphics

end;

// ==============================================================================
// Menus

procedure TMainForm.mnuFileExitClick(Sender: TObject);
begin
    self.Close();
end;

procedure TMainForm.mnuFillDTRClick(Sender: TObject);
begin

end;

// ==============================================================================
// HELP Menu Item
// --------------------------------------------------------------------------------------------------
procedure TMainForm.mnuHelpAboutClick(Sender: TObject);
// ------------------------------------------------------------------------------
var
    xAboutBox: TAboutBox;
begin
    xAboutBox := TAboutBox.Create(nil);
    try
        xAboutBox.ShowModal;
    finally
        xAboutBox.Free;
    end;
end;

// ==============================================================================
// USER Menu Item
// --------------------------------------------------------------------------------------------------
procedure TMainForm.mnuUserLogonClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
begin
    // system is not running, edit forms may be open
    gCommonDll.ChangeCurrentUser;
    UserSetAccess;
    TEdExtern.Instance.ReloadValues;
end;

// --------------------------------------------------------------------------------------------------
procedure TMainForm.mnuUserChangepasswordClick(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    xUser: IUser;
begin
    xUser := gCommonDll.CurrentUser;
    xUser.ChangePassword;
end;

procedure TMainForm.UserSetAccess;
var
    xUser: IUser;
begin
    xUser := gCommonDll.CurrentUser;
    StatusBar1.Panels[0].Text := xUser.Name;

    // Guest Access
    StatusBar1.Panels[1].Text := 'Guest Access: Load Layout, view data, no editing, only simulation mode';

    self.actToolsFlush.Visible := false;
    self.actToolsInit.Enabled := false;

    // System Access
    if gCommonDll.LevelIsIncluded(xUser.Level, usrSystem) then
    begin
        StatusBar1.Panels[1].Text := 'System User Access: Start Runs, no editing';
        self.actToolsFlush.Visible := fUseLiquids;
        self.actToolsInit.Enabled := true;
    end;

    // System Administrator access (CFR 21 complient)
    if gCommonDll.LevelIsIncluded(xUser.Level, usrSystemAdmin) then
    begin
        StatusBar1.Panels[1].Text := 'System Administrator Access: Start runs, change data';
    end;

    // Administrator access (not CFR 21 complient)
    if gCommonDll.LevelIsIncluded(xUser.Level, usrUnlimitedAdmin) then
    begin
        StatusBar1.Panels[1].Text := 'Administrator Access: Start runs, change data';
    end;

    // CFR21 Prepare Mode
    if (xUser.CFR21Mode = TCFR21ComplianceMode.ccmPrepared) then
    begin
        StatusBar1.Panels.Add;
        StatusBar1.Panels[2].Text := '--- CFR21 PART 11 PREPARE MODE ---';
    end;

    // CFR21 Compliant Mode
    if (xUser.CFR21Mode = TCFR21ComplianceMode.ccmCompliant) then
    begin
        StatusBar1.Panels.Add;
        StatusBar1.Panels[2].Text := '--- CFR21 Part 11 Compliant ---';
    end;
end;

procedure TMainForm.AfterSaveStatusChanged(Sender: TObject);
begin
    //
end;

// --------------------------------------------------------------------------------------------------
procedure TMainForm.ExecuteDLLMenu(Sender: TObject);
// --------------------------------------------------------------------------------------------------
var
    x: integer;
begin
    if not(Sender is TMenuItem) then
        EXIT;

    for x := 0 to high(fDLLMenuItems) do
    begin
        if (fDLLMenuItems[x].MenuName <> (Sender as TMenuItem).Name) then
            CONTINUE;
        TEdExtern.Instance.ExecuteDllCall(fDLLMenuItems[x].DLLName, fDLLMenuItems[x].DLLFunction,
            fDLLMenuItems[x].Parameter);
    end;
end;

procedure TMainForm.ExecuteTableEdit(Sender: TObject);
var
    x: integer;
begin
    if not(Sender is TMenuItem) then
        EXIT;

    for x := 0 to high(fTableEditMenuItems) do
    begin
        if (fTableEditMenuItems[x] <> (Sender as TMenuItem).Name) then
            CONTINUE;
        if TEdExtern.Instance.ThreadIsRunning(false) then
            EXIT;
        TEdExtern.Instance.ShowTableEditor(fTableEditMenuItems[x]);
    end;
end;

procedure TMainForm.PostClose(aSender: TObject);
begin
    // Post a close message -> this will result in the Application Main Thread calling MainForm.Close
    PostMessage(self.Handle, WM_CLOSE, 0, 0);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
// Note: This function is always called internally by the TForm class just before closing, and before FormClose
// read TForm.OnCloseQuery for more info
// If CanClose is set to false the form close is cancelled
begin
    CanClose := true;
    if (not fExternalClose) and TEdExtern.Instance.AnyProcessesRunning() then
    begin
        if gGUIManager.MessageBox(TLanguageString.
            Read('One or more processes are active.  Would you like to abort all processes and exit the program?',
            'Ein oder mehrere Prozesse sind aktiv.  Möchten Sie alle Prozesse abbrechen und die Anwendung beenden?'),
            TLanguageString.Read('Processes Active', 'Prozesse Aktiv'), MB_ICONSTOP + MB_OKCANCEL)
            = mrCancel then
        begin
            CanClose := false;
            EXIT;
        end;
    end;
end;

procedure TMainForm.ExternalClose(const aMaxWaitTime: cardinal);
begin
    gLogManager.Log
        ('Another application has sent a Safe Exit request. This application will close automatically when all running actions have completed.',
        false);

    // if a Request has not yet been made then make a request to close
    if TEdExtern.Instance.AnyProcessesRunning() then
    begin
        fExternalClose := true;
        // start a thread which ensures a "Safe" close
        TEdExtern.Instance.RequestSafeAppClose(aMaxWaitTime);
    end
    else
    begin
        self.Close();
    end;
end;

procedure TMainForm.WMExternalClose(var vMsg: TMessage);
begin
    self.ExternalClose(vMsg.WParam);
end;

procedure TMainForm.mnuMethodHistClick(Sender: TObject);
var
    xHistDA: TDataHistoryDataAdaptor;
    i, x: integer;
    xNewItem, xMethodHist: TMenuItem;
begin
    xNewItem := nil;
    xHistDA := TDataHistoryDataAdaptor.Create;
    try
        fMethodHistory := xHistDA.ReadHistoryDataset(STR_DEF_HISTNAME_METHOD);
        // liest Methodennamen aus DATAHISTORY
        if fMethodHistory = nil then
            EXIT; // wenn nichts vorhanden ist, dann wird auch kein Menüpunkt erzeugt

        xMethodHist := mnuMethod.Find(TLanguageString.Read('Recently started methods',
            'Zuletzt gestartete Methoden')); // Menüeintrag wird gesucht
        if xMethodHist = nil then
        begin // falls Menüeintrag nicht gefunden wurde und Einträge in DATAHISTORY
            xMethodHist := TMenuItem.Create(self); // vorhanden sind, wird der Menüpunkt erstellt
            xMethodHist.Name := 'mnuMethodHist';
            xMethodhist.Caption := TLanguageString.Read('Recently started methods',
                'Zuletzt gestartete Methoden');
            xMethodHist.OnClick := self.mnuMethodHistClick;
            mnuMethod.Add(xMethodHist);
        end;

        for i := 0 to high(fMethodHistory) do
        begin // für jeden Eintrag in DATAHISTORY wird ein Untermenüpunkt erzeugt
            for x := 1 to high(fMethodHistory) + 1 do
            begin // für jede mögliche Nummer wird überprüft, ob der ..
                xNewItem := xMethodHist.Find(IntToStr(x) + ': ' + fMethodHistory[i]);
                // .. Menüeintrag bereits vorhanden ist
                if xNewItem <> nil then
                    BREAK;
            end;
            if xNewItem = nil then
            begin // wenn kein Menüeintrag mit dem Methodennamen existiert,
                xNewItem := TMenuItem.Create(Self); // dann wird ein neuer Eintrag erstellt
                xNewItem.Caption := IntToStr(i + 1) + ': ' + fMethodHistory[i];
                xNewItem.OnClick := self.ExecuteMethodHistMenu; // OnClick Ereigniszuweisung
                xMethodHist.Add(xNewItem); // Menüpunkt zum übergeordneten Menü zuweisen
                xNewItem.MenuIndex := i; // Menüindex entsprechend dem Eintrag aus DATAHISTORY
            end
            else
            begin
                xNewItem.MenuIndex := i; // wenn Eintrag schon existiert, dann wird nur der Index geändert
                xNewItem.Caption := IntToStr(i + 1) + ': ' + fMethodHistory[i];
            end;
        end;
        if xMethodHist.Count > INT_NUMBER_OF_HISTORY_METHODS then
            // falls mehr als X Menüeinträge vorhanden sind, wird der 11. gelöscht
            xMethodHist.Delete(INT_NUMBER_OF_HISTORY_METHODS);
    finally
        xHistDA.Free;
    end;
end;

procedure TMainForm.mnuToolsCalibrateExecute(Sender: TObject);
begin
    if TEdExtern.Instance.ThreadIsRunning(false) then
        EXIT;

    TEdExtern.Instance.ShowCalibrateDialog();
end;

procedure TMainForm.ExecuteMethodHistMenu(Sender: TObject);
var
    xMethodName: string;
begin
    // starten der Methode mit dem entsprechenden Methodennamen, Auswahl aus Menü über den Menüindex
    xMethodName := fMethodHistory[(Sender as TMenuItem).MenuIndex];
    TEdExtern.Instance.MethodStart(xMethodName, false, false);
end;

{
  procedure TMainForm.WriteMethodToDataHistoryTable(aMethodName: string);
  var
  xHDA: TDataHistoryDataAdaptor;
  i: integer;
  xString1, xString2: string;
  begin
  if fMethodHistory = nil then
  SetLength(fMethodhistory, 1); // wird nur ausgeführt beim allerersten Eintrag in DATAHISTORY

  xString1 := fMethodHistory[0]; // der erste Menüpunkt wird zwischengespeichert
  fMethodHistory[0] := aMethodName; // die aktuelle Methode wird als erster Menüpunkt eingetragen

  for i := 1 to high(fMethodHistory) do
  begin // alle anderen Einträge müssen um eins verschoben werden
  xString2 := fMethodHistory[i];
  // der nächste Eintrag wird zwischengespeichert, damit er mit dem vorherigen überschrieben werden kann
  if xString1 <> aMethodName then
  begin // falls aktuelle Methodenname dem zu überschreibenden entspricht wird abgebrochen
  fMethodHistory[i] := xString1; // ansonsten werden alle Einträge um eins verschoben
  xString1 := xString2;
  end
  else
  Break;
  end;

  if ( high(fMethodHistory) < (INT_NUMBER_OF_HISTORY_METHODS - 1)) and
  ((xString1 <> aMethodName) and (xString1 <> '')) then
  begin
  SetLength(fMethodHistory, high(fMethodHistory) + 2);
  // wenn weniger als die maximal möglichen Einträge vorhanden sind und
  fMethodhistory[ high(fMethodHistory)] := xString1;
  // die aktuelle Methode noch nicht vorhanden ist, wird sie an neuer letzter
  end; // Stelle eingetragen

  xHDA := TDataHistoryDataAdaptor.Create;
  try
  xHDA.UpdateDataHistory(STR_DEF_HISTNAME_METHOD, fMethodHistory); // DATAHISTORY wird aktualisiert
  self.mnuMethodHistClick(self);
  finally
  xHDA.Free;
  end;
  end;
}


end.
