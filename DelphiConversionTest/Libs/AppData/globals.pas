{ --------------------------------------------------------------------------------------------------
  Ebene 1b (Sam-Globals)
  --------------------------------------------------------------------------------------------------
  Versions- und Lizenz-Information der WinLissy-Projekte:  Sampler, Editor und Layouter
  --------------------------------------------------------------------------------------------------
  Datum     op  function/procedure     Änderung / Neuerung
  --------  --  -------------------    -------------------------------------------------------------
  13.11.97  DL  Liquid Parameter       TransAirPos            : integer;
  DispSubmerge           : integer;
  DilAspSwitchPortNumber : integer;
  DilAspSwitchPos        : integer;

  gelöscht: DispPerFomSteps
  SampleAspGasSwitch
  DilAspGasSwitch
  DispGasSwitch
  25.11.97  DL  Liquid Parameter       Umstellung aller Delay Parameter auf Single
  26.11.97  DL  RESIN Dispense         neue gobale Variable ResinDispense
  InitGlobals            wenn ReadInteger('RESIN','ResinTipVolume',0) > 0
  dann ResinDispense = true (Sampler.Ini);
  08.12.97 hc+dl Wash                  von samintf.pas verschoben
  FlushPeripump          dito
  Washtips               dito

  GetDilNo               Umstellung 6wayValve Schleife von 1 bis 6 statt 0..5
  InitGlobals            dito
  Turn6WayValve          dito
  12.12.97 mo                          == Version auf 3.1 gesetzt
  15.12.97 mo   InitGlobals            Neue Variable ShowRunTable
  Wenn in Sampler.ini [Display] ShowRunTable=1 dann wird die RunDB
  während des Laufs angezeigt
  02.01.98 mo  InitGlobals             Neue Variable ShowRunTable
  Wenn in Sampler.ini [Display] ShowAnimation=1 dann wird die Animation
  während des Laufs angezeigt
  06.01.98 mo  Wash                    Neuer Parameter = UsePeripump:Boolean
  InitSampler             Neue Funktion
  Turn6WayValve           Wird nur durchlaufen wenn Ventil existiert
  07.01.98                             Variable PDBPath von ALIAS SAMINTF umgestellt auf Alias SAMPLER
  08.01.98 mo  Wash                    Aufruf von SamAppsleep anstatt sleep(..)
  09.01.98 mo  GetDilNo                Abfrage auf 1..6 geändert
  == Version auf 3.13 gesetzt
  14.01.98 dl  Turn6wayValve           ActPos auch setzen wenn Moduladresse negativ ist;
  14.01.98 mo                          == Version auf 3.14 gesetzt
  20.01.98 HC  INITGlobals             Der Commport wird jetzt mit eingelesen. Ist er <>0 wird das Omnifitventiel unterstützt
  20.01.98 HC  TURN6WayValve           Das Omnifit6Way Ventil wird ab dieser Version unterstützt
  20.01.98 HC  FlushPeripump           System.break wird anstatt break aufgerufen, da in supercom break eine Konstate ist
  02.02.98 mo                          == Version auf 3.15 gesetzt
  09.02.98 mo  Turn6WayValve           Abfrage auf Port = Actport
  13.02.98                             == Version 3.15.1
  17.02.98                             == Version 3.15.2
  26.03.98 mo  InitGlobals             auslesen der ToolDll in Try/exceptBlock
  == Version 3.15.3
  05.05.98 wl                          Neuer Typ: typDLLMenuPrt (interface-Deklaration)
  InitGlobals             Übernehmen beliebig vieler Tool-Menu-Einträge in eine verkettete Liste
  SetDLLMenuList          neu:Funktion zur Übernahme eines Tool-Menu-Eintrags
  15.05.98  mo                          == Version 3.2.0 Build 980515
  20.05.98 mo                                            Build 980520
  09.07.98 wl                           var Restart gelöscht
  ---------------------------------------------------------------------------------------
  |   ACHTUNG: Für alle Versionen, die zwischen 11.05.98 und 03.06.98 ausgeliefert wurden:
  |   ---> Änderungen im DM_SAMPL verursachten Fehler bei <SaveAs> und <MultiCopy>
  ---------------------------------------------------------------------------------------
  03.06.98 wl                           MultiCopy deaktiviert:  Build 980603
  Fehler behoben:         Build 980603a

  15.06.98 mo                           == Neue ß-Version 3.3.0ß
  mo  Wash                     Abwurf der Wechselspitzen integriert
  17.06.98 mo  WashTips                        "
  18.06.98 wl                           Definition und Zuweisung von PAppPath und PDataPath
  entfernt, da in SamGlobe bereits vorhanden
  -------------------------------------------------------------------------------
  ab 23.06.98: projektbezogene globale Typen, Variablen und Methoden
  -------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    -------------------------------------------
  23.06.98 wl  InitSampler            --> Samintf
  SamplerActive          --> Samintf
  InitSampler6Way        --> SamWash
  Turn6WayValve          --> SamWash
  InitIntf6WayValve      --> SamWash
  GetDilNo               --> SamWash
  Wash                   --> SamWash
  WashTips               --> SamWash
  var Valve6Way,Valve1   --> SamWash
  var ShowRunFormOpen,PLayoutName,PMethodName,PdbPath,SamplerThreadRunning,
  Delay_sec          --> SamGlobe
  var tmpPChar,PScript,PAuftFile,PErgFile,PSamplerFile,FlushFile,
  PGemFile,RunScript,LayoutDefFormOpen,RunAction --> gelöscht (nutzlos)
  TDLLMenu = typDLLMenu
  neu: var Restart (aus main)
  25.06.98 wl                         == Version 3.3.0, Build 980625
  29.06.98 mo                         == Build 980629 enthält DDE Funktionen
  13.07.98 wl                         neu: TMethodSampler
  TMethodSampler.StartSamplerThread  Starten des Sampler-Threads
  15.07.98 wl StartSamplerThread      Übergabe von RunModes
  SendDDEStatus           Leitet string als DDEStatus weiter
  05.08.98 wl                         == Version 3.4.0, Build 980805
  03.09.98 wl                         == Version 3.4.0, Build 980903
  16.09.98 wl                         == Version 3.4.0, Build 980916
  16.09.98 wl                         Version 4.0:
  InitProject            ShowAnimation,ShowRunTable --> ObjSampl
  InitProject            Bestimmen des Alias Pfadnamens nach DBRack verschoben
  StartSamplerThread     getrennte Behandlung von LayoutRun,Method,Script
  ChangeGlobalName       neu: Ändern des globalen (Methoden-, Run-, Script-)Namens
  StartSamplerThread     TSamplerThread -> TLissyThread
  21.09.98 wl                         var SamplerErgFound,MethodisDone,TipCnt gelöscht (sinnlos)
  22.09.98 wl                         == Version 4.0 Build 980922 - 'heißer' Vorab-Build für Thomas
  23.09.98 wl  StartSamplerThread     Logdisplay verbessert (Build 980922a)
  23.09.98 wl  InitProject            LogText mit Versionsangabe
  Build 980922b (6-Wege-Ventil funktionsfähig)
  24.09.98 wl  StartSamplerThread     alle Aufrufe um SchedHandlePtr erweitert
  25.09.98 wl                         MethEditFormOpen gelöscht
  28.09.98 wl  RunCreate              Erzeugt Runtable, Aufruf von gmMethodRunCreate
  RunNameAllowed         vergleicht Run- und GlobalName
  29.09.98 wl  ChangeGlobalName       mit Zusatzparameter: Reload
  30.09.98 wl  NameAllowed            zur Verhinderung, daß Methoden und Scripte gleich heißen
  01.10.98 wl                         TMethodSampler --> Objects
  05.10.98 wl                         entfernt: ShowRunFormOpen, LiqHFormOpen
  InitProject            with IniFile entfernt
  04.11.98 wl  SetDLLMenuList         gmReadSubStr statt ReadField
  20.11.98 wl                         uses Utility2 statt Utility
  = Build 981120
  01.12.98 mo                         == Build 981201 DispTip durch DropTipMap ersetzt
  Negativer SubMerge wird erlaubt
  08.12.98 wl                         == Version 4.0.1 Build 981208 (benötigt sampler.dll 1.5)
  17.12.98 mo                         == Version 4.0.1 Build 981217
  13.01.99 mo                         == Version 4.0.1 Build 990113
  26.01.99 wl                         == KV 4.0.2 Build 990127 (Änderungen beim Mixen f. Jans,AgrEvo)
  04.02.99 wl                         == KV 4.0.2 Build 990204
  09.02.99 wl                         const PFirstTip=1 und PLastTip=4 entfernt
  17.02.99 wl                         neu: var gVirtualRackMove
  24.02.99 mo  InitProject            Lesen der SerienNummer gSerialNo
  25.02.99 mo                         wieder entfernt
  Const Lizenz           zur Variblen geändert - wird hier initialisiert + in Main gesetzt
  == KV 4.0.2 Build 990225
  03.03.99 mo  Const SamplerIniFile   neu
  == KV 4.0.3 Build 990303
  08.03.99 wl                         Const SamplerIniFile redundant - gelöscht
  09.03.99 mo                         == KV 4.0.3 Build 990309
  17.03.99 mo                         == KV 4.0.3 Build 990317
  26.03.99 wl                         TMethodRec (aus DM_Sampl), Racknamen auf 30 Zeichen erhöht
  01.04.99 mo                         == KV 4.0.4 Build 990401
  14.04.99 hc                         Corridorxmoveoffset wird aus Inifile mit ausgelesen !
  09.06.99 mo                         == KV 4.1.0 Build 990609
  12.07.99 wl  InitProject            Corridor_XMoveOffset --> SamStart
  14.07.99 wl                         == KV 4.1.1 Build 990714
  28.07.99 mo                         == KV 4.1.2 Build 990728
  12.08.99 wz                         --> Strings in Ressourcen
  08.09.99 wl                         == KV 4.2.0 Build 990908
  ------------------------------------------ EV 4.3.0 bzw. 5.0.0
  10.09.99 wl                         gSerialNo,Lizenz,ResinDispense --> SamGlobe: gLicence
  01.10.99 wl                         == KV 4.3.0 Build 991001
  11.11.99 mo                         == Version 5.0. ß
  Neue Funktionen Parser.. aus SamPars.dll
  TMethodRec             alle Felder in Strings umgewandelt
  TSamParsStr            neu für Parserfunktionsaufrufe
  25.11.99 mo                         Build 991124, gMaxLoopNes=10
  30.11.99 mo  TSamParsStr            nach SamGlobe
  Parserfunktionen       nach SamIntf
  14.12.99 mo  InitProject            neue globale Variable gRunCreateSeqAuto ( automatisches Erstellen der RunSEQ Nummern )
  17.12.99 wl                         globals wird gemeinsame Datei für WinLissy und Layouter
  04.01.00 mo  InitProject            Ini Einträge für Oem Texte
  07.01.00 mo                         Build 000107
  09.02.00 wl                         gSerComControl: Soll das Serial Port Control angezeigt werden?
  07.03.00 wl                         == KV 5.0.1 Build 307 (WinLissy & Layouter)
  08.03.00 mo                         == KV 5.0.1 Build 308 (WinLissy & Layouter)
  20.03.00 mo                         == KV 5.0.2 Build 320
  22.03.00 mo                         == KV 5.0.2 Build 322
  06.04.00 mo                         == KV 5.0.2 Build 406 (mit Calli-Neuerungen)
  10.04.00 mo                         == KV 5.0.2 Build 410
  20.04.00 mo                         == KV 5.0.3 Build 420
  20.04.00 mo                         neu  gRunCreatePaint schaltet Paint während des builds ab
  17.05.00 mo                         == KV 5.0.3 Build 517
  26.05.00 wl                         [Display] 'SerComControl' wird nicht mehr benutzt
  05.06.00 wl  InitProject,SetDLLMenuList --> LisGlobe.pas (ehm. dbMTools)
  05.06.00 wl  alle Variablen,Typen außer Version,Build,gOEMText --> ScheUtil
  05.06.00 wl  gLicence               aus SamGlobe hierher: AddTitle raus UseScript rein (aus ObjSampl)
  05.06.00 wl  InitWinLissyProjects   einheitliches Auswerten der Seriennummer für alle WinLissy-Projekte
  05.06.00 wl                         == KV 5.1.0 Build 605 (WinLissy & Layouter & Editor)
  20.06.00 wl  InitWinLissyProjects   Namensgebung korrigiert
  26.06.00 wl                         == KV 5.1.0 Build 626 (WinLissy & Layouter & Editor)
  24.07.00 wl                         == KV 5.1.0 Build 724 (WinLissy & Layouter & Editor)
  26.07.00 wl                         == KV 5.1.0 Build 726 (WinLissy) - mit Vortexer-Init
  14.08.00 wl                         == KV 5.1.1 Build 814 (WinLissy & Layouter & Editor)
  17.08.00 mo                         == KV 5.1.2 Build 817 (WinLissy & Layouter & Editor)
  21.08.00 mo                         == KV 5.1.3 Build 821 (WinLissy & Layouter & Editor)
  30.08.00 wl                         == KV 5.1.4 Build 830 (WinLissy & Layouter & Editor)
  06.09.00 wl                         == KV 5.2.0 Build 906 (WinLissy & Layouter & Editor)
  11.09.00 wl                         == KV 5.2.0 Build 911 (nur Editor) -> für Innotides
  12.09.00 wl                         == KV 5.2.0 Build 912 (WinLissy & Layouter & Editor)
  14.09.00 mo                         == KV 5.2.1 Build 914 (WinLissy & Layouter & Editor)
  15.09.00 mo                         == KV 5.2.1 Build 915 (WinLissy & Layouter & Editor)
  19.09.00 mo                         == KV 5.2.1 Build 919 (WinLissy & Layouter & Editor)
  21.09.00 tbh                        == KV 5.2.1 Build 921 (WinLissy & Layouter & Editor)
  25.10.00 tbh                        == KV 5.2.2 Build 1025 (WinLissy & Layouter & Editor)
  31.10.00 tbh                        == KV 5.2.2 Build 1031 (WinLissy & Layouter & Editor)
  14.11.00 mo                         == KV 5.3.0 Build 1114 (WinLissy & Layouter & Editor)
  21.11.00 mo                         == KV 5.3.0 Build 1121 (WinLissy & Layouter & Editor)
  07.12.00 mo                         == KV 5.3.0 Build 1207 (WinLissy & Layouter & Editor) ADO entfernt
  18.12.00 tbh InitWinLissyProjects   neu Lizenz für SAMI, OEM-Titel kommen in Vordergrund
  18.12.00 tbh                        == KV 5.3.0 Build 1218 (WinLissy & Layouter & Editor)
  19.12.00 mo                         == KV 5.3.0 Build 1219 (WinLissy)
  11.01.01 mo                         == KV 5.3.0 Build 010111 (WinLissy)
  19.01.01 mo                         OemText erweitert
  19.01.01 mo                         == KV 5.3.1 Build 010119 (WinLissy
  23.01.01 mo                         == KV 5.3.1 Build 010123 (WinLissy)
  30.01.01 tbh                        == KV 5.3.1 Build 010130 (WinLissy)
  06.02.01 tbh                        == KV 5.3.2 Build 010206 (WinLissy)
  12.02.01 mo                         == KV 5.3.2 Build 010212 (WinLissy)
  20.02.01 mo                         == KV 5.3.3 Build 010220 (WinLissy & Layouter)
  14.03.01 tbh                        == KV 5.3.3 Build 010314 (WinLissy & Layouter)
  29.03.01 mo                         == KV 5.3.4 Build 010329 (WinLissy & Layouter)
  04.03.01 mo                         == KV 5.3.4 Build 010404 (WinLissy)
  25.04.01 mo                         == KV 5.4.0 Build 010425 (WinLissy & Layouter)
  14.05.01 tbh                        == KV 5.4.0 Build 010514 (WinLissy)
  22.05.01 tbh                        == KV 5.4.1 Build 010522 (WinLissy & Layouter)
  22.06.01 mo                         == KV 5.4.1 Build 010622 (WinLissy)
  27.06.01 mo                         == KV 5.4.2 Build 010628 (WinLissy)
  04.07.01 mo                         Testversion
  30.07.01 tbh                        == KV 5.4.3 Build 010730 (WinLissy & Layouter)
  08.08.01 tbh                        == KV 5.4.3 Build 010808 (WinLissy)
  16.08.01 mo                         == KV 5.4.4 Build 010816 (WinLissy)
  23.08.01 mo                         == KV 5.4.4 Build 010823 (WinLissy) TN1018
  28.08.01 mo                         == KV 5.4.4 Build 010828 (WinLissy)
  29.08.01 mo                         == KV 5.4.4 Build 010829 (WinLissy)
  12.09.01 tbh                        == KV 5.4.5 Build 010912 (WinLissy)
  20.09.01 tbh                        == KV 5.4.5 Build 010920 (WinLissy)
  13.10.01 mo                         == KV 5.4.6 Build 011013 (WinLissy & Layouter)
  13.11.01 tbh                        == KV 5.5.0 Build 011113 (WinLissy & Layouter)
  28.11.01 tbh                        == KV 5.5.0 Build 011128 (WinLissy)
  05.12.01 mo                         == KV 5.5.1 Build 011205 (WinLissy)
  11.12.01 mo                         == KV 5.5.1 Build 011211 (WinLissy)
  29.12.01 tbh                        == TV 5.5.2 Build 011229 (WinLissy & Layouter)
  03.01.02 tbh                        == TV 5.5.2 Build 020103 (WinLissy)
  23.01.02 tbh                        == TV 5.5.2 Build 020123 (WinLissy & Layouter)
  05.02.02 tbh                        == TV 5.5.2 Build 020205 (WinLissy)
  06.02.02 tbh                        == TV 5.5.2 Build 020206 (WinLissy)
  12.02.02 tbh                        == KV 5.5.2 Build 020212 (WinLissy & Layouter)
  21.02.02 mo                         == KV 5.5.2 Build 020221 (WinLissy)
  05.03.02 mo                         == TV 5.5.3 Build 020305 (WinLissy)
  06.03.02 mo                         == KV 5.5.3 Build 020306 (WinLissy)
  11.03.02 tbh                        == KV 5.5.3 Build 020311 (WinLissy)
  20.03.02 tbh                        == KV 5.5.3 Build 020320 (WinLissy)
  26.03.02 tbh                        == TV 5.6.0 Build 020326 (WinLissy & Layouter)
  03.05.02 tbh                        == KV 5.6.0 Build 020503 (WinLissy & Layouter)
  03.05.02 tbh                        == TV 5.6.1 Build 020503 (WinLissy)
  08.05.02 mo                         == TV 5.6.1 Build 020508 (WinLissy)
  26.06.02 tbh                        == TV 5.6.2 Build 020626 (WinLissy & Layouter & Editor)
  13.07.02 tbh                        == TV 5.6.2 Build 020713 (WinLissy & Layouter & Editor)
  19.07.02 tbh                        == KV 5.6.2 Build 020719 (WinLissy & Layouter & Editor)
  23.07.02 tbh                        == KV 5.6.2 Build 020723 (WinLissy & Layouter)
  04.09.02 mo                         == TV 5.7 ß
  09.09.02 mo                         == KV 5.7.0 build 020909 (WinLissy & Layouter & Editor)
  20.09.02 mo                         == KV 5.7.1 build 200909 (WinLissy & Layouter & Editor)
  20.09.02 mo                         == KV 5.7.1 build 020920 (WinLissy & Layouter & Editor)
  25.09.02 wl                         == KV 5.7.1 build 020925 (Layouter)
  26.09.02 wl                         TN1283 Application-Name für XAP fest drin
  09.10.02 mo			     == KV 5.7.1 build 021009 [SIAS 1.0.1] (WinLissy & Layouter & Editor)
  10.10.02 wl                         TN1293.2 Ini Access - uses geändert
  15.10.02 mo InitWinLissyProjects    TN1301 gmLogtext gelöscht wegen Zugriffsverletzung ifdef SIAS
  15.10.02 mo                         == ß Testversion
  18.10.02 wl                         TN1293.1 alle Typen/Variablen --> TAppSettings
  15.10.02 wl gmInitApplication       TN1293.1 PAppPath, PDataPath werden nicht mehr gesetzt
  25.10.02 wl PrepareIniAccess        TN1293.1 Alle Ini-Daten werden konvertiert !!!!
  06.11.02 wl PrepareIniAccess        TN1293.1 zunächst nur File Access
  14.11.02 wl gmInitApplication       TN1328.1 TAppSettings.UseScheduler wird gesetzt
  15.11.02 wl			     == KV 5.7.2 build 021115 [XAP 1.0.2] (Sampler,Layouter,Editor,XAP,Deckmate,Siasrobot,Siaslayouter)
  11.12.02 mo                         == TV 5.7.3 ß
  18.12.02 tbh                        == KV 5.7.3 build 021218 (Sampler & Layouter & Editor)
  20.12.02 wl                           TN1293.5 uses und WinlissyIniAccess geändert
  30.12.02 wl  PrepareIniAccess         TN1293.5 ab Version 5.8: Umstellung des Ini-Access
  04.01.03 wl  gmInitApplication        TN1334.1 geänderter Aufruf von InitdataServer
  09.01.03 wl  PrepareIniAccess         TN1293.5 --> ZACommon.dll
  09.01.03 wl  gmInitApplication        TN1334.1 Konvertier-Methoden --> ZACommon.dll
  04.03.03 wl                         == KV 6.0.0 build 030307
  11.03.03 mo                         == KV 6.0.0 build 030311
  31.03.03 mo                         == TV 6.0.2 build 0304xx
  08.04.03 wl  gmInitApplication        TN1293.5 --> AppSettings
  15.04.03 tbh                        == KV 6.0.2 build 030415
  17.04.03 wl                         == KV 6.0.2 build 030417  (CFR21 Compliant)
  28.04.03 wl                         TN1473.1 == KV 6.0.3 (Common Types geändert)
  04.09.03 wl                         == KV 6.0.4 Build 030904
  08.10.03 wl                         == KV 6.0.5 Build 031008
  13.10.03 mo                         == KV 6.0.5 Build 031013
  23.10.03 mo                         == KV 6.0.5 Build 031023
  12.11.03 mo                         == KV 6.0.5 Build 031112
  21.11.03 wl                         == KV 6.0.5 Build 031121
  27.11.03 mo                         == KV 6.0.5 Build 031127
  10.12.03 mo                         == KV 6.0.5 Build 031210
  12.12.03 tbh                        == KV 6.0.5 Build 031212
  27.02.04 pk                         == KV 6.1.0 Build 040226
  05.03.04 wl                         == KV 6.1.0 Build 040305
  17.03.04 wl                         == KV 6.1.0 Build 040317
  25.05.04 wl                         == KV 6.1.1 Build 040525
  07.06.04 tbh                        == KV 6.1.1 Build 040607
  17.06.04 wl                         == TestVersion 6.1.2
  05.07.04 pk                         == KV 6.1.2 Build 040702
  23.07.04 tbh                        == KV 6.1.2 Build 040723
  06.08.04 wl                         == KV 6.1.2 Build 040806
  11.08.04 tbh                        == KV 6.1.2 Build 040811
  13.08.04 tbh                        == KV 6.1.2 Build 040813
  19.08.04 wl                         == KV 6.1.2 Build 040819
  31.08.04 wl                         == TestVersion 6.1.3
  03.09.04 wl                         == KV 6.1.3 Build 040903
  09.09.04 mo                         == KV 6.1.3 Build 040909
  16.09.04 wl                         == KV 6.1.3 Build 040916
  22.09.04 mo                         == KV 6.1.3 Build 040922
  24.09.04 mo                         == KV 6.1.3 Build 040924
  14.10.04 mo                         == KV 6.1.3 Build 041014
  03.11.04 mo                         == KV 6.1.4 build 041103
  10.11.04 wl                         == KV 6.1.4 build 041110
  11.11.04 wl                         == TestVersion 6.1.5 build 041110 -->
  24.11.04 tbh                        == KV 6.1.5 build 041124
  24.11.04 pk                         == KV 6.1.5 build 041126 (official version of 041124)
  02.12.04 tbh                        == KV 6.1.5 build 041202
  08.12.04 wl                         == KV 6.1.5 build 041208
  15.12.04 mo                         == KV 6.1.5 build 041215
  10.01.05 mo                         == KV 6.1.5 build 050110
  10.01.05 mo                         == TestVersion 6.1.5
  03.02.05 wl                         == KV 6.1.5 build 050203
  28.02.05 wl                         == TestVersion 6.2.0
  04.03.05 mo                         == TestVersion 6.2.0
  05.04.05 tbh                        == TestVersion 6.2.0
  12.04.05 pk                         == TestVersion 6.2.0 050406
  25.04.05 pk                         == TestVersion 6.2.0 050425
  26.04.05 pk                         == KV 6.2.0 050425
  29.04.05 pk                         == KV 6.2.0 050429
  25.04.05 pk                         == TestVersion 6.2.0 050429
  03.05.05 pk                         == TestVersion 6.2.0 050503
  31.05.05 wl                         == KV 6.2.1 050531
  31.05.05 wl                         == TestVersion 6.2.1 050531
  20.06.05 tbh                        == TestVersion 6.3.0 050620
  23.06.05 pk                         == TestVersion 6.3.0 050623
  28.06.05 pk                         == KV 6.3.0 050623
  30.06.05 pk                         == KV 6.3.0 050630
  01.07.05 pk                         == KV 6.3.0 050701
  05.07.05 pk                         == KV 6.3.0 050705
  12.07.05 wl                         == TestVersion 6.3.1
  13.07.05 wl                         == KV 6.3.1 050713
  09.08.05 pk                         == TestVersion 6.3.1
  11.08.05 pk                         == KV 6.3.1 050809
  18.08.05 pk                         == KV 6.3.1 050818
  01.09.05 thr                        == KV 6.3.1 050901
  07.09.05 wl                         == TestVersion 6.3.2
  22.09.05 pk                         == TestVersion 7.0.0
  12.10.05 wl                         == KV 7.0.0 051012
  28.10.05 pk                         == KV 7.0.0 051028
  03.11.05 wl                         == TestVersion 7.0.0
  08.11.05 wl                         == TestVersion 7.0.1
  10.11.05 wl                         == KV 7.0.1 051110
  13.11.05 wl                         == TestVersion 7.0.1
  24.11.05 wl                         == KV 7.0.1 051124
  24.11.05 wl                         == TestVersion 7.0.2
  09.12.05 wl                         == KV 7.0.2 051209
  15.12.05 wl                         == KV 7.0.2 051215
  03.01.06 wl                         == KV 7.0.2 060103
  05.01.06 wl                         == TestVersion 7.0.2
  06.01.06 wl                         == TestVersion 7.0.3
  09.01.06 pk                         == TestVersion 7.0.3
  23.01.06 pk                         == KV 7.0.3 060123
  25.01.06 wl                         == TestVersion 7.0.3
  27.01.06 pk                         == KV 7.0.3 060127
  06.02.06 wl                         == TestVersion 7.0.3
  04.05.06 wl                         == TestVersion 7.0.5
  04.05.06 wl                         == TestVersion 7.1.0
  16.05.06 pk                         == KV 7.1.0 060516
  29.05.06 pk                         == TestVersion 7.1.0
  30.05.06 pk                         == KV 7.1.0 060530
  01.06.06 pk                         == TestVersion 7.1.0
  06.06.06 pk                         == KV 7.1.0 060606
  09.06.06 pk                         == TestVersion 7.1.0
  13.06.06 pk                         == TestVersion 7.1.0
  19.06.06 wl                         == TestVersion 7.1.1
  24.07.06 pk                         == KV 7.1.1 060724
  04.08.06 pk                         == TestVersion 7.1.1
  15.08.06 pk                         == KV 7.1.1 060815
  24.08.06 wl                         == TestVersion 7.1.1
  04.09.06 pk                         == KV 7.1.1 060904
  07.09.06 pk                         == KV 7.1.1 060907
  12.09.06 pk                         == KV 7.1.1 060912
  22.09.06 pk                         == TestVersion 7.1.2
  09.10.06 thr                        == KV 7.1.2 061009
  10.10.06 thr                        == TestVersion 7.1.2
  23.10.06 pk                         == TestVersion 7.1.2
  24.10.06 pk                         == KV 7.1.2 061024
  30.10.06 pk                         == KV 7.1.2 061030
  06.11.06 pk                         == KV 7.1.2 061106
  09.11.06 pk                         == KV 7.1.2 061109
  10.11.06 pk                         == KV 7.1.2 061110
  13.11.06 pk                         == KV 7.1.2 061113
  14.11.06 pk                         == KV 7.1.2 061114
  22.11.06 pk                         == TestVersion 7.1.2
  23.11.06 pk                         == KV 7.1.2 061123
  05.12.06 wl                         == TV 7.1.3 061205
  07.12.06 pk                         == KV 7.1.3 061207
  08.12.06 wl                         == TestVersion 7.1.3 061207
  16.12.06 pk                         == KV 7.1.3 061216
  22.12.06 wl                         == TestVersion 7.1.3 061222
  10.01.07 pk                         == KV 7.1.3 070110
  18.01.07 wl                         == TestVersion 7.1.3 070118
  23.01.07 thr                        == KV 7.1.3 070123
  01.02.07 wl                         == TestVersion 7.1.4 070201
  02.02.07 thr                        == KV 7.1.4 070202
  09.02.07 wl                         == KV 7.1.4 070209
  20.02.07 wl                         == TestVersion 7.2.0 070220
  01.03.07 wl                         == TestVersion 7.2.0 070301
  08.04.07 pk                         == TestVersion 7.2.0 070418
  07.02.08 wl                         == TestVersion 7.3.0
  29.10.08 wl                         == KV 7.3.0 081030
  09.12.08 wl                         == TestVersion 7.3.1
  14.04.09 pk                         == TestVersion 7.3.1 090414
  27.07.09 wl                         == TestVersion 7.3.1a 090727
  27.07.09 wl                         == TestVersion 7.3.1b 090727
  28.07.09 wl                         == TestVersion 7.3.1c 090728 - Beim Umbenennen in Kundenversion das c weglassen!
  04.08.09 wl                         == TestVersion 7.3.1c 090804 - Beim Umbenennen in Kundenversion das c weglassen!
  13.08.09 wl                         == Version 7.3.1 090813
  31.08.09 wl                         == Version 7.3.1 090831
  02.10.09 pk                         == Version 7.4.0 021008
  30.11.09 pk                         == Version 7.4.0 091130
  16.12.09 pk                         == Version 7.4.0 091216
  17.12.09 pk                         == Version 7.4.0 091217
  11.02.10 ts                         == TestVersion 7.4.0 100211
  17.02.10 ts                         == Version 7.4.0 100217
  09.03.10 ts                         == Version 7.4.0 100309
  10.03.10 ts                         == Version 7.4.0 100310
  15.03.10 ts                         == Version 7.4.0 100315
  30.03.10 ts                         == TestVersion 7.4.0 100330
  12.05.10 pk                         == Version 7.4.1 100512
  17.05.10 ts                         == Version 7.4.1 100517
  19.05.10 wl                         == TestVersion 7.4.2 100519
  19.05.10 wl                         == TestVersion 8.0.0 100519
  21.05.10 ts                         == Version 7.4.2 100521
  26.05.10 pk                         == Version 7.4.2 100526
  26.05.10 wl                         == TestVersion 8.0.0 100526
  24.08.10 ts                         == TestVersion 8.0.0 100824
  18.01.11 wl                         == Version 8.0.0 110118
  19.01.11 wl                         == TestVersion 8.0.0 110119
  07.02.11 wl                         == Version 8.0.0 110207
  10.02.11 wl                         == Version 8.0.0 110210
  08.03.11 ts                         == TestVersion 8.0.0 110308
  05.05.11 wl                         == Version 8.0.0 110505
  12.05.11 ts                         == TestVersion 8.0.0 110512
  20.05.11 ts                         == Version 8.0.0 110520
  20.05.11 ts                         == TestVersion 8.0.0 110520
  20.06.11 ts                         == Version 8.0.0 110620
  21.06.11 wl                         == TestVersion 8.0.1 110621
  06.07.11 wl                         == Version 8.0.1 110706
  10.08.11 wl                         == Version 8.0.1 110810
  17.08.11 ts                         == Version 8.0.1 110817
  23.08.11 wl                         == Version 8.0.1 110823
  29.08.11 wl                         == Version 8.0.1 110829
  31.08.11 wl                         == Version 8.0.1 110831
  02.09.11 wl                         == Version 8.0.1 110902
  25.09.11 wl                         == TestVersion 8.0.2 110925
  27.09.11 wl                         == Version 8.0.2 110927
  04.10.11 wl                         == TestVersion 8.0.2 111004
  12.10.11 ts                         == TestVersion 8.0.2 111012
  14.10.11 ts                         == Version 8.0.2 111014
  18.10.11 ts                         == Version 8.0.2 111018
  01.11.11 wl                         == Version 8.0.2 111101
  17.11.11 ts                         == TestVersion 8.0.3 111117
  28.11.11 wl                         == Version 8.0.3 111128
  30.11.11 wl                         == Version 8.0.3 111130
  05.12.11 wl                         == Version 8.0.3 111205
  15.12.11 wl                         == Version 8.0.3 111215
  27.12.11 wl                         == Version 8.0.3 111227
  13.01.12 ts                         == Version 8.0.3 120113
  19.01.12 ts                         == Version 8.0.3 120119
  25.01.12 ts                         == Version 8.0.3 120125
  02.03.12 wl                         == TestVersion 8.1.0 120302
  03.09.13 wl                         == TestVersion 8.1.0 130902
  26.11.13 wl                         == Version 8.1.0 131126
  09.12.13 wl                         == Version 8.1.0 131209
  30.01.14 ts                         == Version 8.1.0 140130
  14.02.14 ts                         == Version 8.1.0 140214
  12.03.14 ts                         == Version 8.1.0 140312
  -------------------------------------------------------------------------------------------------- }

unit globals;


interface


const
    STR_APP_VERSION = 'Version 8.1.0';
    STR_APP_BUILD = 'Build 140312'; // 'Build YYMMDD'

    {
      Examples:
      Test Version
      ---------------------------------
      STR_APP_VERSION = 'TestVersion 6.1.0';
      STR_APP_BUILD   = 'Build 040305';

      Customer Build Version
      ---------------------------------
      STR_APP_VERSION = 'Version 6.1.0';
      STR_APP_BUILD   = 'Build 040305';
    }


implementation


end.
