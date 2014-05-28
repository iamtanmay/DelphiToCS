{ --------------------------------------------------------------------------------------------------
  Ebene 2b (Liquid Handling)
  --------------------------------------------------------------------------------------------------
  Wash- und 6WayValve-Funktionen, System- und Abfallflüssigkeiten als Objekte
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  SamWash:
  23.06.98 wl                         neue Unit, entstanden aus Globals
  == Version 3.3.0
  30.06.98 wl                         uses Volumes
  Ventil1                VolForm.ActualSystemLiq wird geändert
  Wash                   führt VolForm.AddWashVol aus
  WashTips               führt VolForm.AddWashVol aus
  InitIntf6WayValve      neu in Sampler.ini: SimBeforeStart
  13.07.98 wl  InitIntf6WayValve      global statt gSampler
  22.07.98 wl                         uses dbTools
  24.07.98 wl  InitIntf6WayValve      Lesen von SimBeforeStart --> global.ReadRunIniFile
  25.08.98 wl  InitIntf6WayValve      NoOfDil entspricht der höchsten Port-Nummer
  23.09.98 wl  Ventil1                VolForm... nur ändern wenn VolForm<>nil
  30.09.98 wl                         uses dbTools,DM_Sampl entfernt
  07.10.98 HC  ReadVentil1Position    neue Funktion :Software kommuniziert jetzt mit 6 Way Valve
  und erkennt Fehler !!
  Ventil1               s.o. angepasst !
  08.10.98 wl  InitSampler6WayValve   gelöscht --> TBasicThread.InitAll
  09.10.98 wl  Wash,WashTips          Auf null setzen von ActSysAirVol und ActWasteVol
  12.10.98 HC  6 Way Valve            Hardcoremodus eingefügt
  13.10.98 wl  InitIntf6WayValve      Aufruf von SamModules.Add6WayValve
  16.10.98 wl  InitIntf6WayValve      Hardcore-Modus auslesen berichtigt
  19.10.98 wl  gmWash                 entspricht Wash, überarbeitet: ersetzt jetzt auch WashTips
  Funktion Wash gibt es jetzt im TBasicThread
  03.11.98 wl  InitIntf6WayValve      [6WayValve],'UsePeriAtWash' wird aus sampler.ini gelesen
  gmWash                 berücksichtigt UsePeriAtWash
  04.11.98 wl  gmWash                 Abfrage UsePeriAtWash --> TBasicThread.ExecuteFlush
  FlushPeriPump          FlushTime *1000
  01.12.98 mo  gmwash()               DispTip durch DropTipMap ersetzt
  DropTip nur wenn DropTipMap >0
  09.02.99 wl  gmWash                 MAX_TIPS durch Sam.nTips ersetzt
  23.02.99 wl  FlushPeripump          Tips und Speed als Parameter entfernt (verwirren nur)
  25.02.99 HC  DRY                    Diese Funktion ermöglicht ein trocknen der Nadelen nach dem Waschen in einer
  Drystation. Diese muß zum Aktivieren der Funktion den Racknamen "DRY" haben.
  03.03.99 HC  ReadVentil1Position    Die Software untersützt jetzt auch 6 Wegeventiele, die nicht die Defaultadresse 0 haben
  13.04.99 wl                         mit - ObjLiq - zusammengefaßt (TSystemLiquid,TWasteLiquid)
  TLiquids: Funktionalität wie VolForm, aber ohne Anzeige !!
  Ventil1                == TLiquids.ChangeActualLiquid
  InitIntf6WayValve      -> TLiquids.ReadIniFile

  14.04.99 HC  gmWash                 unterstüzt jetzt den Corridormove
  21.05.99 HC  initValve1             Abfrage des 6 Wege Valve auf Position !
  26.05.99 wl  InitValve1             doppelte Port-Abfrage entfernt; Hardcore-Mist rausgenommen
  Sampler.ini:  'NoValveRead=1' statt 'Function1=HARDCORE'
  08.06.99 wl                         record Valve6Way in gLiquids integriert
  TLiquids               überarbeitet: kein Flush mehr beim Starten eines Runs,
  Erweiterungsmöglichkeit für weitere Ventile
  Onmifit-Kommunikation in Objekt TOmnifitValve
  alle Wash-Funktionen im Objekt
  12.07.99 wl  Wash                   Corridormove: keine Übergabe der Startposition
  13.07.99 wl  Wash                   normaler CorridorMove-Aufruf
  18.08.99 wz                         --> String in Ressourcen
  27.08.99 wl  TOmnifitValve.Init     Funktion mit Fehlermeldung, wenn Initialisieren fehlschlägt
  TLiquids.ReadIniFile   COM-Port 6-Wege-Ventil wird auf 0 gesetzt, wenn Init scheitert
  02.09.99 wl  TLiquids.ReadIniFile   Fehler beim Erzeugen des TOmnifitValve-Objekts beseitigt
  13.09.99 wl  ChangeActualVol,AddWasteVol,Wash Sam.nTips in allg.Formulierungen mit MAX_TIPS ersetzt
  16.09.99 wl  Washing                jetzt als Public-Methode
  21.09.99 wl  FlushSyringe,EmptyDilutor (aus Samintf)
  22.09.99 wl  Wash                   Übergabeparameter TipSingleArray
  27.09.99 wl  Washing                Übergabeparameter TipSingleArray
  28.09.99 wl  FlushSyringe,FlushPeripump,EmptyDilutor  --> SamCmd
  Dry                    TipMap wird übergeben
  30.09.99 wl  ChangeLiquid           ohne Flush-Parameter
  13.10.99 wl  InitValve              Ermitteln der aktuellen 6-Wege-Ventilposition aus Create genommen
  16.11.99 wl  TLiquids.ChangeLiquid  Übergabeparameter (@cmd) für altes 6-Wege-Ventil korrigiert
  17.11.99 wl  TLiquids.ReadIniFile,
  TLiquids.ChangeSysVol  Volumen steht wieder unter 'Liquids' (Volumenkontrolle AUDAT)
  17.12.99 wl  gmInitSyringes          aud SamCmd hierher verschoben (Für Setup)
  23.12.99 wl  TLiquids.ReadIniFile   Lücken zwischen 2 Ports werden mit Dummy-Liquids gefüllt
  12.01.00 wl  alle Funktionen        LogText() durch gmLogText() ersetzt (für Delphi 5)
  03.02.00 mo  TLiquids.ReadIniFile   neu:  WashRetractSpeed
  03.02.00 mo  TLiquids.Wash          neu:  Fährt nach dem Waschen mit WashRetractSpeed auf Z0
  22.02.00 mo                         _MoveToXY( wieder mit Sam.ZTravel);
  03.04.00 mo  TLiquids.Wash          WB.NewStationCoordinates(..) eingefügt wegen Z Höhe Tips
  03.04.00 mo  gmInitSyringes         WB.NewStationCoordinates(..) eingefügt wegen Z Höhe Tips
  03.04.00 mo  TLiquids.Wash          Fährt nach dem Waschen mit WashRetractSpeed auf Z travel der Washstation
  12.04.00 wl  TOmnifitValve          --> SerObj
  15.04.00 wl                          SamModules -> gModules
  18.04.00 wl  TLiquids.ReadIniFile   Add6WayValve-Aufruf geändert
  04.05.00 wl  gmInitSyringes         Dilutoren 5 bis 8 werden ggf. auch initialisiert
  05.05.00 wl  gmInitSyringes         1.Teil --> SamCmd;  WashAfterInit-Teil -> TLiquids.FlushAfterInit
  05.05.00 wl  FlushDil1To4,FlushDil5To8  diese Funktionen werden in TWash
  05.05.00 wl  Washing                Übergabewerte: Welche Dilutoren?, DispSpeed
  05.05.00 wl  ReadIniFile            [Pipetting]-Values (...AfterInit) werden hier gelesen
  26.05.00 wl                         Deklaration gLiquids --> ObjModul
  30.05.00 wl  ReadIniFile            [Pipetting]- und [6WayValve]-Einstellungen --> SamStart
  30.05.00 wl  Wash                   als DoWash --> BasicLTh
  30.05.00 wl  FlushAfterInit,FlushDil1To4,FlushDil5To8 --> BasicLTh
  30.05.00 wl  ChangeLiquid,InitValve  TOmniFitValve-Funktionen extrahiert --> ObjModul
  04.09.00 wl  alle Funktionen         Cardinal durch Integer ersetzt: negative Werte werden dargestellt
  06.03.01 tbh ChangeActualVol         Volumen FSystemLiquid wird durch ReadIniFile aktualisiert
  20.02.02 mo  TLiquids.ActSystemLiq   ist jetzt beschreibbar
  15.07.02 mo  const                   TN1109 const MAX_SYSTEMLIQUIDS = 30 nach SamGlobe
  18.07.02 mo                          TN1109 IniFileNamen werden aus Konstante gelesen
  19.07.02 tbh TLiquids.ReadIniFile    TN1109 Name SystemLiquidValve wird aus Konstante gelesen
  26.09.02 wl                         TN1283 Kein fester Inifile Name mehr
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  25.10.02 wl  TLiquids.ReadIniFile         TN1293.1 verwendet TWinlissyIniAccess
  25.10.02 wl  TLiquids.ChangeSysVol        TN1293.1 verwendet TWinlissyIniAccess
  25.10.02 wl  TLiquids.ChangeWasteVol      TN1293.1 verwendet TWinlissyIniAccess
  29.11.02 mo  FSystemLiquidList            TN1240 neu
  20.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  11.03.03 wl  TLiquids.ReadIniFile         TN1293.5 Port-Daten werden mit neuer Funktion ReadLiquidPortData gelesen
  03.07.03 wl                               TN1501   Anpassung an Änderungen in Device-Struktur
  14.07.03 wl  ReturnFromSimulationMode     TN1501   entfernt: wird nicht mehr benötigt
  18.12.03 wl                               TN1672   uses geändert
  03.02.05 wl  En/DecodeSystemLiquidIdent   TN2269   für neue Definition der x-Wege-Ventile
  16.02.05 wl  TLiquids,TSystemLiquid       TN2269   komplett überarbeitet, Funktionalität aber wie vorher
  23.02.05 wl  TLiquids.GetSystemLiquid     TN2269   falsche Meldung korrigiert
  26.08.05 pk  GetAllSystemLiquidNames      TN2566   Does not include fake diluent names - now called by methedit
  --------------------------------------------------------------------------------------------------
  20.06.08 pk                               TN4139   uses changed
  27.08.09 pk                               TN4753   reference to runflow removed
  27.08.09 pk  ChangeVolume                 TN4753   must be reimplemented
  28.08.09 pk                               TN4753   Most of code moved to LiquidsBasic
  24.00.09 pk  CreateSystemLiquid           TN4753   bug corrected, Creates TSystemLiquidExt
  18.06.12 wl                               TN5899   Inhalt --> Liquids
  -------------------------------------------------------------------------------------------------- }

unit LiquidsExt;


interface


implementation


end.
