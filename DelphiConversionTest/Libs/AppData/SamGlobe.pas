{ --------------------------------------------------------------------------------------------------
  Ebene 1b (Sam-Globals)
  --------------------------------------------------------------------------------------------------
  Globale Sampler Variablen und Programme für Sampler / Sophas etc.
  --------------------------------------------------------------------------------------------------
  Version  Datum     op  function/procedure     Änderung / Neuerung
  -------  --------  --  -------------------    ---------------------------------------------------
  28.11.97  mo  SamErrBox              Record BC_Reader (Barcode) eingefügt
  04.12.97  dl  Record Rackdata        ZTube implementiert (Höhe des Tube - Bodens in Steps)
  04.12.97  dl  DLL Version            DLLVersionPtr       : ^TVersionString;
  Variablen Deklaration für DLL Versionsnummer
  08.12   dl+hc EmptyDilutor           aus Samintf.pas (Funktion für Sophas und Sampler)
  PickLiquid             dito aus samintf.pas
  DispLiquid             dito
  FlushSyringe           dito
  GetDilutorAmount       DITO
  GetAspSpeed            dito
  GetDispSpeed           dito
  ModuleExist            dito

  TLiqHandlingRec        von globals.pas(Sampler-SW) + SophGlob(Sophas-SW) hierher geschoben
  TAuftRec               dito
  TValvePositions        dito
  T6WayValve             dito

  ReadLiqHandlingRecord  prozedur von global.pas (Sampler-SW) hierher verschoben
  Liquid Parameter DB von DM_SAMPL nach DbRack verschoben
  ReadAuftRecord         prozedur von global.pas (Sampler-SW) hierher verschoben
  09.12.97  dl  ReadAuftRecord         in ReadAuft umbenannt
  17.12.97  dl  BCSCANRANGE (Record)   Ermöglicht das Hin und Herfahren des Handlers bei nicht
  Lesen des Codes in xyz - Richtung
  dl  BCRPOSITION (Record)   xyz - Positionen der Barcode Leser
  dl  InitSamGlobals         Aufruf von procedure ReadBarCodeIniData in BarCode.pas
  dl  ConvertIniString       neu: kopiert durch Kommata getrennte Teilstrings in einen String Array (0..19),
  die so gesplitteten Strings können besser den Variablen zugeordnet werden
  17.12.97  dl  uses Barcode           im Implementation Teil
  06.01.98  mo  ResetAllModules        neue Procedure Sendet ein 'R' an alle Module
  08.01.98  mo  ResetAllModules        überarbeitet
  14.01.98  dl  FlushSyringe           Aspirate Wert muß übergeben werden (darf nicht null sein)
  ReadMachineInifile     WRITE_LOG_INFO    := ReadInteger('WriteLogs','WRITE_LOG_INFO'  ,1);
  jeder Logtyp ist jetzt ausschaltbar
  14.01.98  dl  FlushSyringe           Application.ProcessMessages; in der Pick Disp Schleife, damit ein Thread Stop bemerkt wird
  04.02.98  mo  ResetModules           Command Aa00 nur auf Ports 0-7
  EmptyDilutor           Ramping in Mm0 hinzugefügt
  05.02.98  dl  MaxPorts               Variable Anzahl von Ports aus Machine.Ini [Modules]
  ResetAllModules        Ports ausschalten von 0 bis MaxPorts-1
  DilutorType            globales IntegerFlag für alten oder neuen Dilutortyp 0 = neu
  EmptyDilutor           Angepaßt für DilutorType

  18.03.98  dl  DTErrorMode          : integer = 0;    // Drop Disposal Tips Error Mode
  GTErrorMode          : integer = 0;    // Get Disposal Tips Error Mode
  DispTipOffset        : integer =  0;   // Länge der Disposal Tip Spitze in mm
  von MosslOut hierher verschoben
  ReadmachineIniFile     DTErrorMode ...
  07.04.98  mo  ReadMachineIniFile     DisposalTipLength : auslesen korrigiert
  13.05.98  mo                         Neuer Record TErrMsg + E:Exeption
  14.05.98  mo                         Neuer Typ TintPtr
  03.06.98 wl                          const cMAX_SAMMODULES --> Maximale Anzahl von Hardware-Modulen auf einem Sampler
  type  TSamModErrGroup --> Zusammenfassung der Sampler Module in Gruppen (zur Fehlerbehandlung)
  type  TSamModule --> Definition der Sampler Module (zur Erkennung eines Moduls anhand der Adresse)
  var   gSamModule --> Array, in dem alle vorhandenen Module gespeichert sind
  InitSamGlobals          eingefügt: WriteSamModules
  gmWriteSamModules       Procedure zum Beschreiben des Var-Arrays gSamModule[..]
  gmWriteSamModule        Function, die eine Modulbeschreibung zum Array gSamModule hinzufügt
  08.06.98 wl  gmWriteSamModules       erweitert um alle fehlenden Modul-Adressen
  08.06.98 wl                          neu: gLogFilePath, gLogFileArchives --> Logfile-Verzeichnisse
  ReadMachineIniFile      Lesen der Pfade aus machine.ini
  12.06.98 mo  ReadMachineIniFile      neu : MaxTipVolume = Maximales Volumen für Verteilung
  Disptipoffset unter  DISPOSAL_TIPS eingefügt
  12.06.98 mo                          neue Globale var DispTip;
  15.06.98 wl                          alle TSamModErrGroup-Namen mit m davor
  neu für TSamModErrGroup: mNothing
  23.06.98 wl  gmWriteSamModules       --> nach SamIntf verschoben
  gmWriteSamModule            dito
  var ShowRunFormOpen,PLayoutName,PMethodName,PdbPath,SamplerThreadRunning,
  Delay_sec  --> aus Globals
  == Version 3.3.0
  26.06.98 wl                          type TSamModule: neu: Pointer auf die Modul-Adresse
  neu: TSampler-Objekt, Property: SimulationMode
  InitSamGlobals          gSampler wird erzeugt, SimulationMode:=false
  30.06.98 wl                          neu: TSampler.SimBeforeStart
  gmDeleteString          Tool: ersetzt die nervige DELETE-Funktion
  gmGetFromString         man erhält das, was sonst gelöscht würde
  03.07.98 wl  ResetAllModules         neu: TurnBCReaderOff
  InitSamGlobals          Initialisieren des Barcodelesers
  06.07.98 wl  InitSamGlobals            wieder deaktiviert
  07.07.98 wl                          SLOTGROUPDATA: CarrierType
  10.07.98 wl                          TSampler.SamplerThread,EscapeThread (aus Main)
  13.07.98 wl  TSampler                --> unit ObjSampl
  InitSamGlobals          Erzeugen des Sampler-Objekts --> Main.FormCreate
  gSampler heißt jetzt global (Kompatibilität mit Sophas)
  PMethodName und PLayoutName entfernt --> MAIN
  14.07.98 wl  ReadMachineIniFile      Auslesen von ExportRunFile
  15.07.98 wl  ReadMachineIniFile      gExportRun.Separator wird aus ini-File eingelesen
  ReadMachineIniFile      ExportAfterRun einlesen
  16.07.98 wl                          TRackStruct: AddOffset-Felder eingefügt
  17.07.98 wl  ReadSamIntfIniFile      Logfiles und ExportRunFile werden aus Samintf.ini gelesen
  21.07.98 wl                          var gExportRun:TExportRun
  uses ObjGlobe,Utility  statt  uses barcode,ShowRun
  ReadSamIntfIniFile      liest aus 'Sampler.ini'
  ReadBarCodeIniData      aus Barcode: Lesen der Barcode-Daten aus machine.ini
  ReadBarCodeIniData      MTP_EXTERN wird nicht mehr gelesen
  gmDeleteString          --> Utility
  gmGetFromString         --> Utility
  InitSamGlobals          Global-Offsets von ReadMachineIniFile hierher
  22.07.98 wl                          var global:TSampler und uses ObjSampl --> dbTools
  28.07.98 wl                          TAuftRack alle Racknamen auf string[30]
  31.07.98 wl                          SLOTGROUPDATA: CarrierType entfernt (verursachte Chaos beim GetPlate)
  06.08.98 wl  ReadSamIntfIniFile      Einlesen von ExportRun --> TExportRun.Create (ObjGlobe)
  10.08.98 wl  ReadBarCodeIniData      liest jetzt auch Init-Strings mit mehreren Kommas
  TStringArray deaktiviert
  ConvertIniString        gelöscht
  16.09.98 wl                          neu: var BorderWidthX,BorderWidthY
  gelöscht: var PdbPath,SamplerThreadRunning
  const cMAX_SAMMODULES=40 --> ObjGlobe
  type TSamModErrGroup,TSamModule --> ObjGlobe
  var gSamModule,gSamModCounter -- ObjGlobe
  var NoOfRacks,NoOfCarrier --> SETUP:LayOForm
  FlushSyringe            nich verwendete Variablen entfernt
  ReadMachineIniFile      Einlesen von BorderWidthX,BorderWidthY
  InitSamGlobals          NoOfRacks,NoOfCarrier entfernt; SamModules werden initialisiert
  24.09.98 wl                          neu: type SchedHandle,SchedHandlePtr
  neu: const cActionStarted,cActionDone
  28.09.98 wl                          const cActionCancelled
  07.10.98 wl                          gelöscht var Delay_sec
  08.10.98 wl                          Kommentare zu TAuftRec und TLiqHandlingRec (nichts geändert)
  09.10.98 wl                          var ActSysAirVol,ActWasteVol: aus RunThread: werden durch Scriptfunktion global benötigt
  15.10.98 wl  ReadSamIntfIniFile      gExportRun.Create --> InitSamGlobals; Verdopplung der StoreDays auf 28
  InitSamGlobals          ruft InitIntf auf, nicht umgekehrt
  InitSamGlobals          neu: ChDir(PAppPath)
  ReadBCIni               verkürzte Version von ReadBarcodeIniData: mit führenden Nullen
  02.11.98 wl  InitSamGlobals          Aufruf von ChangePeriSpeedKV4
  ChangePeriSpeedKV4      PeriSpeed (machine.ini) wird für KV4.0 tausend mal genauer
  02.11.98 wl                          SLOTGROUPDATA geändert: HPutOfs (parallel zu SAMPLER.DLL)
  TSlotGroupData geändert: H_ZPut_mm
  04.11.98 HC  READER_RECORD           für Photometer hinzugefügt !
  09.11.98 wl  ReadSamplerIniFile      liest [Documentation], WritePosinfo=
  17.11.98 wl                          uses ObjGlobe,SamIntf,Utility entfernt
  EmptyDilutor,FlushSyringe,GetDilutorAmount,GetAspSpeedm,GetDispSpeed,CalcRestVol,
  ResetAllModules,ModuleExist   --> SamIntf
  InitSamGlobals           --> SamStart
  30.11.98 wl                          neu: var BCRackDelLastPos, BCTubeDelLastPos
  01.12.98 mo  var                     DispTip durch DropTipMap ersetzt
  08.12.98 wl  var                     neu: gPlateCheckPut,gPlateCheckGet
  25.01.99 wl  ReadAuft                --> dbRack
  ReadLiqHandlingRecord   --> dbRack
  gmWriteMixRec           erzeugt neuen LIQPAR_MIXREC
  26.01.99 wl  gmWriteMixRec           Umrechnung mm - Steps;  Cycl,Method integer statt ShortInt
  gmWriteMixRec           AspSpeed wird für DispSpeed übernommen, wenn DispSpeed=0
  29.01.99 mo  TAuftRec                Alle Volumen von Cardinal auf single
  TAuftRec                neu DTransAir = TransAirgapVolumen welches nach Dispense nochmals aufgenommen wird
  01.02.99 mo  TTipLiqError            neu globales Array für Liquid Detection Error Flags
  01.02.99 wl  TLiqHandlingRec         neu: SampleAspMixFirstOnly:boolean
  04.02.99 wl  TBorderWidth            record mit Left,Top,Right,Bottom
  var WBBorders : TBorderWidth statt BorderWidthX,Y
  11.02.99 wl                          var Interflag entfernt
  15.02.99 wl                          für externes Barcodelesen: gReadExtDLLName, -Func, -Param
  17.02.99 wl  gmWriteMixRec           --> postools
  25.02.99 wl                          neu gPhotometerCarrier, -Slot, -Str
  08.03.99 mo                          neu: gMultiCheckXYPos
  09.03.99 mo                          neu: gSwitchPortOnRestart
  16.03.99 wl  var gMaxNameLen : TMaxNameLen  -> maximal erlaubte Längen von Namen
  17.03.99 wl                          var gConfirmIgnore
  26.03.99 wl                          TAuftRec.SEQ : double
  14.04.99 hc  Corridor_XMoveOffset    für den Corridormove mit eingefügt !
  19.04.99 hc  LiquiudHandlingrecord   um RecordDetectionVolume + AspirationMethodRetractZ erweitert
  22.04.99 wl                          neu: TEventRec, var gActualEvent für Tabelle EVENTS.DB
  03.05.99 wl                          neu: Applikations-Sprache gLanguage (Englisch=0,Deutsch=1,...)
  gmLoadLanguage,gmGetResString,gmGetResFString  Laden der sprachspezifischen Recourcen
  31.05.99 wl  gmLoadLanguage,gmGetResString,gmGetResFString  --> Utility
  30.06.99 mo                          neu: gFlushVolAfterInit + gAirVolAfterInit + gRetractStepsAfterInit
  29.07.99 wl                          neu: gFlushCycles
  var CommThreadRunning entfernt
  23.08.99 wl                          neu: Asp-/DispSpeedCalcFactor
  24.08.99 wl                          SlotZ_mm aus TSlotGroupStruct entfernt
  WorkspaceX,WorkspaceY,PipettXZero,PipettYZero entfernt (nutzlos)
  31.08.99 wl                          Devices,DeviceParts,... (wie in Sophas bzw. im alten Redi)
  neu: TBasicTipType,TTipType - var gTipTypes
  09.09.99 wl                          TTipType: Name 20 Zeichen lang; neu: DTOffset_mm
  10.09.99 wl                          neu: TLicence, gLicence
  10.09.99 mo                          neu: TWorkType, gWorkType
  14.09.99 wl  TRackStruct             erweitert um:  MOffsetZ_mm, MOffsetUpX_mm, MOffsetUpY_mm
  RACKDATA geändert !!!!  raus: AssemblyName, Load- ,Park- ,PipSlotName
  rein: MOffsetZ, MOffsetUpX, MOffsetUpY
  16.09.99 wl  TipInfo,ActTipContent   -> TTipInfo,TTipContent
  CharPtr                 entfernt (Unsinn)
  TBasicTipTypes          neu: Set of TBasicTipType
  20.09.99 wl                          neu: TRediDevices
  21.09.99 wl                          neu: TAuftArray; TDevicePart erweiter um TDevicePartType
  22.09.99 wl                          TWorkType um wtVirtual erweitert
  23.09.99 wl                          MaxTipVolume/DispTipOffset entfernt
  27.09.99 wl  TRackStruct             erweitert: Shift_Radius_mm, Shift_NoOfSteps
  29.09.99 wl                          neu: GlobalZOffset, THandlerTip, HTip1, HTip2
  02.11.99 wl                          gTubeToolData: Daten für festen Tube-Greifer
  16.11.99 wl  TImport; gImport        Variablen für Import-Tool
  29.11.99 wl  ghZTravelTubes          neuer hZTravel-Wert für Tubes
  30.11.99 mo  TSamParsStr             Für SamPars.dll
  09.12.99 mo  TAinfo                  Für Retry Funktion
  14.12.99 mo  GlobalZFactor           Für Berechnung der Z Position Redi am Handler ( wird gesetzt in RunThread Dilution )
  29.12.99 mo  record ACTION           korrigiert
  12.01.00 wl                          var CurrentHour, Hour,... entfernt
  13.01.00 wl                          neu: var g24hMode
  17.01.00 wl                          neu: type TExportDefs / var gExpDefs (ersetzt TExportRun-Objekt)
  11.02.00 wl  neu: MinVolume in TTipType; VolMotor in TRediDevices; dptMotor in TDevicePartType
  11.02.00 wl  gChangePosinfo          Schalter für Umschreiben der Posinfo.db in TRack.ChangeRackID
  25.02.00 mo                          neu : gDiTiIgnoreRestTips
  07.03.00 wl                          neu: WashBlower in TRediDevices, TDeviceList, cDeviceParts=20
  22.03.00 mo                          neu:  gWashDryDelay
  22.03.00 wl                          neu: TTubeParameters  (zum Tubes lesen und wiegen)
  04.04.00 wl                          cPosinfoNetWeight = -900; cPosinfoTaraWeight  = -1000
  05.04.00 wl                          neu: gCalliOptions, gBalace1Left/-Top
  12.04.00 wl                          neu: TLicence.Sophas, Devicepart: dptThermostat
  14.04.00 wl                          neu: NoOfVortexer; neue Deviceparts definiert
  18.04.00 wl                          TDevicePart mit TModule aus unit ObjGlobe zusammengefaßt
  19.04.00 wl                          TDevicePart.Adr : Word und .AdrPtr : WortPtr
  28.04.00 wl                          neu: TModulTypes, TParaArray, TUserFunction, gUFunctions
  05.05.00 wl  var    gFlushVolAfterInit,gAirVolAfterInit,gRetractStepsAfterInit  --> SamWash
  05.05.00 wl  type   TWashprogRec       neu: Abbildung eines Washprog.DB-Records
  11.05.00 wl  Const  cMaxUserFunctions erhöht, neu: cMaxParallelThr
  type   neu: TSequenceRack, TPosInfoRack
  26.05.00 wl                          neu: alle variablen & typen aus SamIntf
  31.05.00 wl  type TSamplerThreadModes  aus ThrMan
  31.05.00 wl  var gWashRetractSpeed,gFlushVolAfterInit,.. aus TLiquids hierher
  05.06.00 wl  var gLicence            --> globals
  05.06.00 wl  type TSequenceRec,TUserFunction,TParaArray  var gUFunctions  --> ScheUtil
  06.06.00 wl  type TSchedProperties - neu; TSchedHandle jetzt mit Properties (TSchedProperties)
  14.06.00 wl  type TSchedProperties   neu: ufpFirstInit,ufpComment,ufpLastInit
  19.06.00 wl                          neu: var gWashOnlyUsedTips
  20.06.00 wl  type dptBalance  als DevicePartType
  23.06.00 wl  type TLiqHandlingRec    um UseDispTipWithoutTip (boolean) erweitert
  05.07.00 mo  var gDispSubmergeActive neu
  05.07.00 wl  const MODULENAME_LENGTH, var MODULENAME   Modul-Namen in der DLL
  05.07.00 wl  type TDevicePart   Name begrenzt auf 25 Stellen (=MODULENAME_LENGTH)
  05.07.00 wl  type TLiqHandlingRec  ..SwitchModule ersetzt ..SwitchPortNumber
  11.07.00 wl  type TLiqHandlingRec    neue Felder für Channel 2
  12.07.00 wl  type TLiqHandlingRec    noch mehr LiqPar-Felder bei SpitBack, SampleAspCh2Wash, WashVolCh2
  17.07.00 wl  type TDevicePart        um Device:string erweitert (TDevice kann damit entfallen)
  19.07.00 wl  var gAskForKeepContents neu: wird beim Script-Reset benutzt
  07.08.00 wl  type TModuleType - geänderte Typnamen; TDevice - raus
  08.08.00 wl  type SAMPLER            neu: WiperSpeedX, WiperSpeedY / entfernt: LastCode
  11.08.00 mo  var DLLVersionPtr       jetzt : PChar;
  16.08.00 mo  var gWaitAfterPickSys   Optimierung PickSystemLiq ein/aus
  17.08.00 wl  type TTipType           mit ToolName
  29.08.00 wl  var gToolZTravel,gRediZRetrSpeed  neue Variablen für Handler-Redi-Hardcore
  31.08.00 wl  type TDevicePart        erweitert um Display:TLabel
  04.09.00 wl  type TPosinfoRec        gelöscht
  06.09.00 wl  var gShowTempRequestTime neu: Abfragezeit für ReadTemp, setzt Infofenster an/aus
  18.09.00 mo  type TipLiqError         Array um 1 erweitert
  18.09.00 mo  var  gToolZTravel        von WORD nach Integer
  27.09.00 tbh type TLiqHandlingRec     neues Feld für Zwangswaschen (WashIsForced)
  10.10.00 tbh var gSaveSolvent         neu: (=1) setzt unnütze Waschschritte (2.Kanal) aus
  20.10.00 tbh var gSysLiqMinVol        neu: setzt Volumen bei dem Volumenanzeige auf gelb geht
  27.10.00 tbh var gUsedRediTipType     neu: erfasst RediTipType für umschrieb auf SamplerTip1
  03.11.00 mo  type TWorkType           neu: wtTipPressure  für positiven Druck Speedy
  21.11.00 tbh type TTubeToolData       erweitert um SaveMoveOffset und SaveMoveSpeed
  22.11.00 tbh var gDbToolDllName       neu: Name einer Database-Dll (kundenspezifich)
  23.11.00 tbh var gMoveTubeToBalance   neu: zum setzen der TubeOptionen bei Tubetransport zur Waage
  23.11.00 tbh var gMoveTubeFromBalance neu: zum setzen der TubeOptionen bei Tubetransport von Waage
  28.11.00 tbh var gMoveTubesAfterRestart  neu: wenn =1 räumt Sampler nach Restart selbst Tubes von der Waage
  02.12.00 tbh var gAskForRackPlacement neu: wenn =1 wird vor Runstart Fenster angezeigt wie racks zu platzieren
  02.12.00 tbh var gSamDeviceName       neu: dem Sampler kann ein Name zugewiesen werden
  18.12.00 tbh type TLiqHandlingRec     neu: UseWashMacro und WashMacroName
  19.12.00 mo  type TAuftRec            neues Feld TipBlock
  22.12.00 tbh var gRequestWriteBackData neu: zum einstellen ob vor dem Rückschreiben von Daten bestätigt werden muß
  19.01.01 mo  var gTubeDispColor       neu: Farbe der Tubes bei dispense
  05.02.01 tbh const cCPRunName         neu: Header vor SAMI-Run ist als Konstante einstellbar
  13.03.01 tbh const MAX_VORTEXER       neu: gibt maximale Anzahl an Vortexern vor (nötig für Feldgrößen)
  13.03.01 tbh type TDevicePartArray    neu: Array von TDeviceParts (Feldgröße ist Anzahl der Vortexer)
  13.03.01 tbh var gVortRequestTime     neu: setzt Abfragezeit für Vortexern bei multiplen Zugriffen
  13.03.01 tbh var gTangoRequestTime    neu: setzt Abfragezeit für Tango bei multiplen Zugriffen
  13.03.01 tbh var gLogVortInfo         neu: setzt ob Abfragewerte für Thermostate und Vortexer gelogt werden
  13.03.01 tbh var gFixAfterVortexing   neu: setzt ob nach Abschalten von Vortexern erneut fixiert werden soll
  22.03.01 mo  var gShowPlatesBeforeBCRead neu: SAMI Anzeige Racks vor Barcodelesen
  30.03.01 tbh type TDeviceDisplay      neu: für Vortexeranzeige
  30.03.01 tbh type TDeviceDisplayArray neu: für Vortexeranzeige
  16.05.01 tbh TSamplerThreadMode       neu: mdScriptWithMessages,mdNoInitAtScriptStart
  18.05.01 tbh var gResetScriptAfterDDEStart  neu: wenn true kommt nach DDE-Start automatisch Scriptreset
  21.05.01 tbh var gCloseDoorAfterInit  neu: wenn gesetzt, schließt Waage nach Init die Waagentür
  01.06.01 tbh type TModuleType, TComDevType  erweitert um IKA Vortexer
  06.06.01 tbh type TTipType            neu: XOffset, YOffset
  06.06.01 tbh var gEnableVolumeControl neu: wenn true wird die Volumenkontrolle verwendet
  28.06.01 mo  type TTubeToolData       neu: BringBackVOffset
  17.07.01 tbh var gDestOnBalance       neu: wenn true steht Zieltube für Calli-Option auf Waage
  17.07.01 tbh var gUseRackIDPos        neu: wenn true wird statt barcode RackID und Position des Tubes verwendet
  18.07.01 tbh var gTargetApproach      neu: setzt Teiler in Volumenberechnung von DoCalli
  14.08.01 mo  var gDilutorAspSpeedPerCent TN1004 neu: [Module] DilutorAspSpeedPerCent=1 wertet Dilutor.AspSpeed als % Wert von [6WayValve] AspSpeed
  15.08.01 mo  var gHToolYCorrect       TN1011 neu: Y Korrektur bei Handler Bewegungen abschaltbar
  28.08.01 tbh var gMoveRackSlotDelimiter TN1008 neu: MoveRackSlotDelimiter setzt Delimiter für MoveRack-Option Slot
  05.09.01 tbh var gOpenFixationAtInit  TN1032 neu: setzt ob bei Init Vortexer geöffnet werden
  06.09.01 tbh var gReleaseVortexersAtEnd  TN1030 neu: setzt ob am Programmende CAT-Vortexer/Heizer automatisch stoppen
  06.09.01 tbh var gNoVortexerStopAtInit   TN1030 neu: setzt ob CAT-Vortexer/Heizer beim Init gestoppt werden
  07.09.01 tbh var gCVortexSleepTime    TN1034 neu: setzt SleepTime des seriellen Objekts für die Kommunikation mit CAT-Vortexern
  10.09.01 mo  type TcEventFunc         TN1035 neu: Functionsaufruf in Dll mit cdecl Konvention
  10.09.01 mo  var  gCDllFiles          TN1035 neu: String mit allen C Dlls aus Sampler.ini
  21.09.01 mo  var  gDetachDll          TN1060 neu: Active X Dll's bei denen Detach aufgerufen werden muss
  21.09.01 mo  var  gMethEditActionOnly TN1060 neu: nur Seq,Action,Option wird in Methedit angezeigt
  28.09.01 tbh TBasicTipType            TN1050 neu: RemRediTip für Pickup Redi Tips
  13.10.01 mo                           TN1067 Merge mit SIAS Änderungen
  29.10.01 mo Record APOS               TN1067 korrigiert auf WORD für Zinsser
  06.11.01 mo TAddSamModule             TN1078/TN1078 neuer Record
  06.11.01 mo TEXportDefs               TN1084 Record erweitert
  23.11.01 tbh gChangeTipTypes          TN1112 neu: setzt ob Verwendung mehrerer TipTypes pro Tip möglich (TN1092)
  23.11.01 tbh IMPLEMENTATION-Teil      TN1051 Kompiler-Anweisungen für Kompatibilität mit IBTOOLS.DLL
  26.11.01 tbh type LoginMode(s)        TN1051 setzt welche Interbase-Datenbank angesprochen wird
  26.11.01 mo  var hzTravelTubes        TN1110 Variable entfernt
  27.11.01 tbh type TTipType            TN1051 um AName erweitert
  27.11.01 tbh var gWeighLogicDbName    TN1051 neu: setzt CALLIWEIGH-Datenbank (Pfad\Name)
  27.11.01 tbh var gWeighLogicDbUser    TN1051 neu: setzt User für CALLIWEIGH-Datenbank
  27.11.01 tbh var gWeighLogicDbPassword TN1051 neu: setzt Passwort für CALLIWEIGH-Datenbank
  29.11.01 tbh var gChkLHPByGeneratePipSeq  TN1121 neu: setzt LHP-Kontrolle in RunTrd.GeneratePipSeq
  30.11.01 mo  type TExportDefs         TN1105 neu DateTimeStampStr, xlDelCSV
  03.12.01 tbh allgemein                TN1126 Kompileranweisungen für Editor eingefügt
  04.12.01 mo                           TN1131 Kompileranweisungen korrigiert
  05.12.01 mo  var gMinZTravelTubes     TN1136 neu: Begrenzung des hZTravel für TubeMovements
  13.12.01 mo  type TCWParamSet         TN1051 neu: CalliW ParameterSet
  13.12.01 mo  type TCWGetToolResult    TN1051 neu: CalliW Resultat von CWGetTool
  19.12.01 mo  type TCWstring           TN1051 neu: String[40]
  03.01.02 tbh type TCWGetToolResult    TN1051 erweitert um CorrectVol
  03.01.02 tbh type TRediDevices        TN1158 erweitert um Feeding
  03.01.02 tbh var gPipToolZTravel      TN1161 neu: Reisehöhe für Pipettiertools an Tip 5
  03.01.02 tbh var gMinZTravelPipTool   TN1161 neu: Mindest-Reisehöhe für Pipettiertools an Tip 5
  23.01.02 tbh var gWashRemRediTips     TN1166 neu: Legt fest ob PicFix Spitzen bei Wechsel gewaschen werden
  23.01.02 tbh var gUseSeveralBalanceRacks  TN1135 neu: aktiviert Verwendung mehrerer WaageRackTypen
  22.02.02 tbh var gDilutorArray        TN1052.2 neu: speichert alle verfügbaren Dilutoren eines Systems
  22.02.02 tbh type TTipType            TN1052.2 erweitert um Dilutoradresse (wenn an Handler)
  05.03.02 mo  gmStrToFuncParam         TN1079.1 neu: wandelt String in PFuncParam
  03.05.02 tbh type TSamplerThreadMode  TN1052.1 neue Modi (mdScriptDeleteRuns,mdUpdateBasicLayout) für SlaveModus
  03.05.02 tbh var gSlaveModeActive     TN1052.1 neu: setzt ob SlaveModus verfügbar ist
  05.05.02 mo  const cSlaveModeLayoutPrefix TN1052.1 neu
  11.06.02 tbh var _SBErrCnt            TN1208 FehlerCounter-Zeiger für Fehler bei Adressen auf 2. X-Main
  11.06.02 tbh var gMaxSBError          TN1208 Anzahl maximal erlaubte Retries für Fehler bei Adressen auf 2. X-Main
  20.06.02 pk  var TSchedHandle.Priority TN1052 new field added to record for Scheduler
  20.06.02 pk  const cUnsavedScriptName TN1052 new: scriptname of working copy of scheduler
  14.07.02 pk  var                      TN1243 new gArchiveScript, gArchiveScriptPath , gAutoResetScript
  18.07.02 mo  const MAX_SYSTEMLIQUIDS  TN1109 hiereher verschoben
  19.07.02 pk  var   gUseScheduler      TN1052 New: Used to turn on/off Scheduler
  19.07.02 pk  const cSCRIPT_NAME_MAX_LEN New Constant for limiting length of Script Name
  19.07.02 tbh const cSysLiqValveName   TN1109 Name SystemLiquidValve jetzt Konstante
  20.07.02 pk  var TSchedHandle.Resume..TN1052 ResumeOnCreate: Thread will resume as soon as constructor is finished
  02.09.02 pk  const cACTION_NAME_REMARK TN1052 new: identifier of Remark action made globally accessable
  02.09.02 pk  var gUseScheduler         TN1052 Changed to Byte.  0:No Scheduler, 1:Scheduler + old thread manager, 2:Scheduler + new thread manager
  02.09.02 pk  TSamplerThreadMode       TN1052 mdAdministrator added. Flag to turn on internal status messaging in new thread manager
  02.09.02 pk  const cACTION_NAME_DUMMY TN1052 new: identifier of Dummy action made gloabally accessable
  09.09.02 mo  var                      TN1280 neu: gRunTableExclusive
  11.09.02 mo                           TN1283 Merge mit SIAS
  17.09.02 mo                           TN1262 TCWGetToolResult erweitert für DoWeigh=false
  18.09.02 mo  SetOKTipMap              TN1283 korrigiert für 4 Tips
  26.09.02 wl                           TN1283 neu: Globale Alias-Namen; Inifile-Namen für Sias
  27.09.02 wl  gmStrToFuncParam         TN1294 wirft keine Exceptions mehr
  10.10.02 wl  alle CONST-Definitionen  TN1293.2 --> CommonConstants
  10.10.02 wl  alle TYPE-Definitionen   TN1293.2 --> CommonTypes
  16.10.02 wl  gTipTypes                TN1293.2 neu: als dynamisches Array
  24.10.02 wl  PAppPath, PDataPath      TN1293.1 --> TAppSettings.DataPath
  14.11.02 wl  gUseScheduler            TN1328.1 --> TAppSettings.UseScheduler
  05.12.02 wl                           TN1345   HXSTEPS_PER_MM, HYSTEPS_PER_MM auch für ZP01
  10.12.02 wl  MaxPorts,DilutorType,gAddSamModule     TN1345  --> TRoboticInterfaceZP01
  10.12.02 wl  DLLVersionPtr,SamplerPtr TN1345  --> TRoboticInterface
  10.12.02 wl  Sam                      TN1345  Sam: TMinimalInterface
  11.12.02 wl  OkTipMapPtr,_GlobalErrPtr TN1345 --> RoboticInterfaceZP01, ZP02
  12.12.02 wl  Sam                       TN1345 --> Utility2
  20.12.02 wl                            TN1293.5 uses und WinlissyIniAccess geändert
  27.12.02 wl                            TN1293.5 ErrDlg entfernt
  22.01.03 tbh var gShakeTimeAdjustable TN1193 neu: setzt ob Schüttelzeit bei Resinrack einstellbar sein soll
  28.01.03 wl  var AInfoPtr              TN1345 --> RoboticInterfaceZP01, ZP02
  18.02.03 wl  var PSamEvent, ActTipInfo TN1345 --> AppInterface
  18.02.03 wl  var ZTubeGlobal           TN1345 entfernt
  20.02.03 wl  var gActualEvent          TN1334.3 entfernt
  20.02.03 wl  var gSamErr..             TN1334.3 --> SamErr
  04.03.03 wl  var _SBErrCnt,gMaxSBError TN1345 --> RoboticInterfaceZP01
  04.03.03 wl  global_Alias, global_RunAlias  TN1345 entsorgt
  12.03.03 wl  GlobalErr,SetGlobalErr,gmStrToFuncParam  TN1293.5 --> posTools
  17.03.03 wl  gLogFilePath,gLogFileArchives,gLogStoreDays  TN1332.2 --> ZaCommon.dll (Logging)
  10.04.03 wl  var gArchiveScript, gArchiveScript  TN1332.4 --> Archiving
  13.06.03 wl  BCRack.. BCTube..         TN1501.1 all BC-Reader-Variablen bis auf ..LeadZero --> ObjModulA
  25.06.03 wl  gMoveTubeToBalance, gMoveTubeFromBalance TN1501  TTubeOptions statt integer
  08.07.03 wl  gConradBoardPort          TN1501   --> CommunicationManager
  23.07.03 wl  gBalance..-Variablen      TN1501   --> TBalanceDevice
  12.08.03 wl  gNullArray                TN1526   --> posTools.gmGetNullArray
  17.09.03 wl  Dist_RefPos_Tip1          TN1526   --> Rack
  18.09.03 wl  g24hMode                  TN1597   jetzt als integer (statt boolean)
  18.09.03 wl  ErrorDisableBitmap        TN1564   --> RoboticInterface
  18.09.03 wl  gWiperSpeedX,-Y           TN1526   neu: wird jetzt aus Area "ROBOT" eingelesen
  29.09.03 wl  gDispCh1and2together      TN1580   neu: aus sampler.dll hierher verschoben
  10.10.03 mo  gUseVolControl            TN1489   Schalter für Volumenkontrolle ZP02
  30.10.03 tbh gUseCorrectCarrierOffset  TN1635   neu: gUseCorrectCarrierOffset setzt Berechnungsart für Carrier-ZStartOffset bei TubeHandling
  23.12.03 wl  gTubeRetakeRotation       TN1712   neu
  23.02.04 pk  gHideVoluemsWindow        TN1760   neu
  04.03.04 pk  gDontPutBackToolForActionsTN1789   neu
  12.03.04 wl  GlobalZDispOffset,-TravelOffset,-ScanOffset,-MaxOffset  TN1812   nicht benötigt -> enfernt
  19.03.04 wl  global_CalliWPRTipName    TN1788   neu, Ersatz für TTipType.AName entfernt
  02.04.04 wl  gPipToolZTravel,gMinZTravelPipTool,gRediZRetrSpeed  TN1788    --> DevicesGrpArm
  02.04.04 wl  HTip1...                  TN1788    --> DevicesGrpArm
  02.04.04 wl  GlobalX,Y,ZOffset, GlobalZFactor   TN1788   durch TRobotArmDevice-Methoden ersetzt
  02.04.04 wl  gDilutorArray             TN1788    als AddDilutors --> DevicesGrpArm
  29.04.04 wl  gMTP_ShowNoRackButton     TN1887   neu
  05.05.04 pk                            TN1889.0 RunCreateSeqAuto, Paint from ScheUtil
  05.05.04 pk  gRunCreateRackNameRequired TN1889.0 New: if false racknames not checked during runcreate
  14.05.04 pk  gTubeBarcodesAreUnique     TN1920   New: if true, only unique SubstID's can exist for a tube in the database
  17.05.04 wl  gUseVolControl             TN1933   entfernt
  08.06.04 wl  gToolZTravel,gMinZTravelTubes  TN1963  --> DevicesXBasedMotorSystem/DevicesGrpArms
  08.06.04 pk  gRunStatusFlagEditMode     TN1974.1 New: How status flags should be edited in the showrun form
  17.06.04 wl  gFlushWithRediTips         TN1951.1 New
  17.06.04 wl  gMoveToRealPosBeforeWiper  TN1987.1 New
  21.07.04 pk  gSchedDatasourceType       TN2049   New : Scheduling is based on Method/Run or Script
  20.04.05 pk  gSchedMinTime              TN2395   New : MinTime allowed for Min/Max time field
  19.05.05 wl  gCapParkStrategy           TN2414   neu: für das Entnehmen von Deckeln aus CapParkRack
  25.05.05 wl  gFlushCycles               TN2427   entfernt
  02.06.05 pk  gRunCreateParserType       TN2449   New: RunCreateParserType - Choose between new and old parser
  15.06.05 pk  DistX_Pip2_Tip1            TN2464   New: Distance from Pip arm 2 X Motor to reference tip
  21.06.05 pk  Dist..._H_Tip1             TN2464.3 Removed
  01.08.05 pk  gShowDialogForDelayLongerThan TN2509  New : If delay is longer than this show the delay dialog
  08.08.05 pk  gPipDifferentOptionsTogether TN2524 New: allow pip steps to be combined even if they have different options
  25.08.05 wl  gSchedDataSourceType       TN2558.8 entfernt
  07.11.05 pk  gSchedShiftTime            TN2737   New : Amount of seconds by which the schedule is shifted
  08.11.05 wl  gAutoResetScript,gResetScriptAfterDDEStart  TN2745   entfernt
  30.11.05 wl  gReservedNames             TN2815   entfernt
  30.11.05 wl  global_CalliWPRTipName,gWashRemRediTips     TN2818   entfernt
  06.01.06 wl  gUseCorrectLevelHeights    TN2718   neu
  26.01.06 pk  gRunCreateInsertFreeB      TN2902   New : true=insert FreeB action into run, false=do not insert
  06.02.06 wl                             TN2928   neu gNoMoveIfRackAtDestination
  27.04.06 pk  gDiTiScanSpeed, gDiTiScanRamp TN2958 New : from RoboticInterface
  13.06.06 pk  gRunVarUsePriority         TN3154   New : true=use real priority. false= use priority 0.
  05.07.06 pk  gRunCreateUseMethVars      TN3181   New
  06.09.06 wl                             TN3288   Nicht benötigte Calli-Options entfernt
  22.09.06 wl  gTipTypes                  TN3326   entfernt
  12.12.06 wl                             TN3468   BorderFactor abgeschafft
  20.02.07 wl  gRunCreateParserType         TN3016   entfernt
  25.07.07 wl  gSaveSolvent               TN3792   entfernt
  09.11.07 pk  STEPS_PER_MM               TN3924   removed
  09.01.08 wl  gRunCreateSeqAuto          TN3972    entfernt
  09.01.08 wl  gSwitchPortsOnRestart      TN3972    entfernt
  09.01.08 wl                             TN3972    Master/Slave-Krempel  entfernt
  07.01.08 pk  gWorkbenchZ                TN3971   New: Z Height of workbench in mm
  30.01.08 wl  gOpenFixationAtInit        TN4003   --> CatMixConnection
  30.01.08 wl  gCVortexSleepTime          TN4003   --> CatMixConnection
  30.01.08 wl  gNoVortexerStopAtInit      TN4003   entfernt
  30.01.08 wl  gFixAfterVortexing         TN4003   --> IntfShakerDevice
  14.04.08 wl  GlobalErrBeep              TN4060   --> ErrorMessage
  20.06.08 pk  gGraphicsType              TN4139   New
  24.06.08 wl  gStatusBar                 TN4143   --> MessageHandling
  27.06.08 pk  gCarrierReverseY           TN4139   New
  03.07.08 wl                                         TN4157
  29.07.09 wl  Zoom,WBBorders             TN4693   entfernt
  28.09.09 pk  LogDisplay                 TN4753   removed
  04.11.09 pk                             TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  23.06.10 pk                             TN5163 New gUseDifferentTipTypesTogether
  29.06.10 pk                             TN5143 New gUseDifferentLiqParamsTogether
  15.07.10 pk                             TN5196 WaitAfterPickSys removed
  27.07.10 wl                             TN5123   unbenutzte Settings entfernt
  28.11.10 wl                             TN5355   Delay-Variablen entfernt
  27.04.11 wl  gWashOnlyUsedTips          TN5562   entfernt
  15.09.11 wl                             TN5694   ShowTempRequestTime,LogVortInfo entfernt
  27.09.11 wl  gRunCreatePaint            TN5698   entfernt
  19.10.11 wl  PaintTubes                 TN5723   entfernt
  28.10.11 wl  gTubeColors                TN5725   4 Werte, die TubeDispColor ersetzen
  28.10.11 wl  gWritePosinfo              TN5725   --> SubstanceLoading
  17.11.11 wl  gRunCreateRackNameRequired TN5725   weg, ist jetzt überflüssig
  18.06.12 wl  gEnableVolumeControl       TN5899   entfernt
  18.06.12 wl  gSysLiqMinVol              TN5899   --> Liquids
  10.08.12 wl  gUsePeriAtWash             TN5947   entfernt
  05.06.13 wl  gTubeToolData              TN6154   entfernt
  30.07.13 wl  ErrMessageActive           TN6160   entfernt
  18.09.13 wl                             TN6045   unbenutzte Settings entfernt
  10.12.13 wl  gCarrierReverseY           TN6326   entfernt
  -------------------------------------------------------------------------------------------------- }

unit SamGlobe;


interface


// =============================================
// ==== Diese Unit muss aufgelöst werden!!! ====
// = Hinzufügen von Variablen streng verboten! =
// =============================================

uses
    SysUtils,
    Windows,
    Classes,
    Graphics,
    ComCtrls,
    StdCtrls,
    Generics.Collections,
    AppTypes,
    CommonTypes;

var
    // ---------------------------------- Einstellungen für Rack,Carrier,Workbench
    gWorkbenchZ: double;

    AspSpeedDefault, AspSpeedMax, AspSpeedCalcFactor: word;
    DispSpeedDefault, DispSpeedCalcFactor: word;

    WRITE_LOG_INFO, WRITE_LOG_ERROR, WRITE_LOG_DEBUG, WRITE_LOG_SEND, WRITE_LOG_NO_NEW: integer;

    DTErrorMode: integer = 0; // Drop Disposal Tips Error Mode
    GTErrorMode: integer = 0; // Get Disposal Tips Error Mode

    gCDllFiles: string;
    gDetachDll: string;

    gPlateCheckPut, gPlateCheckGet: word;

    gWashDryDelay: Integer = 1000;

    gConfirmIgnore: boolean;

    gMoveTubeToBalance: TTubeOptions;
    gMoveTubeFromBalance: TTubeOptions;
    gMoveTubesAfterRestart: integer;

    g24hMode: integer;
    gChangePosinfo: boolean;
    gExpDefs: TExportDefs;
    gTubeColors: TTubeColors;
    gHideVolumesWindow: boolean;

    gDiTiIgnoreRestTips: Boolean; // lösche Spitzen aus DT Rack wenn nicht alle Nadeln aufnehmen
    gDiTiScanSpeed, gDiTiScanRamp: integer;

    // --------------------------------------------------------------------- aus SamIntf
    gCalledDLLs: TList<string>; // Liste aller aufgerufenen DLL's
    // ----------------------------------------------------------------------- aus SamWash
    gFlushVolAfterInit: Integer;
    gAirVolAfterInit: Integer;
    gRetractStepsAfterInit: Integer;
    gFlushVol: integer;
    gDispSubmergeActive: boolean;
    gAskForKeepContents: boolean = false;

    // REDI-Variablen
    gWiperSpeedX, gWiperSpeedY: integer;

    gAskForRackPlacement: Integer;
    gDilutorAspSpeedPerCent: integer;
    gHToolYCorrect: integer;
    gMoveRackSlotDelimiter: string;
    // ------------------------------------------------------ vorübergehend für DoCalli -> tbh 12.07.01
    gUseRackIDPos: boolean;

    gChangeTipTypes: Boolean;
    gUseDifferentTipTypesTogether: boolean;
    gUseDifferentLiqParamsTogether: boolean;

    gDbToolDllName: string; // Sami
    gWeighLogicDbName: string; // Weighing Wizard
    gWeighLogicDbUser: string; // Weighing Wizard
    gWeighLogicDbPassword: string; // Weighing Wizard

    gUseSeveralBalanceRacks: Boolean; // Tube Handling

    gShakeTimeAdjustable: Boolean;
    gDispCh1and2together: boolean;

    gUseCorrectCarrierOffset: Boolean;
    gUseCorrectLevelHeights: boolean;

    gTubeRetakeRotation: integer;
    gTubeBarcodesAreUnique: boolean;

    gDontPutBackToolForActions: string; // Actionhandler Dirty Hack

    gFlushWithRediTips: boolean;
    gMoveToRealPosBeforeWiper: boolean;

    gCapParkStrategy: string;

    gNoMoveIfRackAtDestination: boolean;


implementation


end.
