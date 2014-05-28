{ --------------------------------------------------------------------------------------------------
  Gemeinsames Objekt aller Hardware-Module:
  - Verwaltung aller externen seriellen Module
  - Fehlerbehandlung: zur Erkennung eines Moduls anhand der Adresse (Lissy-Module)
  - Simulationsmodus: zum einfachen Deaktivieren aller Hardware-Module
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure           Änderung / Neuerung
  -------- --  ---------------------------  --------------------------------------------------------
  10.09.98 wl                               neu: TSamModules
  SimulationMode von TSampler nach TSamModules
  TSamModules.Activate         aus dbTools
  TSamModules.Deactivate        dito
  TSamModules.CreateSamModules aus Samintf
  TSamModules.ShowList         aus Main
  13.10.98 wl  TSamModules                  FComModule: alle über Com-Port geschalteten Module
  TSamModules.Activate,Deactivate,ShowList  --> erweitert für COM-Module
  TSamModules.Add6WayValve     Ergänzt die SamModules um 1 oder mehrere 6-Wege-Ventile
  TSamModules.CreateSamModules hV-Motor wird jetzt immer mit aufgeführt
  11.11.98 mo                               alle sleep(..) in SamAppSleep(..) umbenannt
  06.01.99 wl  TSamModules.(De)Activate     Änderungen werden wirklich in SamplerPtr geschrieben
  21.01.99 wl                               uses Utility2
  02.02.99 wl  TSamModules.FindModule       Gibt zurück, ob es an Adresse ... Modultyp ... gibt
  18.08.99 wz                               --> String in Ressourcen
  01.09.99 wl                               Ändern der Res-Nummern in 1000 und 1010
  28.09.99 wl                               Aufruf von ResetAllModules geändert (SamCmd)
  05.01.00 wl  TSamModules.CreateSamModules Reihenfolge der Module geändert (Motoren vorn)
  12.01.00 wl  alle Funktionen              LogText() durch gmLogText() ersetzt (für Delphi 5)
  TSamModules.CreateSamModules Peri-Pumpe und Dilutoren in verschiedene Modul-Gruppen
  13.04.00 wl  ReadDeviceIniFile            aus SamStart hierher verschoben, NoOfVortexer wird bestimmt
  TSamModule,TComModule um Port erweitert
  neu: FTango
  14.04.00 wl                               neu: FConradBoard,FCompulabBoard
  FindDevicePart,SwitchOn,SwitchOff (aus SamCmd)
  ResoreNextStatus,ExecuteNextStatus,ResetRelay  neue RelayBoard-Methoden
  GetActTemp,SetTemperature,CheckTemperature  neue Thermostat-Methoden
  ReadConradPort,Create        Teile aus SamStart hierher verschoben
  18.04.00 wl                               TDevicePart(Type) mit TModule(Type) zusammengefaßt
  AddModule,WriteModule        Schreibt kompletten Device-Part
  VortexerInit,SetVortexer,AllVortexerOFF,GetVortexerSpeed,Fixation  neue Methoden für Vortexer
  CheckTemperature,SetTemperature  erweitert für CAT-Vortexer
  19.04.00 wl  alle Methoden                bei Simulation passiert nichts
  CreateSamModules             Debug: FComDevType wurde bei Conrad-Board-Init mit negativen Index beschrieben
  27.04.00 wl                               TModules in eigener Unit (ObjModul)
  28.04.00 wl  FindVortexer,Find..          Suchfunktionen für bestimmte Device-Typen (Aufbau immer gleich)
  InUse                        Ersetzt GetSpeed-Funktion (für Vortexer)
  28.04.00 wl  ShakerOn                     LogText-Angabe mit Speed und Pulse-Werten
  10.05.00 wl  GetActTemp                   Rückgabewert: single
  11.05.00 wl  alle Funktionen              GlobalErrPtr wird abgefragt
  17.05.00 wl  ExecuteNextStatus            auch ohne Conrad-Board muß gewartet werden
  26.05.00 wl  InitSamModule                ersetzt InitSamGlobals (SamStart), ohne Sampler.dll-Initialisierung
  26.05.00 wl                               Deklaration gLiquids,gModules,gDevices hier (sollen irgendwann vereinigt werden)
  30.05.00 wl  CreateSamModules             Com-Module erzeugen --> ObjModulA (CreateComModules)
  30.05.00 wl  ResetAllModules,RestoreNextStatus,ExecuteNextStatus,ResetRelay,SwitchOn,SwitchOff,
  Turn6WayValve                nur noch Dummies --> ObjModulA
  06.06.00 wl  FindTypes                    schlimmen Fehler beseitigt
  06.06.00 wl  InitIntf_Mini                minimal-Version von InitIntf für den Editor
  16.06.00 wl                               FVortexCarrier: StringListe mit allen Thermostat-Devicenamen
  20.06.00 wl  ShutDown                     jetzt virtual
  04.07.00 wl  GetDeviceList,GetSwitchList  Erstellt eine Stringlist mit allen Switches (oder anderes)
  04.07.00 wl  FindDevicePart               Sucht jetzt unter allen Modulen (nicht nur Deviceparts)
  04.07.00 wl  CreateSamModules             alle Ports werden jetzt einzeln als Switches definiert
  04.07.00 wl  CreateSamModules             Beeper,Stirrer,Sensoren werden auch als Switch definiert
  05.07.00 wl  LoadRediDevices              aus ObjArm (TTip)
  07.07.00 tbh ResetExternModules           Tango wird abgeschaltet
  10.07.00 wl  Find_ByType,Find_ByDevice    entstanden aus FindTypes: verschiedene Suchkriterien
  10.07.00 wl  Read6WayValve                benutzt neue Funktion Find_ByType
  10.07.00 wl  VortexerInit                 Votexer Init erscheint in Log-Display
  12.07.00 wl  Add6WayValve,Read6WayValve   --> ObjModulA (damit der Sampler nicht mehr blockiert wird)
  14.07.00 wl  alle Vortexer- und Thermostat-Methoden  --> ObjModulA
  17.07.00 wl  ReadDeviceIniFile            alle gmLogText-Befehle entfernt --> ObjModulA
  17.07.00 wl  WriteModule                  neue Property .Device wird mit beschrieben
  26.07.00 wl  CreateSamModules             ReadDeviceIniFile vor CreateComModules gesetzt
  02.08.00 wl  ReadDeviceIniFile            Protocol wird mit bestimmt
  02.08.00 wl  GetDeviceList                jetzt public
  07.08.00 wl                               FVortexCarrier überflüssig -> raus
  07.08.00 wl  GetModulDeviceList           Die Devicenamen von Devices, auf denen Modules eines best. Typs sitzen
  07.08.00 wl  GetModuleList                = GetDeviceList, erweitert durch die Angabe eines DeviceNamens
  07.08.00 wl  LoadRediDevices,Find_ByDevice gDevices weitestgehend ersetzt durch FModule
  07.08.00 wl  SwitchOffAllShakers,SwitchOnAllShakers  ersetzt alte Shaker-Funktionen in SamCmd & SamIntf
  08.08.00 wl  CreateSamModules             Z-Motoren und Dilutoren werden normal aus Sam. eingelesen
  09.08.00 wl  ReadDeviceIniFile            Alle Module mit Port, die unter 'MACHINE' stehen, sind Switches
  09.08.00 wl  Find_ByDevice,LoadRediDevices,GetModuleList  Bug gefunden: Device statt Name in if-Abfrage
  16.08.00 wl  SwitchOnAllShakers           Bug in 'StSha' beseitigt
  16.08.00 wl  SwitchRackDevice             von gDevices auf FModule umgestellt
  16.08.00 wl  ReadDeviceIniFile            gDevices wird nicht mehr benutzt  :-)
  31.08.00 wl  FindAndSetLabel              entspricht Find_ByDevice, aber Display wird gesetzt
  06.11.00 tbh ReadTTLIn                    neu: Dummy-funktion zum Auslesen der TTL-Eingänge
  13.03.01 tbh GetDevicePartList            neu: erstellt ein DevicePartArray eines Deviceparttypes aller Vortexer
  13.03.01 tbh ThermoDevicePresent          neu: prüft ob überhaupt ein Vortexer oder Tango present ist
  28.03.01 mo  ReadDeviceIniFile            Wenn unter Device MACHINE ein Port 0 ist kann er jetzt geschaltet werden
  01.06.01 tbh GetDevicePartList            auch verschiede DeviceTypen können in einer Liste gesammelt werden
  01.06.01 tbh ReadDeviceIniFile            ergänzt um Eintrag für IKA Vortexer
  03.08.01 tbh ThermoDevicePresent          um IKA-Vortexer erweitert
  07.09.01 tbh ReadDeviceIniFile            TN1034 neu: [RS232] CVortexSleepTime setzt SleepTime des seriellen Objekts für alle CatVortexer
  14.10.01 mo                               TN1067 Merge mit SIAS Änderungen
  02.11.01 tbh FindByRackName               TN1050 neu: findet Device durch den zugeordneten Racknamen
  06.11.01 mo  CreateSamModules             TN1085 Relais Board 5 wird in Modul Liste eingetragen
  03.01.02 tbh LoadRediDevices              TN1158 neuer Part Feeding wird gesetzt
  11.06.02 tbh InitIntf_Mini                TN1208 FehlerCounter-Zeiger für Adressen auf 2. X-Main-Board wird gesetzt
  18.07.02 mo  Turn6WayValve                TN1109 Umbenannt in TurnSystemLiquidValve
  13.09.02 wl                               TN1283 Merge mit SIAS
  27.09.02 wl  InitIntf_Mini                TN1294  DllHandle wird auf 0 statt NULL abgefragt
  10.10.02 wl                               TN1293.2 Ini Access - uses geändert
  11.10.02 wl                               TN1293.2 neuer Aufruf von InitSamGlobals1
  15.10.02 mo CreateSamModules              TN1301 ifdef SIAS -> XAP
  15.10.02 mo InitSamModule                 TN1301 ifdef SIAS -> XAP
  15.10.02 mo InitSamModule                 TN1293.2 Aufruf von InitSamGlobals1 --> Projekt-Quelltext
  25.10.02 wl  CreateSamModules             TN1293.1 verwendet TSimpleIniAccess für Device.ini
  12.12.02 wl  Create(SamModules)           TN1345   Erzeugen der Sam-Module --> ObjModulA (dort vorübergehend deaktiviert)
  12.12.02 wl  SetSimulationMode            TN1345   Dummy, Inhalt --> ObjModulA
  12.12.02 wl  InitIntf_Mini                TN1345   entfernt - Editor soll ohne DLL auskommen
  20.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  27.12.02 wl                               TN1293.5 uses Device: TDevice statt TDevicePart
  14.01.03 wl  ReadDeviceIniFile            TN1293.5 geänderter Aufruf von CreateSimpleIni
  28.01.03 wl                               TN1345 abgeleitet von TCompositeDevice (DeviceList.pas)
  28.01.03 wl  Find- & List-Methoden        TN1345 --> TCompositeDevice (DeviceList.pas)
  12.02.03 wl  InitSamModule                TN1293.5 erzeugt TMinimalFakeInterface
  24.03.03 wl  GetProtocolFromPort          TN1455  simpler Bug - korrigiert
  13.06.03 wl  AddBCReaders, BCReaderSetBounds TN1501.1 neue Dummy-Methoden
  27.06.03 wl  ReadDeviceIniFile            TN1456  neu: 'Type' für Device auslesen (statt Type aus Namen bestimmen)
  27.06.03 wl  ReadDeviceIniFile            TN1501.2 wenn 'Type' für Device = 'DECAPPER' -> TDecapper erzeugen
  27.06.03 wl                               TN1501.2 Änderungen durch Änderung von TDevice
  27.06.03 wl  GetDecapper                  TN1501.2 Neue Suchfunktion für Decapper
  03.07.03 wl                               TN1501.1 Device-Struktur komplett überarbeitet
  04.07.03 wl                               TN1501.1 Änderungen für Decapper-Device
  08.07.03 wl                               TN1501.1 Umstellung BCReader: Barcodestr entfernt
  08.07.03 wl                               TN1536   Umstellung CommunicationManager
  14.07.03 wl  SetSimulationMode            TN1535.1 entfernt: wird in Communication bestimmt
  16.07.03 wl  FindTubeReadingDevice        TN1501.1 neu
  16.07.03 wl  FindDecapperDevice           TN1501.2 neu
  24.07.03 wl  FindBalance                  TN1536   neu: Balance-Device wird zurückgegeben
  28.07.03 wl  ReadDeviceIniFile            TN1536   SwitchOn/SwitchOff lesen -> DeviceList.AddLeafDevice
  29.07.03 wl  ReadTTLIn                    TN1501.3 --> TRoboticInterfaceZP01.Sensor_ReadAsBool
  29.07.03 wl  FindSensor                   TN1501.3 sucht Sensor anhand der Tip-Nummer (für POSPRESS)
  30.07.03 wl  SwitchOnAllShakers           TN1501.2 Delay in Switch-Befehl statt in ExecuteRelay
  03.08.03 wl  GetSimulationMode            TN1535.1 von Device hierher
  03.08.03 wl  FindAndSetLabel              TN1535.1 --> VortInfo
  04.08.03 wl  FindWeighDevice              TN1501.10  sucht WeighingDevice
  25.08.03 wl  FindWeighDevice,FindBalance  TN1556   überarbeitet (Schleife wurde zu oft durchlaufen)
  02.09.03 wl  SwitchRackDevice             TN1559   Char-Arrays durch string ersetzt
  03.09.03 wl  FindTubeReadingDevice        TN1568   --> ObjModulA (für Editor)
  03.09.03 wl  InitSamModule                TN1568   --> EdMain
  19.09.03 mo  TModules.FindVortexer        TN1598 dptShakerIKAOld eingefügt
  15.10.03 wl  FindPipPump                  TN1624   Suchfunktion für neuen Devicetyp: dptPipPump
  18.12.03 wl                               TN1672   uses geändert
  08.01.04 wl  FindPipPump                  TN1672   --> DeviceList
  05.02.04 wl  FindDecapperDevice           TN1729.1  such nach Capper ODER Decapper
  12.03.04 wl  SwitchRackDevice             TN1812   unnötiges GetRackData entfernt
  19.03.04 wl  FTipManager                  TN1788   gTipManager wird hier erzeugt und zerstört
  02.04.04 wl  CreateTipManager             TN1788   wird nur mit Anzahl der Tips erzeugt
  20.04.04 wl  SwitchRackDevice             TN1788   TipIntegerArray statt TipWordArray
  28.04.04 wl                               TN1788   benutzt TCustomArmDevice
  08.06.04 wl  Create                       TN1963   gTipManager wird (hier) immer vor den ArmDevices erzeugt
  26.07.04 pk  TModules.Create              TN2052   try...except
  30.09.04 wl                               TN2157   verschiedene Bezeichner durch const-Werte ersetzt
  30.09.04 wl  FindStateSignal              TN2157.1 neu: für TStateSignalDevice (Ampel)
  16.02.05 wl  ShutDown                     TN2269   unnötig, entfernt
  16.02.05 wl  AddSystemLiquidValve         TN2269   Relay-Valve wird als einzelne Switch-Ventile erzeugt!
  16.02.05 wl  AddSystemLiquidValve         TN2269   Rosys-6-wege-Ventil entfernt
  16.02.05 wl  AddSystemLiquidValve         TN2269   bei allen Ventilen werden die In-Ports mit definiert
  09.03.05 wl  FindVortexer,FindThermostat  TN2340   Rückgabewert jetzt TThermoDevice bzw. TShakerDevice
  09.03.05 wl  List_AllSwitches             TN2340   ersetzt ListSwitch_ByArea
  06.09.05 wl  ReadDeviceIniFile            TN2593   neu: "TipNo" wird mit eingelesen
  23.09.05 wl  ReadDeviceIniFile            TN2624   Bit wird als String gelesen
  23.09.05 wl  ReadDeviceIniFile            TN2624   DllName wird jetzt auch mit gelesen
  03.10.05 wl  ReadAndAddLeafDevice         TN2624   Fehlerbehandlung bei Exceptions
  14.10.05 wl  FindVortexer,FindThermostat  TN2670   sucht jetzt nach Devicename
  15.11.05 pk  Find_ByRackNameAndClass      TN1754   New find device by namecut and rackname
  22.12.05 pk  UserProtRequestLine          TN2875   Removed
  23.02.06 thr AddLeafDevice                TN2941   A Leafdevice now has a carriername
  23.02.06 thr FindBalanceByName,FindBalanceByCarrierName,FindWeighDeviceByName,FindWeighDeviceByBalanceName,
  FindWeighDeviceByCarrierName TN2941   New Functions for finding Balances
  08.03.06 thr FindAllBalances              TN2941   Fills an array of Balancedevices
  31.03.06 pk  ReadDeviceIniFile            TN2958   ReadAndAddComposedDevice with level=1
  06.04.06 pk  FindArmByPipDevice           TN3023   from SubstanceHandling
  12.04.06 pk  InitializeInitID             TN2997   set fInitID
  25.08.06 thr TModules.ThermoDevicePresent TN3264   Liefert auch gleich eine Liste der Devices zurück
  23.10.06 pk  TModules.Create              TN3373   Call the arm loaddevices before CreateComModules
  19.01.07 wl  List_AllSwitches             TN3473   neue Option: dann werden Devices mit Nummern am Ende auch mit * am Ende angezeigt
  07.02.07 wl  SwitchRackDevice             TN3143   Aufruf von gmGetCoord angepasst
  22.02.07 pk  Destroy                      TN3583   call gArmManager.Free
  02.03.07 pk  LoadRediDevices              TN3613   Moved to TRediDevice.LoadDevices
  07.03.07 wl  FindPeriPump                 TN3620   neu, sucht TPeriPumpDevice
  12.11.07 pk                               TN3864   Find.. functions use DeviceManager.FindModule/Ext
  12.11.07 pk                               TN3864   New SetOnCheckSimulated: set the simulation check event for all connections
  07.01.08 pk  DestroyAndUnloadModules      TN3864   New: Disconnect, Unprepare and Free modules
  07.01.08 pk  CreateAndLoadModules         TN3864   Call Connect before calling Prepare
  11.01.08 pk  LoadTypes                    TN3864   Path changed to Lib/Plugins
  07.01.08 pk  InitArmBounds                TN3971   Arm bounds ZMax removed
  30.01.08 pk                               TN3864   ListAll function implemented again
  07.02.08 wl  SwitchOn/OffAllShakers       TN4009   --> PowderHandling
  07.02.08 wl  GenerateResetID              TN4009   neu
  07.02.08 wl  TModuleManager               TN4009   Fehler werden geloggt und in eine Liste geschrieben
  07.02.08 wl  CreateAndLoadModules         TN4009   statt einzelner Fehler wird eine Fehlerliste erzeugt
  17.03.08 wl                               TN4043   uses IntfMixerDevice
  14.04.08 wl  LoadTypes                    TN4060   Loading from plugin package --> PluginLoader
  15.04.08 pk  InitArmManager               TN3864   Searchindex is no longer incremented
  10.05.08 wl  FindBalance,FindBalanceByName,FindAllBalances  TN4105  Dircet Search for Balance, not the weighing device
  21.05.08 wl  LoadTypes                    TN4119   calls just the TPluginLoader
  20.06.08 pk  InitArmBounds                TN4139   deactivated out for now
  03.07.08 wl                                         TN4157
  11.07.08 wl                               TN4164  keine virtuellen Funktionen mehr, da nicht mehr durch TActionModules abgeleitet
  11.07.08 wl  TModuleFinder                TN4164  neuer Name der Klasse
  11.07.08 wl  fInitID                      TN4164   Init & Reset-Funktionen -> ObjModulA
  08.09.08 pk  FindBalanceByNameOrDef..     TN4215   New
  13.10.08 pk  Create                       TN4272.2 Create Appinstance for Device, Driver, Connection
  17.12.08 pk  ConnectModules               TN4372   Moved to ObjModulA
  17.12.08 pk  CreateAndLoadModules         TN4374   Create SettingsManager instances
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  27.08.09 pk  Create                       TN4753   some code moved to AppInstancePeripheryManagement
  28.08.09 pk                               TN4753   SetOnCheckSimulated moved to ActionModules
  02.09.09 pk  Destroy                      TN4753   no longer calls gmUnloadLibs
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                               TN5044   uses geändert
  15.11.10 pk                               TN5340   Changes to prevent memory leak
  09.04.11 wl  FindTubeReadingDevice        TN5545   entfernt
  04.10.12 wl  Find_ByRackNameAndClass      TN5991   Funktion wurde für V7.3 grundlos geändert - jetzt wieder wie früher
  31.10.12 pp  FindUserProtectionDevice:    TN6002   Funktin wurde hinzugefügt, Rückgabewert = IUserProtectionDevice;
  16.01.13 wl  GetXMotor                    TN6066   von ArmConflictManager hierher
  10.04.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit ObjModul;


interface


uses
    ExtCtrls,
    SysUtils,
    GeneralTypes,
    Generics.Collections,
    CommonTypes,
    AppTypes,
    Device,
    Driver,
    ModuleTypeDictionary,
    ModuleSettings,
    Module,
    IntfPipDevice,
    IntfSwitchDevice,
    IntfSensorDevice,
    IntfPeriPumpDevice,
    IntfArmDevice,
    IntfDecapperDevice,
    IntfWeighingDevice,
    IntfBalanceDevice,
    IntfMixerDevice,
    IntfThermostatDevice,
    IntfStateSignalDevice,
    IntfSoftwareProtectionDevice,
    IntfUserProtectionDevice,
    IntfMotorDevice,
    IntfMotorDriver,
    IntfMotorBasedMotionDevice,
    IntfContainerBCReaderDevice;

type
    TArmDeviceList = class(TList<IArmDevice>)
    protected
        function GetArm(aIndex: integer): IArmDevice;
    public
        property Arms[aIndex: integer]: IArmDevice read GetArm; default;
    end;

    TModuleFinder = class
    private
        procedure InitArmManager();
        procedure InitArmBounds;
        procedure CreateAndLoadModules();
        procedure DestroyAndUnloadModules();
    public
        // constructor/destructor
        constructor Create();
        destructor Destroy; override;

        class function ErrorText(aList: TList<string>): string;

        // find and list functions
        function ListAll(aModuleID: TModuleID): TStringArray;
        function Find(const aIID: TModuleID; out oIntf: TModule): boolean;
        function Find_ByName(const aName: string; const aIID: TModuleID; out oIntf): boolean;
        function Find_ByNameCut(const aSubString: string; const aIID: TModuleID;
            out oIntf: ISwitchDevice): boolean;
        function Find_ByRackNameAndClass(const aSubString: string; const aRackName: string;
            const aIID: TModuleID; out oIntf: ISwitchDevice): boolean;
        function FindVortexer_ByNameCut(const aSubString: string; out oIntf: IMixerDevice): boolean;
        function FindThermostat(aName: string): IThermostatDevice;
        function FindVortexer(aName: string): IMixerDevice;
        function ListAllGripArms(): TStringArray;
        function FindDecapperDevice(aRackName: string; aAction: TDecapperAction): IDecapperDevice;
        function FindBalance: IBalanceDevice;
        procedure FindAllBalances(var vBalanceArray: TBalanceArray);
        function FindBalanceByName(aBalanceName: string): IBalanceDevice;
        function FindBalanceByCarrierName(aCarrierName: string): IBalanceDevice;
        function FindWeighDevice: IWeighingDevice;
        function FindBalanceDeviceByNameOrDefaultDevice(const aBalanceName: string): IBalanceDevice;
        function FindWeighDeviceByName(const aWeighDeviceName: string): IWeighingDevice;
        function FindWeighDeviceByCarrierName(const aCarrierName: string): IWeighingDevice;
        function FindWeighDeviceByBalanceName(const aBalanceName: string): IWeighingDevice;
        function FindSoftwareProtectionDevice(): ISoftwareProtectionDevice;
        function FindUserProtectionDevice(): IUserProtectionDevice;
        function FindSensor(aTipNumber: integer): ISensorDevice;
        function FindStateSignal: IStateSignalDevice;
        function FindPeripump(): IPeripumpDevice;
        function FindFirst(aExpectedModuleID: TModuleID; out oIntf): boolean;
        function ModuleTypeExists(aExpectedModuleID: TModuleID): boolean;

        // RobotArm-Methoden
        function FindArmNames(aFindGripArms, aFindPipArms: boolean): TStringArray;
        function FindArm(aFindGripArms, aFindPipArms: boolean; aFindName: boolean; aName: string): IArmDevice;
        function FindGripArmByName(const aName: string): IArmDevice;
        function FindFirstGripArm(): IArmDevice;
        function FindGripArmByNameOrFirst(const aName: string): IArmDevice;
        function FindPipArmByName(const aName: string): IArmDevice;
        function FindFirstPipArm(): IArmDevice;
        function FindPipArmByNameOrFirst(const aName: string): IArmDevice;
        function FindArmByPipDevice(aPipDevice: IPipDevice): IArmDevice;

        class function GetXMotor(aUsedArm: IArmDevice; out oXMotor: IXMotorDevice): boolean; static;
    end;

var
    gModules: TModuleFinder;
    gArmManager: TArmDeviceList;


implementation


uses
    Dialogs,
    Forms,
    LogManager,
    Utility2,
    GUIManager,
    DeviceManager,
    DriverManager,
    ConnectionManager,
    PipDeviceManager,
    TypeInfo,
    Connection,
    LibLoader,
    ConnectionTypeDictionary,
    DriverTypeDictionary,
    DeviceTypeDictionary,
    PluginLoader,
    AppInstanceModuleConnection,
    AppInstanceModuleDriver,
    AppInstanceModuleDevice,
    ConnectionSettingsManager,
    DriverSettingsManager,
    DeviceSettingsManager;

{ TArmDeviceList }

function TArmDeviceList.GetArm(aIndex: integer): IArmDevice;
begin
    result := inherited Items[aIndex] as IArmDevice;
end;

{ TModuleFinder }

constructor TModuleFinder.Create();
begin
    inherited Create();
    try
        gArmManager := TArmDeviceList.Create;
        gPipDeviceManager := TPipDeviceManager.Create;

        CreateAndLoadModules();

        InitArmManager();
        InitArmBounds();
    except
        on E: Exception do
        begin
            gLogManager.Log(e.message, false);
            gGUIManager.MessageBox(e.message, '', 0);
            raise;
        end;
    end;
end;

destructor TModuleFinder.Destroy;
begin
    gPipDeviceManager.Free;
    gArmManager.Free;
    DestroyAndUnloadModules();
    inherited;
end;

class function TModuleFinder.ErrorText(aList: TList<string>): string;
var
    x: integer;
begin
    result := '';
    for x := 0 to aList.Count - 1 do
    begin
        if x >= 1 then
            result := result + #13#10;
        result := result + aList[x];
    end;
end;

procedure TModuleFinder.CreateAndLoadModules();
var
    xLoadErrors: TList<string>;
begin
    xLoadErrors := TList<string>.Create;
    try
        gConnectionManager := TConnectionManager.Create(TConnectionSettingsManager.Instance);
        gConnectionManager.CreateModules(xLoadErrors);
        gConnectionManager.PrepareModules(xLoadErrors);

        gDriverManager := TDriverManager.Create(TDriverSettingsManager.Instance);
        gDriverManager.CreateModules(xLoadErrors);
        gDriverManager.PrepareModules(xLoadErrors);

        gDeviceManager := TDeviceManager.Create(TDeviceSettingsManager.Instance);
        gDeviceManager.CreateModules(xLoadErrors);
        gDeviceManager.PrepareModules(xLoadErrors);

        // self.ConnectModules( xLoadErrors );

        if (xLoadErrors.Count > 0) then
            gGUIManager.MessageBox(ErrorText(xLoadErrors), 'Load Errors', 16);
    finally
        FreeAndNil(xLoadErrors);
    end;
end;

procedure TModuleFinder.DestroyAndUnloadModules();
var
    xUnloadErrors: TList<string>;
begin
    xUnloadErrors := TList<string>.Create;
    try
        // self.DisconnectModules( xUnloadErrors );
        gConnectionManager.UnPrepareModules(xUnloadErrors);

        if (xUnloadErrors.Count > 0) then
            gGUIManager.MessageBox(ErrorText(xUnloadErrors), 'Unload Errors', 16);

        gConnectionManager.DestroyModules();

        FreeAndNil(gDeviceManager);
        FreeAndNil(gDriverManager);
        FreeAndNil(gConnectionManager);
    finally
        FreeAndNil(xUnloadErrors);
    end;
end;

procedure TModuleFinder.InitArmBounds();
begin
    { TODO -oPK : WB }
    {
      var
      x : integer;
      xXMin, xXMax, xYMin, xYMax : TPosMM;
      xCurXMin, xCurXMax, xCurYMin, xCurYMax : TPosMM;

      begin

      xXMin := High( integer );
      xXMax := Low( integer );
      xYMin := High( integer );
      xYMax := Low( integer );

      for x := 0 to gArmManager.Count - 1 do begin
      gArmManager.Arms[ x ].MotionDevice.GetXYBounds( xCurXMin, xCurXMax, xCurYMin, xCurYMax );
      gArmBoundManager.AddArmBound( TArmBound.Create( gArmManager.Arms[ x ].Name, xCurXMin, xCurXMax, xCurYMin, xCurYMax,  gArmManager.Arms[ x ].Color ) );
      if xCurXMin < xXMin then xXMin := xCurXMin;
      if xCurXMax > xXMax then xXMax := xCurXMax;
      if xCurYMin < xYMin then xYMin := xCurYMin;
      if xCurYMax > xYMax then xYMax := xCurYMax;
      end;

      gArmBoundManager.SetAllArmBound( TAllArmBound.Create( xXMin, xXMax, xYMin, xYMax ) );
    }
end;

procedure TModuleFinder.InitArmManager();
var
    x: integer;
    xArm: IArmDevice;
    xSearchIndex: integer;
begin
    xSearchIndex := 0;
    while true do
    begin
        xArm := nil;
        if not gDeviceManager.FindModuleExt(IArmDevice, xSearchIndex, xArm) then
            BREAK;
        gArmManager.Add(xArm);
    end;

    for x := 0 to gArmManager.Count - 1 do
        if Assigned(gArmManager.Arms[x].PipDevice) then
            gPipDeviceManager.AddArm(gArmManager.Arms[x].PipDevice);
end;

function TModuleFinder.Find(const aIID: TModuleID; out oIntf: TModule): boolean;
var
    xIndex: integer;
begin
    xIndex := 0;
    result := gDeviceManager.FindModuleExt(aIID, xIndex, oIntf);
end;

function TModuleFinder.Find_ByNameCut(const aSubString: string; const aIID: TModuleID;
    out oIntf: ISwitchDevice): boolean;
var
    xIndex: integer;
begin
    xIndex := 0;
    while true do
    begin
        oIntf := nil;
        result := gDeviceManager.FindModuleExt(aIID, xIndex, oIntf);
        if not result then
            EXIT;
        if (Pos(Uppercase(aSubString), Uppercase(oIntf.Name)) = 1) then
            EXIT
    end;
end;

function TModuleFinder.FindVortexer_ByNameCut(const aSubString: string; out oIntf: IMixerDevice): boolean;
var
    xIndex: integer;
begin
    xIndex := 0;
    while true do
    begin
        oIntf := nil;
        result := gDeviceManager.FindModuleExt(IMixerDevice, xIndex, oIntf);
        if not result then
            EXIT;
        if (Pos(Uppercase(aSubString), Uppercase(oIntf.Name)) = 1) then
            EXIT;
    end;
end;

function TModuleFinder.Find_ByName(const aName: string; const aIID: TModuleID; out oIntf): boolean;
begin
    result := gDeviceManager.FindModule(false, aName, aIID, oIntf);
end;

function TModuleFinder.Find_ByRackNameAndClass(const aSubString: string; const aRackName: string;
    const aIID: TModuleID; out oIntf: ISwitchDevice): boolean;
// Will find a device which is of class aClass and
// which has a name that begins with aSubString and has a RackName which is the same as aRackName
var
    xIndex: integer;
begin
    xIndex := 0;
    while true do
    begin
        oIntf := nil;
        result := gDeviceManager.FindModuleExt(aIID, xIndex, oIntf);
        if not result then
            EXIT;
        if (Pos(Uppercase(aSubString), Uppercase(oIntf.Name)) = 1) and
            (Uppercase(aRackName) = Uppercase(oIntf.RackName)) then
            EXIT;
    end;
end;

class function TModuleFinder.GetXMotor(aUsedArm: IArmDevice; out oXMotor: IXMotorDevice): boolean;
var
    xMotorBasedMD: IMotorBasedMotionDevice;
begin
    oXMotor := nil;

    if not SysUtils.Supports(aUsedArm.MotionDevice, IMotorBasedMotionDevice, xMotorBasedMD) then
        EXIT(false);

    oXMotor := xMotorBasedMD.XMotor;
    EXIT(Assigned(oXMotor));
end;

function TModuleFinder.FindThermostat(aName: string): IThermostatDevice;
begin
    gDeviceManager.FindModule(false, aName, IThermostatDevice, result);
end;

function TModuleFinder.FindUserProtectionDevice: IUserProtectionDevice;
var
    xIndex: integer;
begin
    xIndex := 0;
    gDeviceManager.FindModuleExt(IUserProtectionDevice, xIndex, result);
end;

function TModuleFinder.FindVortexer(aName: string): IMixerDevice;
begin
    gDeviceManager.FindModule(false, aName, IMixerDevice, result);
end;

function TModuleFinder.ListAll(aModuleID: TModuleID): TStringArray;
var
    xIndex: integer;
    xIntf: IModule;
    xList: TList<string>;
begin
    xIndex := 0;
    xList := TList<string>.Create();
    try
        while true do
        begin
            xIntf := nil;
            if not gDeviceManager.FindModuleExt(aModuleID, xIndex, xIntf) then
                EXIT;
            xList.Add(xIntf.Name);
        end;
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

function TModuleFinder.ListAllGripArms(): TStringArray;
var
    xIndex: integer;
    xIntf: IArmDevice;
    xList: TList<string>;
begin
    xIndex := 0;

    xList := TList<string>.Create();
    try
        while true do
        begin
            xIntf := nil;
            if not gDeviceManager.FindModuleExt(IArmDevice, xIndex, xIntf) then
                EXIT;
            if Assigned(xIntf.GripDevice) then
                xList.Add(xIntf.Name);
        end;
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

function TModuleFinder.FindDecapperDevice(aRackName: string; aAction: TDecapperAction): IDecapperDevice;
var
    xIndex: integer;
    xIntf: IDecapperDevice;
    xFound: boolean;
begin
    result := nil;
    xIndex := 0;
    while true do
    begin
        xFound := gDeviceManager.FindModuleExt(IDecapperDevice, xIndex, xIntf);
        if not xFound then
            EXIT;
        if (Uppercase(aRackName) = Uppercase(xIntf.TubePosRackName)) and (xIntf.ActionPossible(aAction)) then
        begin
            result := xIntf;
            EXIT
        end;
    end;

end;

function TModuleFinder.FindWeighDevice: IWeighingDevice;
var
    xIndex: integer;
begin
    xIndex := 0;
    gDeviceManager.FindModuleExt(IWeighingDevice, xIndex, result);
end;

function TModuleFinder.FindBalanceDeviceByNameOrDefaultDevice(const aBalanceName: string): IBalanceDevice;
begin
    if aBalanceName <> '' then
        result := FindBalanceByName(aBalanceName)
    else
        result := FindBalance;
end;

function TModuleFinder.FindSoftwareProtectionDevice(): ISoftwareProtectionDevice;
var
    xIndex: integer;
begin
    xIndex := 0;
    gDeviceManager.FindModuleExt(ISoftwareProtectionDevice, xIndex, result);
end;

procedure TModuleFinder.FindAllBalances(var vBalanceArray: TBalanceArray);
var
    xCount: integer;
    xIndex: integer;
    xIntf: IBalanceDevice;
    xFound: boolean;
begin
    xCount := 0;
    SetLength(vBalanceArray, xCount);

    xIndex := 0;
    while true do
    begin
        xIntf := nil;
        xFound := gDeviceManager.FindModuleExt(IBalanceDevice, xIndex, xIntf);
        if not xFound then
            EXIT;
        Inc(xCount);
        SetLength(vBalanceArray, xCount);
        vBalanceArray[xCount - 1] := xIntf;
    end;
end;

function TModuleFinder.FindBalance: IBalanceDevice;
begin
    gDeviceManager.FindFirst(IBalanceDevice, result);
end;

function TModuleFinder.FindBalanceByCarrierName(aCarrierName: string): IBalanceDevice;
var
    xWeighDevice: IWeighingDevice;
begin
    xWeighDevice := FindWeighDeviceByCarrierName(aCarrierName);
    if xWeighDevice <> nil then
        Result := xWeighDevice.Balance
    else
        result := nil;
end;

function TModuleFinder.FindBalanceByName(aBalanceName: string): IBalanceDevice;
var
    xIndex: integer;
    xFound: boolean;
begin
    result := nil;
    xIndex := 0;
    while true do
    begin
        xFound := gDeviceManager.FindModuleExt(IBalanceDevice, xIndex, result);
        if not xFound then
            EXIT;
        if (Uppercase(aBalanceName) = Uppercase(result.Name)) then
            EXIT
    end;
end;

function TModuleFinder.FindWeighDeviceByBalanceName(const aBalanceName: string): IWeighingDevice;
var
    xIndex: integer;
    xFound: boolean;
    xIntf: IWeighingDevice;
begin
    result := nil;
    xIndex := 0;
    while true do
    begin
        xIntf := nil;
        xFound := gDeviceManager.FindModuleExt(IWeighingDevice, xIndex, xIntf);
        if not xFound then
            EXIT;
        if not Assigned(xIntf.Balance) then
            CONTINUE;
        if (Uppercase(aBalanceName) = Uppercase(xIntf.Balance.Name)) then
            EXIT
    end;
end;

function TModuleFinder.FindWeighDeviceByCarrierName(const aCarrierName: string): IWeighingDevice;
var
    xIndex: integer;
    xIntf: IWeighingDevice;
    xFound: boolean;
begin
    result := nil;
    xIndex := 0;
    while true do
    begin
        xIntf := nil;
        xFound := gDeviceManager.FindModuleExt(IWeighingDevice, xIndex, xIntf);
        if not xFound then
            EXIT;
        if not Assigned(xIntf.Balance) then
            CONTINUE;
        if xIntf.Balance.GetCarrierName = aCarrierName then
        begin
            result := xIntf;
            EXIT;
        end;
    end;
end;

function TModuleFinder.FindWeighDeviceByName(const aWeighDeviceName: string): IWeighingDevice;
begin
    gDeviceManager.FindModule(false, aWeighDeviceName, IWeighingDevice, result);
end;

function TModuleFinder.FindSensor(aTipNumber: integer): ISensorDevice;
var
    xIndex: integer;
    xIntf: ISensorDevice;
    xFound: boolean;
begin
    result := nil;
    xIndex := 0;
    while true do
    begin
        xIntf := nil;
        xFound := gDeviceManager.FindModuleExt(ISensorDevice, xIndex, xIntf);
        if not xFound then
            EXIT;
        if xIntf.TipNumber = aTipNumber then
        begin
            result := xIntf;
            EXIT;
        end;
    end;
end;

function TModuleFinder.FindStateSignal: IStateSignalDevice;
var
    xIndex: integer;
begin
    xIndex := 0;
    gDeviceManager.FindModuleExt(IStateSignalDevice, xIndex, result);
end;

function TModuleFinder.FindArmNames(aFindGripArms, aFindPipArms: boolean): TStringArray;
var
    xArm: IArmDevice;
    x: integer;
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        for x := 0 to gArmManager.Count - 1 do
        begin
            xArm := gArmManager.Arms[x];
            if (not(aFindGripArms and aFindPipArms)) and
                ((aFindGripArms and (not Assigned(xArm.GripDevice))) or
                (aFindPipArms and Assigned(xArm.GripDevice))) then
                CONTINUE;
            xList.Add(xArm.Name)
        end;
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

function TModuleFinder.FindArm(aFindGripArms, aFindPipArms: boolean; aFindName: boolean; aName: string)
    : IArmDevice;
var
    xArm: IArmDevice;
    x: integer;
begin
    result := nil;
    for x := 0 to gArmManager.Count - 1 do
    begin
        xArm := gArmManager.Arms[x];
        if (not(aFindGripArms and aFindPipArms)) and ((aFindGripArms and (not Assigned(xArm.GripDevice))) or
            (aFindPipArms and Assigned(xArm.GripDevice))) then
            CONTINUE;
        if aFindName and (xArm.Name <> aName) then
            CONTINUE;
        result := xArm;
    end;
end;

function TModuleFinder.FindGripArmByName(const aName: string): IArmDevice;
begin
    result := FindArm(true, false, true, aName);
end;

function TModuleFinder.FindFirstGripArm(): IArmDevice;
begin
    result := FindArm(true, false, false, '');
end;

function TModuleFinder.FindGripArmByNameOrFirst(const aName: string): IArmDevice;
begin
    result := FindArm(true, false, aName <> '', aName);
end;

function TModuleFinder.FindPipArmByName(const aName: string): IArmDevice;
begin
    result := FindArm(false, true, true, aName);
end;

function TModuleFinder.FindFirstPipArm(): IArmDevice;
begin
    result := FindArm(false, true, false, '');
end;

function TModuleFinder.FindPipArmByNameOrFirst(const aName: string): IArmDevice;
begin
    result := FindArm(false, true, aName <> '', aName);
end;

function TModuleFinder.FindArmByPipDevice(aPipDevice: IPipDevice): IArmDevice;
var
    x: integer;
begin
    result := nil;
    for x := 0 to gArmManager.Count - 1 do
    begin
        if gArmManager.Arms[x].PipDevice = aPipDevice then
        begin
            result := gArmManager.Arms[x];
            EXIT;
        end;
    end;
end;

function TModuleFinder.FindPeripump: IPeripumpDevice;
begin
    gDeviceManager.FindFirst(IPeripumpDevice, result);
end;

function TModuleFinder.FindFirst(aExpectedModuleID: TModuleID; out oIntf): boolean;
begin
    result := gDeviceManager.FindFirst(aExpectedModuleID, oIntf)
end;

function TModuleFinder.ModuleTypeExists(aExpectedModuleID: TModuleID): boolean;
begin
    result := gDeviceManager.ModuleTypeExists(aExpectedModuleID);
end;


end.
