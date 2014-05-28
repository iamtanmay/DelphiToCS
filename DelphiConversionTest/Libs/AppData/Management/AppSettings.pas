{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Coordinate Licence settings, User settings and IniAccess settings
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  ZACommonMain:
  20.12.02 wl                               TN1293.5 initial version
  20.12.02 wl                               TN1293.5 neue Funktionen für SamStart und Globals
  30.12.02 wl  CreateFreeIniAccess          TN1293.5 neu: erzeugt neues Interface IIniAccess
  04.01.03 wl                               TN1334.1 new: Current user & user methods
  08.01.03 wl  CreateLicence,GetLicenceKey,CheckLicenceKey   TN1334.1 Anpassung an TLicence
  08.01.03 wl  PrepareIniAccess             TN1293.5 von Globals hierher verschoben
  08.01.03 wl  CreateLicence                TN1334.1 bei DemoMode keine Exception
  09.01.03 wl                               TN1334.1 CFR21Complient ist Teil der Lizenz
  09.01.03 wl  DefinePlatform               TN1334.1 Platform wird über den ALIAS festgelegt
  09.01.03 wl  PrepareIniAccess             TN1293.5 Aufruf von gmChangePeriSpeedKV4 usw.
  15.01.03 wl  alle IniAccess-Methoden      TN1295.5 --> IniSettings
  11.02.03 wl  GetAreaFromAnyName           TN1334.3 neu: wird auch außerhalb des IniDataAdaptors benötigt
  12.02.03 wl  CreateLocalIni               TN1293.5 neu: erzeugt File APPDATA.TMP
  12.02.03 wl  LoginUser                    TN1293.5 benutzt ILocalIniFile
  12.02.03 wl  DefinePlatform               TN1293.5 benutzt ILocalIniFile
  14.02.03 wl  CreateZACommonMain           TN1293.5 Application.Icon wird als TMemoryStream übergeben und hier gesetzt
  10.03.03 wl  CreateZACommonMain           TN1293.5 überflüssige Variablen entfernt
  12.03.03 wl                               TN1293.5 Utility-Methoden entfernt
  17.03.03 wl                               TN1332.2 neu: FLogging & 7 Logging-Methoden
  20.03.03 wl  CreateProcessLog             TN1332.4 bekommt Dateinamen mit Pfad übergeben
  21.03.03 wl  ChangeSystemDataPassword     TN1439.4 das Password für alle System Tabellen wird hier geändert
  21.03.03 wl  Create                       TN1439.4 Reihenfolge geändert - Login erfolgt jetzt zuerst!
  25.03.03 wl  Password                     TN1439.4 bei ungültigen Password wird die Application gestoppt!
  25.03.03 wl  ChangeSystemDataPassword     TN1439.4 --> UserManager
  25.03.03 wl  CreateLogging                TN1439.4 neuer Aufruf von Create
  26.03.03 wl  Create                       TN1439.4 Reihenfolge geändert - DefinePlatform vor login
  26.03.03 wl  CheckRunAlias                TN1439.4 aus DBRack hierher, damit RunAlias schon früher erzeugt wird
  26.03.03 wl  CreateBackupManager          TN1439.5 erzeugt BackupManager
  26.03.03 wl  CompleteZipBackup            TN1439.5 ruft BackupManager-Funktion auf: alle DB-Files in Zip sichern
  27.03.03 wl  WriteLog, WriteProcessLog    TN1439.4 Anpassung an TLogging
  27.03.03 wl  Password                     TN1439.4 wenn das Event beim Start nicht erscheint -> SystemDataPW := ''
  08.04.03 wl  CreateZACommonMain           TN1293.5 benutzt Versionsnummer von globals.pas!
  10.04.03 wl  SavePathToZipAndDelete       TN1332.4 jetzt als public
  15.04.03 wl  CreateZACommonMain           TN1293.5 Alias und Purpose können mit übergeben werden
  16.04.03 tbh CreateDLLIni                 TN1468   neu
  16.04.03 wl  StartCompliantMode           TN1439.4 ersetzt ChangeSystemDataPassword
  16.04.03 wl  Password                     TN1439.4 --> UserDataAdapter
  16.04.03 wl  StopStartupLogging           TN1332.4 beendet das Aufzeichnen der ersten Log-Zeilen
  16.04.03 wl  SavePathToZipAndDelete       TN1332.4 Pfad wird hinterher zurückgesetzt
  30.05.03 wl  AddMemoLine                  TN1485.1 ruft TLogging-Methode auf
  02.06.03 wl  CreateZACommonMain           TN1485.1 ZinsserPlatform kann nicht mehr festgelegt werden
  03.06.03 wl  DefinePlatform               TN1485.1 --> IniSettings
  14.07.03 wl  CreateZACommonMain           TN1501   AppMode (Simulation wird als Parameter übergeben)
  01.10.03 wl  CompleteZipBackup            TN1610   FileName kann als Parameter übergeben werden
  08.03.04 pk  CreateLicence                TN1792   calls UserEntersKeyIfNotValid instead of Checkkey
  28.04.04 wl  CreateZP02CfgIni             TN1788.9 kann primitive Daten aus SiasCfg.ini lesen
  10.05.04 wl  CreateZP02CfgIni             TN1788.9 Aufruf ohne Paramter
  08.06.04 pk  Create                       TN1974.0 Saves AliasPath to member variable fAliasPath
  08.06.04 pk  CheckRunAlias                TN1974.0 Saves RunAliasPath to member variable fRunAliasPath
  29.07.04 wl  CreateZACommonMain           TN2007.1 Laden von Icon in try-except-Block (Aufstarten wird trotzdem fortgesetzt)
  05.08.04 wl  CreateZACommonMain           TN2007.1 Editor: Kein Laden des Icons
  11.11.04 wl  CreateZACommonMain           TN2213   Laden des Icons nur bei Applikationen die mit Delphi 5 compiliert wurden
  08.09.05 wl                               TN2558.9 uses BackupManager entfernt
  10.10.05 wl                               TN2007   uses FileCtrl entfernt
  21.09.06 wl  CompleteZipBackup            TN3325   result ist jetzt Name der Backup-Datei (vollständiger Pfad)
  20.12.06 wl  CreateZACommonMain           TN2198.3 wenn Anmeldung fehl schlägt, wird mit ZACommon.Free der Speicher frei gemacht
  29.07.07 wl  CreateZACommonMain           TN3811   vereinfacht wg. Package statt Dll
  31.07.07 wl  CreateZACommonMain           TN3811   Version wird aus VersionAppCommon.inc gelesen
  01.08.07 wl                               TN3811.2 TZACommonMain wird nicht mehr von IZACommon abgeleitet
  01.08.07 wl  CreateLogging                TN3811.2 erzeugt gLogManager-Instanz
  01.08.07 wl                               TN3811.2 alle Logging-Funktionen entfernt
  31.08.07 wl  FLicence                     TN3811.4  --> AppInstanceLicence
  31.08.07 wl  User-Funktionen              TN3811.4  --> AppInstanceUserCommon
  31.08.07 wl  fSettings-Funktionen         TN3811.4  --> AppInstanceIniAdaptor
  03.09.07 wl                               TN3811.4  Alle Methoden zu class methods
  AppSettings:
  10.10.02 wl                               TN1293.2 initial version
  18.10.02 wl                               TN1293.1 erweitert um globals-Funktionen
  18.10.02 mo  GetAppName                   TN1301 -SAMI => -CP
  24.10.02 wl  CfgIniName,...               TN1293.1 --> TIniDataAdapter
  24.10.02 wl  DataPath,ZinsserPlatform     TN1293.1 neue Get-Methoden
  06.11.02 wl  TIniAccessType               TN1293.1 aus CommonTypes hierher verschoben
  14.11.02 wl  UseScheduler                 TN1328.1 neu
  20.11.02 mo  UseSymyx                     TN1240 neu
  20.12.02 wl                               TN1293.5 neu: Zugriff auf TResLoader
  20.12.02 wl  TZinsserPlatform, TIniAccessType  TN1293.5 --> CommonTypes
  20.12.02 wl  InitDataServer               TN1293.5 setzt ZinsserPlatform & Purpose, initialisiert gCommonDll-Objekt mit ZACommon.dll
  20.12.02 wl                               TN1293.5 FIniAccessType wird in ZACommon.dll gespeichert
  20.12.02 wl                               TN1293.5 uses und WinlissyIniAccess geändert
  04.01.03 wl                               TN1293.3 Übernahme wesentlicher Funktionen von ZACommon.dll
  09.01.03 wl                               TN1293.3 weitere Anpassungen an ZACommon.dll
  13.01.03 wl  IniAccess, SetIniAccess      TN1293.5 entfernt
  18.01.03 mo                               TN1348  neu IsMoss
  27.02.03 wl  IsCFR21Compliant             TN1332.2 neue Abfrage
  28.02.03 wl  MethodActionIsUnsafe         TN1332.2 neue Abfrage für Actions die in CFR21Compliant-Umgebungen gesperrt sind
  14.03.03 mo  TAppSettings.DataPath	   TN1442   result := DataPath anstatt Alias
  17.03.03 wl  InitDataServer               TN1332.2 nur beim Starten von Sampler oder Layouter wird Error.dat neu gestartet (nicht bei Editor!!)
  21.03.03 wl  IsCFR21Compliant             TN1439.4 neu definiert (über User.MustGiveReason)
  08.04.03 wl  gmInitApplication            TN1293.5 aus globals hierher verschoben
  15.04.03 wl  TAppPurpose                  TN1293.5 --> CommonTypes
  15.04.03 wl  gmInitOtherApp               TN1293.5 kann von SettingsEditor & UserMgr benutzt werden
  30.04.03 wl  InitDataServer               TN1480   wenn gCommonDll nicht geladen werden konnte: result = false
  21.05.03 tbh ScriptsAreRackBased          TN1487   neu
  21.05.03 tbh UseScheduler                 TN1487   kein Scheduler wenn Skripts rackbasierend
  21.05.03 tbh SetUsedFunctions             TN1487   kein Scheduler wenn Skripts rackbasierend
  02.06.03 wl  InitDataServer               TN1485.4 ZinsserPlatform kann nicht mehr festgelegt werden
  14.07.03 wl  InitDataServer               TN1535.1 AppMode (Simulation) wird aus den Start-Parametern gelesen
  19.02.04 pk  gmShutDownApplication        TN1753   New.  Responsible for undoing everything that was done in init
  08.03.04 pk  gmInitApplication            TN1791   Determines DebugMode from command line parameters
  08.06.04 pk                               TN1974.0 New: AliasPath and RunAliasPath return paths stored in Commondll
  08.06.04 pk  MethodActionIsUnsafe         TN1976   --> MethodGUIParsing
  11.08.04 wl  SchedSafe                    TN2008.1 neu: von ScheEdit hierher verschoben
  22.09.04 tbh InitDataServer               TN2146   Applikationsname wird nur angepasst wenn es die 'Sampler.exe' ist
  22.09.04 tbh SetOemSettings               TN2146   Applikationsname wird nur angepasst wenn es die 'Sampler.exe' ist
  11.11.04 wl  IsDefaultApplikation         TN2213   von CommonDll hierher, erkennt jetzt auch 'ZADESIGN'
  07.12.04 wl  IsDefaultApplication         TN2246.4 mit 'ZARUNNER'
  04.03.05 pk  GetReason                    TN2330.1 from TDataAdaptor
  21.03.05 wl  UseScript                    TN2357   Sophas-Lizenz ist nicht mehr zwingend mit UseScript verbunden
  07.04.05 pk  GetAppName                   TN2375   Scheduler-Licence : add -SCHED to AppName
  07.04.05 pk  UseScheduler                 TN2375   Scheduler-Licence : UseScheduler = 0 if licScheduler flag not available
  30.06.05 wl  GetAppName                   TN2440   ZADesign heißt immer: 'Zinsser Method Designer'
  08.11.05 wl  UseScript,ScriptsAreRackBased TN2745  entfernt
  19.07.06 wl  STR_CWSTATISTICS_NAME        TN3201   AppSettings könnte jetzt auch von CWStat benutzt werden
  24.11.06 wl  GetAppName                   TN3420   Der Applikationsname wurde bereinigt: REDI,SCHED oder 2002 tauchen nicht mehr auf
  24.11.06 wl  GetSpecialFeatures           TN3420   Featurs wie REDI, Sophas, CFR21 werden hier angezeigt
  24.11.06 wl  gmInitApplication            TN3432   Neuer Startparameter NOSTART bewirkt, dass Designer keine Verbindung mit ZARunner aufnimmt
  24.11.06 wl  GetZARunnerConnection        TN3432   Speichert, ob Designer Verbindung mit ZARunner aufnimmt
  24.11.06 wl  gmInitApplication            TN3433   Startparameter A:Aliasname, um Daten einer anderen Datenbank zu öffnen
  24.11.06 wl  GetExeName                   TN3433   Mit geändertem Exe-Namen ändert man nicht mehr den Alias
  07.12.06 wl  CFR21ComplianceMode          TN3409   untescheidet Prepare- und Compliant-Mode
  19.12.06 wl  gmInitApplication            TN3409   Anmelden als Gast = NOSTART mode
  20.12.06 wl  gmInitApplication,gmInitOtherApp  TN2198.3 Rückgabewert false, wenn Anmeldung abgebrochen wurde
  21.02.07 wl  gmInitApplication            TN3587   Ein spezieller Alias kann auch ohne Simulation starten
  08.03.07 wl  IsSias                       TN3620   sollte verschwinden -> deprecated;
  27.04.07 wl  ItemConfirmDelete,Add,Edit   TN3669   Ersetzen GetReason-Funktion und sollen für mehr Einheitlichkeit sorgen
  29.07.07 wl  InitDataServer               TN3811   statt gmInitCommonDll wird CreateZACommonMain direkt aufgerufen
  31.07.07 wl  InitDataServer               TN3811   Version wird aus VersionAppCommon.inc gelesen
  01.08.07 wl                               TN3811.2 bentzt TZACommonMain statt IZACommon
  31.08.07 wl                               TN3811.4 Licence-Funktionen benutzen direkt AppInstanceLicencing
  31.08.07 wl                               TN3811.4 benutzt DataAdaptor
  03.09.07 wl                               TN3811.4 mit ZACommonMain vereinigt!
  04.09.07 wl  gmInit/ShutdownApplication   TN3811.4 --> SamStart
  14.04.08 wl                               TN4060   uses AppInstanceDataAdaptorCommon
  11.06.08 wl                               TN4143   ZinsserPlatform entfernt
  06.08.08 pk  CreateMethodIni              TN4165.1 removed. Replaced by MethodSettingsDataAdaptor
  16.02.09 wl                               TN4370   entfernt: GetLanguage
  13.07.09 pk  RunPath                      TN4585.4 RunAliasPath replaced by RunPath
  13.07.09 pk  RunAlias                     TN4585.4 Removed
  07.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  07.08.09 wl  Load/SaveFormPosition        TN4702   von postools hierher verschoben
  26.10.09 wl  Load/SaveFormPosition        TN4831   entfernt
  20.04.10 ts  IsTraySy                     TN5067   new
  07.05.10 wl  UseDisplayComponents,..      TN5052   Display-Settings mit Traysy-Lizenz kombiniert
  17.05.10 wl                               TN5111   LocalIniPath,LogfilePath: Bei Win7 ProgramData statt Program Files
  20.05.10 wl                               TN5116   AppInstanceDataAdaptorCommon geändert
  18.06.10 wl  IsMoss,IsSami                TN5116   entfernt
  21.07.10 pk                               TN5203   AppInstanceAppDataAdaptorCommon renamed to AppInstanceAppData
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  29.09.10 wl                               TN5112   uses LicenseKeyUtils
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  29.11.10 pk                               TN5368   TAppSettings.RunPath uses GetApplicationDataPath
  -------------------------------------------------------------------------------------------------- }

unit AppSettings;


interface


uses
    SysUtils,
    Forms,
    CommonTypes;

type
    EInvalidAppSettings = Exception;

    TAppSettings = class
    private
        class function GetDataPath: string; static;
        class function GetLocalIniFileName: string; static;
    public
        // IniAccess
        class function CreateSimpleIni(aAreaName: string): ISimpleIniAccess; static;
        class function CreateAppIni: IWinlissyIniAccess; static;
        class function CreateRobotIni: IWinlissyIniAccess; static;
        class function CreateDLLIni: IWinlissyIniAccess; static;

        // Local Ini File
        class property LocalIniFileName: string read GetLocalIniFileName;

        // User Management
        class function LevelIsIncluded(aIsLevel, aMinimumLevel: TUserLevel): boolean; static;
        class function ChangeCurrentUser: IUser; static;
        class function GetLevelName(aUserLevel: TUserLevel): string; static;
        class function StartCompliantMode: boolean; static;

        // Backup
        class function CompleteZipBackup(aFileName: string): string; static;
        class function SavePathToZipAndDelete(const aPath, aPassword: string; var aZipFileName: string)
            : boolean; static;

        // Data settings
        class property DataPath: string read GetDataPath;
        class function CurrentUser: IUser; static;
        class function AppPurpose: TAppPurpose; static;
        class function Alias: string; static;

        // Writing class methods
        class function RunPath: string; static;

        class function Version: string; static;
        class function Build: string; static;
        class function SerialNo: string; static;
        class function OEM: TOEMSettings; static;

        // Display-Settings & Lizenz-Einstellungen
        class function UseSymyx: boolean; static;
        class function IsSophas: boolean; static;
        class function IsRedi: boolean; static;
        class function IsTraySy: boolean; static;
        class function CFR21ComplianceMode: TCFR21ComplianceMode; static;
        class function IsDebugMode(): boolean; static;
        class function GetSpecialFeatures(): string; static;
        class function UseScheduler: boolean; static;
        class function UseCarriers: boolean; static;
        class function IsOneWorkspaceMode: boolean; static;
        class function UseDisplayComponents: boolean; static;
        class function IsOneTipTypeMode: boolean; static;

        class function GetZARunnerConnection(): boolean; static;

        // Item-Funktionen
        class function ItemConfirmDelete(const aItemTypeName, aItemName: string): boolean; static;
        class function ItemConfirmEdit(const aItemTypeName, aItemName: string): boolean; static;
        class function ItemConfirmAdd(const aItemTypeName, aItemName, aCopiedFrom: string): boolean; static;
    end;

    // Super-Trick:
    gCommonDll = TAppSettings;


implementation


uses
    Windows,
    globals,
    Licence,
    DataAdaptor,
    BackupManager,
    DialogUtils,
    GeneralTypes,
    FileUtilities,
    AppInstanceIniAdaptor,
    AppInstanceLicencing,
    AppInstanceUserCommon,
    AppInstanceAppData,
    User,
    LicenseKeyUtils,
    AppInstanceStartupLib,
    DataProviderFactory;

{ TAppSettings }

class function TAppSettings.GetDataPath: string;
begin
    result := TAppInstanceIniAdaptor.Instance.DataPath;
end;

class function TAppSettings.CreateSimpleIni(aAreaName: string): ISimpleIniAccess;
begin
    result := TAppInstanceIniAdaptor.Instance.Settings.CreateSimpleIni(aAreaName);
end;

class function TAppSettings.CreateAppIni: IWinlissyIniAccess;
begin
    result := TAppInstanceIniAdaptor.Instance.Settings.CreateAppIni;
end;

class function TAppSettings.CreateRobotIni: IWinlissyIniAccess;
begin
    result := TAppInstanceIniAdaptor.Instance.Settings.CreateRobotIni;
end;

class function TAppSettings.CreateDLLIni: IWinlissyIniAccess;
begin
    result := TAppInstanceIniAdaptor.Instance.Settings.CreateDLLIni;
end;

class function TAppSettings.ChangeCurrentUser: IUser;
begin
    result := TAppInstanceUserCommon.Instance.ChangeCurrentUser();
end;

class function TAppSettings.CurrentUser: IUser;
begin
    result := TAppInstanceUserCommon.Instance.CurrentUser;
end;

class function TAppSettings.LevelIsIncluded(aIsLevel, aMinimumLevel: TUserLevel): boolean;
begin
    result := TUser.LevelIsIncluded(aIsLevel, aMinimumLevel);
end;

class function TAppSettings.GetLevelName(aUserLevel: TUserLevel): string;
begin
    result := TUser.GetLevelName(aUserLevel);
end;

class function TAppSettings.GetLocalIniFileName: string;
begin
    result := TAppInstanceIniAdaptor.Instance.LocalIniFileName;
end;

class function TAppSettings.CompleteZipBackup(aFileName: string): string;
begin
    result := TAppInstanceUserCommon.Instance.CompleteZipBackup(aFileName);
end;

class function TAppSettings.SavePathToZipAndDelete(const aPath, aPassword: string;
    var aZipFileName: string): boolean;
begin
    result := TBackupManager.SavePathToZipAndDelete(aPath, aPassword, aZipFileName);
    ChDir(TFileUtilities.GetApplicationDataPath()); // current path = application path
end;

class function TAppSettings.AppPurpose: TAppPurpose;
begin
    result := TAppInstanceAppData.Instance.Purpose;
end;

class function TAppSettings.StartCompliantMode: boolean;
begin
    result := TAppInstanceUserCommon.Instance.StartCompliantMode();
end;

class function TAppSettings.Alias: string;
begin
    result := TDataProviderFactory.Instance.MainDBAlias;
end;

class function TAppSettings.GetSpecialFeatures(): string;
begin
    result := '';

    if TAppSettings.IsRedi() then
        result := result + 'REDI, ';
    if TAppSettings.IsSophas() then
        result := result + 'Sophas, ';
    if (TAppSettings.CFR21ComplianceMode = ccmPrepared) then
        result := result + 'CFR21(Prepare), '
    else if (TAppSettings.CFR21ComplianceMode = ccmCompliant) then
        result := result + 'CFR21(Full), ';
    if TAppSettings.UseScheduler() then
        result := result + 'Scheduler, ';
    if TAppSettings.UseSymyx() then
        result := result + 'SymyxIntf, ';

    result := Copy(result, 1, Length(result) - 2);
end;

class function TAppSettings.UseCarriers: boolean;
// var
// xIniAccess: IWinlissyIniAccess;
begin
    if IsTraysy then
    begin
        result := false;
        EXIT;
    end;

    // man könnte ein allgemeines Setting dafür erfinden:
    // xIniAccess := gCommonDll.CreateAppIni;
    // xIniAccess.ReadBool( 'Display', 'UseCarriers' );

    result := true;
end;

class function TAppSettings.UseDisplayComponents: boolean;
var
    xIniAccess: IWinlissyIniAccess;
begin
    if IsTraysy then
    begin
        result := false;
        EXIT;
    end;
    xIniAccess := gCommonDll.CreateAppIni;
    result := xIniAccess.ReadBool('Display', 'UseDisplayComponents');
end;

class function TAppSettings.RunPath: string;
begin
    result := TFileUtilities.IncludeTrailingPathDelimiter
        (TFileUtilities.IncludeTrailingPathDelimiter(TFileUtilities.GetApplicationDataPath()) + 'Run');

end;

class function TAppSettings.Version: string;
begin
    result := STR_APP_VERSION;
end;

class function TAppSettings.Build: string;
begin
    result := STR_APP_BUILD;
end;

class function TAppSettings.SerialNo: string;
begin
    result := TAppInstanceLicencing.Instance().Licence.Key;
end;

class function TAppSettings.UseScheduler: boolean;
var
    xIniAccess: IWinlissyIniAccess;
begin
    if not TAppInstanceLicencing.Instance().Licence.LicencePartValid(licScheduler) then
    begin
        result := false;
        EXIT;
    end;

    xIniAccess := TAppSettings.CreateAppIni;
    result := (xIniAccess.ReadInteger('Display', 'UseScheduler') <> 0);
end;

class function TAppSettings.UseSymyx: boolean;
begin
    result := TAppInstanceLicencing.Instance().Licence.LicencePartValid(licSymyx);
end;

class function TAppSettings.IsSophas: boolean;
begin
    result := TAppInstanceLicencing.Instance().Licence.LicencePartValid(licSophas);
end;

class function TAppSettings.IsRedi: boolean;
begin
    result := TAppInstanceLicencing.Instance().Licence.LicencePartValid(licRedi);
end;

class function TAppSettings.IsOneTipTypeMode: boolean;
var
    xIniAccess: IWinlissyIniAccess;
begin
    if IsTraysy then
    begin
        result := true;
        EXIT;
    end;
    xIniAccess := gCommonDll.CreateAppIni;
    result := xIniAccess.ReadBool('Display', 'OneTipTypeMode');
end;

class function TAppSettings.IsOneWorkspaceMode: boolean;
var
    xIniAccess: IWinlissyIniAccess;
begin
    if IsTraysy then
    begin
        result := true;
        EXIT;
    end;
    xIniAccess := gCommonDll.CreateAppIni;
    result := xIniAccess.ReadBool('Display', 'OneWorkspaceMode');
end;

class function TAppSettings.IsTraySy: boolean;
begin
    result := TAppInstanceLicencing.Instance().Licence.LicencePartValid(licTraySy);
end;

class function TAppSettings.CFR21ComplianceMode: TCFR21ComplianceMode;
begin
    result := CurrentUser.CFR21Mode;
end;

class function TAppSettings.IsDebugMode(): boolean;
begin
    result := TAppInstanceUserCommon.Instance.IsDebugMode;
end;

class function TAppSettings.GetZARunnerConnection(): boolean;
begin
    result := not TAppInstanceUserCommon.Instance.IsNoStart;
end;

class function TAppSettings.ItemConfirmDelete(const aItemTypeName, aItemName: string): boolean;
begin
    result := false;

    if (TDialogUtils.MessageBox(TLanguageString.Read('Do you really want to delete {0} {1}?',
        'Wollen Sie wirklich {0} {1} löschen?', [aItemTypeName, aItemName]),
        TLanguageString.Read('Delete {0}', '{0} löschen', [aItemTypeName]),
        MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2) <> IDYES) then
        EXIT;

    if not CurrentUser.ConfirmDataChanged(aItemTypeName, aItemTypeName + ' [' + aItemName + ']: Delete!',
        lctRemove) then
        EXIT;

    result := true;
end;

class function TAppSettings.ItemConfirmEdit(const aItemTypeName, aItemName: string): boolean;
begin
    result := false;

    if not CurrentUser.ConfirmDataChanged(aItemTypeName, aItemTypeName + ' [' + aItemName + ']: Data edited',
        lctEdit) then
        EXIT;

    result := true;
end;

class function TAppSettings.ItemConfirmAdd(const aItemTypeName, aItemName, aCopiedFrom: string): boolean;
var
    xText: string;
begin
    result := false;

    xText := aItemTypeName + ' [' + aItemName + ']: Added';
    if (aCopiedFrom <> '') then
        xText := xText + ' (copied from ' + aCopiedFrom + ')';

    if not CurrentUser.ConfirmDataChanged(aItemTypeName, xText, lctAdd) then
        EXIT;

    result := true;
end;

class function TAppSettings.OEM: TOEMSettings;
begin
    result := TAppInstanceUserCommon.Instance.OEMSettings;
end;


end.
