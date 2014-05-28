{ --------------------------------------------------------------------------------------------------
  Copyright © 2002 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Common types (for the ZACommon.DLL and Zinsser Applications)
  -------------------------------------------------------------
  !! If types are changed the version number must be changed !!
  -------------------------------------------------------------
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  version  track-no improvement/change
  -------- --  -------  -------- -------------------------------------------------------------------
  18.12.02 wl   1.0.0   TN1293.5 initial version (CommonTypes --> AppTypes)
  20.12.02 wl   1.0.0   TN1293.5 zusätzliche Methode in TCommon
  20.12.02 wl   1.0.0   TN1293.5 alle const values aus TWinlissyIniAccess
  27.12.02 wl   1.0.0   TN1293.5 neu: ISimpleIniAccess.SectionExists
  30.12.02 wl   1.0.0   TN1293.5 neu: IIniAccess (für SettingsEditor & globals)
  04.01.03 wl   1.0.0   TN1334.1 neu: IUser, TLicenceParts, CommonDll-Properties
  08.01.03 wl   1.0.0   TN1334.1 Database Constants --> IniDataAdapter
  08.01.03 wl   1.0.0   TN1334.1 neu: IUser.ShowAllUsers, DeleteUser, AddNewUser, GetLevelName
  09.01.03 wl   1.0.0   TN1334.1 licCFR21Complient ist Teil der Lizenz, ZACommonMain entsprechend geändert
  09.01.03 wl   1.0.0   TN1293.5 Platform appSIAS entfernt
  14.01.03 wl   1.0.0   TN1293.5 Namen geändert TIniTypeImpl -> TIniType, TIniType -> IIniType
  15.01.03 wl   1.0.0   TN1295.5 TIniAccessSection entfernt, statt dessen Const-Werte STR_SETTINGS_AREA...
  15.01.03 wl   1.0.0   TN1295.5 IIniSection entfernt, IIniAccess vereinfacht
  16.01.03 wl   1.0.0   TN1295.5 IIniAccess neu: EditDialog, GetFileName
  17.01.03 wl   1.0.0   TN1293.3 neue Funktion AddForeignIniEntry für Settings Editor
  21.01.03 wl   1.0.0   TN1332.4 neu: IUser.LogArchiving
  23.01.03 wl   1.0.0   TN1293.6 const 'DEVICE' statt 'DEVICES'
  05.02.03 wl   1.0.0   TN1293.3 IIniAccess stark überarbeitet
  06.02.03 wl   1.0.0   TN1334.3 IIniAccess-Methoden mit Übergabe von IUser
  11.02.03 wl   1.0.0   TN1334.3 neu: TZACommon.GetAreaFromAnyName
  11.02.03 wl   1.0.0   TN1334.3 neu: User level: usrUnlimitedAdmin
  12.02.03 wl   1.0.0   TN1293.5 neu: ILocalIniFile (ZaCommon.Dll)
  12.02.03 wl   1.0.0   TN1293.5 IZACommonDll.TmpIniFile abgeschafft
  14.02.03 wl   1.0.0   TN1293.5 CreateZACommonMain: AppIcon wird als Stream übergeben
  20.02.03 mo   1.0.0   TN1348   Neuer TLicencePart = licMoss
  20.02.03 wl   1.0.0   TN1334.3  neu: TLogRunType = (.. lrtAbort, lrtWorkedUp);
  20.02.03 wl   1.0.0   TN1334.3  IUser.LogRun erweitert
  07.03.03 tbh  1.0.0   TN1443   TTubeToolData um Schüttelparameter erweitert
  11.03.03 wl   1.0.0   TN1293.5  neu: TLiquidPortData (für ini access: SystemLiquidValve, Port..)
  11.03.03 wl   1.0.0   TN1293.5  neu: IWinlissyIniAccess.ReadLiquidPortData
  12.03.03 wl   1.0.1   TN1293.5  IZACommon: Utility-Methoden entfernt
  17.03.03 wl   1.0.1   TN1332.2  IZACommon: um 7 Logging-Methoden erweitert
  20.03.03 wl   1.0.1   TN1332.4 IZACommon.CreateProcessLog: bekommt Dateinamen mit Pfad übergeben
  21.03.03 wl   1.0.1   TN1439.4 neu: IZACommon.ChangeSystemDataPassword, IUser.CFR21Mode
  25.03.03 wl   1.0.1   TN1439.4 neu: IUser.SystemDataPassword
  26.03.03 wl   1.0.1   TN1439.5 neu: IZACommon.CompleteZipBackup
  31.03.03 mo   1.0.1   TN1383.8 STR_ISEC_USERPROTECT
  01.04.03 tbh  1.0.1   TN1383.6 STR_ISEC_STATEMONITOR
  03.04.03 wl   1.0.1   TN1439.4 neu: IUser.LogCompleteBackup
  08.04.03 wl           TN1293.5 benutzt Versionsnummer von globals.pas!
  10.04.03 wl           TN1332.4 neu: IZACommon.SavePathToZipAndDelete
  14.04.03 wl           TN1332.6 neu: IUser.GetArchiveFileInfo (für Retrieve-Applikation)
  15.04.03 wl           TN1293.5 gmInitCommonDLL: Alias und Purpose können mit übergeben werden
  16.04.03 tbh          TN1468   neu: STR_SETTINGS_AREA_DLL
  16.04.03 tbh          TN1468   neu: CreateDLLIni
  16.04.03 wl           TN1439.4 neu: IZACommon.StartCompliantMode statt IUser.SetSystemPassword
  16.04.03 wl           TN1332.4 neu: IZACommon.StopStartupLogging
  28.04.03 wl   6.0.3   TN1473.1 neu: IUser.ChangeUserLevel, ChangeUserDescription, ResetUserPassword
  09.05.03 wl           TN1490   neu: TTipType.DitiType
  30.05.03 wl           TN1485.1 neu: IZACommon.AddMemoLine
  02.06.03 wl           TN1485.4 gmInitCommonDll: ZinsserPlatform kann nicht mehr festgelegt werden
  14.07.03 wl   6.0.4   TN1535.1 gmInitCommonDll: AppMode (Simulation wird als Parameter übergeben)
  17.09.03 wl   6.0.5   TN1526   MAX_TIPS = 9  !!!!!!
  01.10.03 wl           TN1610   IZACommon.CompleteZipBackup - FileName kann als Parameter übergeben werden
  12.11.03 wl           TN1660   BCRPOSITION - neu: RPos als Rotation-Wert
  02.12.03 wl           TN1598.1 Sektion 'VORTEXER' wird nicht mehr gebraucht
  10.12.03 wl   6.1.0   TN1672   APOS: Y wird als MPOSArray gespeichert - Varispan fällt weg
  10.12.03 wl           TN1672   gmGetYArrayFromYWithVarispan von postools.pas hierher verschoben
  17.12.03 wl           TN1672   TXYRangeData: Daten für die Ausmaße des Pipettier- und Greiferarms
  17.12.03 wl           TN1672   neu: TPosMM als Motor Position oder Längenangabe in mm
  04.03.04 pk           TN1789   STR_ISEC_ACTIONDEPENDENT
  08.03.04 pk           TN1646   New exception type: EApplicationAbort
  08.03.04 pk           TN1792   gmInitCommonDLL: raises EApplicationAbort exception if CreateZACommanMain fails
  19.03.04 wl   6.1.1   TN1788   TTipType.AName entfernt
  21.04.04 wl           TN1869   INT_CFR21_USERLOG-Konstanten zum entschlüsseln der LogTypes
  21.04.04 wl           TN1869   neu: IUser.GetUserLog()
  28.04.04 wl           TN1788.9 neu: IZACommon.CreateZP02CfgIni: kann primitive Daten aus SiasCfg.ini lesen
  10.05.04 wl           TN1788.9 Aufruf ohne Paramter
  25.05.04 pk           TN1909.1 STR_ISEC_METHODPARAMS
  08.06.04 pk           TN1974.0 New: IZACommon.GetAliasPath and .GetRunAliasPath
  26.07.04 pk           TN2052   BCRPOSITION : new field - PosName
  29.07.04 wl   6.1.2   TN2007.1 ILocalIniFile: Aufrufe ohne TControl, da nicht Versionskompatibel
  05.08.04 wl           TN2074   APOS --> AppTypes
  18.08.04 wl           TN2099   INCUBATORREC entfernt
  22.09.04 tbh          TN2146   new: gmIsDefaultApplikation checks if application needs default (SAMINTF) alias
  22.09.04 tbh          TN2146   gmInitCommonDLL - default alias is used only if a default application starts up
  11.11.04 wl   6.1.5   TN2213   gmIsDelphi5Compiled - Info an die ZACommonDll, wie dei Exe compiliert wurde
  23.11.04 wl           TN2239.1 MOTOR: alles integer, neu: InitSpeed
  23.11.04 wl           TN2239.2 DILUTOR: alles integer, neu: ScalingFactor
  23.12.04 pk        ?? TN2281.1  Method Settings
  28.01.05 wl           TN2297.4  TTipType mit neuen Parametern: WashZOffset_mm, WasteZOffset_mm, DryZOffset_mm, DryAtInit
  31.01.05 wl           TN2297.4  DryAtInit muß wegen systematischen Fehlers beim Einlesen in DoNotDryAtInit umbenannt werden
  22.02.05 wl           TN2323    DoNotDryAtInit wird umbenannt in DoNotDryAfterFlush
  07.04.05 pk           TN2375    TLicencePart : new licScheduler for Scheduler licence
  25.05.05 wl   6.2.0   TN2427    neu: ILocalIniFile.Read/WriteInteger,Float,Boolean
  02.06.05 wl   6.2.1   TN2436    neu: INT_ZP01CFG_MOTORS, ...
  17.06.05 wl   6.3.0   TN2440    STR_ISEC_METHOD_EDITGRID = 'EditorGrid'
  16.08.05 wl   6.3.1   TN2558.4  neu: IMethodIniAccess.DeleteAllAndSave
  16.08.05 wl           TN2558.4  neu: IIniAccess.DeleteAllValues - ohne Parameter
  23.09.05 wl   7.0.0   TN2624    neu: STR_DEV_INI_DEFAULT_BIT_UNDEFINED,..
  30.09.05 wl           TN2550    TTipType: DilutorName statt DilutorAdr
  30.11.05 wl  7.0.2    TN2818    entfernt: TPRTipData, RemRediTip
  06.03.06 wl   7.0.4   TN2541.4  TDBFieldDefinition: für individuelle Felddefinitionen
  25.03.06 pk  7.1.0    TN3000    TTipType: DoNotInit - do not initialize tips which are of this tiptype
  31.03.06 pk  7.1.0    TN3009    ToolData fields changed to TPosMM
  10.04.06 pk  7.1.0    TN3031    New method settings section : STR_ISEC_METHOD_INITOPTIONS
  19.05.06 wl           TN3109    neu: STR_ISEC_METHOD_ATTRIBUTES
  08.09.06 pk  7.1.1    TN3293   INT_DEV_INI_DEFAULT_ADR_UNDEFINED changed from -1 to -Low(int)
  21.09.06 wl  7.1.2    TN3325    TCommonDll.CompleteZipBackup: result ist string
  21.09.06 wl           TN3326    TTipType für TTipTypeDataAdaptor geändert
  21.09.06 wl           TN3326    WinlissyIniAccess: ReadTipType entfernt
  20.12.06 wl  7.1.3    TN2198.3  gmInitCommonDLL: Exception EApplicationAbort wird hier nicht mehr erzeugt
  16.01.07 pk           TN3481    new : STR_ISEC_METHOD_AUTOSEQOPTIONS
  05.02.07 pk  7.2.0    TN3544    TTableUpdaterInterface required because it is used in DataAdaptor
  06.02.07 wl           TN3620    neu: STR_SETTINGS_AREA_DRIVER     = 'DRIVER';      // für Interfaces
  27.04.07 wl  7.1.5    TN3669    neu: IUser.GetReason & ConfirmDataChanged
  03.05.07 wl           TN3669    neu: IUser.CheckSQLBeforeUpdate, INT_CFR21_USERLOG_LOGTYPE_ALERT
  29.07.07 wl  7.2.0    TN3811    gmInitCommonDll entfernt
  01.08.07 wl           TN3811.2  IZACommon entfernt
  27.08.07 wl           TN3811.4  TLicencePart --> TLicence
  27.08.07 wl           TN3811.4  TBasicTipType jetzt mit PowerOf2-Werten
  31.08.07 wl           TN3811.4  IIniEntry und IIniAccess entfernt
  31.08.07 wl           TN3811.4  STR_SETTINGS_TABLE_SETTINGS --> SettingsDataAdaptor
  03.09.07 wl           TN3811.4  TOEMSettings heirher verschoben
  18.09.07 pk           TN3846    TBasicTipTypes
  18.09.07 wl           TN3811.4  TUserLog mit UserPK
  09.11.07 pk           TN3921    TTableUpdaterInterface
  09.11.07 pk           TN3924    TTubeToolData
  09.01.08 wl  7.3.0    TN3972    RunCreateSeqAuto  entfernt
  09.01.08 wl           TN3972    Master/Slave-Krempel  entfernt
  13.02.08 wl           TN4009    überflüssige Records MOTOR, DILUTOR, BSWITCH, ... entfernt
  11.06.08 wl           TN4143    TZinsserPlatform entfernt
  06.08.08 pk           TN4165.1  ICheckingIniAccess, ISimpleIniAccess - ReadIntoCache, WriteFromCache
  26.11.08 wl  7.3.1    TN4329    'UserProtection' entfernt
  13.03.09 ts           TN4464    new: STR_ISEC_METHOD_STARTABLEOPTIONS = 'StartableOptions'
  06.04.09 pk           TN4503    new STR_ISEQ_METHOD_DISPLAYOPTIONS
  10.06.09 ts           TN4596    MAX_TIPS --> 10
  16.06.09 wl           TN4605    MethodSettings-Konstanten--> MethodSettingsDataAdaptor
  12.09.09 wl  MPOSArray,TPosMMArray        TN4740   entfernt: werden durch TDoubleArray,TIntArray ersetzt
  12.09.09 wl  MPOS                         TN4740   durch integer ersetzt
  26.10.09 wl                               TN4831    ILocalIniFile entfernt
  04.11.09 pk                               TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.12.09 pk           					   TN4908 BCRPOSITION: New X-,Y-,Z-,R-Defined fields
  20.05.10 wl  TAppPurpose, TMachineConnectionType TN5116   neue Bezeichner
  26.01.11 wl  IWinLissyIniAccess.ReadXYRangeData  TN5448   entfernt
  26.07.11 ts  TAppMode                            TN5606   new: appModeUserAbort, if user has no licence and closes the licence window
  05.11.12 wl  ICheckingIniAccess                  TN6006   Funktion WriteIntoCache entfernt: In APPLICATION darf nur eine Section geschrieben werden, mehr nicht
  27.01.13 wl  TBackupPathData                     TN6069   neu
  20.02.13 wl  STR_ISEC_METHODPARAMS               TN6055   entfernt
  27.03.13 wl                                      TN6045   uses geändert
  06.06.13 wl  TTubeToolData                       TN6154   Neu: GetTube-Parameter
  -------------------------------------------------------------------------------------------------- }

unit CommonTypes;


interface


uses
    Windows,
    ComCtrls,
    StdCtrls,
    SysUtils,
    GeneralTypes,
    Generics.Collections,
    ConfigurationFile;

const
    // muß auf den Prüfstand:
    MAX_TIPS = 10;

    // Ini Section names:
    STR_ISEC_BARCODE = 'BARCODE';
    STR_ISEC_SYSLIQVALVE = 'SystemLiquidValve';
    STR_ISEC_LASTRBLOCK = 'LastReactionBlock';
    STR_ISEC_TUBETOOLDATA = 'TubeToolData';
    STR_ISEC_STATEMONITOR = 'StateMonitor';
    STR_ISEC_ACTIONDEPENDENT = 'ActionDependent';

    // Areas (similar to ini file names)
    STR_SETTINGS_AREA_ROBOT = 'ROBOT'; // machine.ini
    STR_SETTINGS_AREA_APP = 'APPLICATION'; // sampler.ini
    STR_SETTINGS_AREA_DEVICES = 'DEVICE'; // device.ini
    STR_SETTINGS_AREA_BALANCE = 'BALANCE'; // balance.ini
    STR_SETTINGS_AREA_DLL = 'DLL'; // zusätzl. Einträge für spez. Dlls
    STR_SETTINGS_AREA_DRIVER = 'DRIVER'; // für Interfaces

    INT_CFR21_USERLOG_LOGTYPE_INTERN = 1;
    INT_CFR21_USERLOG_TYPE_LOGON = 1;
    INT_CFR21_USERLOG_TYPE_LOGOFF = 2;
    INT_CFR21_USERLOG_TYPE_WRONGPW = 3;
    INT_CFR21_USERLOG_TYPE_PWCHANGED = 4;
    INT_CFR21_USERLOG_TYPE_NEWUSER = 5;
    INT_CFR21_USERLOG_TYPE_DELUSER = 6;
    INT_CFR21_USERLOG_TYPE_DATAPWSET = 7;
    INT_CFR21_USERLOG_TYPE_DATAPWDEL = 8;

    INT_CFR21_USERLOG_LOGTYPE_ARCHIVING = 2;
    INT_CFR21_USERLOG_TYPE_RUNARCHIVE = 1;
    INT_CFR21_USERLOG_TYPE_BACKUP = 2;

    INT_CFR21_USERLOG_LOGTYPE_DBCHANGE = 3;
    INT_CFR21_USERLOG_TYPE_DATA_ADD = 1;
    INT_CFR21_USERLOG_TYPE_DATA_EDIT = 2;
    INT_CFR21_USERLOG_TYPE_DATA_REMOVE = 3;
    INT_CFR21_USERLOG_TYPE_DATA_BULK = 4;

    INT_CFR21_USERLOG_LOGTYPE_RUNNING = 4;
    INT_CFR21_USERLOG_TYPE_RUN_START = 1;
    INT_CFR21_USERLOG_TYPE_RUN_CONTINUE = 2;
    INT_CFR21_USERLOG_TYPE_RUN_MACHINEINTERRUPT = 3;
    INT_CFR21_USERLOG_TYPE_RUN_USERINTERRUPT = 4;
    INT_CFR21_USERLOG_TYPE_RUN_ABORT = 5;
    INT_CFR21_USERLOG_TYPE_RUN_WORKEDUP = 6;

    INT_CFR21_USERLOG_LOGTYPE_ALERT = 5;
    INT_CFR21_USERLOG_TYPE_ALERT_SQLEXEC = 1;

    STR_DEV_INI_DEFAULT_BIT_UNDEFINED = '-1';
    INT_DEV_INI_DEFAULT_ADR_UNDEFINED = low(Integer) + 1;
    INT_DEV_INI_DEFAULT_COM_UNDEFINED = low(Integer) + 1;
    STR_DEV_INI_DEFAULT_DLLNAME_UNDEFINED = '';

type
    // ----------------------------------------------------------------------------------------------
    // Exception Types
    // ----------------------------------------------------------------------------------------------
    EApplicationAbort = class(Exception);
    // ----------------------------------------------------------------------------------------------
    // Simple Types
    // ----------------------------------------------------------------------------------------------
    TPosMM = double; // Motor Position oder Längenangabe in mm

    // ----------------------------------------------------------------------------------------------
    // Enumerators
    // ----------------------------------------------------------------------------------------------
    TBasicTipType = (NoTip = 0, DefaultTip = 1, DispTip = 2, RediTip = 4, VarRediTip = 8);

    TBasicTipTypes = set of TBasicTipType;

    TIniAccessType = (iatNothing, iatDatabaseAccess, iatIniFileAccess);

    TUserLevel = (usrNothing, usrGuest, usrSystem, usrSystemAdmin, usrSupervisor, usrUnlimitedAdmin);

    TLogChangeType = (lctAdd, lctEdit, lctRemove, lctBulk);
    TLogRunType = (lrtStart, lrtUserInterrupt, lrtMachineInterrupt, lrtContinue, lrtAbort, lrtWorkedUp);

    TRunNameKind = (rkdNone, rkdMethod, rkdScript);

    TCFR21ComplianceMode = (ccmNone, ccmPrepared, ccmCompliant);

    TMachineConnectionType = (mctNoConnection, mctTpcClientConnection, mctDirectConnection);
    TAppPurpose = (appSimple, appStart, appEditAndStart, appLayouter);

    TAppMode = (appModeSim, appModeReal, appModeUserAbort);

    TStartParameters = record
        AppMode: TAppMode;
        IsDebug: boolean;
        IsNoStart: boolean;
        SpecialAlias: string;
    end;

    // ----------------------------------------------------------------------------------------------
    // Records
    // ----------------------------------------------------------------------------------------------
    BCSCANRANGE = record
        XScanRange: integer;
        XScanSteps: integer;
        YScanRange: integer;
        YScanSteps: integer;
        ZScanRange: integer;
        ZScanSteps: integer;
    end;

    BCRPOSITION = record
        XDefined, YDefined, ZDefined, RDefined: boolean;
        XPos, YPos, ZPos, RPos: integer;
        PosName: string;
    end;

    TTipType = record
        name: string;
        BasicType: TBasicTipType;
        DeviceName: string;
        ToolName: string;
        MaxVolume: double;
        MinVolume: double;
        RelLength_mm: double;
        DTOffset_mm: double;
        DilutorName: string;
        DitiType: string;
        XOffset_mm: double;
        YOffset_mm: double;
        WashZOffset_mm: double;
        WasteZOffset_mm: double;
        DryZOffset_mm: double;
        DoNotDryAfterFlush: boolean;
        MethodNameGetTip: string;
        MethodNamePutTip: string;
        DoNotInit: boolean;
    end;

    TTubeToolData = record
        name: string;
        XOffset, YOffset, ZOffset, ZDrop: TPosMM;
        VOpen, VDrop: TPosMM;
        TubeDY: TPosMM;
        SafeMoveSpeed: integer;
        SafeMoveOffset, ShakeHeight, ShakeZOffset, ShakeYOffset: TPosMM;
        BringBackVOffset: TPosMM;
        GetTubeXYShiftStepCount: integer;
        GetTubeXYShiftRadius1, GetTubeXYShiftRadius2: TPosMM;
    end;

    TDLLMenuItem = record
        DLLName: string;
        DLLFunction: string;
        Parameter: string;
        MenuCaption: string;
        MenuName: string;
    end;

    TDBFieldDefinition = record
        FieldName: string;
        Caption: string;
        PickList: string;
    end;

    TBackupPathData = record
        IsFolder: boolean;
        ArchiveName: string;
        PathName: string;
        Subfolders: boolean;
        CheckDate: boolean;
    end;

    TLiquidPortData = record
        DilName: string;
        AspSpeed: integer;
    end;

    TXYRangeData = record
        XRel1, XRel2, YRel1, YRel2: TPosMM;
    end;

    TOEMSettings = record
        AppTitle: string;
        JobTitle: string;
        OemTitle: string;
        Support: string;
        Copyr1: string;
        Copyr2: string;
        www: string;
    end;

    TUserLog = record
        UserPK: integer;
        UserName: string;
        LogTime: TDateTime;
        LogType: integer;
        ActionType: integer;
        Title: string;
        Action: string;
        Reason: string;
    end;

    TUserLogArray = array of TUserLog;

    // ----------------------------------------------------------------------------------------------
    // interfaces & abstract classes from ZACommon.dll
    // ----------------------------------------------------------------------------------------------
    IUser = class
    protected
        function GetName: string; virtual; abstract;
        function GetLevel: TUserLevel; virtual; abstract;
        function GetMustGiveReason: boolean; virtual; abstract;
        function GetCFR21Mode: TCFR21ComplianceMode; virtual; abstract;
        function GetSystemDataPassword: string; virtual; abstract;
    public
        procedure ChangePassword; virtual; abstract;
        procedure ShowAllUsers(aUserListItems: TListItems); virtual; abstract;
        function DeleteUser(aUserName: string): boolean; virtual; abstract;
        function AddNewUser: boolean; virtual; abstract;
        function LogArchiving(var aFileName: string): boolean; virtual; abstract;
        function LogCompleteBackup(var aFileName: string): boolean; virtual; abstract;
        function GetArchiveFileInfo(const aFileName: string; const aInfo: TList<string>): boolean;
            virtual; abstract;
        procedure LogDataChanged(aTableName, aAction, aReason: string; aChangeType: TLogChangeType);
            virtual; abstract;
        procedure LogRun(aRunKind: TRunNameKind; aRunName, aAction, aReason: string;
            aLogRunType: TLogRunType); virtual; abstract;
        function HasLevel(aMinimumLevel: TUserLevel): boolean; virtual; abstract;
        function ChangeUserLevel(aUserName: string): boolean; virtual; abstract;
        function ChangeUserDescription(aUserName: string): boolean; virtual; abstract;
        function ResetUserPassword(aUserName: string): boolean; virtual; abstract;
        function GetUserLog(aFrom, aTo: TDateTime): TUserLogArray; virtual; abstract;
        function GetReason(out oReason: string): boolean; virtual; abstract;
        function ConfirmDataChanged(const aTableName, aAction: string; aChangeType: TLogChangeType): boolean;
            virtual; abstract;
        function CheckSQLBeforeUpdate(const aSQLText: string): boolean; virtual; abstract;
        // Properties
        property name: string read GetName;
        property Level: TUserLevel read GetLevel;
        property MustGiveReason: boolean read GetMustGiveReason;
        property CFR21Mode: TCFR21ComplianceMode read GetCFR21Mode;
        property SystemDataPassword: string read GetSystemDataPassword;
    end;

    ISimpleIniAccess = interface
        ['{EC62B726-1AB6-4F8F-862D-26D862218851}']
        function GetFileName: string;
        // CacheMethods
        procedure ReadIntoCache();
        procedure WriteFromCache();

        procedure ReadSectionIntoCache(const aSection: string);
        procedure WriteSectionFromCache(const aSection: string);
        // Read Methods
        function ReadBool(aSection, aIdent: string; aDefault: Boolean): Boolean;
        function ReadString(aSection, aIdent, aDefault: string): string;
        function ReadInteger(aSection, aIdent: string; aDefault: Longint): Longint;
        function ReadFloat(aSection, aIdent: string; aDefault: double): double;
        // Write Methods
        procedure WriteBool(aSection, aIdent: string; aValue: Boolean);
        procedure WriteString(aSection, aIdent, aValue: string);
        procedure WriteInteger(aSection, aIdent: string; aValue: Longint);
        procedure WriteFloat(aSection, aIdent: string; aValue: double);
        // other public methods
        function SectionExists(aSection: string): Boolean;
        // deprecated; (erst in Delphi 6) // problematisch, weil bei DB-Access nicht abzubilden
        function ReadSection(aSection: string): TStringArray;
        function DeleteKey(aSection, aIdent: string; aExecute: boolean): boolean;
        // Properties
        property FileName: string read GetFileName;
    end;

    ICheckingIniAccess = interface
        ['{E2A4D938-290A-4E85-ACF8-D41C6392CBC4}']
        function GetFileName: string;
        // CacheMethods
        procedure ReadIntoCache();
        // procedure WriteFromCache();  zu gefährlich, darf es hier nicht geben!

        procedure ReadSectionIntoCache(const aSection: string);
        procedure WriteSectionFromCache(const aSection: string);
        // Read Methods
        function ReadBool(aSection, aIdent: string): Boolean;
        function ReadString(aSection, aIdent: string): string;
        function ReadInteger(aSection, aIdent: string): Longint;
        function ReadFloat(aSection, aIdent: string): double;
        // Write Methods
        procedure WriteBool(aSection, aIdent: string; aValue: Boolean);
        procedure WriteString(aSection, aIdent, aValue: string);
        procedure WriteInteger(aSection, aIdent: string; aValue: Longint);
        procedure WriteFloat(aSection, aIdent: string; aValue: double);
        // other public methods
        function ValueExists(aSection, aIdent: string): boolean;
        function DeleteKey(aSection, aIdent: string; aExecute: boolean): boolean;
        function ReadAllowedSection(aSection, aDefaultIdent: string): TStringArray;
        // Properties
        property FileName: string read GetFileName;
    end;

    IWinLissyIniAccess = interface(ICheckingIniAccess)
        ['{9A18818F-A8EB-458C-8750-12E5429E5041}']
        // Read Methods
        function ReadBcscanrange(aSection, aIdent: string): BCSCANRANGE;
        function ReadBcrposition(aSection, aIdent: string): BCRPOSITION;
        function ReadDLLMenuItem(aSection, aIdent: string): TDLLMenuItem;
        function ReadTubeToolData(aSection, aIdent: string): TTubeToolData;
        function ReadLiquidPortData(aSection, aIdent: string): TLiquidPortData;
        function ReadDBFieldDefinition(aSection, aIdent: string): TDBFieldDefinition;
        function ReadBackupPathData(aSection, aIdent: string): TBackupPathData;
    end;

    IMethodIniAccess = interface(ICheckingIniAccess)
        ['{3BADE811-9B53-4EE6-B738-362DCDB9E576}']
        procedure DeleteAllAndSave(aCurrentUser: IUser; const aReason: string);
    end;


implementation


end.
