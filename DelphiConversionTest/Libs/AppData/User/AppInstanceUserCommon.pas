{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Inherits instances for UserCommon library
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.08.07 wl                               TN3811.4  initial version
  03.09.07 wl  fBackupManager               TN3811.4  neue Member-Variable
  03.09.07 wl                               TN3811.4  erzeugt jetzt auch TAppInstanceLicence
  03.09.07 wl                               TN3811.4  enthält Init-Funktionen aus AppSettings und ZACommonMain
  04.09.07 wl                               TN3811.4  Bugs entfernt
  13.07.09 pk  GetParadoxAliasNames         TN4585.4  RunAlias no longer added
  07.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  26.10.09 wl  LoginUser                    TN4831   IConfigurationSet replaces TIniFile
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.05.10 wl                               TN5111   LocalIniPath,LogfilePath: Bei Win7 ProgramData statt Program Files
  20.05.10 wl                               TN5116   überarbeitet
  29.09.10 wl                               TN5112   uses LicenseKeyUtils
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  12.11.10 wl  CreateLicence                TN5112   Neu: Staff Licence
  17.01.11 wl  CreateLicence                TN5112   Bugfix
  27.01.13 wl                               TN6069   'Backup','Cycles' entfernt
  27.03.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit AppInstanceUserCommon;


interface


uses
    CommonTypes,
    User,
    BackupManager;

type
    TAppInstanceUserCommon = class
    private
        // benutzt TAppInstanceIniAdaptor, TAppInstanceLicence
        fCurrentUser: TUser;
        fBackupManager: TBackupManager;
        fOEMSettings: TOEMSettings;
        fStartParameters: TStartParameters;

        class var uInstUserCommon: TAppInstanceUserCommon; // Single Instance
        //
        class function GetParadoxAliasNames: TArray<string>;
        procedure CreateLicence(aAppMode: TAppMode);
        function CreateBackupManager(aCurrentUser: IUser): TBackupManager;
        procedure ReadOEMSettings();
        function GetIsDebugMode: boolean;
        function GetIsNoStart: boolean;

        constructor Create(aStartParameters: TStartParameters);
    public
        class function CreateInstance(aStartParameters: TStartParameters): TAppInstanceUserCommon;
        destructor Destroy(); override;
        class procedure DestroyInstance();

        function LoginUser(aMustShowLogon: boolean; aCancelVisible: boolean): boolean;
        function ChangeCurrentUser: IUser;
        function CompleteZipBackup(aFileName: string): string;
        function StartCompliantMode(): boolean;
        function GetAppName(aPurpose: TAppPurpose): string;

        property CurrentUser: TUser read fCurrentUser;
        property OEMSettings: TOEMSettings read fOEMSettings;
        property StartParameters: TStartParameters read fStartParameters;
        property IsDebugMode: boolean read GetIsDebugMode;
        property IsNoStart: boolean read GetIsNoStart;

        class property Instance: TAppInstanceUserCommon read uInstUserCommon;
    end;


implementation


uses
    Windows,
    Generics.Collections,
    Forms,
    Controls,
    SysUtils,
    AppInstanceLicencing,
    Licence,
    DataAdaptor,
    AppInstanceIniAdaptor,
    DataProviderFactory,
    DialogUtils,
    GeneralTypes,
    ConfigurationFile,
    FileUtilities,
    LicenseKeyUtils;

{ TAppInstanceUserCommon }

class function TAppInstanceUserCommon.CreateInstance(aStartParameters: TStartParameters)
    : TAppInstanceUserCommon;
begin
    // create instance if instance does not exist
    if not Assigned(uInstUserCommon) then
        uInstUserCommon := TAppInstanceUserCommon.Create(aStartParameters);

    // return instance
    result := uInstUserCommon;
end;

function TAppInstanceUserCommon.GetAppName(aPurpose: TAppPurpose): string;
begin
    result := '';
    if fStartParameters.SpecialAlias <> '' then
    begin
        result := fStartParameters.SpecialAlias + ': ';
    end;

    if (aPurpose = appEditAndStart) then
    begin
        if (fOEMSettings.AppTitle <> '') then
            result := result + fOEMSettings.AppTitle + ' Designer'
        else
            result := result + 'Zinsser Method Designer';
    end
    else if (aPurpose = appLayouter) then
    begin
        if (fOEMSettings.AppTitle <> '') then
            result := result + fOEMSettings.AppTitle + ' Layouter'
        else
            result := result + 'Zinsser Layouter';
    end
    else if (aPurpose = appStart) then
    begin
        if (fOEMSettings.AppTitle <> '') then
            result := result + fOEMSettings.AppTitle
        else
            result := result + 'Zinsser Method Runner';
    end
    else
    begin
        result := result + Application.Title;
    end;
end;

constructor TAppInstanceUserCommon.Create(aStartParameters: TStartParameters);
begin
    inherited Create;

    fStartParameters := aStartParameters;

    // Login User (define FCurrentUser / use FPlatform)
    if not LoginUser(false, true) then
        EXIT;

    if (fCurrentUser.Level = usrNothing) then
        raise Exception.Create('No user!');

    CreateLicence(fStartParameters.AppMode);

    // open backup manager if number of days is over complete backup
    fBackupManager := CreateBackupManager(fCurrentUser);

    ChDir(TFileUtilities.GetApplicationDataPath()); // current path = application path

    ReadOEMSettings();

    if not TUser.LevelIsIncluded(fCurrentUser.Level, usrSystem) then
        fStartParameters.IsNoStart := true;
end;

destructor TAppInstanceUserCommon.Destroy;
begin
    FreeAndNil(FCurrentUser);
    FreeAndNil(fBackupManager);
    TAppInstanceLicencing.Instance.Free;

    inherited;
end;

class procedure TAppInstanceUserCommon.DestroyInstance;
begin
    FreeAndNil(uInstUserCommon);
end;

function TAppInstanceUserCommon.LoginUser(aMustShowLogon: boolean; aCancelVisible: boolean): boolean;
var
    xConfigurationFile: IConfigurationSet;
    xLastUser: string;
begin
    // letzten User aus IniFile lesen
    xConfigurationFile := TConfigurationFile.Create(TAppInstanceIniAdaptor.Instance.LocalIniFileName);
    xConfigurationFile.Open(false);
    try
        xLastUser := xConfigurationFile.ReadString('Info', 'LastUserName', '');

        // Login new user
        FCurrentUser := TUser.Create(xLastUser, aMustShowLogon, aCancelVisible);

        // Check current user
        result := (FCurrentUser.Level <> usrNothing);
        if (result) then
            xConfigurationFile.WriteString('Info', 'LastUserName', FCurrentUser.Name);
    finally
        xConfigurationFile.Close;
    end;
end;

procedure TAppInstanceUserCommon.CreateLicence(aAppMode: TAppMode);
var
    xIniAccess: IWinLissyIniAccess;
begin
    TAppInstanceLicencing.CreateInstance(aAppMode);

    if TAppInstanceLicencing.Instance().Licence.LicencePartValid(licStaffLicense) then
    begin
        // Staff licence: Read serial number from settings!
        xIniAccess := TAppInstanceIniAdaptor.Instance.Settings.CreateAppIni;
        TAppInstanceLicencing.Instance().Licence.ChangeStaffLicenseParts
            (xIniAccess.ReadString('Info', 'SerialNo'));
    end
    else
    begin
        // Customer licence: Write serial number to settings!
        xIniAccess := TAppInstanceIniAdaptor.Instance.Settings.CreateAppIni;
        xIniAccess.WriteString('Info', 'SerialNo', TAppInstanceLicencing.Instance().Licence.Key);
        xIniAccess.WriteSectionFromCache('Info');
    end;

    // system can not be compliant without the CFR21 licence key
    if (fCurrentUser.CFR21Mode <> ccmNone) and
        (not TAppInstanceLicencing.Instance().Licence.LicencePartValid(licCFR21Compliant)) then
        raise Exception.Create('Your licence key does not allow CFR21 compliant mode!');

    // system can not be run without user management if the the CFR21 licence key is set
    if (fCurrentUser.CFR21Mode = ccmNone) and
        (TAppInstanceLicencing.Instance().Licence.LicencePartValid(licCFR21Compliant)) then
        raise Exception.Create('You have to use the CFR21 compliant user management!');
end;

function TAppInstanceUserCommon.CreateBackupManager(aCurrentUser: IUser): TBackupManager;
var
    xIniAccess: IWinlissyIniAccess;
    xPath: string;
    xDayCycle: integer;
begin
    xIniAccess := TAppInstanceIniAdaptor.Instance.Settings.CreateAppIni;
    xPath := ExpandFileName(xIniAccess.ReadString('Backup', 'Path'));
    xDayCycle := 0;

    EXIT(TBackupManager.Create(aCurrentUser, GetParadoxAliasNames(), xPath, xDayCycle));
end;

class function TAppInstanceUserCommon.GetParadoxAliasNames: TArray<string>;
var
    xAliasNames: TList<string>;
begin
    xAliasNames := TList<string>.Create;
    try
        xAliasNames.Add(TDataProviderFactory.Instance.MainDBAlias);
        if (TDataProviderFactory.Instance.UserDBAlias <> '') then
            xAliasNames.Add(TDataProviderFactory.Instance.UserDBAlias);

        EXIT(xAliasNames.ToArray);
    finally
        FreeAndNil(xAliasNames);
    end;
end;

function TAppInstanceUserCommon.ChangeCurrentUser: IUser;
begin
    if TDialogUtils.MessageBox(TLanguageString.Read('The current user {0} will be logged off - Continue?',
        'Der aktuelle Benutzer {0} wird abgemeldet - Vorgang fortsetzen?', [FCurrentUser.Name]), '', MB_YESNO)
        = IDYES then
    begin
        FreeAndNil(FCurrentUser);
        LoginUser(true, false);
    end;
    result := FCurrentUser;
end;

function TAppInstanceUserCommon.CompleteZipBackup(aFileName: string): string;
begin
    Screen.Cursor := crHourglass;
    try
        result := FBackupManager.CompleteZipBackup(fCurrentUser, GetParadoxAliasNames(), aFileName);
    finally
        Screen.Cursor := crDefault;
    end;
end;

function TAppInstanceUserCommon.StartCompliantMode: boolean;
var
    xBackupFileName: string;
    xAliasNames: TArray<string>;
begin
    Screen.Cursor := crHourglass;
    try
        xAliasNames := GetParadoxAliasNames();

        // backup all database files
        xBackupFileName := FBackupManager.CompleteZipBackup(fCurrentUser, xAliasNames, '');

        result := false;
        if (xBackupFileName <> '') then
            result := fCurrentUser.ChangeSystemDataPassword(xAliasNames);
    finally
        Screen.Cursor := crDefault;
    end;
end;

procedure TAppInstanceUserCommon.ReadOEMSettings();
var
    xIniAccess: IWinlissyIniAccess;
begin
    xIniAccess := TAppInstanceIniAdaptor.Instance.Settings.CreateAppIni;

    fOEMSettings.AppTitle := xIniAccess.ReadString('Oem', 'AppTitle');
    fOEMSettings.OemTitle := xIniAccess.ReadString('Oem', 'OemTitle');
    fOEMSettings.JobTitle := xIniAccess.ReadString('Oem', 'JobTitle');
    fOEMSettings.Support := xIniAccess.ReadString('Oem', 'Support');
    fOEMSettings.Copyr1 := xIniAccess.ReadString('Oem', 'Copyr1');
    fOEMSettings.Copyr2 := xIniAccess.ReadString('Oem', 'Copyr2');
    fOEMSettings.www := xIniAccess.ReadString('Oem', 'www');
end;

function TAppInstanceUserCommon.GetIsDebugMode: boolean;
begin
    result := fStartParameters.IsDebug;
end;

function TAppInstanceUserCommon.GetIsNoStart: boolean;
begin
    result := fStartParameters.IsNoStart;
end;


end.
