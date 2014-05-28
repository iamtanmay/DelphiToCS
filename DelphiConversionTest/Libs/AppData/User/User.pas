{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : User object for the user management
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.01.03 wl                               TN1334.1 initial version
  09.01.03 wl                               TN1334.1 komplett überarbeitet
  21.01.03 wl  Login                        TN1334.1 Bugfix: Login as Supervisor at start
  21.01.03 wl  LogArchiving                 TN1332.4 neue Funktion
  06.02.03 wl  LogDataChanged               TN1334.3 neu: Log für alle Datenänderungen
  06.02.03 wl  GetMustGiveReason            TN1334.3 neu: Abfrage, ob Grund für Änderungen angegeben werden muß
  11.02.03 wl  LevelIsIncluded              TN1334.3 neu: User level: usrUnlimitedAdmin
  14.02.03 wl  AddNewUser                   TN1293.5 Calls wizard with user bitmap (Res-no.498)
  14.02.03 wl  LogRun                       TN1334.3 New logs for start run, stop run, ...
  19.02.03 wl  LogRun, Log..                TN1334.3 mit LogType/ActionType und Title
  20.02.03 wl  LogRun                       TN1334.3 mit RunName und GlobalNameKind
  20.02.03 wl  LogDataChanged               TN1334.3 Tabellenname immer ohne extension
  11.03.03 wl  DeleteUser                   TN1334.7 Exceptions werden auch ausgewertet
  12.03.03 wl  gmCalcFileCRC32              TN1293.5 aus ZACommonUtils hierher verschoben
  17.03.03 wl  AddNewUser                   TN1334.7 Exceptions werden auch angezeigt
  21.03.03 wl  ChangeSystemDataPassword     TN1439.4 ändert Password im Data Adaptor
  21.03.03 wl  GetCFR21Mode                 TN1439.4 gibt den Modus zurück (None, Prepared, Complient)
  25.03.03 wl  SetSystemDataPassword        TN1439.4 gibt Password nur an DataAdapter weiter
  25.03.03 wl  GetSystemDataPassword        TN1439.4 jetzt protected
  26.03.03 wl  Login                        TN1439.4 Anpassung an DataAdaptor-Änderungen
  27.03.03 wl                               TN1439.4 Anpassung an DataAdaptor-Änderungen
  03.04.03 wl  LogCompleteBackup            TN1439.5 Unterscheidung zwischen RunArchiv und CompleteBackup
  14.04.03 wl  GetArchiveFileInfo           TN1332.6 neue Methode zum Prüfen der ZipArchive (für Retrieve-Applikation)
  14.04.03 wl  LogArchiveAndChecksum        TN1332.6 --> UserDataAdaptor
  17.04.03 wl  CheckPassword                TN1439.4 von ZACommonMain hierher
  17.04.03 wl  ChangeSystemDataPassword     TN1439.4 Password kann nicht mehr frei gewählt werden
  28.04.03 wl  ChangeUserDescription        TN1473.1 neu: Description ändern
  28.04.03 wl  ChangeUserLevel              TN1473.1 neu: User Level ändern
  28.04.03 wl  ResetUserPassword            TN1473.1 neu: Supervisor bestimmt neues User Password
  21.04.04 wl                               TN1869   INT_CFR21_USERLOG-Konstanten --> CommonTypes
  21.04.04 wl  GetUserLog                   TN1869   neu: Supervisor kann UserLog für einen Zeitraum einsehen
  20.12.06 el  Login                        TN1475   Anmeldefenster bleibt beim 2.Versuch im Vordergrund
  27.04.07 wl  GetReason,ConfirmDataChanged TN3669   aus TAppSettings heirher, benutzt UserConfirmation.pas
  03.05.07 wl  CheckSQLBeforeUpdate         TN3669   Testet, ob der Anwender Tabellen aus ZINSSER_APP manipulieren will
  03.05.07 wl  LogAlert                     TN3669   Loggt, wenn der User Dinge tut, die verboten sind
  29.07.07 wl  AddNewUser,ChangeUserLevel   TN3811   Wizard wird mit Create, nicht mit CreateWithBitmap aufgerufen
  03.09.07 wl                               TN3811.4 uses UserDataWrapper
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  20.05.10 wl                               TN5117   uses ControlUtils
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  27.01.12 wl  GetLevelName                 TN5648   "System" statt "Sytem"
  27.01.12 wl  LevelIsIncluded              TN5648   mit EXIT etwas schneller
  27.03.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit User;


interface


uses
    Generics.Collections,
    ComCtrls,
    GeneralTypes,
    CommonTypes,
    UserDataWrapper;

type
    TUser = class(IUser)
    private const
        STR_USER_SUPERVISOR = 'supervisor';
    private
        FDataAdaptor: TUserDataWrapper;
        FUserPK: integer;
        FName: string;
        FLevel: TUserLevel;
        fLastReason: string;
        procedure Login(aLastUser: string; aMustShowLogon: boolean; aCancelVisible: boolean);
        function GetReasonIntern(out oReason: string; const aLogInfo: TStringArray;
            aChangeType: TLogChangeType): boolean;
    protected
        function GetName: string; override;
        function GetLevel: TUserLevel; override;
        function GetMustGiveReason: boolean; override;
        function GetCFR21Mode: TCFR21ComplianceMode; override;
        function GetSystemDataPassword: string; override;
    public
        // Constructor/Destructor
        constructor Create(aLastUser: string; aMustShowLogon: boolean; aCancelVisible: boolean);
        destructor Destroy; override;
        // public class methods
        class function LevelIsIncluded(aIsLevel, aMinimumLevel: TUserLevel): boolean;
        class function GetLevelName(aUserLevel: TUserLevel): string;
        class function GetIntFromLogType(aChangeType: TLogChangeType): integer;
        // public methods (derived from IUser)
        procedure ChangePassword; override;
        procedure ShowAllUsers(aUserListItems: TListItems); override;
        function DeleteUser(aUserName: string): boolean; override;
        function AddNewUser: boolean; override;
        function LogArchiving(var aFileName: string): boolean; override;
        function LogCompleteBackup(var aFileName: string): boolean; override;
        function GetArchiveFileInfo(const aFileName: string; const aInfo: TList<string>): boolean; override;
        procedure LogDataChanged(aTableName, aAction, aReason: string; aChangeType: TLogChangeType); override;
        procedure LogRun(aRunKind: TRunNameKind; aRunName, aAction, aReason: string;
            aLogRunType: TLogRunType); override;
        function HasLevel(aMinimumLevel: TUserLevel): boolean; override;
        function ChangeUserLevel(aUserName: string): boolean; override;
        function ChangeUserDescription(aUserName: string): boolean; override;
        function ResetUserPassword(aUserName: string): boolean; override;
        function GetUserLog(aFrom, aTo: TDateTime): TUserLogArray; override;
        function GetReason(out oReason: string): boolean; override;
        function ConfirmDataChanged(const aTableName, aAction: string; aChangeType: TLogChangeType)
            : boolean; override;
        function CheckSQLBeforeUpdate(const aSQLText: string): boolean; override;
        procedure LogAlert(const aAction: string);
        function ChangeSystemDataPassword(const aAliasNames: TArray<string>): boolean;
    end;


implementation


uses
    Controls,
    Forms,
    Windows,
    SysUtils,
    Dialogs,
    Graphics,
    UserLogin,
    UserChangePassword,
    ControlUtils,
    DialogUtils,
    Wizard,
    UserAddNew1,
    UserAddNew2,
    DataAdaptor,
    UserConfirmation,
    DataProviderFactory;

{ TUser }

constructor TUser.Create(aLastUser: string; aMustShowLogon: boolean; aCancelVisible: boolean);
begin
    inherited Create;

    FName := '';
    FLevel := usrNothing;
    FUserPK := 0;
    fLastReason := '';

    // Create Data Adaptor
    if (TDataAdaptor.GetCFR21Mode <> ccmNone) then
        FDataAdaptor := TCFR21UserDataWrapper.Create
    else
        FDataAdaptor := TOldUserDataWrapper.Create();

    // Show Login form
    Login(aLastUser, aMustShowLogon, aCancelVisible);
end;

destructor TUser.Destroy;
begin
    if (FLevel <> usrNothing) then
        FDataAdaptor.UserLogoff(FName);
    FDataAdaptor.Free;

    inherited Destroy;
end;

class function TUser.LevelIsIncluded(aIsLevel, aMinimumLevel: TUserLevel): boolean;
begin
    if (aMinimumLevel = usrNothing) then
        EXIT(true);

    if (aIsLevel = aMinimumLevel) then
        EXIT(true);

    // 'System' includes 'Guest'
    if (aIsLevel = usrSystem) and (aMinimumLevel = usrGuest) then
        EXIT(true);

    // 'SystemAdmin' includes 'Guest' & 'System'
    if (aIsLevel = usrSystemAdmin) and (aMinimumLevel = usrGuest) then
        EXIT(true);
    if (aIsLevel = usrSystemAdmin) and (aMinimumLevel = usrSystem) then
        EXIT(true);

    // 'Supervisor' includes 'Guest'
    if (aIsLevel = usrSupervisor) and (aMinimumLevel = usrGuest) then
        EXIT(true);

    // 'SystemAdmin' (not CFR 21-complient) includes 'Guest', 'System', 'SystemAdmin' & 'Supervisor'
    if (aIsLevel = usrUnlimitedAdmin) and (aMinimumLevel = usrGuest) then
        EXIT(true);
    if (aIsLevel = usrUnlimitedAdmin) and (aMinimumLevel = usrSystem) then
        EXIT(true);
    if (aIsLevel = usrUnlimitedAdmin) and (aMinimumLevel = usrSystemAdmin) then
        EXIT(true);
    if (aIsLevel = usrUnlimitedAdmin) and (aMinimumLevel = usrSupervisor) then
        EXIT(true);

    EXIT(false);
end;

procedure TUser.Login(aLastUser: string; aMustShowLogon: boolean; aCancelVisible: boolean);
var
    xUserName, xPassword: string;
    xLevel: TUserLevel;
    xFrmUserLogin: TfrmUserLogin;
    xFrmChangePw: TfrmUserChangePassword;
begin
    xUserName := aLastUser;
    xLevel := usrNothing;

    // wenn User vorhanden & und PW = '' -> keine LoginBox
    if (not aMustShowLogon) then
        xLevel := FDataAdaptor.CheckUserNoPW(xUserName);

    // wenn noch kein Supervisor vorhanden, dann als Supervisor
    if (FDataAdaptor is TCFR21UserDataWrapper) and
        (not(FDataAdaptor as TCFR21UserDataWrapper).FindSupervisor) then
    begin
        TDialogUtils.MessageBox(TLanguageString.
            Read('A user named - supervisor - will be created. Enter a password for this user.',
            'Ein Benutzer mit dem Namen - Supervisor - wird angelegt. Geben Sie ein Kennwort für diesen Benutzer ein.'),
            TLanguageString.Read('Build up user management.', 'Benutzerverwaltung wird angelegt.'), 64);

        // Get supervisor password & create supervisor as new user
        xFrmChangePw := TfrmUserChangePassword.CreateForm(nil, true, STR_USER_SUPERVISOR);
        xFrmChangePw.Caption := TLanguageString.Read('Enter password for: supervisor',
            'Geben Sie ein Kennwort für Supervisor ein');
        xFrmChangePw.ShowModal;
        if (xFrmChangePw.ModalResult = mrCancel) then
            exit;
        FDataAdaptor.CreateNewUser(0, STR_USER_SUPERVISOR, xFrmChangePw.Edit1.Text,
            'The supervisor is created at first start of the software', usrSupervisor);
        xUserName := STR_USER_SUPERVISOR;
        xLevel := usrSupervisor;
        xFrmChangePw.Free;
    end;

    if (xLevel = usrNothing) then
    begin

        xFrmUserLogin := TfrmUserLogin.CreateForm(nil, true);
        try
            xFrmUserLogin.edPassword.Text := '';
            repeat
                // Show Login Box
                xFrmUserLogin.btnAbort.Visible := aCancelVisible;
                xFrmUserLogin.edUsername.Text := xUserName;
                if (xFrmUserLogin.ShowModal = mrAbort) then
                    exit;
                xUserName := xFrmUserLogin.edUsername.Text;
                xPassword := xFrmUserLogin.edPassword.Text;

                // Check user
                xLevel := FDataAdaptor.CheckUser(xUserName, xPassword);

                if (xLevel = usrNothing) then
                    TDialogUtils.MessageBox(TLanguageString.
                        Read('The Zinsser User Manager could not log you on. Please check name and password.',
                        'Der Zinsser Benutzermanager konnte Sie nicht anmelden. Bitte überprüfen Sie Namen und Kennwort.'),
                        '', 16);
            until (xLevel <> usrNothing);

        finally
            xFrmUserLogin.Free;
        end;
    end;

    FLevel := xLevel;
    FName := xUserName;
    if (FLevel <> usrNothing) then
        FUserPK := FDataAdaptor.UserLogin(FName, GetLevelName(FLevel));
end;

class function TUser.GetLevelName(aUserLevel: TUserLevel): string;
begin
    case aUserLevel of
        usrGuest:
            EXIT('Guest');
        usrSystem:
            EXIT('System User');
        usrSystemAdmin:
            EXIT('System Administrator');
        usrSupervisor:
            EXIT('Supervisor');
        else
            EXIT('None');
    end;
end;

function TUser.GetName: string;
begin
    result := FName;
end;

function TUser.GetLevel: TUserLevel;
begin
    result := FLevel;
end;

procedure TUser.ShowAllUsers(aUserListItems: TListItems);
begin
    if LevelIsIncluded(FLevel, usrSupervisor) then
    begin
        FDataAdaptor.ShowAllUsers(aUserListItems);
    end;
end;

function TUser.DeleteUser(aUserName: string): boolean;
begin
    result := false;
    if LevelIsIncluded(FLevel, usrSupervisor) then
    begin
        try
            FDataAdaptor.DeleteUser(FUserPK, aUserName);
            result := true;
        except
            on E: Exception do
                ShowMessage(E.Message);
        end;
    end;
end;

function TUser.AddNewUser: boolean;
var
    xfrmWizard: TfrmWizard;
    xName, xDscr, xPassword: string;
    xLevel: TUserLevel;
begin
    result := false;

    // this can only be done by the supervisor
    if LevelIsIncluded(FLevel, usrSupervisor) then
    begin

        // Create a "Add new user" wizard
        xfrmWizard := TfrmWizard.Create(nil); // WithBitmap(nil, 498{user bitmap});
        xfrmWizard.Caption := 'Introducing a new user';
        xfrmWizard.Add(TfraUserAddNew1.Create(xfrmWizard));
        xfrmWizard.Add(TfraUserAddNew2.Create(xfrmWizard));
        if (xfrmWizard.ShowModal = mrOK) then
        begin
            try
                xName := (xfrmWizard.Items[0] as TfraUserAddNew1).GetName;
                xDscr := (xfrmWizard.Items[0] as TfraUserAddNew1).GetDescription;
                xPassword := (xfrmWizard.Items[0] as TfraUserAddNew1).GetPassword;
                xLevel := (xfrmWizard.Items[1] as TfraUserAddNew2).GetUserLevel;

                // Add the new user
                FDataAdaptor.CreateNewUser(FUserPK, xName, xPassword, xDscr, xLevel);
                result := true;
            except
                on E: Exception do
                    ShowMessage(E.Message);
            end;
        end;
        xfrmWizard.Free;
    end;
end;

procedure TUser.ChangePassword;
var
    xFrmChangePw: TfrmUserChangePassword;
    xNoEmptyPw: boolean;
begin
    // Get new password (if CFR21 & User > Guest: no empty password allowed)
    if (FDataAdaptor is TCFR21UserDataWrapper) and (FLevel <> usrGuest) then
        xNoEmptyPw := true
    else
        xNoEmptyPw := false;

    xFrmChangePw := TfrmUserChangePassword.CreateForm(nil, xNoEmptyPw, FName);
    xFrmChangePw.ShowModal;

    if (xFrmChangePw.ModalResult <> mrCancel) then
        FDataAdaptor.ChangePassword(0, FName, xFrmChangePw.Edit1.Text);

    xFrmChangePw.Free;
end;

function TUser.LogArchiving(var aFileName: string): boolean;
begin
    result := FDataAdaptor.LogArchiveAndChecksum(aFileName, FUserPK, INT_CFR21_USERLOG_TYPE_RUNARCHIVE);
end;

function TUser.LogCompleteBackup(var aFileName: string): boolean;
begin
    result := FDataAdaptor.LogArchiveAndChecksum(aFileName, FUserPK, INT_CFR21_USERLOG_TYPE_BACKUP);
end;

function TUser.GetArchiveFileInfo(const aFileName: string; const aInfo: TList<string>): boolean;
begin
    result := false;
    if (FDataAdaptor is TCFR21UserDataWrapper) then
        result := (FDataAdaptor as TCFR21UserDataWrapper).GetArchiveFileInfo(aFileName, aInfo);
end;

function TUser.HasLevel(aMinimumLevel: TUserLevel): boolean;
begin
    result := LevelIsIncluded(FLevel, aMinimumLevel);
end;

function TUser.GetMustGiveReason: boolean;
begin
    result := false;
    if (GetCFR21Mode = ccmCompliant) then
        result := true;
end;

procedure TUser.LogRun(aRunKind: TRunNameKind; aRunName, aAction, aReason: string; aLogRunType: TLogRunType);
var
    xLogtype: integer;
begin
    xLogtype := 0;
    case (aLogRunType) of
        lrtStart:
            xLogtype := INT_CFR21_USERLOG_TYPE_RUN_START;
        lrtContinue:
            xLogtype := INT_CFR21_USERLOG_TYPE_RUN_CONTINUE;
        lrtMachineInterrupt:
            xLogtype := INT_CFR21_USERLOG_TYPE_RUN_MACHINEINTERRUPT;
        lrtUserInterrupt:
            xLogtype := INT_CFR21_USERLOG_TYPE_RUN_USERINTERRUPT;
        lrtAbort:
            xLogtype := INT_CFR21_USERLOG_TYPE_RUN_ABORT;
        lrtWorkedUp:
            xLogtype := INT_CFR21_USERLOG_TYPE_RUN_WORKEDUP;
    end;
    if (aRunName <> '') then
    begin
        aRunName := Uppercase(aRunName);
        if (aRunKind = rkdMethod) then
            aRunName := 'M: ' + aRunName;
        if (aRunKind = rkdScript) then
            aRunName := 'S: ' + aRunName;
    end;
    FDataAdaptor.Log(FUserPK, INT_CFR21_USERLOG_LOGTYPE_RUNNING, xLogtype, aRunName, aAction, aReason);
end;

function TUser.GetCFR21Mode: TCFR21ComplianceMode;
begin
    result := TDataAdaptor.GetCFR21Mode;
end;

function TUser.GetSystemDataPassword: string;
begin
    result := TDataProviderFactory.Instance.SystemDataPassword;
end;

function TUser.ChangeSystemDataPassword(const aAliasNames: TArray<string>): boolean;
var
    xErrTable: string;
begin
    result := false;
    if (FDataAdaptor is TCFR21UserDataWrapper) then
    begin
        xErrTable := (FDataAdaptor as TCFR21UserDataWrapper).ChangeSystemDataPassword(FUserPK, aAliasNames);
        if (xErrTable = '') then
            result := true
        else
            ShowMessage('Error changing run table: ' + xErrTable);
    end;
end;

function TUser.ChangeUserDescription(aUserName: string): boolean;
var
    xDescription: string;
begin
    result := false;

    if (FDataAdaptor is TCFR21UserDataWrapper) then
        if TDialogUtils.InputQuery(TLanguageString.Read('Enter a new description for user {0}:',
            'Geben Sie eine neue Bescreibung für Benutzter {0} ein:', [aUserName]),
            TLanguageString.Read('Change Description', 'Beschreibung ändern'), xDescription) then
        begin
            (FDataAdaptor as TCFR21UserDataWrapper).ChangeDescription(FUserPK, aUserName, xDescription);
            result := true;
        end;
end;

function TUser.ChangeUserLevel(aUserName: string): boolean;
var
    xfrmWizard: TfrmWizard;
    xLevel: TUserLevel;
begin
    result := false;

    // this can only be done by the supervisor
    if (FDataAdaptor is TCFR21UserDataWrapper) and LevelIsIncluded(FLevel, usrSupervisor) then
    begin

        // Create a "Add new user" wizard
        xfrmWizard := TfrmWizard.Create(nil); // WithBitmap(nil, 498{user bitmap});
        xfrmWizard.Caption := 'Change User Level';
        xfrmWizard.Add(TfraUserAddNew2.Create(xfrmWizard));
        if (xfrmWizard.ShowModal = mrOK) then
        begin
            try
                xLevel := (xfrmWizard.Items[0] as TfraUserAddNew2).GetUserLevel;

                // Add the new user
                (FDataAdaptor as TCFR21UserDataWrapper).ChangeLevel(FUserPK, aUserName, xLevel);
                result := true;
            except
                on E: Exception do
                    ShowMessage(E.Message);
            end;
        end;
        xfrmWizard.Free;
    end;
end;

function TUser.ResetUserPassword(aUserName: string): boolean;
var
    xFrmChangePw: TfrmUserChangePassword;
begin
    result := false;

    // this can only be done by the supervisor
    if LevelIsIncluded(FLevel, usrSupervisor) then
    begin

        // Get new password (no empty password allowed)
        xFrmChangePw := TfrmUserChangePassword.CreateForm(nil, true, aUserName);
        xFrmChangePw.ShowModal;

        if (xFrmChangePw.ModalResult <> mrCancel) then
        begin
            FDataAdaptor.ChangePassword(FUserPK, aUserName, xFrmChangePw.Edit1.Text);
            result := true;
        end;

        xFrmChangePw.Free;
    end;
end;

function TUser.GetUserLog(aFrom, aTo: TDateTime): TUserLogArray;
begin
    result := nil;

    // this can only be done by the supervisor
    if not LevelIsIncluded(FLevel, usrSupervisor) then
        exit;

    if (FDataAdaptor is TCFR21UserDataWrapper) then
        result := (FDataAdaptor as TCFR21UserDataWrapper).GetUserLog(aFrom, aTo);
end;

function TUser.GetReason(out oReason: string): boolean;
begin
    result := self.GetReasonIntern(oReason, nil, lctBulk);
end;

function TUser.GetReasonIntern(out oReason: string; const aLogInfo: TStringArray;
    aChangeType: TLogChangeType): boolean;
var
    xFrmConfirmation: TfrmUserConfirmation;
begin
    result := true;
    oReason := '';
    if (not self.MustGiveReason) then
        EXIT; // User must give reasons for every change

    xFrmConfirmation := TfrmUserConfirmation.Create(nil);
    try
        xFrmConfirmation.ComboBox1.AddItem(fLastReason, nil);
        xFrmConfirmation.stxUserName.Caption := FName;

        if Assigned(aLogInfo) then
        begin
            TControlUtils.AddValuesToMemo(aLogInfo, xFrmConfirmation.Memo1, true);
        end;

        if (aChangeType = lctRemove) then
            xFrmConfirmation.Label2.Font.Color := clRed;

        if (xFrmConfirmation.ShowModal = mrCancel) then
        begin
            result := false;
            EXIT;
        end;

        oReason := xFrmConfirmation.ComboBox1.Text;
        fLastReason := oReason;
    finally
        xFrmConfirmation.Free;
    end;
end;

class function TUser.GetIntFromLogType(aChangeType: TLogChangeType): integer;
begin
    result := 0;
    case (aChangeType) of
        lctAdd:
            result := INT_CFR21_USERLOG_TYPE_DATA_ADD;
        lctEdit:
            result := INT_CFR21_USERLOG_TYPE_DATA_EDIT;
        lctRemove:
            result := INT_CFR21_USERLOG_TYPE_DATA_REMOVE;
        lctBulk:
            result := INT_CFR21_USERLOG_TYPE_DATA_BULK;
    end;
end;

procedure TUser.LogDataChanged(aTableName, aAction, aReason: string; aChangeType: TLogChangeType);
begin
    FDataAdaptor.Log(FUserPK, INT_CFR21_USERLOG_LOGTYPE_DBCHANGE, GetIntFromLogType(aChangeType), aTableName,
        aAction, aReason);
end;

function TUser.ConfirmDataChanged(const aTableName, aAction: string; aChangeType: TLogChangeType): boolean;
var
    xReason: string;
    xLogInfo: TList<string>;
begin
    xLogInfo := TList<string>.Create;
    try
        xLogInfo.Add(aAction);
        result := self.GetReasonIntern(xReason, xLogInfo.ToArray, aChangeType);

        if (result) then
            self.LogDataChanged(aTableName, aAction, xReason, aChangeType);
    finally
        xLogInfo.Free;
    end;
end;

function TUser.CheckSQLBeforeUpdate(const aSQLText: string): boolean;
begin
    result := true;

    if not(FDataAdaptor is TCFR21UserDataWrapper) then
        EXIT;

    if (Pos(':ZINSSER_APP:', UpperCase(aSQLText)) > 0) then
    begin
        self.LogAlert('User tried to manipulate ZINSSER_APP alias - blocked');
        result := false;
    end;
    if (Pos(':USER', UpperCase(aSQLText)) > 0) then
    begin
        self.LogAlert('User tried to manipulate USER table - blocked');
        result := false;
    end;
    if (Pos(':USERLOG', UpperCase(aSQLText)) > 0) then
    begin
        self.LogAlert('User tried to manipulate USERLOG table - blocked');
        result := false;
    end;
end;

procedure TUser.LogAlert(const aAction: string);
var
    xActionType: integer;
begin
    if not(FDataAdaptor is TCFR21UserDataWrapper) then
        EXIT;

    xActionType := INT_CFR21_USERLOG_TYPE_ALERT_SQLEXEC;

    (FDataAdaptor as TCFR21UserDataWrapper).Log(FUserPK, INT_CFR21_USERLOG_LOGTYPE_ALERT, xActionType,
        '---ALERT---', aAction, '!CRITICAL!');
end;


end.
