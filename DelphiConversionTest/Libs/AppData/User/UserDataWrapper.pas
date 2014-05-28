{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : Data Adaptor for the user management:
  TOldUserDataAdaptor: compatible with USER.DLL-user management
  TCFR21UserDataAdaptor: new CFR 21 Part 11 compliant user management
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.01.03 wl                               TN1334.1 initial version
  09.01.03 wl                               TN1334.1 komplett überarbeitet
  10.01.03 wl  CheckUserNoPw                TN1334.1 TOldUserDataAdaptor: kein Name -> wird zu 'Administrator'
  21.01.03 wl  LogArchiving                 TN1332.4 neue Funktion, benutzt gmCalcFileCRC32
  06.02.03 wl  LogArchiving                 TN1332.4 --> User (Unterschiedung interne/externe Logs)
  06.02.03 wl  Log                          TN1332.4 jetzt public (für externe logs)
  11.02.03 wl  TOldUserDataAdaptor.CheckUser   TN1334.3 Admin ist jetzt usrUnlimitedAdmin
  19.02.03 wl  TCFR21UserDataAdaptor.Log    TN1334.3 UserLog erweitert
  04.03.03 wl  GetPasswordFromCode,GetCodeFromPassword  TN1334.1 now with real encrypting (Blowfish)
  11.03.03 wl  TCFR21UserDataAdaptor.DeleteUser         TN1334.7 User werden nur noch mit einem Flag gelöscht
  11.03.03 wl  TCFR21UserDataAdaptor.DeleteUser         TN1334.7 Supervisor kann sich nicht selbst löschen
  21.03.03 wl  Get/SetSystemDataPassword    TN1439.4 das Password für alle System Tabellen wird hier gespeichert
  21.03.03 wl  IsCompliant                  TN1439.4 Wenn System password gesetzt -> Compliant
  25.03.03 wl  SetSystemDataPassword        TN1439.4 Loggt die Aktion
  26.03.03 wl  TOldUserDataAdaptor          TN1334.1 Add, Delete, ShowAllUsers aktiviert für User Manager
  27.03.03 wl  TCFR21UserDataAdaptor        TN1334.1 Umstellung auf SQL bis auf CreateNewUser & Log weil es nicht funktioniert
  14.04.03 wl  LogArchiveAndChecksum        TN1332.6 Checksum-Mechanismus steht nur in Compliant Mode zur Verfügung
  14.04.03 wl  GetArchiveFileInfo           TN1332.6 neue Methode zum Prüfen der ZipArchive (für Retrieve-Applikation)
  16.04.03 wl  Password                     TN1439.4 von ZACommonMain hierher
  16.04.03 wl  ChangeSystemDataPassword     TN1439.4 Password kann nicht mehr frei gewählt werden
  16.04.03 wl                               TN1334.1 eigenes Logging entfernt (UserSQL.dat)
  28.04.03 wl  ChangeDescription            TN1473.1 neu: Description ändern
  28.04.03 wl  ChangeLevel                  TN1473.1 neu: User Level ändern
  28.04.03 wl  ChangePassword               TN1473.1 Supervisor-PK wird mit übergeben
  21.04.04 wl                               TN1869   INT_CFR21_USERLOG-Konstanten --> CommonTypes
  21.04.04 wl  GetUserLog                   TN1869   neu: Supervisor kann UserLog für einen Zeitraum einsehen
  07.10.04 mo                               TN2000.4 CFR21 Tabellenpasswort geändert
  20.10.04 wl  TCFR21UserDataAdaptor.FindUser/CheckUser  TN2185.4  Username nicht mehr case-sensitiv (für CFR21)
  20.10.04 wl  TOldUserDataAdaptor          TN2185.4  Username nicht mehr case-sensitiv
  16.11.05 thr TOldUserDataAdaptor.deleteuserTN2769  Tabelle muss offen sein vor "findkey"
  UserDataWrapper:
  31.08.07 wl                               TN3811.4 Inhalt --> UserTable/CFR21UserDataAdaptor
  13.09.07 wl  TCFR21UserDataAdaptor        TN3811.4 benutzt 2 DataAdaptoren (für USERS und USERLOG)
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  27.03.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit UserDataWrapper;


interface


uses
    Generics.Collections,
    ComCtrls,
    CommonTypes,
    UserTableDataAdaptor,
    CFR21UserlogDataAdaptor,
    CFR21UserDataAdaptor;

type
    TUserDataWrapper = class
    public
        // public methods
        function CheckUser(aUserName, aPassword: string): TUserLevel; virtual; abstract;
        function CheckUserNoPw(var aUserName: string): TUserLevel; virtual; abstract;
        procedure ChangePassword(aSupervisorPK: integer; aUserName, aPassword: string); virtual; abstract;
        function UserLogin(aUserName, aLevelName: string): integer; virtual; abstract;
        procedure UserLogoff(aUserName: string); virtual; abstract;
        procedure CreateNewUser(aSupervisorPK: integer; aUserName, aPassword, aDescription: string;
            aLevel: TUserLevel); virtual; abstract;
        procedure DeleteUser(aSupervisorPK: integer; aUserName: string); virtual; abstract;
        procedure ShowAllUsers(aUserListItems: TListItems); virtual; abstract;
        procedure Log(aUserKey, aLogtype, aActionType: integer; aTitle, aAction, aReason: string);
            virtual; abstract;
        function LogArchiveAndChecksum(var aFileName: string; aUserKey, aActionType: integer): boolean;
            virtual; abstract;
    end;

    TOldUserDataWrapper = class(TUserDataWrapper)
    private
        FDataAdaptor: TUserDataAdaptor;
    public
        // constructor / destructor
        constructor Create();
        destructor Destroy; override;
        // public methods (derived from TUserDataAdaptor)
        function CheckUser(aUserName, aPassword: string): TUserLevel; override;
        function CheckUserNoPw(var aUserName: string): TUserLevel; override;
        procedure ChangePassword(aSupervisorPK: integer; aUserName, aPassword: string); override;
        function UserLogin(aUserName, aLevelName: string): integer; override;
        procedure UserLogoff(aUserName: string); override;
        procedure CreateNewUser(aSupervisorPK: integer; aUserName, aPassword, aDescription: string;
            aLevel: TUserLevel); override;
        procedure DeleteUser(aSupervisorPK: integer; aUserName: string); override;
        procedure ShowAllUsers(aUserListItems: TListItems); override;
        procedure Log(aUserKey, aLogtype, aActionType: integer; aTitle, aAction, aReason: string); override;
        function LogArchiveAndChecksum(var aFileName: string; aUserKey, aActionType: integer)
            : boolean; override;
    end;

    TCFR21UserDataWrapper = class(TUserDataWrapper)
    private
        fUserDA: TCFR21UserDataAdaptor;
        fLogDA: TCFR21UserlogDataAdaptor;
    public
        // constructor / destructor
        constructor Create;
        destructor Destroy; override;
        // public methods (derived from TUserDataAdaptor)
        function CheckUser(aUserName, aPassword: string): TUserLevel; override;
        function CheckUserNoPw(var aUserName: string): TUserLevel; override;
        procedure ChangePassword(aSupervisorPK: integer; aUserName, aPassword: string); override;
        function UserLogin(aUserName, aLevelName: string): integer; override;
        procedure UserLogoff(aUserName: string); override;
        procedure CreateNewUser(aSupervisorPK: integer; aUserName, aPassword, aDescription: string;
            aLevel: TUserLevel); override;
        procedure DeleteUser(aSupervisorPK: integer; aUserName: string); override;
        procedure ShowAllUsers(aUserListItems: TListItems); override;
        procedure Log(aUserKey, aLogtype, aActionType: integer; aTitle, aAction, aReason: string); override;
        function LogArchiveAndChecksum(var aFileName: string; aUserKey, aActionType: integer)
            : boolean; override;
        // public methods (not derived from TUserDataAdaptor)
        function FindSupervisor: boolean;
        function ChangeSystemDataPassword(aSupervisorPK: integer; const aAliasNames: TArray<string>): string;
        function GetArchiveFileInfo(const aFileName: string; const aInfo: TList<string>): boolean;
        procedure ChangeLevel(aSupervisorPK: integer; aUserName: string; aUserLevel: TUserLevel);
        procedure ChangeDescription(aSupervisorPK: integer; aUserName, aDescription: string);
        function GetUserLog(aFrom, aTo: TDateTime): TUserLogArray;
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    DataAdaptor;

{ TOldUserDataWrapper }

procedure TOldUserDataWrapper.ChangePassword(aSupervisorPK: integer; aUserName, aPassword: string);
begin
    FDataAdaptor.ChangePassword(aSupervisorPK, aUserName, aPassword);
end;

function TOldUserDataWrapper.CheckUser(aUserName, aPassword: string): TUserLevel;
begin
    result := FDataAdaptor.CheckUser(aUserName, aPassword);
end;

function TOldUserDataWrapper.CheckUserNoPw(var aUserName: string): TUserLevel;
begin
    result := FDataAdaptor.CheckUserNoPw(aUserName);
end;

constructor TOldUserDataWrapper.Create();
begin
    inherited Create();

    FDataAdaptor := TUserDataAdaptor.Create();
end;

procedure TOldUserDataWrapper.CreateNewUser(aSupervisorPK: integer;
    aUserName, aPassword, aDescription: string; aLevel: TUserLevel);
begin
    FDataAdaptor.CreateNewUser(aSupervisorPK, aUserName, aPassword, aDescription, aLevel)
end;

procedure TOldUserDataWrapper.DeleteUser(aSupervisorPK: integer; aUserName: string);
begin
    fDataAdaptor.DeleteUser(aSupervisorPK, aUserName);
end;

destructor TOldUserDataWrapper.Destroy;
begin
    FreeAndNil(FDataAdaptor);

    inherited;
end;

procedure TOldUserDataWrapper.Log(aUserKey, aLogtype, aActionType: integer; aTitle, aAction, aReason: string);
begin
    // Dummy
end;

function TOldUserDataWrapper.LogArchiveAndChecksum(var aFileName: string;
    aUserKey, aActionType: integer): boolean;
begin
    result := true; // do nothing
end;

procedure TOldUserDataWrapper.ShowAllUsers(aUserListItems: TListItems);
begin
    fDataAdaptor.ShowAllUsers(aUserListItems);
end;

function TOldUserDataWrapper.UserLogin(aUserName, aLevelName: string): integer;
begin
    result := fDataAdaptor.UserLogin(aUserName, aLevelName);
end;

procedure TOldUserDataWrapper.UserLogoff(aUserName: string);
begin
    fDataAdaptor.UserLogoff(aUserName);
end;

{ TCFR21UserDataWrapper }

procedure TCFR21UserDataWrapper.ChangeDescription(aSupervisorPK: integer; aUserName, aDescription: string);
var
    xUserPK: integer;
begin
    if not fUserDA.FindUser(aUserName, true, xUserPK) then
        raise Exception.Create(TLanguageString.Read('User {0} does not exist!',
            'Der Benutzer {0} existiert nicht!', [aUserName]));

    fUserDA.ChangeDescription(xUserPK, aDescription);
end;

procedure TCFR21UserDataWrapper.ChangeLevel(aSupervisorPK: integer; aUserName: string;
    aUserLevel: TUserLevel);
var
    xUserPK: integer;
begin
    if not fUserDA.FindUser(aUserName, true, xUserPK) then
        raise Exception.Create(TLanguageString.Read('User {0} does not exist!',
            'Der Benutzer {0} existiert nicht!', [aUserName]));

    fUserDA.ChangeLevel(xUserPK, aUserLevel);
    fLogDA.InternLog(aSupervisorPK, INT_CFR21_USERLOG_TYPE_PWCHANGED, 'Level of user ' + aUserName +
        ' changed to ' + IntToStr(TCFR21UserDataAdaptor.GetIntLevelFromUserLevel(aUserLevel)), '');
end;

procedure TCFR21UserDataWrapper.ChangePassword(aSupervisorPK: integer; aUserName, aPassword: string);
var
    xUserPK: integer;
begin
    if not fUserDA.FindUser(aUserName, true, xUserPK) then
        raise Exception.Create(TLanguageString.Read('User {0} does not exist!',
            'Der Benutzer {0} existiert nicht!', [aUserName]));

    fUserDA.ChangePassword(xUserPK, aPassword);
    if (aSupervisorPK = 0) then
        fLogDA.InternLog(xUserPK, INT_CFR21_USERLOG_TYPE_PWCHANGED,
            'User ' + aUserName + ' changed his password.', '')
    else
        fLogDA.InternLog(aSupervisorPK, INT_CFR21_USERLOG_TYPE_PWCHANGED, 'Password of user ' + aUserName +
            ' changed by supervisor.', '');
end;

function TCFR21UserDataWrapper.ChangeSystemDataPassword(aSupervisorPK: integer;
    const aAliasNames: TArray<string>): string;
begin
    result := fUserDA.ChangeSystemDataPassword(aSupervisorPK, aAliasNames);
    if result = '' then
        EXIT;

    fLogDA.InternLog(aSupervisorPK, INT_CFR21_USERLOG_TYPE_DATAPWSET,
        'System password set - CFR21 Part 11 Compliant mode started!', '')
end;

function TCFR21UserDataWrapper.CheckUser(aUserName, aPassword: string): TUserLevel;
var
    xWrongPassword: boolean;
begin
    result := fUserDA.CheckUser(aUserName, aPassword, xWrongPassword);
    if (xWrongPassword) then
        fLogDA.InternLog(0, INT_CFR21_USERLOG_TYPE_WRONGPW, 'Wrong password at logon: User ' + aUserName +
            ', ' + aPassword, '');
end;

function TCFR21UserDataWrapper.CheckUserNoPw(var aUserName: string): TUserLevel;
begin
    result := usrNothing; // kein Password gibt es nicht!
end;

constructor TCFR21UserDataWrapper.Create;
begin
    inherited Create();

    fUserDA := TCFR21UserDataAdaptor.Create();
    fLogDA := TCFR21UserlogDataAdaptor.Create();
end;

procedure TCFR21UserDataWrapper.CreateNewUser(aSupervisorPK: integer;
    aUserName, aPassword, aDescription: string; aLevel: TUserLevel);
begin
    fUserDA.CreateNewUser(aUserName, aPassword, aDescription, aLevel);

    fLogDA.InternLog(aSupervisorPK, INT_CFR21_USERLOG_TYPE_NEWUSER,
        'New User ' + aUserName + ' created. Level: ' + TDataAdaptor.UserGetLevelName(aLevel), '');
end;

procedure TCFR21UserDataWrapper.DeleteUser(aSupervisorPK: integer; aUserName: string);
var
    xUserPK: integer;
begin
    // user must exist!
    if not fUserDA.FindUser(aUserName, true, xUserPK) then
        raise Exception.Create(TLanguageString.Read('User {0} does not exist!',
            'Der Benutzer {0} existiert nicht!', [aUserName]));

    // supervisor can not delete himself!
    if (xUserPK = aSupervisorPK) then
        raise Exception.Create(TLanguageString.Read('Supervisor can not delete himself!',
            'Supervisor kann sich nicht selbst löschen!'));

    fUserDA.DeleteUser(xUserPK);
    fLogDA.InternLog(aSupervisorPK, INT_CFR21_USERLOG_TYPE_DELUSER, 'User ' + aUserName + ' deleted.', '');
end;

destructor TCFR21UserDataWrapper.Destroy;
begin
    FreeAndNil(fUserDA);
    FreeAndNil(fLogDA);

    inherited;
end;

function TCFR21UserDataWrapper.FindSupervisor: boolean;
begin
    result := fUserDA.FindSupervisor;
end;

function TCFR21UserDataWrapper.GetArchiveFileInfo(const aFileName: string;
    const aInfo: TList<string>): boolean;
var
    xUserPK: integer;
    xTimeStr, xUserName: string;
    xDeleted: boolean;
begin
    result := fLogDA.GetArchiveFileInfo(aFileName, aInfo, xTimeStr, xUserPK);
    if (result) then
    begin
        fUserDA.FindUserByPK(xUserPK, xUserName, xDeleted);
        aInfo.Add('File was archived on ' + xTimeStr + ' by user ' + xUserName);
    end;
end;

function TCFR21UserDataWrapper.GetUserLog(aFrom, aTo: TDateTime): TUserLogArray;
begin
    result := fLogDA.GetUserLog(aFrom, aTo);

    // erst hinterher werden UserNames hinzugefügt (bessere Performance)
    fUserDA.AddUserNameToUserLogArray(result);
end;

procedure TCFR21UserDataWrapper.Log(aUserKey, aLogtype, aActionType: integer;
    aTitle, aAction, aReason: string);
begin
    fLogDA.DoLog(aUserKey, aLogtype, aActionType, aTitle, aAction, aReason);
end;

function TCFR21UserDataWrapper.LogArchiveAndChecksum(var aFileName: string;
    aUserKey, aActionType: integer): boolean;
begin
    result := fLogDA.LogArchiveAndChecksum(aFileName, aUserKey, aActionType);
end;

procedure TCFR21UserDataWrapper.ShowAllUsers(aUserListItems: TListItems);
begin
    fUserDA.ShowAllUsers(aUserListItems);
end;

function TCFR21UserDataWrapper.UserLogin(aUserName, aLevelName: string): integer;
begin
    if fUserDA.FindUser(aUserName, true, result) then
    begin
        fLogDA.InternLog(result, INT_CFR21_USERLOG_TYPE_LOGON, 'User ' + aUserName + ' logged on as ' +
            aLevelName, '');
    end
    else
        raise Exception.Create('User ' + aUserName + ' not found in Database');
end;

procedure TCFR21UserDataWrapper.UserLogoff(aUserName: string);
var
    xUserPK: integer;
begin
    if fUserDA.FindUser(aUserName, true, xUserPK) then
    begin
        fLogDA.InternLog(xUserPK, INT_CFR21_USERLOG_TYPE_LOGOFF, 'User ' + aUserName + ' logged off.', '');
    end
    else
        raise Exception.Create('User ' + aUserName + ' not found in Database');
end;


end.
