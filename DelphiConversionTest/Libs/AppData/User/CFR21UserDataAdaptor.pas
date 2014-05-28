{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.08.07 wl                               TN3811.4 initial revision
  13.09.07 wl                               TN3811.4 komplett überarbeitet
  21.09.07 wl  CreateUpdater                TN3811.4 Alias-Pfad ZINSSER_APP
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Query, Dataset changed to DataProvider
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  20.05.10 wl                               TN5116   AppInstanceDataAdaptorCommon geändert
  17.06.10 pk                               TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  21.07.10 pk                               TN5203   AppInstanceAppDataAdaptorCommon renamed to AppInstanceAppData
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  27.06.10 wl  GetCodeFromPassword          TN5610   Verschlüsselung funktioniert jetzt auch mit unicode
  27.01.12 wl  AddUserNameToUserLogArray    TN5648   ist keine Endlosschleife mehr
  27.03.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit CFR21UserDataAdaptor;


interface


uses
    ComCtrls,
    CommonTypes,
    QueryDataAdaptor;

type
    TCFR21UserDataAdaptor = class(TQueryDataAdaptor)
    private
        // private class methods
        class function GetCodeFromPassword(aPassword: string): string;
        class function GetPasswordFromCode(aCode: string): string;
        class function GetUserLevelFromIntLevel(aIntLevel: integer): TUserLevel;
    protected
        function GetAlias(): string; override;
    public
        constructor Create();

        function CheckUser(aUserName, aPassword: string; out oWrongPassword: boolean): TUserLevel;
        procedure ChangePassword(const aUserPK: integer; aPassword: string);
        procedure CreateNewUser(aUserName, aPassword, aDescription: string; aLevel: TUserLevel);
        procedure DeleteUser(aUserPK: integer);
        procedure ShowAllUsers(aUserListItems: TListItems);
        function FindUser(aUserName: string; aWithoutDeleted: boolean; var aUserPK: integer): boolean;
        function FindUserByPK(aUserPK: integer; var aUserName: string; var aDeleted: boolean): boolean;
        // public methods (not derived from TUserDataAdaptor)
        function FindSupervisor: boolean;
        function ChangeSystemDataPassword(aSupervisorPK: integer; const aAliasNames: TArray<string>): string;
        procedure ChangeLevel(const aUserPK: integer; aUserLevel: TUserLevel);
        procedure ChangeDescription(const aUserPK: integer; aDescription: string);
        class function GetIntLevelFromUserLevel(aUserLevel: TUserLevel): integer;
        procedure AddUserNameToUserLogArray(var aUserLogArray: TUserLogArray);
    end;


implementation


uses
    SysUtils,
    Classes,
    Dialogs,
    GeneralTypes,
    DataAdaptor,
    DataProviderFactory,
    BlockCiphers,
    AppInstanceAppData;

const
    INT_MOST_RECENT_VERSION = 1;

    STR_CFR21_USERS_NAME = 'USERS';

    STR_CFR21_USERS_FLD_USER_PK = 'USER_PK';
    STR_CFR21_USERS_FLD_NAME = 'NAME';
    STR_CFR21_USERS_FLD_CODE = 'CODE';
    STR_CFR21_USERS_FLD_LEVEL = 'USERLEVEL';
    STR_CFR21_USERS_FLD_DESCRIPTION = 'DESCRIPTION';
    STR_CFR21_USERS_FLD_DELETED = 'DELETED';

    INT_CFR21_USERS_FLDLEN_NAME = 40;
    INT_CFR21_USERS_FLDLEN_CODE = 40;
    INT_CFR21_USERS_FLDLEN_DESCRIPTION = 150;

    STR_CFR21_USERS_INDEX_NAME = 'USERNAME';

    INT_CFR21_USERS_LEVEL_NOTHING = 0;
    INT_CFR21_USERS_LEVEL_GUEST = 1;
    INT_CFR21_USERS_LEVEL_SYSTEM = 2;
    INT_CFR21_USERS_LEVEL_SYSTEMADMIN = 3;
    INT_CFR21_USERS_LEVEL_SUPERVISOR = 4;

function GetPWKey: Int64;
begin
    with TDoubleDWORD(result) do
    begin
        R := $01CD4867;
        L := $893B45EF;
    end;
end;

{ TCFR21UserDataAdaptor }

constructor TCFR21UserDataAdaptor.Create;
begin
    inherited Create(STR_CFR21_USERS_NAME);
end;

class function TCFR21UserDataAdaptor.GetCodeFromPassword(aPassword: string): string;
var
    xBlockCipher: TBlowfishCipher;
    xPWKey: Int64;
    xPlainText, xCypherText: TStringStream;
begin
    xPWKey := GetPWKey;
    xBlockCipher := TBlowfishCipher.Create(xPWKey, SizeOf(xPWKey));
    xPlainText := TStringStream.Create(aPassword);
    xCypherText := TStringStream.Create('');
    try
        xBlockCipher.EncryptStream(xPlainText, xCypherText);
        result := xCypherText.DataString;
    finally
        FreeAndNil(xBlockCipher);
        FreeAndNil(xPlainText);
        FreeAndNil(xCypherText);
    end;
end;

class function TCFR21UserDataAdaptor.GetPasswordFromCode(aCode: string): string;
var
    xBlockCipher: TBlowfishCipher;
    xPWKey: Int64;
    xPlainText, xCypherText: TStringStream;
begin
    xPWKey := GetPWKey;
    xBlockCipher := TBlowfishCipher.Create(xPWKey, SizeOf(xPWKey));
    xPlainText := TStringStream.Create('');
    xCypherText := TStringStream.Create(aCode);
    try
        xBlockCipher.DecryptStream(xCypherText, xPlainText);
        result := xPlainText.DataString;
    finally
        FreeAndNil(xBlockCipher);
        FreeAndNil(xPlainText);
        FreeAndNil(xCypherText);
    end;
end;

// function TCFR21UserDataAdaptor.GetDbPath(): string;
// begin
// result := TDataAdaptor.GetUserAliasPath();
// end;

function TCFR21UserDataAdaptor.GetAlias(): string;
begin
    result := TDataProviderFactory.Instance.UserDBAlias;
end;

function TCFR21UserDataAdaptor.FindUser(aUserName: string; aWithoutDeleted: boolean;
    var aUserPK: integer): boolean;
var
    xSQLString: string;
begin
    aUserPK := 0;
    result := false;
    xSQLString := 'SELECT * FROM ' + STR_CFR21_USERS_NAME + ' WHERE (UPPER(' + STR_CFR21_USERS_FLD_NAME +
        ') =''' + UpperCase(aUserName) + ''')';
    if (aWithoutDeleted) then
        xSQLString := xSQLString + ' AND (' + STR_CFR21_USERS_FLD_DELETED + '=FALSE)';

    self.SelectAndOpen(xSQLString, true);
    try
        if not self.DataProvider.Eof then
        begin
            aUserPK := self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_USER_PK).AsInteger;
            result := true;
        end;
    finally
        self.Close;
    end;
end;

function TCFR21UserDataAdaptor.FindUserByPK(aUserPK: integer; var aUserName: string;
    var aDeleted: boolean): boolean;
var
    xSQLString: string;
begin
    aUserName := '';
    result := false;
    xSQLString := 'SELECT * FROM ' + STR_CFR21_USERS_NAME + ' WHERE (' + STR_CFR21_USERS_FLD_USER_PK + '=' +
        IntToStr(aUserPK) + ')';

    self.SelectAndOpen(xSQLString, true);
    try
        if not self.DataProvider.Eof then
        begin
            aUserName := self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_NAME).AsString;
            aDeleted := self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_DELETED).AsBoolean;
            result := true;
        end;
    finally
        self.Close;
    end;
end;

function TCFR21UserDataAdaptor.FindSupervisor: boolean;
var
    xSQLString: string;
begin
    result := false;
    xSQLString := 'SELECT * FROM ' + STR_CFR21_USERS_NAME + ' WHERE (' + STR_CFR21_USERS_FLD_LEVEL + '=' +
        IntToStr(GetIntLevelFromUserLevel(usrSupervisor)) + ')';

    self.SelectAndOpen(xSQLString, true);
    try
        if not self.DataProvider.Eof then
        begin
            result := true;
        end;
    finally
        self.Close;
    end;
end;

class function TCFR21UserDataAdaptor.GetUserLevelFromIntLevel(aIntLevel: integer): TUserLevel;
begin
    case (aIntLevel) of
        INT_CFR21_USERS_LEVEL_GUEST:
            result := usrGuest;
        INT_CFR21_USERS_LEVEL_SYSTEM:
            result := usrSystem;
        INT_CFR21_USERS_LEVEL_SYSTEMADMIN:
            result := usrSystemAdmin;
        INT_CFR21_USERS_LEVEL_SUPERVISOR:
            result := usrSupervisor;
        else
            result := usrNothing;
    end;
end;

class function TCFR21UserDataAdaptor.GetIntLevelFromUserLevel(aUserLevel: TUserLevel): integer;
begin
    case (aUserLevel) of
        usrGuest:
            result := INT_CFR21_USERS_LEVEL_GUEST;
        usrSystem:
            result := INT_CFR21_USERS_LEVEL_SYSTEM;
        usrSystemAdmin:
            result := INT_CFR21_USERS_LEVEL_SYSTEMADMIN;
        usrSupervisor:
            result := INT_CFR21_USERS_LEVEL_SUPERVISOR;
        else
            result := INT_CFR21_USERS_LEVEL_NOTHING;
    end;
end;

function TCFR21UserDataAdaptor.CheckUser(aUserName, aPassword: string; out oWrongPassword: boolean)
    : TUserLevel;
var
    xCode: string;
    xSQLString: string;
begin
    result := usrNothing;
    oWrongPassword := false;
    xSQLString := 'SELECT * FROM ' + STR_CFR21_USERS_NAME + ' WHERE (UPPER(' + STR_CFR21_USERS_FLD_NAME +
        ') =''' + UpperCase(aUserName) + ''')';

    self.SelectAndOpen(xSQLString, true);
    try
        if self.DataProvider.Eof then
            EXIT;

        xCode := self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_CODE).AsString;
        if (GetPasswordFromCode(xCode) = aPassword) then
        begin
            // Get user level
            result := GetUserLevelFromIntLevel(self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_LEVEL)
                .AsInteger);
        end
        else
        begin
            oWrongPassword := true;
        end;
    finally
        self.Close;
    end;
end;

procedure TCFR21UserDataAdaptor.ChangePassword(const aUserPK: integer; aPassword: string);
var
    xSQLString: string;
begin
    xSQLString := 'UPDATE ' + STR_CFR21_USERS_NAME + ' SET ' + STR_CFR21_USERS_FLD_CODE + '=''' +
        GetCodeFromPassword(aPassword) + '''' + ' WHERE ' + STR_CFR21_USERS_FLD_USER_PK + '=' +
        IntToStr(aUserPK);

    self.ExecSQL(xSQLString);
end;

procedure TCFR21UserDataAdaptor.ChangeDescription(const aUserPK: integer; aDescription: string);
var
    xSQLString: string;
begin
    xSQLString := 'UPDATE ' + STR_CFR21_USERS_NAME + ' SET ' + STR_CFR21_USERS_FLD_DESCRIPTION + '=''' +
        aDescription + '''' + ' WHERE ' + STR_CFR21_USERS_FLD_USER_PK + '=' + IntToStr(aUserPK);

    self.ExecSQL(xSQLString);
end;

procedure TCFR21UserDataAdaptor.ChangeLevel(const aUserPK: integer; aUserLevel: TUserLevel);
var
    xSQLString: string;
begin
    xSQLString := 'UPDATE ' + STR_CFR21_USERS_NAME + ' SET ' + STR_CFR21_USERS_FLD_LEVEL + '=' +
        IntToStr(GetIntLevelFromUserLevel(aUserLevel)) + ' WHERE ' + STR_CFR21_USERS_FLD_USER_PK + '=' +
        IntToStr(aUserPK);

    self.ExecSQL(xSQLString);
end;

procedure TCFR21UserDataAdaptor.CreateNewUser(aUserName, aPassword, aDescription: string; aLevel: TUserLevel);
var
    xUserPK: integer;
begin
    if FindUser(aUserName, false, xUserPK) then
        raise Exception.Create(TLanguageString.Read('User {0} already exists (or did exist)!',
            'Der Benutzer {0} existiert (oder existierte) bereits!', [aUserName]));

    self.SelectAndOpenAll(false);
    try
        self.DataProvider.Append;
        self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_NAME).AsString := aUserName;
        self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_CODE).AsString :=
            self.GetCodeFromPassword(aPassword);
        self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_LEVEL).AsInteger :=
            self.GetIntLevelFromUserLevel(aLevel);
        self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_DESCRIPTION).AsString := aDescription;
        self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_DELETED).AsBoolean := false;
        self.DataProvider.Post;
    finally
        self.Close;
    end;
end;

procedure TCFR21UserDataAdaptor.DeleteUser(aUserPK: integer);
var
    xSQLString: string;
begin
    xSQLString := 'UPDATE ' + STR_CFR21_USERS_NAME + ' SET ' + STR_CFR21_USERS_FLD_DELETED + '=TRUE' +
        ' WHERE ' + STR_CFR21_USERS_FLD_USER_PK + '=' + IntToStr(aUserPK);
    self.ExecSQL(xSQLString);
end;

procedure TCFR21UserDataAdaptor.ShowAllUsers(aUserListItems: TListItems);
var
    xListItem: TListItem;
begin
    self.SelectAndOpenAll(true);
    try
        while (not self.DataProvider.EOF) do
        begin
            if (not self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_DELETED).AsBoolean) then
            begin
                xListItem := aUserListItems.Add;
                xListItem.Caption := self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_NAME).AsString;
                xListItem.SubItems.Add
                    (TDataAdaptor.UserGetLevelName(GetUserLevelFromIntLevel(self.DataProvider.FieldByName
                    (STR_CFR21_USERS_FLD_LEVEL).AsInteger)));
                xListItem.SubItems.Add(self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_DESCRIPTION)
                    .AsString);
            end;
            self.DataProvider.Next;
        end;
    finally
        self.Close;
    end;
end;

function TCFR21UserDataAdaptor.ChangeSystemDataPassword(aSupervisorPK: integer;
    const aAliasNames: TArray<string>): string;
begin
    result := TDataProviderFactory.Instance.ChangeSystemDataPassword(aAliasNames);
end;

procedure TCFR21UserDataAdaptor.AddUserNameToUserLogArray(var aUserLogArray: TUserLogArray);
var
    x: integer;
begin
    self.SelectAndOpenAll(true);
    try
        while (not self.DataProvider.EOF) do
        begin
            for x := low(aUserLogArray) to high(aUserLogArray) do
            begin

                // Namen der User werden hinzugefügt
                if (aUserLogArray[x].UserPK = self.DataProvider.FieldByName(STR_CFR21_USERS_FLD_USER_PK)
                    .AsInteger) then
                    aUserLogArray[x].UserName := self.DataProvider.FieldByName
                        (STR_CFR21_USERS_FLD_NAME).AsString;
            end;
            self.DataProvider.Next;
        end;
    finally
        self.Close;
    end;
end;


end.
