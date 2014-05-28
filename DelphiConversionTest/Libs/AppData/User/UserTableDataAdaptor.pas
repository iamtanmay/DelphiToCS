{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : User.db data adaptor for simple user management
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  31.08.07 wl                               TN3811.4 initial revision
  13.09.07 wl  CreateNewUser                TN3811.4 benutzt SelectAllAndOpen
  19.09.07 wl  CheckUser                    TN3811.4 Fehler behoben
  20.09.07 wl  UpdateVersion1               TN3811.4 Fehler behoben
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  17.06.10 pk                               TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  -------------------------------------------------------------------------------------------------- }

unit UserTableDataAdaptor;


interface


uses
    ComCtrls,
    Classes,
    CommonTypes,
    QueryDataAdaptor;

type
    TUserDataAdaptor = class(TQueryDataAdaptor)
    private
        function FindUser(aUserName: string; var aUserPK: integer): boolean;
    public
        constructor Create();
        // public methods (derived from TUserDataAdaptor)
        function CheckUser(aUserName, aPassword: string): TUserLevel;
        function CheckUserNoPw(var aUserName: string): TUserLevel;
        procedure ChangePassword(aSupervisorPK: integer; aUserName, aPassword: string);
        function UserLogin(aUserName, aLevelName: string): integer;
        procedure UserLogoff(aUserName: string);
        procedure CreateNewUser(aSupervisorPK: integer; aUserName, aPassword, aDescription: string;
            aLevel: TUserLevel);
        procedure DeleteUser(aSupervisorPK: integer; aUserName: string);
        procedure ShowAllUsers(aUserListItems: TListItems);

        // update management
    end;

    // ##################################################################################################


implementation


uses
    SysUtils,
    Dialogs,
    GeneralTypes,
    DataAdaptor;

const
    STR_TIPSET_TBL = 'TIPSET';
    STR_TIPSET_FLD_LAYOUT = 'Layout';
    STR_TIPSET_FLD_TIP = 'Tip';
    STR_TIPSET_FLD_TIPTYPE = 'TipType';

    INT_TIPSET_FLDLEN_LAYOUT = 20;
    INT_TIPSET_FLDLEN_TIPTYPE = 20;

    STR_TIPSET_INDEX_FIELDS = STR_TIPSET_FLD_LAYOUT + ';' + STR_TIPSET_FLD_TIP;

    STR_OLD_USERS_NAME = 'USERS';

    STR_OLD_USERS_FLD_USER_PK = 'USER_PK';
    STR_OLD_USERS_FLD_NAME = 'NAME';
    STR_OLD_USERS_FLD_PW = 'PW';
    STR_OLD_USERS_FLD_ADMIN = 'ADMIN';

    INT_OLD_USERS_FLDLEN_NAME = 30;
    INT_OLD_USERS_FLDLEN_PW = 30;

    { TUserDataAdaptor }

constructor TUserDataAdaptor.Create;
begin
    inherited Create(STR_OLD_USERS_NAME);
end;

function TUserDataAdaptor.CheckUser(aUserName, aPassword: string): TUserLevel;
begin
    result := usrNothing;

    self.SelectAndOpenFmt('SELECT * FROM %s WHERE %s = ''%s''', [STR_OLD_USERS_NAME, STR_OLD_USERS_FLD_NAME,
        aUserName], true);
    try
        if self.DataProvider.Eof then
            EXIT;
        if (self.DataProvider.FieldByName(STR_OLD_USERS_FLD_PW).AsString <> aPassword) then
            EXIT;

        // Get user level
        result := usrSystem;

        if (self.DataProvider.FieldByName(STR_OLD_USERS_FLD_ADMIN).AsBoolean) then
            result := usrUnlimitedAdmin;

    finally
        self.Close;
    end;
end;

function TUserDataAdaptor.CheckUserNoPw(var aUserName: string): TUserLevel;
begin
    if (aUserName = '') then
        aUserName := 'Administrator';
    result := CheckUser(aUserName, '');
end;

procedure TUserDataAdaptor.ChangePassword(aSupervisorPK: integer; aUserName, aPassword: string);
begin
    self.SelectAndOpenFmt('SELECT * FROM %s WHERE UPPER(%s) = ''%s''',
        [STR_OLD_USERS_NAME, STR_OLD_USERS_FLD_NAME, Uppercase(aUserName)], false);
    try
        self.DataProvider.Edit;
        self.DataProvider.FieldByName(STR_OLD_USERS_FLD_PW).AsString := aPassword;
        self.DataProvider.Post;
    finally
        self.Close;
    end;
end;

function TUserDataAdaptor.FindUser(aUserName: string; var aUserPK: integer): boolean;
begin
    aUserPK := 0;
    result := false;

    self.SelectAndOpenFmt('SELECT * FROM %s WHERE %s = ''%s''', [STR_OLD_USERS_NAME, STR_OLD_USERS_FLD_NAME,
        aUserName], true);
    try
        if self.DataProvider.Eof then
            EXIT;

        aUserPK := self.DataProvider.FieldByName(STR_OLD_USERS_FLD_USER_PK).AsInteger;
        result := true;
    finally
        self.Close;
    end;
end;

function TUserDataAdaptor.UserLogin(aUserName, aLevelName: string): integer;
begin
    if not FindUser(aUserName, result) then
        raise Exception.Create('User ' + aUserName + ' not found in Database');
end;

procedure TUserDataAdaptor.UserLogoff(aUserName: string);
begin
    // Dummy
end;

procedure TUserDataAdaptor.CreateNewUser(aSupervisorPK: integer; aUserName, aPassword, aDescription: string;
    aLevel: TUserLevel);
var
    xUserPK: integer;
begin
    if FindUser(aUserName, xUserPK) then
        raise Exception.Create(TLanguageString.Read('User {0} already exists (or did exist)!',
            'Der Benutzer {0} existiert (oder existierte) bereits!', [aUserName]));

    self.SelectAndOpenAll(false);
    try
        self.DataProvider.Append;
        self.DataProvider.FieldByName(STR_OLD_USERS_FLD_NAME).AsString := aUserName;
        self.DataProvider.FieldByName(STR_OLD_USERS_FLD_PW).AsString := aPassword;
        if (aLevel = usrSystemAdmin) or (aLevel = usrSupervisor) or (aLevel = usrUnlimitedAdmin) then
            self.DataProvider.FieldByName(STR_OLD_USERS_FLD_ADMIN).AsBoolean := true
        else
            self.DataProvider.FieldByName(STR_OLD_USERS_FLD_ADMIN).AsBoolean := false;
        self.DataProvider.Post;
    finally
        self.Close;
    end;
end;

procedure TUserDataAdaptor.DeleteUser(aSupervisorPK: integer; aUserName: string);
var
    xUserPK: integer;
begin
    // user must exist!
    if not FindUser(aUserName, xUserPK) then
        raise Exception.Create(TLanguageString.Read('User {0} does not exist!',
            'Der Benutzer {0} existiert nicht!', [aUserName]));

    self.SelectAndOpenFmt('SELECT * FROM %s WHERE UPPER(%s) = ''%s''',
        [STR_OLD_USERS_NAME, STR_OLD_USERS_FLD_NAME, Uppercase(aUserName)], false);
    try
        if (self.DataProvider.FieldByName(STR_OLD_USERS_FLD_USER_PK).AsInteger = aSupervisorPK) then
            raise Exception.Create(TLanguageString.Read('Supervisor can not delete himself!',
                'Supervisor kann sich nicht selbst löschen!'));

        self.DataProvider.Delete;
    finally
        self.Close;
    end;
end;

procedure TUserDataAdaptor.ShowAllUsers(aUserListItems: TListItems);
var
    xListItem: TListItem;
begin
    self.SelectAndOpenFmt('SELECT * FROM %s', [STR_OLD_USERS_NAME], true);
    try
        while (not self.DataProvider.Eof) do
        begin
            xListItem := aUserListItems.Add;
            xListItem.Caption := self.DataProvider.FieldByName(STR_OLD_USERS_FLD_NAME).AsString;
            if (self.DataProvider.FieldByName(STR_OLD_USERS_FLD_ADMIN).AsBoolean) then
                xListItem.SubItems.Add(TDataAdaptor.UserGetLevelName(usrUnlimitedAdmin))
            else
                xListItem.SubItems.Add(TDataAdaptor.UserGetLevelName(usrSystem));
            xListItem.SubItems.Add('');

            self.DataProvider.Next;
        end;
    finally
        self.Close;
    end;
end;


end.
