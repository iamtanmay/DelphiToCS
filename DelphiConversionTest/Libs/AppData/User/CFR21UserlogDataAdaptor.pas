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
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  27.01.12 wl  GetUserLog                   TN5648   SQL geändert
  27.03.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit CFR21UserlogDataAdaptor;


interface


uses
    Generics.Collections,
    CommonTypes,
    QueryDataAdaptor;

type
    TCFR21UserlogDataAdaptor = class(TQueryDataAdaptor)
    private
        function CheckFile(aFileName: string): boolean;
    protected
        function GetAlias(): string; override;
    public
        constructor Create();

        procedure InternLog(aUserKey, aActionType: integer; aAction, aReason: string);
        procedure DoLog(aUserKey, aLogtype, aActionType: integer; aTitle, aAction, aReason: string);
        function GetUserLog(aFrom, aTo: TDateTime): TUserLogArray;
        function GetArchiveFileInfo(aFileName: string; const aInfo: TList<string>; out oTimeStr: string;
            out oUserPK: integer): boolean;
        function LogArchiveAndChecksum(var aFileName: string; aUserKey, aActionType: integer): boolean;
    end;


implementation


uses
    Windows,
    SysUtils,
    Classes,
    CRC32,
    DataAdaptor,
    DataProviderFactory;

const
    STR_CFR21_USERLOG_NAME = 'USERLOG';

    STR_CFR21_USERLOG_FLD_USERLOG_PK = 'USRLOG_PK';
    STR_CFR21_USERLOG_FLD_USER_FK = 'USER_FK';
    STR_CFR21_USERLOG_FLD_LOGTYPE = 'LOGTYPE';
    STR_CFR21_USERLOG_FLD_ACTIONTYPE = 'ACTIONTYPE';
    STR_CFR21_USERLOG_FLD_LOGTIME = 'LOGTIME';
    STR_CFR21_USERLOG_FLD_TITLE = 'TITLE';
    STR_CFR21_USERLOG_FLD_ACTION = 'ACTION';
    STR_CFR21_USERLOG_FLD_REASON = 'REASON';

    INT_CFR21_USERLOG_FLDLEN_TITLE = 30;
    INT_CFR21_USERLOG_FLDLEN_ACTION = 80;
    INT_CFR21_USERLOG_FLDLEN_REASON = 80;

    STR_CFR21_USERLOG_TITLE_INTERN = 'User Management';

    // --------------------------------------------------------------------------------------------------
function gmCalcFileCRC32(aFileName: string; var aSize: Int64; var aCRCValue: longword): boolean;
// --------------------------------------------------------------------------------------------------
// The CRC-32 value calculated here matches the one from the PKZIP program.
// Use MemoryStream to read file in binary mode.
// --------------------------------------------------------------------------------------------------
var
    xStream: TMemoryStream;
begin
    result := true;
    aCRCValue := $FFFFFFFF;
    xStream := TMemoryStream.Create;
    try
        xStream.LoadFromFile(aFileName);
        if (xStream.Size > 0) then
            CalcCRC32(xStream.Memory, xStream.Size, aCRCValue); // uses tool unit "CRC32.pas"

        aCRCvalue := not aCRCvalue;
        aSize := xStream.Size;
    except
        on E: EReadError do
            result := false;
    end;
    FreeAndNil(xStream);
end;

{ TCFR21UserlogDataAdaptor }

constructor TCFR21UserlogDataAdaptor.Create;
begin
    inherited Create(STR_CFR21_USERLOG_NAME);
end;

function TCFR21UserlogDataAdaptor.CheckFile(aFileName: string): boolean;
var
    xCRCValue: Longword;
    xSize: Int64;
    xExt, xSizeStr, xValueStr: string;
begin
    result := false;

    // analyse file & get checksum
    if gmCalcFileCRC32(aFileName, xSize, xCRCvalue) then
    begin

        // extract checksum data from file name
        xExt := ExtractFileExt(aFileName);
        xSizeStr := Copy(aFileName, Length(aFileName) - Length(xExt) - 23, 16);
        xValueStr := Copy(aFileName, Length(aFileName) - Length(xExt) - 7, 8);
        if (IntToHex(xSize, 16) = xSizeStr) and (IntToHex(xCRCvalue, 8) = xValueStr) then
            result := true;
    end;
end;

// function TCFR21UserlogDataAdaptor.GetDbPath(): string;
// begin
// result := TDataAdaptor.GetUserAliasPath();
// end;

function TCFR21UserlogDataAdaptor.GetAlias(): string;
begin
    result := TDataProviderFactory.Instance.UserDBAlias;
end;

procedure TCFR21UserlogDataAdaptor.InternLog(aUserKey, aActionType: integer; aAction, aReason: string);
begin
    DoLog(aUserKey, INT_CFR21_USERLOG_LOGTYPE_INTERN, aActionType, STR_CFR21_USERLOG_TITLE_INTERN,
        aAction, aReason);
end;

function TCFR21UserlogDataAdaptor.GetArchiveFileInfo(aFileName: string; const aInfo: TList<string>;
    out oTimeStr: string; out oUserPK: integer): boolean;
var
    xSQLString: string;
begin
    result := false;

    // check if file has been changed
    if not CheckFile(aFileName) then
    begin
        aInfo.Add('The file could have been changed -> not valid.');
        EXIT;
    end;

    // chack if entry can be found in userlog
    xSQLString := 'SELECT * FROM ' + STR_CFR21_USERLOG_NAME + ' WHERE (' + STR_CFR21_USERLOG_FLD_LOGTYPE + '='
        + IntToStr(INT_CFR21_USERLOG_LOGTYPE_ARCHIVING) + ')' + ' AND (' + STR_CFR21_USERLOG_FLD_ACTION + '="'
        + ExtractFileName(aFileName) + '")';

    // TLogging.WriteIntoLog(xSQLString, '.\UserSQL.dat');

    self.SelectAndOpen(xSQLString, true);
    try
        if self.DataProvider.Eof then
        begin
            aInfo.Add('File not found in journal -> not valid.');
        end
        else
        begin
            result := true;
            oTimeStr := DateTimeToStr(self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_LOGTIME)
                .AsDateTime);
            oUserPK := self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_USER_FK).AsInteger;
        end;
    finally
        self.Close;
    end;
end;

function TCFR21UserlogDataAdaptor.GetUserLog(aFrom, aTo: TDateTime): TUserLogArray;
var
    xSQLString: string;
    x: integer;
begin
    result := nil;
    aTo := aTo + 1; // inclusive des letzten Tages
    xSQLString := 'SELECT * FROM ' + STR_CFR21_USERLOG_NAME + ' WHERE ' + STR_CFR21_USERLOG_FLD_LOGTIME +
        ' BETWEEN ''' + DateToStr(aFrom) + ''' AND ''' + DateToStr(aTo) + '''';

    // TLogging.WriteIntoLog(xSQLString, '.\UserSQL.dat');
    self.SelectAndOpen(xSQLString, true);
    try
        if self.DataProvider.Eof then
            EXIT;

        SetLength(result, self.DataProvider.RecordCount);
        x := 0;
        repeat
            // hier wird
            result[x].UserPK := self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_USER_FK).AsInteger;
            result[x].LogTime := self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_LOGTIME).AsDateTime;
            result[x].LogType := self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_LOGTYPE).AsInteger;
            result[x].ActionType := self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_ACTIONTYPE).AsInteger;
            result[x].Action := self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_ACTION).AsString;
            result[x].Title := self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_TITLE).AsString;
            result[x].Reason := self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_REASON).AsString;
            self.DataProvider.Next;
            inc(x);
        until (self.DataProvider.Eof);
    finally
        self.Close;
    end;
end;

procedure TCFR21UserlogDataAdaptor.DoLog(aUserKey, aLogtype, aActionType: integer;
    aTitle, aAction, aReason: string);
begin
    self.SelectAndOpenAll(false);
    try
        self.DataProvider.Append;
        self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_USER_FK).AsInteger := aUserKey;
        self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_LOGTYPE).AsInteger := aLogtype;
        self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_ACTIONTYPE).AsInteger := aActionType;
        self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_LOGTIME).AsDateTime := Now;
        self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_TITLE).AsString :=
            Copy(aTitle, 1, INT_CFR21_USERLOG_FLDLEN_TITLE);
        self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_ACTION).AsString :=
            Copy(aAction, 1, INT_CFR21_USERLOG_FLDLEN_ACTION);
        self.DataProvider.FieldByName(STR_CFR21_USERLOG_FLD_REASON).AsString :=
            Copy(aReason, 1, INT_CFR21_USERLOG_FLDLEN_REASON);
        self.DataProvider.Post;
    finally
        self.Close;
    end;
end;

function TCFR21UserlogDataAdaptor.LogArchiveAndChecksum(var aFileName: string;
    aUserKey, aActionType: integer): boolean;
var
    xCRCValue: Longword;
    xSize: Int64;
    xExt, xNewName: string;
begin
    // not compliant -> no checksum
    if (TDataAdaptor.GetCFR21Mode <> ccmCompliant) then
    begin
        result := true;
        exit;
    end;
    result := false;

    // analyse file & get checksum
    if gmCalcFileCRC32(aFileName, xSize, xCRCvalue) then
    begin

        // create new name
        xExt := ExtractFileExt(aFileName);
        xNewName := Copy(aFileName, 1, Length(aFileName) - Length(xExt));
        xNewName := xNewName + IntToHex(xSize, 16) + IntToHex(xCRCvalue, 8);
        xNewName := xNewName + xExt;

        // Rename file
        if not RenameFile(aFileName, xNewName) then
            exit;
        aFileName := xNewName;
        result := true;

        // log archived file
        DoLog(aUserKey, INT_CFR21_USERLOG_LOGTYPE_ARCHIVING, aActionType, 'File archived!',
            ExtractFileName(aFileName), ExtractFilePath(aFileName));
    end;
end;


end.
