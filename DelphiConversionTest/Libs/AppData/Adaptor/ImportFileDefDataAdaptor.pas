{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.08.07 wl                               TN3811.2 von ImportDataAdaptor getrennt
  30.08.07 pk  ReadFileDef                  TN3811.2 Bug fixed. aName passed to SelectAndOpenDef
  02.10.07 wl  GetKeyFields                 TN3811.5 neu: damit DeleteName & SaveNameAs benutzt werden können
  09.11.07 pk                               TN3921   Changes for updatemanager
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  09.02.09 ts TFileDefRec                   TN4346   UseDataTypes added
  27.05.09 wl  TFileDefRec                  TN4476   UseDataTypes entfernt
  06.07.09 pk  SetRange, CancelRange        TN4585.4 Removed
  09.07.09 pk                               TN4585.4 DataProvider.Locate removed, functionality replaced by SelectAndOpen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.06.10 pk                               TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  23.02.11 wl  GetNameField                 TN5486   neu
  12.04.11 ts                               TN5548   new: SkipLines setting to skip the first few lines of import file (if Excel-File)
  01.07.11 wl  AppendName                   TN5619   neu
  22.07.11 wl  ReadAllFileDefs              TN5622   entfernt
  13.11.12 wl  GetKeyFields                 TN6015   kann entfernt werden, wenn Key = NameField
  -------------------------------------------------------------------------------------------------- }

unit ImportFileDefDataAdaptor;


interface


uses
    Classes,
    DataProvider,
    QueryDataAdaptor,
    GeneralTypes;

type
    TFileDefRec = record
        Valid: boolean;
        DBType: integer;
        name: string;
        PathName: string;
        TableName: string;
        Delimiter: string;
        HasHeader: boolean;
        RowOffset: integer;
        UserName: string;
        Password: string;
        SQL: string;
        Filter: string;
        OrderBy: string;
        SkipLines: integer;
    end;

    TFileDefRecArray = array of TFileDefRec;

    TImportFileDefDataAdaptor = class(TQueryDataAdaptor)
    protected
        function GetNameField(): string; override;
    public
        constructor Create();
        procedure SelectAndOpenDef(aName: string; aReadOnly: boolean);
        procedure WriteFileDef(aFileDef: TFileDefRec);
        function ReadFileDef(const aName: string; var vRec: TFileDefRec): boolean;
        function GetDataset(aName: string; aReadOnly: boolean = true): TDataProvider;

        class function MakeFileDefRec(aValid: boolean; aDBType: integer;
            const aName, aPathName, aTableName, aDelimiter: string; aRowoffset: integer; aHasHeader: boolean;
            const aUsername, aPassword, aSQL, aFilter, aOrderBy: string; aSkipFiles: integer): TFileDefRec;
        class procedure WriteFileDefToDataset(aDataset: TDataProvider; aFileDef: TFileDefRec;
            aAppend: boolean);
        class function ReadFileDefFromDataSet(aDataset: TDataProvider): TFileDefRec;
        class function ReadAllFileDefsFromDataSet(aDataset: TDataProvider): TFileDefRecArray;
        class function AreEqual(aDef1, aDef2: TFileDefRec): boolean;
        procedure AppendName(const aNewName: string);
    end;


implementation


uses
    SysUtils,
    Variants;

const
    STR_IMPFILE_TBL = 'IMPFILE';
    STR_IMPFILE_FLD_NAME = 'NAME';
    STR_IMPFILE_FLD_PATHNAME = 'PATHNAME';
    STR_IMPFILE_FLD_TABLENAME = 'TABLENAME';
    STR_IMPFILE_FLD_DELIMITER = 'DELIMITER';
    STR_IMPFILE_FLD_HASHEADER = 'HASHEADER';
    STR_IMPFILE_FLD_ROWOFFSET = 'ROWOFFSET';
    STR_IMPFILE_FLD_USERNAME = 'USERNAME';
    STR_IMPFILE_FLD_PASSWORD = 'PASSWORD';
    STR_IMPFILE_FLD_SQL = 'SQL';
    STR_IMPFILE_FLD_FILTER = 'FILTER';
    STR_IMPFILE_FLD_ORDERBY = 'ORDERBY';
    STR_IMPFILE_FLD_DATABASETYPE = 'DATABASETYPE';
    STR_IMPFILE_FLD_SKIPLINES = 'SKIPLINES';

    STR_SQL_IMPFILE_SELECT = 'SELECT * FROM ' + STR_IMPFILE_TBL;
    STR_IMPFILE_FILTER = STR_IMPFILE_FLD_NAME + ' = ''%s''';

    { TImportFileDefDataAdaptor }

constructor TImportFileDefDataAdaptor.Create();
begin
    inherited Create(STR_IMPFILE_TBL);
end;

procedure TImportFileDefDataAdaptor.SelectAndOpenDef(aName: string; aReadOnly: boolean);
var
    xSelect: string;
begin
    xSelect := STR_SQL_IMPFILE_SELECT;
    if aName <> '' then
        xSelect := format('%s WHERE %s = ''%s''', [xSelect, STR_IMPFILE_FLD_NAME, aName]);

    self.SelectAndOpen(xSelect, aReadOnly);
end;

function TImportFileDefDataAdaptor.GetDataset(aName: string; aReadOnly: boolean = true): TDataProvider;
begin
    self.SelectAndOpenDef(aName, aReadOnly);
    result := self.DataProvider;
end;

class function TImportFileDefDataAdaptor.MakeFileDefRec(aValid: boolean; aDBType: integer;
    const aName, aPathName, aTableName, aDelimiter: string; aRowoffset: integer; aHasHeader: boolean;
    const aUsername, aPassword, aSQL, aFilter, aOrderBy: string; aSkipFiles: integer): TFileDefRec;
begin
    result.Valid := aValid;
    result.DBType := aDBType;
    result.Name := aName;
    result.PathName := aPathName;
    result.TableName := aTableName;
    result.Delimiter := aDelimiter;
    result.HasHeader := aHasHeader;
    result.RowOffset := aRowOffset;
    result.UserName := aUserName;
    result.PassWord := aPassword;
    result.SQL := aSQL;
    result.Filter := aFilter;
    result.OrderBy := aOrderBy;
    result.SkipLines := aSkipFiles;
end;

class procedure TImportFileDefDataAdaptor.WriteFileDefToDataset(aDataset: TDataProvider;
    aFileDef: TFileDefRec; aAppend: boolean);
begin
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(STR_IMPFILE_FLD_NAME).AsString := aFileDef.Name;
    aDataset.FieldByName(STR_IMPFILE_FLD_PATHNAME).AsString := aFileDef.PathName;
    aDataset.FieldByName(STR_IMPFILE_FLD_TABLENAME).AsString := aFileDef.TableName;
    aDataset.FieldByName(STR_IMPFILE_FLD_DELIMITER).AsString := aFileDef.Delimiter;
    aDataset.FieldByName(STR_IMPFILE_FLD_HASHEADER).AsBoolean := aFileDef.HasHeader;
    aDataset.FieldByName(STR_IMPFILE_FLD_ROWOFFSET).AsInteger := aFileDef.RowOffset;
    aDataset.FieldByName(STR_IMPFILE_FLD_USERNAME).AsString := aFileDef.UserName;
    aDataset.FieldByName(STR_IMPFILE_FLD_PASSWORD).AsString := aFileDef.PassWord;
    aDataset.FieldByName(STR_IMPFILE_FLD_SQL).AsString := aFileDef.SQL;
    aDataset.FieldByName(STR_IMPFILE_FLD_FILTER).AsString := aFileDef.Filter;
    aDataset.FieldByName(STR_IMPFILE_FLD_ORDERBY).AsString := aFileDef.OrderBy;
    aDataSet.FieldByName(STR_IMPFILE_FLD_DATABASETYPE).AsInteger := aFileDef.DBType;
    aDataset.FieldByName(STR_IMPFILE_FLD_SKIPLINES).AsInteger := aFileDef.SkipLines;
    aDataSet.Post;

end;

class function TImportFileDefDataAdaptor.ReadFileDefFromDataSet(aDataset: TDataProvider): TFileDefRec;
begin
    result.Valid := false;
    if aDataset.Eof then
        Exit;
    result := MakeFileDefRec(true, aDataset.FieldByName(STR_IMPFILE_FLD_DATABASETYPE).AsInteger,
        aDataset.FieldByName(STR_IMPFILE_FLD_NAME).AsString, aDataset.FieldByName(STR_IMPFILE_FLD_PATHNAME)
        .AsString, aDataset.FieldByName(STR_IMPFILE_FLD_TABLENAME).AsString,
        aDataset.FieldByName(STR_IMPFILE_FLD_DELIMITER).AsString,
        aDataset.FieldByName(STR_IMPFILE_FLD_ROWOFFSET).AsInteger,
        aDataset.FieldByName(STR_IMPFILE_FLD_HASHEADER).AsBoolean,
        aDataset.FieldByName(STR_IMPFILE_FLD_USERNAME).AsString,
        aDataset.FieldByName(STR_IMPFILE_FLD_PASSWORD).AsString, aDataset.FieldByName(STR_IMPFILE_FLD_SQL)
        .AsString, aDataset.FieldByName(STR_IMPFILE_FLD_FILTER).AsString,
        aDataset.FieldByName(STR_IMPFILE_FLD_ORDERBY).AsString,
        aDataset.FieldByName(STR_IMPFILE_FLD_SKIPLINES).AsInteger);
end;

class function TImportFileDefDataAdaptor.ReadAllFileDefsFromDataSet(aDataSet: TDataProvider)
    : TFileDefRecArray;
var
    xNumRecs, xCount: integer;
begin
    xNumRecs := aDataSet.RecordCount;
    SetLength(result, xNumRecs);
    if xNumRecs = 0 then
        Exit;
    xCount := 0;
    while not aDataSet.Eof do
    begin
        result[xCount] := ReadFileDefFromDataSet(aDataSet);
        aDataSet.Next;
        Inc(xCount);
    end;
end;

procedure TImportFileDefDataAdaptor.WriteFileDef(aFileDef: TFileDefRec);
var
    xDefNotFound: boolean;
begin
    self.SelectAndOpenDef(aFileDef.Name, false);
    try
        xDefNotFound := self.DataProvider.IsEmpty;
        WriteFileDefToDataSet(self.DataProvider, aFileDef, xDefNotFound);
    finally
        self.Close();
    end;
end;

function TImportFileDefDataAdaptor.ReadFileDef(const aName: string; var vRec: TFileDefRec): boolean;
begin
    vRec.Valid := false;
    self.SelectAndOpenDef(aName, true);
    try
        result := not self.DataProvider.IsEmpty;
        if not result then
            EXIT;
        vRec := ReadFileDefFromDataSet(self.DataProvider);
    finally
        self.Close();
    end;
end;

procedure TImportFileDefDataAdaptor.AppendName(const aNewName: string);
var
    xFileDef: TFileDefRec;
begin
    xFileDef.Valid := true;
    xFileDef.name := aNewName;
    xFileDef.DBType := 1; // Ascii
    xFileDef.HasHeader := false;
    xFileDef.RowOffset := 0;
    xFileDef.SkipLines := 0;
    WriteFileDef(xFileDef);
end;

class function TImportFileDefDataAdaptor.AreEqual(aDef1, aDef2: TFileDefRec): boolean;
begin
    result := false;
    if aDef1.Valid <> aDef2.Valid then
        Exit;
    if aDef1.Name <> aDef2.Name then
        Exit;
    if aDef1.PathName <> aDef2.PathName then
        Exit;
    if aDef1.TableName <> aDef2.TableName then
        Exit;
    if aDef1.Delimiter <> aDef2.Delimiter then
        Exit;
    if aDef1.HasHeader <> aDef2.HasHeader then
        Exit;
    if aDef1.RowOffset <> aDef2.RowOffset then
        Exit;
    if aDef1.UserName <> aDef2.UserName then
        Exit;
    if aDef1.Password <> aDef2.Password then
        Exit;
    if aDef1.SQL <> aDef2.SQL then
        Exit;
    if aDef1.Filter <> aDef2.Filter then
        Exit;
    if aDef1.DBType <> aDef2.DBType then
        Exit;
    result := true;
end;

function TImportFileDefDataAdaptor.GetNameField: string;
begin
    result := STR_IMPFILE_FLD_NAME;
end;


end.
