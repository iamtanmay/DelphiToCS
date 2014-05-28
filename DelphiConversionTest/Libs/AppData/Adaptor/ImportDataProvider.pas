{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Various classes and functions that work with TQuery and TAdoQuery
  The functions provide an encapsulation for the Query object that is required given
  the dbType
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- ----------------------------------------
  26.10.07 wl  TImportBDEQuery.CreateOrGetSession  TN3908   Wenn ein Password gesetzt ist, wird es an die Session übergeben
  04.04.08 wl  TSQLParser                          TN4058   --> SQLParser
  04.04.08 wl  TImportBDEQuery.PrepareConnection   TN4058   --> DataProvider
  04.04.08 wl  TImportBDEQuery.SetCachedUpdates    TN4058   --> DataProvider
  04.04.08 wl  TImportBDEQuery.DatasetApplyUpdates TN4058   --> DataProvider
  17.04.08 wl  TImportQuery                        TN4067   fDataset entfernt: Alle Dataset Funktionen abstract definiert
  17.04.08 wl  TImportAdoQuery                     TN4067   benutzt TAdoQueryExt
  17.04.08 wl  TImportBDEQuery                     TN4067   benutzt TDataProvider
  11.06.08 wl  TImportBDEQuery.ExecuteAfterScroll  TN4143   auskommentiertes wieder aktiviert
  17.06.09 wl  TFileInfo.CopyFileAsTemporaryFile            unnötige Umwandlung in PChar entfernt
  06.07.09 pk  TImportAdoQuery                     TN4585.4 GetFields: must be reimplemented, commented out for now
  09.07.09 pk                                      TN4585.4 Massive changes, Code moved to TADODataProvider, TBDEDataProvider, SQLParser, etc
  10.08.09 wl                                      TN4702   Strings werden jetzt direkt geladen
  31.08.09 pk  TImportQuery.Destroy                TN4760   call DataProvider.Free
  04.11.09 pk                               	      TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.02.10 pk                                      TN4972   changes for Restart
  13.04.10 wl                                      TN5044   uses FileUtilities
  17.06.10 pk                                      TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  04.08.10 pk                                      TN5218   Changes for Plugin database packages
  05.08.10 pk  ResolveAliasesInSQL                 TN5218   Now supports SQLs with ":AliasName:TableName" syntax
  05.08.10 pk                                      TN5221   Changes needed for Component Ace Absolute Database
  12.04.11 ts                                      TN5548   new: SkipLines setting to skip the first few lines of import file (if Excel-File)
  01.07.11 wl                                      TN5619   Semikolon eingefügt
  16.08.11 wl  ResolveAliasesInSQL                 TN5661   --> TSQLParser
  17.08.11 wl  PrepareConnection                   TN5654   entfernt
  26.03.12 wl  TImportQuery.Prepare                TN5846   OrderBy wird jetzt auch wieder berücksichtigt
  -------------------------------------------------------------------------------------------------- }

unit ImportDataProvider;


interface


uses
    DataProvider,
    ListClasses,
    GeneralTypes,
    SQLParser,
    StreamableDatasetClasses,
    DataConnectionParams;

type
    TFileInfo = class
    private const
        cMSAccessTypeName = 'Access';

    const
        cMSExcelTypeName = 'Excel';

    const
        cCSVTypeName = 'CSV';
    private
        fDBType: TEnDBType;
        fSessionName: string;
        fPathName: string;
        fTableName: string;
        fDelimiter: string;
        fHasHeader: boolean;
        fOrderBy: string;
        fFilter: string;
        fRowOffset: integer;
        fSQL: string;
        fOriginalPathName: string;
        fUsername: string;
        fPassword: string;
        fDataLocation: string;
        fSkipLines: integer;
        class function CreateTemporaryFileName(const aFileName: string): string;
    public
        constructor Create(aDBType: TEnDBType; aSessionName, aPathName: string; aTableName: string;
            aDelimiter: string; aHasHeader: boolean; aFilter: string; aOrderBy: string; aRowOffset: integer;
            const aUsername, aPassword: string; aSQL: string; aSkipLines: integer);
        //
        procedure CopyFileAsTemporaryFile();
        procedure DeleteTemporaryFile();
        function FileExtensionFitsToDBType(): boolean;
        class function GetSeparatorChar(aSeparator: string): string;
        class function GetPossibleSeparators(): TStringArray;
        class function GetTypeNameFromDBType(const aDBType: TEnDBType): string;
        //
        property DBType: TEnDBType read fDBType;
        property SessionName: string read fSessionName;
        property PathName: string read fPathName;
        property TableName: string read fTableName;
        property Delimiter: string read fDelimiter;
        property HasHeader: boolean read fHasHeader;
        property Filter: string read fFilter;
        property OrderBy: string read fOrderBy;
        property RowOffset: integer read fRowOffset;
        property Username: string read fUsername;
        property Password: string read fPassword;
        property SQL: string read fSQL;
        property Datalocation: string read fDataLocation write fDataLocation;
        property SkipLines: integer read fSkipLines;
    end;

    TImportQuery = class
    private

        fParsedTableName: string;
        fOwnsDataset: boolean;
        fFileInfo: TFileInfo;
        fDataProvider: TDataProvider;
        fDataLocations: array of string;
        fSystemDataPassword: string;
        fSQL: string;
        fSQLRec: TSQLRec;
        fIsSQLParsed: boolean;
        procedure ExecuteAfterScroll(aDataset: TDataProvider);
        function DetermineSimpleSQL(): TSQLRec;
        function GetEOF(): boolean;
        function GetFieldCount(): integer;
        function GetFields(): TDataFields;
        procedure Prepare();
        function DetermineSQLTableName(): string;
        function CreateConnectionParams(const aFileInfo: TFileInfo): TDataConnectionParams;
        procedure CreateDataProvider;
    public
        constructor Create(aFileInfo: TFileInfo; const aSystemDataPassword: string);
        destructor Destroy; override;
        //
        procedure MoveBy(aOffset: integer);
        procedure First();
        procedure Next();
        procedure Append();
        procedure Post();
        function RecordCount(): integer;
        function GetFieldNames(): TStringArray;
        procedure DatasetApplyUpdates(); virtual;
        procedure Open(aReadOnly, aCachedUpdates: boolean);
        //
        property EOF: boolean read GetEOF;
        property FieldCount: Integer read GetFieldCount;
        property Fields: TDataFields read GetFields;

        property FileInfo: TFileInfo read fFileInfo write fFileInfo;
        function OffsetRecordCount(aTotalRecordCount: integer): integer;
    end;


implementation


uses
    SysUtils,
    StrUtils,
    FileUtilities,
    UtilLib,
    DatabaseProvider,
    DataProviderFactory,
    ExcelOLEManager;

{ TFileInfo }

constructor TFileInfo.Create(aDBType: TEnDBType; aSessionName, aPathName: string; aTableName: string;
    aDelimiter: string; aHasHeader: boolean; aFilter: string; aOrderBy: string; aRowOffset: integer;
    const aUsername, aPassword: string; aSQL: string; aSkipLines: integer);
begin
    inherited Create;

    fDBType := aDBType;
    fSessionName := aSessionName;
    fPathName := aPathName;
    fTableName := aTableName;
    fDelimiter := GetSeparatorChar(aDelimiter);
    fHasHeader := aHasHeader;
    fFilter := aFilter;
    fOrderBy := aOrderBy;
    fRowOffset := aRowOffset;
    fSQL := aSQL;
    fUsername := aUsername;
    fPassword := aPassword;
    fOriginalPathName := '';
    fDataLocation := '';
    fSkipLines := aSkipLines;
end;

procedure TFileInfo.CopyFileAsTemporaryFile();
begin
    if (fOriginalPathName = '') then
    begin
        fOriginalPathName := fPathName;
        fPathName := CreateTemporaryFileName(fOriginalPathName);
        TFileUtilities.CopyFile(fOriginalPathName, fPathName, true);
    end;
end;

procedure TFileInfo.DeleteTemporaryFile;
begin
    if (fOriginalPathName <> '') then
    begin
        TFileUtilities.DeleteFile(fPathName);
        fPathName := fOriginalPathName;
        fOriginalPathName := '';
    end;
end;

function TFileInfo.FileExtensionFitsToDBType: boolean;
begin
    result := true; // Normalfall

    // Textdatei mit anderer Extension:
    if (fDBType = dbAscii) and (UpperCase(ExtractFileExt(fPathName)) <> '.TXT') then
    begin
        result := false;
    end;
end;

class function TFileInfo.CreateTemporaryFileName(const aFileName: string): string;
var
    xIndex, xExtLength: integer;
begin
    xExtLength := Length(ExtractFileExt(aFileName));
    xIndex := 1;

    if UpperCase(ExtractFileExt(aFileName)) = '.XLS' then
    begin
        repeat
            result := Copy(aFileName, 1, Length(aFileName) - xExtLength) + IntToStr(xIndex) + '.XLS';
            Inc(xIndex);
        until not TFileUtilities.FileExists(result);
        EXIT;
    end;

    repeat
        result := Copy(aFileName, 1, Length(aFileName) - xExtLength) + IntToStr(xIndex) + '.TXT';
        Inc(xIndex);
    until not TFileUtilities.FileExists(result);
end;

class function TFileInfo.GetSeparatorChar(aSeparator: string): string;
begin
    result := aSeparator;

    // Tabstopp ersetzen!
    if (Uppercase(aSeparator) = 'TAB') then
        result := #9;
end;

class function TFileInfo.GetPossibleSeparators(): TStringArray;
begin
    SetLength(result, 3);
    result[0] := ',';
    result[1] := ';';
    result[2] := 'TAB';
end;

class function TFileInfo.GetTypeNameFromDBType(const aDBType: TEnDBType): string;
begin
    case aDBType of
        dbAscii:
            result := cCSVTypeName;
        dbExcel:
            result := cMSExcelTypeName;
        dbAccess:
            result := cMSAccessTypeName;
        else
            result := TDataConnectionUtils.StandardPseudoTypeName;
    end;

end;

{ TImportQuery }

function TImportQuery.CreateConnectionParams(const aFileInfo: TFileInfo): TDataConnectionParams;
var
    xConnectionParams: TDataConnectionParams;
    xTypeName: string;

begin
    xTypeName := TFileInfo.GetTypeNameFromDBType(fFileInfo.DBType);

    // 04.08.10 pk This is a HACK!!!  This is because the ImportFileDef Form does not support the plugin concept
    // The properties of each connection should be loaded into a property grid, and then we could save and load the parameters
    // using the typename

    xConnectionParams := TDataProviderFactory.Instance.CreateDataConnectionParams(xTypeName);

    xConnectionParams.DataLocation := TSQLParser.ResolveAliasToPath(fFileInfo.Datalocation);

    if xConnectionParams is TMSAccessDataConnectionParams then
    begin
    end
    else if xConnectionParams is TMSExcelDataConnectionParams then
    begin
        with xConnectionParams as TMSExcelDataConnectionParams do
        begin
            HasHeader := fFileInfo.HasHeader;
        end;
    end
    else if xConnectionParams is TCSVDataConnectionParams then
    begin
        with xConnectionParams as TCSVDataConnectionParams do
        begin
            HasHeader := fFileInfo.HasHeader;
            Delimiter := fFileInfo.Delimiter;
            TableName := fParsedTableName;
        end;
    end
    else if xConnectionParams is TTDBDataConnectionParams then
    begin

    end
    else if xConnectionParams is TACRDBDataConnectionParams then
    begin

    end
    else if xConnectionParams is TABSDBDataConnectionParams then
    begin

    end
    else if xConnectionParams is TBDEDataConnectionParams then
    begin
        with xConnectionParams as TBDEDataConnectionParams do
        begin
            Username := fFileInfo.Username;
            Password := fFileInfo.Password;
            SessionName := fFileInfo.SessionName;
            SystemDataPassword := fSystemDataPassword;
        end;
    end
    else
    begin
        ASSERT(false, TTypeSafeFormat.Format('Typename [{0}] not found', [xTypeName]));
    end;

    result := xConnectionParams;

end;

procedure TImportQuery.CreateDataProvider();
var
    xConnectionParams: TDataConnectionParams;
begin
    xConnectionParams := self.CreateConnectionParams(fFileInfo);

    fDataProvider := TDataProviderFactory.Instance.CreateDataProvider(xConnectionParams);
end;

constructor TImportQuery.Create(aFileInfo: TFileInfo; const aSystemDataPassword: string);
var
    xExcel: TExcelOLEManager;
begin
    inherited Create;
    fFileInfo := aFileInfo;
    fOwnsDataset := false;
    fSystemDataPassword := aSystemDataPassword;

    // Temporäre Datei erzeugen, die für AdoQuery benutzt wird
    if not aFileInfo.FileExtensionFitsToDBType then
    begin
        aFileInfo.CopyFileAsTemporaryFile();
    end;

    if (aFileInfo.SkipLines > 0) and (aFileInfo.fDBType = dbExcel) then
    begin
        aFileInfo.CopyFileAsTemporaryFile();
        xExcel := TExcelOleManager.Create;
        if (xExcel.Connect) then
        begin
            xExcel.DeleteLines(aFileInfo.fPathName, aFileInfo.fTableName, aFileInfo.SkipLines);
            xExcel.Disconnect;
            xExcel.Quit;
        end;
        xExcel.Free;
    end;

    Prepare();

    CreateDataProvider();

    fDataProvider.OnAfterOpen := ExecuteAfterScroll;
    fDataProvider.OnRecordCount := OffsetRecordCount;

end;

destructor TImportQuery.Destroy;
begin
    FreeAndNil(fDataProvider);

    fFileInfo.DeleteTemporaryFile(); // die temporäre Datei löschen

    FreeAndNil(fFileInfo);

    inherited;
end;

function TImportQuery.OffsetRecordCount(aTotalRecordCount: integer): integer;
begin
    result := aTotalRecordCount - fFileInfo.RowOffset;
    if result < 0 then
        result := 0;
end;

function TImportQuery.DetermineSQLTableName(): string;
var
    xTableReference: string;
begin
    xTableReference := fParsedTableName;
    if fFileInfo.DBType = dbStandard then
    begin
        if (fFileInfo.Username <> '') then
        begin
            xTableReference := fFileInfo.PathName;
        end
    end;

    result := xTableReference; // TSQLParser.MakeSQLTableName( fFileInfo.DBType, xTableReference );
end;

function TImportQuery.DetermineSimpleSQL(): TSQLRec;
var
    xTableName, xSQLTableName: string;
    xDataLocation: string;
begin
    TAliasTablePath.ParseDataLocationAndTableName(fFileInfo.DBType, fFileInfo.PathName, fFileInfo.TableName,
        xDataLocation, xTableName);
    fFileInfo.DataLocation := xDataLocation;

    fParsedTableName := xTableName;
    xSQLTableName := DetermineSQLTableName();
    result := TSQLParser.MakeSQLRec('*', xSQLTableName, fFileInfo.Filter, fFileInfo.OrderBy);
end;

procedure TImportQuery.Prepare();
var
    xSQLRec: TSQLRec;
    xDataLocation, xTableName: string;
    xPathNames: TStringArray;
    i: integer;
begin
    fSQL := fFileInfo.SQL;
    fSQL := TSQLParser.ResolveAliasesInSQL(fSQL);

    fIsSQLParsed := false;

    if fSQL <> '' then
    begin
        if (fFileInfo.Filter <> '') or (fFileInfo.Username <> '') then
        begin
            xSQLRec := TSQLParser.ParseSQL(fSQL);

            if (fFileInfo.Filter <> '') or (fFileInfo.OrderBy <> '') then
            begin
                // here we have no choice, we have to use the parser.  We just hope that we dont kill the SQL
                fIsSQLParsed := true;
                fSQLRec := xSQLRec;
                fSQLRec.Where := TSQLParser.ConcatFilters(xSQLRec.Where, fFileInfo.Filter);
                fSQLRec.Orderby := TSQLParser.ConcatOrders(xSQLRec.Orderby, fFileInfo.OrderBy);
            end;

            if (fFileInfo.Username <> '') then
            begin
                // We need the database name for the Login.
                // databasename MUST BE the same as the alias
                // in this implementation only login into only 1 database is allowed
                // It is possible to use more than 1 db in the SQL (ex: select * from ":DB1:Table1" d1, ":DB2:Table1" d2 where d1.fld1 = d2.fld1)
                // BUT a Login will only be done for DB1.  We assume that DB2 uses the same username, and password
                TSQLParser.ParseSQLFromClause(xSQLRec.From, xPathNames);
                SetLength(fDataLocations, Length(xPathNames));
                for i := 0 to high(xPathNames) do
                begin
                    TAliasTablePath.ParadoxParseDatabaseNameAndTableName(xPathNames[i], xDataLocation,
                        xTableName);
                    fDataLocations[i] := xDataLocation;
                end;
            end;
        end;
    end
    else
    begin
        fIsSQLParsed := true;
        fSQLRec := DetermineSimpleSQL();
    end;
end;

procedure TImportQuery.Open(aReadOnly, aCachedUpdates: boolean);
begin
    fDataProvider.SetCachedUpdates(aCachedUpdates);

    // it would maybe make sense to parse every SQL, but the parser is not that good yet.  For example Having, Sum, and In clauses may not be parsed correctly
    // so we try to avoid parsing as much as we can
    if fIsSQLParsed then
    begin
        fDataProvider.SelectAndOpenBySQLRec(fSQLRec, aReadOnly);
    end
    else
    begin
        fDataProvider.SelectAndOpen(fSQL, aReadOnly);
    end;
end;

procedure TImportQuery.DatasetApplyUpdates;
begin
    fDataProvider.DatasetApplyUpdates();
end;

procedure TImportQuery.MoveBy(aOffset: integer);
begin
    fDataProvider.MoveBy(aOffset);
end;

procedure TImportQuery.First();
begin
    fDataProvider.First();
end;

procedure TImportQuery.Next();
begin
    fDataProvider.Next();
end;

procedure TImportQuery.Append();
begin
    fDataProvider.Append();
end;

procedure TImportQuery.Post();
begin
    fDataProvider.Post();
end;

function TImportQuery.RecordCount(): integer;
begin
    result := fDataProvider.RecordCount;
end;

function TImportQuery.GetEOF(): boolean;
begin
    result := fDataProvider.Eof;
end;

function TImportQuery.GetFieldCount(): integer;
begin
    result := fDataProvider.FieldCount;
end;

function TImportQuery.GetFields(): TDataFields;
begin
    result := fDataProvider.Fields;
end;

function TImportQuery.GetFieldNames(): TStringArray;
begin
    result := fDataProvider.GetFieldNames();
end;

procedure TImportQuery.ExecuteAfterScroll(aDataset: TDataProvider);
var
    xDelta: integer;
    xRecNo: integer;
begin
    if fDataProvider.IsInsertState then
        Exit;
    xRecNo := fDataProvider.RecNo;
    if xRecNo = -1 then
        xRecNo := 1; // if RecNo not implemented by dataset
    xDelta := fFileInfo.RowOffset - (xRecNo - 1);
    if xDelta <= 0 then
        Exit;
    fDataProvider.MoveBy(xDelta);
end;


end.
