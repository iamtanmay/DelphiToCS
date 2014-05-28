{ --------------------------------------------------------------------------------------------------
  Copyright  2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Utility class for creating and accessing dataprovider/databaseprovider
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  15.02.11 pk                               TN4780   Code from TableProvider.pas
  03.03.11 wl  CopyTable                    TN5493   Kopieren der VR-Dateien --> DataProviderFactory
  06.03.11 pk                               TN4780   fixed formatting of .pas file
  11.04.11 wl                               TN5549   uses FileUtilities
  17.04.13 wl                               TN6106   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit UpdaterTableProvider;


interface


uses
    TableStructDef,
    DataProvider,
    TableVersionInfo,
    StreamableDatasetClasses;

type
    TUpdaterTableProvider = class
    private
        fVersionInfoAdaptor: TTableVersionInfoAdaptor;

        function GetTablePath(const aPath, aTableName: string): string;
        function GetDBPath: string;
        function GetTempDBPath(): string;
        procedure DeleteTable(const aDBPath: string; const aTableName: string);
        procedure CopyTable(const aSourceDBPath, aTargetDBPath, aTableName: string);
        function TableExists(const aDBPath, aTableName: string): boolean;
        function CreateStreamableTableDefFromStructDef(const aStructDef: TTableStructDef)
            : TStreamableTableDef;
        function CreateStreamableFieldDefFromTableFieldDef(const aTableFieldDef: TTableFieldDef)
            : TStreamableFieldDef;
        function GetDataFieldTypeFromTableFieldType(const aTableFieldType: TTableFieldType): TDataFieldType;
    protected
        fDBPath: string;
        fTempDBPath: string;
        procedure InstallNewTable(const aDBPath: string; aStructDef: TTableStructDef;
            aNewVersion: TTableRevisionNumber);
        function CreateDataProvider(const aDataLocation: string): TDataProvider;
    public
        constructor Create(const aDBPath, aTempDBPath: string);
        procedure InstallNewTableAsTemp(const aStructDef: TTableStructDef; aNewVersion: TTableRevisionNumber);
        procedure CopyTableToDBPath(const aTableName: string);
        function CreateDataProviderForTempTable(): TDataProvider;
        procedure RemoveTempTable(const aTableName: string);
        function GetDBTablePath(const aTableName: string): string;
        function DBPathTableExists(const aTableName: string): boolean;
        procedure ArchiveTable(const aTableName: string; const aArchivePath: string);
        function CreateDataProviderForDBPathTable(): TDataProvider;
    end;


implementation


uses
    SysUtils,
    IniFiles,
    UpdaterDataProviderFactory,
    DatabaseProvider,
    FileUtilities;

const
    STR_TALBEUPDATE_ARCHIVE_DIR = 'ARCHIVE';

constructor TUpdaterTableProvider.Create(const aDBPath, aTempDBPath: string);
begin
    inherited Create();
    fDBPath := aDBPath;
    fTempDBPath := aTempDBPath;
end;

function TUpdaterTableProvider.GetTablePath(const aPath, aTableName: string): string;
begin
    result := Format('%s%s', [aPath, aTableName]);
end;

function TUpdaterTableProvider.GetDBPath: string;
begin
    result := fDBPath;
end;

function TUpdaterTableProvider.GetTempDBPath: string;
begin
    result := fTempDBPath;
end;

procedure TUpdaterTableProvider.DeleteTable(const aDBPath: string; const aTableName: string);
var
    xDatabaseProvider: TDatabaseProvider;
begin
    xDatabaseProvider := TUpdaterDataProviderFactory.CreateStandardDatabaseProvider(aDBPath);
    try
        xDatabaseProvider.DeleteTable(aTableName);
    finally
        FreeAndNil(xDatabaseProvider);
    end;
end;

procedure TUpdaterTableProvider.InstallNewTableAsTemp(const aStructDef: TTableStructDef;
    aNewVersion: TTableRevisionNumber);
var
    xTempPath: string;
begin
    xTempPath := GetTempDBPath(); // ( );
    // create directory and remove old copy of table
    TFileUtilities.ForceDirectories(xTempPath);
    DeleteTable(xTempPath, aStructDef.Name);
    InstallNewTable(xTempPath, aStructDef, aNewVersion);
end;

procedure TUpdaterTableProvider.CopyTable(const aSourceDBPath, aTargetDBPath, aTableName: string);
begin
    TUpdaterDataProviderFactory.CopyTable(aTargetDBPath, aTableName, aSourceDBPath, aTableName);
end;

procedure TUpdaterTableProvider.CopyTableToDBPath(const aTableName: string);
begin
    CopyTable(GetTempDBPath(), GetDBPath(), aTableName);
end;

function TUpdaterTableProvider.CreateDataProviderForTempTable: TDataProvider;
begin
    result := CreateDataProvider(GetTempDBPath()); // TDataProvider.Create( GetTempPath( ) );
end;

procedure TUpdaterTableProvider.RemoveTempTable(const aTableName: string);
begin
    self.DeleteTable(GetTempDBPath(), aTableName);
end;

function TUpdaterTableProvider.GetDBTablePath(const aTableName: string): string;
begin
    result := GetTablePath(GetDBPath(), aTableName);
end;

function TUpdaterTableProvider.TableExists(const aDBPath, aTableName: string): boolean;
var
    xDatabaseProvider: TDatabaseProvider;
begin
    xDatabaseProvider := TUpdaterDataProviderFactory.CreateStandardDatabaseProvider(aDBPath);
    try
        result := xDatabaseProvider.TableExists(aTableName);
    finally
        FreeAndNil(xDatabaseProvider);
    end;
end;

function TUpdaterTableProvider.DBPathTableExists(const aTableName: string): boolean;
begin
    result := TableExists(GetDBPath(), aTableName);
end;

procedure TUpdaterTableProvider.ArchiveTable(const aTableName: string; const aArchivePath: string);
begin
    CopyTable(GetDBPath(), aArchivePath, aTableName);
end;

function TUpdaterTableProvider.CreateDataProviderForDBPathTable: TDataProvider;
begin
    result := self.CreateDataProvider(GetDBPath);
end;

function TUpdaterTableProvider.CreateDataProvider(const aDataLocation: string): TDataProvider;
begin
    result := TUpdaterDataProviderFactory.CreateStandardDataProvider(aDataLocation);
end;

function TUpdaterTableProvider.GetDataFieldTypeFromTableFieldType(const aTableFieldType: TTableFieldType)
    : TDataFieldType;
begin
    result := dftNone;

    case aTableFieldType of
        tftAutoInc:
            result := dftAutoInc;
        tftString:
            result := dftString;
        tftMemo:
            result := dftMemo;
        tftSmallInt:
            result := dftInt16;
        tftInteger:
            result := dftInt32;
        tftBoolean:
            result := dftBoolean;
        tftFloat:
            result := dftFloat;
        tftDateTime:
            result := dftDateTime;
        else
            ASSERT(false, 'FieldType not implemented');
    end;

end;

function TUpdaterTableProvider.CreateStreamableFieldDefFromTableFieldDef(const aTableFieldDef: TTableFieldDef)
    : TStreamableFieldDef;
begin
    result := TStreamableFieldDef.Create();
    result.FieldName := aTableFieldDef.FieldName;
    result.DataSize := aTableFieldDef.FieldLen;
    result.DataType := GetDataFieldTypeFromTableFieldType(aTableFieldDef.FieldType);

end;

function TUpdaterTableProvider.CreateStreamableTableDefFromStructDef(const aStructDef: TTableStructDef)
    : TStreamableTableDef;
var
    xStreamableIndexDef: TStreamableIndexDef;
    xStreamableFieldDef: TStreamableFieldDef;
    x: integer;
begin
    result := TStreamableTableDef.Create();

    // 1. TableName
    result.TableName := aStructDef.Name;

    // 2. Fields
    for x := 0 to aStructDef.FieldDefs.Count - 1 do
    begin
        xStreamableFieldDef := CreateStreamableFieldDefFromTableFieldDef(aStructDef.FieldDefs[x]);
        result.FieldDefs.Add(xStreamableFieldDef);
    end;

    // 3. Index
    xStreamableIndexDef := TStreamableIndexDef.Create();
    // until now options have always been set to primary and unique
    xStreamableIndexDef.IndexOptions.IsPrimary := true;
    xStreamableIndexDef.IndexOptions.IsUnique := true;

    xStreamableIndexDef.IndexFields.AsDelimitedText := aStructDef.IndexDef.IndexText;

    result.IndexDefs.Add(xStreamableIndexDef);
    result.IndexDefs.NameUnnamedIndices();

end;

procedure TUpdaterTableProvider.InstallNewTable(const aDBPath: string; aStructDef: TTableStructDef;
    aNewVersion: TTableRevisionNumber);
var
    xDatabaseProvider: TDatabaseProvider;
    xStreamableTableDef: TStreamableTableDef;
    xTablePath: string;
begin
    xDatabaseProvider := TUpdaterDataProviderFactory.CreateStandardDatabaseProvider(aDBPath);
    try
        xStreamableTableDef := self.CreateStreamableTableDefFromStructDef(aStructDef);
        try
            xDatabaseProvider.CreateTable(xStreamableTableDef);
        finally
            FreeAndNil(xStreamableTableDef);
        end;
    finally
        FreeAndNil(xDatabaseProvider);
    end;
    xTablePath := GetTablePath(aDBPath, aStructDef.Name);
    fVersionInfoAdaptor.WriteVersion(xTablePath, aNewVersion);

end;


end.
