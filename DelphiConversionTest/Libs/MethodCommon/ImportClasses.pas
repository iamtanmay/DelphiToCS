unit ImportClasses;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Various classes and functions that work with TQuery and TAdoQuery
  The functions provide an encapsulation for the Query object that is required given
  the dbType
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.04.04 pk                               TN1880   initial version
  29.04.04 pk                               TN1880   changes for cachedupdates
  29.04.04 pk  ImportRow                    TN1880   If variable, Replace only variable by value, not entire field
  04.05.04 pk  ImportRow                    TN1880   Support for replacing same variable in multiple fields
  17.05.04 pk  TImportAdoQuery.Create       TN1880   Coinitialize called
  18.05.04 pk  DetermineConncectionString   TN1944   Raises exception if excel workbook is already open
  03.08.04 pk                               TN2066   New: STR_IMPORT_SOURCEFIELD_CONSTVAL_SYMBOL
  09.09.04 pk  ParseSQL                     TN2131   New : Parses a simple SQL statement
  09.09.04 pk  OpenQuery                    TN2131   If SQLMode and an additional filter is given, the SQL
  is parsed and the filter is added to the SQL
  22.10.04 pk  Import                       TN2188   MoveBy no longer called explicitly
  03.11.05 wl  IsExcelWorkbookOpen          TN2725   --> ExcelOLEManager
  03.11.05 wl  TFileInfo                    TN2725   kann eine temopräre txt-Datei erstellen, für den Fall dass eine Ascii-Datei eine andere Extension hat
  01.12.05 pk  TImportQuery                 TN2820   contains code that was previously in TImports functions
  01.12.05 pk  OpenQuery                    TN2820   code moved to TImportQuery classes
  01.12.05 pk  TFileInfo.Username, Password TN2820   needed for oracle connection via BDE
  09.12.05 wl  TFileInfo.FileExtensionFitsToDBType   TN2725.1  Datei wird nur noch kopiert, wenn Extension wirklich nicht .txt ist
  12.12.05 pk  PrepareConnection            TN2854   Create a new session
  12.12.05 pk  IDataset                     TN2847   New : An interface for TDataset - needed to prevent problems with free not being called
  14.12.05 pk  PrepareConnection            TN2854   Use TImportQuery.ParsedTableName instead of fileInfo.ParsedTableName
  19.01.05 pk  DetermineSourceFieldValue    TN2889   Now returns the default value if MustFind=false and the field is not found in the source
  03.02.06 wl  DetermineSourceFieldValue    TN2927   '#TotalRecords' importiert die Anzahl der Records der Import-Datei
  03.02.06 wl  DetermineSourceFieldValue    TN2927   '#CurrentRecord' importiert die aktuelle Position der Record-Cursors
  03.02.06 wl  ImportColumnDef              TN2927   aCurrentRecNo als Parameter
  03.02.06 wl  ImportRow                    TN2927   aCurrentRecNo als Parameter
  18.04.06 pk                               TN3001   use a database session if sessionname is not blank
  09.10.06 wl  TImports                     TN3352   mehr Methoden sind jetzt public
  14.10.07 wl  IDataset                     TN3566   Interface abgeschafft, TBasicDataset ist jetzt Basisklasse
  14.10.07 wl  TImports.WriteDatasetToGrid  TN3566   benutzt TBasicDataset, Free am Schluss
  14.10.07 wl  TImports.Import              TN3566   benutzt TBasicDataset, Free am Schluss
  13.03.07 wl  TFileInfo.Create             TN3634   Delimiter TAB wird zu #9
  07.08.07 wl                               TN3811.3  uses geändert
  09.11.07 pk  TFileInfo                    TN3922   --> ImportDataProvider
  09.11.07 pk                               TN3922   all Table/Query specific functions --> ImportDataProvider
  27.11.07 wl  TImports.CreateQuery         TN3908   SystemDataPassword wird an TBDEQuery übergeben
  08.01.08 wl  AssignSourceValueToTarget    TN3972   interne Änderung
  04.04.08 wl                               TN4058    uses geändert
  17.04.08 wl                               TN4067   TBasicDataset replaced by TImportQuery
  02.06.08 wl  TImports.ImportColumnDef     TN4132   Now the value is always written into the target field
  19.09.08 pk  DetermineSourceFieldIndex    TN4215   use TryStrToInt to get column number (avoid raising exception)
  09.02.09 ts  ChangeDataTypes-functions    TN4346   ChangeDatatypes functions added
  20.05.09 wl  DetermineSourceFieldValue    TN4567   Ein leeres Feld wird jetzt immer durch den Default-Wert ersetzt, wenn Requred = false
  27.05.09 wl  TImports.Import              TN4476   liest ImportDataTypes aus Tabelle (wenn vorhanden)
  27.05.09 wl  TImports.CreateAnyDataSet    TN4476   Textfiles werden erst einmal ohne Schema.ini gelesen um geänderte Feldnamen zu erkennen
  06.07.09 pk                               TN4585.4 Some functions commented out for now. Must be reimplemented
  13.07.09 pk  CreateQuery                  TN4585.4 just call TImportQuery.Create
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  19.08.09 wl  CheckOtherDecimalSeparator   TN4227   Import von Float-Werten funktioniert jetzt auch bei deutscher Zeichensetzung
  26.10.09 wl  WriteFieldInfosToSchemaIni   TN4831   IConfigurationSet replaces TIniFile
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  20.11.09 pk  CreateAnyDataset             TN4895   if open fails free the query and return nil
  13.04.10 wl                               TN5044   uses StringUtilities
  20.05.10 wl                               TN5116   AppInstanceDataAdaptorCommon geändert
  20.05.10 wl                               TN5117   uses ControlUtils
  21.07.10 pk                               TN5203   AppInstanceAppDataAdaptorCommon renamed to AppInstanceAppData
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  19.10.10 pk                               TN5305   changes needed for CoreClient/Server
  14.12.10 wl  CheckOtherDecimalSeparator   TN5411   benutzt TFormatUtils statt nut DecimalSeparator
  12.04.11 ts                               TN5548   new: SkipLines setting to skip the first few lines of import file (if Excel-File)
  10.04.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Grids,
    SQLParser,
    ImportDataAdaptor,
    ImportColDefDataAdaptor,
    ImportDataProvider,
    ImportDataTypesDataAdaptor;

const
    INT_DATASET_ROWOFFSET_MIN = 0;
    STR_IMPORT_SOURCEFIELD_CONSTVAL_SYMBOL = '=';
    STR_IMPORT_SOURCEFIELD_TOTALRECORDS = '#TotalRecords';
    STR_IMPORT_SOURCEFIELD_CURRENTRECORD = '#CurrentRecord';

type
    TImports = class
    private
        class function CreateQuery(aFileInfo: TFileInfo): TImportQuery;
        // import row sub-functions
        class procedure ImportColumnDef(const aTargetDataset, aSourceDataset: TImportQuery;
            aColDef: TImportColDefRec; aMustImportAllDefinedCols: boolean; aCurrentRecNo: integer);
        class function DetermineTargetFieldIndex(const aTargetDataset: TImportQuery; aTargetColName: string;
            aStartAfterFieldIndex: integer; out aIsVariableName: boolean): integer;
        class procedure CheckOtherDecimalSeparator(var aValue: variant);

    public
        class function DetermineSourceFieldValue(const aSourceDataset: TImportQuery;
            const aSourceColName: string; aMustFind: boolean; aDefaultValue: variant;
            aCurrentRecNo: integer): variant;
        class procedure AssignSourceValueToTarget(aSourceValue: string; var vTargetString: string;
            aTargetColName: string);

        class function CreateAnyDataSet(aDBType: TEnDBType; aSessionName, aPathName, aTableName: string;
            aDelimiter: string; aHasHeader: boolean; aRowOffset: integer; aFilter, aOrderBy: string;
            const aUsername, aPassword: string; aSQL: string; aReadOnly: boolean; aCachedUpdates: boolean;
            aDataTypesRecArray: TDataTypesRecArray; aSkipLines: integer): TImportQuery;

        class function CreateTargetParadoxDataset(aSessionName: string; aTargetName: string;
            aCachedUpdates: boolean): TImportQuery;

        class procedure WriteDataSetToGrid(const aDataSet: TImportQuery; aGrid: TStringGrid);
        class procedure WriteFieldInfoToGrid(const aDataset: TImportQuery; aGrid: TStringGrid);

        class function GetAllFieldNamesOfTable(aDBType: TEnDBType; aPathName: string; aTableName: string;
            aDelimiter: string; aHasHeaderRow: boolean; const aUsername, aPassword: string; aSQL: string)
            : TArray<string>;
        class function Import(aImportDef: TImportDefRec; const aSessionName: string): integer;
        class procedure ImportRow(const aTargetDataset, aSourceDataset: TImportQuery;
            aImportColDefs: TImportColDefRecArray; aMustImportAllDefinedCols: boolean;
            aCurrentRecNo: integer);
        // --------------------------------------------------------------------------------------------------------------
        // ChangeDataTypes functions
        // --------------------------------------------------------------------------------------------------------------
        class function DataTypesToRecArray(aDataset: TImportQuery): TDataTypesRecArray;
        class procedure WriteFieldInfosToSchemaIni(const aPathName: string;
            aDataTypesRecArray: TDataTypesRecArray);
        class function CaseStr(aValue: string; aArgs: array of string): integer;
        class function ConvertFieldTypeToMSJetDataTypes(aDataType: string): string;
    end;


implementation


uses
    SysUtils,
    Generics.Collections,
    Variants,
    GeneralTypes,
    ConfigurationFile,
    ComObj,
    ExcelOleManager {needed for IsExcelWorkbookOpen} ,
    StringUtilities,
    DataProvider,
    ImportFileDefDataAdaptor,
    LogManager,
    DataProviderFactory,
    ControlUtils;

{ TImports }

class function TImports.CreateQuery(aFileInfo: TFileInfo): TImportQuery;
begin
    ASSERT(aFileInfo.DBType <> dbNone, 'Invalid DBType');
    result := TImportQuery.Create(aFileInfo, TDataProviderFactory.Instance.SystemDataPassword);
    // result := nil;
    // case aFileInfo.DBType of
    // dbParadoxdbAscii, dbExcel, dbAccess : result := TImportQuery.Create( aFileInfo, , TAppInstanceDataAdaptorCommon.Instance().SystemDataPassword );
    // dbParadox : result := TImportQuery.Create( aFileInfo, xSystemPassword );
    // else ASSERT( false, 'Invalid DBType' );
    // end;
end;

class function TImports.CreateAnyDataSet(aDBType: TEnDBType; aSessionName, aPathName, aTableName: string;
    aDelimiter: string; aHasHeader: boolean; aRowOffset: integer; aFilter, aOrderBy: string;
    const aUsername, aPassword: string; aSQL: string; aReadOnly: boolean; aCachedUpdates: boolean;
    aDataTypesRecArray: TDataTypesRecArray; aSkipLines: integer): TImportQuery;
// Excel   : aPathname not used
// aTableName is the sheetname
// Ascii   : given a aPathname, the filename is extracted from it.
// aTableName not used
// Access  : aPathname not used
// aTableName is a name of a table within an Access database
// Paradox : aSQL is an SQL statement.  If aSQL is blank then aPathname is used.
// aPathname
var
    xQuery: TImportQuery;
    xFileInfo: TFileInfo;
    xCurrentDataTypes: TDataTypesRecArray;
    x1, x2: integer;
begin
    try
        // Sonderfall: Textfile mit Schema.ini - Schema.ini aktualisieren, falls sich die Feldnamen geändert haben
        if (aDBType = dbAscii) and (Length(aDataTypesRecArray) > 0) then
        begin
            xFileInfo := TFileInfo.Create(aDBType, aSessionName, aPathName, aTableName, aDelimiter,
                aHasHeader, aFilter, aOrderBy, aRowOffset, aUserName, aPassword, aSQL, aSkipLines);
            xQuery := CreateQuery(xFileInfo);
            TImports.WriteFieldInfosToSchemaIni(xFileInfo.PathName, nil);
            xQuery.Open(aReadOnly, aCachedUpdates);
            try
                xCurrentDataTypes := TImports.DataTypesToRecArray(xQuery);
                for x1 := 0 to high(xCurrentDataTypes) do
                begin
                    for x2 := 0 to high(aDataTypesRecArray) do
                    begin
                        if (xCurrentDataTypes[x1].Column = aDataTypesRecArray[x2].Column) then
                            xCurrentDataTypes[x1].DataType := aDataTypesRecArray[x2].DataType;
                        // definierte Datentypen ändern
                    end;
                end;
            finally
                xQuery.Free;
            end;
        end;

        // normales öffnen der Query
        xFileInfo := TFileInfo.Create(aDBType, aSessionName, aPathName, aTableName, aDelimiter, aHasHeader,
            aFilter, aOrderBy, aRowOffset, aUserName, aPassword, aSQL, aSkipLines);
        xQuery := CreateQuery(xFileInfo);
        try
            if aDBType = dbAscii then
                TImports.WriteFieldInfosToSchemaIni(xFileInfo.PathName, xCurrentDataTypes);

            xQuery.Open(aReadOnly, aCachedUpdates);
        except
            FreeAndNil(xQuery);
            raise;
        end;

        result := xQuery;
    except
        on E: exception do
        begin
            raise Exception.CreateFmt('%s -> %s', [TLanguageString.Read('Could not open dataset for [{0}]',
                'Datensatz für [{0}] konnte nicht geöffnet werden', [aPathName]), E.Message]);
        end;
    end;
end;

class procedure TImports.WriteFieldInfoToGrid(const aDataset: TImportQuery; aGrid: TStringGrid);
const
    INT_NUM_FIXED_ROWS = 1;
    INT_FIELD_INFO_COL_NAME = 0;
    INT_FIELD_INFO_COL_TYPE = 1;
    INT_FIELD_INFO_COL_REQUIRED = 2;
    INT_FIELD_INFO_COL_UNITS = 3;
var
    i: integer;
    xField: TDataField;
    xRequired: string;
    xRowIndex: integer;
    xHeaders: string;
    xHeadersList: TList<string>;
begin
    aGrid.RowCount := 0;
    aGrid.RowCount := aDataset.FieldCount + INT_NUM_FIXED_ROWS;
    aGrid.FixedRows := INT_NUM_FIXED_ROWS;
    aGrid.ColCount := 4;

    // Add header row
    xHeaders := TLanguageString.Read('Name, Type, Required, Units', 'Name, Typ, Erforderlich, Units');
    xHeadersList := TList<string>.Create();
    try
        TStringUtilities.StringToList(xHeadersList, xHeaders, ',');
        TControlUtils.AddValuesToGridRow(xHeadersList.ToArray, aGrid, 0);
    finally
        FreeAndNil(xHeadersList);
    end;

    // add a row for each field
    xRowIndex := INT_NUM_FIXED_ROWS;
    for i := 0 to aDataset.FieldCount - 1 do
    begin
        xField := aDataset.Fields[i];
        aGrid.Cells[INT_FIELD_INFO_COL_NAME, xRowIndex] := xField.FieldName;
        aGrid.Cells[INT_FIELD_INFO_COL_TYPE, xRowIndex] := TDataProvider.GetFieldTypeAsStr(xField.DataType);
        xRequired := 'No';
        if xField.Required then
            xRequired := 'Yes';
        aGrid.Cells[INT_FIELD_INFO_COL_REQUIRED, xRowIndex] := xRequired;
        aGrid.Cells[INT_FIELD_INFO_COL_UNITS, xRowIndex] :=
            IntToStr(TDataProvider.GetFieldDataSize(xField.DataType, xField.DataSize));
        Inc(xRowIndex);
    end;

end;

class procedure TImports.WriteDataSetToGrid(const aDataSet: TImportQuery; aGrid: TStringGrid);
const
    INT_NUMFIXED_ROWS = 1;

var
    xDatasetTotalRows, xDatasetTotalCols: integer;
    xRowIndex: integer;

    procedure WriteDatasetFieldNamesToGrid();
    var
        xDatasetCol: integer;
    begin
        for xDatasetCol := 0 to xDatasetTotalCols - 1 do
        begin
            aGrid.Cells[xDatasetCol, xRowIndex] := aDataset.Fields[xDatasetCol].FieldName;
        end;
    end;
    procedure WriteCurrentDatasetRowToGrid();
    var
        xDatasetCol: integer;
    begin
        for xDatasetCol := 0 to xDatasetTotalCols - 1 do
        begin
            aGrid.Cells[xDatasetCol, xRowIndex] := aDataset.Fields[xDatasetCol].AsString;
        end;
    end;

begin
    // Determine the rowcount and expand grid's rowcount to the needed number
    xDatasetTotalRows := aDataSet.RecordCount;
    if xDatasetTotalRows = 0 then
        Exit;

    aGrid.RowCount := INT_NUMFIXED_ROWS + xDatasetTotalRows;
    // add 1 because for TStringGrid RowCount must be greater than FixedRows
    aGrid.FixedRows := INT_NUMFIXED_ROWS;

    // Determine and define the column count
    xDatasetTotalCols := aDataSet.FieldCount;
    aGrid.ColCount := xDatasetTotalCols;

    xRowIndex := 0;
    WriteDatasetFieldNamesToGrid();

    xRowIndex := INT_NUMFIXED_ROWS;
    // aDataset.MoveBy(aStartAtRow);
    while not aDataset.Eof do
    begin
        WriteCurrentDatasetRowToGrid();
        aDataset.Next;
        Inc(xRowIndex);
    end;
end;

class function TImports.GetAllFieldNamesOfTable(aDBType: TEnDBType; aPathName: string; aTableName: string;
    aDelimiter: string; aHasHeaderRow: boolean; const aUsername, aPassword: string; aSQL: string)
    : TStringArray;
var
    xDataSet: TImportQuery;
begin
    xDataSet := CreateAnyDataSet(aDBType, '', aPathName, aTableName, aDelimiter, aHasHeaderRow,
        INT_DATASET_ROWOFFSET_MIN, '', '', aUsername, aPassword, aSQL, true, false, nil, 0);
    try
        result := xDataSet.GetFieldNames();
    finally
        xDataSet.Free;
    end;
end;

class function TImports.CreateTargetParadoxDataset(aSessionName: string; aTargetName: string;
    aCachedUpdates: boolean): TImportQuery;
begin
    result := CreateAnyDataset(dbStandard, aSessionName, aTargetName, '', '', false,
        INT_DATASET_ROWOFFSET_MIN, '', '', '', '', '', false, aCachedUpdates, nil, 0);
end;

class function TImports.Import(aImportDef: TImportDefRec; const aSessionName: string): integer;
var
    xSourceDataset: TImportQuery;
    xTargetDataset: TImportQuery;
    xSourceFileDef: TFileDefRec;
    xTargetOrigRecCount, xCurrentRecNo: integer;
    xDBType: TEnDBType;
    xDataTypesRecArray: TDataTypesRecArray;
begin
    result := 0;
    xSourceFileDef := aImportDef.SourceFileDef;
    xDBType := TEnDBType(xSourceFileDef.DBType);
    xDataTypesRecArray := TImportDataTypesDataAdaptor.ImpTableToDataTypeRec(xSourceFileDef.Name);
    // try to obtain the source dataset
    xSourceDataset := CreateAnyDataset(xDBType, '', xSourceFileDef.PathName, xSourceFileDef.TableName,
        xSourceFileDef.Delimiter, xSourceFileDef.HasHeader, xSourceFileDef.RowOffset, xSourceFileDef.Filter,
        xSourceFileDef.OrderBy, xSourceFileDef.UserName, xSourceFileDef.Password, xSourceFileDef.SQL, true,
        false, xDataTypesRecArray, xSourceFileDef.SkipLines);
    try
        // try to obtain the destination dataset
        xTargetDataset := CreateTargetParadoxDataset(aSessionName, aImportDef.TargetName, true);

        try
            if not Assigned(xTargetDataset) then
                EXIT;
            // if not (xTargetDataset is TDBDataSet) then Exit;
            xTargetOrigRecCount := xTargetDataset.RecordCount; // save the original record count

            xCurrentRecNo := 1;
            while not xSourceDataset.Eof do
            begin
                xTargetDataset.Append;
                ImportRow(xTargetDataset, xSourceDataset, aImportDef.ImportColDefs, true, xCurrentRecNo);
                xTargetDataset.Post;
                xSourceDataset.Next;
                inc(xCurrentRecNo);
            end;
            result := xTargetDataset.RecordCount - xTargetOrigRecCount;
            // calculate number of affected records
            // if import was successful, apply the updates
            if result > 0 then
            begin
                xTargetDataset.DatasetApplyUpdates();
            end;
        finally
            xTargetDataset.Free;
        end;
    finally
        xSourceDataset.Free;
    end;
end;

// Spezialfall Deutschland: Komma statt Punkt, dadurch werden sonst die Dezimalzahlen nicht erkannt
class procedure TImports.CheckOtherDecimalSeparator(var aValue: variant);
var
    xTempString: string;
    xCorrectValue: double;
    xPos, xErr: integer;
    xSettings: TFormatSettings;
begin
    xSettings := TFormatUtils.GetLocaleSettings;

    if (xSettings.DecimalSeparator <> '.') // Z.B. deutsche Einstellungen: Komma statt Punkt
        and VarIsStr(aValue) then
    begin // es wurde als String gelesen
        xPos := Pos(xSettings.DecimalSeparator, aValue);

        // Es wurde ein Komma gefunden
        if (xPos > 0) then
        begin
            xTempString := aValue;

            xTempString[xPos] := '.';
            Val(xTempString, xCorrectValue, xErr);

            // kein Fehler bei der Konvertierung
            if (xErr = 0) then
            begin
                aValue := xCorrectValue;
            end;
        end;
    end;
end;

class function TImports.DetermineSourceFieldValue(const aSourceDataset: TImportQuery;
    const aSourceColName: string; aMustFind: boolean; aDefaultValue: variant; aCurrentRecNo: integer)
    : variant;
var
    xLength: integer;
    xSourceColNum: integer;
    xSourceColNameIsNum: boolean;
    xIndex: integer;
begin
    result := null;
    xLength := Length(aSourceColName);
    ASSERT(xLength > 0, 'Source: Column name cannot be blank');

    if aSourceColName[1] = STR_IMPORT_SOURCEFIELD_CONSTVAL_SYMBOL then
    begin
        result := Copy(aSourceColName, 2, xLength); // set vValue to constant value
        EXIT;
    end;

    if (aSourceColName = STR_IMPORT_SOURCEFIELD_CURRENTRECORD) then
    begin
        result := aCurrentRecNo; // set vValue to the current record number
        EXIT;
    end;

    if (aSourceColName = STR_IMPORT_SOURCEFIELD_TOTALRECORDS) then
    begin
        result := aSourceDataset.RecordCount; // set vValue to the total number of records
        EXIT;
    end;

    // is sourcename a fieldnumber or a fieldname
    xSourceColNameIsNum := false;
    xSourceColNum := -1;

    if TryStrToInt(aSourceColName, xSourceColNum) then
    begin
        xSourceColNum := xSourceColNum - 1;
        xSourceColNameIsNum := true;
    end;
    if xSourceColNameIsNum then
    begin // if a field NUMBER
        if (xSourceColNum < 0) or (xSourceColNum >= aSourceDataset.FieldCount) then
        begin
            if aMustFind then
            begin
                raise Exception.Create(TLanguageString.Read('Source: Invalid column number [{0}]',
                    'Quelle: Ungültige Spaltenummer [{0}]', [xSourceColNum]))
            end
            else
            begin
                result := aDefaultValue;
                EXIT;
            end;
        end;
        xIndex := xSourceColNum;
    end
    else
    begin // if a field NAME
        xIndex := aSourceDataset.Fields.IndexOf(aSourceColName);
        if xindex < 0 then
        begin
            if aMustFind then
            begin
                raise Exception.Create(TLanguageString.Read('Source: Unknown column name [{0}]',
                    'Quelle: Unbekannter Spaltename [{0}]', [aSourceColName]))
            end
            else
            begin
                result := aDefaultValue;
                EXIT;
            end;
        end;
    end;

    // Wert des Feldes lesen
    result := aSourceDataset.Fields[xIndex].Value;

    // Spezialfall Deutschland: Komma statt Punkt, dadurch werden sonst die Dezimalzahlen nicht erkannt
    CheckOtherDecimalSeparator(result);

    // wenn das Feld leer (null) ist, kann der Wert durch den Defaultwert ersetzt werden
    if VarIsNull(result) and (not aMustFind) then
    begin
        result := aDefaultValue;
    end;
end;

class function TImports.DetermineTargetFieldIndex(const aTargetDataset: TImportQuery; aTargetColName: string;
    aStartAfterFieldIndex: integer; out aIsVariableName: boolean): integer;
// if the aTargetColName is not a field name in aTargetDataset, then the field index of that field will be returned
// otherwise, start looking through the fields of aTargetDataset starting at index aStartAfterFieldIndex to see
// if the contents of a field matches aTargetColName and if a match is found return that fieldindex and
// set aIsVariableName to true
var
    i: integer;
    xTargetValAsStr: string;
begin
    aIsVariableName := false;
    // Determine Target Field
    result := aTargetDataset.Fields.IndexOf(aTargetColName);
    if (result >= 0) then
        Exit;
    // Maybe TargetColName is a variable name in one of the fields
    aTargetColName := UpperCase(aTargetColName);
    for i := (aStartAfterFieldIndex + 1) to aTargetDataset.FieldCount - 1 do
    begin
        try
            xTargetValAsStr := UpperCase(aTargetDataset.Fields[i].AsString)
        except
            Continue;
        end;
        // check the contents of the field to see if any substring matches aTargetColName
        if Pos(aTargetColName, xTargetValAsStr) = 0 then
            Continue;
        aIsVariableName := true;
        result := i;
        Exit;
    end;
    if (result < 0) then
        raise Exception.Create(TLanguageString.Read('Target: Invalid column/variable name [{0}]',
            'Ziel: Ungültige Spalte/Variablename [{0}]', [aTargetColName]));
end;

class procedure TImports.AssignSourceValueToTarget(aSourceValue: string; var vTargetString: string;
    aTargetColName: string);
begin
    vTargetString := StringReplace(vTargetString, aTargetColName, aSourceValue, [rfReplaceAll, rfIgnoreCase]);
end;

class procedure TImports.ImportColumnDef(const aTargetDataset, aSourceDataset: TImportQuery;
    aColDef: TImportColDefRec; aMustImportAllDefinedCols: boolean; aCurrentRecNo: integer);
var
    xTargetColName, xSourceColName: string;
    xTargetField: TDataField;
    xSourceValue: variant;
    xSourceAsStr: string;
    xTargetFieldIndex: integer;
    xIsVariableName: boolean;
    xTargetFoundAtleastOnce: boolean;
    xTargetString: string;
begin
    xSourceColName := aColDef.SourceCol;
    xSourceValue := DetermineSourceFieldValue(aSourceDataset, xSourceColName, not aColDef.SourceNotRequired,
        aColDef.SourceDefault, aCurrentRecNo);

    xTargetFieldIndex := -1;
    xTargetFoundAtleastOnce := false;
    while true do
    begin
        xTargetColName := aColDef.TargetCol;
        try
            xTargetFieldIndex := DetermineTargetFieldIndex(aTargetDataset, xTargetColName, xTargetFieldIndex,
                xIsVariableName);
        except
            // if the target was found in the last iteration Exit to avoid raising an exception
            // OR if finding a target is NOT mandatory, Exit to ignore this column definition
            if xTargetFoundAtleastOnce or (not aMustImportAllDefinedCols) then
                Exit;
            raise; // otherwise re-raise the exception if all target columns MUST be found
        end;
        xTargetFoundAtleastOnce := true;
        xTargetField := aTargetDataset.Fields[xTargetFieldIndex];

        try
            // try to write the xSourceValue to the TargetField
            if (xIsVariableName) then
            begin
                xTargetString := xTargetField.AsString;
                AssignSourceValueToTarget(xSourceValue, xTargetString, xTargetColName);
                xTargetField.AsString := xTargetString;
            end
            else
            begin
                xTargetField.Value := xSourceValue;
            end;
        except
            on E: Exception do
            begin
                xSourceAsStr := '';
                try
                    xSourceAsStr := Format('%s', [xSourceValue]);
                except
                end;
                raise Exception.Create(TLanguageString.
                    Read('The value [{0}] from field [{1}] could not be written to field [{2}]',
                    'Den Wert [{0}] vom Feld [{1}] konnte nicht zum Feld [{2}] geschrieben werden',
                    [xSourceAsStr, xSourceColName, xTargetColName]) + #13#10 + E.Message);
            end;
        end;
        // if a variablename was found in DetermineTargetFieldIndex then there is a chance that the
        // variable will appear in the contents of another field so DO NOT Break out of the loop yet.
        // Otherwise if xIsvariableName is set to false in DetermineTargetFiledIndex then
        if not xIsVariableName then
            Break;
    end;
end;

class procedure TImports.ImportRow(const aTargetDataset, aSourceDataset: TImportQuery;
    aImportColDefs: TImportColDefRecArray; aMustImportAllDefinedCols: boolean; aCurrentRecNo: integer);

// if aMustImportAllDefinedCols is false : If a target column defined in aImportColDefs is not found in
// the target dataset, that column definition will be ignored, and no error will occur.
var
    i: integer;
begin
    for i := 0 to high(aImportColDefs) do
    begin
        ImportColumnDef(aTargetDataset, aSourceDataset, aImportColDefs[i], aMustImportAllDefinedCols,
            aCurrentRecNo);
    end;
end;

class function TImports.DataTypesToRecArray(aDataset: TImportQuery): TDataTypesRecArray;
var
    i: integer;
    xField: TDataField;
begin
    result := nil;
    SetLength(result, aDataset.FieldCount);
    for i := 0 to aDataset.FieldCount - 1 do
    begin
        xField := aDataset.Fields[i];
        result[i].Column := xField.FieldName;
        result[i].DataType := TDataProvider.GetFieldTypeAsStr(xField.DataType);
    end;
end;

class procedure TImports.WriteFieldInfosToSchemaIni(const aPathName: string;
    aDataTypesRecArray: TDataTypesRecArray);
var
    i: integer;
    xIniFile: IConfigurationSet;
    xSchemaFileName, xSectionName: string;
begin
    xSchemaFileName := ExtractFileDir(aPathName) + '\schema.ini';
    xSectionName := ExtractFileName(aPathName);
    xIniFile := TConfigurationFile.Create(xSchemaFileName);
    xIniFile.Open(false);
    try
        xIniFile.EraseSection(xSectionName);
        for i := 0 to length(aDataTypesRecArray) - 1 do
        begin
            xIniFile.WriteString(xSectionName, ('COL' + IntToStr(i + 1)), '"' + aDataTypesRecArray[i].Column +
                '" ' + self.ConvertFieldTypeToMSJetDataTypes(aDataTypesRecArray[i].DataType));
        end;
    finally
        xIniFile.Close;
    end;
end;

class function TImports.CaseStr(aValue: string; aArgs: array of string): integer;
begin
    for result := low(aArgs) to high(aArgs) do
    begin
        if SameText(aValue, aArgs[result]) then
            System.BREAK;
    end;
end;

class function TImports.ConvertFieldTypeToMSJetDataTypes(aDataType: string): string;
begin
    Result := 'Text';
    case self.CaseStr(aDataType, ['AutoInc', 'String', 'Int16', 'Int32', 'Boolean', 'Float64', 'BCD value',
        'Date+Time']) of
        1, 6:
            result := 'Text';
        2:
            result := 'short';
        0, 3:
            result := 'long';
        4:
            result := 'Bit';
        5:
            result := 'Double';
        7:
            result := 'datetime';
    end;
end;


end.
