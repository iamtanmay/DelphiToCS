{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : SQL Parser
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.04.08 wl                               TN4058   initial version
  26.05.09 pk  MakeSQLTableName             TN4350   for Ascii, put brackets around name (for names with space or minus signs)
  13.07.09 pk  TAliasTablePath              TN4585.4 moved here from importdataprovider
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                               TN5044      uses StringUtilities
  25.06.10 wl  ConcatOrders                 TN5161   fasst 2 Sortierreihenfolgen zusammen
  25.06.10 wl  ConcatFilters                TN5161   Trim ermöglicht bei der Eingabe von Variablen auch ein Leerzeichen (= kein Filter)
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  16.08.11 wl  ResolveAliasesInSQL          TN5661   von ImportDataProvider hierher verschoben
  10.04.13 wl                               TN6045   uses geändert
  26.09.13 pbp                              TN6261   ORACLE Verbindung hinzufügen
  -------------------------------------------------------------------------------------------------- }

unit SQLParser;


interface


uses
    GeneralTypes;

type
    TEnDBType = (dbNone, dbAscii, dbStandard, dbExcel, dbAccess, dbOracle);

    TAliasTablePath = class
    private const
        STR_BDE_ALIAS_TABLEPATH_DELIMITER = ':';
    public
        class procedure ParseDataLocationAndTableName(aDBType: TEnDBType; aPathName, aTableName: string;
            out aDataLocation, aTableNameOut: string);
        class procedure ParadoxParseDatabaseNameAndTableName(aPathName: string;
            out aDatabaseName, aTableName: string);
        class function ParseAliasTablePath(aTablePath: string; out aAlias, aTableName: string): boolean;
        class function MakeAliasTablePath(aDBType: TEnDBType; aAlias, aTableName: string;
            aWithQuotes: boolean): string;
    end;

    TSQLRec = record
        Select, From, Where, Orderby: string;
    end;

    TSQLParser = class
    private
        class function MakeSQLWhereFromFilter(aFilter: string): string;
        class function MakeSQLOrderByFromFilter(aOrderBy: string): string;
    public
        class function MakeSQLRec(aSelect, aFrom, aWhere, aOrderBy: string): TSQLRec;
        class function ConcatFilters(aFilter1, aFilter2: string): string;
        class function ConcatOrders(aOrderBy1, aOrderBy2: string): string;
        class function MakeSelectStatement(aSQLRec: TSQLRec): string;
        class function MakeSQLTableName(aDBType: TEnDBType; aTableName: string): string;
        class function ParseSQL(aSQL: string): TSQLRec;
        class procedure ParseSQLFromClause(const aFromClause: string; var vPathNames: TStringArray);
        class function ResolveAliasesInSQL(const aSQL: string): string;
        class function ResolveAliasToPath(const aValue: string): string;
    end;


implementation


uses
    SysUtils,
    StrUtils,
    Generics.Collections,
    DataProviderFactory,
    FileUtilities,
    StringUtilities;

{ TSQLParser }

class function TSQLParser.MakeSQLWhereFromFilter(aFilter: string): string;
begin
    result := '';
    if aFilter <> '' then
        result := ' WHERE ' + aFilter;
end;

class function TSQLParser.MakeSQLOrderByFromFilter(aOrderBy: string): string;
begin
    result := '';
    if aOrderBy <> '' then
        result := ' ORDER BY ' + aOrderBy;
end;

class function TSQLParser.ConcatFilters(aFilter1, aFilter2: string): string;
begin
    aFilter1 := Trim(aFilter1);
    aFilter2 := Trim(aFilter2);

    if aFilter1 = '' then
    begin
        result := aFilter2;
        Exit;
    end;
    if aFilter2 = '' then
    begin
        result := aFilter1;
        Exit;
    end;
    result := Format('%s AND %s', [aFilter1, aFilter2]);
end;

class function TSQLParser.ConcatOrders(aOrderBy1, aOrderBy2: string): string;
begin
    aOrderBy1 := Trim(aOrderBy1);
    aOrderBy2 := Trim(aOrderBy2);

    if aOrderBy1 = '' then
    begin
        result := aOrderBy2;
        Exit;
    end;
    if aOrderBy2 = '' then
    begin
        result := aOrderBy1;
        Exit;
    end;
    result := Format('%s,%s', [aOrderBy1, aOrderBy2]);
end;

class function TSQLParser.MakeSelectStatement(aSQLRec: TSQLRec): string;
var
    xSQLWhere, xSQLOrderBy: string;
begin
    xSQLWhere := MakeSQLWhereFromFilter(aSQLRec.Where);
    xSQLOrderBy := MakeSQLOrderByFromFilter(aSQLRec.OrderBy);
    result := Format('SELECT %s FROM %s %s %s', [aSQLRec.Select, aSQLRec.From, xSQLWhere, xSQLOrderBy]);
end;

class function TSQLParser.MakeSQLTableName(aDBType: TEnDBType; aTableName: string): string;
begin
    case aDBType of
        dbExcel:
            result := Format('[%s$]', [aTableName]);
        dbAscii:
            result := Format('[%s]', [aTableName]);
        dbAccess:
            result := aTableName;
        dbOracle:
            { TODO : Weiß noch nicht, was da für ein Eintrag hinein soll? }
            result := Format('[%s]', [aTableName]);
        dbStandard:
            result := Format('"%s"', [aTableName]);
        else
            ASSERT(false, 'Invalid DBType');
    end;
end;

class function TSQLParser.MakeSQLRec(aSelect, aFrom, aWhere, aOrderBy: string): TSQLRec;
begin
    with result do
    begin
        Select := aSelect;
        From := aFrom;
        Where := aWhere;
        OrderBy := aOrderBy;
    end;
end;

function ParseSQLTableClause_ImplicitAs(const aTableClause: string; var vPathName: string): boolean;
const
    STR_DELIM_IMPLICITAS = ' ';
var
    xPosTableAs: integer;
    xName, xAsName: string;
begin
    result := false;

    xPosTableAs := Pos(STR_DELIM_IMPLICITAS, UpperCase(aTableClause));
    if xPosTableAs > 0 then
    begin
        TStringUtilities.SplitStr(aTableClause, STR_DELIM_IMPLICITAS, xName, xAsName);
        if Trim(xAsName) = '' then
            EXIT;
        result := true;
        vPathName := xName;
    end;
end;

function ParseSQLTableClause_As(const aTableClause: string; var vPathName: string): boolean;
const
    STR_DELIM_AS = ' AS';
var
    xPosTableAs: integer;
begin
    result := false;

    xPosTableAs := Pos(STR_DELIM_AS, UpperCase(aTableClause));
    if xPosTableAs > 0 then
    begin
        result := true;
        vPathName := Copy(aTableClause, 1, xPosTableAs - 1);
    end;
end;

procedure ParseSQLTableClause(const aTableClause: string; var vPathName: string);
const
    STR_DELIM_AS = ' AS';
var
    xTableClause: string;
begin
    xTableClause := Trim(aTableClause);
    if not ParseSQLTableClause_As(xTableClause, vPathName) then
        if not ParseSQLTableClause_ImplicitAs(xTableClause, vPathName) then
            vPathName := xTableClause;

end;

class procedure TSQLParser.ParseSQLFromClause(const aFromClause: string; var vPathNames: TStringArray);
const
    STR_NEXT_TABLE_DELIM = ',';
var
    xFromClause, xTableClause: string;
    xPathName: string;
begin
    xFromClause := aFromClause;
    while true do
    begin
        TStringUtilities.SplitStr(xFromClause, STR_NEXT_TABLE_DELIM, xTableClause, xFromClause);
        ParseSQLTableClause(xTableClause, xPathName);
        SetLength(vPathNames, Length(vPathNames) + 1);
        vPathNames[ high(vPathNames)] := xPathName;
        xFromClause := Trim(xFromClause);
        if xFromClause = '' then
            BREAK;
    end;
end;

class function TSQLParser.ParseSQL(aSQL: string): TSQLRec;
const
    STR_SELECT = 'SELECT';
    STR_FROM = 'FROM';
    STR_WHERE = 'WHERE';
    STR_ORDERBY = 'ORDER BY';
var
    xPosStart, xPosEnd, xLenSQL: integer;
    xWhereFound, xOrderByFound: boolean;
    xSQL: string;
begin
    result := MakeSQLRec('', '', '', '');

    xSQL := UpperCase(aSQL);
    xLenSQL := Length(aSQL);

    // Select
    xPosStart := Pos(STR_SELECT, xSQL);
    xPosEnd := Pos(STR_FROM, xSQL);
    if not(xPosStart > 0) and (xPosEnd > 0) then
        EXIT;
    xPosStart := xPosStart + Length(STR_SELECT);
    result.Select := Copy(aSQL, xPosStart, xPosEnd - xPosStart);

    // From
    xWhereFound := false;
    xOrderByFound := false;
    xPosStart := xPosEnd;
    xPosEnd := Pos(STR_WHERE, xSQL);
    if xPosEnd > 0 then
    begin
        xWhereFound := true;
    end
    else
    begin
        xPosEnd := Pos(STR_ORDERBY, xSQL);
        if xPosEnd > 0 then
        begin
            xOrderByFound := true;
        end
        else
        begin
            xPosEnd := xLenSQL + 1;
        end;
    end;
    xPosStart := xPosStart + Length(STR_FROM);
    result.From := Trim(Copy(aSQL, xPosStart, xPosEnd - xPosStart));

    // Where
    if xWhereFound then
    begin
        xPosStart := xPosEnd;
        xPosEnd := Pos(STR_ORDERBY, xSQL);
        if xPosEnd > 0 then
        begin
            xOrderByFound := true;
        end
        else
        begin
            xPosEnd := xLenSQL + 1;
        end;
        xPosStart := xPosStart + Length(STR_WHERE);
        result.Where := Copy(aSQL, xPosStart, xPosEnd - xPosStart);
    end;

    if xOrderByFound then
    begin
        xPosStart := xPosEnd;
        xPosEnd := xLenSQL + 1;
        xPosStart := xPosStart + Length(STR_ORDERBY);
        result.OrderBy := Copy(aSQL, xPosStart, xPosEnd - xPosStart);
    end;
end;

class function TSQLParser.ResolveAliasToPath(const aValue: string): string;
begin
    result := aValue;
    if result = '' then
        EXIT;

    if Pos('\', result) <= 0 then
    begin
        result := TDataProviderFactory.Instance.GetAliasPath(result);
    end;
end;

class function TSQLParser.ResolveAliasesInSQL(const aSQL: string): string;
const
    cAliasBeginDelim = '":';
    cAliasEndDelim = '"';
var
    xBeginPos: integer;
    xEndPos: integer;
    xSQLSoFar: string;
    xFirstSQLPart, xRemainingSQLPart: string;
    xAliasPath, xResolvedPath: string;
    xDataLocation, xTableName: string;
begin
    if (aSQL = '') then
    begin
        EXIT(aSQL);
    end;

    xEndPos := 0;

    xSQLSoFar := aSQL;
    while true do
    begin

        xBeginPos := PosEx(cAliasBeginDelim, xSQLSoFar, xEndPos + 1);
        if xBeginPos <= 0 then
            BREAK;

        xEndPos := PosEx(cAliasEndDelim, xSQLSoFar, xBeginPos + 1);

        xFirstSQLPart := Copy(xSQLSoFar, 1, xBeginPos - 1);
        xRemainingSQLPart := Copy(xSQLSoFar, xEndPos + 1, Length(xSQLSoFar));
        xAliasPath := Copy(xSQLSoFar, xBeginPos, xEndPos - (xBeginPos) + 1);

        TAliasTablePath.ParadoxParseDatabaseNameAndTableName(xAliasPath, xDataLocation, xTableName);

        xDataLocation := ResolveAliasToPath(xDataLocation);
        xResolvedPath := TFileUtilities.ConcatPaths(xDataLocation, xTableName);

        xSQLSoFar := xFirstSQLPart + '"' + xResolvedPath + '"';
        xEndPos := Length(xSQLSoFar);
        xSQLSoFar := xSQLSoFar + xRemainingSQLPart;

    end;

    EXIT(xSQLSoFar);
end;

{ TAliasTablePath }

class function TAliasTablePath.ParseAliasTablePath(aTablePath: string;
    out aAlias, aTableName: string): boolean;
const
    INT_NUM_PARSED_PARTS = 3;
    // 1stPart:2ndPart:3rdPart    2ndPart is Alias, 3rdPart is TableName , 1stpart must be blank
var
    xList: TList<string>;
begin
    result := false;
    xList := TList<string>.Create();
    try
        aTablePath := StringReplace(aTablePath, '"', '', [rfReplaceAll]);
        TStringUtilities.StringToList(xList, aTablePath, STR_BDE_ALIAS_TABLEPATH_DELIMITER);
        if (xList.Count < INT_NUM_PARSED_PARTS) or (xList.Count > INT_NUM_PARSED_PARTS) then
            Exit;
        if xList[0] <> '' then
            Exit;
        aAlias := xList[1];
        aTableName := xList[2];
        result := true;
    finally
        FreeAndNil(xList);
    end;
end;

class function TAliasTablePath.MakeAliasTablePath(aDBType: TEnDBType; aAlias, aTableName: string;
    aWithQuotes: boolean): string;
var
    xQuotes: string;
begin
    result := '';
    xQuotes := '';
    if aWithQuotes then
        xQuotes := '"';
    case aDBType of
        dbStandard:
            result := Format('%s%s%s%s%s%s', [xQuotes, STR_BDE_ALIAS_TABLEPATH_DELIMITER, aAlias,
                STR_BDE_ALIAS_TABLEPATH_DELIMITER, aTableName, xQuotes]);
    end;
end;

class procedure TAliasTablePath.ParadoxParseDatabaseNameAndTableName(aPathName: string;
    out aDatabaseName, aTableName: string);
var
    xAlias, xTableName: string;
begin
    aDatabaseName := '';
    aTableName := '';

    if aPathName = '' then
        Exit;

    // if it contatins an alias
    if ParseAliasTablePath(aPathName, xAlias, xTableName) then
    begin
        aDatabaseName := xAlias;
        aTableName := xTableName;
        Exit;
    end;

    // if it contains a directory
    aDatabaseName := ExtractFilePath(aPathName);
    aTableName := ExtractFileName(aPathName);
end;

class procedure TAliasTablePath.ParseDataLocationAndTableName(aDBType: TEnDBType;
    aPathName, aTableName: string; out aDataLocation, aTableNameOut: string);
begin
    case aDBType of
        dbExcel:
            begin
                aTableNameOut := aTableName;
                aDataLocation := aPathName;
            end;
        dbAscii:
            begin
                aTableNameOut := ExtractFileName(aPathName);
                aDataLocation := ExtractFilePath(aPathName);
            end;
        dbAccess:
            begin
                aTableNameOut := aTableName;
                aDataLocation := aPathName;
            end;
        dbOracle:
            begin
                // Pathname sollte dem Service Namen von ORACLE entsprechen
                aTableNameOut := aTableName;
                aDataLocation := aPathName;
            end;
        dbStandard:
            ParadoxParseDatabaseNameAndTableName(aPathName, aDataLocation, aTableNameOut);
        else
            ASSERT(false, 'Invalid DBType');
    end;
end;


end.
