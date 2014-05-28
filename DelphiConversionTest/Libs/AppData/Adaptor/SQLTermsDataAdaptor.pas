{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Data Adaptor for table SQLTERMS.DB
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.04.07 wl                                TN3547  Initial Revision
  19.04.07 wl                                TN3547   Changes for updatemanager
  03.05.07 wl  gmDefineQueryType             TN3669  Analysiert, ob es sich um ein SELECT oder UPDATE/DELETE/INSERT handelt
  24.06.07 wl  ReadSQLTermData,WriteSQLTermRec  TN3730  es wird grundsätzlich nicht mehr Case-sensitive gesucht
  25.07.07 pk  UpdateVersion1                TN3804   Exit if table already contains records
  02.10.07 wl                                TN3811.5 benutzt self.Query statt fQuery
  09.11.07 pk                                TN3921   Changes for updatemanager
  09.11.07 pk                                TN3922   Dataset changed to DataProvider
  23.09.08 wl                                TN4236   Vereinigung mit SQLTermsFieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  04.03.09 pk  AppendName                    TN4446   New
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                                TN5044   uses geändert
  17.06.10 pk                                TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  -------------------------------------------------------------------------------------------------- }

unit SQLTermsDataAdaptor;


interface


uses
    GeneralTypes,
    DataProvider,
    QueryDataAdaptor;

type
    TSQLTermRec = record
        name: string;
        Term: string;
        Defaultargs: string;
        Comment: string;
    end;

    TSQLTermsDataAdaptor = class(TQueryDataAdaptor)
    private
        class procedure ReadRecFromDataset(aDataset: TDataProvider; out oRec: TSQLTermRec);
    protected
        function GetNameField(): string; override;
    public
        constructor Create();

        function ReadSQLTermData(aSQLTermName: string; out oRec: TSQLTermRec): boolean;
        class procedure WriteRecAtCursor(aDataset: TDataProvider; const aRec: TSQLTermRec; aAppend: boolean);
        procedure WriteSQLTermRec(const aRec: TSQLTermRec);
        procedure AppendName(const aSQLTermName: string);
    end;

    TSQLTextType = (sttNoQuery, sttSelectQuery, sttChangeQuery);

    TSQLTermUtils = class
    private
        class function ArrayToText(const aLines: TStringArray): string;
    public
        class function GetPureSQLText(aText: string): string;
        class function GetStringFromStrings(const aLines: TStringArray): string;
        class function GetStringsFromString(const aText: string): TStringArray;
        class function DefineQueryType(const aLines: TStringArray): TSQLTextType;
    end;



    // ##################################################################################################


implementation


uses
    Variants,
    SysUtils,
    Classes;

const
    STR_SQLTERMS_TBL = 'SQLTERMS';

    STR_SQLTERMS_FLD_NAME = 'NAME';
    STR_SQLTERMS_FLD_TERM = 'TERM';
    STR_SQLTERMS_FLD_COMMENT = 'COMMENT';
    STR_SQLTERMS_FLD_PARAMSDEFAULT = 'PARAMSDEFAULT';
    STR_SQLTERMS_FLD_PARAMSDESCRIPTION = 'PARAMSDESCRIPTION';
    STR_SQLTERMS_FLD_PARAMSMIN = 'PARAMSMIN';
    STR_SQLTERMS_FLD_PARAMSMAX = 'PARAMSMAX';
    STR_SQLTERMS_FLD_PARAMSFORMAT = 'PARAMSFORMAT';
    STR_SQLTERMS_FLD_PARAMSPICKLIST = 'PARAMSPICKLIST';

    STR_SQLTERMS_INDEX_FIELDS = STR_SQLTERMS_FLD_NAME;

    STR_SQL_FROM = ' FROM ' + STR_SQLTERMS_TBL;
    STR_SQL_SELECT_ALL = 'SELECT * ' + STR_SQL_FROM;
    STR_SQL_SELECT_DISTINCT_ALL = 'SELECT DISTINCT ' + STR_SQLTERMS_FLD_NAME + STR_SQL_FROM;

class function TSQLTermUtils.GetStringFromStrings(const aLines: TStringArray): string;
var
    xStream: TStringStream;
    xLines: TStringList;
    x: integer;
begin
    xLines := TStringList.Create();
    try
        for x := 0 to Length(aLines) - 1 do
            xLines.Add(aLines[x]);

        xStream := TStringStream.Create('');
        try
            xLines.SaveToStream(xStream);
            result := xStream.DataString;
        finally
            FreeAndNil(xStream);
        end;
    finally
        FreeAndNil(xLines);
    end;
end;

class function TSQLTermUtils.GetStringsFromString(const aText: string): TStringArray;
var
    xStream: TStringStream;
    xLines: TStringList;
    x: integer;
begin
    xLines := TStringList.Create();
    try
        xStream := TStringStream.Create(aText);
        try
            xLines.LoadFromStream(xStream);
        finally
            FreeAndNil(xStream);
        end;

        SetLength(result, xLines.Count);
        for x := 0 to xLines.Count - 1 do
            result[x] := xLines[x];
    finally
        FreeAndNil(xLines);
    end;
end;

class function TSQLTermUtils.GetPureSQLText(aText: string): string;
var
    xCStart, xCEnd: integer;
begin
    // Kommentare entfernen:
    repeat
        xCStart := Pos('/*', aText);
        if (xCStart > 0) then
        begin
            xCEnd := Pos('*/', aText);
            if (xCEnd = 0) then
                xCEnd := Length(aText);

            Delete(aText, xCStart, xCEnd + 2 - xCStart);
        end;
    until (xCStart <= 0);

    result := Trim(aText);
end;

class function TSQLTermUtils.ArrayToText(const aLines: TStringArray): string;
var
    xLines: TStringList;
    x: Integer;
begin
    xLines := TStringList.Create();
    try
        for x := 0 to Length(aLines) - 1 do
            xLines.Add(aLines[x]);
        result := GetPureSQLText(xLines.Text);
    finally
        FreeAndNil(xLines);
    end;
end;

class function TSQLTermUtils.DefineQueryType(const aLines: TStringArray): TSQLTextType;
const
    STR_SQL_FIRSTWORD_SELECT = 'SELECT';
    STR_SQL_FIRSTWORD_INSERT = 'INSERT';
    STR_SQL_FIRSTWORD_UPDATE = 'UPDATE';
    STR_SQL_FIRSTWORD_DELETE = 'DELETE';
var
    xText, xFirstWord: string;

begin
    xText := ArrayToText(aLines);
    xText := GetPureSQLText(xText);

    xFirstWord := UpperCase(Copy(xText, 1, 6));

    if (xFirstWord = STR_SQL_FIRSTWORD_SELECT) then
    begin
        result := sttSelectQuery;
    end
    else if (xFirstWord = STR_SQL_FIRSTWORD_INSERT) or (xFirstWord = STR_SQL_FIRSTWORD_UPDATE) or
        (xFirstWord = STR_SQL_FIRSTWORD_DELETE) then
    begin
        result := sttChangeQuery;
    end
    else
    begin
        result := sttNoQuery;
    end;
end;

constructor TSQLTermsDataAdaptor.Create;
begin
    inherited Create(STR_SQLTERMS_TBL);
end;

function TSQLTermsDataAdaptor.ReadSQLTermData(aSQLTermName: string; out oRec: TSQLTermRec): boolean;
begin
    self.SelectAndOpen(Format(STR_SQL_SELECT_ALL + ' WHERE UPPER(%s) = ''%s''',
        [STR_SQLTERMS_FLD_NAME, UpperCase(aSQLTermName)]), true);
    try
        if (self.DataProvider.Eof) then
        begin
            result := false;
            EXIT;
        end;
        result := true;

        self.ReadRecFromDataset(self.DataProvider, oRec);
    finally
        self.Close();
    end;
end;

function TSQLTermsDataAdaptor.GetNameField: string;
begin
    result := STR_SQLTERMS_FLD_NAME;
end;

class procedure TSQLTermsDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider; out oRec: TSQLTermRec);
begin
    oRec.Name := aDataset.FieldByName(STR_SQLTERMS_FLD_NAME).AsString;
    oRec.Term := aDataset.FieldByName(STR_SQLTERMS_FLD_TERM).AsString;
    oRec.Defaultargs := aDataset.FieldByName(STR_SQLTERMS_FLD_PARAMSDEFAULT).AsString;
    oRec.Comment := aDataset.FieldByName(STR_SQLTERMS_FLD_COMMENT).AsString;
end;

class procedure TSQLTermsDataAdaptor.WriteRecAtCursor(aDataset: TDataProvider; const aRec: TSQLTermRec;
    aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(STR_SQLTERMS_FLD_NAME).AsString := aRec.Name;
    aDataset.FieldByName(STR_SQLTERMS_FLD_TERM).AsString := aRec.Term;
    aDataset.FieldByName(STR_SQLTERMS_FLD_PARAMSDEFAULT).AsString := aRec.Defaultargs;
    aDataset.FieldByName(STR_SQLTERMS_FLD_COMMENT).AsString := aRec.Comment;

    aDataset.Post;
end;

function gmGetStringFromTextFile(const aFileName: string): string;
var
    xStream: TStream;
    xSize: Integer;
    xText: string;
begin
    xStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
    try
        xSize := xStream.Size - xStream.Position;
        SetString(xText, nil, xSize);
        xStream.Read(Pointer(xText)^, xSize);
        result := xText;
    finally
        FreeAndNil(xStream);
    end;
end;

procedure gmReadStringListFromTextFile(aLines: TStrings; const aFileName: string);
var
    xStream: TStream;
begin
    xStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
    try
        aLines.LoadFromStream(xStream);
    finally
        FreeAndNil(xStream);
    end;
end;

procedure RemoveZinsserStyleRemarks(aSQLStrings: TStrings; aComments: TStrings);
var
    x: integer;
begin
    for x := aSQLStrings.Count - 1 downto 0 do
    begin

        // Zinsser-Style-Kommentare in Comment-Feld schreiben!
        // -> Diese Art der Kommentare wird nicht mehr verwendet
        if (POS('//', Trim(aSQLStrings[x])) <> 1) then
            CONTINUE;

        aComments.Insert(0, Trim(Copy(aSQLStrings[x], 3, Length(aSQLStrings[x]))));
        aSQLStrings.Delete(x);
    end;
end;

procedure TSQLTermsDataAdaptor.WriteSQLTermRec(const aRec: TSQLTermRec);
var
    xAppend: boolean;
begin
    self.SelectAndOpen(Format(STR_SQL_SELECT_ALL + ' WHERE UPPER(%s) = ''%s''',
        [STR_SQLTERMS_FLD_NAME, UpperCase(aRec.Name)]), false);
    try
        xAppend := self.DataProvider.Eof;
        self.WriteRecAtCursor(self.DataProvider, aRec, xAppend);
    finally
        self.Close();
    end;
end;

procedure TSQLTermsDataAdaptor.AppendName(const aSQLTermName: string);
var
    xRec: TSQLTermRec;
begin
    xRec.Name := aSQLTermName;
    xRec.Term := '';
    xRec.Defaultargs := '';
    xRec.Comment := '';

    self.SelectAndOpenAll(false);
    try
        self.WriteRecAtCursor(self.DataProvider, xRec, true);
    finally
        self.Close();
    end;
end;


end.
