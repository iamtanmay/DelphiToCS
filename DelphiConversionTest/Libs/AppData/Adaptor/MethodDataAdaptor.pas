{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data adaptors for the method table
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.04.04 pk                               TN1880   initial version
  18.05.04 pk  FormatAllSeq, FormatSeq      TN1880   New: formats the sequence field
  25.05.04 pk                               TN1909   Changes needed for nested methods
  15.06.04 pk  ReadMethodRecFromDataset     TN1909.1 Bug: Exit when IsEmpty instead of EOF
  21.07.04 pk                               TN2049   New Fields : Priority, SchedMin, SchedMax
  21.07.04 pk  InstallMethodTable           TN2049   New: creates a new method table
  30.08.04 wl                               TN2097   Anpassung an geändertes ParserInterface
  02.11.04 pk INT_METHOD_FLDLEN_REMARK      TN2210   Changed from 100 to 200
  02.12.04 wl  Create                       TN2254.1 liest 'SubMethods','AcceptOnlyUniversalLayout' aus Settings
  02.12.04 wl  SubMethodLayoutNameAllowed   TN2254.1 von ObjSampl hierher - wenn 'AcceptOnlyUniversalLayout' = 0 wird nicht geprüft
  03.12.04 wl  ReadAllMethodNames           TN2254.2 returns all method names
  17.01.05 pk  InstallMethodTable           TN2281.0  takes const TableName
  19.01.05 pk                               TN2281.0  TStringArray, TIntArray from AppTypes to GeneralTypes
  27.01.05 pk  FindMethodInDataset          TN2299    if methodname is empty return false
  02.02.05 pk                               TN2302.2 New field : ResID
  02.02.05 pk                               TN2302.2 Inherits from TQueryDataAdaptor
  02.02.05 pk  GetMaxMethodNameLength       TN2305   New : returns constant length of field NAME
  07.04.05 pk  ReadAllMethodsAndLayouts     TN2375.1 New : return array of recs with MethodName and LayoutName
  13.04.05 wl  WriteMethodRec               TN2248.8  New parameter aForceAppend: if true the new record will alwaysw be appended
  06.06.05 pk  CopyDB                       TN2449    New : Makes a copy of the table
  07.06.05 wl  INT_METHOD_FLDLEN_SOURCERACK,DESTRACK,DILRACK  TN2452  auf 30 Zeichen gesetzt, damit es beim Update keine Probleme gibt
  08.06.05 wl  STR_METHOD_FLD_INACTIVE      TN2454    Neues Feld "Inactive"
  08.06.05 wl  INT_METHOD_FLDLEN_REMARK     TN2441   Changed from 200 to 255
  08.06.05 wl  STR_METHOD_FLD_COMMENT       TN2441    Neues Feld "Comment"
  15.06.05 wl  WriteMethodRecToDataset      TN2465.2  Bug beseitigt: Priority,SchedMin,SchedMax werden jetzt mit übergeben
  23.06.05 pk  CopyDB                       TN2473    Try to copy the .MB file as well ( but only if it exists )
  01.07.05 pk  InstallTable                 TN2487   Do not call CreateTable
  17.08.05 wl  SelectMethod                 TN2558.7  kann jetzt auch nicht case-sensitive sein
  17.08.05 wl  MethodExists                 TN2558.7  interne Änderungen
  17.08.05 wl  MethodExists_NotCaseSensitive  TN2558.7  neu: nicht case-sensitive
  12.09.05 wl  GetFieldLengths              TN2591    ermittelt die Größe aller String-Felder
  10.10.05 wl  SelectAll                    TN2584    Wird für MethodDataAdaptorExt benötigt
  24.11.05 pk  SQLAllDistinctNames          TN2765    New - needed for QueryDataAdaptor.GetNames
  30.11.05 wl  CheckNewName                 TN2815    Umfangreiche und restriktive Prüfung eines neuen Methodennamens
  30.11.05 wl  NameAllowedAsFileName        TN2815    Prüfung, ob der Name als Dateiname möglich ist (aus TGlobal.NameAllowed)
  30.11.05 wl  NameReservedByUser           TN2815    Verbotene Namen aus ini (von Postools hierher) - braucht wahrscheinlich kein Mensch mehr
  30.11.05 wl  NameContainsForbiddenCharacters  TN2815    außer ' ','_' und '-' sind alle Sonderzeichen verboten
  11.01.06 pk  InstallTable                 TN2871.1 override
  25.03.06 pk  IsSubMethodLayoutNameAllowed TN3001    code from SubMethodLayoutNameAllowed. can now be used without using instance
  06.04.06 pk  SchedSharedID                TN3024    New field
  06.04.06 pk  Inactive                     TN3025    Removed
  10.04.06 pk  Iterate                      TN3032    New field
  18.04.06 wl  TMethodRec                   TN3025    Inactive komplett entfernt
  08.06.06 wl  InstallTable                 TN2969    Neues Feld: REMARKEXTEND ( ftMemo )
  08.06.06 wl  ReadMethodRecFromDataset     TN2969    Options = REMARK + REMARKEXTEND
  08.06.06 wl  WriteMethodRecToDataset      TN2969    Options wird auf die Felder REMARK und REMARKEXTEND aufgeteilt
  08.06.06 wl  GetFieldLengths              TN2969    Keine Begranzung mehr für REMARK
  19.06.06 wl  ReadRemark/WriteRemark       TN3159    Verallgemeinerte Funktion zum Zusammenführen von REMARK und REMARKEXTEND
  07.07.06 wl  DivideRemark                 TN3190    Neu: Teilt einen String richtig für die Remark-Felder auf
  06.09.06 pk  UpdateDefaults               TN3283.2  it is no longer required that all records have priority=1
  21.09.06 wl  VerifyTable                  TN3326    mit Parameter aFirstInit
  25.01.07 wl  DivideRemark                 TN3524    Sonderbehandlung, wenn der 255. Buchstabe = ' ' (sonst wird er beim Speichern vergessen)
  05.02.07 pk                                TN3544   Changes for updatemanager
  22.02.07 pk  Create                       TN3583    reading of fSubMethodsAcceptOnlyUniversalLayout setting moved to function where it is needed
  22.02.07 pk  finalization                 TN3583    uMethodDataAdaptorInstance.free
  24.06.07 wl  MethodExistsIntern           TN3740    public statt private, weil von TMethodItem verwendet
  07.08.07 wl  SelectAndOpen...             TN3811.3  alle Select-Funktionen in SelectandOpen.. geändert
  07.08.07 wl                               TN3811.3  benutzt DatasetUtils
  17.08.07 pk                               TN3811.3  DatasetUtils removed. Now use SaveRecordsAs and DeleteRecordsByKey
  30.08.07 pk                               TN3840.1  SaveRecordAs parameters changed
  03.09.07 pk  CheckNewName,etc             TN3847    Application-specific funcs moved to TMethodDataAdaptorExt
  02.10.07 wl                               TN3811.5  uses dbTables entfernt: TDBDataset durch TDataset ersetzt
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  07.01.08 wl                               TN3972    Felder radikal zusammengestrichen
  10.01.08 wl                               TN3972    Bug behoben
  21.01.08 wl                               TN3972    Bug behoben
  06.05.08 wl  Inactive                     TN4074    new field
  23.09.08 wl                               TN4236   Vereinigung mit MethodFieldnames
  29.09.08 wl  INT_METHOD_FLDLEN_NAME       TN4242   MethodName hat jetzt 50 chars
  16.01.09 wl                               TN4362   an Änderungen in TQueryDataAdaptor angepasst
  16.06.09 wl  ReadAllMethodNames           TN4605    entfernt
  06.07.09 pk  SetRange, CancelRange        TN4585.4 Removed
  04.12.10 pk  SelectAndOpenMethod          TN5039   UpperCase replaced by AnsiUpperCase in order to support German Characters
  17.06.10 pk                               TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  21.07.10 pk                               TN5203   uneeded functions removed
  10.12.10 pk                               TN5389   now all selects with Order By
  13.11.12 wl  GetKeyFields                 TN6015   kann entfernt werden, wenn Key = NameField
  30.08.13 wl  MethodExists-Methoden        TN6236   --> TMethodSettingsDataAdaptor
  -------------------------------------------------------------------------------------------------- }

unit MethodDataAdaptor;


interface


uses
    DataProvider,
    QueryDataAdaptor;

const
    STR_METHOD_TBL = 'METHOD';

type
    TMethodRec = record
        Valid: boolean;
        name: string;
        Seq: integer;
        Action: string;
        Options: string;
        Comment: string;
        Inactive: boolean;
    end;

    TMethodRecArray = array of TMethodRec;

    TMethodDataAdaptor = class(TQueryDataAdaptor)
    private const
        INT_METHOD_FLDLEN_NAME = 50;
        INT_METHOD_FLDLEN_ACTION = 20;
    protected const
        STR_METHOD_FLD_NAME = 'NAME';
        STR_METHOD_FLD_SEQ = 'SEQ';
        STR_METHOD_FLD_ACTION = 'ACTION';
        STR_METHOD_FLD_AOPTIONS = 'AOPTIONS';
        STR_METHOD_FLD_COMMENT = 'COMMENT';
        STR_METHOD_FLD_INACTIVE = 'LINEDISABLED';
    protected
        function GetNameField(): string; override;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure SelectAndOpenAll(aReadOnly: boolean);
        procedure SelectAndOpenMethod(aName: string; aCaseSensitive: boolean; aReadOnly: boolean);
        procedure SelectAndOpenDistinctMethod(aMethodName: string; aReadOnly: boolean);
        function GetDataset(aName: string; aReadOnly: boolean = true): TDataProvider;
        procedure WriteMethodRec(aMethodRec: TMethodRec; aForceAppend: boolean = false);
        function ReadMethod(aName: string): TMethodRecArray;

        class function MakeMethodRec(aValid: boolean; const aName: string; aSeq: integer;
            const aAction, aOptions, aComment: string; aInactive: boolean): TMethodRec;

        class function ReadMethodRecFromDataset(aDataset: TDataProvider): TMethodRec;
        class procedure WriteMethodRecToDataset(aDataset: TDataProvider; aMethodRec: TMethodRec;
            aAppend: boolean);
        class function ReadAllMethodRecsFromDataSet(aDataSet: TDataProvider): TMethodRecArray;

        class function GetMaxMethodNameLength(): integer;
        class function GetMaxActionNameLength(): integer;
    end;


implementation


uses
    SysUtils;

{ TMethodDataAdaptor }

constructor TMethodDataAdaptor.Create();
begin
    inherited Create(STR_METHOD_TBL);
end;

destructor TMethodDataAdaptor.Destroy;
begin
    inherited;
end;

procedure TMethodDataAdaptor.SelectAndOpenMethod(aName: string; aCaseSensitive: boolean; aReadOnly: boolean);
var
    xSelect: string;
begin
    xSelect := 'SELECT * FROM ' + STR_METHOD_TBL;

    if (aName = '') then
    begin
        self.SelectAndOpen(xSelect, aReadOnly);
        EXIT;
    end;

    if (aCaseSensitive) then
        xSelect := Format('%s WHERE %s = ''%s''', [xSelect, STR_METHOD_FLD_NAME, aName])
    else
        xSelect := Format('%s WHERE UPPER(%s) = ''%s''', [xSelect, STR_METHOD_FLD_NAME,
            AnsiUpperCase(aName)]);

    xSelect := xSelect + ' ORDER BY ' + STR_METHOD_FLD_NAME + ',' + STR_METHOD_FLD_SEQ;
    SelectAndOpen(xSelect, aReadOnly);
end;

procedure TMethodDataAdaptor.SelectAndOpenDistinctMethod(aMethodName: string; aReadOnly: boolean);
var
    xSQLWhere, xSQL: string;
begin
    xSQLWhere := '';
    if aMethodName <> '' then
    begin
        xSQLWhere := Format('%s = ''%s''', [STR_METHOD_FLD_NAME, aMethodName]);
    end;
    if xSQLWhere <> '' then
        xSQLWhere := 'WHERE ' + xSQLWhere;

    xSQL := Format('SELECT DISTINCT %s FROM ''%s'' %s', [STR_METHOD_FLD_NAME, STR_METHOD_TBL, xSQLWhere]);
    xSQL := xSQL + ' ORDER BY ' + STR_METHOD_FLD_NAME + ',' + STR_METHOD_FLD_SEQ;
    self.SelectAndOpen(xSQL, aReadOnly);
end;

function TMethodDataAdaptor.GetDataset(aName: string; aReadOnly: boolean = true): TDataProvider;
begin
    SelectAndOpenMethod(aName, true, aReadOnly);
    result := self.DataProvider;
end;

procedure TMethodDataAdaptor.WriteMethodRec(aMethodRec: TMethodRec; aForceAppend: boolean);
begin
    self.SelectAndOpenMethod('', true, false);
    try
        WriteMethodRecToDataSet(self.DataProvider, aMethodRec, aForceAppend);
    finally
        Close();
    end;
end;

function TMethodDataAdaptor.ReadMethod(aName: string): TMethodRecArray;
begin
    SetLength(result, 0);
    self.SelectAndOpenMethod(aName, true, true);
    try
        result := ReadAllMethodRecsFromDataSet(self.DataProvider);
    finally
        Close();
    end;
end;

class function TMethodDataAdaptor.MakeMethodRec(aValid: boolean; const aName: string; aSeq: integer;
    const aAction, aOptions, aComment: string; aInactive: boolean): TMethodRec;
begin
    result.Valid := aValid;
    result.Name := aName;
    result.Seq := aSeq;
    result.Action := aAction;
    result.Options := aOptions;
    result.Comment := aComment;
    result.Inactive := aInactive;
end;

class function TMethodDataAdaptor.ReadMethodRecFromDataset(aDataset: TDataProvider): TMethodRec;
begin
    result.Valid := false;
    if aDataset.IsEmpty then
        Exit;
    result := MakeMethodRec(true, aDataset.FieldByName(STR_METHOD_FLD_NAME).AsString,
        aDataset.FieldByName(STR_METHOD_FLD_SEQ).AsInteger, aDataset.FieldByName(STR_METHOD_FLD_ACTION)
        .AsString, aDataset.FieldByName(STR_METHOD_FLD_AOPTIONS).AsString,
        aDataset.FieldByName(STR_METHOD_FLD_COMMENT).AsString, aDataset.FieldByName(STR_METHOD_FLD_INACTIVE)
        .AsBoolean);
end;

class procedure TMethodDataAdaptor.WriteMethodRecToDataset(aDataset: TDataProvider; aMethodRec: TMethodRec;
    aAppend: boolean);
begin
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(STR_METHOD_FLD_NAME).AsString := aMethodRec.Name;
    aDataset.FieldByName(STR_METHOD_FLD_SEQ).AsInteger := aMethodRec.Seq;
    aDataset.FieldByName(STR_METHOD_FLD_ACTION).AsString := aMethodRec.Action;
    aDataset.FieldByName(STR_METHOD_FLD_AOPTIONS).AsString := aMethodRec.Options;
    aDataset.FieldByName(STR_METHOD_FLD_COMMENT).AsString := aMethodRec.Comment;
    aDataset.FieldByName(STR_METHOD_FLD_INACTIVE).AsBoolean := aMethodRec.Inactive;
    aDataset.Post;
end;

class function TMethodDataAdaptor.ReadAllMethodRecsFromDataSet(aDataSet: TDataProvider): TMethodRecArray;
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
        result[xCount] := ReadMethodrecFromDataSet(aDataSet);
        aDataSet.Next;
        Inc(xCount);
    end;
end;

class function TMethodDataAdaptor.GetMaxMethodNameLength(): integer;
begin
    result := INT_METHOD_FLDLEN_NAME;
end;

procedure TMethodDataAdaptor.SelectAndOpenAll(aReadOnly: boolean);
begin
    self.SelectAndOpen('SELECT * FROM ' + STR_METHOD_TBL + ' ORDER BY ' + STR_METHOD_FLD_NAME + ',' +
        STR_METHOD_FLD_SEQ, aReadOnly);
end;

function TMethodDataAdaptor.GetNameField(): string;
begin
    result := STR_METHOD_FLD_NAME;
end;

class function TMethodDataAdaptor.GetMaxActionNameLength: integer;
begin
    result := INT_METHOD_FLDLEN_ACTION;
end;


end.
