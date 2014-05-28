{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.08.07 wl                               TN3811.2 von ImportDataAdaptor getrennt
  02.10.07 wl  GetKeyFields                 TN3811.5 neu: damit DeleteName & SaveNameAs benutzt werden können
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Functions moved here from ImportDataAdaptor
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  09.07.09 pk                                TN4585.4 DataProvider.Locate removed, functionality replaced by SelectAndOpen
  17.06.10 pk                               TN5152.1  Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  13.11.12 wl  GetKeyFields                 TN6015   kann entfernt werden, wenn Key = NameField
  -------------------------------------------------------------------------------------------------- }

unit ImportColDefDataAdaptor;


interface


uses
    Classes,
    DataProvider,
    QueryDataAdaptor;

type
    TImportColDefRec = record
        ImportDefName: string;
        TargetCol: string;
        SourceCol: string;
        SourceNotRequired: boolean;
        SourceDefault: variant;
    end;

    TImportColDefRecArray = array of TImportColDefRec;

    TImportColDefsDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_IMPCOLS_TBL = 'IMPCOLS';
        STR_IMPCOLS_FLD_DEFNAME = 'IMPORTDEFNAME';
        STR_IMPCOLS_FLD_TARGETCOL = 'TARGETCOL';
        STR_IMPCOLS_FLD_SOURCECOL = 'SOURCECOL';
        STR_IMPCOLS_FLD_SOURCENOTREQUIRED = 'SOURCENOTREQUIRED';
        STR_IMPCOLS_FLD_SOURCEDEFAULT = 'SOURCEDEFAULT';

        STR_IMPCOLS_INDEX = STR_IMPCOLS_FLD_DEFNAME + ';' + STR_IMPCOLS_FLD_TARGETCOL;

        STR_SQL_COLDEF_SELECT = 'SELECT * FROM ' + STR_IMPCOLS_TBL;
        STR_COLDEF_FILTER = STR_IMPCOLS_FLD_DEFNAME + ' = ''%s''';

    protected
        function GetNameField(): string; override;
    public
        constructor Create();
        procedure WriteColDef(const aRec: TImportColDefRec);
        function ReadColDefs(const aDefName: string): TImportColDefRecArray;
        procedure SelectAndOpenColDef(const aDefName: string; const aTargetCol: string; aReadOnly: boolean);
        procedure SelectAndOpenDef(aName: string; aReadOnly: boolean);
        class procedure WriteRecToDataset(aDataset: TDataProvider; aColDef: TImportColDefRec;
            aAppend: boolean);
        class function ReadColDefFromDataset(aDataset: TDataProvider): TImportColDefRec;
        procedure DeleteColDefsByDefName(aDefName: string);
        class function MakeColDefRec(aImportDefName, aTargetCol, aSourceCol: string;
            aSourceNotRequired: boolean; aSourceDefault: variant): TImportColDefRec;
        class procedure AddColDefRecToArray(var aColDefArray: TImportColDefRecArray;
            aColDefRec: TImportColDefRec);
    end;


implementation


uses
    SysUtils,
    Variants;

{ TImportColDefsDataAdaptor }

constructor TImportColDefsDataAdaptor.Create();
begin
    inherited Create(STR_IMPCOLS_TBL);
end;

procedure TImportColDefsDataAdaptor.SelectAndOpenDef(aName: string; aReadOnly: boolean);
var
    xSelect: string;
begin
    xSelect := STR_SQL_COLDEF_SELECT;
    if aName <> '' then
        xSelect := format('%s WHERE UPPER( %s ) = ''%s''', [xSelect, STR_IMPCOLS_FLD_DEFNAME,
            UpperCase(aName)]);

    self.SelectAndOpen(xSelect, aReadOnly);
end;

procedure TImportColDefsDataAdaptor.SelectAndOpenColDef(const aDefName: string; const aTargetCol: string;
    aReadOnly: boolean);
var
    xSelect: string;
begin
    xSelect := format('%s WHERE UPPER( %s ) = ''%s'' AND UPPER( %s ) = ''%s''',
        [STR_SQL_COLDEF_SELECT, STR_IMPCOLS_FLD_DEFNAME, UpperCase(aDefName), STR_IMPCOLS_FLD_TARGETCOL,
        UpperCase(aTargetCol)]);

    self.SelectAndOpen(xSelect, aReadOnly);
end;

function TImportColDefsDataAdaptor.GetNameField(): string;
begin
    result := STR_IMPCOLS_FLD_DEFNAME;
end;

procedure TImportColDefsDataAdaptor.DeleteColDefsByDefName(aDefName: string);
begin
    self.ExecSQLFmt('DELETE FROM %s WHERE %s = ''%s''', [STR_IMPCOLS_TBL, STR_IMPCOLS_FLD_DEFNAME, aDefName]);
end;

class procedure TImportColDefsDataAdaptor.WriteRecToDataset(aDataset: TDataProvider;
    aColDef: TImportColDefRec; aAppend: boolean);
begin
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(STR_IMPCOLS_FLD_DEFNAME).AsString := aColDef.ImportDefName;
    aDataset.FieldByName(STR_IMPCOLS_FLD_TARGETCOL).AsString := aColDef.TargetCol;
    aDataset.FieldByName(STR_IMPCOLS_FLD_SOURCECOL).AsString := aColDef.SourceCol;
    aDataset.FieldByName(STR_IMPCOLS_FLD_SOURCENOTREQUIRED).AsBoolean := aColDef.SourceNotRequired;
    aDataset.FieldByName(STR_IMPCOLS_FLD_SOURCEDEFAULT).Value := aColDef.SourceDefault;

    aDataset.Post;
end;

class function TImportColDefsDataAdaptor.ReadColDefFromDataset(aDataset: TDataProvider): TImportColDefRec;
begin
    result := MakeColDefRec(aDataset.FieldByName(STR_IMPCOLS_FLD_DEFNAME).AsString,
        aDataset.FieldByName(STR_IMPCOLS_FLD_TARGETCOL).AsString,
        aDataset.FieldByName(STR_IMPCOLS_FLD_SOURCECOL).AsString,
        aDataset.FieldByName(STR_IMPCOLS_FLD_SOURCENOTREQUIRED).AsBoolean,
        aDataset.FieldByName(STR_IMPCOLS_FLD_SOURCEDEFAULT).Value);
end;

function TImportColDefsDataAdaptor.ReadColDefs(const aDefName: string): TImportColDefRecArray;
var
    x: integer;
begin
    SelectAndOpenDef(aDefName, true);
    try
        SetLength(result, self.DataProvider.RecordCount);
        x := 0;
        while not self.DataProvider.Eof do
        begin
            result[x] := TImportColDefsDataAdaptor.ReadColDefFromDataset(self.DataProvider);
            Inc(x);
            self.DataProvider.Next;
        end;
    finally
        self.Close;
    end;
end;

procedure TImportColDefsDataAdaptor.WriteColDef(const aRec: TImportColDefRec);
var
    xAppend: boolean;
begin
    self.SelectAndOpenColDef(aRec.ImportDefName, aRec.TargetCol, false);
    try
        xAppend := self.DataProvider.IsEmpty;
        self.WriteRecToDataset(self.DataProvider, aRec, xAppend);
    finally
        self.Close();
    end;
end;

class function TImportColDefsDataAdaptor.MakeColDefRec(aImportDefName, aTargetCol, aSourceCol: string;
    aSourceNotRequired: boolean; aSourceDefault: variant): TImportColDefRec;
begin
    result.ImportDefName := aImportDefName;
    result.TargetCol := aTargetCol;
    result.SourceCol := aSourceCol;
    result.SourceNotRequired := aSourceNotRequired;
    result.SourceDefault := aSourceDefault;
end;

class procedure TImportColDefsDataAdaptor.AddColDefRecToArray(var aColDefArray: TImportColDefRecArray;
    aColDefRec: TImportColDefRec);
var
    xTotalColDefs: integer;
begin
    xTotalColDefs := high(aColDefArray) + 1;
    SetLength(aColDefArray, xTotalColDefs + 1);
    aColDefArray[xTotalColDefs] := aColDefRec
end;


end.
