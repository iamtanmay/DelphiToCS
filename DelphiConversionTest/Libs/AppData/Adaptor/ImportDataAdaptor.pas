{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Data adaptors for the ImpFile, ImpDef, and ImpCols tables
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.04.04 pk                               TN1880   initial version
  28.04.04 pk                               TN1880   FileDef table renamed to ImpFile
  13.05.04 pk  TImportAdaptorsUpdateManager TN1916   New: Updates import tables
  17.05.04 pk  TImportDefDataAdaptor        TN1880   SelectDef, SelectColDef : Defname should not be case-sensitive
  24.06.04 wl                               TN2007   uses Variants (nur Delphi 6 und 7)
  03.08.04 pk  ResolveVariables             TN2066   New: MakeColDefRec, AddColDefRecToArray
  19.01.05 pk  TImportColDefsDataAdaptor    TN2889   New: can now be updated via update manager
  07.03.06 wl  INT_DEF_MODE_..              TN2889.2 die möglichen Werte sind als CONST hinterlegt
  21.09.06 wl  VerifyTable                  TN3326   mit Parameter aFirstInit
  05.02.07 pk                               TN3544   Changes for updatemanager
  07.08.07 wl                               TN3811.3 jetzt abgeleitet von TQueryDataAdaptor
  07.08.07 wl                               TN3811.3 fQryDef erstezt durch self.Query
  07.08.07 wl  TImportFileDefDataAdaptor    TN3811.3 --> ImportFileDefDataAdaptor
  07.08.07 wl  TImportColDefsDataAdaptor    TN3811.3 --> ImportColDefDataAdaptor
  17.08.07 pk  TImportDefDataAdaptor        TN3811.3 Bug fixed: call inherited Create
  02.10.07 wl  GetKeyFields                 TN3811.5 neu: damit DeleteName & SaveNameAs benutzt werden können
  09.11.07 pk                               TN3921   Changes for updatemanager
  09.11.07 pk                               TN3922   Functions moved to ImportColDefDataAdaptor
  23.09.08 wl                               TN4236   Vereinigung mit ..Fieldnames
  16.01.09 wl                                TN4362   an Änderungen in TQueryDataAdaptor angepasst
  09.07.09 pk                               TN4585.4 DataProvider.Locate removed, functionality replaced by SelectAndOpen
  25.08.09 wl  ReadAllDefsOfMode             TN4611   ist jetzt public
  04.11.09 pk                               TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.11.09 pk  ReadAllDefNames              TN4843 setlenght with length instead of high
  17.06.10 pk                               TN5152.1  Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  23.02.11 wl  GetNameField                 TN5486   neu
  01.07.11 wl  AppendName                   TN5619   neu
  22.07.11 wl  SelectAndOpenDef             TN5614.2   jetzt sortiert nach Namen
  13.11.12 wl  GetKeyFields                 TN6015   kann entfernt werden, wenn Key = NameField
  -------------------------------------------------------------------------------------------------- }

unit ImportDataAdaptor;


interface


uses
    GeneralTypes,
    DataProvider,
    QueryDataAdaptor,
    ImportFileDefDataAdaptor,
    ImportColDefDataAdaptor;

const

    INT_DEF_MODE_INVALID = -1;
    INT_DEF_MODE_ALL = 0;
    INT_DEF_MODE_TABLEIMPORT = 1;
    INT_DEF_MODE_METHVARIMPORT = 2;

type
    TImportDefRec = record
        Valid: boolean;
        name: string;
        Mode: integer;
        TargetName: string;
        SourceFileDefName: string;
        SourceFileDef: TFileDefRec;
        ImportColDefs: TImportColDefRecArray;
    end;

    TImportDefRecArray = array of TImportDefRec;

    TImportDefDataAdaptor = class(TQueryDataAdaptor)
    private const
        STR_IMPDEF_TBL = 'IMPDEF';
        STR_IMPDEF_FLD_NAME = 'NAME';
        STR_IMPDEF_FLD_MODE = 'MODE';
        STR_IMPDEF_FLD_TARGETNAME = 'TARGETPATHNAME';
        STR_IMPDEF_FLD_SOURCENAME = 'SOURCENAME';

        STR_SQL_DEF_SELECT = 'SELECT * FROM ' + STR_IMPDEF_TBL;
        STR_DEF_FILTER = STR_IMPDEF_FLD_NAME + ' = ''%s''';
    private
        fColDefDA: TImportColDefsDataAdaptor;
        fIncludeColDefs: boolean;
        fIncludeSourceFileDef: boolean;
    protected
        function GetNameField(): string; override;
    public
        constructor Create();
        destructor Destroy(); override;

        procedure Reset();
        procedure SelectAndOpenDef(aName: string; aMode: integer; aReadOnly: boolean);

        procedure CloseQuery();
        procedure WriteDef(aDef: TImportDefRec);
        function ReadDef(aName: string): TImportDefRec;
        function ReadAllDefsOfMode(aMode: integer): TImportDefRecArray;

        class procedure WriteDefToDataset(aDataset: TDataProvider; aColDefsDA: TImportColDefsDataAdaptor;
            aDef: TImportDefRec; aAppend: boolean);
        class function ReadDefFromDataSet(aDataSet: TDataProvider; aColDefsDA: TImportColDefsDataAdaptor;
            aFileDefDataAdaptor: TImportFileDefDataAdaptor): TImportDefRec;
        class function ReadAllDefsFromDataset(aDataset: TDataProvider; aColDefsDA: TImportColDefsDataAdaptor;
            aFileDefDataAdaptor: TImportFileDefDataAdaptor): TImportDefRecArray;

        class function ReadAllDefNames(aMode: integer): TStringArray;

        class function QuickReadDef(aName: string; aIncludeColDefs, aIncludeSourceFileDef: boolean)
            : TImportDefRec;

        property ColDefsDataAdaptor: TImportColDefsDataAdaptor read fColDefDA;
        property IncludeColDefs: boolean read fIncludeColDefs write fIncludeColDefs;
        property IncludeSourceFileDef: boolean read fIncludeSourceFileDef write fIncludeSourceFileDef;
        procedure AppendName(const aNewName: string; aMode: integer);
    end;


implementation


uses
    Variants,
    SysUtils;

{ TImportDefDataAdaptor }

constructor TImportDefDataAdaptor.Create();
begin
    inherited Create(STR_IMPDEF_TBL);

    fColDefDA := TImportColDefsDataAdaptor.Create();

    fIncludeColDefs := false;
    fIncludeSourceFileDef := false;
end;

destructor TImportDefDataAdaptor.Destroy();
begin
    FreeAndNil(fColDefDA);
    inherited;
end;

procedure TImportDefDataAdaptor.Reset();
begin
    fIncludeColDefs := false;
    fIncludeSourceFileDef := false;
    CloseQuery();
end;

procedure TImportDefDataAdaptor.SelectAndOpenDef(aName: string; aMode: integer; aReadOnly: boolean);
var
    xWhere, xSelect: string;
begin
    xWhere := '';
    if aName <> '' then
        xWhere := Format('UPPER( %s ) = ''%s''', [STR_IMPDEF_FLD_NAME, UpperCase(aName)]);

    if (aMode > INT_DEF_MODE_ALL) then
    begin
        if xWhere <> '' then
            xWhere := Format('%s AND ', [xWhere]);
        xWhere := xWhere + Format('%s = %d', [STR_IMPDEF_FLD_MODE, aMode]);
    end;

    if xWhere <> '' then
        xWhere := Format('WHERE %s', [xWhere]);

    xSelect := Format('%s %s order by name', [STR_SQL_DEF_SELECT, xWhere]);

    self.SelectAndOpen(xSelect, areadOnly);

    // fColDefDA.SelectAndOpenDef(aName, aReadOnly);
end;

procedure TImportDefDataAdaptor.CloseQuery();
begin
    self.Close();
end;

procedure TImportDefDataAdaptor.WriteDef(aDef: TImportDefRec);
var
    xDefNotFound: boolean;

begin
    SelectAndOpenDef(aDef.Name, -1, false);
    try
        xDefNotFound := self.DataProvider.IsEmpty;
        WriteDefToDataSet(self.DataProvider, fColDefDA, aDef, xDefNotFound);
    finally
        CloseQuery();
    end;
end;

function TImportDefDataAdaptor.ReadDef(aName: string): TImportDefRec;
var
    xColDefsDA: TImportColDefsDataAdaptor;
    xFileDefDA: TImportFileDefDataAdaptor;
begin
    result.Valid := false;
    SelectAndOpenDef(aName, -1, true);

    xColDefsDA := nil;
    xFileDefDA := nil;
    try
        if fIncludeColDefs then
            xColDefsDA := fColDefDA;
        try
            if fIncludeSourceFileDef then
            begin
                xFileDefDA := TImportFileDefDataAdaptor.Create();
            end;

            result := ReadDefFromDataSet(self.DataProvider, xColDefsDA, xFileDefDA);
        finally
            FreeAndNil(xFileDefDA);
        end;
    finally
        CloseQuery();
    end;
end;

function TImportDefDataAdaptor.ReadAllDefsOfMode(aMode: integer): TImportDefRecArray;
var
    xColDefsDA: TImportColDefsDataAdaptor;
    xFileDefDA: TImportFileDefDataAdaptor;
begin
    SelectAndOpenDef('', aMode, true);
    xColDefsDA := nil;
    xFileDefDA := nil;
    try
        if fIncludeColDefs then
            xColDefsDA := fColDefDA;
        try
            if fIncludeSourceFileDef then
            begin
                xFileDefDA := TImportFileDefDataAdaptor.Create();
            end;
            result := ReadAllDefsFromDataSet(self.DataProvider, xColDefsDA, xFileDefDA);
        finally
            FreeAndNil(xFileDefDA);
        end;
    finally
        CloseQuery();
    end;
end;

class procedure TImportDefDataAdaptor.WriteDefToDataset(aDataset: TDataProvider;
    aColDefsDA: TImportColDefsDataAdaptor; aDef: TImportDefRec; aAppend: boolean);
var
    i: integer;
begin
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;

    aDataset.FieldByName(STR_IMPDEF_FLD_NAME).AsString := aDef.Name;
    aDataset.FieldByName(STR_IMPDEF_FLD_MODE).AsInteger := aDef.Mode;
    aDataset.FieldByName(STR_IMPDEF_FLD_TARGETNAME).AsString := aDef.TargetName;
    aDataset.FieldByName(STR_IMPDEF_FLD_SOURCENAME).AsString := aDef.SourceFileDefName;

    if Assigned(aColDefsDA) then
    begin
        for i := 0 to high(aDef.ImportColDefs) do
        begin
            aColDefsDA.WriteColDef(aDef.ImportColDefs[i]);
        end;
    end;

    aDataset.Post;
end;

class function TImportDefDataAdaptor.ReadAllDefsFromDataset(aDataset: TDataProvider;
    aColDefsDA: TImportColDefsDataAdaptor; aFileDefDataAdaptor: TImportFileDefDataAdaptor)
    : TImportDefRecArray;
var
    i: integer;
begin
    SetLength(result, aDataset.RecordCount);
    i := 0;
    while not aDataset.Eof do
    begin
        result[i] := ReadDefFromDataset(aDataset, aColDefsDA, aFileDefDataAdaptor);
        aDataset.Next;
        Inc(i);
    end;
end;

class function TImportDefDataAdaptor.ReadDefFromDataset(aDataset: TDataProvider;
    aColDefsDA: TImportColDefsDataAdaptor; aFileDefDataAdaptor: TImportFileDefDataAdaptor): TImportDefRec;

begin
    result.Valid := false;
    if aDataset.Eof then
        Exit;
    result.Name := aDataset.FieldByName(STR_IMPDEF_FLD_NAME).AsString;
    result.Mode := aDataset.FieldByName(STR_IMPDEF_FLD_MODE).AsInteger;
    result.TargetName := aDataset.FieldByName(STR_IMPDEF_FLD_TARGETNAME).AsString;
    result.SourceFileDefName := aDataset.FieldByName(STR_IMPDEF_FLD_SOURCENAME).AsString;

    result.SourceFileDef.Valid := false;
    if Assigned(aFileDefDataAdaptor) then
    begin
        aFileDefDataAdaptor.ReadFileDef(result.SourceFileDefName, result.SourceFileDef);
    end;
    if Assigned(aColDefsDA) then
    begin
        result.ImportColDefs := aColDefsDA.ReadColDefs(result.Name)
    end;

    result.Valid := true;
end;

class function TImportDefDataAdaptor.QuickReadDef(aName: string;
    aIncludeColDefs, aIncludeSourceFileDef: boolean): TImportDefRec;
var
    xDataAdaptor: TImportDefDataAdaptor;
begin
    xDataAdaptor := TImportDefDataAdaptor.Create();
    try
        xDataAdaptor.IncludeColDefs := aIncludeColDefs;
        xDataAdaptor.IncludeSourceFileDef := aIncludeSourceFileDef;
        result := xDataAdaptor.ReadDef(aName);
    finally
        FreeAndNil(xDataAdaptor);
    end;
end;

function TImportDefDataAdaptor.GetNameField: string;
begin
    result := STR_IMPDEF_FLD_NAME;
end;

class function TImportDefDataAdaptor.ReadAllDefNames(aMode: integer): TStringArray;
var
    xDefs: TImportDefRecArray;
    x: integer;
    xDA: TImportDefDataAdaptor;
begin

    xDA := TImportDefDataAdaptor.Create;
    try
        xDefs := xDA.ReadAllDefsOfMode(aMode);
    finally
        FreeAndNil(xDA);
    end;

    SetLength(result, Length(xDefs));
    for x := 0 to Length(xDefs) - 1 do
    begin
        result[x] := xDefs[x].Name;
    end;
end;

procedure TImportDefDataAdaptor.AppendName(const aNewName: string; aMode: integer);
var
    xDefRec: TImportDefRec;
begin
    xDefRec.Valid := true;
    xDefRec.Name := aNewName;
    xDefRec.Mode := aMode;
    xDefRec.SourceFileDef.Valid := true;
    xDefRec.SourceFileDef.name := '';
    SetLength(xDefRec.ImportColDefs, 0);

    self.WriteDef(xDefRec);
end;


end.
