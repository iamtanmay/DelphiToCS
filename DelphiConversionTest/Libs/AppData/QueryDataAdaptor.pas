{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Base class for all TQuery-based DataAdaptors
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.12.04 pk                               TN2281.0 Inital revision
  02.02.05 pk                               TN2302.2 New : fFreeQuery
  28.02.05 pk                               TN2314.1 New: IsEof, MoveNext, MovePrior
  18.03.05 pk  IsActive                     TN2355   New
  24.11.05 pk  GetNames, ReadName           TN2765   New
  22.02.07 pk  CreateQuery                  TN3583   calls StandardCreateQuery, but can be overriden
  06.08.07 wl  IsBOF,MoveFirst,..           TN3811.3 nützliche Funktionen, um DataAdaptor wie ein Dataset zu bedienen
  06.08.07 wl  Select,Open                  TN3811.3 gibt es nur noch als SelectAndOpen-Funktion
  07.08.07 wl  CreateUpdater,TableName      TN3811.3  von DataAdaptor hierher
  07.08.07 wl                               TN3811.3  QueryDataAdaptor ist nicht mehr von DataAdaptor abgeleitet
  07.08.07 wl  ReadAllItemNames             TN3811.3 neu
  17.08.07 pk                               TN3811.3  New: SelectAllAndOpen, SaveRecordsAs, and DeleteRecordsByKey
  30.08.07 pk                               TN3840.1  SaveRecordAs parameters changed
  18.09.07 pk  SelectAllNamesAndOpen        TN3864    overriden in TModuleDataAdaptor
  02.10.07 wl  fQuery                       TN3811.5  TQuery durch TZinsserDataProvider ersetzt
  09.11.07 pk                               TN3922    Dataset changed to DataProvider
  09.11.07 pk                               TN3922    RealDataset removed
  13.01.09 wl  BuildSQLWhere                TN4312    neu für Delphi 2009: UnicodeString
  16.01.09 wl                               TN4362    viele notwendige Strukturänderungen
  16.06.09 wl  ReadAllNames                 TN4605    entspricht GetNames, aber mit StringArray
  06.07.09 pk  CreateDataProvider           TN4585.4  New
  21.09.09 pk  Destroy                      TN4787    call inherited
  12.10.09 wl  BuildSQLWhere                TN4800    changes for Delphi 2010
  04.11.09 pk                               TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  30.04.10 wl  NameExists                   TN5070    ist virtual
  17.06.10 pk                               TN5152.1  Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  04.08.10 pk                               TN5218    Changes for Plugin database packages
  19.10.10 pk                               TN5305    changes needed for CoreClient/Server
  14.12.10 pk                               TN5412    DelphiXE: Changes made to a method with the MethodEditor cannot be saved.
  22.07.11 wl  SelectAndOpenAllSorted       TN5614.2  neu
  27.09.11 wl                               TN5698   Format-Aufrufe ersetzt
  27.03.13 wl                               TN6045   uses geändert
  19.04.13 wl  TCustomDataManager           TN6106   neu: für TViewItem
  -------------------------------------------------------------------------------------------------- }

unit QueryDataAdaptor;


interface


uses
    ListClasses,
    GeneralTypes,
    DataProvider;

type
    TCustomDataManager = class
    public
        procedure SaveNameAs(const aOldName, aNewName: string); virtual; abstract;
        procedure DeleteName(const aName: string); virtual; abstract;
        function NameExists(const aName: string): boolean; virtual; abstract;
    end;

    TQueryDataAdaptor = class(TCustomDataManager)
    private
        fTableName: string;
        fFreeQuery: boolean;
        fDataProvider: TDataProvider;
        function BuildSQLWhere(const aKeyFields: TArray<string>; const aKeyValues: TArray<variant>): string;
        procedure DeleteRecordsByKey(const aKeyFields: TArray<string>; const aKeyValues: TArray<variant>);
    protected
        function GetNameField(): string; virtual;

        function GetAlias(): string; virtual;
        function GetKeyFields(): TArray<string>; virtual;
        function GetNameValues(const aName: string): TArray<variant>; virtual;
        function CreateDataProvider(): TDataProvider; virtual;

        class function FieldValArrayOf(const aFieldVals: array of variant): TArray<variant>;
        class function FieldKeyArrayOf(const aFieldKeys: array of string): TArray<string>;
        procedure SaveRecordsAs(const aKeyFields: TArray<string>; const aOldValues: TArray<variant>;
            const aNewValues: TArray<variant>);
        function ReadName(): string;

    public
        constructor Create(const aTableName: string);
        destructor Destroy(); override;

        procedure SelectAndOpen(const aSQL: string; aReadOnly: boolean);
        procedure SelectAndOpenFmt(const aSQL: string; aArgs: array of const; aReadOnly: boolean);
        procedure SelectAndOpenByKey(const aKeyFields: TArray<string>; const aValues: TArray<variant>;
            aReadOnly: boolean);
        procedure SelectAndOpenByName(const aName: string; aReadOnly: boolean);
        procedure SelectAndOpenAll(aReadOnly: boolean);
        procedure SelectAndOpenAllSorted(aReadOnly: boolean);
        procedure SelectAndOpenAllNames(aReadOnly: boolean);

        procedure Close(); virtual;

        procedure ExecSQL(const aSQL: string);
        procedure ExecSQLFmt(const aSQL: string; aArgs: array of const );

        function ReadAllNames: TStringArray; // entspricht GetNames, aber mit StringArray

        procedure SaveNameAs(const aOldName, aNewName: string); override;
        procedure DeleteName(const aName: string); override;
        function NameExists(const aName: string): boolean; override;

        property FreeQuery: boolean read fFreeQuery write fFreeQuery;
        property DataProvider: TDataProvider read fDataProvider;
        property TableName: string read fTableName;
        property NameField: string read GetNameField;
    end;


implementation


uses
    SysUtils,
    Variants,
    DataAdaptor,
    DataProviderFactory;

{ TQueryDataAdaptor }

constructor TQueryDataAdaptor.Create(const aTableName: string);
begin
    inherited Create;

    fFreeQuery := true;
    fTableName := aTableName;
    fDataProvider := CreateDataProvider();
    ASSERT(Assigned(fDataProvider));
end;

function TQueryDataAdaptor.CreateDataProvider: TDataProvider;
begin
    result := nil;
    if (self.GetAlias = '') then
        EXIT;
    result := TDataProviderFactory.Instance.CreateDataProvider(self.GetAlias);
end;

destructor TQueryDataAdaptor.Destroy();
begin
    if fFreeQuery then
        FreeAndNil(fDataProvider);
    inherited;
end;

function TQueryDataAdaptor.GetAlias: string;
begin
    result := TDataProviderFactory.Instance.MainDBAlias; // Normalfall: Kann überschrieben werden
end;

procedure TQueryDataAdaptor.SelectAndOpen(const aSQL: string; aReadOnly: boolean);
begin
    self.DataProvider.SelectAndOpen(aSQL, aReadOnly);
end;

procedure TQueryDataAdaptor.SelectAndOpenFmt(const aSQL: string; aArgs: array of const; aReadOnly: boolean);
begin
    self.SelectAndOpen(Format(aSQL, aArgs), aReadOnly);
end;

procedure TQueryDataAdaptor.SelectAndOpenAll(aReadOnly: boolean);
begin
    SelectAndOpen('SELECT * FROM "' + self.TableName + '"', aReadOnly);
end;

procedure TQueryDataAdaptor.SelectAndOpenAllNames(aReadOnly: boolean);
begin
    SelectAndOpen('SELECT DISTINCT ' + self.NameField + ' FROM "' + self.TableName + '"', aReadOnly);
end;

procedure TQueryDataAdaptor.SelectAndOpenAllSorted(aReadOnly: boolean);
begin
    SelectAndOpen('SELECT * FROM "' + self.TableName + '" ORDER BY ' + self.NameField, aReadOnly);
end;

procedure TQueryDataAdaptor.SelectAndOpenByKey(const aKeyFields: TArray<string>;
    const aValues: TArray<variant>; aReadOnly: boolean);
var
    xSQLWhere: string;
begin
    xSQLWhere := BuildSQLWhere(aKeyFields, aValues);
    self.SelectAndOpen('SELECT * FROM "' + self.TableName + '" ' + xSQLWhere, aReadOnly);
end;

procedure TQueryDataAdaptor.SelectAndOpenByName(const aName: string; aReadOnly: boolean);
var
    xKeyFields: TArray<string>;
    xValues: TArray<variant>;
begin
    xKeyFields := self.GetKeyFields();
    ASSERT(Assigned(xKeyFields) and (Length(xKeyFields) > 0));
    xValues := GetNameValues(aName);
    self.SelectAndOpenByKey(xKeyFields, xValues, aReadOnly);
end;

procedure TQueryDataAdaptor.ExecSQL(const aSQL: string);
begin
    self.DataProvider.ExecSQL(aSQL);
end;

procedure TQueryDataAdaptor.ExecSQLFmt(const aSQL: string; aArgs: array of const );
begin
    ExecSQL(Format(aSQL, aArgs));
end;

function TQueryDataAdaptor.ReadName(): string;
begin
    if (self.NameField = '') then
    begin
        result := '';
        EXIT;
    end;

    result := self.DataProvider.FieldByName(self.NameField).AsString;
end;

function TQueryDataAdaptor.GetNameField(): string;
begin
    result := '';
end;

function TQueryDataAdaptor.ReadAllNames: TStringArray;
var
    x: integer;
begin
    self.SelectAndOpenAllNames(true);
    try
        SetLength(result, self.DataProvider.RecordCount);
        x := 0;
        while not self.DataProvider.Eof do
        begin
            result[x] := self.ReadName();
            self.DataProvider.Next;
            Inc(x);
        end;
    finally
        self.Close();
    end;
end;

function TQueryDataAdaptor.BuildSQLWhere(const aKeyFields: TArray<string>;
    const aKeyValues: TArray<variant>): string;
var
    x: integer;
    xValue: string;
    xWhereSubPart: string;
    xVarType: TVarType;
begin
    result := '';
    for x := 0 to high(aKeyFields) do
    begin
        if VarIsNull(aKeyValues[x]) then
        begin
            xWhereSubPart := aKeyFields[x] + ' IS NULL';
        end
        else
        begin
            xVarType := VarType(aKeyValues[x]);
            case (xVarType) of
                varUString:
                    xValue := '''' + aKeyValues[x] + '''';

                else
                    xValue := aKeyValues[x];
            end;
            xWhereSubPart := aKeyFields[x] + ' = ' + xValue;
        end;

        if x > 0 then
            result := result + ' AND ';
        result := result + xWhereSubPart;

    end;

    if result <> '' then
        result := 'WHERE ' + result;
end;

procedure TQueryDataAdaptor.DeleteRecordsByKey(const aKeyFields: TArray<string>;
    const aKeyValues: TArray<variant>);
var
    xSQLWhere, xSQL: string;
begin
    xSQLWhere := BuildSQLWhere(aKeyFields, aKeyValues);
    xSQL := 'DELETE FROM "' + self.TableName + '" ' + xSQLWhere;
    ExecSQL(xSQL);
end;

procedure TQueryDataAdaptor.SaveRecordsAs(const aKeyFields: TArray<string>; const aOldValues: TArray<variant>;
    const aNewValues: TArray<variant>);
var
    x, xRecNo: integer;
    xValues: array of TArray<variant>;
    xSourceValue: variant;
    xReplaceIndex: integer;

    function IndexOfReplaceField(const aFieldName: string): integer;
    var
        j: integer;
    begin
        result := -1;
        for j := 0 to high(aKeyFields) do
            if UpperCase(aKeyFields[j]) = UpperCase(aFieldName) then
            begin
                result := j;
                Exit;
            end;
    end;

begin

    // Delete existing records which match newvalues
    DeleteRecordsByKey(aKeyFields, aNewValues);

    SelectAndOpenByKey(aKeyFields, aOldValues, true);
    try
        SetLength(xValues, fDataProvider.RecordCount, fDataProvider.FieldCount);

        xRecNo := 0;
        while not self.DataProvider.Eof do
        begin
            for x := 0 to fDataProvider.FieldCount - 1 do
            begin
                xReplaceIndex := IndexOfReplaceField(self.DataProvider.Fields[x].FieldName);

                if xReplaceIndex >= 0 then
                    xSourceValue := aNewValues[xReplaceIndex]
                else
                    xSourceValue := fDataProvider.Fields[x].Value;

                xValues[xRecNo, x] := xSourceValue;
            end;
            inc(xRecNo);
            self.DataProvider.Next;
        end;
    finally
        Close();
    end;

    SelectAndOpenAll(false);
    try
        for xRecNo := 0 to high(xValues) do
        begin
            fDataProvider.Append;

            for x := 0 to fDataProvider.FieldCount - 1 do
                fDataProvider.Fields[x].Value := xValues[xRecNo, x];

            fDataProvider.Post;
        end;
    finally
        Close();
    end;

end;

function TQueryDataAdaptor.GetNameValues(const aName: string): TArray<variant>;
begin
    result := FieldValArrayOf([aName]);
end;

class function TQueryDataAdaptor.FieldValArrayOf(const aFieldVals: array of variant): TArray<variant>;
var
    i: integer;
begin
    SetLength(result, high(aFieldVals) + 1);
    for i := 0 to high(aFieldVals) do
    begin
        result[i] := aFieldVals[i];
    end;
end;

class function TQueryDataAdaptor.FieldKeyArrayOf(const aFieldKeys: array of string): TArray<string>;
var
    i: integer;
begin
    SetLength(result, high(aFieldKeys) + 1);
    for i := 0 to high(aFieldKeys) do
    begin
        result[i] := aFieldKeys[i];
    end;
end;

procedure TQueryDataAdaptor.SaveNameAs(const aOldName, aNewName: string);
var
    xKeyFields: TArray<string>;
    xOldValues, xNewValues: TArray<variant>;
begin
    xKeyFields := GetKeyFields;
    ASSERT(Assigned(xKeyFields) and (Length(xKeyFields) > 0));
    xOldValues := GetNameValues(aOldName);
    xNewValues := GetNameValues(aNewName);
    SaveRecordsAs(xKeyFields, xOldValues, xNewValues);
end;

procedure TQueryDataAdaptor.DeleteName(const aName: string);
var
    xKeyFields: TArray<string>;
    xValues: TArray<variant>;
begin
    xKeyFields := GetKeyFields;
    ASSERT(Assigned(xKeyFields) and (Length(xKeyFields) > 0));
    xValues := GetNameValues(aName);
    DeleteRecordsByKey(xKeyFields, xValues);
end;

function TQueryDataAdaptor.NameExists(const aName: string): boolean;
begin
    self.SelectAndOpenByName(aName, true);
    try
        result := not self.DataProvider.IsEmpty;
    finally
        Close;
    end;
end;

function TQueryDataAdaptor.GetKeyFields: TArray<string>;
begin
    result := nil;

    if (self.NameField <> '') then
    begin
        result := self.FieldKeyArrayOf([self.NameField]);
    end;
end;

procedure TQueryDataAdaptor.Close;
begin
    self.DataProvider.Close();
end;


end.
