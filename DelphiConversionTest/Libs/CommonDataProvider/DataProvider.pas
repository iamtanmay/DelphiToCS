{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Should be the only object that uses dbTables
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  02.10.07 wl                               TN3811.5  initial version
  04.04.08 wl  PrepareConnection            TN4058    von ImportDataProvider hierher
  04.04.08 wl  SetCachedUpdates             TN4058    von ImportDataProvider hierher
  04.04.08 wl  DatasetApplyUpdates          TN4058    von ImportDataProvider hierher
  11.06.08 wl  SelectAndOpen                TN4143    führt fOnAfterOpen aus, wenn vorhanden
  06.07.09 pk                               TN4585.4  Now virtual class, BDE-specific code moved to BDEDataProvider
  13.07.09 pk                               TN4585.4  Locate removed
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  16.12.09 pk  TDataProviderFactory         TN4933    New function: CreateDatabaseProvider
  04.02.10 pk                               TN4972    changes for Restart
  17.06.10 pk                               TN5152.1  DataFactory moved to new unit
  04.08.10 pk                               TN5218    Changes for Plugin database packages
  05.08.10 pk                               TN5221    Changes needed for Component Ace Absolute Database
  13.09.10 pk                               TN5218    Changes for Plugin database packages
  15.02.11 pk  CopyRecordsFrom              TN4780    new SourceTableName Param
  01.03.11 wl  IDataProvider                TN5491    Neu: IDataProvider in unit DLLInterface
  02.03.11 wl  fOnBeforeOpenSQL,-ExecSQL    TN5491    Neu: Damit können alle SQL's vor der Ausführung geloggt werden
  02.03.11 wl  FieldByName...               TN5491    jetzt auch writable
  08.03.11 wl  SelectAndOpenAll             TN5497    new parameter: OrderBy
  13.04.11 wl                               TN5491    neues Konzept: statt IDataProvider wird ein Pointer übergeben
  29.06.11 wl  ExecSQLIntern                TN5613    result = RowsAffected
  13.07.11 ts  DestroyDatabase              TN5627    new (explicit close/destroy of database)
  17.08.11 wl  DestroyDatabase              TN5654   Änderungen wieder rückgängig gemacht
  17.08.11 wl  PrepareConnection            TN5654   entfernt
  15.12.11 wl                               TN5767   uses geändert
  01.10.13 wl  ExecStoredProcedure          TN6264   angelegt für Stored Procedures
  15.10.13 pbp ExecStoredProcedure          TN6264.1 Für Output Parameter die Prozedursignatur angepasst
  30.10.13 pbp ExecStoredProcedure          TN6293   Oracle Result hinzufügen (Rückgabe wird in Variable gespeichert)
  13.12.13 ts  ExecStoredProcedure          TN6330   Arrays ermöglicht für InputParams
  -------------------------------------------------------------------------------------------------- }

unit DataProvider;


interface


uses
    GeneralTypes,
    StreamableDatasetClasses,
    Streamable,
    DataConnectionParams,
    SQLParser;

type
    TOnGetRecordCountEvent = function(aTotalRecordCount: integer): integer of object;

    TDataField = TStreamableField;
    TDataFields = TStreamableFieldList;

    TDataProvider = class;

    TOnAfterOpen = procedure(aDataSet: TDataProvider) of object;

    TDataProvider = class
    strict protected
        fConnectionParams: TDataConnectionParams;
        fOnAfterOpen: TOnAfterOpen;
        fOnRecordCount: TOnGetRecordCountEvent;

        function GetActive: boolean; virtual; abstract;
        function GetBof: boolean; virtual; abstract;
        function GetEof: boolean; virtual; abstract;
        function GetFieldCount: Integer; virtual; abstract;
        function GetRecordCount: Integer; virtual; abstract;
        function GetRecNo: integer; virtual; abstract;
        function GetFields: TDataFields; virtual; abstract;
        function GetAfterOpen: TOnAfterOpen;
        procedure SetAfterOpen(const aValue: TOnAfterOpen);
        function GetIsInsertState(): boolean; virtual; abstract;
    public
        constructor Create(const aConnectionParams: TDataConnectionParams);
        destructor Destroy; override;

        procedure SelectAndOpen(const aSQL: string; aReadOnly: boolean); virtual; abstract;
        procedure SelectAndOpenBySQLRec(const aSQL: TSQLRec; const aReadOnly: boolean); virtual; abstract;
        procedure SelectAndOpenAll(const aTableName: string; const aReadOnly: boolean; aOrderBy: string = '');
        function ExecSQL(const aSQL: string): integer; virtual; abstract;
        // function ExecStoredProcedure(const aProcedureName: string; const aParameters: array of variant)
        // : TArray<variant>; virtual;
        function ExecStoredProcedure(const aProcedureName: string; const aInputParams: TStreamableObjectList;
            const aOutputParams: TArray<variant>; out oOracleResult: string): TArray<variant>; virtual;

        procedure Append; virtual; abstract;
        procedure Close; virtual; abstract;
        procedure Delete; virtual; abstract;
        procedure Edit; virtual; abstract;
        procedure First; virtual; abstract;
        procedure Insert; virtual; abstract;
        function IsEmpty: Boolean; virtual; abstract;
        procedure Last; virtual; abstract;
        procedure Next; virtual; abstract;
        procedure Post; virtual; abstract;
        procedure Prior; virtual; abstract;
        procedure MoveBy(aOffset: integer); virtual; abstract;
        function GetFieldNames(): TArray<string>; virtual; abstract;
        procedure Refresh; virtual; abstract;

        function FieldByName(const aFieldName: string): TDataField; virtual; abstract;

        function FieldByNameAsString(const aFieldName: string): string; overload;
        procedure FieldByNameAsString(const aFieldName: string; const aValue: string); overload;
        function FieldByNameAsInt(const aFieldName: string): integer; overload;
        procedure FieldByNameAsInt(const aFieldName: string; aValue: integer); overload;
        function FieldByNameAsFloat(const aFieldName: string): double; overload;
        procedure FieldByNameAsFloat(const aFieldName: string; aValue: double); overload;
        function FieldByNameAsBoolean(const aFieldName: string): boolean; overload;
        procedure FieldByNameAsBoolean(const aFieldName: string; aValue: boolean); overload;
        function FieldByNameAsDateTime(const aFieldName: string): TDateTime; overload;
        procedure FieldByNameAsDateTime(const aFieldName: string; aValue: TDateTime); overload;

        function FieldAsString(const aIndex: integer): string; overload;
        procedure FieldAsString(const aIndex: integer; const aValue: string); overload;
        function FieldAsInt(const aIndex: integer): integer; overload;
        procedure FieldAsInt(const aIndex: integer; aValue: integer); overload;
        function FieldAsFloat(const aIndex: integer): double; overload;
        procedure FieldAsFloat(const aIndex: integer; aValue: double); overload;
        function FieldAsBoolean(const aIndex: integer): boolean; overload;
        procedure FieldAsBoolean(const aIndex: integer; aValue: boolean); overload;
        function FieldAsDateTime(const aIndex: integer): TDateTime; overload;
        procedure FieldAsDateTime(const aIndex: integer; aValue: TDateTime); overload;

        procedure SetCachedUpdates(aCachedUpdates: boolean); virtual; abstract;
        procedure DatasetApplyUpdates(); virtual; abstract;
        procedure CopyRecordsFrom(const aTableName: string; const aSourceDataProvider: TDataProvider;
            const aSourceTableName: string); virtual; abstract;

        class function GetFieldDataSize(aDataType: TDataFieldType; aDataSize: integer): integer;
        class function GetFieldTypeAsStr(aDataType: TDataFieldType): string;
        class procedure CopyDatasetRecord(aSourceDataset, aTargetDataset: TDataProvider; aAppend: boolean;
            aReplaceFields: array of string; aReplaceValues: array of variant);

        property Active: Boolean read GetActive;
        property Bof: Boolean read GetBof;
        property Eof: Boolean read GetEof; { Upper case EOF conflicts with C++ }
        property FieldCount: Integer read GetFieldCount;
        property RecordCount: Integer read GetRecordCount;
        property RecNo: integer read GetRecNo;
        property Fields: TDataFields read GetFields;
        property IsInsertState: boolean read GetIsInsertState;
        property OnAfterOpen: TOnAfterOpen read GetAfterOpen write SetAfterOpen;
        property OnRecordCount: TOnGetRecordCountEvent read fOnRecordCount write fOnRecordCount;
    end;


implementation


uses
    SysUtils;

{ TDataProvider }

constructor TDataProvider.Create(const aConnectionParams: TDataConnectionParams);
begin
    inherited Create;
    fConnectionParams := aConnectionParams;
end;

destructor TDataProvider.Destroy();
begin
    FreeAndNil(fConnectionParams);
    inherited;
end;

// function TDataProvider.ExecStoredProcedure(const aProcedureName: string; const aParameters: array of variant)
// : TArray<variant>;
function TDataProvider.ExecStoredProcedure(const aProcedureName: string;
    const aInputParams: TStreamableObjectList; const aOutputParams: TArray<variant>;
    out oOracleResult: string): TArray<variant>;
begin
    raise Exception.Create('Stored procedure [' + aProcedureName + '] not implemented');
end;

function TDataProvider.FieldAsBoolean(const aIndex: integer): boolean;
begin
    EXIT(self.Fields[aIndex].AsBoolean);
end;

function TDataProvider.FieldAsDateTime(const aIndex: integer): TDateTime;
begin
    EXIT(self.Fields[aIndex].AsDateTime);
end;

function TDataProvider.FieldAsFloat(const aIndex: integer): double;
begin
    EXIT(self.Fields[aIndex].AsFloat);
end;

function TDataProvider.FieldAsInt(const aIndex: integer): integer;
begin
    EXIT(self.Fields[aIndex].AsInteger);
end;

function TDataProvider.FieldAsString(const aIndex: integer): string;
begin
    EXIT(self.Fields[aIndex].AsString);
end;

function TDataProvider.FieldByNameAsBoolean(const aFieldName: string): boolean;
begin
    EXIT(self.FieldByName(aFieldName).AsBoolean);
end;

function TDataProvider.FieldByNameAsDateTime(const aFieldName: string): TDateTime;
begin
    EXIT(self.FieldByName(aFieldName).AsDateTime);
end;

function TDataProvider.FieldByNameAsFloat(const aFieldName: string): double;
begin
    EXIT(self.FieldByName(aFieldName).AsFloat);
end;

function TDataProvider.FieldByNameAsInt(const aFieldName: string): integer;
begin
    EXIT(self.FieldByName(aFieldName).AsInteger);
end;

procedure TDataProvider.FieldAsBoolean(const aIndex: integer; aValue: boolean);
begin
    self.Fields[aIndex].AsBoolean := aValue;
end;

procedure TDataProvider.FieldAsDateTime(const aIndex: integer; aValue: TDateTime);
begin
    self.Fields[aIndex].AsDateTime := aValue;
end;

procedure TDataProvider.FieldAsFloat(const aIndex: integer; aValue: double);
begin
    self.Fields[aIndex].AsFloat := aValue;
end;

procedure TDataProvider.FieldAsInt(const aIndex: integer; aValue: integer);
begin
    self.Fields[aIndex].AsInteger := aValue;
end;

procedure TDataProvider.FieldAsString(const aIndex: integer; const aValue: string);
begin
    self.Fields[aIndex].AsString := aValue;
end;

procedure TDataProvider.FieldByNameAsBoolean(const aFieldName: string; aValue: boolean);
begin
    self.FieldByName(aFieldName).AsBoolean := aValue;
end;

procedure TDataProvider.FieldByNameAsDateTime(const aFieldName: string; aValue: TDateTime);
begin
    self.FieldByName(aFieldName).AsDateTime := aValue;
end;

procedure TDataProvider.FieldByNameAsFloat(const aFieldName: string; aValue: double);
begin
    self.FieldByName(aFieldName).AsFloat := aValue;
end;

procedure TDataProvider.FieldByNameAsInt(const aFieldName: string; aValue: integer);
begin
    self.FieldByName(aFieldName).AsInteger := aValue;
end;

procedure TDataProvider.FieldByNameAsString(const aFieldName, aValue: string);
begin
    self.FieldByName(aFieldName).AsString := aValue;
end;

function TDataProvider.FieldByNameAsString(const aFieldName: string): string;
begin
    EXIT(self.FieldByName(aFieldName).AsString);
end;

function TDataProvider.GetAfterOpen(): TOnAfterOpen;
begin
    result := fOnAfterOpen;
end;

procedure TDataProvider.SelectAndOpenAll(const aTableName: string; const aReadOnly: boolean;
    aOrderBy: string);
var
    xSQLRec: TSQLRec;
begin
    xSQLRec.Select := '*';
    xSQLRec.Orderby := aOrderBy;
    xSQLRec.From := aTableName;
    self.SelectAndOpenBySQLRec(xSQLRec, aReadOnly);
end;

procedure TDataProvider.SetAfterOpen(const aValue: TOnAfterOpen);
begin
    fOnAfterOpen := aValue;
end;

class function TDataProvider.GetFieldTypeAsStr(aDataType: TDataFieldType): string;
begin
    Result := '';
    case aDataType of
        dftNone:
            exit;
        dftAutoInc:
            result := 'AutoInc';
        dftWideString:
            result := 'String (WideString)';
        dftFixedChar:
            result := 'String (FixedChar)';
        dftString:
            result := 'String (Ansi)';
        dftInt16:
            result := 'Int16';
        dftInt32:
            result := 'Int32';
        dftBoolean:
            result := 'Boolean';
        dftFloat:
            result := 'Float64';
        dftDate:
            result := 'Date';
        dftTime:
            result := 'Time';
        dftDateTime:
            result := 'Date+Time';
        dftMemo:
            result := 'Memo';
    end;
end;

class function TDataProvider.GetFieldDataSize(aDataType: TDataFieldType; aDataSize: integer): integer;
begin
    result := 1;
    case aDataType of
        dftString, dftWidestring:
            result := aDatasize - 1;
        dftNone:
            result := 0;
    end;
end;

class procedure TDataProvider.CopyDatasetRecord(aSourceDataset, aTargetDataset: TDataProvider;
    aAppend: boolean; aReplaceFields: array of string; aReplaceValues: array of variant);
var
    i: integer;
    xSourceValue: variant;
    xReplaceIndex: integer;
    function IndexOfReplaceField(aFieldName: string): integer;
    var
        j: integer;
    begin
        result := -1;
        aFieldName := UpperCase(aFieldName);
        for j := 0 to high(aReplaceFields) do
            if UpperCase(aReplaceFields[j]) = aFieldName then
            begin
                result := j;
                Exit;
            end;
    end;

begin
    if aSourceDataset.Eof then
        Exit;

    if aAppend then
    begin
        aTargetDataset.Append;
    end
    else
    begin
        aTargetDataset.Edit;
        if aTargetDataset.Eof then
            Exit;
    end;

    for i := 0 to aTargetDataset.FieldCount - 1 do
    begin
        if (aSourceDataset.FieldCount - 1) < i then
            Break;
        xReplaceIndex := IndexOfReplaceField(aTargetDataset.Fields[i].FieldName);

        if xReplaceIndex >= 0 then
            xSourceValue := aReplaceValues[xReplaceIndex]
        else
            xSourceValue := aSourceDataset.Fields[i].Value;

        aTargetDataset.Fields[i].Value := xSourceValue;
    end;

    aTargetDataset.Post;
end;


end.
