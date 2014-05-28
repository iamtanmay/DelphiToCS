{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  13.07.09 pk                                        TN4585.4  Initial Revision
  13.07.09 pk  Destroy                               TN4585.4  Close no longer called
  04.11.09 pk                               	        TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  23.11.09 pk  SetFieldDataToDBField                 TN4891    call DBField.Clear instead of DBField.Value = null
  04.02.10 pk                                        TN4972    changes for Restart
  18.06.10 pk                                        TN5152.1  new: this is still needed for Forms wit dataaware components
  29.06.10 pk                                        TN5174    Date Field implemented
  04.08.10 pk                                        TN5218    Changes for Plugin database packages
  05.08.10 pk                                        TN5221   Changes needed for Component Ace Absolute Database
  13.09.10 pk                                        TN5218   Changes for Plugin database packages
  03.11.10 pk  CopyRecordsFrom                       TN5328   use CopyData function
  15.02.11 pk  CopyRecordsFrom                       TN4780    new SourceTableName Param
  02.03.11 wl  ExecSQL                               TN5491    ruft ExecSQLIntern auf, das dann überschrieben wird
  02.03.11 wl  OpenSQLIntern                         TN5491    neu: OnBeforeOpenSQL
  13.04.11 wl                                        TN5491   neues Konzept: statt IDataProvider wird ein Pointer übergeben
  29.06.11 wl  ExecSQLIntern                         TN5613    result = RowsAffected
  17.08.11 wl  RowsAffected                          TN5613    entfernt
  17.07.12 ts  TDatasetFieldAdaptor                  TN5939    new: dftInt64 for LargeInt
  12.07.13 ts  TDatasetFieldAdaptor                  TN6202    ftBCD: dftString; Workaround, da SS5 bsp nicht als string erkannt wird
  15.07.13 ts  TDatasetFieldAdaptor                  TN6202.1  ftWideMemo: dftMemo; ist am Ende alles ein String
  13.02.14 ts  FieldTypeFromDBFieldType              TN6358    entfernt ASSERT(false, 'FieldType not implemented');
  ----------------------------------------------------------------------------------------------------------------------- }

unit DatasetDataProvider;


interface


uses
    db,
    GeneralTypes,
    DataConnectionParams,
    StreamableDatasetClasses,
    DataProvider;

type
    TDatasetFieldAdaptor = class
    private
        fFields: TDataFields;
        fDataset: TDataset;
    public
        constructor Create(const aDataset: TDataset);
        destructor Destroy; override;
        procedure WriteData;
        procedure ReadData;
        procedure ReadFields;
        property Fields: TDataFields read fFields;

        class procedure SetFieldDataFromDBField(const aField: TDataField; const aDBField: TField);
        class procedure SetFieldDataToDBField(const aField: TDataField; const aDBField: TField);
        class function CreateField(const aFieldDef: TStreamableFieldDef): TDataField;
        class function FieldTypeFromDBFieldType(const aFieldType: TFieldType): TDataFieldType;
        class function CreateFieldDefFromDBFieldDef(const aFieldDef: TFieldDef): TStreamableFieldDef;
        class procedure FieldDefsFromDBFieldDefs(const aFieldDefs: TStreamableFieldDefList;
            const aDBFieldDefs: TFieldDefs);
        class function DBFieldTypeFromFieldType(const aFieldType: TDataFieldType): TFieldType;

        class function GetDBIndexOptionsFromIndexOptions(const aIndexOptions: TStreamableIndexOptions)
            : TIndexOptions;
        class procedure SetDBIndexOptionsToIndexOptions(const aDBIndexOptions: TIndexOptions;
            const aIndexOptions: TStreamableIndexOptions);

        class procedure SetDBIndexDefFromIndexDef(const aDBIndexDef: TIndexDef;
            const aIndexDef: TStreamableIndexDef);
        class procedure SetIndexDefFromDBIndexDef(const aIndexDef: TStreamableIndexDef;
            const aDBIndexDef: TIndexDef);
    end;

    TDatasetAdaptor = class
    protected
        fDataset: TDataset;
        fFieldsAdaptor: TDatasetFieldAdaptor;
        procedure ReadData;
        procedure ReadFields;
        procedure WriteData;
        function GetActive: boolean;
        function GetBof: boolean;
        function GetEof: boolean;
        function GetRecordCount: Integer;
        function GetFieldCount: Integer;
        function GetFields: TDataFields;
        function GetIsInsertState(): boolean;
        function GetRecNo: integer;
        function GetRealDataset: TDataset;
    public
        constructor Create(const aDataset: TDataset);
        destructor Destroy; override;
        procedure Open();
        procedure Close;
        procedure Delete;
        procedure Append;
        procedure Edit;
        function FieldByName(const aFieldName: string): TDataField;
        procedure First;
        procedure Insert;
        function IsEmpty: Boolean;
        procedure Last;
        procedure Next;
        procedure Post;
        procedure Prior;
        procedure MoveBy(aOffset: integer);
        function GetFieldNames(): TStringArray;
        procedure Refresh;
        procedure AddRecords(const aRecords: TStreamableRecordList);
        property Active: Boolean read GetActive;
        property Bof: Boolean read GetBof;
        property Eof: Boolean read GetEof; { Upper case EOF conflicts with C++ }
        property FieldCount: Integer read GetFieldCount;
        property Fields: TDataFields read GetFields;
        property RecordCount: Integer read GetRecordCount;
        property RecNo: integer read GetRecNo;
        property IsInsertState: boolean read GetIsInsertState;
    end;

    TDatasetDataProvider = class(TDataProvider)
    strict private
        fDatasetAdaptor: TDatasetAdaptor;
    strict protected
        function GetActive: boolean; override;
        function GetBof: boolean; override;
        function GetEof: boolean; override;
        function GetRecordCount: Integer; override;
        function GetFieldCount: Integer; override;
        function GetFields: TDataFields; override;
        function GetIsInsertState(): boolean; override;
        function GetRecNo: integer; override;
        procedure OpenIntern(const aSQL: string);
        function ExecSQLIntern(const aSQL: string): integer; virtual; abstract;
    public
        constructor Create(const aDataset: TDataset; const aConnectionParams: TDataConnectionParams);
        destructor Destroy; override;
        procedure Append; override;
        procedure Close; override;
        procedure Delete; override;
        procedure Edit; override;
        function FieldByName(const aFieldName: string): TDataField; override;
        procedure First; override;
        procedure Insert; override;
        function IsEmpty: Boolean; override;
        procedure Last; override;
        procedure Next; override;
        procedure Post; override;
        procedure Prior; override;
        procedure MoveBy(aOffset: integer); override;
        function GetFieldNames(): TStringArray; override;
        procedure Refresh; override;
        function ExecSQL(const aSQL: string): integer; override;
        procedure CopyRecordsFrom(const aTableName: string; const aSourceDataProvider: TDataProvider;
            const aSourceTableName: string); override;

        // 18.06.10 pk this is a HACK because some Forms still use DataAware components.  This function should NOT be PUBLIC!!!!
        function GetRealDataset(): TDataset;
    end;


implementation


uses
    SysUtils,
    Classes,
    Streamable;

{ TDatasetFieldAdaptor }

class function TDatasetFieldAdaptor.CreateFieldDefFromDBFieldDef(const aFieldDef: TFieldDef)
    : TStreamableFieldDef;
var
    xFieldType: TDataFieldType;
begin
    xFieldType := FieldTypeFromDBFieldType(aFieldDef.DataType);

    result := TStreamableFieldDef.Create();
    result.FieldName := aFieldDef.Name;
    result.DataType := xFieldType;
    result.DataSize := aFieldDef.Size;
    result.Required := aFieldDef.Required;
end;

constructor TDatasetFieldAdaptor.Create(const aDataset: TDataset);
begin
    inherited Create();
    fFields := TDataFields.Create();
    fDataset := aDataset;
end;

destructor TDatasetFieldAdaptor.Destroy();
begin
    FreeAndNil(fFields);
    inherited;
end;

class procedure TDatasetFieldAdaptor.FieldDefsFromDBFieldDefs(const aFieldDefs: TStreamableFieldDefList;
    const aDBFieldDefs: TFieldDefs);

var
    x: integer;
    xFieldDef: TStreamableFieldDef;
begin
    aFieldDefs.Clear();
    for x := 0 to aDBFieldDefs.Count - 1 do
    begin
        xFieldDef := CreateFieldDefFromDBFieldDef(aDBFieldDefs[x]);
        aFieldDefs.Add(xFieldDef);
    end;
end;

class function TDatasetFieldAdaptor.FieldTypeFromDBFieldType(const aFieldType: TFieldType): TDataFieldType;
begin
    case aFieldType of
        ftAutoInc:
            result := dftAutoInc;
        ftString:
            result := dftString;
        ftFixedChar:
            result := dftFixedChar;
        ftWideString:
            result := dftWideString;
        ftMemo:
            result := dftMemo;
        ftWideMemo:
            result := dftMemo;
        ftWord, ftSmallint:
            result := dftInt16;
        ftInteger:
            result := dftInt32;
        ftBoolean:
            result := dftBoolean;
        ftFloat:
            result := dftFloat;
        ftDateTime:
            result := dftDateTime;
        ftDate:
            result := dftDate;
        ftLargeInt:
            result := dftInt64;
        ftBCD:
            result := dftString;
        else
            result := dftString;
            // ASSERT(false, 'FieldType not implemented');
    end;
end;

class function TDatasetFieldAdaptor.DBFieldTypeFromFieldType(const aFieldType: TDataFieldType): TFieldType;
begin
    result := ftUnknown;

    case aFieldType of
        dftAutoInc:
            result := ftAutoInc;
        dftString:
            result := ftString;
        dftFixedChar:
            result := ftFixedChar;
        dftWideString:
            result := ftWideString;
        dftMemo:
            result := ftMemo;
        dftInt16:
            result := ftSmallint;
        dftInt32:
            result := ftInteger;
        dftInt64:
            result := ftLargeInt;
        dftBoolean:
            result := ftBoolean;
        dftFloat:
            result := ftFloat;
        dftDateTime:
            result := ftDateTime;
        dftDate:
            result := ftDate;
        else
            ASSERT(false, 'FieldType not implemented');
    end;
end;

class function TDatasetFieldAdaptor.CreateField(const aFieldDef: TStreamableFieldDef): TDataField;
var
    xData: TStreamableItem;
begin

    result := TStreamableField.Create();
    result.FieldDef := aFieldDef;
    xData := TDataField.CreateFieldDataFromType(aFieldDef.DataType);
    result.Data := xData;
end;

procedure TDatasetFieldAdaptor.ReadFields();
var
    x: integer;
    xDBFieldDef: TFieldDef;
    xFieldDef: TStreamableFieldDef;
begin
    fFields.Clear();
    for x := 0 to fDataset.FieldDefs.Count - 1 do
    begin
        xDBFieldDef := fDataset.FieldDefs[x];
        xFieldDef := CreateFieldDefFromDBFieldDef(xDBFieldDef);
        fFields.Add(CreateField(xFieldDef));
    end;
end;

class function TDatasetFieldAdaptor.GetDBIndexOptionsFromIndexOptions(const aIndexOptions
    : TStreamableIndexOptions): TIndexOptions;
begin
    result := [];
    if aIndexOptions.IsPrimary then
        result := result + [ixPrimary];

    if aIndexOptions.IsUnique then
        result := result + [ixUnique];

    if aIndexOptions.IsDescending then
        result := result + [ixDescending];

    if aIndexOptions.IsCaseInsensitive then
        result := result + [ixCaseInsensitive];

    if aIndexOptions.IsExpression then
        result := result + [ixExpression];

    if aIndexOptions.IsNonMaintained then
        result := result + [ixNonMaintained];
end;

class procedure TDatasetFieldAdaptor.SetDBIndexOptionsToIndexOptions(const aDBIndexOptions: TIndexOptions;
    const aIndexOptions: TStreamableIndexOptions);
begin
    aIndexOptions.IsPrimary := ixPrimary in aDBIndexOptions;
    aIndexOptions.IsUnique := ixUnique in aDBIndexOptions;
    aIndexOptions.IsDescending := ixDescending in aDBIndexOptions;
    aIndexOptions.IsCaseInsensitive := ixCaseInsensitive in aDBIndexOptions;
    aIndexOptions.IsExpression := ixExpression in aDBIndexOptions;
    aIndexOptions.IsNonMaintained := ixNonMaintained in aDBIndexOptions;
end;

class procedure TDatasetFieldAdaptor.SetDBIndexDefFromIndexDef(const aDBIndexDef: TIndexDef;
    const aIndexDef: TStreamableIndexDef);
begin
    aDBIndexDef.Name := aIndexDef.IndexName;
    aDBIndexDef.Fields := aIndexDef.IndexFields.AsDelimitedText;
    aDBIndexDef.Options := GetDBIndexOptionsFromIndexOptions(aIndexDef.IndexOptions);
end;

class procedure TDatasetFieldAdaptor.SetIndexDefFromDBIndexDef(const aIndexDef: TStreamableIndexDef;
    const aDBIndexDef: TIndexDef);
begin
    aIndexDef.IndexName := aDBIndexDef.Name;
    aIndexDef.IndexFields.AsDelimitedText := aDBIndexDef.Fields;
    SetDBIndexOptionsToIndexOptions(aDBIndexDef.Options, aIndexDef.IndexOptions);
end;

class procedure TDatasetFieldAdaptor.SetFieldDataFromDBField(const aField: TDataField;
    const aDBField: TField);
var
    xFieldType: TDataFieldType;
begin
    aField.IsNull := aDBField.IsNull;
    if aField.IsNull then
        EXIT;

    xFieldType := FieldTypeFromDBFieldType(aDBField.DataType);
    try
        case xFieldType of
            dftAutoInc:
                aField.AsInteger := aDBField.AsInteger;
            dftString, dftWideString, dftMemo, dftFixedChar:
                aField.AsString := aDBField.AsString;
            dftInt16, dftInt32, dftInt64:
                aField.AsInteger := aDBField.AsInteger;
            dftBoolean:
                aField.AsBoolean := aDBField.AsBoolean;
            dftFloat:
                aField.AsFloat := aDBField.AsFloat;
            dftDateTime:
                aField.AsDateTime := aDBField.AsDateTime;
            dftDate:
                aField.AsDate := aDBField.AsDateTime;
        end;
    except
        on e: Exception do
            raise Exception.CreateFmt('Error setting field %s - %s', [aDBField.FieldName, e.Message]);
    end;
end;

procedure TDatasetFieldAdaptor.ReadData();
var
    x: integer;
begin
    for x := 0 to fDataset.Fields.Count - 1 do
    begin
        SetFieldDataFromDBField(fFields[x], fDataset.Fields[x]);
    end;
    fFields.ClearChanges();
end;

class procedure TDatasetFieldAdaptor.SetFieldDataToDBField(const aField: TDataField; const aDBField: TField);
var
    xFieldType: TDataFieldType;
begin
    if not aField.IsChanged then
        EXIT;

    if aField.IsNull then
    begin
        aDBField.Clear;
        EXIT;
    end;

    xFieldType := FieldTypeFromDBFieldType(aDBField.DataType);
    case xFieldType of
        dftString, dftWideString, dftMemo, dftFixedChar:
            aDBField.AsString := aField.AsString;
        dftInt16, dftInt32, dftInt64:
            aDBField.AsInteger := aField.AsInteger;
        dftBoolean:
            aDBField.AsBoolean := aField.AsBoolean;
        dftFloat:
            aDBField.AsFloat := aField.AsFloat;
        dftDateTime:
            aDBField.AsDateTime := aField.AsDateTime;
    end;
end;

procedure TDatasetFieldAdaptor.WriteData();
var
    x: integer;
begin
    for x := 0 to fDataset.Fields.Count - 1 do
    begin
        SetFieldDataToDBField(fFields[x], fDataset.Fields[x]);
    end;
end;

{ TDatasetAdaptor }

constructor TDatasetAdaptor.Create(const aDataset: TDataset);
begin
    inherited Create;

    fDataset := aDataset;

    fFieldsAdaptor := TDatasetFieldAdaptor.Create(fDataset);
    ASSERT(Assigned(fDataset));
end;

destructor TDatasetAdaptor.Destroy();
begin
    FreeAndNil(fFieldsAdaptor);
    inherited;
end;

procedure TDatasetAdaptor.Open();
begin
    fDataset.Open;

    ReadFields;
    ReadData();
end;

procedure TDatasetAdaptor.Append();
begin
    fDataset.Append();
    ReadData();
end;

procedure TDatasetAdaptor.Close();
begin
    fDataset.Close();
end;

function TDatasetAdaptor.GetEof(): boolean;
begin
    result := fDataset.Eof;
end;

procedure TDatasetAdaptor.Post;
begin
    WriteData();
    fDataset.Post();
    ReadData();
end;

function TDatasetAdaptor.GetActive(): boolean;
begin
    result := fDataset.Active;
end;

function TDatasetAdaptor.GetBof: boolean;
begin
    result := fDataset.Bof;
end;

procedure TDatasetAdaptor.Delete;
begin
    fDataset.Delete;
    ReadData();
end;

procedure TDatasetAdaptor.Edit;
begin
    fDataset.Edit;
end;

function TDatasetAdaptor.FieldByName(const aFieldName: string): TDataField;
begin
    result := self.Fields.FieldByName(aFieldName);
end;

procedure TDatasetAdaptor.Next();
begin
    fDataset.Next;
    ReadData();
end;

procedure TDatasetAdaptor.Prior();
begin
    fDataset.Prior;
    ReadData();
end;

procedure TDatasetAdaptor.First;
begin
    fDataset.First;
    ReadData();
end;

procedure TDatasetAdaptor.Last;
begin
    fDataset.Last;
    ReadData();
end;

procedure TDatasetAdaptor.MoveBy(aOffset: integer);
begin
    fDataset.MoveBy(aOffset);
    self.ReadData();
end;

function TDatasetAdaptor.GetRealDataset: TDataset;
begin
    result := fDataset;
end;

function TDatasetAdaptor.GetRecNo: integer;
begin
    result := fDataset.RecNo;
end;

function TDatasetAdaptor.GetRecordCount: Integer;
begin
    result := fDataset.RecordCount;
end;

procedure TDatasetAdaptor.Insert;
begin
    fDataset.Insert;
    self.ReadData();
end;

function TDatasetAdaptor.IsEmpty: Boolean;
begin
    result := fDataset.IsEmpty;
end;

procedure TDatasetAdaptor.ReadData;
begin
    fFieldsAdaptor.ReadData();
end;

procedure TDatasetAdaptor.ReadFields;
begin
    fFieldsAdaptor.ReadFields();
end;

procedure TDatasetAdaptor.Refresh;
begin
    fDataset.Refresh;
    ReadData();
end;

function TDatasetAdaptor.GetFieldCount: Integer;
begin
    result := fDataset.FieldCount;
end;

function TDatasetAdaptor.GetFields: TDataFields;
begin
    result := fFieldsAdaptor.Fields;
end;

function TDatasetAdaptor.GetIsInsertState: boolean;
begin
    result := fDataset.State = dsInsert;
end;

function TDatasetAdaptor.GetFieldNames(): TStringArray;
var
    xList: TStrings;
    x: integer;
begin
    xList := TStringList.Create();
    try
        fDataset.GetFieldNames(xList);
        SetLength(result, xList.Count);
        for x := 0 to xList.Count - 1 do
            result[x] := xList[x];
    finally
        FreeAndNil(xList);
    end;
end;

procedure TDatasetAdaptor.WriteData;
begin
    fFieldsAdaptor.WriteData;
end;

procedure TDatasetAdaptor.AddRecords(const aRecords: TStreamableRecordList);
var
    xRecord: TStreamableRecord;
    i, x: integer;
    xFieldData: TStreamableFieldData;
begin
    for i := 0 to aRecords.Count - 1 do
    begin
        xRecord := aRecords[i];
        self.Append;

        for x := 0 to xRecord.Count - 1 do
        begin
            xFieldData := xRecord[x];
            try
                self.Fields[x].AssignData(xFieldData);
            except
                on e: exception do
                begin
                    raise Exception.CreateFmt('Error assigning value for Field %s - %s',
                        [self.Fields[x].FieldName, e.Message]);
                end;

            end;
        end;
        self.Post;
    end;
end;

{ TDatasetDataProvider }

constructor TDatasetDataProvider.Create(const aDataset: TDataset;
    const aConnectionParams: TDataConnectionParams);
begin
    inherited Create(aConnectionParams);
    fDatasetAdaptor := TDatasetAdaptor.Create(aDataset);
end;

destructor TDatasetDataProvider.Destroy;
begin
    FreeAndNil(fDatasetAdaptor);
    inherited;
end;

procedure TDatasetDataProvider.Append;
begin
    fDatasetAdaptor.Append;
end;

procedure TDatasetDataProvider.Close;
begin
    fDatasetAdaptor.Close;
end;

procedure TDatasetDataProvider.Delete;
begin
    fDatasetAdaptor.Delete;
end;

procedure TDatasetDataProvider.Edit;
begin
    fDatasetAdaptor.Edit;
end;

function TDatasetDataProvider.ExecSQL(const aSQL: string): integer;
begin
    EXIT(self.ExecSQLIntern(aSQL));
end;

function TDatasetDataProvider.FieldByName(const aFieldName: string): TDataField;
begin
    result := fDatasetAdaptor.FieldByName(aFieldName);
end;

procedure TDatasetDataProvider.First;
begin
    fDatasetAdaptor.First;
end;

function TDatasetDataProvider.GetActive: boolean;
begin
    result := fDatasetAdaptor.GetActive;
end;

function TDatasetDataProvider.GetBof: boolean;
begin
    result := fDatasetAdaptor.GetBof;
end;

function TDatasetDataProvider.GetEof: boolean;
begin
    result := fDatasetAdaptor.GetEof;
end;

function TDatasetDataProvider.GetFieldCount: Integer;
begin
    result := fDatasetAdaptor.GetFieldCount;
end;

function TDatasetDataProvider.GetFieldNames: TStringArray;
begin
    result := fDatasetAdaptor.GetFieldNames;
end;

function TDatasetDataProvider.GetFields: TDataFields;
begin
    result := fDatasetAdaptor.GetFields;
end;

function TDatasetDataProvider.GetIsInsertState: boolean;
begin
    result := fDatasetAdaptor.GetIsInsertState;
end;

function TDatasetDataProvider.GetRealDataset: TDataset;
begin
    result := fDatasetAdaptor.GetRealDataset;
end;

function TDatasetDataProvider.GetRecNo: integer;
begin
    result := fDatasetAdaptor.GetRecNo;
end;

function TDatasetDataProvider.GetRecordCount: Integer;
begin
    result := fDatasetAdaptor.GetRecordCount;
    // für Import-DataProvider
    if Assigned(fOnRecordCount) then
        result := fOnRecordCount(result)
end;

procedure TDatasetDataProvider.Insert;
begin
    fDatasetAdaptor.Insert;
end;

function TDatasetDataProvider.IsEmpty: Boolean;
begin
    result := fDatasetAdaptor.IsEmpty;
end;

procedure TDatasetDataProvider.Last;
begin
    fDatasetAdaptor.Last;
end;

procedure TDatasetDataProvider.MoveBy(aOffset: integer);
begin
    fDatasetAdaptor.MoveBy(aOffset);
end;

procedure TDatasetDataProvider.Next;
begin
    fDatasetAdaptor.Next;
end;

procedure TDatasetDataProvider.OpenIntern(const aSQL: string);
begin
    fDatasetAdaptor.Open;

    if Assigned(fOnAfterOpen) then
        fOnAfterOpen(self);
end;

procedure TDatasetDataProvider.Post;
begin
    fDatasetAdaptor.Post;
end;

procedure TDatasetDataProvider.Prior;
begin
    fDatasetAdaptor.Prior;
end;

procedure TDatasetDataProvider.Refresh;
begin
    fDatasetAdaptor.Refresh;
end;

procedure TDatasetDataProvider.CopyRecordsFrom(const aTableName: string;
    const aSourceDataProvider: TDataProvider; const aSourceTableName: string);
var
    x: integer;
    xData: TStreamableFieldData;
    xField: TStreamableField;
begin
    self.SelectAndOpenAll(aTableName, false);
    try
        aSourceDataProvider.SelectAndOpenAll(aSourceTableName, true);
        try
            while not aSourceDataProvider.Eof do
            begin

                self.Append;

                for x := 0 to aSourceDataProvider.Fields.Count - 1 do
                begin
                    try

                        xField := aSourceDataProvider.Fields[x];
                        xData := xField.CopyData();
                        try
                            self.Fields[x].AssignData(xData);
                        finally
                            FreeAndNil(xData);
                        end;
                    except
                        on e: exception do
                        begin
                            raise Exception.CreateFmt('Error assigning value for Field %s - %s',
                                [self.Fields[x].FieldName, e.Message]);
                        end;

                    end;
                end;

                self.Post;
                aSourceDataProvider.Next;
            end;
        finally
            aSourceDataProvider.Close();
        end;
    finally
        self.Close();
    end;
end;


end.
