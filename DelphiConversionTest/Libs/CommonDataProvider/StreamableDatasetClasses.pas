{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  06.07.09 pk                                        TN4585.4   Initial revision
  13.07.09 pk  TStreamableField                      TN4585.4   fRequired
  13.07.09 pk  TStreamableFieldList                  TN4585.4   GetFieldNames
  26.08.09 wl  TStreamableField.GetValue             TN4746     wenn self.IsNull, dann ist der Rückgabewert auch Null
  04.11.09 pk                               	        TN4843     Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  19.11.09 pk  GetAsString                           TN4891     converts all field types to string (needed for WRSQL action TSQLRunActionUtility.WriteResultfile)
  19.11.09 pk  GetAsString                           TN4891     set result to '' if field is null
  04.02.10 pk                                        TN4972     changes for Restart
  29.06.10 pk                                        TN5174     Date Field Type implemented
  04.08.10 pk                                        TN5218     Changes for Plugin database packages
  05.08.10 pk                                        TN5221     Changes needed for Component Ace Absolute Database
  13.09.10 pk                                        TN5218   Changes for Plugin database packages
  15.11.10 pk                                        TN5340     Changes to prevent memory leak
  13.09.11 wl  TStreamableField.Destroy              TN5672   Memory leak fixed
  17.07.12 ts  TDataFieldType                        TN5939   new: dftInt64 for LargeInt
  ----------------------------------------------------------------------------------------------------------------------- }

unit StreamableDatasetClasses;


interface


uses
    Streamable,
    GeneralTypes;

type
    TDataFieldType = (dftNone, dftAutoInc, dftString, dftWideString, dftMemo, dftInt16, dftInt32, dftFloat,
        dftBoolean, dftDate, dftTime, dftDateTime, dftFixedChar, dftInt64);

    TStreamableFieldData = TStreamableItem;

    TStreamableFieldDataList = class(TStreamableObjectList)
    private
        function GetDataAt(aIndex: integer): TStreamableFieldData;
    public
        property this[aIndex: integer]: TStreamableFieldData read GetDataAt; default;
    end;

    TStreamableRecord = TStreamableFieldDataList;

    TStreamableRecordList = class(TStreamableObjectList)
    private
        function GetRecordAt(aIndex: integer): TStreamableRecord;
    public
        property this[aIndex: integer]: TStreamableRecord read GetRecordAt; default;
    end;

    TStreamableFieldDef = class(TCustomStreamable)
    private
        fFieldName: string;
        fDataType: TDataFieldType;
        fDataSize: integer;
        fRequired: boolean;
    published
        property FieldName: string read fFieldName write fFieldName;
        property DataType: TDataFieldType read fDataType write fDataType;
        property DataSize: integer read fDataSize write fDataSize;
        property Required: boolean read fRequired write fRequired;
    end;

    TStreamableFieldDefList = class(TStreamableObjectList)
    private
        function GetFieldDefAt(aIndex: Integer): TStreamableFieldDef;
    public
        property this[aIndex: integer]: TStreamableFieldDef read GetFieldDefAt; default;
    end;

    TStreamableField = class(TCustomStreamable)
    private
        fData: TStreamableFieldData;
        fFieldDef: TStreamableFieldDef;
        fIsNull: boolean;
        fChanged: boolean;

        function GetDataType: TDataFieldType;
        function GetDataSize: integer;
        function GetRequired: boolean;
        function GetFieldName: string;

        procedure Changed();
        procedure DataChanged();
        function GetAsString: string;
        procedure SetAsString(const aValue: string);
        function GetAsFloat: double;
        function GetAsInteger: integer;
        procedure SetAsFloat(const aValue: double);
        procedure SetAsInteger(const aValue: integer);
        function GetAsBoolean: boolean;
        procedure SetAsBoolean(const aValue: boolean);
        function GetAsDateTime: TDateTime;
        procedure SetAsDateTime(const aValue: TDateTime);
        function GetAsDate: TDate;
        procedure SetAsDate(const aValue: TDate);
        function GetValue: variant;
        procedure SetValue(const aValue: variant);

        class procedure AssignDataIntern(const aSourceData, aTargetData: TStreamableFieldData;
            const aDataType: TDataFieldType); static;
        procedure SetIsNull(const aValue: boolean);
        procedure SetDefaultValue;

    public
        constructor Create(); override;
        destructor Destroy(); override;
        procedure ClearChanged();
        procedure AssignData(const aData: TStreamableFieldData);
        function CopyData: TStreamableFieldData;
        property AsString: string read GetAsString write SetAsString;
        property AsFloat: double read GetAsFloat write SetAsFloat;
        property AsInteger: integer read GetAsInteger write SetAsInteger;
        property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
        property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
        property AsDate: TDate read GetAsDate write SetAsDate;
        property Value: variant read GetValue write SetValue;
        property IsChanged: boolean read fChanged;

        class function CreateFieldDataFromType(const aDataType: TDataFieldType): TStreamableFieldData; static;
        property DataType: TDataFieldType read GetDataType;
        property DataSize: integer read GetDataSize;
        property Required: boolean read GetRequired;
        property FieldName: string read GetFieldName;
    published
        property FieldDef: TStreamableFieldDef read fFieldDef write fFieldDef;
        property Data: TStreamableFieldData read fData write fData;
        property IsNull: boolean read fIsNull write SetIsNull;
    end;

    TStreamableFieldList = class(TStreamableObjectList)
    private
        function GetFieldAt(aIndex: integer): TStreamableField;
    public
        procedure GetData(const aDataList: TStreamableFieldDataList);
        procedure SetData(const aDataList: TStreamableFieldDataList);
        procedure ClearChanges();
        function FieldByName(const aFieldName: string): TStreamableField;
        function IndexOf(const aFieldName: string): integer;
        function GetFieldNames(): TStringArray;
        property this[aIndex: integer]: TStreamableField read GetFieldAt; default;
    end;

    TStreamableIndexOptions = class(TStreamable)
    private const
        cBitIndexPrimary = 0;

    const
        cBitIndexUnique = 1;

    const
        cBitIndexDescending = 2;

    const
        cBitIndexCaseInsensitive = 3;

    const
        cBitIndexExpression = 4;

    const
        cBitIndexNonMaintained = 5;
    private
        fBitmap: integer;
        function GetIsCaseInsensitive: boolean;
        function GetIsDescending: boolean;
        function GetIsExpression: boolean;
        function GetIsNonMaintained: boolean;
        function GetIsPriamary: boolean;
        function GetIsUnique: boolean;
        procedure SetIsCaseInsensitive(const aValue: boolean);
        procedure SetIsDescending(const aValue: boolean);
        procedure SetIsExpression(const aValue: boolean);
        procedure SetIsNonMaintained(const aValue: boolean);
        procedure SetIsPrimary(const aValue: boolean);
        procedure SetIsUnique(const aValue: boolean);
        class procedure SetOption(var vOptions: integer; aIndex: integer; const aValue: boolean);
        class function GetOption(aOptions: integer; aIndex: integer): boolean;
    public
        property IsPrimary: boolean read GetIsPriamary write SetIsPrimary;
        property IsUnique: boolean read GetIsUnique write SetIsUnique;
        property IsDescending: boolean read GetIsDescending write SetIsDescending;
        property IsCaseInsensitive: boolean read GetIsCaseInsensitive write SetIsCaseInsensitive;
        property IsExpression: boolean read GetIsExpression write SetIsExpression;
        property IsNonMaintained: boolean read GetIsNonMaintained write SetIsNonMaintained;
    published
        property Bitmap: integer read fBitmap write fBitmap;
    end;

    TStreamableIndexField = class(TStreamable)
    private
        fFieldName: string;
    published
        property FieldName: string read fFieldName write fFieldName;
    end;

    TStreamableIndexFields = class(TStreamableObjectList)
    private const
        cDelimiter = ';';
    private
        function GetFieldAt(aIndex: integer): TStreamableIndexField;
        function GetAsDelimitedText: string;
        procedure SetAsDelimitedText(const aValue: string);
        function GetAsTextArray: TStringArray;
        procedure SetAsTextArray(const aArray: TStringArray);
    public
        property AsTextArray: TStringArray read GetAsTextArray write SetAsTextArray;
        property AsDelimitedText: string read GetAsDelimitedText write SetAsDelimitedText;
        property this[aIndex: integer]: TStreamableIndexField read GetFieldAt; default;
    end;

    TStreamableIndexDef = class(TStreamable)
    private
        fIndexName: string;
        fIndexFields: TStreamableIndexFields;
        fIndexOptions: TStreamableIndexOptions;
    public
        constructor Create(); reintroduce;
        destructor Destroy(); override;
        procedure NameIfUnnamed(const aName: string);
    published
        property IndexName: string read fIndexName write fIndexName;
        property IndexFields: TStreamableIndexFields read fIndexFields write fIndexFields;
        property IndexOptions: TStreamableIndexOptions read fIndexOptions write fIndexOptions;
    end;

    TStreamableIndexDefList = class(TStreamableObjectList)
    private
        function GetIndexDefAt(aIndex: integer): TStreamableIndexDef;
    public
        procedure NameUnnamedIndices();
        property this[aIndex: integer]: TStreamableIndexDef read GetIndexDefAt; default;
    end;

    TStreamableTableDef = class(TStreamable)
    private
        fTableName: string;
        fVersion: string;
        fFieldDefs: TStreamableFieldDefList;
        fIndexDefs: TStreamableIndexDefList;
    public
        constructor Create(); reintroduce;
        destructor Destroy(); override;
    published
        property TableName: string read fTableName write fTableName;
        property Version: string read fVersion write fVersion;
        property FieldDefs: TStreamableFieldDefList read fFieldDefs write fFieldDefs;
        property IndexDefs: TStreamableIndexDefList read fIndexDefs write fIndexDefs;
    end;

    TStreamableDatabaseTableInfo = class(TStreamable)
    private
        fTableName: string;
    public
        constructor Create(); reintroduce;
        destructor Destroy(); override;
    published
        property TableName: string read fTableName write fTableName;

    end;

    TStreamableDatabaseTableInfoList = class(TStreamableObjectList)
    private
        function GetTableInfoAt(aIndex: Integer): TStreamableDatabaseTableInfo;
    public
        property this[aIndex: integer]: TStreamableDatabaseTableInfo read GetTableInfoAt; default;
    end;

    TStreamableDatabaseDef = class(TStreamable)
    private
        fTableInfos: TStreamableDatabaseTableInfoList;
    public
        constructor Create(); reintroduce;
        destructor Destroy(); override;
    published
        property TableInfos: TStreamableDatabaseTableInfoList read fTableInfos write fTableInfos;
    end;


implementation


uses
    SysUtils,
    Variants,
    Classes,
    StringUtilities;

{ TStreamableRecordList }

function TStreamableRecordList.GetRecordAt(aIndex: integer): TStreamableRecord;
begin
    result := ( inherited Items[aIndex]) as TStreamableRecord;
end;

{ TStreamableFieldList }

procedure TStreamableFieldList.ClearChanges;
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
    begin
        self[x].ClearChanged();
    end;
end;

function TStreamableFieldList.IndexOf(const aFieldName: string): integer;
var
    x: integer;
begin
    result := -1;
    for x := 0 to self.Count - 1 do
    begin
        if SameText(self[x].FieldName, aFieldName) then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

function TStreamableFieldList.FieldByName(const aFieldName: string): TStreamableField;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := self.IndexOf(aFieldName);
    if xIndex < 0 then
        EXIT;
    result := self[xIndex];
end;

procedure TStreamableFieldList.GetData(const aDataList: TStreamableFieldDataList);
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
        aDataList.Add(self[x].CopyData());
end;

function TStreamableFieldList.GetFieldAt(aIndex: integer): TStreamableField;
begin
    result := ( inherited Items[aIndex]) as TStreamableField;
end;

function TStreamableFieldList.GetFieldNames(): TStringArray;
var
    x: integer;
begin
    SetLength(result, self.Count);
    for x := 0 to self.Count - 1 do
    begin
        result[x] := self[x].FieldName;
    end;

end;

procedure TStreamableFieldList.SetData(const aDataList: TStreamableFieldDataList);
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
    begin
        self[x].AssignData(aDataList[x]);
        self[x].Changed;
    end;
end;

{ TStreamableField }

class procedure TStreamableField.AssignDataIntern(const aSourceData, aTargetData: TStreamableFieldData;
    const aDataType: TDataFieldType);
begin
    case aDataType of
        dftString, dftWideString, dftMemo, dftFixedChar:
            aTargetData.AsStr := aSourceData.AsStr;
        dftInt16, dftInt32, dftInt64, dftAutoInc:
            aTargetData.AsInt := aSourceData.AsInt;
        dftBoolean:
            aTargetData.AsBool := aSourceData.AsBool;
        dftFloat:
            aTargetData.AsFloat := aSourceData.AsFloat;
        dftDateTime:
            aTargetData.AsDateTime := aSourceData.AsDateTime;
        dftDate:
            aTargetData.AsDateTime := aSourceData.AsDateTime;
    end;
end;

constructor TStreamableField.Create();
begin
    inherited Create();
    fData := nil;
end;

destructor TStreamableField.Destroy();
begin
    FreeAndNil(fFieldDef);
    FreeAndNil(fData);
    inherited;
end;

procedure TStreamableField.SetDefaultValue();
begin
    case self.DataType of
        dftString, dftWideString, dftMemo, dftFixedChar:
            self.Data.AsStr := '';
        dftAutoInc, dftInt16, dftInt64, dftInt32:
            self.Data.AsInt := 0;
        dftBoolean:
            self.Data.AsBool := false;
        dftFloat:
            self.Data.AsFloat := 0;
        dftDateTime:
            self.Data.AsDateTime := 0;
        dftDate:
            self.Data.AsDateTime := 0;
    end;
end;

procedure TStreamableField.SetIsNull(const aValue: boolean);
begin
    fIsNull := aValue;
    Changed();

    if not aValue then
        EXIT;
    SetDefaultValue();
end;

procedure TStreamableField.AssignData(const aData: TStreamableFieldData);
begin
    try
        self.IsNull := aData is TNullStreamableItem;
        if self.IsNull then
            EXIT;

        AssignDataIntern(aData, self.Data, self.DataType);

    except
        on e: Exception do
            raise Exception.CreateFmt('Error setting field Date %s - %s', [aData.ClassName, e.Message]);
    end;
end;

class function TStreamableField.CreateFieldDataFromType(const aDataType: TDataFieldType)
    : TStreamableFieldData;
begin
    result := nil;
    case aDataType of
        dftWideString, dftFixedChar:
            result := TStrStreamableItem.Create();
        dftString, dftMemo:
            result := TAnsiStrStreamableItem.Create();
        dftInt16, dftInt32, dftInt64, dftAutoInc:
            result := TIntStreamableItem.Create();
        dftBoolean:
            result := TBoolStreamableItem.Create();
        dftFloat:
            result := TFloatStreamableItem.Create();
        dftDateTime, dftDate:
            result := TDateTimeStreamableItem.Create();
    end;
end;

function TStreamableField.GetDataSize: integer;
begin
    result := fFieldDef.DataSize;
end;

function TStreamableField.GetDataType: TDataFieldType;
begin
    result := fFieldDef.DataType;
end;

function TStreamableField.GetFieldName: string;
begin
    result := fFieldDef.FieldName;
end;

function TStreamableField.GetRequired: boolean;
begin
    result := fFieldDef.Required;
end;

function TStreamableField.CopyData(): TStreamableFieldData;
begin
    if self.IsNull then
    begin
        result := TNullStreamableItem.Create();
        EXIT;
    end;

    result := CreateFieldDataFromType(self.DataType);
    AssignDataIntern(self.Data, result, self.DataType);
end;

procedure TStreamableField.Changed;
begin
    fChanged := true;
end;

function TStreamableField.GetAsBoolean: boolean;
begin
    result := fData.AsBool;
end;

function TStreamableField.GetAsDate: TDate;
begin
    result := fData.AsDateTime;
end;

function TStreamableField.GetAsDateTime: TDateTime;
begin
    result := fData.AsDateTime;
end;

function TStreamableField.GetAsFloat: double;
begin
    result := fData.AsFloat;
end;

function TStreamableField.GetAsInteger: integer;
begin
    result := fData.AsInt;
end;

function TStreamableField.GetAsString: string;
begin
    result := '';
    if self.IsNull then
        EXIT;

    case self.DataType of
        dftString, dftWideString, dftMemo, dftFixedChar:
            result := self.Data.AsStr;
        dftAutoInc, dftInt16, dftInt32, dftInt64:
            result := IntToStr(self.Data.AsInt);
        dftBoolean:
            result := BoolToStr(self.Data.AsBool);
        dftFloat:
            result := FloatToStr(self.Data.AsFloat);
        dftDateTime:
            result := DateTimeToStr(self.Data.AsDateTime);
        dftDate:
            result := DateToStr(self.Data.AsDateTime);
    end;
end;

function TStreamableField.GetValue: variant;
begin
    if self.IsNull then
    begin
        result := Null;
        EXIT;
    end;

    case self.DataType of
        dftString, dftWideString, dftMemo, dftFixedChar:
            result := self.Data.AsStr;
        dftAutoInc, dftInt16, dftInt32, dftInt64:
            result := self.Data.AsInt;
        dftBoolean:
            result := self.Data.AsBool;
        dftFloat:
            result := self.Data.AsFloat;
        dftDateTime:
            result := self.Data.AsDateTime;
        dftDate:
            result := self.Data.AsDateTime
    end;
end;

procedure TStreamableField.DataChanged();
begin
    self.IsNull := false;
    Changed();
end;

procedure TStreamableField.SetAsBoolean(const aValue: boolean);
begin
    fData.AsBool := aValue;
    DataChanged();
end;

procedure TStreamableField.SetAsDate(const aValue: TDate);
begin
    fData.AsDateTime := aValue;
    DataChanged();
end;

procedure TStreamableField.SetAsDateTime(const aValue: TDateTime);
begin
    fData.AsDateTime := aValue;
    DataChanged();
end;

procedure TStreamableField.SetAsFloat(const aValue: double);
begin
    fData.AsFloat := aValue;
    DataChanged();
end;

procedure TStreamableField.SetAsInteger(const aValue: integer);
begin
    fData.AsInt := aValue;
    DataChanged();
end;

procedure TStreamableField.SetAsString(const aValue: string);
begin
    fData.AsStr := aValue;
    DataChanged();
end;

procedure TStreamableField.SetValue(const aValue: variant);
begin
    if VarIsNull(aValue) then
    begin
        self.IsNull := true;
    end
    else
    begin

        case self.DataType of
            dftString, dftWideString, dftMemo, dftFixedChar:
                self.AsString := aValue;
            dftAutoInc, dftInt16, dftInt32, dftInt64:
                self.AsInteger := aValue;
            dftBoolean:
                self.AsBoolean := aValue;
            dftFloat:
                self.AsFloat := aValue;
            dftDateTime:
                self.AsDateTime := aValue;
            dftDate:
                self.AsDate := aValue;
        end;
    end;
end;

procedure TStreamableField.ClearChanged();
begin
    fChanged := false;
end;

{ TStreamableFieldDataList }

function TStreamableFieldDataList.GetDataAt(aIndex: integer): TStreamableFieldData;
begin
    result := ( inherited Items[aIndex]) as TStreamableFieldData;
end;

{ TStreamableFieldDefList }

function TStreamableFieldDefList.GetFieldDefAt(aIndex: Integer): TStreamableFieldDef;
begin
    result := ( inherited Items[aIndex]) as TStreamableFieldDef;
end;

{ TStreamableTable }

constructor TStreamableTableDef.Create;
begin
    inherited Create();
    fTableName := 'Unknown';
    fVersion := '0.0.0';
    fFieldDefs := TStreamableFieldDefList.Create();
    fIndexDefs := TStreamableIndexDefList.Create();
end;

destructor TStreamableTableDef.Destroy();
begin
    FreeAndNil(fIndexDefs);
    FreeAndNil(fFieldDefs);

    inherited;
end;

{ TStreamableIndexDefList }

function TStreamableIndexDefList.GetIndexDefAt(aIndex: integer): TStreamableIndexDef;
begin
    result := ( inherited Items[aIndex]) as TStreamableIndexDef;
end;

procedure TStreamableIndexDefList.NameUnnamedIndices;
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
    begin
        self[x].NameIfUnnamed('Index' + IntToStr(x + 1));
    end;

end;

{ TStreamableIndexDef }

constructor TStreamableIndexDef.Create;
begin
    inherited Create();
    fIndexFields := TStreamableIndexFields.Create();
    fIndexOptions := TStreamableIndexOptions.Create();
end;

destructor TStreamableIndexDef.Destroy;
begin
    FreeAndNil(fIndexOptions);
    FreeAndNil(fIndexFields);
    inherited;
end;

procedure TStreamableIndexDef.NameIfUnnamed(const aName: string);
begin
    if fIndexName = '' then
        fIndexName := aName;
end;

{ TStreamableIndexOptions }

class procedure TStreamableIndexOptions.SetOption(var vOptions: integer; aIndex: integer;
    const aValue: boolean);
begin
    if aValue then
        vOptions := vOptions or (1 shl aIndex)
    else
        vOptions := vOptions and (not(1 shl aIndex));
end;

class function TStreamableIndexOptions.GetOption(aOptions: integer; aIndex: integer): boolean;
begin
    result := (((aOptions shr aIndex) and 1) = 1);
end;

function TStreamableIndexOptions.GetIsPriamary: boolean;
begin
    result := GetOption(fBitmap, cBitIndexPrimary);
end;

function TStreamableIndexOptions.GetIsUnique: boolean;
begin
    result := GetOption(fBitmap, cBitIndexUnique);
end;

function TStreamableIndexOptions.GetIsDescending: boolean;
begin
    result := GetOption(fBitmap, cBitIndexDescending);
end;

function TStreamableIndexOptions.GetIsCaseInsensitive: boolean;
begin
    result := GetOption(fBitmap, cBitIndexCaseInsensitive);
end;

function TStreamableIndexOptions.GetIsExpression: boolean;
begin
    result := GetOption(fBitmap, cBitIndexExpression);
end;

function TStreamableIndexOptions.GetIsNonMaintained: boolean;
begin
    result := GetOption(fBitmap, cBitIndexNonMaintained);
end;

procedure TStreamableIndexOptions.SetIsPrimary(const aValue: boolean);
begin
    SetOption(fBitmap, cBitIndexPrimary, aValue);
end;

procedure TStreamableIndexOptions.SetIsUnique(const aValue: boolean);
begin
    SetOption(fBitmap, cBitIndexUnique, aValue);
end;

procedure TStreamableIndexOptions.SetIsDescending(const aValue: boolean);
begin
    SetOption(fBitmap, cBitIndexDescending, aValue);
end;

procedure TStreamableIndexOptions.SetIsCaseInsensitive(const aValue: boolean);
begin
    SetOption(fBitmap, cBitIndexCaseInsensitive, aValue);
end;

procedure TStreamableIndexOptions.SetIsExpression(const aValue: boolean);
begin
    SetOption(fBitmap, cBitIndexExpression, aValue);
end;

procedure TStreamableIndexOptions.SetIsNonMaintained(const aValue: boolean);
begin
    SetOption(fBitmap, cBitIndexNonMaintained, aValue);
end;

{ TStreamableIndexFields }

function TStreamableIndexFields.GetFieldAt(aIndex: integer): TStreamableIndexField;
begin
    result := ( inherited Items[aIndex]) as TStreamableIndexField;
end;

procedure TStreamableIndexFields.SetAsTextArray(const aArray: TStringArray);
var
    x: integer;
    xField: TStreamableIndexField;
begin
    self.Clear();
    for x := 0 to Length(aArray) - 1 do
    begin
        xField := TStreamableIndexField.Create();
        xField.FieldName := aArray[x];
        self.Add(xField);
    end;
end;

function TStreamableIndexFields.GetAsTextArray: TStringArray;
var
    x: integer;
begin
    SetLength(result, self.Count);
    for x := 0 to self.Count - 1 do
    begin
        result[x] := self[x].FieldName;
    end;
end;

function TStreamableIndexFields.GetAsDelimitedText: string;
begin
    result := TStringUtilities.StringArrayToString(self.AsTextArray, cDelimiter);
end;

procedure TStreamableIndexFields.SetAsDelimitedText(const aValue: string);
begin
    self.AsTextArray := TStringUtilities.StringToStringArray(aValue, cDelimiter);
end;

{ TStreamableDatabaseTableInfo }

constructor TStreamableDatabaseTableInfo.Create;
begin
    inherited;
end;

destructor TStreamableDatabaseTableInfo.Destroy;
begin

    inherited;
end;

{ TStreamableDatabaseTableInfoList }

function TStreamableDatabaseTableInfoList.GetTableInfoAt(aIndex: Integer): TStreamableDatabaseTableInfo;
begin
    result := self.Items[aIndex] as TStreamableDatabaseTableInfo;
end;
{ TStreamableDatabaseDef }

constructor TStreamableDatabaseDef.Create;
begin
    inherited Create();
    fTableInfos := TStreamableDatabaseTableInfoList.Create();
end;

destructor TStreamableDatabaseDef.Destroy;
begin

    inherited;
end;


end.
