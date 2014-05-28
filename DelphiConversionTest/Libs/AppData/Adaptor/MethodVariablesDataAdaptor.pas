{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  16.01.13 wl                                      TN6055   Initial Revision
  22.02.13 wl                                      TN6094   Neu: RequestOrder
  08.03.13 wl                                      TN6095   Neu: DialogType, DialogCaption, DialogHide
  11.03.13 wl                                      TN6095   Neu: ArrayLengthRefToOrder
  19.04.13 wl                                      TN6095   neues Feld DataIsArray
  14.05.13 wl                                      TN6095   DialogCaption entfernt
  ----------------------------------------------------------------------------------------------------------- }

unit MethodVariablesDataAdaptor;


interface


uses
    DataProvider,
    MethodVariableTypes,
    QueryDataAdaptor;

type
    TMethodVariablesDataAdaptor = class(TQueryDataAdaptor)
    private const
        cMethodVariablesTable = 'METHODVARIABLES';
        cFieldNameMethodName = 'MethodName';
        cFieldNameVariableName = 'VariableName';
        cFieldNameRequestOrder = 'RequestOrder';
        cFieldNameRequestText = 'RequestText';
        cFieldNameDefaultValue = 'DefaultValue';
        cFieldNameMinCheck = 'MinCheck';
        cFieldNameMinValue = 'MinValue';
        cFieldNameMaxCheck = 'MaxCheck';
        cFieldNameMaxValue = 'MaxValue';
        cFieldNamePickList = 'PickList';
        cFieldNameDataType = 'DataType';
        cFieldNameDataIsArray = 'DataIsArray';
        cFieldNameDialogHide = 'DialogHide';
        cFieldNameArrayLengthRefToOrder = 'ArrayLengthRefToOrder';
    protected
        function GetNameField(): string; override;
    public
        constructor Create();

        class function ReadRecFromDataset(aDataset: TDataProvider; var vRec: TMethodVariableRec): boolean;
        class function ReadRecsFromDataset(aDataset: TDataProvider): TArray<TMethodVariableRec>;

        class procedure WriteRecToDataset(aDataset: TDataProvider; const aRec: TMethodVariableRec;
            aAppend: boolean);
        class procedure WriteRecsToDataset(aDataset: TDataProvider; const aRecs: TArray<TMethodVariableRec>;
            aAppend: boolean);

        function ReadAllRecs(): TArray<TMethodVariableRec>;
        function ReadRecs(const aMethodName: string): TArray<TMethodVariableRec>;
        procedure WriteRecs(const aMethodName: string; aRecs: TArray<TMethodVariableRec>);

        procedure SelectAndOpenByVarName(const aMethodName, aVariableName: string; aReadOnly: boolean);
        function ReadVariableRec(const aMethodName, aVariableName: string): TMethodVariableData;
        procedure WriteVariableRec(const aMethodName, aVariableName: string; const aRec: TMethodVariableData);
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    Generics.Collections;

{ TMethodVariablesDataAdaptor }

constructor TMethodVariablesDataAdaptor.Create;
begin
    inherited Create(cMethodVariablesTable);
end;

class function TMethodVariablesDataAdaptor.ReadRecFromDataset(aDataset: TDataProvider;
    var vRec: TMethodVariableRec): boolean;
begin
    if aDataset.Eof then
        EXIT(true);

    vRec.MethodName := aDataset.FieldbyName(cFieldNameMethodName).AsString;
    vRec.VariableName := aDataset.FieldbyName(cFieldNameVariableName).AsString;
    vRec.RequestOrder := aDataset.FieldbyName(cFieldNameRequestOrder).AsInteger;
    vRec.RequestText := aDataset.FieldbyName(cFieldNameRequestText).AsString;
    vRec.DefaultValue := aDataset.FieldbyName(cFieldNameDefaultValue).AsString;
    vRec.MinCheck := aDataset.FieldbyName(cFieldNameMinCheck).AsBoolean;
    vRec.MinValue := aDataset.FieldbyName(cFieldNameMinValue).AsFloat;
    vRec.MaxCheck := aDataset.FieldbyName(cFieldNameMaxCheck).AsBoolean;
    vRec.MaxValue := aDataset.FieldbyName(cFieldNameMaxValue).AsFloat;
    vRec.PickList := aDataset.FieldbyName(cFieldNamePickList).AsString;
    vRec.DataType := aDataset.FieldbyName(cFieldNameDataType).AsInteger;
    vRec.DataIsArray := aDataset.FieldbyName(cFieldNameDataIsArray).AsBoolean;
    vRec.DialogHide := aDataset.FieldbyName(cFieldNameDialogHide).AsBoolean;
    vRec.ArrayLengthRefToOrder := aDataset.FieldbyName(cFieldNameArrayLengthRefToOrder).AsInteger;

    EXIT(false);
end;

class function TMethodVariablesDataAdaptor.ReadRecsFromDataset(aDataset: TDataProvider)
    : TArray<TMethodVariableRec>;
var
    x: integer;
begin
    x := 0;
    SetLength(result, aDataset.RecordCount);
    while not aDataset.Eof do
    begin
        ReadRecFromDataset(aDataset, result[x]);
        aDataset.Next;
        Inc(x);
    end;
end;

class procedure TMethodVariablesDataAdaptor.WriteRecToDataset(aDataset: TDataProvider;
    const aRec: TMethodVariableRec; aAppend: boolean);
begin
    ASSERT(aDataSet.Active, 'Dataset not active');
    if aAppend then
        aDataset.Append
    else
        aDataset.Edit;
    aDataset.FieldbyName(cFieldNameMethodName).AsString := aRec.MethodName;
    aDataset.FieldbyName(cFieldNameVariableName).AsString := aRec.VariableName;
    aDataset.FieldbyName(cFieldNameRequestOrder).AsInteger := aRec.RequestOrder;
    aDataset.FieldbyName(cFieldNameRequestText).AsString := aRec.RequestText;
    aDataset.FieldbyName(cFieldNameDefaultValue).AsString := aRec.DefaultValue;
    aDataset.FieldbyName(cFieldNameMinCheck).AsBoolean := aRec.MinCheck;
    aDataset.FieldbyName(cFieldNameMinValue).AsFloat := aRec.MinValue;
    aDataset.FieldbyName(cFieldNameMaxCheck).AsBoolean := aRec.MaxCheck;
    aDataset.FieldbyName(cFieldNameMaxValue).AsFloat := aRec.MaxValue;
    aDataset.FieldbyName(cFieldNamePickList).AsString := aRec.PickList;
    aDataset.FieldbyName(cFieldNameDataType).AsInteger := aRec.DataType;
    aDataset.FieldbyName(cFieldNameDataIsArray).AsBoolean := aRec.DataIsArray;
    aDataset.FieldbyName(cFieldNameDialogHide).AsBoolean := aRec.DialogHide;
    aDataset.FieldbyName(cFieldNameArrayLengthRefToOrder).AsInteger := aRec.ArrayLengthRefToOrder;

    aDataset.Post;
end;

procedure TMethodVariablesDataAdaptor.WriteRecs(const aMethodName: string; aRecs: TArray<TMethodVariableRec>);
begin
    // dann schreiben
    self.SelectAndOpenAll(false);
    try
        WriteRecsToDataset(self.DataProvider, aRecs, true);
    finally
        self.Close();
    end;
end;

class procedure TMethodVariablesDataAdaptor.WriteRecsToDataset(aDataset: TDataProvider;
    const aRecs: TArray<TMethodVariableRec>; aAppend: boolean);
var
    x: integer;
begin
    for x := 0 to high(aRecs) do
    begin
        WriteRecToDataset(aDataset, aRecs[x], true);
    end;
end;

function TMethodVariablesDataAdaptor.GetNameField: string;
begin
    EXIT(cFieldNameMethodName);
end;

function TMethodVariablesDataAdaptor.ReadAllRecs(): TArray<TMethodVariableRec>;
begin
    self.SelectAndOpenAll(true);
    try
        EXIT(ReadRecsFromDataset(self.DataProvider));
    finally
        self.Close();
    end;
end;

function TMethodVariablesDataAdaptor.ReadRecs(const aMethodName: string): TArray<TMethodVariableRec>;
begin
    self.SelectAndOpenByName(aMethodName, true);
    try
        EXIT(ReadRecsFromDataset(self.DataProvider));
    finally
        self.Close();
    end;
end;

procedure TMethodVariablesDataAdaptor.SelectAndOpenByVarName(const aMethodName, aVariableName: string;
    aReadOnly: boolean);
begin
    self.SelectAndOpen('select * from ' + self.TableName + ' where ' + cFieldNameMethodName + '=''' +
        aMethodName + ''' and ' + cFieldNameVariableName + '=''' + aVariableName + '''', aReadOnly);
end;

function TMethodVariablesDataAdaptor.ReadVariableRec(const aMethodName, aVariableName: string)
    : TMethodVariableData;
var
    xRec: TMethodVariableRec;
begin
    SelectAndOpenByVarName(aMethodName, aVariableName, true);
    try
        if ReadRecFromDataset(self.DataProvider, xRec) then
            EXIT(TMethodVariableUtils.GetEmptyRec())
        else
            EXIT(TMethodVariableUtils.MethodVariableRecToData(xRec));
    finally
        self.Close();
    end;
end;

procedure TMethodVariablesDataAdaptor.WriteVariableRec(const aMethodName, aVariableName: string;
    const aRec: TMethodVariableData);
begin
    SelectAndOpenByVarName(aMethodName, aVariableName, false);
    try
        self.WriteRecToDataset(self.DataProvider, TMethodVariableUtils.MethodVariableDataToRec(aMethodName,
            aVariableName, aRec), self.DataProvider.Eof);
    finally
        self.Close();
    end;
end;


end.
