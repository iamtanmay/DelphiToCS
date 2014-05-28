{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  21.12.12 wl                                      TN6055   Initial Revision
  22.02.13 wl                                      TN6094   Neu: RequestOrder
  08.03.13 wl                                      TN6095   Neu: DialogType, DialogCaption, DialogHide
  11.03.13 wl                                      TN6095   Neu: ArrayLengthRefToOrder
  19.04.13 wl                                      TN6095   neues Feld DataIsArray
  14.05.13 wl                                      TN6095   DialogCaption entfernt
  ----------------------------------------------------------------------------------------------------------- }

unit MethodVariablesTableUpdate;


interface


uses
    Generics.Collections,
    Update,
    TableStructDef,
    TableUpdate;

type
    TMethodVariablesConversionRec = record
        VariableName: string;
        DefaultValue: string;
        MinValue: string;
        MaxValue: string;
        PickList: string;
        RequestText: string;
        RequestFormat: integer;
    end;

    // table structure definitions
    TMethodVariablesTableStructDefV0 = class(TTableStructDef)
    protected
        procedure DoDefineStruct(); override;
    end;

    TMethodVariablesTableStructDefV1 = class(TMethodVariablesTableStructDefV0)
    protected
        procedure DoDefineStruct(); override;
    end;

    // table updates
    TMethodVariablesTableUpdateV1 = class(TTableUpdate)
    strict private
        fVarSettings: TArray<TMethodVariablesConversionRec>;
        function GetMethodNames(): TArray<string>;
        function GetVarSettingsNamesForMethod(const aMethodName: string)
            : TArray<TMethodVariablesConversionRec>;
        procedure ReadDataFromSettings();
        procedure ReadValueFromSettings(const aCombinedName, aValue: string);
        function GetVariableIndexAndAdd(const aVariableName: string): integer;
        procedure MoveDataFromSettings(aSender: TObject);
    public
        constructor Create(aUpdateNumber: TUpdateID);
    end;


implementation


uses
    SysUtils,
    DataProvider;

const
    INT_REVISION_1 = 1;
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
    cFieldNameDialogCaption = 'DialogCaption';
    cFieldNameDialogHide = 'DialogHide';
    cFieldNameArrayLengthRefToOrder = 'ArrayLengthRefToOrder';

    { TMethodVariablesTableStructDefV0 }

procedure TMethodVariablesTableStructDefV0.DoDefineStruct;
begin
    inherited;
    fName := cMethodVariablesTable;
end;

{ TMethodVariablesTableStructDefV1 }

procedure TMethodVariablesTableStructDefV1.DoDefineStruct();
const
    cFieldLengthMethodName = 50;
    cFieldLengthVariableName = 100;
    cFieldLengthRequestText = 200;
    cFieldLengthDefaultValue = 100;
begin
    inherited;
    AddField(cFieldNameMethodName, tftString, cFieldLengthMethodName);
    AddField(cFieldNameVariableName, tftString, cFieldLengthVariableName);
    AddField(cFieldNameRequestOrder, tftInteger, 0);
    AddField(cFieldNameRequestText, tftString, cFieldLengthRequestText);
    AddField(cFieldNameDefaultValue, tftString, cFieldLengthDefaultValue);
    AddField(cFieldNameMinCheck, tftBoolean, 0);
    AddField(cFieldNameMinValue, tftFloat, 0);
    AddField(cFieldNameMaxCheck, tftBoolean, 0);
    AddField(cFieldNameMaxValue, tftFloat, 0);
    AddField(cFieldNamePickList, tftMemo, 0);
    AddField(cFieldNameDataType, tftInteger, 0);
    AddField(cFieldNameDataIsArray, tftBoolean, 0);
    AddField(cFieldNameDialogCaption, tftMemo, 0);
    AddField(cFieldNameDialogHide, tftBoolean, 0);
    AddField(cFieldNameArrayLengthRefToOrder, tftInteger, 0);

    AddIndex(cFieldNameMethodName + ';' + cFieldNameVariableName);
end;

{ TMethodVariablesTableUpdateV1 }

constructor TMethodVariablesTableUpdateV1.Create(aUpdateNumber: TUpdateID);
begin
    inherited Create(aUpdateNumber, TMethodVariablesTableStructDefV0, INT_REVISION_1);
    AlterStructure(TMethodVariablesTableStructDefV1);
    CopyMatchingFields([]);

    self.CustomDataFunc(MoveDataFromSettings);
end;

procedure TMethodVariablesTableUpdateV1.ReadDataFromSettings();
var
    xSourceDP: TDataProvider;
begin
    SetLength(fVarSettings, 0);

    xSourceDP := fTableChangeAdaptor.CreateSourceDataProvider();
    try
        xSourceDP.SelectAndOpen
            ('select * from SETTINGS where [AREA]=''APPLICATION'' and  [SECTION]=''MethodParameters''', false);

        while (not xSourceDP.Eof) do
        begin
            ReadValueFromSettings(xSourceDP.FieldByName('IDENT').AsString,
                xSourceDP.FieldByName('VALUE').AsString);

            xSourceDP.Next;
        end;

        xSourceDP.Close;
    finally
        xSourceDP.Free;
    end;
end;

procedure TMethodVariablesTableUpdateV1.ReadValueFromSettings(const aCombinedName, aValue: string);
const
    STR_DEFVALUE_SUFFIX = 'DefValue';
    STR_MINVALUE_SUFFIX = 'MinValue';
    STR_MAXVALUE_SUFFIX = 'MaxValue';
    STR_PICKLIST_SUFFIX = 'PickList';
    STR_FORMAT_SUFFIX = 'ValFormat';
    STR_VALUE_SUFFIX = 'Value';
var
    xPosition: integer;
    xVariableName: string;
    xIndex: integer;
begin
    xPosition := Pos(STR_DEFVALUE_SUFFIX, aCombinedName);
    if (xPosition > 0) and (xPosition = Length(aCombinedName) - Length(STR_DEFVALUE_SUFFIX) + 1) then
    begin
        xVariableName := Copy(aCombinedName, 1, xPosition - 1);
        xIndex := GetVariableIndexAndAdd(xVariableName);
        fVarSettings[xIndex].DefaultValue := aValue;
        EXIT;
    end;

    xPosition := Pos(STR_MINVALUE_SUFFIX, aCombinedName);
    if (xPosition > 0) and (xPosition = Length(aCombinedName) - Length(STR_MINVALUE_SUFFIX) + 1) then
    begin
        xVariableName := Copy(aCombinedName, 1, xPosition - 1);
        xIndex := GetVariableIndexAndAdd(xVariableName);
        fVarSettings[xIndex].MinValue := aValue;
        EXIT;
    end;

    xPosition := Pos(STR_MAXVALUE_SUFFIX, aCombinedName);
    if (xPosition > 0) and (xPosition = Length(aCombinedName) - Length(STR_MAXVALUE_SUFFIX) + 1) then
    begin
        xVariableName := Copy(aCombinedName, 1, xPosition - 1);
        xIndex := GetVariableIndexAndAdd(xVariableName);
        fVarSettings[xIndex].MaxValue := aValue;
        EXIT;
    end;

    xPosition := Pos(STR_PICKLIST_SUFFIX, aCombinedName);
    if (xPosition > 0) and (xPosition = Length(aCombinedName) - Length(STR_PICKLIST_SUFFIX) + 1) then
    begin
        xVariableName := Copy(aCombinedName, 1, xPosition - 1);
        xIndex := GetVariableIndexAndAdd(xVariableName);
        fVarSettings[xIndex].PickList := aValue;
        EXIT;
    end;

    xPosition := Pos(STR_FORMAT_SUFFIX, aCombinedName);
    if (xPosition > 0) and (xPosition = Length(aCombinedName) - Length(STR_FORMAT_SUFFIX) + 1) then
    begin
        xVariableName := Copy(aCombinedName, 1, xPosition - 1);
        xIndex := GetVariableIndexAndAdd(xVariableName);
        fVarSettings[xIndex].RequestFormat := StrToIntDef(aValue, 0);
        EXIT;
    end;

    xPosition := Pos(STR_VALUE_SUFFIX, aCombinedName);
    if (xPosition > 0) and (xPosition = Length(aCombinedName) - Length(STR_VALUE_SUFFIX) + 1) then
    begin
        // alte Value-Einträge: nicht beachten
        EXIT;
    end;

    // RequestText: Ohne Suffix
    xIndex := GetVariableIndexAndAdd(aCombinedName);
    fVarSettings[xIndex].RequestText := aValue;
end;

function TMethodVariablesTableUpdateV1.GetMethodNames: TArray<string>;
var
    xSourceDP: TDataProvider;
    xResultList: TList<string>;
begin
    xResultList := TList<string>.Create();
    try
        xSourceDP := fTableChangeAdaptor.CreateSourceDataProvider();
        try
            xSourceDP.SelectAndOpen('select distinct [NAME] from METHOD', false);

            while (not xSourceDP.Eof) do
            begin
                xResultList.Add(xSourceDP.FieldByName('NAME').AsString);
                xSourceDP.Next;
            end;

            xSourceDP.Close;
        finally
            xSourceDP.Free;
        end;

        EXIT(xResultList.ToArray());
    finally
        FreeAndNil(xResultList);
    end;
end;

function TMethodVariablesTableUpdateV1.GetVariableIndexAndAdd(const aVariableName: string): integer;
var
    x: integer;
begin
    for x := 0 to high(fVarSettings) do
    begin
        if (fVarSettings[x].VariableName = aVariableName) then
            EXIT(x);
    end;

    result := Length(fVarSettings);
    SetLength(fVarSettings, result + 1);
    fVarSettings[result].VariableName := aVariableName;
end;

function TMethodVariablesTableUpdateV1.GetVarSettingsNamesForMethod(const aMethodName: string)
    : TArray<TMethodVariablesConversionRec>;
var
    xSourceDP: TDataProvider;
    xResultList: TList<TMethodVariablesConversionRec>;
    xSetting: TMethodVariablesConversionRec;
    xAllOptions: string;
begin
    xResultList := TList<TMethodVariablesConversionRec>.Create();
    try
        xAllOptions := '';
        xSourceDP := fTableChangeAdaptor.CreateSourceDataProvider();
        try
            xSourceDP.SelectAndOpen('select * from METHOD where NAME=''' + aMethodName + '''', true);

            while (not xSourceDP.Eof) do
            begin
                // einfach alles hitereinanderschreiben
                xAllOptions := xAllOptions + xSourceDP.FieldByName('AOPTIONS').AsString + ' ';

                xSourceDP.Next;
            end;

            xSourceDP.Close;
        finally
            xSourceDP.Free;
        end;

        for xSetting in fVarSettings do
        begin
            // Variable in den Options suchen - wenn zuviel gefunden wird, ist das nicht so schlimm
            if (Pos(xSetting.VariableName, xAllOptions) > 0) then
                xResultList.Add(xSetting);
        end;

        EXIT(xResultList.ToArray());
    finally
        FreeAndNil(xResultList);
    end;
end;

procedure TMethodVariablesTableUpdateV1.MoveDataFromSettings(aSender: TObject);
var
    xMethodVariables: TArray<TMethodVariablesConversionRec>;
    xMethodVar: TMethodVariablesConversionRec;
    xDestDP: TDataProvider;
    xMethods: TArray<string>;
    xMethodName: string;
    xValue: double;
begin
    ReadDataFromSettings();

    xDestDP := fTableChangeAdaptor.CreateDestDataProvider();
    try
        xDestDP.SelectAndOpen('SELECT * FROM ' + cMethodVariablesTable, false);

        xMethods := GetMethodNames();
        for xMethodName in xMethods do
        begin
            xMethodVariables := self.GetVarSettingsNamesForMethod(xMethodName);
            for xMethodVar in xMethodVariables do
            begin
                xDestDP.Append;
                xDestDP.FieldByName(cFieldNameMethodName).AsString := xMethodName;
                xDestDP.FieldByName(cFieldNameVariableName).AsString := xMethodVar.VariableName;
                xDestDP.FieldByName(cFieldNameRequestText).AsString := xMethodVar.RequestText;
                xDestDP.FieldByName(cFieldNameDefaultValue).AsString := xMethodVar.DefaultValue;

                if TryStrToFloat(xMethodVar.MinValue, xValue) then
                begin
                    xDestDP.FieldByName(cFieldNameMinCheck).AsBoolean := true;
                    xDestDP.FieldByName(cFieldNameMinValue).AsFloat := xValue;
                end
                else
                    xDestDP.FieldByName(cFieldNameMinCheck).AsBoolean := false;

                if TryStrToFloat(xMethodVar.MaxValue, xValue) then
                begin
                    xDestDP.FieldByName(cFieldNameMaxCheck).AsBoolean := true;
                    xDestDP.FieldByName(cFieldNameMaxValue).AsFloat := xValue;
                end
                else
                    xDestDP.FieldByName(cFieldNameMaxCheck).AsBoolean := false;

                xDestDP.FieldByName(cFieldNamePickList).AsString := xMethodVar.PickList;

                if (xMethodVar.RequestFormat > 0) then // Neue Zahlencodierung
                    xDestDP.FieldByName(cFieldNameDataType).AsInteger := xMethodVar.RequestFormat + 10
                else
                    xDestDP.FieldByName(cFieldNameDataType).AsInteger := 0;

                xDestDP.FieldByName(cFieldNameDataIsArray).AsBoolean := false;
                xDestDP.FieldByName(cFieldNameDialogHide).AsBoolean := false;
                xDestDP.Post;
            end;
        end;

        xDestDP.Close;
    finally
        xDestDP.Free;
    end;
end;


end.
