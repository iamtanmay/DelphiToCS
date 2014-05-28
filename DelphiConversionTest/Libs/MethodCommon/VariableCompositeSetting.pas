{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  12.05.10 wl                                        TN5064    Initial Revision
  30.09.10 pk  DoFindText                            TN5287    New: needed for Find in Methods
  06.10.10 pk                                        TN5290    Changes for TCustomSettingGetEditFunctionParamsEvent
  07.02.11 wl  TCustomSetting_DialogInput            TN5460   von DialogInputRunStep hierher
  29.06.11 wl  TCustomSetting_RunVarNames            TN5618   EvaluateType = EvaluateIndex statt constant
  01.03.12 wl                                        TN5822   uses geändert
  05.07.12 wl  EvaluateParseValue                    TN5917   Methodenparameter geändert
  13.09.12 wl  TCustomSetting_DialogInput.GetKayNameText  TN5981   neu
  14.12.12 wl                                        TN6054   neu: TCustomSetting_MethodName.GetMethodName
  22.02.13 wl  GetDescriptionFromMethodParamKey      TN6055   Zugriff auf TMethodVariablesDataAdaptor
  21.03.13 wl  TCustomSetting_MethodParams           TN6045   sortiert die Params jetzt in der richtigen Reihenfolge!
  19.04.13 wl                                        TN6095   neues Feld DataIsArray
  10.09.13 wl  GetParseValuesAsArgArray              TN6187   neu für Rückgabewerte von RETURN
  ----------------------------------------------------------------------------------------------------------------------- }

unit VariableCompositeSetting;


interface


uses
    Generics.Defaults,
    Generics.Collections,
    MethodVariableTypes,
    Classes,
    GeneralTypes,
    CustomSetting,
    ParserIdentDataType,
    MethodTypes;

type
    TVariableCompositeSettingGetDescriptionFromKey = function(const aKey: string): string of object;

    TVariableCompositeSettingGetMethodName = function(): string of object;

    TVariableCompositeSetting = class(TCustomCompositeSetting)
    private
        function GetValuesAsKeyValueArray: TKeyValueParamArray;
        function GetParseValuesAsStringArray: TArray<string>;
        function GetParseValuesAsArgArray: TArray<TArg>;
    protected
        procedure SetValuesAsKeyValueArray(const aValues: TKeyValueParamArray); virtual;
        function AddSubParam(const aKey: string): TCustomSetting; virtual;
        procedure SetValue(const aValue: string); override;

        function DoFindText(const aSearchText: string; const aResults: TCustomSettingSearchResultList)
            : boolean; override;
    public
        procedure EvaluateParseValue(const aResultList: TKeyArgValueList);

        property ParseValuesAsStringArray: TArray<string>read GetParseValuesAsStringArray;
        property ParseValuesAsArgArray: TArray<TArg>read GetParseValuesAsArgArray;
        property ValuesAsKeyValueArray: TKeyValueParamArray read GetValuesAsKeyValueArray
            write SetValuesAsKeyValueArray;
    end;

    TCustomSetting_MethodSubParam = class(TCustomLeafSetting)
    private
        fData: TMethodVariableData;
        fStandardOrder: integer;
        function GetCompareOrder: integer;
    public
        constructor Create(const aKey, aDescription: string; const aData: TMethodVariableData;
            aStandardOrder: integer);
        property CompareOrder: integer read GetCompareOrder;
        property Data: TMethodVariableData read fData;
    end;

    TCustomSetting_MethodParamsComparer = class(TInterfacedObject, IComparer<TCustomSetting>)
        function Compare(const Left, Right: TCustomSetting): Integer;
    end;

    TCustomSetting_MethodParams = class(TVariableCompositeSetting)
    private
        fOnGetMethodName: TVariableCompositeSettingGetMethodName;
        class function FindVariableData(const aRecs: TArray<TMethodVariableRec>; const aKey: string)
            : TMethodVariableData;
    protected
        procedure SetValuesAsKeyValueArray(const aValues: TKeyValueParamArray); override;
        function AddSubParam(const aKey: string): TCustomSetting; override;
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent;
            aOnGetMethodName: TVariableCompositeSettingGetMethodName);
        procedure GetParamText(var vText: string; aLevel: integer); override;
    end;

    TCustomSetting_RunVarNames = class(TVariableCompositeSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_SQLTermParams = class(TVariableCompositeSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_DialogInput = class(TVariableCompositeSetting)
    private
        function GetParseValuesAsDialogInputKeyParamArray: TDialogInputKeyParamArray;
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    protected
        function AddSubParam(const aKey: string): TCustomSetting; override;
    public
        constructor Create(const aKey, aDescription: string; aOnAddEditFunctions: TNotifyEvent);
        function GetKeyNameText: string;
        property ParseValuesAsDialogInputKeyParamArray: TDialogInputKeyParamArray
            read GetParseValuesAsDialogInputKeyParamArray;
    end;


implementation


uses
    SysUtils,
    StrUtils,
    MethodGUIParsing,
    MethodVariablesDataAdaptor,
    MethodStepSettings,
    CustomEditFunctionParams;

{ TVariableCompositeSetting }

function TVariableCompositeSetting.GetParseValuesAsArgArray: TArray<TArg>;
var
    x: integer;
begin
    SetLength(result, self.Params.Count);
    for x := 0 to self.Params.Count - 1 do
        result[x] := self.Params[x].EvaluateParseValue;
end;

function TVariableCompositeSetting.GetParseValuesAsStringArray: TArray<string>;
var
    x: integer;
begin
    SetLength(result, self.Params.Count);
    for x := 0 to self.Params.Count - 1 do
        result[x] := self.Params[x].ParseValue;
end;

function TVariableCompositeSetting.DoFindText(const aSearchText: string;
    const aResults: TCustomSettingSearchResultList): boolean;
var
    xPos: integer;
    xUpperSearchText: string;
    xSearchResult: TCustomSettingSearchResult;
    xSearchTextLen: integer;
    xText: string;
begin
    result := false;

    xText := self.Value;
    xUpperSearchText := Uppercase(aSearchText);

    if Pos(xUpperSearchText, UpperCase(xText)) < 1 then
        EXIT;

    xSearchTextLen := Length(aSearchText);

    xPos := 1;
    while true do
    begin
        xPos := PosEx(xUpperSearchText, Uppercase(xText), xPos);
        if xPos < 1 then
        begin
            BREAK;
        end;
        xSearchResult := TCustomSettingSearchResult.Create(self, xPos);
        aResults.Add(xSearchResult);
        result := true;

        xPos := xPos + xSearchTextLen;

    end;
end;

procedure TVariableCompositeSetting.EvaluateParseValue(const aResultList: TKeyArgValueList);
var
    x: integer;
    xValue: TArg;
begin
    for x := 0 to self.Params.Count - 1 do
    begin
        xValue := self.Params[x].EvaluateParseValue();
        aResultList.Add(self.Params[x].Key, xValue);
    end;
end;

function TVariableCompositeSetting.GetValuesAsKeyValueArray: TKeyValueParamArray;
var
    x: integer;
begin
    SetLength(result, self.Params.Count);
    for x := 0 to self.Params.Count - 1 do
    begin
        result[x].Key := self.Params[x].Key;
        result[x].Value := self.Params[x].Value;
    end;
end;

function TVariableCompositeSetting.AddSubParam(const aKey: string): TCustomSetting;
begin
    result := self.AddParam(TCustomLeafSetting.Create(aKey, aKey, true, cpeNone, nil));
end;

procedure TVariableCompositeSetting.SetValue(const aValue: string);
var
    xKeyValueParamArray: TKeyValueParamArray;
begin
    xKeyValueParamArray := TMethodGUIParser.KeyValueParamArrayFromStr(aValue);
    self.ValuesAsKeyValueArray := xKeyValueParamArray;
end;

procedure TVariableCompositeSetting.SetValuesAsKeyValueArray(const aValues: TKeyValueParamArray);
var
    x: integer;
    xSetting: TCustomSetting;
begin
    self.ClearParams();
    for x := 0 to high(aValues) do
    begin
        xSetting := self.AddSubParam(aValues[x].Key);
        xSetting.OnChange := self.OnChange;
        xSetting.Value := aValues[x].Value;
    end;
end;

{ TCustomSetting_MethodParamsComparer }

function TCustomSetting_MethodParamsComparer.Compare(const Left, Right: TCustomSetting): Integer;
begin
    if (Left as TCustomSetting_MethodSubParam).CompareOrder < (Right as TCustomSetting_MethodSubParam)
        .CompareOrder then
        EXIT(-1);
    if (Left as TCustomSetting_MethodSubParam).CompareOrder > (Right as TCustomSetting_MethodSubParam)
        .CompareOrder then
        EXIT(1);

    // Wenn Order gleich ist: Zuerst niedrigeres RequestFormat
    if (Left as TCustomSetting_MethodSubParam).Data.DataType < (Right as TCustomSetting_MethodSubParam)
        .Data.DataType then
        EXIT(-1);
    if (Left as TCustomSetting_MethodSubParam).Data.DataType > (Right as TCustomSetting_MethodSubParam)
        .Data.DataType then
        EXIT(1);

    EXIT(0);
end;

{ TCustomSetting_MethodSubParam }

constructor TCustomSetting_MethodSubParam.Create(const aKey, aDescription: string;
    const aData: TMethodVariableData; aStandardOrder: integer);
begin
    inherited Create(aKey, aDescription, true, cpeNone, nil);

    fStandardOrder := aStandardOrder;
    fData := aData;
end;

function TCustomSetting_MethodSubParam.GetCompareOrder: integer;
begin
    EXIT(TMethodVariableUtils.GetCompareOrder(fData.RequestOrder, fStandardOrder));
end;

{ TCustomSetting_MethodParams }

constructor TCustomSetting_MethodParams.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent;
    aOnGetMethodName: TVariableCompositeSettingGetMethodName);
var
    aParams: TObjectList<TCustomSetting>;
begin
    aParams := TObjectList<TCustomSetting>.Create(TCustomSetting_MethodParamsComparer.Create);

    inherited Create(aParams, aKey, aDescription, true, cpeMethodParams, aOnAddEditFunctions,
        aOnGetEditFunctionParams);

    fOnGetMethodName := aOnGetMethodName;
end;

function TCustomSetting_MethodParams.AddSubParam(const aKey: string): TCustomSetting;
begin
    result := inherited AddSubParam(aKey);
    result.OnGetKeyValueText := nil;
end;

procedure TCustomSetting_MethodParams.GetParamText(var vText: string; aLevel: integer);
var
    x: integer;
begin
    // Subparameter in die richtige Reihenfolge bringen
    self.Params.Sort;

    // Text der Subparameter:
    for x := 0 to self.Params.Count - 1 do
    begin
        self.Params[x].GetParamText(vText, aLevel);
        // Level bleibt so
    end;
end;

class function TCustomSetting_MethodParams.FindVariableData(const aRecs: TArray<TMethodVariableRec>;
    const aKey: string): TMethodVariableData;
var
    x: integer;
begin
    for x := 0 to high(aRecs) do
    begin
        if (aRecs[x].VariableName = aKey) then
        begin
            EXIT(TMethodVariableUtils.MethodVariableRecToData(aRecs[x]));
        end;
    end;

    EXIT(TMethodVariableUtils.GetEmptyRec());
end;

procedure TCustomSetting_MethodParams.SetValuesAsKeyValueArray(const aValues: TKeyValueParamArray);
var
    x: integer;
    xSetting: TCustomSetting;
    xDA: TMethodVariablesDataAdaptor;
    xRecs: TArray<TMethodVariableRec>;
    xCurrentData: TMethodVariableData;
    xDescription: string;
begin
    // Description und Order-Daten direkt aus der Datanbank suchen
    xDA := TMethodVariablesDataAdaptor.Create;
    try
        xRecs := xDA.ReadRecs(fOnGetMethodName);
    finally
        FreeAndNil(xDA);
    end;

    self.ClearParams();
    for x := 0 to high(aValues) do
    begin
        xCurrentData := FindVariableData(xRecs, aValues[x].Key);
        if (xCurrentData.RequestText = '') then
            xDescription := aValues[x].Key
        else
            xDescription := xCurrentData.RequestText;

        xSetting := self.AddParam(TCustomSetting_MethodSubParam.Create(aValues[x].Key, xDescription,
            xCurrentData, x));
        xSetting.OnChange := self.OnChange;
        xSetting.Value := aValues[x].Value;
    end;
end;

{ TCustomSetting_RunVarNames }

constructor TCustomSetting_RunVarNames.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeNone, aOnAddEditFunctions);
    self.EvaluateType := TCustomSettingEvaluateType.EvaluateIndex;
end;

{ TCustomSetting_SQLTermParams }

constructor TCustomSetting_SQLTermParams.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpeSQLTermParams, aOnAddEditFunctions,
        aOnGetEditFunctionParams);
end;

{ TCustomSetting_DialogInput }

constructor TCustomSetting_DialogInput.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeDialogInput, aOnAddEditFunctions,
        DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_DialogInput.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSingleValueEditFunctionParams.Create(self);;
end;

function TCustomSetting_DialogInput.AddSubParam(const aKey: string): TCustomSetting;
begin
    result := AddParam(TCustomSetting_DialogInputItem.Create(aKey, aKey, nil))
end;

function TCustomSetting_DialogInput.GetKeyNameText: string;
var
    x: integer;
begin
    result := '';
    for x := 0 to self.Params.Count - 1 do
    begin
        if x > 0 then
            result := result + ',';
        result := result + self.Params[x].Key;
    end;
end;

function TCustomSetting_DialogInput.GetParseValuesAsDialogInputKeyParamArray: TDialogInputKeyParamArray;
var
    x: integer;
begin
    SetLength(result, self.Params.Count);
    for x := 0 to self.Params.Count - 1 do
    begin
        result[x].Key := self.Params[x].Key;
        result[x].V := (self.Params[x] as TCustomSetting_DialogInputItem).DialogInputItemOptionsParseValue;
    end;
end;


end.
