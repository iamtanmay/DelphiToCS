{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Definition für Settings, die im Parameterfenster benutzt werden können (bisher nur für MethodStepSettings)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  08.01.08 wl                               TN3972    initial version
  29.01.08 wl                               TN3980    neu: TriggerDevice, MemoryDevice
  25.04.08 wl  cpeVolMotorDevice            TN4051    neu für MOTOR (auf VolMotor reduziert)
  03.07.08 wl                               TN4157
  21.07.08 pk  cpeCarrierTypeName           TN4179    New
  31.07.08 pk  cpeRackTypeName              TN4193    New
  02.09.08 pk  TCustomSettingEditType       TN4215    changed to constants because type could not be extended in plugin packages
  19.09.08 pk  TCustomSettingEditType       TN4215    changed to type again. cpeLocationBasedMotionDevice added
  25.09.08 wl  cpeCommandName               TN4242    entfernt
  06.10.08 pk  fParseField, fKey, fConstant TN4258    New
  06.10.08 pk  TCustomCompositeSetting      TN4258    SetValue
  07.10.08 pk  TCustomCompositeSetting      TN4258    SetValue: All settings are set even if the key for that setting is not contained in string
  15.10.08 pk  TVariableCompositeSetting    TN4258    New
  15.10.08 pk  EditFunction                 TN4258    changed to procedure.  has new Cancel parameter
  07.11.08 pk  AddParam                     TN4306    IsConstant only set if true
  17.11.08 pk  TCustomSettingEditType       TN4280    New cpeResourceSchemeName
  17.11.08 wl  cpeDialogInput               TN4310    new
  10.12.08 pk  TVariableCompositeSetting    TN4310    New AddSubParam
  20.02.09 wl  Create                       TN4438    parameter aDescription statt aResNo
  20.02.09 wl  fCaption                     TN4438    entfernt (wurde nicht benutzt)
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  12.11.09 pk  TCustomSettingEditType       TN4857    New cpeBCReaderName
  07.05.10 pk  EvaluateParseValue           TN5092    New
  12.05.10 wl  TVariableCompositeSetting    TN5064    --> VariableCompositeSetting.pas
  12.05.10 wl  GetParamText                 TN5064    neu für MethodEditor-Hint
  30.09.10 pk  DoFindText                   TN5287    New: needed for Find in Methods
  06.10.10 pk                               TN5290    New TCustomSettingEditFunctionParams class
  15.11.10 pk                               TN5340    Changes to prevent memory leak
  07.02.11 wl  OpenFunction                 TN5460   neu (für kommende Funktionen)
  14.02.11 wl  cpePosOfDouble/SingleRackPos TN5468   neu: unteschiedliche Eigenschaften
  05.03.11 wl  RelatedItemParam             TN5472   ersetzt NameParam
  29.06.11 wl  TCustomSettingEvaluateType   TN5618   neu
  29.06.11 wl  EvaluateType                 TN5618   ersetzt IsConstant (Boolean)
  08.07.11 wl  ..OfSingleRackPosArray       TN5626   neu, für: XYMovement, Aspirate, Dispense, Pipette (New)
  08.07.11 wl  cpeVolumeArray               TN5626   neu, für: Aspirate, Dispense, Pipette (New)
  31.08.11 wl  cpeTimeUnit                  TN5677   neu
  03.10.11 ts  cpePHDevice/PumpDeviceName   TN5574   neu
  20.09.11 wl  cpeColor                      TN5723   neu: für Color-Parameter bei ManualFill
  01.11.11 wl  cpeVolumeCorrectionCurve      TN5731   neu: für PUMPA, PUMPD
  03.11.11 wl  cpeSubstanceSet               TN5729   neu für SubstanceLoad, SubstanceSave
  12.12.11 wl  cpeMultiPipStrategy           TN5764   von PipetteBlockRunAction hierher
  14.12.11 wl  cpeIterate                    TN5765   entfernt
  03.02.12 wl  cpeSubstanceID                TN5792   neu
  01.03.12 wl                                TN5822   uses geändert
  05.07.12 wl  EvaluateParseValue            TN5917   Methodenparameter geändert
  11.02.13 wl  cpeYesNoDelete                TN6078   neu
  -------------------------------------------------------------------------------------------------- }

unit CustomSetting;


interface


uses
    Classes,
    SysUtils,
    ListClasses,
    GeneralTypes,
    ParseField,
    AppTypes,
    ParserIdentDataType;

type
    TCustomSettingEditType = (cpeNone, cpeBalance, cpeBit, cpeBCReaderName, cpeCarrierName,
        cpeCarrierNameAndSlot, cpeCarrierSlotNo, cpeCarrierSlotRotation, cpeCarrierTypeName, cpeColor,
        cpeDeviceName, cpeDialogInput, cpeDiluentName, cpeFileName, cpeGripDevice, cpeGripperArmName,
        cpeGroupID, cpeImportDefName, cpeLocationBasedMotionDevice, cpeLiqParam, cpeMemoryDevice,
        cpeMethodName, cpeMethodParams, cpeMethodVariable, cpeMotorDevice, cpeMultiPipStrategy, cpePathName,
        cpePHDeviceName, cpePipDeviceName, cpePosOfDoubleRackPos, cpePosOfSingleRackPos,
        cpePosOfSingleRackPosArray, cpePumpDeviceName, cpeRack, cpeRackOfDoubleRackPos,
        cpeRackOfSingleRackPos, cpeRackOfSingleRackPosArray, cpeRackTypeName, cpeResourceSchemeName,
        cpeSensorDevice, cpeSequenceName, cpeShakerDevice, cpeSQLTermName, cpeSQLTermParams, cpeSubstanceID,
        cpeSubstanceSet, cpeSwitchDevice, cpeSystemRack, cpeTernary, cpeThermostat, cpeTimeUnit, cpeTipMap,
        cpeTipType, cpeToolRack, cpeTriggerDevice, cpeVolMotorDevice, cpeVolumeArray,
        cpeVolumeCorrectionCurve, cpeWashProgram, cpeYesNo, cpeYesNoDelete);

    ESetCustomSettingValueException = class(Exception);

    TCustomSetting = class;

    TCustomSettingEditFunctionParam = class
    private
        fParam: TCustomSetting;
        fOldValue: string;
        fNewValue: string;
        fIsReadOnly: boolean;
        function GetIsValueChanged: boolean;
        procedure SetNewValue(const aValue: string);
    public
        constructor Create(const aParam: TCustomSetting; const aIsReadOnly: boolean);
        property Param: TCustomSetting read fParam;
        property NewValue: string read fNewValue write SetNewValue;
        property IsReadOnly: boolean read fIsReadOnly;
        property IsValueChanged: boolean read GetIsValueChanged;
    end;

    TCustomSettingEditFunctionParams = class(TGenericObjectList<TCustomSettingEditFunctionParam>)
    public
        procedure AddEditFuncParam(const aParam: TCustomSetting; const aIsReadOnly: boolean);
    end;

    TCustomSettingGetEditFunctionParamsEvent = procedure(aSender: TObject;
        out oParams: TCustomSettingEditFunctionParams) of object;

    TCustomSettingGetPickListEvent = function(const aParams: TCustomSettingEditFunctionParams)
        : TStringArray of object;
    TCustomSettingEditFunctionEvent = procedure(const aParams: TCustomSettingEditFunctionParams;
        var vCancel: boolean) of object;
    TCustomSettingOpenFunctionEvent = procedure(const aParams: TCustomSettingEditFunctionParams) of object;
    TCustomSetingGetKeyValueText = function(aLevel: integer): string of object;

    TCustomSettingSearchResultList = class; // forward decl

    TCustomSettingEvaluateType = (Constant, EvaluateIndex, EvaluateAll);

    TCustomSetting = class
    protected
        fDescription: string;
        fViewData: TObject;

        fDataHandle: TObject;
        fRootDataHandle: TObject;
        fOnChange: TNotifyEvent;
        fVisible: boolean;
        fMaxLength: integer;
        fEditType: TCustomSettingEditType;
        fEvaluateType: TCustomSettingEvaluateType;
        fKey: string;
        fParseField: TParseField;
        fParent: TCustomSetting;
        // Events:
        fOnGetPickList: TCustomSettingGetPickListEvent;
        fOnEditFunction: TCustomSettingEditFunctionEvent;
        fOnOpenFunction: TCustomSettingOpenFunctionEvent;
        fOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent;
        fOnGetKeyValueText: TCustomSetingGetKeyValueText;
        procedure SetValue(const aValue: string); virtual; abstract;
        function GetValue: string; virtual; abstract;

        function GetParseValue: string; virtual;
        procedure SetRootDataHandle(const aValue: TObject); virtual;
        class function AddNewLine(aLevel: integer): string; static;
        function DoFindText(const aSearchText: string; const aResults: TCustomSettingSearchResultList)
            : boolean; virtual; abstract;
    public
        // constructor/destructor
        constructor Create(const aKey: string; const aDescription: string; aVisible: boolean;
            aEditType: TCustomSettingEditType; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
        destructor Destroy(); override;
        //
        function GetPickList(): TStringArray;
        function HasEditFunction: boolean;
        function HasOpenFunction: boolean;
        procedure OpenFunction();
        procedure EditFunction(const aFunctionParams: TCustomSettingEditFunctionParams; out oCancel: boolean);
        function GetEditFunctionParams(): TCustomSettingEditFunctionParams;

        procedure SetOnChangeForAll(aValue: TNotifyEvent); virtual;
        procedure SetParent(const aValue: TCustomSetting);
        procedure Changed(); virtual;

        function EvaluateParseValue: TArg;
        procedure GetParamText(var vText: string; aLevel: integer); virtual; abstract;
        function GetDefaultKeyValueText(aLevel: integer): string;

        function FindText(const aSearchText: string; const aResults: TCustomSettingSearchResultList): boolean;
        function FindRecursive(const aKeyArray: TStringArray; const aCurrentLevel: integer)
            : TCustomSetting; virtual;

        procedure GetKeyArray(var vKeyArray: TStringArray);
        // Properties
        property Description: string read fDescription write fDescription;
        property Value: string read GetValue write SetValue;
        property OnChange: TNotifyEvent read fOnChange write fOnChange;
        property MaxLength: integer read fMaxLength write fMaxLength;
        property ViewData: TObject read fViewData write fViewData;
        property DataHandle: TObject read fDataHandle write fDataHandle;
        property RootDataHandle: TObject read fRootDataHandle write SetRootDataHandle;
        property Visible: boolean read fVisible;
        property OnGetPickList: TCustomSettingGetPickListEvent read fOnGetPickList write fOnGetPickList;
        property OnEditFunction: TCustomSettingEditFunctionEvent read fOnEditFunction write fOnEditFunction;
        property OnOpenFunction: TCustomSettingOpenFunctionEvent read fOnOpenFunction write fOnOpenFunction;
        property EditType: TCustomSettingEditType read fEditType write fEditType;
        property EvaluateType: TCustomSettingEvaluateType read fEvaluateType write fEvaluateType;
        property ParseField: TParseField read fParseField write fParseField;
        property ParseValue: string read GetParseValue;
        property Key: string read fKey write fKey;
        property OnGetKeyValueText: TCustomSetingGetKeyValueText read fOnGetKeyValueText
            write fOnGetKeyValueText;

    end;

    TCustomLeafSetting = class(TCustomSetting)
    protected
        FValue: string;
        procedure SetValue(const aValue: string); override;
        // hier kracht es, wenn der Wert nicht gesetzt werden kann
        function GetValue: string; override;
        function DoFindText(const aSearchText: string; const aResults: TCustomSettingSearchResultList)
            : boolean; override;
    public
        constructor Create(const aKey, aDescription: string; aVisible: boolean;
            aEditType: TCustomSettingEditType = cpeNone; aOnAddEditFunctions: TNotifyEvent = nil;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent = nil);
        procedure GetParamText(var vText: string; aLevel: integer); override;
    end;

    TCustomCompositeSetting = class(TCustomSetting)
    private
        FList: TGenericObjectList<TCustomSetting>;
        function GetParamCount: integer;
        function GetParam(aIndex: Integer): TCustomSetting;
    protected
        procedure SetRootDataHandle(const aValue: TObject); override;
        procedure SetValue(const aValue: string); override;
        function GetValue: string; override;
        procedure ClearParams();
        function DoFindText(const aSearchText: string; const aResults: TCustomSettingSearchResultList)
            : boolean; override;
    public
        // constructor/destructor
        constructor Create(const aKey: string; const aDescription: string; aVisible: boolean;
            aEditType: TCustomSettingEditType = cpeNone; aOnAddEditFunctions: TNotifyEvent = nil;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent = nil);
        destructor Destroy; override;

        procedure SetOnChangeForAll(aValue: TNotifyEvent); override;
        function Find(const aKey: string): TCustomSetting;
        function FindRecursive(const aKeyArray: TStringArray; const aCurrentLevel: integer)
            : TCustomSetting; override;

        function AddParam(const aKey: string; const aDescription: string; aVisible: boolean;
            aEditType: TCustomSettingEditType = cpeNone; aOnAddEditFunctions: TNotifyEvent = nil)
            : TCustomSetting; overload;
        function AddParam(aParam: TCustomSetting): TCustomSetting; overload;
        procedure GetParamText(var vText: string; aLevel: integer); override;

        // Properties
        property ParamCount: integer read GetParamCount;
        property Params[index: integer]: TCustomSetting read GetParam;
    end;

    TCustomSettingSearchResult = class
    private
        fSetting: TCustomSetting;
        fFoundPos: integer;
    public
        constructor Create(const aSetting: TCustomSetting; const aFoundPos: integer);
        property Setting: TCustomSetting read fSetting;
        property FoundPos: integer read fFoundPos;
    end;

    TCustomSettingSearchResultList = class(TGenericObjectList<TCustomSettingSearchResult>);


implementation


uses
    StrUtils,
    MethodGUIParsing;

{ TCustomSetting }

constructor TCustomSetting.Create(const aKey: string; const aDescription: string; aVisible: boolean;
    aEditType: TCustomSettingEditType; aOnAddEditFunctions: TNotifyEvent;
    aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin

    inherited Create;
    fKey := aKey;
    FDescription := aDescription;
    fDataHandle := nil;
    fRootDataHandle := nil;
    fOnChange := nil;
    fVisible := aVisible;
    fEditType := aEditType;
    fEvaluateType := TCustomSettingEvaluateType.EvaluateAll;
    fOnGetEditFunctionParams := aOnGetEditFunctionParams;
    if Assigned(aOnAddEditFunctions) then
        aOnAddEditFunctions(self);
    fParseField := nil;
end;

destructor TCustomSetting.Destroy();
begin
    FreeAndNil(fParseField);
    inherited;
end;

procedure TCustomSetting.SetParent(const aValue: TCustomSetting);
begin
    fParent := aValue;
end;

procedure TCustomSetting.Changed();
begin
    if Assigned(fOnChange) then
        fOnChange(self);
end;

procedure TCustomSetting.SetOnChangeForAll(aValue: TNotifyEvent);
begin
    fOnChange := aValue;
end;

procedure TCustomSetting.EditFunction(const aFunctionParams: TCustomSettingEditFunctionParams;
    out oCancel: boolean);
begin
    oCancel := true;
    if not Assigned(fOnEditFunction) then
        EXIT;
    oCancel := false;

    fOnEditFunction(aFunctionParams, oCancel)
end;

procedure TCustomSetting.OpenFunction();
begin
    if Assigned(fOnOpenFunction) then
        fOnOpenFunction(self.GetEditFunctionParams)
end;

function TCustomSetting.GetEditFunctionParams: TCustomSettingEditFunctionParams;
begin
    result := nil;
    if not Assigned(fOnGetEditFunctionParams) then
        EXIT;
    fOnGetEditFunctionParams(self, result);
end;

function TCustomSetting.EvaluateParseValue: TArg;
begin
    // this is similar to the GetParseValue function, but we dont want to convert it to a string and loose the type information
    EXIT(fParseField.Evaluate);
end;

function TCustomSetting.FindText(const aSearchText: string;
    const aResults: TCustomSettingSearchResultList): boolean;
begin
    result := DoFindtext(aSearchText, aResults);
end;

procedure TCustomSetting.GetKeyArray(var vKeyArray: TStringArray);
begin
    if self.Key <> '' then
    begin
        SetLength(vKeyArray, Length(vKeyArray) + 1);
        vKeyArray[ high(vKeyArray)] := self.Key;
    end;

    if Assigned(fParent) then
        fParent.GetKeyArray(vKeyArray);
end;

function TCustomSetting.GetPickList(): TStringArray;
begin
    if Assigned(fOnGetPickList) then
        result := fOnGetPickList(self.GetEditFunctionParams);
end;

function TCustomSetting.HasEditFunction: boolean;
begin
    result := Assigned(fOnEditFunction);
end;

function TCustomSetting.HasOpenFunction: boolean;
begin
    result := Assigned(fOnOpenFunction);
end;

procedure TCustomSetting.SetRootDataHandle(const aValue: TObject);
begin
    fRootDataHandle := aValue;
end;

class function TCustomSetting.AddNewLine(aLevel: integer): string;
var
    x: integer;
begin
    result := #13#10;
    for x := 0 to aLevel - 1 do
        result := result + '   ';
end;

function TCustomSetting.GetParseValue: string;
begin
    result := fParseField.AsString;
end;

function TCustomSetting.GetDefaultKeyValueText(aLevel: integer): string;
begin
    if (self.Value = '') then
    begin
        // Wert nicht gesetzt
        result := '';
    end
    else
    begin
        // Description und Wert anfügen
        result := self.Description + ':  ' + self.Value;
    end;
end;

function TCustomSetting.FindRecursive(const aKeyArray: TStringArray; const aCurrentLevel: integer)
    : TCustomSetting;
begin
    result := nil;
    if SameText(self.Key, aKeyArray[aCurrentLevel]) then
    begin
        result := self;
    end;
end;

{ TCustomLeafSetting }

constructor TCustomLeafSetting.Create(const aKey, aDescription: string; aVisible: boolean;
    aEditType: TCustomSettingEditType; aOnAddEditFunctions: TNotifyEvent;
    aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, aVisible, aEditType, aOnAddEditFunctions, aOnGetEditFunctionParams);

    FValue := '';
    fOnGetKeyValueText := GetDefaultKeyValueText;
end;

procedure TCustomLeafSetting.GetParamText(var vText: string; aLevel: integer);
var
    xKeyValueText: string;
begin
    if not self.Visible then
        EXIT;

    // Es gibt eine spezielle GetKeyValueText-Funktion
    if Assigned(self.OnGetKeyValueText) then
    begin
        xKeyValueText := self.OnGetKeyValueText(aLevel);
    end;
    // Text hinzufügen
    if (xKeyValueText <> '') then
        vText := vText + TCustomSetting.AddNewLine(aLevel) + xKeyValueText;
end;

function TCustomLeafSetting.GetValue: string;
begin
    result := fValue;
end;

procedure TCustomLeafSetting.SetValue(const aValue: string);
begin
    FValue := aValue;
end;

function TCustomLeafSetting.DoFindText(const aSearchText: string;
    const aResults: TCustomSettingSearchResultList): boolean;
var
    xPos: integer;
    xUpperSearchText: string;
    xSearchResult: TCustomSettingSearchResult;
    xSearchTextLen: integer;
    xText: string;
begin
    result := false;

    xUpperSearchText := Uppercase(aSearchText);
    xText := self.Value;
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
{ TCompositeCustomSetting }

constructor TCustomCompositeSetting.Create(const aKey: string; const aDescription: string; aVisible: boolean;
    aEditType: TCustomSettingEditType; aOnAddEditFunctions: TNotifyEvent;
    aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
var
    xKey: string;
begin
    xKey := aKey;
    if xKey = '' then
        xKey := self.ClassName;

    inherited Create(xKey, aDescription, aVisible, aEditType, aOnAddEditFunctions, aOnGetEditFunctionParams);

    FList := TGenericObjectList<TCustomSetting>.Create(true);
end;

destructor TCustomCompositeSetting.Destroy;
begin
    FList.Free;

    inherited;
end;

function TCustomCompositeSetting.DoFindText(const aSearchText: string;
    const aResults: TCustomSettingSearchResultList): boolean;
var
    x: integer;
begin
    result := false;
    for x := 0 to self.ParamCount - 1 do
    begin
        result := result or self.Params[x].FindText(aSearchText, aResults);
    end;
end;

function TCustomCompositeSetting.GetParam(aIndex: Integer): TCustomSetting;
begin
    result := nil;
    if (aIndex < 0) or (aIndex >= FList.Count) then
        EXIT;
    if not(FList.Items[aIndex] is TCustomSetting) then
        EXIT;

    result := FList.Items[aIndex] as TCustomSetting;
end;

function TCustomCompositeSetting.GetParamCount: integer;
begin
    result := FList.Count;
end;

procedure TCustomCompositeSetting.GetParamText(var vText: string; aLevel: integer);
var
    x: integer;
    xSubText: string;
begin
    // Text der Subparameter:
    xSubText := '';
    for x := 0 to self.ParamCount - 1 do
    begin
        self.Params[x].GetParamText(xSubText, aLevel + 1);
    end;

    if (xSubText <> '') then
    begin
        if self.Visible then
        begin
            // Überschrift mit SubParameter-Text hinzufügen
            vText := vText + TCustomSetting.AddNewLine(aLevel) + self.Description + ':' + xSubText;
        end
        else
        begin
            vText := vText + xSubText;
        end;
    end;
end;

function TCustomCompositeSetting.AddParam(aParam: TCustomSetting): TCustomSetting;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := FList.Add(aParam);

    if (xIndex >= 0) then
    begin
        aParam.SetParent(self);
        result := aParam;
        if self.EvaluateType <> TCustomSettingEvaluateType.EvaluateAll then
            aParam.EvaluateType := self.EvaluateType;
    end;
end;

function TCustomCompositeSetting.AddParam(const aKey: string; const aDescription: string; aVisible: boolean;
    aEditType: TCustomSettingEditType; aOnAddEditFunctions: TNotifyEvent): TCustomSetting;
begin
    result := AddParam(TCustomLeafSetting.Create(aKey, aDescription, aVisible, aEditType,
        aOnAddEditFunctions))
end;

procedure TCustomCompositeSetting.SetRootDataHandle(const aValue: TObject);
var
    x: integer;
begin
    inherited;
    for x := 0 to self.ParamCount - 1 do
    begin
        self.Params[x].RootDataHandle := aValue;
    end;
end;

procedure TCustomCompositeSetting.SetOnChangeForAll(aValue: TNotifyEvent);
var
    x: integer;
begin
    fOnChange := aValue;
    for x := 0 to self.ParamCount - 1 do
    begin
        self.Params[x].SetOnChangeForAll(aValue);
    end;
end;

function TCustomCompositeSetting.Find(const aKey: string): TCustomSetting;
var
    x: integer;
    xSetting: TCustomSetting;
begin
    result := nil;
    for x := 0 to self.ParamCount - 1 do
    begin
        xSetting := self.Params[x];
        if SameText(xSetting.Key, aKey) then
        begin
            result := xSetting;
            EXIT;
        end;
    end;

end;

function TCustomCompositeSetting.FindRecursive(const aKeyArray: TStringArray; const aCurrentLevel: integer)
    : TCustomSetting;
var
    x: integer;
    xSetting: TCustomSetting;
begin
    result := inherited FindRecursive(aKeyArray, aCurrentLevel);

    if not Assigned(result) then
        EXIT;

    if aCurrentLevel = 0 then
        EXIT;

    result := nil;
    for x := 0 to self.ParamCount - 1 do
    begin
        xSetting := self.Params[x];
        result := xSetting.FindRecursive(aKeyArray, aCurrentLevel - 1);
        if Assigned(result) then
            EXIT;

    end;
end;

function TCustomCompositeSetting.GetValue: string;
var
    x: integer;
    xSetting: TCustomSetting;
    xValue: string;
    xList: TStringKeyStringValueList;
begin
    result := '';
    xList := TMethodGUIParser.CreateKeyValueList();
    try
        for x := 0 to self.ParamCount - 1 do
        begin
            xSetting := self.Params[x];
            xValue := xSetting.Value;
            if (xValue = '') then
                CONTINUE;
            xList.AddValue(xSetting.Key, xValue)
        end;
        result := TMethodGUIParser.ListToOptionStr(xList);
    finally
        xList.Free;
    end;

end;

procedure TCustomCompositeSetting.SetValue(const aValue: string);
var
    x: integer;
    xSetting: TCustomSetting;
    xList: TStringKeyStringValueList;
    xValue: string;
begin
    xList := TMethodGUIParser.CreateKeyValueList();
    try
        TMethodGUIParser.OptionsStrToList(xList, aValue);

        for x := 0 to self.ParamCount - 1 do
        begin
            xSetting := self.Params[x];
            xList.TryGetValue(xSetting.Key, xValue);
            xSetting.Value := xValue;
        end;
        {
          // this caused problems in method editor because some settings were left with old values when the Method Step was
          // reused for different method lines
          for x := 0 to xList.Count - 1 do begin
          xSetting := Find( xList.Names[x] );
          if not Assigned( xSetting ) then CONTINUE;
          xSetting.Value := xList.ValueFromIndex[x];;
          end; }
    finally
        xList.Free;
    end;

end;

procedure TCustomCompositeSetting.ClearParams;
begin
    FList.Clear();
end;

{ TCustomSettingSearchResult }

constructor TCustomSettingSearchResult.Create(const aSetting: TCustomSetting; const aFoundPos: integer);
begin
    inherited Create();
    fSetting := aSetting;
    fFoundPos := aFoundPos;
end;

{ TCustomSettingEditFunctionParam }

constructor TCustomSettingEditFunctionParam.Create(const aParam: TCustomSetting; const aIsReadOnly: boolean);
begin
    inherited Create();
    fParam := aParam;
    fIsReadOnly := aIsReadOnly;
    fNewValue := fParam.Value;
    fOldValue := fNewValue;
end;

function TCustomSettingEditFunctionParam.GetIsValueChanged: boolean;
begin
    result := (not self.IsReadOnly) and (fOldValue <> fNewValue);
end;

procedure TCustomSettingEditFunctionParam.SetNewValue(const aValue: string);
begin
    ASSERT(not self.IsReadOnly);
    fNewValue := aValue;
end;

{ TCustomSettingEditFunctionParams }

procedure TCustomSettingEditFunctionParams.AddEditFuncParam(const aParam: TCustomSetting;
    const aIsReadOnly: boolean);
begin
    self.Add(TCustomSettingEditFunctionParam.Create(aParam, aIsReadOnly));
end;


end.
