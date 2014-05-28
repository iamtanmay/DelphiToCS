{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Allgem. Settings, die im Parameterfenster benutzt werden können (bisher nur für MethodStepSettings)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                    track-no  improvement/change
  -------- --  ---------------------------------------  --------  ----------------------------------------------
  08.01.08 wl                                            TN3972    initial version
  29.01.08 wl                                            TN3980    neu: TriggerDevice, MemoryDevice
  25.04.08 wl  TCustomSetting_VolMotorDevice             TN4051    neu für MOTOR (auf VolMotor reduziert)
  25.04.08 wl  TCustomSetting_MotorDevice                TN4051    Motoren für MMOVE
  03.07.08 wl                                            TN4157
  21.07.08 pk  TCustomSetting_CarrierTypeName            TN4179    New
  31.07.08 pk  TCustomSetting_RackTypeName               TN4193    New
  19.09.08 pk  TCustomSetting_GripDevice                 TN4215    New
  25.09.08 wl  TCustomSetting_CommandName                TN4242    entfernt
  25.09.08 wl  TCustomSetting_Ternary                    TN4242    aVisible kann als Parameter übergeben werden
  06.10.08 pk                                            TN4258    new Key parameter
  15.10.08 pk  TCustomSetting_SQLTermParams, etc.        TN4258    inherit from TVariableCompositeSetting
  17.11.08 pk  TCustomSetting_ResourceSchemeName         TN4280    New
  17.11.08 wl  TCustomSetting_DialogInput                TN4310    for INPUT action parameters
  25.11.08 wl  TCustomSetting_DialogInput                TN4310    hat eigene Parse-Funktion
  26.11.08 wl  TCustomSetting_DialogInput                TN4310    ParseValue durch Value ersetzt
  10.12.08 pk  TCustomSetting_DialogInput                TN4310    moved to DialogInputRunStep
  20.02.09 wl  Create                                    TN4438    parameter aDescription statt aResNo und aValue
  12.11.09 pk  TCustomSetting_BCReaderName               TN4857    New
  16.11.09 ts  TCustomSetting_BCReaderName.Create        TN4857    cpeBCReaderName instead of cpeBalance
  12.05.10 wl                                            TN5064    von TVariableCompositeSetting abgeleitete --> VariableCompositeSetting.pas
  06.10.10 pk                                            TN5290    New TCustomSettingEditFunctionParams classes
  14.12.10 wl  TCustomSetting_Float.SetValue             TN5411    DecimalSeparator wird nicht mehr umgeschrieben
  07.02.11 wl  ..._EditFunctionParams                    TN5460   --> CustomEditFunctionParams
  14.02.11 wl  TCustomSetting_PosOfDouble/SingleRackPos  TN5468   neu: unteschiedliche Eigenschaften
  05.03.11 wl  RelatedItemParam                          TN5472   ersetzt NameParam
  10.03.11 wl  TCustomSetting_ResourceSchemeName         TN5499   enthält Funktion zur Trennung von Wert und Kommentar
  29.06.11 wl  TCustomSetting_RunVarName                 TN5618   EvaluateType = EvaluateIndex statt constant
  08.07.11 wl  TCustomSetting_..OfSingleRackPosArray     TN5626   neu, für: XYMovement, Aspirate, Dispense, Pipette (New)
  08.07.11 wl  TCustomSetting_VolumeArray                TN5626   neu, für: Aspirate, Dispense, Pipette (New)
  31.08.11 wl  TCustomSetting_TimeUnit                   TN5677   neu
  03.10.11 ts   TCustomSetting_PHDeviceName /PumpDevice  TN5574   neu
  20.09.11 wl  TCustomSetting_Color                      TN5723   neu: für Color-Parameter bei ManualFill
  01.11.11 wl  TCustomSetting_VolumeCorrectionCurve      TN5731   neu: für PUMPA, PUMPD
  03.11.11 wl  TCustomSetting_SubstanceSet               TN5729   neu für SubstanceLoad, SubstanceSave
  12.12.11 wl  TCustomSetting_MultiPipStrategy           TN5764   von PipetteBlockRunAction hierher
  03.02.12 wl  TCustomSetting_SubstanceID                TN5792   neu
  25.04.12 wl  TCustomSetting_MultiPipStrategy           TN5878   --> BasicPipetteTypes
  14.12.12 wl  TCustomSetting_MethodName                 TN6054   neu: GetMethodName
  11.02.13 wl  TCustomSetting_YesNoDelete                TN6078   neu
  15.08.13 wl  TCustomSetting_Float                      TN6221   entfernt
  -------------------------------------------------------------------------------------------------- }

unit CustomLeafSettings;


interface


uses
    Classes,
    AppTypes,
    CustomSetting,
    MethodTypes;

type
    TCustomSetting_Balance = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_BCReaderName = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_Bit = class(TCustomLeafSetting)
    private
        function GetBitValue: boolean;
        procedure SetBitValue(const aValue: boolean);
    public
        constructor Create(const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
        class function GetYesString: string;
        class function GetNoString: string;
        property BitValue: boolean read GetBitValue write SetBitValue;
    end;

    TCustomSetting_CarrierName = class(TCustomLeafSetting)
    private
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_CarrierNameAndSlot = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_CarrierSlotNo = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_CarrierSlotRotation = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_CarrierTypeName = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_Color = class(TCustomLeafSetting)
    private
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_DeviceName = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_DiluentName = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_FileName = class(TCustomLeafSetting)
    private
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_GripDevice = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_GripperArmName = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_GroupID = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_ImportDefName = class(TCustomLeafSetting)
    private
        fImportDefMode: integer;
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aImportDefMode: integer);
        //
        property ImportDefMode: integer read fImportDefMode;
    end;

    TCustomSetting_Integer = class(TCustomLeafSetting)
    protected
        procedure SetValue(const aValue: string); override;
    end;

    TCustomSetting_LiqParam = class(TCustomLeafSetting)
    private
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_MemoryDevice = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_MethodName = class(TCustomLeafSetting)
    private
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
        function GetMethodName: string;
    end;

    TCustomSetting_LocationBasedMotionDevice = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_MotorDevice = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_NoKeyOption = class(TCustomLeafSetting)
    private
        function GetKeyValueText(aLevel: integer): string;
    protected
        procedure SetValue(const aValue: string); override;
        // hier kracht es, wenn der Wert nicht gesetzt werden kann
        function GetValue: string; override;
    public
        constructor Create(const aDescription: string; aVisible: boolean);
    end;

    TCustomSetting_NotEmpty = class(TCustomLeafSetting)
    protected
        procedure SetValue(const aValue: string); override;
        // hier kracht es, wenn der Wert nicht gesetzt werden kann
    end;

    TCustomSetting_PathName = class(TCustomLeafSetting)
    private
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_PHDeviceName = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_PipDeviceName = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_PosOfDoubleRackPos = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_PosOfSingleRackPos = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_PosOfSingleRackPosArray = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_PumpDeviceName = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_Rack = class(TCustomLeafSetting)
    private
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_RackOfDoubleRackPos = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_RackOfSingleRackPos = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_RackOfSingleRackPosArray = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_RackTypeName = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_ResourceSchemeName = class(TCustomLeafSetting)
    strict protected
        procedure SetValue(const aValue: string); override;
    public const
        cValueDescriptionDelimiter = '|';
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_RunVarName = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_SensorDevice = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_SequenceName = class(TCustomLeafSetting)
    private
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_ShakerDevice = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_SQLTermName = class(TCustomLeafSetting)
    private
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_SubstanceID = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_SubstanceSet = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_SwitchDevice = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_SystemRack = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_Ternary = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aVisible: boolean = true);
    end;

    TCustomSetting_Thermostat = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_TimeUnit = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_TipMap = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent;
            aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
    end;

    TCustomSetting_TipType = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_ToolRack = class(TCustomLeafSetting)
    private
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_TriggerDevice = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_VolMotorDevice = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_VolumeArray = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_VolumeCorrectionCurve = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_WashProgram = class(TCustomLeafSetting)
    private
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_YesNo = class(TCustomLeafSetting)
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
    end;

    TCustomSetting_YesNoDelete = class(TCustomLeafSetting)
    public const
        cDeleteText = 'DELETE';
    public
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
        class function GetStoreOrDelete(const aText: string): TStoreOrDelete;
    end;


implementation


uses
    SysUtils,
    MethodGUIParsing,
    GeneralTypes,
    UtilLib,
    CustomEditFunctionParams;

{ TCustomSetting_Balance }

constructor TCustomSetting_Balance.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeBalance, aOnAddEditFunctions);
end;

{ TCustomSetting_BCReaderName }

constructor TCustomSetting_BCReaderName.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeBCReaderName, aOnAddEditFunctions);
end;

{ TCustomSetting_Bit }

constructor TCustomSetting_Bit.Create(const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', aDescription, true, cpeBit, aOnAddEditFunctions);
end;

function TCustomSetting_Bit.GetBitValue: boolean;
begin
    result := (FValue = self.GetYesString);
end;

class function TCustomSetting_Bit.GetNoString: string;
begin
    result := TLanguageString.Read('No', 'Nein');
end;

class function TCustomSetting_Bit.GetYesString: string;
begin
    result := TLanguageString.Read('Yes', 'Ja');
end;

procedure TCustomSetting_Bit.SetBitValue(const aValue: boolean);
begin
    if (aValue) then
        SetValue(self.GetYesString)
    else
        SetValue(self.GetNoString);
end;

{ TCustomSetting_CarrierName }

constructor TCustomSetting_CarrierName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeCarrierName, aOnAddEditFunctions,
        DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_CarrierName.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSingleValueEditFunctionParams.Create(self);
end;

{ TCustomSetting_CarrierNameAndSlot }

constructor TCustomSetting_CarrierNameAndSlot.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpeCarrierNameAndSlot, aOnAddEditFunctions,
        aOnGetEditFunctionParams);
end;

{ TCustomSetting_CarrierSlotNo }

constructor TCustomSetting_CarrierSlotNo.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpeCarrierSlotNo, aOnAddEditFunctions,
        aOnGetEditFunctionParams);
end;

{ TCustomSetting_CarrierSlotRotation }

constructor TCustomSetting_CarrierSlotRotation.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpeCarrierSlotRotation, aOnAddEditFunctions,
        aOnGetEditFunctionParams);
end;

{ TCustomSetting_CarrierTypeName }

constructor TCustomSetting_CarrierTypeName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeCarrierTypeName, aOnAddEditFunctions);
end;

{ TCustomSetting_Color }

constructor TCustomSetting_Color.Create(const aKey, aDescription: string; aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeColor, aOnAddEditFunctions, DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_Color.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSingleValueEditFunctionParams.Create(self);
end;

{ TCustomSetting_DeviceName }

constructor TCustomSetting_DeviceName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeDeviceName, aOnAddEditFunctions);
end;

{ TCustomSetting_DiluentName }

constructor TCustomSetting_DiluentName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeDiluentName, aOnAddEditFunctions);
end;

{ TCustomSetting_FileName }

constructor TCustomSetting_FileName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeFileName, aOnAddEditFunctions, DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_FileName.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSingleValueEditFunctionParams.Create(self);
end;

{ TCustomSetting_GripDevice }

constructor TCustomSetting_GripDevice.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeGripDevice, aOnAddEditFunctions);
end;

{ TCustomSetting_GripperArmName }

constructor TCustomSetting_GripperArmName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeGripperArmName, aOnAddEditFunctions);
end;

{ TCustomSetting_GroupID }

constructor TCustomSetting_GroupID.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeGroupID, aOnAddEditFunctions);
end;

{ TCustomSetting_ImportDefName }

constructor TCustomSetting_ImportDefName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aImportDefMode: integer);
begin
    inherited Create(aKey, aDescription, true, cpeImportDefName, aOnAddEditFunctions,
        DoOnGetEditFunctionParams);

    fImportDefMode := aImportDefMode;
end;

procedure TCustomSetting_ImportDefName.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSingleValueEditFunctionParams.Create(self);
end;

{ TCustomSetting_Integer }

procedure TCustomSetting_Integer.SetValue(const aValue: string);
var
    xIntValue: integer;
begin
    if trim(aValue) = '' then
        inherited SetValue('')
    else
    begin
        xIntValue := StrToIntDef(aValue, 0);
        inherited SetValue(IntToStr(xIntValue));
    end;
end;

{ TCustomSetting_LiqParam }

constructor TCustomSetting_LiqParam.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeLiqParam, aOnAddEditFunctions, DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_LiqParam.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSingleValueEditFunctionParams.Create(self);
end;

{ TCustomSetting_MemoryDevice }

constructor TCustomSetting_MemoryDevice.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeMemoryDevice, aOnAddEditFunctions);
end;

{ TCustomSetting_MethodName }

constructor TCustomSetting_MethodName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeMethodName, aOnAddEditFunctions, DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_MethodName.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSingleValueEditFunctionParams.Create(self);
end;

function TCustomSetting_MethodName.GetMethodName: string;
begin
    EXIT(self.Value);
end;

{ TCustomSetting_LocationBasedMotionDevice }

constructor TCustomSetting_LocationBasedMotionDevice.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeLocationBasedMotionDevice, aOnAddEditFunctions);
end;

{ TCustomSetting_MotorDevice }

constructor TCustomSetting_MotorDevice.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeMotorDevice, aOnAddEditFunctions);
end;

{ TCustomSetting_NoKeyOption }

constructor TCustomSetting_NoKeyOption.Create(const aDescription: string; aVisible: boolean);
begin
    inherited Create('', aDescription, aVisible);

    self.OnGetKeyValueText := GetKeyValueText;
end;

function TCustomSetting_NoKeyOption.GetKeyValueText(aLevel: integer): string;
begin
    result := self.Value;
end;

function TCustomSetting_NoKeyOption.GetValue: string;
begin
    result := fValue;
end;

procedure TCustomSetting_NoKeyOption.SetValue(const aValue: string);
begin
    fValue := aValue;
end;

{ TCustomSetting_NotEmpty }

procedure TCustomSetting_NotEmpty.SetValue(const aValue: string);
begin
    if (aValue = '') then
        ESetCustomSettingValueException.Create(TLanguageString.Read('Parameter must not be empty!',
            'Parameter darf nicht leer sein!'));
    inherited;
end;

{ TCustomSetting_PathName }

constructor TCustomSetting_PathName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpePathName, aOnAddEditFunctions, DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_PathName.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSingleValueEditFunctionParams.Create(self);
end;

{ TCustomSetting_PHDeviceName }

constructor TCustomSetting_PHDeviceName.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpePHDeviceName, aOnAddEditFunctions);
end;

{ TCustomSetting_PipDeviceName }

constructor TCustomSetting_PipDeviceName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpePipDeviceName, aOnAddEditFunctions);
end;

{ TCustomSetting_PosOfDoubleRackPos }

constructor TCustomSetting_PosOfDoubleRackPos.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpePosOfDoubleRackPos, aOnAddEditFunctions,
        aOnGetEditFunctionParams);
end;

{ TCustomSetting_PosOfSingleRackPos }

constructor TCustomSetting_PosOfSingleRackPos.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpePosOfSingleRackPos, aOnAddEditFunctions,
        aOnGetEditFunctionParams);
end;

{ TCustomSetting_PosOfSingleRackPosArray }

constructor TCustomSetting_PosOfSingleRackPosArray.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpePosOfSingleRackPosArray, aOnAddEditFunctions,
        aOnGetEditFunctionParams);
end;

{ TCustomSetting_PumpDeviceName }

constructor TCustomSetting_PumpDeviceName.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpePumpDeviceName, aOnAddEditFunctions);
end;

{ TCustomSetting_Rack }

constructor TCustomSetting_Rack.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeRack, aOnAddEditFunctions, DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_Rack.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TRackEditFunctionParams.Create(self);
end;

{ TCustomSetting_RackOfDoubleRackPos }

constructor TCustomSetting_RackOfDoubleRackPos.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpeRackOfDoubleRackPos, aOnAddEditFunctions,
        aOnGetEditFunctionParams);
end;

{ TCustomSetting_RackOfSingleRackPos }

constructor TCustomSetting_RackOfSingleRackPos.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpeRackOfSingleRackPos, aOnAddEditFunctions,
        aOnGetEditFunctionParams);
end;

{ TCustomSetting_RackOfSingleRackPosArray }

constructor TCustomSetting_RackOfSingleRackPosArray.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpeRackOfSingleRackPosArray, aOnAddEditFunctions,
        aOnGetEditFunctionParams);
end;

{ TCustomSetting_RackTypeName }

constructor TCustomSetting_RackTypeName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeRackTypeName, aOnAddEditFunctions);
end;

{ TCustomSetting_ResourceSchemeName }

constructor TCustomSetting_ResourceSchemeName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeResourceSchemeName, aOnAddEditFunctions);
end;

procedure TCustomSetting_ResourceSchemeName.SetValue(const aValue: string);
var
    xPos: integer;
    xNewValue: string;
begin
    // hinten angehängten Kommentar wieder entfernen
    xPos := Pos(cValueDescriptionDelimiter, aValue);
    if (xPos > 0) then
        xNewValue := Copy(aValue, 1, xPos - 1)
    else
        xNewValue := aValue;

    inherited SetValue(xNewValue);
end;

{ TCustomSetting_RunVarName }

constructor TCustomSetting_RunVarName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeNone, aOnAddEditFunctions);
    self.EvaluateType := TCustomSettingEvaluateType.EvaluateIndex;
end;

{ TCustomSetting_SensorDevice }

constructor TCustomSetting_SensorDevice.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeSensorDevice, aOnAddEditFunctions);
end;

{ TCustomSetting_SequenceName }

constructor TCustomSetting_SequenceName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeSequenceName, aOnAddEditFunctions,
        DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_SequenceName.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSingleValueEditFunctionParams.Create(self);
end;

{ TCustomSetting_ShakerDevice }

constructor TCustomSetting_ShakerDevice.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeShakerDevice, aOnAddEditFunctions);
end;

{ TCustomSetting_SQLTermName }

constructor TCustomSetting_SQLTermName.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeSQLTermName, aOnAddEditFunctions,
        DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_SQLTermName.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSingleValueEditFunctionParams.Create(self);
end;

{ TCustomSetting_SubstanceID }

constructor TCustomSetting_SubstanceID.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeSubstanceID, aOnAddEditFunctions);
end;

{ TCustomSetting_SubstanceSet }

constructor TCustomSetting_SubstanceSet.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeSubstanceSet, aOnAddEditFunctions);
end;

{ TCustomSetting_SwitchDevice }

constructor TCustomSetting_SwitchDevice.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeSwitchDevice, aOnAddEditFunctions);
end;

{ TCustomSetting_SystemRack }

constructor TCustomSetting_SystemRack.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpeSystemRack, aOnAddEditFunctions, aOnGetEditFunctionParams);
end;

{ TCustomSetting_Ternary }

constructor TCustomSetting_Ternary.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aVisible: boolean = true);
begin
    inherited Create(aKey, aDescription, aVisible, cpeTernary, aOnAddEditFunctions);
end;

{ TCustomSetting_Thermostat }

constructor TCustomSetting_Thermostat.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeThermostat, aOnAddEditFunctions);
end;

{ TCustomSetting_TimeUnit }

constructor TCustomSetting_TimeUnit.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeTimeUnit, aOnAddEditFunctions);
end;

{ TCustomSetting_TipMap }

constructor TCustomSetting_TipMap.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent; aOnGetEditFunctionParams: TCustomSettingGetEditFunctionParamsEvent);
begin
    inherited Create(aKey, aDescription, true, cpeTipMap, aOnAddEditFunctions, aOnGetEditFunctionParams);
end;

{ TCustomSetting_TipType }

constructor TCustomSetting_TipType.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeTipType, aOnAddEditFunctions);
end;

{ TCustomSetting_ToolRack }

constructor TCustomSetting_ToolRack.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeToolRack, aOnAddEditFunctions, DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_ToolRack.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TRackEditFunctionParams.Create(self);
end;

{ TCustomSetting_TriggerDevice }

constructor TCustomSetting_TriggerDevice.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeTriggerDevice, aOnAddEditFunctions);
end;

{ TCustomSetting_VolMotorDevice }

constructor TCustomSetting_VolMotorDevice.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeVolMotorDevice, aOnAddEditFunctions);
end;

{ TCustomSetting_VolumeArray }

constructor TCustomSetting_VolumeArray.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeVolumeArray, aOnAddEditFunctions);
end;

{ TCustomSetting_VolumeCorrectionCurve }

constructor TCustomSetting_VolumeCorrectionCurve.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeVolumeCorrectionCurve, aOnAddEditFunctions);
end;

{ TCustomSetting_WashProgram }

constructor TCustomSetting_WashProgram.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeWashProgram, aOnAddEditFunctions,
        DoOnGetEditFunctionParams);
end;

procedure TCustomSetting_WashProgram.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSingleValueEditFunctionParams.Create(self);
end;

{ TCustomSetting_YesNo }

constructor TCustomSetting_YesNo.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeYesNo, aOnAddEditFunctions);
end;

{ TCustomSetting_YesNoDelete }

constructor TCustomSetting_YesNoDelete.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true, cpeYesNoDelete, aOnAddEditFunctions);
end;

class function TCustomSetting_YesNoDelete.GetStoreOrDelete(const aText: string): TStoreOrDelete;
begin
    if (aText = STR_YES) then
        EXIT(sdStore);
    if (aText = cDeleteText) then
        EXIT(sdDelete);
    EXIT(sdNull);
end;


end.
