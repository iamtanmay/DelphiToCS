{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  : Method step settings (parameters)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  04.10.04 wl                               TN2008.3  initial version
  21.12.04 wl                               TN2247.1  verschiedene Ergänzungen
  04.05.05 wl  TMethodStepParameter_SQLFileName                TN2248.8  Besseres Editieren des SQL-Dateipfades
  14.06.05 wl  TMethodStepParameter_CommandMacro.GetPickList   TN2439.2  Wenn Command-Table nicht vorhanden keine PickList erstellen
  26.08.05 wl  TMethodStepParameter_Rack.GetEditingProperties  TN2541.0  Bezug auf cxEdit entfernt
  23.09.05 wl  TMethodStepParameter_DllCall.Create             TN2627    new parameter: Store as ...
  13.10.05 wl  TMethodStepParameter_DllCall.GetValue           TN2627    Bugfix
  13.10.05 wl  TMethodStepParameter_Ternary                    TN2546.1  neu: bietet YES und NO an
  14.10.05 wl  TMethodStepParameter_Thermostat                 TN2670    erweitert für CANBus-Thermostate
  15.10.05 wl  TMethodStepParameter_SwitchDevice               TN2581    listet Switch-Devices auf
  15.10.05 wl  TMethodStepParameter_Shaker                     TN2582    listet Shaker-Devices auf
  15.10.05 wl  TMethodStepParameter_WashProgram                TN2679    listet Waschprogramme auf
  03.11.05 wl  TMethodStepParameter_GenericImportOptions       TN2724    beinhaltet Import-Optionen für alle Actions außer IMPOR-Action
  03.11.05 wl  TMethodStepParameter_ImportDefName              TN2724    zeigt Liste mit allen Import-Definitionen
  03.11.05 wl  TMethodStepParameter_FileName                   TN2724    hat die Möglichkeit, eine Datei auszuwählen
  08.11.05 wl  TMethodStepParameter_ZPositionType              TN2728    für ZPosMovementAction
  08.11.05 wl  TMethodStepParameter_DeviceState                TN2435    jetzt mit DEFAULT!
  17.11.05 wl  TMethodStepParameter_GripperArmName             TN2771    zeigt Gripperarm-Namen
  20.11.05 wl  TMethodStepParameter_TipType                    TN2784    für ChangeTips-Action
  24.11.05 pk  TMethodStepParameter_Washprogram                TN2765    use TWashProgDataAdaptor
  28.11.05 wl  TMethodStepParameter_DeviceState                TN2812    jetzt mit BOTH!
  13.12.05 wl  TMethodStepParameter_PathName                   TN2856    hat die Möglichkeit, einen Pfad auszuwählen
  05.01.06 pk  TMethodStepParameter_GripperArmName             TN2877    GetGripperArmNames was removed from tipmanager
  20.11.05 pk                                                  TN2891    new params from MethodStepsRack, MethodStepsSimple, etc.
  24.01.06 wl  TMethodStepParameter_SQLSelect                  TN2885    neue Parameter für SLSQL
  02.02.06 wl  TMethodStepParameter_SQLSelect                  TN2885.1  neu: DefaultVauesStr
  08.03.06 thr TMethodStepParameter_Balance                    TN2941    neu
  14.03.06 wl  TMethodStepParameter_ImportDefName              TN2889.2  neu: Parameter ImportDefMode
  15.03.06 wl  TMethodStepParameter_ZPosMovement               TN2966    neu: DisableError
  25.03.06 pk  TMethodStepParameter_InitDevice                 TN1997    New
  25.03.06 pk  TMethodStepParameter_RunLoad                    TN2998    New Parameter: NumLinesToRepeat, repeat more than just the RUN line
  29.03.06 wl  TMethodStepParameter_XYMovement                 TN3005    Neue Parameter: XSpeed,xRamp,YSpeed,YRamp
  29.03.06 wl  TMethodStepParameter_ZPosMovement               TN3006    Neuer Parameter: ZRamp
  03.04.06 thr TMethodStepParameter_Float                      TN3007    neu
  06.06.06 wl  TMethodStepParameter_TimerWait                  TN3128    neue Parameter
  18.07.06 pk  TMethodStepParameter_TubeBitOptions             TN3205    new 8192 for entering  barcode manually
  04.09.06 pk  TMethodStepParameter_GroupID                    TN3280    new
  07.09.06 wl  TMethodStepParameter_Calli                      TN3288    CALLI action entfernt
  18.09.06 pk  TMethodStepParameter_VortexerFixationState      TN3248    New
  20.09.06 wl  TMethodStepParameter_MotorMove                  TN3318    neu: MOTOR Action
  20.09.06 wl  TMethodStepParameter_MotorDevice                TN3318    sucht nach Motor-Devices
  20.09.06 wl  TMethodStepParameter_KindOfMove                 TN3318    ABSOLUTE oder RELATIVE
  27.10.06 pk  TMethodStepParameter_TubeBitOptions             TN3386    New tube option. Methods changes for easier addition of new options
  31.10.06 pk  TMethodStepParameter_Message                    TN3391    New parameter: PauseRun
  06.11.06 wl  TMethodStepParameter_SensorDevice               TN3394    enthält alle Sensoren-Namen
  06.11.06 wl  TMethodStepParameter_SensorWaitForState         TN3394    Default, NotDefault oder DoNotWait
  06.11.06 wl  TMethodStepParameter_SensorCheck                TN3394    alle Parameter für TSensorCheckMethodStep
  27.11.06 wl  TMethodStepParameter_WeighWizard                TN3362    neu: für WeighWizardMethodStep, enthält TeachVol & TeachParam
  27.11.06 wl  TMethodStepParameter_WeighWizard                TN3419    neuer Parameter UseLastWeightAsTare
  28.11.06 wl  TMethodStepParameter_ImportIntoTable            TN3397    Import-Action: Parameter nur noch für neuen Import
  05.12.06 wl                                                  TN3448    Actions WASH und WASH? entfernt
  07.12.06 wl                                                  TN3456    aOwner aus TMethodStepParameter entfernt
  07.12.06 wl  TMethodStepParameter_NoKeyOption                TN3456    kann keine GenericImportOptionen enthalten
  18.01.07 wl  TMethodStepParameter_VortexerSpeedWaitFor       TN3507    zur Zeit NO und IFSPEEDISNULL
  18.01.07 wl  TMethodStepParameter_VortexerSpeed              TN3507    mit neuen Parameter WaitFor
  18.01.07 pk  TMethodStepParameter_AddSequence                TN3482    New
  19.02.07 wl  TMethodStepParameter_VirtualRackMove            TN3585    neu: VirtualRackMoveAction
  20.02.07 wl  TMethodStepParameter_MethodParams.EditValue     TN3016    ParserType old entfernt
  21.02.07 wl  TMethodStepParameter_TipsGet                    TN3588    neu
  12.03.07 pk  TMethodStepParameter_RackMove                   TN3629    new RackMoveOptions
  13.03.07 pk  TMethodStepParameter_TipMap.EditValue           TN3633    New
  16.04.07 wl  TMethodStepParameter_SQLTermParams              TN3547    neu: benutzt SQLTermParserInterface zur Eingabe der Parameter
  16.04.07 wl  TMethodStepParameter_SQLTermName                TN3547    vorher: SQLFileName
  16.04.07 wl  TMethodStepSubItemParameter                     TN3547    Basisklasse für SQLTerm und SubMethod-Steps
  17.07.07 pk  TMethodStepParameter_AddSequence                TN3653    New Optimization options
  23.07.07 wl  TMethodStepParameter_Washprogram.GetPickList    TN3792    WashProgDataAdaptor ohne Instance
  24.07.07 wl  TMethodStepParameter_WashProg                   TN3792   jetzt mit UsedTips und UsedTipType
  26.07.07 pk  TMethodStepSubItemParameter                     TN3805    removed. use NameParam instead
  26.07.07 pk                                                  TN3805    WashP, AddSeq, Command, now all set the NameParam field
  07.08.07 wl  TMethodStepParameter_CommandName.GetPickList    TN3811.3  benutzt TCommandDataAdaptor.ReadAllItemNames
  07.08.07 wl  TMethodStepParameter_ImportDefName.GetPickList  TN3811.3  benutzt TImportDefDataAdaptor.ReadAllDefNames
  07.08.07 wl                                                  TN3811.3 benutzt ReadAllItemNames
  09.11.07 pk                                                  TN3922   uses QryTools2 Removed
  13.11.07 wl  TMethodStepParameter_PowderDetection            TN3844   Neue Action: PWDET
  08.01.08 wl                                                  TN3972    neu: enthält große Teile der unit MethodStepParameters
  04.04.08 wl                                                  TN4058    uses geändert
  25.09.08 wl  TMethodStepSetting_CallDll                      TN4242    --> CallDllRunStep
  06.10.08 pk                                                  TN4258   Various changes
  15.10.08 pk  TMethodStepSetting_MethodParams                 TN4258   removed
  15.10.08 pk                                                  TN4258   inherit from TMethodStepCompositeSetting again
  20.02.09 wl  TMethodStepSetting_...                          TN4438   aDescription direkt angegeben statt mit aResNo
  17.07.09 pk  GetParseValueAsTubeBitOptions                   TN4662   New
  04.11.09 pk                               		      TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  15.12.09 ts  TMethodStepSetting_RackBitOptions               TN4947   new
  13.04.10 wl                                                  TN5044   uses geändert
  06.10.10 pk                                                  TN5290   Changes for TCustomSettingGetEditFunctionParamsEvent
  07.02.11 wl  TCustomSetting_DialogInputItem                  TN5460   von DialogInputRunStep hierher
  12.12.11 wl  TMethodStepSetting_MultiPipParam                TN5764   von PipetteBlockRunAction hierher
  25.04.12 wl  TMethodStepSetting_MultiPipParam                TN5878   --> BasicPipetteTypes
  11.04.13 wl                                                  TN6045   an TCustomSetting-Änderungen angepasst
  -------------------------------------------------------------------------------------------------- }

unit MethodStepSettings;


interface


uses
    SysUtils,
    Classes,
    AppTypes,
    MethodTypes,
    MethodStep,
    CustomSetting;

type
    TCustomSetting_DialogInputItem = class(TMethodStepCompositeSetting)
    private
        function GetDialogInputItemOptionsParseValue: TDialogInputValues;
        function GetInputDefaultValue: TCustomSetting;
        function GetInputDescription: TCustomSetting;
        function GetInputDropdownList: TCustomSetting;
        function GetInputEditType: TCustomSetting;
        function GetInputMaxValue: TCustomSetting;
        function GetInputMinValue: TCustomSetting;
    public
        constructor Create(const aKey, aDescription: string; aOnAddEditFunctions: TNotifyEvent);
        property InputDescription: TCustomSetting read GetInputDescription;
        property InputDefaultValue: TCustomSetting read GetInputDefaultValue;
        property InputMinValue: TCustomSetting read GetInputMinValue;
        property InputMaxValue: TCustomSetting read GetInputMaxValue;
        property InputEditType: TCustomSetting read GetInputEditType;
        property InputDropdownList: TCustomSetting read GetInputDropdownList;
        property DialogInputItemOptionsParseValue: TDialogInputValues
            read GetDialogInputItemOptionsParseValue;
    end;

    TMethodStepSetting_SlotStruct = class(TMethodStepCompositeSetting)
    private
        function GetCarrierName: TCustomSetting;
        function GetRotation: TCustomSetting;
        function GetSlot: TCustomSetting;
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    protected
        function GetOptionSummary: string; override;
    public
        // constructor/destructor
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
        property CarrierName: TCustomSetting read GetCarrierName;
        property Slot: TCustomSetting read GetSlot;
        property Rotation: TCustomSetting read GetRotation;
        // property OptionSummary : string read GetOptionSummary;
    end;

    TMethodStepSetting_TubeBitOptions = class(TMethodStepCompositeSetting)
    private
        function GetIntValueFrumSubItems: integer;
        function GetParseValueAsTubeBitOptions: integer;
    protected
        function GetValue(): string; override;
        procedure SetValue(const aValue: string); override;
        function GetParseValue: string; override;
    public
        // constructor/destructor
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
        property ParseValueAsTubeBitOptions: integer read GetParseValueAsTubeBitOptions;
    end;

    TMethodStepSetting_RackBitOptions = class(TMethodStepCompositeSetting)
    private
        function GetIntValueFromSubItems: integer;
        function GetParseValueAsRackBitOptions: integer;
    protected
        function GetValue(): string; override;
        procedure SetValue(const aValue: string); override;
        function GetParseValue: string; override;
    public
        // constructor/destructor
        constructor Create(const aKey: string; const aDescription: string; aOnAddEditFunctions: TNotifyEvent);
        property ParseValueAsRackBitOptions: integer read GetParseValueAsRackBitOptions;
    end;

    TMethodStepSetting_PipDeviceAndUsedTips = class(TMethodStepCompositeSetting)
    private
        fPipDeviceKey, fUsedTipsKey: string;
        function GetPipDeviceName: TCustomSetting;
        function GetUsedTips: TCustomSetting;
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(const aPipDeviceKey, aUsedTipsKey: string;
            aOnAddEditFunctions: TNotifyEvent); overload;
        constructor Create(aOnAddEditFunctions: TNotifyEvent); overload;
        property PipDeviceName: TCustomSetting read GetPipDeviceName;
        property UsedTips: TCustomSetting read GetUsedTips;
    end;


implementation


uses
    AppSettings,
    MethodGUIParsing,
    UtilLib,
    MethodDataAdaptor,
    GeneralTypes,
    ImportDataAdaptor,
    SQLTermsDataAdaptor,
    SamGlobe,
    WashprogDataAdaptor,
    CustomLeafSettings,
    SequenceDataAdaptor,
    CustomEditFunctionParams;

const
    cMethOptionKeyDefaultUsedPipDevice = 'USEDPIPDEVICE';
    cMethOptionKeyDefaultUsedTips = 'USEDTIPS';

    { TCustomSetting_DialogInputItem }

constructor TCustomSetting_DialogInputItem.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true);
    AddParam(TCustomLeafSetting.Create(cOptionKeyDialogInputDescription, '', false));
    AddParam(TCustomLeafSetting.Create(cOptionKeyDialogInputDefaultValue, '', false));
    AddParam(TCustomLeafSetting.Create(cOptionKeyDialogInputMinValue, '', false));
    AddParam(TCustomLeafSetting.Create(cOptionKeyDialogInputMixValue, '', false));
    AddParam(TCustomLeafSetting.Create(cOptionKeyDialogInputEditType, '', false));
    AddParam(TCustomLeafSetting.Create(cOptionKeyDialogInputDropdownList, '', false));
    SetValue('');
end;

function TCustomSetting_DialogInputItem.GetDialogInputItemOptionsParseValue: TDialogInputValues;
begin
    result.Description := self.InputDescription.ParseValue;
    result.DefaultValue := self.InputDefaultValue.ParseValue;
    result.MinValue := self.InputMinValue.ParseValue;
    result.MaxValue := self.InputMaxValue.ParseValue;
    result.EditType := self.InputEditType.ParseValue;
    result.DropdownList := self.InputDropdownList.ParseValue;
    // result. := self.RunName.ParseValue;
    // result.Params  := self.Params.ParseValuesAsKeyValueArray;
end;

function TCustomSetting_DialogInputItem.GetInputDescription: TCustomSetting;
begin
    result := self.Find(cOptionKeyDialogInputDescription);
end;

function TCustomSetting_DialogInputItem.GetInputDefaultValue: TCustomSetting;
begin
    result := self.Find(cOptionKeyDialogInputDefaultValue);
end;

function TCustomSetting_DialogInputItem.GetInputMinValue: TCustomSetting;
begin
    result := self.Find(cOptionKeyDialogInputMinValue);
end;

function TCustomSetting_DialogInputItem.GetInputMaxValue: TCustomSetting;
begin
    result := self.Find(cOptionKeyDialogInputMixValue);
end;

function TCustomSetting_DialogInputItem.GetInputEditType: TCustomSetting;
begin
    result := self.Find(cOptionKeyDialogInputEditType);
end;

function TCustomSetting_DialogInputItem.GetInputDropdownList: TCustomSetting;
begin
    result := self.Find(cOptionKeyDialogInputDropdownList);
end;

{ TMethodStepSetting_SlotStruct }

constructor TMethodStepSetting_SlotStruct.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true);
    // Setting 1 - 7
    AddParam(TCustomSetting_CarrierNameAndSlot.Create(STR_OPTION_SUBKEY_SLOT_CARRIER,
        TLanguageString.Read('Carrier Name', 'Carriername'), aOnAddEditFunctions, DoOnGetEditFunctionParams));
    AddParam(TCustomSetting_CarrierSlotNo.Create(STR_OPTION_SUBKEY_SLOT_SLOTNO,
        TLanguageString.Read('Slot Number', 'Slot-Nummer'), aOnAddEditFunctions, DoOnGetEditFunctionParams));
    AddParam(TCustomSetting_CarrierSlotRotation.Create(STR_OPTION_SUBKEY_SLOT_ROTATION,
        TLanguageString.Read('Rotation value of the rack on this slot',
        'Rotationswert des Racks auf diesem Slot'), aOnAddEditFunctions, DoOnGetEditFunctionParams));

    SetValue('');
end;

procedure TMethodStepSetting_SlotStruct.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TSlotStructEditFunctionParams.Create(self.CarrierName, self.Slot);
end;

function TMethodStepSetting_SlotStruct.GetCarrierName: TCustomSetting;
begin
    result := self.Find(STR_OPTION_SUBKEY_SLOT_CARRIER);
end;

function TMethodStepSetting_SlotStruct.GetRotation: TCustomSetting;
begin
    result := self.Find(STR_OPTION_SUBKEY_SLOT_ROTATION);
end;

function TMethodStepSetting_SlotStruct.GetSlot: TCustomSetting;
begin
    result := self.Find(STR_OPTION_SUBKEY_SLOT_SLOTNO);
end;

function TMethodStepSetting_SlotStruct.GetOptionSummary: string;
begin
    result := TTypeSafeFormat.Format('Carrier: {0}, Slot: {1}, Rotation: {2}',
        [self.CarrierName.Value, self.Slot.Value, self.Rotation.Value])
end;

{ TMethodStepSetting_TubeBitOptions }

constructor TMethodStepSetting_TubeBitOptions.Create(const aKey: string; const aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true);

    // Setting 0 - 11
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('Use Rack ID Pos', 'Use Rack ID Pos'),
        aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('Tubes are shaken', 'Tubes werden geschüttelt'),
        aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.
        Read('Tubes that can not be read are put to the TubeWaste',
        'nicht gelesene Tubes wandern in den TubeWaste'), aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('Tubes are not weighed a second time',
        'Tubes werden kein zweites Mal gewogen'), aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('Use SafeMove-Option',
        'Tubes werden im SafeMove-Modus bewegt'), aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.
        Read('No waiting times after getting or putting the tube',
        'Sleeps nach dem Aufnehmen und Ablegen der Tubes werden weggelassen'), aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.
        Read('Decapping at/after barcode reading and if CapWaste is defined put cap into waste',
        'Decapping beim/nach Barcodelesen und (wenn vorhanden) Deckel in Waste bringen'),
        aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.
        Read('Put cap on tube after reading and weighing (from cap-park-rack if existing)',
        'Nach dem Lesen & Wiegen einen neuen Deckel aufsetzen (aus Cap-Park-Rack, wenn vorhanden)'),
        aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.
        Read('Decap, hold cap in cap gripper, (weigh), put cap on tube',
        'Entdeckeln und Kappe in Cap-Greifer belassen, evtl. Wiegen, Cap zurück auf Tube'),
        aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('An existing cap sensor will not be used',
        'vorhandener Sensor zum Prüfen des Decappers wird nicht benutzt'), aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('An existing tube sensor will not be used',
        'vorhandener Sensor zum Prüfen des Tube-Transports wird nicht benutzt'), aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('Decap and put cap into cap-park-rack',
        'Decapping und Kappe auf nächste Position eines Cap-Park-Racks bringen (statt in Cap-Waste)'),
        aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('Decap and hold cap in cap gripper',
        'Entdeckeln und Kappe in Cap-Greifer belassen.'), aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('Do not move tube information',
        'Tube informationen nicht der Zielposition zuordnen'), aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('Enter barcode manually',
        'Barcode manuell eintragen'), aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.
        Read('Move tube over the decapper, balance, or destination position during barcode reading',
        'Tube wird während des Barcodlesens über die Decapper-, Waage-, oder Zielposition bewegt'),
        aOnAddEditFunctions));

    SetValue('');
end;

function TMethodStepSetting_TubeBitOptions.GetIntValueFrumSubItems(): integer;
var
    x: integer;
begin
    if ((self.Params[0] as TCustomSetting_Bit).BitValue) then
    begin
        result := -1;
        EXIT;
    end;

    result := 0;
    for x := 1 to INT_NUM_TUBEOPTIONS - 1 do
    begin
        if ((self.Params[x] as TCustomSetting_Bit).BitValue) then
            result := result + ARR_POWER_OF_TWO[x];
    end;

end;

function TMethodStepSetting_TubeBitOptions.GetValue: string;
begin
    result := IntToStr(GetIntValueFrumSubItems());
end;

procedure TMethodStepSetting_TubeBitOptions.SetValue(const aValue: string);
var
    xIntValue, xErr, x: integer;
begin
    Val(aValue, xIntValue, xErr);

    (self.Params[0] as TCustomSetting_Bit).BitValue := (xIntValue = -1);
    if (xIntValue < 0) then
        xIntValue := 0;

    for x := 1 to INT_NUM_TUBEOPTIONS - 1 do
    begin
        (self.Params[x] as TCustomSetting_Bit).BitValue := ((xIntValue and (1 shl (x - 1))) <> 0);
    end;
end;

function TMethodStepSetting_TubeBitOptions.GetParseValue: string;
var
    xValue: string;
    x: integer;
    xValues: TStringKeyStringValueList;
    xIntResult: integer;
begin
    xValue := inherited GetParseValue;
    xValues := TMethodGUIParser.CreateKeyValueList();;
    try
        TMethodGUIParser.OptionsStrToList(xValues, xValue);

        xIntResult := 0;

        for x := 1 to INT_NUM_TUBEOPTIONS - 1 do
        begin
            if (xValues.Values[x] = TCustomSetting_Bit.GetYesString) then
                xIntResult := xIntResult + ARR_POWER_OF_TWO[x];
        end;
    finally
        xValues.Free;
    end;
    result := IntToStr(xIntResult);
end;

function TMethodStepSetting_TubeBitOptions.GetParseValueAsTubeBitOptions: integer;
var
    x: integer;
    xIntResult: integer;
begin
    xIntResult := 0;
    for x := 1 to self.Params.Count - 1 do
    begin
        if SameText(self.Params[x].ParseValue, TCustomSetting_Bit.GetYesString) then
            xIntResult := xIntResult + ARR_POWER_OF_TWO[x];
    end;
    result := xIntResult;
end;

{ TMethodStepSetting_RackBitOptions }

constructor TMethodStepSetting_RackBitOptions.Create(const aKey, aDescription: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aKey, aDescription, true);

    // Setting 0 - 1
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('Allow non unique barcode',
        'Nicht eindeutige Barcodes erlauben'), aOnAddEditFunctions));
    AddParam(TCustomSetting_Bit.Create(TLanguageString.Read('Read Barcode manually', 'Barcode manuell lesen'),
        aOnAddEditFunctions));

    SetValue('');
end;

function TMethodStepSetting_RackBitOptions.GetIntValueFromSubItems: integer;
var
    x: integer;
begin
    result := 0;
    for x := 0 to INT_NUM_RACKOPTIONS - 1 do
    begin
        if ((self.Params[x] as TCustomSetting_Bit).BitValue) then
            result := result + ARR_POWER_OF_TWO[x + 1];
    end;

end;

function TMethodStepSetting_RackBitOptions.GetParseValue: string;
var
    xValue: string;
    x: integer;
    xValues: TStringKeyStringValueList;
    xIntResult: integer;
begin
    xValue := inherited GetParseValue;
    xValues := TMethodGUIParser.CreateKeyValueList();;
    try
        TMethodGUIParser.OptionsStrToList(xValues, xValue);

        xIntResult := 0;

        for x := 0 to INT_NUM_RACKOPTIONS - 1 do
        begin
            if (xValues.Values[x] = TCustomSetting_Bit.GetYesString) then
                xIntResult := xIntResult + ARR_POWER_OF_TWO[x + 1];
        end;
    finally
        xValues.Free;
    end;
    result := IntToStr(xIntResult);
end;

function TMethodStepSetting_RackBitOptions.GetParseValueAsRackBitOptions: integer;
var
    x: integer;
    xIntResult: integer;
begin
    xIntResult := 0;
    for x := 0 to self.Params.Count - 1 do
    begin
        if SameText(self.Params[x].ParseValue, TCustomSetting_Bit.GetYesString) then
            xIntResult := xIntResult + ARR_POWER_OF_TWO[x + 1];
    end;
    result := xIntResult;
end;

function TMethodStepSetting_RackBitOptions.GetValue: string;
begin
    result := IntToStr(GetIntValueFromSubItems());
end;

procedure TMethodStepSetting_RackBitOptions.SetValue(const aValue: string);
var
    xIntValue, xErr, x: integer;
begin
    Val(aValue, xIntValue, xErr);

    if (xIntValue < 0) then
        xIntValue := 0;

    for x := 0 to INT_NUM_RACKOPTIONS - 1 do
    begin
        (self.Params[x] as TCustomSetting_Bit).BitValue := ((xIntValue and (1 shl (x))) <> 0);
    end;
end;

{ TMethodStepSetting_PipDeviceAndUsedTips }

constructor TMethodStepSetting_PipDeviceAndUsedTips.Create(const aPipDeviceKey, aUsedTipsKey: string;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);
    fPipDeviceKey := aPipDeviceKey;
    fUsedTipsKey := aUsedTipsKey;
    AddParam(TCustomSetting_PipDeviceName.Create(fPipDeviceKey, TLanguageString.Read('Pipetting device name',
        'Pipettierdevice (Name)'), aOnAddEditFunctions));
    AddParam(TCustomSetting_TipMap.Create(fUsedTipsKey, TLanguageString.Read('Used Tips (Bitmap)',
        'Benutzte Nadeln (Bitmap)'), aOnAddEditFunctions, DoOnGetEditFunctionParams));
    SetValue('');
end;

constructor TMethodStepSetting_PipDeviceAndUsedTips.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    Create(cMethOptionKeyDefaultUsedPipDevice, cMethOptionKeyDefaultUsedTips, aOnAddEditFunctions);
end;

procedure TMethodStepSetting_PipDeviceAndUsedTips.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TPipDeviceAndUsedTipsEditFunctionParams.Create(self.PipDeviceName, self.UsedTips);
end;

function TMethodStepSetting_PipDeviceAndUsedTips.GetOptionSummary: string;
begin
    result := TTypeSafeFormat.Format('Device {0}, Used Tips {1}',
        [self.PipDeviceName.Value, self.UsedTips.Value]);
end;

function TMethodStepSetting_PipDeviceAndUsedTips.GetPipDeviceName: TCustomSetting;
begin
    result := self.Find(fPipDeviceKey);
end;

function TMethodStepSetting_PipDeviceAndUsedTips.GetUsedTips: TCustomSetting;
begin
    result := self.Find(fUsedTipsKey);
end;


end.
