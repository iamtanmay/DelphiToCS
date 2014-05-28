{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Edit functions for the different TCustomLeafSettings
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  09.01.08 wl                               TN3972    initial version
  30.01.08 pk                               TN3864    gModules.ListAll function added again
  07.02.08 wl                               TN4009    uses IntfThermostatDevice
  29.02.08 wl  LiqParam_EditValue           TN4009    neu: zum Oeffnen der Liquid Handling Parameter
  17.03.08 wl                               TN4043    uses IntfMixerDevice
  25.04.08 wl  VolMotorDevice_GetPickList   TN4051    neu für MOTOR (auf VolMotor reduziert)
  25.04.08 wl  MotorDevice_GetPickList      TN4051    Motoren für MMOVE
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  03.07.08 wl                               TN4157
  09.07.08 pk CarrierName_GetPickList       TN4139    Check if currentlayout exists before using it
  21.07.08 pk CarrierTypeName_GetPickList   TN4179    New
  31.07.08 pk CarrierTypeName_GetPickList   TN4193    New
  08.09.08 pk                               TN4215    uses changed
  20.09.08 pk GripDevice_GetPickList        TN4215    New
  20.09.08 pk TriggerDevice_GetPickList     TN4215    New
  25.09.08 wl                               TN4242    Command macros endgültig entfernt
  07.10.08 pk TipMapEditValue               TN4266    Return old value if user pressed cancel
  07.10.08 pk MethodName_GetPickList        TN4265    Uses MethodDataCache
  15.10.08 pk                               TN4258    Editvalue functions no longer return string
  17.11.08 wl  TCustomSetting_DialogInput   TN4310    for INPUT action parameters
  09.12.08 pk  MethodParams_EditValue       TN4279    calls function from TEditMethodParams
  17.12.08 pk  ReadDeviceNamesForType       TN4374    New based on ModuleSettingsManager instead of ModuleManagers
  07.01.09 pk  MethodParams_EditValue       TN4380.1  changes for EditMethodParameters
  16.01.09 wl                               TN4362   an Änderungen in TViewItem angepasst
  20.02.09 wl  procedure YesNo_GetPickList  TN4438    benutzt keine Resourcen mehr
  04.03.09 pk                               TN4232    uses GeneralTypes
  04.03.09 pk  FileName_EditValue           TN4451    now with NoChangeDir Option
  09.04.09 pk                               TN4521    Assigned( CurrentLayout ) changed to not IsCurrentLayoutEmpty
  06.07.09 pk  EditMethodParameters         TN4585.4  aParameters changed to TStreamableKeyValueList
  21.08.09 wl                               TN4702   uses RessourceLoader entfernt
  28.08.09 pk                               TN4753   LayoutManager replaced by ZADesignLayoutManager
  28.08.09 pk                               TN4753   Liquids replaced by DesignLiquids
  28.08.09 pk                               TN4753   gPipDeviceManager removed, functions now based on DesignModuleSettingFinder
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  12.11.09 pk  BCReaderDevice_GetPickList   TN4857   New
  04.02.10 pk                               TN4972   Changes for Restart
  09.02.10 pk                               TN4973   use TDialogUtils.SelectDirectory instead of FileCtrl.SelectDirectory
  23.04.10 pk                               TN5062   EditMethodParameters New Parameter aIsBuildMode set to false
  07.05.10 pk  MethodParams_EditValue       TN5092   Now calls new Editor_EditMethodParameters funtions
  10.05.10 ts  TipMap_EditValue             TN5098   Messagebox with Warning appears if Tipmap will be selected via button without an entered pipdevice
  12.05.10 wl                               TN5064   uses VariableCompositeSetting
  06.10.10 pk                               TN5290   More flexible mechanism of calling EditFunction using TCustomSettingEditFunctionParams
  03.11.10 wl  GripperArmName_GetPickList   TN5306   ruft neue Funktion TDesignModuleSettingFinder.ReadGripperArmNames auf
  17.12.10 wl  MethodParams_EditValue       TN5414   Editierte Methodenparameter werden jetzt auch übernommen
  07.02.11 wl                               TN5460   alle Funktionen mit neuen Parametern
  14.02.11 wl  cpePosOfDouble/SingleRackPos TN5468   auch bei den Positionen erscheint ein Edit-Button
  05.03.11 wl  ..._OpenValue                TN5472   neue Funktionen für MethodName, LiquidParamName, WashProgram, SQLTerm, Sequence
  10.03.11 wl  ResourceSchemeName_GetPickList  TN5499   NOVUM: Picklist enthält Werte und Kommentare, die nicht mitgespeichert werden
  08.07.11 wl  RackSinglePosArray_EditValue    TN5626   neu, für: XYMovement, Aspirate, Dispense, Pipette (New)
  31.08.11 wl  TimeUnit_GetPickList            TN5677   von DelayRunStep hierher
  03.10.11 ts  PHMeter/PumpDevice_GetPickList  TN5574   neu
  20.09.11 wl  Color_EditValue                 TN5723   neu: für Color-Parameter bei ManualFill
  01.11.11 wl  VolumeCorrectionCurve_GetPickList TN5731   neu
  03.11.11 wl  SubstanceSet_GetPickList          TN5729   neu für SubstanceLoad, SubstanceSave
  14.12.11 wl                                    TN5765   ohne Iterate
  03.02.12 wl  TCustomSetting_SubstanceID        TN5792   neu
  13.03.12 wl  RackSinglePosArray_EditValue      TN5797   funktioniert jetzt wie RackSinglePos
  05.12.12 wl                                    TN6045   DesignLiquids entfernt
  11.02.13 wl  TCustomSetting_YesNoDelete        TN6078   neu
  21.02.13 wl                                    TN6045   in Editing verschoben
  21.02.13 wl                                    TN6045   Methoden teilweise in DesignerEditFunctions verschoben
  01.03.13 wl                                    TN6101   vorbereitet für neuen Dialog
  05.03.13 wl  ...ArrayRefToRackPos              TN6101   neue Klassen
  06.03.13 wl  OnGetPickList, OnOpenFunction     TN6103   Parameter geändert
  05.03.13 wl                                    TN6101   uses BasicEditFunctions
  11.03.13 wl                                    TN6095   jetzt abgeleitet von TBasicEditFunctions
  27.03.13 wl                                    TN6095   an TBasicEditFunctions angepasst
  19.04.13 wl                                    TN6095   für MultiPageDialog geändert
  14.05.13 wl                                    TN6095   mit Caption
  12.08.13 wl  EditRackPosArray                  TN6214   Parameter SingleValueAllowed = true
  -------------------------------------------------------------------------------------------------- }

unit EditFunctions;


interface


uses
    Generics.Collections,
    Layout,
    CustomSetting,
    BasicEditFunctions,
    EditableParameter,
    ModuleSettings;

type
    TEditFunctions = class(TBasicEditFunctions)
    private
        fCheckMethodParams: boolean;
        fEditRackPosArrayNoOfPos: integer;
        function EditRackPosArrayGetNoOfPos(aDummy: integer): integer;
    protected
        function ReadDeviceNamesForType(const aModuleID: TModuleID): TArray<string>;
        function GetCurrentLayout(): TLayout; virtual; abstract;
        procedure AddRackAndPos(aEditParams: TObjectList<TSimpleParameter>;
            aRackNameParam, aPosParam: TCustomSetting; aColumnEnabled: boolean);

        function EditRack(var vValue: string): boolean;
        function EditRackSinglePos(var vRackName: string; var vPos: string; const aTitle: string): boolean;
        function EditCarrier(var vValue: string): boolean;
        function EditCarrierSlot(var vCarrierName: string; var vCarrierSlot: integer): boolean;
        function EditRackPosArray(aCheckValues: boolean; aParams: TList<TSimpleParameter>;
            const aTitle: string; aLineCountFixed: boolean; const aNoOfPos: integer): boolean;
    public
        constructor Create(aCheckMethodParams: boolean);
        function EditRackDoublePos(var vRackName: string; var vFirstPos, vLastPos: string;
            const aTitle: string): boolean;
        procedure Parameter_OnAddEditFunctions(aSender: TObject);
        //
        function Balance_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        function BCReaderDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function Bit_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        function CarrierName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        procedure CarrierName_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function CarrierSlotNo_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        procedure CarrierSlotNo_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function CarrierSlotRotation_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function CarrierTypeName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        procedure Color_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        function DeviceName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        procedure DialogInput_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function DiluentName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        procedure FileName_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        function GripDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        function GripperArmName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function GroupID_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        function ImportDefName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function LiqParam_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        procedure LiqParam_OpenValue(aSetting: TObject); virtual;
        function MemoryDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function MethodName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        procedure MethodName_OpenValue(aSetting: TObject); virtual;
        procedure MethodParams_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function LocationBasedMotionDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function MotorDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        procedure PathName_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        function PHMeterDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function PipDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        function PumpDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        procedure Rack_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        function Rack_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        procedure RackSinglePos_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        procedure RackPosArray_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        procedure RackDoublePos_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function RackTypeName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        procedure ReferringToRackPosArray_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function ResourceSchemeName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function SensorDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function SequenceName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        procedure SequenceName_OpenValue(aSetting: TObject); virtual;
        function ShakerDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function SQLTermName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        procedure SQLTermName_OpenValue(aSetting: TObject); virtual;
        procedure SQLTermParams_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function SubstanceID_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function SubstanceSet_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function SwitchDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function SystemRack_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        function TimeUnit_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        function TriggerDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function Thermostat_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        procedure TipMap_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        function TipType_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        procedure ToolRack_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        function ToolRack_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        function VolumeCorrectionCurve_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function VolMotorDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        function WashProgram_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
        procedure WashProgram_OpenValue(aSetting: TObject); virtual;
        function YesNo_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
        function YesNoDelete_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
            : TArray<string>;
    end;


implementation


uses
    Controls,
    SysUtils,
    Dialogs,
    Forms,
    Windows,
    DialogUtils,
    ArrayUtils,
    GeneralTypes,
    AppTypes,
    ImportDataAdaptor,
    CustomLeafSettings,
    MethodStepSettings,
    MethodTypes,
    SequenceDataAdaptor,
    EditMethodParams,
    MethodDataCache,
    TipMapDialog,
    ParserStoredIdentifier,
    ResSchemeDataAdaptor,
    MethodGUIParsing,
    LayoutManager,
    Carrier,
    WashProgDataAdaptor,
    SQLTermsDataAdaptor,
    SQLTermParserInterface,
    LiqHDataCache,
    CarrierDataAdaptor,
    RackDataAdaptor,
    IntfDevice,
    IntfArmDevice,
    IntfPipDevice,
    IntfBalanceDevice,
    IntfBCReaderDevice,
    IntfSwitchDevice,
    IntfSensorDevice,
    IntfMixerDevice,
    IntfMemoryDevice,
    IntfVarRediMotorDevice,
    IntfMotorDevice,
    IntfThermostatDevice,
    IntfTriggerDevice,
    IntfMotionDevice,
    IntfGripDevice,
    SubstanceSetDataAdaptor,
    SubstanceDataDataAdaptor,
    DialogInputRunStepEditor,
    DeviceSettingsManager,
    DeviceTypeDictionary,
    DeviceDataCache,
    VolcorrDataAdaptor,
    Streamable,
    TipTypeDataAdaptor,
    DesignModuleSettingFinder,
    Liquids,
    VariableCompositeSetting,
    CustomEditFunctionParams,
    StringUtilities,
    MultiPageDialog,
    IntfPHMeterDevice,
    IntfPipPumpDevice;

{ TEditFunctions }

constructor TEditFunctions.Create(aCheckMethodParams: boolean);
begin
    inherited Create;
    fCheckMethodParams := aCheckMethodParams;
end;

function TEditFunctions.EditCarrier(var vValue: string): boolean;
var
    xCarrierParam: TSimpleParameter;
    xMainForm: TfrmMultiPageDialog;
begin
    xCarrierParam := TSimpleParameter.Create(vValue);
    try
        xMainForm := TfrmMultiPageDialog.Create(nil);
        try
            result := ShowModalSinglePage(xMainForm, '', CreateCarrierMode(xMainForm, false, '',
                xCarrierParam));
        finally
            FreeAndNil(xMainForm);
        end;
        if result then
            vValue := xCarrierParam.Value;
    finally
        FreeAndNil(xCarrierParam);
    end;
end;

function TEditFunctions.EditCarrierSlot(var vCarrierName: string; var vCarrierSlot: integer): boolean;
var
    xCarrierParam: TSimpleParameter;
    xCarrierSlotParam: TSimpleParameter;
    xMainForm: TfrmMultiPageDialog;
begin
    xCarrierParam := TSimpleParameter.Create(vCarrierName);
    xCarrierSlotParam := TSimpleParameter.Create(IntToStr(vCarrierSlot));
    try
        xMainForm := TfrmMultiPageDialog.Create(nil);
        try
            result := ShowModalSinglePage(xMainForm, '', CreateCarrierSlotMode(xMainForm, false, '',
                xCarrierParam, xCarrierSlotParam));
        finally
            FreeAndNil(xMainForm);
        end;

        if result then
        begin
            vCarrierName := xCarrierParam.Value;
            vCarrierSlot := StrToIntDef(xCarrierSlotParam.Value, 0);
        end;
    finally
        FreeAndNil(xCarrierParam);
    end;
end;

function TEditFunctions.EditRack(var vValue: string): boolean;
var
    xRackParam: TSimpleParameter;
    xMainForm: TfrmMultiPageDialog;
begin
    xRackParam := TSimpleParameter.Create(vValue);
    try
        xMainForm := TfrmMultiPageDialog.Create(nil);
        try
            result := ShowModalSinglePage(xMainForm, '', CreateRackMode(xMainForm, false, '', xRackParam));
        finally
            FreeAndNil(xMainForm);
        end;

        if result then
            vValue := xRackParam.Value;
    finally
        FreeAndNil(xRackParam);
    end;
end;

function TEditFunctions.EditRackDoublePos(var vRackName: string; var vFirstPos, vLastPos: string;
    const aTitle: string): boolean;
var
    xRackParam, xFirstPosParam, xLastPosParam: TSimpleParameter;
    xMainForm: TfrmMultiPageDialog;
begin
    xRackParam := TSimpleParameter.Create(vRackName);
    xFirstPosParam := TSimpleParameter.Create(vFirstPos);
    xLastPosParam := TSimpleParameter.Create(vLastPos);
    try
        xMainForm := TfrmMultiPageDialog.Create(nil);
        try
            result := ShowModalSinglePage(xMainForm, aTitle, CreateDoublePosMode(xMainForm, false, '',
                xRackParam, xFirstPosParam, xLastPosParam));
        finally
            FreeAndNil(xMainForm);
        end;

        if result then
        begin
            vRackName := xRackParam.Value;
            vFirstPos := xFirstPosParam.Value;
            vLastPos := xLastPosParam.Value;
        end;
    finally
        FreeAndNil(xRackParam);
        FreeAndNil(xFirstPosParam);
        FreeAndNil(xLastPosParam);
    end;
end;

function TEditFunctions.EditRackSinglePos(var vRackName, vPos: string; const aTitle: string): boolean;
var
    xRackParam, xFirstPosParam: TSimpleParameter;
    xMainForm: TfrmMultiPageDialog;
begin
    xRackParam := TSimpleParameter.Create(vRackName);
    xFirstPosParam := TSimpleParameter.Create(vPos);
    try
        xMainForm := TfrmMultiPageDialog.Create(nil);
        try
            result := ShowModalSinglePage(xMainForm, aTitle, CreateSinglePosMode(xMainForm, false, '',
                xRackParam, xFirstPosParam));
        finally
            FreeAndNil(xMainForm);
        end;

        if result then
        begin
            vRackName := xRackParam.Value;
            vPos := xFirstPosParam.Value;
        end;
    finally
        FreeAndNil(xRackParam);
        FreeAndNil(xFirstPosParam);
    end;
end;

function TEditFunctions.EditRackPosArrayGetNoOfPos(aDummy: integer): integer;
begin
    EXIT(fEditRackPosArrayNoOfPos);
end;

function TEditFunctions.EditRackPosArray(aCheckValues: boolean; aParams: TList<TSimpleParameter>;
    const aTitle: string; aLineCountFixed: boolean; const aNoOfPos: integer): boolean;
var
    x: integer;
    xData: TArray<TEditableParameterEditData>;
    xMainForm: TfrmMultiPageDialog;
begin
    SetLength(xData, aParams.Count);
    for x := 0 to aParams.Count - 1 do
    begin
        xData[x].Param := aParams[x];
        xData[x].Enabled := aParams[x].ColumnEnabled;
    end;

    if aLineCountFixed then
        fEditRackPosArrayNoOfPos := aNoOfPos
    else
        fEditRackPosArrayNoOfPos := 0;

    // Modal anzeigen
    xMainForm := TfrmMultiPageDialog.Create(nil);
    try
        EXIT(ShowModalSinglePage(xMainForm, aTitle, CreateRackPosArrayPage(xMainForm, aCheckValues, xData,
            EditRackPosArrayGetNoOfPos, 0, true)));
    finally
        FreeAndNil(xMainForm);
    end;
end;

function TEditFunctions.ReadDeviceNamesForType(const aModuleID: TModuleID): TArray<string>;
begin
    result := TDesignModuleSettingFinder.ReadDeviceNamesForType(aModuleID);
end;

procedure TEditFunctions.Parameter_OnAddEditFunctions(aSender: TObject);
var
    xParam: TCustomSetting;
begin
    if not(aSender is TCustomSetting) then
        EXIT;

    xParam := aSender as TCustomSetting;
    case (xParam.EditType) of
        cpeBalance:
            xParam.OnGetPickList := self.Balance_GetPickList;
        cpeBCReaderName:
            xParam.OnGetPickList := self.BCReaderDevice_GetPickList;
        cpeBit:
            xParam.OnGetPickList := self.Bit_GetPickList;
        cpeCarrierName:
            begin
                xParam.OnGetPickList := self.CarrierName_GetPickList;
                xParam.OnEditFunction := self.CarrierName_EditValue;
            end;
        cpeCarrierSlotNo:
            begin
                xParam.OnGetPickList := self.CarrierSlotNo_GetPickList;
                xParam.OnEditFunction := self.CarrierSlotNo_EditValue;
            end;
        cpeCarrierNameAndSlot:
            begin
                xParam.OnGetPickList := self.CarrierName_GetPickList;
                xParam.OnEditFunction := self.CarrierSlotNo_EditValue;
            end;
        cpeCarrierSlotRotation:
            xParam.OnGetPickList := self.CarrierSlotRotation_GetPickList;
        cpeCarrierTypeName:
            xParam.OnGetPickList := self.CarrierTypeName_GetPickList;
        cpeColor:
            xParam.OnEditFunction := self.Color_EditValue;
        cpeDeviceName:
            xParam.OnGetPickList := self.DeviceName_GetPickList;
        cpeDialogInput:
            xParam.OnEditFunction := self.DialogInput_EditValue;
        cpeDiluentName:
            xParam.OnGetPickList := self.DiluentName_GetPickList;
        cpeDilNameArrayRefToRackPos:
            begin
                xParam.OnGetPickList := self.DiluentName_GetPickList;
                xParam.OnEditFunction := self.ReferringToRackPosArray_EditValue;
            end;
        cpeDiluentRack:
            begin
                xParam.OnGetPickList := self.SystemRack_GetPickList;
                xParam.OnEditFunction := self.RackSinglePos_EditValue;
            end;
        cpeDilRackArrayRefToRackPos:
            begin
                xParam.OnGetPickList := self.SystemRack_GetPickList;
                xParam.OnEditFunction := self.ReferringToRackPosArray_EditValue;
            end;
        cpeFileName:
            xParam.OnEditFunction := self.FileName_EditValue;
        cpeGripDevice:
            xParam.OnGetPickList := self.GripDevice_GetPickList;
        cpeGripperArmName:
            xParam.OnGetPickList := self.GripperArmName_GetPickList;
        cpeGroupID:
            xParam.OnGetPickList := self.GroupID_GetPickList;
        cpeImportDefName:
            xParam.OnGetPickList := self.ImportDefName_GetPickList;
        cpeLiqParam:
            begin
                xParam.OnGetPickList := self.LiqParam_GetPickList;
                xParam.OnOpenFunction := self.LiqParam_OpenValue;
            end;
        cpeLiqParamArrayRefToRackPos:
            begin
                xParam.OnGetPickList := self.LiqParam_GetPickList;
                xParam.OnEditFunction := self.ReferringToRackPosArray_EditValue;
                xParam.OnOpenFunction := self.LiqParam_OpenValue;
            end;
        cpeMemoryDevice:
            xParam.OnGetPickList := self.MemoryDevice_GetPickList;
        cpeMethodName:
            begin
                xParam.OnGetPickList := self.MethodName_GetPickList;
                xParam.OnOpenFunction := self.MethodName_OpenValue;
            end;
        cpeMethodParams:
            begin
                xParam.OnEditFunction := self.MethodParams_EditValue;
            end;
        cpeLocationBasedMotionDevice:
            xParam.OnGetPickList := self.LocationBasedMotionDevice_GetPickList;
        cpeMotorDevice:
            xParam.OnGetPickList := self.MotorDevice_GetPickList;
        cpePathName:
            xParam.OnEditFunction := self.PathName_EditValue;
        cpePHDeviceName:
            xParam.OnGetPickList := self.PHMeterDevice_GetPickList;
        cpePipDeviceName:
            xParam.OnGetPickList := self.PipDevice_GetPickList;
        cpePosOfDoubleRackPos:
            begin
                xParam.OnEditFunction := self.RackDoublePos_EditValue;
            end;
        cpePosOfSingleRackPos:
            begin
                xParam.OnEditFunction := self.RackSinglePos_EditValue;
            end;
        cpePosOfRackPosArray:
            begin
                xParam.OnEditFunction := self.RackPosArray_EditValue;
            end;
        cpePosArrayRefToRackPos:
            xParam.OnEditFunction := self.ReferringToRackPosArray_EditValue;
        cpePumpDeviceName:
            xParam.OnGetPickList := self.PumpDevice_GetPickList;
        cpeRack:
            begin
                xParam.OnGetPickList := self.Rack_GetPickList;
                xParam.OnEditFunction := self.Rack_EditValue;
            end;
        cpeRackOfDoubleRackPos:
            begin
                xParam.OnGetPickList := self.Rack_GetPickList;
                xParam.OnEditFunction := self.RackDoublePos_EditValue;
            end;
        cpeRackOfSingleRackPos:
            begin
                xParam.OnGetPickList := self.Rack_GetPickList;
                xParam.OnEditFunction := self.RackSinglePos_EditValue;
            end;
        cpeRackOfRackPosArray:
            begin
                xParam.OnGetPickList := self.Rack_GetPickList;
                xParam.OnEditFunction := self.RackPosArray_EditValue;
            end;
        cpeRackArrayRefToRackPos:
            begin
                xParam.OnGetPickList := self.Rack_GetPickList;
                xParam.OnEditFunction := self.ReferringToRackPosArray_EditValue;
            end;
        cpeRackTypeName:
            xParam.OnGetPickList := self.RackTypeName_GetPickList;
        cpeResourceSchemeName:
            begin
                xParam.OnGetPickList := self.ResourceSchemeName_GetPickList;
            end;
        cpeSensorDevice:
            xParam.OnGetPickList := self.SensorDevice_GetPickList;
        cpeSequenceName:
            begin
                xParam.OnGetPickList := self.SequenceName_GetPickList;
                xParam.OnOpenFunction := self.SequenceName_OpenValue;
            end;
        cpeShakerDevice:
            xParam.OnGetPickList := self.ShakerDevice_GetPickList;
        cpeSQLTermName:
            begin
                xParam.OnGetPickList := self.SQLTermName_GetPickList;
                xParam.OnOpenFunction := self.SQLTermName_OpenValue;
            end;
        cpeSQLTermParams:
            xParam.OnEditFunction := self.SQLTermParams_EditValue;
        cpeSubstanceID:
            xParam.OnGetPickList := self.SubstanceID_GetPickList;
        cpeSubstIDArrayRefToRackPos:
            begin
                xParam.OnGetPickList := self.SubstanceID_GetPickList;
                xParam.OnEditFunction := self.ReferringToRackPosArray_EditValue;
            end;
        cpeSubstanceSet:
            xParam.OnGetPickList := self.SubstanceSet_GetPickList;
        cpeSwitchDevice:
            xParam.OnGetPickList := self.SwitchDevice_GetPickList;
        cpeTernary:
            xParam.OnGetPickList := self.YesNo_GetPickList;
        cpeThermostat:
            xParam.OnGetPickList := self.Thermostat_GetPickList;
        cpeTimeUnit:
            xParam.OnGetPickList := self.TimeUnit_GetPickList;
        cpeTipMap:
            xParam.OnEditFunction := self.TipMap_EditValue;
        cpeTipArrayRefToRackPos:
            xParam.OnEditFunction := self.ReferringToRackPosArray_EditValue;
        cpeTipType:
            xParam.OnGetPickList := self.TipType_GetPickList;
        cpeToolRack:
            begin
                xParam.OnGetPickList := self.ToolRack_GetPickList;
                xParam.OnEditFunction := self.ToolRack_EditValue;
            end;
        cpeTriggerDevice:
            xParam.OnGetPickList := self.TriggerDevice_GetPickList;
        cpeVolMotorDevice:
            xParam.OnGetPickList := self.VolMotorDevice_GetPickList;
        cpeVolumeCorrectionCurve:
            xParam.OnGetPickList := self.VolumeCorrectionCurve_GetPickList;
        cpeVolumeArrayRefToRackPos:
            xParam.OnEditFunction := self.ReferringToRackPosArray_EditValue;
        cpeWashProgram:
            begin
                xParam.OnGetPickList := self.WashProgram_GetPickList;
                xParam.OnOpenFunction := self.WashProgram_OpenValue;
            end;
        cpeYesNo:
            xParam.OnGetPickList := self.YesNo_GetPickList;
        cpeYesNoDelete:
            xParam.OnGetPickList := self.YesNoDelete_GetPickList;
    end;
end;

function TEditFunctions.Balance_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := ReadDeviceNamesForType(IBalanceDevice);
end;

function TEditFunctions.BCReaderDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := ReadDeviceNamesForType(IBCReaderDevice);
end;

function TEditFunctions.Bit_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent): TArray<string>;
begin
    SetLength(result, 2);
    result[0] := TCustomSetting_Bit.GetYesString;
    result[1] := TCustomSetting_Bit.GetNoString;
end;

procedure TEditFunctions.CarrierName_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TSingleValueEditFunctionParams;
    xValue: string;
begin
    ASSERT(aParams is TSingleValueEditFunctionParams);
    xParams := (aParams as TSingleValueEditFunctionParams);
    xValue := xParams.SingleParam.Param.Value;

    vCancel := not self.EditCarrier(xValue);

    if vCancel then
        EXIT;
    xParams.SingleParam.NewValue := xValue;
end;

function TEditFunctions.CarrierName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    SetLength(result, 0);
    if self.IsCurrentLayoutEmpty then
        EXIT;
    result := self.GetCurrentLayout.GetCarrierNames();
end;

procedure TEditFunctions.CarrierSlotNo_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xCarrierName: string;
    xCarrierSlot: integer;
    xParams: TSlotStructEditFunctionParams;
begin
    ASSERT(aParams is TSlotStructEditFunctionParams);
    xParams := (aParams as TSlotStructEditFunctionParams);
    xCarrierSlot := StrToIntDef(xParams.Slot.Param.Value, 1);
    xCarrierName := xParams.CarrierName.Param.Value;

    vCancel := not self.EditCarrierSlot(xCarrierName, xCarrierSlot);

    if vCancel then
        EXIT;
    xParams.Slot.NewValue := IntToStr(xCarrierSlot);
    xParams.CarrierName.NewValue := xCarrierName;
end;

function TEditFunctions.CarrierSlotNo_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
var
    xSlot: integer;
    xCarrierName: string;
    xCarrier: TCarrier;
    xParams: TCustomSettingEditFunctionParams;
begin
    SetLength(result, 0);

    xParams := aOnCreateParams();
    try
        ASSERT(xParams is TSlotStructEditFunctionParams);
        xCarrierName := (xParams as TSlotStructEditFunctionParams).CarrierName.Param.Value;

        if self.IsCurrentLayoutEmpty() then
            EXIT;
        xCarrier := self.GetCurrentLayout.FindCarrierByName(xCarrierName);
        if not Assigned(xCarrier) then
            EXIT;

        SetLength(result, xCarrier.SlotCount);
        for xSlot := 0 to xCarrier.SlotCount - 1 do
            result[xSlot] := IntToStr(xSlot + 1);
    finally
        FreeAndNil(xParams);
    end;
end;

function TEditFunctions.CarrierSlotRotation_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    // noch nicht Carrier-abhängig:
    SetLength(result, 4);
    result[0] := '0';
    result[1] := '90';
    result[2] := '180';
    result[3] := '270';
end;

function TEditFunctions.CarrierTypeName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
var
    xDA: TCarrierDataAdaptor;
begin
    xDA := TCarrierDataAdaptor.Create();
    try
        result := xDA.ReadAllNames();
    finally
        xDA.Free;
    end;
end;

procedure TEditFunctions.Color_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TSingleValueEditFunctionParams;
    xValue: integer;
    xColorDialog: TColorDialog;
begin
    ASSERT(aParams is TSingleValueEditFunctionParams);
    xParams := (aParams as TSingleValueEditFunctionParams);
    xValue := StrToIntDef(xParams.SingleParam.Param.Value, 0);

    xColorDialog := TColorDialog.Create(nil);
    try
        xColorDialog.Color := xValue;
        vCancel := not xColorDialog.Execute;
        if vCancel then
            EXIT;

        xParams.SingleParam.NewValue := IntToStr(xColorDialog.Color);
    finally
        FreeAndNil(xColorDialog)
    end;
end;

function TEditFunctions.DeviceName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := ReadDeviceNamesForType(IDevice);
end;

procedure TEditFunctions.DialogInput_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TSingleValueEditFunctionParams;
    xValue: string;
    xEditor: TfrmDialogInputRunStepEditor;
begin
    ASSERT(aParams is TSingleValueEditFunctionParams);
    xParams := (aParams as TSingleValueEditFunctionParams);
    xValue := xParams.SingleParam.Param.Value;

    xEditor := TfrmDialogInputRunStepEditor.Create(nil);
    try
        xEditor.Value := xValue;
        vCancel := (xEditor.ShowModal <> mrOK);
        xValue := xEditor.Value;
    finally
        xEditor.Free;
    end;

    if vCancel then
        EXIT;
    xParams.SingleParam.NewValue := xValue;
end;

function TEditFunctions.DiluentName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := TLiquids.Instance.GetAllSystemLiquidNames();
end;

procedure TEditFunctions.FileName_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TSingleValueEditFunctionParams;
    xValue: string;
    xOpenDialog: TOpenDialog;
begin
    ASSERT(aParams is TSingleValueEditFunctionParams);
    xParams := (aParams as TSingleValueEditFunctionParams);
    xValue := xParams.SingleParam.Param.Value;

    xOpenDialog := TOpenDialog.Create(Application);
    try
        xOpenDialog.Options := xOpenDialog.Options + [ofNoChangeDir];
        xOpenDialog.FileName := xValue;
        vCancel := not xOpenDialog.Execute();
        xValue := xOpenDialog.FileName;
    finally
        xOpenDialog.Free;
    end;

    if vCancel then
        EXIT;
    xParams.SingleParam.NewValue := xValue;
end;

function TEditFunctions.GripDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := ReadDeviceNamesForType(IGripDevice);
end;

function TEditFunctions.GripperArmName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := TDesignModuleSettingFinder.ReadGripperArmNames();
end;

function TEditFunctions.GroupID_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    SetLength(result, 1);
    result[0] := STR_GROUPID_NOGROUP;
end;

function TEditFunctions.ImportDefName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
var
    xParam: TCustomSetting_ImportDefName;
    xParams: TCustomSettingEditFunctionParams;
begin
    SetLength(result, 0);

    xParams := aOnCreateParams();
    try
        ASSERT(xParams is TSingleValueEditFunctionParams);
        if not((xParams as TSingleValueEditFunctionParams)
            .SingleParam.Param is TCustomSetting_ImportDefName) then
            EXIT;

        xParam := ((xParams as TSingleValueEditFunctionParams)
            .SingleParam.Param as TCustomSetting_ImportDefName);
        result := TImportDefDataAdaptor.ReadAllDefNames(xParam.ImportDefMode);
    finally
        FreeAndNil(xParams);
    end;
end;

function TEditFunctions.LiqParam_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := TLiqHDataCache.Instance.GetLiqHNames();
end;

procedure TEditFunctions.LiqParam_OpenValue(aSetting: TObject);
begin
    // Dummy
end;

function TEditFunctions.MemoryDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := ReadDeviceNamesForType(IMemoryDevice);
end;

function TEditFunctions.MethodName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := TMethodDataCache.Instance.GetMethodNames();
end;

procedure TEditFunctions.MethodName_OpenValue(aSetting: TObject);
begin
    // Dummy
end;

procedure TEditFunctions.MethodParams_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xInputParams, xResultParams: TKeyValueParamArray;
    xParams: TMethodParamsEditFunctionParams;
begin
    ASSERT(aParams is TMethodParamsEditFunctionParams);
    xParams := (aParams as TMethodParamsEditFunctionParams);
    xInputParams := (xParams.MethodParamsParam.Param as TCustomSetting_MethodParams).ValuesAsKeyValueArray;

    vCancel := not TEditMethodParams.Editor_EditMethodParameters(xParams.MethodNameParam.Param.Value,
        xInputParams, xResultParams, self, fCheckMethodParams);

    if vCancel then
        EXIT;
    (xParams.MethodParamsParam.Param as TCustomSetting_MethodParams).ValuesAsKeyValueArray := xResultParams;
    xParams.MethodParamsParam.NewValue := xParams.MethodParamsParam.Param.Value;
end;

function TEditFunctions.LocationBasedMotionDevice_GetPickList(aOnCreateParams
    : TCustomSettingCreateEditParamsEvent): TArray<string>;
begin
    result := ReadDeviceNamesForType(ILocationBasedMotionDevice);
end;

function TEditFunctions.MotorDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := ReadDeviceNamesForType(IMotorDevice);
end;

procedure TEditFunctions.PathName_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TSingleValueEditFunctionParams;
    xValue: string;
begin
    ASSERT(aParams is TSingleValueEditFunctionParams);
    xParams := (aParams as TSingleValueEditFunctionParams);
    xValue := xParams.SingleParam.Param.Value;

    vCancel := not TDialogUtils.SelectDirectory('', '', xValue);

    if vCancel then
        EXIT;
    xParams.SingleParam.NewValue := xValue;
end;

function TEditFunctions.PHMeterDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := TDeviceSettingsManager.Instance.ReadNamesForType(IPHMeterDevice);
end;

function TEditFunctions.PipDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := TDeviceSettingsManager.Instance.ReadNamesForType(IPipDevice);
end;

function TEditFunctions.PumpDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := TDeviceSettingsManager.Instance.ReadNamesForType(IPipPumpDevice);
end;

function TEditFunctions.Rack_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    SetLength(result, 0);
    if self.IsCurrentLayoutEmpty() then
        EXIT;
    result := self.GetCurrentLayout.GetRackNames();
end;

procedure TEditFunctions.Rack_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TRackEditFunctionParams;
    xRackName: string;
begin
    ASSERT(aParams is TRackEditFunctionParams);
    xParams := (aParams as TRackEditFunctionParams);
    xRackName := xParams.RackName.Param.Value;

    vCancel := not self.EditRack(xRackName);

    if vCancel then
        EXIT;
    xParams.RackName.NewValue := xRackName;
end;

procedure TEditFunctions.RackSinglePos_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TRackSinglePosEditFunctionParams;
    xRackName: string;
    xPos: string;
begin
    ASSERT(aParams is TRackSinglePosEditFunctionParams);
    xParams := (aParams as TRackSinglePosEditFunctionParams);
    xRackName := xParams.RackName.Param.Value;
    xPos := xParams.Pos.Param.Value;

    vCancel := not self.EditRackSinglePos(xRackName, xPos, xParams.EditTitle);

    if vCancel then
        EXIT;
    xParams.RackName.NewValue := xRackName;
    xParams.Pos.NewValue := xPos;
end;

procedure TEditFunctions.AddRackAndPos(aEditParams: TObjectList<TSimpleParameter>;
    aRackNameParam, aPosParam: TCustomSetting; aColumnEnabled: boolean);
begin
    // Rack und Position hinzufügen
    aEditParams.Add(TSimpleParameter.Create(aRackNameParam.Value, aColumnEnabled, aRackNameParam.Description,
        120, scdRackName, aRackNameParam.GetPickList()));
    aEditParams.Add(TSimpleParameter.Create(aPosParam.Value, aColumnEnabled, aPosParam.Description, 60,
        scdPosition, nil));
end;

procedure TEditFunctions.RackPosArray_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TRackPosArrayEditFunctionParams;
    xEditParams: TObjectList<TSimpleParameter>;
    xPosCount: integer;
begin
    ASSERT(aParams is TRackPosArrayEditFunctionParams);
    xParams := (aParams as TRackPosArrayEditFunctionParams);

    if self.IsCurrentLayoutEmpty then
        EXIT;

    xPosCount := self.GetRackPositionArrayLength(xParams.RackNames.Param.Value,
        xParams.Positions.Param.Value);

    xEditParams := TObjectList<TSimpleParameter>.Create;
    try
        AddRackAndPos(xEditParams, xParams.RackNames.Param, xParams.Positions.Param, true);

        vCancel := not self.EditRackPosArray(false, xEditParams, xParams.EditTitle, false, xPosCount);
        if vCancel then
            EXIT;

        xParams.RackNames.NewValue := xEditParams[0].Value;
        xParams.Positions.NewValue := xEditParams[1].Value;
    finally
        FreeAndNil(xEditParams);
    end;
end;

procedure TEditFunctions.RackDoublePos_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TRackDoublePosEditFunctionParams;
    xRackName: string;
    xFirstPos, xLastPos: string;
begin
    ASSERT(aParams is TRackDoublePosEditFunctionParams);
    xParams := (aParams as TRackDoublePosEditFunctionParams);
    xRackName := xParams.RackName.Param.Value;
    xFirstPos := xParams.FirstPos.Param.Value;
    xLastPos := xParams.LastPos.Param.Value;

    vCancel := not self.EditRackDoublePos(xRackName, xFirstPos, xLastPos, xParams.EditTitle);
    if vCancel then
        EXIT;

    xParams.RackName.NewValue := xRackName;
    xParams.FirstPos.NewValue := xFirstPos;
    xParams.LastPos.NewValue := xLastPos;
end;

function TEditFunctions.RackTypeName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
var
    xDA: TRackDataAdaptor;
begin
    xDA := TRackDataAdaptor.Create();
    try
        result := xDA.ReadAllNames();
    finally
        xDA.Free;
    end;
end;

function GetArrayDataType(aEditType: TCustomSettingEditType): TEditableParameterDataType;
begin
    case aEditType of
        cpeRackArrayRefToRackPos:
            EXIT(scdRackName);
        cpePosArrayRefToRackPos:
            EXIT(scdPosition);
        else
            EXIT(scdOther);
    end;
end;

procedure TEditFunctions.ReferringToRackPosArray_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TReferringToRackPosArrayEditFunctionParams;
    x, xPosCount: integer;
    xEditParams: TObjectList<TSimpleParameter>;
    xRefParam: TCustomSetting_ReferringToRackPosArray;
    xEditIndexArray: TArray<integer>;
begin
    ASSERT(aParams is TReferringToRackPosArrayEditFunctionParams);
    xParams := (aParams as TReferringToRackPosArrayEditFunctionParams);

    if self.IsCurrentLayoutEmpty then
        EXIT;

    // Anzahl der Positionen ermitteln
    xPosCount := self.GetRackPositionArrayLength(xParams.RackNames.Param.Value,
        xParams.Positions.Param.Value);

    xEditParams := TObjectList<TSimpleParameter>.Create;
    try
        SetLength(xEditIndexArray, xParams.Count);
        for x := 0 to xParams.Count - 1 do
        begin
            if (x = xParams.RackPosAtIndex) then // Rack und Position hinzufügen
                AddRackAndPos(xEditParams, xParams.RackNames.Param, xParams.Positions.Param, false);

            xRefParam := xParams[x].Param as TCustomSetting_ReferringToRackPosArray;
            xEditIndexArray[x] := xEditParams.Add(TSimpleParameter.Create(xRefParam.Value, true,
                xRefParam.ColumnDescription, xRefParam.ColumnWidth,
                GetArrayDataType(xParams[x].Param.EditType), xRefParam.GetPickList()));
        end;

        if (xParams.RackPosAtIndex >= xParams.Count) then // Rack und Position hinzufügen
            AddRackAndPos(xEditParams, xParams.RackNames.Param, xParams.Positions.Param, false);

        vCancel := not self.EditRackPosArray(false, xEditParams, xParams.EditTitle, false, xPosCount);
        if vCancel then
            EXIT;

        for x := 0 to xParams.Count - 1 do
            xParams[x].NewValue := xEditParams[xEditIndexArray[x]].Value;
    finally
        FreeAndNil(xEditParams);
    end;
end;

function TEditFunctions.ResourceSchemeName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
var
    xDA: TResSchemeDataAdaptor;
begin
    xDA := TResSchemeDataAdaptor.Create;
    try
        EXIT(xDA.ReadSchemeIDsWithResourceNames
            (TCustomSetting_ResourceSchemeName.cValueDescriptionDelimiter, ','));
    finally
        xDA.Free;
    end;
end;

function TEditFunctions.SensorDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := ReadDeviceNamesForType(ISensorDevice);
end;

function TEditFunctions.SequenceName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := TSequenceDataAdaptor.InstReadAllNames();
end;

procedure TEditFunctions.SequenceName_OpenValue(aSetting: TObject);
begin
    // Dummy
end;

function TEditFunctions.ShakerDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := ReadDeviceNamesForType(IMixerDevice);
end;

function TEditFunctions.SQLTermName_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
var
    xDA: TSQLTermsDataAdaptor;
begin
    xDA := TSQLTermsDataAdaptor.Create;
    try
        result := xDA.ReadAllNames();
    finally
        xDA.Free;
    end;
end;

procedure TEditFunctions.SQLTermName_OpenValue(aSetting: TObject);
begin
    // Dummy
end;

procedure TEditFunctions.SQLTermParams_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParamStr: string;
    xParams: TSQLArgsEditFunctionParams;
begin
    ASSERT(aParams is TSQLArgsEditFunctionParams);
    xParams := (aParams as TSQLArgsEditFunctionParams);
    xParamStr := xParams.ArgsParam.Param.Value;

    vCancel := not gmEditSQLTermParameters(xParams.TermNameParam.Param.Value, xParamStr);

    if vCancel then
        EXIT;
    xParams.ArgsParam.NewValue := xParamStr;
end;

function TEditFunctions.SubstanceID_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    EXIT(TSubstanceDataDataAdaptor.InstReadAllNames);
end;

function TEditFunctions.SubstanceSet_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
var
    xDataAdaptor: TSubstanceSetDataAdaptor;
begin
    xDataAdaptor := TSubstanceSetDataAdaptor.Create;
    try
        result := xDataAdaptor.ReadAllNames();
    finally
        xDataAdaptor.Free;
    end;
end;

function TEditFunctions.SwitchDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := TDesignModuleSettingFinder.GetSwitchDeviceNames();
end;

function TEditFunctions.SystemRack_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
var
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        xList.Add('SYSTEM');
        xList.AddRange(self.Rack_GetPickList(aOnCreateParams));
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

function TEditFunctions.Thermostat_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := ReadDeviceNamesForType(IThermostatDevice);
end;

function TEditFunctions.TipType_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := TTipTypeDataAdaptor.InstReadAllNames();
end;

function TEditFunctions.TimeUnit_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    SetLength(result, 4);
    result[0] := 'msec';
    result[1] := 'sec';
    result[2] := 'min';
    result[3] := 'h';
end;

procedure TEditFunctions.TipMap_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TPipDeviceAndUsedTipsEditFunctionParams;
    xValue: integer;
    xPipDevicName: string;
begin
    ASSERT(aParams is TPipDeviceAndUsedTipsEditFunctionParams);
    xParams := (aParams as TPipDeviceAndUsedTipsEditFunctionParams);

    xPipDevicName := xParams.PipDeviceName.Param.Value;
    if xPipDevicName = '' then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Please enter PipDevice first!',
            'Bitte zuerst Pipettierdevice eingeben!', ['Error']), TLanguageString.
            Read('Warning', 'Warnung'), mb_OK);
        EXIT;
    end;

    vCancel := not TfrmTipMapDialog.EditTipMap(xPipDevicName,
        TDesignModuleSettingFinder.ReadTipCount(xPipDevicName), StrToIntDef(xParams.UsedTips.Param.Value,
        0), xValue);

    if vCancel then
        EXIT;
    xParams.UsedTips.NewValue := IntToStr(xValue);
end;

procedure TEditFunctions.ToolRack_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
begin
    self.Rack_EditValue(aParams, vCancel);
end;

function TEditFunctions.ToolRack_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := self.Rack_GetPickList(aOnCreateParams);
end;

function TEditFunctions.TriggerDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := ReadDeviceNamesForType(ITriggerDevice);
end;

function TEditFunctions.VolMotorDevice_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := ReadDeviceNamesForType(IVarRediMotorDevice);
end;

function TEditFunctions.VolumeCorrectionCurve_GetPickList(aOnCreateParams
    : TCustomSettingCreateEditParamsEvent): TArray<string>;
var
    xDataAdaptor: TVolCorrDataAdaptor;
begin
    xDataAdaptor := TVolCorrDataAdaptor.Create;
    try
        result := xDataAdaptor.ReadAllNames();
    finally
        xDataAdaptor.Free;
    end;
end;

function TEditFunctions.WashProgram_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
var
    xDataAdaptor: TWashProgDataAdaptor;
begin
    xDataAdaptor := TWashProgDataAdaptor.Create;
    try
        result := xDataAdaptor.ReadAllNames();
    finally
        xDataAdaptor.Free;
    end;
end;

procedure TEditFunctions.WashProgram_OpenValue(aSetting: TObject);
begin
    // Dummy
end;

function TEditFunctions.YesNo_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    result := TMethodGUIParser.GetBinaryStrings();
end;

function TEditFunctions.YesNoDelete_GetPickList(aOnCreateParams: TCustomSettingCreateEditParamsEvent)
    : TArray<string>;
begin
    SetLength(result, 3);
    result[0] := STR_YES;
    result[1] := STR_NO;
    result[2] := TCustomSetting_YesNoDelete.cDeleteText;
end;


end.
