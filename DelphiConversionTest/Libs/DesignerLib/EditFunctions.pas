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
  11.02.13 wl  TCustomSetting_YesNoDelete        TN6078   neu
  -------------------------------------------------------------------------------------------------- }

unit EditFunctions;


interface


uses

    GeneralTypes,
    CustomSetting,
    ModuleSettings;

type
    TEditFunctions = class
    private
        function ReadDeviceNamesForType(const aModuleID: TModuleID): TStringArray;
    public
        procedure Parameter_OnAddEditFunctions(aSender: TObject);
        //
        function Balance_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function BCReaderDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function Bit_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function CarrierName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure CarrierName_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function CarrierSlotNo_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure CarrierSlotNo_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function CarrierSlotRotation_GetPickList(const aParams: TCustomSettingEditFunctionParams)
            : TStringArray;
        function CarrierTypeName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure Color_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        function DeviceName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure DialogInput_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function DiluentName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure FileName_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        function GripDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function GripperArmName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function GroupID_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function ImportDefName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function LiqParam_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure LiqParam_OpenValue(const aParams: TCustomSettingEditFunctionParams);
        function MemoryDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function MethodName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure MethodName_OpenValue(const aParams: TCustomSettingEditFunctionParams);
        procedure MethodParams_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function LocationBasedMotionDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams)
            : TStringArray;
        function MotorDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure PathName_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        function PHMeterDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function PipDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function PumpDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function Rack_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure Rack_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        procedure RackSinglePos_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        procedure RackSinglePosArray_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        procedure RackDoublePos_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function RackTypeName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function ResourceSchemeName_GetPickList(const aParams: TCustomSettingEditFunctionParams)
            : TStringArray;
        function SensorDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function SequenceName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure SequenceName_OpenValue(const aParams: TCustomSettingEditFunctionParams);
        function ShakerDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function SQLTermName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure SQLTermName_OpenValue(const aParams: TCustomSettingEditFunctionParams);
        procedure SQLTermParams_EditValue(const aParams: TCustomSettingEditFunctionParams;
            var vCancel: boolean);
        function SubstanceID_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function SubstanceSet_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function SwitchDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function SystemRack_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function TimeUnit_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function TriggerDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function Thermostat_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure TipMap_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        function TipType_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure ToolRack_EditValue(const aParams: TCustomSettingEditFunctionParams; var vCancel: boolean);
        function ToolRack_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function VolumeCorrectionCurve_GetPickList(const aParams: TCustomSettingEditFunctionParams)
            : TStringArray;
        function VolMotorDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function WashProgram_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        procedure WashProgram_OpenValue(const aParams: TCustomSettingEditFunctionParams);
        function YesNo_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
        function YesNoDelete_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
    end;


implementation


uses
    Controls,
    SysUtils,
    Dialogs,
    Forms,
    Windows,
    ListClasses,
    DialogUtils,
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
    ZADesignLayoutManager,
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
    ZADesignMain,
    VolcorrDataAdaptor,
    Streamable,
    TipTypeDataAdaptor,
    DesignModuleSettingFinder,
    ViewItem,
    Liquids,
    VariableCompositeSetting,
    LayoutElementSelectDialog,
    CustomEditFunctionParams,
    StringUtilities,
    ViewItemsWorkflow,
    IntfPHMeterDevice,
    IntfPipPumpDevice;

{ TEditFunctions }

function TEditFunctions.ReadDeviceNamesForType(const aModuleID: TModuleID): TStringArray;
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
        cpePosOfSingleRackPosArray:
            begin
                xParam.OnEditFunction := self.RackSinglePosArray_EditValue;
            end;
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
        cpeRackOfSingleRackPosArray:
            begin
                xParam.OnGetPickList := self.Rack_GetPickList;
                xParam.OnEditFunction := self.RackSinglePosArray_EditValue;
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
        cpeSubstanceSet:
            xParam.OnGetPickList := self.SubstanceSet_GetPickList;
        cpeSwitchDevice:
            xParam.OnGetPickList := self.SwitchDevice_GetPickList;
        cpeSystemRack:
            begin
                xParam.OnGetPickList := self.SystemRack_GetPickList;
                xParam.OnEditFunction := self.RackSinglePos_EditValue;
            end;
        cpeTernary:
            xParam.OnGetPickList := self.YesNo_GetPickList;
        cpeThermostat:
            xParam.OnGetPickList := self.Thermostat_GetPickList;
        cpeTimeUnit:
            xParam.OnGetPickList := self.TimeUnit_GetPickList;
        cpeTipMap:
            xParam.OnEditFunction := self.TipMap_EditValue;
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

function TEditFunctions.Balance_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    result := ReadDeviceNamesForType(IBalanceDevice);
end;

function TEditFunctions.BCReaderDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    result := ReadDeviceNamesForType(IBCReaderDevice);
end;

function TEditFunctions.Bit_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    SetLength(result, 2);
    result[0] := TCustomSetting_Bit.GetYesString;
    result[1] := TCustomSetting_Bit.GetNoString;
end;

function TEditFunctions.CarrierName_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    SetLength(result, 0);
    if TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;
    result := TZADesignLayoutManager.Instance.CurrentLayout.GetCarrierNames();
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

    if TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;
    vCancel := TfrmLayoutElementSelectDialog.SelectCarrierModal
        (TZADesignLayoutManager.Instance.CurrentLayout.Name, xValue) <> mrOK;

    if vCancel then
        EXIT;
    xParams.SingleParam.NewValue := xValue;
end;

function TEditFunctions.CarrierSlotNo_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
var
    xSlot: integer;
    xCarrierName: string;
    xCarrier: TCarrier;
    xParams: TSlotStructEditFunctionParams;
begin
    SetLength(result, 0);

    ASSERT(aParams is TSlotStructEditFunctionParams);
    xParams := (aParams as TSlotStructEditFunctionParams);
    xCarrierName := xParams.CarrierName.Param.Value;

    if TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;
    xCarrier := TZADesignLayoutManager.Instance.CurrentLayout.FindCarrierByName(xCarrierName);
    if not Assigned(xCarrier) then
        EXIT;

    SetLength(result, xCarrier.SlotCount);
    for xSlot := 0 to xCarrier.SlotCount - 1 do
        result[xSlot] := IntToStr(xSlot + 1);
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

    if TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;
    vCancel := TfrmLayoutElementSelectDialog.SelectCarrierSlotModal
        (TZADesignLayoutManager.Instance.CurrentLayout.Name, xCarrierName, xCarrierSlot) <> mrOK;

    if vCancel then
        EXIT;
    xParams.Slot.NewValue := IntToStr(xCarrierSlot);
    xParams.CarrierName.NewValue := xCarrierName;
end;

function TEditFunctions.CarrierSlotRotation_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    // noch nicht Carrier-abhängig:
    SetLength(result, 4);
    result[0] := '0';
    result[1] := '90';
    result[2] := '180';
    result[3] := '270';
end;

function TEditFunctions.CarrierTypeName_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
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

function TEditFunctions.DeviceName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
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

function TEditFunctions.DiluentName_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
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

function TEditFunctions.GripDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    result := ReadDeviceNamesForType(IGripDevice);
end;

function TEditFunctions.GripperArmName_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    result := TDesignModuleSettingFinder.ReadGripperArmNames();
end;

function TEditFunctions.GroupID_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    SetLength(result, 1);
    result[0] := STR_GROUPID_NOGROUP;
end;

function TEditFunctions.ImportDefName_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
var
    xParams: TSingleValueEditFunctionParams;
    xParam: TCustomSetting_ImportDefName;
begin
    SetLength(result, 0);

    ASSERT(aParams is TSingleValueEditFunctionParams);
    xParams := (aParams as TSingleValueEditFunctionParams);

    if not(xParams.SingleParam.Param is TCustomSetting_ImportDefName) then
        EXIT;
    xParam := (xParams.SingleParam.Param as TCustomSetting_ImportDefName);

    result := TImportDefDataAdaptor.ReadAllDefNames(xParam.ImportDefMode);
end;

function TEditFunctions.LiqParam_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    result := TLiqHDataCache.Instance.GetLiqHNames();
end;

procedure TEditFunctions.LiqParam_OpenValue(const aParams: TCustomSettingEditFunctionParams);
var
    xParams: TSingleValueEditFunctionParams;
    xValue: string;
begin
    ASSERT(aParams is TSingleValueEditFunctionParams);
    xParams := (aParams as TSingleValueEditFunctionParams);
    xValue := xParams.SingleParam.Param.Value;

    frmEdMain.OpenPipetteParameter(xValue);
end;

function TEditFunctions.MemoryDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    result := ReadDeviceNamesForType(IMemoryDevice);
end;

function TEditFunctions.MethodName_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    result := TMethodDataCache.Instance().GetMethodNames();
end;

procedure TEditFunctions.MethodName_OpenValue(const aParams: TCustomSettingEditFunctionParams);
var
    xParams: TSingleValueEditFunctionParams;
    xValue: string;
begin
    ASSERT(aParams is TSingleValueEditFunctionParams);
    xParams := (aParams as TSingleValueEditFunctionParams);
    xValue := xParams.SingleParam.Param.Value;

    TViewItemsWorkflow.OpenEditForm(xValue, ntMethod, true)
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
        xInputParams, xResultParams);

    if vCancel then
        EXIT;
    (xParams.MethodParamsParam.Param as TCustomSetting_MethodParams).ValuesAsKeyValueArray := xResultParams;
    xParams.MethodParamsParam.NewValue := xParams.MethodParamsParam.Param.Value;
end;

function TEditFunctions.LocationBasedMotionDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    result := ReadDeviceNamesForType(ILocationBasedMotionDevice);
end;

function TEditFunctions.MotorDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
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

function TEditFunctions.PHMeterDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    result := TDeviceSettingsManager.Instance.ReadNamesForType(IPHMeterDevice);
end;

function TEditFunctions.PipDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    result := TDeviceSettingsManager.Instance.ReadNamesForType(IPipDevice);
end;

function TEditFunctions.PumpDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    result := TDeviceSettingsManager.Instance.ReadNamesForType(IPipPumpDevice);
end;

function TEditFunctions.Rack_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    if TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;
    result := TZADesignLayoutManager.Instance.CurrentLayout.GetRackNames();
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

    if TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;
    vCancel := TfrmLayoutElementSelectDialog.SelectRackModal
        (TZADesignLayoutManager.Instance.CurrentLayout.Name, xRackName) <> mrOK;

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

    if TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;
    vCancel := TfrmLayoutElementSelectDialog.SelectRackSinglePosModal
        (TZADesignLayoutManager.Instance.CurrentLayout.Name, xRackName, xPos, xParams.EditTitle) <> mrOK;

    if vCancel then
        EXIT;
    xParams.RackName.NewValue := xRackName;
    xParams.Pos.NewValue := xPos;
end;

procedure TEditFunctions.RackSinglePosArray_EditValue(const aParams: TCustomSettingEditFunctionParams;
    var vCancel: boolean);
var
    xParams: TSingleRackPosArrayEditFunctionParams;
    xRackName: string;
    xPos: string;
begin
    ASSERT(aParams is TSingleRackPosArrayEditFunctionParams);
    xParams := (aParams as TSingleRackPosArrayEditFunctionParams);
    xRackName := xParams.RackName.Param.Value;
    xPos := xParams.Positions.Param.Value;
    // xPosArr := TStringUtilities.StringToStringArray(xParams.Positions.Param.Value, ',');

    if TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;
    vCancel := TfrmLayoutElementSelectDialog.SelectRackSinglePosModal
        (TZADesignLayoutManager.Instance.CurrentLayout.Name, xRackName, xPos, xParams.EditTitle) <> mrOK;

    if vCancel then
        EXIT;
    xParams.RackName.NewValue := xRackName;
    xParams.Positions.NewValue := xPos;
    // xParams.Positions.NewValue := TStringUtilities.StringArrayToString(xPosArr, ',');
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

    if TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;
    vCancel := TfrmLayoutElementSelectDialog.SelectRackDoublePosModal
        (TZADesignLayoutManager.Instance.CurrentLayout.Name, xRackName, xFirstPos, xLastPos,
        xParams.EditTitle) <> mrOK;

    if vCancel then
        EXIT;
    xParams.RackName.NewValue := xRackName;
    xParams.FirstPos.NewValue := xFirstPos;
    xParams.LastPos.NewValue := xLastPos;
end;

function TEditFunctions.RackTypeName_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
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

function TEditFunctions.ResourceSchemeName_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
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

function TEditFunctions.SensorDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    result := ReadDeviceNamesForType(ISensorDevice);
end;

function TEditFunctions.SequenceName_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    result := TSequenceDataAdaptor.InstReadAllNames();
end;

procedure TEditFunctions.SequenceName_OpenValue(const aParams: TCustomSettingEditFunctionParams);
var
    xParams: TSingleValueEditFunctionParams;
    xValue: string;
begin
    ASSERT(aParams is TSingleValueEditFunctionParams);
    xParams := (aParams as TSingleValueEditFunctionParams);
    xValue := xParams.SingleParam.Param.Value;

    TViewItemsWorkflow.OpenEditForm(xValue, ntSequence, true)
end;

function TEditFunctions.ShakerDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    result := ReadDeviceNamesForType(IMixerDevice);
end;

function TEditFunctions.SQLTermName_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
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

procedure TEditFunctions.SQLTermName_OpenValue(const aParams: TCustomSettingEditFunctionParams);
var
    xParams: TSingleValueEditFunctionParams;
    xValue: string;
begin
    ASSERT(aParams is TSingleValueEditFunctionParams);
    xParams := (aParams as TSingleValueEditFunctionParams);
    xValue := xParams.SingleParam.Param.Value;

    TViewItemsWorkflow.OpenEditForm(xValue, ntSQLTerm, true)
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

function TEditFunctions.SubstanceID_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    EXIT(TSubstanceDataDataAdaptor.InstReadAllNames);
end;

function TEditFunctions.SubstanceSet_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
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

function TEditFunctions.SwitchDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    result := TDesignModuleSettingFinder.GetSwitchDeviceNames();
end;

function TEditFunctions.SystemRack_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
var
    xList: TStringValueList;
begin
    xList := TStringValueList.Create();
    try
        xList.Add('SYSTEM');
        xList.AddRange(self.Rack_GetPickList(aParams));
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

function TEditFunctions.Thermostat_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    result := ReadDeviceNamesForType(IThermostatDevice);
end;

function TEditFunctions.TipType_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    result := TTipTypeDataAdaptor.InstReadAllNames();
end;

function TEditFunctions.TimeUnit_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
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

function TEditFunctions.ToolRack_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    result := self.Rack_GetPickList(aParams);
end;

function TEditFunctions.TriggerDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    result := ReadDeviceNamesForType(ITriggerDevice);
end;

function TEditFunctions.VolMotorDevice_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    result := ReadDeviceNamesForType(IVarRediMotorDevice);
end;

function TEditFunctions.VolumeCorrectionCurve_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
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

function TEditFunctions.WashProgram_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
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

procedure TEditFunctions.WashProgram_OpenValue(const aParams: TCustomSettingEditFunctionParams);
var
    xParams: TSingleValueEditFunctionParams;
    xValue: string;
begin
    ASSERT(aParams is TSingleValueEditFunctionParams);
    xParams := (aParams as TSingleValueEditFunctionParams);
    xValue := xParams.SingleParam.Param.Value;

    TViewItemsWorkflow.OpenEditForm(xValue, ntWashProg, true)
end;

function TEditFunctions.YesNo_GetPickList(const aParams: TCustomSettingEditFunctionParams): TStringArray;
begin
    result := TMethodGUIParser.GetBinaryStrings();
end;

function TEditFunctions.YesNoDelete_GetPickList(const aParams: TCustomSettingEditFunctionParams)
    : TStringArray;
begin
    SetLength(result, 3);
    result[0] := STR_YES;
    result[1] := STR_NO;
    result[2] := TCustomSetting_YesNoDelete.cDeleteText;
end;


end.
