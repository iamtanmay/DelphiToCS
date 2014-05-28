{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  27.08.09 pk                                        TN4753     Initial Revision
  04.11.09 pk                               	    TN4843     Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                               TN5044   uses StringUtilities
  27.05.10 wl                               TN5116   ist jetzt Teil von SimpleDialogs
  27.05.10 wl                               TN5116   enthält neue Methoden für Flush-Dialog
  09.06.10 pk                                        TN5116  new TDesignSystemLiquidSettingsFinder
  17.06.10 wl  ReadDeviceNamesForType                TN5150  kann jetzt mit Parameter UsedDeviceNames aufgerufen werden
  17.06.10 wl  ReadTipCount                          TN5156   ein nicht existenter Name wird toleriert
  03.11.10 wl  ReadGripperArmNames                   TN5306   findet alle ArmDevices, die ein GripDevice enthalten
  10.04.13 wl                                        TN6045   uses geändert
  08.11.13 wl                                        TN6298   überflüssiges entfernt
  ----------------------------------------------------------------------------------------------------------------------- }

unit DesignModuleSettingFinder;


interface


uses
    ModuleSettings,
    GeneralTypes,
    AppTypes,
    LiquidManager;

type
    TDesignSystemLiquidSettingsFinder = class(TSystemLiquidSettingsFinder)
    public
        function GetXWayValveInputPortNames(const aXWayValveDeviceName: string): TStringArray; override;
        function XWayValveExists(const aXWayValveDeviceName: string): boolean; override;
        function GetPipPumpDeviceName(const aPipDeviceName: string; const aTipIndex: integer)
            : string; override;
        function PipDeviceTipCount(const aPipDeviceName: string): integer; override;
        function PipPumpExists(const aPipPumpDeviceName: string): boolean; override;
    end;

    TDesignModuleSettingFinder = class
    private
        class function AddWildCardNames(const aNames: TStringArray): TStringArray;
        class function ArmDeviceHasGripDevice(const aArmDeviceName: string): boolean;
    public
        class function DeviceNameExists(const aDeviceName: string): boolean;
        class function ReadGripperArmNames(): TStringArray;
        class function ReadDeviceNamesForType(const aModuleID: TModuleID): TStringArray; overload;
        class function ReadDeviceNamesForType(const aModuleID: TModuleID; aUsedDeviceNames: TStringArray;
            aUseAllDevices: boolean): TStringArray; overload;
        class function DeviceExists(const aModuleID: TModuleID): boolean;
        class function ListAllSwitches(): TStringArray;
        class function GetPipDeviceNames(): TStringArray; overload;
        class function GetPipDeviceNames(aUsedDeviceNames: TStringArray; aUseAllDevices: boolean)
            : TStringArray; overload;
        class function GetSwitchDeviceNames(): TStringArray;
        class function ReadTipCount(const aPipDeviceName: string): integer;
        class function PumpGetTipNo(const aPipPumpDeviceName: string): integer;
        class function GetPumpDeviceName(const aPipDeviceName: string; const aTipIndex: integer): string;
        class function GetXWayValveInputPortNames(const aXWayValveDeviceName: string): TStringArray;
    end;


implementation


uses
    SysUtils,
    Generics.Collections,
    DeviceSettingsManager,
    IntfSwitchDevice,
    IntfPipDevice,
    StringUtilities,
    IntfArmDevice;

{ TDesignModuleSettingFinder }

class function TDesignModuleSettingFinder.ReadDeviceNamesForType(const aModuleID: TModuleID): TStringArray;
begin
    result := TDeviceSettingsManager.Instance.ReadNamesForType(aModuleID);
end;

class function TDesignModuleSettingFinder.ReadDeviceNamesForType(const aModuleID: TModuleID;
    aUsedDeviceNames: TStringArray; aUseAllDevices: boolean): TStringArray;
begin
    if (aUseAllDevices) then
        result := TDeviceSettingsManager.Instance.ReadNamesForType(aModuleID)
    else
        result := TDeviceSettingsManager.Instance.ReadNamesForTypeByUsedDeviceNames(aModuleID,
            aUsedDeviceNames);
end;

class function TDesignModuleSettingFinder.ArmDeviceHasGripDevice(const aArmDeviceName: string): boolean;
var
    xModuleSettings: TModuleSettingList;
begin
    result := false;
    if not TDeviceSettingsManager.Instance.ModuleExists(aArmDeviceName) then
        EXIT;

    xModuleSettings := TDeviceSettingsManager.Instance.CreateExistingModuleSettings(aArmDeviceName);
    if not Assigned(xModuleSettings) then
        EXIT;
    try
        xModuleSettings.ReadAll();
        result := (xModuleSettings.Find('Grip').AsStr <> '');
    finally
        xModuleSettings.Free;
    end;
end;

class function TDesignModuleSettingFinder.ReadGripperArmNames(): TStringArray;
var
    xGripperArmList: TList<string>;
    xArmNames: TStringArray;
    x: integer;
begin
    xArmNames := TDeviceSettingsManager.Instance.ReadNamesForType(IArmDevice);

    xGripperArmList := TList<string>.Create;
    try
        for x := 0 to high(xArmNames) do
        begin
            if ArmDeviceHasGripDevice(xArmNames[x]) then
                xGripperArmList.Add(xArmNames[x]);
        end;

        result := xGripperArmList.ToArray;
    finally
        FreeAndNil(xGripperArmList);
    end;
end;

class function TDesignModuleSettingFinder.AddWildCardNames(const aNames: TStringArray): TStringArray;
var
    x: integer;
    xNameWithoutNumber: string;
    xWildCardNames: TList<string>;
begin
    xWildCardNames := TList<string>.Create;
    try
        for x := 0 to Length(aNames) - 1 do
        begin
            xNameWithoutNumber := TStringUtilities.GetNameWithoutNumber(aNames[x]);
            if (xNameWithoutNumber <> '') then
                xWildCardNames.Add(xNameWithoutNumber + STR_SWITCH_WILDCARD)
        end;
        result := xWildCardNames.ToArray;
    finally
        FreeAndNil(xWildCardNames);
    end;
end;

class function TDesignModuleSettingFinder.GetSwitchDeviceNames(): TStringArray;
begin
    result := ReadDeviceNamesForType(ISwitchDevice);
end;

class function TDesignModuleSettingFinder.ListAllSwitches(): TStringArray;
var
    xNames: TStringArray;
begin
    xNames := GetSwitchDeviceNames();
    result := AddWildCardNames(xNames);
end;

class function TDesignModuleSettingFinder.DeviceExists(const aModuleID: TModuleID): boolean;
var
    xDeviceNames: TStringArray;
begin
    xDeviceNames := TDeviceSettingsManager.Instance.ReadNamesForType(aModuleID);
    result := high(xDeviceNames) >= 0;
end;

class function TDesignModuleSettingFinder.GetPipDeviceNames(): TStringArray;
begin
    result := ReadDeviceNamesForType(IPipDevice);
end;

class function TDesignModuleSettingFinder.GetPipDeviceNames(aUsedDeviceNames: TStringArray;
    aUseAllDevices: boolean): TStringArray;
begin
    result := ReadDeviceNamesForType(IPipDevice, aUsedDeviceNames, aUseAllDevices);
end;

class function TDesignModuleSettingFinder.ReadTipCount(const aPipDeviceName: string): integer;
var
    xModuleSettings: TModuleSettingList;
begin
    result := 0;
    if not TDeviceSettingsManager.Instance.ModuleExists(aPipDeviceName) then
        EXIT;

    xModuleSettings := TDeviceSettingsManager.Instance.CreateExistingModuleSettings(aPipDeviceName);
    if not Assigned(xModuleSettings) then
        EXIT;
    try
        xModuleSettings.ReadAll();
        // 28.08.09 pk HACK!!! actually xModuleSettings should be casted to a IPipDeviceSettingList interface and then
        // IPipDeviceSettingList.TipCount property should be used
        result := xModuleSettings.Find('Tips').AsInt;
    finally
        xModuleSettings.Free;
    end;
end;

class function TDesignModuleSettingFinder.PumpGetTipNo(const aPipPumpDeviceName: string): integer;
var
    xModuleSettings: TModuleSettingList;
begin
    result := 0;
    if not TDeviceSettingsManager.Instance.ModuleExists(aPipPumpDeviceName) then
        EXIT;

    xModuleSettings := TDeviceSettingsManager.Instance.CreateExistingModuleSettings(aPipPumpDeviceName);
    if not Assigned(xModuleSettings) then
        EXIT;
    try
        xModuleSettings.ReadAll();
        result := xModuleSettings.Find('TipNo').AsInt;
    finally
        xModuleSettings.Free;
    end;
end;

class function TDesignModuleSettingFinder.GetPumpDeviceName(const aPipDeviceName: string;
    const aTipIndex: integer): string;
var
    xModuleSettings: TModuleSettingList;
    xTipCount: integer;
    x: integer;
    xPipPumpDeviceName: string;
    xTipNo: integer;
begin
    result := '';
    xTipCount := ReadTipCount(aPipDeviceName);

    xModuleSettings := TDeviceSettingsManager.Instance.CreateExistingModuleSettings(aPipDeviceName);
    if not Assigned(xModuleSettings) then
        EXIT;
    try
        xModuleSettings.ReadAll();
        for x := 0 to xTipCount - 1 do
        begin
            xPipPumpDeviceName := xModuleSettings.Find('Pump' + IntToStr(x + 1)).AsStr;
            if xPipPumpDeviceName = '' then
                CONTINUE;

            xTipNo := PumpGetTipNo(xPipPumpDeviceName);
            if xTipNo = (aTipIndex + 1) then
            begin
                result := xPipPumpDeviceName;
                EXIT;
            end;
        end;
    finally
        xModuleSettings.Free;
    end;
end;

class function TDesignModuleSettingFinder.GetXWayValveInputPortNames(const aXWayValveDeviceName: string)
    : TStringArray;
var
    xModuleSettings: TModuleSettingList;
    x: Integer;

const
    cMaxPorts = 6;

begin
    SetLength(result, 0);
    if not TDeviceSettingsManager.Instance.ModuleExists(aXWayValveDeviceName) then
        EXIT;

    xModuleSettings := TDeviceSettingsManager.Instance.CreateExistingModuleSettings(aXWayValveDeviceName);
    if not Assigned(xModuleSettings) then
        EXIT;
    try
        xModuleSettings.ReadAll();
        SetLength(result, cMaxPorts);
        for x := 0 to cMaxPorts - 1 do
            result[x] := xModuleSettings.Find('In' + IntToStr(x + 1)).AsStr;

    finally
        xModuleSettings.Free;
    end;

end;

class function TDesignModuleSettingFinder.DeviceNameExists(const aDeviceName: string): boolean;
begin
    result := TDeviceSettingsManager.Instance.ModuleExists(aDeviceName);
end;

{ TDesignSystemLiquidSettingsFinder }

function TDesignSystemLiquidSettingsFinder.GetPipPumpDeviceName(const aPipDeviceName: string;
    const aTipIndex: integer): string;
begin
    result := TDesignModuleSettingFinder.GetPumpDeviceName(aPipDeviceName, aTipIndex);
end;

function TDesignSystemLiquidSettingsFinder.GetXWayValveInputPortNames(const aXWayValveDeviceName: string)
    : TStringArray;
begin
    result := TDesignModuleSettingFinder.GetXWayValveInputPortNames(aXWayValveDeviceName);
end;

function TDesignSystemLiquidSettingsFinder.XWayValveExists(const aXWayValveDeviceName: string): boolean;
begin
    result := TDesignModuleSettingFinder.DeviceNameExists(aXWayValveDeviceName);
end;

function TDesignSystemLiquidSettingsFinder.PipDeviceTipCount(const aPipDeviceName: string): integer;
begin
    result := TDesignModuleSettingFinder.ReadTipCount(aPipDeviceName);
end;

function TDesignSystemLiquidSettingsFinder.PipPumpExists(const aPipPumpDeviceName: string): boolean;
begin
    result := TDesignModuleSettingFinder.DeviceNameExists(aPipPumpDeviceName);
end;


end.
