unit DeviceSettingList;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  28.09.07 pk                                        Initial Revision
  13.10.08 pk  CreateDataCache              TN4272.2 New
  28.01.09 wl  AddDevice,AddDriver          TN4243   overload-Methode mit Description
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    Classes,
    Module,
    ModuleSettings,
    Driver,
    CommonTypes,
    ModuleDataCache;

type
    TMSDevice = class(TMSModule)
    end;

    TMSDeviceType = class(TMSModuleType)
    end;

    TDeviceSettingList = class(TModuleSettingList)
    protected
        function CreateDataCache(): TModuleDataCache; override;
    public
        procedure AddModuleType(const aDefaultValue: string); override;
        procedure AddDevice(const aSettingName: string; aID: TModuleID); overload;
        procedure AddDevice(const aSettingName, aDescription: string; aID: TModuleID); overload;
        procedure AddDriver(const aSettingName: string; aID: TModuleID); overload;
        procedure AddDriver(const aSettingName, aDescription: string; aID: TModuleID); overload;
    end;


implementation


uses
    DriverSettingList,
    DeviceDataCache;

procedure TDeviceSettingList.AddDevice(const aSettingName, aDescription: string; aID: TModuleID);
begin
    self.Add(TMSDevice.Create(aSettingName, aDescription, '', aID));
end;

procedure TDeviceSettingList.AddDevice(const aSettingName: string; aID: TModuleID);
begin
    self.AddDevice(aSettingName, '', aID);
end;

procedure TDeviceSettingList.AddDriver(const aSettingName, aDescription: string; aID: TModuleID);
begin
    self.Add(TMSDriver.Create(aSettingName, aDescription, '', aID));
end;

procedure TDeviceSettingList.AddDriver(const aSettingName: string; aID: TModuleID);
begin
    self.AddDriver(aSettingName, '', aID);
end;

procedure TDeviceSettingList.AddModuleType(const aDefaultValue: string);
begin
    self.Add(TMSDeviceType.Create(aDefaultValue));
end;

function TDeviceSettingList.CreateDataCache(): TModuleDataCache;
begin
    result := TDevicesDataCache.Instance.CreateByModuleName(fSectionName);
end;


end.
