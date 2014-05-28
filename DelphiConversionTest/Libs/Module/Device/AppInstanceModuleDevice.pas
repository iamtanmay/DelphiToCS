unit AppInstanceModuleDevice;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : App Instance for Module Devices
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  13.10.08 pk                               TN4272.2  Initial Revision
  31.08.09 pk  Create                       TN4753    create SettingsMangaer, LoadTypes, create instance of ModuleDriver
  31.08.09 pk  uRefCount                    TN4753    New reference counting
  02.09.09 pk  Create                       TN4753    creates TPluginLoader instance
  08.09.09 pk  Create                       TN4753    Read Connection, Driver, Device and Robot Settings
  17.06.10 pk                               TN5152.1  BDEDataProviderFactory replaced by LocalDataProviderFactory
  17.06.10 pk                               TN5152.1  PluginLoader CreateInstance moved to AppInstanceDataAdaptorCommon
  ----------------------------------------------------------------------------------------------------------------------- }
interface


type
    TAppInstanceModuleDevice = class
    private
        class var fInstance: TAppInstanceModuleDevice;
        class var fRefCount: integer;
        constructor Create();
    public
        destructor Destroy(); override;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class property RefCount: integer read fRefCount write fRefCount;
        class property Instance: TAppInstanceModuleDevice read fInstance;
    end;


implementation


uses
    DeviceDataCache,
    DeviceSettingsManager,
    PluginLoader,
    DeviceTypeDictionary,
    AppInstanceModuleDriver,
    AppInstanceIniAdaptor,
    CommonTypes;

{ TAppInstanceModuleDevice }

constructor TAppInstanceModuleDevice.Create;
begin
    inherited Create();
    TAppInstanceIniAdaptor.Instance.Settings.DataCache.ReadAreaIntoCache(STR_SETTINGS_AREA_ROBOT);
    TAppInstanceIniAdaptor.Instance.Settings.DataCache.ReadAreaIntoCache(STR_SETTINGS_AREA_DEVICES);
    TAppInstanceIniAdaptor.Instance.Settings.DataCache.ReadAreaIntoCache(STR_SETTINGS_AREA_DRIVER);
    TAppInstanceIniAdaptor.Instance.Settings.DataCache.ReadAreaIntoCache('CONNECTION');

    TDevicesDataCache.CreateInstance();
    TDeviceSettingsManager.CreateInstance();
    TPluginLoader.LoadAllTypes(gDeviceTypeDictionary);
    TAppInstanceModuleDriver.CreateInstance();
end;

destructor TAppInstanceModuleDevice.Destroy;
begin
    TAppInstanceModuleDriver.DestroyInstance();
    TDeviceSettingsManager.DestroyInstance();
    TDevicesDataCache.DestroyInstance();
    inherited;
end;

class procedure TAppInstanceModuleDevice.CreateInstance();
begin
    if not Assigned(fInstance) then
        fInstance := TAppInstanceModuleDevice.Create();

    Inc(fRefCount);
end;

class procedure TAppInstanceModuleDevice.DestroyInstance;
begin
    if fRefCount = 1 then
        fInstance.Free;

    Dec(fRefCount);
end;


initialization


TAppInstanceModuleDevice.RefCount := 0;


end.
