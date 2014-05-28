{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : App Instance for Module Drivers
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  13.10.08 pk                               TN4272.2  Initial Revision
  31.08.09 pk  Create                       TN4753    create SettingsMangaer, LoadTypes, create instance of ModuleConnection
  ----------------------------------------------------------------------------------------------------------------------- }

unit AppInstanceModuleDriver;


interface


type
    TAppInstanceModuleDriver = class
    private
        constructor Create();
    public
        destructor Destroy(); override;
        class function CreateInstance(): TAppInstanceModuleDriver;
        class procedure DestroyInstance();
        class function Instance(): TAppInstanceModuleDriver;
    end;


implementation


uses
    DriverDataCache,
    DriverSettingsManager,
    PluginLoader,
    DriverTypeDictionary,
    AppInstanceModuleConnection;

var
    uInstModuleDriver: TAppInstanceModuleDriver;

    { TAppInstanceModuleDriver }

constructor TAppInstanceModuleDriver.Create;
begin
    inherited Create();
    TDriversDataCache.CreateInstance();
    TDriverSettingsManager.CreateInstance();
    TPluginLoader.LoadAllTypes(gDriverTypeDictionary);
    TAppInstanceModuleConnection.CreateInstance();
end;

destructor TAppInstanceModuleDriver.Destroy;
begin
    TAppInstanceModuleConnection.DestroyInstance();
    TDriverSettingsManager.DestroyInstance();
    TDriversDataCache.DestroyInstance();
    inherited;
end;

class function TAppInstanceModuleDriver.CreateInstance(): TAppInstanceModuleDriver;
begin
    if not Assigned(uInstModuleDriver) then
        uInstModuleDriver := TAppInstanceModuleDriver.Create();

    result := uInstModuleDriver;
end;

class function TAppInstanceModuleDriver.Instance(): TAppInstanceModuleDriver;
begin
    result := uInstModuleDriver;
end;

class procedure TAppInstanceModuleDriver.DestroyInstance;
begin
    uInstModuleDriver.Free;
end;


end.
