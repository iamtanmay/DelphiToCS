unit DeviceDataCache;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : wrapper for device settings cache
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  13.10.08 pk                               TN4272.2  Initial Revision
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    ModuleDataCache;

type
    TDeviceDataCache = class(TModuleDataCache)

    end;

    TDevicesDataCache = class(TModulesDataCache)
    protected
        function GetAreaName(): string; override;
    public
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TDevicesDataCache;
    end;


implementation


uses
    CommonTypes;

var
    uDevicesDataCache: TDevicesDataCache = nil;

    { TAllDeviceDataCache }

class procedure TDevicesDataCache.CreateInstance;
begin
    uDevicesDataCache := TDevicesDataCache.Create();
end;

class procedure TDevicesDataCache.DestroyInstance;
begin
    uDevicesDataCache.Free;
end;

function TDevicesDataCache.GetAreaName: string;
begin
    result := STR_SETTINGS_AREA_DEVICES;
end;

class function TDevicesDataCache.Instance: TDevicesDataCache;
begin
    result := uDevicesDataCache;
end;


end.
