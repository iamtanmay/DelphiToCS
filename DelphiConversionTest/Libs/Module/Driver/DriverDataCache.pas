{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : wrapper for driver settings cache
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  13.10.08 pk                               TN4272.2  Initial Revision
  ----------------------------------------------------------------------------------------------------------------------- }

unit DriverDataCache;


interface


uses
    ModuleDataCache;

type
    TDriverDataCache = class(TModuleDataCache)

    end;

    TDriversDataCache = class(TModulesDataCache)
    protected
        function GetAreaName(): string; override;
    public
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TDriversDataCache;
    end;


implementation


uses
    CommonTypes;

var
    uDriversDataCache: TDriversDataCache = nil;

    { TAllDriverDataCache }

class procedure TDriversDataCache.CreateInstance;
begin
    uDriversDataCache := TDriversDataCache.Create();
end;

class procedure TDriversDataCache.DestroyInstance;
begin
    uDriversDataCache.Free;
end;

function TDriversDataCache.GetAreaName: string;
begin
    result := STR_SETTINGS_AREA_DRIVER;
end;

class function TDriversDataCache.Instance: TDriversDataCache;
begin
    result := uDriversDataCache;
end;


end.
