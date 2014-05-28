{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  17.12.08 pk                                TN4374  Initial Revision
  ---------------------------------------------------------------------------------------------------------------------- }

unit DriverSettingsManager;


interface


uses
    ModuleSettingsManager,
    DriverDataCache,
    ModuleSettings;

type
    TDriverSettingsManager = class(TModuleSettingsManager)
    public
        function CreateFakeModuleSettings(const aModuleName: string): TModuleSettingList; override;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TDriverSettingsManager;
    end;


implementation


uses
    DriverTypeDictionary,
    DriverSettingList;

var
    uDriverSettingsManager: TDriverSettingsManager;

    { TDriverSettingsManager }

class procedure TDriverSettingsManager.CreateInstance;
begin
    if Assigned(uDriverSettingsManager) then
        EXIT;
    uDriverSettingsManager := TDriverSettingsManager.Create(gDriverTypeDictionary,
        TDriversDataCache.Instance);
end;

class procedure TDriverSettingsManager.DestroyInstance;
begin
    uDriverSettingsManager.Free;
end;

class function TDriverSettingsManager.Instance: TDriverSettingsManager;
begin
    result := uDriverSettingsManager;
end;

function TDriverSettingsManager.CreateFakeModuleSettings(const aModuleName: string): TModuleSettingList;
begin
    result := TDriverSettingList.Create(self.GetAreaName(), aModuleName, '');
end;


end.
