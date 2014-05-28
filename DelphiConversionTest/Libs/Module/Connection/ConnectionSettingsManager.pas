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

unit ConnectionSettingsManager;


interface


uses
    ModuleSettingsManager,
    ConnectionDataCache,
    ModuleSettings;

type
    TConnectionSettingsManager = class(TModuleSettingsManager)
    public
        function CreateFakeModuleSettings(const aModuleName: string): TModuleSettingList; override;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TConnectionSettingsManager;
    end;


implementation


uses
    ConnectionTypeDictionary,
    ConnectionSettingList;

var
    uConnectionSettingsManager: TConnectionSettingsManager;

    { TConnectionSettingsManager }

class procedure TConnectionSettingsManager.CreateInstance;
begin
    if Assigned(uConnectionSettingsManager) then
        EXIT;
    uConnectionSettingsManager := TConnectionSettingsManager.Create(gConnectionTypeDictionary,
        TConnectionsDataCache.Instance);
end;

class procedure TConnectionSettingsManager.DestroyInstance;
begin
    uConnectionSettingsManager.Free;
end;

class function TConnectionSettingsManager.Instance: TConnectionSettingsManager;
begin
    result := uConnectionSettingsManager;
end;

function TConnectionSettingsManager.CreateFakeModuleSettings(const aModuleName: string): TModuleSettingList;
begin
    result := TConnectionSettingList.Create(self.GetAreaName(), aModuleName, '');
end;


end.
