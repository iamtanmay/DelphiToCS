{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  14.04.08 wl                               TN4060   basic members --> TTypeInfo
  26.05.08 wl  CreateModuleSettings         TN4119   new
  26.05.08 wl  CreateModuleByTypeName       TN4119   new
  06.06.08 pk                               TN4119   fModuleID removed.
  27.02.13 wl                               TN6045   uses geändert
  ---------------------------------------------------------------------------------------------------------------------- }

unit ModuleTypeInfo;


interface


uses
    Module,
    ModuleSettings,
    TypeInfo;

type
    TModuleTypeInfo = class(TTypeInfo)
    private
        fModuleClass: TModuleClass;
        fModuleSettingsClass: TModuleSettingsClass;
    public
        // 06.06.08 pk ToDo : aModuleID must be removed from Create
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string;
            aModuleID: TModuleID; aModuleClass: TModuleClass; aModuleSettingsClass: TModuleSettingsClass);

        function SupportsModule(aModuleID: TModuleID): boolean;
        function CreateModule(const aAreaName, aModuleName: string): IModule;
        function CreateModuleSettings(const aAreaName, aModuleName: string): TModuleSettingList;
    end;


implementation


uses
    SysUtils;

{ TModuleTypeInfo }

constructor TModuleTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string;
    aModuleID: TModuleID; aModuleClass: TModuleClass; aModuleSettingsClass: TModuleSettingsClass);
begin
    inherited Create(aTypeName, aTypeInfoVersion, aLibName, aLibVersion);

    // fModuleID := aModuleID;
    fModuleClass := aModuleClass;
    fModuleSettingsClass := aModuleSettingsClass;
end;

function TModuleTypeInfo.CreateModule(const aAreaName, aModuleName: string): IModule;
begin
    result := fModuleClass.Create(aModuleName);
    result.ModuleSettings := fModuleSettingsClass.Create(aAreaName, aModuleName, '');
end;

function TModuleTypeInfo.CreateModuleSettings(const aAreaName, aModuleName: string): TModuleSettingList;
begin
    result := fModuleSettingsClass.Create(aAreaName, aModuleName, self.TypeName);
end;

function TModuleTypeInfo.SupportsModule(aModuleID: TModuleID): boolean;
begin
    result := Supports(fModuleClass, aModuleID);
end;


end.
