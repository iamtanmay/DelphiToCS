{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  17.12.08 pk                                TN4374  Initial Revision
  11.08.09 wl  ReadModuleNames               TN4702    TStringArray statt TStringList
  28.08.09 pk  CreateExistingModuleSettings  TN4753  New
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.11.09 pk  ModuleExists                  TN4856  New
  17.06.10 wl  ReadCompatibleModuleTypeNamesByType  TN5150  new
  18.09.13 wl                                TN6045   uses geändert
  ---------------------------------------------------------------------------------------------------------------------- }

unit ModuleSettingsManager;


interface


uses
    TypeInfo,
    ModuleTypeInfo,
    Module,
    ModuleSettings,
    ModuleDataCache,
    ModuleTypeDictionary;

type
    TModuleSettingsManager = class
    private
        fTypeDictionary: TModuleTypeDictionary;
        fModulesDataCache: TModulesDataCache;
    protected
        function ReadCompatibleModuleTypeNamesByType(aModuleID: TModuleID): TArray<string>;
    public
        constructor Create(const aTypeDicitionary: TModuleTypeDictionary;
            const aModulesDataCache: TModulesDataCache);
        function ReadNamesForType(aModuleID: TModuleID): TArray<string>;
        function GetAreaName(): string;
        function CreateFakeModuleSettings(const aModuleName: string): TModuleSettingList; virtual; abstract;
        function CreateModuleSettings(const aModuleName: string; const aModuleTypeName: string)
            : TModuleSettingList;
        function CreateExistingModuleSettings(const aModuleName: string): TModuleSettingList;
        function GetModuleTypeInfo(const aModuleTypeName: string): TModuleTypeInfo;
        function ReadModuleType(const aModuleName: string): string;
        function ReadModuleNames(): TArray<string>;
        function ModuleExists(const aModuleName: string): boolean;
    end;


implementation


uses
    SysUtils;

{ TModuleSettingsManager }

constructor TModuleSettingsManager.Create(const aTypeDicitionary: TModuleTypeDictionary;
    const aModulesDataCache: TModulesDataCache);
begin
    inherited Create();
    fTypeDictionary := aTypeDicitionary;
    fModulesDataCache := aModulesDataCache;
end;

function TModuleSettingsManager.ReadCompatibleModuleTypeNamesByType(aModuleID: TModuleID): TArray<string>;
begin
    result := fTypeDictionary.ReadCompatibleModuleTypeNamesByType(aModuleID);
end;

function TModuleSettingsManager.ReadNamesForType(aModuleID: TModuleID): TArray<string>;
var
    xTypeNames: TArray<string>;
begin
    xTypeNames := self.ReadCompatibleModuleTypeNamesByType(aModuleID);
    result := fModulesDataCache.ReadNamesForTypes(xTypeNames);
end;

function TModuleSettingsManager.GetModuleTypeInfo(const aModuleTypeName: string): TModuleTypeInfo;
begin
    result := fTypeDictionary.GetTypeFromTypeName(aModuleTypeName) as TModuleTypeInfo;
end;

function TModuleSettingsManager.ModuleExists(const aModuleName: string): boolean;
begin
    result := (ReadModuleType(aModuleName) <> '');
end;

function TModuleSettingsManager.CreateModuleSettings(const aModuleName: string; const aModuleTypeName: string)
    : TModuleSettingList;
var
    xModuleTypeInfo: TModuleTypeInfo;
begin
    result := nil;
    xModuleTypeInfo := GetModuleTypeInfo(aModuleTypeName);
    if not Assigned(xModuleTypeInfo) then
        EXIT;
    result := xModuleTypeInfo.CreateModuleSettings(self.GetAreaName, aModuleName);
end;

function TModuleSettingsManager.CreateExistingModuleSettings(const aModuleName: string): TModuleSettingList;
var
    xModuleTypeName: string;
begin
    xModuleTypeName := ReadModuleType(aModuleName);
    ASSERT(xModuleTypeName <> '', Format('Module %s not found', [aModuleName]));
    result := CreateModuleSettings(aModuleName, xModuleTypeName);
end;

function TModuleSettingsManager.GetAreaName: string;
begin
    result := fModulesDataCache.AreaName;
end;

function TModuleSettingsManager.ReadModuleType(const aModuleName: string): string;
begin
    result := fModulesDataCache.ReadTypeByModuleName(aModuleName);
end;

function TModuleSettingsManager.ReadModuleNames(): TArray<string>;
begin
    result := fModulesDataCache.ReadAllNames;
end;


end.
