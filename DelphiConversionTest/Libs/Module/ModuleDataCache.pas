{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : wrapper for module settings cache
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  13.10.08 pk                               TN4272.2  Initial Revision
  14.10.08 pk  CopyModule                   TN4272.2  explicitly set value property
  11.08.09 wl  ReadAllNames                 TN4702    TStringArray statt TStringList
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  27.03.13 wl                               TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit ModuleDataCache;


interface


uses
    SettingsDataCache,
    GeneralTypes;

const
    STR_IDENT_MODULETYPE = 'Type';

type
    TModuleDataCache = class
    protected
        fDataCache: TSettingsDataCacheSection;
    public
        constructor Create(const aDataCache: TSettingsDataCacheSection);
    public
        procedure ReadIntoCache(const aModuleName: string);
        procedure WriteFromCache(const aModuleName: string);
        function ReadSetting(const aSettingName: string; const aDefaultValue: string): string; virtual;
        function ReadTypeSetting(): string;
        function WriteSetting(const aSettingName: string; const aValue: string): string; virtual;
        procedure DeleteSetting(const aSettingName: string);
        procedure DeleteAllSettings();
    end;

    TModulesDataCache = class
    protected
        fAreaDataCache: TSettingsDataCacheArea;
        function GetAreaName: string; virtual; abstract;
    public
        constructor Create();
        procedure ReadIntoCache();
        procedure WriteFromCache();
        procedure CopyModule(const aSourceModuleName, aTargetModuleName: string);
        function CreateByModuleName(const aModuleName: string): TModuleDataCache;
        procedure DeleteByModuleName(const aModuleName: string);
        procedure AddByModuleName(const aModuleName: string);
        function ReadAllNames(): TStringArray;
        function ReadTypeByModuleName(const aModuleName: string): string;
        function ReadNamesForTypes(const aModuleTypeNames: TStringArray): TStringArray;
        class function FindCacheArea(const aAreaName: string): TSettingsDataCacheArea;
        property AreaName: string read GetAreaName;
    end;


implementation


uses
    SysUtils,
    Generics.Collections,
    UtilLib,
    AppInstanceIniAdaptor;

{ TModuleDataCache }

constructor TModuleDataCache.Create(const aDataCache: TSettingsDataCacheSection);
begin
    inherited Create();
    fDataCache := aDataCache;
end;

procedure TModuleDataCache.ReadIntoCache(const aModuleName: string);
begin
    fDataCache.ReadIntoCache();
end;

procedure TModuleDataCache.WriteFromCache(const aModuleName: string);
begin
    fDataCache.WriteFromCache();
end;

function TModuleDataCache.ReadSetting(const aSettingName, aDefaultValue: string): string;
var
    xIdent: TSettingsDataCacheItem;
begin
    result := aDefaultValue;
    xIdent := fDataCache.FindItemByIdentName(aSettingName);
    if xIdent = nil then
        EXIT;
    result := xIdent.Value;
end;

function TModuleDataCache.WriteSetting(const aSettingName, aValue: string): string;
var
    xIdent: TSettingsDataCacheItem;
begin
    xIdent := fDataCache.FindOrCreateIdent(aSettingName);
    xIdent.Value := aValue;
end;

procedure TModuleDataCache.DeleteSetting(const aSettingName: string);
begin
    fDataCache.DeleteItemByIdentName(aSettingName);
end;

procedure TModuleDataCache.DeleteAllSettings;
begin
    fDataCache.DeleteAllIdents();
end;

function TModuleDataCache.ReadTypeSetting: string;
begin
    result := ReadSetting(STR_IDENT_MODULETYPE, '');
end;

{ TModulesDataCache }

constructor TModulesDataCache.Create();
begin
    inherited Create();
    fAreaDataCache := FindCacheArea(self.AreaName);
end;

class function TModulesDataCache.FindCacheArea(const aAreaName: string): TSettingsDataCacheArea;
begin
    result := TAppInstanceIniAdaptor.Instance.Settings.DataCache.FindOrCreateArea(aAreaName);
end;

function TModulesDataCache.CreateByModuleName(const aModuleName: string): TModuleDataCache;
var
    xDataCacheSection: TSettingsDataCacheSection;
begin
    xDataCacheSection := fAreaDataCache.FindOrCreateSection(aModuleName);
    result := TModuleDataCache.Create(xDataCacheSection);
end;

procedure TModulesDataCache.AddByModuleName(const aModuleName: string);
var
    xDataCacheSection: TSettingsDataCacheSection;
begin
    xDataCacheSection := fAreaDataCache.FindOrCreateSection(aModuleName);
    xDataCacheSection.ReadIntoCache();
end;

procedure TModulesDataCache.DeleteByModuleName(const aModuleName: string);
begin
    fAreaDataCache.DeleteSectionBySectionName(aModuleName);
end;

function TModulesDataCache.ReadNamesForTypes(const aModuleTypeNames: TStringArray): TStringArray;
// add the names of the modules that have a type which is in aModuleTypeNames to the result list
var
    xModuleNames: TList<string>;
    x, j: integer;
    xModuleType: string;
    xDataCacheSection: TSettingsDataCacheSection;
    xModuleDataCache: TModuleDataCache;
begin

    xModuleNames := TList<string>.Create();
    try
        // for each module name check if the type of the module is found in aTypes
        for x := 0 to fAreaDataCache.Sections.Count - 1 do
        begin
            xDataCacheSection := fAreaDataCache.Sections[x];

            xModuleDataCache := TModuleDataCache.Create(xDataCacheSection);
            try
                xModuleType := xModuleDataCache.ReadTypeSetting();
            finally
                xModuleDataCache.Free;
            end;

            for j := 0 to high(aModuleTypeNames) do
            begin
                if SameText(xModuleType, aModuleTypeNames[j]) then
                begin
                    xModuleNames.Add(xDataCacheSection.SectionName);
                    BREAK;
                end;
            end;
        end;
        result := xModuleNames.ToArray;
    finally
        FreeAndNil(xModuleNames);
    end;
end;

function TModulesDataCache.ReadAllNames(): TStringArray;
var
    x: integer;
begin
    SetLength(result, fAreaDataCache.Sections.Count);
    for x := 0 to fAreaDataCache.Sections.Count - 1 do
        result[x] := fAreaDataCache.Sections[x].SectionName;

end;

procedure TModulesDataCache.ReadIntoCache;
begin
    fAreaDataCache.ReadIntoCache();
end;

procedure TModulesDataCache.WriteFromCache;
begin
    fAreaDataCache.WriteFromCache();
end;

procedure TModulesDataCache.CopyModule(const aSourceModuleName, aTargetModuleName: string);
var
    xSourceSection, xTargetSection: TSettingsDataCacheSection;
    xSourceItem, xTargetItem: TSettingsDataCacheItem;
    x: integer;
begin
    xSourceSection := fAreaDataCache.FindOrCreateSection(aSourceModuleName);
    xTargetSection := fAreaDataCache.FindOrCreateSection(aTargetModuleName);
    for x := 0 to xSourceSection.Items.Count - 1 do
    begin
        xSourceItem := xSourceSection.Items[x];
        xTargetItem := xTargetSection.FindOrCreateIdent(xSourceItem.Ident);
        xTargetItem.Value := xSourceItem.Value;
    end;
end;

function TModulesDataCache.ReadTypeByModuleName(const aModuleName: string): string;
var
    xModuleDataCache: TModuleDataCache;
begin
    xModuleDataCache := self.CreateByModuleName(aModuleName);
    try
        result := xModuleDataCache.ReadTypeSetting();
    finally
        xModuleDataCache.Free;
    end;
end;


end.
