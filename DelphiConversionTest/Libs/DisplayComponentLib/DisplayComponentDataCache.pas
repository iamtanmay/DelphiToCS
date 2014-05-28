unit DisplayComponentDataCache;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    ListClasses,
    SettingsDataCache,
    GeneralTypes;

const
    STR_IDENT_MODULETYPE = 'Type';

type
    TDisplayComponentDataCache = class
    protected
        fDataCache: TSettingsDataCacheSection;
    public
        constructor Create(const aDataCache: TSettingsDataCacheSection);
    public
        procedure ReadIntoCache(const aDisplayComponentName: string);
        procedure WriteFromCache(const aDisplayComponentName: string);
        function ReadSetting(const aSettingName: string; const aDefaultValue: string): string; virtual;
        function ReadTypeSetting(): string;
        function WriteSetting(const aSettingName: string; const aValue: string): string; virtual;
        procedure DeleteSetting(const aSettingName: string);
        procedure DeleteAllSettings();
    end;

    TDisplayComponentsDataCache = class
    protected
        fAreaDataCache: TSettingsDataCacheArea;
        function GetAreaName(): string;
    public
        constructor Create();
        procedure ReadIntoCache();
        procedure WriteFromCache();
        procedure CopyDisplayComponent(const aSourceDisplayComponentName, aTargetDisplayComponentName
            : string);
        function CreateByDisplayComponentName(const aDisplayComponentName: string)
            : TDisplayComponentDataCache;
        procedure DeleteByDisplayComponentName(const aDisplayComponentName: string);
        procedure AddByDisplayComponentName(const aDisplayComponentName: string);
        function ReadNames(): TStringArray;
        function ReadTypeByDisplayComponentName(const aDisplayComponentName: string): string;
        function ReadNamesForTypes(const aDisplayComponentTypeNames: TStringArray): TStringArray;
        class function FindCacheArea(const aAreaName: string): TSettingsDataCacheArea;
        property AreaName: string read GetAreaName;
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class function Instance(): TDisplayComponentsDataCache;
    end;


implementation


uses
    SysUtils,
    UtilLib,
    AppInstanceIniAdaptor,
    CommonTypes;

var
    uDisplayComponentsDataCache: TDisplayComponentsDataCache = nil;

    { TDisplayComponentsDataCache }

class procedure TDisplayComponentsDataCache.CreateInstance;
begin
    uDisplayComponentsDataCache := TDisplayComponentsDataCache.Create();
end;

class procedure TDisplayComponentsDataCache.DestroyInstance;
begin
    uDisplayComponentsDataCache.Free;
end;

function TDisplayComponentsDataCache.GetAreaName: string;
begin
    result := 'DISPLAYCOMPONENTS';
end;

class function TDisplayComponentsDataCache.Instance: TDisplayComponentsDataCache;
begin
    result := uDisplayComponentsDataCache;
end;

{ TDisplayComponentDataCache }

constructor TDisplayComponentDataCache.Create(const aDataCache: TSettingsDataCacheSection);
begin
    inherited Create();
    fDataCache := aDataCache;
end;

procedure TDisplayComponentDataCache.ReadIntoCache(const aDisplayComponentName: string);
begin
    fDataCache.ReadIntoCache();
end;

procedure TDisplayComponentDataCache.WriteFromCache(const aDisplayComponentName: string);
begin
    fDataCache.WriteFromCache();
end;

function TDisplayComponentDataCache.ReadSetting(const aSettingName, aDefaultValue: string): string;
var
    xIdent: TSettingsDataCacheItem;
begin
    result := aDefaultValue;
    xIdent := fDataCache.FindItemByIdentName(aSettingName);
    if xIdent = nil then
        EXIT;
    result := xIdent.Value;
end;

function TDisplayComponentDataCache.WriteSetting(const aSettingName, aValue: string): string;
var
    xIdent: TSettingsDataCacheItem;
begin
    xIdent := fDataCache.FindOrCreateIdent(aSettingName);
    xIdent.Value := aValue;
end;

procedure TDisplayComponentDataCache.DeleteSetting(const aSettingName: string);
begin
    fDataCache.DeleteItemByIdentName(aSettingName);
end;

procedure TDisplayComponentDataCache.DeleteAllSettings;
begin
    fDataCache.DeleteAllIdents();
end;

function TDisplayComponentDataCache.ReadTypeSetting: string;
begin
    result := ReadSetting(STR_IDENT_MODULETYPE, '');
end;

{ TDisplayComponentsDataCache }

constructor TDisplayComponentsDataCache.Create();
begin
    inherited Create();
    fAreaDataCache := FindCacheArea(self.AreaName);
end;

class function TDisplayComponentsDataCache.FindCacheArea(const aAreaName: string): TSettingsDataCacheArea;
begin
    result := TAppInstanceIniAdaptor.Instance.Settings.DataCache.FindOrCreateArea(aAreaName);
end;

function TDisplayComponentsDataCache.CreateByDisplayComponentName(const aDisplayComponentName: string)
    : TDisplayComponentDataCache;
var
    xDataCacheSection: TSettingsDataCacheSection;
begin
    xDataCacheSection := fAreaDataCache.FindOrCreateSection(aDisplayComponentName);
    result := TDisplayComponentDataCache.Create(xDataCacheSection);
end;

procedure TDisplayComponentsDataCache.AddByDisplayComponentName(const aDisplayComponentName: string);
var
    xDataCacheSection: TSettingsDataCacheSection;
begin
    xDataCacheSection := fAreaDataCache.FindOrCreateSection(aDisplayComponentName);
    xDataCacheSection.ReadIntoCache();
end;

procedure TDisplayComponentsDataCache.DeleteByDisplayComponentName(const aDisplayComponentName: string);
begin
    fAreaDataCache.DeleteSectionBySectionName(aDisplayComponentName);
end;

function TDisplayComponentsDataCache.ReadNamesForTypes(const aDisplayComponentTypeNames: TStringArray)
    : TStringArray;
// add the names of the modules that have a type which is in aDisplayComponentTypeNames to the result list
var
    xDisplayComponentNames: TStringValueList;
    x, j: integer;
    xDisplayComponentType: string;
    xDataCacheSection: TSettingsDataCacheSection;
    xDisplayComponentDataCache: TDisplayComponentDataCache;
begin

    xDisplayComponentNames := TStringValueList.Create();
    try
        // for each module name check if the type of the module is found in aTypes
        for x := 0 to fAreaDataCache.Sections.Count - 1 do
        begin
            xDataCacheSection := fAreaDataCache.Sections[x];

            xDisplayComponentDataCache := TDisplayComponentDataCache.Create(xDataCacheSection);
            try
                xDisplayComponentType := xDisplayComponentDataCache.ReadTypeSetting();
            finally
                xDisplayComponentDataCache.Free;
            end;

            for j := 0 to high(aDisplayComponentTypeNames) do
            begin
                if SameText(xDisplayComponentType, aDisplayComponentTypeNames[j]) then
                begin
                    xDisplayComponentNames.Add(xDataCacheSection.SectionName);
                    BREAK;
                end;
            end;
        end;
        result := xDisplayComponentNames.ToArray();
    finally
        FreeAndNil(xDisplayComponentNames);
    end;
end;

function TDisplayComponentsDataCache.ReadNames(): TStringArray;
var
    x: integer;
begin
    SetLength(result, fAreaDataCache.Sections.Count);
    for x := 0 to fAreaDataCache.Sections.Count - 1 do
        result[x] := fAreaDataCache.Sections[x].SectionName;

end;

procedure TDisplayComponentsDataCache.ReadIntoCache;
begin
    fAreaDataCache.ReadIntoCache();
end;

procedure TDisplayComponentsDataCache.WriteFromCache;
begin
    fAreaDataCache.WriteFromCache();
end;

procedure TDisplayComponentsDataCache.CopyDisplayComponent(const aSourceDisplayComponentName,
    aTargetDisplayComponentName: string);
var
    xSourceSection, xTargetSection: TSettingsDataCacheSection;
    xSourceItem, xTargetItem: TSettingsDataCacheItem;
    x: integer;
begin
    xSourceSection := fAreaDataCache.FindOrCreateSection(aSourceDisplayComponentName);
    xTargetSection := fAreaDataCache.FindOrCreateSection(aTargetDisplayComponentName);
    for x := 0 to xSourceSection.Items.Count - 1 do
    begin
        xSourceItem := xSourceSection.Items[x];
        xTargetItem := xTargetSection.FindOrCreateIdent(xSourceItem.Ident);
        xTargetItem.Value := xSourceItem.Value;
    end;
end;

function TDisplayComponentsDataCache.ReadTypeByDisplayComponentName(const aDisplayComponentName
    : string): string;
var
    xDisplayComponentDataCache: TDisplayComponentDataCache;
begin
    xDisplayComponentDataCache := self.CreateByDisplayComponentName(aDisplayComponentName);
    try
        result := xDisplayComponentDataCache.ReadTypeSetting();
    finally
        xDisplayComponentDataCache.Free;
    end;
end;


end.
