{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  27.08.09 pk                                TN4753  Various Changes
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.11.12 wl                                TN6015   überarbeitet für DisplayComponentsDataAdaptor
  23.11.12 wl  Instance                      TN6015.1 Es gibt keine Instanz mehr von TDisplayComponentSettingsManager
  23.11.12 wl  GetDisplayComponentEventNames TN6015.1 Die Daten der Komponente werden zuerst aus der Tabelle gelesen
  ----------------------------------------------------------------------------------------------------------------------- }

unit DisplayComponentSettingsManager;


interface


uses
    Generics.Collections,
    DisplayComponentTypeInfo,
    DisplayComponentSettings,
    DisplayComponentsDataAdaptor;

type

    TDisplayComponentSettingsManager = class
    private
        class procedure GetDisplayComponentEventNames(const aDisplayComponentName: string;
            const aEventNames: TList<string>; aDA: TDisplayComponentsDataAdaptor);
    public
        class function ReadNamesForType(aComponentID: TDisplayComponentID): TArray<string>;
        class function CreateFakeDisplayComponentSettings(const aDisplayComponentName: string)
            : TDisplayComponentSettingList;
        class function CreateDisplayComponentSettings(const aDisplayComponentName: string;
            const aDisplayComponentTypeName: string): TDisplayComponentSettingList;
        class function GetDisplayComponentSettingsTypeInfo(const aDisplayComponentTypeName: string)
            : TDisplayComponentSettingsTypeInfo;
        class function GetDisplayComponentNames(): TArray<string>;
        class function GetAllDisplayComponentEventNames(const aDisplayComponentName: string): TArray<string>;
    end;


implementation


uses
    SysUtils,
    DisplayComponentTypeDictionary;

{ TDisplayComponentSettingsManager }

class function TDisplayComponentSettingsManager.CreateFakeDisplayComponentSettings(const aDisplayComponentName
    : string): TDisplayComponentSettingList;
begin
    result := TDisplayComponentSettingList.Create(aDisplayComponentName, '');
end;

class function TDisplayComponentSettingsManager.ReadNamesForType(aComponentID: TDisplayComponentID)
    : TArray<string>;
var
    xTypeNames: TArray<string>;
begin
    xTypeNames := TDisplayComponentSettingsTypeDictionary.Instance.
        ReadCompatibleDisplayComponentTypeNamesByType(aComponentID);
    EXIT(TDisplayComponentsDataAdaptor.ReadNamesForTypes(xTypeNames));
end;

class function TDisplayComponentSettingsManager.GetDisplayComponentSettingsTypeInfo
    (const aDisplayComponentTypeName: string): TDisplayComponentSettingsTypeInfo;
begin
    result := TDisplayComponentSettingsTypeDictionary.Instance.GetTypeFromTypeName(aDisplayComponentTypeName)
        as TDisplayComponentSettingsTypeInfo;
end;

class function TDisplayComponentSettingsManager.CreateDisplayComponentSettings(const aDisplayComponentName
    : string; const aDisplayComponentTypeName: string): TDisplayComponentSettingList;
var
    xDisplayComponentSettingsTypeInfo: TDisplayComponentSettingsTypeInfo;
begin
    result := nil;
    xDisplayComponentSettingsTypeInfo := GetDisplayComponentSettingsTypeInfo(aDisplayComponentTypeName);
    if not Assigned(xDisplayComponentSettingsTypeInfo) then
        EXIT;
    result := xDisplayComponentSettingsTypeInfo.CreateDisplayComponentSettings(aDisplayComponentName);
end;

class function TDisplayComponentSettingsManager.GetDisplayComponentNames(): TArray<string>;
var
    xDA: TDisplayComponentsDataAdaptor;
begin
    xDA := TDisplayComponentsDataAdaptor.Create;
    try
        EXIT(xDA.ReadAllNames());
    finally
        FreeAndNil(xDA);
    end;
end;

class procedure TDisplayComponentSettingsManager.GetDisplayComponentEventNames(const aDisplayComponentName
    : string; const aEventNames: TList<string>; aDA: TDisplayComponentsDataAdaptor);
var
    x: integer;
    xTypeName: string;
    xSetting: TDisplayComponentSetting;
    xSettings: TDisplayComponentSettingList;
    xChildSettingNames: TArray<string>;
    xChildComponentSetting: TDSChildComponents;
    xEventName: string;
begin
    xTypeName := TDisplayComponentsDataAdaptor.ReadDisplayComponentType(aDisplayComponentName);
    xSettings := self.CreateDisplayComponentSettings(aDisplayComponentName, xTypeName);

    // alle Daten aus der Tabelle lesen
    xSettings.ReadAll();

    for x := 0 to xSettings.Count - 1 do
    begin
        xSetting := xSettings[x];
        if not(xSetting is TDSEvent) then
            CONTINUE;

        xEventName := (xSetting as TDSEvent).Value;
        if xEventName = '' then
            CONTINUE;
        if aEventNames.IndexOf(xEventName) >= 0 then
            CONTINUE;
        aEventNames.Add(xEventName);
    end;

    if xSettings.CanHaveChildren then
    begin
        xChildComponentSetting := xSettings.Find(TDisplayComponentSetting.cSettingNameChildComponents)
            as TDSChildComponents;
        xChildSettingNames := xChildComponentSetting.GetAllChildNames();
        for x := 0 to high(xChildSettingNames) do
        begin
            GetDisplayComponentEventNames(xChildSettingNames[x], aEventNames, aDA);
        end;
    end;
end;

class function TDisplayComponentSettingsManager.GetAllDisplayComponentEventNames(const aDisplayComponentName
    : string): TArray<string>;
var
    xList: TList<string>;
    xDA: TDisplayComponentsDataAdaptor;
begin
    xList := TList<string>.Create();
    try
        xDA := TDisplayComponentsDataAdaptor.Create;
        try
            GetDisplayComponentEventNames(aDisplayComponentName, xList, xDA);
        finally
            FreeAndNil(xDA);
        end;
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;


end.
