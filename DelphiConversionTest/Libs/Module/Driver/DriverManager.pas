{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  14.04.08 wl  GetTypeInfoFromTypeName      TN4060   uses TypeInfo
  26.05.08 wl  CreateFakeModuleSettings     TN4119   GetModuleSettingsClass replaced
  13.10.08 pk  ReadModuleType               TN4272.2 New
  17.12.08 pk                               TN4374   Code moved to DriverSettingsManager
  14.04.09 pk  FindAllConnectionsForDriver  TN4523   New
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  27.03.13 wl                               TN6045   uses geändert
  ---------------------------------------------------------------------------------------------------------------------- }

unit DriverManager;


interface


uses
    Generics.Collections,
    GeneralTypes,
    ModuleManager,
    Driver;

type
    TDriverManager = class(TModuleManager)
    private
        procedure FindAllConnectionsForDriverRecursive(const aDriver: IDriver;
            const aConnectionNames: TList<string>);
    public
        function FindAllConnectionsForDriver(const aDriver: IDriver): TStringArray;
    end;

var
    gDriverManager: TDriverManager;


implementation


uses
    SysUtils,
    ModuleSettings,
    DriverSettingList,
    ConnectionSettingList;

function TDriverManager.FindAllConnectionsForDriver(const aDriver: IDriver): TStringArray;
var
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        FindAllConnectionsForDriverRecursive(aDriver, xList);
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

procedure TDriverManager.FindAllConnectionsForDriverRecursive(const aDriver: IDriver;
    const aConnectionNames: TList<string>);
var
    x: integer;
    xSetting: TModuleSetting;
    xSubDriver: IDriver;
begin
    for x := 0 to aDriver.ModuleSettings.Count - 1 do
    begin
        xSetting := aDriver.ModuleSettings[x];
        if xSetting.Value = '' then
            CONTINUE;
        // Search for Drivers
        if (xSetting is TMSDriver) and self.FindModule(true, xSetting.Value, (xSetting as TMSDriver).ModuleID,
            xSubDriver) then
        begin
            FindAllConnectionsForDriverRecursive(xSubDriver, aConnectionNames);
        end
        else if (xSetting is TMSConnection) then
        begin
            if aConnectionNames.IndexOf(xSetting.Value) < 0 then
                aConnectionNames.Add(xSetting.Value);
        end;

    end;
end;


end.
