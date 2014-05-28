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
  26.08.08 wl  PrepareModules               TN4164   schreibt jetzt in allen Devices die Property Independent
  26.08.08 wl                               TN4164   neue Funktionen zur Darstellung der Device-Hirarchie
  13.10.08 pk  ReadModuleType               TN4272.2 New
  12.12.08 wl  MarkDependentDevices         TN4363   FindModule darf keine Exception verursachen!
  17.12.08 pk                               TN4374   Code moved to DeviceSettingsManager
  14.04.09 pk  FindAllDriversForDevice      TN4523   New
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.11.09 pk                               TN4856   ShowDeviceHirarchy moved to DeviceSettingsManager
  09.04.11 wl  MarkAllDependentDevices      TN5546   Ein falsch konfiguriertes Device soll nicht mehr zum Absturz führen
  10.04.13 wl                               TN6045   uses Generics.Collections
  ---------------------------------------------------------------------------------------------------------------------- }

unit DeviceManager;


interface


uses
    Generics.Collections,
    Module,
    TypeInfo,
    ModuleManager,
    ModuleSettings,
    GeneralTypes,
    IntfDevice;

type
    TDeviceManager = class(TModuleManager)
    private
        procedure MarkAllDependentDevices;
        procedure AddDependentDevices(aUsedDevices, aUnusedDevices: TModuleList;
            aSettings: TModuleSettingList);
        procedure AddDeviceAndDependents(aDevice: IDevice; aUsedDevices, aUnusedDevices: TModuleList);
        procedure FindAllDriversForDeviceRecursive(const aDevice: IDevice; const aDriverNames: TList<string>);
    public
        procedure PrepareModules(const aErrors: TList<string>); override;
        procedure FillLayoutDeviceList(aUsedDevices, aUnusedDevices: TModuleList;
            aLayoutDeviceNames: TStringArray; aUseAllDevices: boolean);
        function FindAllDriversForDevice(const aDevice: IDevice): TStringArray;
    end;

var
    gDeviceManager: TDeviceManager;


implementation


uses
    SysUtils,
    DeviceTypeDictionary,
    DeviceSettingList,
    DriverSettingList,
    DeviceDataCache,
    DeviceSettingsManager;

procedure TDeviceManager.AddDependentDevices(aUsedDevices, aUnusedDevices: TModuleList;
    aSettings: TModuleSettingList);
var
    x: integer;
    xDevice: IDevice;
begin
    for x := 0 to aSettings.Count - 1 do
    begin

        // Search for devices
        if (aSettings[x] is TMSDevice) and (aSettings[x].Value <> '') and
            self.FindModule(true, aSettings[x].Value, IDevice, xDevice) then
        begin

            // Device zur Used-Liste hinzufügen und aus der Unused-Liste löschen
            AddDeviceAndDependents(xDevice, aUsedDevices, aUnusedDevices);
        end;
    end;
end;

procedure TDeviceManager.AddDeviceAndDependents(aDevice: IDevice; aUsedDevices, aUnusedDevices: TModuleList);
begin
    // prüfen, ob das Device schon in der Used-Liste enthalten ist
    if (aUsedDevices.IndexOf(aDevice.Name) >= 0) then
        EXIT;

    // zu Used-Liste hinzufügen
    aUsedDevices.Add(aDevice);

    // Device aus der Unused-Liste löschen
    if Assigned(aUnusedDevices) then
        aUnusedDevices.Remove(aDevice);

    // call this method recursive if device has been found
    self.AddDependentDevices(aUsedDevices, aUnusedDevices, aDevice.ModuleSettings);
end;

procedure TDeviceManager.FillLayoutDeviceList(aUsedDevices, aUnusedDevices: TModuleList;
    aLayoutDeviceNames: TStringArray; aUseAllDevices: boolean);
var
    x: integer;
    xDevice: IDevice;
begin
    aUsedDevices.Clear;
    if Assigned(aUnusedDevices) then
        aUnusedDevices.Clear;

    if (aUseAllDevices) then
    begin
        // ALLE Devices laden!
        for x := 0 to self.Modules.Count - 1 do
        begin
            aUsedDevices.Add(self.Modules[x]);
        end;
        EXIT;
    end;

    // Zunächst alle als Unused laden!
    if Assigned(aUnusedDevices) then
    begin
        for x := 0 to self.Modules.Count - 1 do
        begin
            aUnusedDevices.Add(self.Modules[x]);
        end;
    end;

    // All verwendeten Devices aus der Unused-Liste nehmen
    for x := 0 to high(aLayoutDeviceNames) do
    begin
        if self.FindModule(true, aLayoutDeviceNames[x], IDevice, xDevice) then
        begin

            // Device zur Used-Liste hinzufügen und aus der Unused-Liste löschen
            AddDeviceAndDependents(xDevice, aUsedDevices, aUnusedDevices);
        end;
    end;
end;

procedure TDeviceManager.PrepareModules(const aErrors: TList<string>);
begin
    inherited;

    MarkAllDependentDevices();
end;

procedure TDeviceManager.MarkAllDependentDevices();
var
    x: integer;
    xDevice: IDevice;
    xIndependentDeviceNames: TList<string>;
begin
    for x := 0 to fModules.Count - 1 do
    begin
        if not Supports(fModules[x], IDevice, xDevice) then
            CONTINUE;

        // Alles zurücksetzen
        xDevice.Independent := false;
    end;

    xIndependentDeviceNames := TList<string>.Create();
    try
        (fModuleSettingsManager as TDeviceSettingsManager).GetAllIndependentDeviceNames
            (xIndependentDeviceNames);
        for x := 0 to xIndependentDeviceNames.Count - 1 do
        begin
            self.FindModule(false, xIndependentDeviceNames[x], IDevice, xDevice);
            if (xDevice <> nil) then
                xDevice.Independent := true;
        end;
    finally
        FreeAndNil(xIndependentDeviceNames);
    end;
end;

procedure TDeviceManager.FindAllDriversForDeviceRecursive(const aDevice: IDevice;
    const aDriverNames: TList<string>);
var
    x: integer;
    xSetting: TModuleSetting;
    xSubDevice: IDevice;
begin
    for x := 0 to aDevice.ModuleSettings.Count - 1 do
    begin
        xSetting := aDevice.ModuleSettings[x];
        if xSetting.Value = '' then
            CONTINUE;
        // Search for devices
        if (xSetting is TMSDevice) and self.FindModule(true, xSetting.Value, (xSetting as TMSDevice).ModuleID,
            xSubDevice) then
        begin
            FindAllDriversForDeviceRecursive(xSubDevice, aDriverNames);
        end
        else if (xSetting is TMSDriver) then
        begin
            if aDriverNames.IndexOf(xSetting.Value) < 0 then
                aDriverNames.Add(xSetting.Value);
        end;

    end;
end;

function TDeviceManager.FindAllDriversForDevice(const aDevice: IDevice): TStringArray;
var
    xList: TList<string>;
begin
    xList := TList<string>.Create();
    try
        FindAllDriversForDeviceRecursive(aDevice, xList);
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;


end.
