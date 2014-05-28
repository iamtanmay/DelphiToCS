{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  17.12.08 pk                                TN4374  Initial Revision
  11.11.09 pk                                TN4856  New ShowDeviceHirarchy, GetAllIndependantDeviceNames
  17.03.10 wl                               TN5031   uses GenericTree
  17.06.10 wl  ReadNamesForTypeByUsedDeviceNames  TN5150  new
  09.04.11 wl                               TN5546   Ein falsch konfiguriertes Device soll nicht mehr zum Absturz führen
  10.04.13 wl                               TN6045   uses Generics.Collections
  ---------------------------------------------------------------------------------------------------------------------- }

unit DeviceSettingsManager;


interface


uses
    Generics.Collections,
    ModuleSettingsManager,
    DeviceDataCache,
    ModuleSettings,
    GenericTree;

type
    TDeviceSettingsManager = class(TModuleSettingsManager)
    private
        class var uInstance: TDeviceSettingsManager;

        procedure GetSubDeviceNames(const aDeviceName: string; const aSubDeviceNames: TList<string>;
            const aRecursive: boolean);
        procedure ShowDependentDevices(const aDeviceName: string; const aNodes: TStringTreeNodeList);
        procedure FindDependentDevices(const aDeviceName: string; aModuleTypeNames: TArray<string>;
            aFoundDeviceNames: TList<string>);
    public
        function CreateFakeModuleSettings(const aModuleName: string): TModuleSettingList; override;
        function IsDeviceIndependent(const aDeviceName: string): boolean;
        procedure GetAllIndependentDeviceNames(const aDeviceNames: TList<string>);
        procedure ShowDeviceWithDependentDevices(const aDeviceName: string; const aTree: TStringTree);
        procedure ShowDeviceHirarchy(const aDeviceNames: TArray<string>; const aTree: TStringTree);
        function ReadNamesForTypeByUsedDeviceNames(aModuleID: TModuleID; aUsedDeviceNames: TArray<string>)
            : TArray<string>;

        class procedure CreateInstance();
        class procedure DestroyInstance();
        class property Instance: TDeviceSettingsManager read uInstance;
    end;


implementation


uses
    SysUtils,
    DeviceTypeDictionary,
    DeviceSettingList;

{ TDeviceSettingsManager }

class procedure TDeviceSettingsManager.CreateInstance;
begin
    if Assigned(uInstance) then
        EXIT;
    uInstance := TDeviceSettingsManager.Create(gDeviceTypeDictionary, TDevicesDataCache.Instance);
end;

class procedure TDeviceSettingsManager.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;

function TDeviceSettingsManager.CreateFakeModuleSettings(const aModuleName: string): TModuleSettingList;
begin
    result := TDeviceSettingList.Create(self.GetAreaName(), aModuleName, '');
end;

procedure TDeviceSettingsManager.GetSubDeviceNames(const aDeviceName: string;
    const aSubDeviceNames: TList<string>; const aRecursive: boolean);
var
    x: integer;
    xSubDeviceName: string;
    xModuleSettings: TModuleSettingList;
begin
    if not self.ModuleExists(aDeviceName) then
        EXIT;

    xModuleSettings := CreateExistingModuleSettings(aDeviceName);
    if xModuleSettings = nil then
        EXIT;

    try
        xModuleSettings.ReadAll();
        for x := 0 to xModuleSettings.Count - 1 do
        begin
            // Search for devices
            if not(xModuleSettings[x] is TMSDevice) then
                CONTINUE;
            xSubDeviceName := xModuleSettings[x].Value;
            if not self.ModuleExists(xSubDeviceName) then
                CONTINUE;
            if aSubDeviceNames.IndexOf(xSubDeviceName) >= 0 then
                CONTINUE;
            aSubDeviceNames.Add(xSubDeviceName);
            if aRecursive then
                GetSubDeviceNames(xSubDeviceName, aSubDeviceNames, true);
        end;
    finally
        FreeAndNil(xModuleSettings);
    end;
end;

procedure TDeviceSettingsManager.GetAllIndependentDeviceNames(const aDeviceNames: TList<string>);
var
    x: integer;
    xModuleNames: TArray<string>;
    xDependantDeviceNames: TList<string>;
begin
    xModuleNames := self.ReadModuleNames;

    xDependantDeviceNames := TList<string>.Create();
    try
        // Get names of all devices that are sub devices of another device
        for x := 0 to Length(xModuleNames) - 1 do
        begin
            GetSubDeviceNames(xModuleNames[x], xDependantDeviceNames, false);
        end;

        // Add those devices that are not subdevices to aDeviceNames
        for x := 0 to Length(xModuleNames) - 1 do
        begin
            if xDependantDeviceNames.IndexOf(xModuleNames[x]) >= 0 then
                CONTINUE;
            aDeviceNames.Add(xModuleNames[x]);
        end;
    finally
        FreeAndNil(xDependantDeviceNames);
    end;
end;

function TDeviceSettingsManager.IsDeviceIndependent(const aDeviceName: string): boolean;
var
    xIndependentDeviceNames: TList<string>;
begin
    xIndependentDeviceNames := TList<string>.Create();
    try
        GetAllIndependentDeviceNames(xIndependentDeviceNames);
        result := xIndependentDeviceNames.IndexOf(aDeviceName) >= 0;
    finally
        FreeAndNil(xIndependentDeviceNames);
    end;
end;

procedure TDeviceSettingsManager.ShowDependentDevices(const aDeviceName: string;
    const aNodes: TStringTreeNodeList);
// var
// x: integer;

// begin
// for x := 0 to aSettings.Count-1 do begin
//
// // Search for devices
// if ( aSettings[ x ] is TMSDevice )
// and ( aSettings[ x ].Value <> '' )
// and self.FindModule( true, aSettings[ x ].Value, ( aSettings[ x ] as TMSDevice ).ModuleID, xModule ) then begin
// xChildNode := aNodes.AddChild( aParentNode, aSettings[ x ].Value );
//
// // call this method recursive if device has been found
// self.ShowDependentDevices( aNodes, xChildNode, xModule.ModuleSettings );
// end;
// end;

var
    x: integer;
    xSubDeviceName: string;
    xModuleSettings: TModuleSettingList;
    xCurrentNode: TStringTreeNode;
begin
    if not self.ModuleExists(aDeviceName) then
        EXIT;
    xCurrentNode := TStringTreeNode.Create();
    xCurrentNode.NodeValue := aDeviceName;
    aNodes.Add(xCurrentNode);

    xModuleSettings := CreateExistingModuleSettings(aDeviceName);
    if xModuleSettings = nil then
        EXIT;

    try
        xModuleSettings.ReadAll();
        for x := 0 to xModuleSettings.Count - 1 do
        begin
            // Search for devices
            if not(xModuleSettings[x] is TMSDevice) then
                CONTINUE;
            xSubDeviceName := xModuleSettings[x].Value;
            if not self.ModuleExists(xSubDeviceName) then
                CONTINUE;
            ShowDependentDevices(xSubDeviceName, xCurrentNode.Nodes);
        end;
    finally
        FreeAndNil(xModuleSettings);
    end;
end;

procedure TDeviceSettingsManager.FindDependentDevices(const aDeviceName: string;
    aModuleTypeNames: TArray<string>; aFoundDeviceNames: TList<string>);
var
    x, j: integer;
    xSubDeviceName: string;
    xModuleSettings: TModuleSettingList;
begin
    if not self.ModuleExists(aDeviceName) then
        EXIT;

    xModuleSettings := CreateExistingModuleSettings(aDeviceName);
    if xModuleSettings = nil then
        EXIT;

    try
        xModuleSettings.ReadAll();
        for x := 0 to xModuleSettings.Count - 1 do
        begin

            // wenn Typ übereinstimmt, in die Liste nehmen
            if (xModuleSettings[x] is TMSDeviceType) then
            begin
                for j := 0 to high(aModuleTypeNames) do
                begin
                    if SameText(xModuleSettings[x].Value, aModuleTypeNames[j]) then
                    begin
                        aFoundDeviceNames.Add(aDeviceName);
                    end;
                end;
            end;

            // Search for devices
            if (xModuleSettings[x] is TMSDevice) then
            begin
                xSubDeviceName := xModuleSettings[x].Value;
                if not self.ModuleExists(xSubDeviceName) then
                    CONTINUE;
                FindDependentDevices(xSubDeviceName, aModuleTypeNames, aFoundDeviceNames);
            end;
        end;
    finally
        FreeAndNil(xModuleSettings);
    end;
end;

procedure TDeviceSettingsManager.ShowDeviceWithDependentDevices(const aDeviceName: string;
    const aTree: TStringTree);
begin
    self.ShowDependentDevices(aDeviceName, aTree.Nodes);
end;

procedure TDeviceSettingsManager.ShowDeviceHirarchy(const aDeviceNames: TArray<string>;
    const aTree: TStringTree);
var
    x: integer;
var
    xIndependentDeviceNames: TList<string>;
begin
    aTree.Nodes.Clear;

    xIndependentDeviceNames := TList<string>.Create();
    try
        GetAllIndependentDeviceNames(xIndependentDeviceNames);

        for x := 0 to Length(aDeviceNames) - 1 do
        begin
            if xIndependentDeviceNames.IndexOf(aDeviceNames[x]) < 0 then
                CONTINUE;

            ShowDeviceWithDependentDevices(aDeviceNames[x], aTree);
        end;

    finally
        FreeAndNil(xIndependentDeviceNames);
    end;
end;

function TDeviceSettingsManager.ReadNamesForTypeByUsedDeviceNames(aModuleID: TModuleID;
    aUsedDeviceNames: TArray<string>): TArray<string>;
var
    xUsedDevicesList: TList<string>;
    xModuleTypeNames: TArray<string>;
    x: integer;
begin
    xModuleTypeNames := self.ReadCompatibleModuleTypeNamesByType(aModuleID);

    xUsedDevicesList := TList<string>.Create;
    try
        for x := 0 to high(aUsedDeviceNames) do
        begin
            FindDependentDevices(aUsedDeviceNames[x], xModuleTypeNames, xUsedDevicesList);
        end;

        result := xUsedDevicesList.ToArray;
    finally
        FreeAndNil(xUsedDevicesList);
    end;
end;


end.
