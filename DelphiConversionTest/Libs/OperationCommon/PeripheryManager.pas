{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Connection between the LayoutElements and the Modules part
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  16.07.08 wl                               TN4164   initial revision
  26.08.08 wl                               TN4164   einige Änderungen zu LayoutDevices
  11.11.09 pk  ShowLayoutDevices            TN4856   uses settingsmanager instead of devicemanager
  17.03.10 wl                               TN5031   uses GenericTree
  17.06.10 wl  InitTipTypes                 TN5150   von LayoutWithDevices heirher
  22.06.10 wl  RefreshLayoutDeviceList      TN5150   ReloadTipsetDevices muss nach dem Laden der Devices noch mal durchgeführt werden
  02.09.10 pk  GetCurrentUsedDevices        TN5269   handle case where no layout is loaded
  18.09.13 wl                               TN6252.3 uses geändert
  ---------------------------------------------------------------------------------------------------------------------- }

unit PeripheryManager;


interface


uses
    ModuleManager,
    Layout,
    ModuleSettings,
    GenericTree;

type
    // Gedacht als Verbindung zwischen gModules und TLayoutManager
    TPeripheryManager = class
    private
        fLayoutDevices: TModuleList;
        class var uInstance: TPeripheryManager;
        function GetCurrentLayout: TLayout;
        function GetCurrentUsedDevices(out oUseAllDevices: boolean): TArray<string>;
        property CurrentLayout: TLayout read GetCurrentLayout;
        constructor Create();
    public
        destructor Destroy(); override;
        class function Instance(): TPeripheryManager;

        procedure RefreshLayoutDeviceList();
        function FindNextModule(aExpectedModuleID: TModuleID; var vSearchIndex: integer; out oIntf): boolean;
        procedure ShowLayoutDevices(const aNodes: TStringTree);
        procedure InitTipTypes(const aPipDeviceName: string; const aTipTypeNames: TArray<string>);

        property LayoutDeviceList: TModuleList read fLayoutDevices;
    end;


implementation


uses
    SysUtils,
    LayoutManager,
    DeviceManager,
    Module,
    IntfDevice,
    DeviceSettingsManager,
    IntfPipDevice;

{ TPerypheryManager }

constructor TPeripheryManager.Create;
begin
    inherited;

    fLayoutDevices := TModuleList.Create;
end;

destructor TPeripheryManager.Destroy;
begin
    fLayoutDevices.Free;

    inherited;
end;

function TPeripheryManager.GetCurrentLayout: TLayout;
begin
    result := TLayoutManager.Instance.CurrentLayout;
end;

class function TPeripheryManager.Instance: TPeripheryManager;
begin
    if not Assigned(uInstance) then
        uInstance := TPeripheryManager.Create;

    result := uInstance;
end;

function TPeripheryManager.GetCurrentUsedDevices(out oUseAllDevices: boolean): TArray<string>;
begin
    oUseAllDevices := true;
    SetLength(result, 0);
    if TLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;

    result := self.CurrentLayout.GetUsedDevices(oUseAllDevices);
end;

procedure TPeripheryManager.RefreshLayoutDeviceList;
var
    xUseAllDevices: boolean;
    xDeviceNames: TArray<string>;
begin
    xDeviceNames := GetCurrentUsedDevices(xUseAllDevices);
    gDeviceManager.FillLayoutDeviceList(fLayoutDevices, nil, xDeviceNames, xUseAllDevices);

    if not TLayoutManager.Instance.IsCurrentLayoutEmpty then
    begin
        self.CurrentLayout.ReloadTipsetDevices();
    end;
end;

function TPeripheryManager.FindNextModule(aExpectedModuleID: TModuleID; var vSearchIndex: integer;
    out oIntf): boolean;
begin
    result := fLayoutDevices.FindNextModule(aExpectedModuleID, vSearchIndex, oIntf);
end;

procedure TPeripheryManager.ShowLayoutDevices(const aNodes: TStringTree);
begin
    TDeviceSettingsManager.Instance.ShowDeviceHirarchy(fLayoutDevices.GetNames, aNodes);
end;

procedure TPeripheryManager.InitTipTypes(const aPipDeviceName: string; const aTipTypeNames: TArray<string>);
var
    xDevice: IPipDevice;
begin
    if fLayoutDevices.FindModuleByName(false, aPipDeviceName, IPipDevice, xDevice) then
    begin
        xDevice.InitTipTypes(aTipTypeNames);
    end;
end;


end.
