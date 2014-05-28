{ ------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  16.01.09 wl                                    TN4362   TModuleViewItem extracted from ViewItems.pas
  11.08.09 wl  ReadAllNames                      TN4702    TStringArray statt TStringList
  20.11.09 ts  ItemNameExistsIntern              TN4893   new
  23.11.09 ts  ItemNameExistsIntern              TN4893   SameText instead of UpperCase
  14.03.13 wl                                    TN5960   Erweiterungen, um im Runner, die richtigen Menüpunkte zu zeigen
  ------------------------------------------------------------------------------------------------------------ }

unit ModuleViewItem;


interface


uses
    Classes,
    QueryDataAdaptor,
    ModuleDataCache,
    ViewItem,
    DockableForm,
    GeneralTypes;

type
    TModuleViewItem = class(TDatasetViewItem)
    protected
        function GetModulesDataCache(): TModulesDataCache; virtual; abstract;
        procedure DeleteNamePhysically(); override;
        procedure DeleteNameFromMemory(); override;
        function ItemNameExistsIntern(var vName: string; aCaseSensitive, aGoodResult: boolean)
            : boolean; override;
    public
        procedure SaveAs(const aSourceName, aTargetName: string); override;
        function ReadAllNames: TStringArray; override;
        procedure AddName(const aName: string); override;
    end;

    TConnectionViewItem = class(TModuleViewItem)
    protected
        function GetTypeCaption(): string; override;
        function GetItemType(): TViewItemType; override;
        function GetModulesDataCache(): TModulesDataCache; override;
    public
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;

    TDriverViewItem = class(TModuleViewItem)
    protected
        function GetTypeCaption(): string; override;
        function GetItemType(): TViewItemType; override;
        function GetModulesDataCache(): TModulesDataCache; override;
    public
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;

    TDeviceViewItem = class(TModuleViewItem)
    protected
        function GetTypeCaption(): string; override;
        function GetItemType(): TViewItemType; override;
        function GetModulesDataCache(): TModulesDataCache; override;
    public
        function CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
            : TDockableEditForm; override;
    end;


implementation


uses
    DeviceDataCache,
    DriverDataCache,
    ConnectionDataCache,
    CommonTypes,
    AppSettings,
    ModuleEditor,
    UtilLib,
    SysUtils;

{ TModuleViewItem }

procedure TModuleViewItem.DeleteNameFromMemory();
begin
    GetModulesDataCache.DeleteByModuleName(self.Name);
end;

procedure TModuleViewItem.DeleteNamePhysically();
begin
    GetModulesDataCache.WriteFromCache();
end;

function TModuleViewItem.ItemNameExistsIntern(var vName: string;
    aCaseSensitive, aGoodResult: boolean): boolean;
var
    xNames: TStringArray;
    i: integer;
begin
    result := false;
    xNames := GetModulesDataCache.ReadAllNames;
    for i := 0 to high(xNames) do
    begin
        if SameText(xNames[i], vName) then
        begin
            result := true;
            EXIT;
        end;
    end;
end;

function TModuleViewItem.ReadAllNames: TStringArray;
begin
    result := GetModulesDataCache.ReadAllNames();
end;

procedure TModuleViewItem.AddName(const aName: string);
begin
    GetModulesDataCache.AddByModuleName(aName);
end;

procedure TModuleViewItem.SaveAs(const aSourceName, aTargetName: string);
begin
    GetModulesDataCache.CopyModule(aSourceName, aTargetName);
    GetModulesDataCache.WriteFromCache();
end;

{ TDeviceViewItem }

function TDeviceViewItem.GetTypeCaption: string;
begin
    result := 'Device';
end;

function TDeviceViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmDeviceEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

function TDeviceViewItem.GetItemType: TViewItemType;
begin
    result := ntDevice;
end;

function TDeviceViewItem.GetModulesDataCache(): TModulesDataCache;
begin
    result := TDevicesDataCache.Instance;
end;

{ TDriverViewItem }

function TDriverViewItem.GetTypeCaption: string;
begin
    result := 'Driver';
end;

function TDriverViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmDriverEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

function TDriverViewItem.GetItemType: TViewItemType;
begin
    result := ntDriver;
end;

function TDriverViewItem.GetModulesDataCache(): TModulesDataCache;
begin
    result := TDriversDataCache.Instance;
end;

{ TConnectionViewItem }

function TConnectionViewItem.GetTypeCaption: string;
begin
    result := 'Connection';
end;

function TConnectionViewItem.CreateEditForm(aOwner: TComponent; aOnSaveStatusChanged: TNotifyEvent)
    : TDockableEditForm;
begin
    result := TfrmConnectionEditor.Create(aOwner, self.Name, aOnSaveStatusChanged);
end;

function TConnectionViewItem.GetItemType: TViewItemType;
begin
    result := ntConnection;
end;

function TConnectionViewItem.GetModulesDataCache(): TModulesDataCache;
begin
    result := TConnectionsDataCache.Instance;
end;


end.
