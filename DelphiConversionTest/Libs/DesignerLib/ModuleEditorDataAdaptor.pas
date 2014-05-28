{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  09.01.08 wl                               TN3972    uses MethodTypes
  31.01.08 pk                               TN3864    ImagedIndex functions added
  11.02.08 wl  TMSEModuleType.Create        TN4009    Liste wird alphabetisch sortiert
  14.04.08 wl                               TN4060    fTypes is TStringArray instead of TStringList
  13.10.08 pk                               TN4272.2  DataAdaptor replaced by DataCache
  17.12.08 pk                               TN4374    uses SettingsManager instead of ModuleManager
  17.12.08 pk                               TN4243    New Description column
  16.01.09 wl                               TN4362   an Änderungen in TViewItem angepasst
  28.01.09 wl  SetNode                      TN4243    TResLoader wird nicht mehr benutzt
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  10.05.12 wl  DefaultValue                 TN5891   neu als property
  13.03.13 wl                               TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  -------------------------------------------------------------------------------------------------- }

unit ModuleEditorDataAdaptor;


interface


uses
    Classes,
    Generics.Collections,
    cxTL,

    ModuleSettings,
    ViewItem;

const
    INT_COL_PROPNAME = 0;
    INT_COL_PROPVALUE = 1;
    INT_COL_PROPDESCR = 2;

type
    TMSEditor = class
    strict private
        fRepositoryIndex: integer;
        fSetting: TModuleSetting;
        fNode: TcxTreeListNode;
        fOnValueChanged: TNotifyEvent;
        function GetValue: string;
        procedure SetValue(const aValue: string);
        function GetIsEditValueDefault: boolean;
        function GetDefaultValue: string;
    strict protected
        function GetEditValue(): string;
        procedure SetEditValue(const aValue: string);
        procedure ValueChanged();
        function GetImageIndex(): integer; virtual;
    public
        constructor Create(aModuleSetting: TModuleSetting);
        function HasEditFunction(): boolean; virtual;
        function ReadListItems(): TArray<string>; virtual;
        procedure EditFunction(aSender: TObject); virtual;
        procedure EditValueChanged();
        procedure SetNode(aNode: TcxTreeListNode);

        property Setting: TModuleSetting read fSetting;
        property Value: string read GetValue write SetValue;
        property EditValue: string read GetEditValue write SetEditValue;
        property DefaultValue: string read GetDefaultValue;
        property OnValueChanged: TNotifyEvent read fOnValueChanged write fOnValueChanged;
        property RepositoryIndex: integer read fRepositoryIndex write fRepositoryIndex;
        property IsEditValueDefault: boolean read GetIsEditValueDefault;
    end;

    TMSESimple = class(TMSEditor)
    end;

    TMSEModule = class(TMSEditor)
    strict protected
        fTypes: TArray<string>;
        function ReadAllTypeNames(): TArray<string>; virtual; abstract;
        function GetImageIndex: integer; override;
        function GetViewItemType(): TViewItemType; virtual; abstract;
    public
        constructor Create(aModuleSetting: TModuleSetting);
        function ReadListItems(): TArray<string>; override;
        function HasEditFunction: boolean; override;
        procedure EditFunction(aSender: TObject); override;
    end;

    TMSEConnection = class(TMSEModule)
    protected
        function ReadAllTypeNames(): TArray<string>; override;
        function GetImageIndex: integer; override;
        function GetViewItemType(): TViewItemType; override;
    public
        function ReadListItems(): TArray<string>; override;
    end;

    TMSEDriver = class(TMSEModule)
    protected
        function ReadAllTypeNames(): TArray<string>; override;
        function GetImageIndex: integer; override;
        function GetViewItemType(): TViewItemType; override;
    public
        function ReadListItems(): TArray<string>; override;
    end;

    TMSEDevice = class(TMSEModule)
    protected
        function ReadAllTypeNames(): TArray<string>; override;
        function GetImageIndex: integer; override;
        function GetViewItemType(): TViewItemType; override;
    public
        function ReadListItems(): TArray<string>; override;
    end;

    TMSEModuleType = class(TMSEditor)
    protected
        fTypeNames: TArray<string>;
        function ReadTypeNames(): TArray<string>; virtual; abstract;
    public
        constructor Create(aModuleSetting: TModuleSetting);
        function ReadListItems(): TArray<string>; override;
        property TypeNames: TArray<string>read fTypeNames write fTypeNames;
    end;

    TMSEDeviceType = class(TMSEModuleType)
    protected
        function ReadTypeNames(): TArray<string>; override;
    end;

    TMSEDriverType = class(TMSEModuleType)
    protected
        function ReadTypeNames(): TArray<string>; override;
    end;

    TMSEConnectionType = class(TMSEModuleType)
    protected
        function ReadTypeNames(): TArray<string>; override;
    end;

    TMSEBool = class(TMSEditor)
    public
        function ReadListItems(): TArray<string>; override;
    end;

    TDeviceEditorFactory = class
    private
        fEditors: TObjectList<TMSEditor>;
        function GetEditor(aIndex: integer): TMSEditor;
    public
        constructor Create();
        function CreateEditor(aSetting: TModuleSetting): TMSEditor;
        destructor Destroy; override;
        property Editors[aIndex: integer]: TMSEditor read GetEditor;
    end;


implementation


uses
    SysUtils,
    ModuleEditor,
    DeviceSettingList,
    DeviceTypeDictionary,
    DriverSettingList,
    MethodTypes,
    DriverTypeDictionary,
    ConnectionSettingList,
    ConnectionTypeDictionary,
    ConnectionDataCache,
    DriverDataCache,
    DeviceDataCache,
    ViewItemsWorkflow,
    ModuleDataCache;

{ TDeviceEditorFactory }

constructor TDeviceEditorFactory.Create;
begin
    inherited Create();
    fEditors := TObjectList<TMSEditor>.Create();
end;

destructor TDeviceEditorFactory.Destroy();
begin
    FreeAndNil(fEditors);
    inherited;
end;

function TDeviceEditorFactory.CreateEditor(aSetting: TModuleSetting): TMSEditor;
begin
    if aSetting is TMSDevice then
        EXIT(TMSEDevice.Create(aSetting));
    if aSetting is TMSDriver then
        EXIT(TMSEDriver.Create(aSetting));
    if aSetting is TMSConnection then
        EXIT(TMSEConnection.Create(aSetting));

    if aSetting is TMSDeviceType then
        EXIT(TMSEDeviceType.Create(aSetting));
    if aSetting is TMSDriverType then
        EXIT(TMSEDriverType.Create(aSetting));
    if aSetting is TMSConnectionType then
        EXIT(TMSEConnectionType.Create(aSetting));

    if aSetting is TMSBool then
        EXIT(TMSEBool.Create(aSetting));

    EXIT(TMSESimple.Create(aSetting));
end;

function TDeviceEditorFactory.GetEditor(aIndex: integer): TMSEditor;
begin
    EXIT(fEditors[aIndex]);
end;

{ TMSEditor }

constructor TMSEditor.Create(aModuleSetting: TModuleSetting);
begin
    inherited Create;
    fSetting := aModuleSetting;
    fNode := nil;
end;

function TMSEditor.ReadListItems(): TArray<string>;
begin
    result := fSetting.PossibleValues.ToArray;
end;

function TMSEditor.GetDefaultValue: string;
begin
    EXIT(self.Setting.DefaultValue);
end;

function TMSEditor.GetEditValue(): string;
begin
    result := fNode.Texts[INT_COL_PROPVALUE];
end;

procedure TMSEditor.SetEditValue(const aValue: string);
begin
    fNode.Texts[INT_COL_PROPVALUE] := aValue;
end;

procedure TMSEditor.EditValueChanged();
begin
    self.Value := self.EditValue;

end;

procedure TMSEditor.EditFunction(aSender: TObject);
begin
    if Assigned(fSetting.OnEditValue) then
        fSetting.OnEditValue(aSender);
end;

function TMSEditor.GetValue: string;
begin
    result := fSetting.Value;
end;

function TMSEditor.HasEditFunction: boolean;
begin
    result := false;
end;

procedure TMSEditor.ValueChanged();
begin
    if Assigned(fOnValueChanged) then
        fOnValueChanged(self);
end;

procedure TMSEditor.SetValue(const aValue: string);
begin
    if aValue = self.Value then
        EXIT;
    fSetting.Value := aValue;
    ValueChanged();
end;

procedure TMSEditor.SetNode(aNode: TcxTreeListNode);
begin
    fNode := aNode;
    fNode.Texts[INT_COL_PROPNAME] := fSetting.SettingName;
    fNode.Texts[INT_COL_PROPVALUE] := self.Value;
    fNode.Texts[INT_COL_PROPDESCR] := fSetting.Description;
    fNode.ImageIndex := GetImageIndex;
    fNode.SelectedIndex := aNode.ImageIndex;

    fNode.Data := self;
end;

function TMSEditor.GetImageIndex: integer;
begin
    result := -1;
end;

function TMSEditor.GetIsEditValueDefault: boolean;
begin
    result := self.EditValue = self.Setting.DefaultValue;
end;

{ TMSEModule }

constructor TMSEModule.Create(aModuleSetting: TModuleSetting);
begin
    inherited Create(aModuleSetting);
    fTypes := ReadAllTypeNames();
end;

function TMSEModule.HasEditFunction: boolean;
begin
    result := true;
end;

procedure TMSEModule.EditFunction(aSender: TObject);
var
    xValue: string;
    xEditForm: TfrmModuleEditor;
    xDefaultName: string;
begin
    xValue := self.EditValue;
    if xValue = '' then
    begin
        xDefaultName := (aSender as TfrmModuleEditor).DataName + self.Setting.SettingName;
        xEditForm := TViewItemsWorkflow.Instance.NewEditForm(GetViewItemType, xDefaultName)
            as TfrmModuleEditor;
        if not Assigned(xEditForm) then
            EXIT;

        self.EditValue := xEditForm.DataName;
        EditValueChanged();
    end
    else
    begin
        xEditForm := TViewItemsWorkflow.Instance.OpenEditForm(xValue, GetViewItemType) as TfrmModuleEditor;
    end;
    xEditForm.ChangeModuleTypeFilter(fTypes);
end;

function TMSEModule.GetImageIndex: integer;
begin
    result := 0;
end;

function TMSEModule.ReadListItems(): TArray<string>;
begin
    result := nil;
end;

{ TMSEDevice }

function TMSEDevice.GetImageIndex: integer;
begin
    result := INT_IM_INDEX_DEVICE;
end;

function TMSEDevice.ReadListItems(): TArray<string>;
begin
    result := TDevicesDataCache.Instance.ReadNamesForTypes(fTypes);
end;

function TMSEDevice.ReadAllTypeNames(): TArray<string>;
begin
    result := gDeviceTypeDictionary.ReadCompatibleModuleTypeNamesByType((self.Setting as TMSDevice).ModuleID);
end;

function TMSEDevice.GetViewItemType(): TViewItemType;
begin
    result := ntDevice;
end;

{ TMSEDriver }

function TMSEDriver.ReadListItems(): TArray<string>;
begin
    result := TDriversDataCache.Instance.ReadNamesForTypes(fTypes);
end;

function TMSEDriver.ReadAllTypeNames(): TArray<string>;
begin
    result := gDriverTypeDictionary.ReadCompatibleModuleTypeNamesByType((self.Setting as TMSDriver).ModuleID);
end;

function TMSEDriver.GetViewItemType(): TViewItemType;
begin
    result := ntDriver;
end;

function TMSEDriver.GetImageIndex: integer;
begin
    result := INT_IM_INDEX_DRIVER;
end;

{ TMSEConnection }

function TMSEConnection.ReadListItems(): TArray<string>;
begin
    result := TConnectionsDataCache.Instance.ReadNamesForTypes(fTypes);
end;

function TMSEConnection.ReadAllTypeNames(): TArray<string>;
begin
    result := gConnectionTypeDictionary.ReadCompatibleModuleTypeNamesByType
        ((self.Setting as TMSConnection).ModuleID);
end;

function TMSEConnection.GetViewItemType: TViewItemType;
begin
    result := ntConnection;
end;

function TMSEConnection.GetImageIndex: integer;
begin
    result := INT_IM_INDEX_CONNECTION;
end;

{ TMSEModuleType }

constructor TMSEModuleType.Create(aModuleSetting: TModuleSetting);
begin
    inherited Create(aModuleSetting);
    fTypeNames := self.ReadTypeNames();
    // GetTypes();
    // fTypes.Sort;
end;

function TMSEModuleType.ReadListItems(): TArray<string>;
begin
    result := fTypeNames;
end;

{ TMSEConnectionType }

function TMSEConnectionType.ReadTypeNames(): TArray<string>;
begin
    result := gConnectionTypeDictionary.ReadTypeNames(true);
end;

{ TMSEDeviceType }

function TMSEDeviceType.ReadTypeNames(): TArray<string>;
begin
    result := gDeviceTypeDictionary.ReadTypeNames(true);
end;

{ TMSEDriverType }

function TMSEDriverType.ReadTypeNames(): TArray<string>;
begin
    result := gDriverTypeDictionary.ReadTypeNames(true);
end;

{ TMSEBool }

function TMSEBool.ReadListItems(): TArray<string>;
begin
    SetLength(result, 2);
    result[0] := 'YES';
    result[1] := 'NO';
end;


end.
