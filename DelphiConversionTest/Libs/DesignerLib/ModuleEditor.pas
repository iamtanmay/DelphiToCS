{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Editor for modules
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  05.09.07 pk                               TN3864    Initial Revision
  05.02.08 pk  cxTreeList1Exit              TN3864    New: call FocusedNode.EndEdit
  14.04.08 wl                               TN4060    an Änderungen in ModuleSettings angepasst
  16.05.08 pk  cxTreeList1Edited            TN4110    New function.  Called when Enter or Arrow key is used to exit the field being edited
  16.05.08 pk  cxTreeList1EditValueChanged  TN4110    Code moved to cxTreeList1Edited.  Call EndEdit to cause Edited event to be called
  13.10.08 pk  SaveData                     TN4272.2  call WriteFromCache
  17.12.08 pk                               TN4374    uses SettingsManager instead of ModuleManager
  17.12.08 pk                               TN4243    New Description column
  16.01.09 wl                               TN4362    an Änderungen in TViewItem angepasst
  10.06.09 pk  SaveStatusChanged            TN4601    New
  20.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  03.09.09 wl  cxTreeList1                  TN4800   Anpassungen an Treelist Version 5 (nicht kompatibel)
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  16.11.09 pk                               TN4800   cxTreeList1CustomDrawDataCell the updated treelist version did not automatically draw the gridlines
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  30.06.11 wl                               TN5620   alten Save-Button entfernt
  21.07.11 wl  Create                       TN5614   abhängig von CurrentUser ist TreeList enabled oder nicht
  10.05.12 wl  mnuRestoreDefaultClick       TN5891   Wenn eine Default-Wert existiert (<> ''), wird er damit wiederhergestellt
  13.03.13 wl                               TN5960   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit ModuleEditor;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    ViewItem,
    StdCtrls,
    ExtCtrls,
    cxGraphics,
    cxCustomData,
    cxStyles,
    cxTL,
    cxTextEdit,
    cxInplaceContainer,
    cxControls,
    cxEdit,
    cxDropDownEdit,
    cxButtons,
    cxEditRepositoryItems,
    Utility_DevExpress,
    ModuleSettings,
    Buttons,
    Device,
    ModuleEditorDataAdaptor,
    GeneralTypes,
    ViewItemEditForm,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    Menus;

type
    TfrmModuleEditor = class(TViewItemEditForm)
        pnlGrid: TPanel;
        cxTreeList1: TcxTreeList;
        cxTreeList1cxTreeListColumn1: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn2: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn3: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn4: TcxTreeListColumn;
        cxEditRepository1: TcxEditRepository;
        PopupMenu1: TPopupMenu;
        mnuRestoreDefault: TMenuItem;
        procedure cxTreeList1cxTreeListColumn2GetEditingProperties(Sender: TcxTreeListColumn;
            ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
        procedure sbSaveClick(Sender: TObject);
        procedure cxTreeList1EditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
        procedure cxTreeList1Exit(Sender: TObject);
        procedure cxTreeList1Edited(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
        procedure cxTreeList1CustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
            AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
        procedure mnuRestoreDefaultClick(Sender: TObject);
        procedure cxTreeList1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure PopupMenu1Popup(Sender: TObject);
    private
        procedure ChangeModuleType(const aModuleTypeName: string; aParentNode: TcxTreeListNode);
        function CreateSettingList(const aTypeName: string): TModuleSettingList; virtual; abstract;
        function ReadTypeByName(): string; virtual; abstract;
        procedure UnloadPropertyGrid(aParentNode: TcxTreeListNode);
        procedure LoadPropertyGrid(aParentNode: TcxTreeListNode);
        procedure AddNodeFromSetting(aSetting: TModuleSetting; aParentNode: TcxTreeListNode);
        procedure SetEditorProps(aEditor: TMSEditor);
        procedure EdButtonClick(aSender: TObject);
        procedure EdButtonClick2(aSender: TObject; aButtonIndex: Integer);
        procedure RefreshPickList(aSender: TObject);
        function GetMaxLength(aEditor: TMSEditor): integer;
        function GetFocusedNodeAndEditor(out oNode: TcxTreeListNode; out oEditor: TMSEditor): boolean;
        function GetFocusedEditor(out oEditor: TMSEditor): boolean;
        function GetEditorFromNode(aNode: TcxTreeListNode): TMSEditor;
        procedure MSEditor_OnValueChanged(aSender: TObject);
        procedure EditValueChanged(aNode: TcxTreeListNode; aEditor: TMSEditor);
        procedure ModuleTypeChanged(aNode: TcxTreeListNode; aEditor: TMSEModuleType);
    protected
        fDeviceEditorFactory: TDeviceEditorFactory;
        fModuleName: string;
        fParentNode: TcxTreeListNode;
        function AddEditRepositoryItem(aEditor: TMSEditor): integer;
        function CheckLastChange(): boolean; override;
        function GetDataName(): string; override;
        procedure SaveData; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
        procedure ChangeModuleTypeFilter(aModuleTypes: TStringArray);
        procedure FirstLoad; override;
    end;

    TfrmDeviceEditor = class(TfrmModuleEditor)
    protected
        function CreateSettingList(const aTypeName: string): TModuleSettingList; override;
        function ReadTypeByName(): string; override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
    end;

    TfrmDriverEditor = class(TfrmModuleEditor)
    protected
        function CreateSettingList(const aTypeName: string): TModuleSettingList; override;
        function ReadTypeByName(): string; override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
    end;

    TfrmConnectionEditor = class(TfrmModuleEditor)
    protected
        function CreateSettingList(const aTypeName: string): TModuleSettingList; override;
        function ReadTypeByName(): string; override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
    end;


implementation


{$R *.dfm}

uses
    Driver,
    CommonTypes,
    AppSettings,
    UtilLib,
    DockableForm,
    Controlutils,
    ConnectionSettingsManager,
    DriverSettingsManager,
    DeviceSettingsManager,
    ModuleViewItem;

{ TfrmModuleEditor }

constructor TfrmModuleEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited Create(aOwner, aItemName, aOnsaveStatusChanged);

    TControlUtils.ResetFontForWinXP(self);
    fModuleName := fViewItem.Name;
    self.Caption := GetCaption();
    cxTreeList1.Enabled := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);

    fDeviceEditorFactory := TDeviceEditorFactory.Create;
end;

function TfrmModuleEditor.GetDataName: string;
begin
    result := fModuleName;
end;

procedure TfrmModuleEditor.FirstLoad;
var
    xTypeName: string;
begin
    fParentNode := self.cxTreeList1.Add();
    fParentNode.ImageIndex := -1;
    fParentNode.SelectedIndex := fParentNode.ImageIndex;
    xTypeName := ReadTypeByName();
    ChangeModuleType(xTypeName, fParentNode);
end;

procedure TfrmModuleEditor.ChangeModuleType(const aModuleTypeName: string; aParentNode: TcxTreeListNode);
begin
    aParentNode.Data := CreateSettingList(aModuleTypeName);
    LoadPropertyGrid(aParentNode);
end;

procedure TfrmModuleEditor.SetEditorProps(aEditor: TMSEditor);
begin
    aEditor.OnValueChanged := self.MSEditor_OnValueChanged;
    aEditor.RepositoryIndex := AddEditRepositoryItem(aEditor);
end;

procedure TfrmModuleEditor.AddNodeFromSetting(aSetting: TModuleSetting; aParentNode: TcxTreeListNode);
var
    xNode: TcxTreeListNode;
    xEditor: TMSEditor;
begin
    xEditor := fDeviceEditorFactory.CreateEditor(aSetting);
    xNode := aParentNode.AddChild();
    xEditor.SetNode(xNode);
    SetEditorProps(xEditor);
end;

procedure TfrmModuleEditor.UnloadPropertyGrid(aParentNode: TcxTreeListNode);
var
    x: integer;
    xNode: TcxTreeListNode;
begin
    for x := aParentNode.Count - 1 downto 0 do
    begin
        xNode := aParentNode.Items[x];
        if Assigned(xNode.Data) then
            TObject(xNode.Data).Free;
        xNode.Delete;
    end;
end;

procedure TfrmModuleEditor.LoadPropertyGrid(aParentNode: TcxTreeListNode);
var
    x: integer;
    xSettings: TModuleSettingList;
begin
    xSettings := aParentNode.Data;
    UnloadPropertyGrid(aParentNode);
    if Assigned(xSettings) then
    begin
        xSettings.ReadAll();
        for x := 0 to xSettings.Count - 1 do
        begin
            AddNodeFromSetting(xSettings.Settings[x], aParentNode);
        end;
    end;
    aParentNode.Expanded := true;
end;

procedure TfrmModuleEditor.mnuRestoreDefaultClick(Sender: TObject);
var
    xEditor: TMSEditor;
    xNode: TcxTreeListNode;
begin
    if not GetFocusedNodeAndEditor(xNode, xEditor) then
        EXIT;

    xEditor.EditValue := xEditor.DefaultValue;
    xNode.EndEdit(false);
    EditValueChanged(xNode, xEditor);
end;

function TfrmModuleEditor.AddEditRepositoryItem(aEditor: TMSEditor): integer;
var
    xListItems: TStringArray;
    xItem: TcxEditRepositoryItem;
begin
    xListItems := aEditor.ReadListItems();
    if (Length(xListItems) > 0) then
    begin
        if (aEditor.HasEditFunction) then
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryComboButtonItem);
            TcxUtils.AddValuesToComboBoxProps(xListItems, (xItem as TcxEditRepositoryComboButtonItem)
                .Properties);
            (xItem as TcxEditRepositoryComboButtonItem).Properties.OnButtonClick := EdButtonClick;
            (xItem as TcxEditRepositoryComboButtonItem).Properties.MaxLength := self.GetMaxLength(aEditor);
            result := cxEditRepository1.Count - 1;
        end
        else
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryComboBoxItem);
            (xItem as TcxEditRepositoryComboBoxItem).Properties.OnInitPopup := RefreshPickList;
            TcxUtils.AddValuesToComboBoxProps(xListItems, (xItem as TcxEditRepositoryComboBoxItem)
                .Properties);
            (xItem as TcxEditRepositoryComboBoxItem).Properties.MaxLength := self.GetMaxLength(aEditor);
            // if aEditor.HasFixedList then
            // ( xItem as TcxEditRepositoryComboBoxItem ).Properties.DropDownListStyle := lsFixedList;
            result := cxEditRepository1.Count - 1;
        end;
    end

    else
    begin
        if (aEditor.HasEditFunction) then
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryButtonItem);
            (xItem as TcxEditRepositoryButtonItem).Properties.OnButtonClick := EdButtonClick2;
            (xItem as TcxEditRepositoryButtonItem).Properties.MaxLength := self.GetMaxLength(aEditor);
            result := cxEditRepository1.Count - 1;
        end
        else
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryTextItem);
            (xItem as TcxEditRepositoryTextItem).Properties.MaxLength := self.GetMaxLength(aEditor);
            result := cxEditRepository1.Count - 1;
        end;
    end;
end;

procedure TfrmModuleEditor.cxTreeList1cxTreeListColumn2GetEditingProperties(Sender: TcxTreeListColumn;
    ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
var
    xRepIndex: integer;
    xEditor: TMSEditor;
begin
    // The exact same EditProperties object is used again and again for different nodes, so we always
    // have to set the readonly
    EditProperties.ReadOnly := (aNode.Data = nil);

    if (aNode.Data = nil) then
    begin
        EXIT;
    end;
    xEditor := self.GetEditorFromNode(aNode);
    xRepIndex := xEditor.RepositoryIndex;
    if (xRepIndex < 0) then
        EXIT;

    EditProperties := cxEditRepository1.Items[xRepIndex].Properties;

end;

function TfrmModuleEditor.GetEditorFromNode(aNode: TcxTreeListNode): TMSEditor;
begin
    result := TMSEditor(aNode.Data);
end;

function TfrmModuleEditor.GetFocusedNodeAndEditor(out oNode: TcxTreeListNode; out oEditor: TMSEditor)
    : boolean;
// returns true if vParam is assigned
begin
    result := false;
    oNode := cxTreeList1.FocusedNode;
    ASSERT(Assigned(oNode), 'FocuesdNode is nil');
    oEditor := self.GetEditorFromNode(oNode);
    if not Assigned(oEditor) then
        EXIT;
    result := true;
end;

function TfrmModuleEditor.GetFocusedEditor(out oEditor: TMSEditor): boolean;
var
    xDummy: TcxTreeListNode;
begin
    result := GetFocusedNodeAndEditor(xDummy, oEditor);
end;

procedure TfrmModuleEditor.RefreshPickList(aSender: TObject);
var
    xEditor: TMSEditor;
    xList: TStrings;
    xItems: TStringArray;
    x: integer;
begin
    if not GetFocusedEditor(xEditor) then
        EXIT;

    xList := nil;
    if aSender is TcxComboBox then
        xList := (aSender as TcxComboBox).Properties.Items;

    if not Assigned(xList) then
        EXIT;

    xList.Clear;
    xItems := xEditor.ReadListItems();
    for x := 0 to Length(xItems) - 1 do
        xList.Add(xItems[x])
end;

procedure TfrmModuleEditor.EdButtonClick(aSender: TObject);
var
    xEditor: TMSEditor;
begin
    if not GetFocusedEditor(xEditor) then
        EXIT;

    // if we have changed the name of the subdevice, driver or connection name we have to end the Nodeedit mode before
    // we call the EditFunction
    cxTreeList1.FocusedNode.EndEdit(false);

    xEditor.EditFunction(self);
end;

procedure TfrmModuleEditor.EdButtonClick2(aSender: TObject; aButtonIndex: Integer);
begin
    EdButtonClick(aSender);
end;

function TfrmModuleEditor.GetMaxLength(aEditor: TMSEditor): integer;
begin
    result := 255;
end;

procedure TfrmModuleEditor.sbSaveClick(Sender: TObject);
begin
    if self.DataChanged then
        self.Save();
end;

function TfrmModuleEditor.CheckLastChange(): boolean;
begin
    result := true;

    if Assigned(cxTreeList1.FocusedNode) then
        cxTreeList1.FocusedNode.EndEdit(false);
end;

procedure TfrmModuleEditor.SaveData;
var
    xSettings: TModuleSettingList;
begin

    xSettings := fParentNode.Data;
    if Assigned(xSettings) then
    begin
        xSettings.DeleteAll();
        xSettings.WriteAll();
        xSettings.WriteFromCache();
    end;
end;

procedure TfrmModuleEditor.ModuleTypeChanged(aNode: TcxTreeListNode; aEditor: TMSEModuleType);
var
    xTypeChanged: boolean;
begin
    if aEditor.EditValue = '' then
    begin
        aEditor.EditValue := aEditor.Value;
        EXIT;
    end;

    xTypeChanged := (aEditor.EditValue <> aEditor.Value);
    aEditor.EditValueChanged();

    if xTypeChanged then
    begin
        ChangeModuleType(aEditor.EditValue, aNode.Parent);
    end;
end;

procedure TfrmModuleEditor.EditValueChanged(aNode: TcxTreeListNode; aEditor: TMSEditor);
begin
    if (aEditor is TMSEModuleType) then
    begin
        ModuleTypeChanged(aNode, (aEditor as TMSEModuleType));
    end
    else
    begin
        aEditor.EditValueChanged();
    end;
end;

procedure TfrmModuleEditor.cxTreeList1EditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
begin
    // This is necessary for situations where the combo box is changed and we want to
    // react to the change immediately.  Callint EndEdit causes the Edited event to be fired
    if Assigned(cxTreeList1.FocusedNode) then
        cxTreeList1.FocusedNode.EndEdit(false);
end;

procedure TfrmModuleEditor.cxTreeList1Edited(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
var
    xEditor: TMSEditor;
    xNode: TcxTreeListNode;
begin
    GetFocusedNodeAndEditor(xNode, xEditor);
    cxTreeList1.FocusedNode.EndEdit(false);
    EditValueChanged(xNode, xEditor);
end;

procedure TfrmModuleEditor.cxTreeList1Exit(Sender: TObject);
begin
    if not Assigned(cxTreeList1.FocusedNode) then
        EXIT;
    cxTreeList1.FocusedNode.EndEdit(false);
end;

procedure TfrmModuleEditor.cxTreeList1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
    xNode: TcxTreeListNode;
begin
    if (Button = mbRight) then
    begin
        xNode := cxTreeList1.GetNodeAt(X, Y);
        if Assigned(xNode) then
            cxTreeList1.FocusedNode := xNode;
    end;
end;

procedure TfrmModuleEditor.MSEditor_OnValueChanged(aSender: TObject);
begin
    self.ChangeData();
end;

procedure TfrmModuleEditor.PopupMenu1Popup(Sender: TObject);
var
    x: integer;
var
    xEditor: TMSEditor;
    xNode: TcxTreeListNode;
begin
    for x := 0 to PopupMenu1.Items.count - 1 do
        PopupMenu1.Items.Items[x].Visible := false;

    if not GetFocusedNodeAndEditor(xNode, xEditor) then
        EXIT;
    if (xNode = fParentNode) then
        EXIT;
    if (xEditor.DefaultValue = '') or (xEditor.DefaultValue = xEditor.EditValue) then
        EXIT;

    self.mnuRestoreDefault.Caption := TLanguageString.
        Read('Restore default value: ', 'Standardwert wiederherstellen: ') + xEditor.DefaultValue;
    self.mnuRestoreDefault.Visible := true;
end;

procedure TfrmModuleEditor.ChangeModuleTypeFilter(aModuleTypes: TStringArray);
var
    xEditor: TMSEditor;
begin
    xEditor := self.cxTreeList1.Root.Items[0].Items[0].Data;
    if xEditor is TMSEModuleType then
        (xEditor as TMSEModuleType).TypeNames := aModuleTypes;
end;

procedure TfrmModuleEditor.cxTreeList1CustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
    AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
var
    xCol: integer;
    xRect: TRect;
    xData: TMSEditor;
begin
    if AViewInfo.Selected then
        EXIT;
    xCol := aViewInfo.Column.ItemIndex;

    if xCol <> INT_COL_PROPVALUE then
        Exit;

    if aViewInfo.Node.Data = nil then
        EXIT;
    xData := (aViewInfo.Node.Data);
    if xData = nil then
        EXIT;

    if not xData.IsEditValueDefault then
        aCanvas.Font.Color := clBlue
    else
        aCanvas.Font.Color := clBlack;

    if xData.EditValue = '' then
        EXIT;
    xRect := aViewInfo.ContentRect;

    aCanvas.DrawText(xData.EditValue, Rect(xRect.Left + 2, xRect.Top + 2, xRect.Right, xRect.Bottom), 0);

    xRect := aViewInfo.BoundsRect;
    aCanvas.Brush.Color := AViewInfo.BorderColor;
    ACanvas.FrameRect(xRect);
    aDone := true;
end;

{ TfrmDeviceEditor }

constructor TfrmDeviceEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged);
end;

function TfrmDeviceEditor.CreateSettingList(const aTypeName: string): TModuleSettingList;
begin
    result := TDeviceSettingsManager.Instance.CreateModuleSettings(fModuleName, aTypeName);
    if not Assigned(result) then
    begin
        result := TDeviceSettingsManager.Instance.CreateFakeModuleSettings(fModuleName);
    end;
end;

function TfrmDeviceEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TDeviceViewItem.Create(aItemName);
end;

function TfrmDeviceEditor.ReadTypeByName(): string;
begin
    result := TDeviceSettingsManager.Instance.ReadModuleType(fModuleName);
end;

{ TfrmDriverEditor }

constructor TfrmDriverEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited;
end;

function TfrmDriverEditor.CreateSettingList(const aTypeName: string): TModuleSettingList;
begin
    result := TDriverSettingsManager.Instance.CreateModuleSettings(fModuleName, aTypeName);
    if not Assigned(result) then
    begin
        result := TDriverSettingsManager.Instance.CreateFakeModuleSettings(fModuleName);
    end;
end;

function TfrmDriverEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TDriverViewItem.Create(aItemName);
end;

function TfrmDriverEditor.ReadTypeByName: string;
begin
    result := TDriverSettingsManager.Instance.ReadModuleType(fModuleName);
end;

{ TfrmCommEditor }

constructor TfrmConnectionEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited;
end;

function TfrmConnectionEditor.CreateSettingList(const aTypeName: string): TModuleSettingList;
begin
    result := TConnectionSettingsManager.Instance.CreateModuleSettings(fModuleName, aTypeName);
    if not Assigned(result) then
    begin
        result := TConnectionSettingsManager.Instance.CreateFakeModuleSettings(fModuleName);
    end;
end;

function TfrmConnectionEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TConnectionViewItem.Create(aItemName);
end;

function TfrmConnectionEditor.ReadTypeByName: string;
begin
    result := TConnectionSettingsManager.Instance.ReadModuleType(fModuleName);
end;


end.
