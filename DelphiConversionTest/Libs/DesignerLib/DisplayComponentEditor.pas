{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  10.06.09 pk  SaveStatusChanged             TN4601  New
  11.06.09 wl                                TN4601  uses DockableForm
  20.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  03.09.09 wl  cxTreeList1                   TN4800   Anpassungen an Treelist Version 5 (nicht kompatibel)
  04.11.09 pk                                TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.12.09 pk  cxTreeList1CustomDrawDataCell TN4800   Grid lines correctly drawn with new Treelist version
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  09.06.10 wl                                TN5116   geänderter Aufruf von ViewDisplayComponentPreview
  30.06.11 wl                                TN5620   Save-Button entfernt
  21.07.11 wl  Create                        TN5614   abhängig von CurrentUser ist TreeList enabled oder nicht
  13.11.12 wl                                TN6015   überarbeitet für DisplayComponentsDataAdaptor
  23.11.12 wl                                TN6015.1 Es gibt keine Instanz mehr von TDisplayComponentSettingsManager
  13.03.13 wl                                TN5960   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit DisplayComponentEditor;


interface


uses
    Windows,
    Controls,
    Forms,
    Dialogs,
    ViewItem,
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
    DisplayComponentSettings,
    DisplayComponentEditorDataAdaptor,
    ViewItemEditForm,
    Menus,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    Classes,
    ExtCtrls,
    Buttons;

type
    TfrmDisplayComponentEditor = class(TViewItemEditForm)
        pnlGrid: TPanel;
        cxTreeList1: TcxTreeList;
        cxTreeList1cxTreeListColumn1: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn2: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn3: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn4: TcxTreeListColumn;
        cxEditRepository1: TcxEditRepository;
        pnlTop: TPanel;
        PopupMenu1: TPopupMenu;
        pmnuAdd: TMenuItem;
        btnPreview: TSpeedButton;
        pmnuDelete: TMenuItem;
        procedure cxTreeList1cxTreeListColumn2GetEditingProperties(Sender: TcxTreeListColumn;
            ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
        procedure sbSaveClick(Sender: TObject);
        procedure cxTreeList1Exit(Sender: TObject);
        procedure PopupMenu1Popup(Sender: TObject);
        procedure pmnuAddClick(Sender: TObject);
        procedure btnPreviewClick(Sender: TObject);
        procedure pmnuDeleteClick(Sender: TObject);
        procedure cxTreeList1CustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
            AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
        procedure cxTreeList1Edited(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
        procedure cxTreeList1EditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
        procedure FormCreate(Sender: TObject);
    private
        procedure ChangeDisplayComponentType(const aDisplayComponentTypeName: string;
            aParentNode: TcxTreeListNode);
        function CreateSettingList(const aTypeName: string): TDisplayComponentSettingList;
        function ReadTypeByName(): string;
        procedure UnloadPropertyGrid(aParentNode: TcxTreeListNode);
        procedure LoadPropertyGrid(aParentNode: TcxTreeListNode);
        procedure EdButtonClick(aSender: TObject);
        procedure EdButtonClick2(aSender: TObject; aButtonIndex: Integer);
        procedure RefreshPickList(aSender: TObject);
        function GetMaxLength(aEditor: TDSEditor): integer;
        function GetFocusedNodeAndEditor(out oNode: TcxTreeListNode; out oEditor: TDSEditor): boolean;
        function GetFocusedEditor(out oEditor: TDSEditor): boolean;
        function GetEditorFromNode(aNode: TcxTreeListNode): TDSEditor;
        procedure DSEditor_OnValueChanged(aSender: TObject);
        procedure DSEditor_OnAddEditRepository(aSender: TObject);
        procedure EditValueChanged(aNode: TcxTreeListNode; aEditor: TDSEditor);
        procedure DisplayComponentTypeChanged(aNode: TcxTreeListNode; aEditor: TDSEDisplayComponentType);
    protected
        fDisplayComponentEditorFactory: TDisplayComponentEditorFactory;
        fDisplayComponentName: string;
        fParentNode: TcxTreeListNode;
        function AddEditRepositoryItem(aEditor: TDSEditor): integer;
        function CheckLastChange(): boolean; override;
        function GetDataName(): string; override;
        procedure SaveData; override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
        procedure ChangeDisplayComponentTypeFilter(aDisplayComponentTypes: TArray<string>);
        procedure FirstLoad; override;
    end;


implementation


{$R *.dfm}

uses
    Graphics,
    Driver,
    CommonTypes,
    AppSettings,
    DisplayComponentsDataAdaptor,
    DisplayComponentSettingsManager,
    SpecialViewItems,
    ViewDisplayComponentPreview,
    DisplayComponentIntf,
    ControlUtils,
    DesignerMain;

{ TfrmDisplayComponentEditor }

constructor TfrmDisplayComponentEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited Create(aOwner, aItemName, aOnsaveStatusChanged);

    fDisplayComponentName := fViewItem.Name;
    Caption := GetCaption();
    cxTreeList1.Enabled := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin);

    fDisplayComponentEditorFactory := TDisplayComponentEditorFactory.Create;
    fDisplayComponentEditorFactory.OnValueChanged := self.DSEditor_OnValueChanged;
    fDisplayComponentEditorFactory.OnAddRepository := self.DSEditor_OnAddEditRepository;
end;

function TfrmDisplayComponentEditor.GetDataName: string;
begin
    result := fDisplayComponentName;
end;

procedure TfrmDisplayComponentEditor.FirstLoad;
var
    xTypeName: string;
begin
    fParentNode := self.cxTreeList1.Add();
    fParentNode.ImageIndex := -1;
    fParentNode.SelectedIndex := fParentNode.ImageIndex;
    xTypeName := ReadTypeByName();
    ChangeDisplayComponentType(xTypeName, fParentNode);
end;

procedure TfrmDisplayComponentEditor.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;

procedure TfrmDisplayComponentEditor.ChangeDisplayComponentType(const aDisplayComponentTypeName: string;
    aParentNode: TcxTreeListNode);
begin
    aParentNode.Data := CreateSettingList(aDisplayComponentTypeName);
    LoadPropertyGrid(aParentNode);
end;

procedure TfrmDisplayComponentEditor.UnloadPropertyGrid(aParentNode: TcxTreeListNode);
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

procedure TfrmDisplayComponentEditor.LoadPropertyGrid(aParentNode: TcxTreeListNode);
var
    x: integer;
    xSetting: TDisplayComponentSetting;
    xSettings: TDisplayComponentSettingList;
begin
    xSettings := aParentNode.Data;
    UnloadPropertyGrid(aParentNode);
    if Assigned(xSettings) then
    begin
        xSettings.ReadAll();
        for x := 0 to xSettings.Count - 1 do
        begin
            xSetting := xSettings[x];
            self.fDisplayComponentEditorFactory.AddNodeFromSetting(xSetting, aParentNode);
        end;
    end;
    aParentNode.Expanded := true;
end;

function TfrmDisplayComponentEditor.AddEditRepositoryItem(aEditor: TDSEditor): integer;
var
    xListItems: TArray<string>;
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

procedure TfrmDisplayComponentEditor.cxTreeList1cxTreeListColumn2GetEditingProperties
    (Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
var
    xRepIndex: integer;
    xEditor: TDSEditor;
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

function TfrmDisplayComponentEditor.GetEditorFromNode(aNode: TcxTreeListNode): TDSEditor;
begin
    result := TDSEditor(aNode.Data);
end;

function TfrmDisplayComponentEditor.GetFocusedNodeAndEditor(out oNode: TcxTreeListNode;
    out oEditor: TDSEditor): boolean;
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

function TfrmDisplayComponentEditor.GetFocusedEditor(out oEditor: TDSEditor): boolean;
var
    xDummy: TcxTreeListNode;
begin
    result := GetFocusedNodeAndEditor(xDummy, oEditor);
end;

procedure TfrmDisplayComponentEditor.RefreshPickList(aSender: TObject);
var
    xEditor: TDSEditor;
    xList: TStrings;
    xValues: TArray<string>;
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
    xValues := xEditor.ReadListItems();
    for x := 0 to Length(xValues) - 1 do
        xList.Add(xValues[x]);
end;

procedure TfrmDisplayComponentEditor.EdButtonClick(aSender: TObject);
var
    xEditor: TDSEditor;
begin
    if not GetFocusedEditor(xEditor) then
        EXIT;

    // if we have changed the name of the subdevice, driver or connection name we have to end the Nodeedit mode before
    // we call the EditFunction
    cxTreeList1.FocusedNode.EndEdit(false);

    xEditor.EditFunction(self);
end;

procedure TfrmDisplayComponentEditor.EdButtonClick2(aSender: TObject; aButtonIndex: Integer);
begin
    EdButtonClick(aSender);
end;

function TfrmDisplayComponentEditor.GetMaxLength(aEditor: TDSEditor): integer;
begin
    result := 255;
end;

procedure TfrmDisplayComponentEditor.sbSaveClick(Sender: TObject);
begin
    if self.DataChanged then
        self.Save();
end;

function TfrmDisplayComponentEditor.CheckLastChange(): boolean;
begin
    result := true;

    if Assigned(cxTreeList1.FocusedNode) then
        cxTreeList1.FocusedNode.EndEdit(false);
end;

procedure TfrmDisplayComponentEditor.SaveData;
var
    xSettings: TDisplayComponentSettingList;
begin

    xSettings := fParentNode.Data;
    if Assigned(xSettings) then
    begin
        xSettings.DeleteAll();
        xSettings.WriteAll();
    end;
end;

procedure TfrmDisplayComponentEditor.DisplayComponentTypeChanged(aNode: TcxTreeListNode;
    aEditor: TDSEDisplayComponentType);
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
        ChangeDisplayComponentType(aEditor.EditValue, aNode.Parent);
    end;
end;

procedure TfrmDisplayComponentEditor.EditValueChanged(aNode: TcxTreeListNode; aEditor: TDSEditor);
begin
    if (aEditor is TDSEDisplayComponentType) then
    begin
        DisplayComponentTypeChanged(aNode, (aEditor as TDSEDisplayComponentType));
    end
    else
    begin
        aEditor.EditValueChanged();
    end;
end;

procedure TfrmDisplayComponentEditor.cxTreeList1EditValueChanged(Sender: TcxCustomTreeList;
    AColumn: TcxTreeListColumn);
begin
    // This is necessary for situations where the combo box is changed and we want to
    // react to the change immediately.  Callint EndEdit causes the Edited event to be fired
    if Assigned(cxTreeList1.FocusedNode) then
        cxTreeList1.FocusedNode.EndEdit(false);
end;

procedure TfrmDisplayComponentEditor.cxTreeList1Edited(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
var
    xEditor: TDSEditor;
    xNode: TcxTreeListNode;
begin
    GetFocusedNodeAndEditor(xNode, xEditor);
    cxTreeList1.FocusedNode.EndEdit(false);
    EditValueChanged(xNode, xEditor);
end;

procedure TfrmDisplayComponentEditor.cxTreeList1Exit(Sender: TObject);
begin
    if not Assigned(cxTreeList1.FocusedNode) then
        EXIT;
    cxTreeList1.FocusedNode.EndEdit(false);
end;

procedure TfrmDisplayComponentEditor.DSEditor_OnValueChanged(aSender: TObject);
begin
    self.ChangeData();
end;

procedure TfrmDisplayComponentEditor.DSEditor_OnAddEditRepository(aSender: TObject);
var
    xEditor: TDSEditor;
begin
    xEditor := aSender as TDSEditor;
    xEditor.RepositoryIndex := self.AddEditRepositoryItem(xEditor);

end;

procedure TfrmDisplayComponentEditor.ChangeDisplayComponentTypeFilter(aDisplayComponentTypes: TArray<string>);
var
    xEditor: TDSEditor;
begin
    xEditor := self.cxTreeList1.Root.Items[0].Items[0].Data;
    if xEditor is TDSEDisplayComponentType then
        (xEditor as TDSEDisplayComponentType).TypeNames := aDisplayComponentTypes;
end;

procedure TfrmDisplayComponentEditor.cxTreeList1CustomDrawDataCell(Sender: TcxCustomTreeList;
    ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
var
    xCol: integer;
    xRect: TRect;
    xData: TDSEditor;
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

function TfrmDisplayComponentEditor.CreateSettingList(const aTypeName: string): TDisplayComponentSettingList;
begin
    result := TDisplayComponentSettingsManager.CreateDisplayComponentSettings(fDisplayComponentName,
        aTypeName);
    if not Assigned(result) then
    begin
        result := TDisplayComponentSettingsManager.CreateFakeDisplayComponentSettings(fDisplayComponentName);
    end;
end;

function TfrmDisplayComponentEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TDisplayComponentViewItem.Create(aItemName);
end;

function TfrmDisplayComponentEditor.ReadTypeByName(): string;
begin
    result := TDisplayComponentsDataAdaptor.ReadDisplayComponentType(fDisplayComponentName);
end;

procedure TfrmDisplayComponentEditor.PopupMenu1Popup(Sender: TObject);
var
    xEditor, xParentEditor: TDSEditor;
begin
    if not GetFocusedEditor(xEditor) then
        EXIT;
    pmnuAdd.Visible := xEditor.IsAddMenuVisible;

    pmnuDelete.Visible := false;

    xParentEditor := xEditor.ParentEditor;
    if Assigned(xParentEditor) then
    begin
        pmnuDelete.Visible := xParentEditor.IsDeleteMenuVisible;
    end;

end;

procedure TfrmDisplayComponentEditor.pmnuAddClick(Sender: TObject);
var
    xEditor: TDSEditor;
begin
    if not GetFocusedEditor(xEditor) then
        EXIT;
    self.fDisplayComponentEditorFactory.AddChild(xEditor);
end;

procedure TfrmDisplayComponentEditor.btnPreviewClick(Sender: TObject);
begin
    TfrmViewDisplayComponentPreview.InstanceShowComponent(fDisplayComponentName);
end;

procedure TfrmDisplayComponentEditor.pmnuDeleteClick(Sender: TObject);
var
    xEditor: TDSEditor;
begin
    if not GetFocusedEditor(xEditor) then
        EXIT;
    self.fDisplayComponentEditorFactory.DeleteChild(xEditor);
end;


end.
