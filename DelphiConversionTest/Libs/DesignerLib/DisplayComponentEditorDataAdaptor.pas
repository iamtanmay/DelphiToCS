{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
  07.04.09 pk  AddNodeFromSetting            TN4503  call ParentNode Expand
  27.08.09 pk                                TN4753  various changes
  04.11.09 pk                                TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  14.03.12 wl  TDSEEvent                     TN5832  neu: zeigt Methodenliste, öffnet Methode
  13.11.12 wl                                TN6015   überarbeitet für DisplayComponentsDataAdaptor
  13.03.13 wl                                TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  ----------------------------------------------------------------------------------------------------------------------- }

unit DisplayComponentEditorDataAdaptor;


interface


uses
    Classes,
    cxTL,
    Generics.Collections,
    Device,
    DisplayComponentSettings,
    QueryDataAdaptor,
    ViewItem,
    GeneralTypes;

const
    INT_COL_PROPNAME = 0;
    INT_COL_PROPVALUE = 1;
    INT_COL_PROPDESCR = 2;

type
    TDSEditor = class
    strict private
        fRepositoryIndex: integer;
        fSetting: TDisplayComponentSetting;
        fNode: TcxTreeListNode;
        fOnValueChanged: TNotifyEvent;
        function GetValue: string;
        procedure SetValue(const aValue: string);
        function GetIsEditValueDefault: boolean;
    protected
        function GetEditValue(): string;
        procedure SetEditValue(const aValue: string);
        procedure ValueChanged();
        function GetImageIndex(): integer; virtual;
        function GetIsAddMenuVisible(): boolean; virtual;
        function GetIsDeleteMenuVisible: boolean; virtual;
        class procedure SetNodeValues(aNode: TcxTreeListNode;
            const aSettingName, aDescription, aValue: string; aImageIndex: integer);
        function GetParentEditor(): TDSEditor;
    public
        constructor Create(aDisplayComponentSetting: TDisplayComponentSetting);
        function HasEditFunction(): boolean; virtual;
        function ReadListItems(): TStringArray; virtual;
        procedure EditFunction(aSender: TObject); virtual;
        procedure EditValueChanged();
        procedure SetNode(aNode: TcxTreeListNode); virtual;
        function AddNewChildSetting(): TDisplayComponentSetting; virtual;
        procedure DeleteChildSetting(const aChildSettingName: string); virtual;
        property IsAddMenuVisible: boolean read GetIsAddMenuVisible;
        property IsDeleteMenuVisible: boolean read GetIsDeleteMenuVisible;
        property Setting: TDisplayComponentSetting read fSetting;
        property Value: string read GetValue write SetValue;
        property EditValue: string read GetEditValue write SetEditValue;
        property OnValueChanged: TNotifyEvent read fOnValueChanged write fOnValueChanged;
        property RepositoryIndex: integer read fRepositoryIndex write fRepositoryIndex;
        property IsEditValueDefault: boolean read GetIsEditValueDefault;
        property ParentEditor: TDSEditor read GetParentEditor;
        property Node: TcxTreeListNode read fNode;
    end;

    TDSESimple = class(TDSEditor)
    end;

    TDSEDisplayComponent = class(TDSEditor)
    strict private
        fTypes: TStringArray;
        function ReadAllTypeNames(): TStringArray;
        function GetViewItemType(): TViewItemType;
    protected
        function GetImageIndex: integer; override;
    public
        constructor Create(aDisplayComponentSetting: TDisplayComponentSetting);
        function ReadListItems(): TStringArray; override;
        function HasEditFunction: boolean; override;
        procedure EditFunction(aSender: TObject); override;
    end;

    TDSEDisplayComponentType = class(TDSEditor)
    strict private
        fTypeNames: TStringArray;
        function ReadTypeNames(): TStringArray;
    public
        constructor Create(aDisplayComponentSetting: TDisplayComponentSetting);
        function ReadListItems(): TStringArray; override;
        property TypeNames: TStringArray read fTypeNames write fTypeNames;
    end;

    TDSEEvent = class(TDSEditor)
    public
        constructor Create(aDisplayComponentSetting: TDisplayComponentSetting);

        function ReadListItems(): TStringArray; override;
        function HasEditFunction: boolean; override;
        procedure EditFunction(aSender: TObject); override;
    end;

    TDSEMulti = class(TDSEditor)
    strict private
        function GetMultiSetting(): TDSMulti;
    protected
        function GetIsAddMenuVisible: boolean; override;
        function GetIsDeleteMenuVisible: boolean; override;
    public
        constructor Create(aSetting: TDSMulti);
        function AddNewChildSetting(): TDisplayComponentSetting; override;
        procedure DeleteChildSetting(const aChildSettingName: string); override;
        property MultiSetting: TDSMulti read GetMultiSetting;
    end;

    TDSEChildComponents = class(TDSEMulti)
    end;

    TDisplayComponentEditorFactory = class
    strict private
        fEditors: TObjectList<TDSEditor>;
        fOnValueChanged: TNotifyEvent;
        fOnAddRepository: TNotifyEvent;
        procedure DoOnAddRepository(aSender: TObject);
        procedure DoOnValueChanged(aSender: TObject);
        procedure AddChildNodesFromSetting(aSetting: TDisplayComponentSetting; aParentNode: TcxTreeListNode);
    public
        constructor Create();
        destructor Destroy; override;

        function CreateEditor(aSetting: TDisplayComponentSetting): TDSEditor;
        procedure AddNodeFromSetting(aSetting: TDisplayComponentSetting; aParentNode: TcxTreeListNode);
        procedure AddChild(aEditor: TDSEditor);
        procedure DeleteChild(aEditor: TDSEditor);
        property OnValueChanged: TNotifyEvent read fOnValueChanged write fOnValueChanged;
        property OnAddRepository: TNotifyEvent read fOnAddRepository write fOnAddRepository;
    end;


implementation


uses
    SysUtils,
    DisplayComponentEditor,
    DisplayComponentTypeDictionary,
    MethodDataAdaptor,
    DisplayComponentSettingsManager,
    MainCustomDevelopment,
    DisplayComponentsDataAdaptor,
    ViewItemEditForm,
    DesignerViewItemsWorkflow,
    ViewItemsWorkflow;

{ TDisplayComponentEditorFactory }

constructor TDisplayComponentEditorFactory.Create;
begin
    inherited Create();
    fEditors := TObjectList<TDSEditor>.Create();
end;

destructor TDisplayComponentEditorFactory.Destroy();
begin
    fEditors.Free;
    inherited;
end;

function TDisplayComponentEditorFactory.CreateEditor(aSetting: TDisplayComponentSetting): TDSEditor;
begin
    if aSetting is TDSDisplayComponent then
        EXIT(TDSEDisplayComponent.Create(aSetting));

    if aSetting is TDSDisplayComponentType then
        EXIT(TDSEDisplayComponentType.Create(aSetting));

    if aSetting is TDSMulti then
        EXIT(TDSEMulti.Create(aSetting as TDSMulti));

    if aSetting is TDSEvent then
        EXIT(TDSEEvent.Create(aSetting));

    EXIT(TDSESimple.Create(aSetting));
end;

procedure TDisplayComponentEditorFactory.DoOnValueChanged(aSender: TObject);
begin
    if Assigned(fOnValueChanged) then
        fOnValueChanged(aSender);
end;

procedure TDisplayComponentEditorFactory.DoOnAddRepository(aSender: TObject);
begin
    if Assigned(fOnAddRepository) then
        fOnAddRepository(aSender);
end;

procedure TDisplayComponentEditorFactory.AddChildNodesFromSetting(aSetting: TDisplayComponentSetting;
    aParentNode: TcxTreeListNode);
var
    x: integer;
begin
    if aSetting is TDSMulti then
    begin
        for x := 0 to (aSetting as TDSMulti).ChildSettings.Count - 1 do
            AddNodeFromSetting((aSetting as TDSMulti).ChildSettings[x], aParentNode);
    end;
end;

procedure TDisplayComponentEditorFactory.AddNodeFromSetting(aSetting: TDisplayComponentSetting;
    aParentNode: TcxTreeListNode);
var
    xNode: TcxTreeListNode;
    xEditor: TDSEditor;

begin
    xEditor := CreateEditor(aSetting);
    xNode := aParentNode.AddChild();
    xEditor.SetNode(xNode);
    xEditor.OnValueChanged := self.DoOnValueChanged;
    self.DoOnAddRepository(xEditor);

    AddChildNodesFromSetting(aSetting, xNode);

    aParentNode.Expand(false);
end;

procedure TDisplayComponentEditorFactory.AddChild(aEditor: TDSEditor);
var
    xChildSetting: TDisplayComponentSetting;
begin
    xChildSetting := aEditor.AddNewChildSetting();

    self.AddNodeFromSetting(xChildSetting, aEditor.Node);

    aEditor.Node.GetLastChild.Focused := true;
end;

procedure TDisplayComponentEditorFactory.DeleteChild(aEditor: TDSEditor);
var
    xParentEditor: TDSEditor;
begin
    xParentEditor := aEditor.ParentEditor;
    xParentEditor.DeleteChildSetting(aEditor.Setting.SettingName);
    aEditor.Free;

    xParentEditor.Node.DeleteChildren();
    AddChildNodesFromSetting(xParentEditor.Setting, xParentEditor.Node);
    self.DoOnValueChanged(xParentEditor);
end;

{ TDSEditor }

constructor TDSEditor.Create(aDisplayComponentSetting: TDisplayComponentSetting);
begin
    inherited Create;
    fSetting := aDisplayComponentSetting;
    fNode := nil;
end;

function TDSEditor.ReadListItems(): TStringArray;
begin
    result := fSetting.PossibleValues.ToArray;
end;

function TDSEditor.GetEditValue(): string;
begin
    result := fNode.Texts[INT_COL_PROPVALUE];
end;

procedure TDSEditor.SetEditValue(const aValue: string);
begin
    fNode.Texts[INT_COL_PROPVALUE] := aValue;
end;

procedure TDSEditor.EditValueChanged();
begin
    self.Value := self.EditValue;

end;

procedure TDSEditor.EditFunction(aSender: TObject);
begin
    if Assigned(fSetting.OnEditValue) then
        fSetting.OnEditValue(aSender);
end;

function TDSEditor.GetValue: string;
begin
    result := fSetting.Value;
end;

function TDSEditor.HasEditFunction: boolean;
begin
    result := false;
end;

procedure TDSEditor.ValueChanged();
begin
    if Assigned(fOnValueChanged) then
        fOnValueChanged(self);
end;

procedure TDSEditor.SetValue(const aValue: string);
begin
    if aValue = self.Value then
        EXIT;
    fSetting.Value := aValue;
    ValueChanged();
end;

class procedure TDSEditor.SetNodeValues(aNode: TcxTreeListNode;
    const aSettingName, aDescription, aValue: string; aImageIndex: integer);
begin
    aNode.Texts[INT_COL_PROPNAME] := aSettingName;
    aNode.Texts[INT_COL_PROPVALUE] := aValue;
    aNode.Texts[INT_COL_PROPDESCR] := aDescription;
    aNode.ImageIndex := aImageIndex;
    aNode.SelectedIndex := aImageIndex;
end;

procedure TDSEditor.SetNode(aNode: TcxTreeListNode);
begin
    fNode := aNode;
    SetNodeValues(aNode, fSetting.SettingName, fSetting.Description, self.Value, GetImageIndex);
    fNode.Data := self;
end;

function TDSEditor.GetImageIndex: integer;
begin
    result := -1;
end;

function TDSEditor.GetIsEditValueDefault: boolean;
begin
    result := self.EditValue = self.Setting.DefaultValue;
end;

function TDSEditor.GetIsAddMenuVisible: boolean;
begin
    result := false;
end;

function TDSEditor.AddNewChildSetting(): TDisplayComponentSetting;
begin
    result := nil;
end;

procedure TDSEditor.DeleteChildSetting(const aChildSettingName: string);
begin

end;

function TDSEditor.GetIsDeleteMenuVisible: boolean;
begin
    result := false;
end;

function TDSEditor.GetParentEditor: TDSEditor;
begin
    result := nil;
    if not Assigned(fNode.Parent.Data) then
        EXIT;
    if TObject(fNode.Parent.Data) is TDSEditor then
        result := fNode.Parent.Data;
end;

{ TDSEDisplayComponent }

constructor TDSEDisplayComponent.Create(aDisplayComponentSetting: TDisplayComponentSetting);
begin
    inherited Create(aDisplayComponentSetting);
    fTypes := ReadAllTypeNames();
end;

function TDSEDisplayComponent.HasEditFunction: boolean;
begin
    result := true;
end;

procedure TDSEDisplayComponent.EditFunction(aSender: TObject);
var
    xValue: string;
    xEditForm: TfrmDisplayComponentEditor;
    xDefaultName: string;
begin
    xValue := self.EditValue;
    if xValue = '' then
    begin
        xDefaultName := (aSender as TfrmDisplayComponentEditor).DataName + self.Setting.SettingName;
        xEditForm := TViewItemsWorkflow.Instance.NewEditForm(GetViewItemType, xDefaultName)
            as TfrmDisplayComponentEditor;
        if not Assigned(xEditForm) then
            EXIT;

        self.EditValue := xEditForm.DataName;
        EditValueChanged();
    end
    else
    begin
        xEditForm := TViewItemsWorkflow.Instance.OpenEditForm(xValue, GetViewItemType)
            as TfrmDisplayComponentEditor;
    end;
    xEditForm.ChangeDisplayComponentTypeFilter(fTypes);
end;

function TDSEDisplayComponent.GetImageIndex: integer;
const
    cImageIndexDisplayComponent = 30;
begin
    result := cImageIndexDisplayComponent;
end;

function TDSEDisplayComponent.ReadListItems(): TStringArray;
begin
    result := TDisplayComponentsDataAdaptor.ReadNamesForTypes(fTypes);
end;

function TDSEDisplayComponent.ReadAllTypeNames(): TStringArray;
begin
    result := TDisplayComponentSettingsTypeDictionary.Instance.ReadCompatibleDisplayComponentTypeNamesByType
        ((self.Setting as TDSDisplayComponent).DisplayComponentID);
end;

function TDSEDisplayComponent.GetViewItemType(): TViewItemType;
begin
    result := ntDisplayComponent;
end;

{ TDSEDisplayComponentType }

constructor TDSEDisplayComponentType.Create(aDisplayComponentSetting: TDisplayComponentSetting);
begin
    inherited Create(aDisplayComponentSetting);
    fTypeNames := self.ReadTypeNames();
    // GetTypes();
    // fTypes.Sort;
end;

function TDSEDisplayComponentType.ReadListItems(): TStringArray;
begin
    result := fTypeNames;
end;

{ TDSEDisplayComponentType }

function TDSEDisplayComponentType.ReadTypeNames(): TStringArray;
begin
    result := TDisplayComponentSettingsTypeDictionary.Instance.ReadTypeNames(true);
end;

{ TDSEEvent }

constructor TDSEEvent.Create(aDisplayComponentSetting: TDisplayComponentSetting);
begin
    inherited Create(aDisplayComponentSetting);
end;

function TDSEEvent.HasEditFunction: boolean;
begin
    result := true;
end;

procedure TDSEEvent.EditFunction(aSender: TObject);
var
    xValue: string;
begin
    xValue := self.EditValue;
    if (xValue <> '') then
        (TViewItemsWorkflow.Instance as TDesignerViewItemsWorkflow).OpenEditFormExt(xValue, ntMethod, false,
            aemMethodDevelopment);
end;

function TDSEEvent.ReadListItems(): TStringArray;
var
    xDA: TMethodDataAdaptor;
begin
    xDA := TMethodDataAdaptor.Create;
    try
        result := xDA.ReadAllNames;
    finally
        FreeAndNil(xDA);
    end;
end;

{ TDSEMulti }

constructor TDSEMulti.Create(aSetting: TDSMulti);
begin
    inherited Create(aSetting);

end;

function TDSEMulti.GetMultiSetting: TDSMulti;
begin
    result := self.Setting as TDSMulti;
end;

function TDSEMulti.GetIsAddMenuVisible: boolean;
begin
    result := true;
end;

function TDSEMulti.GetIsDeleteMenuVisible: boolean;
begin
    result := true;
end;

function TDSEMulti.AddNewChildSetting(): TDisplayComponentSetting;
begin
    result := self.MultiSetting.AddNewChildSetting();
end;

procedure TDSEMulti.DeleteChildSetting(const aChildSettingName: string);
begin
    self.MultiSetting.DeleteChildSetting(aChildSettingName);
end;


end.
