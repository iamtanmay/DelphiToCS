{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  : Form that shows the options for a method step
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  17.08.04 wl                               TN2008.3  initial version
  24.08.04 wl                               TN2008.2  mit Laden und Speichern der DockableForms
  24.09.04 wl                               TN2008.2  Laden und Speichern verbessert
  24.09.04 wl                               TN2008.3  Zusammenspiel mit MethEdit-Fenster verbessert
  11.10.04 wl                               TN2008.3  jetzt mit Apply-Button
  21.12.04 wl                               TN2247.1  vollständige Umstellung auf TcxTreeList
  11.03.05 pk  UpdateEditedParameter        TN2339.2  Call gmMessageBox instead of VCLMessageBox
  22.06.05 wl                               TN2440    uses MethodEditor
  28.06.05 wl  FullUpdate                   TN2441    wenn Sub-Properties > '' wird Node expanded gezeigt
  28.06.05 wl  UpdateEditedParameter        TN2441    ruft jetzt EndEdit() auf
  05.07.05 pk  EdButtonClick                TN2494    use new parser if parsertype = new parser
  23.09.05 pk  ChooseRack                   TN2579    call UpdateEditedParameter to update the SRack value in the  methodeditor
  15.10.05 wl  ChooseRackPositions          TN2574    WGHP: "List Index Out Of Bounds" kommt nicht mehr
  20.10.05 wl  InstanceSetFocus,InstanceExists  TN2659    neu, noch nicht genutzt
  03.11.05 wl  FullUpdate,GetParamIndex     TN2724    Der index des MethodStep wird jetzt in der property StateIndex gespeichert (nicht mehr in Texts[2])
  03.11.05 wl  GetParameterFromNode         TN2724    Das Finden des zugehörigen parameters gelingt jetzt auch für Node.Level = 2
  17.11.05 pk  AddEditRepositoryItem        TN2777    For ComboBoxItem add an InitPopUp event which calls RefreshPickList
  24.11.05 pk  cxTreeList1Exit              TN2805    set LastSelected using TZADesignWBEvents
  24.11.05 pk  EdButtonClick                TN2765    OpenPipetteParameter with only one argument
  04.12.05 pk  AddEditRepositoryItem        TN2828    create TcxEditRepositoryComboButtonItem instead of TcxEditRepositoryMRUItem
  04.12.05 pk  EdButtonClick                TN2828    call EndEdit. otherwise old combobox value is used in liqparam editor
  20.01.06 pk  UpdateProperty               TN2891    Changed! Now works recursively. Gets data from Node.Data instead of stateindex
  20.01.06 pk  FullUpdate                   TN2891    Calls AddParam which recursivley updates subnodes
  20.01.06 pk  UpdateEditedParameter        TN2891    Triggers Param.Changed event
  20.01.06 pk  ChooseRackPositions          TN2891    call ChooseRackPos
  20.01.06 pk  ChooseRackPos                TN2891    New
  25.01.06 pk  fLastMethodStepName          TN2899    Changed to fLastMethodStep.  This is set to Null in StepChanged
  02.02.06 pk                               TN2913    cxTreeList1cxTreeListColumn2GetEditingProperties - always set readonly property
  08.08.06 wl  CheckMethodStepHasChanged    TN3254    Wenn der Methodeneditor ReadOnly ist, sind es die Properties auch
  02.10.06 wl  GetMaxLength                 TN3236    bestimmt die Länge der Edit-Felder
  19.12.06 wl                               TN3409    uses ViewItems
  05.11.07 wl                               TN3919    einige Änderungen, damit Read-Only-methoden wirklich nicht mehr editiert werden können
  09.01.08 wl                               TN3972    TCustomSetting statt TMethodStepParameter
  05.03.08 wl  cxTreeList1KeyDown           TN4009    bei TAB wird zwar in die nchste Zeile gesprungen, aber jetzt wird vorher auch der Wert gespeichert
  20.06.08 pk                               TN4139    uses ZADesignLayout
  15.10.08 pk  EdButtonClick                TN4258    EditFunction no longer returns string
  16.01.09 wl                               TN4362   an Änderungen in TViewItem angepasst
  20.02.09 wl  AddParam                     TN4438    Setting.Description statt Caption
  08.05.09 wl  FullUpdate                   TN4555    Description ohne : am Ende
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  03.09.09 wl  cxTreeList1                  TN4800   Anpassungen an Treelist Version 5 (nicht kompatibel)
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  23.11.09 pk  SaveChanges                  TN4896   New: End the edit mode. This is a workaround because The OnExit event does not get fired for the treelist
  07.12.09 ts  UpdateEditedParameter        TN4902   trim Parameter to remove leading and trailing spaces
  15.12.09 pk  ChooseCarrierSlot            TN4948   New
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.07.10 pk  InstanceChooseRackPos        TN5066   New
  21.09.10 pk                               TN5047   Form is no longer dockable. Only opened modal from MethodEditor
  23.09.10 pk                               TN5047   Set focus to first property node when form is shown
  24.09.10 pk                               TN5047   Show Icon and action description at top
  29.09.10 pk                               TN5047   fPropertiesNode = RootNode
  30.09.10 pk  SetEditFocusToFirstProperty  TN5047   AV occured when methodstep had 0 properties
  30.09.10 pk                               TN5287   Search in methods reimplemented
  01.10.10 pk                               TN5287   PropertiesNode is childnode again, being root caused problems
  06.10.10 pk  EdButtonClick                TN5290   More flexible mechanism of calling EditFunction
  07.02.11 wl                               TN5461   deutsche Überschrift
  14.02.11 wl                               TN5468   entrümpelt
  20.07.11 wl  Create                       TN5614   wenn aWrite = false, wird OK-Button nicht gezeigt
  05.03.13 wl                               TN6101   an TCustomSetting angepasst
  06.03.13 wl  cxTreeList1FocusedNodeChanged  TN6103   Die Pickliste wird erst in dem Moment erzeugt, wenn der Focus auf die Zeile kommt
  13.03.13 wl                                 TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  21.03.13 wl                                 TN6045   an TCustomSetting-Änderungen angepasst
  08.08.13 wl                                 TN6103   wenn es keine PickList gibt, wird auch kein DropDown-Element gezeigt
  -------------------------------------------------------------------------------------------------- }

unit EdActionProps;


interface


uses
    Windows,
    Forms,
    Classes,
    Controls,
    ImgList,
    cxGraphics,
    cxLookAndFeels,
    cxControls,
    cxLookAndFeelPainters,
    cxButtons,
    cxEditRepositoryItems,
    cxEdit,
    cxDropDownEdit,
    cxTL,
    cxTextEdit,
    cxInplaceContainer,
    cxStyles,
    cxCustomData,
    StdCtrls,
    ExtCtrls,
    GeneralTypes,
    MethodStep,
    CustomSetting;

type
    TEdActionPropsFocusOptions = record
        SettingKey: TStringArray;
        FocusStart, FocusLen: integer;
    end;

    TfrmActionProps = class(TForm)
        cxTreeList1: TcxTreeList;
        cxLookAndFeelController1: TcxLookAndFeelController;
        cxTreeList1cxTreeListColumn1: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn2: TcxTreeListColumn;
        cxEditRepository1: TcxEditRepository;
        cxTreeList1cxTreeListColumn3: TcxTreeListColumn;
        cxTreeList1cxTreeListColumn4: TcxTreeListColumn;
        Panel2: TPanel;
        Bevel1: TBevel;
        Button2: TButton;
        btnOK: TButton;
        pnlTop: TPanel;
        pnlActionImage: TPanel;
        imgActionImage: TImage;
        pnlTopRight: TPanel;
        edActionName: TEdit;
        pnlTopMiddle: TPanel;
        mmoActionDescription: TMemo;
        procedure FormCreate(Sender: TObject);
        procedure cxTreeList1cxTreeListColumn2GetEditingProperties(Sender: TcxTreeListColumn;
            ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
        procedure cxTreeList1Edited(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
        procedure cxTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormShow(Sender: TObject);
        procedure cxTreeList1FocusedNodeChanged(Sender: TcxCustomTreeList;
            APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    private
        fPropertiesNode: TcxTreeListNode;
        fMethodStep: TMethodStep;
        fMethodStepOptionOldValue: string;
        fImages: TCustomImageList;
        fSpecialFocusOptions: TEdActionPropsFocusOptions;
        procedure LoadImage();
        procedure LoadActionDescription();

        // repository event handlers
        procedure EdButtonClick(aSender: TObject);
        procedure EdButtonClick2(aSender: TObject; aButtonIndex: Integer);
        procedure RefreshPickList(aSender: TObject);

        function GetRepIndex(aNode: TcxTreeListNode): integer;
        function AddEditRepositoryItem(aParam: TCustomSetting; aIndex: integer): integer;
        procedure UpdateEditedParameter(aNode: TcxTreeListNode);
        function GetParameterFromNode(aNode: TcxTreeListNode): TCustomSetting;
        function GetNodeFromParameter(const aParam: TCustomSetting): TcxTreeListNode;
        function GetFocusedNodeAndParam(out oNode: TcxTreeListNode; out oParam: TCustomSetting): boolean;
        function GetFocusedParam(out oParam: TCustomSetting): boolean;
        function GetMaxLength(aParam: TCustomSetting): integer;

        function AddParam(aParam: TCustomSetting; aParentNode: TcxTreeListNode; aIndex: integer): boolean;
        procedure UpdateNode(aNode: TcxTreeListNode);
        procedure MethodStepToGUI();
        procedure GUINodeToMethodStep(aNode: TcxTreeListNode);
        procedure GUIToMethodStep();
        procedure UpdateAfterSetValue(aFunctionParam: TCustomSettingEditFunctionParam);

        procedure SetSpecialEditFocus();
    public
        constructor Create(const aMethodStep: TMethodStep; const aImages: TCustomImageList;
            aWritable: boolean; const aFocusOptions: TEdActionPropsFocusOptions); reintroduce;
        procedure UndoChanges();
        class function InstanceEditMethodStepModal(const aMethodStep: TMethodStep;
            const aImages: TCustomImageList; aWritable: boolean): TModalResult;
        class function InstanceEditMethodStepSpecialFocusModal(const aMethodStep: TMethodStep;
            const aImages: TCustomImageList; aWritable: boolean;
            const aFocusOptions: TEdActionPropsFocusOptions): TModalResult;
    end;


implementation


{$R *.dfm}

uses
    SysUtils,
    MethodStepSettings,
    Utility_DevExpress,
    AppTypes,
    GUIManager,
    ViewItemsWorkflow,
    CustomLeafSettings,
    ControlUtils;

{ TfrmActionProps }

constructor TfrmActionProps.Create(const aMethodStep: TMethodStep; const aImages: TCustomImageList;
    aWritable: boolean; const aFocusOptions: TEdActionPropsFocusOptions);
begin
    inherited Create(nil);
    fMethodStep := aMethodStep;
    fMethodStepOptionOldValue := fMethodStep.M_Options;
    fImages := aImages;
    fSpecialFocusOptions := aFocusOptions;

    TControlUtils.ResetFontForWinXP(self);

    self.Caption := TLanguageString.Read('Action properties', 'Aktionseigenschaften');
    cxTreeList1cxTreeListColumn1.Caption.Text := TLanguageString.Read('Property', 'Eigenschaft');
    cxTreeList1cxTreeListColumn1.Caption.Text := TLanguageString.Read('Value', 'Wert');
    btnOK.Visible := aWritable;
end;

procedure TfrmActionProps.UndoChanges();
begin
    fMethodStep.M_Options := fMethodStepOptionOldValue;
end;

procedure TfrmActionProps.FormCreate(Sender: TObject);
begin
    fPropertiesNode := cxTreeList1.Add;
end;

class function TfrmActionProps.InstanceEditMethodStepSpecialFocusModal(const aMethodStep: TMethodStep;
    const aImages: TCustomImageList; aWritable: boolean; const aFocusOptions: TEdActionPropsFocusOptions)
    : TModalResult;
var
    xForm: TfrmActionProps;
begin
    xForm := TfrmActionProps.Create(aMethodStep, aImages, aWritable, aFocusOptions);
    try
        xForm.MethodStepToGUI();
        result := xForm.ShowModal();
        if result <> mrOK then
        begin
            xForm.UndoChanges();
            EXIT;
        end;

        xForm.GUIToMethodStep();
    finally
        FreeAndNil(xForm);
    end;

end;

class function TfrmActionProps.InstanceEditMethodStepModal(const aMethodStep: TMethodStep;
    const aImages: TCustomImageList; aWritable: boolean): TModalResult;
var
    xFocusOptions: TEdActionPropsFocusOptions;
begin
    SetLength(xFocusOptions.SettingKey, 0);

    result := TfrmActionProps.InstanceEditMethodStepSpecialFocusModal(aMethodStep, aImages, aWritable,
        xFocusOptions);
end;

procedure TfrmActionProps.FormShow(Sender: TObject);
begin
    self.SetSpecialEditFocus();
end;

function TfrmActionProps.GetParameterFromNode(aNode: TcxTreeListNode): TCustomSetting;
begin
    result := TCustomSetting(aNode.Data);
end;

function TfrmActionProps.GetNodeFromParameter(const aParam: TCustomSetting): TcxTreeListNode;
begin
    ASSERT(Assigned(aParam) and (aParam.ViewData is TcxTreeListNode));
    result := (aParam.ViewData as TcxTreeListNode);
end;

procedure TfrmActionProps.SetSpecialEditFocus();
var
    xPropNode: TcxTreeListNode;
    xEditControl: TcxCustomTextEdit;
    xFocusStart, xFocusLen: integer;
    xSetting: TCustomSetting;
    xFocusedSetting: TCustomSetting;
begin
    self.cxTreeList1.SetFocus;
    self.cxTreeList1.Columns[1].Focused := true;
    if fPropertiesNode.ChildVisibleCount <= 0 then
        EXIT;

    if Length(self.fSpecialFocusOptions.SettingKey) > 0 then
    begin
        xSetting := self.GetParameterFromNode(fPropertiesNode);
        Assert((xSetting is TCustomCompositeSetting));
        xFocusedSetting := (xSetting as TCustomCompositeSetting)
            .FindRecursive(self.fSpecialFocusOptions.SettingKey, high(self.fSpecialFocusOptions.SettingKey));
        if not Assigned(xFocusedSetting) then
            raise Exception.Create(TTypeSafeFormat.Format('Property {0} not found',
                [fSpecialFocusOptions.SettingKey]));
        xPropNode := GetNodeFromParameter(xFocusedSetting);
        xFocusStart := fSpecialFocusOptions.FocusStart - 1;
        xFocusLen := fSpecialFocusOptions.FocusLen;
    end
    else
    begin
        xPropNode := fPropertiesNode.GetFirstChildVisible();
        xFocusStart := 0;
        xFocusLen := 0;
    end;

    xPropNode.Focused := true;

    self.cxTreeList1.ShowEdit;
    // now we can set the Selstart and SelLength properties of the Edit control
    if (self.cxTreeList1.InplaceEditor is TcxCustomTextEdit) then
    begin
        xEditControl := (self.cxTreeList1.InplaceEditor as TcxCustomTextEdit);
        xEditControl.SetSelection(xFocusStart, xFocusLen);
    end;
end;

procedure TfrmActionProps.GUINodeToMethodStep(aNode: TcxTreeListNode);
var
    x: integer;
begin
    UpdateEditedParameter(aNode);

    for x := 0 to aNode.Count - 1 do
    begin
        GUINodeToMethodStep(aNode.Items[x]);
    end;
end;

procedure TfrmActionProps.GUIToMethodStep();
begin
    if (fMethodStep = nil) then
        EXIT;
    if (fMethodStep.Settings.Params.Count <= 0) then
        EXIT;

    // Parameter zeigen
    GUINodeToMethodStep(fPropertiesNode);
end;

procedure TfrmActionProps.UpdateNode(aNode: TcxTreeListNode);
var
    x: integer;
begin
    aNode.Texts[1] := TCustomSetting(aNode.Data).Value;

    for x := 0 to aNode.Count - 1 do
    begin
        UpdateNode(aNode.Items[x]);
    end;
end;

function TfrmActionProps.AddParam(aParam: TCustomSetting; aParentNode: TcxTreeListNode;
    aIndex: integer): boolean;
var
    x: integer;
    xNode: TcxTreeListNode;
    xNextParentNode: TcxTreeListNode;
begin
    result := false;

    // add me to the tree if I am visible
    if (aIndex >= 0) and (aParam.Visible) then
    begin
        xNode := aParentNode.AddChild();
        xNode.ImageIndex := -1;
        xNode.SelectedIndex := xNode.ImageIndex;
        xNode.Texts[0] := aParam.Description;
        xNode.Texts[1] := aParam.Value;

        if (xNode.Texts[1] <> '') then
            result := true;

        // the node has a link to the param
        xNode.Data := aParam;
        // the param has a link to the node
        aParam.ViewData := xNode;

        // wichtig: für jeden Parameter wird ein Item in Repository angelegt!
        xNode.Texts[3] := IntToStr(AddEditRepositoryItem(aParam, aIndex));
        xNextParentNode := xNode;
    end
    else
    begin
        // if am invisible just add my subnodes
        result := true;
        xNextParentNode := aParentNode;
    end;

    if not(aParam is TMethodStepCompositeSetting) then
        EXIT;

    for x := 0 to (aParam as TMethodStepCompositeSetting).Params.Count - 1 do
    begin

        if AddParam((aParam as TMethodStepCompositeSetting).Params[x], xNextParentNode, x) then
            result := true;
    end;
    if (result) then
        xNextParentNode.Expand(false);
end;

procedure TfrmActionProps.LoadImage();
begin
    fImages.Draw(self.imgActionImage.Canvas, 0, 0, fMethodStep.StepInfo.IconIndex);
end;

procedure TfrmActionProps.LoadActionDescription();
begin
    self.edActionName.Text := fMethodStep.GetDefaultName;
    self.mmoActionDescription.Lines.Clear;
    self.mmoActionDescription.Lines.Add(fMethodStep.GetActionCaption);
    self.mmoActionDescription.Lines.Add(fMethodStep.GetActionDescription);
end;

procedure TfrmActionProps.MethodStepToGUI();
begin
    cxEditRepository1.Clear;
    fPropertiesNode.DeleteChildren();

    if (fMethodStep = nil) then
        EXIT;

    LoadImage();
    LoadActionDescription();

    if (fMethodStep.Settings.Params.Count <= 0) then
        EXIT;
    fPropertiesNode.Data := fMethodStep.Settings;

    AddParam(fMethodStep.Settings, fPropertiesNode, -1);
end;

function TfrmActionProps.GetMaxLength(aParam: TCustomSetting): integer;
begin
    result := aParam.MaxLength;
end;

function TfrmActionProps.AddEditRepositoryItem(aParam: TCustomSetting; aIndex: integer): integer;
var
    xItem: TcxEditRepositoryItem;
begin
    if (aParam.HasPickListFunction) then
    begin
        if (aParam.HasEditFunction) then
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryComboButtonItem);
            // (xItem as TcxEditRepositoryComboButtonItem).Properties.OnInitPopup := RefreshPickList;
            (xItem as TcxEditRepositoryComboButtonItem).Properties.OnButtonClick := EdButtonClick;
            (xItem as TcxEditRepositoryComboButtonItem).Properties.MaxLength := self.GetMaxLength(aParam);
            result := cxEditRepository1.Count - 1;
        end
        else
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryComboBoxItem);
            (xItem as TcxEditRepositoryComboBoxItem).Properties.OnInitPopup := RefreshPickList;
            (xItem as TcxEditRepositoryComboBoxItem).Properties.MaxLength := self.GetMaxLength(aParam);
            result := cxEditRepository1.Count - 1;
        end;
    end
    else
    begin
        if (aParam.HasEditFunction) then
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryButtonItem);
            (xItem as TcxEditRepositoryButtonItem).Properties.OnButtonClick := EdButtonClick2;
            (xItem as TcxEditRepositoryButtonItem).Properties.MaxLength := self.GetMaxLength(aParam);
            result := cxEditRepository1.Count - 1;
        end
        else
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryTextItem);
            (xItem as TcxEditRepositoryTextItem).Properties.MaxLength := self.GetMaxLength(aParam);
            result := cxEditRepository1.Count - 1;
        end;
    end;
end;

function TfrmActionProps.GetFocusedNodeAndParam(out oNode: TcxTreeListNode;
    out oParam: TCustomSetting): boolean;
// returns true if vParam is assigned
begin
    result := false;
    oNode := cxTreeList1.FocusedNode;
    ASSERT(Assigned(oNode), 'FocuesdNode is nil');
    oParam := self.GetParameterFromNode(oNode);
    if not Assigned(oParam) then
        EXIT;
    result := true;
end;

function TfrmActionProps.GetFocusedParam(out oParam: TCustomSetting): boolean;
var
    xDummy: TcxTreeListNode;
begin
    result := GetFocusedNodeAndParam(xDummy, oParam);
end;

function TfrmActionProps.GetRepIndex(aNode: TcxTreeListNode): integer;
var
    xIntText: string;
begin
    result := -1;
    xIntText := aNode.Texts[3];
    if (xIntText = '') then
        EXIT;

    result := StrToInt(aNode.Texts[3]);
end;

procedure TfrmActionProps.UpdateEditedParameter(aNode: TcxTreeListNode);
var
    xPropName: string;
    xParam: TCustomSetting;
begin
    xPropName := aNode.Texts[0];
    try
        xParam := self.GetParameterFromNode(aNode);
        if not Assigned(xParam) then
            EXIT;
        xParam.Value := trim(aNode.Texts[1]);
        xParam.Changed();
    except
        on E: ESetCustomSettingValueException do
        begin
            gGUIManager.MessageBox(E.Message, Format('Property %s is not valid', [xPropName]), MB_ICONSTOP);
        end;
        else
            raise;
    end;
end;

procedure TfrmActionProps.cxTreeList1Edited(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
begin
    // aktuellen Parameter updaten
    UpdateEditedParameter(cxTreeList1.FocusedNode);
end;

procedure TfrmActionProps.cxTreeList1FocusedNodeChanged(Sender: TcxCustomTreeList;
    APrevFocusedNode, AFocusedNode: TcxTreeListNode);
var
    xRepIndex: integer;
    xStrings: TStringArray;
begin
    xRepIndex := GetRepIndex(AFocusedNode);
    if (xRepIndex < 0) then
        EXIT;

    if (cxEditRepository1.Items[xRepIndex].Properties is TcxCustomComboBoxProperties) then
    begin
        xStrings := TCustomSetting(aFocusedNode.Data).GetPickList();
        // etwas umständlich, aber wie soll es sonst gehen
        if (Length(xStrings) > 0) then
            TcxUtils.AddValuesToComboBoxProps(xStrings,
                cxEditRepository1.Items[xRepIndex].Properties as TcxCustomComboBoxProperties);
    end;
end;

procedure TfrmActionProps.cxTreeList1cxTreeListColumn2GetEditingProperties(Sender: TcxTreeListColumn;
    aNode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
var
    xRepIndex: integer;
begin
    xRepIndex := GetRepIndex(aNode);
    if (xRepIndex < 0) then
        EXIT;

    EditProperties := cxEditRepository1.Items[xRepIndex].Properties;
end;

procedure TfrmActionProps.RefreshPickList(aSender: TObject);
var
    xParameter: TCustomSetting;
    xList: TStrings;
    xValues: TStringArray;
    x: integer;
begin
    if not GetFocusedParam(xParameter) then
        EXIT;

    xList := nil;
    if aSender is TcxComboBox then
        xList := (aSender as TcxComboBox).Properties.Items;

    if not Assigned(xList) then
        EXIT;

    xList.Clear;

    xValues := xParameter.GetPickList();
    for x := 0 to Length(xValues) - 1 do
        xList.Add(xValues[x]);
end;

procedure TfrmActionProps.UpdateAfterSetValue(aFunctionParam: TCustomSettingEditFunctionParam);
var
    xNode: TcxTreeListNode;
begin
    xNode := self.GetNodeFromParameter(aFunctionParam.Param);
    self.UpdateNode(xNode);
end;

procedure TfrmActionProps.EdButtonClick(aSender: TObject);
var
    xFunctionParams: TCustomSettingEditFunctionParams;
    xParameter: TCustomSetting;
    xNode: TcxTreeListNode;
    xCancel: boolean;
begin
    if not GetFocusedNodeAndParam(xNode, xParameter) then
        EXIT;

    if (xParameter is TCustomSetting_LiqParam) then
    begin
        cxTreeList1.FocusedNode.EndEdit(false);
        TViewItemsWorkflow.Instance.OpenPipetteParameter(xNode.Texts[1]);
    end
    else
    begin
        xFunctionParams := xParameter.CreateEditFunctionParams();
        try
            cxTreeList1.FocusedNode.EndEdit(false);

            xParameter.EditFunction(xFunctionParams, xCancel);
            if xCancel then
                EXIT;
            if Assigned(xFunctionParams) then
                xFunctionParams.SetAllValuesToNewValue(UpdateAfterSetValue);
        finally
            FreeAndNil(xFunctionParams);
        end;
    end;
end;

procedure TfrmActionProps.EdButtonClick2(aSender: TObject; aButtonIndex: Integer);
begin
    EdButtonClick(aSender);
end;

procedure TfrmActionProps.cxTreeList1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if (Key = VK_TAB) then
    begin
        if Assigned(cxTreeList1.FocusedNode) then
        begin
            if (cxTreeList1.FocusedNode.VisibleIndex + 1 < cxTreeList1.VisibleCount) then
            begin

                // aktuellen Parameter updaten
                UpdateEditedParameter(cxTreeList1.FocusedNode);

                cxTreeList1.FocusedNode := cxTreeList1.AbsoluteItems
                    [cxTreeList1.FocusedNode.VisibleIndex + 1];
            end;
        end;
    end;
end;


end.
