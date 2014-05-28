{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  10.12.12 wl                                      TN6045   Initial Revision
  27.03.13 wl                                      TN6045   andere Darstellung als im Designer
  04.04.13 wl  AddMethod                           TN6045   macht sofort die Parameterabfrage auf
  30.08.13 wl  sbChangeSettingsClick               TN6236   neu: Settings-Fenster statt bisher Layout und Comments getrennt
  03.09.13 wl  ShowMethodStepCommentInColumn       TN6236   Kommentare erscheinen wieder wie vorher
  05.12.13 wl  ResetData                           TN6045.1 wird von FirstLoad aufgerufen
  05.12.13 wl  ResetData                           TN6045.1 No-Layout-Problem gelöst
  ------------------------------------------------------------------------------------------------------ }

unit BuildingBlockMethodEditor;


interface


uses
    Windows,
    Generics.Collections,
    Classes,
    Menus,
    ImgList,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    Buttons,
    ExtCtrls,
    cxGraphics,
    cxCustomData,
    cxStyles,
    cxTL,
    cxTextEdit,
    cxControls,
    cxInplaceContainer,
    cxEdit,
    cxEditRepositoryItems,
    cxFilter,
    cxData,
    cxDataStorage,
    cxDBData,
    cxGridCustomTableView,
    cxGridTableView,
    cxGridDBTableView,
    cxGridLevel,
    cxClasses,
    cxGridCustomView,
    cxGrid,
    cxImage,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    //
    DockableForm,
    ViewItemEditForm,
    MethodStep,
    CustomSetting,
    ViewItem,
    MethodEditDataHelper,
    MethodEditForm,
    AppTypes,
    StringLoader,
    GeneralTypes;

type
    TMethodEditorGetAddMethodIndexEvent = function(const aMethodName: string): integer of object;

    TfrmBuildingBlockMethodEditor = class(TMethodEditForm)
        Panel3: TPanel;
        PopupMenu1: TPopupMenu;
        mnuCutItem: TMenuItem;
        mnuCopyItem: TMenuItem;
        mnuPasteItem: TMenuItem;
        cxGrid1Level1: TcxGridLevel;
        cxGrid1: TcxGrid;
        cxGrid1TableView1: TcxGridTableView;
        cxGrid1TableView1ColumnSummary: TcxGridColumn;
        cxGrid1TableView1ColumnComment: TcxGridColumn;
        Panel1: TPanel;
        mnuAddEmptyLine: TMenuItem;
        mnuDeleteLines: TMenuItem;
        sbDeleteRecord: TSpeedButton;
        sbChangeSettings: TSpeedButton;
        pmnuDeactivateSelectedLines: TMenuItem;
        pmnuActivateSelectedLines: TMenuItem;
        pnlStatus: TPanel;
        pnlCurLineNumber: TPanel;
        lblCurLineNumber: TLabel;
        edCurLineNumber: TEdit;
        Panel2: TPanel;
        Panel4: TPanel;
        edModifiedStatus: TEdit;
        Panel5: TPanel;
        edAttribute: TEdit;
        Memo1: TMemo;
        ImageList1: TImageList;
        mnuEditComment: TMenuItem;
        sbCheckMethod: TSpeedButton;
        ImageListWizard: TImageList;
        mnuOpenItem: TMenuItem;
        N4: TMenuItem;
        MainImageList: TImageList;
        mnuOpenProperties: TMenuItem;
        sbMethVaraibles: TSpeedButton;
        procedure FormShow(Sender: TObject);
        procedure cxGrid1TableView1DragDrop(Sender, Source: TObject; aX, aY: Integer);
        procedure cxGrid1TableView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
            var Accept: Boolean);
        procedure cxGrid1TableView1FocusedRecordChanged(Sender: TcxCustomGridTableView;
            APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
        procedure cxGrid1TableView1SelectionChanged(Sender: TcxCustomGridTableView);
        procedure cxGrid1TableView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure cxGrid1TableView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
        procedure mnuAddEmptyLineClick(Sender: TObject);
        procedure mnuDeleteLinesClick(Sender: TObject);
        procedure sbDeleteRecordClick(Sender: TObject);
        procedure cxGrid1TableView1DataControllerAfterDelete(ADataController: TcxCustomDataController);
        procedure sbChangeSettingsClick(Sender: TObject);
        procedure cxGrid1TableView1CellDblClick(Sender: TcxCustomGridTableView;
            ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState;
            var AHandled: Boolean);
        procedure PopupMenu1Popup(Sender: TObject);
        procedure cxGrid1TableView1CustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
            AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
        procedure pmnuDeactivateSelectedLinesClick(Sender: TObject);
        procedure pmnuActivateSelectedLinesClick(Sender: TObject);
        procedure cxGrid1Resize(Sender: TObject);
        procedure cxGrid1TableView1CanSelectRecord(Sender: TcxCustomGridTableView;
            ARecord: TcxCustomGridRecord; var AAllow: Boolean);
        procedure cxGrid1TableView1GetCellHeight(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
            AItem: TcxCustomGridTableItem; ACellViewInfo: TcxGridTableDataCellViewInfo; var AHeight: Integer);
        procedure mnuEditCommentClick(Sender: TObject);
        procedure cxGrid1TableView1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure sbCheckMethodClick(Sender: TObject);
        procedure mnuOpenItemClick(Sender: TObject);
        procedure mnuCopyItemClick(Sender: TObject);
        procedure mnuPasteItemClick(Sender: TObject);
        procedure mnuCutItemClick(Sender: TObject);
        procedure mnuOpenPropertiesClick(Sender: TObject);
        procedure sbMethVaraiblesClick(Sender: TObject);

    private const
        cIndentWidth = 20;
        cFirstXOffset = 8;
    private
        fActionImages: TImageList;
        fAddMethodImages: TImageList;
        FInternUpdate: boolean;
        fStringLoader: TMethodEditorStringLoader;
        fColumnMode: TMethodEditorColumnMode;
        fIsSelecting: boolean;
        fPrevMousePos: TPoint;
        fOnGetAddMethodIndex: TMethodEditorGetAddMethodIndexEvent;

        procedure DefineMenuItems(aX, aY: Integer);
        procedure CheckClipboardPaste();
        function GetSavedInsertRecIndex: integer;
        procedure SelectAllData(aSender: TObject);

        function GetMethodName: string;
        procedure CheckSelectedLineLevels();
        procedure MoveSelectedRecords();
        procedure CopySelectedRecords();
        procedure PasteSelectedRecords(aInsertRecIndex: integer);
        procedure DeleteSelectedRecords(var vFocusIndex: integer);
        procedure DeleteSelectedRecords_Intern(var vFocusIndex: integer);
        procedure EditCut;
        procedure EditCopy;
        procedure EditPaste;
        procedure WriteAttributeText();
        procedure DeterminePopupEnabled();

        // procedure AskActionNameAndAdd(aRecIndex: integer);
        procedure DefineCurrentMethodStep();
        procedure SetCurLineNumberDisplay(aRecIndex: integer);

        procedure SetColumnMode(const Value: TMethodEditorColumnMode);
        procedure CustomizeView;

        procedure DeleteEditLines(const aFirstIndex, aLastIndex: integer);

        function GetLineStartX(const aMethodLine: TMethodEditLine): integer;
        function GetLineIconX(const aMethodLine: TMethodEditLine): integer;
        function IsXOnLineIcon(const aX: integer; const aMethodLine: TMethodEditLine): boolean;

        procedure EditMethodStepPropertiesOrComment(const aIndex: integer);
        procedure OpenMethodStepRelatedEditor(const aIndex: integer);
        procedure EditCellProperties(const aRecordIndex: integer; const aColIndex: integer;
            const aEditRelatedProperties: boolean);

        procedure DrawComment(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
            const aIsSummaryCol: boolean; const aIsSelected: boolean; var vRect: TRect);
        procedure DrawGroupBrackets(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
            const aRect: TRect);

        procedure DrawIconOrShape(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
            const aIsSelected: boolean; var vRect: TRect);

        procedure DrawSummaryText(const aCanvas: TcxCanvas; const aMethodLine: TMethodEditLine;
            const aIsSelected: boolean; var vRect: TRect);

        procedure SetSelectedLinesInactive(const aInactive: boolean);

        class function CalculateTextHeight(const aText: string; const aCanvas: TCxCanvas;
            const aPossibleCellWidth: integer): integer;

        function ShowMethodStepCommentInColumn(const aMethodStep: TMethodStep): integer;
        function CreateClipboardData(const aFirstRecordIndex: integer = 0): TClipboardDataMethodSteps;
        procedure DoClipboardDataOnLoadFromStream(const aMethodSteps: TObjectList<TMethodStep>;
            const aFirstRecordIndex: integer);
        procedure DoClipboardDataOnSaveToStream(const aMethodSteps: TObjectList<TMethodStep>);
    protected
        function GetAttribute: TVisibilityAttribute; override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
        procedure ResetData(); override;
        function CanClose(): boolean; override;
        function CheckLastChange(): boolean; override;
        procedure SaveStatusChanged; override;
        function EditMethodStepModal(const aMethodStep: TMethodStep): boolean; override;
        function GetInsertRecIndex: integer; override;
        procedure SaveData(); override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string; aAddMethodImages: TImageList;
            aOnSaveStatusChanged: TNotifyEvent; aOnGetAddMethodIndex: TMethodEditorGetAddMethodIndexEvent);
            reintroduce; virtual;
        destructor Destroy; override;
        //
        procedure FirstLoad(); override;
        function EditAction(aSender: TObject; aEditAction: TEditActionType): boolean; override;
        procedure SelectEditor(); override;

        function AddMethodStep(const aActionName, aOption: string; aRecIndex: integer;
            aSetFocus: boolean): integer;
        procedure AddMethod(const aMethodName: string); overload;
        procedure AddMethod(const aMethodName: string; aRecIndex: integer); overload;
        procedure AddMethodParallel(const aMethodName: string); overload;
        procedure AddMethodParallel(const aMethodName: string; aRecIndex: integer); overload;
        procedure AddWashProg(const aWashProgName: string; aRecIndex: integer);
        procedure UpdateMethodAttributes(); override;
        function Build(): boolean; override;
        procedure EditSetFocusToRow(aRow: integer); override;
        procedure EditSetFocusToTextPart(const aRow: integer; const aSettingKey: TStringArray;
            const aFocusStart, aFocusLen: integer); override;
        //
        property InternUpdate: boolean read FInternUpdate;
        property PMethodName: string read GetMethodName;
        property Attribute: TVisibilityAttribute read GetAttribute;
        property ColumnMode: TMethodEditorColumnMode read fColumnMode write SetColumnMode;
    end;


implementation


uses
    StrUtils,
    SysUtils,
    ComCtrls,
    IfRunStep,
    ForRunStep,
    WhileRunStep,
    BlankRunStep,
    AddMethodRunStep,
    Graphics,
    GUIManager,
    MethodDataAdaptor,
    MethodTypes,
    ZARunnerMain,
    DialogUtils,
    RunStepInfoTypeDictionary,
    ImportDataAdaptor,
    ViewItemsWorkflow,
    MethodDataCache,
    CustomLeafSettings,
    BuildingBlockEditFunctions,
    BuildingBlockMethodViewItem,
    MethodCompile,
    MethodViewItem,
    MethodSettingsDataAdaptor,
    MethodStepDataFields,
    RunStepBuilderTypeDictionary,
    DisplayComponentSettingsManager,
    DisplayComponentIntf,
    ControlUtils,
    ParamStoreRunStepBuilder,
    MethodStepCommentEditor;

{$R *.DFM}

var
    uGridColumn: TcxGridColumn;
    uGridRecord: TcxCustomGridRecord;

    { TfrmMethodEditor }

constructor TfrmBuildingBlockMethodEditor.Create(aOwner: TComponent; const aItemName: string;
    aAddMethodImages: TImageList; aOnSaveStatusChanged: TNotifyEvent;
    aOnGetAddMethodIndex: TMethodEditorGetAddMethodIndexEvent);
begin
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged,
        MainForm.EditFunctions.Parameter_OnAddEditFunctions);

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TMethodEditorStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    SetGridPointer(self.cxGrid1, self.cxGrid1TableView1);

    edAttribute.Text := '';
    FInternUpdate := true;
    fAddingNewRecord := false;
    fHasUndoFunction := true;
    fHasClipboardFunction := true;
    self.OnSelectAll := SelectAllData;
    // caption MUST be set AFTER loadlanguage otherwise caption=''
    Caption := GetCaption();

    FInternUpdate := false;

    self.cxGrid1Resize(nil);

    fActionImages := TViewItemsWorkflow.Instance.ActionImages.ActionImages64;
    fAddMethodImages := aAddMethodImages;
    fOnGetAddMethodIndex := aOnGetAddMethodIndex;
end;

function TfrmBuildingBlockMethodEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TBuildingBlockMethodViewItem.Create(aItemName);
end;

procedure TfrmBuildingBlockMethodEditor.FirstLoad();
begin
    FInternUpdate := true;

    InsertNewItemRowLine();

    ResetData();

    SetFocusAndSelectRecord(0);

    cxGrid1TableView1.OnGetCellHeight := cxGrid1TableView1GetCellHeight;

    FInternUpdate := false;
end;

function TfrmBuildingBlockMethodEditor.Build(): boolean;
// var
// xSavedCursor: integer;
begin
    result := false;
    if not RequestSaveChanges() then
        EXIT;
    {
      frmEdMain.actViewCompilerMessagesExecute(nil);

      xSavedCursor := gGUIManager.SetCursor(crHourglass);
      try
      TMethodCompile.InstCompileJustCheck(fMethodName, frmEdMain.MethodDevelopment.CompilerMessagesStart);
      finally
      gGUIManager.SetCursor(xSavedCursor);
      end;

      result := (frmEdMain.MethodDevelopment.CompilerMessagesLoad = 0);
    } end;

procedure TfrmBuildingBlockMethodEditor.SelectEditor();
var
    xRec: integer;
begin
    // Problems occured with the copy/paste popup and mainform edit menu, so we have to select a record and call determinepopupenabled
    if cxGrid1.CanFocus and (not cxGrid1.IsFocused) then
    begin
        cxGrid1.SetFocus;

        xRec := cxGrid1TableView1.DataController.FocusedRecordIndex;
        SetFocusAndSelectRecord(xRec);
    end;

    DeterminePopupEnabled();
end;

procedure TfrmBuildingBlockMethodEditor.DeleteEditLines(const aFirstIndex, aLastIndex: integer);
var
    x: integer;
    xEditLine: TMethodEditLine;
    xBeginGroupEditLine: TMethodEditGroupBeginLine;
begin
    for x := aLastIndex downto aFirstIndex do
    begin
        if not MethodEditLineExists(x) then
            CONTINUE;

        xEditLine := fMethodEditLines[x];

        xBeginGroupEditLine := xEditLine.Parent as TMethodEditGroupBeginLine;
        cxGrid1TableView1.DataController.DeleteRecord(x);
        xBeginGroupEditLine.RemoveLine(xEditLine);
        fMethodEditLines.Remove(xEditLine);
        ChangeData();
    end;
end;

procedure TfrmBuildingBlockMethodEditor.SetCurLineNumberDisplay(aRecIndex: integer);
var
    xCurLineNumber: integer;
begin
    xCurLineNumber := aRecIndex + 1;
    edCurLineNumber.Text := IntToStr(xCurLineNumber);
end;

procedure TfrmBuildingBlockMethodEditor.DefineCurrentMethodStep();
var
    xRecIndex: integer;
begin
    xRecIndex := cxGrid1TableView1.DataController.FocusedRecordIndex;
    SetCurLineNumberDisplay(xRecIndex);
end;

procedure TfrmBuildingBlockMethodEditor.FormShow(Sender: TObject);
begin
    if cxGrid1.CanFocus then
        cxGrid1.SetFocus;
end;

function TfrmBuildingBlockMethodEditor.CanClose(): boolean;
var
    xAskClose: integer;
begin
    result := true;
    if (self.IsDeleted) then
        EXIT;
    if cxGrid1TableView1.DataController.RecordCount > 0 then
        EXIT;

    xAskClose := Application.MessageBox
        (PChar(Format('Method %s contains no data and will be deleted after closing the editor.',
        [fViewItem.name]) + #13#10 + 'Close editor and delete method?'), 'No data',
        MB_OKCANCEL + MB_ICONWARNING);
    result := (xAskClose = ID_OK);
    if not result then
        EXIT;
    // frmEdMain.UpdateTV([ntMethod]);
end;

function TfrmBuildingBlockMethodEditor.CheckLastChange(): boolean;
var
    xRecIndex: integer;
begin
    result := true;

    // Editoren, die gerade benutzt werden, schließen!
    xRecIndex := cxGrid1TableView1.DataController.FocusedRecordIndex;
    if (xRecIndex < 0) then
    begin
        EXIT;
    end;

    // TBD_WL: Diese Lösung ist noch nicht ideal, weil der Fokus immer versetzt wird. Nötig wäre
    // dies aber nur, wenn cxGrid1 noch den Fokus hat.
end;

function TfrmBuildingBlockMethodEditor.EditMethodStepModal(const aMethodStep: TMethodStep): boolean;
var
    xParameter: TCustomSetting;
begin
    if not(aMethodStep is TAddMethodMethodStep) then
        EXIT(false);

    xParameter := (aMethodStep as TAddMethodMethodStep).MainSubOptionSetting.MethParams;
    EXIT(EditMethodStepModalIntern(xParameter));
end;

procedure TfrmBuildingBlockMethodEditor.EditMethodStepPropertiesOrComment(const aIndex: integer);
var
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
begin
    if not MethodEditLineExists(aIndex) then
        EXIT;

    xMethodLine := fMethodEditLines[aIndex];

    xMethodStep := xMethodLine.Value;
    if xMethodStep is TBlankMethodStep then
    begin
        EditMethodStepComment(aIndex);
    end
    else
    begin
        EditMethodStepProperties(aIndex);
    end;
end;

procedure TfrmBuildingBlockMethodEditor.OpenMethodStepRelatedEditor(const aIndex: integer);
var
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
    xRelatedItemParam: TCustomSetting;
    xSubItemName: string;
begin
    if not MethodEditLineExists(aIndex) then
        EXIT;

    xMethodLine := fMethodEditLines[aIndex];
    xMethodStep := xMethodLine.Value;

    xRelatedItemParam := xMethodStep.RelatedItemParam;
    if Assigned(xRelatedItemParam) then
    begin
        xSubItemName := xRelatedItemParam.Value;
        if (xSubItemName = '') then
            EXIT;

        xRelatedItemParam.OpenFunction();
    end;
end;

procedure TfrmBuildingBlockMethodEditor.mnuAddEmptyLineClick(Sender: TObject);
begin
    if not CheckLastChange() then
        EXIT;
    self.AddAction('BLANK', GetInsertRecIndex());
end;

procedure TfrmBuildingBlockMethodEditor.mnuCopyItemClick(Sender: TObject);
begin
    EditAction(Sender, eaCopy);
end;

procedure TfrmBuildingBlockMethodEditor.mnuCutItemClick(Sender: TObject);
begin
    EditAction(Sender, eaCut);
end;

procedure TfrmBuildingBlockMethodEditor.AddMethod(const aMethodName: string; aRecIndex: integer);
var
    xMethodStep: TMethodStep;
    xCommentShort, xCommentLong: string;
begin
    xMethodStep := self.CreateMethodStep(cActionAddMethod, aMethodName);

    // TMethodDataCache.Instance.Settings.LoadComments(aMethodName, xCommentShort, xCommentLong);
    xMethodStep.M_Comments := xCommentShort + #13 + xCommentLong;

    self.AddMethodStepLine(xMethodStep, aRecIndex, true);
    self.EditMethodStepProperties(aRecIndex);
    ForceGridResize();
end;

procedure TfrmBuildingBlockMethodEditor.AddMethod(const aMethodName: string);
begin
    AddMethod(aMethodName, self.GetInsertRecIndex());
end;

procedure TfrmBuildingBlockMethodEditor.AddMethodParallel(const aMethodName: string; aRecIndex: integer);
const
    cThreadStartActionName = 'THRST'; // redundant!!
var
    xMethodStep: TMethodStep;
    xCommentShort, xCommentLong: string;
begin
    xMethodStep := self.CreateMethodStep(cThreadStartActionName, aMethodName);

    // TMethodSettings.LoadComments(aMethodName, xCommentShort, xCommentLong);
    xMethodStep.M_Comments := xCommentShort + #13 + xCommentLong;

    self.AddMethodStepLine(xMethodStep, aRecIndex, true);
    SetFocusAndSelectRecord(aRecIndex);
    ForceGridResize();
end;

procedure TfrmBuildingBlockMethodEditor.AddMethodParallel(const aMethodName: string);
begin
    AddMethodParallel(aMethodName, self.GetInsertRecIndex());
end;

procedure TfrmBuildingBlockMethodEditor.AddWashProg(const aWashProgName: string; aRecIndex: integer);
const
    cActionNameWashProgram = 'WASHP'; // redundant!!
var
    xRecIndex: integer;
    xMethodStep: TMethodStep;
begin
    if aRecIndex = -1 then
        xRecIndex := self.GetInsertRecIndex()
    else
        xRecIndex := aRecIndex;

    xMethodStep := self.CreateMethodStep(cActionNameWashProgram, aWashProgName);

    self.AddMethodStepLine(xMethodStep, xRecIndex, true);
    SetFocusAndSelectRecord(aRecIndex);
    ForceGridResize();
end;

procedure TfrmBuildingBlockMethodEditor.ResetData;
begin
    LoadViewAndMethodData(Memo1);
    MainForm.EditFunctions.MethodLayoutName := fSettings.LayoutName;
end;

function TfrmBuildingBlockMethodEditor.EditAction(aSender: TObject; aEditAction: TEditActionType): boolean;
var
    xIsEditingSingleCell: boolean;
begin
    xIsEditingSingleCell := cxGrid1TableView1.Controller.IsEditing;

    // We only want to overwrite the actions for Row editing operations
    // We also dont do anything if sender is not a child control of cxGrid1TableView1
    result := (not(xIsEditingSingleCell));
    // and cxGrid1TableView1.Control.ContainsControl(aSender as TControl);
    if not result then
        EXIT;

    case aEditAction of
        eaCut:
            EditCut;
        eaCopy:
            EditCopy;
        eaPaste:
            EditPaste;
    end;
end;

procedure TfrmBuildingBlockMethodEditor.EditCut;
var
    xRecIndex: integer;
begin
    CopySelectedRecords();
    if (self.Attribute <> meaDefault) then
        EXIT;

    xRecIndex := cxGrid1TableView1.Controller.FocusedRow.Index;
    DeleteSelectedRecords(xRecIndex);
end;

procedure TfrmBuildingBlockMethodEditor.EditCopy;
begin
    CopySelectedRecords();
end;

function TfrmBuildingBlockMethodEditor.GetSavedInsertRecIndex(): integer;
begin
    if Assigned(uGridRecord) then
        result := uGridRecord.Index // für Rechtsklick und DragDrop
    else
        result := cxGrid1TableView1.DataController.RecordCount - 1;
end;

function TfrmBuildingBlockMethodEditor.GetInsertRecIndex(): integer;
begin
    result := cxGrid1TableView1.DataController.FocusedRecordIndex;
    // für Buttons und Menus außerhalb des Grids
    if result < 0 then
        result := cxGrid1TableView1.DataController.RecordCount - 1;
end;

procedure TfrmBuildingBlockMethodEditor.EditPaste;
begin
    PasteSelectedRecords(GetInsertRecIndex());
end;

procedure TfrmBuildingBlockMethodEditor.DoClipboardDataOnLoadFromStream(const aMethodSteps
    : TObjectList<TMethodStep>; const aFirstRecordIndex: integer);
var
    x: integer;
    xRecordIndex: integer;
begin
    self.cxGrid1TableView1.Controller.ClearSelection;
    for x := 0 to aMethodSteps.Count - 1 do
    begin
        xRecordIndex := aFirstRecordIndex + x;
        InsertMethodStepAtIndex(aMethodSteps[x], xRecordIndex);
        self.ChangeData();
    end;
end;

procedure TfrmBuildingBlockMethodEditor.DoClipboardDataOnSaveToStream(const aMethodSteps
    : TObjectList<TMethodStep>);
var
    x: integer;
    xRecIndex: integer;
begin
    for x := 0 to cxGrid1TableView1.Controller.SelectedRecordCount - 1 do
    begin
        xRecIndex := cxGrid1TableView1.Controller.SelectedRecords[x].RecordIndex;
        if not MethodEditLineExists(xRecIndex) then
            CONTINUE;

        aMethodSteps.Add(fMethodEditLines[xRecIndex].Value);
    end;
end;

function TfrmBuildingBlockMethodEditor.CreateClipboardData(const aFirstRecordIndex: integer = 0)
    : TClipboardDataMethodSteps;
begin
    result := TClipboardDataMethodSteps.Create(self.fMethodEditDataHelper, aFirstRecordIndex);
    result.OnLoadFromStream := DoClipboardDataOnLoadFromStream;
    result.OnSaveToStream := DoClipboardDataOnSaveToStream;
end;

procedure TfrmBuildingBlockMethodEditor.CheckClipboardPaste;
var
    xClipboardData: TClipboardDataMethodSteps;
begin
    // Krücke: Nur damit Paste immer enabled ist!
    if (self.mnuPasteItem.Enabled) then
        EXIT;
    xClipboardData := CreateClipboardData();
    try
        if xClipboardData.ClipboardHasFormat() then
            self.mnuPasteItem.Enabled := true;
    finally
        xClipboardData.Free;
    end;
end;

procedure TfrmBuildingBlockMethodEditor.SetColumnMode(const Value: TMethodEditorColumnMode);
begin
    if Value = fColumnMode then
        EXIT;

    fColumnMode := Value;

    self.CustomizeView;
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1Resize(Sender: TObject);
begin
    if (cxGrid1.Width > 750) then
    begin
        self.ColumnMode := mcmTwoColumns;
    end;

    if (cxGrid1.Width < 550) then
        self.ColumnMode := mcmOneColumn;
end;

procedure TfrmBuildingBlockMethodEditor.CheckSelectedLineLevels();
var
    xIndices: TIntArray;
    x: integer;
    xRecIndex: integer;
begin
    if cxGrid1TableView1.Controller.SelectedRecordCount = 0 then
        EXIT;
    SetLength(xIndices, cxGrid1TableView1.Controller.SelectedRecordCount);
    for x := 0 to cxGrid1TableView1.Controller.SelectedRecordCount - 1 do
    begin
        xRecIndex := cxGrid1TableView1.Controller.SelectedRecords[x].RecordIndex;
        xIndices[x] := xRecIndex;
    end;

    CheckLineLevels(xIndices);
end;

procedure TfrmBuildingBlockMethodEditor.DeleteSelectedRecords_Intern(var vFocusIndex: integer);
var
    x, xRecIndex: integer;
begin
    if cxGrid1TableView1.Controller.SelectedRecordCount = 0 then
        EXIT;

    try
        for x := cxGrid1TableView1.Controller.SelectedRecordCount - 1 downto 0 do
        begin
            xRecIndex := cxGrid1TableView1.Controller.SelectedRecords[x].RecordIndex;
            if (xRecIndex < 0) then
                CONTINUE;

            if (xRecIndex < vFocusIndex) then
                dec(vFocusIndex); // FocusedRecordIndex verschiebt sich

            DeleteEditLines(xRecIndex, xRecIndex);
        end;
        cxGrid1TableView1.Controller.ClearSelection();

        ForceGridResize();

    finally
        ChangeData();
    end;
end;

destructor TfrmBuildingBlockMethodEditor.Destroy;
begin
    inherited;
end;

procedure TfrmBuildingBlockMethodEditor.DeleteSelectedRecords(var vFocusIndex: integer);
begin
    if (cxGrid1TableView1.Controller.SelectedRecordCount <= 0) then
        EXIT;

    CheckSelectedLineLevels();

    gGUIManager.SetCursor(crHourglass);
    fAddingNewRecord := true;
    try
        DeleteSelectedRecords_Intern(vFocusIndex);
        SetFocusAndSelectRecord(vFocusIndex);
    finally
        fAddingNewRecord := false;
        gGUIManager.SetCursor(crDefault);
    end;
end;

procedure TfrmBuildingBlockMethodEditor.CopySelectedRecords();
var
    xClipboardData: TClipboardDataMethodSteps;
begin
    if (cxGrid1TableView1.Controller.SelectedRecordCount <= 0) then
        EXIT;
    CheckSelectedLineLevels();

    gGUIManager.SetCursor(crHourglass);
    fAddingNewRecord := true;
    try
        xClipboardData := CreateClipboardData();
        try
            xClipboardData.SaveToClipboard();
        finally
            xClipboardData.Free;
        end;
    finally
        fAddingNewRecord := false;
        gGUIManager.SetCursor(crDefault);
    end;
end;

procedure TfrmBuildingBlockMethodEditor.PasteSelectedRecords(aInsertRecIndex: integer);
var
    xClipboardData: TClipboardDataMethodSteps;
begin
    if (self.Attribute <> meaDefault) then
        EXIT;

    gGUIManager.SetCursor(crHourglass);
    fAddingNewRecord := true;
    try
        xClipboardData := CreateClipboardData(aInsertRecIndex);
        // TClipboardDataMethodSteps.Create( cxGrid1TableView1, aInsertRecIndex );
        try
            xClipboardData.LoadFromClipboard();
        finally
            xClipboardData.Free;
        end;
    finally
        gGUIManager.SetCursor(crDefault);
        fAddingNewRecord := false;
    end;
end;

procedure TfrmBuildingBlockMethodEditor.MoveSelectedRecords();
var
    xInsertRecIndex: integer;
    xClipboardData: TClipboardDataMethodSteps;
begin
    // prüfen,ob überhaupt etwas verschoben werden kann
    if (not Assigned(cxGrid1TableView1.Controller.FocusedRecord)) or
        (cxGrid1TableView1.Controller.FocusedRecord.Selected and Assigned(uGridRecord)) then
        EXIT; // wenn uGridRecord nil ist, dann wurde in leeren Bereich geklickt
    if Assigned(uGridRecord) then
        // SelectedRecord nur ausgewählt, wenn nicht in leeren Bereich geklickt wurde
        if (cxGrid1TableView1.Controller.SelectedRecordCount <= 0) then
            EXIT;
    if (self.Attribute <> meaDefault) then
        EXIT;

    if TDialogUtils.MessageBox(TLanguageString.Read('Move selected method step?',
        'Gewählten Methodenschritt verschieben?'), TLanguageString.Read('Move line', 'Zeile verschieben'),
        MB_YESNO) = mrNO then
    begin
        cxGrid1TableView1.Controller.ClearSelection();
        EXIT;
    end;

    if Assigned(uGridRecord) then
        xInsertRecIndex := cxGrid1TableView1.Controller.FocusedRecordIndex
    else
        xInsertRecIndex := GetSavedInsertRecIndex();
    // wenn in leeren Bereich geklickt wurde, dann wird RecordCount als InsertIndex verwendet (letzte Position)
    if (cxGrid1TableView1.Controller.SelectedRecordCount <= 0) then
        EXIT;

    // da ich keine Funktion zum Verschieben von Zeilen gefunden habe, benutze ich NOCH die
    // Clipboard-Funktionen Cut und Paste (WL)

    CheckSelectedLineLevels();

    gGUIManager.SetCursor(crHourglass);
    fAddingNewRecord := true;
    try
        // copy
        xClipboardData := CreateClipboardData(); // TClipboardDataMethodSteps.Create( cxGrid1TableView1 );
        try
            xClipboardData.SaveToClipboard();
        finally
            xClipboardData.Free;
        end;

        // delete records
        DeleteSelectedRecords_Intern(xInsertRecIndex);

        // paste
        xClipboardData := CreateClipboardData(xInsertRecIndex);
        try
            xClipboardData.LoadFromClipboard();
        finally
            xClipboardData.Free;
        end;
    finally
        fAddingNewRecord := false;
        gGUIManager.SetCursor(crDefault);
        ChangeData();
    end;
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1DragDrop(Sender, Source: TObject; aX, aY: Integer);
var
    xHitTest: TcxCustomGridHitTest;
    xRecIndex: integer;
begin
    if not(Sender is TcxGridSite) then
        EXIT;

    xHitTest := (Sender as TcxGridSite).ViewInfo.GetHitTest(aX, aY);
    if (xHitTest is TcxGridRecordHitTest) then
        uGridRecord := (xHitTest as TcxGridRecordHitTest).GridRecord
    else
        uGridRecord := nil;
    xRecIndex := GetSavedInsertRecIndex();

    if (Source is TListView) and (self.Attribute = meaDefault) and
        Assigned((Source as TListView).Selected) then
    begin
        AddMethod((Source as TListView).Selected.Caption, xRecIndex);
        EXIT;
    end;

    if (Source is TcxTreeList) // Overview oder Favourites
        and (self.Attribute = meaDefault) and Assigned((Source as TcxTreeList).DragNode) then
    begin

        if (Source as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_METHOD then
            AddMethod((Source as TcxTreeList).DragNode.Texts[0], xRecIndex);

        if (Source as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_ACTION then
            AddAction((Source as TcxTreeList).DragNode.Texts[0], xRecIndex);

        if (Source as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_WASHPROG then
            AddWashProg((Source as TcxTreeList).DragNode.Texts[0], xRecIndex);
    end;

    // interne Verschiebungen (nicht aktiv)
    if (Source is TDragControlObject) and (TDragControlObject(Source).Control is TcxGridSite) and
        ((TDragControlObject(Source).Control as TcxGridSite).GridView is TcxGridTableView) then
    begin
        MoveSelectedRecords;
    end;
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1DragOver(Sender, Source: TObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
begin
    if (Source is TListView) and Assigned((Source as TListView).Selected) then
    begin
        Accept := (self.Attribute = meaDefault);
        EXIT;
    end;

    if (Source is TcxTreeList) and Assigned((Source as TcxTreeList).DragNode) then
    begin
        Accept := (self.Attribute = meaDefault) and
            (((Source as TcxTreeList).DragNode.StateIndex <> INT_IM_INDEX_METHOD) or
            ((Source as TcxTreeList).DragNode.StateIndex <> INT_IM_INDEX_ACTION) or
            ((Source as TcxTreeList).DragNode.StateIndex <> INT_IM_INDEX_WASHPROG));
        EXIT;
    end;
end;

procedure TfrmBuildingBlockMethodEditor.DeterminePopupEnabled();
var
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
    xRelatedItemParam: TCustomSetting;
begin
    self.mnuCutItem.Enabled := (cxGrid1TableView1.DataController.GetSelectedCount > 0);
    self.mnuCopyItem.Enabled := (cxGrid1TableView1.DataController.GetSelectedCount > 0);

    self.mnuOpenItem.Visible := false;
    if self.cxGrid1TableView1.Controller.FocusedRowIndex <= 0 then
        EXIT;
    if not MethodEditLineExists(self.cxGrid1TableView1.Controller.FocusedRowIndex) then
        EXIT;
    xMethodLine := fMethodEditLines[self.cxGrid1TableView1.Controller.FocusedRowIndex];
    xMethodStep := xMethodLine.Value;
    xRelatedItemParam := xMethodStep.RelatedItemParam;
    if not Assigned(xRelatedItemParam) then
        EXIT;
    if (xRelatedItemParam.Value = '') then
        EXIT;
    self.mnuOpenItem.Caption := TLanguageString.Read('Open ' + xRelatedItemParam.Value,
        xRelatedItemParam.Value + ' bearbeiten');
    self.mnuOpenItem.Visible := true;
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1SelectionChanged(Sender: TcxCustomGridTableView);
begin
    DeterminePopupEnabled();
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1FocusedRecordChanged(Sender: TcxCustomGridTableView;
    APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
begin
    if (fAddingNewRecord) then
        EXIT;
    DefineCurrentMethodStep();
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1KeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
var
    xRecordIndex, xColumnIndex: integer;
begin

    xRecordIndex := self.cxGrid1TableView1.DataController.FocusedRecordIndex;

    if Key = VK_RETURN then
    begin
        xColumnIndex := INT_METHOD_COL_SUMMARY;
        { 28.09.10 pk commented out, confuses user. use ctrl + K to edit comments self.cxGrid1TableView1.Controller.FocusedColumn.Index , }
        EditCellProperties(xRecordIndex, xColumnIndex, ssCtrl in Shift);
    end
    else
    begin

        CheckClipboardPaste();
    end;
end;

procedure TfrmBuildingBlockMethodEditor.DefineMenuItems(aX, aY: Integer);
var
    xHitTest: TcxCustomGridHitTest;
begin
    mnuAddEmptyLine.Visible := false;
    mnuDeleteLines.Visible := false;

    // Menüpunkte bestimmen
    xHitTest := cxGrid1TableView1.GetHitTest(aX, aY);

    if (xHitTest is TcxGridColumnHeaderHitTest) then
    begin
        uGridRecord := nil;
        uGridColumn := (xHitTest as TcxGridColumnHeaderHitTest).Column;

    end
    else
    begin
        if (xHitTest is TcxGridRecordHitTest) then
            uGridRecord := (xHitTest as TcxGridRecordHitTest).GridRecord // Paste in welcher Zeile
        else
            uGridRecord := nil;

        uGridColumn := nil;

        if (self.Attribute = meaDefault) then
        begin
            mnuAddEmptyLine.Visible := true;
            mnuDeleteLines.Visible := true;
        end;
    end;
    CheckClipboardPaste();
end;

procedure TfrmBuildingBlockMethodEditor.SelectAllData(aSender: TObject);
begin
    cxGrid1TableView1.Controller.SelectAll;
end;

function TfrmBuildingBlockMethodEditor.GetMethodName: string;
begin
    result := fViewItem.name;
end;

procedure TfrmBuildingBlockMethodEditor.mnuDeleteLinesClick(Sender: TObject);
var
    xFocusedRecordIndex: integer;
begin
    if not self.IsWritable then
        EXIT;

    fAddingNewRecord := true;
    try
        if (cxGrid1TableView1.Controller.SelectedRecordCount <= 0) then
            EXIT;
        if (cxGrid1TableView1.Controller.SelectedRecordCount > 1) and
            (TDialogUtils.MessageBox(TLanguageString.Read('Delete selected rows?',
            'Sollen die makierten Zeilen gelöscht werden?'), TLanguageString.Read('Delete selection',
            'Auswahl löschen'), MB_YESNO) <> MRYES) then
            EXIT;

        xFocusedRecordIndex := self.cxGrid1TableView1.DataController.FocusedRecordIndex;
        DeleteSelectedRecords(xFocusedRecordIndex);
    finally
        fAddingNewRecord := false;
    end;
end;

procedure TfrmBuildingBlockMethodEditor.mnuEditCommentClick(Sender: TObject);
begin
    self.EditMethodStepComment(self.cxGrid1TableView1.Controller.FocusedRowIndex);
end;

procedure TfrmBuildingBlockMethodEditor.sbDeleteRecordClick(Sender: TObject);
begin
    ASSERT(cxGrid1.CanFocus, 'Can not set focus to method editor');
    cxGrid1.SetFocus;
    mnuDeleteLinesClick(Sender);
end;

procedure TfrmBuildingBlockMethodEditor.sbMethVaraiblesClick(Sender: TObject);
begin
    EditMethodVaraibles();
end;

procedure TfrmBuildingBlockMethodEditor.WriteAttributeText();
begin
    edAttribute.Text := GetAttributeText();
end;

procedure TfrmBuildingBlockMethodEditor.UpdateMethodAttributes();
var
    xAttribute: TVisibilityAttribute;
begin
    xAttribute := self.GetAttribute;
    self.WriteAttributeText();

    cxGrid1TableView1.OptionsData.Deleting := (xAttribute = meaDefault);
    cxGrid1TableView1.OptionsData.Editing := (xAttribute = meaDefault);
    sbDeleteRecord.Visible := (xAttribute = meaDefault);
    sbChangeSettings.Visible := (xAttribute = meaDefault);
    sbMethVaraibles.Visible := (xAttribute = meaDefault);

    self.pmnuDeactivateSelectedLines.Visible := (xAttribute = meaDefault);
    self.pmnuActivateSelectedLines.Visible := (xAttribute = meaDefault);

    // nur bei Hidden
    cxGrid1.Visible := (xAttribute in [meaDefault, meaReadOnly]);
end;

function TfrmBuildingBlockMethodEditor.GetAttribute: TVisibilityAttribute;
begin
    result := fViewItem.GetVisibilityAttribute();
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1DataControllerAfterDelete
    (ADataController: TcxCustomDataController);
begin
    ChangeData();
end;

procedure TfrmBuildingBlockMethodEditor.sbChangeSettingsClick(Sender: TObject);
begin
    // Editieren!
    EditMethodSettings(fViewItem.name, Memo1, true);
end;

procedure TfrmBuildingBlockMethodEditor.sbCheckMethodClick(Sender: TObject);
begin
    self.Build();
end;

procedure TfrmBuildingBlockMethodEditor.EditSetFocusToRow(aRow: integer);
begin
    if not MethodEditLineExists(aRow) then
        EXIT;
    // Set Focus to grid: if the grid does not have focus, no editing can occur and no Edit control is created. An edit control
    // is needed for selecting a part of a text.
    if cxGrid1.CanFocus then
        cxGrid1.SetFocus;

    // Set focus to row
    cxGrid1TableView1.DataController.FocusedRecordIndex := aRow;

    cxGrid1TableView1.Controller.ClearSelection();
    cxGrid1TableView1.DataController.ChangeRowSelection(aRow, true);
end;

procedure TfrmBuildingBlockMethodEditor.EditSetFocusToTextPart(const aRow: integer;
    const aSettingKey: TStringArray; const aFocusStart, aFocusLen: integer);
// var
// xMethodLine: TMethodEditLine;
// xMethodStep: TMethodStep;
// xFocusOptions: TEdActionPropsFocusOptions;
begin
    { if not MethodEditLineExists(aRow) then
      EXIT;

      xMethodLine := fMethodEditLines[aRow];
      xMethodStep := xMethodLine.Value;

      EditSetFocusToRow(aRow);

      xFocusOptions.SettingKey := aSettingKey;
      xFocusOptions.FocusStart := aFocusStart;
      xFocusOptions.FocusLen := aFocusLen;

      if TfrmActionProps.InstanceEditMethodStepSpecialFocusModal(xMethodStep, frmEdMain.ActionImages64,
      self.IsWritable, xFocusOptions) = mrOK then
      begin
      self.ChangeData();
      end; }
end;

procedure TfrmBuildingBlockMethodEditor.PopupMenu1Popup(Sender: TObject);
begin
    DeterminePopupEnabled();
end;

function TfrmBuildingBlockMethodEditor.AddMethodStep(const aActionName, aOption: string; aRecIndex: integer;
    aSetFocus: boolean): integer;
begin
    EXIT(-1);
end;

procedure TfrmBuildingBlockMethodEditor.mnuOpenItemClick(Sender: TObject);
begin
    self.OpenMethodStepRelatedEditor(self.cxGrid1TableView1.Controller.FocusedRowIndex);
end;

procedure TfrmBuildingBlockMethodEditor.mnuOpenPropertiesClick(Sender: TObject);
begin
    self.EditMethodStepPropertiesOrComment(self.cxGrid1TableView1.Controller.FocusedRowIndex);
end;

procedure TfrmBuildingBlockMethodEditor.mnuPasteItemClick(Sender: TObject);
begin
    EditAction(Sender, eaPaste);
end;

function TfrmBuildingBlockMethodEditor.GetLineStartX(const aMethodLine: TMethodEditLine): integer;
var
    xLevel: integer;
begin
    xLevel := GetLineLevel(aMethodLine);
    result := (xLevel) * cIndentWidth;
end;

function TfrmBuildingBlockMethodEditor.GetLineIconX(const aMethodLine: TMethodEditLine): integer;
begin
    result := GetLineStartX(aMethodLine) + cIndentWidth;
end;

function TfrmBuildingBlockMethodEditor.IsXOnLineIcon(const aX: integer;
    const aMethodLine: TMethodEditLine): boolean;
var
    xIconX: integer;
begin
    xIconX := GetLineIconX(aMethodLine) + cFirstXOffset;

    result := { ( aX > xIconX ) and } (aX < (xIconX + 16));
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1MouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
    xHitTest: TcxCustomGridHitTest;
    xRecordIndex: integer;
begin
    if (Button = mbRight) then
    begin
        self.DefineMenuItems(X, Y);
    end
    else if (button = mbleft) and (not(ssDouble in Shift)) then
    begin

        if (self.Attribute <> meaDefault) then
            EXIT;

        xHitTest := (Sender as TcxGridSite).ViewInfo.GetHitTest(X, Y);
        if (xHitTest is TcxGridRecordCellHitTest) then
        begin
            xRecordIndex := (xHitTest as TcxGridRecordCellHitTest).GridRecord.RecordIndex;
            if not MethodEditLineExists(xRecordIndex) then
                EXIT;

            // cxGrid1.BeginDrag( false ); This doesn't kill the multi-select, but pullfocusing is not released at the end of the drag.
            (Sender as TcxGridSite).BeginDrag(false); // works. but kills the multiselect
        end
    end;
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1MouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
const
    cXTolerance = 3;
    cYTolerance = 3;
var
    xMethodLine: TMethodEditLine;
    xRecordIndex: integer;
    xHitTest: TcxCustomGridHitTest;
    xPoint: TPoint;
begin
    xPoint.X := X;
    xPoint.Y := Y;

    if (Abs(fPrevMousePos.X - xPoint.X) <= cXTolerance) and
        (Abs(fPrevMousePos.Y - xPoint.Y) <= cYTolerance) then
        EXIT;
    fPrevMousePos := xPoint;
    xHitTest := (Sender as TcxGridSite).ViewInfo.GetHitTest(X, Y);
    if not(xHitTest is TcxGridRecordCellHitTest) then
    begin
        EXIT;
    end;

    xRecordIndex := (xHitTest as TcxGridRecordCellHitTest).GridRecord.RecordIndex;

    if not MethodEditLineExists(xRecordIndex) then
        EXIT;
    xMethodLine := fMethodEditLines[xRecordIndex];
    if not self.IsXOnLineIcon(X, xMethodLine) then
        EXIT;
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1CanSelectRecord(Sender: TcxCustomGridTableView;
    ARecord: TcxCustomGridRecord; var AAllow: Boolean);
var
    xRecordIndex: integer;
    xMethodLine: TMethodEditLine;
    xGroupMatchingLineIndex: integer;
begin
    AAllow := false;
    xRecordIndex := ARecord.RecordIndex;
    if not MethodEditLineExists(xRecordIndex) then
        EXIT;

    AAllow := true;
    xMethodLine := fMethodEditLines[xRecordIndex];

    if (not fIsSelecting) and (xMethodLine is TMethodEditGroupEndLine) then
    begin
        xGroupMatchingLineIndex := FindLineIndexOf((xMethodLine.Parent));
        AAllow := self.cxGrid1TableView1.DataController.IsRowSelected(xGroupMatchingLineIndex);
        EXIT;
    end;

    if xMethodLine is TMethodEditGroupBeginLine then
    begin
        xGroupMatchingLineIndex := FindLineIndexOf((xMethodLine as TMethodEditGroupBeginLine).EndLine);
        fIsSelecting := true;
        try
            if xGroupMatchingLineIndex >= 0 then
            begin
                self.cxGrid1TableView1.DataController.SelectRows(xGroupMatchingLineIndex,
                    xGroupMatchingLineIndex);
            end;
        finally
            fIsSelecting := false;
        end;
    end;
end;

procedure TfrmBuildingBlockMethodEditor.EditCellProperties(const aRecordIndex: integer;
    const aColIndex: integer; const aEditRelatedProperties: boolean);
begin
    if aColIndex = INT_METHOD_COL_SUMMARY then
    begin
        if aEditRelatedProperties then
        begin
            self.OpenMethodStepRelatedEditor(aRecordIndex);
        end
        else
        begin
            self.EditMethodStepPropertiesOrComment(aRecordIndex);
        end;
    end
    else if aColIndex = INT_METHOD_COL_COMMENT then
    begin
        self.EditMethodStepComment(aRecordIndex);
    end;
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1CellDblClick(Sender: TcxCustomGridTableView;
    ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState;
    var AHandled: Boolean);
var
    xColIndex: integer;
    xRecordIndex: integer;
    xMethodLine: TMethodEditLine;
begin
    xColIndex := (ACellViewInfo.Item as TcxGridColumn).Index;
    xRecordIndex := aCellViewinfo.GridRecord.RecordIndex;

    if not MethodEditLineExists(xRecordIndex) then
        EXIT;
    xMethodLine := fMethodEditLines[xRecordIndex];

    if self.IsXOnLineIcon(fPrevMousePos.X, xMethodLine) then
        self.OpenMethodStepRelatedEditor(xRecordIndex)
    else
        EditCellProperties(xRecordIndex, xColIndex, ssCtrl in AShift);
end;

class function TfrmBuildingBlockMethodEditor.CalculateTextHeight(const aText: string;
    const aCanvas: TCxCanvas; const aPossibleCellWidth: integer): integer;
var
    xRect: TRect;
begin
    xRect := Rect(0, 0, aPossibleCellWidth, 0);
    aCanvas.TextExtent(aText, xRect, cxWordBreak);
    result := xRect.Bottom - xRect.Top;
end;

function TfrmBuildingBlockMethodEditor.ShowMethodStepCommentInColumn(const aMethodStep: TMethodStep): integer;
begin
    if (self.ColumnMode = mcmOneColumn) or (aMethodStep is TBlankMethodStep) then
        result := INT_METHOD_COL_SUMMARY
    else
        result := INT_METHOD_COL_COMMENT;
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1GetCellHeight(Sender: TcxCustomGridTableView;
    ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; ACellViewInfo: TcxGridTableDataCellViewInfo;
    var AHeight: Integer);
var
    xRecordIndex: integer;
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
    xText: string;
    xHeight: integer;
    xShowMethodStepCommentInThisCol: boolean;
begin
    if not Assigned(ARecord) then
        EXIT;

    xRecordIndex := ARecord.RecordIndex;
    if xRecordIndex = GetNewItemRowIndex() then
    begin
        AHeight := CalculateTextHeight('X', aCellViewInfo.GridView.Painter.Site.Canvas, aCellViewInfo.Width);
        AHeight := AHeight * 4;
    end;

    if not MethodEditLineExists(xRecordIndex) then
        EXIT;

    xMethodLine := fMethodEditLines[xRecordIndex];
    xMethodStep := xMethodLine.Value;

    AHeight := 0;

    xShowMethodStepCommentInThisCol :=
        (ACellViewInfo.Item.Index = ShowMethodStepCommentInColumn(xMethodStep));

    if xShowMethodStepCommentInThisCol then
    begin
        xText := xMethodStep.M_Comments;
        xHeight := CalculateTextHeight(xText, aCellViewInfo.GridView.Painter.Site.Canvas,
            aCellViewInfo.Width);
        AHeight := AHeight + xHeight;
    end;

    if ACellViewInfo.Item.Index = INT_METHOD_COL_SUMMARY then
    begin
        xText := xMethodStep.GetActionCaptionText + #13#10 + xMethodStep.SummaryParamters;
        xHeight := CalculateTextHeight(xText, aCellViewInfo.GridView.Painter.Site.Canvas,
            aCellViewInfo.Width);
        AHeight := AHeight + xHeight;
    end;

    // atleast one line
    if AHeight = 0 then
    begin
        AHeight := CalculateTextHeight('X', aCellViewInfo.GridView.Painter.Site.Canvas, aCellViewInfo.Width);
    end;

    AHeight := AHeight + 2;

    if AHeight < fActionImages.Height then
        AHeight := fActionImages.Height;

end;

procedure TfrmBuildingBlockMethodEditor.DrawComment(const aCanvas: TcxCanvas;
    const aMethodLine: TMethodEditLine; const aIsSummaryCol: boolean; const aIsSelected: boolean;
    var vRect: TRect);
var
    xText: string;
    xSavedColor: TColor;
    xMethodStep: TMethodStep;
begin

    xMethodStep := aMethodLine.Value;
    xText := xMethodStep.M_Comments;
    if xText = '' then
        EXIT;

    xSavedColor := aCanvas.Font.Color;
    try
        if (not aIsSelected) and (not xMethodStep.InactiveAsBool) then
        begin
            aCanvas.Font.Color := clPurple;
        end;

        aCanvas.DrawText(xText, Rect(vRect.Left, vRect.Top, vRect.Right, vRect.Bottom), cxWordBreak);

        vRect.Top := vRect.Top + CalculateTextHeight(xText, aCanvas, vRect.Right - vRect.Left) + 1;
        aCanvas.Font.Style := [];
    finally
        aCanvas.Font.Color := xSavedColor;
    end;
end;

procedure TfrmBuildingBlockMethodEditor.DrawGroupBrackets(const aCanvas: TcxCanvas;
    const aMethodLine: TMethodEditLine; const aRect: TRect);
var
    xSavedColor: TColor;
    xColor: TColor;
    xTempRect, xTempRect2: TRect;
    x: integer;
    xLineLevel: integer;
begin
    xLineLevel := GetLineLevel(aMethodLine);

    xSavedColor := aCanvas.Brush.Color;
    try
        for x := 1 to xLineLevel + 1 do
        begin
            xTempRect := aRect;

            if x mod 2 = 0 then
            begin
                xColor := clMoneyGreen;
            end
            else
            begin
                xColor := clSkyBlue;
            end;

            xTempRect.Left := aRect.Left + ((x) * cIndentWidth) + 4;
            xTempRect.Right := xTempRect.Left + 3;

            if x = xLineLevel + 1 then
            begin
                if (aMethodLine is TMethodEditGroupBeginLine) or (aMethodLine is TMethodEditGroupEndLine) then
                begin
                    xTempRect2 := xTempRect;

                    xTempRect2.Left := xTempRect2.Left;
                    xTempRect2.Right := xTempRect2.Left + 16;

                    if (aMethodLine is TMethodEditGroupBeginLine) then
                    begin
                        xTempRect2.Top := xTempRect2.Top + 4;
                        xTempRect.Top := xTempRect2.Top;

                        xTempRect2.Bottom := xTempRect2.Top + 6;
                    end
                    else if (aMethodLine is TMethodEditGroupEndLine) then
                    begin
                        xTempRect2.Top := xTempRect2.Top + 8;

                        xTempRect2.Bottom := xTempRect2.Top + 4;
                        xTempRect.Bottom := xTempRect2.Bottom;
                    end;

                    aCanvas.FillRect(xTempRect2, xColor);
                    aCanvas.FillRect(xTempRect, xColor);
                end;

            end
            else
            begin
                aCanvas.FillRect(xTempRect, xColor);
            end;
        end;

    finally
        aCanvas.Brush.Color := xSavedColor;
    end;
end;

procedure TfrmBuildingBlockMethodEditor.DrawIconOrShape(const aCanvas: TcxCanvas;
    const aMethodLine: TMethodEditLine; const aIsSelected: boolean; var vRect: TRect);
var
    xMethodStep: TMethodStep;
begin
    xMethodStep := aMethodLine.Value;

    if not(xMethodStep.InactiveAsBool) then
    begin
        if (xMethodStep is TAddMethodMethodStep) then
        begin
            aCanvas.DrawImage(fAddMethodImages, vRect.Left, vRect.Top,
                fOnGetAddMethodIndex((xMethodStep as TAddMethodMethodStep)
                .MainSubOptionSetting.MethName.Value));
        end
        else if (not xMethodStep.IsCodeStep) or (xMethodStep is TParamStoreMethodStep) then
        begin
            aCanvas.DrawImage(fActionImages, vRect.Left, vRect.Top, xMethodStep.StepInfo.IconIndex);
        end;
    end;

    vRect.Left := vRect.Left + fActionImages.Width + 5;
end;

procedure TfrmBuildingBlockMethodEditor.DrawSummaryText(const aCanvas: TcxCanvas;
    const aMethodLine: TMethodEditLine; const aIsSelected: boolean; var vRect: TRect);
var
    xText: string;
    xSavedColor: TColor;
    xMethodStep: TMethodStep;
    xTop: integer;
begin

    xMethodStep := aMethodLine.Value;
    xTop := vRect.Top;
    if not xMethodStep.IsCodeStep then
    begin
        xSavedColor := aCanvas.Font.Color;
        try
            if (not aIsSelected) and (not xMethodStep.InactiveAsBool) then
            begin
                aCanvas.Font.Color := clBlue;
            end;

            if (xMethodStep.InactiveAsBool) then
            begin
                aCanvas.Font.Color := clDkGray;
            end;
            xText := xMethodStep.GetActionCaptionText();
            aCanvas.DrawText(xText, Rect(vRect.Left, xTop, vRect.Right, vRect.Bottom), cxWordBreak);

            aCanvas.Font.Style := [];
            xTop := xTop + aCanvas.TextHeight(xText) + 1;
        finally
            aCanvas.Font.Color := xSavedColor;
        end;
    end;

    xText := xMethodStep.SummaryParamters;
    aCanvas.DrawText(xText, Rect(vRect.Left, xTop, vRect.Right, vRect.Bottom), cxWordBreak);
    vRect.Top := vRect.Top + CalculateTextHeight(xText, aCanvas, vRect.Right - vRect.Left) + 1;
end;

procedure TfrmBuildingBlockMethodEditor.cxGrid1TableView1CustomDrawCell(Sender: TcxCustomGridTableView;
    ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
    xRect: TRect;
    xRecordIndex: integer;
    xMethodStep: TMethodStep;
    xMethodLine: TMethodEditLine;
    xTempRect: TRect;
begin
    if not Assigned(aViewInfo.RecordViewInfo) then
        EXIT;

    xRecordIndex := aViewInfo.RecordViewInfo.GridRecord.RecordIndex;

    if xRecordIndex = GetNewItemRowIndex() then
    begin
        if aViewInfo.Item.Index = INT_METHOD_COL_SUMMARY then
        begin
            if not aViewInfo.Selected then
            begin
                aCanvas.Brush.Color := clWhite;
                ACanvas.FillRect(aViewInfo.Bounds);

                xRect := aViewInfo.ContentBounds;
                xRect.Left := xRect.Left + 5;
                aCanvas.Font.Style := [fsBold];
                aCanvas.DrawText('. . .', xRect, cxAlignVCenter);
                aDone := true;
            end;
        end;
        EXIT;
    end;

    if not MethodEditLineExists(xRecordIndex) then
        EXIT;

    xMethodLine := fMethodEditLines[xRecordIndex];
    xMethodStep := xMethodLine.Value;

    // clear old drawings
    if not aViewInfo.Selected then
    begin
        aCanvas.Brush.Color := clWhite;
        if (xMethodStep.InactiveAsBool) then
        begin
            aCanvas.Brush.Color := clLtGray;
        end;
    end;

    if (xMethodStep.InactiveAsBool) then
    begin
        aCanvas.Font.Color := clDkGray;
    end;

    ACanvas.FillRect(aViewInfo.Bounds);

    xRect := aViewInfo.ContentBounds;

    // Kommentarzeile purple
    if aViewInfo.Item.Index = INT_METHOD_COL_COMMENT then
    begin
        if ShowMethodStepCommentInColumn(xMethodStep) = INT_METHOD_COL_COMMENT then
        begin

            xRect.Left := xRect.Left + 2;
            DrawComment(aCanvas, xMethodLine, false, aViewInfo.Selected, xRect);
        end;
    end
    else if aViewInfo.Item.Index = INT_METHOD_COL_SUMMARY then
    begin

        if not(xMethodStep.InactiveAsBool) then
        begin
            DrawGroupBrackets(aCanvas, xMethodLine, xRect);
        end;

        xRect.Left := xRect.Left + self.GetLineIconX(xMethodLine);

        // Comment
        if ShowMethodStepCommentInColumn(xMethodStep) = INT_METHOD_COL_SUMMARY then
        begin
            xTempRect := xRect;
            xTempRect.Left := xTempRect.Left + fActionImages.Width + 5;
            DrawComment(aCanvas, xMethodLine, true, aViewInfo.Selected, xTempRect);
            xRect.Top := xTempRect.Top;
        end;

        // Icon
        DrawIconOrShape(aCanvas, xMethodLine, aViewInfo.Selected, xRect);

        // Summary Text
        DrawSummaryText(aCanvas, xMethodLine, aViewInfo.Selected, xRect);
    end;

    aDone := true;
end;

procedure TfrmBuildingBlockMethodEditor.SetSelectedLinesInactive(const aInactive: boolean);
var
    x: integer;
    xRecIndex: integer;
begin
    for x := 0 to cxGrid1TableView1.Controller.SelectedRecordCount - 1 do
    begin
        xRecIndex := cxGrid1TableView1.Controller.SelectedRecords[x].RecordIndex;
        if not MethodEditLineExists(xRecIndex) then
            CONTINUE;

        fMethodEditLines[xRecIndex].Value.InactiveAsBool := aInactive;
        if (x = 0) then
        begin
            self.ChangeData;
            self.cxGrid1TableView1.Invalidate();
        end;
    end;
end;

procedure TfrmBuildingBlockMethodEditor.pmnuDeactivateSelectedLinesClick(Sender: TObject);
begin
    SetSelectedLinesInactive(true);
end;

procedure TfrmBuildingBlockMethodEditor.pmnuActivateSelectedLinesClick(Sender: TObject);
begin
    SetSelectedLinesInactive(false);
end;

procedure TfrmBuildingBlockMethodEditor.SaveStatusChanged();
begin
    if fSaveStatus = efssDataChanged then
    begin
        self.edModifiedStatus.Text := TLanguageString.Read('Modified', 'Geändert');
    end
    else
    begin
        self.edModifiedStatus.Text := '';
    end;
end;

procedure TfrmBuildingBlockMethodEditor.CustomizeView;
begin
    if (fColumnMode = mcmTwoColumns) then
    begin
        cxGrid1TableView1.Columns[INT_METHOD_COL_COMMENT].Visible := true;
        cxGrid1TableView1.Columns[INT_METHOD_COL_SUMMARY].Width := cxGrid1.Width div 2;
    end
    else
    begin
        cxGrid1TableView1.Columns[INT_METHOD_COL_COMMENT].Visible := false;
    end;
end;

procedure TfrmBuildingBlockMethodEditor.SaveData;
begin
    gGUIManager.SetCursor(crHourglass);
    try
        SaveDataIntern;
    finally
        gGUIManager.SetCursor(crDefault);
    end;
end;


end.
