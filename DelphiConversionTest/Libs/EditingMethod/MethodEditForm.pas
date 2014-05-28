{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  10.12.12 wl                                      TN6045   Initial Revision
  11.12.12 wl                                      TN6045   Teile aus MethodEditor hierher verschoben
  27.02.13 wl                                      TN6045   uses Generics.Collections
  08.03.13 wl  EditMethodVaraibles                 TN6095   neu
  21.03.13 wl  EditMethodComments                  TN6045   von MethodEditor hierher
  27.03.13 wl  EditMethodStepModalIntern           TN6095   neu
  30.08.13 wl                                      TN6236   mehrere Funktionen von MethodEditor und BuildingBlockMethodEditor hierher
  04.04.14 ts  SaveDataIntern                      TN6388   Methodenicons werden wieder aktualisiert bei Refresh
  ----------------------------------------------------------------------------------------------------------- }

unit MethodEditForm;


interface


uses
    Classes,
    StdCtrls,
    Generics.Collections,
    Controls,
    cxGrid,
    CustomSetting,
    cxGridTableView,
    MethodEditDataHelper,
    MethodSettingsDataAdaptor,
    MethodStep,
    AppTypes,
    StringLoader,
    ViewItemEditForm;

const
    STR_TEMP_METHODNAME = '_TEMP~~~~~METHOD';

type
    TMethodEditorColumnMode = (mcmOneColumn, mcmTwoColumns);
    TMethodEditorViewMode = (mvmDetailedSingleCol, mvmDetailedHybridComment, mvmDetailedSeparateComment);

    TMethodEditorStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TMethodEditForm = class abstract(TViewItemEditForm)
    private
        // doppelt vorhanden:
        cxGrid1: TcxGrid;
        cxGrid1TableView1: TcxGridTableView;

        fMethodEditMainGroupLine: TMethodEditGroupBeginLine;
        function CreateMethodEditLine(const aMethodStep: TMethodStep): TMethodEditLine;
        procedure InsertMethodStepAfter(const aMethodStep: TMethodStep;
            const aInsertAfterEditLine: TMethodEditLine);
        function InsertRecordExt(aRecIndex: integer): integer;
        procedure LoadView(aOrderText, aWidthText: string);
        procedure LoadMethodData(aMemo: TMemo);
    protected
        fMethodEditLines: TMethodEditLineList;
        fMethodEditDataHelper: TMethodEditDataHelper;
        fAddingNewRecord: boolean;
        fSettings: TMethodSettingsRec;
        procedure ForceGridResize();
        function MethodEditLineExists(const aIndex: integer): boolean;
        procedure SetGridPointer(aGrid: TcxGrid; aGridTableView: TcxGridTableView);
        function GetAttributeText(): string;

        procedure InsertMethodStepAtIndex(const aMethodStep: TMethodStep; const aIndex: integer);
        function FindLineIndexOf(const aMethodLine: TMethodEditLine): integer;
        procedure CheckLineLevels(const aIndices: TArray<integer>);
        procedure MoveLinesToGroup(const aGroupBeginIndex: integer; const aFirstIndex, aLastIndex: integer);
        procedure DoClipboardDataOnSaveToStream(const aMethodSteps: TObjectList<TMethodStep>);
        class function GetLineLevel(const aEditLine: TMethodEditLine): integer;
        procedure BuildStepHierarchy(const aMethodStepList: TObjectList<TMethodStep>;
            const aBeginLine: TMethodEditGroupBeginLine);
        procedure InsertNewItemRowLine();
        procedure SetFocusAndSelectRecord(const aRecIndex: integer);
        function GetNewItemRowIndex(): integer;
        procedure DeleteEditLines(const aFirstIndex, aLastIndex: integer);
        function CreateMethodStep(const aActionName, aOption: string): TMethodStep;
        function AddMethodStepLine(const aMethodStep: TMethodStep; const aRecIndex: integer;
            aSetFocus: boolean): integer;
        function AddMethodStep(const aActionName, aOption: string; aRecIndex: integer;
            aSetFocus: boolean): integer;
        procedure EditMethodStepComment(const aIndex: integer);
        function IsWritable: boolean;
        procedure EditMethodStepProperties(const aIndex: integer);
        procedure EditMethodVaraibles();
        procedure EditMethodSettings(const aMethodName: string; aMemo: TMemo; aIsRunner: boolean);
        class function GetCompleteSummaryText(const aMethodStep: TMethodStep): string; static;
        class function EditMethodStepModalIntern(aParameter: TCustomSetting): boolean; static;
        procedure LoadViewAndMethodData(aMemo: TMemo);
        procedure UpdateMethodInfo(aMemo: TMemo);

        property MainGroupLine: TMethodEditGroupBeginLine read fMethodEditMainGroupLine;
    protected
        // abstract methods
        function GetAttribute: TVisibilityAttribute; virtual; abstract;
        function EditMethodStepModal(const aMethodStep: TMethodStep): boolean; virtual; abstract;
        function GetInsertRecIndex: integer; virtual; abstract;
        procedure SaveDataIntern();
    public
        constructor Create(aOwner: TComponent; const aItemName: string; aOnSaveStatusChanged: TNotifyEvent;
            const aOnAddEditFunctions: TNotifyEvent); reintroduce; virtual;
        destructor Destroy; override;

        procedure UpdateMethodAttributes(); virtual; abstract;
        procedure AddAction(const aActionName: string); overload;
        procedure AddAction(const aActionName: string; aRecIndex: integer); overload;
    end;


implementation


uses
    SysUtils,
    MethodStepCommentEditor,
    WhileRunStep,
    MethodViewItem,
    MethodDataAdaptor,
    IfRunStep,
    MethodSettingsEditor,
    MethodVariablesEditor,
    EditMethodParamsExt,
    ForRunStep,
    ControlUtils,
    GUIManager,
    GeneralTypes,
    BlankRunStep,
    MethodDataCache;

{ TMethodEditorStringLoader }

procedure TMethodEditorStringLoader.AddAllItems;
begin
    AddSingle(43010, 'Delete Line(s)', 'Zeile(n) löschen');
    AddSingle(43020, 'Add action', 'Action hinzufügen');
    AddSingle(43025, 'Add empty line', 'Leere Zeile hinzufügen');
    AddSingle(43020, 'Add line', 'Zeile hinzufügen');
    AddDouble(43090, '', 'Delete line', '', 'Zeile löschen');
    AddDouble(43100, '', 'Add line', '', 'Zeile hinzufügen');
    AddDouble(44870, '', 'Surround selected lines by a WHILE-loop', '',
        'Ausgewählte Zeilen in eine WHILE-Schleife einfassen');
    AddDouble(44872, '', 'Surround selected lines by a FOR-loop', '',
        'Ausgewählte Zeilen in eine FOR-Schleife einfassen');
    AddDouble(44880, '', 'Surround selected lines by a condition', '',
        'Ausgewählte Zeilen in eine Bedingung einfassen');
    AddSingle(44890, 'Surround selected lines by a WHILE-loop',
        'Ausgewählte Zeilen in eine WHILE-Schleife einfassen');
    AddSingle(44892, 'Surround selected lines by a FOR-loop',
        'Ausgewählte Zeilen in eine FOR-Schleife einfassen');
    AddSingle(44900, 'Surround selected lines by a condition',
        'Ausgewählte Zeilen in eine Bedingung einfassen');
    AddDouble(44910, '', 'Surround selected lines by a import loop', '',
        'Ausgewählte Zeilen in eine Import-Schleife einfassen');
    AddSingle(44920, 'Surround selected lines by a import loop',
        'Ausgewählte Zeilen in eine Import-Schleife einfassen');
    AddDouble(52040, '', 'Change settings for this method', '', 'Einstellungen für diese Methode ändern');
    AddDouble(52041, '', 'Change variable input for this method', '',
        'Variablen-Eingabe für diese Methode ändern');
    AddDouble(52080, '', 'Save and Check Method', '', 'Methode speichern und prüfen');
    AddSingle(52490, 'Line', 'Zeile');
    AddSingle(52520, 'Goto line', 'Gehe zu Zeile');
    AddSingle(52530, 'Line comments', 'Zeilenkommentare');
    AddSingle(52531, 'Action properties', 'Aktionseigenschaften');
    AddSingle(52998, 'Deactivate selected lines', 'Zeilen deaktivieren');
    AddSingle(52999, 'Activate selected lines', 'Zeilen aktivieren');
end;

{ TMethodEditorDataAdaptor }

constructor TMethodEditForm.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent; const aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged);

    fMethodEditLines := TMethodEditLineList.Create();
    fMethodEditMainGroupLine := TMethodEditGroupBeginLine.Create();

    fMethodEditDataHelper := TMethodEditDataHelper.Create(aOnAddEditFunctions);
end;

destructor TMethodEditForm.Destroy;
begin
    FreeAndNil(fMethodEditLines);
    FreeAndNil(fMethodEditMainGroupLine);

    inherited;
end;

procedure TMethodEditForm.SetGridPointer(aGrid: TcxGrid; aGridTableView: TcxGridTableView);
begin
    // Zeiger auf Grid-Objekt setzen
    cxGrid1 := aGrid;
    cxGrid1TableView1 := aGridTableView;
    cxGrid1TableView1.DragMode := dmManual;
end;

procedure TMethodEditForm.UpdateMethodInfo(aMemo: TMemo);
var
    xMethodText: string;
begin
    aMemo.Lines.Clear;

    xMethodText := TMethodSettingsDataAdaptor.GetStartableText(fSettings);
    if (xMethodText <> '') then
        aMemo.Lines.Add(xMethodText);

    // Comments
    if (fSettings.Comment <> '') then
        aMemo.Lines.Add(fSettings.Comment);

    TControlUtils.AutoAdjustMemoHeight(aMemo);
end;

procedure TMethodEditForm.SetFocusAndSelectRecord(const aRecIndex: integer);
begin
    if (aRecIndex < 0) or (aRecIndex >= cxGrid1TableView1.DataController.RecordCount) then
        EXIT;

    self.cxGrid1TableView1.DataController.FocusedRecordIndex := aRecIndex;

    if self.cxGrid1TableView1.DataController.GetSelectedCount = 0 then
    begin
        self.cxGrid1TableView1.DataController.SelectRows(aRecIndex, aRecIndex);
    end;
end;

function TMethodEditForm.GetNewItemRowIndex(): integer;
begin
    // the final line should be always be the newitemrow
    result := self.cxGrid1TableView1.DataController.RecordCount - 1;
end;

function TMethodEditForm.InsertRecordExt(aRecIndex: integer): integer;
begin
    if (aRecIndex < 0) then
        aRecIndex := cxGrid1TableView1.DataController.RecordCount; // Add as last record

    result := cxGrid1TableView1.DataController.InsertRecord(aRecIndex);
    cxGrid1TableView1.DataController.FocusedRecordIndex := result;
end;

procedure TMethodEditForm.LoadView(aOrderText, aWidthText: string);
var
    x: integer;
begin
    // alle Spalten markieren (um sie nach dem Verschieben wieder zu erkennen)
    for x := 0 to INT_NUMBEROFCOLUMNS - 1 do
        cxGrid1TableView1.Columns[x].Tag := x;
end;

procedure TMethodEditForm.DeleteEditLines(const aFirstIndex, aLastIndex: integer);
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

procedure TMethodEditForm.ForceGridResize();
var
    xSavedCellHeight: boolean;
begin
    // I haven't found the function yet for making the grid recalculate the sizes again
    xSavedCellHeight := cxGrid1TableView1.OptionsView.CellAutoHeight;
    try
        cxGrid1TableView1.OptionsView.CellAutoHeight := not cxGrid1TableView1.OptionsView.CellAutoHeight;
    finally
        cxGrid1TableView1.OptionsView.CellAutoHeight := xSavedCellHeight;
    end;
end;

function TMethodEditForm.MethodEditLineExists(const aIndex: integer): boolean;
begin
    result := (aIndex >= 0) and (aIndex < fMethodEditLines.Count);
end;

function TMethodEditForm.CreateMethodEditLine(const aMethodStep: TMethodStep): TMethodEditLine;
begin
    if (aMethodStep is TWhileMethodStep) or (aMethodStep is TForMethodStep) or
        (aMethodStep is TIfMethodStep) then
    begin
        result := TMethodEditGroupBeginLine.Create();

    end
    else if (aMethodStep is TEndWhileMethodStep) or (aMethodStep is TEndForMethodStep) or
        (aMethodStep is TEndIfMethodStep) then
    begin
        result := TMethodEditGroupEndLine.Create();
    end
    else
    begin
        result := TMethodEditLine.Create();
    end;

    result.Value := aMethodStep;
end;

function TMethodEditForm.CreateMethodStep(const aActionName, aOption: string): TMethodStep;
begin
    result := fMethodEditDataHelper.CreateMethodStep(aActionName);
    if (aOption <> '') then
        result.SetFirstSubOptionValue(aOption);
end;

procedure TMethodEditForm.InsertMethodStepAfter(const aMethodStep: TMethodStep;
    const aInsertAfterEditLine: TMethodEditLine);
var
    xNewMethdodEditLine: TMethodEditLine;
    xGroupBeginLine: TMethodEditGroupBeginLine;
    xGroupInsertAfterLine: TMethodEditLine;
begin
    xNewMethdodEditLine := CreateMethodEditLine(aMethodStep);

    xGroupInsertAfterLine := nil;

    if Assigned(aInsertAfterEditLine) then
    begin
        if aInsertAfterEditLine is TMethodEditGroupBeginLine then
        begin
            xGroupBeginLine := aInsertAfterEditLine as TMethodEditGroupBeginLine;
            xGroupInsertAfterLine := nil;
        end
        else if aInsertAfterEditLine is TMethodEditGroupEndLine then
        begin
            xGroupBeginLine := aInsertAfterEditLine.Parent.Parent as TMethodEditGroupBeginLine;
            xGroupInsertAfterLine := aInsertAfterEditLine.Parent;
        end
        else
        begin
            xGroupBeginLine := aInsertAfterEditLine.Parent as TMethodEditGroupBeginLine;
            xGroupInsertAfterLine := aInsertAfterEditLine;
        end;
    end
    else
    begin
        xGroupBeginLine := fMethodEditMainGroupLine;
    end;

    fMethodEditLines.InsertAfterLine(xNewMethdodEditLine, aInsertAfterEditLine);

    xGroupBeginLine.InsertAfterLine(xNewMethdodEditLine, xGroupInsertAfterLine);
end;

procedure TMethodEditForm.InsertMethodStepAtIndex(const aMethodStep: TMethodStep; const aIndex: integer);
var
    xInsertAfterEditLine: TMethodEditLine;
    xPrevIndex: integer;
begin
    xInsertAfterEditLine := nil;
    xPrevIndex := aIndex - 1;
    if self.MethodEditLineExists(xPrevIndex) then
    begin
        xInsertAfterEditLine := fMethodEditLines[xPrevIndex];
    end;

    InsertRecordExt(aIndex);

    InsertMethodStepAfter(aMethodStep, xInsertAfterEditLine);
end;

procedure TMethodEditForm.BuildStepHierarchy(const aMethodStepList: TObjectList<TMethodStep>;
    const aBeginLine: TMethodEditGroupBeginLine);
var
    x: integer;
    xMethodStep: TMethodStep;
begin
    for x := 0 to aMethodStepList.Count - 1 do
    begin
        xMethodStep := aMethodStepList[x];

        InsertMethodStepAtIndex(xMethodStep, x);
    end;
end;

class function TMethodEditForm.GetLineLevel(const aEditLine: TMethodEditLine): integer;
var
    xCurrentLine: TMethodEditLine;
    xCurrentGroupBeginLine: TMethodEditGroupBeginLine;
begin
    result := -1;
    xCurrentLine := aEditLine;

    while true do
    begin
        if not Assigned(xCurrentLine.Parent) then
            BREAK;

        xCurrentGroupBeginLine := xCurrentLine.Parent as TMethodEditGroupBeginLine;
        xCurrentLine := xCurrentGroupBeginLine;
        Inc(result);
    end;

    if aEditLine is TMethodEditGroupEndLine then
        result := result - 1;

end;

function TMethodEditForm.FindLineIndexOf(const aMethodLine: TMethodEditLine): integer;
var
    x: integer;
begin
    result := -1;
    for x := 0 to fMethodEditLines.Count - 1 do
    begin
        if fMethodEditLines[x] = aMethodLine then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

procedure TMethodEditForm.CheckLineLevels(const aIndices: TArray<integer>);
var
    x: integer;
    xEditLine: TMethodEditLine;
    xStack: TStack<TMethodEditLine>;
    xMisMatchError: string;
    xRecordIndex: integer;
begin
    xMisMatchError := '';

    xStack := TStack<TMethodEditLine>.Create();
    try
        for x := 0 to Length(aIndices) - 1 do
        begin
            xRecordIndex := aIndices[x];
            if not self.MethodEditLineExists(xRecordIndex) then
                CONTINUE;

            xEditLine := fMethodEditLines[xRecordIndex];

            if xEditLine is TMethodEditGroupBeginLine then
            begin
                xStack.Push(xEditLine as TMethodEditGroupBeginLine);
            end;

            if xEditLine is TMethodEditGroupEndLine then
            begin
                if xStack.Count = 0 then
                begin
                    xMisMatchError := 'End Condition without a mathing Begin Condition';
                    BREAK;
                end;
                xStack.Pop();
            end;
        end;

        if xStack.Count <> 0 then
        begin
            xMisMatchError := 'Begin Condition without a matching End Condition';
        end;

        if xMisMatchError <> '' then
            raise Exception.Create(xMisMatchError);

    finally
        FreeAndNil(xStack);
    end;
end;

procedure TMethodEditForm.MoveLinesToGroup(const aGroupBeginIndex: integer;
    const aFirstIndex, aLastIndex: integer);
var
    x: integer;
    xGroupBeginEditLine: TMethodEditGroupBeginLine;
    xCurrentEditLine: TMethodEditLine;
begin
    xGroupBeginEditLine := self.fMethodEditLines[aGroupBeginIndex] as TMethodEditGroupBeginLine;

    for x := aFirstIndex to aLastIndex do
    begin
        xCurrentEditLine := self.fMethodEditLines[x];
        if xCurrentEditLine.Parent = xGroupBeginEditLine.Parent then
        begin
            xGroupBeginEditLine.AddLine(xCurrentEditLine);
        end;
    end;
end;

procedure TMethodEditForm.DoClipboardDataOnSaveToStream(const aMethodSteps: TObjectList<TMethodStep>);
var
    x: integer;
    xRecIndex: integer;
begin
    for x := 0 to cxGrid1TableView1.Controller.SelectedRecordCount - 1 do
    begin
        xRecIndex := cxGrid1TableView1.Controller.SelectedRecords[x].RecordIndex;
        if not self.MethodEditLineExists(xRecIndex) then
            CONTINUE;

        aMethodSteps.Add(fMethodEditLines[xRecIndex].Value);
    end;
end;

procedure TMethodEditForm.InsertNewItemRowLine();
begin
    // NewItemRow concept:
    // The DevExpr Grid has a NewItemRow, but this did not work well so we try to simulate this idea by adding an extra
    // row at the end of the grid.  This row should not be editable or deletable, or selectable, it can only be focused.
    // The only purpose of this line is to allow the user to insert or paste other rows at the end of the grid.
    // This conept works right now only because in most of the functions we exit
    // because self.MethodEditLineExists( NewItemRowIndex ) = false
    InsertRecordExt(0);
end;

function TMethodEditForm.AddMethodStepLine(const aMethodStep: TMethodStep; const aRecIndex: integer;
    aSetFocus: boolean): integer;
begin
    result := -1;
    fAddingNewRecord := true;
    try
        if (aMethodStep = nil) then
            EXIT;

        InsertMethodStepAtIndex(aMethodStep, aRecIndex);

        ChangeData();

        if (aSetFocus) then
        begin
            cxGrid1TableView1.Controller.FocusRecord(result, true);
            if cxGrid1.CanFocus then
            begin
                cxGrid1.SetFocus;
            end;
        end;
    finally
        fAddingNewRecord := false;
    end;
end;

function TMethodEditForm.AddMethodStep(const aActionName, aOption: string; aRecIndex: integer;
    aSetFocus: boolean): integer;
var
    xMethodStep: TMethodStep;
begin
    result := -1;
    if (aActionName = '') then
        EXIT;

    xMethodStep := CreateMethodStep(aActionName, aOption);
    result := self.AddMethodStepLine(xMethodStep, aRecIndex, aSetFocus);
    SetFocusAndSelectRecord(aRecIndex);
end;

class function TMethodEditForm.GetCompleteSummaryText(const aMethodStep: TMethodStep): string;
begin
    if aMethodStep.IsCodeStep then
        result := ''
    else
        result := aMethodStep.GetActionCaptionText();

    result := result + aMethodStep.OptionSummary;
end;

procedure TMethodEditForm.EditMethodStepComment(const aIndex: integer);
var
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
    xComment: string;
    xDescriptionText: string;
begin
    if not MethodEditLineExists(aIndex) then
        EXIT;

    xMethodLine := fMethodEditLines[aIndex];

    xMethodStep := xMethodLine.Value;

    xComment := xMethodStep.M_Comments;

    xDescriptionText := GetCompleteSummaryText(xMethodStep);

    if TfrmMethodStepCommentEditor.ShowDialog(xDescriptionText, xComment, self.IsWritable) <> mrOK then
        EXIT;

    xMethodStep.M_Comments := xComment;
    ForceGridResize();
    self.ChangeData();
end;

function TMethodEditForm.IsWritable: boolean;
begin
    EXIT(self.GetAttribute = meaDefault);
end;

procedure TMethodEditForm.EditMethodStepProperties(const aIndex: integer);
var
    xMethodLine: TMethodEditLine;
    xMethodStep: TMethodStep;
begin
    if not MethodEditLineExists(aIndex) then
        EXIT;

    xMethodLine := fMethodEditLines[aIndex];

    xMethodStep := xMethodLine.Value;
    if xMethodStep is TBlankMethodStep then
        EXIT;

    if self.EditMethodStepModal(xMethodStep) then
    begin
        self.ChangeData();
    end;
end;

procedure TMethodEditForm.EditMethodVaraibles;
begin
    TEditMethodParamsExt.EditMethodStoredParameters(self.DataName);
end;

procedure TMethodEditForm.EditMethodSettings(const aMethodName: string; aMemo: TMemo; aIsRunner: boolean);
var
    xForm: TfrmMethodSettingsEditor;
    xRec: TMethodSettingsRec;
begin
    xForm := TfrmMethodSettingsEditor.Create(nil);
    try
        xRec := fSettings;
        xForm.Settings := xRec;
        xForm.IsRunner := aIsRunner;
        if (xForm.ShowModal = mrOK) then
        begin
            fSettings := xForm.Settings;
            self.ChangeData();
        end;
    finally
        FreeAndNil(xForm);
    end;

    UpdateMethodInfo(aMemo);
end;

class function TMethodEditForm.EditMethodStepModalIntern(aParameter: TCustomSetting): boolean;
var
    xFunctionParams: TCustomSettingEditFunctionParams;
    xCancel: boolean;
begin
    xFunctionParams := aParameter.CreateEditFunctionParams();
    try
        aParameter.EditFunction(xFunctionParams, xCancel);
        if xCancel then
            EXIT(false);

        if Assigned(xFunctionParams) then
            xFunctionParams.SetAllValuesToNewValue(nil);
        EXIT(true);
    finally
        FreeAndNil(xFunctionParams);
    end;
end;

procedure TMethodEditForm.LoadViewAndMethodData(aMemo: TMemo);
var
    xWidthText, xOrderText: string;
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        fSettings := xDA.ReadRec(fViewItem.name);
    finally
        FreeAndNil(xDA);
    end;

    LoadView(xOrderText, xWidthText);
    LoadMethodData(aMemo);
end;

procedure TMethodEditForm.LoadMethodData(aMemo: TMemo);
var
    xMethodStepList: TObjectList<TMethodStep>;
begin
    gGUIManager.SetCursor(crHourglass);

    try
        UpdateMethodAttributes();

        xMethodStepList := TObjectList<TMethodStep>.Create();
        xMethodStepList.OwnsObjects := true;
        cxGrid1TableView1.BeginUpdate;
        try
            fMethodEditDataHelper.ReadMethod(fViewItem.name, xMethodStepList, true);

            fMethodEditLines.Clear();

            BuildStepHierarchy(xMethodStepList, MainGroupLine);

        finally
            cxGrid1TableView1.EndUpdate;
        end;
        // Methoden-Informationen laden
        UpdateMethodInfo(aMemo);
    finally
        gGUIManager.SetCursor(crDefault);
    end;
end;

procedure TMethodEditForm.SaveDataIntern;
var
    x: integer;
    xMethodDA: TMethodDataAdaptor;
    xSettingsDA: TMethodSettingsDataAdaptor;
    xSeq: integer;
    xMethodStep: TMethodStep;
    xMethodRec: TMethodRec;
begin
    // Methode muss eine Zeile haben, sonnst verschwindet sie
    if fMethodEditLines.Count = 0 then
        self.AddAction('BLANK', GetInsertRecIndex());

    xSeq := 0;
    xMethodDA := TMethodDataAdaptor.Create();
    try
        // delete temp. method
        xMethodDA.DeleteName(STR_TEMP_METHODNAME);

        // save method as temp. method
        for x := 0 to fMethodEditLines.Count - 1 do
        begin
            xMethodStep := fMethodEditLines[x].Value;
            // Bestimmen von xSeq
            xSeq := xSeq + 10;
            // Abstand >= 10, sonst kann man mit alten Editoren keine Aktionen mehr dazwischensetzen!

            xMethodRec := fMethodEditDataHelper.MakeMethodRecFromMethodStep(xMethodStep,
                STR_TEMP_METHODNAME, xSeq);
            xMethodDA.WriteMethodRec(xMethodRec, true { ForceAppend } );
        end;

        // delete real method
        xMethodDA.DeleteName(fSettings.MethodName);

        // save temp. method as real method
        xMethodDA.SaveNameAs(STR_TEMP_METHODNAME, fSettings.MethodName);

        // delete temp. method
        xMethodDA.DeleteName(STR_TEMP_METHODNAME);
    finally
        FreeAndNil(xMethodDA);
    end;

    xSettingsDA := TMethodSettingsDataAdaptor.Create;
    try
        xSettingsDA.WriteRec(fSettings);
    finally
        FreeAndNil(xSettingsDA);
    end;
    TMethodDataCache.Instance.RefreshMethodNames;
end;

procedure TMethodEditForm.AddAction(const aActionName: string; aRecIndex: integer);
begin
    AddMethodStep(aActionName, '', aRecIndex, true);

    self.EditMethodStepProperties(aRecIndex);
    ForceGridResize();
end;

procedure TMethodEditForm.AddAction(const aActionName: string);
begin
    AddAction(aActionName, self.GetInsertRecIndex());
end;

function TMethodEditForm.GetAttributeText(): string;
var
    xAttribute: TVisibilityAttribute;
begin
    xAttribute := fViewItem.GetVisibilityAttribute();

    if (xAttribute = meaHidden) then
        EXIT(TLanguageString.Read('Hidden', 'Verborgen'));
    if (xAttribute = meaReadOnly) then
        EXIT(TLanguageString.Read('Read-Only', 'Schreibgeschützt'));

    EXIT('');
end;


end.
