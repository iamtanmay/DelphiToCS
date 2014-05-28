unit MethodEditorDataAdaptor;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Project      : New Editor
  Author       : Wolfgang Lyncke (wl)
  Description  : Data adaptor specific for method editor
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  22.06.05 wl                               TN2440    initial version
  22.06.05 wl  TClipboardDataMethodSteps.LoadFromStream  TN2441    Nach Paste wird jetzt selektiert
  24.06.05 wl  TClipboardDataMethodSteps.LoadFromStream  TN2441    Einfügen an der richtigen Stelle
  07.07.05 wl  STR_VIEW_DATA_WIDE                        TN2495    entfernt
  13.07.05 wl  TMethodEditorDataAdaptor.SaveViewValues   TN2440.1  fügt eine neue Spaltenanordnung hinzu oder ändert die Daten einer bekannten Anordnung
  13.07.05 wl  TMethodEditorDataAdaptor.DeleteView       TN2440.1  löscht eine Spaltenanordnung
  02.08.05 wl                                            TN2501.1  gSchedDatasourceType entfernt - braucht im Editor nicht mehr abgefragt werden
  15.08.05 wl  LoadMethodViewValues                      TN2558.3  Wenn noch keine View-Daten existieren, wird der erste View in der Liste geladen (Default View)
  15.08.05 wl  SaveViewValues                            TN2558.3  Wenn ein View-Name schon existiert, werden die Daten überschrieben
  20.10.05 wl  CheckMethodStepBeforePost                 TN2659    von DM_sampl hierher, mit Unterscheidung Unterscheidung MoveTubeAction und SimpleTubeAction
  14.11.05 pk  CheckMethodStepBeforePost                 TN2720    use CreateParser to create the correct parser
  14.11.05 pk  CheckMethodStepBeforePost                 TN2720    call RackExistsInLayout instead of directly using the Layout.db
  14.11.05 pk  CheckMethodStepBeforePost                 TN2720    new param : aLiqHDataAdaptor - replaces the need to use DMRack
  24.11.05 pk                                            TN2805    objsampl removed from uses
  25.11.05 pk  CheckMethodStepBeforePost                 TN2899    TubeTool can be a variable
  29.03.06 wl  CheckMethodStepBeforePost                 TN2970    Funktion komplett entfernt
  06.04.06 pk                                            TN3024   New field SchedSharedID
  06.04.06 pk                                            TN3032   New field Iterate
  18.04.06 wl                                            TN3025    Inactive komplett entfernt
  28.06.06 wl  TExcelOleManagerMethodSteps               TN3172    Neu: Objekt für den Excel-Export/Import von Methoden
  28.06.06 wl  TExcelOleManagerMethodSteps               TN3172    benutzt keine Tabelle, sondern das Method-Grid
  28.06.06 wl  TExcelOleManagerMethodSteps               TN3172    es können auch Teile einer Methode exportiert werden
  04.08.06 pk  TExcelOleManagerMethodSteps.ImportDataSet TN3427    Increment grid recordindex as well
  02.10.06 wl  GetMaxFieldLength                         TN3236    ermittelt die Feldlänge über die DataID
  20.02.07 wl                                            TN3016   entfernt: uses ParserWrapper
  27.02.07 pk  gmInsertRecordExt                         TN3597   default value for priority field is NOT set anymore
  09.01.08 wl                                            TN3972   diverse Änderungen aufgrund der neuen Method.db-Struktur
  09.01.08 wl                                            TN3972   nicht mehr vorhanden: Seq; neu: Used Tips, Method name
  07.02.08 wl  INT_COL_FIRST_SIZABLE                     TN4009   neu
  06.05.08 wl  INT_METHOD_COL_INACTIVE                   TN4074   New column Inactive
  06.05.08 wl  ColumnMayBeVisible                        TN4074   Column Inactive is not visible
  05.06.08 pk                                            TN4139   uses changed
  15.10.08 pk  gmGetStringValue                          TN4258   avoid exceptions
  09.12.08 pk                                            TN4279   uses changed
  16.01.09 wl                                            TN4362   an Änderungen in TViewItem angepasst
  10.08.09 wl                                            TN4702   Strings werden jetzt direkt geladen
  28.08.09 pk                                            TN4753   Liquids replaced by DesignLiquids
  26.10.09 wl  LoadViews/SaveViews                       TN4831   IConfigurationSet replaces ILocalIniFile
  06.05.10 wl                                   TN5087   Spalten sind jetzt nicht mehr verschiebbar oder editierbar
  06.05.10 wl                                   TN5087   überflüssige Spalten entfernt
  12.05.10 wl                                   TN5064   Massive changes
  18.05.10 wl                                   TN5064   Methode entfernt
  21.09.10 pk                                   TN5089   Various changes to MethodEditor for displaying indented actions
  23.09.10 pk  TClipboardDataMethodSteps        TN5089   now has an instance of TMethodEditDataAdaptor for creating new methodsteps
  29.09.10 pk                                   TN5283   Short and Long comment combined, unneeded MethodEditor columns removed
  30.09.10 pk  SaveToStream                              TN5283   add Clipboard End Delimiter in finally, otherwise Load function hangs in endless loop
  07.02.11 wl  CreateMethodStep                 TN5461   Parameter geändert
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Classes,
    cxGridTableView,
    ContNrs,
    SysUtils,
    GeneralTypes,
    ClipboardData,
    ParserWrapperCommon,
    LiqHDataAdaptor,
    ExcelOleManager,
    ListClasses,
    MethodStep,
    MethodStepList,
    MethodDataAdaptor,
    MethodStepDataFields;

const
    INT_METHOD_COL_SUMMARY = 0;
    INT_METHOD_COL_COMMENT = 1;

    INT_NUMBEROFCOLUMNS = 2; // muss unbedingt mit Spaltenanzahl übereinstimmen

    STR_TEMP_METHODNAME = '_TEMP~~~~~METHOD';

type
    EMethodLineSave = class(Exception);

    TColumnPositions = array [0 .. INT_NUMBEROFCOLUMNS - 1] of integer;

    TExcelOleManagerMethodSteps = class(TExcelOleManager)
    public
        function MethodConnectionExists(const aMethodName: string): boolean;
        procedure ExportDataSet(const aMethodName: string; aGridTableView: TcxGridTableView);
        procedure ImportDataSet(const aMethodName: string; aGridTableView: TcxGridTableView;
            aFocusedRecIndex: integer);
    end;

    TMethodEditLine = class
    private
        fValue: TMethodStep;
        fParent: TMethodEditLine;
    public
        property Value: TMethodStep read fValue write fValue;
        property Parent: TMethodEditLine read fParent write fParent;
    end;

    TMethodEditLineList = class(TGenericObjectList<TMethodEditLine>)
    public
        procedure InsertAfterLine(const aMethodEditLine, aInsertAfterMethodEditLine: TMethodEditLine);
    end;

    TMethodEditGroupBeginLine = class(TMethodEditLine)
    private
        fLines: TMethodEditLineList;
        // fEndLine : TMethodEditLine;
        fExpanded: boolean;
        procedure AdoptLine(const aMethodEditLine: TMethodEditLine);
        function GetEndLine: TMethodEditLine;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure ClearLines();
        procedure RemoveLine(const aMethodEditLine: TMethodEditLine);
        procedure AddLine(const aMethodEditLine: TMethodEditLine);
        procedure InsertAfterLine(const aMethodEditLine, aInsertAfterMethodEditLine: TMethodEditLine);
        procedure MoveLinesToGroup(const aMethodEditGroupBeginLine: TMethodEditGroupBeginLine;
            const aInsertLinesAfter: TMethodEditLine);
        // property EndLine : TMethodEditLine read fEndLine write fEndLine;
        property Expanded: boolean read fExpanded write fExpanded;
        property EndLine: TMethodEditLine read GetEndLine;
    end;

    TMethodEditGroupEndLine = class(TMethodEditLine)

    end;

    TMethodEditDataAdaptor = class
    public
        fOnAddEditFunctions: TNotifyEvent;
        constructor Create(const aOnAddEditFunctions: TNotifyEvent);
        function CreateMethodStep(const aActionName: string): TMethodStep;
        function CreateMethodStepFromMethodRec(const aMethodRec: TMethodRec; const aCreateIfInactive: boolean)
            : TMethodStep;
        function MakeMethodRecFromMethodStep(const aMethodStep: TMethodStep; const aMethodName: string;
            const aSeq: integer): TMethodRec;
        procedure ReadMethod(aMethodName: string; aMethodStepList: TMethodStepList;
            aReadInactiveRecs: boolean); overload;
        function MethodStepToStrArray(const aMethodStep: TMethodStep): TStringArray;
        function MethodStepFromStrArray(const aArray: TStringArray): TMethodStep;
    end;

    TClipboardLoadFromStream = procedure(const aMethodSteps: TMethodStepList;
        const aFirstRecordIndex: integer) of object;
    TClipboardSaveToStream = procedure(const aMethodSteps: TMethodStepList) of object;

    TClipboardDataMethodSteps = class(TClipboardData)
    protected
        fFirstRecordIndex: integer;
        fMethodEditDataAdaptor: TMethodEditDataAdaptor;
        fOnLoadFromStream: TClipboardLoadFromStream;
        fOnSaveToStream: TClipboardSaveToStream;

        procedure LoadFromStream(Stream: TStream); override;
        procedure SaveToStream(Stream: TStream); override;
    public
        constructor Create(const aMethodEditDataAdaptor: TMethodEditDataAdaptor;
            const aFirstRecordIndex: integer = 0);
        property OnLoadFromStream: TClipboardLoadFromStream read fOnLoadFromStream write fOnLoadFromStream;
        property OnSaveToStream: TClipboardSaveToStream read fOnSaveToStream write fOnSaveToStream;
    end;

function gmGetStringValue(aGridTableView: TcxGridTableView; aRecIndex, aItemIndex: integer): string;
procedure gmSetStringValue(aGridTableView: TcxGridTableView; aRecIndex, aItemIndex: integer; aValue: string);
function gmInsertRecordExt(aGridTableView: TcxGridTableView; aRecIndex: integer): integer;


implementation


uses
    Windows,
    Forms,
    Controls,
    Variants,
    MethodTypes,
    MethodGUIParsing,
    AppSettings,
    SamGlobe,
    AppTypes,
    DesignLiquids,
    MethodSettings,
    DialogUtils,
    CommonTypes,
    ParserWrapperOptimal,
    ConfigurationFile,
    RunStepBuilderTypeDictionary;

var
    global_ClipboardFormat_MethodSteps: integer;

const
    ZINSSERMETHODSTEPS_CLIPBOARD = 'ZinsserWinLissyMethodSteps';

    STR_CLIPBOARDTEXT_END = #10 + #10 + 'END';

    cNumMethodStepFields = 4;

function gmGetStringValue(aGridTableView: TcxGridTableView; aRecIndex, aItemIndex: integer): string;
begin
    result := ''; // Variant-Value ist Null
    if aGridTableView.DataController.Values[aRecIndex, aItemIndex] = Null then
        EXIT;
    result := aGridTableView.DataController.Values[aRecIndex, aItemIndex];
end;

function gmGetColumnName(aGridTableView: TcxGridTableView; aItemIndex: integer): string;
begin
    result := ''; // Variant-Value ist Null
    if not Assigned(aGridTableView.Columns[aItemIndex]) then
        EXIT;
    result := aGridTableView.Columns[aItemIndex].Caption;
end;

procedure gmSetStringValue(aGridTableView: TcxGridTableView; aRecIndex, aItemIndex: integer; aValue: string);
begin
    aGridTableView.DataController.Values[aRecIndex, aItemIndex] := aValue;
end;

function gmInsertRecordExt(aGridTableView: TcxGridTableView; aRecIndex: integer): integer;
begin
    if (aRecIndex < 0) then
        aRecIndex := aGridTableView.DataController.RecordCount; // Add as last record

    result := aGridTableView.DataController.InsertRecord(aRecIndex);
    aGridTableView.DataController.FocusedRecordIndex := result;
end;

{ TExcelOleManagerMethodSteps }

procedure TExcelOleManagerMethodSteps.ExportDataSet(const aMethodName: string;
    aGridTableView: TcxGridTableView);
var
    x, xRecIndex, xItemIndex: integer;
    xText: string;
    xModalResult: TModalResult;
begin
    if (aGridTableView.Controller.SelectedRecordCount >= 1) and
        (aGridTableView.Controller.SelectedRecordCount < aGridTableView.DataController.RecordCount) then
    begin
        xModalResult := TDialogUtils.MessageBox(TLanguageString.
            Read('Export selected lines? (No = Export all)',
            'Markierte Zeilen exportieren? (Nein = Alles exportieren)'),
            TLanguageString.Read('Export to Excel', 'Excel-Export'), MB_YESNOCANCEL);
        if (xModalResult = IDCANCEL) then
            EXIT;
        if (xModalResult = IDNO) then
            aGridTableView.Controller.SelectAll;
    end
    else
        aGridTableView.Controller.SelectAll;

    self.CreateNewWorksheet(aMethodName);

    // Feldnamen
    for xItemIndex := 0 to aGridTableView.DataController.ItemCount - 1 do
    begin

        xText := gmGetColumnName(aGridTableView, xItemIndex);

        self.SetFieldValue(1, xItemIndex + 1, xText);
    end;

    // erst auslesen
    for x := 0 to aGridTableView.Controller.SelectedRecordCount - 1 do
    begin
        xRecIndex := aGridTableView.Controller.SelectedRecords[x].RecordIndex;
        if (xRecIndex < 0) then
            CONTINUE;

        for xItemIndex := 0 to aGridTableView.DataController.ItemCount - 1 do
        begin

            xText := gmGetStringValue(aGridTableView, xRecIndex, xItemIndex);

            self.SetFieldValue(x + 2, xItemIndex + 1, xText);
        end;
    end;

    self.SetWorksheetVisible();
end;

procedure TExcelOleManagerMethodSteps.ImportDataSet(const aMethodName: string;
    aGridTableView: TcxGridTableView; aFocusedRecIndex: integer);
var
    xRow, xRecIndex, xItemIndex: integer;
    xText: string;
begin
    xRecIndex := aFocusedRecIndex;
    xRow := 2;
    while (self.GetFieldValue(xRow, 1) <> '') do
    begin

        xRecIndex := gmInsertRecordExt(aGridTableView, xRecIndex);

        for xItemIndex := 0 to aGridTableView.DataController.ItemCount - 1 do
        begin

            xText := self.GetFieldValue(xRow, xItemIndex + 1);

            gmSetStringValue(aGridTableView, xRecIndex, xItemIndex, xText);

        end;
        Inc(xRecIndex);
        Inc(xRow);
    end;
end;

function TExcelOleManagerMethodSteps.MethodConnectionExists(const aMethodName: string): boolean;
begin
    result := self.ConnectionExists() and self.WorksheetHasName(aMethodName);
end;

{ TClipboardDataMethodSteps }

constructor TClipboardDataMethodSteps.Create(const aMethodEditDataAdaptor: TMethodEditDataAdaptor;
    const aFirstRecordIndex: integer = 0);
begin
    inherited Create(global_ClipboardFormat_MethodSteps);
    fFirstRecordIndex := aFirstRecordIndex;
    fMethodEditDataAdaptor := aMethodEditDataAdaptor;

end;

procedure TClipboardDataMethodSteps.SaveToStream(Stream: TStream);
var
    x, i: integer;
    xText: string;
    xMethodStepList: TMethodStepList;
    xArray: TStringArray;
begin
    if not Assigned(self.fOnSaveToStream) then
        EXIT;
    try
        xMethodStepList := TMethodStepList.Create();
        try
            xMethodStepList.OwnsObjects := false;
            fOnSaveToStream(xMethodStepList);

            if xMethodStepList.Count = 0 then
                EXIT;
            SetLength(xArray, cNumMethodStepFields);
            for i := 0 to xMethodStepList.Count - 1 do
            begin
                xArray := fMethodEditDataAdaptor.MethodStepToStrArray(xMethodStepList[i]);
                for x := 0 to Length(xArray) - 1 do
                begin
                    xText := xArray[x];
                    self.WriteAnsiString(Stream, ansistring(xText));
                end;

            end;

        finally
            FreeAndNil(xMethodStepList);
        end;
    finally
        self.WriteAnsiString(Stream, STR_CLIPBOARDTEXT_END);
    end;
end;

procedure TClipboardDataMethodSteps.LoadFromStream(Stream: TStream);
var
    x: integer;
    xText: string;
    xMethodStep: TMethodStep;
    xMethodSteps: TMethodStepList;
    xArray: TStringArray;
begin
    try
        xMethodSteps := TMethodStepList.Create();
        try
            xMethodSteps.OwnsObjects := false;

            SetLength(xArray, cNumMethodStepFields);
            while (true) do
            begin

                for x := 0 to cNumMethodStepFields - 1 do
                begin
                    xText := string(ReadAnsiString(Stream));
                    if (xText = STR_CLIPBOARDTEXT_END) then
                        BREAK;
                    xArray[x] := xText;
                end;

                if (xText = STR_CLIPBOARDTEXT_END) then
                    BREAK;

                xMethodStep := fMethodEditDataAdaptor.MethodStepFromStrArray(xArray);
                xMethodSteps.Add(xMethodStep);

            end;

            self.fOnLoadFromStream(xMethodSteps, fFirstRecordIndex);

        finally
            FreeAndNil(xMethodSteps);
        end;
    except

    end;
end;

{ TMethodEditLineList }

procedure TMethodEditLineList.InsertAfterLine(const aMethodEditLine, aInsertAfterMethodEditLine
    : TMethodEditLine);
var
    xIndex: integer;
begin
    xIndex := 0;
    if Assigned(aInsertAfterMethodEditLine) then
    begin
        xIndex := self.IndexOf(aInsertAfterMethodEditLine) + 1;
    end;

    self.Insert(xIndex, aMethodEditLine);
end;

{ TMethodEditGroupBeginLine }

procedure TMethodEditGroupBeginLine.RemoveLine(const aMethodEditLine: TMethodEditLine);
begin
    if aMethodEditLine is TMethodEditGroupBeginLine then
    begin
        // Add my child to my parent's child list after myself
        (aMethodEditLine as TMethodEditGroupBeginLine).MoveLinesToGroup(self, aMethodEditLine);
    end;

    fLines.Remove(aMethodEditLine);
end;

procedure TMethodEditGroupBeginLine.AdoptLine(const aMethodEditLine: TMethodEditLine);
var
    xOldParent: TMethodEditLine;
begin
    xOldParent := aMethodEditLine.Parent;
    if xOldParent = self then
        EXIT;

    if Assigned(xOldParent) then
    begin
        (xOldParent as TMethodEditGroupBeginLine).RemoveLine(aMethodEditLine);
    end;

    aMethodEditLine.Parent := self;
end;

procedure TMethodEditGroupBeginLine.AddLine(const aMethodEditLine: TMethodEditLine);
begin
    AdoptLine(aMethodEditLine);
    fLines.Add(aMethodEditLine);
end;

procedure TMethodEditGroupBeginLine.MoveLinesToGroup(const aMethodEditGroupBeginLine
    : TMethodEditGroupBeginLine; const aInsertLinesAfter: TMethodEditLine);
var
    x: integer;
    xLine: TMethodEditLine;
begin
    for x := fLines.Count - 1 downto 0 do
    begin
        xLine := fLines[x];
        if xLine is TMethodEditGroupEndLine then
            BREAK;

        RemoveLine(xLine);
        aMethodEditGroupBeginLine.InsertAfterLine(xLine, aInsertLinesAfter);
    end;

end;

procedure TMethodEditGroupBeginLine.InsertAfterLine(const aMethodEditLine, aInsertAfterMethodEditLine
    : TMethodEditLine);
begin
    AdoptLine(aMethodEditLine);
    fLines.InsertAfterLine(aMethodEditLine, aInsertAfterMethodEditLine);
end;

procedure TMethodEditGroupBeginLine.ClearLines;
begin
    fLines.Clear();
    // fEndLine := nil;
end;

constructor TMethodEditGroupBeginLine.Create;
begin
    inherited Create();
    fLines := TMethodEditLineList.Create(false);
    fExpanded := true;
end;

destructor TMethodEditGroupBeginLine.Destroy;
begin
    FreeAndNil(fLines);
    inherited;
end;

function TMethodEditGroupBeginLine.GetEndLine: TMethodEditLine;
begin
    result := nil;
    if fLines.Count <= 0 then
        EXIT;
    result := fLines[fLines.Count - 1];
end;

{ TMethodEditDataAdaptor }
constructor TMethodEditDataAdaptor.Create(const aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create();
    fOnAddEditFunctions := aOnAddEditFunctions;
end;

function TMethodEditDataAdaptor.CreateMethodStep(const aActionName: string): TMethodStep;
begin
    result := TRunStepBuilderTypeDictionary.Instance.CreateMethodStep(aActionName, '', '', false, true,
        fOnAddEditFunctions);
end;

function TMethodEditDataAdaptor.CreateMethodStepFromMethodRec(const aMethodRec: TMethodRec;
    const aCreateIfInactive: boolean): TMethodStep;
begin
    result := TRunStepBuilderTypeDictionary.Instance.CreateMethodStepFromMethodRec(aMethodRec,
        aCreateIfInactive, fOnAddEditFunctions);
end;

function TMethodEditDataAdaptor.MakeMethodRecFromMethodStep(const aMethodStep: TMethodStep;
    const aMethodName: string; const aSeq: integer): TMethodRec;
begin
    result := aMethodStep.AsMethodRec(aMethodName, aSeq);
end;

function TMethodEditDataAdaptor.MethodStepFromStrArray(const aArray: TStringArray): TMethodStep;
var
    xMethodRec: TMethodRec;
begin
    xMethodRec.Valid := true;
    xMethodRec.Action := aArray[0];
    xMethodRec.Options := aArray[1];
    xMethodRec.Comment := aArray[2];
    xMethodRec.Inactive := aArray[3] = '1';
    result := CreateMethodStepFromMethodRec(xMethodRec, true);
end;

function TMethodEditDataAdaptor.MethodStepToStrArray(const aMethodStep: TMethodStep): TStringArray;
var
    xMethodRec: TMethodRec;
begin
    xMethodRec := MakeMethodRecFromMethodStep(aMethodStep, '', 0);
    SetLength(result, cNumMethodStepFields);
    result[0] := xMethodRec.Action;
    result[1] := xMethodRec.Options;
    result[2] := xMethodRec.Comment;
    if xMethodRec.Inactive then
        result[3] := '1'
    else
        result[3] := '0'
end;

procedure TMethodEditDataAdaptor.ReadMethod(aMethodName: string; aMethodStepList: TMethodStepList;
    aReadInactiveRecs: boolean);
var
    xDA: TMethodDataAdaptor;
    xMethodRec: TMethodRec;
    xMethodStep: TMethodStep;
begin
    xDA := TMethodDataAdaptor.Create();
    try
        xDA.SelectAndOpenMethod(aMethodName, false, true);
        try
            while not xDA.DataProvider.Eof do
            begin
                xMethodRec := xDA.ReadMethodrecFromDataSet(xDA.DataProvider);
                xDA.DataProvider.Next;

                xMethodStep := CreateMethodStepFromMethodRec(xMethodRec, aReadInactiveRecs);
                if not Assigned(xMethodStep) then
                    CONTINUE;

                aMethodStepList.Add(xMethodStep);

            end;
        finally
            xDA.Close();
        end;
    finally
        xDA.Free;
    end;
end;


initialization


global_ClipboardFormat_MethodSteps := RegisterClipboardFormat(ZINSSERMETHODSTEPS_CLIPBOARD);


end.
