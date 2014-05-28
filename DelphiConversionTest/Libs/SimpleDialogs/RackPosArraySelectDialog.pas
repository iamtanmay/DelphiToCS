{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  01.03.13 wl                                      TN6101   Initial Revision
  22.04.13 wl                                      TN6095   für MultiPageDialog geändert
  08.08.13 wl                                      TN6095   verbesserte Benutzerführung
  12.08.13 wl  Create                              TN6214   neuer Parameter SingleValueAllowed
  20.01.14 ts                                      TN6348   FitLayoutToScreen auskommentiert, sonst wird gespeicherter Zoom nicht verwendet
  10.02.14 ts                                      TN6353   Volume,Substance,LiqPar added
  08.04.14 ts                                      TN6391   SourceRackName,-Position hinzu für Anzeige von 2.Layout
  25.04.14 ts                                      TN6400   TN6391 ohne 2.Layout werden zoom Werte nicht korrekt gespeichert
  ----------------------------------------------------------------------------------------------------------- }

unit RackPosArraySelectDialog;


interface


uses
    Forms,
    Classes,
    Controls,
    StdCtrls,
    ExtCtrls,
    ComCtrls,
    Generics.Collections,
    cxGraphics,
    cxControls,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    cxStyles,
    cxCustomData,
    cxFilter,
    cxData,
    cxDataStorage,
    cxEdit,
    cxGridLevel,
    cxGridCustomTableView,
    cxGridTableView,
    cxClasses,
    cxGridCustomView,
    cxGrid,

    ViewLayout,
    Rack,
    CustomLayoutManager,
    MultiPageDialog,
    LayoutElementSelectDlgEvents,
    EditableParameter;

type
    TBasicEditFunctionsGetNoOfPos = function(aOrder: integer): integer of object;

    TfrmRackPosArraySelectDialog = class(TMultiDialogPage)
        cxEditRepository1: TcxEditRepository;
        Panel1: TPanel;
        cxGrid1: TcxGrid;
        cxGrid1TableView1: TcxGridTableView;
        cxGrid1Level1: TcxGridLevel;
        Splitter1: TSplitter;
        PanelTop: TPanel;
        pnlSourceLayout: TPanel;
        Panel3: TPanel;
        btnClear: TButton;
        btnAddPositions: TButton;
        rbInteger: TRadioButton;
        rbMatrix: TRadioButton;
        lblTitle: TLabel;
        GroupBox1: TGroupBox;
        edSourceRackLastPos: TEdit;
        lblPositionSeparator: TLabel;
        edSourceRackFirstPos: TEdit;
        lblSourcePosition: TLabel;
        edSourceRackName: TEdit;
        lblSourceRack: TLabel;
        edSourceNoOfPos: TEdit;
        lblSourceNoOfPos: TLabel;
        GroupBox2: TGroupBox;
        lblLiqParam: TLabel;
        lblVolume: TLabel;
        lblSubstance: TLabel;
        edVolume: TEdit;
        cmbLiqParam: TComboBox;
        cmbSubstance: TComboBox;
        GroupBox3: TGroupBox;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        edRackLastPos: TEdit;
        edRackFirstPos: TEdit;
        edRackName: TEdit;
        edNoOfPos: TEdit;
        pnlLayout: TPanel;
        rbArray: TRadioButton;
        edSourcePositionArray: TEdit;
        edPositionArray: TEdit;
        procedure FormResize(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure btnRefreshClick(Sender: TObject);
        procedure btnAddPositionsClick(Sender: TObject);
        procedure btnClearClick(Sender: TObject);
        procedure rbArrayClick(Sender: TObject);
        procedure rbIntegerClick(Sender: TObject);
        procedure rbMatrixClick(Sender: TObject);
    private
        fLayoutManager: TLayoutElementSelectDlgLayoutManager;
        fSourceLayoutManager: TLayoutElementSelectDlgLayoutManager;
        fLayoutName: string;
        fViewLayout: TfrmViewLayout;
        fViewSourceLayout: TfrmViewLayout;
        fColIndexRackName: integer;
        fColIndexPosition: integer;
        fColIndexSourceRackName: integer;
        fColIndexSourcePosition: integer;
        fColIndexVolume: integer;
        fColIndexSubstance: integer;
        fColIndexLiqParam: integer;
        fReadOnlyRackPos: boolean;
        fData: TArray<TEditableParameterEditData>;
        fCheckValues: boolean;
        fSingleValueAllowed: boolean;
        fFixedNoOfPos: integer;
        fDialogEvents, fDialogEventsSource: TLayoutElementSelectDlgEvents;
        fOnGetNoOfPos: TBasicEditFunctionsGetNoOfPos;
        fArrayLengthRefToOrder: integer;
        // procedure FitLayoutToScreen();
        procedure ChangeSelectionAfterManualChange();
        procedure AddLine(aIndex: integer; const aRackName, aPosition, aSourceRackName, aSourcePosition,
            aVolume, aSubstance, aLiqParam: string);
        function DoOnChooseRackFirstPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        function DoOnChooseRackLastPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        function DoOnChooseSourceRackFirstPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        function DoOnChooseSourceRackLastPos(const aRackName: string; aPosInt, aMaxRows: integer;
            const aParamIndexOffset: integer; aSetFocus: boolean): string;
        function NeedsLayout(const aData: TArray<TEditableParameterEditData>): boolean;
        procedure AddColumns();
        function GetColumnData(aIndex: integer): TArray<string>;
        procedure RemoveEmptyColumns();
        procedure AddPositions(aRack, aSourceRack: TRack);
    public
        constructor Create(aOwner: TComponent; const aLayoutName: string; aCheckValues: boolean;
            aSingleValueAllowed: boolean); reintroduce;
        destructor Destroy; override;

        procedure Prepare(const aData: TArray<TEditableParameterEditData>;
            aOnGetNoOfPos: TBasicEditFunctionsGetNoOfPos; aArrayLengthRefToOrder: integer);
        procedure FirstSetFocus(); override;
        procedure RefreshPageData(); override;
        function WritePageData(aCheckBefore: boolean): boolean; override;
        function GetEvents: TLayoutElementSelectDlgEvents;
        function GetEventsSource: TLayoutElementSelectDlgEvents;
    end;


implementation


{$R *.dfm}

uses
    SysUtils,
    Windows,
    Variants,
    cxEditRepositoryItems,

    MathUtils,
    GeneralTypes,
    DialogUtils,
    BasicEditFunctions,
    ControlUtils,
    Utility_DevExpress,
    MethodGUIParsing,
    SubstanceDataDataAdaptor,
    LiqHDataAdaptor,
    StringUtilities;

{ TfrmRackPosArraySelectDialog }

constructor TfrmRackPosArraySelectDialog.Create(aOwner: TComponent; const aLayoutName: string;
    aCheckValues: boolean; aSingleValueAllowed: boolean);
begin
    inherited Create();

    fColIndexRackName := -1;
    fColIndexPosition := -1;
    fColIndexSourceRackName := -1;
    fColIndexSourcePosition := -1;
    fColIndexVolume := -1;
    fColIndexSubstance := -1;
    fColIndexLiqParam := -1;
    fReadOnlyRackPos := false;

    fLayoutName := aLayoutName;
    fCheckValues := aCheckValues;
    fSingleValueAllowed := aSingleValueAllowed;

    fDialogEvents := TLayoutElementSelectDlgEvents.Create;
    fDialogEvents.OnChooseRackFirstPos := self.DoOnChooseRackFirstPos;
    fDialogEvents.OnChooseRackLastPos := self.DoOnChooseRackLastPos;

    fDialogEventsSource := TLayoutElementSelectDlgEvents.Create;
    fDialogEventsSource.OnChooseRackFirstPos := self.DoOnChooseSourceRackFirstPos;
    fDialogEventsSource.OnChooseRackLastPos := self.DoOnChooseSourceRackLastPos;

    fViewLayout := TfrmViewLayout.Create(nil);
    fViewLayout.Parent := self.pnlLayout;
    // fViewLayout.Align := alLeft;
    // fViewLayout.Width := self.Width div 2;
    fViewLayout.Visible := true;

    fViewSourceLayout := TfrmViewLayout.Create(nil);
    fViewSourceLayout.Parent := self.pnlSourceLayout;
    // fViewSourceLayout.Align := alClient;
    fViewSourceLayout.Visible := true;

    fLayoutManager := TLayoutElementSelectDlgLayoutManager.Create(fViewLayout.DrawPanel, GetEvents);
    fViewLayout.SceneGraphics := fLayoutManager.GetSceneGraphicsForLayout(nil);

    fSourceLayoutManager := TLayoutElementSelectDlgLayoutManager.Create(fViewSourceLayout.DrawPanel,
        GetEventsSource);
    fViewSourceLayout.SceneGraphics := fSourceLayoutManager.GetSceneGraphicsForLayout(nil);

    fLayoutManager.RegisterLayout('', fLayoutName);
    fLayoutManager.Load();

    fSourceLayoutManager.RegisterLayout('', fLayoutName);
    fSourceLayoutManager.Load();
    // FitLayoutToScreen();

    self.lblTitle.Caption := TLanguageString.Read('Select rack positions', 'Markiere die Rack-Positionen');
end;

destructor TfrmRackPosArraySelectDialog.Destroy;
begin
    fLayoutManager.UnregisterCurrentLayout();
    fLayoutManager.DestroyDefaultSceneGraphics();
    FreeAndNil(fLayoutManager);
    if Assigned(fSourceLayoutManager) then
    begin
        fSourceLayoutManager.UnregisterCurrentLayout();
        fSourceLayoutManager.DestroyDefaultSceneGraphics();
        FreeAndNil(fSourceLayoutManager);
    end;
    FreeAndNil(fViewLayout);
    FreeAndNil(fViewSourceLayout);
    FreeAndNil(fDialogEvents);
    FreeAndNil(fDialogEventsSource);
    inherited;
end;

procedure TfrmRackPosArraySelectDialog.Prepare(const aData: TArray<TEditableParameterEditData>;
    aOnGetNoOfPos: TBasicEditFunctionsGetNoOfPos; aArrayLengthRefToOrder: integer);
var
    xNames: TArray<string>;
    xDA: TLiqHDataAdaptor;
    xGBCount: integer;
begin
    xGBCount := 3;
    fData := aData;
    fOnGetNoOfPos := aOnGetNoOfPos;
    fArrayLengthRefToOrder := aArrayLengthRefToOrder;

    // Tabelle vorbereiten
    self.AddColumns();
    if not self.NeedsLayout(fData) then
    begin
        self.PanelTop.Visible := false;
        self.Splitter1.Visible := false;
    end;
    if (fColIndexVolume <= -1) and (fColIndexSubstance <= -1) and (fColIndexLiqParam <= -1) then
    begin
        GroupBox2.Visible := false;
        Dec(xGBCount);
    end;

    if (fColIndexSubstance <> -1) then
    begin
        cmbSubstance.Visible := true;
        xNames := TSubstanceDataDataAdaptor.InstReadAllNames;
        TControlUtils.AddValuesToComboBox(xNames, self.cmbSubstance, true);
    end
    else
    begin
        lblSubstance.Visible := false;
        cmbSubstance.Visible := false;
    end;

    if (fColIndexLiqParam <> -1) then
    begin
        xDA := TLiqHDataAdaptor.Create();
        try
            xNames := xDA.ReadAllNames();
        finally
            FreeAndNil(xDA);
        end;
        TControlUtils.AddValuesToComboBox(xNames, self.cmbLiqParam, true);
        cmbLiqParam.Visible := true;
    end
    else
    begin
        cmbLiqParam.Visible := false;
        lblLiqParam.Visible := false;
    end;

    if (fColIndexVolume <= -1) then
    begin
        edVolume.Visible := false;
        lblVolume.Visible := false;
    end;
    edPositionArray.Visible := false;
    edSourcePositionArray.Visible := false;

    if (fColIndexSourceRackName <= -1) and (fColIndexSourcePosition <= -1) then
    begin
        GroupBox1.Visible := false;
        Dec(xGBCount);
        GroupBox3.Left := 6;
        GroupBox2.Left := 215;
        pnlSourceLayout.Visible := false;
        fSourceLayoutManager.UnregisterCurrentLayout();
        fSourceLayoutManager.DestroyDefaultSceneGraphics();
        FreeAndNil(fSourceLayoutManager);
    end
    else
    begin
        pnlSourceLayout.Align := alLeft;
        pnlSourceLayout.Width := self.Width div 2;
        fViewSourceLayout.Align := alLeft;
        fViewSourceLayout.Width := self.Width div 2;
    end;

    btnClear.Left := 6 + xGBCount * 208;
    btnAddPositions.Left := 6 + xGBCount * 208;
    self.rbArrayClick(self);
    // Daten in die Tabelle schreiben
    RefreshPageData;
end;

procedure TfrmRackPosArraySelectDialog.rbArrayClick(Sender: TObject);
begin
    edPositionArray.Left := 60;
    edPositionArray.Width := 133;
    edPositionArray.Visible := true;
    edPositionArray.Top := 49;
    edSourcePositionArray.Left := 60;
    edSourcePositionArray.Width := 133;
    edSourcePositionArray.Visible := true;
    edSourcePositionArray.Top := 49;
end;

procedure TfrmRackPosArraySelectDialog.rbIntegerClick(Sender: TObject);
begin
    edPositionArray.Visible := false;
    edSourcePositionArray.Visible := false;
end;

procedure TfrmRackPosArraySelectDialog.rbMatrixClick(Sender: TObject);
begin
    edPositionArray.Visible := false;
    edSourcePositionArray.Visible := false;
end;

procedure TfrmRackPosArraySelectDialog.RefreshPageData;
var
    x1, x2: integer;
    xValues: TArray<string>;
begin
    // wurde die Array-Länge über die Referenz bestimmt -> Dann Zahl der records read-only
    fFixedNoOfPos := fOnGetNoOfPos(fArrayLengthRefToOrder);

    if (fFixedNoOfPos > 0) then
    begin
        cxGrid1TableView1.DataController.RecordCount := fFixedNoOfPos;
    end
    else
    begin
        // Zeilenanzahl wird durch die Daten bestimmt
        for x1 := 0 to high(fData) do
        begin
            xValues := TBasicEditFunctions.GetStringArrayOfPos(fData[x1].Param.Value, fFixedNoOfPos);
            for x2 := 0 to high(xValues) do
            begin
                // Zeilen hinzufügen wenn nötig
                if (x2 >= cxGrid1TableView1.DataController.RecordCount) then
                    cxGrid1TableView1.DataController.AppendRecord;
            end;
        end;
    end;

    // Daten eintragen
    for x1 := 0 to high(fData) do
    begin
        xValues := TBasicEditFunctions.GetStringArrayOfPos(fData[x1].Param.Value,
            cxGrid1TableView1.DataController.RecordCount);
        for x2 := 0 to cxGrid1TableView1.DataController.RecordCount - 1 do
        begin
            cxGrid1TableView1.DataController.Values[x2, x1] := xValues[x2];
        end;
    end;
end;

procedure TfrmRackPosArraySelectDialog.AddLine(aIndex: integer; const aRackName, aPosition, aSourceRackName,
    aSourcePosition, aVolume, aSubstance, aLiqParam: string);
begin
    cxGrid1TableView1.DataController.Values[aIndex, fColIndexRackName] := aRackName;
    cxGrid1TableView1.DataController.Values[aIndex, fColIndexPosition] := aPosition;
    if fColIndexSourceRackName > 0 then
        cxGrid1TableView1.DataController.Values[aIndex, fColIndexSourceRackName] := aSourceRackName;
    if fColIndexSourcePosition > 0 then
        cxGrid1TableView1.DataController.Values[aIndex, fColIndexSourcePosition] := aSourcePosition;
    if fColIndexVolume > -1 then
        cxGrid1TableView1.DataController.Values[aIndex, fColIndexVolume] := aVolume;
    if fColIndexSubstance > -1 then
        cxGrid1TableView1.DataController.Values[aIndex, fColIndexSubstance] := aSubstance;
    if fColIndexLiqParam > -1 then
        cxGrid1TableView1.DataController.Values[aIndex, fColIndexLiqParam] := aLiqParam;
end;

procedure TfrmRackPosArraySelectDialog.AddColumns();
var
    x1: integer;
    xColumn: TcxGridColumn;
    xItem: TcxEditRepositoryItem;
    xPickList: TArray<string>;
begin
    for x1 := 0 to high(fData) do
    begin

        xColumn := cxGrid1TableView1.CreateColumn;
        xColumn.Caption := fData[x1].Param.ColumnDescription;
        xColumn.Width := fData[x1].Param.ColumnWidth;
        if not fData[x1].Enabled then
        begin
            xColumn.Options.Editing := false;
            xColumn.Options.Focusing := false;
        end;

        // PickList
        xPickList := fData[x1].Param.GetPickList;
        if (Length(xPickList) > 0) then
        begin
            xItem := cxEditRepository1.CreateItem(TcxEditRepositoryComboBoxItem);
            TcxUtils.AddValuesToComboBoxProps(xPickList, (xItem as TcxEditRepositoryComboBoxItem).Properties);
            xColumn.RepositoryItem := xItem;
        end;
    end;
end;

procedure TfrmRackPosArraySelectDialog.FirstSetFocus;
begin
    self.NextButtonSetDefault(true);
    // btnAddPositions.SetFocus;
end;

{ procedure TfrmRackPosArraySelectDialog.FitLayoutToScreen();
  begin
  fViewLayout.btnFitToScreenClick(fViewLayout.btnFitToScreen);
  end; }

procedure TfrmRackPosArraySelectDialog.FormResize(Sender: TObject);
begin
    // FitLayoutToScreen();
end;

procedure TfrmRackPosArraySelectDialog.FormShow(Sender: TObject);
begin
    ChangeSelectionAfterManualChange();
end;

function TfrmRackPosArraySelectDialog.GetColumnData(aIndex: integer): TArray<string>;
var
    x: integer;
begin
    SetLength(result, cxGrid1TableView1.DataController.RecordCount);
    for x := 0 to cxGrid1TableView1.DataController.RecordCount - 1 do
    begin
        if (cxGrid1TableView1.DataController.Values[x, aIndex] = null) then
            result[x] := ''
        else
            result[x] := cxGrid1TableView1.DataController.Values[x, aIndex];
    end;
end;

function TfrmRackPosArraySelectDialog.GetEvents: TLayoutElementSelectDlgEvents;
begin
    EXIT(fDialogEvents);
end;

function TfrmRackPosArraySelectDialog.GetEventsSource: TLayoutElementSelectDlgEvents;
begin
    EXIT(fDialogEventsSource);
end;

function TfrmRackPosArraySelectDialog.NeedsLayout(const aData: TArray<TEditableParameterEditData>): boolean;
var
    x: integer;
begin
    for x := 0 to high(aData) do
    begin
        if (aData[x].Param.DialogType = scdRackName) and (aData[x].Enabled) then
        begin
            if (fColIndexRackName <> -1) then
                raise Exception.Create('2 writable RackName fields');
            fColIndexRackName := x;
        end;
        if (aData[x].Param.DialogType = scdPosition) and (aData[x].Enabled) then
        begin
            if (fColIndexPosition <> -1) then
                raise Exception.Create('2 writable Position fields');
            fColIndexPosition := x;
        end;
        if (aData[x].Param.DialogType = scdSourceRackName) and (aData[x].Enabled) then
        begin
            if (fColIndexSourceRackName <> -1) then
                raise Exception.Create('2 writable RackName fields');
            fColIndexSourceRackName := x;
        end;
        if (aData[x].Param.DialogType = scdSourcePosition) and (aData[x].Enabled) then
        begin
            if (fColIndexSourcePosition <> -1) then
                raise Exception.Create('2 writable Position fields');
            fColIndexSourcePosition := x;
        end;
        if (aData[x].Param.DialogType = scdVolume) and (aData[x].Enabled) then
        begin
            if (fColIndexVolume <> -1) then
                raise Exception.Create('2 writable Volume fields');
            fColIndexVolume := x;
        end;
        if (aData[x].Param.DialogType = scdSubstance) and (aData[x].Enabled) then
        begin
            if (fColIndexSubstance <> -1) then
                raise Exception.Create('2 writable Substance fields');
            fColIndexSubstance := x;
        end;
        if (aData[x].Param.DialogType = scdLiqParam) and (aData[x].Enabled) then
        begin
            if (fColIndexLiqParam <> -1) then
                raise Exception.Create('2 writable LHP fields');
            fColIndexLiqParam := x;
        end;

        if ((aData[x].Param.DialogType = scdRackName) or (aData[x].Param.DialogType = scdPosition)) and
            (not aData[x].Enabled) then
            fReadOnlyRackPos := true;
    end;

    EXIT((fColIndexRackName >= 0) and (fColIndexPosition >= 0));
end;

procedure TfrmRackPosArraySelectDialog.RemoveEmptyColumns;
var
    x, xCol: integer;
begin
    if (fFixedNoOfPos > 0) then
        EXIT;

    // eventuell Zeilen entfernen, die leer sind
    for x := (cxGrid1TableView1.DataController.RecordCount - 1) downto 0 do
    begin
        for xCol := 0 to cxGrid1TableView1.ColumnCount - 1 do
        begin
            if (cxGrid1TableView1.DataController.Values[x, xCol] = null) or
                (cxGrid1TableView1.DataController.Values[x, xCol] = '') then
                CONTINUE;

            EXIT;
        end;
        cxGrid1TableView1.DataController.DeleteRecord(x);
    end;
end;

function TfrmRackPosArraySelectDialog.WritePageData(aCheckBefore: boolean): boolean;
var
    x: integer;
begin
    // eventuell Zeilen entfernen, die leer sind
    RemoveEmptyColumns;

    for x := 0 to high(fData) do
    begin
        if fData[x].Enabled then
            fData[x].Param.Value := TBasicEditFunctions.StringArrayToStringValue(self.GetColumnData(x),
                fSingleValueAllowed);
    end;

    // ResultValue prüfen
    if aCheckBefore and fCheckValues then
    begin
        try
            for x := 0 to high(fData) do
            begin
                if fData[x].Enabled then
                    fData[x].Param.CheckValue;
            end;

        except
            on E: ESetEditableParameterException do
            begin
                TDialogUtils.MessageBox(E.Message, 'Property is not valid', MB_ICONSTOP);
                EXIT(false);
            end
            else
                raise;
        end;
    end;

    EXIT(true);
end;

procedure TfrmRackPosArraySelectDialog.btnAddPositionsClick(Sender: TObject);
var
    xRack: TRack;
    xSourceRack: TRack;
begin
    xSourceRack := nil;
    xRack := fLayoutManager.CurrentLayout.FindRackByName(edRackName.Text);
    if not Assigned(xRack) then
        EXIT;
    if fColIndexSourceRackName <> -1 then
    begin
        xSourceRack := fSourceLayoutManager.CurrentLayout.FindRackByName(edSourceRackName.Text);
        if not Assigned(xSourceRack) then
            EXIT;

        if edNoOfPos.Text <> edSourceNoOfPos.Text then
            if edSourceNoOfPos.Text <> '1' then
            begin
                TDialogUtils.MessageBox('Source and destination positions doesn´t match.',
                    'Wrong number of positions', MB_ICONSTOP);
                EXIT;
            end;
    end;
    AddPositions(xRack, xSourceRack);
    NextButtonSetFocus;
end;

procedure TfrmRackPosArraySelectDialog.AddPositions(aRack, aSourceRack: TRack);
var
    xPositions, xSourcePositions: TArray<integer>;
    xPositionsString: TArray<string>;
    x, xStartIndex, xNextIndex: Integer;
begin
    if not self.rbArray.Checked then
    begin
        xPositions := TMethodGUIParser.GetPositionArray(edRackFirstPos.Text, edRackLastPos.Text,
            aRack.RackStructure.Rows, aRack.RackStructure.Cols, 0);
        if fColIndexSourceRackName <> -1 then
            xSourcePositions := TMethodGUIParser.GetPositionArray(edSourceRackFirstPos.Text,
                edSourceRackLastPos.Text, aSourceRack.RackStructure.Rows, aSourceRack.RackStructure.Cols, 0);
    end
    else
    begin
        xPositionsString := TStringUtilities.StringToStringArray(edPositionArray.Text, ',');
        SetLength(xPositions, Length(xPositionsString));
        for x := 0 to high(xPositionsString) do
        begin
            xPositions[x] := TMethodGUIParser.MatrixCoordCurrentPos(1, xPositionsString[x],
                xPositionsString[x], aRack.Structure.Rows, aRack.Structure.Cols, 0)
        end;
        if fColIndexSourceRackName <> -1 then
        begin
            xPositionsString := TStringUtilities.StringToStringArray(edSourcePositionArray.Text, ',');
            SetLength(xSourcePositions, Length(xPositionsString));
            for x := 0 to high(xPositionsString) do
            begin
                xSourcePositions[x] := TMethodGUIParser.MatrixCoordCurrentPos(1, xPositionsString[x],
                    xPositionsString[x], aSourceRack.Structure.Rows, aSourceRack.Structure.Cols, 0)
            end;
            if Length(xSourcePositions) = 1 then
            begin
                SetLength(xSourcePositions, Length(xPositions));
                for x := 1 to high(xPositions) do
                    xSourcePositions[x] := xSourcePositions[0];
            end;
        end;
    end;

    if (fReadOnlyRackPos) then
        xStartIndex := TMathUtils.MaxIntValue([0, cxGrid1TableView1.DataController.FocusedRecordIndex])
    else
        xStartIndex := cxGrid1TableView1.DataController.RecordCount;

    for x := 0 to high(xPositions) do
    begin
        if (x + xStartIndex >= cxGrid1TableView1.DataController.RecordCount) then
        begin
            if (fFixedNoOfPos > 0) then
                BREAK;

            cxGrid1TableView1.DataController.AppendRecord;
        end;
        if fColIndexSourceRackName <> -1 then
            AddLine(x + xStartIndex, aRack.name, IntToStr(xPositions[x]), aSourceRack.name,
                IntToStr(xSourcePositions[x]), edVolume.Text, cmbSubstance.Text, cmbLiqParam.Text)
        else
            AddLine(x + xStartIndex, aRack.name, IntToStr(xPositions[x]), '', '', edVolume.Text,
                cmbSubstance.Text, cmbLiqParam.Text)
    end;

    if (fReadOnlyRackPos) then
    begin
        xNextIndex := xStartIndex + high(xPositions) + 1;
        if (xNextIndex >= cxGrid1TableView1.DataController.RecordCount) then
        begin
            if (fFixedNoOfPos > 0) then
                EXIT;

            cxGrid1TableView1.DataController.AppendRecord;
        end;
        cxGrid1TableView1.DataController.FocusedRecordIndex := xNextIndex;
    end;
end;

procedure TfrmRackPosArraySelectDialog.btnClearClick(Sender: TObject);
var
    x: Integer;
begin
    if (fReadOnlyRackPos) then
    begin
        // Tabelle leeren
        if (cxGrid1TableView1.DataController.RecordCount >= 1) then
        begin
            for x := 0 to cxGrid1TableView1.DataController.RecordCount - 1 do
                self.AddLine(x, '', '', '', '', '', '', '');

            cxGrid1TableView1.DataController.FocusedRecordIndex := 0;
        end;
    end
    else
    begin
        // Tabelle löschen
        for x := cxGrid1TableView1.DataController.RecordCount - 1 downto 0 do
            cxGrid1TableView1.DataController.DeleteRecord(x);
    end;
end;

procedure TfrmRackPosArraySelectDialog.btnRefreshClick(Sender: TObject);
begin
    self.ChangeSelectionAfterManualChange();
end;

procedure TfrmRackPosArraySelectDialog.ChangeSelectionAfterManualChange();
begin
    { TODO : Rackposiitonen markieren }
end;

function TfrmRackPosArraySelectDialog.DoOnChooseRackFirstPos(const aRackName: string;
    aPosInt, aMaxRows: integer; const aParamIndexOffset: integer; aSetFocus: boolean): string;
var
    xPos: integer;
    xPosArrayText: string;
begin
    edRackName.Text := aRackName;
    if (self.rbInteger.Checked) then
        result := IntToStr(aPosInt)
    else
        result := TMethodGUIParser.GetCoordString(aMaxRows, aPosInt);
    if GetKeyState(VK_CONTROL) < 0 then
        xPosArrayText := edPositionArray.Text
    else
        xPosArrayText := '';
    if self.rbArray.Checked then
        if xPosArrayText = '' then
            xPosArrayText := result
        else
        begin
            xPos := Pos(result, xPosArrayText);
            if xPos > 0 then
            begin
                xPosArrayText := StringReplace(xPosArrayText, result, '', [rfReplaceAll]);
                xPosArrayText := StringReplace(xPosArrayText, ',,', ',', [rfReplaceAll]);
                if Length(xPosArrayText) > 0 then
                begin
                    if xPosArrayText[Length(xPosArrayText)] = ',' then
                        xPosArrayText := Copy(xPosArrayText, 1, Length(xPosArrayText) - 1);
                    if xPosArrayText[1] = ',' then
                        xPosArrayText := Copy(xPosArrayText, 2, Length(xPosArrayText));
                end;
            end
            else
                xPosArrayText := xPosArrayText + ',' + result;
        end;

    edPositionArray.Text := xPosArrayText;
    edRackFirstPos.Text := result;
end;

function TfrmRackPosArraySelectDialog.DoOnChooseRackLastPos(const aRackName: string;
    aPosInt, aMaxRows: integer; const aParamIndexOffset: integer; aSetFocus: boolean): string;
begin
    if self.rbArray.Checked then
        self.edNoOfPos.Text :=
            IntToStr(Length(TStringUtilities.StringToStringArray(self.edPositionArray.Text, ',')))
    else
    begin
        edRackName.Text := aRackName;
        if (self.rbInteger.Checked) then
            result := IntToStr(aPosInt)
        else
            result := TMethodGUIParser.GetCoordString(aMaxRows, aPosInt);
        edRackLastPos.Text := result;

        self.edNoOfPos.Text := IntToStr(TMethodGUIParser.GetLastPos(edRackFirstPos.Text, edRackLastPos.Text));
    end;
end;

function TfrmRackPosArraySelectDialog.DoOnChooseSourceRackFirstPos(const aRackName: string;
    aPosInt, aMaxRows: integer; const aParamIndexOffset: integer; aSetFocus: boolean): string;
var
    xPos: integer;
    xPosArrayText: string;
begin
    edSourceRackName.Text := aRackName;
    if (self.rbInteger.Checked) then
        result := IntToStr(aPosInt)
    else
        result := TMethodGUIParser.GetCoordString(aMaxRows, aPosInt);
    if GetKeyState(VK_CONTROL) < 0 then
        xPosArrayText := edSourcePositionArray.Text
    else
        xPosArrayText := '';
    if self.rbArray.Checked then
        if xPosArrayText = '' then
            xPosArrayText := result
        else
        begin
            xPos := Pos(result, xPosArrayText);
            if xPos > 0 then
            begin
                xPosArrayText := StringReplace(xPosArrayText, result, '', [rfReplaceAll]);
                xPosArrayText := StringReplace(xPosArrayText, ',,', ',', [rfReplaceAll]);
                if Length(xPosArrayText) > 0 then
                begin
                    if xPosArrayText[Length(xPosArrayText)] = ',' then
                        xPosArrayText := Copy(xPosArrayText, 1, Length(xPosArrayText) - 1);
                    if xPosArrayText[1] = ',' then
                        xPosArrayText := Copy(xPosArrayText, 2, Length(xPosArrayText));
                end;
            end
            else
                xPosArrayText := xPosArrayText + ',' + result;
        end;

    edSourcePositionArray.Text := xPosArrayText;
    edSourceRackFirstPos.Text := result;
end;

function TfrmRackPosArraySelectDialog.DoOnChooseSourceRackLastPos(const aRackName: string;
    aPosInt, aMaxRows: integer; const aParamIndexOffset: integer; aSetFocus: boolean): string;
begin
    if self.rbArray.Checked then
        self.edSourceNoOfPos.Text :=
            IntToStr(Length(TStringUtilities.StringToStringArray(self.edSourcePositionArray.Text, ',')))
    else
    begin
        edSourceRackName.Text := aRackName;
        if (self.rbInteger.Checked) then
            result := IntToStr(aPosInt)
        else
            result := TMethodGUIParser.GetCoordString(aMaxRows, aPosInt);
        edSourceRackLastPos.Text := result;
        self.edSourceNoOfPos.Text := IntToStr(TMethodGUIParser.GetLastPos(edSourceRackFirstPos.Text,
            edSourceRackLastPos.Text));
    end;
end;


end.
