{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Editor for reagent racks
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------- ----------------------------------------------
  04.03.06 wl                               TN2954    komplett neuer Editor: ersetzt alten TubeEditor
  04.03.06 wl                               TN2955    Current Volume kann durch manuelle Zugeben geändert werden
  19.12.06 wl                               TN3409   class von TDockableEditForm in TViewItemEditForm geändert
  12.01.07 thr                              TN3502    SaveData: Konvertierungsfehler wenn Variant=NULL
  07.08.07 wl                               TN3811.3  TPosinfoDataAdaptor.Create statt Instance()
  09.01.08 wl                               TN3972    uses MethodTypes
  20.06.08 pk DisplayRack                   TN4139    Deactivated for now
  02.07.08 pk                               TN4139    PaintTubePos called with pos=1 instead of row=0 and col=0
  16.01.09 wl                                TN4362   an Änderungen in TViewItem angepasst
  13.07.09 pk                               TN4585.4  Find functions replaced by selectandopen
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  20.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  14.12.09 pk                               TN4939    DisplayRack reimplemented
  14.12.09 pk                               TN4939    use DataProvider instead of TQuery
  13.04.10 wl                                TN5044   uses geändert
  06.05.10 wl                               TN5052    uses TViewItemsWorkflow.OverviewManager
  20.05.10 wl                               TN5117    neue Schriftart "Segoe UI", Schriftgröße 9
  17.06.10 pk                               TN5152.1  Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  26.07.11 wl                               TN5614   Zugriff auf ViewReagentsOfLayout geändert
  20.09.11 wl                               TN5723   neue Spalte: Color (Editor ist noch nicht schön)
  28.10.11 wl                               TN5729   Substances-Einträge (Step = 0) --> SubstanceSetDataAdaptor
  03.11.11 wl                               TN5725   Color wieder entfernt
  17.11.11 wl                               TN5725   weiter demontiert
  09.12.11 wl                               TN5761   weiter demontiert
  03.02.12 wl                               TN5792   läuft wieder
  20.02.12 wl                               TN5812   Viele Funktionen zu Ende programmiert
  22.06.12 wl                               TN5924   Drag&Drop von Substanzen funktioniert jetzt richtig
  10.09.12 wl                               TN5979   neue Spalte für MinVolume2
  13.03.13 wl                               TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  21.03.13 pp                               TN6112   Spaltenreihenfolge der DBT Tbl SUBSTANCESET angepasst
  27.03.13 wl                               TN6095   verwendet EditFunctions
  -------------------------------------------------------------------------------------------------- }

unit TubeEditor;


interface


uses
    Forms,
    Controls,
    Classes,
    Dialogs,
    Menus,
    ExtCtrls,
    cxStyles,
    cxCustomData,
    cxGraphics,
    cxFilter,
    cxData,
    cxDataStorage,
    cxEdit,
    cxGridLevel,
    cxGridCustomTableView,
    cxGridTableView,
    cxClasses,
    cxControls,
    cxGridCustomView,
    cxGrid,
    cxExtEditRepositoryItems,
    cxEditRepositoryItems,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    cxButtonEdit,

    ViewItem,
    ViewItemEditForm;

type
    TfrmTubeEditor = class(TViewItemEditForm)
        Panel2: TPanel;
        Splitter1: TSplitter;
        cxEditRepository1: TcxEditRepository;
        Panel1: TPanel;
        cxGrid1: TcxGrid;
        cxGrid1TableView1: TcxGridTableView;
        cxGrid1TableView1ColumnPos: TcxGridColumn;
        cxGrid1TableView1Column1: TcxGridColumn;
        cxGrid1TableView1Column2: TcxGridColumn;
        cxGrid1Level1: TcxGridLevel;
        PopupMenu1: TPopupMenu;
        pmnuDelete: TMenuItem;
        cxEditRepository1SpinItem1: TcxEditRepositorySpinItem;
        ColorDialog1: TColorDialog;
        cxGrid1TableView1Column7: TcxGridColumn;
        AddRow1: TMenuItem;
        cxGrid1TableView1Column3: TcxGridColumn;
        cxGrid1TableView1Column4: TcxGridColumn;
        Appendlinesbyselectingpositions1: TMenuItem;
        N1: TMenuItem;
        cxEditRepository1ComboBoxItem1: TcxEditRepositoryComboBoxItem;
        cxGrid1TableView1Column5: TcxGridColumn;
        procedure Start_AmountGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
        procedure cxGrid1TableView1DragDrop(aSender, aSource: TObject; aX, aY: Integer);
        procedure cxGrid1TableView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
            var Accept: Boolean);
        procedure cxGrid1TableView1EditValueChanged(Sender: TcxCustomGridTableView;
            AItem: TcxCustomGridTableItem);
        procedure pmnuDeleteClick(Sender: TObject);
        procedure PopupMenu1Popup(Sender: TObject);
        procedure cxGrid1TableView1Column6PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
        procedure AddRow1Click(Sender: TObject);
        procedure Appendlinesbyselectingpositions1Click(Sender: TObject);
    private
        function AddLine(const aRackID: string; aPos: integer): integer;
        function AddLineWithNextPos(): integer;
        procedure GetDataArrayFromSubstanceSet();
        function GetInsertRecIndex: integer;
        procedure EdButtonClick(aSender: TObject);
        procedure AddReagentRepositoryItem();
    protected
        procedure SaveData(); override;
        procedure UnloadData(); override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
        class function GetCaption(const aRackID: string): string;
    end;


implementation


{$R *.DFM}

uses
    SysUtils,
    Variants,
    cxTL,
    Rack,
    SubstanceDataDataAdaptor,
    MethodGUIParsing,
    Generics.Collections,
    Utility_DevExpress,
    DesignerMain,
    MethodTypes,
    GeneralTypes,
    ViewItemsWorkflow,
    SpecialViewItems,
    SubstanceSetDataAdaptor,
    ZADesignLayoutManager,
    ControlUtils,
    DataProviderFactory;

{ TfrmTubeEditor }

constructor TfrmTubeEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged);

    TControlUtils.ResetFontForWinXP(self);

    self.Caption := GetCaption(fViewItem.Name);

    self.cxGrid1TableView1.Columns[0].Caption := TLanguageString.Read('Rack name', 'Rackname');
    self.cxGrid1TableView1.Columns[1].Caption := TLanguageString.Read('Pos.', 'Pos.');
    self.cxGrid1TableView1.Columns[2].Caption := TLanguageString.Read('Substance ID', 'Substanz-ID');
    self.cxGrid1TableView1.Columns[3].Caption := TLanguageString.Read('Volume [µL]', 'Volumen [µL]');
    self.cxGrid1TableView1.Columns[4].Caption := TLanguageString.Read('Min. vol. 1 [µL]', 'Min.-vol. 1 [µL]');
    self.cxGrid1TableView1.Columns[5].Caption := TLanguageString.
        Read('Max. volume [µL]', 'Max.-volumen [µL]');
    self.cxGrid1TableView1.Columns[6].Caption := TLanguageString.Read('Min. vol. 2 [µL]', 'Min.-vol. 2 [µL]');

    GetDataArrayFromSubstanceSet();
    AddReagentRepositoryItem();
end;

function TfrmTubeEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TSubstanceSetViewItem.Create(aItemName);
end;

function TfrmTubeEditor.AddLineWithNextPos(): integer;
var
    xPos: integer;
    xRackID: string;
begin
    xRackID := '';
    xPos := 0;

    if (cxGrid1TableView1.DataController.RecordCount > 0) then
    begin
        xRackID := cxGrid1TableView1.DataController.Values
            [cxGrid1TableView1.DataController.RecordCount - 1, 0];
        if not TryStrToInt(cxGrid1TableView1.DataController.Values
            [cxGrid1TableView1.DataController.RecordCount - 1, 1], xPos) then
            xPos := 0;
    end;

    result := self.AddLine(xRackID, xPos + 1);

    cxGrid1TableView1.DataController.FocusedRecordIndex := result;
end;

procedure TfrmTubeEditor.AddReagentRepositoryItem();
var
    xItem: TcxEditRepositoryItem;
begin
    xItem := cxEditRepository1.CreateItem(TcxEditRepositoryComboButtonItem);
    TcxUtils.AddValuesToComboBoxProps(TSubstanceDataDataAdaptor.InstReadAllNames,
        (xItem as TcxEditRepositoryComboButtonItem).Properties);
    (xItem as TcxEditRepositoryComboButtonItem).Properties.OnButtonClick := EdButtonClick;
    cxGrid1TableView1Column1.RepositoryItem := xItem;
end;

procedure TfrmTubeEditor.AddRow1Click(Sender: TObject);
begin
    AddLineWithNextPos();
end;

procedure TfrmTubeEditor.Appendlinesbyselectingpositions1Click(Sender: TObject);
var
    xRackName: string;
    xFirstPos, xLastPos: string;
    xRack: TRack;
    xPositionArray: TArray<integer>;
    x: integer;
begin
    xRackName := '';
    xFirstPos := '';
    xLastPos := '';

    if TZADesignLayoutManager.Instance.IsCurrentLayoutEmpty then
        EXIT;
    if not frmDesignerMain.EditFunctions.EditRackDoublePos(xRackName, xFirstPos, xLastPos,
        'Add reagent positions') then
        EXIT;

    xRack := TZADesignLayoutManager.Instance.CurrentLayout.FindRackByName(xRackName);
    if not Assigned(xRack) then
        EXIT;
    if (xRack.RackID = '') then
    begin
        ShowMessage('This rack has no Rack-ID');
        EXIT;
    end;

    xPositionArray := TMethodGUIParser.GetPositionArray(xFirstPos, xLastPos, xRack.RackStructure.Rows,
        xRack.RackStructure.Cols, 0);
    for x := 0 to high(xPositionArray) do
    begin
        AddLine(xRack.RackID, xPositionArray[x]);
    end;
    self.ChangeData();
end;

function TfrmTubeEditor.AddLine(const aRackID: string; aPos: integer): integer;
begin
    result := cxGrid1TableView1.DataController.AppendRecord;
    cxGrid1TableView1.DataController.Values[result, 0] := aRackID;
    if aPos > 0 then
        cxGrid1TableView1.DataController.Values[result, 1] := aPos;

    // Min und max volume von der vorhergehenden Zeile übernehmen
    if result > 0 then
    begin
        cxGrid1TableView1.DataController.Values[result, 4] := cxGrid1TableView1.DataController.Values
            [result - 1, 4];
        cxGrid1TableView1.DataController.Values[result, 5] := cxGrid1TableView1.DataController.Values
            [result - 1, 5];
        cxGrid1TableView1.DataController.Values[result, 6] := cxGrid1TableView1.DataController.Values
            [result - 1, 6];
    end;

    self.ChangeData();
end;

procedure TfrmTubeEditor.GetDataArrayFromSubstanceSet();
var
    xRecs: TArray<TSubstanceSetRec>;
    x, xLine: integer;
    xDA: TSubstanceSetDataAdaptor;
begin
    xDA := TSubstanceSetDataAdaptor.Create();
    try
        xRecs := xDA.ReadSubstanceSetRecsBySetName(fViewItem.Name);
        for x := 0 to high(xRecs) do
        begin
            xLine := cxGrid1TableView1.DataController.AppendRecord;
            cxGrid1TableView1.DataController.Values[xLine, 0] := xRecs[x].RackID;
            cxGrid1TableView1.DataController.Values[xLine, 1] := xRecs[x].Pos;
            cxGrid1TableView1.DataController.Values[xLine, 2] := xRecs[x].SubstID;
            cxGrid1TableView1.DataController.Values[xLine, 3] := xRecs[x].Amount;
            cxGrid1TableView1.DataController.Values[xLine, 4] := xRecs[X].MinVolume1;
            cxGrid1TableView1.DataController.Values[xLine, 5] := xRecs[X].MaxVolume;
            cxGrid1TableView1.DataController.Values[xLine, 6] := xRecs[X].MinVolume2;
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TfrmTubeEditor.SaveData;
var
    x: integer;
    xRec: TSubstanceSetRec;
    xRecList: TList<TSubstanceSetRec>;
    xDataAdaptor: TSubstanceSetDataAdaptor;
begin
    xRecList := TList<TSubstanceSetRec>.Create;
    try
        // Daten lesen
        for x := 0 to cxGrid1TableView1.DataController.RecordCount - 1 do
        begin
            if (cxGrid1TableView1.DataController.Values[x, 0] = NULL) or
                (cxGrid1TableView1.DataController.Values[x, 1] = NULL) then
                CONTINUE;
            if cxGrid1TableView1.DataController.Values[x, 0] = '' then
                CONTINUE;
            xRec.RackID := cxGrid1TableView1.DataController.Values[x, 0];
            xRec.Pos := StrToIntDef(cxGrid1TableView1.DataController.Values[x, 1], 0);
            if xRec.Pos < 1 then
                CONTINUE;

            xRec.Valid := true;
            xRec.SetName := self.DataName;
            if (cxGrid1TableView1.DataController.Values[x, 2] <> NULL) then
                xRec.SubstID := cxGrid1TableView1.DataController.Values[x, 2]
            else
                xRec.SubstID := '';
            if (cxGrid1TableView1.DataController.Values[x, 3] <> NULL) then
                xRec.Amount := cxGrid1TableView1.DataController.Values[x, 3]
            else
                xRec.Amount := 0;
            if (cxGrid1TableView1.DataController.Values[x, 4] <> NULL) then
                xRec.MaxVolume := cxGrid1TableView1.DataController.Values[x, 4]
            else
                xRec.MaxVolume := 0;
            if (cxGrid1TableView1.DataController.Values[x, 5] <> NULL) then
                xRec.MinVolume1 := cxGrid1TableView1.DataController.Values[x, 5]
            else
                xRec.MinVolume1 := 0;
            if (cxGrid1TableView1.DataController.Values[x, 6] <> NULL) then
                xRec.MinVolume2 := cxGrid1TableView1.DataController.Values[x, 6]
            else
                xRec.MinVolume2 := 0;

            xRecList.Add(xRec);
        end;

        // in Tablle schreiben
        xDataAdaptor := TSubstanceSetDataAdaptor.Create();
        try
            xDataAdaptor.DeleteName(self.DataName);
            for x := 0 to xRecList.Count - 1 do
            begin
                xDataAdaptor.EditOrAppendRec(xRecList[x]);
            end;
        finally
            xDataAdaptor.Free;
        end;
    finally
        FreeAndNil(xRecList);
    end;
end;

class function TfrmTubeEditor.GetCaption(const aRackID: string): string;
begin
    result := TLanguageString.Read('Substance set: {0}', 'Substanz-Set: {0}', [aRackId]);
end;

procedure TfrmTubeEditor.Start_AmountGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
begin
    self.ChangeData();
end;

procedure TfrmTubeEditor.UnloadData;
begin
    TViewItemsWorkflow.Instance.OverviewFormsUpdateItems([ntSubstanceSet]);
end;

var
    uGridRecord: TcxCustomGridRecord;

function TfrmTubeEditor.GetInsertRecIndex(): integer;
begin
    if Assigned(uGridRecord) then
        result := uGridRecord.Index
    else
        result := cxGrid1TableView1.DataController.RecordCount;
end;

procedure TfrmTubeEditor.cxGrid1TableView1Column6PropertiesButtonClick(Sender: TObject;
    AButtonIndex: Integer);
var
    xRow: integer;
begin
    xRow := cxGrid1TableView1.Controller.FocusedRow.Index;
    ColorDialog1.Color := StrToIntDef(cxGrid1TableView1.DataController.Values[xRow, 6], 0);
    if (ColorDialog1.Execute) then
    begin
        cxGrid1TableView1.DataController.Values[xRow, 6] := ColorDialog1.Color;
        self.ChangeData();
    end;
end;

procedure TfrmTubeEditor.cxGrid1TableView1DragDrop(aSender, aSource: TObject; aX, aY: Integer);
var
    xHitTest: TcxCustomGridHitTest;
    xRecIndex: integer;
begin
    if not(aSender is TcxGridSite) then
        EXIT;

    xHitTest := (aSender as TcxGridSite).ViewInfo.GetHitTest(aX, aY);
    if (xHitTest is TcxGridRecordHitTest) then
        uGridRecord := (xHitTest as TcxGridRecordHitTest).GridRecord
    else
        uGridRecord := nil;
    xRecIndex := GetInsertRecIndex();

    if (aSource is TcxTreeList) // Overview oder Favourites
        and Assigned((aSource as TcxTreeList).DragNode) then
    begin

        if (aSource as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_REAGENT then
        begin
            if (xRecIndex < 0) or (xRecIndex >= cxGrid1TableView1.DataController.RecordCount) then
                xRecIndex := AddLineWithNextPos();

            cxGrid1TableView1.DataController.Values[xRecIndex, 2] := (aSource as TcxTreeList)
                .DragNode.Texts[0];
            cxGrid1TableView1.Controller.FocusedRecordIndex := xRecIndex;
            cxGrid1TableView1.Controller.FocusedItemIndex := 2;
            self.ChangeData();
        end;
    end;
end;

procedure TfrmTubeEditor.cxGrid1TableView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
    if (Source is TcxTreeList) and Assigned((Source as TcxTreeList).DragNode) then
    begin
        Accept := ((Source as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_REAGENT);
    end;
end;

procedure TfrmTubeEditor.EdButtonClick(aSender: TObject);
begin
    if VarIsStr(cxGrid1TableView1.Controller.FocusedRecord.Values[2]) then
        TViewItemsWorkflow.Instance.OpenEditForm(cxGrid1TableView1.Controller.FocusedRecord.Values[2],
            ntSubstance);
end;

procedure TfrmTubeEditor.cxGrid1TableView1EditValueChanged(Sender: TcxCustomGridTableView;
    AItem: TcxCustomGridTableItem);
begin
    cxGrid1TableView1.DataController.Post;

    self.ChangeData();
end;

procedure TfrmTubeEditor.pmnuDeleteClick(Sender: TObject);
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

            cxGrid1TableView1.DataController.DeleteRecord(xRecIndex);
            cxGrid1TableView1.DataController.UpdateData;
        end;
        cxGrid1TableView1.Controller.ClearSelection();
    finally
        ChangeData();
    end;
end;

procedure TfrmTubeEditor.PopupMenu1Popup(Sender: TObject);
begin
    pmnuDelete.Visible := (cxGrid1TableView1.Controller.SelectedRecordCount > 0);
end;


end.
