{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Editor for SQL statements - based on SQLTermsDataAdaptor
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.04.07 wl                               TN3547   Initial Revision
  17.04.07 wl                               TN3547   User management implementiert
  27.04.07 wl  TablePreview_Update          TN3669   beutzt ConfirmDataChanged
  03.05.07 wl  DefineQueryType              TN3669   die Zuordnung SELECT oder UPDATE/DELETE/INSERT ist jetzt viel besser
  03.05.07 wl  TablePreview_Update          TN3669   Auch im Compliant-Mode erlaubt - Aber SQL-Text wird vorher auf verbotene Tabellen geprüft
  14.04.08 wl                               TN4060   uses DialogUtils
  13.10.08 pk                               TN4272.2 uses changed
  16.01.09 wl                               TN4362   an Änderungen in TViewItem angepasst
  20.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  17.06.10 pk                               TN5152.1 Change SQLs so that they also work for TurboDB (dbl quoatations to single)
  16.08.11 wl                               TN5661   SQLs mit ":AliasName:TableName" syntax werden durch TSQLParser.ResolveAliasesInSQL unterstützt
  10.12.12 wl  TablePreview_Update          TN6050   Affected rows werden angezeigt
  -------------------------------------------------------------------------------------------------- }

unit SQLTermEditor;


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
    StdCtrls,
    ViewItem,
    ViewItemEditForm,
    ExtCtrls,
    Buttons,
    Grids,
    DataProvider;

type
    TfrmSQLTermEditor = class(TViewItemEditForm)
        Panel1: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        Splitter1: TSplitter;
        Label1: TLabel;
        sgPreviewRecords: TStringGrid;
        Memo2: TMemo;
        Splitter2: TSplitter;
        Panel5: TPanel;
        Panel2: TPanel;
        sbExecuteQuery: TSpeedButton;
        rgQueryType: TRadioGroup;
        StringGrid1: TStringGrid;
        Memo1: TMemo;
        procedure sbExecuteQueryClick(Sender: TObject);
        procedure Memo1Change(Sender: TObject);
        procedure Memo2Change(Sender: TObject);
        procedure FormActivate(Sender: TObject);
    private
        procedure TablePreview_Select();
        procedure TablePreview_Update();
        procedure ResetPreview();
        procedure DefineQueryType();
        procedure WriteDataSetToGrid(const aDataSet: TDataProvider; aGrid: TStringGrid);
    protected
        procedure SaveData(); override;
        procedure ResetData(); override;
        procedure UnloadData(); override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
        //
        procedure FirstLoad(); override;
    end;


implementation


{$R *.dfm}

uses
    MathUtils,
    LogManager,
    SQLTermsDataAdaptor,
    AppSettings,
    ImportDataAdaptor,
    SQLParser,
    ImportClasses,
    SQLTermParserInterface,
    CommonTypes,
    DialogUtils,
    SpecialViewItems,
    ControlUtils,
    DataProviderFactory;

{ TfrmSQLTermEditor }

constructor TfrmSQLTermEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged);

    TControlUtils.ResetFontForWinXP(self);
    // Update darf nur der System-Administrator (mit Begründung)
    if not gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
    begin
        Memo1.ReadOnly := true;
        Memo2.ReadOnly := true;
    end;

end;

procedure TfrmSQLTermEditor.FirstLoad;
var
    xDA: TSQLTermsDataAdaptor;
    xRec: TSQLTermRec;
    xValues: TArray<string>;
begin
    inherited;
    self.Caption := self.GetCaption;

    xDA := TSQLTermsDataAdaptor.Create();
    try
        xDA.ReadSQLTermData(self.GetDataName, xRec);
    finally
        xDA.Free;
    end;

    // SQL Term
    xValues := TSQLTermUtils.GetStringsFromString(xRec.Term);
    TControlUtils.AddValuesToMemo(xValues, Memo1, true);

    xValues := TSQLTermUtils.GetStringsFromString(xRec.Comment);
    TControlUtils.AddValuesToMemo(xValues, Memo2, true);

    self.DefineQueryType;
    self.Reset;
end;

procedure TfrmSQLTermEditor.ResetData;
begin
    inherited;

end;

procedure TfrmSQLTermEditor.WriteDataSetToGrid(const aDataSet: TDataProvider; aGrid: TStringGrid);
const
    INT_NUMFIXED_ROWS = 1;
var
    xDatasetTotalRows, xDatasetTotalCols: integer;
    xRowIndex: integer;

    procedure WriteDatasetFieldNamesToGrid();
    var
        xDatasetCol: integer;
    begin
        for xDatasetCol := 0 to xDatasetTotalCols - 1 do
        begin
            aGrid.Cells[xDatasetCol, xRowIndex] := aDataset.Fields[xDatasetCol].FieldName;
        end;
    end;
    procedure WriteCurrentDatasetRowToGrid();
    var
        xDatasetCol: integer;
    begin
        for xDatasetCol := 0 to xDatasetTotalCols - 1 do
        begin
            aGrid.Cells[xDatasetCol, xRowIndex] := aDataset.Fields[xDatasetCol].AsString;
        end;
    end;

begin
    // Determine the rowcount and expand grid's rowcount to the needed number
    xDatasetTotalRows := aDataSet.RecordCount;
    aGrid.RowCount := INT_NUMFIXED_ROWS + xDatasetTotalRows;
    // add 1 because for TStringGrid RowCount must be greater than FixedRows
    aGrid.FixedRows := TMathUtils.MinIntValue([INT_NUMFIXED_ROWS, aDataSet.RecordCount]);

    // Determine and define the column count
    xDatasetTotalCols := aDataSet.FieldCount;
    aGrid.ColCount := xDatasetTotalCols;

    xRowIndex := 0;
    WriteDatasetFieldNamesToGrid();

    xRowIndex := INT_NUMFIXED_ROWS;
    // aDataset.MoveBy(aStartAtRow);
    while not aDataset.Eof do
    begin
        WriteCurrentDatasetRowToGrid();
        aDataset.Next;
        Inc(xRowIndex);
    end;
end;

procedure TfrmSQLTermEditor.TablePreview_Select();
// Es handelt sich um ein SELECT-Statement
var
    xQuery: TDataProvider;
    xSQLLines: TStringList;
begin
    self.ResetPreview();

    // Select darf der User
    if not gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystem) then
        EXIT;

    xSQLLines := TStringList.Create();
    try
        xSQLLines.AddStrings(Memo1.Lines);

        gmSQLTermPrepareBeforeExecute(self.DataName, xSQLLines, '');

        xQuery := TDataProviderFactory.Instance.CreateDataProvider();
        try
            xQuery.SelectAndOpen(TSQLParser.ResolveAliasesInSQL(xSQLLines.Text), true);

            try
                self.WriteDataSetToGrid(xQuery, self.sgPreviewRecords);
                TControlUtils.OptimizeGridColumnSizes(sgPreviewRecords, 0);
                self.sgPreviewRecords.Visible := true;
            finally
                xQuery.Close();
            end;
        finally
            FreeAndNil(xQuery);
        end;

    finally
        FreeAndNil(xSQLLines);
    end;
end;

procedure TfrmSQLTermEditor.TablePreview_Update();
// Es handelt sich um ein UPDATE/INSERT/DELETE-Statement
var
    xQuery: TDataProvider;
    xSQLLines: TStringList;
    xPureSQLText: string;
    xAffectedRows: integer;
begin
    self.ResetPreview();

    // Update darf nur der System-Administrator
    if not gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
        EXIT;

    // Text: ZINSSER_APP darf nicht vorkommen
    if not gCommonDll.CurrentUser.CheckSQLBeforeUpdate(Memo1.Lines.Text) then
    begin
        ShowMessage('You are trying to manipulate critical data sections! The action is blocked!');
        EXIT;
    end;

    if not gCommonDll.CurrentUser.ConfirmDataChanged('SQL Execute',
        self.DataName + ': ' + TSQLTermUtils.GetPureSQLText(Memo1.Lines.Text), lctBulk) then
        EXIT;

    xSQLLines := TStringList.Create();
    try
        xSQLLines.AddStrings(Memo1.Lines);

        gmSQLTermPrepareBeforeExecute(self.DataName, xSQLLines, '');

        xQuery := TDataProviderFactory.Instance.CreateDataProvider();
        try
            xPureSQLText := TSQLParser.ResolveAliasesInSQL(TSQLTermUtils.GetPureSQLText(xSQLLines.Text));

            xAffectedRows := xQuery.ExecSQL(xPureSQLText);
            gLogManager.Log(xPureSQLText, false);

            Application.MessageBox(PChar('Query was executed successfully' + #13 + 'Data rows affected: ' +
                IntToStr(xAffectedRows)), 'SQL Editor', 64);
        finally
            FreeAndNil(xQuery);
        end;
    finally
        FreeAndNil(xSQLLines);
    end;
end;

procedure TfrmSQLTermEditor.ResetPreview;
begin
    self.sgPreviewRecords.Visible := false;
end;

procedure TfrmSQLTermEditor.SaveData;
var
    xDA: TSQLTermsDataAdaptor;
    xRec: TSQLTermRec;
    xValues: TArray<string>;
begin
    inherited;

    xRec.Name := self.DataName;

    xValues := TControlUtils.GetValuesFromMemo(Memo1);
    xRec.Term := TSQLTermUtils.GetStringFromStrings(xValues);

    xValues := TControlUtils.GetValuesFromMemo(Memo2);
    xRec.Comment := TSQLTermUtils.GetStringFromStrings(xValues);

    xDA := TSQLTermsDataAdaptor.Create();
    try
        xDA.WriteSQLTermRec(xRec);
    finally
        xDA.Free;
    end;
end;

procedure TfrmSQLTermEditor.UnloadData;
begin
    inherited;

end;

function TfrmSQLTermEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TSQLTermViewItem.Create(aItemName);
end;

procedure TfrmSQLTermEditor.DefineQueryType;
var
    xQueryType: TSQLTextType;
    xValues: TArray<string>;
begin
    xValues := TControlUtils.GetValuesFromMemo(Memo1);
    xQueryType := TSQLTermUtils.DefineQueryType(xValues);

    case xQueryType of
        sttNoQuery:
            begin
                rgQueryType.ItemIndex := -1;
                sbExecuteQuery.Enabled := false;
            end;
        sttSelectQuery:
            begin
                rgQueryType.ItemIndex := 0;
                sbExecuteQuery.Enabled := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystem);
            end;
        else
            begin
                rgQueryType.ItemIndex := 1;
                sbExecuteQuery.Enabled := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level,
                    usrSystemAdmin);
            end;
    end;
end;

procedure TfrmSQLTermEditor.sbExecuteQueryClick(Sender: TObject);
begin
    if (rgQueryType.ItemIndex = 0) then
        self.TablePreview_Select()
    else
        self.TablePreview_Update();
end;

procedure TfrmSQLTermEditor.Memo1Change(Sender: TObject);
begin
    self.ChangeData();
    DefineQueryType;
end;

procedure TfrmSQLTermEditor.Memo2Change(Sender: TObject);
begin
    self.ChangeData();
end;

procedure TfrmSQLTermEditor.FormActivate(Sender: TObject);
begin
    DefineQueryType;
end;


end.
