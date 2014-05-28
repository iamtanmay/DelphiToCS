{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.08.10 pk                                        TN5218     Initial revision
  24.09.10 pk  DBGrid1DblClick                       TN5280     It is now possible to edit memo fields
  08.03.11 wl  DBGrid1TitleClick                     TN5497     It is possible to sort records
  04.08.11 wl                                        TN5647     optisch überarbeitet
  08.04.14 ts                                        TN6393     new to edit tables from ZARunner (copy and changed from ZADBAdministrator)
  08.04.14 ts                                        TN6393     Caption is tablename
  08.04.14 ts                                        TN6393     Drag corrected
  ----------------------------------------------------------------------------------------------------------------------- }

unit TableEditor;


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
    DBCtrls,
    ExtCtrls,
    DB,
    Grids,
    DBGrids,
    DataProvider,
    DatabaseConfig;

type
    TfrmTableEditor = class(TForm)
        DBGrid1: TDBGrid;
        dsData: TDataSource;
        pnlTop: TPanel;
        DBNavigator1: TDBNavigator;
        procedure DBGrid1DblClick(Sender: TObject);
        procedure DBGrid1TitleClick(Column: TColumn);

    private
        fDataProvider: TDataProvider;
        fDatabaseConfig: TDatabaseConfig;
        fTableName: string;
        fOrderBy: string;
    public
        constructor Create(const aDatabaseConfig: TDatabaseConfig); reintroduce;
        procedure LoadTableToDataGrid(const aTableName: string; aOrderBy: string = '');
        procedure ResetDataGrid();
        procedure CloseDataProvider();
        procedure OpenDatabase();
        procedure CloseDatabase();
    end;


implementation


uses
    DatasetDataProvider,
    DataProviderFactory,
    TableMemoEditor;

{$R *.dfm}

constructor TfrmTableEditor.Create(const aDatabaseConfig: TDatabaseConfig);
begin
    inherited Create(nil);
    fDataProvider := nil;
    fDatabaseConfig := aDatabaseConfig;
end;

procedure TfrmTableEditor.DBGrid1DblClick(Sender: TObject);
var
    xMemoText: string;
begin
    if not Assigned(self.DBGrid1.SelectedField) then
        EXIT;

    if self.DBGrid1.SelectedField.DataType = ftMemo then
    begin
        xMemoText := self.DBGrid1.SelectedField.Value;
        if TfrmTableMemoEditor.ShowDialog(self.DBGrid1.SelectedField.FieldName, xMemoText) <> mrOK then
            EXIT;
        self.DBGrid1.DataSource.DataSet.Edit;
        self.DBGrid1.SelectedField.Value := xMemoText;
        self.DBGrid1.DataSource.DataSet.Post;
    end;

end;

procedure TfrmTableEditor.DBGrid1TitleClick(Column: TColumn);
var
    xNewOrderBy: string;
begin
    if fTableName = '' then
        EXIT;
    xNewOrderBy := Column.Title.Caption;
    if (fOrderBy = xNewOrderBy) then
        xNewOrderBy := xNewOrderBy + ' DESC';

    LoadTableToDataGrid(fTableName, xNewOrderBy);
end;

procedure TfrmTableEditor.CloseDataProvider();
begin
    if not Assigned(fDataProvider) then
        EXIT;
    fDataProvider.Close();
end;

procedure TfrmTableEditor.LoadTableToDataGrid(const aTableName: string; aOrderBy: string);
begin
    if not Assigned(fDataProvider) then
        OpenDatabase();

    fDataProvider.Close();
    fTableName := aTableName;
    fOrderBy := aOrderBy;
    if fTableName = '' then
        EXIT;
    self.Caption := fTableName;
    fDataProvider.SelectAndOpenAll(aTableName, false, aOrderBy);
    self.dsData.DataSet := (fDataProvider as TDatasetDataProvider).GetRealDataset;
end;

procedure TfrmTableEditor.ResetDataGrid();
begin
    CloseDataProvider();
    self.dsData.DataSet := nil;
end;

procedure TfrmTableEditor.OpenDatabase();
begin
    CloseDatabase();
    fDataProvider := TDataProviderFactory.Instance.CreateDataProvider(fDatabaseConfig.Alias);
end;

procedure TfrmTableEditor.CloseDatabase;
begin
    ResetDataGrid();
    FreeAndNil(fDataProvider);
end;


end.
