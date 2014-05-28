unit ImportDataTypes;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  : A GUI used to edit Datatypes of an Import Definition
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  27.01.09 ts                               TN4346   Initial Revision
  20.03.09 wl                               TN4476   An geändertes TDataTypesRecArray angepasst
  24.08.09 wl  fStringLoader                TN4702   fStringLoader lädt Strings für Dialog-Elemente
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    Forms,
    StdCtrls,
    ExtCtrls,
    Controls,
    Classes,
    cxStyles,
    cxCustomData,
    cxGraphics,
    cxFilter,
    cxData,
    cxDataStorage,
    cxEdit,
    cxEditRepositoryItems,
    cxExtEditRepositoryItems,
    cxGridLevel,
    cxGridCustomTableView,
    cxGridTableView,
    cxClasses,
    cxControls,
    cxGridCustomView,
    db,
    cxGrid,
    ImportDataTypesDataAdaptor,
    ImportFileDefDataAdaptor,
    StringLoader,
    cxLookAndFeels,
    cxLookAndFeelPainters;

type
    TImportDataTypesStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TFrmImportDataTypes = class(TForm)
        pnlTop: TPanel;
        edImportDefName: TEdit;
        lblImportDefName: TLabel;
        cxGrid1: TcxGrid;
        cxGrid1TableView1: TcxGridTableView;
        cxGrid1TableView1Column1: TcxGridColumn;
        cxGrid1TableView1Column2: TcxGridColumn;
        cxGrid1Level1: TcxGridLevel;
        cxEditRepository1: TcxEditRepository;
        cxEditRepository1ComboBoxItem1: TcxEditRepositoryComboBoxItem;
        pnlBottom: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        procedure btnCancelClick(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        fDataTypesRecArray: TDataTypesRecArray;
        fStringLoader: TImportDataTypesStringLoader;
        function GridToRec(): TDataTypesRecArray;
        procedure PrepareDataset(const aImportDefName: string; aDataTypesRecArray: TDataTypesRecArray);
    public
        fFileDef: TFileDefRec;
        class function ModalOpen(const aImportDefName: string; aDataTypesRecArray: TDataTypesRecArray)
            : TModalResult;
    end;


implementation


{$R *.dfm}

uses
    GeneralTypes,
    ControlUtils;

{ TImportDataTypesStringLoader }

procedure TImportDataTypesStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
    AddSingle(9200, 'Import File Definition', 'Importdatei-Definition');
    AddSingle(9360, 'Field Name', 'Feldname');
    AddSingle(9810, 'Data type', 'Datentyp');
    AddSingle(9815, 'Change Datatypes for Import', 'Datentypen ändern für Import');
end;

class function TFrmImportDataTypes.ModalOpen(const aImportDefName: string;
    aDataTypesRecArray: TDataTypesRecArray): TModalResult;
var
    xForm: TFrmImportDataTypes;
begin
    xForm := TFrmImportDataTypes.Create(nil);
    try
        xForm.PrepareDataset(aImportDefName, aDataTypesRecArray);
        result := xForm.ShowModal;
    finally
        xForm.Free;
    end;
end;

procedure TFrmImportDataTypes.PrepareDataset(const aImportDefName: string;
    aDataTypesRecArray: TDataTypesRecArray);
var
    xInt, i: integer;
begin
    fDataTypesRecArray := aDataTypesRecArray;
    edImportDefName.Text := aImportDefName;
    for i := 0 to length(aDataTypesRecArray) - 1 do
    begin
        xInt := cxGrid1TableView1.DataController.AppendRecord;
        cxGrid1TableView1.DataController.Values[xInt, 0] := aDataTypesRecArray[i].Column;
        cxGrid1TableView1.DataController.Values[xInt, 1] := aDataTypesRecArray[i].DataType;
    end;
end;

function TFrmImportDataTypes.GridToRec(): TDataTypesRecArray;
var
    i: integer;
begin
    for i := 0 to length(fDataTypesRecArray) - 1 do
    begin
        fDataTypesRecArray[i].Column := cxGrid1TableView1.DataController.Values[i, 0];
        fDataTypesRecArray[i].DataType := cxGrid1TableView1.DataController.Values[i, 1];
    end;
    result := fDataTypesRecArray;
end;

procedure TFrmImportDataTypes.btnCancelClick(Sender: TObject);
begin
    self.Close;
end;

procedure TFrmImportDataTypes.btnOKClick(Sender: TObject);
begin
    fDataTypesRecArray := self.GridToRec;
    ModalResult := mrOK;
end;

procedure TFrmImportDataTypes.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TImportDataTypesStringLoader.Create;
    fStringLoader.LoadLanguage(self);
    cxGrid1TableView1.Columns[0].Caption := fStringLoader.GetResString(9360);
    cxGrid1TableView1.Columns[1].Caption := fStringLoader.GetResString(9810);
end;

procedure TFrmImportDataTypes.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;


end.
