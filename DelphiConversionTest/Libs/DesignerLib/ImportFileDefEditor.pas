{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : A GUI used to create/edit an Import File Definition
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.04.04 pk                               TN1880   initial version
  04.05.04 pk                               TN1880   Various bug fixes
  19.05.04 pk cmbTableNameDropDown          TN1880   Remove the character ' if it appears in the name
  25.08.05 wl                               TN2558.8 Form-Variable entfernt
  01.11.05 wl  GetNewPathName               TN2541.0 Resourcen ausgelegert
  25.11.05 wl  GetNewPathName               TN2541.0 Textdateien können auch andere Endungen haben als .txt
  01.12.05 pk                               TN2820   username and password for BDE mode
  12.12.05 pk                               TN2847   IDataset instead of TDataset
  15.03.06 wl                               TN2889.3 verwirrendes Menu entfernt
  18.04.06 pk  TablePreview                 TN3001   ObtainDataSet - new session parameter
  19.12.06 wl                               TN3409   Kann nur noch von SystemAdmin editiert werden
  14.10.06 wl  TablePreview                 TN3566   benutzt TBasicDataset, Free am Schluss
  13.03.07 wl  FormCreate                   TN3634   benutzt gmGetPossibleSeparators, um Liste zu füllen
  04.05.07 wl  SaveFileDef                  TN3669   Abfrage nach Begründung einegebaut
  07.08.07 wl  GetFileDefNames              TN3811.3 --> TImportFileDefDataAdaptor.ReadAllItemNames
  09.11.07 pk  GetTableNames                TN3922   uses AdoConnectionMaker
  12.11.07 wl                               TN3922   uses ADO entfernt
  09.01.08 wl                               TN3972   uses geändert
  04.04.08 wl                               TN4058   uses geändert
  14.04.08 wl                               TN4060   uses DialogUtils
  17.04.08 wl                               TN4067   TBasicDataset replaced by TImportQuery
  09.02.09 ts  ChangeDataTypes              TN4346   Datentypen von Ascii-Imports können geändert werden
  06.06.09 ts  TablePreview                 TN4416   Schema.ini wird nur in Ascii-Mode geschrieben
  06.06.09 ts  TablePreview,btnChangeDataT..TN4416   WriteSchemaIniFile entfernt -> ImportClasses
  20.03.09 wl  GetNewPathName               TN4476   Alter Pfad wird für OpenDialog benutzt
  20.03.09 wl  btnResetDatatypesClick       TN4476   Neuer Button zum Zurücksetzen der Datentypen
  13.07.09 pk                               TN4585.4 ReadFileDef params changed
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  24.08.09 wl  fStringLoader                TN4702   fStringLoader lädt Strings für Dialog-Elemente
  24.08.09 wl  btnChangeDatatypesClick      TN4734   Zweites Dataset.Free entfernt
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  16.12.09 pk                               TN4933   TDatabaseProvider functions no longer class functions
  04.02.10 pk                               TN4972   Changes for Restart
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  17.06.10 pk                               TN5152.1  uses DataProviderFactory
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  07.04.11 wl                               TN5541   an SelTablePath angepasst
  12.04.11 ts                               TN5548   new: SkipLines setting to skip the first few lines of import file (if Excel-File)
  01.07.11 wl                               TN5619   komplett überarbeitet: Ist jetzt ein TViewItemEditForm
  22.07.11 wl                               TN5622   viele Details verbessert
  22.07.11 wl                               TN5614   User-Verwaltung integriert
  19.09.11 wl  GetTableNames                TN5672   CreateDatabaseProvider: Neuer Parameter: aOwnsConnectionParams
  03.09.13 wl  GetNewPathName               TN6242   Der InitialPath entspricht jetzt dem gespeicherten Pfad
  -------------------------------------------------------------------------------------------------- }

unit ImportFileDefEditor;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    ExtCtrls,
    Buttons,
    ViewItem,
    ComCtrls,
    Grids,
    StdCtrls,
    ViewItemEditForm,
    ImportDataAdaptor,
    ImportClasses,
    ImportDataProvider,
    ImportFileDefDataAdaptor,
    SQLParser,
    ImportDataTypesDataAdaptor,
    ImportDataTypes,
    StringLoader;

type
    TImportFileDefStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmImportFileDefEditor = class(TViewItemEditForm)
        opdlgPathName: TOpenDialog;
        pnlTab: TPanel;
        pnlPreview: TPanel;
        pnlPreviewBottom: TPanel;
        btnPreview: TButton;
        pgctlPreview: TPageControl;
        shtPreviewRecords: TTabSheet;
        sgPreviewRecords: TStringGrid;
        shtPreviewFields: TTabSheet;
        sgPreviewFields: TStringGrid;
        rgrpPreviewColResize: TRadioGroup;
        pnlFileDefinitionBottom: TPanel;
        lblPathName: TLabel;
        lblTableName: TLabel;
        lblDelimiter: TLabel;
        lblHasHeader: TLabel;
        lblRowOffset: TLabel;
        lblFilter: TLabel;
        lblSQL: TLabel;
        lblUsername: TLabel;
        lblPassword: TLabel;
        lblOrderBy: TLabel;
        lblDatatypes: TLabel;
        lblSkipLines: TLabel;
        edPathName: TEdit;
        btnPathName: TButton;
        cmbTableName: TComboBox;
        cmbDelimiter: TComboBox;
        chkHeader: TCheckBox;
        edRowOffset: TEdit;
        edFilter: TEdit;
        edSQL: TEdit;
        edUsername: TEdit;
        edPassword: TEdit;
        btnSQL: TButton;
        edOrderBy: TEdit;
        btnChangeDatatypes: TButton;
        chkDatatypes: TCheckBox;
        btnResetDatatypes: TButton;
        edSkipLines: TEdit;
        pnlFileDefinitionButtons: TPanel;
        sbtnUseSQL: TSpeedButton;
        sbtnASCIIMode: TSpeedButton;
        sbtnExcelMode: TSpeedButton;
        sbtnAccessMode: TSpeedButton;
        procedure btnPathNameClick(Sender: TObject);
        procedure btnPreviewClick(Sender: TObject);
        procedure cmbTableNameDropDown(Sender: TObject);
        procedure edRowOffsetChange(Sender: TObject);
        procedure btnSQLClick(Sender: TObject);
        procedure pgctlDefinitionChanging(Sender: TObject; var AllowChange: Boolean);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure sbtnUseSQLClick(Sender: TObject);
        procedure rgrpPreviewColResizeClick(Sender: TObject);
        procedure pgctlPreviewChange(Sender: TObject);
        procedure sbtnASCIIModeClick(Sender: TObject);
        procedure sbtnExcelModeClick(Sender: TObject);
        procedure sbtnAccessModeClick(Sender: TObject);
        procedure btnChangeDatatypesClick(Sender: TObject);
        procedure btnResetDatatypesClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure ChangeAnyControl(Sender: TObject);
    private const
        INT_ROWOFFSET_MIN = 0;
        INT_ROWOFFSET_DB_ALTERBY = 1;
        ENUM_MODE_DEFAULT = dbASCII;
    private
        fDataTypesRecArray: TDataTypesRecArray;
        fDataTypesChanged: boolean;
        fMode: TEnDBType;
        fStringLoader: TImportFileDefStringLoader;
        fFirstLoaded: boolean;

        procedure SetGUI(aName, aPathName, aTableName, aDelimiter: string; aRowoffset: integer;
            aHasHeader: boolean; aUsername, aPassword, aSQL, aFilter, aOrderBy: string; aDBType: integer;
            aSkipLines: integer);
        procedure ResetGUI(aMode: TEnDBType);
        function GUIToFileDef(): TFileDefRec;
        procedure FileDefToGUI(aFileDef: TFileDefRec);
        procedure GetSQLDependentParamsFromGUI(var aSQL, aPathName, aFilter, aOrderBy: string);
        function GetRowOffsetFromGUI(): integer;
        procedure TablePreview();
        procedure GetTableNames();
        procedure ResetPreview();

        function ChangeDBType(aMode: TEnDBType): boolean;
        procedure SetModeButtons(aMode: TEnDBType);
        procedure SetGrpSQLVisible(aVisible: boolean);
        procedure SetGrpPathNameVisible(aVisible: boolean);
        procedure SetGrpFilterVisible(aVisible: boolean);
        procedure SetGrpOrderByVisible(aVisible: boolean);
        procedure ResizePreviewGridColumns();
        function GetNewPathName(aOldPathName: string; var aNewPathName: string): boolean;
        function GetModeTypeName(): string;
    protected

        procedure SaveData(); override;
        procedure ResetData(); override;
        procedure UnloadData(); override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;

        procedure FirstLoad(); override;
        class function AskForNewFileDefName(var vNewDefName: string): boolean;
    end;


implementation


{$R *.DFM}

uses
    ControlUtils,
    AppSettings,
    SelTablePath,
    ImportViewItems,
    CommonTypes,
    DialogUtils,
    GeneralTypes,
    ImportSQLEditor,
    DatabaseProvider,
    FileUtilities,
    DataProviderFactory,
    DataConnectionParams;

{ TImportFileDefStringLoader }

procedure TImportFileDefStringLoader.AddAllItems;
begin
    AddSingle(8000, 'Table file', 'Tabellendatei');
    AddSingle(8010, 'Text file', 'Textdatei');
    AddSingle(9390, 'SQL', 'SQL');
    AddSingle(9190, 'Import File Definition', 'Importdatei-Definition');
    AddSingle(9200, 'Definition Name', 'Definitionsname');
    AddSingle(9210, 'Path Name', 'Pfadname');
    AddSingle(9220, 'Table Name', 'Tabellenname');
    AddSingle(9230, 'List delimiter', 'Listentrennzeichen');
    AddSingle(9240, 'Header', 'Kopfzeile');
    AddSingle(9245, 'Skip lines', 'Zeilen ignorieren');
    AddSingle(9250, 'First Row', 'Erste Zeile');
    AddSingle(9260, 'Filter', 'Filter');
    AddSingle(9270, 'SQL', 'SQL');
    AddSingle(9280, 'Sort By', 'Sortiere nach');
    AddSingle(9290, 'Username', 'Benutzername');
    AddSingle(9300, 'Password', 'Passwort');
    AddSingle(9330, 'Preview', 'Vorschau');
    AddSingle(9340, 'File Definition', 'Dateidefinition');
    AddDouble(9380, 'Optimize Column Size', 'Use Header;Ignore Header', 'Spaltebreite optimieren',
        'Header verwenden;Header ignorieren');
    AddSingle(9390, 'SQL', 'SQL');
    AddSingle(9800, 'Use changed Datatypes', 'Geänderte Datentypen nutzen');
    AddSingle(9805, 'Change', 'Ändern');
    AddSingle(9815, 'Reset', 'Zurücksetzen');
    AddSingle(9820, 'Data types:', 'Datentypen:');
    AddSingle(14200, 'Records', 'Datensätze');
    AddSingle(14210, 'Fields', 'Felder');
end;

{ TFrmImportFileDef }

constructor TfrmImportFileDefEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
var
    xSeparators: TStringArray;
begin
    inherited;
    fFirstLoaded := false;
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TImportFileDefStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    fMode := dbNone;
    ResetGUI(ENUM_MODE_DEFAULT);
    self.pgctlPreview.ActivePageIndex := 0;
    xSeparators := TFileInfo.GetPossibleSeparators();
    TControlUtils.AddValuesToComboBox(xSeparators, cmbDelimiter, true);

    pnlFileDefinitionBottom.Enabled := gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level,
        usrSystemAdmin);
end;

procedure TfrmImportFileDefEditor.FirstLoad;
var
    xFileDef: TFileDefRec;
    xDA: TImportFileDefDataAdaptor;
begin
    inherited;
    self.Caption := self.GetCaption;

    xDA := TImportFileDefDataAdaptor.Create;
    try
        xDA.ReadFileDef(self.DataName, xFileDef);
    finally
        xDA.Free;
    end;
    if not xFileDef.Valid then
        raise Exception.Create(TLanguageString.Read('No such Import File Definition [{0}]',
            'Unbekannte Import-Dateidefinition [{0}]', [self.DataName]));

    fDataTypesRecArray := TImportDataTypesDataAdaptor.ImpTableToDataTypeRec(self.DataName);
    fDataTypesChanged := false;
    FileDefToGUI(xFileDef);

    fFirstLoaded := true;
end;

procedure TfrmImportFileDefEditor.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

function TfrmImportFileDefEditor.GUIToFileDef(): TFileDefRec;
var
    xHasHeader: boolean;
    xDelimiter: string;
    xSQL, xPathname, xTableName, xFilter, xOrderBy: string;
    xRowOffset: integer;
begin
    // definition name
    result.Name := self.DataName;

    // SQL or (Pathname, filter and orderby)
    self.GetSQLDependentParamsFromGUI(xSQL, xPathName, xFilter, xOrderBy);
    result.SQL := xSQL;
    result.PathName := xPathname;
    result.Filter := xFilter;
    result.OrderBy := xOrderBy;

    // Tablename
    xTableName := '';
    if cmbTableName.Visible then
    begin
        xTableName := cmbTableName.Text;
    end;
    result.TableName := xTableName;
    // Delimiter
    xDelimiter := '';
    if cmbDelimiter.Visible then
    begin
        xDelimiter := cmbDelimiter.Text;
    end;
    result.Delimiter := xDelimiter;
    // RowOffset
    xRowOffset := INT_ROWOFFSET_MIN;
    if edRowOffSet.Visible then
    begin
        xRowOffset := GetRowOffsetFromGUI();
    end;
    result.RowOffset := xRowOffset;
    // HasHeader
    xHasHeader := false;
    if chkHeader.Visible then
    begin
        xHasHeader := chkHeader.Checked;
    end;
    result.HasHeader := xHasHeader;

    result.Username := edUsername.Text;
    result.Password := edPassword.Text;
    result.DBType := integer(fMode);

    result.Valid := true;
    result.SkipLines := StrToInt(edSkipLines.Text);
end;

procedure TfrmImportFileDefEditor.FileDefToGUI(aFileDef: TFileDefRec);
begin
    SetGUI(aFileDef.Name, aFileDef.PathName, aFileDef.TableName, aFileDef.Delimiter,
        aFileDef.RowOffset + INT_ROWOFFSET_DB_ALTERBY, aFileDef.HasHeader, aFileDef.Username,
        aFileDef.Password, aFileDef.SQL, aFileDef.Filter, aFileDef.OrderBy, aFileDef.DBType,
        aFileDef.SkipLines);
end;

procedure TfrmImportFileDefEditor.SetGUI(aName, aPathName, aTableName, aDelimiter: string;
    aRowoffset: integer; aHasHeader: boolean; aUsername, aPassword, aSQL, aFilter, aOrderBy: string;
    aDBType: integer; aSkipLines: integer);
begin
    edPathName.Text := aPathName;
    cmbTableName.Text := aTableName;
    cmbDelimiter.Text := aDelimiter;
    edRowOffset.Text := IntToStr(aRowOffset);
    chkHeader.Checked := aHasHeader;
    edUsername.Text := aUsername;
    edPassword.Text := aPassword;
    edSQL.Text := aSQL;
    edFilter.Text := aFilter;
    edOrderBy.Text := aOrderBy;
    ChangeDBType(TEnDBType(aDBType));
    chkDataTypes.Checked := false;
    if TImportDataTypesDataAdaptor.ImportDefExists(aName) then
        chkDataTypes.Checked := true;
    edSkipLines.Text := IntToStr(aSkipLines);
end;

procedure TfrmImportFileDefEditor.ResetData;
begin
    inherited;

end;

procedure TfrmImportFileDefEditor.ResetGUI(aMode: TEnDBType);
var
    xFileDef: TFileDefRec;
begin
    xFileDef := TImportFileDefDataAdaptor.MakeFileDefRec(false, Integer(aMode), '', '', '', '',
        INT_ROWOFFSET_MIN, false, '', '', '', '', '', 0);
    self.FileDefToGUI(xFileDef);
end;

function TfrmImportFileDefEditor.GetRowOffsetFromGUI(): integer;
begin
    result := INT_ROWOFFSET_MIN - 1;
    try
        result := StrToInt(edRowOffset.Text) - INT_ROWOFFSET_DB_ALTERBY;
    except
    end; // alter by -1 when storing in table;
end;

function TfrmImportFileDefEditor.GetModeTypeName: string;
begin
    result := TFileInfo.GetTypeNameFromDBType(fMode)
end;

function TfrmImportFileDefEditor.GetNewPathName(aOldPathName: string; var aNewPathName: string): boolean;
var
    xNewPathName: string;
    xFilter: string;
begin
    result := false;
    case fMode of
        dbExcel:
            xFilter := TLanguageString.Read('Microsoft Excel files(*.XLS)|*.XLS|',
                'Microsoft Excel Dateien(*.XLS)|*.XLS|');
        dbStandard:
            xFilter := TLanguageString.Read('TurboDB files(*.dat)|*.dat;Paradox files(*.db)|*.db|',
                'TurboDB-Dateien(*.dat)|*.dat;Paradox-Dateien(*.db)|*.db|');
        dbAscii:
            xFilter := TLanguageString.
                Read('Text files (*.TXT;*.ASC;*.CSV)|*.TXT;*.ASC;*.CSV|All files (*.*)|*.*|',
                'Textdateien (*.TXT;*.ASC;*.CSV)|*.TXT;*.ASC;*.CSV|Alle Dateien (*.*)|*.*|');
        dbAccess:
            xFilter := TLanguageString.Read('Microsoft Access files(*.MDB)|*.MDB|',
                'Microsoft Access Dateien(*.MDB)|*.MDB|');
        else
            xFilter := '';
    end;
    opdlgPathName.Filter := xFilter;

    if fMode = dbStandard then
    begin
        xNewPathName := aOldPathName;
        if TFrmSelTablePath.SelectTablePathModal(xNewPathName) = mrCancel then
            Exit;
    end
    else
    begin
        if Pos(':', aOldPathName) <> 1 then
        begin
            opdlgPathName.FileName := ExtractFileName(aOldPathName);
            opdlgPathName.InitialDir := TFileUtilities.ExtractFileDir(aOldPathName);
        end;
        if not opdlgPathName.Execute() then
            Exit;
        xNewPathName := opdlgPathName.FileName;
    end;

    if xNewPathName = aOldPathName then
        Exit;
    aNewPathName := xNewPathName;
    result := true;
end;

procedure TfrmImportFileDefEditor.btnPathNameClick(Sender: TObject);
var
    xNewPathName: string;
begin
    if not GetNewPathName(edPathName.Text, xNewPathName) then
        Exit;

    edPathName.Text := xNewPathName;
    cmbTableName.Items.Clear;
    cmbTableName.Text := '';
end;

procedure TfrmImportFileDefEditor.GetTableNames();
var
    xDatabaseProvider: TDatabaseProvider;
    xDataConnectionParams: TDataConnectionParams;
    xNames: TStringArray;
begin
    xDataConnectionParams := TDataConnectionParams.Create(self.GetModeTypeName, edPathName.Text);
    xDatabaseProvider := TDataProviderFactory.Instance.CreateDatabaseProvider(xDataConnectionParams, true);
    try
        xNames := xDatabaseProvider.GetAllTableNames();
    finally
        xDatabaseProvider.Free;
    end;
    TControlUtils.AddValuesToComboBox(xNames, cmbTableName, true);
end;

procedure TfrmImportFileDefEditor.GetSQLDependentParamsFromGUI(var aSQL, aPathName, aFilter,
    aOrderBy: string);
begin
    aSQL := '';
    aPathName := '';
    aFilter := '';
    aOrderBy := '';
    if sbtnUseSQL.Down then
        aSQL := edSQL.Text
    else
    begin
        aPathName := edPathname.Text;
        aFilter := edFilter.Text;
        aOrderBy := edOrderBy.Text;
    end;
end;

procedure TfrmImportFileDefEditor.TablePreview();
var
    xGUIFileDef: TFileDefRec;
    xDataset: TImportQuery;
    xDBType: TEnDBType;
begin
    xGUIFileDef := GUIToFileDef();
    xDBType := TEnDBType(xGUIFileDef.DBType);
    if xDBType = dbNone then
        Exit;

    xDataSet := TImports.CreateAnyDataSet(xDBType, '', xGUIFileDef.PathName, xGUIFileDef.TableName,
        xGUIFileDef.Delimiter, xGUIFileDef.HasHeader, xGUIFileDef.RowOffset, xGUIFileDef.Filter,
        xGUIFileDef.OrderBy, xGUIFileDef.Username, xGUIFileDef.Password, xGUIFileDef.SQL, true, false,
        fDataTypesRecArray, xGUIFileDef.SkipLines);
    try
        ResetPreview();
        TImports.WriteDataSetToGrid(xDataSet, self.sgPreviewRecords);
        TImports.WriteFieldInfoToGrid(xDataSet, self.sgPreviewFields);
        self.ResizePreviewGridColumns();
    finally
        xDataSet.Free;
    end;
end;

procedure TfrmImportFileDefEditor.UnloadData;
begin
    inherited;

end;

procedure TfrmImportFileDefEditor.ResetPreview();
begin
    self.sgPreviewRecords.RowCount := 0;
    self.sgPreviewRecords.ColCount := 0;
    self.sgPreviewRecords.Cells[0, 0] := '';

    self.sgPreviewFields.RowCount := 0;
    self.sgPreviewFields.ColCount := 0;
    self.sgPreviewFields.Cells[0, 0] := '';
end;

procedure TfrmImportFileDefEditor.btnPreviewClick(Sender: TObject);
begin
    TablePreview();
end;

procedure TfrmImportFileDefEditor.cmbTableNameDropDown(Sender: TObject);
var
    i: integer;
    xWorksheetName: string;
begin
    if not(fMode in [dbExcel, dbAccess]) then
        Exit;
    if cmbTableName.Items.Count > 0 then
        Exit;
    GetTableNames();

    for i := 0 to cmbTableName.Items.Count - 1 do
    begin
        xWorksheetName := cmbTableName.Items[i];
        xWorksheetName := StringReplace(xWorksheetName, '$', '', []);
        xWorksheetName := StringReplace(xWorksheetName, '''', '', [rfReplaceAll]);
        cmbTableName.Items[i] := xWorksheetName;
    end;
end;

function TfrmImportFileDefEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TImportFileDefViewItem.Create(aItemName);

end;

procedure TfrmImportFileDefEditor.ChangeAnyControl(Sender: TObject);
begin
    if fFirstLoaded then
        ChangeData();
end;

function TfrmImportFileDefEditor.ChangeDBType(aMode: TEnDBType): boolean;
begin
    if (fMode = aMode) then
        EXIT(false);

    fMode := aMode;
    SetModeButtons(fMode);

    self.cmbTableName.Visible := fMode in [dbExcel, dbAccess];
    self.lblTableName.Visible := fMode in [dbExcel, dbAccess];

    self.cmbDelimiter.Visible := fMode in [dbAscii];
    self.lblDelimiter.Visible := fMode in [dbAscii];

    self.chkHeader.Visible := fMode in [dbExcel, dbAscii];
    self.lblHasHeader.Visible := fMode in [dbExcel, dbAscii];

    self.lblSkipLines.Visible := fMode in [dbExcel];
    self.edSkipLines.Visible := fMode in [dbExcel];

    self.edUsername.Visible := fMode in [dbStandard];
    self.lblUsername.Visible := fMode in [dbStandard];

    self.edPassword.Visible := fMode in [dbStandard];
    self.lblPassword.Visible := fMode in [dbStandard];

    self.btnChangeDatatypes.Visible := fMode in [dbAscii];
    self.btnResetDatatypes.Visible := fMode in [dbAscii];
    self.chkDatatypes.Visible := fMode in [dbAscii];
    self.lblDatatypes.Visible := fMode in [dbAscii];

    self.edRowOffset.Visible := fMode <> dbNone;
    self.lblRowOffset.Visible := fMode <> dbNone;
    SetGrpSQLVisible(fMode in [dbStandard]);

    SetGrpPathNameVisible(not(fMode in [dbStandard]) and (fMode <> dbNone));
    SetGrpFilterVisible(not(fMode in [dbStandard]) and (fMode <> dbNone));
    SetGrpOrderByVisible(not(fMode in [dbStandard]) and (fMode <> dbNone));
    TControlUtils.SetControlMemberProperty(pnlFileDefinitionButtons, cpEnabledSpecial, fMode <> dbNone);
    EXIT(true);
end;

procedure TfrmImportFileDefEditor.SetModeButtons(aMode: TEnDBType);
begin
    if gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
    begin
        self.sbtnUseSQL.Down := aMode = dbStandard;
        self.sbtnAsciiMode.Down := aMode = dbASCII;
        self.sbtnExcelMode.Down := aMode = dbExcel;
        self.sbtnAccessMode.Down := aMode = dbAccess;
    end
    else
    begin
        self.sbtnUseSQL.Visible := aMode = dbStandard;
        self.sbtnAsciiMode.Visible := aMode = dbASCII;
        self.sbtnExcelMode.Visible := aMode = dbExcel;
        self.sbtnAccessMode.Visible := aMode = dbAccess;
    end;
end;

procedure TfrmImportFileDefEditor.SaveData;
var
    xFileDef: TFileDefRec;
    xDA: TImportFileDefDataAdaptor;
begin
    inherited;

    if (chkDataTypes.Checked) then
        TImportDataTypesDataAdaptor.DatasetToTable(self.DataName, fDataTypesRecArray)
    else
        TImportDataTypesDataAdaptor.DeleteImportDef(self.DataName);

    xFileDef := GUIToFileDef();
    xDA := TImportFileDefDataAdaptor.Create;
    try
        xDA.WriteFileDef(xFileDef);
    finally
        xDA.Free;
    end;
end;

class function TfrmImportFileDefEditor.AskForNewFileDefName(var vNewDefName: string): boolean;
var
    xDA: TImportFileDefDataAdaptor;
    xExistingItems: TStringArray;
begin
    result := false;

    xDA := TImportFileDefDataAdaptor.Create();
    try
        xExistingItems := xDA.ReadAllNames();
    finally
        xDA.Free;
    end;

    if not TDialogUtils.AskForNewName(vNewDefName, TLanguageString.Read('Enter new {0} name:',
        'Bitte den neuen {0}-Namen eingeben:', [TLanguageString.Read('File Definition', 'Dateidefinition')]),
        TLanguageString.Read('New {0}', '{0}: Neu', [TLanguageString.Read('File Definition',
        'Dateidefinition')]), TLanguageString.Read('{0} already exists!', '{0} existiert bereits!'),
        xExistingItems) then
        EXIT;

    if (vNewDefName = '') then
        EXIT;
    result := true;
end;

procedure TfrmImportFileDefEditor.edRowOffsetChange(Sender: TObject);
begin
    if GetRowOffsetFromGUI() < INT_ROWOFFSET_MIN then
        edRowOffset.Text := IntToStr(INT_ROWOFFSET_MIN + INT_ROWOFFSET_DB_ALTERBY);
    ChangeAnyControl(Sender);
end;

procedure TfrmImportFileDefEditor.SetGrpSQLVisible(aVisible: boolean);
begin
    self.edSQL.Visible := aVisible;
    self.lblSQL.Visible := aVisible;
    self.btnSQL.Visible := aVisible;
end;

procedure TfrmImportFileDefEditor.SetGrpPathNameVisible(aVisible: boolean);
begin
    self.edPathName.Visible := aVisible;
    self.lblPathName.Visible := aVisible;
    self.btnPathName.Visible := aVisible and gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level,
        usrSystemAdmin);
end;

procedure TfrmImportFileDefEditor.SetGrpFilterVisible(aVisible: boolean);
begin
    self.edFilter.Visible := aVisible;
    self.lblFilter.Visible := aVisible;
end;

procedure TfrmImportFileDefEditor.SetGrpOrderByVisible(aVisible: boolean);
begin
    self.edOrderBy.Visible := aVisible;
    self.lblOrderBy.Visible := aVisible;
end;

procedure TfrmImportFileDefEditor.btnSQLClick(Sender: TObject);
begin
    edSQL.Text := TfrmSQLEditor.EditAnySQL(edSQL.Text, self.edUsername.Text, self.edPassword.Text);
end;

procedure TfrmImportFileDefEditor.pgctlDefinitionChanging(Sender: TObject; var AllowChange: Boolean);
begin
    AllowChange := false;
end;

procedure TfrmImportFileDefEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    self.ModalResult := mrOK;
end;

procedure TfrmImportFileDefEditor.ResizePreviewGridColumns();
var
    xFirstRow: integer;
begin
    xFirstRow := 0;
    case rgrpPreviewColResize.ItemIndex of
        0:
            xFirstRow := 0;
        1:
            xFirstRow := 1;
    end;
    TControlUtils.OptimizeGridColumnSizes(sgPreviewRecords, xFirstRow);
end;

procedure TfrmImportFileDefEditor.rgrpPreviewColResizeClick(Sender: TObject);
begin
    ResizePreviewGridColumns();
end;

procedure TfrmImportFileDefEditor.pgctlPreviewChange(Sender: TObject);
begin
    rgrpPreviewColResize.Visible := (pgctlPreview.ActivePageIndex = 0);
end;

procedure TfrmImportFileDefEditor.sbtnASCIIModeClick(Sender: TObject);
begin
    if gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
        if ChangeDBType(dbAscii) then
            self.ChangeData;
end;

procedure TfrmImportFileDefEditor.sbtnUseSQLClick(Sender: TObject);
begin
    if gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
        if ChangeDBType(dbStandard) then
            self.ChangeData;
end;

procedure TfrmImportFileDefEditor.sbtnExcelModeClick(Sender: TObject);
begin
    if gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
        if ChangeDBType(dbExcel) then
            self.ChangeData;
end;

procedure TfrmImportFileDefEditor.sbtnAccessModeClick(Sender: TObject);
begin
    if gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
        if ChangeDBType(dbAccess) then
            self.ChangeData;
end;

procedure TfrmImportFileDefEditor.btnChangeDatatypesClick(Sender: TObject);
var
    xDBType: TEnDBType;
    xDataset: TImportQuery;
    xGUIFileDef: TFileDefRec;
    xDataTypesRecArray: TDataTypesRecArray;
begin
    xGUIFileDef := GUIToFileDef();
    xDBType := TEnDBType(xGUIFileDef.DBType);
    if xDBType = dbNone then
        Exit;

    xDataSet := TImports.CreateAnyDataSet(xDBType, '', xGUIFileDef.PathName, xGUIFileDef.TableName,
        xGUIFileDef.Delimiter, xGUIFileDef.HasHeader, xGUIFileDef.RowOffset, xGUIFileDef.Filter,
        xGUIFileDef.OrderBy, xGUIFileDef.Username, xGUIFileDef.Password, xGUIFileDef.SQL, true, false,
        fDataTypesRecArray, xGUIFileDef.SkipLines);
    try
        xDataTypesRecArray := TImports.DataTypesToRecArray(xDataset);
    finally
        xDataSet.Free;
    end;

    if TFrmImportDataTypes.ModalOpen(xGUIFileDef.Name, xDataTypesRecArray) = mrOK then
    begin
        fDataTypesRecArray := xDataTypesRecArray;
        chkDatatypes.Checked := true;
        fDataTypesChanged := true;
        self.ChangeData;
    end;
end;

procedure TfrmImportFileDefEditor.btnResetDatatypesClick(Sender: TObject);
begin
    if not Assigned(fDataTypesRecArray) then
        EXIT;

    fDataTypesRecArray := nil;
    chkDatatypes.Checked := false;
    fDataTypesChanged := true;
    self.ChangeData;
end;


end.
