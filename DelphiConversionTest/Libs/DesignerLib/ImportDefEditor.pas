{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : A GUI used to create/edit an Import Definition
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.04.04 pk                               TN1880   initial version
  04.05.04 pk                               TN1880   Various bug fixes
  17.05.04 pk  GUIToImportDef               TN1880   Do not allow source column name to end with blank space
  09.09.04 pk                               TN2129   Use line number instead of variable name for the component.name
  01.12.05 pk GetAllFieldNamesOfTable       TN2820   pass username and password as arguments
  03.02.06 wl  SourceNameChanged            TN2927   '#TotalRecords' und '#CurrentRecord' werden angezeigt
  07.03.06 wl  AskForNewDefName             TN2956   Es werden alle Def-Namen überprüft, nicht nur die Mode gleichen Mode!
  07.03.06 wl  cxGrid1                      TN2889.2 für die Oberfläche wird jetzt cxGrid verwendet
  07.03.06 wl                               TN2889.2 neue Felder: "Required" und "Default value"
  15.03.06 wl                               TN2889.3 an ImportFileDef angepasst
  19.12.06 wl                               TN3409   Kann nur noch von SystemAdmin editiert werden
  04.05.07 wl  SaveDef                      TN3669   Abfrage nach Begründung einegebaut
  07.08.07 wl  GetDefNames                  TN3811.3 --> TImportDefDataAdaptor.ReadAllDefNames
  09.11.07 pk  SaveDef                      TN3922   use ImportColDefsDataAdaptor to delete old entries
  09.01.08 wl                               TN3972   uses geändert
  04.04.08 wl                               TN4058    uses geändert
  14.04.08 wl                               TN4060   uses DialogUtils
  13.07.09 pk                               TN4585.4 ReadFileDef params changed
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  24.08.09 wl  fStringLoader                TN4702   fStringLoader lädt Strings für Dialog-Elemente
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  20.11.09 pk  SourceNameChanged            TN4843   clear the dropdown items before refreshing
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  18.06.10 pk                               TN5152.1 uses changed
  04.08.10 pk                               TN5218   Changes for Plugin database packages
  07.04.11 wl                               TN5541   an SelTablePath angepasst
  01.07.11 wl                               TN5619   komplett überarbeitet: Ist jetzt ein TViewItemEditForm
  22.07.11 wl                               TN5622   viele Details verbessert
  22.07.11 wl                               TN5614   User-Verwaltung integriert
  12.03.12 wl  btnOpenFileDefClick          TN5830   neu: Button, um direkt in die File-Definition zu springen
  12.03.12 wl  cmbSourceDefDropDown         TN5830   Liste wird jedes Mal neu erstellt -> Refresh-Button entfernt
  18.04.12 ts  SaveData                     TN5871   SetFocus replaced by UpdateData (focus cannot be set to hidden window)
  02.11.12 wl  pmnuDeleteClick              TN6005   Nach dem Löschen wurde der Save-Button nicht aktiviert
  13.03.13 wl                               TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  -------------------------------------------------------------------------------------------------- }

unit ImportDefEditor;


interface


uses
    Forms,
    Classes,
    Controls,
    StdCtrls,
    ExtCtrls,
    Menus,
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
    cxGrid,
    cxLookAndFeels,
    cxLookAndFeelPainters,

    ImportDataAdaptor,
    ViewItem,
    ImportColDefDataAdaptor,
    StringLoader,
    ViewItemEditForm;

type
    TImportDefStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TFrmImportDefMode = (idmNone, idmTableImport, idmMethVarImport);

    TfrmImportDefEditor = class(TViewItemEditForm)
        pnlMiddle: TPanel;
        pnlTargetAndSourceTops: TPanel;
        Bevel2: TBevel;
        pnlSourceTop: TPanel;
        pnlTargetTop: TPanel;
        edTargetFile: TEdit;
        Label1: TLabel;
        btnGetTargetFileName: TButton;
        cmbSourceDef: TComboBox;
        Label2: TLabel;
        Splitter1: TSplitter;
        cxEditRepository1: TcxEditRepository;
        cxEditRepository1CheckBoxItem1: TcxEditRepositoryCheckBoxItem;
        cxEditRepository1ComboBoxItem1: TcxEditRepositoryComboBoxItem;
        cxGrid1: TcxGrid;
        cxGrid1TableView1: TcxGridTableView;
        cxGrid1TableView1Column1: TcxGridColumn;
        cxGrid1TableView1Column2: TcxGridColumn;
        cxGrid1TableView1Column3: TcxGridColumn;
        cxGrid1TableView1Column4: TcxGridColumn;
        cxGrid1Level1: TcxGridLevel;
        PopupMenu1Grid: TPopupMenu;
        pmnuDelete: TMenuItem;
        pmnuAppend: TMenuItem;
        N1: TMenuItem;
        pmnuMatchTarget: TMenuItem;
        btnOpenFileDef: TButton;
        procedure btnGetTargetFileNameClick(Sender: TObject);
        procedure cmbSourceDefChange(Sender: TObject);
        procedure pmnuAppendClick(Sender: TObject);
        procedure pmnuDeleteClick(Sender: TObject);
        procedure pmnuMatchTargetClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure btnOpenFileDefClick(Sender: TObject);
        procedure cxGrid1TableView1EditChanged(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
        procedure cmbSourceDefDropDown(Sender: TObject);
    private const
        INT_COLUMN_INDEX_TARGETCOLUMN = 0;
        INT_COLUMN_INDEX_SOURCECOLUMN = 1;
        INT_COLUMN_INDEX_SOURCEREQUIRED = 2;
        INT_COLUMN_INDEX_SOURCEDEFAULT = 3;
    private
        fMode: TFrmImportDefMode;
        fStringLoader: TImportDefStringLoader;

        function GUIToColDefArray(): TImportColDefRecArray;
        procedure RequerySourceDefComboBox();
        function GUIToImportDef: TImportDefRec;
        procedure TargetFileChanged(aTargetFile: string; aAddMissingTargetNames: boolean;
            aImportDefRec: TImportDefRec);
        procedure SourceNameChanged(aSourceName: string; aSkipIfUnchanged: boolean);
        function SelectSourceName(aSourceName: string): string;
        procedure SetMode;
        function AddLine(const aTargetColName: string): integer;
        procedure AddLineForEachColumnInTargetFile(aTargetFile: string);
        procedure LinkSourceNameToTargetName(aRec: TImportColDefRec; aAddMissingTargetNames: boolean);
        function FindTargetNameInRecArray(const aTargetName: string;
            aDefRecArray: TImportColDefRecArray): boolean;
    protected
        procedure SaveData(); override;
        procedure ResetData(); override;
        procedure UnloadData(); override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string; aOnSaveStatusChanged: TNotifyEvent;
            aMode: TFrmImportDefMode); reintroduce;

        procedure FirstLoad(); override;

        class function GetDefMode(aFrmDefMode: TFrmImportDefMode): integer;
        class function GetFrmDefMode(aDefMode: integer): TFrmImportDefMode;
        class function AskForNewDefName(var vNewDefName: string): boolean;
    end;


implementation


{$R *.DFM}

uses
    Dialogs,
    Variants,
    SysUtils,
    AppSettings,
    ImportClasses,
    SelTablePath,
    ImportFileDefEditor,
    CommonTypes,
    ImportDataProvider,
    ControlUtils,
    ImportFileDefDataAdaptor,
    DialogUtils,
    SQLParser,
    ViewItemsWorkflow,
    ImportViewItems,
    GeneralTypes;

{ TImportDefStringLoader }

procedure TImportDefStringLoader.AddAllItems;
begin
    AddSingle(160, 'Options', 'Optionen');
    AddSingle(9050, 'Import Definition', 'Import Definition');
    AddSingle(9070, 'Target File', 'Zieldatei');
    AddSingle(9080, 'Source File Definition', 'Quelldatei-Definition');
    AddSingle(9090, 'Refresh', 'Refresh');
    AddSingle(9100, 'Add Line', 'Zeile Hinzufügen');
    AddSingle(9110, 'Delete selected line(s)', 'Markierte Zeile(n) löschen');
    AddSingle(9130, 'Match to Target', 'Dem Ziel Angleichen');
end;

{ TFrmImportDef }

constructor TfrmImportDefEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent; aMode: TFrmImportDefMode);
begin
    fMode := aMode;
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged);

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TImportDefStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    cxGrid1TableView1Column1.Caption := TLanguageString.Read('Target Column', 'Zielspalte');
    cxGrid1TableView1Column2.Caption := TLanguageString.Read('Source Column', 'Quellspalte');
    cxGrid1TableView1Column3.Caption := TLanguageString.Read('Required', 'Notwendig');
    cxGrid1TableView1Column4.Caption := TLanguageString.Read('Default value', 'Standardwert');

    RequerySourceDefComboBox();
    SetMode;

    if not gCommonDll.LevelIsIncluded(gCommonDll.CurrentUser.Level, usrSystemAdmin) then
    begin
        btnGetTargetFileName.Visible := false;
        cmbSourceDef.Enabled := false;
        self.cxGrid1.Enabled := false;
    end;
end;

procedure TfrmImportDefEditor.FirstLoad;
var
    xImportDefRec: TImportDefRec;
    xAddMissingTargetNames: boolean;
begin
    inherited;

    self.Caption := self.GetCaption;

    xImportDefRec := TImportDefDataAdaptor.QuickReadDef(self.DataName, true, false);
    if not xImportDefRec.Valid then
        raise Exception.Create(TLanguageString.Read('No such Import Definition [{0}]',
            'Unbekannte Importdefinition [{0}]', [self.DataName]));

    // fMode := self.GetFrmDefMode(xImportDefRec.Mode);
    SetMode();
    self.SourceNameChanged(xImportDefRec.SourceFileDefName, true);
    xAddMissingTargetNames := (xImportDefRec.TargetName = '');
    self.TargetFileChanged(xImportDefRec.TargetName, xAddMissingTargetNames, xImportDefRec);
end;

procedure TfrmImportDefEditor.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

class function TfrmImportDefEditor.GetFrmDefMode(aDefMode: integer): TFrmImportDefMode;
begin
    case aDefMode of
        INT_DEF_MODE_TABLEIMPORT:
            result := idmTableImport;
        INT_DEF_MODE_METHVARIMPORT:
            result := idmMethVarImport;
        else
            result := idmNone;
    end;
end;

class function TfrmImportDefEditor.GetDefMode(aFrmDefMode: TFrmImportDefMode): integer;
begin
    case aFrmDefMode of
        idmTableImport:
            result := INT_DEF_MODE_TABLEIMPORT;
        idmMethVarImport:
            result := INT_DEF_MODE_METHVARIMPORT;
        else
            result := INT_DEF_MODE_INVALID;
    end;
end;

procedure TfrmImportDefEditor.btnGetTargetFileNameClick(Sender: TObject);
var
    xPath: string;
    xImportDefRec: TImportDefRec;
begin
    xPath := self.edTargetFile.Text;
    if TFrmSelTablePath.SelectTablePathModal(xPath) = mrCancel then
        Exit;

    if (xPath = edTargetFile.Text) then
        EXIT;

    xImportDefRec := TImportDefDataAdaptor.QuickReadDef(self.DataName, true, false);
    TargetFileChanged(xPath, false, xImportDefRec);
    ChangeData();
end;

procedure TfrmImportDefEditor.btnOpenFileDefClick(Sender: TObject);
begin
    TViewItemsWorkflow.Instance.OpenEditForm(self.cmbSourceDef.Text, TViewItemType.ntImportFileDef);
end;

procedure TfrmImportDefEditor.AddLineForEachColumnInTargetFile(aTargetFile: string);
var
    x: integer;
    xNames: TStringArray;
begin
    // always get all the target column names fresh from the target file itself instead of
    // using aImportDefRec -> It is very possible that aImportDefRec doesn't have all of the column names of the target file
    xNames := TImports.GetAllFieldNamesOfTable(dbStandard, aTargetFile, '', '', false, '', '', '');

    for x := 0 to high(xNames) do
    begin
        self.AddLine(xNames[x]);
    end;
end;

procedure TfrmImportDefEditor.TargetFileChanged(aTargetFile: string; aAddMissingTargetNames: boolean;
    aImportDefRec: TImportDefRec);
var
    x: integer;
begin
    if (aTargetFile <> '') and (aTargetFile = edTargetFile.Text) then
        EXIT;

    edTargetFile.Text := aTargetFile;

    // alle Zeilen löschen
    for x := cxGrid1TableView1.DataController.RecordCount - 1 downto 0 do
        cxGrid1TableView1.DataController.DeleteRecord(x);

    if (aTargetFile <> '') then
        AddLineForEachColumnInTargetFile(aTargetFile);

    if aImportDefRec.Valid then
        for x := 0 to high(aImportDefRec.ImportColDefs) do
            self.LinkSourceNameToTargetName(aImportDefRec.ImportColDefs[x], aAddMissingTargetNames);
end;

procedure TfrmImportDefEditor.UnloadData;
begin
    inherited;

end;

procedure TfrmImportDefEditor.RequerySourceDefComboBox();
var
    xDA: TImportFileDefDataAdaptor;
    xExistingItems: TStringArray;
begin
    xDA := TImportFileDefDataAdaptor.Create();
    try
        xExistingItems := xDA.ReadAllNames();
    finally
        xDA.Free;
    end;

    cmbSourceDef.Clear;
    TControlUtils.AddValuesToComboBox(xExistingItems, cmbSourceDef, true);
end;

procedure TfrmImportDefEditor.ResetData;
begin
    inherited;

end;

procedure TfrmImportDefEditor.SourceNameChanged(aSourceName: string; aSkipIfUnchanged: boolean);
var
    xFileDef: TFileDefRec;
    xFileDefName: string;
    xFieldCount: integer;
    i: integer;
    xDBType: TEnDBType;
    xFileDefDataAdaptor: TImportFileDefDataAdaptor;
    xNames: TStringArray;
begin
    if (aSkipIfUnchanged) and (self.cmbSourceDef.Text = aSourceName) then
        EXIT;

    // end the edit mode if the source fields column is being edited, otherwise the properties/drop down items of the item being edited
    // are not updated
    if (cxGrid1TableView1.Controller.FocusedColumn = cxGrid1TableView1Column2) and
        (cxGrid1TableView1.Controller.EditingController.IsEditing) then
        cxGrid1TableView1.Controller.EditingController.HideEdit(true);

    cxEditRepository1ComboBoxItem1.Properties.Items.Clear;

    xFileDefName := SelectSourceName(aSourceName);
    if (xFileDefName = '') then
        EXIT;

    xFileDefDataAdaptor := TImportFileDefDataAdaptor.Create;
    try
        xFileDefDataAdaptor.ReadFileDef(xFileDefName, xFileDef);
    finally
        xFileDefDataAdaptor.Free;
    end;

    if (not xFileDef.Valid) then
        EXIT;

    xDBType := TEnDBType(xFileDef.DBType);
    try
        xNames := TImports.GetAllFieldNamesOfTable(xDBType, xFileDef.PathName, xFileDef.TableName,
            xFileDef.Delimiter, xFileDef.HasHeader, xFileDef.Username, xFileDef.Password, xFileDef.SQL);

        for i := 0 to high(xNames) do
            cxEditRepository1ComboBoxItem1.Properties.Items.Add(xNames[i]);
    except
        on E: Exception do
            ShowMessage(E.Message);
    end;

    xFieldCount := cxEditRepository1ComboBoxItem1.Properties.Items.Count;
    if (not xFileDef.HasHeader) and (xDBType in [dbExcel, dbAscii]) then
        cxEditRepository1ComboBoxItem1.Properties.Items.Clear;
    for i := 0 to xFieldCount - 1 do
        cxEditRepository1ComboBoxItem1.Properties.Items.Add(format('%.2d', [i + 1]));
    cxEditRepository1ComboBoxItem1.Properties.Items.Add(STR_IMPORT_SOURCEFIELD_TOTALRECORDS);
    cxEditRepository1ComboBoxItem1.Properties.Items.Add(STR_IMPORT_SOURCEFIELD_CURRENTRECORD);
    cxEditRepository1ComboBoxItem1.Properties.Items.Add('=');
end;

function TfrmImportDefEditor.SelectSourceName(aSourceName: string): string;
// attempt to select aSourceName from the combobox
// if the selection was successful aSourceName will be returned
// otherwise an empty string will be returned
begin
    result := TControlUtils.SelectComboBoxEntry(self.cmbSourceDef, aSourceName);
end;

procedure TfrmImportDefEditor.cmbSourceDefChange(Sender: TObject);
begin
    ChangeData;
    SourceNameChanged(self.cmbSourceDef.Text, false);
end;

procedure TfrmImportDefEditor.cmbSourceDefDropDown(Sender: TObject);
var
    xName: string;
begin
    xName := self.cmbSourceDef.Text;
    self.RequerySourceDefComboBox();

    self.SourceNameChanged(xName, false);
    self.cmbSourceDef.Text := xName;
end;

function TfrmImportDefEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    if (fMode = idmTableImport) then
        EXIT(TTableImportDefViewItem.Create(aItemName));
    if (fMode = idmMethVarImport) then
        EXIT(TVarImportDefViewItem.Create(aItemName));
    EXIT(nil);
end;

procedure TfrmImportDefEditor.cxGrid1TableView1EditChanged(Sender: TcxCustomGridTableView;
    AItem: TcxCustomGridTableItem);
begin
    ChangeData();
end;

function TfrmImportDefEditor.GUIToImportDef: TImportDefRec;
var
    i: integer;
begin
    result.Name := self.DataName;
    result.Mode := Integer(fMode);
    result.TargetName := self.edTargetFile.Text;
    result.SourceFileDefName := cmbSourceDef.Text;
    result.ImportColDefs := self.GUIToColDefArray();

    for i := 0 to high(result.ImportColDefs) do
    begin
        result.ImportColDefs[i].ImportDefName := result.Name;
    end;
end;

procedure TfrmImportDefEditor.SaveData;
var
    xImportDefRec: TImportDefRec;
    xDataAdaptor: TImportDefDataAdaptor;
    xColDefsDA: TImportColDefsDataAdaptor;
begin
    inherited;

    // nur, damit der letzte editierte Wert auch gespeichert wird
    cxGrid1TableView1.DataController.UpdateData;

    xImportDefRec := GUIToImportDef;

    xColDefsDA := TImportColDefsDataAdaptor.Create();
    try
        xColDefsDA.DeleteColDefsByDefName(xImportDefRec.Name);
    finally
        xColDefsDA.Free;
    end;

    xDataAdaptor := TImportDefDataAdaptor.Create();
    try
        xDataAdaptor.WriteDef(xImportDefRec);
    finally
        xDataAdaptor.Free;
    end;
end;

procedure TfrmImportDefEditor.SetMode;
begin
    pmnuAppend.Visible := (fMode = idmMethVarImport);
    N1.Visible := (fMode = idmMethVarImport);
    pmnuDelete.Visible := (fMode = idmMethVarImport);
    cxGrid1TableView1Column1.Options.Editing := (fMode = idmMethVarImport);
    cxGrid1TableView1Column1.Options.Focusing := (fMode = idmMethVarImport);

    pmnuMatchTarget.Visible := (fMode = idmTableImport);
    pnlTargetTop.Visible := (fMode = idmTableImport);
    Splitter1.Visible := (fMode = idmTableImport);
end;

procedure TfrmImportDefEditor.pmnuDeleteClick(Sender: TObject);
var
    x: integer;
begin
    if (cxGrid1TableView1.Controller.SelectedRecordCount < 0) then
        EXIT;

    for x := cxGrid1TableView1.Controller.SelectedRecordCount - 1 downto 0 do
        cxGrid1TableView1.DataController.DeleteRecord(cxGrid1TableView1.Controller.SelectedRecords[x]
            .RecordIndex);
    ChangeData();

    cxGrid1TableView1.DataController.ClearSelection;
end;

procedure TfrmImportDefEditor.pmnuAppendClick(Sender: TObject);
var
    xIndex: integer;
begin
    xIndex := self.AddLine('');
    cxGrid1TableView1.DataController.FocusedRecordIndex := xIndex;
end;

procedure TfrmImportDefEditor.pmnuMatchTargetClick(Sender: TObject);
var
    xTargetName: string;
    x, xSourceListIndex: integer;
begin
    for x := 0 to cxGrid1TableView1.DataController.RecordCount - 1 do
    begin
        xTargetName := cxGrid1TableView1.DataController.Values[x, INT_COLUMN_INDEX_TARGETCOLUMN];
        xSourceListIndex := cxEditRepository1ComboBoxItem1.Properties.Items.IndexOf(xTargetName);
        if (xSourceListIndex = -1) then
            CONTINUE;

        cxGrid1TableView1.DataController.Values[x, INT_COLUMN_INDEX_SOURCECOLUMN] := xTargetName;
    end;
end;

class function TfrmImportDefEditor.AskForNewDefName(var vNewDefName: string): boolean;
var
    xNames: TStringArray;
begin
    result := false;

    xNames := TImportDefDataAdaptor.ReadAllDefNames(INT_DEF_MODE_ALL);
    if not TDialogUtils.AskForNewName(vNewDefName, TLanguageString.Read('Enter new {0} name:',
        'Bitte den neuen {0}-Namen eingeben:', [TLanguageString.Read('Import Definition Name',
        'Importdefinition-Name')]), TLanguageString.Read('New {0}', '{0}: Neu',
        [TLanguageString.Read('Import Definition Name', 'Importdefinition-Name')]),
        TLanguageString.Read('{0} already exists!', '{0} existiert bereits!'), xNames) then
        EXIT;

    if (vNewDefName = '') then
        EXIT;
    result := true;
end;

procedure TfrmImportDefEditor.LinkSourceNameToTargetName(aRec: TImportColDefRec;
    aAddMissingTargetNames: boolean);
var
    x, xIndex: integer;
begin
    xIndex := -1;
    for x := 0 to cxGrid1TableView1.DataController.RecordCount - 1 do
    begin
        if (cxGrid1TableView1.DataController.Values[x, INT_COLUMN_INDEX_TARGETCOLUMN] <> aRec.TargetCol) then
            CONTINUE;

        xIndex := x;
        BREAK;
    end;

    if (xIndex = -1) then
    begin
        if (not aAddMissingTargetNames) then
            Exit;
        xIndex := self.AddLine(aRec.TargetCol);
    end;

    cxGrid1TableView1.DataController.Values[xIndex, INT_COLUMN_INDEX_SOURCECOLUMN] := aRec.SourceCol;
    cxGrid1TableView1.DataController.Values[xIndex, INT_COLUMN_INDEX_SOURCEREQUIRED] :=
        not aRec.SourceNotRequired;
    cxGrid1TableView1.DataController.Values[xIndex, INT_COLUMN_INDEX_SOURCEDEFAULT] := aRec.SourceDefault;
end;

function TfrmImportDefEditor.FindTargetNameInRecArray(const aTargetName: string;
    aDefRecArray: TImportColDefRecArray): boolean;
var
    x: integer;
begin
    result := false;
    for x := 0 to high(aDefRecArray) do
    begin
        if (aDefRecArray[x].TargetCol <> aTargetName) then
            CONTINUE;

        result := true;
        EXIT;
    end;
end;

function TfrmImportDefEditor.GUIToColDefArray: TImportColDefRecArray;
var
    xTargetName, xSourceName: string;
    xCount: integer;
    x: integer;
begin
    xCount := 0;
    for x := 0 to cxGrid1TableView1.DataController.RecordCount - 1 do
    begin

        if (cxGrid1TableView1.DataController.Values[x, INT_COLUMN_INDEX_SOURCECOLUMN] = Null) then
            CONTINUE;
        if (cxGrid1TableView1.DataController.Values[x, INT_COLUMN_INDEX_TARGETCOLUMN] = Null) then
            CONTINUE;

        xSourceName := cxGrid1TableView1.DataController.Values[x, INT_COLUMN_INDEX_SOURCECOLUMN];
        xTargetName := cxGrid1TableView1.DataController.Values[x, INT_COLUMN_INDEX_TARGETCOLUMN];

        if FindTargetNameInRecArray(xTargetName, result) then
            CONTINUE;

        Inc(xCount);
        SetLength(result, xCount);
        result[xCount - 1].TargetCol := xTargetName;
        result[xCount - 1].SourceCol := Trim(xSourceName);
        result[xCount - 1].SourceNotRequired := not cxGrid1TableView1.DataController.Values
            [x, INT_COLUMN_INDEX_SOURCEREQUIRED];
        result[xCount - 1].SourceDefault := cxGrid1TableView1.DataController.Values
            [x, INT_COLUMN_INDEX_SOURCEDEFAULT];
    end;
end;

function TfrmImportDefEditor.AddLine(const aTargetColName: string): integer;
begin
    result := cxGrid1TableView1.DataController.AppendRecord;
    cxGrid1TableView1.DataController.Values[result, INT_COLUMN_INDEX_TARGETCOLUMN] := aTargetColName;
    cxGrid1TableView1.DataController.Values[result, INT_COLUMN_INDEX_SOURCEREQUIRED] := true;
end;


end.
