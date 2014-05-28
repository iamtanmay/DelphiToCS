{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : GUI for complete method import
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------- ----------------------------------------------
  18.08.05 wl                               TN2541.0  initial version
  31.08.05 wl                               TN2541.0  Import ist jetzt möglich
  24.11.05 pk  btnFinishClick               TN2765    call openeditform using TViewItemsWorkflow
  21.12.05 wl                               TN2541.0  Import wird als Dll-Funktion eingebunden
  03.01.06 wl                               TN2541.0  benutzt TDatasetViewItem.AskNewName
  04.05.06 wl                               TN2541.0  neu: SchedMin, SchedMax, ResID
  16.01.09 wl                               TN4362   an Änderungen in TViewItem angepasst
  27.08.09 pk  btnCreateImportFileClick     TN4753   Commented out for now. reference to RunCommon and DLLCall removed
  26.10.09 wl  ReadFromIni                  TN4831   IConfigurationSet replaces TIniFile
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                               TN5044   uses FileUtilities
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  18.06.10 pk                               TN5152.1 TTable component removed and replaced by TDataProvider
  21.06.10 wl                               TN5160   Position = poScreenCenter
  10.12.12 wl                               TN6045   verwendet DesignerMethodViewItem
  13.03.13 wl                               TN5960   TViewItemsWorkflow.Instance statt statischer Methode
  -------------------------------------------------------------------------------------------------- }

unit CompleteMethodImportForm;


interface


uses
    Forms,
    Classes,
    Controls,
    StdCtrls,
    Buttons,
    cxStyles,
    cxCustomData,
    cxGraphics,
    cxFilter,
    cxData,
    cxDataStorage,
    cxEdit,
    ExtCtrls,
    cxGridLevel,
    cxClasses,
    cxControls,
    cxGridCustomView,
    cxGridCustomTableView,
    cxGridTableView,
    cxGridDBTableView,
    cxGrid,
    cxLookAndFeels,
    cxLookAndFeelPainters,
    cxDBData,
    DB,
    CommonTypes,
    DataProvider;

type
    TfrmCompleteMethodImport = class(TForm)
        cxGrid1DBTableView1: TcxGridDBTableView;
        cxGrid1Level1: TcxGridLevel;
        cxGrid1: TcxGrid;
        Panel1: TPanel;
        Label3: TLabel;
        cbImportFile: TComboBox;
        btnFinish: TButton;
        btnBrowse: TBitBtn;
        DataSource1: TDataSource;
        cxGrid1DBTableView1DBColumn1: TcxGridDBColumn;
        cxGrid1DBTableView1DBColumn2: TcxGridDBColumn;
        cxGrid1DBTableView1DBColumn3: TcxGridDBColumn;
        Label1: TLabel;
        StaticText2: TStaticText;
        cxGrid1DBTableView1DBColumn4: TcxGridDBColumn;
        btnCreateImportFile: TButton;
        cxGrid1DBTableView1DBColumn5: TcxGridDBColumn;
        cxGrid1DBTableView1DBColumn6: TcxGridDBColumn;
        cxGrid1DBTableView1DBColumn7: TcxGridDBColumn;
        procedure btnBrowseClick(Sender: TObject);
        procedure btnFinishClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure btnCreateImportFileClick(Sender: TObject);
        procedure cbImportFileChange(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private const
        STR_INI_SECTION_COMPLETEMETHODIMPORT = 'CompleteMethodImport';
        STR_INI_IDENT_INITIALDIR = 'InitialDirectory';
        STR_INI_IDENT_FILE = 'File';
        STR_INI_IDENT_LAYOUT = 'Layout';
        INT_INI_MAX_STORED_FILES = 8;
    private
        fDataProvider: TDataProvider;
        fImportButtonData: TDLLMenuItem;
        procedure InsertTextAsFirstItem();
        class procedure ReadFromIni(aFileNames: TStrings);
        class procedure WriteToIni(aFileNames: TStrings);
    public
        class procedure ShowAndImport();
    end;


implementation


uses
    Dialogs,
    SysUtils,
    AppSettings,
    CompleteMethodImport,
    DesignerMethodViewItem,
    FileUtilities,
    ViewItem,
    ViewItemsWorkflow,
    ConfigurationFile,
    ControlUtils,
    DataProviderFactory,
    DatasetDataProvider;

{$R *.DFM}

procedure TfrmCompleteMethodImport.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fDataProvider := TDataProviderFactory.Instance.CreateDataProvider();
    fDataProvider.SelectAndOpen('SELECT * FROM METHODIMPORTDEFS', false);
    ASSERT(fDataProvider is TDatasetDataProvider);
    self.DataSource1.DataSet := (fDataProvider as TDatasetDataProvider).GetRealDataset;
end;

procedure TfrmCompleteMethodImport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    self.DataSource1.DataSet.Close();
    self.DataSource1.DataSet := nil;
    FreeAndNil(fDataProvider);
end;

procedure TfrmCompleteMethodImport.btnBrowseClick(Sender: TObject);
var
    xOpenDialog: TOpenDialog;
begin
    xOpenDialog := TOpenDialog.Create(self);
    xOpenDialog.InitialDir := ExtractFileDir(cbImportFile.Text);

    if not xOpenDialog.Execute() then
        EXIT;

    cbImportFile.Text := xOpenDialog.FileName;
    cbImportFileChange(nil);
    xOpenDialog.Free;
end;

class procedure TfrmCompleteMethodImport.ReadFromIni(aFileNames: TStrings);
var
    xIniFile: IConfigurationSet;
    x: integer;
    xText: string;
begin
    aFileNames.Clear;

    xIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xIniFile.Open(true);
    try
        for x := 1 to INT_INI_MAX_STORED_FILES do
        begin
            xText := xIniFile.ReadString(STR_INI_SECTION_COMPLETEMETHODIMPORT,
                STR_INI_IDENT_FILE + IntToStr(x), '');
            if (xText = '') then
                CONTINUE;
            aFileNames.Add(xText);
        end;
    finally
        xIniFile.Close;
    end;
end;

class procedure TfrmCompleteMethodImport.WriteToIni(aFileNames: TStrings);
var
    xIniFile: IConfigurationSet;
    x: integer;
begin
    xIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xIniFile.Open(false);
    try
        for x := 1 to INT_INI_MAX_STORED_FILES do
        begin
            if (x > aFileNames.Count) then
                BREAK;
            xIniFile.WriteString(STR_INI_SECTION_COMPLETEMETHODIMPORT, STR_INI_IDENT_FILE + IntToStr(x),
                aFileNames[x - 1]);
        end;
    finally
        xIniFile.Close;
    end;
end;

procedure TfrmCompleteMethodImport.InsertTextAsFirstItem();
var
    x: integer;
    xText: string;
begin
    xText := cbImportFile.Text;
    if (xText = '') then
        EXIT;

    // Testen, ob es den Eintrag schon gibt
    for x := 0 to cbImportFile.Items.Count - 1 do
    begin
        if (cbImportFile.Items[x] <> cbImportFile.Text) then
            CONTINUE;

        cbImportFile.Items.Delete(x); // alten Eintrag löschen
        BREAK;
    end;

    cbImportFile.Items.Insert(0, xText);
    cbImportFile.Text := xText;
    cbImportFileChange(nil);
end;

procedure TfrmCompleteMethodImport.btnFinishClick(Sender: TObject);
var
    xMethodName: string;
    xImport: TCompleteMethodImport;
    xViewItem: TDesignerMethodViewItem;
begin
    xMethodName := TFileUtilities.DeleteExtension(Copy(ExtractFileName(cbImportFile.Text), 1, 20));

    xViewItem := TDesignerMethodViewItem.Create('');
    try
        if not xViewItem.AskNewName(xMethodName) then
            EXIT;
    finally
        xViewItem.Free;
    end;

    StaticText2.Visible := false;
    self.InsertTextAsFirstItem();
    self.WriteToIni(cbImportFile.Items);

    xImport := TCompleteMethodImport.Create();
    try
        xImport.ImportMethod(cbImportFile.Text, xMethodName);
        TViewItemsWorkflow.Instance.OverviewFormsUpdateItems([ntMethod]);
        TViewItemsWorkflow.Instance.OpenEditForm(xMethodName, ntMethod);
        self.Close();
    except
        on E: Exception do
        begin
            StaticText2.Caption := E.Message;
            StaticText2.Visible := true;
        end;
    end;
    xImport.Free;
end;

procedure TfrmCompleteMethodImport.FormShow(Sender: TObject);
var
    xIniAccess: IWinlissyIniAccess;
begin
    self.ReadFromIni(cbImportFile.Items);

    xIniAccess := gCommonDll.CreateAppIni;
    fImportButtonData := xIniAccess.ReadDLLMenuItem('CompleteMethodImport', 'ImportButton');
    btnCreateImportFile.Visible := (fImportButtonData.DLLName <> '');
    btnCreateImportFile.Caption := fImportButtonData.MenuCaption;

end;

class procedure TfrmCompleteMethodImport.ShowAndImport;
var
    xInstance: TfrmCompleteMethodImport;
begin
    xInstance := TfrmCompleteMethodImport.Create(Application);
    xInstance.ShowModal;
    xInstance.Free;
end;

procedure TfrmCompleteMethodImport.btnCreateImportFileClick(Sender: TObject);
var
    xDllResult: string;
begin
    // 27.08.09 pk reference to dllcall removed
    ASSERT(false, 'Not implemented');
    // xDllResult := TDllCall.DirectExecute( fImportButtonData.DLLName, fImportButtonData.DLLFunction,
    // fImportButtonData.Parameter);
    cbImportFile.Text := xDllResult;
    cbImportFileChange(nil);
end;

procedure TfrmCompleteMethodImport.cbImportFileChange(Sender: TObject);
begin
    btnFinish.Enabled := FileExists(cbImportFile.Text);
    if btnFinish.Enabled then
        btnFinish.SetFocus;
end;


end.
