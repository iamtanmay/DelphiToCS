unit Import;
{ --------------------------------------------------------------------------------------------------
  Ebene 2 (Sam-Interface)
  --------------------------------------------------------------------------------------------------
  Import-Fenster, angepaßt an die Sampler-Software
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure   Änderung / Neuerung
  -------- --  -------------------  ---------------------------------------------------------------
  15.11.99 wl                       neu: Bestandteil der WinLissy-Software
  16.11.99 wl                       Strings in Ressourcen
  Lesen und Schreiben in Ini-Datei --> SamStart
  29.11.99 wl  bbOKClick            gmImport: aufgerufene Funktion steht qryTools, nicht in SamUtil.dll
  16.12.99 wl  bbOKClick            gmImport wird mit string statt mit PChar aufgerufen
  12.03.03 wl                       TN1293.5 uses posTools
  01.11.05 wl                       TN2541.0 Resourcen 2000,2010 statt 29540,29550
  24.11.05 pk  FormCreate           TN2805  use TAppSettings to get Alias path
  25.08.09 wl                       TN4611   Komplettes Redesign mit Import-Definitionen
  25.08.09 wl  fStringLoader        TN4702   fStringLoader lädt Strings für Dialog-Elemente
  26.10.09 wl  FormCreate           TN4831   IConfigurationSet replaces ILocalIniFile
  14.11.09 wl                       TN4869   FreeAndNil statt TGarbage.FreeAndNil
  20.05.10 wl                       TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                       TN5160   Position = poScreenCenter
  22.07.10 wl                       TN5169.1 Fehler korrigiert, der sich mit TN5161 eingeschlichen hatte
  22.07.10 wl                       TN5169.2 Fenster um Sortierreihenfolge erweitert
  23.02.11 wl                       TN5486   Mit icon
  30.06.11 wl                       TN5620   Button Definition entfernt
  -------------------------------------------------------------------------------------------------- }


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
    StdCtrls,
    Buttons,
    ExtCtrls,
    StringLoader;

type
    TImportExecuteStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TImportForm = class(TForm)
        OpenDialog2: TOpenDialog;
        cbImportDefinition: TComboBox;
        edImportFile: TEdit;
        edFilter: TEdit;
        BitBtn2: TBitBtn;
        Shape2: TShape;
        Bevel2: TBevel;
        btnOK: TButton;
        btnCancel: TButton;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        edOrder: TEdit;
        Label4: TLabel;
        imgActionImage: TImage;
        procedure BitBtn2Click(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure cbImportDefinitionChange(Sender: TObject);
        procedure btnDefineClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        fStringLoader: TImportExecuteStringLoader;
        procedure RefreshImportDefs;
        procedure RefreshImportDefDetails;
    end;


implementation


{$R *.DFM}

uses
    DialogUtils,
    GeneralTypes,
    CommonTypes,
    AppSettings,
    ImportDataAdaptor,
    MethodImport,
    ConfigurationFile,
    ControlUtils;

{ TImportExecuteStringLoader }

procedure TImportExecuteStringLoader.AddAllItems;
begin
    AddSingle(10, 'Import', 'Importieren');
    AddSingle(20, 'Cancel', 'Abbrechen');
    AddSingle(29500, 'Execute table import', 'Tabellen-Import durchführen');
    AddSingle(29510, 'Import definition:', 'Import-Definition:');
    AddSingle(29520, 'Source file:', 'Quell-Datei:');
    AddSingle(29530, 'Edit Definitions', 'Definitionen bearbeiten');
    AddSingle(19540, 'Sorting order:', 'Sortierreihenfolge:');
end;

{ TImportForm }

procedure TImportForm.FormCreate(Sender: TObject);
var
    xLocalIniFile: IConfigurationSet;
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TImportExecuteStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    // Text lesen aus appdata.tmp
    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(true);
    try
        self.cbImportDefinition.Text := xLocalIniFile.ReadString('SimpleImport', 'LastImportDef', '');
        self.edFilter.Text := xLocalIniFile.ReadString('SimpleImport', 'LastImportFilter', '');
        self.edOrder.Text := xLocalIniFile.ReadString('SimpleImport', 'LastImportOrder', '');
        self.edImportFile.Text := xLocalIniFile.ReadString('SimpleImport', 'LastImportFile', '');
    finally
        xLocalIniFile.Close;
    end;

    self.RefreshImportDefs;
end;

procedure TImportForm.FormDestroy(Sender: TObject);
begin
    FreeAndNil(fStringLoader);
end;

procedure TImportForm.RefreshImportDefs;
var
    xDA: TImportDefDataAdaptor;
    xDefinitions: TImportDefRecArray;
    x: integer;
begin
    xDA := TImportDefDataAdaptor.Create;
    try
        xDefinitions := xDA.ReadAllDefsOfMode(INT_DEF_MODE_TABLEIMPORT);
    finally
        FreeAndNil(xDA);
    end;

    self.cbImportDefinition.Items.Clear;
    for x := 0 to high(xDefinitions) do
    begin
        self.cbImportDefinition.Items.Add(xDefinitions[x].Name);
    end;
end;

procedure TImportForm.BitBtn2Click(Sender: TObject);
begin
    if OpenDialog2.Execute() then
        edImportFile.Text := OpenDialog2.FileName;
end;

procedure TImportForm.btnOKClick(Sender: TObject);
var
    xLocalIniFile: IConfigurationSet;
    xNumImportedRows: integer;
begin
    xNumImportedRows := TGeneralImport.ImportUsingImportDef(self.cbImportDefinition.Text, self.edFilter.Text,
        self.edOrder.Text, self.edImportFile.Text);
    TDialogUtils.MessageBox(TLanguageString.Read('Number of rows imported: {0}',
        'Anzahl der importierten Reihen: {0}', [xNumImportedRows]), TLanguageString.Read('Import Successful',
        'Import erfolgreich'), MB_OK);

    xLocalIniFile := TConfigurationFile.Create(TAppsettings.LocalIniFileName);
    xLocalIniFile.Open(false);
    try
        // Text lesen aus appdata.tmp
        xLocalIniFile.WriteString('SimpleImport', 'LastImportDef', self.cbImportDefinition.Text);
        xLocalIniFile.WriteString('SimpleImport', 'LastImportFilter', self.edFilter.Text);
        xLocalIniFile.WriteString('SimpleImport', 'LastImportOrder', self.edOrder.Text);
        xLocalIniFile.WriteString('SimpleImport', 'LastImportFile', self.edImportFile.Text);
    finally
        xLocalIniFile.Close;
    end;

    self.Close;
end;

procedure TImportForm.cbImportDefinitionChange(Sender: TObject);
begin
    self.RefreshImportDefDetails;
end;

procedure TImportForm.btnDefineClick(Sender: TObject);
begin
//    TFrmImportMain.ModalOpen(immImport);
//    self.RefreshImportDefs;
//    self.RefreshImportDefDetails;
end;

procedure TImportForm.RefreshImportDefDetails;
var
    xFileName, xExtension: string;
    xImportDefRec: TImportDefRec;
begin
    xImportDefRec := TImportDefDataAdaptor.QuickReadDef(self.cbImportDefinition.Text, true, true);
    if not xImportDefRec.Valid then
        raise Exception.Create(TLanguageString.Read('No such Import Definition [{0}]',
            'Unbekannte Importdefinition [{0}]', [self.cbImportDefinition.Text]));

    xFileName := xImportDefRec.SourceFileDef.PathName;
    self.edImportFile.Text := xFileName;
    OpenDialog2.InitialDir := ExtractFilePath(xFileName);
    xExtension := ExtractFileExt(xFileName);
    OpenDialog2.Filter := TLanguageString.Read('Import files (*{0})|*{0}|All files (*.*)|*.*',
        'Importdateien (*{0})|*{0}|Alle Dateien (*.*)|*.*', [xExtension]);
end;


end.
