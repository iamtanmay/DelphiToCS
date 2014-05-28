unit ImportSQLEditor;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  01.07.11 wl                               TN5619   aus ImportFileDefEditor herausgelöst
  ---------------------------------------------------------------------------------------------------------------------- }


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
    ExtCtrls,
    StringLoader;

type
    TSQLEditorStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmSQLEditor = class(TForm)
        Bevel4: TBevel;
        pnlSQLMiddle: TPanel;
        Splitter2: TSplitter;
        mmoSQLBuilder: TMemo;
        pnlSQLTools: TPanel;
        lblSQLFieldName: TLabel;
        lblSQLTablePath: TLabel;
        btnInsertTableName: TButton;
        cmbSQLFieldName: TComboBox;
        btnSQLInsertFiledName: TButton;
        edSQLTablePath: TEdit;
        btnSQLSelectTablePath: TButton;
        pnSQLBottom: TPanel;
        btnSQLOK: TButton;
        Button1: TButton;
        procedure pnlSQLToolsResize(Sender: TObject);
        procedure btnInsertTableNameClick(Sender: TObject);
        procedure btnSQLInsertFiledNameClick(Sender: TObject);
        procedure btnSQLSelectTablePathClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
        fStringLoader: TSQLEditorStringLoader;
        fUserName, fPassword: string;
        function GetNewPathName(aOldPathName: string; var aNewPathName: string): boolean;
    public
        class function EditAnySQL(const aSQLText, aUserName, aPassword: string): string;
    end;


implementation


{$R *.dfm}

uses
    ControlUtils,
    GeneralTypes,
    ImportClasses,
    SQLParser,
    SelTablePath;

{ TSQLEditorStringLoader }

procedure TSQLEditorStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
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
    AddSingle(9350, 'Table Path', 'Tabellenpfad');
    AddSingle(9360, 'Field Name', 'Feldname');
    AddSingle(9370, 'Insert', 'Einfügen');
    AddDouble(9380, 'Optimize Column Size', 'Use Header;Ignore Header', 'Spaltebreite optimieren',
        'Header verwenden;Header ignorieren');
    AddSingle(14200, 'Records', 'Datensätze');
    AddSingle(14210, 'Fields', 'Felder');
end;

{ TfrmSQLEditor }

procedure TfrmSQLEditor.btnInsertTableNameClick(Sender: TObject);

var
    xTablePath: string;
begin
    xTablePath := edSQLTablePath.Text;
    if xTablePath = '' then
        Exit;
    xTablePath := Format('"%s"', [xTablePath]);
    TControlUtils.InsertIntoControlAtCurrentCursorPos(mmoSQLBuilder, xTablePath);
end;

procedure TfrmSQLEditor.btnSQLInsertFiledNameClick(Sender: TObject);
begin
    TControlUtils.InsertIntoControlAtCurrentCursorPos(self.mmoSQLBuilder, cmbSQLFieldName.Text);
end;

procedure TfrmSQLEditor.btnSQLSelectTablePathClick(Sender: TObject);
var
    xNewPathName: string;
    xNames: TStringArray;
begin
    if not GetNewPathName(edSQLTablePath.Text, xNewPathName) then
        Exit;
    edSQLTablePath.Text := xNewPathName;
    xNames := TImports.GetAllFieldNamesOfTable(dbStandard, xNewPathName, '', '', true, fUserName,
        fPassword, '');
    TControlUtils.AddValuesToComboBox(xNames, self.cmbSQLFieldName, true);
end;

class function TfrmSQLEditor.EditAnySQL(const aSQLText, aUserName, aPassword: string): string;
var
    xForm: TfrmSQLEditor;
    xNewSQL: string;
begin
    xNewSQL := aSQLText;
    xForm := TfrmSQLEditor.Create(nil);
    try
        xForm.mmoSQLBuilder.Text := aSQLText;
        xForm.fUserName := aUserName;
        xForm.fPassword := aPassword;
        if (xForm.ShowModal = mrOK) then
            xNewSQL := xForm.mmoSQLBuilder.Text;
    finally
        FreeAndNil(xForm);
    end;
    EXIT(xNewSQL);
end;

procedure TfrmSQLEditor.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TSQLEditorStringLoader.Create;
    fStringLoader.LoadLanguage(self);
end;

procedure TfrmSQLEditor.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

function TfrmSQLEditor.GetNewPathName(aOldPathName: string; var aNewPathName: string): boolean;
var
    xFilter, xNewPathName: string;
begin
    result := false;
    xFilter := TLanguageString.Read('Paradox files(*.db)|*.db|', 'Paradox-Dateien(*.db)|*.db|');
    xNewPathName := aOldPathName;
    if TFrmSelTablePath.SelectTablePathModal(xNewPathName) = mrCancel then
        Exit;

    if xNewPathName = aOldPathName then
        Exit;
    aNewPathName := xNewPathName;
    result := true;
end;

procedure TfrmSQLEditor.pnlSQLToolsResize(Sender: TObject);
const
    INT_PANEL_WIDTH_MIN_SQLTOOLS = 120;
begin
    if pnlSQLTools.Width < INT_PANEL_WIDTH_MIN_SQLTOOLS then
        pnlSQLTools.Width := INT_PANEL_WIDTH_MIN_SQLTOOLS;
end;


end.
