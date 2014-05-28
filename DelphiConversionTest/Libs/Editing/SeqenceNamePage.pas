{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  24.05.04 wl                               TN1945   initial version
  04.04.08 wl                               TN4058    uses geändert
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  05.11.12 wl  CheckBeforeNext              TN6006   die Settings werden jetzt wirklich geschrieben (und nicht beim Speichern der Liquids)
  -------------------------------------------------------------------------------------------------- }

unit SeqenceNamePage;


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
    Wizard,
    StdCtrls,
    Grids,
    Spin,
    GeneralTypes;

type
    TSeqenceNamePage = class(TWizardPage)
    private
        FActiveControl: TWinControl;
        FEdit1: TEdit;
        FSpinCols: TSpinEdit;
        FSpinRows: TSpinEdit;
        function GetSeqName: string;
        function GetCols: integer;
        function GetRows: integer;
    public
        // constructor
        constructor Create(aOwner: TComponent); reintroduce;
        // methods derived from TWizardPage
        function GetHeaderTitle: string; override;
        function GetHeaderText(): TStringArray; override;
        function CheckBeforeNext: boolean; override;
        function GetActiveControl: TWinControl; override;
        procedure SetUp(aPreviousPage: TWizardPage); override;

        property SeqName: string read GetSeqName;
        property Rows: integer read GetRows;
        property Cols: integer read GetCols;
    end;


implementation


uses
    DialogUtils,
    CommonTypes,
    AppSettings,
    SeqenceImport2,
    SeqenceDataAdaptor;

constructor TSeqenceNamePage.Create(aOwner: TComponent);
var
    xIniAccess: IWinLissyIniAccess;
begin
    inherited Create(aOwner);

    AddLabel(TLanguageString.Read('Name for the new sequence:', 'Name der neuen Sequenz:'),
        INT_WIZARD_LEFT_1);
    FEdit1 := AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    AddDistance(wzdXLarge);

    AddLabel(TLanguageString.Read('Rows:', 'Reihen:'), INT_WIZARD_LEFT_1);
    FSpinRows := AddSpinEdit(INT_WIZARD_LEFT_EDIT, 100);
    AddDistance(wzdXLarge);

    AddLabel(TLanguageString.Read('Columns:', 'Spalten:'), INT_WIZARD_LEFT_1);
    FSpinCols := AddSpinEdit(INT_WIZARD_LEFT_EDIT, 100);

    xIniAccess := gCommonDll.CreateAppIni;
    FSpinCols.Value := xIniAccess.ReadInteger(STR_ISEC_LASTRBLOCK, 'Columns');
    FSpinRows.Value := xIniAccess.ReadInteger(STR_ISEC_LASTRBLOCK, 'Rows');

    FActiveControl := FEdit1;
end;

function TSeqenceNamePage.GetHeaderTitle: string;
begin
    result := TLanguageString.Read('Create new sequence', 'Neue Pipettier-Sequenz erstellen');
end;

function TSeqenceNamePage.GetHeaderText(): TStringArray;
begin
    result := self.GetSingleTextAsHeader(TLanguageString.Read('You have to define a new sequence name',
        'Sie müssen den Namen der Sequenz bestimmen'));
end;

function TSeqenceNamePage.CheckBeforeNext: boolean;
var
    xIniAccess: IWinLissyIniAccess;
begin
    result := false;

    // check if sequence name is empty
    if (FEdit1.Text = '') then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Please enter a sequence name!',
            'Bitte geben Sie einen Namen ein!'), 'Info', 64);
        FActiveControl := FEdit1;
        exit;
    end;

    // check if sequence name exists
    if TSeqenceDataAdaptor.NameExists(FEdit1.Text) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('This sequence name exists. Please try another name!',
            'Dieser Squenzname existiert schon. Bitte wählen Sie einen anderen Namen!'), 'Info', 64);
        FActiveControl := FEdit1;
        exit;
    end;

    // store row/column values
    xIniAccess := gCommonDll.CreateAppIni;
    xIniAccess.WriteInteger(STR_ISEC_LASTRBLOCK, 'Columns', FSpinCols.Value);
    xIniAccess.WriteInteger(STR_ISEC_LASTRBLOCK, 'Rows', FSpinRows.Value);
    xIniAccess.WriteSectionFromCache(STR_ISEC_LASTRBLOCK);

    result := true;
end;

function TSeqenceNamePage.GetActiveControl: TWinControl;
begin
    result := FActiveControl;
end;

procedure TSeqenceNamePage.SetUp(aPreviousPage: TWizardPage);
var
    xExt, xName: string;
begin
    inherited;

    if (aPreviousPage is TSeqenceImportPage2) then
    begin
        xName := ExtractFileName((aPreviousPage as TSeqenceImportPage2).FileName);
        xExt := ExtractFileExt(xName);
        FEdit1.Text := Copy(xName, 1, Length(xName) - Length(xExt));
    end;
end;

function TSeqenceNamePage.GetSeqName: string;
begin
    result := FEdit1.Text;
end;

function TSeqenceNamePage.GetCols: integer;
begin
    result := FSpinCols.Value;
end;

function TSeqenceNamePage.GetRows: integer;
begin
    result := FSpinRows.Value;
end;


end.
