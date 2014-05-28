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
  03.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  13.04.10 wl                               TN5044   uses FileUtilities
  10.04.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit SeqenceImport1;


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
    Spin,
    GeneralTypes,
    Generics.Collections;

type
    TSeqenceImportPage1 = class(TWizardPage)
    private
        FActiveControl: TWinControl;
        FRadioButton1: TRadioButton;
        FRadioButton2: TRadioButton;
        FSpinEdit1: TSpinEdit;
        FComboBox1: TComboBox;
        FLabel1: TLabel;
        FLabel2: TLabel;
        FLabel3: TLabel;
        FStaticText1: TStaticText;
        FFileName: string;
        FLines: TList<string>;
        FSetupDone: boolean;
        procedure RadioButtonClick(aSender: TObject);
        function GetConstSubstWidth: boolean;
        function GetDelimiter: char;
        function GetSubstWidth: integer;
    public
        // constructor
        constructor Create(aOwner: TComponent; aFileName: string); reintroduce;
        // methods derived from TWizardPage
        function GetHeaderTitle: string; override;
        function GetHeaderText(): TStringArray; override;
        function CheckBeforeNext: boolean; override;
        function GetActiveControl: TWinControl; override;
        procedure SetUp(aPreviousPage: TWizardPage); override;

        property FileName: string read FFileName;
        property ConstSubstWidth: boolean read GetConstSubstWidth;
        property SubstWidth: integer read GetSubstWidth;
        property Delimiter: char read GetDelimiter;
        property Lines: TList<string>read FLines;
    end;


implementation


uses
    CommonTypes,
    AppSettings,
    FileUtilities;

constructor TSeqenceImportPage1.Create(aOwner: TComponent; aFileName: string);
begin
    inherited Create(aOwner);

    FFileName := aFileName;

    FLabel3 := AddLabel(TLanguageString.Read('Import File:', 'Importierte Datei:'), INT_WIZARD_LEFT_1);
    FStaticText1 := AddStaticText(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    FStaticText1.Caption := aFileName;
    AddDistance(wzdXLarge);

    FRadioButton1 := AddRadioButton(TLanguageString.Read('constant number of letters for each substance',
        'Konstante Anzahl Buchstaben für jede Substanz'), INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1, true);
    AddDistance(wzdLarge);

    FLabel1 := AddLabel(TLanguageString.Read('Name length:', 'Länge der Bezeichnungen:'), INT_WIZARD_LEFT_2);
    FSpinEdit1 := AddSpinEdit(INT_WIZARD_LEFT_EDIT, 100);
    AddDistance(wzdXLarge);

    FRadioButton2 := AddRadioButton(TLanguageString.Read('Substances separated by a character',
        'Substanzen werden mit Trennzeichen getrennt'), INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1, true);
    AddDistance(wzdLarge);

    FLabel2 := AddLabel(TLanguageString.Read('Delimiter:', 'Trennzeichen:'), INT_WIZARD_LEFT_2);
    FComboBox1 := AddComboBox(INT_WIZARD_LEFT_EDIT, 100);

    FRadioButton1.OnClick := RadioButtonClick;
    FRadioButton2.OnClick := RadioButtonClick;

    FActiveControl := FRadioButton1;
    FLines := TList<string>.Create;
    FSetupDone := false;
end;

function TSeqenceImportPage1.GetHeaderTitle: string;
begin
    result := TLanguageString.Read('Sequence import wizard page 1 of 3',
        'Sequenzimport-Assistent Seite 1 von 3');
end;

function TSeqenceImportPage1.GetHeaderText(): TStringArray;
begin
    result := self.GetSingleTextAsHeader(TLanguageString.Read('You import text files as sequences',
        'Sie können Textdateien als Sequenzen importieren'));
end;

function TSeqenceImportPage1.CheckBeforeNext: boolean;
begin
    result := false;
    if (FRadioButton1.Checked) and (FSpinEdit1.Value < 1) then
    begin
        exit;
    end;

    // Do import from file
    FLines.Clear;
    try
        FLines.AddRange(TFileUtilities.TakeStringsFromFile(FFileName, 1, 2000));
    except
        on E: Exception do
        begin
            ShowMessage(E.Message);
            exit;
        end;
    end;

    if (FLines.Count < 1) then
    begin
        ShowMessage('No data in file');
        exit;
    end;

    result := true;
end;

function TSeqenceImportPage1.GetActiveControl: TWinControl;
begin
    result := FActiveControl;
end;

procedure TSeqenceImportPage1.SetUp(aPreviousPage: TWizardPage);
begin
    inherited;

    if (FSetupDone) then
        exit;

    FSpinEdit1.Value := 1;
    FComboBox1.Items.Add(',');
    FComboBox1.Items.Add(';');
    FComboBox1.Text := ';';
    RadioButtonClick(nil);
    FSetupDone := true;
end;

procedure TSeqenceImportPage1.RadioButtonClick(aSender: TObject);
begin
    FSpinEdit1.Enabled := FRadioButton1.Checked;
    FComboBox1.Enabled := FRadioButton2.Checked;
end;

function TSeqenceImportPage1.GetConstSubstWidth: boolean;
begin
    result := FRadioButton1.Checked;
end;

function TSeqenceImportPage1.GetDelimiter: char;
begin
    result := FComboBox1.Text[1];
end;

function TSeqenceImportPage1.GetSubstWidth: integer;
begin
    result := FSpinEdit1.Value;
end;


end.
