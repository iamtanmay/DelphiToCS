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
  10.04.13 wl                               TN6045   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit SeqenceImport2;


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
    GeneralTypes,
    Generics.Collections;

type
    TSequenceGrid = class(TStringGrid)
    private
        procedure FillCell(aCol, aRow: integer; aText: string);
        procedure ImportDataConstWidth(aLineText: string; aRow: integer; aSubstWidth: integer);
        procedure ImportDataWithDelimiter(aLineText: string; aRow: integer; aDelimiter: char);
        procedure Clear;
    public
        constructor Create(AOwner: TComponent); override;
        procedure ImportData(aLines: TList<string>; aConstSubstWidth: boolean; aSubstWidth: integer;
            aDelimiter: char);
    end;

    TSeqenceImportPage2 = class(TWizardPage)
    private
        FActiveControl: TWinControl;
        FEdit1: TEdit;
        FFileName: string;
        FStaticText1: TStaticText;
        FPreviewGrid: TSequenceGrid;
        function GetVolume: double;
    public
        // constructor
        constructor Create(aOwner: TComponent); reintroduce;
        // methods derived from TWizardPage
        function GetHeaderTitle: string; override;
        function GetHeaderText(): TStringArray; override;
        function CheckBeforeNext: boolean; override;
        function GetActiveControl: TWinControl; override;
        procedure SetUp(aPreviousPage: TWizardPage); override;
        //
        property FileName: string read FFileName;
        property PreviewGrid: TSequenceGrid read FPreviewGrid;
        property Volume: double read GetVolume;
    end;


implementation


uses
    DialogUtils,
    AppSettings,
    SeqenceImport1,
    Utility2;

{ TSequenceGrid }

constructor TSequenceGrid.Create(AOwner: TComponent);
begin
    inherited;

end;

procedure TSequenceGrid.FillCell(aCol, aRow: integer; aText: string);
begin
    if (self.ColCount <= aCol) then
        self.ColCount := aCol + 1;
    self.Cells[aCol, aRow] := aText;
end;

procedure TSequenceGrid.Clear;
var
    xRow, xCol: integer;
begin
    for xRow := 0 to self.RowCount - 1 do
        for xCol := 0 to self.ColCount - 1 do
            self.Cells[xCol, xRow] := '';

end;

procedure TSequenceGrid.ImportDataConstWidth(aLineText: string; aRow: integer; aSubstWidth: integer);
var
    x, xLength: integer;
begin
    xLength := Length(aLineText);
    for x := 1 to (xLength div aSubstWidth) do
    begin
        FillCell(x, aRow, Copy(aLineText, 1 + ((x - 1) * aSubstWidth), aSubstWidth));
    end;
end;

procedure TSequenceGrid.ImportDataWithDelimiter(aLineText: string; aRow: integer; aDelimiter: char);
var
    xActPos, xCol: integer;
    xSubStr: string;
begin
    xActPos := 1;
    xCol := 1;
    repeat
        xSubStr := gmReadSubString(xActPos, aLineText, aDelimiter);
        if (xSubStr <> '') then
        begin
            FillCell(xCol, aRow, xSubStr);
            inc(xCol);
        end;
    until (xSubStr = '');
end;

procedure TSequenceGrid.ImportData(aLines: TList<string>; aConstSubstWidth: boolean; aSubstWidth: integer;
    aDelimiter: char);
var
    x, xRow: integer;
begin
    self.Clear;
    self.RowCount := aLines.Count + 1;
    self.ColCount := 2;

    for x := 0 to aLines.Count - 1 do
    begin

        xRow := x + 1;
        self.Cells[0, xRow] := IntToStr(xRow);

        if (aConstSubstWidth) then
            ImportDataConstWidth(aLines[x], xRow, aSubstWidth)
        else
            ImportDataWithDelimiter(aLines[x], xRow, aDelimiter);
    end;

    for x := 1 to self.ColCount - 1 do
        self.Cells[x, 0] := 'Step ' + IntToStr(x);
end;

constructor TSeqenceImportPage2.Create(aOwner: TComponent);
begin
    inherited Create(aOwner);

    AddLabel(TLanguageString.Read('Import File:', 'Importierte Datei:'), INT_WIZARD_LEFT_1);
    FStaticText1 := AddStaticText(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    AddDistance(wzdLarge);

    AddLabel(TLanguageString.Read('Sample volume [uL]:', 'Probenvolumen [uL]:'), INT_WIZARD_LEFT_1);
    FEdit1 := AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    AddDistance(wzdLarge);

    AddLabel(TLanguageString.Read('Preview:', 'Vorschau:'), INT_WIZARD_LEFT_1);
    AddDistance(wzdSmall);
    FPreviewGrid := TSequenceGrid.Create(self);
    AddAnyControl(FPreviewGrid, INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1, 100);
    FPreviewGrid.DefaultRowHeight := 18;
    FPreviewGrid.ColWidths[0] := 16;

    FActiveControl := FEdit1;
end;

function TSeqenceImportPage2.GetHeaderTitle: string;
begin
    result := TLanguageString.Read('Sequence import wizard page 2 of 3',
        'Sequenzimport-Assistent Seite 2 von 3');
end;

function TSeqenceImportPage2.GetHeaderText(): TStringArray;
begin
    result := self.GetSingleTextAsHeader(TLanguageString.Read('You import text files as sequences',
        'Sie können Textdateien als Sequenzen importieren'));
end;

function TSeqenceImportPage2.CheckBeforeNext: boolean;
begin
    result := false;

    // check if sequence name is empty
    if (GetVolume = 0) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Please enter a volume in uL!',
            'Bitte geben Sie ein Volumen in uL ein!'), 'Info', 64);
        FActiveControl := FEdit1;
        exit;
    end;

    result := true;
end;

function TSeqenceImportPage2.GetActiveControl: TWinControl;
begin
    result := FActiveControl;
end;

procedure TSeqenceImportPage2.SetUp(aPreviousPage: TWizardPage);
begin
    inherited;

    if (aPreviousPage is TSeqenceImportPage1) then
    begin
        FFileName := (aPreviousPage as TSeqenceImportPage1).FileName;
        FStaticText1.Caption := FFileName;

        // import data to string grid
        FPreviewGrid.ImportData((aPreviousPage as TSeqenceImportPage1).Lines,
            (aPreviousPage as TSeqenceImportPage1).ConstSubstWidth, (aPreviousPage as TSeqenceImportPage1)
            .SubstWidth, (aPreviousPage as TSeqenceImportPage1).Delimiter);

    end;
end;

function TSeqenceImportPage2.GetVolume: double;
var
    xErr: integer;
begin
    Val(FEdit1.Text, result, xErr);
end;


end.
