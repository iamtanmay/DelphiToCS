{ --------------------------------------------------------------------------------------------------
  EDITOR (Ebene 5)
  --------------------------------------------------------------------------------------------------
  Editor für Pipettiersequenzen im 96er-Raster oder Level-Übersicht
  --------------------------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    --------------------------------------------------------------
  31.05.00 wl                         als SeqRBEd in EDITOR
  05.07.00 wl                         FormStyle = fsMDIChild
  19.07.00 wl  Create                 Rows und Cols werden übergeben (sind variabel)
  19.07.00 wl  SpinEdit1Change        Unterstützung verschiedener Levels
  19.07.00 wl                         Dieser Editor ersetzt den normalen Sequence-Editor
  08.08.00 wl  FormShow               Das Rack-Format wird aus der Tabelle (Pos=0) oder der sampler.ini gelesen
  08.08.00 wl  FillBlock              Anzahl Rows/Columns ist veränderlich
  08.08.00 wl  UpdateValues           Das Rack-Format wird gespeichert (Pos=0; SUBSTID='Format: .x.')
  11.08.00 wl  spinEditLevelChange    neu: FCurrentLevel, um Fehler beim Speichern zu verhindern
  11.08.00 wl                         --> String in Ressourcen (52600ff)
  16.08.00 wl  rgView                 2.Ansichts-Modus: Level-Übersicht (ersetzt SeqEdit)
  16.08.00 wl  cbShowVol              Die Volumen können mit angezeigt werden
  17.08.00 wl  edVolume               Volumen kann als Float-Wert eingegeben werden
  24.08.00 wl  FillBlock              Die Spaltenbreite passt sich der zur Verfügung stehenden Fläche an
  01.09.00 wl                         umbenannt in SeqEdit (ersetzt SeqEdit)
  01.09.00 wl  FormCreate,FormDestroy   Fenster-Position wird geladen und gespeichert
  11.09.00 wl  FVolume,FReagent       Dynamisches Array -> Begrenzung auf 15 Levels aufgehoben
  12.09.00 wl  FormClose,ChangeSeqName  vor dem Schließen des Fensters um dem Laden einer neuen Sequenz: Speichern???
  12.09.00 wl  CheckArrayBounds       auch die maximale Anzahl der Positionen ist jetzt dynamisch
  12.09.00 wl  FillField              wenn das Feld außerhalb der Array-Grenzen liegt: Inhalt = ''
  12.09.00 wl  ChangeSeqName          Vor dem Laden der Daten wird kein FillBlock ausgeführt
  25.10.00 tbh ChangeSeqName          Fehler behoben: Format für Block wird jetzt auch bei Wechsel Sequenz gelesen
  23.10.02 wl  ChangeSeqName          TN1293.1 verwendet TWinlissyIniAccess
  23.10.02 wl  BitBtn1Click           TN1293.1 verwendet TWinlissyIniAccess
  20.12.02 wl                         TN1293.5 uses und WinlissyIniAccess geändert
  26.02.04 wl  BitBtn3Click           TN1574   Erzeugen von Quick-Report-Fenster in try-except-Block
  06.08.04 wl                         TN2008.2  FormStyle = fsNormal, Form derivated from TManualDockableForm
  17.08.04 wl  Reagents               TN1958.1  --> EdReagentList
  29.06.05 wl                         TN2444    an Änderungen von DockableForm angepasst
  05.09.05 wl                         TN2541.4  uses ViewReagentsOfLayout
  03.10.05 wl                         TN2642    Property Visible = false
  12.10.05 wl                         TN2541.4  kann jetzt mit Drag & Drop bedient werden
  24.11.05 pk                         TN2765    GetDataName
  04.03.06 wl  stgBlockDragDrop       TN2541.4  uses ViewReagentsOfLayout entfernt: gibt es nicht mehr
  27.11.06 wl  PrintData              TN3411   Funktion muß zunächst entfernt werden
  19.12.06 wl                         TN3409   class von TDockableEditForm in TViewItemEditForm geändert
  20.06.08 pk                         TN4139   uses changed
  16.01.09 wl                         TN4362   an Änderungen in TViewItem angepasst
  10.08.09 wl                         TN4702   Strings werden jetzt direkt geladen
  13.04.10 wl                         TN5044   uses geändert
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  25.05.10 wl                         TN5116   MainForm.DockPanel --> MainMethodDevelopment
  09.06.10 wl                         TN5116   anderes DockPanel für Reagenzien
  18.06.10 pk                         TN5152.1 TTable component removed and replaced by TDataProvider
  20.06.11 wl                         TN5603   Neue experimentelle Version
  30.06.11 wl                         TN5620   Reagenzien-Fenster wird jetzt unten angedockt
  26.07.11 wl                         TN5614   Zugriff auf ViewReagentsOfLayout geändert
  22.06.12 wl                         TN5924   grob überarbeitet - funktioniert jetzt wieder
  05.11.12 wl  UpdateValues2          TN6006   die Settings werden jetzt wirklich geschrieben (und nicht beim Speichern der Liquids)
  13.03.13 wl                         TN5960   uses geändert
  15.08.13 wl  CreateAmountComponents TN6217   LHDataAdaptor-Instance-Variable gelöscht
  -------------------------------------------------------------------------------------------------- }

unit SeqenceEditor;


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
    Spin,
    Buttons,
    DB,
    ComCtrls,
    ExtCtrls,
    Grids,
    Menus,
    ViewItemEditForm,
    ViewItem,
    StringLoader,
    DataProvider,
    SequenceDataAdaptor;

type
    TSequenceStep = class
    public
        function GetName: string; virtual; abstract;
        function GetCaption: string; virtual; abstract;
        function TextToRecord(const aText: string): TSequenceRec; virtual; abstract;
        function RecordToText(const aRec: TSequenceRec): string; virtual; abstract;

        function HasGrid: boolean; virtual;
        function HasAmount: boolean; virtual;
        function AmountLabelText: string; virtual;
        function DefaultAmountUnit: string; virtual;
        function HasExportFileName: boolean; virtual;
        function HasStoreWeightsBox: boolean; virtual;
        function HasLiquidParamBox: boolean; virtual;
        function HasTempGradient: boolean; virtual;
        function HasSubstance: boolean; virtual;
        function SubstanceLabelText: string; virtual;
        class function TextToRecord1(const aText: string): TSequenceRec;
        class function RecordToText1(const aRec: TSequenceRec): string;
        class function GetEmptyRecord(): TSequenceRec;
    end;

    TAddReagentsAbsSequenceStep = class(TSequenceStep)
    public
        function GetName: string; override;
        function GetCaption: string; override;
        function TextToRecord(const aText: string): TSequenceRec; override;
        function RecordToText(const aRec: TSequenceRec): string; override;
        function HasAmount: boolean; override;
        function AmountLabelText: string; override;
        function DefaultAmountUnit: string; override;
        function HasLiquidParamBox: boolean; override;
        function HasSubstance: boolean; override;
        function SubstanceLabelText: string; override;
    end;

    TAddReagentsConcSequenceStep = class(TSequenceStep)
    public
        function GetName: string; override;
        function GetCaption: string; override;
        function TextToRecord(const aText: string): TSequenceRec; override;
        function RecordToText(const aRec: TSequenceRec): string; override;
        function HasAmount: boolean; override;
        function AmountLabelText: string; override;
        function DefaultAmountUnit: string; override;
        function HasLiquidParamBox: boolean; override;
        function HasSubstance: boolean; override;
        function SubstanceLabelText: string; override;
    end;

    TPowderSequenceStep = class(TSequenceStep)
    public
        function GetName: string; override;
        function GetCaption: string; override;
        function TextToRecord(const aText: string): TSequenceRec; override;
        function RecordToText(const aRec: TSequenceRec): string; override;
        function HasAmount: boolean; override;
        function AmountLabelText: string; override;
        function DefaultAmountUnit: string; override;
        function HasExportFileName: boolean; override;
        function HasStoreWeightsBox: boolean; override;
        function HasSubstance: boolean; override;
        function SubstanceLabelText: string; override;
    end;

    TTitrationSequenceStep = class(TSequenceStep)
    public
        function GetName: string; override;
        function GetCaption: string; override;
        function TextToRecord(const aText: string): TSequenceRec; override;
        function RecordToText(const aRec: TSequenceRec): string; override;
        function HasAmount: boolean; override;
        function AmountLabelText: string; override;
    end;

    TPhMeasureSequenceStep = class(TSequenceStep)
    public
        function GetName: string; override;
        function GetCaption: string; override;
        function TextToRecord(const aText: string): TSequenceRec; override;
        function RecordToText(const aRec: TSequenceRec): string; override;
        function HasExportFileName: boolean; override;
    end;

    TVortexAndTempSequenceStep = class(TSequenceStep)
    public
        function HasGrid: boolean; override;
        function GetName: string; override;
        function GetCaption: string; override;
        function TextToRecord(const aText: string): TSequenceRec; override;
        function RecordToText(const aRec: TSequenceRec): string; override;
        function HasTempGradient: boolean; override;
    end;

    TSequenceStepFactory = class
    public
        class function CreateSequenceStep(const aName: string): TSequenceStep;
        class function GetAllNames(): TArray<string>;
    end;

    TSequenceEditorStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmSequenceEditor = class(TViewItemEditForm)
        Panel3: TPanel;
        Panel2: TPanel;
        Label3: TLabel;
        spinCols: TSpinEdit;
        Label4: TLabel;
        spinRows: TSpinEdit;
        btAddLevel: TButton;
        PopupMenu1: TPopupMenu;
        pmnuDelete: TMenuItem;
        pmnuEdit: TMenuItem;
        PageControl1: TPageControl;
        mnuSetToValue: TMenuItem;
        procedure btAddLevelClick(Sender: TObject);
        procedure spinRowsChange(Sender: TObject);
        procedure spinColsChange(Sender: TObject);
        procedure pmnuDeleteClick(Sender: TObject);
        procedure pmnuEditClick(Sender: TObject);
        procedure mnuSetToValueClick(Sender: TObject);
    private const
        cFloat1EditTag = 41;
        cFloat2EditTag = 42;
        cFloat3EditTag = 43;
        cDeviceEditTag = 55;
        cAmountEditTag = 56;
        cAmountUnitEditTag = 57;
        cExportFileNameEditTag = 58;
    private
        FSeqName: string;
        fLastCol, fLastRow: integer;
        fStringLoader: TSequenceEditorStringLoader;
        fLastVolumeText: string;

        procedure stgBlockKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure stgBlockDragDrop(Sender, Source: TObject; X, Y: Integer);
        procedure stgBlockMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure stgBlockDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
            var Accept: Boolean);
        procedure EditChanged(Sender: TObject);

        function GetValuesText(const aSubstID: string; aTabSheet: TTabSheet; aStep: TSequenceStep): string;
        procedure CreateAmountComponents(aSeqStep: TSequenceStep; aParent: TWinControl;
            aFirstRec: TSequenceRec);
        function CreateSeqStep(aGrid: TStringGrid): TSequenceStep;
        function FindGrid(aTabSheet: TTabSheet): TStringGrid;
        function FindTabSheet(aGrid: TStringGrid): TTabSheet;
        function FindEditWithTag(aTabSheet: TTabSheet; aTag: integer): TEdit;
        procedure ReadAndFill();
        procedure UpdateValues2;
        function AddLevel(aSeqStep: TSequenceStep; aFirstRec: TSequenceRec): TStringGrid;
        procedure DoFillBlock(const aRecs: TArray<TSequenceRec>);
        procedure ChangeSeqName();
        procedure OpenReagentEdit(aGrid: TStringGrid; aRow, aCol: integer);
        procedure AddReagentIntern(aGrid: TStringGrid; aReagent: string);
        function GetCurrentGrid: TStringGrid;
        function GetPosition(aGrid: TStringGrid; aCol, aRow: integer): integer;
        procedure GetColAndRow(aGrid: TStringGrid; aPosition: integer; out oCol, oRow: integer);
        function GetLevelData(aTabSheet: TTabSheet): TArray<TSequenceRec>;
        procedure SaveSequence(aDA: TSequenceDataAdaptor; const aSeqName: string);
    protected
        procedure SaveData(); override;
        procedure ResetData(); override;
        procedure UnloadData(); override;
        function GetDataName(): string; override;
        function CreateViewItem(const aItemName: string): TViewItem; override;
    public
        constructor Create(aOwner: TComponent; const aItemName: string;
            aOnSaveStatusChanged: TNotifyEvent); override;
        destructor Destroy; override;

        procedure FirstLoad(); override;
        procedure AddReagent(aReagent: string);
    end;


implementation


{$R *.DFM}

uses
    cxTL,
    EditReagent,
    MethodTypes,
    GeneralTypes,
    CommonTypes,
    AppSettings,
    SeqenceManager,
    GUIManager,
    LiqHDataAdaptorExt,
    SpecialViewItems,
    DialogUtils,
    Generics.Collections,
    MainMethodDevelopment,
    ControlUtils;

{ TSequenceEditorStringLoader }

procedure TSequenceEditorStringLoader.AddAllItems;
begin
    AddSingle(52630, 'Columns:', 'Spalten:');
    AddSingle(52640, 'Rows:', 'Reihen:');
    AddSingle(52690, 'Add Step', 'Schritt hinzufügen');
end;

{ TfrmSequenceEditor }

constructor TfrmSequenceEditor.Create(aOwner: TComponent; const aItemName: string;
    aOnSaveStatusChanged: TNotifyEvent);
begin
    inherited Create(aOwner, aItemName, aOnSaveStatusChanged);

    TControlUtils.ResetFontForWinXP(self);
    fStringLoader := TSequenceEditorStringLoader.Create;
    fStringLoader.LoadLanguage(self);

    FSeqName := fViewItem.Name;
end;

procedure TfrmSequenceEditor.CreateAmountComponents(aSeqStep: TSequenceStep; aParent: TWinControl;
    aFirstRec: TSequenceRec);
var
    xLabel: TLabel;
    xEdit: TEdit;
    xCheckBox: TCheckBox;
    xComboBox: TComboBox;
    xStart: integer;
    xDA: TLiquidParDataAdaptor;
begin
    xStart := 10;

    if aSeqStep.HasAmount then
    begin
        xLabel := TLabel.Create(self);
        xLabel.Parent := aParent;
        xLabel.Left := xStart;
        xLabel.Top := 8;
        xLabel.Caption := aSeqStep.AmountLabelText;
        xStart := xStart + 110;

        xEdit := TEdit.Create(self);
        xEdit.Parent := aParent;
        xEdit.Left := xStart;
        xEdit.Top := 8;
        xEdit.Tag := cAmountEditTag;
        xEdit.Text := FloatToStr(aFirstRec.Amount);
        xEdit.Width := 60;
        xStart := xStart + 80;

        if aSeqStep.DefaultAmountUnit <> '' then
        begin
            xEdit := TEdit.Create(self);
            xEdit.Parent := aParent;
            xEdit.Left := xStart;
            xEdit.Top := 8;
            xEdit.Tag := cAmountUnitEditTag;
            xEdit.Text := aSeqStep.DefaultAmountUnit;
            xEdit.Width := 60;
            xStart := xStart + 80;
        end;
    end;

    if aSeqStep.HasStoreWeightsBox then
    begin
        xCheckBox := TCheckBox.Create(self);
        xCheckBox.Parent := aParent;
        xCheckBox.Left := xStart;
        xCheckBox.Top := 8;
        xCheckBox.Caption := 'Store weights';
        xCheckBox.Width := 90;
        xStart := xStart + 110;
    end;

    if aSeqStep.HasExportFileName then
    begin
        xLabel := TLabel.Create(self);
        xLabel.Parent := aParent;
        xLabel.Left := xStart;
        xLabel.Top := 8;
        xLabel.Caption := 'Export File:';
        xStart := xStart + 100;

        xEdit := TEdit.Create(self);
        xEdit.Parent := aParent;
        xEdit.Left := xStart;
        xEdit.Top := 8;
        xEdit.Tag := cExportFileNameEditTag;
        xEdit.Width := 200;
        xStart := xStart + 220;
    end;

    if aSeqStep.HasLiquidParamBox then
    begin
        xLabel := TLabel.Create(self);
        xLabel.Parent := aParent;
        xLabel.Left := xStart;
        xLabel.Top := 8;
        xLabel.Caption := 'Liquid Handling:';
        xStart := xStart + 100;

        xComboBox := TComboBox.Create(self);
        xComboBox.Parent := aParent;
        xComboBox.Left := xStart;
        xComboBox.Top := 8;
        xComboBox.Tag := cExportFileNameEditTag;
        xComboBox.Width := 200;
        xDA := TLiquidParDataAdaptor.Create;
        try
            TControlUtils.AddValuesToComboBox(xDA.ReadAllNames, xComboBox, true);
        finally
            FreeAndNil(xDA);
        end;
        xComboBox.Text := xComboBox.Items[0];
        xStart := xStart + 220;
    end;

    if aSeqStep.HasTempGradient then
    begin
        xLabel := TLabel.Create(self);
        xLabel.Parent := aParent;
        xLabel.Left := xStart;
        xLabel.Top := 8;
        xLabel.Caption := 'Device:';
        xStart := xStart + 100;

        xEdit := TEdit.Create(self);
        xEdit.Parent := aParent;
        xEdit.Left := xStart;
        xEdit.Top := 8;
        xEdit.Tag := cDeviceEditTag;
        xEdit.Text := aFirstRec.SubstID;
        xEdit.OnChange := self.EditChanged;
        xEdit.Width := 200;
        xStart := xStart + 220;

        xLabel := TLabel.Create(self);
        xLabel.Parent := aParent;
        xLabel.Left := xStart;
        xLabel.Top := 8;
        xLabel.Caption := 'Speed:';
        xStart := xStart + 100;

        xEdit := TEdit.Create(self);
        xEdit.Parent := aParent;
        xEdit.Left := xStart;
        xEdit.Top := 8;
        xEdit.Text := FloatToStr(aFirstRec.Amount);
        xEdit.OnChange := self.EditChanged;
        xEdit.Tag := cAmountEditTag;
        xEdit.Width := 100;

        xLabel := TLabel.Create(self);
        xLabel.Parent := aParent;
        xLabel.Left := 10;
        xLabel.Top := 34;
        xLabel.Caption := 'Temperature Gradient:';

        xStart := 10;

        xLabel := TLabel.Create(self);
        xLabel.Parent := aParent;
        xLabel.Left := xStart;
        xLabel.Top := 60;
        xLabel.Caption := 'Start Temp. [°C]:';
        xStart := xStart + 100;

        xEdit := TEdit.Create(self);
        xEdit.Parent := aParent;
        xEdit.Left := xStart;
        xEdit.Top := 60;
        xEdit.Tag := cFloat1EditTag;
        xEdit.OnChange := self.EditChanged;
        xEdit.Text := FloatToStr(aFirstRec.ValFloat1);
        xEdit.Width := 100;
        xStart := xStart + 120;

        xLabel := TLabel.Create(self);
        xLabel.Parent := aParent;
        xLabel.Left := xStart;
        xLabel.Top := 60;
        xLabel.Caption := 'End Temp. [°C]:';
        xStart := xStart + 100;

        xEdit := TEdit.Create(self);
        xEdit.Parent := aParent;
        xEdit.Left := xStart;
        xEdit.Top := 60;
        xEdit.Tag := cFloat2EditTag;
        xEdit.OnChange := self.EditChanged;
        xEdit.Text := FloatToStr(aFirstRec.ValFloat2);
        xEdit.Width := 100;
        xStart := xStart + 120;

        xLabel := TLabel.Create(self);
        xLabel.Parent := aParent;
        xLabel.Left := xStart;
        xLabel.Top := 60;
        xLabel.Caption := 'Time to achieve end time [min]:';
        xStart := xStart + 200;

        xEdit := TEdit.Create(self);
        xEdit.Parent := aParent;
        xEdit.Left := xStart;
        xEdit.Top := 60;
        xEdit.Text := FloatToStr(aFirstRec.ValFloat3);
        xEdit.Tag := cFloat3EditTag;
        xEdit.OnChange := self.EditChanged;
        xEdit.Width := 100;
    end;

end;

destructor TfrmSequenceEditor.Destroy;
begin
    fStringLoader.Free;
    inherited;
end;

procedure TfrmSequenceEditor.DoFillBlock(const aRecs: TArray<TSequenceRec>);
var
    xGrid: TStringGrid;
    x, xRow, xCol: integer;
    xSeqStep: TSequenceStep;
begin
    if Length(aRecs) <= 0 then
        EXIT;

    xSeqStep := TSequenceStepFactory.CreateSequenceStep(aRecs[0].SeqTag);
    try
        xGrid := self.AddLevel(xSeqStep, aRecs[0]);
        if Assigned(xGrid) then
        begin
            for x := 0 to high(aRecs) do
            begin
                self.GetColAndRow(xGrid, aRecs[x].Position, xCol, xRow);
                xGrid.Cells[xCol, xRow] := xSeqStep.RecordToText(aRecs[x]);
            end;
        end;
    finally
        xSeqStep.Free;
    end;
end;

procedure TfrmSequenceEditor.EditChanged(Sender: TObject);
begin
    self.ChangeData;
end;

function TfrmSequenceEditor.CreateViewItem(const aItemName: string): TViewItem;
begin
    result := TSequenceViewItem.Create(aItemName);
end;

function TfrmSequenceEditor.FindEditWithTag(aTabSheet: TTabSheet; aTag: integer): TEdit;
var
    x: integer;
begin
    for x := 0 to self.ComponentCount - 1 do
    begin
        if (self.Components[x] is TEdit) then
        begin
            if ((self.Components[x] as TEdit).Parent.Parent = aTabSheet) and
                ((self.Components[x] as TEdit).Tag = aTag) then
                EXIT(self.Components[x] as TEdit);
        end;
    end;

    EXIT(nil);
end;

function TfrmSequenceEditor.FindGrid(aTabSheet: TTabSheet): TStringGrid;
var
    x: integer;
begin
    for x := 0 to self.ComponentCount - 1 do
    begin
        if (self.Components[x] is TStringGrid) then
        begin
            if ((self.Components[x] as TStringGrid).Parent = aTabSheet) then
                EXIT(self.Components[x] as TStringGrid);
        end;
    end;

    EXIT(nil);
end;

procedure TfrmSequenceEditor.FirstLoad;
begin
    ChangeSeqName();
end;

procedure TfrmSequenceEditor.ChangeSeqName();
var
    xIniAccess: IWinLissyIniAccess;
    iFormat: string;
    iRows, iCols: integer;
    xDA: TSequenceDataAdaptor;
begin
    Screen.Cursor := crHourglass;

    xIniAccess := gCommonDll.CreateAppIni;
    iCols := xIniAccess.ReadInteger(STR_ISEC_LASTRBLOCK, 'Columns');
    iRows := xIniAccess.ReadInteger(STR_ISEC_LASTRBLOCK, 'Rows');
    fLastVolumeText := FloatToStr(xIniAccess.ReadInteger(STR_ISEC_LASTRBLOCK, 'VolumeF1000') / 1000);

    // Rows and Colums laden
    xDA := TSequenceDataAdaptor.Create;
    try
        xDA.ReadSequenceAtLevelAndPosition(fSeqName, 0, 0);
        try
            if not xDA.DataProvider.IsEmpty then
            begin
                iFormat := xDA.DataProvider.FieldbyName('SUBSTID').AsString;
                spinCols.Value := StrToInt(Copy(iFormat, 9, Pos('x', iFormat) - 9));
                spinRows.Value := StrToInt(Copy(iFormat, Pos('x', iFormat) + 1, Length(iFormat) - Pos('x',
                    iFormat)));
            end
            else
            begin
                spinCols.Value := iCols;
                spinRows.Value := iRows;
            end;
        finally
            xDA.DataProvider.Close;
        end;
    finally
        FreeAndNil(xDA);
    end;

    Caption := GetCaption();
    // ------------------------------------------------------------------------------- stgBlock füllen
    ReadAndFill;

    spinRows.OnChange := spinRowsChange;
    spinCols.OnChange := spinColsChange;

    Screen.Cursor := crDefault;

    if Self.PageControl1.PageCount = 0 then
        btAddLevelClick(self);
end;

procedure TfrmSequenceEditor.ReadAndFill();
var
    xLevel: integer;
    xDA: TSequenceDataAdaptor;
    xRecs: TArray<TSequenceRec>;
begin
    xDA := TSequenceDataAdaptor.Create;
    try

        xLevel := 1;
        while True do
        begin
            xRecs := xDA.ReadSequenceAtLevel(FSeqName, xLevel);
            inc(xLevel);

            if Length(xRecs) = 0 then
                EXIT;

            DoFillBlock(xRecs);
        end;

    finally
        FreeAndNil(xDA);
    end;
end;

function TfrmSequenceEditor.FindTabSheet(aGrid: TStringGrid): TTabSheet;
begin
    EXIT(aGrid.Parent as TTabSheet);
end;

function TfrmSequenceEditor.CreateSeqStep(aGrid: TStringGrid): TSequenceStep;
var
    aParent: TTabSheet;
begin
    aParent := FindTabSheet(aGrid);
    result := TSequenceStepFactory.CreateSequenceStep(aParent.Caption);
end;

procedure TfrmSequenceEditor.GetColAndRow(aGrid: TStringGrid; aPosition: integer; out oCol, oRow: integer);
begin
    oCol := ((aPosition - 1) div (aGrid.RowCount - 1)) + 1;
    oRow := ((aPosition - 1) mod (aGrid.RowCount - 1)) + 1;
end;

function TfrmSequenceEditor.GetPosition(aGrid: TStringGrid; aCol, aRow: integer): integer;
begin
    result := aRow + (aGrid.RowCount - 1) * (aCol - 1);
end;

function TfrmSequenceEditor.AddLevel(aSeqStep: TSequenceStep; aFirstRec: TSequenceRec): TStringGrid;
var
    xSheet: TTabSheet;
    xTopPanel: TPanel;
    c, r: integer;
begin

    xSheet := TTabSheet.Create(self);
    xSheet.Caption := aSeqStep.GetName;
    xSheet.PageControl := self.PageControl1;
    self.PageControl1.ActivePageIndex := xSheet.PageIndex;

    xTopPanel := TPanel.Create(self);
    xTopPanel.Parent := xSheet;
    xTopPanel.Align := alTop;
    xTopPanel.Height := 40;
    xTopPanel.Caption := '';

    CreateAmountComponents(aSeqStep, xTopPanel, aFirstRec);

    if not aSeqStep.HasGrid then
    begin
        xTopPanel.Align := alClient;
        EXIT(nil);
    end;

    result := TStringGrid.Create(self);
    result.Parent := xSheet;
    result.Align := alClient;
    result.OnDragDrop := self.stgBlockDragDrop;
    result.OnDragOver := self.stgBlockDragOver;
    result.OnKeyDown := self.stgBlockKeyDown;
    result.OnMouseDown := self.stgBlockMouseDown;
    result.PopupMenu := self.PopupMenu1;
    result.Options := result.Options + [goRowSizing, goColSizing];

    result.FixedColor := clSilver;
    result.ColCount := spinCols.Value + 1;
    result.RowCount := spinRows.Value + 1;
    for c := 1 to result.colcount - 1 do
        result.Cells[c, 0] := TLanguageString.Read('Column {0}', 'Spalte {0}', [c]);
    for r := 1 to result.rowcount - 1 do
        result.Cells[0, r] := TLanguageString.Read('Row {0}', 'Reihe {0}', [r]);
    result.DefaultColWidth := Round((result.Width - 40) / result.ColCount);
end;

procedure TfrmSequenceEditor.AddReagent(aReagent: string);
begin
    self.AddReagentIntern(self.GetCurrentGrid, aReagent);
end;

function TfrmSequenceEditor.GetValuesText(const aSubstID: string; aTabSheet: TTabSheet;
    aStep: TSequenceStep): string;
var
    xRec: TSequenceRec;
    xEdit: TEdit;
begin
    xRec := TSequenceStep.GetEmptyRecord();

    if aStep.HasAmount then
    begin
        xRec.SubstID := aSubstID;

        xEdit := self.FindEditWithTag(aTabSheet, cAmountEditTag);
        xRec.Amount := StrToFloatDef(xEdit.Text, 0);

        xEdit := self.FindEditWithTag(aTabSheet, cAmountUnitEditTag);
        xRec.AmountUnit := xEdit.Text;
    end;

    xRec.ValBool1 := true;

    EXIT(aStep.RecordToText(xRec));
end;

procedure TfrmSequenceEditor.AddReagentIntern(aGrid: TStringGrid; aReagent: string);
var
    c, r: integer;
    iVolume: Extended;
    xSeqStep: TSequenceStep;
    xRec: TSequenceRec;
    xEdit: TEdit;
begin
    xSeqStep := self.CreateSeqStep(aGrid);
    try
        if xSeqStep.HasAmount then
        begin

            iVolume := 0;
            if (aReagent <> '') then
            begin
                try
                    xEdit := self.FindEditWithTag(self.FindTabSheet(aGrid), cAmountEditTag);
                    iVolume := StrToFloat(xEdit.Text);
                except
                    gGUIManager.MessageBox(TLanguageString.Read('The volume is not a numeric value!',
                        'Das Volumen ist kein numerischer Ausdruck!'), TLanguageString.Read('Edit Sequence',
                        'Sequenz bearbeiten'), 16);
                    exit;
                end;
            end;
            xRec.SubstID := aReagent;
            xRec.Amount := iVolume;
            xEdit := self.FindEditWithTag(self.FindTabSheet(aGrid), cAmountUnitEditTag);
            xRec.AmountUnit := xEdit.Text;
        end;

        for c := aGrid.Selection.left to aGrid.Selection.right do
        begin
            for r := aGrid.Selection.top to aGrid.Selection.bottom do
            begin
                aGrid.Cells[c, r] := xSeqStep.RecordToText(xRec);
            end;
        end;
        ChangeData();
    finally
        FreeAndNil(xSeqStep);
    end;
end;

procedure TfrmSequenceEditor.UpdateValues2;
var
    xIniAccess: IWinLissyIniAccess;
    xVol: double;
    xVolF1000: integer;
begin
    xVol := 0;
    // val(edVolume.Text, xVol, xError);
    xVolF1000 := Round(xVol * 1000);

    // write values to ini file
    xIniAccess := gCommonDll.CreateAppIni;
    xIniAccess.WriteInteger(STR_ISEC_LASTRBLOCK, 'Columns', spinCols.Value);
    xIniAccess.WriteInteger(STR_ISEC_LASTRBLOCK, 'Rows', spinRows.Value);
    if (xVolF1000 > 0) then
        xIniAccess.WriteInteger(STR_ISEC_LASTRBLOCK, 'VolumeF1000', xVolF1000);
    xIniAccess.WriteSectionFromCache(STR_ISEC_LASTRBLOCK);
end;

procedure TfrmSequenceEditor.btAddLevelClick(Sender: TObject);
var
    xValue: string;
    xSeqStep: TSequenceStep;
    xFirstRec: TSequenceRec;
begin
    xValue := TDialogUtils.SelectItemBox(TSequenceStepFactory.GetAllNames, 'Add next step',
        'Choose the kind of step');
    xSeqStep := TSequenceStepFactory.CreateSequenceStep(xValue);
    try
        xFirstRec := TSequenceStep.GetEmptyRecord();
        if xSeqStep.DefaultAmountUnit <> '' then
            xFirstRec.Amount := StrToFloatDef(fLastVolumeText, 0)
        else
            xFirstRec.Amount := 7;

        AddLevel(xSeqStep, xFirstRec);
    finally
        xSeqStep.Free;
    end;
end;

procedure TfrmSequenceEditor.UnloadData;
begin

end;

function TfrmSequenceEditor.GetCurrentGrid: TStringGrid;
begin
    EXIT(self.FindGrid(self.PageControl1.ActivePage));
end;

function TfrmSequenceEditor.GetDataName(): string;
begin
    result := FSeqName;
end;

function TfrmSequenceEditor.GetLevelData(aTabSheet: TTabSheet): TArray<TSequenceRec>;
var
    xRow, xCol, x: integer;
    xRec: TSequenceRec;
    xStep: TSequenceStep;
    xList: TList<TSequenceRec>;
    xGrid: TStringGrid;
    xEdit: TEdit;
begin
    xGrid := FindGrid(aTabSheet);

    xStep := TSequenceStepFactory.CreateSequenceStep(aTabSheet.Caption);
    try

        xList := TList<TSequenceRec>.Create;
        try

            if xGrid = nil then
            begin
                xEdit := self.FindEditWithTag(aTabSheet, cAmountEditTag);
                if Assigned(xEdit) and (xEdit.Text <> '') then
                begin
                    xRec := TSequenceStep.GetEmptyRecord;
                    xRec.Valid := true;
                    xRec.Amount := StrToFloatDef(xEdit.Text, 0);

                    xEdit := self.FindEditWithTag(aTabSheet, cDeviceEditTag);
                    xRec.SubstID := xEdit.Text;

                    xEdit := self.FindEditWithTag(aTabSheet, cFloat1EditTag);
                    xRec.ValFloat1 := StrToFloatDef(xEdit.Text, 0);

                    xEdit := self.FindEditWithTag(aTabSheet, cFloat2EditTag);
                    xRec.ValFloat2 := StrToFloatDef(xEdit.Text, 0);

                    xEdit := self.FindEditWithTag(aTabSheet, cFloat3EditTag);
                    xRec.ValFloat3 := StrToFloatDef(xEdit.Text, 0);

                    for x := 1 to (spinRows.Value * spinCols.Value) do
                    begin
                        xRec.Position := x;
                        xRec.SeqTag := aTabSheet.Caption;
                        xList.Add(xRec);
                    end;
                    EXIT(xList.ToArray)
                end
                else
                    EXIT(nil);
            end;

            for xCol := 1 to xGrid.ColCount - 1 do
            begin
                for xRow := 1 to xGrid.RowCount - 1 do
                begin
                    if (xGrid.Cells[xCol, xRow] <> '') then
                    begin
                        xRec := xStep.TextToRecord(xGrid.Cells[xCol, xRow]);
                        xRec.Position := GetPosition(xGrid, xCol, xRow);
                        xRec.SeqTag := aTabSheet.Caption;

                        if xRec.Valid then
                            xList.Add(xRec);
                    end;
                end;
            end;

            result := xList.ToArray;
        finally
            FreeAndNil(xList)
        end;
    finally
        FreeAndNil(xStep);
    end;
end;

procedure TfrmSequenceEditor.SaveData();
const
    cTempSeqName = '_TEMP~~~~~SEQ';
var
    xDA: TSequenceDataAdaptor;
begin
    inherited;

    Screen.Cursor := crHourglass;
    try
        UpdateValues2;

        xDA := TSequenceDataAdaptor.Create;
        try
            // delete temp. sequence
            xDA.DeleteName(cTempSeqName);

            // save sequence as temp. sequence
            SaveSequence(xDA, cTempSeqName);
            // delete real sequence
            xDA.DeleteName(fSeqName);

            // save temp. sequence as real sequence
            xDA.SaveNameAs(cTempSeqName, fSeqName);

            // delete temp. sequence
            xDA.DeleteName(cTempSeqName);

        finally
            FreeAndNil(xDA);
        end;
    finally
        Screen.Cursor := crDefault;
    end;
end;

procedure TfrmSequenceEditor.SaveSequence(aDA: TSequenceDataAdaptor; const aSeqName: string);
var
    xPage, xLevel, xRec: integer;
    xRecs: TArray<TSequenceRec>;
begin
    // alle Level-Informationen speichern
    xLevel := 1;
    for xPage := 0 to self.PageControl1.PageCount - 1 do
    begin
        xRecs := GetLevelData(self.PageControl1.Pages[xPage]);
        if Length(xRecs) <= 0 then
            CONTINUE;

        for xRec := 0 to high(xRecs) do
        begin
            xRecs[xRec].SeqName := aSeqName;
            xRecs[xRec].Level := xLevel;
        end;
        aDA.WriteRecs(xRecs);
        inc(xLevel);
    end;

    // Format des Reaktionsblocks speichern
    SetLength(xRecs, 1);
    xRecs[0].SeqName := aSeqName;
    xRecs[0].Position := 0;
    xRecs[0].Level := 0;
    xRecs[0].SubstID := Format('Format: %dx%d', [spinCols.Value, spinRows.Value]);
    aDA.WriteRecs(xRecs);
end;

procedure TfrmSequenceEditor.mnuSetToValueClick(Sender: TObject);
var
    xSelection: TGridRect;
    xGrid: TStringGrid;
    c, r: integer;
    xStep: TSequenceStep;
    xTabSheet: TTabSheet;
    xRec: TSequenceRec;
begin
    xTabSheet := self.PageControl1.ActivePage;
    xGrid := FindGrid(xTabSheet);
    if not(xGrid is TStringGrid) then
        exit;

    // Wenn das Feld nicht innerhalb der Selektion liegt, wird das gewählte Feld selektiert
    if not((fLastCol >= xGrid.Selection.Left) and (fLastCol <= xGrid.Selection.Right) and
        (fLastRow >= xGrid.Selection.Top) and (fLastRow <= xGrid.Selection.Bottom)) then
    begin
        xSelection.Left := fLastCol;
        xSelection.Right := fLastCol;
        xSelection.Top := fLastRow;
        xSelection.Bottom := fLastRow;
        xGrid.Selection := xSelection;
    end;

    xStep := TSequenceStepFactory.CreateSequenceStep(xTabSheet.Caption);
    try

        for c := xGrid.Selection.left to xGrid.Selection.right do
        begin
            for r := xGrid.Selection.top to xGrid.Selection.bottom do
            begin
                xRec := xStep.TextToRecord(xGrid.Cells[c, r]);
                xGrid.Cells[c, r] := GetValuesText(xRec.SubstID, xTabSheet, xStep);

                ChangeData();
            end;
        end;
    finally
        xStep.Free;
    end;
end;

procedure TfrmSequenceEditor.spinRowsChange(Sender: TObject);
begin
    ChangeData(); // noch nicht richtig
end;

procedure TfrmSequenceEditor.spinColsChange(Sender: TObject);
begin
    ChangeData(); // noch nicht richtig
end;

procedure TfrmSequenceEditor.ResetData;
begin
    ChangeSeqName();
end;

procedure TfrmSequenceEditor.stgBlockKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    case (Key) of
        VK_DELETE:
            AddReagentIntern(Sender as TStringGrid, '');
    end;
end;

procedure TfrmSequenceEditor.pmnuDeleteClick(Sender: TObject);
var
    xSelection: TGridRect;
    xGrid: TStringGrid;
    c, r: integer;
begin
    xGrid := FindGrid(self.PageControl1.ActivePage);
    if not(xGrid is TStringGrid) then
        exit;

    // Wenn das Feld nicht innerhalb der Selektion liegt, wird das gewählte Feld selektiert
    if not((fLastCol >= xGrid.Selection.Left) and (fLastCol <= xGrid.Selection.Right) and
        (fLastRow >= xGrid.Selection.Top) and (fLastRow <= xGrid.Selection.Bottom)) then
    begin
        xSelection.Left := fLastCol;
        xSelection.Right := fLastCol;
        xSelection.Top := fLastRow;
        xSelection.Bottom := fLastRow;
        xGrid.Selection := xSelection;
    end;

    for c := xGrid.Selection.left to xGrid.Selection.right do
    begin
        for r := xGrid.Selection.top to xGrid.Selection.bottom do
        begin
            xGrid.Cells[c, r] := '';

            ChangeData();
        end;
    end;
end;

procedure TfrmSequenceEditor.stgBlockDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
    if (Source is TcxTreeList) and Assigned((Source as TcxTreeList).DragNode) then
    begin
        Accept := ((Source as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_REAGENT);
    end;
end;

procedure TfrmSequenceEditor.stgBlockDragDrop(Sender, Source: TObject; X, Y: Integer);
var
    xRow, xCol: integer;
    xReagentName: string;
    xSelection: TGridRect;
    xGrid: TStringGrid;
begin
    if not(Sender is TStringGrid) then
        exit;
    xGrid := (Sender as TStringGrid);

    xReagentName := '';

    if (Source is TcxTreeList) // Overview oder Favourites
        and Assigned((Source as TcxTreeList).DragNode) then
    begin
        if (Source as TcxTreeList).DragNode.StateIndex = INT_IM_INDEX_REAGENT then
        begin
            xReagentName := (Source as TcxTreeList).DragNode.Texts[0];
        end;
    end;

    if (xReagentName = '') then
        EXIT;

    xGrid.MouseToCell(X, Y, xCol, xRow);

    // Wenn das Feld nicht innerhalb der Selektion liegt, wird das gewählte Feld selektiert
    if not((xCol >= xGrid.Selection.Left) and (xCol <= xGrid.Selection.Right) and
        (xRow >= xGrid.Selection.Top) and (xRow <= xGrid.Selection.Bottom)) then
    begin
        xSelection.Left := xCol;
        xSelection.Right := xCol;
        xSelection.Top := xRow;
        xSelection.Bottom := xRow;
        xGrid.Selection := xSelection;
    end;

    self.AddReagentIntern(xGrid, xReagentName);
end;

procedure TfrmSequenceEditor.OpenReagentEdit(aGrid: TStringGrid; aRow, aCol: integer);
var
    xEditForm: TfrmEditReagent;
    xSeqStep: TSequenceStep;
    xRec: TSequenceRec;
begin
    xEditForm := TfrmEditReagent.Create(aRow, aCol, []);
    try
        xSeqStep := self.CreateSeqStep(aGrid);
        try
            xRec := xSeqStep.TextToRecord(aGrid.Cells[aCol, aRow]);
            xEditForm.ReagentName := xRec.SubstID;
            xEditForm.Amount := xRec.Amount;

            if (xEditForm.ShowModal = mrOK) then
            begin
                xRec.SubstID := xEditForm.ReagentName;
                xRec.Amount := xEditForm.Amount;
                xRec.AmountUnit := 'µL';
                aGrid.Cells[aCol, aRow] := xSeqStep.RecordToText(xRec);

                ChangeData();
            end;
        finally
            FreeAndNil(xSeqStep);
        end;
    finally
        xEditForm.Free;
    end;
end;

procedure TfrmSequenceEditor.pmnuEditClick(Sender: TObject);
begin
    OpenReagentEdit(self.GetCurrentGrid, fLastRow, fLastCol);
end;

procedure TfrmSequenceEditor.stgBlockMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
    xGrid: TStringGrid;
begin
    if not(Sender is TStringGrid) then
        exit;
    xGrid := (Sender as TStringGrid);

    if (Button = mbRight) then
    begin
        xGrid.MouseToCell(X, Y, fLastCol, fLastRow);
    end;
end;

{ TSequenceStepFactory }

class function TSequenceStepFactory.CreateSequenceStep(const aName: string): TSequenceStep;
begin
    if (aName = 'AddReagents(Absolute)') then
    begin
        result := TAddReagentsAbsSequenceStep.Create;
    end
    else if (aName = 'AddReagents(Concentration)') then
    begin
        result := TAddReagentsConcSequenceStep.Create;
    end
    else if (aName = 'Powder') then
    begin
        result := TPowderSequenceStep.Create;
    end
    else if (aName = 'Titration') then
    begin
        result := TTitrationSequenceStep.Create;
    end
    else if (aName = 'pHMeasurement') then
    begin
        result := TPhMeasureSequenceStep.Create;
    end
    else if (aName = 'HeatingCoolingVortexing') then
    begin
        result := TVortexAndTempSequenceStep.Create;
    end
    else
        raise Exception.Create('Sequnce Step ' + aName + ' is unknown');
end;

class function TSequenceStepFactory.GetAllNames: TArray<string>;
begin
    SetLength(result, 6);
    result[0] := 'Powder';
    result[1] := 'AddReagents(Absolute)';
    result[2] := 'AddReagents(Concentration)';
    result[3] := 'Titration';
    result[4] := 'pHMeasurement';
    result[5] := 'HeatingCoolingVortexing';
end;

{ TSequenceStep }

function TSequenceStep.AmountLabelText: string;
begin
    EXIT('');
end;

function TSequenceStep.DefaultAmountUnit: string;
begin
    EXIT('');
end;

function TSequenceStep.HasAmount: boolean;
begin
    EXIT(false);
end;

function TSequenceStep.HasExportFileName: boolean;
begin
    EXIT(false);
end;

function TSequenceStep.HasGrid: boolean;
begin
    EXIT(true);
end;

function TSequenceStep.HasLiquidParamBox: boolean;
begin
    EXIT(false);
end;

function TSequenceStep.HasStoreWeightsBox: boolean;
begin
    EXIT(false);
end;

function TSequenceStep.HasSubstance: boolean;
begin
    EXIT(false)
end;

function TSequenceStep.HasTempGradient: boolean;
begin
    EXIT(false);
end;

class function TSequenceStep.RecordToText1(const aRec: TSequenceRec): string;
begin
    if aRec.SubstID = '' then
        EXIT('')
    else
        EXIT(aRec.SubstID + ': ' + FloatToStr(aRec.Amount) + ' ' + aRec.AmountUnit);
end;

function TSequenceStep.SubstanceLabelText: string;
begin
    EXIT('');
end;

class function TSequenceStep.GetEmptyRecord(): TSequenceRec;
begin
    result.Valid := false;
    result.SeqName := '';
    result.Level := 0;
    result.Position := 0;
    result.SeqTag := '';
    result.SubstID := '';
    result.Amount := 0;
    result.AmountUnit := '';
    result.ValBool1 := false;
    result.ValInt1 := 0;
    result.ValFloat1 := 0;
    result.ValString1 := '';
end;

class function TSequenceStep.TextToRecord1(const aText: string): TSequenceRec;
var
    xPos1, xPos2: integer;
    xText2: string;
begin
    result := GetEmptyRecord();

    if (aText = '') then
        EXIT;

    xPos1 := Pos(': ', aText);
    result.SubstID := Copy(aText, 1, xPos1 - 1);
    xText2 := Copy(aText, xPos1 + 2, Length(aText) - xPos1 - 1);
    xPos2 := Pos(' ', xText2);
    result.Amount := StrToFloatDef(Copy(xText2, 1, xPos2 - 1), 0);
    result.AmountUnit := Copy(xText2, xPos2 + 1, Length(aText) - xPos2 - 1);
    result.Valid := true;
end;

{ TAddReagentsAbsSequenceStep }

function TAddReagentsAbsSequenceStep.AmountLabelText: string;
begin
    EXIT(TLanguageString.Read('Volume:', 'Volumen:'));
end;

function TAddReagentsAbsSequenceStep.DefaultAmountUnit: string;
begin
    EXIT('µL');
end;

function TAddReagentsAbsSequenceStep.GetCaption: string;
begin
    EXIT('Add Reagents');
end;

function TAddReagentsAbsSequenceStep.GetName: string;
begin
    EXIT('AddReagents(Absolute)');
end;

function TAddReagentsAbsSequenceStep.HasAmount: boolean;
begin
    EXIT(true);
end;

function TAddReagentsAbsSequenceStep.HasLiquidParamBox: boolean;
begin
    EXIT(true);
end;

function TAddReagentsAbsSequenceStep.HasSubstance: boolean;
begin
    EXIT(true);
end;

function TAddReagentsAbsSequenceStep.RecordToText(const aRec: TSequenceRec): string;
begin
    EXIT(RecordToText1(aRec));
end;

function TAddReagentsAbsSequenceStep.SubstanceLabelText: string;
begin
    EXIT('Reagent:');
end;

function TAddReagentsAbsSequenceStep.TextToRecord(const aText: string): TSequenceRec;
begin
    EXIT(TextToRecord1(aText));
end;

{ TAddReagentsConcSequenceStep }

function TAddReagentsConcSequenceStep.AmountLabelText: string;
begin
    EXIT(TLanguageString.Read('Concentration:', 'Konzentration:'));
end;

function TAddReagentsConcSequenceStep.DefaultAmountUnit: string;
begin
    EXIT('%');
end;

function TAddReagentsConcSequenceStep.GetCaption: string;
begin
    EXIT('Add Reagents');
end;

function TAddReagentsConcSequenceStep.GetName: string;
begin
    EXIT('AddReagents(Concentration)');
end;

function TAddReagentsConcSequenceStep.HasAmount: boolean;
begin
    EXIT(true);
end;

function TAddReagentsConcSequenceStep.HasLiquidParamBox: boolean;
begin
    EXIT(true);
end;

function TAddReagentsConcSequenceStep.HasSubstance: boolean;
begin
    EXIT(true);
end;

function TAddReagentsConcSequenceStep.RecordToText(const aRec: TSequenceRec): string;
begin
    EXIT(RecordToText1(aRec));
end;

function TAddReagentsConcSequenceStep.SubstanceLabelText: string;
begin
    EXIT('Reagent:');
end;

function TAddReagentsConcSequenceStep.TextToRecord(const aText: string): TSequenceRec;
begin
    EXIT(TextToRecord1(aText));
end;

{ TPowderSequenceStep }

function TPowderSequenceStep.AmountLabelText: string;
begin
    EXIT(TLanguageString.Read('Target weight:', 'Zielgewicht:'));
end;

function TPowderSequenceStep.DefaultAmountUnit: string;
begin
    EXIT('mg');
end;

function TPowderSequenceStep.GetCaption: string;
begin
    EXIT('Add Powder');
end;

function TPowderSequenceStep.GetName: string;
begin
    EXIT('Powder');
end;

function TPowderSequenceStep.HasAmount: boolean;
begin
    EXIT(true);
end;

function TPowderSequenceStep.HasExportFileName: boolean;
begin
    EXIT(true);
end;

function TPowderSequenceStep.HasStoreWeightsBox: boolean;
begin
    EXIT(true);
end;

function TPowderSequenceStep.HasSubstance: boolean;
begin
    EXIT(true);
end;

function TPowderSequenceStep.RecordToText(const aRec: TSequenceRec): string;
begin
    EXIT(RecordToText1(aRec));
end;

function TPowderSequenceStep.SubstanceLabelText: string;
begin
    EXIT('Powder:');
end;

function TPowderSequenceStep.TextToRecord(const aText: string): TSequenceRec;
begin
    EXIT(TextToRecord1(aText));
end;

{ TTitrationSequenceStep }

function TTitrationSequenceStep.AmountLabelText: string;
begin
    EXIT('pH:');
end;

function TTitrationSequenceStep.GetCaption: string;
begin
    EXIT('Titration');
end;

function TTitrationSequenceStep.GetName: string;
begin
    EXIT('Titration');
end;

function TTitrationSequenceStep.HasAmount: boolean;
begin
    EXIT(true);
end;

function TTitrationSequenceStep.RecordToText(const aRec: TSequenceRec): string;
begin
    EXIT('pH: ' + FloatToStr(aRec.Amount));
end;

function TTitrationSequenceStep.TextToRecord(const aText: string): TSequenceRec;
var
    xPos1: integer;
    xText2: string;
begin
    result := GetEmptyRecord();

    if (aText = '') then
        EXIT;

    xPos1 := Pos(': ', aText);
    xText2 := Copy(aText, xPos1 + 2, Length(aText) - xPos1 - 1);
    result.Amount := StrToFloatDef(xText2, 0);
    if (result.Amount = 0) then
        EXIT;
    result.Valid := true;
end;

{ TPhMeasureSequenceStep }

function TPhMeasureSequenceStep.GetCaption: string;
begin
    EXIT('pH Measurement');
end;

function TPhMeasureSequenceStep.GetName: string;
begin
    EXIT('pHMeasurement');
end;

function TPhMeasureSequenceStep.HasExportFileName: boolean;
begin
    EXIT(true);
end;

function TPhMeasureSequenceStep.RecordToText(const aRec: TSequenceRec): string;
begin
    if aRec.ValBool1 then
        EXIT('YES')
    else
        EXIT('NO');
end;

function TPhMeasureSequenceStep.TextToRecord(const aText: string): TSequenceRec;
begin
    result := GetEmptyRecord();
    result.ValBool1 := (aText = 'YES');
    result.Valid := result.ValBool1;
end;

{ TVortexAndTempSequenceStep }

function TVortexAndTempSequenceStep.GetCaption: string;
begin
    EXIT('HeatingCoolingVortexing');
end;

function TVortexAndTempSequenceStep.GetName: string;
begin
    EXIT('HeatingCoolingVortexing');
end;

function TVortexAndTempSequenceStep.HasGrid: boolean;
begin
    EXIT(false);
end;

function TVortexAndTempSequenceStep.HasTempGradient: boolean;
begin
    EXIT(true);
end;

function TVortexAndTempSequenceStep.RecordToText(const aRec: TSequenceRec): string;
begin
    result := '';
end;

function TVortexAndTempSequenceStep.TextToRecord(const aText: string): TSequenceRec;
begin
    result := GetEmptyRecord();
end;


end.
