{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Wizard page for defining method name and/or layout
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  02.12.04 wl                               TN2254.2  initial version
  17.08.05 wl  CheckBeforeNext              TN2558.7  NICHT CASE-SENSITIVE: Existierende Methodennamen werden nicht mehr akzeptiert
  24.11.05 pk                               TN2805    references to global removed
  30.11.05 wl  CheckBeforeNext              TN2815    Namens-Prüfung wieder aktiviert
  21.12.05 wl  Create                       TN2541.0  kann Methoden-Namen übernehmen
  30.08.07 pk  Setup                        TN3840.1  Call GetNames instead of GetLayoutNames
  03.09.07 pk  CheckBeforeNext              TN3847    uses TMethodDataAdaptorExt
  04.04.08 wl                               TN4058    uses geändert
  15.04.09 pk  Create                       TN4518    original layout name is shown in combo box
  17.06.09 wl                               TN4612    zusammengefasst mit UseInitCheckBox, DisplayComponentWizardPage
  06.08.09 wl                               TN4702    Strings werden direkt geladen
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.11.09 pk  Create                       TN4843    create fDisplayComponentNames
  07.05.10 wl                               TN5052    berücksichtigt TAppSettings.UseDisplayComponenets
  20.05.10 wl                               TN5117   uses ControlUtils
  29.10.10 pk  RestartEvent                 TN5320    New
  02.02.11 wl  FChooseLayout                TN5466    neue Checkbox: Soll Layout zur Laufzeit gewählt werden
  08.02.11 wl                               TN5475   Zugriffe auf TLayoutDataAdaptor.Instance geändert
  27.12.11 wl                               TN5768   DeleteRunDataOption: wenn false, werden vor dem Laden des Layouts keine Daten gelöscht!
  -------------------------------------------------------------------------------------------------- }

unit MethodLayoutPage;


interface


uses
    Classes,
    Controls,
    Forms,
    Wizard,
    StdCtrls,
    GeneralTypes;

type
    TMethodLayoutPage = class(TWizardPage)
    private
        FActiveControl: TWinControl;
        FNameCombo: TComboBox;
        FUseLayout: TCheckBox;
        FChooseLayout: TCheckBox;

        FUseStandardInit: TCheckBox;
        FDeleteRunData: TCheckBox;

        FDCNameCombo: TComboBox;
        FUseDisplayComponent: TRadioButton;
        FNoDisplayComponent: TRadioButton;
        fPossibleDisplayComponentNames: TStringArray;

        fUseRestartEvent: TCheckBox;
        fPossibleRestartEventNames: TStringArray;
        fPossibleRestartEventNamesCombo: TComboBox;
        function GetLayoutName: string;
        function LayoutExists: boolean;
        function GetDisplayComponentName: string;
        function GetUseStandardInit: boolean;
        function GetDeleteRunData: boolean;
        function GetRestartEventName: string;
        procedure UseLayoutOnClick(Sender: TObject);
        procedure UseDisplayComponentOnClick(Sender: TObject);
        procedure UseRestartEventOnClick(Sender: TObject);
    public
        // constructor
        constructor Create(aOwner: TComponent; aUseStandardInit, aDeleteRunData: boolean;
            const aCurrentMethod: string; const aOriginalLayout: string;
            const aCurrentDisplayComponentName: string; const aPossibleDisplayComponentNames: TStringArray;
            const aShowRestartOptions: boolean; const aCurrentRestartEventName: string;
            const aPossibleRestartEventNames: TStringArray); reintroduce;
        // methods derived from TWizardPage
        function GetHeaderTitle: string; override;
        function GetHeaderText(): TStringArray; override;
        function CheckBeforeNext: boolean; override;
        function GetActiveControl: TWinControl; override;
        procedure SetUp(aPreviousPage: TWizardPage); override;
        // properties
        property LayoutName: string read GetLayoutName;
        property DisplayComponentName: string read GetDisplayComponentName;
        property UseStandardInit: boolean read GetUseStandardInit;
        property DeleteRunData: boolean read GetDeleteRunData;
        property RestartEventName: string read GetRestartEventName;
    end;


implementation


uses
    ControlUtils,
    DialogUtils,
    CommonTypes,
    AppSettings,
    LayoutDataAdaptor,
    MethodDataAdaptor,
    MethodDataAdaptorExt,
    MethodSettings;

{ TMethodLayoutPage }

constructor TMethodLayoutPage.Create(aOwner: TComponent; aUseStandardInit, aDeleteRunData: boolean;
    const aCurrentMethod: string; const aOriginalLayout: string; const aCurrentDisplayComponentName: string;
    const aPossibleDisplayComponentNames: TStringArray; const aShowRestartOptions: boolean;
    const aCurrentRestartEventName: string; const aPossibleRestartEventNames: TStringArray);
begin
    inherited Create(aOwner);

    FUseLayout := self.AddCheckBox(TLanguageString.Read('Load this layout:', 'Dieses Layout laden:'),
        INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1, false);
    FUseLayout.Checked := (aOriginalLayout <> '') and
        (aOriginalLayout <> TMethodSettings.cChooseLayoutAtRuntime);
    FUseLayout.OnClick := UseLayoutOnClick;
    FNameCombo := self.AddComboBox(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    if (aOriginalLayout <> TMethodSettings.cChooseLayoutAtRuntime) then
        fNameCombo.Text := aOriginalLayout;
    AddDistance(wzdMedium);
    FChooseLayout := self.AddCheckBox(TLanguageString.Read('Choose layout at runtime',
        'Das Layout zur Laufzeit wählen'), INT_WIZARD_LEFT_2, INT_WIZARD_WIDTH_1, false);
    FChooseLayout.Checked := (aOriginalLayout = TMethodSettings.cChooseLayoutAtRuntime);
    FChooseLayout.OnClick := UseLayoutOnClick;

    UseLayoutOnClick(self);

    AddDistance(wzdLarge);
    FUseStandardInit := self.AddCheckBox(TLanguageString.Read('Use standard initialization',
        'Standard-Initialisierung benutzen'), INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1, false);
    FUseStandardInit.Checked := aUseStandardInit;
    AddDistance(wzdMedium);
    FDeleteRunData := self.AddCheckBox(TLanguageString.Read('Delete run layout and posinfo data at begin',
        'Zu Beginn Run-Layout und Posinfo-Daten löschen'), INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1, false);
    FDeleteRunData.Checked := aDeleteRunData;
    AddDistance(wzdLarge);

    if TAppSettings.UseDisplayComponents then
    begin
        fPossibleDisplayComponentNames := aPossibleDisplayComponentNames;

        FUseDisplayComponent := self.AddRadioButton(TLanguageString.Read('Choose a Display Component',
            'Display-Komponente auswählen'), INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1, false);
        FUseDisplayComponent.Checked := (aCurrentDisplayComponentName <> '');
        FUseDisplayComponent.OnClick := UseDisplayComponentOnClick;
        fDCNameCombo := self.AddComboBox(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
        fDCNameCombo.Text := aCurrentDisplayComponentName;

        AddDistance(wzdMedium);
        FNoDisplayComponent := self.AddRadioButton(TLanguageString.Read('Use Default Display Component',
            'Default Display-Komponente verwenden'), INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1, false);
        FNoDisplayComponent.Checked := (aCurrentDisplayComponentName = '');
        FNoDisplayComponent.OnClick := UseDisplayComponentOnClick;
        UseDisplayComponentOnClick(self);
    end;

    AddDistance(wzdLarge);
    fPossibleRestartEventNames := aPossibleRestartEventNames;
    FUseRestartEvent := self.AddCheckBox(TLanguageString.Read('Use a Restart Event',
        'Restart-Ereigniss verwenden'), INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1, false);
    FUseRestartEvent.Checked := (aCurrentRestartEventName <> '');
    FUseRestartEvent.OnClick := UseRestartEventOnClick;
    fPossibleRestartEventNamesCombo := self.AddComboBox(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    fPossibleRestartEventNamesCombo.Text := aCurrentRestartEventName;
    UseRestartEventOnClick(self);

    if not aShowRestartOptions then
    begin
        FUseRestartEvent.Visible := false;
        fPossibleRestartEventNamesCombo.Visible := false;
    end;

    FActiveControl := FUseLayout;
end;

function TMethodLayoutPage.GetHeaderTitle: string;
begin
    result := TLanguageString.Read('Define start options', 'Start-Optionen definieren');
end;

function TMethodLayoutPage.GetHeaderText(): TStringArray;
begin
    SetLength(result, 3);
    result[0] := TLanguageString.Read('Define the layout loaded for this method',
        'Definiere das Layout, das für diese Methode gestartet wird');
    result[1] := TLanguageString.Read('Define if there is a standard initialization',
        'Definiere, ob es eine Standard-Initialisierung geben soll');
    result[2] := TLanguageString.Read('Define the Display component', 'Definiere die Display-Komponente');
end;

function TMethodLayoutPage.LayoutExists(): boolean;
var
    x: integer;
begin
    for x := 0 to FNameCombo.Items.Count - 1 do
    begin
        if (FNameCombo.Items[x] = FNameCombo.Text) then
        begin
            result := true;
            EXIT;
        end;
    end;
    result := false;
end;

function TMethodLayoutPage.CheckBeforeNext: boolean;
begin
    result := false;

    // check if layout name exists
    if (FUseLayout.Checked) and (not self.LayoutExists()) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('This layout name does not exist.',
            'Dieser Layoutname existiert nicht.'), TLanguageString.Read('Layout name', 'Layoutname'), 64);
        FActiveControl := FNameCombo;
        EXIT;
    end;

    result := true;
end;

function TMethodLayoutPage.GetActiveControl: TWinControl;
begin
    result := FActiveControl;
end;

function TMethodLayoutPage.GetLayoutName: string;
begin
    if (FUseLayout.Checked) then
        EXIT(FNameCombo.Text);

    if (FChooseLayout.Checked) then
        EXIT(TMethodSettings.cChooseLayoutAtRuntime);

    EXIT('');
end;

function TMethodLayoutPage.GetDisplayComponentName: string;
begin
    if Assigned(FUseDisplayComponent) and (FUseDisplayComponent.Checked) then
        result := fDCNameCombo.Text
    else
        result := '';
end;

function TMethodLayoutPage.GetRestartEventName: string;
begin
    if (FUseRestartEvent.Checked) then
        result := fPossibleRestartEventNamesCombo.Text
    else
        result := '';
end;

procedure TMethodLayoutPage.SetUp(aPreviousPage: TWizardPage);
var
    xNames: TStringArray;
begin
    inherited;

    xNames := TLayoutDataAdaptor.InstReadAllNames();
    TControlUtils.AddValuesToComboBox(xNames, FNameCombo, true);
    if Assigned(fPossibleDisplayComponentNames) then
        TControlUtils.AddValuesToComboBox(fPossibleDisplayComponentNames, fDCNameCombo, true);

    TControlUtils.AddValuesToComboBox(fPossibleRestartEventNames, fPossibleRestartEventNamesCombo, true);
end;

function TMethodLayoutPage.GetUseStandardInit: boolean;
begin
    result := FUseStandardInit.Checked;
end;

function TMethodLayoutPage.GetDeleteRunData: boolean;
begin
    result := FDeleteRunData.Checked;
end;

procedure TMethodLayoutPage.UseLayoutOnClick(Sender: TObject);
begin
    fNameCombo.Visible := FUseLayout.Checked;
    FChooseLayout.Visible := not FUseLayout.Checked;
end;

procedure TMethodLayoutPage.UseDisplayComponentOnClick(Sender: TObject);
begin
    fDCNameCombo.Visible := FUseDisplayComponent.Checked;
end;

procedure TMethodLayoutPage.UseRestartEventOnClick(Sender: TObject);
begin
    fPossibleRestartEventNamesCombo.Visible := FUseRestartEvent.Checked;
end;


end.
