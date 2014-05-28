{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : First page of the "Add New Entry"-Wizard
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  13.01.03 wl                               TN1334.1 initial version
  16.01.03 wl                               TN1293.3 verschiedene Änderungen
  29.01.03 wl  GetSectionControl            TN1293.3 FActiveControl abhängig von CheckingAccess
  02.02.03 wl  CheckBeforeNext              TN1293.3 die Felder Section & Ident dürfen nicht leer sein
  11.02.03 wl  Create                       TN1293.3 maxLength der TEdit-Controls wird gesetzt
  04.02.05 wl  SetUp                        TN2307   Allowed Sections werden sortiert gezeigt
  31.08.07 wl                               TN3811.4 uses IniDataWrapper
  17.12.08 wl  CheckBeforeNext              TN4371   Richtige Fehlermeldung statt Access Violation
  07.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  11.11.09 pk  cbSectionChange		       TN4843   call GetSection to get the IniSection object
  20.05.10 wl                               TN5117   uses ControlUtils
  -------------------------------------------------------------------------------------------------- }

unit IniAddEntry;


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
    CommonTypes,
    Wizard,
    IniEntry,
    IniAccess,
    GeneralTypes;

type
    TfraIniAddEntry = class(TWizardPage)
    private
        FIniEntry: TIniEntry;
        FIniAccess: TIniAccess;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        edIdent: TEdit;
        edSection: TEdit;
        edArea: TEdit;
        cbSection: TComboBox;
        FActiveControl: TWinControl;
        procedure cbSectionChange(Sender: TObject);
        function GetSectionControl: TWinControl;
    public
        // constructor
        constructor Create(aOwner: TComponent); override;
        // methods derived from TWizardPage
        function GetHeaderTitle: string; override;
        function GetHeaderText(): TStringArray; override;
        function CheckBeforeNext: boolean; override;
        function GetActiveControl: TWinControl; override;
        procedure SetUp(aPreviousPage: TWizardPage); override;
        // properties
        property IniEntry: TIniEntry read FIniEntry;
        property IniAccess: TIniAccess read FIniAccess write FIniAccess;
    end;


implementation


uses
    IniSection,
    IniDataWrapper,
    DialogUtils,
    ControlUtils;

constructor TfraIniAddEntry.Create(aOwner: TComponent);
begin
    inherited Create(aOwner);

    AddDistance(wzdXSmall);

    Label1 := AddLabel(TLanguageString.Read('Area:', 'Bereich:'), INT_WIZARD_LEFT_1);
    edArea := AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    edArea.Enabled := false;

    AddDistance(wzdXLarge);

    Label2 := AddLabel(TLanguageString.Read('Section:', 'Sektion:'), INT_WIZARD_LEFT_1);
    cbSection := AddComboBox(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    cbSection.OnChange := cbSectionChange;
    cbSection.Style := csDropDownList;
    edSection := AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT - 18);
    edSection.MaxLength := TIniDataWrapper.GetFieldLength_Section;

    AddDistance(wzdXLarge);

    Label3 := AddLabel(TLanguageString.Read('Ident:', 'Identifizierer:'), INT_WIZARD_LEFT_1);
    edIdent := AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    edIdent.MaxLength := TIniDataWrapper.GetFieldLength_Ident;
end;

function TfraIniAddEntry.GetHeaderTitle: string;
begin
    result := TLanguageString.Read('Add a setting entry', 'Hinzufügen eines Eintrags');
end;

function TfraIniAddEntry.GetHeaderText(): TStringArray;
begin
    result := GetSingleTextAsHeader(TLanguageString.Read('You must define a section and an ident name',
        'Sie müssen eine Sektion und einen Identifizierer eingeben'));
end;

function TfraIniAddEntry.CheckBeforeNext: boolean;
begin
    FActiveControl := GetSectionControl;
    result := false;

    if (edSection.Text = '') then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Enter a section name!',
            'Geben Sie einen Namen für die Sektion ein!'), 'Info', 64);
        exit;
    end;

    if (edIdent.Text = '') then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Enter an ident name!',
            'Geben Sie einen Namen für den Identifizierer ein!'), 'Info', 64);
        FActiveControl := edIdent;
        exit;
    end;

    FIniEntry := FIniAccess.GetItemByIdent(edSection.Text, edIdent.Text, false);
    if (FIniEntry <> nil) and (FIniEntry.ValueExists) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Ident {0} already exists!',
            'Der Identifizierer {0} existiert schon!', [FIniEntry.Ident]), 'Info', 64);
        FActiveControl := edIdent;
        exit;
    end;

    FIniEntry := FIniAccess.GetItemByIdent(edSection.Text, edIdent.Text, true);
    if (FIniEntry = nil) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('This entry is not allowed!',
            'Dieser Eintrag ist nicht erlaubt!'), 'Info', 64);
        exit;
    end;

    result := true;
end;

procedure TfraIniAddEntry.SetUp(aPreviousPage: TWizardPage);
var
    xSectionNames: TStringArray;
begin
    inherited SetUp(aPreviousPage);

    edArea.Text := FIniAccess.FileName;
    if (FIniAccess is TCheckingIniAccess) then
    begin
        xSectionNames := (FIniAccess as TCheckingIniAccess).GetAllowedSections();
        TControlUtils.AddValuesToComboBox(xSectionNames, cbSection, true);

        cbSection.Visible := true;
        cbSection.Sorted := true;
        edSection.Width := INT_WIZARD_WIDTH_EDIT - 18;
    end
    else
    begin
        cbSection.Visible := false;
        edSection.Width := INT_WIZARD_WIDTH_EDIT;
    end;
    FActiveControl := GetSectionControl;
end;

procedure TfraIniAddEntry.cbSectionChange(Sender: TObject);
var
    xIniSection: TIniSection;
begin
    xIniSection := (FIniAccess as TCheckingIniAccess).GetSection(cbSection.Items[cbSection.ItemIndex]);
    if not Assigned(xIniSection) then
        EXIT;

    edSection.Text := xIniSection.Section;
    edIdent.Text := xIniSection.GetNextPossibleIdent;

end;

function TfraIniAddEntry.GetActiveControl: TWinControl;
begin
    result := FActiveControl;
end;

function TfraIniAddEntry.GetSectionControl: TWinControl;
begin
    if (FIniAccess is TCheckingIniAccess) then
        result := cbSection
    else
        result := edSection;
end;


end.
