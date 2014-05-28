{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : page of the "Edit Value Entry"-Wizard
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.01.03 wl                               TN1293.3 initial version
  11.02.03 wl  Create                       TN1293.3 maxLength der TEdit-Controls wird gesetzt
  31.08.07 wl                               TN3811.4 uses IniDataWrapper
  03.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  09.02.10 pk                               TN4973   UnitName changed to ValueUnitName to avoid overwriting TObject.UnitName
  27.10.10 wl                               TN5300   Eigenschaft ValueHidden wird bei der Anzeige mit Sternchen angezeigt
  -------------------------------------------------------------------------------------------------- }

unit IniEditValue;


interface


uses
    Classes,
    Controls,
    StdCtrls,
    GeneralTypes,
    CommonTypes,
    Wizard,
    IniEntry;

type
    TfraIniEditValue = class(TWizardPage)
    private
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        edIdent: TStaticText;
        edSection: TStaticText;
        edArea: TStaticText;
        edValue: TEdit;
        edDefault: TStaticText;
        edUnit1: TStaticText;
        edUnit2: TStaticText;
        edDescr: TStaticText;
        FIniEntry: TIniEntry;
        procedure SetArea(aValue: string);
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
        property IniEntry: TIniEntry read FIniEntry write FIniEntry;
        property Area: string write SetArea;
    end;


    // ##################################################################################################


implementation


uses
    SysUtils,
    Dialogs,
    IniAddEntry,
    IniDataWrapper;

const
    INT_WIDTH_UNIT = 33;

constructor TfraIniEditValue.Create(aOwner: TComponent);
begin
    inherited Create(aOwner);

    Label1 := AddLabel(TLanguageString.Read('Area:', 'Bereich:'), INT_WIZARD_LEFT_1);
    edArea := AddStaticText(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT + INT_WIDTH_UNIT);
    AddDistance(wzdSmall);
    Label2 := AddLabel(TLanguageString.Read('Section:', 'Sektion:'), INT_WIZARD_LEFT_1);
    edSection := AddStaticText(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT + INT_WIDTH_UNIT);
    AddDistance(wzdSmall);
    Label3 := AddLabel(TLanguageString.Read('Ident:', 'Identifizierer:'), INT_WIZARD_LEFT_1);
    edIdent := AddStaticText(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT + INT_WIDTH_UNIT);
    AddDistance(wzdMedium);

    edDescr := AddStaticText(INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_EDIT + INT_WIDTH_UNIT + INT_WIZARD_LEFT_EDIT
        - INT_WIZARD_LEFT_1);
    edDescr.Height := 48;
    AddDistance(wzdXXLarge);

    Label4 := AddLabel(TLanguageString.Read('Value:', 'Wert:'), INT_WIZARD_LEFT_1);
    edValue := AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    edValue.MaxLength := TIniDataWrapper.GetFieldLength_Value;
    edUnit1 := AddStaticText(INT_WIZARD_LEFT_EDIT + INT_WIZARD_WIDTH_EDIT + 3, INT_WIDTH_UNIT - 3);
    edUnit1.Height := 21;
    AddDistance(wzdMedium);

    Label5 := AddLabel(TLanguageString.Read('Default Value:', 'Vorgabewert:'), INT_WIZARD_LEFT_1);
    edDefault := AddStaticText(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    edDefault.Height := 21;
    edUnit2 := AddStaticText(INT_WIZARD_LEFT_EDIT + INT_WIZARD_WIDTH_EDIT + 3, INT_WIDTH_UNIT - 3);
    edUnit2.Height := 21;
end;

function TfraIniEditValue.GetHeaderTitle: string;
begin
    result := TLanguageString.Read('Enter a value for the setting entry',
        'Geben Sie einen Wert für den Eintrag an');
end;

function TfraIniEditValue.GetHeaderText(): TStringArray;
begin
    result := self.GetSingleTextAsHeader(TLanguageString.Read('Please enter a value.',
        'Bitte geben Sie einen Wert ein.'));
end;

function TfraIniEditValue.CheckBeforeNext: boolean;
begin
    result := false;
    try
        FIniEntry.Value := edValue.Text;
        FIniEntry.ValueExists := true;
    except
        on E: Exception do
        begin
            ShowMessage(E.Message);
            exit;
        end;
    end;

    result := true;
end;

procedure TfraIniEditValue.SetUp(aPreviousPage: TWizardPage);
begin
    { if (aPreviousPage is TfraIniAddEntry) then begin
      FIniEntry := (aPreviousPage as TfraIniAddEntry).IniEntry;
      SetArea((aPreviousPage as TfraIniAddEntry).GetArea);
      end;
    }
    if (FIniEntry <> nil) then
    begin
        edSection.Caption := FIniEntry.Section;
        edIdent.Caption := FIniEntry.Ident;
        edValue.Text := FIniEntry.Value;
        edDefault.Caption := FIniEntry.DefaultValue;
        edUnit1.Caption := FIniEntry.ValueUnitName;
        edUnit2.Caption := FIniEntry.ValueUnitName;

        if (FIniEntry is TSingleIniEntry) then
            edDescr.Caption := (FIniEntry as TSingleIniEntry).GetExtDescription
        else
            edDescr.Caption := FIniEntry.Description;

        if FIniEntry.ValueHidden then
            edValue.PasswordChar := '*';

    end;
end;

function TfraIniEditValue.GetActiveControl: TWinControl;
begin
    result := edValue;
end;

procedure TfraIniEditValue.SetArea(aValue: string);
begin
    edArea.Caption := aValue;
end;


end.
