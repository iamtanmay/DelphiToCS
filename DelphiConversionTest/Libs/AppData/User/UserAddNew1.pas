{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : First page of the "Add New User"-Wizard
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  08.01.03 wl                               TN1334.1 initial version
  09.01.03 wl  TWizardPage                  TN1334.1 derived from TPanel
  09.01.03 wl                               TN1334.1 new: constant values for Left, Width, Top
  10.01.03 wl  Create                       TN1334.1 uses AddLabel, AddDistance, ..
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }

unit UserAddNew1;


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
    GeneralTypes;

type
    TfraUserAddNew1 = class(TWizardPage)
    private
        edDescription: TEdit;
        edName: TEdit;
        Label1: TLabel;
        edPW1: TEdit;
        Label2: TLabel;
        edPW2: TEdit;
        Label3: TLabel;
        Label4: TLabel;
        FActiveControl: TWinControl;
    public
        // constructor
        constructor Create(aOwner: TComponent); override;
        // methods derived from TWizardPage
        function GetHeaderTitle: string; override;
        function GetHeaderText(): TStringArray; override;
        function CheckBeforeNext: boolean; override;
        function GetActiveControl: TWinControl; override;
        // individual result functions
        function GetDescription: string;
        function GetName: string;
        function GetPassword: string;
    end;


implementation


uses
    DialogUtils;

constructor TfraUserAddNew1.Create(aOwner: TComponent);
begin
    inherited Create(aOwner);

    AddDistance(wzdXSmall);

    Label1 := AddLabel(TLanguageString.Read('User name:', 'Benutzername:'), INT_WIZARD_LEFT_1);
    edName := AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    AddDistance(wzdXLarge);

    Label2 := AddLabel(TLanguageString.Read('Description:', 'Beschreibung:'), INT_WIZARD_LEFT_1);
    edDescription := AddEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    AddDistance(wzdXLarge);

    Label3 := AddLabel(TLanguageString.Read('Password:', 'Kennwort:'), INT_WIZARD_LEFT_1);
    edPW1 := AddPasswordEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);
    AddDistance(wzdMedium);

    Label4 := AddLabel(TLanguageString.Read('Confirm password:', 'Kennwortbestätigung:'), INT_WIZARD_LEFT_1);
    edPW2 := AddPasswordEdit(INT_WIZARD_LEFT_EDIT, INT_WIZARD_WIDTH_EDIT);

    FActiveControl := edName;
end;

function TfraUserAddNew1.GetHeaderTitle: string;
begin
    result := TLanguageString.Read('Define user name and password',
        'Eingabe des Benutzernamens und -kennworts');
end;

function TfraUserAddNew1.GetHeaderText(): TStringArray;
begin
    SetLength(result, 2);
    result[0] := TLanguageString.Read('1: Enter user name', '1: Benutzernamen eingeben');
    result[1] := TLanguageString.Read('2: Enter user password (2 times)', '2: Kennwort eingeben (zweimal)');
end;

function TfraUserAddNew1.CheckBeforeNext: boolean;
begin
    FActiveControl := edName;
    result := false;

    if (edName.Text = '') then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Please enter a user name',
            'Bitte geben Sie einen Benutzernamen ein'), 'Info', 64);
        exit;
    end;

    if (edPW1.Text <> edPW2.Text) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Password confirmation is not correct',
            'Die eingegebenen Kennwörter stimmen nicht überein'), 'Info', 16);
        edPW1.Text := '';
        edPW2.Text := '';
        FActiveControl := edPW1;
        exit;
    end;

    if (edPW1.Text = '') then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('You must enter a password',
            'Sie müssen ein Kennwort eingeben'), 'Info', 64);
        edPW2.Text := '';
        FActiveControl := edPW1;
        exit;
    end;
    result := true;
end;

function TfraUserAddNew1.GetActiveControl: TWinControl;
begin
    result := FActiveControl;
end;

function TfraUserAddNew1.GetDescription: string;
begin
    result := edDescription.Text;
end;

function TfraUserAddNew1.GetName: string;
begin
    result := edName.Text;
end;

function TfraUserAddNew1.GetPassword: string;
begin
    result := edPW1.Text;
end;


end.
