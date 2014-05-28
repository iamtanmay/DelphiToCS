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
  03.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }

unit UserAddNew2;


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
    CommonTypes,
    GeneralTypes;

type
    TfraUserAddNew2 = class(TWizardPage)
    private
        FActiveControl: TWinControl;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        RadioButton1: TRadioButton;
        RadioButton2: TRadioButton;
        RadioButton3: TRadioButton;
        RadioButton4: TRadioButton;
        procedure RadioButtonClick(Sender: TObject);
    public
        // constructor
        constructor Create(aOwner: TComponent); override;
        // methods derived from TWizardPage
        function GetHeaderTitle: string; override;
        function GetHeaderText(): TStringArray; override;
        function CheckBeforeNext: boolean; override;
        function GetActiveControl: TWinControl; override;
        // individual result functions
        function GetUserLevel: TUserLevel;
    end;


implementation


constructor TfraUserAddNew2.Create(aOwner: TComponent);
begin
    inherited Create(aOwner);

    RadioButton1 := AddRadioButton(TLanguageString.Read('Default System User',
        'Standardbenutzer des Systems'), INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1, true);
    AddDistance(wzdSmall);
    Label1 := AddLabel(TLanguageString.
        Read('Can read settings and methods, but cannot write them. Can run the system',
        'Kann das System bedienen, aber keine Einstellungen oder Methoden ändern'), INT_WIZARD_LEFT_2);
    AddDistance(wzdMedium);

    RadioButton2 := AddRadioButton(TLanguageString.Read('Guest', 'Gastzugang'), INT_WIZARD_LEFT_1,
        INT_WIZARD_WIDTH_1, true);
    AddDistance(wzdSmall);
    Label2 := AddLabel(TLanguageString.Read('Restricted reading access to data',
        'Eingeschränkter lesender Zugriff auf Daten'), INT_WIZARD_LEFT_2);
    AddDistance(wzdMedium);

    RadioButton3 := AddRadioButton(TLanguageString.Read('System Administrator', 'Systemadministrator'),
        INT_WIZARD_LEFT_1, INT_WIZARD_WIDTH_1, true);
    AddDistance(wzdSmall);
    Label3 := AddLabel(TLanguageString.
        Read('Can read and write everything, but cannot write the user management settings',
        'Benutzer der alle Einstellungen verändern kann außer der Benutzerverwaltung'), INT_WIZARD_LEFT_2);
    AddDistance(wzdMedium);

    RadioButton4 := AddRadioButton(TLanguageString.Read('Supervisor', 'Supervisor'), INT_WIZARD_LEFT_1,
        INT_WIZARD_WIDTH_1, true);
    AddDistance(wzdSmall);
    Label4 := AddLabel(TLanguageString.
        Read('Administrator for the user management, but Guest user for the rest of the system',
        'Administrator für Benutzermanagement, Gastbenutzer für das System'), INT_WIZARD_LEFT_2);

    // Event: OnClick
    RadioButton1.OnClick := RadioButtonClick;
    RadioButton2.OnClick := RadioButtonClick;
    RadioButton3.OnClick := RadioButtonClick;
    RadioButton4.OnClick := RadioButtonClick;

    RadioButton1.Checked := true;
    FActiveControl := RadioButton1;
end;

function TfraUserAddNew2.GetHeaderTitle: string;
begin
    result := TLanguageString.Read('Define the user level', 'Definition der Zugriffsstufe');
end;

function TfraUserAddNew2.GetHeaderText(): TStringArray;
begin
    result := self.GetSingleTextAsHeader(TLanguageString.
        Read('Which access level should be assigned to the user?',
        'Welche Zugriffsstufe soll dem Benutzer zugewiesen werden?'));
end;

function TfraUserAddNew2.CheckBeforeNext: boolean;
begin
    result := true;
end;

function TfraUserAddNew2.GetActiveControl: TWinControl;
begin
    result := FActiveControl;
end;

procedure TfraUserAddNew2.RadioButtonClick(Sender: TObject);
begin
    FActiveControl := Sender as TWinControl;
end;

function TfraUserAddNew2.GetUserLevel: TUserLevel;
begin
    result := usrNothing;
    if RadioButton1.Checked then
        result := usrSystem;
    if RadioButton2.Checked then
        result := usrGuest;
    if RadioButton3.Checked then
        result := usrSystemAdmin;
    if RadioButton4.Checked then
        result := usrSupervisor;
end;


end.
