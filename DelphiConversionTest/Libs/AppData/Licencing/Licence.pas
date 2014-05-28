{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : Licence object has access to Hardlock and checks the licence key (serial number)
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.01.03 wl                               TN1334.1 initial version
  08.01.03 wl                               TN1334.1 komplett überarbeitet
  08.01.03 wl  property DemoMode            TN1334.1 neu
  09.01.03 wl  GetLicenceParts              TN1334.1 wenn an 5.Stelle 'B' oder 'C': CFR21 compliant!
  10.01.03 wl                               TN1334.1 angepasst an Änderungen in Hardlock.pas
  19.02.03 mo                               TN1348   neu Moss Licence
  17.04.03 wl  IsCFRLicence                 TN1334.1 prüft nur auf CFR-Lizenz
  14.07.03 wl  Create                       TN1501   AppMode (Simulation wird als Parameter übergeben)
  08.03.04 pk  Create                       TN1792   Do not call CheckKey or UserEntersKey in constructor
  08.03.04 pk  UserEntersKeyIfNotValid      TN1792   New
  07.04.05 pk  TLicence.GetLicenceParts     TN2375   Get Scheduler licence
  27.08.07 wl  TLicencePart                 TN3811.4 Enum mit Integer-Zuordnung
  27.08.07 wl  LicencePartValid             TN3811.4 vereinfachte Prüfung von Lizenzen
  31.08.07 wl  gLicence                     TN3811.4  --> AppInstanceLicencing
  07.08.09 wl                               TN4702   Strings werden direkt geladen
  20.04.10 ts  licTraySy                    TN5067   new
  17.08.10 wl                               TN5112   Hardlock-Teile entfernt
  29.09.10 wl                               TN5112   div. Änderungen
  12.11.10 wl  ChangeStaffLicenseParts      TN5112   Neu: Staff Licence
  26.07.11 ts  UserEntersKey                TN5606   appModeUserAbort, if user closes the licence window (Abort)
  22.03.13 pp  Store License Key            TN6111   License Key saved on button click automatically
  14.06.13 pp  if um Store Licence Key      TN6174   Musste mit if umschlossen werden, da im Demo Mod keine Datei erstellt werden soll (Methode: UserEntersKey)
  15.07.13 pp  UserEntersKey                TN6203   If musste entfernt werden, da es im ZADesigner nicht ging. try except um StoreLicenceKey geschrieben
  -------------------------------------------------------------------------------------------------- }

unit Licence;


interface


uses
    LicenseKeyUtils,
    CommonTypes,
    Dialogs;

type
    TLicence = class
    private
        fKey: string;
        fParts: cardinal;
        fKeyValid: boolean;
        fKeyErrorMessage: string;
        fAppMode: TAppMode;
        procedure UserEntersKey;
    public
        // constructor
        constructor Create(aAppMode: TAppMode);
        // public methods
        function LicencePartValid(aLicensePart: TLicensePart): boolean;
        function ChangeStaffLicenseParts(aKey: string): boolean;
        // properties
        property Key: string read fKey;
        property AppMode: TAppMode read fAppMode;
    end;


implementation


uses
    Controls,
    Forms,
    DialogUtils,
    GeneralTypes,
    LicenceKeyDlg;

{ TLicence }

constructor TLicence.Create(aAppMode: TAppMode);
var
    xLicenseRec: TLicenseRec;
begin
    inherited Create;

    fAppMode := aAppMode;

    fKey := TLicenseKeyUtils.ReadLicenseKey();
    fKeyValid := TLicenseKeyUtils.CheckLicenseKey(fKey, aAppMode = appModeReal, fKeyErrorMessage);

    xLicenseRec := TLicenseKeyUtils.GetLicenseKeyOptions(fKey);
    fParts := xLicenseRec.Parts;

    if (not fKeyValid) then
        self.UserEntersKey;
end;

procedure TLicence.UserEntersKey;
var
    xFrmKeyDlg: TfrmLicenceKeyDialog;
begin
    xFrmKeyDlg := TfrmLicenceKeyDialog.Create(nil);
    xFrmKeyDlg.edLicenseKey.Text := fKey;
    try
        repeat
            // Get Serial-No from Input Dialog
            case xFrmKeyDlg.ShowModal() of
                mrOK:
                    begin // User selects 'OK'
                        fKey := xFrmKeyDlg.edLicenseKey.Text;

                        // Check new key
                        fKeyValid := TLicenseKeyUtils.CheckLicenseKey(fKey, fAppMode = appModeReal,
                            fKeyErrorMessage);

                        if (not fKeyValid) then
                            TDialogUtils.MessageBox(fKeyErrorMessage, TLanguageString.Read('Check licence',
                                'Lizenz überprüfen'), 16);
                        try
                            // Store License Key
                            TLicenseKeyUtils.StoreLicenseKey(xFrmKeyDlg.edLicenseKey.Text);
                        except
                            ShowMessage(TLanguageString.
                                read('Can not write license file, try with admin rights!',
                                'Lizenzdatei kann nicht geschrieben werden, bitte als Admin ausführen!'));
                        end;
                    end;
                mrIgnore:
                    begin // User selects 'demo mode' (mrIgnore)
                        fAppMode := appModeSim;
                        BREAK;
                    end;
                else
                    begin // User selects 'Cancel'
                        fAppMode := appModeUserAbort;
                        Application.Terminate;
                        EXIT;
                    end;
            end;

        until (fKeyValid);
    finally
        xFrmKeyDlg.Free;
    end;
end;

function TLicence.ChangeStaffLicenseParts(aKey: string): boolean;
var
    xLicenseRec: TLicenseRec;
    xRunSystem: boolean;
begin
    // Spezialfunktion für Mitarbeiterlizenz:
    // Diese Lizenz bezieht ihre Lizenzteile aus den jeweiligen Daten, sorgt also dafür, dass
    // die Daten immer wie beim Kunden dargestellt werden

    if not self.LicencePartValid(licStaffLicense) then
        EXIT(false);

    xLicenseRec := TLicenseKeyUtils.GetLicenseKeyOptions(aKey);

    // RunSystem & StaffLicense bleiben erhalten, alles andere wird ersetzt:
    xRunSystem := self.LicencePartValid(licRunSystem);
    fParts := xLicenseRec.Parts or cardinal(licStaffLicense);
    if xRunSystem then
        fParts := fParts or cardinal(licRunSystem)
    else
        fParts := fParts and not cardinal(licRunSystem);

    EXIT(true);
end;

{
  function TLicence.GetLicenceParts: integer;
  begin
  result := 0;

  // Run-System-Licence
  if (FKeyValid) then result := result + Integer( licRunSystem );

  // Symyx-Licence
  if (Copy(FKey,8,1) = 'S') then result := result or Integer( TLicensePart.cScheduler );

  // Symyx-TLicensePart.cence
  if (Copy(FKey,7,1) = 'S') then result := result or Integer( TLicensePart.cSymyx );

  // Redi & Sophas-Licence
  if (Copy(FKey,6,1) = 'A') then result := result or Integer( TLicensePart.cRedi );
  if (Copy(FKey,6,1) = 'B') then result := result or Integer( TLicensePart.cSophas );
  if (Copy(FKey,6,1) = 'C') then result := result or Integer( TLicensePart.cSophas ) or Integer( TLicensePart.cRedi );
  // Moss-Licence
  if (Copy(FKey,6,1) = 'M') then result := result or Integer( TLicensePart.cMoss );
  // TraySy-Licence
  if (Copy(FKey,6,1) = 'T') then result := result or Integer( TLicensePart.cTraySy );


  // SAMI-Licence & CFR21 Part 11 Compliance
  if (Copy(FKey,5,1) = 'A') then result := result or Integer( TLicensePart.cSami );
  if (Copy(FKey,5,1) = 'B') then result := result or Integer( TLicensePart.cCFR21Compliant );
  if (Copy(FKey,5,1) = 'C') then result := result or Integer( TLicensePart.cSami ) or Integer( TLicensePart.cCFR21Compliant );

  end;
}

function TLicence.LicencePartValid(aLicensePart: TLicensePart): boolean;
begin
    result := TLicenseKeyUtils.LicensePartValid(fParts, aLicensePart);
end;


end.
