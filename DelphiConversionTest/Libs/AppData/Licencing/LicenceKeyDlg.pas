{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : Dialog to enter the licence key
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.01.03 wl                               TN1334.1 initial version
  19.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  17.08.10 wl                               TN5112   komplett neu gestaltet
  29.09.10 wl                               TN5112   div. Änderungen
  12.11.10 wl                               TN5112   Cancel geht jetzt
  17.01.11 wl                               TN5112   interne Änderungen
  22.03.13 pp                               TN6111   Store Licence Button entfernt (geht jetzt über "Prüfe Key" - automatisch)
  -------------------------------------------------------------------------------------------------- }

unit LicenceKeyDlg;

interface

uses
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  ExtCtrls,
  Forms;

type
  TfrmLicenceKeyDialog = class(TForm)
    Button1: TButton;
    Button3: TButton;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    btnGetPCKey: TButton;
    edLicenseKey: TEdit;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnGetPCKeyClick(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses
  Registry,
  ControlUtils,
  LicenseGetPCKey,
  GeneralTypes,
  LicenseKeyUtils,
  DialogUtils;

procedure TfrmLicenceKeyDialog.btnGetPCKeyClick(Sender: TObject);
var
  xfrm: TfrmLicenseGetPCKey;
  xPCKey: string;
begin
  xPCKey := TLicenseKeyUtils.GetPCKey();
  if xPCKey = '' then
  begin
    TDialogUtils.MessageBox('Please connect your LAN adaptor to a network',
      'No PC Key');
    EXIT;
  end;

  xfrm := TfrmLicenseGetPCKey.Create(nil);
  try
    xfrm.Edit1.Text := xPCKey;
    xfrm.ShowModal;
  finally
    FreeAndNil(xfrm);
  end;
end;

procedure TfrmLicenceKeyDialog.FormCreate(Sender: TObject);
begin
  TControlUtils.ResetFontForWinXP(self);
  self.Caption := TLanguageString.Read('Please enter Product Key',
    'Bitte Seriennummer eingeben');
  Label2.Caption := TLanguageString.Read('1. Get your personal PC Key:',
    '1. Get your personal PC Key:');
  Label3.Caption := TLanguageString.Read('2. Send PC Key to Zinsser Analytic',
    '2. Send PC Key to Zinsser Analytic');
  Label4.Caption := TLanguageString.
    Read('3. Receive License Key from Zinsser Analytic',
    '3. Receive License Key from Zinsser Analytic');
  Label5.Caption := TLanguageString.Read('4. Enter License key:',
    '4. Enter License key:');
  Button1.Caption := TLanguageString.Read('&Check Key', '&Key prüfen');
  Button2.Caption := TLanguageString.Read('&Abort', '&Abbrechen');
  Button3.Caption := TLanguageString.Read('Demo Mode', 'Demo-Modus');
end;

end.
