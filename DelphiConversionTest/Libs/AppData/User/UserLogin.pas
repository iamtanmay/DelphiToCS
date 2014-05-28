{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : Dialog to log in a user
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.01.03 wl                               TN1334.1 initial version
  10.03.03 wl                               TN1334.7 optische Änderungen (für deutsche Spracheinstellung)
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  19.08.09 wl  fStringLoader                TN4702   fStringLoader lädt Strings für Dialog-Elemente
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  -------------------------------------------------------------------------------------------------- }

unit UserLogin;


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
    Forms,
    StringLoader;

type
    TUserLoginStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmUserLogin = class(TForm)
        btnOk: TButton;
        btnAbort: TButton;
        edUsername: TEdit;
        edPassword: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        procedure FormShow(Sender: TObject);
        procedure btnAbortClick(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        FShowMessage: boolean;
        fStringLoader: TUserLoginStringLoader;
    public
        constructor CreateForm(aOwner: TComponent; aShowMessage: boolean);
    end;


implementation


{$R *.DFM}

uses
    ControlUtils,
    DialogUtils,
    GeneralTypes;

{ TUserLoginStringLoader }

procedure TUserLoginStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(530, '&Abort', '&Abbrechen');
    AddSingle(12000, 'User Login', 'Benutzeranmeldung');
    AddSingle(12010, 'Please enter a  username  and a password that is valid for this system',
        'Bitte einen für dieses System gültigen Namen und ein Kennwort eingeben');
    AddSingle(12020, 'User name:', 'Benutzername:');
    AddSingle(12030, 'Password:', 'Kennwort:');
end;

{ TfrmUserLogin }

constructor TfrmUserLogin.CreateForm(aOwner: TComponent; aShowMessage: boolean);
begin
    inherited Create(aOwner);

    TControlUtils.ResetFontForWinXP(self);
    FShowMessage := aShowMessage;
    fStringLoader := TUserLoginStringLoader.Create;
    fStringLoader.LoadLanguage(self);
end;

procedure TfrmUserLogin.FormShow(Sender: TObject);
begin
    if edUserName.Text <> '' then
        ActiveControl := edPassword;
end;

procedure TfrmUserLogin.btnAbortClick(Sender: TObject);
begin
    if (FShowMessage) then
    begin
        if (TDialogUtils.MessageBox(TLanguageString.Read('Abort will terminate the software - Continue?',
            'Abbbrechen wird die Software beenden - Vorgang fortsetzen?'), '', MB_YESNO) = IDYES) then
        begin
            ModalResult := mrAbort;
        end;
    end
    else
        ModalResult := mrAbort;
end;

procedure TfrmUserLogin.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;


end.
