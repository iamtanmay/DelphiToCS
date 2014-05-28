{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Project      : ZACommon.dll
  Description  : Dialog to change a user's password
  --------------------------------------------------------------------------------------------------
  Revision History:
  Date     op  Method                       Track-no Improvement/Change
  -------- --  ---------------------------  -------- -----------------------------------------------
  04.01.03 wl                               TN1334.1 initial version
  09.01.03 wl  CreateForm                   TN1334.1 Übergabe von NoEmptyPassword
  09.01.03 wl  Button1Click                 TN1334.1 Bei fehlerhafter Eingabe kein mrOK
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  19.08.09 wl  fStringLoader                TN4702   fStringLoader lädt Strings für Dialog-Elemente
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  -------------------------------------------------------------------------------------------------- }

unit UserChangePassword;


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
    TUserChangePasswordStringLoader = class(TTextListStringLoader)
    protected
        procedure AddAllItems(); override;
    end;

    TfrmUserChangePassword = class(TForm)
        Button1: TButton;
        Button2: TButton;
        Label1: TLabel;
        Edit1: TEdit;
        Label2: TLabel;
        Edit2: TEdit;
        Label3: TLabel;
        stxUserName: TStaticText;
        procedure Button1Click(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        FNoEmptyPassword: boolean;
        fStringLoader: TUserChangePasswordStringLoader;
    public
        constructor CreateForm(aOwner: TComponent; aNoEmptyPassword: boolean; aUserName: string);
    end;


implementation


{$R *.DFM}

uses
    ControlUtils,
    DialogUtils,
    GeneralTypes;

{ TUserChangePasswordStringLoader }

procedure TUserChangePasswordStringLoader.AddAllItems;
begin
    AddSingle(510, '&OK', '&OK');
    AddSingle(520, '&Cancel', '&Abbrechen');
    AddSingle(12200, 'Change Password', 'Kennwort ändern');
    AddSingle(12210, 'Please enter new password', 'Bitte geben Sie ein neues Kennwort ein');
    AddSingle(12220, 'Please confirm new password', 'Bitte bestätigen Sie das neue Kennwort');
    AddSingle(12230, 'User Name:', 'Benutzername:');
end;

{ TfrmUserChangePassword }

constructor TfrmUserChangePassword.CreateForm(aOwner: TComponent; aNoEmptyPassword: boolean;
    aUserName: string);
begin
    inherited Create(aOwner);

    TControlUtils.ResetFontForWinXP(self);
    FNoEmptyPassword := aNoEmptyPassword;
    stxUserName.Caption := aUserName;
    fStringLoader := TUserChangePasswordStringLoader.Create;
    fStringLoader.LoadLanguage(self);
end;

procedure TfrmUserChangePassword.FormDestroy(Sender: TObject);
begin
    fStringLoader.Free;
end;

procedure TfrmUserChangePassword.Button1Click(Sender: TObject);
begin
    if (Edit1.Text <> Edit2.Text) then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('Password confirmation is not correct!',
            'Das Kennwort wurde nicht korrekt bestätigt!'), TLanguageString.Read('Change Password',
            'Kennwort ändern'), 16);
        Edit1.Text := '';
        Edit2.Text := '';
        ActiveControl := Edit1;
        exit;
    end;
    if (FNoEmptyPassword) and (Edit1.Text = '') then
    begin
        TDialogUtils.MessageBox(TLanguageString.Read('It is not allowed to leave the password blank',
            'Es ist nicht erlaubt, kein Kennwort einzugeben'), TLanguageString.Read('Change Password',
            'Kennwort ändern'), 16);
        Edit2.Text := '';
        ActiveControl := Edit1;
        exit;
    end;
    ModalResult := mrOK
end;


end.
