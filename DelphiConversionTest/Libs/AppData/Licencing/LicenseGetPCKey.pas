{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.08.10 wl                                        TN5112   initial revision
  ----------------------------------------------------------------------------------------------------------------------- }

unit LicenseGetPCKey;


interface


uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls;

type
    TfrmLicenseGetPCKey = class(TForm)
        Edit1: TEdit;
        Label1: TLabel;
        Button1: TButton;
        procedure Button1Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    end;


implementation


{$R *.dfm}

uses
    ControlUtils,
    GeneralTypes;

procedure TfrmLicenseGetPCKey.Button1Click(Sender: TObject);
begin
    Close;
end;

procedure TfrmLicenseGetPCKey.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    self.Caption := TLanguageString.Read('Please enter Product Key', 'Bitte Seriennummer eingeben');
end;


end.
