{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : User Interface for CFR21 Request for reason
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.04.07 wl                                TN3669  Initial Revision
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  -------------------------------------------------------------------------------------------------- }

unit UserConfirmation;


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
    TfrmUserConfirmation = class(TForm)
        Label1: TLabel;
        Label2: TLabel;
        ComboBox1: TComboBox;
        btnOK: TButton;
        btnCancel: TButton;
        Label3: TLabel;
        stxUserName: TStaticText;
        Memo1: TMemo;
        procedure ComboBox1Change(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    end;


implementation


{$R *.dfm}

uses
    ControlUtils;

procedure TfrmUserConfirmation.ComboBox1Change(Sender: TObject);
begin
    // Reason must be at least 4 characters
    btnOK.Enabled := Length(ComboBox1.Text) >= 4;
end;

procedure TfrmUserConfirmation.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;


end.
