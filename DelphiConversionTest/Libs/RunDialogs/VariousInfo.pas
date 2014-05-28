unit VariousInfo;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl), Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision - Code from ZARunnerMain
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  ----------------------------------------------------------------------------------------------------------------------- }


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
    StdCtrls,
    Buttons;

type
    TfrmVariousInfo = class(TForm)
        btnStop: TBitBtn;
        procedure FormCreate(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    frmVariousInfo: TfrmVariousInfo;


implementation


{$R *.dfm}

uses
    ControlUtils;

procedure TfrmVariousInfo.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;


end.
