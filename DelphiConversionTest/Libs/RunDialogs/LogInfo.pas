unit LogInfo;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- --------------------------------------------------------------------
  06.04.09 pk                                TN4503  Initial Revision
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
    ComCtrls,
    Grids,
    ExtCtrls;

type
    TfrmLogInfo = class(TForm)
        MainLogDisplay: TMemo;
        procedure FormCreate(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    frmLogInfo: TfrmLogInfo;


implementation


{$R *.dfm}

uses
    ControlUtils;

{ TfrmLogInfo }

procedure TfrmLogInfo.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
end;


end.
