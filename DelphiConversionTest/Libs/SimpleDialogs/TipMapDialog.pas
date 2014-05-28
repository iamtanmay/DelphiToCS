unit TipMapDialog;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : This form shows the tipmap select GUI in a form
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  13.03.07 pk                               TN3633   New
  09.01.08 wl var frmTipMapSelect           TN3972   entfernt
  03.07.08 wl                                         TN4157
  07.10.08 pk EditTipMap                    TN4266    returns false if not mrOk
  28.08.09 pk                               TN4753   references to PipDevice removed
  20.05.10 wl                               TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  07.02.11 wl  position                     TN5461   erscheint jetzt in der Mitte
  10.02.11 wl                               TN5475   TfraTipMapSelect ist TForm statt TFrame
  -------------------------------------------------------------------------------------------------- }


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
    TipMapSelect,
    StdCtrls,
    ExtCtrls,
    AppTypes;

type
    TfrmTipMapDialog = class(TForm)
        pnlMain: TPanel;
        pnlBottom: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    public
        fraTipMapSelect1: TfraTipMapSelect;
        function GetTipMap(): TIPMAP;
        procedure SetTipMap(const aPipDeviceName: string; const aTipCount: integer; const aTipMap: TIPMAP);
        class function EditTipMap(const aPipDeviceName: string; const aTipCount: integer;
            const aTipMap: TIPMAP; out oResultTipMap: TIPMAP): boolean;
    end;


implementation


{$R *.dfm}

uses
    ControlUtils;

procedure TfrmTipMapDialog.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    fraTipMapSelect1 := TfraTipMapSelect.Create(nil);
    fraTipMapSelect1.Parent := pnlMain;
    fraTipMapSelect1.Visible := true;
    fraTipMapSelect1.Align := TAlign.alClient;
    fraTipMapSelect1.Init();
end;

procedure TfrmTipMapDialog.FormDestroy(Sender: TObject);
begin
    FreeAndNil(fraTipMapSelect1);
end;

function TfrmTipMapDialog.GetTipMap: TIPMAP;
begin
    result := fraTipMapSelect1.GetTipMap();
end;

procedure TfrmTipMapDialog.SetTipMap(const aPipDeviceName: string; const aTipCount: integer;
    const aTipMap: TIPMAP);
begin
    fraTipMapSelect1.SetTipMap(aPipDeviceName, aTipCount, aTipMap);
end;

class function TfrmTipMapDialog.EditTipMap(const aPipDeviceName: string; const aTipCount: integer;
    const aTipMap: TIPMAP; out oResultTipMap: TIPMAP): boolean;
var
    xFrmTipMapSelect: TFrmTipMapDialog;
    xModalRes: integer;
begin
    xFrmTipMapSelect := TFrmTipMapDialog.Create(nil);
    try
        xFrmTipMapSelect.SetTipMap(aPipDeviceName, aTipCount, aTipMap);
        xModalRes := xFrmTipMapSelect.ShowModal();
        result := xModalRes = mrOK;
        if not result then
        begin
            oResultTipMap := aTipMap;
            EXIT;
        end;
        oResultTipMap := xFrmTipMapSelect.GetTipMap();
    finally
        xFrmTipMapSelect.Free;
    end;
end;


end.
