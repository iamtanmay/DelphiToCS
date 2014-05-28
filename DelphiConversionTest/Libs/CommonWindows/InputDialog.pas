{ ------------------------------------------------------------------------------------------------------------
  BASEUNIT!
  ---------
  Copyright  2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Replacement for InputBox/InputQuery functions
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  09.04.10 wl                                    TN5044   initial revision
  21.07.10 wl                                    TN5202   neue Schriftart "Segoe UI", Schriftgröße 9
  ------------------------------------------------------------------------------------------------------------ }

unit InputDialog;


interface


uses
    Forms,
    StdCtrls,
    Buttons,
    Controls,
    Classes,
    GeneralTypes,
    ExtCtrls;

type
    TfrmInputDialog = class(TForm)
        btnOK: TButton;
        btnCancel: TButton;
        Shape2: TShape;
        Bevel2: TBevel;
        Label1: TLabel;
        Edit1: TEdit;
    public
        class function ShowDialog(const aText, aCaption, aButtonText: string; var vValue: string): boolean;
    end;


implementation


{$R *.DFM}

uses
    ControlUtils;

{ TGetNameDlg }

class function TfrmInputDialog.ShowDialog(const aText, aCaption, aButtonText: string;
    var vValue: string): boolean;
var
    xDialog: TfrmInputDialog;
begin
    result := false;

    xDialog := TfrmInputDialog.Create(nil);
    try
        xDialog.Caption := aCaption;
        xDialog.Edit1.Clear;
        xDialog.Label1.Caption := aText;
        xDialog.btnOK.Caption := aButtonText;
        xDialog.btnCancel.Caption := TLanguageString.Read('Cancel', 'Abbrechen');
        xDialog.Edit1.Text := vValue;
        TControlUtils.ResetFontForWinXP(xDialog);

        if (xDialog.Showmodal = mrOK) then
        begin
            result := true;
            vValue := xDialog.Edit1.Text;
        end;

    finally
        xDialog.Free;
    end;
end;


end.
