{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  24.09.10 pk                                        TN5280    Initial revision
  04.08.11 wl                                        TN5647    optisch überarbeitet
  08.04.14 ts                                        TN6393    new to edit tables from ZARunner (copy and changed from ZADBAdministrator)
  08.04.14 ts                                        TN6393    name of frmTableMemoEditor corrected
  ----------------------------------------------------------------------------------------------------------------------- }

unit TableMemoEditor;


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
    ExtCtrls;

type
    TfrmTableMemoEditor = class(TForm)
        Panel2: TPanel;
        Label1: TLabel;
        Panel1: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        pnlMiddle: TPanel;
        edFieldName: TEdit;
        Memo1: TMemo;
    private
        function DoShowDialog(const aFieldName: string; var vMemoText: string): TModalResult;
    public
        class function ShowDialog(const aFieldName: string; var vMemoText: string): TModalResult;

    end;


implementation


{$R *.dfm}

{ TfrmMemoTextEditor }
function TfrmTableMemoEditor.DoShowDialog(const aFieldName: string; var vMemoText: string): TModalResult;
begin
    self.Caption := 'Field Editor';
    edFieldName.Text := aFieldName;
    self.Memo1.Lines.Text := vMemoText;
    result := self.ShowModal();
    if result <> mrOK then
        EXIT;
    vMemoText := self.Memo1.Lines.Text;
end;

class function TfrmTableMemoEditor.ShowDialog(const aFieldName: string; var vMemoText: string): TModalResult;
var
    xForm: TfrmTableMemoEditor;
begin
    xForm := TfrmTableMemoEditor.Create(nil);
    try
        result := xForm.DoShowDialog(aFieldName, vMemoText);
    finally
        FreeAndNil(xForm);
    end;

end;


end.
