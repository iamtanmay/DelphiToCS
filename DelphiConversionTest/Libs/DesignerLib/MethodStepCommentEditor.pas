{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  29.09.10 pk                                        TN5283    Initial revision
  21.07.11 wl  FormCreate                            TN5614   wenn aWritable = false, wird OK-Button nicht gezeigt
  08.08.13 wl  Memo1KeyDown                          TN6169   mit Return schließt man das Fenster, mit Shift+Return oder Alt+Return Zeilenumbruch
  09.08.13 wl  lblLineBreakInfo                      TN6169   Label zur Information
  ----------------------------------------------------------------------------------------------------------------------- }

unit MethodStepCommentEditor;


interface


uses
    Forms,
    Controls,
    Classes,
    StdCtrls,
    ExtCtrls;

type
    TfrmMethodStepCommentEditor = class(TForm)
        pnlTop: TPanel;
        Label1: TLabel;
        pnlBottom: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        pnlMiddle: TPanel;
        Memo1: TMemo;
        mmoActionDescription: TMemo;
        lblLineBreakInfo: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    private
        function DoShowDialog(const aActionDescription: string; var vMemoText: string; aWritable: boolean)
            : TModalResult;
    public
        class function ShowDialog(const aActionDescription: string; var vMemoText: string; aWritable: boolean)
            : TModalResult;
    end;


implementation


{$R *.dfm}

uses
    Windows,
    SysUtils,
    ControlUtils,
    GeneralTypes;

{ TForm2 }

function TfrmMethodStepCommentEditor.DoShowDialog(const aActionDescription: string; var vMemoText: string;
    aWritable: boolean): TModalResult;
var
    xModalResult: TModalResult;
begin
    mmoActionDescription.Text := aActionDescription;
    self.Memo1.Text := vMemoText;
    btnOK.Visible := aWritable;
    Memo1.ReadOnly := not aWritable;

    xModalResult := self.ShowModal();

    if (not aWritable) or (xModalResult <> mrOK) then
        EXIT(mrCancel);

    vMemoText := self.Memo1.Text;
    EXIT(mrOK);
end;

procedure TfrmMethodStepCommentEditor.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    self.Caption := TLanguageString.Read('Line comments', 'Zeilenkommentare');
    Label1.Caption := TLanguageString.Read('Comment this action:', 'Diese Action kommentieren:');
    lblLineBreakInfo.Caption := TLanguageString.Read('Line break: Shift+Return/Alt+Return',
        'Zeilenumbruch: Shift+Return/Alt+Return');
    mmoActionDescription.Lines.Clear();
end;

procedure TfrmMethodStepCommentEditor.Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    xSelStart: integer;
begin
    if (Key = VK_RETURN) and (Shift = []) then
    begin
        self.ModalResult := mrOK;
    end;
    if (Key = VK_RETURN) and (ssAlt in Shift) and not Memo1.ReadOnly then
    begin
        // Alt + Return funktioniert leider nicht von selbst, deshalb muss man nachhelfen
        xSelStart := Memo1.SelStart;
        // Selektierte Zeichen löschen
        Memo1.Text := Copy(Memo1.Text, 1, Memo1.SelStart) + #13 + #10 +
            Copy(Memo1.Text, Memo1.SelStart + Memo1.SelLength + 1);
        Memo1.SelStart := xSelStart + 2;
    end;
end;

class function TfrmMethodStepCommentEditor.ShowDialog(const aActionDescription: string; var vMemoText: string;
    aWritable: boolean): TModalResult;
var
    xForm: TfrmMethodStepCommentEditor;
begin
    xForm := TfrmMethodStepCommentEditor.Create(nil);
    try
        result := xForm.DoShowDialog(aActionDescription, vMemoText, aWritable);
    finally
        FreeAndNil(xForm);
    end;
end;


end.
