{ -------------------------------------------------------------------------------
  Eingabefenster für Barcodes
  -------------------------------------------------------------------------------
  Datum    op  function/procedure     Änderung / Neuerung
  -------- --  -------------------    -------------------------------------------
  30.11.98 wl                         neue Unit
  04.06.99 mo FormShow                ErrMessageActive:=true; GlobalErrBeep:=true;
  Bei doppelter Eingabe Farbe = rot
  04.06.99 mo OnClose                 neu
  04.06.99 mo Edit1Change             GlobalErrBeep:=false;
  13.09.99 wl  FormCreate             gmLoadLanguage eingebaut
  26.05.00 wl                         uses geändert
  12.03.03 wl                         TN1293.5 uses RessourceLoader
  04.02.04 pk                         TN1719   New functions for synchronization requirements
  24.06.04 wl                         TN2007   uses Variants (nur Delphi 6 und 7)
  11.03.05 pk                         TN2339.2 Synchronized Call --> GUIManager
  14.04.08 wl                         TN4060   uses ErrorMessage ( GlobalErrBeep )
  20.09.08 pk  InstPromptInput        TN4215   New
  20.08.09 wl                         TN4702   Strings werden jetzt direkt geladen
  20.05.10 wl                         TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  10.01.12 ts  Start/EndBeep          TN5760    Beep functionality reactivated
  30.07.13 wl                         TN6160   verwendet gGUIManager.InterruptSignalMute statt GlobalErrBeep
  ------------------------------------------------------------------------------- }

unit BarcDlg;


interface


uses
    Forms,
    Controls,
    StdCtrls,
    ExtCtrls,
    Classes;

type
    TBarcodeDLG = class(TForm)
        btnOK: TButton;
        btnAbort: TButton;
        btnNoRack: TButton;
        Edit1: TEdit;
        Label1: TLabel;
        procedure FormShow(Sender: TObject);
        procedure Edit1Change(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    public
        function PromptInput(var aText: string; aCaption: string; aLabelCaption: string;
            aNoRackBtnCaption: string): integer;
        class function InstPromptInput(var aText: string; aCaption: string; aLabelCaption: string;
            aNoRackBtnCaption: string): integer;
    end;


implementation


{$R *.DFM}

uses
    Graphics,
    Windows,
    GUIManager,
    GeneralTypes,
    ControlUtils;

{ TBarcodeDLG }

class function TBarcodeDLG.InstPromptInput(var aText: string; aCaption: string; aLabelCaption: string;
    aNoRackBtnCaption: string): integer;
var
    xBarcodeDLG: TBarcodeDLG;
begin
    xBarcodeDLG := TBarcodeDLG.Create(nil);
    try
        result := xBarcodeDLG.PromptInput(aText, aCaption, aLabelCaption, aNoRackBtnCaption);
    finally
        xBarcodeDLG.Free;
    end;

end;

function TBarcodeDLG.PromptInput(var aText: string; aCaption: string; aLabelCaption: string;
    aNoRackBtnCaption: string): integer;
var
    xSaveBtnNoRackVisible: boolean;
begin
    windows.MessageBeep(0);
    self.Caption := aCaption;
    self.Label1.Caption := aLabelCaption;
    xSaveBtnNoRackVisible := self.btnNoRack.Visible; // save the visible property of the btnNoRack button
    self.btnNoRack.Visible := (aNoRackBtnCaption <> '');
    self.btnNoRack.Caption := aNoRackBtnCaption;
    self.Edit1.Text := '';
    result := self.ShowModal;
    self.btnNoRack.Visible := xSaveBtnNoRackVisible;
    // restore the visible property to whatever it was before this function was called
    aText := Edit1.Text;
end;

procedure TBarcodeDLG.FormShow(Sender: TObject);
begin
    Color := clBtnFace;
    if (pos('error', Caption) > 0) or (pos('equal to', Caption) > 0) then
        Color := clRed;
    Edit1.SetFocus;
end;

procedure TBarcodeDLG.Edit1Change(Sender: TObject);
begin
    gGUIManager.InterruptSignalMute();
end;

procedure TBarcodeDLG.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    self.btnOK.Caption := TLanguageString.Read('&OK', '&OK');
    self.btnAbort.Caption := TLanguageString.Read('&Abort', '&Abbrechen');
    self.btnNoRack.Caption := TLanguageString.Read('No Rack', 'Kein Rack');
end;


end.
