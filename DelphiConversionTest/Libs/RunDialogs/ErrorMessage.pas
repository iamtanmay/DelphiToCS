{ --------------------------------------------------------------------------------------------------
  Copyright © 2006 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Error Handling dialog
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                           track-no  improvement/change
  -------- --  -------------------------------  --------- ----------------------------------------------
  29.11.06 wl                                   TN3243    initial version
  01.12.06 wl                                   TN3243    neu: Buttons-Property
  04.12.06 wl                                   TN3243    Cancel-Eigenschaft von Buttons entfernt
  05.12.06 wl                                   TN3243    mit Abort-Hint
  13.04.07 pk  FormShow                         TN3667    Clear Memos. Sometimes Show is used instead of ShowModal. Show is called several times.
  03.08.07 wl  FormShow                         TN3811.2 gmGetPictureFromIcon statt TPicture-cast
  14.04.08 wl  GlobalErrBeep                    TN4060    von SamGlobe hierher verschoben
  11.07.08 pk                                   TN3996    Memo1, Memo2, Memo3 ReadOnly set to true
  17.12.08 pk  Image1                           TN4373    Background now transparent
  10.08.09 wl                                   TN4702   Strings werden jetzt direkt geladen
  08.09.09 pk  TErrorInfo                       TN4753   code move to new unit ErrorInfo.Pas
  08.09.09 pk  InstPromptModal                  TN4753   New
  12.10.09 pk                                   TN4812   AdditionalInfo is no longer normal TStringList class
  13.04.10 wl                                   TN5044   uses geändert
  20.05.10 wl                                   TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  07.06.10 wl                                   TN5135   erscheint jetzt in der Bildschirmmitte
  21.06.10 wl                                   TN5160   Position = poScreenCenter
  10.01.12 ts                                   TN5760   Beep functionality reactivated, GlobalErrBeep removed to ErrorInfo
  30.07.13 wl                                   TN6160   verwendet gGUIManager.InterruptSignalMute statt GlobalErrBeep
  -------------------------------------------------------------------------------------------------- }

unit ErrorMessage;


interface


uses
    Forms,
    Classes,
    Graphics,
    Controls,
    StdCtrls,
    ExtCtrls,
    ErrorInfo;

type
    TfrmErrorMessage = class(TForm)
        Bevel2: TBevel;
        Shape2: TShape;
        Panel1: TPanel;
        btnAbort: TButton;
        btnRetry: TButton;
        btnIgnore: TButton;
        Bevel1: TBevel;
        Image1: TImage;
        Memo1: TMemo;
        Memo2: TMemo;
        btnBeep: TButton;
        Image2: TImage;
        Memo3: TMemo;
        procedure FormShow(Sender: TObject);
        procedure btnBeepClick(Sender: TObject);
    private
        fErrorInfo: TErrorInfo;
        class var uErrBoxInstance: TfrmErrorMessage;
    public
        constructor Create(aOwner: TComponent; aErrorInfo: TErrorInfo); reintroduce;
        class procedure ErrMsgBringToFront();
        class procedure ErrBoxClose();
        class function InstPromptModal(const aErrorInfo: TErrorInfo): integer;
        //
        property Info: TErrorInfo read fErrorInfo;
    end;


implementation


{$R *.dfm}

uses
    GUIManager,
    GeneralTypes,
    UtilLib,
    ControlUtils;

{ TfrmErrorMessage }

constructor TfrmErrorMessage.Create(aOwner: TComponent; aErrorInfo: TErrorInfo);
begin
    inherited Create(aOwner);

    TControlUtils.ResetFontForWinXP(self);
    fErrorInfo := aErrorInfo;
end;

procedure TfrmErrorMessage.FormShow(Sender: TObject);
var
    x: integer;
begin
    Image2.Picture := gmGetPictureFromIcon(Application.Icon);
    // Bild könnte auch von ErrorInfo übergeben werden

    self.Caption := fErrorInfo.Caption;
    btnAbort.Caption := TLanguageString.Read('&Abort', '&Abbrechen');
    btnRetry.Caption := TLanguageString.Read('&Retry', '&Wiederholen');
    btnIgnore.Caption := TLanguageString.Read('&Ignore', '&Ignorieren');
    btnBeep.Caption := TLanguageString.Read('Beeper Off', 'Ton aus');

    Memo1.Text := fErrorInfo.InfoText;

    Memo2.Clear; // clear memo. Sometimes formshow is called several times
    for x := 0 to fErrorInfo.AdditionalInfo.Count - 1 do
        Memo2.Lines.Add(fErrorInfo.AdditionalInfo[x]);

    btnRetry.Enabled := fErrorInfo.Buttons in [eibAbortRetry, eibAbortRetryIgnore];
    btnIgnore.Enabled := fErrorInfo.Buttons in [eibAbortIgnore, eibAbortRetryIgnore];

    Memo3.Clear;
    if (btnAbort.Enabled) and (fErrorInfo.AbortHint <> '') then
    begin
        btnAbort.ShowHint := true;
        btnAbort.Hint := fErrorInfo.AbortHint;
        Memo3.Lines.Add('Abort: ' + fErrorInfo.AbortHint);
    end;
    if (btnRetry.Enabled) and (fErrorInfo.RetryHint <> '') then
    begin
        btnRetry.ShowHint := true;
        btnRetry.Hint := fErrorInfo.RetryHint;
        Memo3.Lines.Add('Retry: ' + fErrorInfo.RetryHint);
    end;
    if (btnIgnore.Enabled) and (fErrorInfo.IgnoreHint <> '') then
    begin
        btnIgnore.ShowHint := true;
        btnIgnore.Hint := fErrorInfo.IgnoreHint;
        Memo3.Lines.Add('Ignore: ' + fErrorInfo.IgnoreHint);
    end;
end;

procedure TfrmErrorMessage.btnBeepClick(Sender: TObject);
begin
    gGUIManager.InterruptSignalMute();
    btnBeep.Enabled := false;
end;

class procedure TfrmErrorMessage.ErrBoxClose;
begin
    if not Assigned(uErrBoxInstance) then
        EXIT;
end;

class procedure TfrmErrorMessage.ErrMsgBringToFront;
begin
    Application.BringToFront;

    if not Assigned(uErrBoxInstance) then
        EXIT;
    if (not uErrBoxInstance.Active) then
        EXIT;

    Application.RestoreTopMosts;
end;

class function TfrmErrorMessage.InstPromptModal(const aErrorInfo: TErrorInfo): integer;
begin
    uErrBoxInstance := TfrmErrorMessage.Create(Application, aErrorInfo);
    try
        result := uErrBoxInstance.ShowModal;
    finally
        uErrBoxInstance.Free;
    end;
end;


end.
