unit RunAbortInfo;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2004 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Dialog Form after Run Abort
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  01.11.04 wl                                TN2181   initial version
  11.03.05 pk                                TN2339.2 Synchronized Call --> GUIManager
  06.06.06 pk  RichEdit1                     TN3136   Replaced by memo
  02.09.08 pk  PromptModal                   TN4215   ActionStatus parameter removed
  25.09.08 pk                                TN4241   uses changed
  20.08.09 wl                                TN4702   Strings werden jetzt direkt geladen
  27.04.10 pk  PromptModal                   TN5074   TStrings changed to TStringArray
  20.05.10 wl                                TN5117   neue Schriftart "Segoe UI", Schriftgröße 9
  21.06.10 wl                                TN5160   Position = poScreenCenter
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Forms,
    StdCtrls,
    Controls,
    Classes,
    ExtCtrls,
    GeneralTypes;

type
    TfrmRunAbortInfo = class(TForm)
        Bevel1: TBevel;
        btnOK: TButton;
        mmoMessage: TMemo;
        procedure FormCreate(Sender: TObject);
    public
        class function PromptModal(const aLines: TStringArray): integer;
    end;


implementation


{$R *.DFM}

uses
    ControlUtils;

{ TfrmAbortInfo }

class function TfrmRunAbortInfo.PromptModal(const aLines: TStringArray): integer;
var
    xForm: TfrmRunAbortInfo;
begin
    xForm := TfrmRunAbortInfo.Create(nil);
    try
        TControlUtils.AddValuesToMemo(aLines, xForm.mmoMessage, true);
        result := xForm.ShowModal();
    finally
        xForm.Free;
    end;
end;

procedure TfrmRunAbortInfo.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    self.btnOK.Caption := TLanguageString.Read('&OK', '&OK');
    self.Caption := TLanguageString.Read('Run Abort Information', 'Abbruch eines Laufs: Information');
end;


end.
