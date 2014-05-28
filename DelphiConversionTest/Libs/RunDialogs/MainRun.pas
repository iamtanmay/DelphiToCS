{ ------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                            track-no improvement/change
  -------- --  --------------------------------  -------- ----------------------------------------------------
  25.05.10 wl                                    TN5116   initial revision
  26.05.10 wl                                    TN5116   Caption hinzugefügt
  27.05.10 wl                                    TN5116   neue Buttons
  04.06.10 wl                                    TN5116   genau wie RunnerMain
  18.06.10 wl                                    TN5116   Buttons überarbeitet
  20.04.12 wl                                    TN5858   komplett neu mit Pause/Fortsetzen/Abort
  04.05.12 wl                                    TN5858   neue Buttons, neue Animation
  09.05.12 wl  fUserLogRun                       TN5858   von ThrMan hierher
  09.05.12 wl  StateSetPause,-Abort,-Running     TN5858   neu: können jetzt von außen aufgerufen werden
  29.05.12 wl  StateSetPause                     TN5894   neuer Zustand RequestState unterscheidet sich von Pause
  29.05.12 wl  btnStepByStep                     TN5894   neu: Für schrittweise Abarbeitung
  04.12.12 wl  MainMethodSelection               TN5960   Methodenauswahl als neues Hauptfenster-Element
  10.04.13 ts  FormCreate                        TN6126   Reload1.avi als Resource an Animate1 übergeben
  17.07.13 ts  StateSetAbort                     TN6204   SetGlobalError wird sofort gesetzt, sonst können noch Actions ausgeführt werden
  13.08.13 wl  btnStepByStep                     TN6215   Button ist zunächst unsichtbar, solange er noch nicht 100%ig funktioniert
  ------------------------------------------------------------------------------------------------------------ }

unit MainRun;


interface


uses
    Forms,
    ToolWin,
    ComCtrls,
    StdCtrls,
    Controls,
    Classes,
    ExtCtrls,
    Buttons,
    Graphics,
    ImgList,
    AppTypes;

type
    TfrmMainRun = class(TForm)
        pnDuringRun: TPanel;
        Panel1: TPanel;
        btnPause: TSpeedButton;
        btnStart: TSpeedButton;
        btnAbort: TSpeedButton;
        lblMethod: TLabel;
        lblJob: TLabel;
        btnStepByStep: TSpeedButton;
        edSimulationCaption: TEdit;
        Animate1: TAnimate;
        pnlMain: TPanel;
        edMethod: TEdit;
        procedure FormCreate(Sender: TObject);
        procedure btnStartClick(Sender: TObject);
        procedure btnAbortClick(Sender: TObject);
        procedure btnPauseClick(Sender: TObject);
        procedure btnStepByStepClick(Sender: TObject);
    private
        fRunMainState: TRunMainState;
        fUserLogRun: boolean;
        fStepByStepMode: boolean;
        fMainMethodSelection: TPanel;
        procedure SetRunMainState(const aState: TRunMainState);
        procedure ActivateSelectPanel;
        procedure DeactivateSelectPanel;
    public
        procedure StateSetPause(aShowRequest: boolean);
        procedure StateSetAbort;
        procedure StateSetRunning;
        property RunMainState: TRunMainState read fRunMainState write SetRunMainState;
        property UserLogRun: boolean read fUserLogRun write fUserLogRun;
        function RequestStepByStepMode: boolean;
        property MainMethodSelection: TPanel read fMainMethodSelection write fMainMethodSelection;
    end;


implementation


{$R *.dfm}
{$R MainRun.RES}

uses
    GeneralTypes,
    RequestAbort,
    AppSettings,
    CommonTypes,
    LogManager,
    ControlUtils,
    EdExtern;

{ TfrmMainRun }

procedure TfrmMainRun.FormCreate(Sender: TObject);
begin
    TControlUtils.ResetFontForWinXP(self);
    self.Caption := TLanguageString.Read('Run Method', 'Methode starten');

    fRunMainState := rmsReady;
    fUserLogRun := false;
    fStepByStepMode := false;
    Animate1.ResHandle := HInstance;
    Animate1.ResName := 'RELOAD1';
end;

function TfrmMainRun.RequestStepByStepMode: boolean;
begin
    result := fStepByStepMode;
    fStepByStepMode := false; // nach jeder Abfrage zurücksetzen!!!
end;

procedure TfrmMainRun.btnAbortClick(Sender: TObject);
begin
    StateSetAbort;
end;

procedure TfrmMainRun.btnPauseClick(Sender: TObject);
begin
    StateSetPause(false);
end;

procedure TfrmMainRun.btnStartClick(Sender: TObject);
begin
    StateSetRunning;
end;

procedure TfrmMainRun.btnStepByStepClick(Sender: TObject);
begin
    fStepByStepMode := true;
    StateSetRunning;
end;

procedure TfrmMainRun.ActivateSelectPanel;
begin
    if Assigned(fMainMethodSelection) then
        fMainMethodSelection.Visible := true;
end;

procedure TfrmMainRun.DeactivateSelectPanel;
begin
    if Assigned(fMainMethodSelection) then
        fMainMethodSelection.Visible := false;
end;

procedure TfrmMainRun.SetRunMainState(const aState: TRunMainState);
begin
    case (aState) of
        rmsReady:
            begin
                if Assigned(self.btnStart) then
                begin
                    // self.btnStart.Hint := 'Start Method';
                    // self.ImageList1.GetBitmap(0, self.btnStart.Glyph);

                    { TODO : Im Ready-Zustand soll das hier der normale Start-Button sein }
                    // ------------------------------
                    // zur Zeit disabled:
                    self.btnStart.Enabled := false; // true;
                    // ------------------------------
                end;
                if Assigned(self.btnStepByStep) then
                    self.btnStepByStep.Enabled := false;
                if Assigned(self.btnPause) then
                    self.btnPause.Enabled := false;
                if Assigned(self.btnAbort) then
                    self.btnAbort.Enabled := false;
                self.Animate1.Stop;
                self.Animate1.Visible := false;
                self.ActivateSelectPanel;
            end;
        rmsRunning:
            begin
                if Assigned(self.btnStart) then
                    self.btnStart.Enabled := false;
                if Assigned(self.btnStepByStep) then
                    self.btnStepByStep.Enabled := false;
                if Assigned(self.btnPause) then
                    self.btnPause.Enabled := true;
                if Assigned(self.btnAbort) then
                    self.btnAbort.Enabled := false;
                self.Animate1.Visible := true;
                self.Animate1.Play(1, 6, 0);
                self.DeactivateSelectPanel;
            end;
        rmsPaused:
            begin
                if Assigned(self.btnStart) then
                begin
                    self.btnStart.Hint := 'Continue Process';
                    // self.ImageList1.GetBitmap(1, self.btnStart.Glyph);
                    self.btnStart.Enabled := true;
                end;
                if Assigned(self.btnStepByStep) then
                    self.btnStepByStep.Enabled := true;
                if Assigned(self.btnPause) then
                    self.btnPause.Enabled := false;
                if Assigned(self.btnAbort) then
                    self.btnAbort.Enabled := true;
                self.Animate1.Stop;
                self.Animate1.Visible := false;
                self.DeactivateSelectPanel;
            end;
        rmsRequestState:
            begin
                if Assigned(self.btnStart) then
                    self.btnStart.Enabled := false;
                if Assigned(self.btnStepByStep) then
                    self.btnStepByStep.Enabled := false;
                if Assigned(self.btnPause) then
                    self.btnPause.Enabled := false;
                if Assigned(self.btnAbort) then
                    self.btnAbort.Enabled := false;
                self.Animate1.Stop;
                self.Animate1.Visible := false;
            end;
        else
            Assert(false, 'State undefined');
    end;

    fRunMainState := aState;
end;

procedure TfrmMainRun.StateSetAbort;
begin
    TEdExtern.Instance.SetGlobalError('Process aborted by user');
    TEdExtern.Instance.InterruptFinish('Process aborted by user');
    gLogManager.Log('==> User Interrupt', false);
    if fUserLogRun then
        gCommonDll.CurrentUser.LogRun(rkdNone, '', 'Thread aborted by user', '', lrtAbort);
end;

procedure TfrmMainRun.StateSetPause(aShowRequest: boolean);
var
    xState: TRunMainState;
begin
    if (aShowRequest) then
        xState := rmsRequestState
    else
        xState := rmsPaused;

    if (fRunMainState = rmsRunning) then
    begin
        if TEdExtern.Instance.InterruptStart('Process paused by user') then
            SetRunMainState(xState);
        if fUserLogRun then
            gCommonDll.CurrentUser.LogRun(rkdNone, '', 'Thread stopped', '', lrtUserInterrupt);
    end
    else
        SetRunMainState(xState);
end;

procedure TfrmMainRun.StateSetRunning;
begin
    if (fRunMainState = rmsPaused) or (fRunMainState = rmsRequestState) then
    begin
        if TEdExtern.Instance.InterruptFinish('Process continued by user') then
            SetRunMainState(rmsRunning);
        if fUserLogRun then
            gCommonDll.CurrentUser.LogRun(rkdNone, '', 'Thread continued by user', '', lrtContinue);
    end;
end;


end.
