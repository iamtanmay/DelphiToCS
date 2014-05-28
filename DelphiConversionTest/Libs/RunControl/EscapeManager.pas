{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Escape Manager for user interruption with ESC-Key or "Stop" button
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.04.03 wl                               TN1345   divided from ThrdMan.pas
  08.11.04 wl  Execute                      TN2213   benutzt TDllCall.DirectExecute statt gmLoadEventFunction
  07.11.05 pk  Execute                      TN2735   PressStopButton is NOT called with Synchronize
  07.11.05 pk  fStopRequested               TN2735   New : if set the true PressStopButton will be called
  22.12.05 pk  fInterruptRequests           TN2875   Escape thread can now handle any interrupt instead of just the ESCAPE key
  23.01.06 pk                               TN2894   Threadsafe access to Interrupt Request List
  01.11.06 pk  RequestInterrupt             TN3391   call Handled function to make requesting thread wait on event
  09.11.06 pk                               TN3399   Interrupts can now have arguements and results
  13.11.06 pk  HandleNextInterrupt          TN3401   Do not make reference to interruptitem after handled is called
  21.11.06 pk  RequestInterrupt             TN3424   new paramter aWaitTillHandledLock
  21.11.06 pk  RequestInterrupt             TN3424   InterruptItem is now created here
  27.11.06 wl  TEscapeThread.Execute        TN3243   calls TErrorMessageFactory.ErrMsgBringToFront();
  01.12.06 pk                               TN3441   Interrupt handling moved to TInterruptManager
  01.12.06 pk  TEscapeThread                TN3441   changed to TEscapeManager, thread created in thread registry
  --------------------------------------------------------------------------------------------------
  07.12.06 pk                               TN3455   unit renamed from EscapeThread to EscapeManager
  07.12.06 pk  Execute                      TN3455   exceptions handled in while loop
  07.12.06 wl                               TN3243    uses SamErr entfernt
  17.02.07 pk  Execute                      TN3583   Bug: Increment of cnt caused an integer overflow
  14.04.08 wl                               TN4060   uses ErrorMessage ( GlobalErrBeep )
  03.07.08 wl                                         TN4157
  06.11.08 pk  fTerminated                  TN4280   removed. already in TExecutable
  08.09.09 pk                               TN4753   Beep functionality removed for now
  09.03.10 pk                               TN4985   new SimpleGlobalErr
  09.03.10 pk                               TN5015   Exceptionhandler removed.  Standard Exception Handler should take care of this
  10.01.12 ts                               TN5760   Beep functionality reactivated (without DLL call)
  19.01.12 ts  Execute                      TN5781   TErrorMessageFactory.ErrMsgBringToFront() abgeschaltet, da ErrorMsgBox hinter Applikation verschwindet
  30.07.13 wl  fGlobalErrBeep               TN6160   von ErrorINfo hierher
  30.07.13 wl  InterruptBeepStart,-End      TN6160   kapseln fGlobalErrBeep
  -------------------------------------------------------------------------------------------------- }

unit EscapeManager;


interface


uses
    Executable;

type
    TBeepOptions = record
        DLLName, DLLFunc: string;
        DLLOffFunc: string;
        Time, Sound: integer;
    end;

    TEscapeManager = class(TExecutable)
    private
        fGlobalErrBeep: boolean;
    protected
        FBeep: TBeepOptions;
    public
        // constructor
        constructor Create(aBeep: TBeepOptions);
        procedure Execute; override;
        procedure InterruptBeepStart();
        procedure InterruptBeepEnd();
    end;


implementation


uses
    Windows,
    ThrdMan,
    ErrorMessageFactory;

constructor TEscapeManager.Create(aBeep: TBeepOptions);
begin
    inherited Create;
    FBeep := aBeep;
end;

procedure TEscapeManager.Execute;
var
    xBeeperCount: integer;
    xBringToFrontCount: byte;
    xBeepOn: boolean;
    xRepeatBeep: boolean;
begin

    xBeepOn := false;
    xBeeperCount := 0;
    xBringToFrontCount := 0;

    while not self.Terminated do
    begin
        Sleep(10);
        if (GetAsyncKeyState(27) <> 0) then
        begin // ESC-Button wurde gedrückt
            // gmGetResString( 10700 { "User interrupt ..." }
            TThreadManagerSetup.Instance.RequestStopRunInterrupt('ESC key pressed', false);
            while (GetAsyncKeyState(27)) <> 0 do
                Sleep(5);
        end;

        if (fGlobalErrBeep) then
        begin
            Inc(xBeeperCount);

            // do a beep everytime the counter reaches beep.Time
            xRepeatBeep := (xBeeperCount = FBeep.Time);
            if xRepeatBeep then
            begin
                xBeeperCount := 0;
            end;

            if (not xBeepOn) or xRepeatBeep then
            begin
                // -------------------------------------------------------------- Einschalten des Signaltons
                { TODO : TDllCall }
                if (FBeep.DLLName = '') then
                    MessageBeep(FBeep.Sound);
                // else TDllCall.DirectExecute(FBeep.DLLName,FBeep.DLLFunc,'');
                xBeepOn := true;
                // ---------------------------------- Bei jedem 10. Signalton: BringToFront, RestoreTopMosts
                if (xBringToFrontCount = 0) then
                begin // BringToFront erfolgt nur alle 10 Durchläufe
                    // TErrorMessageFactory.ErrMsgBringToFront();
                end;
                Inc(xBringToFrontCount);
                if (xBringToFrontCount >= 10) then
                    xBringToFrontCount := 0;
            end;
        end
        else if xBeepOn then
        begin
            xBeeperCount := 0;
            xBringToFrontCount := 0;
            // -------------------------------------------------------------- Ausschalten des Signaltons
            { TODO : TDllCall }
            // if (FBeep.DLLOffFunc<>'') then TDllCall.DirectExecute(FBeep.DLLName,FBeep.DLLOffFunc,'');
            xBeepOn := false;
        end;
        // Application.ProcessMessages;
    end;

end;

procedure TEscapeManager.InterruptBeepEnd;
begin
    fGlobalErrBeep := false;
end;

procedure TEscapeManager.InterruptBeepStart;
begin
    fGlobalErrBeep := true;
end;


end.
