{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Error Flags for System Errors in Runner and Layouter (global errors)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                           track-no  improvement/change
  -------- --  -------------------------------  --------- ----------------------------------------------
  26.01.07 pk                                    TN3525.3    initial version
  22.02.07 pk DoSetGlobalErr                     TN3525.3    did not set global error field
  22.02.07 pk                                    TN3525.3    from ErrorManager SetGlobalErrTemporary, GetAndDeleteErrorReason
  02.09.08 pk                                    TN4215      thrman global variable removed
  06.11.08 pk                                    TN4280      ThreadManager changed to ThreadAPI
  09.03.10 pk                                    TN4985      new SimpleGlobalErr
  15.08.13 wl                                    TN6223   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit ErrorManagerRun;


interface


uses
    ErrorManager;

type
    TErrorManagerRun = class(TErrorManager)
    protected
        fPrevError, fCurrentError: TGlobalErrorRec;
        procedure DoSetGlobalErr(const aErrorRec: TGlobalErrorRec; aDisplay: boolean); override;
        function GetGlobalErr: TErrorValue; override;
        function GetSimpleGlobalErr: TErrorValue; override;
    public
        constructor Create();
        function GetAndDeleteErrorReason: string; override;
        procedure SetGlobalErrTemporary(aSetToNone: boolean); override;
    end;


implementation


uses
    SysUtils,
    ThreadAPI;

constructor TErrorManagerRun.Create();
begin
    inherited Create;
    ResetErrorRec(fPrevError);
    ResetErrorRec(fCurrentError);
end;

procedure TErrorManagerRun.DoSetGlobalErr(const aErrorRec: TGlobalErrorRec; aDisplay: boolean);
begin
    fPrevError := fCurrentError;
    fCurrentError := aErrorRec;

    if IsErrorValue(fCurrentError.Value) then
    begin
        DoLog('- - - - - Global Error set!! - - - - - - -', aDisplay);
        if (aErrorRec.Reason <> '') then
        begin
            DoLog(Format('Global Error set!! - Reason: %s', [fCurrentError.Reason]), aDisplay);
        end;
    end;
end;

function TErrorManagerRun.GetSimpleGlobalErr: TErrorValue;
begin
    result := fCurrentError.Value;
end;

function TErrorManagerRun.GetGlobalErr: integer;
begin
    // HACK!  when an interrupt occurs, this is actually the place where all threads wait.
    // its a Hack, but the globalerr function is the only function that is called before any command is executed
    TThreadAPI.WaitIfInterrupt();

    result := GetSimpleGlobalErr;
end;

procedure TErrorManagerRun.SetGlobalErrTemporary(aSetToNone: boolean);
var
    xErrorRec: TGlobalErrorRec;
begin
    if aSetToNone then
    begin
        // before we set to none make sure we have the last error - ZP02 sets error during init without calling SetGlobalErr
        ResetErrorRec(xErrorRec);
        DoLog('- - - - - Global Error temporarily reset - - - - - - -', false);
    end
    else
    begin
        xErrorRec := fPrevError;
    end;

    DoSetGlobalErr(xErrorRec, false);
end;

function TErrorManagerRun.GetAndDeleteErrorReason: string;
begin
    result := fCurrentError.Reason;
    fCurrentError.Reason := '';
end;


end.
