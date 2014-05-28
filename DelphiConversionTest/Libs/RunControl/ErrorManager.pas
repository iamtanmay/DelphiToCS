unit ErrorManager;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Error Flags for System Errors (global errors)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                           track-no  improvement/change
  -------- --  -------------------------------  --------- ----------------------------------------------
  26.01.07 pk                                    TN3525.3    initial version
  22.02.07 pk                                    TN3525.3    to ErrorManagerRun: SetGlobalErrTemporary, GetAndDeleteErrorReason
  09.03.10 pk                                    TN4985      new SimpleGlobalErr
  -------------------------------------------------------------------------------------------------- }


interface


uses
    Classes;

type
    TErrorValue = integer;

    TGlobalErrorRec = record
        Value: TErrorValue;
        Reason: string;
    end;

    TErrorManager = class
    protected
        fOnError: TNotifyEvent;
        procedure DoSetGlobalErr(const aErrorRec: TGlobalErrorRec; aDisplay: boolean); virtual;
        function GetGlobalErr: integer; virtual;
        function GetSimpleGlobalErr: TErrorValue; virtual;
        class function IsErrorValue(const aValue: TErrorValue): boolean;
        class procedure DoLog(const aText: string; aDisplay: boolean);
        class function NoneErrorValue(): TErrorValue;
    public
        class procedure ResetErrorRec(var vErrorRec: TGlobalErrorRec);
        procedure SetGlobalErrTemporary(aSetToNone: boolean); virtual;
        procedure SetGlobalErr(aValue: TErrorValue; aReason: string = ''; aDisplay: boolean = true);
        procedure ResetGlobalErr();
        procedure LayouterResetGlobalErr();
        function GetAndDeleteErrorReason: string; virtual;
        function IsGlobalErr: boolean;
        property GlobalErr: TErrorValue read GetGlobalErr;
        property SimpleGlobalErr: TErrorValue read GetSimpleGlobalErr;
        property OnError: TNotifyEvent read fOnError write fOnError;
    end;

var
    gErrorManager: TErrorManager;


implementation


uses
    AppTypes,
    LogManager;

procedure TErrorManager.DoSetGlobalErr(const aErrorRec: TGlobalErrorRec; aDisplay: boolean);
begin
end;

procedure TErrorManager.SetGlobalErrTemporary(aSetToNone: boolean);
begin
end;

function TErrorManager.GetAndDeleteErrorReason: string;
begin
    result := '';
end;

function TErrorManager.GetGlobalErr: TErrorValue;
begin
    result := NoneErrorValue();
end;

function TErrorManager.GetSimpleGlobalErr: TErrorValue;
begin
    result := NoneErrorValue();
end;

class function TErrorManager.NoneErrorValue(): TErrorValue;
begin
    result := ERR_NONE;
end;

class function TErrorManager.IsErrorValue(const aValue: TErrorValue): boolean;
begin
    result := aValue <> NoneErrorValue();
end;

class procedure TErrorManager.DoLog(const aText: string; aDisplay: boolean);
begin
    gLogManager.Log(aText, aDisplay);
end;

class procedure TErrorManager.ResetErrorRec(var vErrorRec: TGlobalErrorRec);
begin
    vErrorRec.Value := NoneErrorValue();
    vErrorRec.Reason := '';
end;

procedure TErrorManager.SetGlobalErr(aValue: TErrorValue; aReason: string = ''; aDisplay: boolean = true);
var
    xErrorRec: TGlobalErrorRec;
begin
    if IsErrorValue(aValue) then
    begin
        if Assigned(fOnError) then
            fOnError(self);
    end;

    xErrorRec.Value := aValue;
    xErrorRec.Reason := aReason;
    DoSetGlobalErr(xErrorRec, aDisplay);
end;

procedure TErrorManager.ResetGlobalErr();
begin
    if not IsGlobalErr() then
        EXIT;
    SetGlobalErr(NoneErrorValue(), '');
end;

procedure TErrorManager.LayouterResetGlobalErr();
begin
    ResetGlobalErr();
end;

function TErrorManager.IsGlobalErr: boolean;
begin
    result := IsErrorValue(GetGlobalErr());
end;


end.
