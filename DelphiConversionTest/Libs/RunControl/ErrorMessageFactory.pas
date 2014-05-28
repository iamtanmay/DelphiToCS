{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                           track-no  improvement/change
  -------- --  -------------------------------  --------- ----------------------------------------------
  09.11.07 pk  gErrorMessageFactory             TN3923    New
  31.07.08 pk  WriteErrorLog                    TN3945    New: Log all lines of the errortext
  24.08.09 pk  WriteErrorLog                    TN4735.4  now public
  08.09.09 pk                                   TN4753    massive changes
  27.10.10 wl  Create                           TN5300    neu
  19.01.12 wl  ShowAnyError                     TN5759.2  neuer Parameter IgnoreInterruptStartError, Default = false
  -------------------------------------------------------------------------------------------------- }

unit ErrorMessageFactory;


interface


uses
    ErrorInfo;

type
    TErrorMessageFactory = class
    public
        constructor Create;

        class function GetNameFromResult(aModalResult: integer): string;
        class procedure ErrMsgBringToFront();
        class procedure ErrBoxClose();
        class procedure WriteErrorLog(aErrorInfo: TErrorInfo);

        function ErrBox(aErrorInfo: TErrorInfo): integer;
        function ErrBoxSimple(const aText, aCaption: string; aButtons: TErrorInfoButtons;
            const aRetryHint: string = ''; const aIgnoreHint: string = ''): integer;
        function ShowAnyError(aErrorInfo: TErrorInfo; aIgnoreInterruptStartError: boolean = false)
            : integer; virtual;
    end;

var
    gErrorMessageFactory: TErrorMessageFactory;


implementation


uses
    SysUtils,
    Variants,
    Controls,
    Forms,
    SamGlobe,
    LogManager,
    AppSettings,
    CommonTypes,
    GUIManager;

{ TErrorMessageFactory }

constructor TErrorMessageFactory.Create;
begin
    inherited;
end;

class function TErrorMessageFactory.GetNameFromResult(aModalResult: integer): string;
begin
    case (aModalResult) of
        mrAbort:
            result := 'Abort';
        mrIgnore:
            result := 'Ignore';
        mrRetry:
            result := 'Retry';
        else
            result := IntToStr(aModalResult);
    end;
end;

function TErrorMessageFactory.ShowAnyError(aErrorInfo: TErrorInfo;
    aIgnoreInterruptStartError: boolean): integer;
begin
    result := 0; // Dummy
end;

function TErrorMessageFactory.ErrBoxSimple(const aText, aCaption: string; aButtons: TErrorInfoButtons;
    const aRetryHint, aIgnoreHint: string): integer;
var
    xErrorInfo: TErrorInfo;
begin
    xErrorInfo := TErrorInfo.CreateAndInit(aText, aCaption, aButtons);
    try
        xErrorInfo.RetryHint := aRetryHint;
        xErrorInfo.IgnoreHint := aIgnoreHint;
        xErrorInfo.AddText(aText);
        result := ShowAnyError(xErrorInfo);
    finally
        xErrorInfo.Free;
    end;
end;

function TErrorMessageFactory.ErrBox(aErrorInfo: TErrorInfo): integer;
begin
    result := ShowAnyError(aErrorInfo);
end;

class procedure TErrorMessageFactory.WriteErrorLog(aErrorInfo: TErrorInfo);
var
    x: integer;
begin
    gLogManager.Log('Errorbox - Caption: ' + aErrorInfo.LogText, true);
    for x := 0 to aErrorInfo.AdditionalInfo.Count - 1 do
    begin
        gLogManager.Log('Errorbox - ErrorText: ' + aErrorInfo.AdditionalInfo[x], true);
    end;
end;

class procedure TErrorMessageFactory.ErrBoxClose;
begin
    // if ErrMessageActive then
    // gGUIManager.ErrBox_CloseWithAbort();
    // uErrBoxInstance.ModalResult := mrAbort;

end;

class procedure TErrorMessageFactory.ErrMsgBringToFront;
begin
    Application.BringToFront;

    if (gConfirmIgnore) then
        EXIT;

    // gGUIManager.ErrBox_BringToFront();

    Application.RestoreTopMosts;
end;


end.
