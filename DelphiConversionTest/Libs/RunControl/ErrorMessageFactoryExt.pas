{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                           track-no  improvement/change
  -------- --  -------------------------------  --------- ----------------------------------------------
  09.11.07 pk  ShowAnyError                      TN3923    No longer a class function
  08.09.09 pk                                    TN4753    Massive changes
  27.10.10 wl  fMailHost                         TN5300    neu: bei einem Fehler kann sofort eine Mail abgeschickt werden
  30.11.10 wl  fRecipients                       TN5300    Settings-Ident geändert
  19.01.12 wl  ShowAnyError                      TN5759.2  neuer Parameter IgnoreInterruptStartError, Default = false
  30.07.13 wl  ErrMessageActive                  TN6160   entfernt
  -------------------------------------------------------------------------------------------------- }

unit ErrorMessageFactoryExt;


interface


uses
    ThreadClasses,
    ErrorInfo,
    ErrorInfoExt,
    ErrorMessageFactory,
    MailHost;

type
    TErrorMessageFactoryExt = class(TErrorMessageFactory)
    private
        fMailHost: TMailHost;
        fRecipients, fCCRecipients: string;
        procedure SendMail(aErrorInfo: TErrorInfo);
        function ErrorBox(const aErrorInfo: TErrorInfo): integer;
    protected
        function InterruptRoutine(aSender: TObject; aIRArgs: TInterruptRoutineArgs): TInterruptRoutineResult;
    public
        constructor Create(aCreateMailHost: boolean = false);
        destructor Destroy; override;

        function ShowAnyError(aErrorInfo: TErrorInfo; aIgnoreInterruptStartError: boolean = false)
            : integer; override;
        function ErrBoxSerialIntf(aErrorCode: TComInterfaceErrorInfoType; aComPort: integer;
            const aModuleName, aInfoText: string): integer;
    end;


implementation


uses
    Windows,
    SysUtils,
    Variants,
    Controls,
    GUIManager,
    GUIManagerSetup,
    LogManager,
    AppSettings,
    CommonTypes,
    ThrdMan,
    GeneralTypes;

{ TErrorMessageFactory }

constructor TErrorMessageFactoryExt.Create;
var
    xIniAccess: IWinlissyIniAccess;
    xMailAddress, xSendHostIP: string;
begin
    inherited Create;

    xIniAccess := TAppSettings.CreateAppIni();
    fRecipients := xIniAccess.ReadString('MailSender', 'ErrorAutomaticRecipients');
    fCCRecipients := xIniAccess.ReadString('MailSender', 'ErrorAutomaticCCRecipients');

    // Recipients müssen schon eingetragen sein
    if (fRecipients <> '') or (fCCRecipients <> '') then
    begin

        xMailAddress := xIniAccess.ReadString('MailSender', 'Address');
        xSendHostIP := xIniAccess.ReadString('MailSender', 'SendHostIP');

        // MailHost erzeugen
        if (xMailAddress <> '') and (xSendHostIP <> '') then
        begin
            fMailHost := TMailHost.Create(xSendHostIP, xIniAccess.ReadInteger('MailSender', 'SendPort'),
                xMailAddress, xIniAccess.ReadString('MailSender', 'Password'));
            fMailHost.Active := true;
        end;
    end;
end;

destructor TErrorMessageFactoryExt.Destroy;
begin
    FreeAndNil(fMailHost);

    inherited;
end;

function TErrorMessageFactoryExt.ErrorBox(const aErrorInfo: TErrorInfo): integer;
begin
    gLogManager.Log('Errorbox - BEGIN:' + aErrorInfo.InfoText, false);

    TAppSettings.CurrentUser.LogRun(rkdNone, 'Machine Error', aErrorInfo.FullInfoText, '',
        lrtMachineInterrupt);

    WriteErrorLog(aErrorInfo);

    SendMail(aErrorInfo);

    result := (gGUIManager as TGUIManagerSetup).ErrBox_PromptModal(LongInt(aErrorInfo));

    if (result = mrAbort) then
        TAppSettings.CurrentUser.LogRun(rkdNone, 'Machine Error', 'ResultBtn: Abort',
            aErrorInfo.ErrReason, lrtAbort)
    else
        TAppSettings.CurrentUser.LogRun(rkdNone, 'Machine Error',
            Format('ResultBtn: %s', [GetNameFromResult(result)]), aErrorInfo.ErrReason, lrtContinue);

    gLogManager.Log(aErrorInfo.LogText + ' ResultBtn: ' + GetNameFromResult(Result), true);

    gLogManager.Log('Errorbox - END:' + aErrorInfo.InfoText, false);
end;

function TErrorMessageFactoryExt.InterruptRoutine(aSender: TObject; aIRArgs: TInterruptRoutineArgs)
    : TInterruptRoutineResult;
begin
    result := ErrorBox(TErrorInfo(Longint(aIRArgs[0])));
end;

procedure TErrorMessageFactoryExt.SendMail(aErrorInfo: TErrorInfo);
var
    xText: TStringArray;
    x: integer;
    xSubject: string;
begin
    if (fMailHost = nil) then
        EXIT;

    gLogManager.Log('Send Mail to:' + fRecipients + ' CC ' + fCCRecipients, false);

    SetLength(xText, aErrorInfo.AdditionalInfo.Count + 3);

    xSubject := 'Machine: ' + aErrorInfo.Caption;

    xText[0] := aErrorInfo.Caption;
    xText[1] := aErrorInfo.InfoText;
    xText[2] := aErrorInfo.ErrReason;
    for x := 0 to aErrorInfo.AdditionalInfo.Count - 1 do
    begin
        xText[3 + x] := aErrorInfo.AdditionalInfo[x];
    end;

    try
        fMailHost.SendMail(fRecipients, fCCRecipients, xSubject, xText);
    except
        gLogManager.Log('Send Mail failed!', true);
    end;
end;

function TErrorMessageFactoryExt.ShowAnyError(aErrorInfo: TErrorInfo;
    aIgnoreInterruptStartError: boolean): integer;
var
    xResult: TInterruptRoutineResult;
    xIRArgs: TInterruptRoutineArgs;
begin
    xIRArgs := VarArrayOf([LongInt(aErrorInfo)]);

    if GetCurrentThreadID() = System.MainThreadID then
    begin
        // 13.11.06 pk HACK! in the layouter the main thread does motor moves, and if we used the requestinterrupt function
        // the software would hang because the main thread would wait on a lock in the requestinterrupt function
        // The best solution would be to make the motor moves in the layouter run inside of a thread
        result := InterruptRoutine(nil, xIRArgs);
    end
    else
    begin
        ThrMan.RequestInterrupt('Errorbox - ' + aErrorInfo.InfoText, InterruptRoutine, xIRArgs, xResult, true,
            nil, aIgnoreInterruptStartError);
        result := xResult;
    end;
end;

function TErrorMessageFactoryExt.ErrBoxSerialIntf(aErrorCode: TComInterfaceErrorInfoType; aComPort: integer;
    const aModuleName, aInfoText: string): integer;
var
    xErrorInfo: TComInterfaceErrorInfo;
begin
    xErrorInfo := TComInterfaceErrorInfo.Create(aErrorCode, aComPort, aModuleName, aInfoText);
    try
        result := ShowAnyError(xErrorInfo);
    finally
        xErrorInfo.Free;
    end;
end;


end.
