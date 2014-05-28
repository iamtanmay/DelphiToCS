{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  08.09.09 pk                                        TN4753     code from ErrorMessage.pas
  12.10.09 pk                                        TN4812     made streamable
  04.02.10 pk                                        TN4972     changes for Restart
  10.01.12 ts                                        TN5760     Beep functionality reactivated, GlobalErrBeep from ErrorMessage moved to ErrorInfo
  30.07.13 wl  GlobalErrBeep                         TN6160   --> EscapeManager
  ----------------------------------------------------------------------------------------------------------------------- }

unit ErrorInfo;


interface


uses
    Classes,
    Streamable;

type
    TErrorInfoButtons = (eibAbort, eibAbortRetry, eibAbortIgnore, eibAbortRetryIgnore);

    TErrorInfo = class(TStreamableItem)
    private
        fErrReason: string;
    protected
        fInfoText: string;
        fCaption: string;
        fAdditionalInfo: TStreamableStringList;
        fButtons: TErrorInfoButtons;
        fRetryHint: string;
        fIgnoreHint: string;
        fAbortHint: string;
        //
        function GetFullInfoText: string; virtual;
        function GetLogText: string; virtual;
        procedure AddCauseAndAction(const aCause, aAction: string);
    public
        constructor Create(); override;
        class function CreateAndInit(const aText, aCaption: string; aButtons: TErrorInfoButtons): TErrorInfo;
        destructor Destroy(); override;
        //
        procedure Init(const aText, aCaption: string; aButtons: TErrorInfoButtons);
        procedure AddText(const aText: string);

        property FullInfoText: string read GetFullInfoText;
        property LogText: string read GetLogText;
    published
        property Caption: string read fCaption write fCaption;
        property InfoText: string read fInfoText write fInfoText;
        property ErrReason: string read fErrReason write fErrReason;
        property AdditionalInfo: TStreamableStringList read fAdditionalInfo write fAdditionalInfo;
        property Buttons: TErrorInfoButtons read fButtons write fButtons;
        property AbortHint: string read fAbortHint write fAbortHint;
        property IgnoreHint: string read fIgnoreHint write fIgnoreHint;
        property RetryHint: string read fRetryHint write fRetryHint;
    end;


implementation


{ TErrorInfo }

constructor TErrorInfo.Create();
begin
    inherited Create();

    fAbortHint := 'Stop all forthcoming actions!';
    fRetryHint := '';
    fIgnoreHint := '';
    fAdditionalInfo := TStreamableStringList.Create;
    fCaption := '';
    fInfoText := '';
end;

class function TErrorInfo.CreateAndInit(const aText, aCaption: string; aButtons: TErrorInfoButtons)
    : TErrorInfo;
begin
    result := TErrorInfo.Create();
    result.Init(aText, aCaption, aButtons);
end;

destructor TErrorInfo.Destroy;
begin
    fAdditionalInfo.Free;

    inherited;
end;

function TErrorInfo.GetFullInfoText: string;
begin
    result := fInfoText;
end;

function TErrorInfo.GetLogText: string;
begin
    result := fInfoText;
end;

procedure TErrorInfo.Init(const aText, aCaption: string; aButtons: TErrorInfoButtons);
begin
    fInfoText := aText;
    fCaption := aCaption;
    fButtons := aButtons;
end;

procedure TErrorInfo.AddCauseAndAction(const aCause, aAction: string);
begin
    fAdditionalInfo.Add(aCause);
    fAdditionalInfo.Add(' -> ' + aAction);
    fAdditionalInfo.Add('');
end;

procedure TErrorInfo.AddText(const aText: string);
begin
    fAdditionalInfo.Add(aText);
end;


end.
