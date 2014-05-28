{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  14.09.11 wl                               TN5672   von ThrdMan hierher
  ---------------------------------------------------------------------------------------------------------------------- }

unit ThreadAPICallbackImplementor;


interface


uses
    SysUtils;

type
    TThreadAPICallbackImplementor = class
    public
        class procedure ShowThreadException(const aCaption: string; aException: Exception);
        class procedure Log(const aText: string);
    end;


implementation


uses
    Windows,
    AppTypes,
    LogManager,
    GUIManager,
    ErrorManager;

{ TThreadAPICallbackImplementor }

class procedure TThreadAPICallbackImplementor.Log(const aText: string);
begin
    if not Assigned(TLogManager.Instance) then
        EXIT;

    TLogManager.Instance.Log(aText, false);
end;

class procedure TThreadAPICallbackImplementor.ShowThreadException(const aCaption: string;
    aException: Exception);
begin
    if not Assigned(gGUIManager) then
        EXIT;
    if not Assigned(gErrorManager) then
        EXIT;

    gGUIManager.MessageBox(aException.Message, aCaption, MB_OK + MB_ICONSTOP);
    gErrorManager.SetGlobalErr(ERR_APPLICATION, '');
end;


end.
