{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  18.06.09 pk  ProcessFinished                       TN4585.2    New aIsError Parameter
  19.11.10 wl  evStartMethodOrScript        TN5358   kein Rückgabewert mehr
  30.07.13 wl                                        TN6160   an TSystemEvents angepasst
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreServerSystemEvents;


interface


uses
    SystemEventsRunner;

type
    TCoreServerSystemEvents = class(TSystemEventsRunner)
    public
        procedure MethodStart(); override;
        procedure MethodFinish(); override;
    end;


implementation


uses
       ThreadAPI,
    CoreServerEventManager,
    ErrorManager;

{ TCoreServerSystemEvents }

procedure TCoreServerSystemEvents.MethodStart();
var
    xSourceDataName: string;
    xProcessID: string;
begin
    inherited;

    xSourceDataName := TThreadAPI.GetCurrentSourceDataName();
    xProcessID := TThreadAPI.GetCurrentThreadDescription();
    ASSERT(TCoreServerEventManager.Instance.ProcessStarted(xSourceDataName, xProcessID, true),
        'No start message received');
end;

procedure TCoreServerSystemEvents.MethodFinish();
var
    xProcessID: string;
begin
    xProcessID := TThreadAPI.GetCurrentThreadDescription();
    TCoreServerEventManager.Instance.ProcessFinished(xProcessID, gErrorManager.IsGlobalErr, true);
    inherited;
end;


end.
