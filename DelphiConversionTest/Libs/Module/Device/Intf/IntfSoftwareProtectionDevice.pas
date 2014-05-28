unit IntfSoftwareProtectionDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                    track-no  improvement/change
  -------- --  ----------------------------------------  --------  ------------------------------------------------------
  03.07.08 wl                                            TN4157
  06.11.08 pk  TCreateThreadEvent                        TN4280    Parameters changed
  24.11.08 pk  TCreateThreadEvent                        TN4280    TSysHandleID returned
  09.03.10 pk                                            TN5015    Massive changes in ThreadClasses
  ----------------------------------------------------------------------------------------------------------------------- }


interface


uses
    Classes,
    IntfDevice,
    Executable,
    ThreadClasses;

type
    TCreateThreadEvent = function(const aDescription: string; const aCreateSuspended, aCloseHandleOnFinish,
        aCanSuspendSafe: boolean; const aExecutable: IExecutable): TSysHandleID of object;

    ISoftwareProtectionDevice = interface(IDevice)
        ['{EE2B17EC-FB55-45B7-9A34-6EDC508A90B6}']
        function GetOnCreateThread: TCreateThreadEvent;
        procedure SetOnCreateThread(const Value: TCreateThreadEvent);
        procedure ProtectionOff;
        procedure ProtectionOn;
        property OnCreateThread: TCreateThreadEvent read GetOnCreateThread write SetOnCreateThread;
    end;


implementation


end.
