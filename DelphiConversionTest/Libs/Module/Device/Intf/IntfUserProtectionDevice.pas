unit IntfUserProtectionDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                          track-no improvement/change
  -------- --  ------------------------------  -------- -----------------------------------------------
  19.11.10 wl  ProtectionOn,ProtectionUnpause  TN5358   kein Rückgabewert mehr
  17.10.12 ts                                  TN5993   ProtectionPoll als property, damit InterruptMonitor in ExecHandler beendet werden kann
  18.10.12 ts                                  TN5995   ProtectionPoll schreibbar, PollDelay, damit InterruptMonitor in ExecHandler für Flush korrekt beendet werden kann
  -------------------------------------------------------------------------------------------------- }


interface


uses
    ThreadClasses,
    InterruptMonitor,
    IntfDevice;

type
    TCreateProtectionPollEvent = function(const aInterruptText: string; aPollDelay: integer;
        aInterruptRoutine: TInterruptRoutineEvent; aOnCheck: TInterruptCheckEvent)
        : TInterruptMonitor of object;

    IUserProtectionDevice = interface(IDevice)
        ['{1C5CB647-8930-46C5-A10F-9771AF86A3D5}']
        function GetOnCreateProtectionPoll: TCreateProtectionPollEvent;
        procedure SetOnCreateProtectionPoll(const Value: TCreateProtectionPollEvent);
        function GetProtectionPoll: TInterruptMonitor;
        procedure SetProtectionPoll(aProtectionPoll: TInterruptMonitor);
        function GetProtectionPollDelay: integer;
        procedure ProtectionOn();
        procedure ProtectionOff();
        procedure ProtectionPause();
        procedure ProtectionUnpause();
        property OnCreateProtectionPoll: TCreateProtectionPollEvent read GetOnCreateProtectionPoll
            write SetOnCreateProtectionPoll;
        property ProtectionPoll: TInterruptMonitor read GetProtectionPoll write SetProtectionPoll;
        property ProtectionPollDelay: integer read GetProtectionPollDelay;
    end;


implementation


end.
