{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Interface for an object which communicates with the watchdog hardware
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  12.03.07       pk                         TN3631   New
  -------------------------------------------------------------------------------------------------- }

unit IntfWatchDogDriver;


interface


uses
    Driver,
    DriverSettingList;

type
    IWatchDogDriver = interface(IDriver)
        ['{69FB57D5-F0A9-46BD-A37B-5798C4A2DEA7}']
        function GetIntervalInS: integer;
        procedure Start();
        procedure Stop();
        procedure Trigger();
        property IntervalInS: integer read GetIntervalInS;
    end;


implementation


end.
