unit IntfTriggerDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Nearly a switch device, but has no state
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.01.08 wl                               TN3980    initial version
  08.04.09 pk  Reset                        TN4514    New
  -------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice,
    Driver;

type
    ITriggerDevice = interface(IDevice)
        ['{9A51859B-D727-4FD2-92EE-09986A2BDAAE}']
        procedure Reset(aResetID: TDevResetID);
        procedure StartTrigger();
    end;


implementation


end.
