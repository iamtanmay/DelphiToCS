unit IntfThermometerDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Nearly a switch device, but has no state
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  17.04.08 wl                               TN3726   initial version
  -------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice;

type
    IThermometerDevice = interface(IDevice)
        ['{B3560289-C22F-41E3-9B29-29B550ECF9ED}']
        function GetPHValue(aStable: boolean): double;
    end;


implementation


end.
