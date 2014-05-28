{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  10.08.12 wl                                      TN5947   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit IntfPeriPumpDevice;


interface


uses
    IntfDevice;

type
    IPeriPumpDevice = interface(IDevice)
        ['{482E1D99-E766-4320-B4BA-0AAC41F0D3F5}']
        procedure Dispense(aVolume: extended);
        function GetInputPortName(): string;
        property InputPortName: string read GetInputPortName;
    end;


implementation


end.
