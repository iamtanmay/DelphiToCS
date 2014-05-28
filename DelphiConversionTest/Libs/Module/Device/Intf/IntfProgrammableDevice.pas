{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  29.09.11 wl                                      TN5700   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit IntfProgrammableDevice;


interface


type
    IProgrammableDevice = interface
        ['{4B37FC16-6200-4C10-A8BC-D80FF512B3C6}']
        procedure StartProgram(const aParameters: string);
        function WaitForProgram(const aParameters: string; const aTimeout: int64): boolean;
    end;


implementation


end.
