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

unit IntfProgrammableDriver;


interface


type
    IProgrammableDriver = interface
        ['{3F386256-D4A7-46BE-A62B-B369B7E6969C}']
        procedure StartProgram(const aParameters: string);
        function WaitForProgram(const aParameters: string; const aTimeout: int64): boolean;
    end;


implementation


end.
