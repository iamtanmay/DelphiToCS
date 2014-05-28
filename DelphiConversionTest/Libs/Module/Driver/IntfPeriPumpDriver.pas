{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Device that can store a string (or other) value
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  29.01.08 pk                                   initial version
  -------------------------------------------------------------------------------------------------- }

unit IntfPeriPumpDriver;


interface


uses
    Driver;

type
    IPeriPumpDriver = interface(IDriver)
        ['{0B05EDD9-1FE4-488C-AAC5-E13BAA4327C6}']
        procedure Disp(aVolume: extended);
    end;


implementation


end.
