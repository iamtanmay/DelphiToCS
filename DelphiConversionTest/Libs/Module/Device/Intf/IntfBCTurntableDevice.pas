unit IntfBCTurntableDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  25.01.11 wl  SavePos                      TN4192    called to save original position
  -------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice;

type
    IBCTurntableDevice = interface(IDevice)
        ['{7496C125-D597-4613-82D3-F6E78064CC8C}']
        procedure Turn(aCnt: integer);
        procedure Return();
        procedure Restore();
        procedure SavePos();
    end;


implementation


end.
