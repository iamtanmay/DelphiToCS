unit IntfValveDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  : Device that can turn positions of a valve
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  10.12.09 ts                               TN4921   initial revision
  -------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice,
    Driver,
    IntfPipPumpDriver;

type
    IValveDevice = interface(IDevice)
        ['{54215194-F92D-46AA-AC7C-74E1285C9158}']
        procedure Init(aInitID: TDevInitID);
        procedure TurnValve(aNewValvePos: TValvePos);
    end;


implementation


end.
