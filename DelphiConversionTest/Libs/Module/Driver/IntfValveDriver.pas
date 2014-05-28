{ --------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  : Device that can turn a valve to a position
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  10.12.09 ts                               TN4921   initial revision
  -------------------------------------------------------------------------------------------------- }

unit IntfValveDriver;


interface


uses
    Driver,
    IntfPipPumpDriver;

type
    IValveDriver = interface(IDriver)
        ['{B738B0C6-B377-4314-AEBF-D02BA7C83737}']
        procedure Init(aInitID: TDevInitID);
        procedure TurnValve(aNewValvePos: TValvePos);
    end;


implementation


end.
