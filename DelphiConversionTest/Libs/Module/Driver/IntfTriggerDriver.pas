{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Device that can store a string (or other) value
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  29.01.08 pk                                        initial version
  08.04.09 ts  Reset                         TN4514  SetTriggerAtReset damit Trigger bei Reset gesetzt werden kann
  -------------------------------------------------------------------------------------------------- }

unit IntfTriggerDriver;


interface


uses
    Driver;

type
    ITriggerDriver = interface(IDriver)
        ['{DA0CA033-687A-4E33-BD89-92BDA71E78C6}']
        procedure StartTrigger();
        procedure Reset(aResetID: TDevResetID);
    end;


implementation


end.
