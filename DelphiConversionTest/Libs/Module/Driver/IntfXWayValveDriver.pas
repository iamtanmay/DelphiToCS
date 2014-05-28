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

unit IntfXWayValveDriver;


interface


uses
    Driver;

type
    IXWayValveDriver = interface(IDriver)
        ['{0A99C692-1173-4C98-AEC6-509FB896D4C0}']
        procedure XWayValve_Turn(aPosition: integer);
        function XWayValve_GetPosition(aMustRead: boolean): integer;
        procedure XWayValve_CheckPositionReading();
    end;


implementation


end.
