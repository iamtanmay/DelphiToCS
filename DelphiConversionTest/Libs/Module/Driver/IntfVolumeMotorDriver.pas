{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Device that can store a string (or other) value
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  29.01.08 pk                                   initial version
  11.01.11 wl  Init                         TN5424   mit aInitID
  -------------------------------------------------------------------------------------------------- }

unit IntfVolumeMotorDriver;


interface


uses
    Driver;

type
    IVolumeMotorDriver = interface(IDriver)
        ['{C84FFBCB-F994-4C57-881E-A307A251D204}']
        procedure Init(aInitID: TDevInitID);
        procedure SetMinMaxVols(aMinVolume, aMaxVolume: double);
        procedure MoveMotorVol(aVolume: extended; out oVarixMotorSteps: integer);
        procedure MoveMotorStep(var vVarixMotorSteps: integer);
        function GetMinVolume: extended;
        property MinVolume: extended read GetMinVolume;
    end;


implementation


end.
