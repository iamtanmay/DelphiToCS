{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Mixer Driver (Shaker, Vortexer or Stirrer)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.01.11 wl  IModuleMotorExecute          TN5357   neu: SingleExecute nur noch für Motoren
  13.01.12 ts  IModuleMotorExecute          TN5769   SingleExecuteWait (für ZP01 notwendig, sonst wird bei SingleExecute nicht gewartet)
  -------------------------------------------------------------------------------------------------- }

unit IntfModuleExecute;


interface


type
    IModuleExecute = interface(IInterface)
        ['{E09E531A-C812-4EC7-BBE5-26CDCDB71566}']
        procedure Execute();
    end;

    IModuleMotorExecute = interface(IModuleExecute)
        ['{2CEDD24F-BA5F-4708-A799-85BAA2E5C7A9}']
        procedure Execute();
        procedure SingleExecute();
        procedure SingleExecuteWait();
    end;


implementation


end.
