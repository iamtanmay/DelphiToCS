{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  26.07.13 wl                               TN6160   new States: sStateTest, sStateInterruptMessage
  26.07.13 wl                               TN6160   sStateReady = sStateSetup
  26.07.13 wl                               TN6160   sStateInterruptError = sStateError
  26.07.13 wl  IStateSignalDevice           TN6160   Interface vereinfacht
  ---------------------------------------------------------------------------------------------------------------------- }

unit IntfStateSignalDevice;


interface


uses
    IntfDevice;

type
    TSystemState = (sStateActive, sStateReady, sStateTest, sStateInterruptMessage, sStateInterruptError,
        sStateUnknown);

    IStateSignalDevice = interface(IDevice)
        ['{4752F02A-D555-4B0B-BF14-2B92EC301CDC}']
        procedure ChangeSignal(aState: TSystemState);
        function GetSwitchNameForState(aState: TSystemState): string;
        function GetCurrentState(): TSystemState;
    end;


implementation


end.
