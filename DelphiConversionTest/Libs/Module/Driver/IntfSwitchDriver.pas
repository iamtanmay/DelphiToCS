{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Device that can store a string (or other) value
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  28.01.08 wl  SetNeverReset                TN3980   AtReset & AtInit unterschieden
  19.02.08 wl                               TN4009   Init & Reset ´neu, Relay-Funktionen entfernt
  -------------------------------------------------------------------------------------------------- }

unit IntfSwitchDriver;


interface


uses
    Driver;

type
    TSingleSwitchState = (swsUnknown, swsOff, swsOn);
    TDoubleSwitchState = (dswsUnknown, dswsOff, dswsDefault, dswsOn, dswsBothOn);

    ISwitchDriver = interface(IDriver)
        ['{C110DE11-A570-4BE4-A81E-DEAD031F7981}']
        procedure Init(aInitID: TDevInitID);
        procedure Reset(aResetID: TDevResetID);
        procedure Switch(aValue: TDoubleSwitchState; aDelayAfter_msec: int64);

        function HasDefaultPort(): boolean;
        function GetState(): TDoubleSwitchState;
        property State: TDoubleSwitchState read GetState;
    end;


implementation


end.
