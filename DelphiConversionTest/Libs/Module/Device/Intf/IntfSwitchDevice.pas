unit IntfSwitchDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Device that can store a string (or other) value
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  28.01.08 wl  SetNeverReset                TN3980   AtReset & AtInit unterschieden
  19.02.08 wl                               TN4009   Init & Reset neu, Relay-Funktionen entfernt
  12.12.08 wl  SwitchOnOrOff                TN4363   entfernt
  28.08.09 pk                               TN4753   from ObjModul
  07.04.14 ts                               TN6370   new: SwitchForToolReturn
  -------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice,
    IntfSwitchDriver,
    Driver;

const
    STR_SWITCH_WILDCARD = '*';

type
    ISwitchDevice = interface(IDevice)
        ['{59CC5EFC-65AB-4463-95E5-585755E414D7}']
        procedure Init(aInitID: TDevInitID);
        procedure Reset(aResetID: TDevResetID);
        function GetSwitchState: TDoubleSwitchState;
        function GetDelay: integer;
        function GetRackName: string;
        function HasDefaultPort(): boolean;
        function GetSwitchForToolReturn: boolean;
        procedure SwitchOn(aExecuteWait: boolean; aDelayAfter: integer = 0);
        procedure SwitchOff(aExecuteWait: boolean; aDelayAfter: integer = 0);
        procedure SwitchToDefault(aExecuteWait: boolean; aDelayAfter: integer = 0);
        procedure SwitchBothOn(aExecuteWait: boolean; aDelayAfter: integer = 0);
        property SwitchState: TDoubleSwitchState read GetSwitchState;
        property Delay: integer read GetDelay;
        property RackName: string read GetRackName;
        property SwitchForToolReturn: boolean read GetSwitchForToolReturn;
    end;


implementation


end.
