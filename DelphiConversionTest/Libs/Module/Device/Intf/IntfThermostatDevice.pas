unit IntfThermostatDevice;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  31.01.08 wl  IThermoDevice                TN4003   verschieden Änderungen am Interface
  07.02.08 wl  IThermostatDevice            TN4003   Reset mit ResetID
  17.03.08 wl                               TN4043   uses IntfMixerDevice
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    IntfMixerDevice,
    Driver;

type
    IThermostatDevice = interface(IDisplayableDevice)
        ['{647FDD6A-1495-4348-9934-BA3373E90E94}']
        procedure Reset(aResetID: TDevResetID);
        function GetTempCaption: string;
        procedure SetTargetTemp(aTemperature: extended);
        function ReadActualTemp(): extended;
        function GetTargetTemp(): extended;
    end;


implementation


end.
