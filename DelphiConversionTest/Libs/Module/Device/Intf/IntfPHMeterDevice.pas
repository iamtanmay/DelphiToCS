unit IntfPHMeterDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Nearly a switch device, but has no state
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  17.04.08 wl                               TN3726   initial version
  03.10.11 ts                               TN5574   calibration functions added
  14.10.11 ts                               TN5712   internal calculation of stability added
  -------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice;

type
    IPHMeterDevice = interface(IDevice)
        ['{B3560289-C22F-41E3-9B29-29B550ECF9ED}']
        function GetPHValue(aStable, aManual: boolean; aIntCalcNumValues: integer; aIntCalcDeviation: double;
            aIntCalcTimeout: integer; aIntCalcEnabled: boolean): double;
        function GetCalib: double;
        function DoCalib(aOperator: string; aTemperature: double;
            aReference1, aReference2, aCalib1, aCalib2: double): boolean;
    end;


implementation


end.
