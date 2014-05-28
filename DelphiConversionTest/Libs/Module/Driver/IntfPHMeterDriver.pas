{ --------------------------------------------------------------------------------------------------
  Copyright  2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert
  Description  : Device that can return a measured ph value
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  03.10.11 ts                               TN5574   initial revision
  14.10.11 ts                               TN5712   internal calculation of stability added
  -------------------------------------------------------------------------------------------------- }

unit IntfPHMeterDriver;


interface


uses
    Driver;

type
    IPHMeterDriver = interface(IDriver)
        ['{466F067E-E03E-4CB6-8AFC-70DAD9012B37}']
        function GetPHValue(aStable, aManual: boolean; aIntCalcNumValues: integer; aIntCalcDeviation: double;
            aIntCalcTimeout: integer; aIntCalcEnabled: boolean): double;
        function GetCalib: double;
        function DoCalib(aOperator: string; aTemperature: double;
            aReference1, aReference2, aCalib1, aCalib2: double): boolean;
    end;


implementation


end.
