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

unit IntfSensorDriver;


interface


uses
    Driver;

const
    INT_SENSORREAD_VALUE_NONE = -1;
    // When IIntfSensor.SensorRead is called this value is returned if the Sensor hardware is not active

type
    ISensorDriver = interface(IDriver)
        ['{CD3B7DAE-62D3-40C2-897D-A6BE8FA54742}']
        function GetDefaultValue(): integer;
        function read(): integer;
        property DefaultValue: integer read GetDefaultValue;
    end;


implementation


end.
