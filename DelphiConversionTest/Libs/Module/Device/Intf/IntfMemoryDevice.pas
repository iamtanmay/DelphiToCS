unit IntfMemoryDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Device that can store a string (or other) value
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  23.01.08 wl                               TN3980    initial version
  -------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice;

type
    IMemoryDevice = interface(IDevice)
        ['{865E772F-2958-4CCC-9ADB-537D680F8575}']
        // function GetDelay: integer;
        function ReadValueAsString(): string;
        procedure WriteValueAsString(aValue: string);
        procedure Init(aInitID: TDateTime);
        // property Delay : integer read GetDelay;
    end;


implementation


end.
