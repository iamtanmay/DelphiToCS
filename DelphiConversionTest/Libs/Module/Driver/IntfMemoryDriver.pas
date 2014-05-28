{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Device that can store a string (or other) value
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  28.01.08 wl                               TN3980    initial version
  -------------------------------------------------------------------------------------------------- }

unit IntfMemoryDriver;


interface


uses
    Driver;

type
    IMemoryDriver = interface(IDriver)
        ['{7AAA5F3F-8091-4408-92AB-BF5B352BD4EA}']
        function ReadValueAsString(): string;
        procedure WriteValueAsString(aValue: string);
    end;


implementation


end.
