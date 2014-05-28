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

unit IntfBCReaderDriver;


interface


uses
    Driver;

type
    IBCReaderDriver = interface(IDriver)
        ['{6F35464F-FE6E-4684-B089-BB74F146979F}']
        procedure Init;
        function LongRead(var aErrCode: integer; aCodeFilter: string): string;
        function GetBarcode(var aErrCode: integer; aCodeFilter: string): string;
        procedure TriggerOn();
        procedure ResetWithRead(var aErrCode: integer; aCodeFilter: string; var aBarcode: string);
        procedure Reset();
    end;


implementation


end.
