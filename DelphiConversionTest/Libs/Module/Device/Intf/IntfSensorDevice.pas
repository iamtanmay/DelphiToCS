unit IntfSensorDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.09.07 pk                                         Initial Revision
  09.11.07 pk  GetTipNumber                 TN3864
  -------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice;

type
    TSensorValue = (svNone, svDefault, svNotDefault);

    ISensorDevice = interface(IDevice)
        ['{5D40F749-D933-49E2-87A8-F071B083445F}']
        function AskValue(): TSensorValue;
        function ValueIsDefault(): boolean;
        function GetTipNumber: integer;
        property TipNumber: integer read GetTipNumber;
    end;


implementation


end.
