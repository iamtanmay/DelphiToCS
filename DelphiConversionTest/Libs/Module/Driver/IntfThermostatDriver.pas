{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Device that can store a string (or other) value
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  29.01.08 pk                                        initial version
  31.01.08 wl  IThermostatDriver            TN4003   verschieden Änderungen am Interface
  07.02.08 wl  Reset                        TN4009   mit TDevResetID als Parameter
  14.04.09 pk  StandBy                      TN4524   New
  17.08.10 wl  ReadActualTemp,GetTargetTemp TN5222.3  Rückgabewert bool: wurde überhaupt etwas gelesen?
  -------------------------------------------------------------------------------------------------- }

unit IntfThermostatDriver;


interface


uses
    Driver;

type
    IThermostatDriver = interface(IDriver)
        ['{588184C1-0E57-4E80-95E4-9E3D33178F9E}']
        procedure Init(aInitID: TDevInitID);
        procedure Reset(aResetID: TDevResetID);
        procedure StandBy(const aStandBy: boolean);
        function ReadActualTemp(var oTempValue: extended): boolean;
        function GetTargetTemp(var oTempValue: extended): boolean;
        procedure SetTargetTemp(aValue: extended);
    end;


implementation


end.
