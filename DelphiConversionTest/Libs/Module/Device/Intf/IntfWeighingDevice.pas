unit IntfWeighingDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.05.08 pk  DoorSwitch                   TN4092   removed from WeighingDevice
  -------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice,
    IntfBalanceDevice,
    IntfSwitchDevice,
    IntfSensorDevice;

type
    IWeighingDevice = interface(IDevice)
        ['{B3671F15-4232-425E-833F-02D636B6B9F6}']
        procedure SetUseTubeSensor(const aValue: boolean);
        function GetUseTubeSensor: boolean;
        function GetBalance: IBalanceDevice;
        function GetTubeSensor: ISensorDevice;
        property Balance: IBalanceDevice read GetBalance;
        property TubeSensor: ISensorDevice read GetTubeSensor;
        property UseTubeSensor: boolean read GetUseTubeSensor write SetUseTubeSensor;
    end;


implementation


end.
