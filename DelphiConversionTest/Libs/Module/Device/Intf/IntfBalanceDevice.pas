unit IntfBalanceDevice;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  13.11.07 wl  SimpleReadWeight_mg          TN3844   neu (PWDET)
  17.12.08 wl  StartWeight                  TN4359   neuer Parameter aStoreAsTare
  16.10.09 pk  Wait                         TN4810   New
  -------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice,
    Controls;

const
    STR_DEVICE_BALANCE_PARAM_ON = 'ON';
    STR_DEVICE_BALANCE_PARAM_OFF = 'OFF';

    STR_DEVICE_BALANCE_FUNC_CALIBRATE = 'CALIBRATE';
    STR_DEVICE_BALANCE_FUNC_PRELOADSET = 'PRELOADSET';
    STR_DEVICE_BALANCE_FUNC_PRELOADDELETE = 'PRELOADDELETE';
    STR_DEVICE_BALANCE_FUNC_IONIZER = 'IONIZER';
    STR_DEVICE_BALANCE_FUNC_KEYLOCK = 'KEYLOCK';

    INT_THREAD_WAIT_SEC = 300; // 300 Sekunden

type
    TVCLType = (vclPaint);

    IBalanceDevice = interface(IDevice)
        ['{7304941A-D08C-42D9-ACF0-4167C6A10560}']
        procedure SetBalanceDoorBlocked(const aValue: boolean);
        function GetBalanceDoorBlocked: boolean;
        procedure StartTare(aNumValues: integer; aDeviation: double);
        function Wait(out oWeight: double; out oIsTimeout: boolean): boolean;
        procedure OpenDoor;
        procedure StartWeight(const aRackID: string; aTubePos: integer; aSubstID: string; aNumValues: integer;
            aDeviation, aTarget: double; aStoreAsTare: boolean = false);
        procedure CreateBalDigit(aWinControl: TWinControl);
        procedure DestroyBalDigit();
        function AskTara: boolean;
        procedure WaitForBalanceThread;
        function GetCarrierName: string;
        function SimpleReadWeight_mg(): double;
        property BalanceDoorBlocked: Boolean read GetBalanceDoorBlocked write SetBalanceDoorBlocked;
    end;

    TBalanceArray = array of IBalanceDevice;


implementation


end.
