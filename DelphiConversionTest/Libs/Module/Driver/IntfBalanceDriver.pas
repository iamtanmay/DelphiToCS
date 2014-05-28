{ --------------------------------------------------------------------------------------------------
  Copyright © 2003 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Interface for the Balance at the interface level
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  17.09.03 pk                                TN1556.1  New
  18.02.05 pk                                TN2317    constants for Calibrate, PreloadSet, PreloadDelete
  03.04.06 thr                               TN3007    Additional Weighing parameters
  16.05.08 wl  DoorAction                    TN4107    Door is not part of the balance drive any move - it has to be a switch device
  16.10.09 pk  DoWeight, Tare                TN4810    New Parameters: aTimeout, oIsTimeout
  -------------------------------------------------------------------------------------------------- }

unit IntfBalanceDriver;


interface


uses
    Driver;

const
    STR_FUNCNAME_INIT = 'Balance_Init';
    STR_FUNCNAME_TARA = 'Balance_Tara';
    STR_FUNCNAME_DOWEIGHT = 'Balance_DoWeight';
    STR_FUNCNAME_COMMAND = 'Balance_Command';

    STR_COMMAND_RESET = 'RESET';
    STR_COMMAND_SAVESETUP = 'SAVESETUP';
    STR_COMMAND_KEYLOCK = 'KEYLOCK';
    STR_COMMAND_KEYUNLOCK = 'KEYUNLOCK';
    STR_COMMAND_IONIZER = 'IONIZER';
    STR_IONIZER_ON = 'ON';
    STR_IONIZER_OFF = 'OFF';
    STR_COMMAND_CALIBRATE = 'CALIBRATE';
    STR_COMMAND_PRELOADSET = 'PRELOADSET';
    STR_COMMAND_PRELOADDELETE = 'PRELOADDELETE';

    STR_SETUP_PREFIX = 'SETUP';
    STR_COMMAND_SETUP_VIBRATION = 'VIBRATION';
    STR_COMMAND_SETUP_FILTER = 'FILTER';
    STR_COMMAND_SETUP_STILLSTAND = 'STILLSTAND';
    STR_COMMAND_SETUP_UNIT = 'UNIT';
    STR_COMMAND_SETUP_TARECONDITION = 'TARECONDITION';
    STR_COMMAND_SETUP_TAREONRESET = 'TAREONRESET';
    STR_COMMAND_SETUP_WEIGHINGRESOLUTION = 'WEIGHINGRESOLUTION';
    STR_COMMAND_SETUP_AUTOZERO = 'AUTOZERO';

    STR_SETUP_VIBRATION_VERYLOW = 'VERYLOW';
    STR_SETUP_VIBRATION_LOW = 'LOW';
    STR_SETUP_VIBRATION_HIGH = 'HIGH';
    STR_SETUP_VIBRATION_VERYHIGH = 'VERYHIGH';

    STR_SETUP_FILTER_NORMAL = 'NORMAL';
    STR_SETUP_FILTER_DOSING = 'DOSING';
    STR_SETUP_FILTER_CALCDOSING = 'CALCDOSING';
    STR_SETUP_FILTER_OFF = 'OFF';

    STR_SETUP_STILLSTAND_QUARTERD = 'QUARTERDIGIT';
    STR_SETUP_STILLSTAND_HALFD = 'HALFDIGIT';
    STR_SETUP_STILLSTAND_ONED = 'ONEDIGIT';
    STR_SETUP_STILLSTAND_TWOD = 'TWODIGIT';
    STR_SETUP_STILLSTAND_FOURD = 'FOURDIGIT';
    STR_SETUP_STILLSTAND_EIGHTD = 'EIGHTDIGIT';

    STR_SETUP_TARECONDITION_ANYTIME = 'ANYTIME';
    STR_SETUP_TARECONDITION_AFTERSTILLSTAND = 'AFTERSTILLSTAND';

    STR_SETUP_UNIT_GRAM = 'G';
    STR_SETUP_UNIT_POUND = 'LB';
    STR_SETUP_UNIT_OUNCE = 'OZ';
    STR_SETUP_UNIT_MILLIGRAM = 'MG';

    STR_SETUP_TAREONRESET_OFF = 'OFF';
    STR_SETUP_TAREONRESET_ON = 'ON';

    STR_RESET_STABLE = 'STABLE';
    STR_RESET_UNSTABLE = 'UNSTABLE';

    STR_SETUP_WEIGHINGRESOLUTION_FINE = 'FINE';
    STR_SETUP_WEIGHINGRESOLUTION_COARSE = 'COARSE';

    STR_COMMAND_SETUP_AUTOZERO_OFF = 'OFF';
    STR_COMMAND_SETUP_AUTOZERO_ON = 'ON';

type
    TWeighUnit = (digNoUnit, digUnitG, digUnitMG);
    TWeighPrecision = (digPrecHigh, digPrecLow);

    IBalanceDriver = interface(IDriver)
        ['{888CB233-B211-4600-A020-94C708CDE654}']
        procedure Init(aWaitTillStable: boolean);
        procedure DoWeight(var aWeight: double; var aUnit: TWeighUnit; var aPrec: TWeighPrecision;
            var aStable: boolean; out oIsTimeout: boolean; aNumValues: integer; aDeviation: double;
            const aTimeout: cardinal; const aShowTimeoutError: boolean; aTarget: double);
        procedure Tare(out oIsTimeout: boolean; aWaitTillStable: boolean; aNumValues: integer;
            aDeviation: double; const aTimeout: cardinal; const aShowTimeoutError: boolean);
        procedure Command(aCommand: string; aParams: string; var aResult: string; aDelimiter: string);
    end;


implementation


end.
