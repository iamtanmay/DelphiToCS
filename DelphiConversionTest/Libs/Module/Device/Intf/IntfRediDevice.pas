{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  15.02.13 wl  fVolumes                            TN5914   von TipSystem hierher
  ----------------------------------------------------------------------------------------------------------- }

unit IntfRediDevice;


interface


uses
    Generics.Collections,
    VolumeInfo,
    IntfDevice;

type
    IRediDevice = interface(IDevice)
        ['{2420A62D-DEC2-4AAD-A1E2-CA1E0ACA11DC}']
        procedure PumpSwitchOnOrOff(aOn, aExecuteWait: boolean; aDelayAfter: integer = 0);
        procedure VacuumSwitchOnOrOff(aOn, aExecuteWait: boolean; aDelayAfter: integer = 0);
        procedure BlowerSwitchOnOrOff(aOn, aExecuteWait: boolean; aDelayAfter: integer = 0);
        procedure FeedingSwitchOnOrOff(aOn, aExecuteWait: boolean; aDelayAfter: integer = 0);
        procedure MotorSetVol(aVolume: double; var vMotorSteps: integer);
        function HasMotor(): boolean;
        procedure MotorSetMinMaxVols(aMinVolume, aMaxVolume: double);
        function GetVolumes: TObjectList<TVolumeInfo>;
        property Volumes: TObjectList<TVolumeInfo>read GetVolumes;
    end;


implementation


end.
