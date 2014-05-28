unit IntfMixerDevice;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  31.01.08 wl  IShakerDevice                TN4003   verschieden Änderungen am Interface
  07.02.08 wl  Reset                        TN4009   neu: Vortexer ausschalten
  17.03.08 wl  IMixerDevice                 TN4043   Klasse umbenannt: Mixer statt Vortexer
  17.03.08 wl  TMixerSpeedWaitForType       TN4043   umbenannt
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    IntfDevice,
    Driver;

type
    TMixerSpeedWaitForType = (vswIfSpeedIsNull, vswYes, vswNo);

    IDisplayableDevice = interface(IDevice)
        ['{EFF10EE9-FBBC-4320-A0A2-D85FA0DBC9F8}']
        function GetName: string;
        function GetAreaName: string;
        function GetSpeedCaption: string;
        function GetTempCaption: string;
        procedure WaitForAnswer();
        property AreaName: string read GetAreaName;
        property name: string read GetName;
    end;

    IMixerDevice = interface(IDisplayableDevice)
        ['{FF467DCF-126D-4DAA-ACD3-C3F3CE23E12E}']
        procedure Reset(aResetID: TDevResetID);
        function HasPulseFunction(): boolean;
        function ReadActualSpeed(): integer;
        procedure SetPulseSpeed(aSpeed, aOnPulse, aOffPulse: integer; aWaitFor: TMixerSpeedWaitForType);
        procedure ResetSpeedAndOpenFixation();
        //
        function GetSpeed(): integer;
        property Speed: integer read GetSpeed;
        function GetOnPulse(): integer;
        property OnPulse: integer read GetOnPulse;
        function GetOffPulse(): integer;
        property OffPulse: integer read GetOffPulse;

        procedure Fixation(aOn: boolean);
    end;


implementation


end.
