{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : Mixer Driver (Shaker, Vortexer or Stirrer)
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  29.01.08 pk                                   initial version
  31.01.08 wl  IVortexerDriver              TN4003   verschieden Änderungen am Interface
  07.02.08 wl  Reset                        TN4009   mit TDevResetID als Parameter
  17.03.08 wl  IMixerDriver                 TN4043   Klasse umbenannt: Mixer statt Vortexer
  -------------------------------------------------------------------------------------------------- }

unit IntfMixerDriver;


interface


uses
    Driver;

type
    IMixerDriver = interface(IDriver)
        ['{92EAD026-84CC-4535-8EA1-88FFED7F4A70}']
        procedure Init(aInitID: TDevInitID);
        procedure Reset(aResetID: TDevResetID);
        function HasPulseFunction(): boolean;
        function ReadActualSpeed(): integer;
        procedure SetPulseSpeed(aSpeed, aOnPulse, aOffPulse: integer; aWaitFor: boolean);
        //
        function GetSpeed(): integer;
        property Speed: integer read GetSpeed;
        function GetOnPulse(): integer;
        property OnPulse: integer read GetOnPulse;
        function GetOffPulse(): integer;
        property OffPulse: integer read GetOffPulse;
    end;


implementation


end.
