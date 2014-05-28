{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  30.07.09 pk                                        TN4585.5    Various Changes
  23.09.09 pk                                        TN4793      GetIsActive moved to protected
  12.10.09 pk  TransmitRunInfoInsert                 TN4812      New
  12.11.12 wl                                        TN6008   Log-Funktion angepasst
  ----------------------------------------------------------------------------------------------------------------------- }

unit CoreEventTransmitter;


interface


type

    TCoreEventTransmitter = class
    protected
        function GetIsActive: boolean; virtual; abstract;
    public
        function Connect(const aAddress: string; const aPort: integer): boolean; virtual; abstract;
        procedure Disconnect(); virtual; abstract;
        procedure ControlEventPending(); virtual; abstract;
        procedure TransmitRunInfoInsert(const aDisplayID: string; const aGroupNames: TArray<string>;
            const aKey, aText: string; aInfoGroupBehaviour: integer); virtual; abstract;
        procedure TransmitWriteLogEvent(const aLogText: string; aThreadID: integer; aDisplayType: integer);
            virtual; abstract;
        procedure TransmitLoadDisplayComponentEvent(const aDisplayComponentName: string;
            const aContextID: string); virtual; abstract;
        procedure TransmitLinkProcessToRunIDEvent(const aRunID, aProcessID: string); virtual; abstract;
        property IsActive: boolean read GetIsActive;
    end;


implementation


end.
