{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl), Payman Kamali (pk)
  Description  : Device interfaces for tube and rack barcode reader devices
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  22.04.08 pk                               TN4080   Initial Revision
  15.07.10 pk                               TN5194   New ScanSpeed, ScanRamp
  15.04.11 ts                               TN5555   new Setting TurnAfterTriggerOn
  19.04.11 ts                               TN5555   new Setting ReturnBeforeGetBC
  09.09.11 ts                               TN5687   new Setting RetryRead
  29.03.12 wl  SetGraphicsColor             TN5845   entfernt
  -------------------------------------------------------------------------------------------------- }

unit IntfContainerBCReaderDevice;


interface


uses
    CommonTypes,
    IntfDevice,
    IntfBCReaderDevice,
    IntfBCTurnTableDevice,
    Driver;

type
    IContainerBCReaderDevice = interface(IDevice)
        function GetBCReader(): IBCReaderDevice;
        function GetBCScanRange(): BCSCANRANGE;
        function GetBCPosition(): BCRPOSITION;
        function GetXScanSpeed(): integer;
        function GetXScanRamp(): integer;
        function GetYScanSpeed(): integer;
        function GetYScanRamp(): integer;
        function GetZScanSpeed(): integer;
        function GetZScanRamp(): integer;

        function GetBCDelLastPos(): integer;
        property BCReader: IBCReaderDevice read GetBCReader;
        property BCScanRange: BCSCANRANGE read GetBCScanRange;
        property BCPosition: BCRPOSITION read GetBCPosition;
        property BCDelLastPos: integer read GetBCDelLastPos;
        property XScanSpeed: integer read GetXScanSpeed;
        property XScanRamp: integer read GetXScanRamp;
        property YScanSpeed: integer read GetYScanSpeed;
        property YScanRamp: integer read GetYScanRamp;
        property ZScanSpeed: integer read GetZScanSpeed;
        property ZScanRamp: integer read GetZScanRamp;
    end;

    ITubeBCReaderDevice = interface(IContainerBCReaderDevice)
        ['{14C02EB0-3873-4647-9BF2-8EBD81700A6D}']
        function GetCodeFilter(const aToolName, aRackTypeName: string): string;
        function GetBCTubePosition(const aToolName, aRackTypeName: string): BCRPOSITION;
        function HasTurnDevice: boolean;
        function GetTurnDevice(): IBCTurnTableDevice;
        function GetNoOfTurns(): integer;
        function GetTurnAfterTriggerOn(): boolean;
        function GetReturnBeforeGetBC(): boolean;
        function GetRetryRead(): integer;
        property TurnDevice: IBCTurnTableDevice read GetTurnDevice;
        property NoOfTurns: integer read GetNoOfTurns;
        property TurnAfterTriggerOn: boolean read GetTurnAfterTriggerOn;
        property ReturnBeforeGetBC: boolean read GetReturnBeforeGetBC;
        property RetryRead: integer read GetRetryRead;
    end;

    IRackBCReaderDevice = interface(IContainerBCReaderDevice)
        ['{E1BDF717-A2F0-44E3-85C8-594A03E5EA91}']
        function GetCodeFilter: string;
    end;


implementation


end.
