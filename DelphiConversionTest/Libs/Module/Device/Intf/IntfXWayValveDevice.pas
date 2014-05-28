unit IntfXWayValveDevice;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  09.06.10 pk                               TN5116   AddPositions, etc removed
  08.11.13 wl                               TN6298   TXWayValvePos -> LiquidManager
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    GeneralTypes,
    IntfDevice;

type
    IXWayValveDevice = interface(IDevice)
        ['{16234D71-6E82-4F73-A2F2-5B2A41766D3C}']
        function GetInputPortCount: integer;
        function GetInputPortName(index: Integer): string;
        function GetCurrentPosition(aMustRead: boolean): integer;
        procedure TurnValve(aPosition: integer);
        procedure CheckPositionReading();
        procedure SimpleTurnValve(aPosition: integer);
        function GetCurrentInputPortName(): string;
        function GetPortNames(): TStringArray;
        property InputPortCount: integer read GetInputPortCount;
        property InputPortName[index: Integer]: string read GetInputPortName;
    end;


implementation


end.
