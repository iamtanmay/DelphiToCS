{ --------------------------------------------------------------------------------------------------
  Copyright  2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Herculano De Biasi (hd)
  Description  : Driver for linear motors, for example, linear track
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  18.03.08 hd                               TN4163    initial version
  10.12.08 wl                               TN4358    an LinearMotor01Driver angepasst
  20.01.09 wl  TLinearMotorArmIsSafeEvent   TN4358    neu
  27.09.09 ts  PositionAccepted             TN4678    neu: true, wenn aktuelle Position und Zielposition gleich sind (Toleranzbereich)
  -------------------------------------------------------------------------------------------------- }

unit IntfLinearMotorDriver;


interface


uses
    Driver;

type
    TLinearMotorArmIsSafeEvent = function(aSender: TObject): boolean of object;
    TLinearMotorArmClearErrorEvent = procedure(aSender: TObject) of object;

    ILinearMotorDriver = interface(IDriver)
        ['{F82E3C3C-EB59-4621-8B13-CF1AE11744BA}']
        procedure CheckPositionAndInit(aArmIsSafeEvent: TLinearMotorArmIsSafeEvent;
            aClearErrorEvent: TLinearMotorArmClearErrorEvent);
        procedure Reset(aResetID: TDevResetID);
        //
        // auch noch schlecht: Position in Steps statt mm
        procedure MoveToPos(aPosition: integer; aTime: integer; aWaitAtEnd: boolean);
        function GetCurrentPosition(): integer;
        function PositionAccepted(aTargetPosition, aCurrentPosition: integer): boolean;
        // wenn Position in Toleranzgrenzen ist, dann ist Result = true
    end;


implementation


end.
