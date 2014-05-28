{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  05.09.07 pk                                         Initial Revision
  09.11.07 pk                               TN3924    Steps to mm
  29.01.08 wl                               TN3980   uses geändert
  19.05.08 wl                               TN4114   Methodennamen ohne "_Unit"
  03.06.08 pk  OnMultiYMoveAllowed          TN4133   New
  12.09.09 wl                               TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  12.09.09 wl                               TN4740   default-Werte aus interface entfernt
  19.10.09 pk  Init                         TN4820   Replaces InitMotors, and requires new InitID parameter
  17.12.09 pk  Execute                      TN4915   New
  19.05.10 wl  IMultiYMotorDevice           TN5115   abgeleitet von ICustomMotorDevice
  11.06.10 pk                               TN5138   ReadCurrentPositions
  22.03.11 wl  GetVMotor                    TN5401   neu
  25.03.11 wl  DisableInitCheck             TN5519   entfernt
  05.04.11 wl  LocalToWorldY,WorldToLocalY  TN5525   umbenannt
  06.04.11 wl  MoveControl...               TN5401   spezielle Funktionen, die nur für MoveControl verwendet werden
  19.05.11 ts  UseOwnExecute                TN5587   Y-Motor kann auch fahren, wenn andere ExecGroup
  28.06.12 wl  MoveControlYGetPosition      TN5527   neuer Parameter: aRead
  28.06.12 wl  MoveControlGetCurrentPositions   TN5527   ersetzt ReadCurrentPositions: Positionen werden nur noch aus dem Speicher gelesen
  28.09.12 ts  CalculateSteps                   TN5987   new: MoveVarispan, Möglichkeit Varispan-Bewegung abzuschalten (für XYMOV-Action)
  -------------------------------------------------------------------------------------------------- }

unit IntfMultiYMotorDevice;


interface


uses
    CommonTypes,
    GeneralTypes,
    AppTypes,
    IntfMotorDriver,
    Driver,
    IntfDevice,
    IntfMotorDevice;

type
    IMultiYMotorDevice = interface(ICustomMotorDevice)
        ['{5F06468A-7FFD-491B-A976-F792AA546785}']
        procedure MoveToInitOffset(aExec: EXEC_MODE; aMode: ACTION_TYPE);
        function AnyMotorsActive(): boolean;
        procedure VerifyStepBounds(aStep, aCurrentPos: TMultiYStep);
        procedure MoveY(aDest: TMultiYStep; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer;
            aRamp: integer);
        function GetStepFromRefMove(aDest: TPosMM): TMultiYStep;
        procedure MoveRelativeToRef(aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer;
            aRamp: integer);
        function RefReadCurrentPos(): TPosMM;
        function RefMinPos(): TPosMM;
        function RefMaxPos(): TPosMM;
        procedure GetTipRangeOf(aDest: TMultiYStep; out oRange1, oRange2: TPosMM);
        function GetRefPosOf(aDest: TMultiYStep): TPosMM;
        procedure GetCurrentTipRange(out oRange1, oRange2: TPosMM);
        procedure MinimizeTipRange(aRefYPos: TPosMM);
        function IsMoveRequired(aDest: TMultiYStep): boolean;
        procedure CalculateSteps(aMotorMap: TIPMAP; const aYArray: TDoubleArray;
            aResultSteps: TMultiYStepList; aMoveVarispan: boolean);
        function IsPosReachable(aMotorIndex: integer; aDest: TPosMM): boolean;
        function SetToMaxPosIfIsBeyondMax(aIndex: integer; var vValue: TPosMM): boolean;
        function SetToMinPosIfIsBeyondMin(aIndex: integer; var vValue: TPosMM): boolean;
        function ReadCurrentPos(aIndex: integer): TPosMM;
        function GetHomeStep(): TMultiYStep;
        procedure ShiftBy(aStep: TMultiYStep; aShiftBy: TPosMM);
        procedure GetBounds(out oMinBound, oMaxBound: TPosMM);
        function CreateSingleYStep(aYPos: TPosMM): TMultiYStep;
        function GetFirstMotor(): IYMotorDevice;
        function GetVMotor(): IVMotorDevice;
        function GetUnitsFromSteps(aIndex, aSteps: integer): TPosMM;
        function GetStepsFromUnits(aIndex: integer; aUnits: TPosMM): integer;
        function GetOnMultiYMoveAllowed(): TOnMoveAllowed;
        procedure SetOnMultiYMoveAllowed(aValue: TOnMoveAllowed);
        function GetUseOwnExecute: boolean;
        property FirstMotor: IYMotorDevice read GetFirstMotor;
        property VMotor: IVMotorDevice read GetVMotor;
        property OnMultiYMoveAllowed: TOnMoveAllowed read GetOnMultiYMoveAllowed write SetOnMultiYMoveAllowed;
        property UseOwnExecute: boolean read GetUseOwnExecute;
        // nur für MoveControl:
        function MoveControlYGetPosition(aRead: boolean; aIndex: integer): TPosMM;
        function MoveControlGetCurrentPositions: TDoubleArray;
        function MoveControlYLocalToWorld(aIndex: integer; aUnits: TPosMM): TPosMM;
        function MoveControlYWorldToLocal(aIndex: integer; aUnits: TPosMM): TPosMM;
        procedure MoveControlYMove(aIndex: integer; aDest: TPosMM);
        function MoveControlSetToMinOrMaxPosIfBeyond(aIndex: integer; var vValue: TPosMM): boolean;
    end;


implementation


end.
