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
  21.02.08 wl  DistanceToMin/Max            TN4009    neu
  04.06.08 pk  CalcSpeedByDistanceAndTime   TN4135    New
  03.07.08 wl                               TN4157
  16.07.08 pk  GetTipLiqDetError,etc.       TN4157   New
  12.09.09 wl                               TN4740   TipFloat-/TPosMM-/TipInteger-/MPOSArray durch TDoubleArray/TIntArray ersetzt
  12.09.09 wl                               TN4740   default-Werte aus interface entfernt
  19.10.09 pk  Init                         TN4820   Replaces InitMotors, and requires new InitID parameter
  19.05.10 wl  IMultiZMotorDevice           TN5115  abgeleitet von ICustomMotorDevice
  11.06.10 pk                               TN5138   ReadCurrentPositions
  25.03.11 wl  DisableInitCheck             TN5519   entfernt
  29.03.11 wl  MoveUpIfNecessary            TN5516   neu: aus MotionSystemTravel hierher
  13.01.12 ts  IModuleMotorExecute          TN5769   SingleExecuteWait (für ZP01 notwendig, sonst wird bei SingleExecute nicht gewartet)
  28.06.12 wl  GetCurrentPositions          TN5527   neu
  28.08.12 ts  MoveUpIfNecessary            TN5943   unterschiedliche Z-Geschwindigkeiten für Z-Bewegungen mit Tube
  16.01.13 wl                               TN6068   ArmGroupID statt GroupID
  25.02.13 wl  MoveToInitOffset             TN6068   entfernt
  06.06.13 wl  ExecuteDisabledErrorsAndCheck    TN6154   neu
  04.09.13 wl  MapAllToFirst                    TN6243   neu als property
  11.11.13 ts  MoveToInitOffset                 TN6300   reimplemented
  -------------------------------------------------------------------------------------------------- }

unit IntfMultiZMotorDevice;


interface


uses
    CommonTypes,
    AppTypes,
    GeneralTypes,
    IntfMotorDriver,
    Driver,
    IntfDevice,
    IntfZTravelManager,
    IntfMotorDevice;

type
    IMultiZMotorDevice = interface(ICustomMotorDevice)
        ['{1A3B6E04-9D8D-4D39-9E96-244C7BBC3E4F}']
        function GetName: string;
        function GetMaxMotorIndex(): integer;
        function GetZMotorsHaveBlockMoveOption: boolean;
        function GetZTravelManager: IZTravelManager;
        function GetArmGroupID(): integer;
        procedure SetArmGroupID(const Value: integer);
        procedure SetZTravelManager(const Value: IZTravelManager);

        procedure SingleExecute;
        procedure SingleExecuteWait;
        function AnyMotorsActive(): boolean;
        function GetMotor(index: Integer): IZMotorDevice;
        function MoveUpSingleZIfNeeded(aTipIndex: integer; aDest: TPosMM; aExec: EXEC_MODE;
            aZSpeed, aZRamp: integer): boolean;
        procedure MoveUpIfNecessary(aMotorMap: TIPMAP; aZTravelArr: TDoubleArray; aSingleRetract: boolean;
            aZSpeed, aZRamp: integer);
        procedure MoveUpIfNecessaryAllTips();

        procedure MoveSingleZ(aTipIndex: integer; aDest: TPosMM; aExec: EXEC_MODE); overload;
        procedure MoveSingleZ(aTipIndex: integer; aDest: TPosMM; aExec: EXEC_MODE;
            aMode: ACTION_TYPE); overload;
        procedure MoveSingleZ(aTipIndex: integer; aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE;
            aSpeed: integer); overload;
        procedure MoveSingleZ(aTipIndex: integer; aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE;
            aSpeed: integer; aRamp: integer); overload;

        procedure MoveZ(aMotorMap: TIPMAP; const aDest: TDoubleArray; aExec: EXEC_MODE); overload;
        procedure MoveZ(aMotorMap: TIPMAP; const aDest: TDoubleArray; aExec: EXEC_MODE;
            aMode: ACTION_TYPE); overload;
        procedure MoveZ(aMotorMap: TIPMAP; const aDest: TDoubleArray; aExec: EXEC_MODE; aMode: ACTION_TYPE;
            aSpeed: integer); overload;
        procedure MoveZ(aMotorMap: TIPMAP; const aDest: TDoubleArray; aExec: EXEC_MODE; aMode: ACTION_TYPE;
            aSpeed: integer; aRamp: integer); overload;
        procedure MoveZ(aMotorMap: TIPMAP; const aDest: TDoubleArray; aExec: EXEC_MODE; aMode: ACTION_TYPE;
            aSpeed: integer; aRamp: integer; aSingle: boolean); overload;
        procedure MoveZ_MultiSpeed(aMotorMap: TIPMAP; const aDest: TDoubleArray; aExec: EXEC_MODE;
            aMode: ACTION_TYPE; const aSpeed: TIntArray; aRamp: integer; aSingle: boolean);

        procedure MoveZ_WashRetract(aMotorMap: TIPMAP; const aDest: TDoubleArray; aExec: EXEC_MODE;
            aSpeed: integer);
        function GetScanSpeeds(aMotorMap: TIPMAP; aSpeed: integer): TIntArray;

        procedure BlockMoveZ(aMotorMap: TIPMAP; const aDest: TDoubleArray; aSpeed: integer);
        procedure MoveSingleZ_Scan(aMotor: integer; aDest: TPosMM; aExec: EXEC_MODE; aMode: integer;
            aSpeed: integer; aRamp: integer);
        procedure MoveSingleZ_Retract(aMotor: integer; aDest: TPosMM; aExec: EXEC_MODE; aSpeed: integer);
        function SetToMaxPosIfIsBeyondMax(aIndex: Integer; var vValue: TPosMM): boolean;
        function SetToMinPosIfIsBeyondMin(aIndex: Integer; var vValue: TPosMM): boolean;
        function ReadCurrentPos(aIndex: integer): TPosMM;
        function ReadCurrentPositions: TDoubleArray;
        function GetCurrentPositions: TDoubleArray;
        procedure MoveZAll(aMotorMap: TIPMAP; aDest: TPosMM; aSingleRetract: boolean);

        // ztravel
        function CalcZTravel(aMotorMap: TIPMAP): TDoubleArray;
        procedure MoveToZTravel(aMotorMap: TIPMAP); overload;
        procedure MoveToZTravel(aMotorMap: TIPMAP; aSingleRetract: boolean); overload;

        function DetectedMotors(var vMotorMap: TIPMAP; var vLiqPos: TDoubleArray): TIPMAP;

        procedure MoveToInitOffset;

        function GetOKTips_ZP02(aMotorMap: TIPMAP; const aMaxPos: TDoubleArray): TIPMAP;
        procedure DisableErrors(aMotorMap: TIPMAP);
        procedure ResetDisabledErrors(aMotorMap: TIPMAP);
        function GetZErrorMap(aMotorMap: TIPMAP): TIPMAP;
        function GetMotorMap: TIPMAP;
        function GetFirstMotor(): IZMotorDevice;
        function GetMapAllToFirst: boolean;

        function LocalToWorld(aIndex: integer; aUnits: TPosMM): TPosMM;
        function GetUnitsFromSteps(aIndex, aSteps: integer): TPosMM;
        function GetStepsFromUnits(aIndex: integer; aUnits: TPosMM): integer;
        function WorldToLocal(aIndex: integer; aUnits: TPosMM): TPosMM;

        function DistanceToMin(aIndex: integer; aWorldDest: TPosMM): TPosMM;
        function DistanceToMax(aIndex: integer; aWorldDest: TPosMM): TPosMM;

        procedure GetBounds(out oMaxBound: TPosMM);

        function GetNullDoubleArray: TDoubleArray;
        function GetNullIntArray: TIntArray;

        function CalcSpeedByDistanceAndTime(aIndex: integer; aDistance: TPosMM; aTimeInSec: double): integer;
        function TipNoLiqDetError(aIndex: integer): boolean;
        function GetTipLiqDetError(aMotorMap: TIPMAP): TTipLiqErrorTypeArray;
        procedure SetTipLiqDetError(aMotorMap: TIPMAP; aLiqError: TTipLiqErrorType);

        function ExecuteDisabledErrorsAndCheck(aDisabledMotors: TIPMAP): TIPMAP;

        property HaveBlockMoveOption: boolean read GetZMotorsHaveBlockMoveOption;
        property ZTravelManager: IZTravelManager read GetZTravelManager write SetZTravelManager;
        property MaxIndex: integer read GetMaxMotorIndex;
        property name: string read GetName;
        property FirstMotor: IZMotorDevice read GetFirstMotor;
        property ArmGroupID: integer read GetArmGroupID write SetArmGroupID;
        property this[index: integer]: IZMotorDevice read GetMotor; default;
        property MapAllToFirst: boolean read GetMapAllToFirst;
    end;


implementation


end.
