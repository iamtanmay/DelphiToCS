{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                               track-no improvement/change
  -------- --  -----------------------------------  -------- -----------------------------------------------
  05.09.07 pk                                                Initial Revision
  09.11.07 pk                                        TN3924  Steps to mm
  29.01.08 wl                                        TN3980  uses geändert
  15.05.08 wl  TMultiYStep.GetYPosWithoutVarispan    TN4100  neu für Evaluate Carrier Position
  15.05.08 wl  TX-,TY-,TXYStepList.this              TN4100  Default property, relaces .Steps[]
  19.05.08 wl  TMultiYStep.ShiftBy                   TN4114  hier entfernt
  03.06.08 pk  TOnMoveAllowed                        TN4133  New
  03.07.08 wl                                        TN4157
  19.11.08 pk  IXMotorDevice                         TN4280  New LastUsedByID
  21.01.09 wl  IYMotorDevice                         TN4387  new: GetWorldMoveAwayPosition
  24.07.09 pk  IXMotorDevice                         TN4677  New: LastDestPos
  05.08.09 wl  IXMotorDevice                        TN4705   TrackID von IMotorDevice nach IXMotorDevice
  12.09.09 wl                                        TN4740   MPOS durch integer ersetzt
  12.09.09 wl                                        TN4740   default-Werte aus interface entfernt
  19.10.09 pk  IMotorDevice.Init                     TN4820  New InitID parameter
  04.11.09 pk                               	        TN4843  Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  17.12.09 pk  IMotorUsage                           TN4915  New for XMotor collision detection
  19.05.10 wl  IMotorDevice                          TN5115  abgeleitet von ICustomMotorDevice
  19.05.10 wl  ICustomMotorDevice                    TN5115  neue Property DefinedIncrement
  27.01.11 wl  GetExecIntf                           TN5357  Rückgabewert: IModuleMotorExecute
  09.03.11 wl  IMotorUsage                           TN5496   --> IMotorDriver
  10.03.11 wl  IXMotorDevice.ReadCurrentPosThreadSafe TN5496   Position lesen aus einem konkurrierenden Thread heraus
  28.06.12 wl  MoveDone,OnBeforeMove,OnAfterMove      TN5527   von IMotorDevice nach IXMotorDevice
  27.02.13 wl  ConflictHomeMinPos, ConflictHomeMaxPos TN6066   neu
  27.02.13 wl  SetConflictBufferMinus, -Plus          TN6066   neu
  27.02.13 wl  IXMotorDevice.CheckAndInit             TN6066   neu: Prüft vor dem Init, ob andere Arme im Weg stehen
  06.06.13 wl  ExecuteDisabledErrorAndCheck           TN6154   neu
  12.07.13 ts  IMotorDevice.IsMoveRequired            TN6201   Tolerance for position
  04.03.12 tp  SetZPos, EnableVSpanCorrection,  SetVSpanFactor, GetVariSpanCorrectionSpeed(),Move(aDest: TPosMM; aDirection: integer; aExec: EXEC_MODE; aSpeed: integer)
  GetVariSpanCorrectionSpeed()                       TN6374   Wegen Geschwindigkeit andergung von Varispan Korrigierung
  -------------------------------------------------------------------------------------------------- }

unit IntfMotorDevice;


interface


uses
    Generics.Collections,
    IntfDevice,
    CommonTypes,
    AppTypes,
    IntfModuleExecute,
    IntfMotorDriver,
    Driver;

type
    TOnMoveAllowed = procedure(out oAllowed: boolean) of object;

    TMotorUnit = (mu_MM, mu_Degree);

    // ICustomMotorDevice ist (single) MotorDevice oder MultiMotorDevice
    ICustomMotorDevice = interface(IDevice)
        ['{86D14E19-EE63-4303-9D0E-D203244673D7}']
        function GetDefinedIncrement: TPosMM;
        procedure SetDefinedIncrement(aValue: TPosMM);
        property DefinedIncrement: TPosMM read GetDefinedIncrement write SetDefinedIncrement;
        procedure Init(aInitID: TDevInitID);
        procedure Execute();
    end;

    IMotorDevice = interface(ICustomMotorDevice)
        ['{D86E54EB-145C-490C-97BD-2499541BB96F}']
        function GetMaxPos: TPosMM;
        function GetMinPos: TPosMM;
        function GetCurrentPos: TPosMM;
        function GetMotorUnit: TMotorUnit;
        function GetActive(): boolean;
        function GetDefaultRamp: integer;
        function GetDefaultSpeed: integer;

        function GetWorldOffset(): TPosMM;
        // public functions
        function GetUnitFromSteps(aValue: integer): TPosMM;
        function GetStepsFromUnit(aValue: TPosMM): integer;
        function ReadCurrentPos: TPosMM;
        function IsMoveRequired(aWorldDest: TPosMM; aTolerance: TPosMM = 0): boolean;
        procedure Move(aWorldDest: TPosMM); overload;
        procedure Move(aWorldDest: TPosMM; aExec: EXEC_MODE); overload;
        procedure Move(aWorldDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE); overload;
        procedure Move(aWorldDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer); overload;
        procedure Move(aWorldDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer;
            aRamp: integer); overload;
        function HasError: boolean;
        procedure DisableError;
        procedure ResetDisabledError();
        function ExecuteDisabledErrorAndCheck(): boolean;
        procedure SetNextWaiting(aDisable: boolean);
        function DistanceToMin(aWorldDest: TPosMM): TPosMM;
        function DistanceToMax(aWorldDest: TPosMM): TPosMM;
        function IsBeyondMin(aWorldDest: TPosMM): boolean;
        function IsBeyondMax(aWorldDest: TPosMM): boolean;
        function IsBeyondMinOrMax(aWorldDest: TPosMM): boolean;
        function SetToMinPosIfIsBeyondMin(var vValue: TPosMM): boolean;
        function SetToMaxPosIfIsBeyondMax(var vValue: TPosMM): boolean;
        function SetToMinOrMaxPosIfBeyondMinOrMax(var vValue: TPosMM): boolean;
        procedure DisableInitCheck(aDisable: boolean);
        procedure InitMotor(aInitID: TDevInitID; aExec: EXEC_MODE);
        function GetWorldInitOffset(): TPosMM;
        procedure MoveToInitOffset(aExec: EXEC_MODE; aMode: ACTION_TYPE);
        function IsStepMotor(): boolean;
        function IsInLB(): boolean;

        function LocalToWorld(aLocalPos: TPosMM): TPosMM;
        function WorldToLocal(aWorldPos: TPosMM): TPosMM;
        procedure ReduceMotorSpeeds(aSpeedFactor: integer);

        function GetExecIntf(): IModuleMotorExecute;
        function GetMotorID: integer;
        function GetDriver: IMotorDriver;

        // properties
        property MinPos: TPosMM read GetMinPos;
        property MaxPos: TPosMM read GetMaxPos;
        property CurrentPos: TPosMM read GetCurrentPos;
        property DefaultSpeed: integer read GetDefaultSpeed;
        property WorldOffset: TPosMM read GetWorldOffset;

        property Active: boolean read GetActive;
        property MotorID: integer read GetMotorID;
        property Driver: IMotorDriver read GetDriver;
    end;

    IGMotorDevice = interface(IMotorDevice)
        ['{193DC868-4956-457E-9270-42156DCB2843}']
        function PosWithHandTrk(aDest: TPosMM): TPosMM;
        procedure MoveErrDisabled_Unit(aDest: TPosMM; aExec: EXEC_MODE; aMode: ACTION_TYPE; aSpeed: integer);
        procedure SetZPos(aZPosSteps: integer);
        procedure EnableVSpanCorrection(aFlag: boolean);
        procedure SetVSpanFactor(aFactor: integer);
        function GetVariSpanCorrectionSpeed(): integer;
    end;

    TXMotorDirection = (xmhLeft, xmhRight);

    IXMotorDevice = interface(IMotorDevice)
        ['{7FFA031D-4583-488D-8DFB-280BC0DC7B1F}']
        function GetTrackID: integer;
        function GetConflictBufferMinus: TPosMM;
        function GetConflictBufferPlus: TPosMM;
        procedure SetConflictBufferMinus(aValue: TPosMM);
        procedure SetConflictBufferPlus(aValue: TPosMM);
        function GetConflictHomePos(aDirection: TXMotorDirection): TPosMM;
        procedure GetBounds(out oMinBound, oMaxBound: TPosMM);
        function GetMotorUsage: IMotorUsage;
        function ReadCurrentPosThreadSafe: TPosMM;
        procedure MoveDone();
        function GetOnBeforeMove: TMoveMotorNotifyEvent;
        procedure SetOnBeforeMove(aOnBeforeMove: TMoveMotorNotifyEvent);
        function GetOnAfterMove: TMoveMotorDoneNotifyEvent;
        procedure SetOnAfterMove(aOnAfterMove: TMoveMotorDoneNotifyEvent);
        procedure CheckAndInit(aInitID: TDevInitID);

        property TrackID: integer read GetTrackID;
        property ConflictBufferMinus: TPosMM read GetConflictBufferMinus write SetConflictBufferMinus;
        property ConflictBufferPlus: TPosMM read GetConflictBufferPlus write SetConflictBufferPlus;
        property MotorUsage: IMotorUsage read GetMotorUsage;
        property OnBeforeMove: TMoveMotorNotifyEvent read GetOnBeforeMove write SetOnBeforeMove;
        property OnAfterMove: TMoveMotorDoneNotifyEvent read GetOnAfterMove write SetOnAfterMove;
    end;

    IYMotorDevice = interface(IMotorDevice)
        ['{049D42E4-4553-4ED1-A017-F7E98069FB1C}']
        function TH_GetRealYPosition(aPipArmYPos: TPosMM; aYOffset: TPosMM): TPosMM;
        function GetTip1YPos(aOriginalYPos: TPosMM; aIsToolInHandler: boolean): TPosMM;
        function GetYOffset(): TPosMM;
        function GetWorldMoveAwayPosition(): TPosMM;
    end;

    IZMotorDevice = interface(IMotorDevice)
        ['{C071F1AF-3E54-4873-AED7-65B4B675AFE5}']
        function GetUseRealBlockMove(): boolean;
        procedure SetUseRealBlockMove(aValue: boolean);
        function GetMotorIndex(): integer;
        function GetLiquidDetection(): boolean;
        function GetRetractSpeed(): integer;
        function GetScanSpeed(): integer;
        function GetWashRetractSpeed(): integer;
        function DetectionDone(var vDetected: boolean; var vPos: TPosMM): boolean;
        procedure SetScanMode(aMode: integer);
        function IsOKTip_ZP02(aMaxPos: TPosMM; aIndex: integer): boolean;
        function GetLiqErrorType: TTipLiqErrorType;
        procedure SetLiqErrorType(const Value: TTipLiqErrorType);
        function VarispanCorrectionEnabled(): boolean;
        function VarispanFactor(): integer;
        property UseRealBlockMove: boolean read GetUseRealBlockMove write SetUseRealBlockMove;
        property MotorIndex: integer read GetMotorIndex;
        property LiquidDetection: boolean read GetLiquidDetection;
        property RetractSpeed: integer read GetRetractSpeed;
        property ScanSpeed: integer read GetScanSpeed;
        property WashRetractSpeed: integer read GetWashRetractSpeed;
        property LiqErrorType: TTipLiqErrorType read GetLiqErrorType write SetLiqErrorType;
    end;

    IRMotorDevice = interface(IMotorDevice)
        ['{37948932-15F0-469F-93EA-EE0B60E07B96}']
    end;

    IVMotorDevice = interface(IMotorDevice)
        ['{26BA1C34-F801-4778-9182-471646F24A7E}']
    end;

    TMultiYStep = class
    protected
        fMotorMap: TIPMAP;
    public
        constructor Create(aMotorMap: TIPMAP);

        function Clone(): TMultiYStep; virtual; abstract;
        function ToStr(): string; virtual; abstract;
        function GetYPosWithoutVarispan(aMotorIndex: integer): TPosMM; virtual; abstract;

        property MotorMap: TIPMAP read fMotorMap write fMotorMap;
    end;

    TMultiYStepList = class(TObjectList<TMultiYStep>);

    TXStep = class
    protected
        fX: TPosMM;
        fMotorMap: TIPMAP;
    public
        constructor Create(aX: TPosMM);
        function Clone(): TXStep;
        procedure AddToMotorMap(aMotorIndex: integer);
        property X: TPosMM read fX;
        property MotorMap: TIPMAP read fMotorMap write fMotorMap;
    end;

    TXStepList = class(TObjectList<TXStep>);

    TXYStep = class
    protected
        fX: TPosMM;
        fY: TMultiYStep;
        function GetMotorMap(): TIPMAP;
    public
        constructor Create(aX: TPosMM; aY: TMultiYStep);
        function Clone(): TXYStep;
        property X: TPosMM read fX write fX;
        property Y: TMultiYStep read fY;
        property MotorMap: TIPMAP read GetMotorMap;
    end;

    TXYStepList = class(TObjectList<TXYStep>);


implementation


uses
    TipMapUtils;

{ TMultiYStep }

constructor TMultiYStep.Create(aMotorMap: TIPMAP);
begin
    inherited Create;
    fMotorMap := aMotorMap;
end;

{ TXStep }

constructor TXStep.Create(aX: TPosMM);
begin
    inherited Create;
    fX := aX;
    fMotorMap := 0; { TODO -opk -cDEVICE : No access to postools }{ gmEmptyTipMap() };
end;

function TXStep.Clone(): TXStep;
begin
    result := TXStep.Create(fX);
    result.MotorMap := fMotorMap;
end;

procedure TXStep.AddToMotorMap(aMotorIndex: integer);
begin
    TTipMapUtils.SelectTip(fMotorMap, aMotorIndex);
end;

{ TXYStep }

constructor TXYStep.Create(aX: TPosMM; aY: TMultiYStep);
begin
    inherited Create;
    fX := aX;
    fY := aY;
end;

function TXYStep.Clone(): TXYStep;
begin
    result := TXYStep.Create(fX, fY.Clone);
end;

function TXYStep.GetMotorMap(): TIPMAP;
begin
    result := fY.MotorMap;
end;


end.
