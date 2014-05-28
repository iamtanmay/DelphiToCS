{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  class.method/member                    track-no improvement/change
  -------- --  -------------------------------------  -------- -------------------------------------
  16.07.08 wl                                         TN4164   von MoveCtrl hierher
  21.07.09 pk  ConvertStepDisplay                     TN4665   StepDisplay now also converted when mode changes
  21.07.09 pk  GetUnitsFromSteps,GetStepsFromUnits    TN4665   New: used in Move
  21.07.09 pk  Move                                   TN4665   Safety now done at > 5 mm instead of > 10 mm
  06.08.09 ts  Move, cSafetyRangeInMM                 TN4708   Safety now done at > 1 mm instead of > 5 mm
  12.09.09 wl                                         TN4740   MPOS durch integer ersetzt
  19.05.10 wl  ChangeDefinedIncrement                 TN5115   benutzt neue Property DefinedIncrement
  11.06.10 pk  GetCurrentPos                          TN5138   there is now a possibility to just get the last value from memory
  29.06.10 pk  EnableButtons                          TN5166   New
  10.12.10 pk  TMoveControl.Move                      TN5400   an unneeded calculation cause invalid floating point error
  22.03.11 wl  TVMotorMoveControl                     TN5401   neu
  05.04.11 wl  LocalToWorldY,WorldToLocalY            TN5525   umbenannt
  05.04.11 wl  fCombinedControl                       TN5401   Nach V-Motor-Move wird auch Y-Anzeige upgadated
  06.04.11 wl                                         TN5401   Änderungen bei Y- und V-Motor
  07.04.11 wl  TVMotorMoveControl.DoMove              TN5401   mit Execute
  07.04.11 wl  TVMotorMoveControl.GetCurrentPos       TN5401   der aktuelle Wert wird für Y mit V aus SumPosDisplay genommen
  21.02.12 wl  TMoveControl.RoundUnits                TN5777   ist jetzt public
  28.06.12 wl  fCurrentPos                            TN5527   Das redundante Vorhalten der CurrentPos führt nur zu Problemen - weg damit
  27.02.13 wl  TXMotorMoveControl.DoInit              TN6066   CheckAndInit: Prüft vor dem Init, ob andere Arme im Weg stehen
  21.08.13 wl                                         TN6231   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit MoveControl;


interface


uses
    Buttons,
    StdCtrls,
    Classes,
    IntfMultiYMotorDevice,
    IntfMultiZMotorDevice,
    IntfGripDevice,
    IntfMotorDevice,
    GeneralTypes,
    CommonTypes;

type
    TOnMoveControlSafetyMove = TNotifyEvent;

    TMoveControl = class
    strict private
    const
        cSafetyRangeInMM = 1; // waren früher 10 steps -> 1 mm
    strict private
        fStepDisplayIsUpdating: boolean;
        fStepDisplay: TEdit;
        fUnitsLabel: TLabel;
        fPlusBtn, fMinusBtn: TSpeedButton;
        fInitBtn: TButton;
        fOnSafetyMove: TOnMoveControlSafetyMove;
        procedure SetMoveMode(const aValue: integer);
        procedure SafetyMove();
        function GetCurrentPos(): TPosMM;
        procedure ChangeStepDisplay;
        procedure SetStepDisplay(aValue: TEdit);
        procedure PositionChanged(const aIsPosOK: boolean; const aCurrentPos: TPosMM);
    strict protected
    const
        INT_MOVEMODE_WORLDMETRIC = 0;
        INT_MOVEMODE_LOCALSTEPS = 1;
    strict protected
        fMoveMode: integer;
        fPosDisplay: TEdit;
        fSumPosDisplay: TEdit;

        procedure DoMove(aDestPos: TPosMM; aDirection: integer); virtual; abstract;
        function DoGetPosition(aRead: boolean): TPosMM; virtual; abstract;
        function DoGetSumPosition(aRead: boolean): TPosMM; virtual;
        procedure DoInit(); virtual; abstract;
        function Active(): boolean; virtual; abstract;
        function ClampToMinOrMax(var vDestPos: TPosMM): boolean; virtual; abstract;
        function LocalStepsToWorldUnits(aSteps: integer): TPosMM; virtual; abstract;
        function WorldUnitsToLocalSteps(aUnits: TPosMM): integer; virtual; abstract;
        function GetUnitsFromSteps(aSteps: integer): TPosMM; virtual; abstract;
        function GetStepsFromUnits(aUnits: TPosMM): integer; virtual; abstract;
        function GetDefinedIncrement: TPosMM; virtual; abstract;
        procedure SetDefinedIncrement(const Value: TPosMM); virtual; abstract;
        function DoGetCurrentPosDisplayText: string; virtual;
    public
        constructor Create();

        class function RoundUnits(aUnits: TPosMM): TPosMM;

        procedure Move(aDirection: integer; aSafety: boolean);
        procedure GetPosition();
        procedure ReadPosition();
        procedure Init();
        procedure SetTipIndex(aTipIndex: integer); virtual;
        procedure ChangeDefinedIncrement();
        procedure EnableButtons(const aEnable: boolean);

        property PosDisplay: TEdit read fPosDisplay write fPosDisplay;
        property SumPosDisplay: TEdit read fSumPosDisplay write fSumPosDisplay;
        property StepDisplay: TEdit read fStepDisplay write SetStepDisplay;
        property PlusBtn: TSpeedButton read fPlusBtn write fPlusBtn;
        property MinusBtn: TSpeedButton read fMinusBtn write fMinusBtn;
        property InitBtn: TButton read fInitBtn write fInitBtn;
        property UnitsLabel: TLabel read fUnitsLabel write fUnitsLabel;
        property OnSafetyMove: TOnMoveControlSafetyMove read fOnSafetyMove write fOnSafetyMove;
        property CurrentPos: TPosMM read GetCurrentPos;
        property DefinedIncrement: TPosMM read GetDefinedIncrement write SetDefinedIncrement;
        property MoveMode: integer read fMoveMode write SetMoveMode;
    end;

    TXMotorMoveControl = class(TMoveControl)
    strict private
        fMotor: IXMotorDevice;
    strict protected
        procedure DoMove(aDestPos: TPosMM; aDirection: integer); override;
        function DoGetPosition(aRead: boolean): TPosMM; override;
        procedure DoInit(); override;
        function Active(): boolean; override;
        function ClampToMinOrMax(var vDestPos: TPosMM): boolean; override;
        function GetUnitsFromSteps(aSteps: integer): TPosMM; override;
        function GetStepsFromUnits(aUnits: TPosMM): integer; override;
        function LocalStepsToWorldUnits(aSteps: integer): TPosMM; override;
        function WorldUnitsToLocalSteps(aUnits: TPosMM): integer; override;
        function GetDefinedIncrement: TPosMM; override;
        procedure SetDefinedIncrement(const Value: TPosMM); override;
    public
        constructor Create(aMotor: IXMotorDevice);
    end;

    TYSumMotorMoveControl = class(TMoveControl)
    strict protected
        fTipIndex: integer;
        fMotors: IMultiYMotorDevice;
        function DoGetSumPosition(aRead: boolean): TPosMM; override;
    public
        constructor Create(aMotors: IMultiYMotorDevice);
        procedure SetTipIndex(aTipIndex: integer); override;
    end;

    TYMotorMoveControl = class(TYSumMotorMoveControl)
    strict protected
        procedure DoMove(aDestPos: TPosMM; aDirection: integer); override;
        function DoGetPosition(aRead: boolean): TPosMM; override;
        procedure DoInit(); override;
        function Active(): boolean; override;
        function ClampToMinOrMax(var vDestPos: TPosMM): boolean; override;
        function GetUnitsFromSteps(aSteps: integer): TPosMM; override;
        function GetStepsFromUnits(aUnits: TPosMM): integer; override;
        function LocalStepsToWorldUnits(aSteps: integer): TPosMM; override;
        function WorldUnitsToLocalSteps(aUnits: TPosMM): integer; override;
        function GetDefinedIncrement: TPosMM; override;
        procedure SetDefinedIncrement(const Value: TPosMM); override;
        function DoGetCurrentPosDisplayText: string; override;
    end;

    TVMotorMoveControl = class(TYSumMotorMoveControl)
    strict protected
        procedure DoMove(aDestPos: TPosMM; aDirection: integer); override;
        function DoGetPosition(aRead: boolean): TPosMM; override;
        procedure DoInit(); override;
        function Active(): boolean; override;
        function ClampToMinOrMax(var vDestPos: TPosMM): boolean; override;
        function GetUnitsFromSteps(aSteps: integer): TPosMM; override;
        function GetStepsFromUnits(aUnits: TPosMM): integer; override;
        function LocalStepsToWorldUnits(aSteps: integer): TPosMM; override;
        function WorldUnitsToLocalSteps(aUnits: TPosMM): integer; override;
        function GetDefinedIncrement: TPosMM; override;
        procedure SetDefinedIncrement(const Value: TPosMM); override;
    end;

    TZMotorMoveControl = class(TMoveControl)
    strict private
        fTipIndex: integer;
        fMotors: IMultiZMotorDevice;
    strict protected
        procedure DoMove(aDestPos: TPosMM; aDirection: integer); override;
        function DoGetPosition(aRead: boolean): TPosMM; override;
        procedure DoInit(); override;
        function Active(): boolean; override;
        function ClampToMinOrMax(var vDestPos: TPosMM): boolean; override;
        function GetUnitsFromSteps(aSteps: integer): TPosMM; override;
        function GetStepsFromUnits(aUnits: TPosMM): integer; override;
        function LocalStepsToWorldUnits(aSteps: integer): TPosMM; override;
        function WorldUnitsToLocalSteps(aUnits: TPosMM): integer; override;
        function GetDefinedIncrement: TPosMM; override;
        procedure SetDefinedIncrement(const Value: TPosMM); override;
    public
        constructor Create(aMotors: IMultiZMotorDevice);
        procedure SetTipIndex(aTipIndex: integer); override;
    end;

    TRMotorMoveControl = class(TMoveControl)
    strict private
        fMotor: IRMotorDevice;
    strict protected
        procedure DoMove(aDestPos: TPosMM; aDirection: integer); override;
        function DoGetPosition(aRead: boolean): TPosMM; override;
        procedure DoInit(); override;
        function Active(): boolean; override;
        function ClampToMinOrMax(var vDestPos: TPosMM): boolean; override;
        function GetUnitsFromSteps(aSteps: integer): TPosMM; override;
        function GetStepsFromUnits(aUnits: TPosMM): integer; override;
        function LocalStepsToWorldUnits(aSteps: integer): TPosMM; override;
        function WorldUnitsToLocalSteps(aUnits: TPosMM): integer; override;
        function GetDefinedIncrement: TPosMM; override;
        procedure SetDefinedIncrement(const Value: TPosMM); override;
    public
        constructor Create(aMotor: IRMotorDevice);
    end;

    TGripMoveControl = class(TMoveControl)
    strict private
        fGripper: IGripDevice;
    strict protected
        procedure DoMove(aDestPos: TPosMM; aDirection: integer); override;
        function DoGetPosition(aRead: boolean): TPosMM; override;
        procedure DoInit(); override;
        function Active(): boolean; override;
        function ClampToMinOrMax(var vDestPos: TPosMM): boolean; override;
        function GetUnitsFromSteps(aSteps: integer): TPosMM; override;
        function GetStepsFromUnits(aUnits: TPosMM): integer; override;
        function LocalStepsToWorldUnits(aSteps: integer): TPosMM; override;
        function WorldUnitsToLocalSteps(aUnits: TPosMM): integer; override;
        function GetDefinedIncrement: TPosMM; override;
        procedure SetDefinedIncrement(const Value: TPosMM); override;
    public
        constructor Create(aGripper: IGripDevice);
    end;


implementation


uses
    Math,
    SysUtils,
    ErrorManager,
    AppTypes,
    DeviceInitHandling;

{ TMoveControl }

constructor TMoveControl.Create;
begin
    inherited;
    fStepDisplayIsUpdating := false;
end;

function TMoveControl.DoGetCurrentPosDisplayText: string;
begin
    result := fPosDisplay.Text
end;

function TMoveControl.DoGetSumPosition(aRead: boolean): TPosMM;
begin
    result := 0;
end;

procedure TMoveControl.EnableButtons(const aEnable: boolean);
begin
    self.fPlusBtn.Enabled := aEnable;
    self.fMinusBtn.Enabled := aEnable;
    self.fInitBtn.Enabled := aEnable;
end;

function TMoveControl.GetCurrentPos: TPosMM;
begin
    result := -9999;
    if not TryStrToFloat(self.DoGetCurrentPosDisplayText(), result) then
        EXIT;
    if fMoveMode = INT_MOVEMODE_LOCALSTEPS then
    begin
        result := RoundUnits(self.LocalStepsToWorldUnits(Round(result)));
    end;
end;

class function TMoveControl.RoundUnits(aUnits: TPosMM): TPosMM;
begin
    result := RoundTo(aUnits, -3);
end;

procedure TMoveControl.PositionChanged(const aIsPosOK: boolean; const aCurrentPos: TPosMM);
var
    xCurrentPos: TPosMM;
begin
    if not aIsPosOK then
    begin

        fPosDisplay.Text := '***';
        if Assigned(fSumPosDisplay) then
            fSumPosDisplay.Text := '***';

        EXIT;
    end;

    // all motors commented out
    // if not Assigned( fCurrentArm ) then EXIT;
    xCurrentPos := aCurrentPos;
    fUnitsLabel.Caption := 'mm';
    if fMoveMode = INT_MOVEMODE_LOCALSTEPS then
    begin
        xCurrentPos := self.WorldUnitsToLocalSteps(xCurrentPos);
        fUnitsLabel.Caption := 'steps';
    end;

    fPosDisplay.Text := FloatToStr(RoundUnits(xCurrentPos)); // get motor position

    if Assigned(fSumPosDisplay) then
        fSumPosDisplay.Text := FloatToStr(RoundUnits(DoGetSumPosition(false))); // get sum of Y and V position
end;

procedure TMoveControl.ReadPosition();
var
    xCurrentPos: TPosMM;
    xIsPosOK: boolean;
begin
    xIsPosOK := false;
    xCurrentPos := 0;
    try
        if (gErrorManager.IsGlobalErr) then
            EXIT;
        if not Active() then
            EXIT;
        xCurrentPos := self.DoGetPosition(true);
        xIsPosOK := true;
    finally
        PositionChanged(xIsPosOK, xCurrentPos);
    end;
end;

procedure TMoveControl.GetPosition();
begin
    PositionChanged(true, self.DoGetPosition(false));
end;

procedure TMoveControl.Init();
begin
    SafetyMove();
    DoInit();
    ReadPosition();
end;

procedure TMoveControl.Move(aDirection: integer; aSafety: boolean);
var
    xDestPos, xCurrentPos, xDisplayedPos: TPosMM;
begin
    if (gErrorManager.IsGlobalErr) then
        EXIT;
    if not Active() then
        EXIT;

    if (aDirection = 0) then
    begin
        // wenn Direction=0, wird die PosDisplay-Position angefahren
        if not TryStrToFloat(fPosDisplay.Text, xDestPos) then
            EXIT;
        if fMoveMode = INT_MOVEMODE_LOCALSTEPS then
        begin
            xDestPos := self.LocalStepsToWorldUnits(Round(xDestPos));
        end;
    end
    else
    begin
        if not TryStrToFloat(fPosDisplay.Text, xDisplayedPos) then
            EXIT;
        ReadPosition();
        if not TryStrToFloat(fPosDisplay.Text, xCurrentPos) then
            EXIT;

        if fMoveMode = INT_MOVEMODE_LOCALSTEPS then
        begin
            // Wenn sich die alte und die neu ermittelte Position kaum unterscheiden:
            // Mit der vorher angezeigten Position rechnen, sonst ist der User irritiert
            if (Abs(xDisplayedPos - xCurrentPos) < 5) then
                xCurrentPos := xDisplayedPos;

            xCurrentPos := self.LocalStepsToWorldUnits(Round(xCurrentPos));
        end;

        xDestPos := xCurrentPos + (aDirection * self.DefinedIncrement);
    end;

    self.ClampToMinOrMax(xDestPos);

    // check if safe range for XY-movement is exceeded
    if (aSafety) and (Abs(xDestPos - xCurrentPos) > cSafetyRangeInMM) then
        SafetyMove();

    DoMove(xDestPos, aDirection);

    ReadPosition();
end;

procedure TMoveControl.SafetyMove();
begin
    if Assigned(fOnSafetyMove) then
        fOnSafetyMove(self);
end;

procedure TMoveControl.SetTipIndex(aTipIndex: integer);
begin
    // dummy
end;

procedure TMoveControl.ChangeDefinedIncrement;
var
    xMoveValue: TPosMM;
begin
    if fStepDisplayIsUpdating then
        EXIT;

    if TryStrToFloat(fStepDisplay.Text, xMoveValue) then
    begin

        if fMoveMode = INT_MOVEMODE_LOCALSTEPS then
        begin
            self.DefinedIncrement := self.GetUnitsFromSteps(Round(xMoveValue));
        end
        else
        begin
            self.DefinedIncrement := xMoveValue;
        end;
    end;
end;

procedure TMoveControl.ChangeStepDisplay();
begin
    fStepDisplayIsUpdating := true;
    if fMoveMode = INT_MOVEMODE_LOCALSTEPS then
    begin
        self.fStepDisplay.Text := IntToStr(self.GetStepsFromUnits(self.DefinedIncrement));
    end
    else
    begin
        // Wert soll nur 2 Nachkommastellen haben
        self.fStepDisplay.Text := FloatToStr(Round(self.DefinedIncrement * 100) / 100);
    end;
    fStepDisplayIsUpdating := false;
end;

procedure TMoveControl.SetMoveMode(const aValue: integer);

begin
    if fMoveMode = aValue then
        EXIT;

    fMoveMode := aValue;
    ChangeStepDisplay();
end;

procedure TMoveControl.SetStepDisplay(aValue: TEdit);
begin
    fStepDisplay := aValue;
    self.ChangeStepDisplay;
end;

{ TXMotorMoveControl }

function TXMotorMoveControl.Active: boolean;
begin
    result := Assigned(fMotor)
end;

function TXMotorMoveControl.ClampToMinOrMax(var vDestPos: TPosMM): boolean;
var
    xDestPos: TPosMM;
begin
    xDestPos := vDestPos;
    result := fMotor.SetToMinOrMaxPosIfBeyondMinOrMax(xDestPos);
    if result then
        vDestPos := xDestPos;
end;

constructor TXMotorMoveControl.Create(aMotor: IXMotorDevice);
begin
    inherited Create();
    fMotor := aMotor;
end;

procedure TXMotorMoveControl.DoMove(aDestPos: TPosMM; aDirection: integer);
begin
    fMotor.Move(aDestPos, m_Execute);
end;

function TXMotorMoveControl.DoGetPosition(aRead: boolean): TPosMM;
begin
    if (aRead) then
        EXIT(fMotor.ReadCurrentPos)
    else
        EXIT(fMotor.GetCurrentPos);
end;

procedure TXMotorMoveControl.DoInit;
begin
    fMotor.CheckAndInit(TDeviceInitHandling.GenerateInitID());
end;

function TXMotorMoveControl.GetDefinedIncrement: TPosMM;
begin
    result := fMotor.DefinedIncrement;
end;

function TXMotorMoveControl.GetStepsFromUnits(aUnits: TPosMM): integer;
begin
    result := fMotor.GetStepsFromUnit(aUnits);
end;

function TXMotorMoveControl.GetUnitsFromSteps(aSteps: integer): TPosMM;
begin
    result := fMotor.GetUnitFromSteps(aSteps);
end;

function TXMotorMoveControl.LocalStepsToWorldUnits(aSteps: integer): TPosMM;
begin
    result := fMotor.LocalToWorld(GetUnitsFromSteps(aSteps));
end;

procedure TXMotorMoveControl.SetDefinedIncrement(const Value: TPosMM);
begin
    fMotor.DefinedIncrement := Value;
end;

function TXMotorMoveControl.WorldUnitsToLocalSteps(aUnits: TPosMM): integer;
begin
    result := GetStepsFromUnits(fMotor.WorldToLocal(aUnits));
end;

{ TRMotorMoveControl }

constructor TRMotorMoveControl.Create(aMotor: IRMotorDevice);
begin
    inherited Create();
    fMotor := aMotor;
end;

function TRMotorMoveControl.Active: boolean;
begin
    result := Assigned(fMotor)
end;

procedure TRMotorMoveControl.DoMove(aDestPos: TPosMM; aDirection: integer);
begin
    fMotor.Move(aDestPos, m_Execute);
end;

function TRMotorMoveControl.ClampToMinOrMax(var vDestPos: TPosMM): boolean;
var
    xDestPos: TPosMM;
begin
    xDestPos := vDestPos;
    result := fMotor.SetToMinOrMaxPosIfBeyondMinOrMax(xDestPos);
    if result then
        vDestPos := xDestPos;
end;

function TRMotorMoveControl.DoGetPosition(aRead: boolean): TPosMM;
begin
    if (aRead) then
        EXIT(fMotor.ReadCurrentPos)
    else
        EXIT(fMotor.GetCurrentPos);
end;

procedure TRMotorMoveControl.DoInit;
begin
    fMotor.Init(TDeviceInitHandling.GenerateInitID());
end;

function TRMotorMoveControl.GetDefinedIncrement: TPosMM;
begin
    result := fMotor.DefinedIncrement;
end;

function TRMotorMoveControl.GetStepsFromUnits(aUnits: TPosMM): integer;
begin
    result := fMotor.GetStepsFromUnit(aUnits);
end;

function TRMotorMoveControl.GetUnitsFromSteps(aSteps: integer): TPosMM;
begin
    result := fMotor.GetUnitFromSteps(aSteps);
end;

function TRMotorMoveControl.LocalStepsToWorldUnits(aSteps: integer): TPosMM;
begin
    result := fMotor.LocalToWorld(GetUnitsFromSteps(aSteps));
end;

procedure TRMotorMoveControl.SetDefinedIncrement(const Value: TPosMM);
begin
    fMotor.DefinedIncrement := Value;
end;

function TRMotorMoveControl.WorldUnitsToLocalSteps(aUnits: TPosMM): integer;
begin
    result := GetStepsFromUnits(fMotor.WorldToLocal(aUnits));
end;

{ TVYMotorMoveControl }

constructor TYSumMotorMoveControl.Create(aMotors: IMultiYMotorDevice);
begin
    inherited Create();
    fMotors := aMotors;
end;

function TYSumMotorMoveControl.DoGetSumPosition(aRead: boolean): TPosMM;
var
    xYSumPositions: TDoubleArray;
begin
    xYSumPositions := fMotors.MoveControlGetCurrentPositions;
    result := xYSumPositions[fTipIndex];
end;

procedure TYSumMotorMoveControl.SetTipIndex(aTipIndex: integer);
begin
    fTipIndex := aTipIndex;
end;

{ TYMotorMoveControl }

function TYMotorMoveControl.Active: boolean;
begin
    result := fMotors.AnyMotorsActive;
end;

function TYMotorMoveControl.ClampToMinOrMax(var vDestPos: TPosMM): boolean;
var
    xDestPos: TPosMM;
begin
    xDestPos := vDestPos;
    result := fMotors.MoveControlSetToMinOrMaxPosIfBeyond(fTipIndex, xDestPos);
    if result then
        vDestPos := xDestPos;
end;

procedure TYMotorMoveControl.DoMove(aDestPos: TPosMM; aDirection: integer);
begin
    fMotors.MoveControlYMove(fTipIndex, aDestPos);
end;

function TYMotorMoveControl.DoGetCurrentPosDisplayText: string;
begin
    if Assigned(fMotors.VMotor) then
        result := fSumPosDisplay.Text
    else
        result := fPosDisplay.Text;
end;

function TYMotorMoveControl.DoGetPosition(aRead: boolean): TPosMM;
begin
    EXIT(fMotors.MoveControlYGetPosition(aRead, fTipIndex));
end;

procedure TYMotorMoveControl.DoInit;
begin
    fMotors.Init(TDeviceInitHandling.GenerateInitID());
end;

procedure TYMotorMoveControl.SetDefinedIncrement(const Value: TPosMM);
begin
    fMotors.DefinedIncrement := Value;
end;

function TYMotorMoveControl.GetDefinedIncrement: TPosMM;
begin
    result := fMotors.DefinedIncrement;
end;

function TYMotorMoveControl.GetStepsFromUnits(aUnits: TPosMM): integer;
begin
    result := fMotors.GetStepsFromUnits(fTipIndex, aUnits);
end;

function TYMotorMoveControl.GetUnitsFromSteps(aSteps: integer): TPosMM;
begin
    result := fMotors.GetUnitsFromSteps(fTipIndex, aSteps);
end;

function TYMotorMoveControl.LocalStepsToWorldUnits(aSteps: integer): TPosMM;
begin
    result := fMotors.MoveControlYLocalToWorld(fTipIndex, GetUnitsFromSteps(aSteps));
end;

function TYMotorMoveControl.WorldUnitsToLocalSteps(aUnits: TPosMM): integer;
begin
    result := GetStepsFromUnits(fMotors.MoveControlYWorldToLocal(fTipIndex, aUnits));
end;

{ TVMotorMoveControl }

function TVMotorMoveControl.Active: boolean;
begin
    result := Assigned(fMotors.VMotor)
end;

function TVMotorMoveControl.ClampToMinOrMax(var vDestPos: TPosMM): boolean;
var
    xDestPos: TPosMM;
begin
    xDestPos := vDestPos;
    result := fMotors.VMotor.SetToMinOrMaxPosIfBeyondMinOrMax(xDestPos);
    if result then
        vDestPos := xDestPos;
end;

procedure TVMotorMoveControl.DoMove(aDestPos: TPosMM; aDirection: integer);
begin
    fMotors.VMotor.Move(aDestPos, m_Execute);
end;

function TVMotorMoveControl.DoGetPosition(aRead: boolean): TPosMM;
begin
    if (aRead) then
        EXIT(fMotors.VMotor.ReadCurrentPos)
    else
        EXIT(fMotors.VMotor.GetCurrentPos);
end;

procedure TVMotorMoveControl.DoInit;
begin
    fMotors.VMotor.Init(TDeviceInitHandling.GenerateInitID());
end;

function TVMotorMoveControl.GetDefinedIncrement: TPosMM;
begin
    result := fMotors.VMotor.DefinedIncrement;
end;

function TVMotorMoveControl.GetStepsFromUnits(aUnits: TPosMM): integer;
begin
    result := fMotors.VMotor.GetStepsFromUnit(aUnits);
end;

function TVMotorMoveControl.GetUnitsFromSteps(aSteps: integer): TPosMM;
begin
    result := fMotors.VMotor.GetUnitFromSteps(aSteps);
end;

function TVMotorMoveControl.LocalStepsToWorldUnits(aSteps: integer): TPosMM;
begin
    result := fMotors.VMotor.LocalToWorld(GetUnitsFromSteps(aSteps));
end;

procedure TVMotorMoveControl.SetDefinedIncrement(const Value: TPosMM);
begin
    fMotors.VMotor.DefinedIncrement := Value;
end;

function TVMotorMoveControl.WorldUnitsToLocalSteps(aUnits: TPosMM): integer;
begin
    result := GetStepsFromUnits(fMotors.VMotor.WorldToLocal(aUnits));
end;

{ TZMotorMoveControl }

function TZMotorMoveControl.Active: boolean;
begin
    result := fMotors.AnyMotorsActive;
end;

function TZMotorMoveControl.ClampToMinOrMax(var vDestPos: TPosMM): boolean;
var
    xDestPos: TPosMM;
begin
    xDestPos := vDestPos;
    result := fMotors.SetToMinPosIfIsBeyondMin(fTipIndex, xDestPos);
    if not result then
        result := fMotors.SetToMaxPosIfIsBeyondMax(fTipIndex, xDestPos);

    if result then
        vDestPos := xDestPos;
end;

constructor TZMotorMoveControl.Create(aMotors: IMultiZMotorDevice);
begin
    inherited Create;
    fMotors := aMotors;
end;

procedure TZMotorMoveControl.DoMove(aDestPos: TPosMM; aDirection: integer);
begin
    fMotors.MoveSingleZ(fTipIndex, aDestPos, m_Execute);
end;

function TZMotorMoveControl.DoGetPosition(aRead: boolean): TPosMM;
var
    xCurrentPositions: TDoubleArray;
begin
    if (aRead) then
        xCurrentPositions := fMotors.ReadCurrentPositions()
    else
        xCurrentPositions := fMotors.GetCurrentPositions();

    result := xCurrentPositions[fTipIndex];
end;

procedure TZMotorMoveControl.DoInit;
begin
    fMotors.Init(TDeviceInitHandling.GenerateInitID());
end;

procedure TZMotorMoveControl.SetDefinedIncrement(const Value: TPosMM);
begin
    fMotors.DefinedIncrement := Value;
end;

procedure TZMotorMoveControl.SetTipIndex(aTipIndex: integer);
begin
    fTipIndex := aTipIndex;
end;

function TZMotorMoveControl.GetDefinedIncrement: TPosMM;
begin
    result := fMotors.DefinedIncrement;
end;

function TZMotorMoveControl.GetStepsFromUnits(aUnits: TPosMM): integer;
begin
    result := fMotors.GetStepsFromUnits(fTipIndex, aUnits);
end;

function TZMotorMoveControl.GetUnitsFromSteps(aSteps: integer): TPosMM;
begin
    result := fMotors.GetUnitsFromSteps(fTipIndex, aSteps);
end;

function TZMotorMoveControl.LocalStepsToWorldUnits(aSteps: integer): TPosMM;
begin
    result := fMotors.LocalToWorld(fTipIndex, GetUnitsFromSteps(aSteps));
end;

function TZMotorMoveControl.WorldUnitsToLocalSteps(aUnits: TPosMM): integer;
begin
    result := GetStepsFromUnits(fMotors.WorldToLocal(fTipIndex, aUnits));
end;

{ TGripControl }

constructor TGripMoveControl.Create(aGripper: IGripDevice);
begin
    inherited Create();
    fGripper := aGripper;
end;

function TGripMoveControl.Active: boolean;
begin
    result := Assigned(fGripper);
end;

function TGripMoveControl.ClampToMinOrMax(var vDestPos: TPosMM): boolean;
var
    xDestPos: TPosMM;
begin
    xDestPos := vDestPos;
    result := fGripper.SetToMinPosIfIsBeyondMin(xDestPos);
    if not result then
        result := fGripper.SetToMaxPosIfIsBeyondMax(xDestPos);

    if result then
        vDestPos := xDestPos;
end;

procedure TGripMoveControl.DoMove(aDestPos: TPosMM; aDirection: integer);
begin
    fGripper.Move(aDestPos, aDirection);
end;

function TGripMoveControl.DoGetPosition(aRead: boolean): TPosMM;
begin
    // noch wird immer gelesen

    result := fGripper.GetPosition(aRead);
end;

function TGripMoveControl.GetDefinedIncrement: TPosMM;
begin
    result := fGripper.DefinedIncrement;
end;

function TGripMoveControl.GetStepsFromUnits(aUnits: TPosMM): integer;
begin
    result := fGripper.GetStepsFromUnits(aUnits);
end;

function TGripMoveControl.GetUnitsFromSteps(aSteps: integer): TPosMM;
begin
    result := fGripper.GetUnitsFromSteps(aSteps);
end;

function TGripMoveControl.LocalStepsToWorldUnits(aSteps: integer): TPosMM;
begin
    result := fGripper.LocalToWorld(GetUnitsFromSteps(aSteps));
end;

procedure TGripMoveControl.SetDefinedIncrement(const Value: TPosMM);
begin
    fGripper.DefinedIncrement := Value;
end;

function TGripMoveControl.WorldUnitsToLocalSteps(aUnits: TPosMM): integer;
begin
    result := GetStepsFromUnits(fGripper.WorldToLocal(aUnits));
end;

procedure TGripMoveControl.DoInit;
begin
    fGripper.Init(TDeviceInitHandling.GenerateInitID());
end;


end.
