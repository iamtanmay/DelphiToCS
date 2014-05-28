{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk), Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --D------ ----------------------------------------------------------
  08.09.08 pk                                        TN4215     Code From Action.pas
  20.09.08 pk  GetMustPutBackTool                    TN4215
  23.09.08 wl  DilIndex                              TN4238    Diluent in DilIndex umbenannt, da Index immer 0-basiert
  09.08.12 wl  ExecFirst                             TN5946   DiluentName wird erst hier DilIndex gewandelt
  10.08.12 wl                                        TN5947   BitOptions durch 3 neue Parameter ersetzt
  24.04.13 wl  ExecFirst                             TN6137   verwendet TLiquids.FindDiluentIndex
  21.08.13 wl                                        TN6231   uses geändert
  03.09.13 wl  DryAfterWash                          TN6238   neuer Parameter
  25.09.13 wl  ExecuteFlush                          TN6258   an geänderte ChangeLiquid-Funktion angepasst
  30.09.13 wl  ExecuteFlush                          TN6260   Cycles werden nur noch an wash-Methode weitergegeben
  ----------------------------------------------------------------------------------------------------------------------- }

unit FlushRunAction;


interface


uses
    RunStep,
    RunAction,
    LiquidHandling,
    RunActionTypeInfo,
    ThreadClasses,
    IntfArmDevice,
    FlushRunStep;

type
    TFlushRunAction = class(TRunAction)
    private
        function GetRunStep: TFlushRunStep;
        function GetDryAfterWashSetting: TDryAfterWashType;
        class procedure ExecuteFlush(aUsedArm: IArmDevice; aTipMap: integer; aDilIndex: integer;
            aCycles: integer; aVolume: extended; aPumpIndex: integer; aUseAllPumps, aUsePeriPump: boolean;
            aDryAfterWash: TDryAfterWashType);
    protected
        function GetMustPutBackTool: boolean; override;
    public
        procedure ExecFirst(); override;
        property RunStep: TFlushRunStep read GetRunStep;
    end;

    TFlushRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TFlushRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    SysUtils,
    ErrorManager,
    WashHandling,
    PipDeviceManager,
    ObjModul,
    LiquidManager,
    TipSystem,
    LiquidHandlingLow,
    SamGlobe,
    MethodTypes,
    CommonTypes,
    Liquids,
    IntfPeriPumpDevice,
    AppTypes,
    IntfPipDevice;

{ TFlushRunActionCreator }

function TFlushRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TFlushRunStep;
end;

function TFlushRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TFlushRunAction.Create(aRunStep);
end;

{ TFlushRunAction }

function TFlushRunAction.GetDryAfterWashSetting: TDryAfterWashType;
begin
    case self.RunStep.DryAfterWash of
        tTrue:
            EXIT(dawDoDryAfterWash);
        tFalse:
            EXIT(dawDoNotDryAfterWash);
        else
            EXIT(dawUseTipParameter);
    end;
end;

function TFlushRunAction.GetMustPutBackTool: boolean;
begin
    result := true;
end;

function TFlushRunAction.GetRunStep: TFlushRunStep;
begin
    result := fRunStep as TFlushRunStep;
end;

class procedure TFlushRunAction.ExecuteFlush(aUsedArm: IArmDevice; aTipMap: integer; aDilIndex: integer;
    aCycles: integer; aVolume: extended; aPumpIndex: integer; aUseAllPumps, aUsePeriPump: boolean;
    aDryAfterWash: TDryAfterWashType);
var
    x: integer;
    xTipUsageArray: TTipUsageRecArray;
    xFlushVolCh1, xFlushVolCh2: TArray<double>;
    xPipDevice: IPipDevice;
    xPeriPump: IPeriPumpDevice;
begin
    xPeriPump := gModules.FindPeripump();
    if not Assigned(xPeriPump) then
        aUsePeriPump := false;

    xPipDevice := aUsedArm.PipDevice;

    if not Assigned(xPeriPump) then
    begin // bei PeriPump kann man sich den Wechsel sparen
        gSysLiqManager.ChangeLiquid(xPipDevice, aTipMap, aDilIndex);
    end;

    // Flush-Volumen definieren
    xFlushVolCh1 := xPipDevice.GetNullDoubleArray;
    xFlushVolCh2 := xPipDevice.GetNullDoubleArray;
    for x := 0 to xPipDevice.TipCount - 1 do
    begin
        if (((aTipMap shr x) and 1) = 0) then
            CONTINUE;

        if Assigned(xPipDevice.Tips[x].GetPipPump(aPumpIndex)) then
        begin
            if (aVolume = 0) then
                xFlushVolCh1[x] := xPipDevice.Tips[x].GetPipPump(aPumpIndex).MaxVolume_ul
            else
                xFlushVolCh1[x] := aVolume;
        end;
        if (aUseAllPumps) and (aPumpIndex <> 1) and Assigned(xPipDevice.Tips[x].GetPipPump(1)) then
        begin
            if (aVolume = 0) then
                xFlushVolCh2[x] := xPipDevice.Tips[x].GetPipPump(1).MaxVolume_ul
            else
                xFlushVolCh2[x] := aVolume;
        end;
    end;

    if (gFlushWithRediTips) then
        xTipUsageArray := xPipDevice.GetTipUsageForTipMap(aTipMap, [DefaultTip, DispTip, RediTip, VarRediTip])
    else
        xTipUsageArray := xPipDevice.GetTipUsageForTipMap(aTipMap, [DefaultTip, DispTip]);

    if (Length(xTipUsageArray) > 0) then
    begin
        for x := 0 to Length(xTipUsageArray) - 1 do
        begin
            xPipDevice.SetUseTips(xTipUsageArray[x].TipTypeName, xTipUsageArray[x].TipMap, false);
            TWashHandling.Wash(aUsedArm, xFlushVolCh1, xFlushVolCh2, aCycles, aUsePeriPump, 0, '',
                aDryAfterWash, false, TLiquidChannels.GetPumpIndexArray(aUsedArm.PipDevice, aPumpIndex));
        end;
    end;
end;

procedure TFlushRunAction.ExecFirst();
var
    xPipDevice: IPipDevice;
    xUsedArm: IArmDevice;
    xDilIndex: integer;
    xTipMap: integer;
    xCycles: integer;
begin
    if (gErrorManager.IsGlobalErr) then
        Exit;
    // 22.04.08 pk This prevented multiple tiptypes to be flushed at once.  // xPipDevice := gPipDeviceManager.FindArmToUse( '', fTipMap, false );
    xPipDevice := gPipDeviceManager.FindPipDevice_ByName(self.RunStep.PipDeviceName);
    if not(Assigned(xPipDevice)) then
        raise Exception.CreateFmt('Pipdevice [%s] not found', [self.RunStep.PipDeviceName]);
    xUsedArm := gModules.FindArmByPipDevice(xPipDevice);

    xDilIndex := TLiquids.Instance.FindDiluentIndex(self.RunStep.Diluent, true);

    // keine Tipmap: Nimm alle Tips
    xTipMap := self.RunStep.TipMap;
    if (xTipMap = 0) then
        xTipMap := xPipDevice.OKTips;

    // Cycles muss immer mindestens 1 sein
    xCycles := self.RunStep.Cycles;
    if (xCycles < 1) then
        xCycles := 1;

    ExecuteFlush(xUsedArm, xTipMap, xDilIndex, xCycles, self.RunStep.Volume,
        TTipSystem.PipPumpNumberToPipPumpIndex(self.RunStep.PumpNumber), self.RunStep.UseAllPumps,
        self.RunStep.UsePeriPump, GetDryAfterWashSetting());
end;

{ TFlushRunActionTypeInfo }

constructor TFlushRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionFlush = '1.0.0';
    cStepTypeNameFlush = cActionNameFlush;
begin
    inherited Create(cStepTypeNameFlush, cStepTypeVersionFlush, aLibName, aLibVersion);
end;

procedure TFlushRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TFlushRunActionCreator.Create();

end;


end.
