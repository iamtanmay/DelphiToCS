unit TipRunStepBatcher;
{ ----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ---------------------------------------------------------
  20.09.08 pk                                         TN4215    Initial Revision
  12.09.09 wl                                         TN4740    an TipMapUtils angepasst
  15.12.09 pk  FindPossibleTip                        TN4943    New: determines which tip should be used by using the AllowedTips property
  15.12.09 pk  IsStepCompatible                       TN4943    changes to allow Tip property do be determined dynamically
  ---------------------------------------------------------------------------------------------------------------------- }


interface


uses
    RunStep,
    IntfPipDevice,
    AppTypes,
    RunstepBatcher;

type
    TTipRunStepBatcher = class(TRunStepBatcher)
    protected
        function GetCompositeRunStep: TMultiTipRunStep;
        function FindPossibleTip(const aStep: TMultiTipSubRunStep; const aPipDevice: IPipDevice)
            : integer; virtual;
        procedure DoBeforeAddStepToBatch(const aStep: TRunStep); override;
        procedure IsStepCompatible(const aStep: TRunStep); override;
        property CompositeRunStep: TMultiTipRunStep read GetCompositeRunStep;
    public
        constructor Create(const aCompositeRunStep: TMultiTipRunStep);
    end;


implementation


uses
    SysUtils,
    RackTypes,
    ErrorManager,
    MethodGUIParsing,
    TipMapUtils,
    PipDeviceManager;

const
    cTipIndexUndefined = -1;

    { TTipRunStepBatcher }

constructor TTipRunStepBatcher.Create(const aCompositeRunStep: TMultiTipRunStep);
begin
    inherited Create(aCompositeRunStep);
    self.CompositeRunStep.UsedTips := TTipMapUtils.EmptyTipMap();
    self.CompositeRunStep.PipDeviceName := '';
end;

function TTipRunStepBatcher.FindPossibleTip(const aStep: TMultiTipSubRunStep;
    const aPipDevice: IPipDevice): integer;
begin
    result := aStep.Tip;
    if result <> cTipIndexUndefined then
        EXIT;

    // if the tip is undefined, used the AllowedTips property to determine the next available tip
    // This is a very trivial, sub-optimal way of generating tip usage.  For example, it would be better if we checked if all tips can reach the required rack position.
    result := TTipMapUtils.GetFirstUnselectedTipIndex(self.CompositeRunStep.UsedTips, aStep.AllowedTips,
        aPipDevice.TipCount);
end;

procedure TTipRunStepBatcher.DoBeforeAddStepToBatch(const aStep: TRunStep);
var
    xStep: TMultiTipSubRunStep;
    xUsedDevice: IPipDevice;
    xUsedTips: TIPMAP;
begin
    inherited;
    ASSERT(aStep is TMultiTipSubRunStep);
    xStep := aStep as TMultiTipSubRunStep;

    xUsedDevice := gPipDeviceManager.FindPipDevice_ByName(xStep.PipDeviceName);

    if (fCompositeRunStep.Count = 0) then
    begin
        ASSERT(Assigned(xUsedDevice), 'PipDevice not be found'); // Arm is defined
        self.CompositeRunStep.PipDeviceName := xStep.PipDeviceName;

        if xStep.Tip = cTipIndexUndefined then
        begin
            self.CompositeRunStep.AllowedTips := xStep.AllowedTips;
        end;
    end;

    xStep.Tip := FindPossibleTip(xStep, xUsedDevice);

    xUsedTips := self.CompositeRunStep.UsedTips;
    TTipMapUtils.SelectTip(xUsedTips, xStep.Tip);
    self.CompositeRunStep.UsedTips := xUsedTips;
end;

function TTipRunStepBatcher.GetCompositeRunStep: TMultiTipRunStep;
begin
    result := fCompositeRunStep as TMultiTipRunStep;
end;

procedure TTipRunStepBatcher.IsStepCompatible(const aStep: TRunStep);
var
    xPipDevice: IPipDevice;
    xStep: TMultiTipSubRunStep;
    xPossibleTip: integer;
begin
    inherited;

    ASSERT(aStep is TMultiTipSubRunStep);
    xStep := aStep as TMultiTipSubRunStep;

    xPipDevice := gPipDeviceManager.FindPipDevice_ByName(xStep.PipDeviceName);
    ASSERT(Assigned(xPipDevice), 'PipDevice not found');

    if (fCompositeRunStep.Count > 0) then
    begin
        if (not SameText(self.CompositeRunStep.PipDeviceName, xPipDevice.Name)) then
            RaiseBreak('Different Arm');
    end;

    // check if current job can be done
    if (fCompositeRunStep.Count >= xPipDevice.TipCount) then
        RaiseBreak('No of steps > TipCount'); // Ensure Sample number is smaller than tip count

    // if the tip is not defined by the user, check if there is any tip available
    if xStep.Tip = cTipIndexUndefined then
    begin
        xPossibleTip := FindPossibleTip(xStep, xPipDevice);
        if xPossibleTip = cTipIndexUndefined then
            RaiseBreak('No possible tip');

        // steps that have a different AllowedTips tipmap CANNOT be combined
        if fCompositeRunStep.Count > 0 then
        begin
            if (self.CompositeRunStep.AllowedTips <> xStep.AllowedTips) then
                RaiseBreak('Different Tipmap');
        end;
    end
    else
    begin
        if (fCompositeRunStep.Count > 0) then
        begin
            if TTipMapUtils.TipSelected(self.CompositeRunStep.UsedTips, xStep.Tip) then
                RaiseBreak('Tip already used');
        end;
    end;

end;


end.
