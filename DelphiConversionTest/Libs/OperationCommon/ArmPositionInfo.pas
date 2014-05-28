{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  23.05.13 wl                                      TN6153   Initial Revision
  05.06.13 wl                                      TN6164   Bezeichnung "Previous" entfernt
  ----------------------------------------------------------------------------------------------------------- }

unit ArmPositionInfo;


interface


uses
    AppTypes,
    RackTypes,
    IntfArmDevice;

type
    TArmPositionInfo = class(TCustomArmPositionInfo)
    private
        procedure SetZStep(const aValue: TPipStepZPos);
    protected
        fRP: TArray<TXRackPosition>;
        fUsedTips: TIPMAP;
        fZStep: TPipStepZPos;
    public
        constructor Create(aUsedTips: TIPMAP; const aRP: TArray<TXRackPosition>; const aZStep: TPipStepZPos);

        class function GetArmPosition(aOnGetPreviousPosition: TGetArmPositionEvent): TArmPositionInfo;

        property RP: TArray<TXRackPosition>read fRP;
        property UsedTips: TIPMAP read fUsedTips;
        property ZStep: TPipStepZPos read fZStep;
    end;


implementation


{ TArmPositionInfo }

constructor TArmPositionInfo.Create(aUsedTips: TIPMAP; const aRP: TArray<TXRackPosition>;
    const aZStep: TPipStepZPos);
begin
    inherited Create;

    fRP := aRP;
    fUsedTips := aUsedTips;
    SetZStep(aZStep);
end;

procedure TArmPositionInfo.SetZStep(const aValue: TPipStepZPos);
begin
    fZStep.SyrMap := aValue.SyrMap;
    fZStep.Z.ZTravel := Copy(aValue.Z.ZTravel);
    fZStep.Z.ZScan := Copy(aValue.Z.ZScan);
    fZStep.Z.ZDisp := Copy(aValue.Z.ZDisp);
    fZStep.Z.ZMax := Copy(aValue.Z.ZMax);
    fZStep.Z.ZTube := Copy(aValue.Z.ZTube);
    fZStep.Z.MOffset := Copy(aValue.Z.MOffset);
end;

class function TArmPositionInfo.GetArmPosition(aOnGetPreviousPosition: TGetArmPositionEvent)
    : TArmPositionInfo;
var
    xPosition: TCustomArmPositionInfo;
begin
    if Assigned(aOnGetPreviousPosition) then
    begin
        xPosition := aOnGetPreviousPosition();
        if (xPosition is TArmPositionInfo) then
            EXIT(xPosition as TArmPositionInfo);
    end;

    EXIT(nil);
end;


end.
