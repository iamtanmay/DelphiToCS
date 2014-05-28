{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  09.08.12 wl                                      TN5946   Initial Revision
  ----------------------------------------------------------------------------------------------------------- }

unit UnknownMethodStep;


interface


uses
    Classes,
    MethodStep,
    RunStepInfo;

type
    TUnknownMethodStep = class(TMethodStep)
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo: TRunStepInfo; override;
    end;


implementation


uses
    GeneralTypes,
    CustomLeafSettings;

{ TUnknownMethodStep }

procedure TUnknownMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TCustomSetting_NoKeyOption.Create(TLanguageString.Read('Remark text',
        'Bemerkungstext'), true));
end;

function TUnknownMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TUnknownRunStepInfo.Create();
end;


end.
