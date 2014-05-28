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

unit SubMethodRunStep;


interface


uses
    RunStep,
    CustomSetting,
    MethodStep;

type
    TSubMethodRunStep = class(TCompositeRunStep)
    protected
        fName: string;
        function DoGetDescription(): string; override;
    public
        constructor Create(aOwnsSteps: boolean; const aName: string);
    end;

    TMethodStepSetting_SubMethod = class(TMethodStepCompositeSetting)
    protected
        function GetMethName: TCustomSetting; virtual; abstract;
    public
        property MethName: TCustomSetting read GetMethName;
    end;

    TSubMethodMethodStep = class(TMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_SubMethod;
    public
        property MainSubOptionSetting: TMethodStepSetting_SubMethod read GetMainSubOptionSetting;
    end;


implementation


uses
    GeneralTypes;

{ TSubMethodRunStep }

constructor TSubMethodRunStep.Create(aOwnsSteps: boolean; const aName: string);
begin
    inherited Create(aOwnsSteps);
    fName := aName;
end;

function TSubMethodRunStep.DoGetDescription(): string;
begin
    result := TTypeSafeFormat.Format('Method Name: {0}', [fName]);
end;

{ TSubMethodMethodStep }

function TSubMethodMethodStep.GetMainSubOptionSetting: TMethodStepSetting_SubMethod;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_SubMethod;
end;


end.
