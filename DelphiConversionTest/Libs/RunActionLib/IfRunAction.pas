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

unit IfRunAction;


interface


uses
    RunStep,
    RunAction,
    RunActionTypeInfo,
    ThreadClasses,
    IfRunStep;

type
    TIfRunAction = class(TRunAction)
    private
        function GetRunStep: TIfRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TIfRunStep read GetRunStep;
    end;

    TIfRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TIfRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TEndIfRunAction = class(TRunAction)
    private
        function GetRunStep: TEndIfRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TEndIfRunStep read GetRunStep;
    end;

    TEndIfRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TEndIfRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    MethBuildField,
    EventManager;

{ TIfRunActionCreator }

function TIfRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TIfRunStep;
end;

function TIfRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TIfRunAction.Create(aRunStep);
end;

{ TIfRunAction }

function TIfRunAction.GetRunStep: TIfRunStep;
begin
    result := fRunStep as TIfRunStep;
end;

procedure TIfRunAction.ExecFirst();
begin
    if not self.RunStep.ConditionResult then
    begin
        TEventManager.Instance.JumpLocal(self.RunStep.EndRelativeAddress);
    end;
end;

{ TIfRunActionTypeInfo }

constructor TIfRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionIf = '1.0.0';
    cStepTypeNameIf = cActionNameIf;
begin
    inherited Create(cStepTypeNameIf, cStepTypeVersionIf, aLibName, aLibVersion);
end;

procedure TIfRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TIfRunActionCreator.Create();

end;

{ TEndIfRunActionCreator }

function TEndIfRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TEndIfRunStep;
end;

function TEndIfRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TEndIfRunAction.Create(aRunStep);
end;

{ TEndIfRunAction }

function TEndIfRunAction.GetRunStep: TEndIfRunStep;
begin
    result := fRunStep as TEndIfRunStep;
end;

procedure TEndIfRunAction.ExecFirst();
begin
    // No action
end;

{ TEndIfRunActionTypeInfo }

constructor TEndIfRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndIf = '1.0.0';
    cStepTypeNameEndIf = cActionNameEndIf;
begin
    inherited Create(cStepTypeNameEndIf, cStepTypeVersionEndIf, aLibName, aLibVersion);
end;

procedure TEndIfRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TEndIfRunActionCreator.Create();

end;


end.
