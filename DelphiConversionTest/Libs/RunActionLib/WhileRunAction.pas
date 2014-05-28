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

unit WhileRunAction;


interface


uses
    RunStep,
    RunAction,
    RunActionTypeInfo,
    ThreadClasses,
    WhileRunStep;

type

    TWhileRunAction = class(TRunAction)
    private
        function GetRunStep: TWhileRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TWhileRunStep read GetRunStep;
    end;

    TWhileRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TWhileRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TEndWhileRunAction = class(TRunAction)
    private
        function GetRunStep: TEndWhileRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TEndWhileRunStep read GetRunStep;
    end;

    TEndWhileRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TEndWhileRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    MethBuildField,
    EventManager;

{ TWhileRunActionCreator }

function TWhileRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TWhileRunStep;
end;

function TWhileRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TWhileRunAction.Create(aRunStep);
end;

{ TWhileRunAction }
function TWhileRunAction.GetRunStep: TWhileRunStep;
begin
    result := fRunStep as TWhileRunStep;
end;

procedure TWhileRunAction.ExecFirst();
begin
    if not self.RunStep.ConditionResult then
    begin
        TEventManager.Instance.JumpLocal(self.RunStep.EndRelativeAddress + 1);
    end;
end;

{ TWhileRunActionTypeInfo }

constructor TWhileRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionWhile = '1.0.0';
    cStepTypeNameWhile = cActionNameWhile;
begin
    inherited Create(cStepTypeNameWhile, cStepTypeVersionWhile, aLibName, aLibVersion);
end;

procedure TWhileRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TWhileRunActionCreator.Create();

end;

{ TEndWhileRunActionCreator }

function TEndWhileRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TEndWhileRunStep;
end;

function TEndWhileRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TEndWhileRunAction.Create(aRunStep);
end;

{ TEndWhileRunAction }
function TEndWhileRunAction.GetRunStep: TEndWhileRunStep;
begin
    result := fRunStep as TEndWhileRunStep;
end;

procedure TEndWhileRunAction.ExecFirst();
begin
    TEventManager.Instance.JumpLocal(self.RunStep.BeginRelativeAddress);
end;

{ TEndWhileRunActionTypeInfo }

constructor TEndWhileRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndWhile = '1.0.0';
    cStepTypeNameEndWhile = cActionNameEndWhile;
begin
    inherited Create(cStepTypeNameEndWhile, cStepTypeVersionEndWhile, aLibName, aLibVersion);
end;

procedure TEndWhileRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TEndWhileRunActionCreator.Create();

end;


end.
