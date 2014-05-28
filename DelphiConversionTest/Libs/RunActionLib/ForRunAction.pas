{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  20.09.12 wl                                      TN5982   Initial Revision
  13.02.13 wl  ExecFirst                           TN6075   Variable wird nur noch bis zum Endwert geschrieben
  ----------------------------------------------------------------------------------------------------------- }

unit ForRunAction;


interface


uses
    RunStep,
    RunAction,
    RunActionTypeInfo,
    ThreadClasses,
    ForRunStep;

type
    TForRunAction = class(TRunAction)
    private
        function GetRunStep: TForRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TForRunStep read GetRunStep;
    end;

    TForRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TForRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TEndForRunAction = class(TRunAction)
    private
        function GetRunStep: TEndForRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TEndForRunStep read GetRunStep;
    end;

    TEndForRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TEndForRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    MethBuildField,
    MethodCompiledFile,
    EventManager;

{ TForRunActionCreator }

function TForRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TForRunStep;
end;

function TForRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TForRunAction.Create(aRunStep);
end;

{ TForRunAction }

function TForRunAction.GetRunStep: TForRunStep;
begin
    result := fRunStep as TForRunStep;
end;

procedure TForRunAction.ExecFirst();
begin
    if self.RunStep.ConditionResult then
    begin
        // Counter-Variable speichern (wird intern nicht benötigt)
        TMethodEvalTable.SetIdentValAsInt(self.RunStep.StoreKeyCounter, self.RunStep.CounterValue)
    end
    else
    begin
        // Sprung hinter das Ende der Schleife (wenn Bedingung nicht erfüllt)
        TEventManager.Instance.JumpLocal(self.RunStep.EndRelativeAddress + 1);
    end;
end;

{ TForRunActionTypeInfo }

constructor TForRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionFor = '1.0.0';
    cStepTypeNameFor = TForRunStepInfo.cActionNameFor;
begin
    inherited Create(cStepTypeNameFor, cStepTypeVersionFor, aLibName, aLibVersion);
end;

procedure TForRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TForRunActionCreator.Create();

end;

{ TEndForRunActionCreator }

function TEndForRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TEndForRunStep;
end;

function TEndForRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TEndForRunAction.Create(aRunStep);
end;

{ TEndForRunAction }

function TEndForRunAction.GetRunStep: TEndForRunStep;
begin
    result := fRunStep as TEndForRunStep;
end;

procedure TEndForRunAction.ExecFirst();
begin
    TEventManager.Instance.JumpLocal(self.RunStep.BeginRelativeAddress);
end;

{ TEndForRunActionTypeInfo }

constructor TEndForRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndFor = '1.0.0';
    cStepTypeNameEndFor = TEndForRunStepInfo.cActionNameEndFor;
begin
    inherited Create(cStepTypeNameEndFor, cStepTypeVersionEndFor, aLibName, aLibVersion);
end;

procedure TEndForRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TEndForRunActionCreator.Create();

end;


end.
