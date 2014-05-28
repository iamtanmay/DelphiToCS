unit VariableSetLengthRunAction;
{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  07.05.10 pk  ExecFirst                             TN5092      New ArrayIndex property
  ----------------------------------------------------------------------------------------------------------------------- }


interface


uses
    RunStep,
    RunAction,
    RunActionTypeInfo,
    ThreadClasses,
    VariableSetLengthRunStep;

type

    TVariableSetLengthRunAction = class(TRunAction)
    private
        function GetRunStep: TVariableSetLengthRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TVariableSetLengthRunStep read GetRunStep;
    end;

    TVariableSetLengthRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TVariableSetLengthRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    MethodCompiledFile;

{ TVariableSetLengthRunActionCreator }

function TVariableSetLengthRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TVariableSetLengthRunStep;
end;

function TVariableSetLengthRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TVariableSetLengthRunAction.Create(aRunStep);
end;

{ TVariableSetLengthRunAction }
function TVariableSetLengthRunAction.GetRunStep: TVariableSetLengthRunStep;
begin
    result := fRunStep as TVariableSetLengthRunStep;
end;

procedure TVariableSetLengthRunAction.ExecFirst();
begin
    TMethodEvalTable.SetIdentifierValueArrayLength(self.RunStep.VariableName, self.RunStep.VariableLength);
end;

{ TVariableSetLengthRunActionTypeInfo }

constructor TVariableSetLengthRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionVariableSetLength = '1.0.0';
    cStepTypeNameVariableSetLength = cActionNameVariableSetLength;
begin
    inherited Create(cStepTypeNameVariableSetLength, cStepTypeVersionVariableSetLength, aLibName,
        aLibVersion);
end;

procedure TVariableSetLengthRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TVariableSetLengthRunActionCreator.Create();

end;


end.
