{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  09.12.08 pk                                        TN4279      Initial Revision
  17.02.09 pk  ExecFirst                             TN4232      Add SideEffect
  24.02.09 pk  ExecFirst                             TN4232      check TraceManager.Enabled
  25.02.09 pk  ExecFirst                             TN4279      New isEvent param for CallMethod
  04.03.09 pk  ExecFirst                             TN4232      Params changed to TStreamableKeyValueList
  04.02.10 pk                                        TN4972      Changes for Restart
  07.05.10 pk  ExecFirst                             TN5092      RunStep.Params is now a TKeyArgValueList
  07.06.10 pk                                        TN5077      uses changed
  08.08.12 wl                                        TN5946   uses geändert
  25.06.13 wl                                        TN6178   Neuer Parameter 'Return variable name' (Mehrere Namen komma-getrennt bereits möglich)
  ----------------------------------------------------------------------------------------------------------------------- }

unit AddMethodRunAction;


interface


uses
    RunStep,
    RunAction,
    RunActionTypeInfo,
    AddMethodRunStep,
    AppTypes;

type

    TAddMethodRunAction = class(TRunAction)
    private
        function GetRunStep: TAddMethodRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TAddMethodRunStep read GetRunStep;
    end;

    TAddMethodRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TAddMethodRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    LogManager,
    RunStepBuilderProcessor,
    RunTraceManager,
    MethodCompiledFile,
    MethBuildField;

{ TAddMethodRunActionCreator }

function TAddMethodRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TAddMethodRunStep;
end;

function TAddMethodRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TAddMethodRunAction.Create(aRunStep);
end;

{ TAddMethodRunAction }

function TAddMethodRunAction.GetRunStep: TAddMethodRunStep;
begin
    result := fRunStep as TAddMethodRunStep;
end;

procedure TAddMethodRunAction.ExecFirst();
var
    xReturnKeyNames: TArray<string>;
begin
    xReturnKeyNames := self.RunStep.ReturnKeys.ToStringArray();

    TRunStepBuilderProcessor.PrepareCallMethod(self.RunStep.SubMethodName, self.RunStep.Params, false,
        xReturnKeyNames);
end;

{ TAddMethodRunActionTypeInfo }

constructor TAddMethodRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionAddMethod = '1.0.0';
    cStepTypeNameAddMethod = cActionAddMethod;
begin
    inherited Create(cStepTypeNameAddMethod, cStepTypeVersionAddMethod, aLibName, aLibVersion);
end;

procedure TAddMethodRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TAddMethodRunActionCreator.Create();

end;


end.
