{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  25.06.13 wl                                      TN6178   Initial Revision
  10.09.13 wl                                      TN6187   Args: Mehrere Werte können jetzt übergeben werden
  ----------------------------------------------------------------------------------------------------------- }

unit ReturnRunAction;


interface


uses
    RunStep,
    RunAction,
    RunActionTypeInfo,
    ThreadClasses,
    ReturnRunStep;

type
    TReturnRunAction = class(TRunAction)
    private
        function GetRunStep: TReturnRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TReturnRunStep read GetRunStep;
    end;

    TReturnRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TReturnRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    MethBuildField,
    Streamable,
    RunStepBuilderProcessor;

{ TReturnRunActionCreator }

function TReturnRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TReturnRunStep;
end;

function TReturnRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TReturnRunAction.Create(aRunStep);
end;

{ TReturnRunAction }

function TReturnRunAction.GetRunStep: TReturnRunStep;
begin
    result := fRunStep as TReturnRunStep;
end;

procedure TReturnRunAction.ExecFirst();
var
    xArray: TArray<TStreamableItem>;
    x: integer;
begin
    SetLength(xArray, self.RunStep.Args.Count);
    for x := 0 to self.RunStep.Args.Count - 1 do
        xArray[x] := self.RunStep.Args[x] as TStreamableItem;
    TRunStepBuilderProcessor.WriteReturnKeyValues(xArray);

    TRunStepBuilderProcessor.JumpLocal(self.RunStep.EndRelativeAddress + 1);
end;

{ TReturnRunActionTypeInfo }

constructor TReturnRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionReturn = '1.0.0';
    cStepTypeNameReturn = cActionNameReturn;
begin
    inherited Create(cStepTypeNameReturn, cStepTypeVersionReturn, aLibName, aLibVersion);
end;

procedure TReturnRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TReturnRunActionCreator.Create();

end;


end.
