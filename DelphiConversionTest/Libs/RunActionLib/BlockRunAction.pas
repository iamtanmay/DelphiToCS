{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  08.04.10 pk  PreProcessSteps                       TN4996   Changes for MultiPip
  14.08.13 wl                                        TN6218   verwendet TRunStepListIterator
  ----------------------------------------------------------------------------------------------------------------------- }

unit BlockRunAction;


interface


uses
    RunStep,
    RunStepInfo,
    RunAction,
    RunActionTypeInfo,
    RunStepBatcher,
    RunstepTranslator,
    RunStepBlock;

type
    TRunActionBlockType = TRunStepClass; // WL: furchtbar

    TBlockRunAction = class(TRunAction)
    protected
        function GetBlockType(): TRunActionBlockType; virtual; abstract;
    end;

    TBlockBeginRunAction = class(TBlockRunAction)
    protected
        function CreateBlockParams(): TRunStepBlockParams; virtual;
    public
        procedure ExecFirst(); override;
    end;

    TBlockAddRunAction = class(TBlockRunAction)
    public
        procedure ExecFirst(); override;
    end;

    TBlockEndRunAction = class(TBlockRunAction)
    protected
        procedure PreProcessSteps(const aRunSteps: TRunStepList;
            const aBlockParams: TRunStepBlockParams); virtual;
    public
        procedure ExecFirst(); override;
    end;

    TBlockRunStepTranslator = class(TRunStepTranslator)
    protected
        function DoTranslateSteps(const aSourceRunSteps: TRunStepList; const aDestRunSteps: TRunStepList)
            : boolean; override;
        function GetBlockType(): TRunActionBlockType; virtual; abstract;
        function CreateBlockAddRunStepInfo: TRunStepInfo; virtual; abstract;
    end;

    TBlockRunActionCreator = class(TRunActionCreator)
    protected
        function CreateMultiRunStepInfo(): TRunStepInfo; virtual; abstract;
        function CreateMultiRunStep(): TCompositeRunStep; virtual; abstract;
        function CreateRunStepBatcher(const aMultiRunStep: TCompositeRunStep): TRunStepBatcher;
            virtual; abstract;
        function CreateRunActionFromMultiRunStep(const aMultiRunStep: TCompositeRunStep): TRunAction;
            virtual; abstract;
        function BatchAndCreateActionFromStepList(const aRunStepListIterator: TRunStepListIterator)
            : TRunAction;
    public
        function DoCreateRunActionFromStepList(const aRunStepListIterator: TRunStepListIterator)
            : TRunAction; override;
    end;


implementation


uses
    SysUtils,
    RunStepBlockManager,
    EventManager,
    RunStepFactory,
    RunActionFactory;

{ TBlockBeginRunAction }

function TBlockBeginRunAction.CreateBlockParams(): TRunStepBlockParams;
begin
    result := TRunStepBlockParams.Create();
end;

procedure TBlockBeginRunAction.ExecFirst;
begin
    TRunStepBlockManager.Instance.BeginBlockForCurrentThread(self.GetBlockType, CreateBlockParams());
end;

{ TBlockEndRunAction }

procedure TBlockEndRunAction.PreProcessSteps(const aRunSteps: TRunStepList;
    const aBlockParams: TRunStepBlockParams);
begin

end;

procedure TBlockEndRunAction.ExecFirst;
var
    xRunSteps: TRunStepList;
    xBlockParams: TRunStepBlockParams;
begin
    xRunSteps := TRunStepList.Create(false);
    try
        TRunStepBlockManager.Instance.CopyStepsForCurrentThread(self.GetBlockType, xRunSteps, xBlockParams);
        PreProcessSteps(xRunSteps, xBlockParams);
        TEventManager.Instance.AddStepsToPendingRunSteps(xRunSteps, true);
    finally
        FreeAndNil(xRunSteps);
    end;

    TRunStepBlockManager.Instance.EndBlockForCurrentThread(self.GetBlockType);
end;

{ TBlockAddRunAction }

procedure TBlockAddRunAction.ExecFirst;
var
    xRunSteps: TRunStepList;
begin
    ASSERT(self.RunStep is TBlockAddRunStep);
    xRunSteps := TRunStepList.Create(false);
    try
        xRunSteps.AddSteps((self.RunStep as TBlockAddRunStep));
        TRunStepBlockManager.Instance.AddStepsForCurrentThread(self.GetBlockType, xRunSteps);
    finally
        FreeAndNil(xRunSteps);
    end;
end;

{ TBlockRunActionCreator }

function TBlockRunActionCreator.BatchAndCreateActionFromStepList(const aRunStepListIterator
    : TRunStepListIterator): TRunAction;
var
    xRunStepBatcher: TRunStepBatcher;
    xRunStep: TCompositeRunStep;
begin
    xRunStep := CreateMultiRunStep();
    xRunStep.RunStepInfo := CreateMultiRunStepInfo();

    xRunStepBatcher := CreateRunStepBatcher(xRunStep);
    try
        xRunStepBatcher.AddStepsToBatch(aRunStepListIterator);
    finally
        xRunStepBatcher.Free;
    end;

    result := CreateRunActionFromMultiRunStep(xRunStep);
    result.OwnsRunStep := true;
end;

function TBlockRunActionCreator.DoCreateRunActionFromStepList(const aRunStepListIterator
    : TRunStepListIterator): TRunAction;
begin
    result := BatchAndCreateActionFromStepList(aRunStepListIterator);
end;

{ TBlockRunStepTranslator }

function TBlockRunStepTranslator.DoTranslateSteps(const aSourceRunSteps: TRunStepList;
    const aDestRunSteps: TRunStepList): boolean;
var
    xBlockAddRunStep: TBlockAddRunStep;
    xBlockAddRunStepInfo: TRunStepInfo;
begin
    result := TRunStepBlockManager.Instance.BlockExistsForCurrentThread(self.GetBlockType);

    if not result then
        EXIT;

    xBlockAddRunStepInfo := CreateBlockAddRunStepInfo();
    xBlockAddRunStep := TRunStepFactory.CreateRunStepByStepInfo(xBlockAddRunStepInfo) as TBlockAddRunStep;
    xBlockAddRunStep.AddSteps(aSourceRunSteps);
    aDestRunSteps.AddStep(xBlockAddRunStep);
end;


end.
