{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  19.05.10 pk                                        TN5113    initial revision
  08.08.12 wl                                        TN5946   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit RestartSetMarkRunAction;


interface


uses
    RunStep,
    RunAction,
    RunActionTypeInfo,
    ThreadClasses,
    RestartSetMarkRunStep;

type

    TRestartSetMarkRunAction = class(TRunAction)
    private
        function GetRunStep: TRestartSetMarkRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TRestartSetMarkRunStep read GetRunStep;
    end;

    TRestartSetMarkRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TRestartSetMarkRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    MethBuildField,
    EventManager,
    TraceManager;

{ TRestartSetMarkRunActionCreator }

function TRestartSetMarkRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TRestartSetMarkRunStep;
end;

function TRestartSetMarkRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TRestartSetMarkRunAction.Create(aRunStep);
end;

{ TRestartSetMarkRunAction }
function TRestartSetMarkRunAction.GetRunStep: TRestartSetMarkRunStep;
begin
    result := fRunStep as TRestartSetMarkRunStep;
end;

procedure TRestartSetMarkRunAction.ExecFirst();
begin
    TTraceManager.Instance.FlushCurrentProcessAndThread();
end;

{ TRestartSetMarkRunActionTypeInfo }

constructor TRestartSetMarkRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionRestartSetMark = '1.0.0';
    cStepTypeNameRestartSetMark = cActionNameRestartSetMark;
begin
    inherited Create(cStepTypeNameRestartSetMark, cStepTypeVersionRestartSetMark, aLibName, aLibVersion);
end;

procedure TRestartSetMarkRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TRestartSetMarkRunActionCreator.Create();

end;


end.
