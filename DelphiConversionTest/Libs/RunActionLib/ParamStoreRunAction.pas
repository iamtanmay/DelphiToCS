{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  07.05.10 pk  ExecFirst                             TN5092      New ArrayIndex property
  21.09.10 pk                                        TN5047      Various changes to MethodEditor for displaying indented actions
  29.06.11 wl  ArrayIndex                            TN5618   entfernt (ist nicht mehr notwendig)
  02.03.12 wl  ExecFirst                             TN5822   Speichern mit TMethodEvalTable.SetIdentValRaw
  ----------------------------------------------------------------------------------------------------------------------- }

unit ParamStoreRunAction;


interface


uses
    RunStep,
    RunAction,
    RunActionTypeInfo,
    ThreadClasses,
    ParamStoreRunStepBuilder;

type

    TParamStoreRunAction = class(TRunAction)
    private
        function GetRunStep: TParamStoreRunStep;
    public
        procedure ExecFirst(); override;
        property RunStep: TParamStoreRunStep read GetRunStep;
    end;

    TParamStoreRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TParamStoreRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    MethodCompiledFile;

{ TParamStoreRunActionCreator }

function TParamStoreRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TParamStoreRunStep;
end;

function TParamStoreRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TParamStoreRunAction.Create(aRunStep);
end;

{ TParamStoreRunAction }
function TParamStoreRunAction.GetRunStep: TParamStoreRunStep;
begin
    result := fRunStep as TParamStoreRunStep;
end;

procedure TParamStoreRunAction.ExecFirst();
begin
    TMethodEvalTable.SetIdentValRaw(self.RunStep.Key, self.RunStep.Value);
end;

{ TParamStoreRunActionTypeInfo }

constructor TParamStoreRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionParamStore = '1.0.0';
    cStepTypeNameParamStore = cActionNameParamStore;
begin
    inherited Create(cStepTypeNameParamStore, cStepTypeVersionParamStore, aLibName, aLibVersion);
end;

procedure TParamStoreRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TParamStoreRunActionCreator.Create();

end;


end.
