{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk), Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  08.09.08 pk                                        TN4215     Code From Action.pas
  20.09.08 pk  GetMustPutBackTool                    TN4215
  16.11.10 wl                                        TN5351   uses ActionLow entfernt
  22.08.13 wl                                        TN6233   an TDeviceInitHandling angepasst
  ----------------------------------------------------------------------------------------------------------------------- }

unit InitRunAction;


interface


uses
    RunStep,
    RunAction,
    RunActionTypeInfo,
    ThreadClasses,
    InitRunStep;

type
    TInitRunAction = class(TRunAction)
    private
        function GetRunStep: TInitRunStep;
    protected
        function GetMustPutBackTool: boolean; override;
    public
        procedure ExecFirst(); override;
        property RunStep: TInitRunStep read GetRunStep;
    end;

    TInitRunActionCreator = class(TRunActionCreator)
    protected
        function GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean; override;
        function DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction; override;
    end;

    TInitRunActionTypeInfo = class(TRunActionTypeInfo)
    protected
        procedure DoCreateRunActionCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    DeviceInitHandling;

{ TInitRunActionCreator }

function TInitRunActionCreator.GetIsValidCreatorForRunStep(const aRunStep: TRunStep): boolean;
begin
    result := aRunStep is TInitRunStep;
end;

function TInitRunActionCreator.DoCreateRunActionFromStep(const aRunStep: TRunStep): TRunAction;
begin
    result := TInitRunAction.Create(aRunStep);
end;

{ TInitRunAction }

function TInitRunAction.GetMustPutBackTool: boolean;
begin
    result := true;
end;

function TInitRunAction.GetRunStep: TInitRunStep;
begin
    result := fRunStep as TInitRunStep;
end;

procedure TInitRunAction.ExecFirst();
begin
    TDeviceInitHandling.ConnectAndInitRun(true);
end;

{ TInitRunActionTypeInfo }

constructor TInitRunActionTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionInit = '1.0.0';
    cStepTypeNameInit = cActionNameInit;
begin
    inherited Create(cStepTypeNameInit, cStepTypeVersionInit, aLibName, aLibVersion);
end;

procedure TInitRunActionTypeInfo.DoCreateRunActionCreator;
begin
    fRunActionCreator := TInitRunActionCreator.Create();
end;


end.
