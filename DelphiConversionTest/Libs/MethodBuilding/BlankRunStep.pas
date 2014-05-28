{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  08.08.12 wl                                      TN5946   Initial Revision
  20.09.12 wl                                      TN5982   nicht mehr von ControlFlowRunstep abgeleitet
  ----------------------------------------------------------------------------------------------------------- }

unit BlankRunStep;


interface


uses
    Classes,
    RunStepInfo,
    RunStepInfoTypeInfo,
    RunStep,
    RunStepBuilderHelper,
    RunStepTypeInfo,
    MethBuildField,
    MethodStep,
    RunStepBuilder,
    CustomSetting;

const
    cActionNameBlank = 'BLANK';

type
    TBlankRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
        function GetIsCodeStep: boolean; override;
        function GetIsExecutable: boolean; override;
    public
        constructor Create(); override;
    end;

    TBlankRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TBlankRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function GetIsHidden(): boolean; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TBlankRunStep = class(TRunStep)
    protected
        function DoGetDescription(): string; override;
    end;

    TBlankRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TBlankRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TMethodStepSetting_Blank = class(TMethodStepCompositeSetting)
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
    end;

    TBlankMethodStep = class(TMethodStep)
    private
        function GetCondition: TCustomSetting;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        property Condition: TCustomSetting read GetCondition;
    end;

    TBlankRunStepBuilder = class(TRunStepByMethodStepBuilder)
    protected
        function GetMStep(): TBlankMethodStep;
        function GetParseField: TMethBuildEvaluableField;
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TBlankMethodStep read GetMStep;
        property ParseField: TMethBuildEvaluableField read GetParseField;
    end;

    TBlankRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    GeneralTypes,
    CustomLeafSettings;

{ TBlankRunStepInfo }

constructor TBlankRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('Blank line', 'Leere Zeile');
    fDescription := TLanguageString.Read('This is blank line.', 'Diese ist eine leere Zeile.');
end;

function TBlankRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TBlankRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameBlank;
end;

function TBlankRunStepInfo.GetIsCodeStep: boolean;
begin
    result := true;
end;

function TBlankRunStepInfo.GetIsExecutable: boolean;
begin
    result := false;
end;

{ TBlankRunStepInfoCreator }

function TBlankRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TBlankRunStepInfo.Create();
end;

{ TBlankRunStepInfoTypeInfo }

constructor TBlankRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionBlank = '1.0.0';
    cStepTypeNameBlank = cActionNameBlank;
begin
    inherited Create(cStepTypeNameBlank, cStepTypeVersionBlank, aLibName, aLibVersion);
end;

procedure TBlankRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TBlankRunStepInfoCreator.Create();
end;

function TBlankRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;

{ TBlankRunStep }

function TBlankRunStep.DoGetDescription: string;
begin
    result := '';
end;

{ TBlankRunStepCreator }

function TBlankRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TBlankRunStep.Create();
end;

{ TBlankRunStepTypeInfo }

constructor TBlankRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionBlank = '1.0.0';
    cStepTypeNameBlank = cActionNameBlank;
begin
    inherited Create(cStepTypeNameBlank, cStepTypeVersionBlank, aLibName, aLibVersion);
end;

procedure TBlankRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TBlankRunStepCreator.Create();
end;

{ TMethodStepSetting_Blank }

constructor TMethodStepSetting_Blank.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);
    AddParam(TCustomSetting_NoKeyOption.Create(TLanguageString.Read('Blank line', 'Leere Zeile'), false));
end;

function TMethodStepSetting_Blank.GetOptionSummary: string;
begin
    result := '';
end;

{ TBlankMethodStep }

procedure TBlankMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_Blank.Create(aOnAddEditFunctions));
end;

function TBlankMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TBlankRunStepInfo.Create();
end;

function TBlankMethodStep.GetCondition: TCustomSetting;
begin
    result := self.OptionsParam.Params[0];
end;

function TBlankRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TBlankMethodStep;
end;

{ TBlankRunStepBuilderTypeInfo }

constructor TBlankRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionBlank = '1.0.0';
    cStepTypeNameBlank = cActionNameBlank;
begin
    inherited Create(cStepTypeNameBlank, cStepTypeVersionBlank, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TBlankRunStepBuilder, TBlankMethodStep));
end;

function TBlankRunStepBuilder.GetMStep: TBlankMethodStep;
begin
    result := inherited MStep as TBlankMethodStep;
end;

function TBlankRunStepBuilder.GetParseField: TMethBuildEvaluableField;
begin
    result := self.MStep.Condition.ParseField as TMethBuildEvaluableField;
end;

procedure TBlankRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper);
begin
    CreateRunStepByNameAndAdd(aRunSteps);
end;


end.
