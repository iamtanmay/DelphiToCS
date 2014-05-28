{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  08.08.12 wl                                      TN5946   Initial Revision
  20.09.12 wl                                      TN5982   new description for Condition Parameter
  14.11.12 wl                                      TN6018   Neue Icons für IF, FOR, WHILE
  ----------------------------------------------------------------------------------------------------------- }

unit WhileRunStep;


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
    CustomSetting,
    ControlFlowRunStepBuilder;

const
    cActionNameWhile = 'WHILE';

type
    TWhileRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
        function GetIsCodeStep: boolean; override;
        function GetIconIndex: integer; override;
    public
        constructor Create(); override;
    end;

    TWhileRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TWhileRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function GetIsHidden: boolean; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

const
    cActionNameEndWhile = 'WHEND';

type
    TEndWhileRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
        function GetIsCodeStep: boolean; override;
        function GetIconIndex: integer; override;
    public
        constructor Create(); override;
    end;

    TEndWhileRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TEndWhileRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function GetIsHidden: boolean; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TWhileRunStep = class(TRunStep)
    protected
        fConditionResult: boolean;
        fEndRelativeAddress: integer;
        function DoGetDescription(): string; override;
    public
        property ConditionResult: boolean read fConditionResult write fConditionResult;
        property EndRelativeAddress: integer read fEndRelativeAddress write fEndRelativeAddress;
    end;

    TWhileRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TWhileRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TEndWhileRunStep = class(TRunStep)
    protected
        fBeginRelativeAddress: integer;
        function DoGetDescription(): string; override;
    published
        property BeginRelativeAddress: integer read fBeginRelativeAddress write fBeginRelativeAddress;
    end;

    TEndWhileRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TEndWhileRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ############### WHILE ####################

    TMethodStepSetting_While = class(TMethodStepCompositeSetting)
    private const
        cWhileMethOptionKeyCondition = 'CONDITION';
    protected
        function GetOptionSummary: string; override;
        function GetCondition: TCustomSetting;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
        property Condition: TCustomSetting read GetCondition;

    end;

    TWhileMethodStep = class(TMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_While;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        property MainSubOptionSetting: TMethodStepSetting_While read GetMainSubOptionSetting;
    end;

    TWhileRunStepBuilder = class(TControlFlowBlockBeginRunStepBuilder)
    protected
        function GetMStep(): TWhileMethodStep;
        function GetParseField: TMethBuildEvaluableField;
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function DoIsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder)
            : boolean; override;
        function GetRunOptions(): TMethodStepSetting_While;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TWhileMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_While read GetRunOptions;
    end;

    TWhileRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ############### ENDWHILE ####################

    TMethodStepSetting_EndWhile = class(TMethodStepCompositeSetting)
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
    end;

    TEndWhileMethodStep = class(TMethodStep)
    private
        function GetCondition: TCustomSetting;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        property Condition: TCustomSetting read GetCondition;
    end;

    TEndWhileRunStepBuilder = class(TControlFlowBlockEndRunStepBuilder)
    protected
        function GetMStep(): TEndWhileMethodStep;
        function GetParseField: TMethBuildEvaluableField;
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function DoIsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder)
            : boolean; override;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TEndWhileMethodStep read GetMStep;
        property ParseField: TMethBuildEvaluableField read GetParseField;
    end;

    TEndWhileRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    SysUtils,
    MethodTypes,
    GeneralTypes,
    ParserIdentDataType,
    CustomLeafSettings;

{ TWhileRunStepInfo }

constructor TWhileRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('While-Loop Begin', 'While-Schleife Beginn');
    fDescription := TLanguageString.Read('This step begins a While-loop.',
        'Dieser Schritt beginnt eine While-Schleife.');
end;

function TWhileRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TWhileRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameWhile;
end;

function TWhileRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionWhile);
end;

function TWhileRunStepInfo.GetIsCodeStep: boolean;
begin
    result := true;
end;

{ TWhileRunStepInfoCreator }

function TWhileRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TWhileRunStepInfo.Create();
end;

{ TWhileRunStepInfoTypeInfo }

constructor TWhileRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionWhile = '1.0.0';
    cStepTypeNameWhile = cActionNameWhile;
begin
    inherited Create(cStepTypeNameWhile, cStepTypeVersionWhile, aLibName, aLibVersion);
end;

procedure TWhileRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TWhileRunStepInfoCreator.Create();
end;

function TWhileRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;

{ TEndWhileRunStepInfo }

constructor TEndWhileRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('While-Loop End', 'While-Schleife Ende');
    fDescription := TLanguageString.Read('This step ends a While-loop.',
        'Dieser Schritt beendet eine While-Schleife.');
end;

function TEndWhileRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TEndWhileRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameEndWhile;
end;

function TEndWhileRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionWhile);
end;

function TEndWhileRunStepInfo.GetIsCodeStep: boolean;
begin
    result := true;
end;

{ TEndWhileRunStepInfoCreator }

function TEndWhileRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TEndWhileRunStepInfo.Create();
end;

{ TEndWhileRunStepInfoTypeInfo }

constructor TEndWhileRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndWhile = '1.0.0';
    cStepTypeNameEndWhile = cActionNameEndWhile;
begin
    inherited Create(cStepTypeNameEndWhile, cStepTypeVersionEndWhile, aLibName, aLibVersion);
end;

procedure TEndWhileRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TEndWhileRunStepInfoCreator.Create();
end;

function TEndWhileRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;

{ TWhileRunStep }

function TWhileRunStep.DoGetDescription: string;
begin
    result := 'Loop condition value: ';
    if fConditionResult then
        result := result + 'TRUE'
    else
        result := result + 'FALSE';

    if not fConditionResult then
        result := result + ' - Loop ended. Jumping behind end of loop, line ' +
            IntToStr(fEndRelativeAddress + 1);

end;

{ TWhileRunStepCreator }

function TWhileRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TWhileRunStep.Create();
end;

{ TWhileRunStepTypeInfo }

constructor TWhileRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionWhile = '1.0.0';
    cStepTypeNameWhile = cActionNameWhile;
begin
    inherited Create(cStepTypeNameWhile, cStepTypeVersionWhile, aLibName, aLibVersion);
end;

procedure TWhileRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TWhileRunStepCreator.Create();
end;

{ TEndWhileRunStep }

function TEndWhileRunStep.DoGetDescription: string;
begin
    result := 'Jumping to start of loop at line ' + IntToStr(fBeginRelativeAddress + 1);
end;

{ TEndWhileRunStepCreator }

function TEndWhileRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TEndWhileRunStep.Create();
end;

{ TEndWhileRunStepTypeInfo }

constructor TEndWhileRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndWhile = '1.0.0';
    cStepTypeNameEndWhile = cActionNameEndWhile;
begin
    inherited Create(cStepTypeNameEndWhile, cStepTypeVersionEndWhile, aLibName, aLibVersion);
end;

procedure TEndWhileRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TEndWhileRunStepCreator.Create();
end;

{ TWhileMethodStepSetting }

constructor TMethodStepSetting_While.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);

    AddParam(TCustomLeafSetting.Create(cWhileMethOptionKeyCondition, TLanguageString.
        Read('Condition, e.g.: ((_$0Name <> "ABC") AND (_$c1 <= 1)) OR (_$c2 = 0)',
        'Bedingung, z.B.: ((_$0Name <> "ABC") AND (_$c1 <= 1)) OR (_$c2 = 0)'), true));
    SetValue('');
end;

function TMethodStepSetting_While.GetCondition: TCustomSetting;
begin
    result := self.Find(cWhileMethOptionKeyCondition)
end;

function TMethodStepSetting_While.GetOptionSummary: string;
begin
    result := 'While ' + self.Condition.Value;

end;

{ TWhileMethodStep }

procedure TWhileMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_While.Create(aOnAddEditFunctions));
end;

function TWhileMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TWhileRunStepInfo.Create();
end;

function TWhileMethodStep.GetMainSubOptionSetting: TMethodStepSetting_While;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_While;
end;

{ TWhileRunStepBuilderTypeInfo }

constructor TWhileRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionWhile = '1.0.0';
    cStepTypeNameWhile = cActionNameWhile;
begin
    inherited Create(cStepTypeNameWhile, cStepTypeVersionWhile, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TWhileRunStepBuilder, TWhileMethodStep));
end;

{ TWhileRunStepBuilder }

function TWhileRunStepBuilder.GetRunOptions(): TMethodStepSetting_While;
begin
    result := self.MStep.MainSubOptionSetting;
end;

function TWhileRunStepBuilder.DoIsMatchingControlFlow(const aRunStepBuilder
    : TControlFlowBlockRunStepBuilder): boolean;
begin
    result := aRunStepBuilder is TEndWhileRunStepBuilder;
end;

function TWhileRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TWhileMethodStep;
end;

function TWhileRunStepBuilder.GetMStep: TWhileMethodStep;
begin
    result := inherited MStep as TWhileMethodStep;
end;

function TWhileRunStepBuilder.GetParseField: TMethBuildEvaluableField;
begin
    result := self.RunOptions.Condition.ParseField as TMethBuildEvaluableField;
end;

procedure TWhileRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TWhileRunStep;
    xValue: TArg;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TWhileRunStep;

    xValue := self.RunOptions.Condition.EvaluateParseValue();
    xRunStep.ConditionResult := xValue.AsBool;

    xRunStep.EndRelativeAddress := fRelativeAddress;

end;

// ############### ENDWHILE ####################

{ TMethodStepSetting_EndWhile }

constructor TMethodStepSetting_EndWhile.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);
    AddParam(TCustomSetting_NoKeyOption.Create(TLanguageString.Read('While-Loop End',
        'While-Schleife Ende'), true));
end;

function TMethodStepSetting_EndWhile.GetOptionSummary: string;
begin
    result := 'End While';
end;

{ TEndWhileMethodStep }

procedure TEndWhileMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_EndWhile.Create(aOnAddEditFunctions));
end;

function TEndWhileMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TEndWhileRunStepInfo.Create();
end;

function TEndWhileMethodStep.GetCondition: TCustomSetting;
begin
    result := self.OptionsParam.Params[0];
end;

{ TEndWhileRunStepBuilder }

function TEndWhileRunStepBuilder.DoIsMatchingControlFlow(const aRunStepBuilder
    : TControlFlowBlockRunStepBuilder): boolean;
begin
    result := aRunStepBuilder is TWhileRunStepBuilder;
end;

function TEndWhileRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TEndWhileMethodStep;
end;

{ TEndWhileRunStepBuilderTypeInfo }

constructor TEndWhileRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndWhile = '1.0.0';
    cStepTypeNameEndWhile = cActionNameEndWhile;
begin
    inherited Create(cStepTypeNameEndWhile, cStepTypeVersionEndWhile, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TEndWhileRunStepBuilder, TEndWhileMethodStep));
end;

function TEndWhileRunStepBuilder.GetMStep: TEndWhileMethodStep;
begin
    result := inherited MStep as TEndWhileMethodStep;
end;

function TEndWhileRunStepBuilder.GetParseField: TMethBuildEvaluableField;
begin
    result := self.MStep.Condition.ParseField as TMethBuildEvaluableField;
end;

procedure TEndWhileRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TEndWhileRunStep;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TEndWhileRunStep;

    xRunStep.BeginRelativeAddress := fRelativeAddress;

end;


end.
