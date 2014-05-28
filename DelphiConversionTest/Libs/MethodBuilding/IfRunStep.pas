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

unit IfRunStep;


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
    cActionNameIf = 'IF';

type
    TIfRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
        function GetIsCodeStep: boolean; override;
        function GetIconIndex: integer; override;
    public
        constructor Create(); override;
    end;

    TIfRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TIfRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function GetIsHidden: boolean; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

const
    cActionNameEndIf = 'IFEND';

type
    TEndIfRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
        function GetIsCodeStep: boolean; override;
        function GetIconIndex: integer; override;
    public
        constructor Create(); override;
    end;

    TEndIfRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TEndIfRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function GetIsHidden: boolean; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TIfRunStep = class(TRunStep)
    protected
        fConditionResult: boolean;
        fEndRelativeAddress: integer;
        function DoGetDescription(): string; override;
    public
        property ConditionResult: boolean read fConditionResult write fConditionResult;
        property EndRelativeAddress: integer read fEndRelativeAddress write fEndRelativeAddress;
    end;

    TIfRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TIfRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TEndIfRunStep = class(TRunStep)
    protected
        fBeginRelativeAddress: integer;
        function DoGetDescription(): string; override;
    published
        property BeginRelativeAddress: integer read fBeginRelativeAddress write fBeginRelativeAddress;
    end;

    TEndIfRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TEndIfRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    // ############### IF ####################

    TMethodStepSetting_If = class(TMethodStepCompositeSetting)
    private const
        cIfMethOptionKeyCondition = 'CONDITION';
    protected
        function GetOptionSummary: string; override;
        function GetCondition: TCustomSetting;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
        property Condition: TCustomSetting read GetCondition;

    end;

    TIfMethodStep = class(TMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_If;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        property MainSubOptionSetting: TMethodStepSetting_If read GetMainSubOptionSetting;
    end;

    TIfRunStepBuilder = class(TControlFlowBlockBeginRunStepBuilder)
    protected
        function GetMStep(): TIfMethodStep;
        function GetParseField: TMethBuildEvaluableField;
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function DoIsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder)
            : boolean; override;
        function GetRunOptions(): TMethodStepSetting_If;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TIfMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_If read GetRunOptions;
    end;

    TIfRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TMethodStepSetting_EndIf = class(TMethodStepCompositeSetting)
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
    end;

    TEndIfMethodStep = class(TMethodStep)
    private
        function GetCondition: TCustomSetting;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        property Condition: TCustomSetting read GetCondition;
    end;

    TEndIfRunStepBuilder = class(TControlFlowBlockEndRunStepBuilder)
    protected
        function GetMStep(): TEndIfMethodStep;
        function GetParseField: TMethBuildEvaluableField;
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function DoIsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder)
            : boolean; override;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TEndIfMethodStep read GetMStep;
        property ParseField: TMethBuildEvaluableField read GetParseField;
    end;

    TEndIfRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
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

{ TIfRunStepInfo }

constructor TIfRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('If-condition Begin', 'If-Bedingung Beginn');
    fDescription := TLanguageString.Read('This step begins an If condition.',
        'Dieser Schritt beginnt eine If-Bedingung.');
end;

function TIfRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TIfRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameIf;
end;

function TIfRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionIf);
end;

function TIfRunStepInfo.GetIsCodeStep: boolean;
begin
    result := true;
end;

{ TIfRunStepInfoCreator }

function TIfRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TIfRunStepInfo.Create();
end;

{ TIfRunStepInfoTypeInfo }

constructor TIfRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionIf = '1.0.0';
    cStepTypeNameIf = cActionNameIf;
begin
    inherited Create(cStepTypeNameIf, cStepTypeVersionIf, aLibName, aLibVersion);
end;

procedure TIfRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TIfRunStepInfoCreator.Create();
end;

function TIfRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;

{ TEndIfRunStepInfo }

constructor TEndIfRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('If-condition End', 'If-Bedingung Ende');
    fDescription := TLanguageString.Read('This step ends an If-condition.',
        'Dieser Schritt beendet eine If-Bedingung.');
end;

function TEndIfRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TEndIfRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameEndIf;
end;

function TEndIfRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionIf);
end;

function TEndIfRunStepInfo.GetIsCodeStep: boolean;
begin
    result := true;
end;

{ TEndIfRunStepInfoCreator }

function TEndIfRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TEndIfRunStepInfo.Create();
end;

{ TEndIfRunStepInfoTypeInfo }

constructor TEndIfRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndIf = '1.0.0';
    cStepTypeNameEndIf = cActionNameEndIf;
begin
    inherited Create(cStepTypeNameEndIf, cStepTypeVersionEndIf, aLibName, aLibVersion);
end;

procedure TEndIfRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TEndIfRunStepInfoCreator.Create();
end;

function TEndIfRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;

{ TIfRunStep }

function TIfRunStep.DoGetDescription: string;
begin
    result := 'If-condition value: ';
    if fConditionResult then
        result := result + 'TRUE'
    else
        result := result + 'FALSE';

    if not fConditionResult then
        result := result + ' - Jumping to end of If-condition, line ' + IntToStr(fEndRelativeAddress + 1);

end;

{ TIfRunStepCreator }

function TIfRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TIfRunStep.Create();
end;

{ TIfRunStepTypeInfo }

constructor TIfRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionIf = '1.0.0';
    cStepTypeNameIf = cActionNameIf;
begin
    inherited Create(cStepTypeNameIf, cStepTypeVersionIf, aLibName, aLibVersion);
end;

procedure TIfRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TIfRunStepCreator.Create();
end;

{ TEndIfRunStep }

function TEndIfRunStep.DoGetDescription: string;
begin
    result := '';
end;

{ TEndIfRunStepCreator }

function TEndIfRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TEndIfRunStep.Create();
end;

{ TEndIfRunStepTypeInfo }

constructor TEndIfRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndIf = '1.0.0';
    cStepTypeNameEndIf = cActionNameEndIf;
begin
    inherited Create(cStepTypeNameEndIf, cStepTypeVersionEndIf, aLibName, aLibVersion);
end;

procedure TEndIfRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TEndIfRunStepCreator.Create();
end;

{ TIfMethodStepSetting }

constructor TMethodStepSetting_If.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);

    AddParam(TCustomLeafSetting.Create(cIfMethOptionKeyCondition, TLanguageString.
        Read('Condition, e.g.: ((_$0Name <> "ABC") AND (_$c1 <= 1)) OR (_$c2 = 0)',
        'Bedingung, z.B.: ((_$0Name <> "ABC") AND (_$c1 <= 1)) OR (_$c2 = 0)'), true));
    SetValue('');
end;

function TMethodStepSetting_If.GetCondition: TCustomSetting;
begin
    result := self.Find(cIfMethOptionKeyCondition)
end;

function TMethodStepSetting_If.GetOptionSummary: string;
begin
    result := 'IF ' + self.Condition.Value;

end;

{ TIfMethodStep }

procedure TIfMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_If.Create(aOnAddEditFunctions));
end;

function TIfMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TIfRunStepInfo.Create();
end;

function TIfMethodStep.GetMainSubOptionSetting: TMethodStepSetting_If;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_If;
end;

{ TIfRunStepBuilderTypeInfo }

constructor TIfRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionIf = '1.0.0';
    cStepTypeNameIf = cActionNameIf;
begin
    inherited Create(cStepTypeNameIf, cStepTypeVersionIf, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TIfRunStepBuilder, TIfMethodStep));
end;

{ TIfRunStepBuilder }

function TIfRunStepBuilder.GetRunOptions(): TMethodStepSetting_If;
begin
    result := self.MStep.MainSubOptionSetting;
end;

function TIfRunStepBuilder.DoIsMatchingControlFlow(const aRunStepBuilder
    : TControlFlowBlockRunStepBuilder): boolean;
begin
    result := aRunStepBuilder is TEndIfRunStepBuilder;
end;

function TIfRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TIfMethodStep;
end;

function TIfRunStepBuilder.GetMStep: TIfMethodStep;
begin
    result := inherited MStep as TIfMethodStep;
end;

function TIfRunStepBuilder.GetParseField: TMethBuildEvaluableField;
begin
    result := self.RunOptions.Condition.ParseField as TMethBuildEvaluableField;
end;

procedure TIfRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TIfRunStep;
    xValue: TArg;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TIfRunStep;

    xValue := self.RunOptions.Condition.EvaluateParseValue();
    xRunStep.ConditionResult := xValue.AsBool;

    xRunStep.EndRelativeAddress := fRelativeAddress;
end;

// ############### ENDIF ####################

{ TMethodStepSetting_EndIf }

constructor TMethodStepSetting_EndIf.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);

    AddParam(TCustomSetting_NoKeyOption.Create(TLanguageString.Read('If-Condition End',
        'If-Bedingung Ende'), true));
end;

function TMethodStepSetting_EndIf.GetOptionSummary: string;
begin
    result := 'End If';
end;

{ TEndIfMethodStep }

procedure TEndIfMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_EndIf.Create(aOnAddEditFunctions));
end;

function TEndIfMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TEndIfRunStepInfo.Create();
end;

function TEndIfMethodStep.GetCondition: TCustomSetting;
begin
    result := self.OptionsParam.Params[0];
end;

function TEndIfRunStepBuilder.DoIsMatchingControlFlow(const aRunStepBuilder
    : TControlFlowBlockRunStepBuilder): boolean;
begin
    result := aRunStepBuilder is TIfRunStepBuilder;
end;

function TEndIfRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TEndIfMethodStep;
end;

{ TEndIfRunStepBuilderTypeInfo }

constructor TEndIfRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndIf = '1.0.0';
    cStepTypeNameEndIf = cActionNameEndIf;
begin
    inherited Create(cStepTypeNameEndIf, cStepTypeVersionEndIf, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TEndIfRunStepBuilder, TEndIfMethodStep));
end;

function TEndIfRunStepBuilder.GetMStep: TEndIfMethodStep;
begin
    result := inherited MStep as TEndIfMethodStep;
end;

function TEndIfRunStepBuilder.GetParseField: TMethBuildEvaluableField;
begin
    result := self.MStep.Condition.ParseField as TMethBuildEvaluableField;
end;

procedure TEndIfRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TEndIfRunStep;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TEndIfRunStep;

    xRunStep.BeginRelativeAddress := fRelativeAddress;

end;


end.
