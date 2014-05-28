{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2012 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  20.09.12 wl                                      TN5982   Initial Revision
  21.09.12 wl                                      TN5982   Description geändert
  14.11.12 wl                                      TN6018   Neue Icons für IF, FOR, WHILE
  13.02.13 wl  TForRunStepBuilderLiveCounter       TN6075   aktuelle Werte werden Thread-bezogen gespeichert
  ----------------------------------------------------------------------------------------------------------- }

unit ForRunStep;


interface


uses
    Generics.Collections,
    Classes,
    RunStepInfo,
    RunStepInfoTypeInfo,
    RunStep,
    RunStepBuilderHelper,
    RunStepTypeInfo,
    MethBuildField,
    MethodStep,
    RunStepBuilder,
    MethodStepDataFields,
    CustomSetting,
    ControlFlowRunStepBuilder;

type
    TForRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
        function GetIsCodeStep: boolean; override;
        function GetIconIndex: integer; override;
    public const
        cActionNameFor = 'FOR';
    public
        constructor Create(); override;
    end;

    TForRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TForRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function GetIsHidden: boolean; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TEndForRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
        function GetIsCodeStep: boolean; override;
        function GetIconIndex: integer; override;
    public const
        cActionNameEndFor = 'FOREND';
    public
        constructor Create(); override;
    end;

    TEndForRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TEndForRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function GetIsHidden: boolean; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TForRunStep = class(TRunStep)
    protected
        fStoreKeyCounter: string;
        fCounterValue: integer;
        fEndValue: integer;
        fConditionResult: boolean;
        fEndRelativeAddress: integer;
        function DoGetDescription(): string; override;
    published
        property StoreKeyCounter: string read fStoreKeyCounter write fStoreKeyCounter;
        property CounterValue: integer read fCounterValue write fCounterValue;
        property EndValue: integer read fEndValue write fEndValue;
        property ConditionResult: boolean read fConditionResult write fConditionResult;
        property EndRelativeAddress: integer read fEndRelativeAddress write fEndRelativeAddress;
    end;

    TForRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TForRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TEndForRunStep = class(TRunStep)
    protected
        fBeginRelativeAddress: integer;
        function DoGetDescription(): string; override;
    published
        property BeginRelativeAddress: integer read fBeginRelativeAddress write fBeginRelativeAddress;
    end;

    TEndForRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TEndForRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TMethodStepSetting_For = class(TMethodStepCompositeSetting)
    private const
        cMethOptionKeyStoreKeyCounter = 'FORKEY';
        cMethOptionKeyStartValue = 'FORSTARTVAL';
        cMethOptionKeyEndValue = 'FORENDVAL';
        cMethOptionKeyIncrement = 'FORINCR';
    strict private
        function GetStoreKeyCounter: TCustomSetting;
        function GetStartValue: TCustomSetting;
        function GetEndValue: TCustomSetting;
        function GetIncrement: TCustomSetting;
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
        property StoreKeyCounter: TCustomSetting read GetStoreKeyCounter;
        property StartValue: TCustomSetting read GetStartValue;
        property EndValue: TCustomSetting read GetEndValue;
        property Increment: TCustomSetting read GetIncrement;
    end;

    TForMethodStep = class(TMethodStep)
    strict private
        function GetMainSubOptionSetting: TMethodStepSetting_For;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        constructor Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent); override;
        property MainSubOptionSetting: TMethodStepSetting_For read GetMainSubOptionSetting;
    end;

    TForRunStepBuilderLiveCounter = class
    strict private
        fThreadID: cardinal;
        fInitialized: boolean;
        fValue: integer;
    public
        constructor Create(const aThreadID: cardinal);
        property ThreadID: cardinal read fThreadID;
        property Initialized: boolean read fInitialized write fInitialized;
        property Value: integer read fValue write fValue;
    end;

    TForRunStepBuilder = class(TControlFlowBlockBeginRunStepBuilder)
    strict private
        fLiveCounters: TObjectList<TForRunStepBuilderLiveCounter>;
        function FindCounter(const aThreadID: cardinal): integer;
        function FindOrCreateCounter(const aThreadID: cardinal): TForRunStepBuilderLiveCounter;
    strict protected
        function GetMStep(): TForMethodStep;
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function DoIsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder)
            : boolean; override;
        function GetRunOptions(): TMethodStepSetting_For;
    public
        constructor Create(const aName: string); override;
        destructor Destroy(); override;

        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TForMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_For read GetRunOptions;
    end;

    TForRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TMethodStepSetting_EndFor = class(TMethodStepCompositeSetting)
    protected
        function GetOptionSummary: string; override;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
    end;

    TEndForMethodStep = class(TMethodStep)
    private
        function GetCondition: TCustomSetting;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        property Condition: TCustomSetting read GetCondition;
    end;

    TEndForRunStepBuilder = class(TControlFlowBlockEndRunStepBuilder)
    protected
        function GetMStep(): TEndForMethodStep;
        function GetParseField: TMethBuildEvaluableField;
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function DoIsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder)
            : boolean; override;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TEndForMethodStep read GetMStep;
        property ParseField: TMethBuildEvaluableField read GetParseField;
    end;

    TEndForRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    MethodTypes,
    ParserIdentDataType,
    CustomLeafSettings;

{ TForRunStepInfo }

constructor TForRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('For-Loop Begin', 'For-Schleife Beginn');
    fDescription := TLanguageString.Read('This step begins a For-loop.',
        'Dieser Schritt beginnt eine For-Schleife.');
end;

function TForRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TForRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameFor;
end;

function TForRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionFor);
end;

function TForRunStepInfo.GetIsCodeStep: boolean;
begin
    result := true;
end;

{ TForRunStepInfoCreator }

function TForRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TForRunStepInfo.Create();
end;

{ TForRunStepInfoTypeInfo }

constructor TForRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionFor = '1.0.0';
    cStepTypeNameFor = TForRunStepInfo.cActionNameFor;
begin
    inherited Create(cStepTypeNameFor, cStepTypeVersionFor, aLibName, aLibVersion);
end;

procedure TForRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TForRunStepInfoCreator.Create();
end;

function TForRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;

{ TEndForRunStepInfo }

constructor TEndForRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('For-Loop End', 'For-Schleife Ende');
    fDescription := TLanguageString.Read('This step ends a For-loop.',
        'Dieser Schritt beendet eine For-Schleife.');
end;

function TEndForRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TEndForRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameEndFor;
end;

function TEndForRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionFor);
end;

function TEndForRunStepInfo.GetIsCodeStep: boolean;
begin
    result := true;
end;

{ TEndForRunStepInfoCreator }

function TEndForRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TEndForRunStepInfo.Create();
end;

{ TEndForRunStepInfoTypeInfo }

constructor TEndForRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndFor = '1.0.0';
    cStepTypeNameEndFor = TEndForRunStepInfo.cActionNameEndFor;
begin
    inherited Create(cStepTypeNameEndFor, cStepTypeVersionEndFor, aLibName, aLibVersion);
end;

procedure TEndForRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TEndForRunStepInfoCreator.Create();
end;

function TEndForRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;

{ TForRunStep }

function TForRunStep.DoGetDescription: string;
begin
    if not fConditionResult then
        EXIT('Loop ended. Jumping behind end of loop, line ' + IntToStr(fEndRelativeAddress + 1));

    if fStoreKeyCounter <> '' then
        result := fStoreKeyCounter + ' = '
    else
        result := 'Counter = ';

    result := result + IntToStr(fCounterValue) + ', End value = ' + IntToStr(fEndValue);
end;

{ TForRunStepCreator }

function TForRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TForRunStep.Create();
end;

{ TForRunStepTypeInfo }

constructor TForRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionFor = '1.0.0';
    cStepTypeNameFor = TForRunStepInfo.cActionNameFor;
begin
    inherited Create(cStepTypeNameFor, cStepTypeVersionFor, aLibName, aLibVersion);
end;

procedure TForRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TForRunStepCreator.Create();
end;

{ TEndForRunStep }

function TEndForRunStep.DoGetDescription: string;
begin
    result := 'Jumping to start of loop at line ' + IntToStr(fBeginRelativeAddress + 1);
end;

{ TEndForRunStepCreator }

function TEndForRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TEndForRunStep.Create();
end;

{ TEndForRunStepTypeInfo }

constructor TEndForRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndFor = '1.0.0';
    cStepTypeNameEndFor = TEndForRunStepInfo.cActionNameEndFor;
begin
    inherited Create(cStepTypeNameEndFor, cStepTypeVersionEndFor, aLibName, aLibVersion);
end;

procedure TEndForRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TEndForRunStepCreator.Create();
end;

{ TForMethodStepSetting }

constructor TMethodStepSetting_For.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);

    AddParam(TCustomLeafSetting.Create(cMethOptionKeyEndValue, TLanguageString.
        Read('End value (number of loops)', 'Endwert (Anzahl der Durchläufe)'), true));
    AddParam(TCustomLeafSetting.Create(cMethOptionKeyStartValue, TLanguageString.
        Read('Start value (Default: 1)', 'Startwert (Default: 1)'), true));
    AddParam(TCustomLeafSetting.Create(cMethOptionKeyIncrement, TLanguageString.Read('Increment (Default: 1)',
        'Inkrement (Default: 1)'), true));
    AddParam(TCustomSetting_RunVarName.Create(cMethOptionKeyStoreKeyCounter,
        TLanguageString.Read('Store counter as ...', 'Den Zähler speichern unter ...'), aOnAddEditFunctions));
    SetValue('');
end;

function TMethodStepSetting_For.GetStoreKeyCounter: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyStoreKeyCounter)
end;

function TMethodStepSetting_For.GetStartValue: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyStartValue)
end;

function TMethodStepSetting_For.GetEndValue: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyEndValue)
end;

function TMethodStepSetting_For.GetIncrement: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyIncrement)
end;

function TMethodStepSetting_For.GetOptionSummary: string;
var
    xStartValIs1, IncrementIs1: boolean;
begin
    if self.StoreKeyCounter.Value = '' then
        result := 'For '
    else
        result := 'For ' + self.StoreKeyCounter.Value + ' := ';

    xStartValIs1 := (self.StartValue.Value = '') or (self.StartValue.Value = '1');
    IncrementIs1 := (self.Increment.Value = '') or (self.Increment.Value = '1');

    if (xStartValIs1) then
        result := result + '1 to ' + self.EndValue.Value
    else
        result := result + self.StartValue.Value + ' to ' + self.EndValue.Value;

    if (not IncrementIs1) then
        result := result + ' (Increment: ' + self.Increment.Value + ')';

    if (IncrementIs1 and xStartValIs1) then
        result := result + ' (Repeat ' + self.EndValue.Value + ' times)';
end;

{ TForMethodStep }

constructor TForMethodStep.Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited;
end;

procedure TForMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_For.Create(aOnAddEditFunctions));
end;

function TForMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TForRunStepInfo.Create();
end;

function TForMethodStep.GetMainSubOptionSetting: TMethodStepSetting_For;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_For;
end;

{ TForRunStepBuilderTypeInfo }

constructor TForRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionFor = '1.0.0';
    cStepTypeNameFor = TForRunStepInfo.cActionNameFor;
begin
    inherited Create(cStepTypeNameFor, cStepTypeVersionFor, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TForRunStepBuilder, TForMethodStep));
end;

{ TForRunStepBuilderLiveCounter }

constructor TForRunStepBuilderLiveCounter.Create(const aThreadID: cardinal);
begin
    inherited Create;
    fThreadID := aThreadID;
    fInitialized := false;
    fValue := 0;
end;

{ TForRunStepBuilder }

function TForRunStepBuilder.GetRunOptions(): TMethodStepSetting_For;
begin
    result := self.MStep.MainSubOptionSetting;
end;

function TForRunStepBuilder.DoIsMatchingControlFlow(const aRunStepBuilder
    : TControlFlowBlockRunStepBuilder): boolean;
begin
    result := aRunStepBuilder is TEndForRunStepBuilder;
end;

function TForRunStepBuilder.FindCounter(const aThreadID: cardinal): integer;
var
    x: integer;
begin
    for x := 0 to fLiveCounters.Count - 1 do
    begin
        if (aThreadID = fLiveCounters[x].ThreadID) then
            EXIT(x);
    end;
    EXIT(-1);
end;

function TForRunStepBuilder.FindOrCreateCounter(const aThreadID: cardinal): TForRunStepBuilderLiveCounter;
var
    xIndex: integer;
begin
    xIndex := FindCounter(aThreadID);
    if (xIndex = -1) then
    begin
        result := TForRunStepBuilderLiveCounter.Create(aThreadID);
        fLiveCounters.Add(result);
        EXIT;
    end;
    EXIT(fLiveCounters[xIndex]);
end;

function TForRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TForMethodStep;
end;

function TForRunStepBuilder.GetMStep: TForMethodStep;
begin
    result := inherited MStep as TForMethodStep;
end;

constructor TForRunStepBuilder.Create(const aName: string);
begin
    inherited;
    fLiveCounters := TObjectList<TForRunStepBuilderLiveCounter>.Create;
end;

destructor TForRunStepBuilder.Destroy;
begin
    FreeAndNil(fLiveCounters);
    inherited;
end;

procedure TForRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TForRunStep;
    xIncrement, xEndValue: integer;
    xContinueLoop: boolean;
    xThreadID: cardinal;
    xCounter: TForRunStepBuilderLiveCounter;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TForRunStep;

    xIncrement := StrToIntDef(self.RunOptions.Increment.ParseValue, 1);
    if (xIncrement = 0) then
        raise Exception.Create('For-Loop: Increment 0 not allowed');

    xThreadID := aHelper.GetCurrentThreadID();
    xCounter := self.FindOrCreateCounter(xThreadID);
    if not xCounter.Initialized then
    begin
        // Init Counter: Counter auf Startwert setzen
        xCounter.Initialized := true;
        xCounter.Value := StrToIntDef(self.RunOptions.StartValue.ParseValue, 1);
    end
    else
    begin
        // Counter um das Increment erhöhen
        xCounter.Value := xCounter.Value + xIncrement;
    end;

    try
        xEndValue := StrToInt(self.RunOptions.EndValue.ParseValue);
    except
        raise Exception.Create('For-Loop: End value is no integer value');
    end;

    // Endwert erreicht?
    if xIncrement > 0 then
        xContinueLoop := (xCounter.Value <= xEndValue)
    else // negatives Inkrement: Wir zählen runter bis zum Endwert
        xContinueLoop := (xCounter.Value >= xEndValue);

    // wenn Endwert erreicht, muss der Counter beim nächsten Mal wieder initialisiert werden
    if not xContinueLoop then
        xCounter.Initialized := false;

    xRunStep.ConditionResult := xContinueLoop;
    xRunStep.StoreKeyCounter := self.RunOptions.StoreKeyCounter.ParseValue;
    xRunStep.CounterValue := xCounter.Value;
    xRunStep.EndValue := xEndValue;
    xRunStep.EndRelativeAddress := fRelativeAddress;
end;

{ TMethodStepSetting_EndFor }

constructor TMethodStepSetting_EndFor.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);
    AddParam(TCustomSetting_NoKeyOption.Create(TLanguageString.Read('For-Loop End',
        'For-Schleife Ende'), true));
end;

function TMethodStepSetting_EndFor.GetOptionSummary: string;
begin
    result := 'End For';
end;

{ TEndForMethodStep }

procedure TEndForMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_EndFor.Create(aOnAddEditFunctions));
end;

function TEndForMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TEndForRunStepInfo.Create();
end;

function TEndForMethodStep.GetCondition: TCustomSetting;
begin
    result := self.OptionsParam.Params[0];
end;

{ TEndForRunStepBuilder }

function TEndForRunStepBuilder.DoIsMatchingControlFlow(const aRunStepBuilder
    : TControlFlowBlockRunStepBuilder): boolean;
begin
    result := aRunStepBuilder is TForRunStepBuilder;
end;

function TEndForRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TEndForMethodStep;
end;

{ TEndForRunStepBuilderTypeInfo }

constructor TEndForRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionEndFor = '1.0.0';
    cStepTypeNameEndFor = TEndForRunStepInfo.cActionNameEndFor;
begin
    inherited Create(cStepTypeNameEndFor, cStepTypeVersionEndFor, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TEndForRunStepBuilder, TEndForMethodStep));
end;

function TEndForRunStepBuilder.GetMStep: TEndForMethodStep;
begin
    result := inherited MStep as TEndForMethodStep;
end;

function TEndForRunStepBuilder.GetParseField: TMethBuildEvaluableField;
begin
    result := self.MStep.Condition.ParseField as TMethBuildEvaluableField;
end;

procedure TEndForRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TEndForRunStep;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TEndForRunStep;

    xRunStep.BeginRelativeAddress := fRelativeAddress;
end;


end.
