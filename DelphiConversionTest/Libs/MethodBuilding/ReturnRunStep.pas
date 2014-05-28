{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  25.06.13 wl                                      TN6178   Initial Revision
  12.07.13 ts                                      TN6200   new Icons
  10.09.13 wl                                      TN6187   Args: Mehrere Werte können jetzt übergeben werden
  ----------------------------------------------------------------------------------------------------------- }

unit ReturnRunStep;


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
    MethodTypes,
    CustomSetting,
    VariableCompositeSetting,
    Streamable,
    ParserIdentDataType,
    ControlFlowRunStepBuilder;

const
    cActionNameReturn = 'RETURN';

type
    TReturnRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
        function GetIconIndex: integer; override;
    public
        constructor Create(); override;
    end;

    TReturnRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TReturnRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TMethodStepSetting_Return = class(TMethodStepCompositeSetting)
    private const
        cKeyArgs = 'RETURNVALUES';
    protected
        function GetOptionSummary: string; override;
        function GetArgs: TVariableCompositeSetting;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
        property Args: TVariableCompositeSetting read GetArgs;
    end;

    TReturnMethodStep = class(TMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_Return;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        property MainSubOptionSetting: TMethodStepSetting_Return read GetMainSubOptionSetting;
    end;

    TReturnRunStepBuilder = class(TControlFlowReturnRunStepBuilder)
    protected
        function GetMStep(): TReturnMethodStep;
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function DoIsMatchingControlFlow(const aRunStepBuilder: TControlFlowBlockRunStepBuilder)
            : boolean; override;
        function GetRunOptions(): TMethodStepSetting_Return;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TReturnMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_Return read GetRunOptions;
    end;

    TReturnRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TReturnRunStep = class(TRunStep)
    private
        fArgs: TStreamableObjectList;
        fEndRelativeAddress: integer;
    protected
        function DoGetDescription(): string; override;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property Args: TStreamableObjectList read fArgs write fArgs;
        property EndRelativeAddress: integer read fEndRelativeAddress write fEndRelativeAddress;
    end;

    TReturnRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TReturnRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    CustomLeafSettings;

{ TReturnRunStepInfo }

constructor TReturnRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('RETURN', 'RETURN');
    fDescription := TLanguageString.Read('Jump out of method.', 'Springe aus der Methode.');
end;

function TReturnRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TReturnRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameReturn;
end;

function TReturnRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionReturn);
end;

{ TReturnRunStepInfoCreator }

function TReturnRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TReturnRunStepInfo.Create();
end;

{ TReturnRunStepInfoTypeInfo }

constructor TReturnRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionReturn = '1.0.0';
    cStepTypeNameReturn = cActionNameReturn;
begin
    inherited Create(cStepTypeNameReturn, cStepTypeVersionReturn, aLibName, aLibVersion);
end;

procedure TReturnRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TReturnRunStepInfoCreator.Create();
end;

{ TReturnMethodStepSetting }

constructor TMethodStepSetting_Return.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);

    AddParam(TVariableCompositeSetting.Create(cKeyArgs, TLanguageString.
        Read('Return values (Comma separated)', 'Rückgabewerte (durch Komma getrennt)'), true));

    SetValue('');
end;

function TMethodStepSetting_Return.GetArgs: TVariableCompositeSetting;
begin
    result := self.Find(cKeyArgs) as TVariableCompositeSetting;
end;

function TMethodStepSetting_Return.GetOptionSummary: string;
begin
    result := 'RETURN ' + self.Args.Value;

end;

{ TReturnMethodStep }

procedure TReturnMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_Return.Create(aOnAddEditFunctions));
end;

function TReturnMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TReturnRunStepInfo.Create();
end;

function TReturnMethodStep.GetMainSubOptionSetting: TMethodStepSetting_Return;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_Return;
end;

{ TReturnRunStepBuilderTypeInfo }

constructor TReturnRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionReturn = '1.0.0';
    cStepTypeNameReturn = cActionNameReturn;
begin
    inherited Create(cStepTypeNameReturn, cStepTypeVersionReturn, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TReturnRunStepBuilder, TReturnMethodStep));
end;

{ TReturnRunStepBuilder }

function TReturnRunStepBuilder.GetRunOptions(): TMethodStepSetting_Return;
begin
    result := self.MStep.MainSubOptionSetting;
end;

function TReturnRunStepBuilder.DoIsMatchingControlFlow(const aRunStepBuilder
    : TControlFlowBlockRunStepBuilder): boolean;
begin
    result := aRunStepBuilder is TRunStepBuilder;
end;

function TReturnRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TReturnMethodStep;
end;

function TReturnRunStepBuilder.GetMStep: TReturnMethodStep;
begin
    result := inherited MStep as TReturnMethodStep;
end;

procedure TReturnRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TReturnRunStep;
    xArray: TArray<TArg>;
    x: integer;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TReturnRunStep;

    xArray := self.RunOptions.Args.ParseValuesAsArgArray;
    for x := 0 to high(xArray) do
        xRunStep.Args.Add(xArray[x]);

    xRunStep.EndRelativeAddress := fRelativeAddress;
end;

{ TReturnRunStep }

constructor TReturnRunStep.Create;
begin
    inherited;
    fArgs := TStreamableObjectList.Create(false);
end;

destructor TReturnRunStep.Destroy;
begin
    FreeAndNil(fArgs);
    inherited;
end;

function TReturnRunStep.DoGetDescription: string;
var
    x: integer;
begin
    result := 'RETURN ';
    for x := 0 to fArgs.Count - 1 do
    begin
        if (x > 0) then
            result := result + ', ';
        result := result + TArgUtils.ItemToStr(fArgs[x] as TArg);
    end;
end;

{ TReturnRunStepCreator }

function TReturnRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TReturnRunStep.Create();
end;

{ TReturnRunStepTypeInfo }

constructor TReturnRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionReturn = '1.0.0';
    cStepTypeNameReturn = cActionNameReturn;
begin
    inherited Create(cStepTypeNameReturn, cStepTypeVersionReturn, aLibName, aLibVersion);
end;

procedure TReturnRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TReturnRunStepCreator.Create();
end;


end.
