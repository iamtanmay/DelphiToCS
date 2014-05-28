{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Types and structures used by Run step STORE
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  07.01.08 wl                               TN3972    initial version
  20.05.08 wl  TMethodStepSetting_..GetOptionSummary TN4113    new
  08.09.08 pk                               TN4215
  06.10.08 pk                               TN4258    Various changes
  10.11.08 pk DoCreateRunSteps              TN4279    new aRunSteps param
  20.02.09 wl  ..RunStepInfo.Create                TN4438   bestimmt fCaption und fDescription (GetResNo entfernt)
  20.02.09 wl  ..RunStep.DoReadData,DoWriteData    TN4438   entfernt
  20.02.09 wl  TMethodStepSetting_...              TN4438   aDescription direkt angegeben statt mit aResNo
  08.05.09 wl  GetOptionSummary                    TN4555   Zuweisungs-Schreibweise für Rückgabeparameter: ... := ...
  07.05.10 pk  TParamStoreRunStep                  TN5092   New ArrayIndex property
  21.09.10 pk                                      TN5089   Various changes to MethodEditor for displaying indented actions
  08.10.10 pk                                      TN5295   New DoGetCategoryName(s)
  29.06.11 wl  ArrayIndex                          TN5618   entfernt (ist nicht mehr notwendig)
  01.03.12 wl                                      TN5822   TArg statt TAttrValue
  30.03.12 wl                                      TN5852   hat eigenes Icon
  05.07.12 wl  DoCreateRunSteps                    TN5917   EvaluateParseValue: Methodenparameter geändert
  -------------------------------------------------------------------------------------------------- }

unit ParamStoreRunStepBuilder;


interface


uses
    Classes,
    RunStepInfo,
    RunStepInfoTypeInfo,
    MethodStep,
    RunStepBuilder,
    CustomSetting,
    RunStepBuilderHelper,
    RunStep,
    RunStepTypeInfo,
    ParserIdentDataType;

const
    cActionNameParamStore = 'STORE';

type
    TParamStoreRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
        function GetIsCodeStep(): boolean; override;
        function GetIconIndex(): integer; override;
    public
        constructor Create(); override;
    end;

    TParamStoreRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo(): TRunStepInfo; override;
    end;

    TParamStoreRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function DoGetSingleCategoryName: string; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TMethodStepSetting_ParamStore = class(TMethodStepCompositeSetting)
    private const
        cMethOptionKeyPrefix = 'STORE';
        cMethOptionKeyStoreKey = cMethOptionKeyPrefix + 'KEY';
        cMethOptionKeyStoreValue = cMethOptionKeyPrefix + 'VALUE';
    protected
        function GetOptionSummary: string; override;
        function GetStoreKey: TCustomSetting;
        function GetStoreValue: TCustomSetting;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
        property StoreKey: TCustomSetting read GetStoreKey;
        property StoreValue: TCustomSetting read GetStoreValue;
    end;

    TParamStoreMethodStep = class(TMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_ParamStore;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        property MainSubOptionSetting: TMethodStepSetting_ParamStore read GetMainSubOptionSetting;
    end;

    TParamStoreRunStepBuilder = class(TRunStepByMethodStepBuilder)
    private
        function GetMStep: TParamStoreMethodStep;
        function GetRunOptions(): TMethodStepSetting_ParamStore;
    protected
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TParamStoreMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_ParamStore read GetRunOptions;
    end;

    TParamStoreRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TParamStoreRunStep = class(TRunStep)
    protected
        fKey: string;
        fValue: TArg;
        function DoGetDescription(): string; override;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property Key: string read fKey write fKey;
        property Value: TArg read fValue write fValue;
    end;

    TParamStoreRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TParamStoreRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    SysUtils,
    MethodTypes,
    CustomLeafSettings,
    GeneralTypes,
    MethBuildField;

{ TParamStoreRunStepInfo }

constructor TParamStoreRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('Store variable', 'Variable speichern');
    fDescription := ''
end;

function TParamStoreRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoSchedulingAndIterate;
end;

function TParamStoreRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameParamStore;
end;

function TParamStoreRunStepInfo.GetIconIndex: integer;
begin
    EXIT(cImageIndexActionParamStore);
end;

function TParamStoreRunStepInfo.GetIsCodeStep: boolean;
begin
    result := true;
end;

{ TParamStoreRunStepInfoCreator }

function TParamStoreRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TParamStoreRunStepInfo.Create();
end;

{ TParamStoreRunStepInfoTypeInfo }

constructor TParamStoreRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionParamStore = '1.0.0';
    cStepTypeNameParamStore = cActionNameParamStore;
begin
    inherited Create(cStepTypeNameParamStore, cStepTypeVersionParamStore, aLibName, aLibVersion);
end;

procedure TParamStoreRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TParamStoreRunStepInfoCreator.Create();
end;

function TParamStoreRunStepInfoTypeInfo.DoGetSingleCategoryName: string;
begin
    result := self.CategoryNameControlFlow;
end;

{ TMethodStepSetting_ParamStore }

constructor TMethodStepSetting_ParamStore.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);

    AddParam(TCustomSetting_RunVarName.Create(cMethOptionKeyStoreKey, TLanguageString.Read('Variable name',
        'Variablenname'), aOnAddEditFunctions));
    AddParam(TCustomLeafSetting.Create(cMethOptionKeyStoreValue, TLanguageString.Read('Assigned value',
        'Zugewiesener Wert'), true));
    SetValue('');
end;

function TMethodStepSetting_ParamStore.GetStoreKey: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyStoreKey);
end;

function TMethodStepSetting_ParamStore.GetStoreValue: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyStoreValue);
end;

function TMethodStepSetting_ParamStore.GetOptionSummary: string;
begin
    result := self.StoreKey.Value + ' := ' + self.StoreValue.Value;
end;

{ TParamStoreMethodStep }

procedure TParamStoreMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_ParamStore.Create(aOnAddEditFunctions));
end;

function TParamStoreMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TParamStoreRunStepInfo.Create();
end;

function TParamStoreMethodStep.GetMainSubOptionSetting: TMethodStepSetting_ParamStore;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_ParamStore;
end;

{ TParamStoreRunStepBuilder }

function TParamStoreRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TParamStoreMethodStep;
end;

function TParamStoreRunStepBuilder.GetMStep: TParamStoreMethodStep;
begin
    result := inherited MStep as TParamStoreMethodStep;
end;

function TParamStoreRunStepBuilder.GetRunOptions(): TMethodStepSetting_ParamStore;
begin
    result := self.MStep.MainSubOptionSetting;
end;

procedure TParamStoreRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TParamStoreRunStep;
    xValue: TArg;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TParamStoreRunStep;

    xRunStep.Key := self.RunOptions.StoreKey.ParseValue;
    xValue := self.RunOptions.StoreValue.EvaluateParseValue();
    xRunStep.Value := xValue;
end;

{ TParamStoreRunStepBuilderTypeInfo }

constructor TParamStoreRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const // 1 - 5
    cStepTypeVersionParamStore = '1.0.0';
    cStepTypeNameParamStore = cActionNameParamStore;
begin
    inherited Create(cStepTypeNameParamStore, cStepTypeVersionParamStore, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TParamStoreRunStepBuilder, TParamStoreMethodStep));
end;

{ TParamStoreRunStep }

constructor TParamStoreRunStep.Create;
begin
    inherited;
    fValue := nil;
end;

destructor TParamStoreRunStep.Destroy;
begin

    FreeAndNil(fValue);
    inherited;
end;

function TParamStoreRunStep.DoGetDescription: string;
begin
    result := fKey;
    result := result + ' := ' + TParserIdentValueUtils.IdentValueToLogStr(fValue,
        TArrayArg.UndefinedBoundsIndex);
end;

{ TParamStoreRunStepCreator }

function TParamStoreRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TParamStoreRunStep.Create();
end;

{ TParamStoreRunStepTypeInfo }

constructor TParamStoreRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionParamStore = '1.0.0';
    cStepTypeNameParamStore = cActionNameParamStore;
begin
    inherited Create(cStepTypeNameParamStore, cStepTypeVersionParamStore, aLibName, aLibVersion);
end;

procedure TParamStoreRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TParamStoreRunStepCreator.Create();
end;


end.
