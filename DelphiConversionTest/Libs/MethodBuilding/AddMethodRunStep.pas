{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl), Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.12.08 pk                                        TN4279
  20.02.09 pk  TAddMethodRunStep                     TN4232   SubMethodName published
  20.02.09 wl  DoReadData,DoWriteData                TN4438   entfernt
  04.03.09 pk  TAddMethodRunStep                     TN4232   Params changed to TStreamableKeyValueList
  04.02.10 pk                                        TN4972   Changes for Restart
  07.05.10 pk  TAddMethodRunStep                     TN5092   Params is now a TKeyArgValueList
  01.03.12 wl                                        TN5822   uses geändert
  08.08.12 wl                                        TN5946   mit AddMethodRunStepInfo und AddMethodRunStepBuilder zusammengefasst
  09.08.12 wl                                        TN5946   von SubMethodRunStep abgeleitet
  14.12.12 wl                                        TN6054   neu: TCustomSetting_MethodName.GetMethodName
  21.03.13 wl  GetActionCaptionText                  TN6045   neu für BuildingBlock-Editor und Hint
  25.06.13 wl                                        TN6178   Parameter GroupID wird ausgeblendet
  25.06.13 wl                                        TN6178   Neuer Parameter 'Return variable name' (Mehrere Namen komma-getrennt bereits möglich)
  26.08.13 wl  GetSummaryFirstLine                   TN6236   (nutzloser) Zugriff auf MethodSettings entfernt
  10.09.13 wl                                        TN6187   "Return variable names"  statt nur name
  ----------------------------------------------------------------------------------------------------------------------- }

unit AddMethodRunStep;


interface


uses
    Classes,
    Streamable,
    RunStep,
    MethodTypes,
    CustomSetting,
    MethodStep,
    MethodStepDataFields,
    VariableCompositeSetting,
    RunStepTypeInfo,
    RunStepInfoTypeInfo,
    RunStepInfo,
    RunStepBuilder,
    SubMethodRunStep,
    RunStepBuilderHelper;

const
    cActionAddMethod = 'ADDM';

type
    TAddMethodActionOptions = record
        name: string;
        Params: string;
        GroupID: string;
        ReadMode: string;
    end;

    TAddMethodRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
        function GetIconIndex: integer; override;
    public
        constructor Create(); override;
    end;

    TAddMethodRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo(): TRunStepInfo; override;
    end;

    TAddMethodRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function DoGetSingleCategoryName(): string; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TMethodStepSetting_AddMethod = class(TMethodStepSetting_SubMethod)
    private const
        cMethOptionKeyPrefix = 'ADDM';
        cMethOptionKeyName = cMethOptionKeyPrefix + 'NAME';
        cMethOptionKeyParams = cMethOptionKeyPrefix + 'PARAMS';
        cMethOptionKeyGroupID = cMethOptionKeyPrefix + 'GROUPID';
        cMethOptionKeyReturnKeys = cMethOptionKeyPrefix + 'VAR';
    private
        function GetGroupID: TCustomSetting;
        function GetReturnKeys: TCustomSetting_RunVarNames;
        function GetMethParams: TCustomSetting_MethodParams;
        procedure DoOnGetEditFunctionParams(aSender: TObject; out oParams: TCustomSettingEditFunctionParams);
    protected
        function GetMethName: TCustomSetting; override;
        function GetOptionSummary: string; override;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent; out oRelatedItemParam: TCustomSetting);
        property MethParams: TCustomSetting_MethodParams read GetMethParams;
        property GroupID: TCustomSetting read GetGroupID;
        property ReturnKeys: TCustomSetting_RunVarNames read GetReturnKeys;
    end;

    TAddMethodMethodStep = class(TSubMethodMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_AddMethod;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
        function GetSummaryFirstLine(): string; override;
    public
        constructor Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent = nil); override;
        function GetActionCaptionText(): string; override;
        procedure SetFirstSubOptionValue(const aValue: string); override;
        property MainSubOptionSetting: TMethodStepSetting_AddMethod read GetMainSubOptionSetting;
    end;

    TAddMethodRunStepBuilder = class(TRunStepByMethodStepBuilder)
    protected
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
        function GetMStep: TAddMethodMethodStep;
        function GetRunOptions(): TMethodStepSetting_AddMethod;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TAddMethodMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_AddMethod read GetRunOptions;
    end;

    TAddMethodRunStepBuilderCreator = class(TRunStepBuilderCreator)
    public
        function CreateMethodStep(aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent): TMethodStep; override;
        function CreateRunStepBuilder(const aActionName: string): TRunStepBuilder; override;
    end;

    TAddMethodRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TAddMethodRunStep = class(TSubMethodRunStep)
    private
        fParams: TKeyArgValueList;
        fReturnKeys: TStreamableStringList;
        function GetSubMethodName: string;
        procedure SetSubMethodName(const aValue: string);
    protected
        function DoGetDescription: string; override;
    public
        constructor Create();
        destructor Destroy(); override;
    published
        property SubMethodName: string read GetSubMethodName write SetSubMethodName;
        property Params: TKeyArgValueList read fParams write fParams;
        property ReturnKeys: TStreamableStringList read fReturnKeys write fReturnKeys;
    end;

    TAddMethodRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TAddMethodRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    SysUtils,
    GeneralTypes,
    ParserIdentDataType,
    CustomLeafSettings,
    CustomEditFunctionParams;

{ TAddMethodRunStepInfo }

constructor TAddMethodRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('Call submethod', 'Untermethode aufrufen');
    fDescription := TLanguageString.Read('This action calls an existing submethod.',
        'Diese Aktion ruft eine existierende Untermethode auf.');
end;

function TAddMethodRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoSchedulingAndIterate;
end;

function TAddMethodRunStepInfo.GetDefaultName(): string;
begin
    result := cActionAddMethod;
end;

function TAddMethodRunStepInfo.GetIconIndex: integer;
begin
    result := INT_IM_INDEX_METHOD;
end;

{ TAddMethodRunStepInfoCreator }

function TAddMethodRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TAddMethodRunStepInfo.Create();
end;

{ TAddMethodRunStepInfoTypeInfo }

constructor TAddMethodRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionAddMethod = '1.0.0';
    cStepTypeNameAddMethod = cActionAddMethod;
begin
    inherited Create(cStepTypeNameAddMethod, cStepTypeVersionAddMethod, aLibName, aLibVersion);
end;

procedure TAddMethodRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TAddMethodRunStepInfoCreator.Create();
end;

function TAddMethodRunStepInfoTypeInfo.DoGetSingleCategoryName(): string;
begin
    result := self.CategoryNameControlFlow;
end;

{ TMethodStepSetting_AddMethod }

constructor TMethodStepSetting_AddMethod.Create(aOnAddEditFunctions: TNotifyEvent;
    out oRelatedItemParam: TCustomSetting);
var
    xMethodNameParam: TCustomSetting_MethodName;
begin
    inherited Create('', '', false);

    xMethodNameParam := TCustomSetting_MethodName.Create(cMethOptionKeyName,
        TLanguageString.Read('Submethod name', 'Name der Methode'), aOnAddEditFunctions);
    oRelatedItemParam := AddParam(xMethodNameParam);
    oRelatedItemParam.OnGetKeyValueText := nil; // Dieser parameter wird bereits in der 1.Zeile gezeigt
    AddParam(TCustomSetting_MethodParams.Create(cMethOptionKeyParams, TLanguageString.Read('Parameters',
        'Parameter'), aOnAddEditFunctions, DoOnGetEditFunctionParams, xMethodNameParam.GetMethodName));
    AddParam(TCustomSetting_RunVarNames.Create(cMethOptionKeyReturnKeys,
        TLanguageString.Read('Return variable names (Comma-separated)',
        'Namen der Rückgabe-Variablen (durch Komma getrennt)'), aOnAddEditFunctions));

    { GroupID wird ausgeblendet, weil es zur Zeit überhaupt nicht benutzt wird }
    // AddParam(TCustomSetting_GroupID.Create(cMethOptionKeyGroupID, TLanguageString.Read('Group ID',
    // 'Group ID'), aOnAddEditFunctions));

    SetValue('');
end;

procedure TMethodStepSetting_AddMethod.DoOnGetEditFunctionParams(aSender: TObject;
    out oParams: TCustomSettingEditFunctionParams);
begin
    oParams := TMethodParamsEditFunctionParams.Create(self.MethName, self.MethParams);
end;

function TMethodStepSetting_AddMethod.GetMethName: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyName);
end;

function TMethodStepSetting_AddMethod.GetMethParams: TCustomSetting_MethodParams;
begin
    result := self.Find(cMethOptionKeyParams) as TCustomSetting_MethodParams;
end;

function TMethodStepSetting_AddMethod.GetGroupID: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyGroupID);
end;

function TMethodStepSetting_AddMethod.GetReturnKeys: TCustomSetting_RunVarNames;
begin
    result := self.Find(cMethOptionKeyReturnKeys) as TCustomSetting_RunVarNames;
end;

function TMethodStepSetting_AddMethod.GetOptionSummary: string;
begin
    if (self.MethParams.Value = '') then
        result := 'No parameters'
    else
        result := self.MethParams.Value;

    if (self.ReturnKeys.Value <> '') then
        result := self.ReturnKeys.Value + ' := ' + result;
end;

{ TAddMethodMethodStep }

constructor TAddMethodMethodStep.Create(const aActionName: string; aMDataLink: TMethodStepDataLink;
    aOnAddEditFunctions: TNotifyEvent);
begin
    inherited;

end;

procedure TAddMethodMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
var
    xRelatedItemParam: TCustomSetting;
begin
    AddOptionViewSubParam(TMethodStepSetting_AddMethod.Create(aOnAddEditFunctions, xRelatedItemParam));
    self.RelatedItemParam := xRelatedItemParam;
end;

function TAddMethodMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TAddMethodRunStepInfo.Create();
end;

function TAddMethodMethodStep.GetActionCaptionText: string;
begin
    EXIT(GetMainSubOptionSetting.GetMethName.Value + ': ');
end;

function TAddMethodMethodStep.GetMainSubOptionSetting: TMethodStepSetting_AddMethod;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_AddMethod;
end;

function TAddMethodMethodStep.GetSummaryFirstLine: string;
begin
    result := GetMainSubOptionSetting.GetMethName.Value;
end;

procedure TAddMethodMethodStep.SetFirstSubOptionValue(const aValue: string);
begin
    GetMainSubOptionSetting.GetMethName.Value := aValue;
end;

{ TAddMethodRunStepBuilder }

function TAddMethodRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TAddMethodMethodStep;
end;

function TAddMethodRunStepBuilder.GetMStep: TAddMethodMethodStep;
begin
    result := inherited MStep as TAddMethodMethodStep;
end;

function TAddMethodRunStepBuilder.GetRunOptions(): TMethodStepSetting_AddMethod;
begin
    result := self.MStep.MainSubOptionSetting;
end;

procedure TAddMethodRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TAddMethodRunStep;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TAddMethodRunStep;

    xRunStep.SubMethodName := self.RunOptions.MethName.ParseValue;
    self.RunOptions.MethParams.EvaluateParseValue(xRunStep.Params);
    xRunStep.ReturnKeys.FromStringArray(self.RunOptions.ReturnKeys.ParseValuesAsStringArray);
end;

{ TAddMethodRunStepBuilderCreator }

function TAddMethodRunStepBuilderCreator.CreateMethodStep(aActionName: string;
    aMDataLink: TMethodStepDataLink; aOnAddEditFunctions: TNotifyEvent): TMethodStep;
begin
    result := TAddMethodMethodStep.Create(aActionName, aMDataLink, aOnAddEditFunctions);
end;

function TAddMethodRunStepBuilderCreator.CreateRunStepBuilder(const aActionName: string): TRunStepBuilder;
begin
    result := TAddMethodRunStepBuilder.Create(aActionName);
end;

{ TAddMethodRunStepBuilderTypeInfo }

constructor TAddMethodRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionAddMethod = '1.0.0';
    cStepTypeNameAddMethod = cActionAddMethod;
begin
    inherited Create(cStepTypeNameAddMethod, cStepTypeVersionAddMethod, aLibName, aLibVersion,
        TAddMethodRunStepBuilderCreator.Create());
end;

{ TAddMethodRunStep }

constructor TAddMethodRunStep.Create();
begin
    inherited Create(false, '');
    fParams := TKeyArgValueList.Create();
    fReturnKeys := TStreamableStringList.Create;
end;

destructor TAddMethodRunStep.Destroy();
begin
    FreeAndNil(fReturnKeys);
    FreeAndNil(fParams);
    inherited;
end;

function TAddMethodRunStep.DoGetDescription: string;
begin
    result := TTypeSafeFormat.Format('Method {0}, Params: {1}, Store: {2}',
        [self.SubMethodName, TArgUtils.ListToStr(fParams), fReturnKeys.ToString]);
end;

function TAddMethodRunStep.GetSubMethodName: string;
begin
    result := fName;
end;

procedure TAddMethodRunStep.SetSubMethodName(const aValue: string);
begin
    fName := aValue;
end;

{ TAddMethodRunStepCreator }

function TAddMethodRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TAddMethodRunStep.Create();
end;

{ TAddMethodRunStepTypeInfo }

constructor TAddMethodRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionAddMethod = '1.0.0';
    cStepTypeNameAddMethod = cActionAddMethod;
begin
    inherited Create(cStepTypeNameAddMethod, cStepTypeVersionAddMethod, aLibName, aLibVersion);
end;

procedure TAddMethodRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TAddMethodRunStepCreator.Create();
end;


end.
