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
  08.10.10 pk                                      TN5295   New DoGetCategoryName(s)
  02.03.12 wl                                      TN5822   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit VariableSetLengthRunStep;


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
    RunStepTypeInfo;

const
    cActionNameVariableSetLength = 'VSLEN';

type
    TVariableSetLengthRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
    public
        constructor Create(); override;
    end;

    TVariableSetLengthRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo(): TRunStepInfo; override;
    end;

    TVariableSetLengthRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function DoGetSingleCategoryName: string; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TMethodStepSetting_VariableSetLength = class(TMethodStepCompositeSetting)
    private const
        cMethOptionKeyPrefix = 'VSLEN';
        cMethOptionKeyVariableName = cMethOptionKeyPrefix + 'NAME';
        cMethOptionKeyVariableLength = cMethOptionKeyPrefix + 'LENGTH';
    protected
        function GetOptionSummary: string; override;
        function GetVariableName: TCustomSetting;
        function GetVariableLength: TCustomSetting;
    public
        constructor Create(aOnAddEditFunctions: TNotifyEvent);
        property VariableName: TCustomSetting read GetVariableName;
        property VariableLength: TCustomSetting read GetVariableLength;
    end;

    TVariableSetLengthMethodStep = class(TMethodStep)
    private
        function GetMainSubOptionSetting: TMethodStepSetting_VariableSetLength;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        property MainSubOptionSetting: TMethodStepSetting_VariableSetLength read GetMainSubOptionSetting;
    end;

    TVariableSetLengthRunStepBuilder = class(TRunStepByMethodStepBuilder)
    private
        function GetMStep: TVariableSetLengthMethodStep;
        function GetRunOptions(): TMethodStepSetting_VariableSetLength;
    protected
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TVariableSetLengthMethodStep read GetMStep;
        property RunOptions: TMethodStepSetting_VariableSetLength read GetRunOptions;
    end;

    TVariableSetLengthRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TVariableSetLengthRunStep = class(TRunStep)
    protected
        fVariableName: string;
        fVariableLength: integer;
        function DoGetDescription(): string; override;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property VariableName: string read fVariableName write fVariableName;
        property VariableLength: integer read fVariableLength write fVariableLength;
    end;

    TVariableSetLengthRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TVariableSetLengthRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    SysUtils,
    CustomLeafSettings,
    GeneralTypes;

{ TVariableSetLengthRunStepInfo }

constructor TVariableSetLengthRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('Set the length of an array variable',
        'Länge einer Arrayvariable setzen');
    fDescription := ''
end;

function TVariableSetLengthRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoSchedulingAndIterate;
end;

function TVariableSetLengthRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameVariableSetLength;
end;

{ TVariableSetLengthRunStepInfoCreator }

function TVariableSetLengthRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TVariableSetLengthRunStepInfo.Create();
end;

{ TVariableSetLengthRunStepInfoTypeInfo }

constructor TVariableSetLengthRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionVariableSetLength = '1.0.0';
    cStepTypeNameVariableSetLength = cActionNameVariableSetLength;
begin
    inherited Create(cStepTypeNameVariableSetLength, cStepTypeVersionVariableSetLength, aLibName,
        aLibVersion);
end;

procedure TVariableSetLengthRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TVariableSetLengthRunStepInfoCreator.Create();
end;

function TVariableSetLengthRunStepInfoTypeInfo.DoGetSingleCategoryName: string;
begin
    result := self.CategoryNameControlFlow;
end;

{ TMethodStepSetting_VariableSetLength }

constructor TMethodStepSetting_VariableSetLength.Create(aOnAddEditFunctions: TNotifyEvent);
begin
    inherited Create('', '', false);

    AddParam(TCustomSetting_RunVarName.Create(cMethOptionKeyVariableName,
        TLanguageString.Read('Variable name', 'Variablenname'), aOnAddEditFunctions));
    AddParam(TCustomLeafSetting.Create(cMethOptionKeyVariableLength, TLanguageString.Read('Variable Length',
        'Variablelänge'), true));
    SetValue('');
end;

function TMethodStepSetting_VariableSetLength.GetVariableName: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyVariableName);
end;

function TMethodStepSetting_VariableSetLength.GetVariableLength: TCustomSetting;
begin
    result := self.Find(cMethOptionKeyVariableLength);
end;

function TMethodStepSetting_VariableSetLength.GetOptionSummary: string;
begin
    result := TTypeSafeFormat.Format('Set Length of {0} to {1}',
        [self.VariableName.Value, self.VariableLength.Value]);
end;

{ TVariableSetLengthMethodStep }

procedure TVariableSetLengthMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TMethodStepSetting_VariableSetLength.Create(aOnAddEditFunctions));
end;

function TVariableSetLengthMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TVariableSetLengthRunStepInfo.Create();
end;

function TVariableSetLengthMethodStep.GetMainSubOptionSetting: TMethodStepSetting_VariableSetLength;
begin
    result := inherited MainSubOptionSetting as TMethodStepSetting_VariableSetLength;
end;

{ TVariableSetLengthRunStepBuilder }

function TVariableSetLengthRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TVariableSetLengthMethodStep;
end;

function TVariableSetLengthRunStepBuilder.GetMStep: TVariableSetLengthMethodStep;
begin
    result := inherited MStep as TVariableSetLengthMethodStep;
end;

function TVariableSetLengthRunStepBuilder.GetRunOptions(): TMethodStepSetting_VariableSetLength;
begin
    result := self.MStep.MainSubOptionSetting;
end;

procedure TVariableSetLengthRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TVariableSetLengthRunStep;
    xVariableLength: integer;
    xVariableLengthAsStr: string;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TVariableSetLengthRunStep;

    xRunStep.VariableName := self.RunOptions.VariableName.ParseValue;

    xRunStep.VariableLength := 0;

    // if it is defined, it must be an integer
    if self.RunOptions.VariableLength.Value <> '' then
    begin
        xVariableLengthAsStr := self.RunOptions.VariableLength.ParseValue;
        if not TryStrToInt(xVariableLengthAsStr, xVariableLength) or (xVariableLength < 0) then
            raise Exception.Create
                (TTypeSafeFormat.Format('The value {0} is not a valid length for the variable {1}',
                [xVariableLengthAsStr, xRunStep.VariableName]));
        xRunStep.VariableLength := xVariableLength;
    end;

end;

{ TVariableSetLengthRunStepBuilderTypeInfo }

constructor TVariableSetLengthRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const // 1 - 5
    cStepTypeVersionVariableSetLength = '1.0.0';
    cStepTypeNameVariableSetLength = cActionNameVariableSetLength;
begin
    inherited Create(cStepTypeNameVariableSetLength, cStepTypeVersionVariableSetLength, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TVariableSetLengthRunStepBuilder,
        TVariableSetLengthMethodStep));
end;

{ TVariableSetLengthRunStep }

constructor TVariableSetLengthRunStep.Create;
begin
    inherited;
end;

destructor TVariableSetLengthRunStep.Destroy;
begin
    inherited;
end;

function TVariableSetLengthRunStep.DoGetDescription: string;
begin
    result := TTypeSafeFormat.Format('Set Length of {0} to {1}', [fVariableName, fVariableLength]);
end;

{ TVariableSetLengthRunStepCreator }

function TVariableSetLengthRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TVariableSetLengthRunStep.Create();
end;

{ TVariableSetLengthRunStepTypeInfo }

constructor TVariableSetLengthRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionVariableSetLength = '1.0.0';
    cStepTypeNameVariableSetLength = cActionNameVariableSetLength;
begin
    inherited Create(cStepTypeNameVariableSetLength, cStepTypeVersionVariableSetLength, aLibName,
        aLibVersion);
end;

procedure TVariableSetLengthRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TVariableSetLengthRunStepCreator.Create();
end;


end.
