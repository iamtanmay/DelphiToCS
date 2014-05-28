{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Types and structures used by Run step INIT
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  07.01.08 wl                               TN3972    initial version
  08.09.08 pk                               TN4215
  06.10.08 pk                               TN4258    Various changes
  10.11.08 pk DoCreateRunSteps              TN4279    new aRunSteps param
  20.02.09 wl  ..RunStepInfo.Create                TN4438   bestimmt fCaption und fDescription (GetResNo entfernt)
  20.02.09 wl  ..RunStep.DoReadData,DoWriteData    TN4438   entfernt
  20.02.09 wl  TMethodStepSetting_...              TN4438   aDescription direkt angegeben statt mit aResNo
  08.10.10 pk                                      TN5295   New DoGetCategoryName(s)
  14.12.11 wl                                      TN5765   uses geändert
  -------------------------------------------------------------------------------------------------- }

unit InitRunStep;


interface


uses
    Classes,
    RunStepInfo,
    RunStepInfoTypeInfo,
    MethodStep,
    RunStepBuilder,
    MethodStepDataFields,
    RunStepBuilderHelper,
    RunStep,
    RunStepTypeInfo;

const
    cActionNameInit = 'INIT';

type
    TInitRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
        function GetIconIndex: integer; override;
    public
        constructor Create(); override;
    end;

    TInitRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo(): TRunStepInfo; override;
    end;

    TInitRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        procedure DoCreateRunStepInfoCreator(); override;
        function DoGetSingleCategoryName: string; override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TInitMethodStep = class(TMethodStep)
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    end;

    TInitRunStepBuilder = class(TRunStepByMethodStepBuilder)
    protected
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
    end;

    TInitRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TInitRunStep = class(TRunStep)
    protected
        function DoGetDescription(): string; override;
    end;

    TInitRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TInitRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    MethodTypes,
    GeneralTypes;

{ TInitRunStepInfo }

constructor TInitRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('Initialize devices', 'Geräte initialisieren');
    fDescription := TLanguageString.Read('This action initializes all hardware modules.',
        'Diese Aktion führt eine Initialisierung aller Hardware-Module aus.');
end;

function TInitRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoSchedulingAndIterate;
end;

function TInitRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameInit;
end;

function TInitRunStepInfo.GetIconIndex: integer;
begin
    result := INT_IM_INDEX_ACTION_INIT;
end;

{ TInitRunStepInfoCreator }

function TInitRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TInitRunStepInfo.Create();
end;

{ TInitRunStepInfoTypeInfo }

constructor TInitRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionInit = '1.0.0';
    cStepTypeNameInit = cActionNameInit;
begin
    inherited Create(cStepTypeNameInit, cStepTypeVersionInit, aLibName, aLibVersion);
end;

procedure TInitRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TInitRunStepInfoCreator.Create();
end;

function TInitRunStepInfoTypeInfo.DoGetSingleCategoryName: string;
begin
    result := self.CategoryNameDevice;
end;

{ TInitMethodStep }

procedure TInitMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    // keine Options
end;

function TInitMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TInitRunStepInfo.Create();
end;

{ TInitRunStepBuilder }

function TInitRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TInitMethodStep;
end;

procedure TInitRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper);
begin
    CreateRunStepByNameAndAdd(aRunSteps);
end;

{ TInitRunStepBuilderTypeInfo }

constructor TInitRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const // 1 - 5
    cStepTypeVersionInit = '1.0.0';
    cStepTypeNameInit = cActionNameInit;
begin
    inherited Create(cStepTypeNameInit, cStepTypeVersionInit, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TInitRunStepBuilder, TInitMethodStep));
end;

{ TInitRunStep }

function TInitRunStep.DoGetDescription: string;
begin
    result := '';
end;

{ TInitRunStepCreator }

function TInitRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TInitRunStep.Create();
end;

{ TInitRunStepTypeInfo }

constructor TInitRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionInit = '1.0.0';
    cStepTypeNameInit = cActionNameInit;
begin
    inherited Create(cStepTypeNameInit, cStepTypeVersionInit, aLibName, aLibVersion);
end;

procedure TInitRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TInitRunStepCreator.Create();
end;


end.
