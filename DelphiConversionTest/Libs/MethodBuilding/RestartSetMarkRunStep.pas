{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  19.05.10 pk                                        TN5113    initial revision
  08.08.12 wl                                        TN5946   mit RestartSetMarkRunStepInfo und RestartSetMarkRunStepBuilder zusammengefasst
  ----------------------------------------------------------------------------------------------------------------------- }

unit RestartSetMarkRunStep;


interface


uses
    Classes,
    RunStepInfo,
    RunStepInfoTypeInfo,
    RunStep,
    RunStepTypeInfo,
    MethodStep,
    RunStepBuilder,
    CustomSetting,
    RunStepBuilderHelper;

const
    cActionNameRestartSetMark = 'RESTA';

type
    TRestartSetMarkRunStepInfo = class(TRunStepInfo)
    protected
        function GetDefaultName: string; override;
        function GetAdditionalOptions: TRunStepInfoOptions; override;
    public
        constructor Create(); override;
    end;

    TRestartSetMarkRunStepInfoCreator = class(TRunStepInfoCreator)
    protected
        function DoCreateRunStepInfo: TRunStepInfo; override;
    end;

    TRestartSetMarkRunStepInfoTypeInfo = class(TRunStepInfoTypeInfo)
    protected
        function GetIsHidden: boolean; override;
        procedure DoCreateRunStepInfoCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TRestartSetMarkRunStep = class(TRunStep)
    protected
        fCaption: string;
        function DoGetDescription(): string; override;
    published
        property Caption: string read fCaption write fCaption;
    end;

    TRestartSetMarkRunStepCreator = class(TRunStepCreator)
    protected
        function DoCreateRunStep(): TRunStep; override;
    end;

    TRestartSetMarkRunStepTypeInfo = class(TRunStepTypeInfo)
    protected
        procedure DoCreateRunStepCreator(); override;
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;

    TRestartSetMarkMethodStep = class(TMethodStep)
    private
        function GetCaption: TCustomSetting;
    protected
        procedure CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent); override;
        function CreateStepInfo(): TRunStepInfo; override;
    public
        property Caption: TCustomSetting read GetCaption;
    end;

    TRestartSetMarkRunStepBuilder = class(TRunStepByMethodStepBuilder)
    private
        function GetMStep: TRestartSetMarkMethodStep;
    protected
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); override;
    public
        function GetMethodStepClass(): TMethodStepClass; override;
        property MStep: TRestartSetMarkMethodStep read GetMStep;
    end;

    TRestartSetMarkRunStepBuilderTypeInfo = class(TRunStepBuilderTypeInfo)
    public
        constructor Create(const aLibName, aLibVersion: string);
    end;


implementation


uses
    GeneralTypes,
    CustomLeafSettings,
    MethodGUIParsing;

{ TRestartSetMarkRunStepInfo }

constructor TRestartSetMarkRunStepInfo.Create;
begin
    inherited Create;

    fCaption := TLanguageString.Read('Set a restart point', 'Einen Restartpunkt setzen');
    fDescription := TLanguageString.
        Read('This step will save the current progress so that a restart from the current point will be possible.',
        'Dieser Schritt speichert den Verlauf damit eine Fortsetzung des Laufs ab diesem Punkt zu ermöglichen.');
end;

function TRestartSetMarkRunStepInfo.GetAdditionalOptions: TRunStepInfoOptions;
begin
    result := rsoNone;
end;

function TRestartSetMarkRunStepInfo.GetDefaultName(): string;
begin
    result := cActionNameRestartSetMark;
end;

{ TRestartSetMarkRunStepInfoCreator }

function TRestartSetMarkRunStepInfoCreator.DoCreateRunStepInfo: TRunStepInfo;
begin
    result := TRestartSetMarkRunStepInfo.Create();
end;

{ TRestartSetMarkRunStepInfoTypeInfo }

constructor TRestartSetMarkRunStepInfoTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionRestartSetMark = '1.0.0';
    cStepTypeNameRestartSetMark = cActionNameRestartSetMark;
begin
    inherited Create(cStepTypeNameRestartSetMark, cStepTypeVersionRestartSetMark, aLibName, aLibVersion);
end;

procedure TRestartSetMarkRunStepInfoTypeInfo.DoCreateRunStepInfoCreator;
begin
    fRunStepInfoCreator := TRestartSetMarkRunStepInfoCreator.Create();
end;

function TRestartSetMarkRunStepInfoTypeInfo.GetIsHidden: boolean;
begin
    result := true;
end;

{ TRestartSetMarkRunStep }

function TRestartSetMarkRunStep.DoGetDescription: string;
begin
    result := fCaption;
end;

{ TRestartSetMarkRunStepCreator }

function TRestartSetMarkRunStepCreator.DoCreateRunStep: TRunStep;
begin
    result := TRestartSetMarkRunStep.Create();
end;

{ TRestartSetMarkRunStepTypeInfo }

constructor TRestartSetMarkRunStepTypeInfo.Create(const aLibName, aLibVersion: string);
const
    cStepTypeVersionRestartSetMark = '1.0.0';
    cStepTypeNameRestartSetMark = cActionNameRestartSetMark;
begin
    inherited Create(cStepTypeNameRestartSetMark, cStepTypeVersionRestartSetMark, aLibName, aLibVersion);
end;

procedure TRestartSetMarkRunStepTypeInfo.DoCreateRunStepCreator;
begin
    fRunStepCreator := TRestartSetMarkRunStepCreator.Create();
end;

{ TRestartSetMarkMethodStep }

procedure TRestartSetMarkMethodStep.CreateOptionsTreeSpecific(aOnAddEditFunctions: TNotifyEvent);
begin
    AddOptionViewSubParam(TCustomSetting_NoKeyOption.Create(TLanguageString.Read('Description',
        'Beschreibung'), true));
end;

function TRestartSetMarkMethodStep.CreateStepInfo: TRunStepInfo;
begin
    result := TRestartSetMarkRunStepInfo.Create();
end;

function TRestartSetMarkMethodStep.GetCaption: TCustomSetting;
begin
    result := self.OptionsParam.Params[0];
end;

{ TRestartSetMarkRunStepBuilder }

function TRestartSetMarkRunStepBuilder.GetMethodStepClass(): TMethodStepClass;
begin
    result := TRestartSetMarkMethodStep;
end;

function TRestartSetMarkRunStepBuilder.GetMStep: TRestartSetMarkMethodStep;
begin
    result := inherited MStep as TRestartSetMarkMethodStep;
end;

procedure TRestartSetMarkRunStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
var
    xGeneralRunStep: TRunStep;
    xRunStep: TRestartSetMarkRunStep;
begin
    xGeneralRunStep := CreateRunStepByNameAndAdd(aRunSteps);
    xRunStep := xGeneralRunStep as TRestartSetMarkRunStep;

    xRunStep.Caption := self.MStep.Caption.ParseValue;
end;

{ TRestartSetMarkRunStepBuilderTypeInfo }

constructor TRestartSetMarkRunStepBuilderTypeInfo.Create(const aLibName, aLibVersion: string);
const // 1 - 5
    cStepTypeVersionRestartSetMark = '1.0.0';
    cStepTypeNameRestartSetMark = cActionNameRestartSetMark;
begin
    inherited Create(cStepTypeNameRestartSetMark, cStepTypeVersionRestartSetMark, aLibName, aLibVersion,
        TRunStepBuilderMetaClassCreator.Create(TRestartSetMarkRunStepBuilder, TRestartSetMarkMethodStep));
end;


end.
