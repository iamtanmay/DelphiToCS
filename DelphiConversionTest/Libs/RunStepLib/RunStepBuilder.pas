{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  08.01.08 wl                               TN3972    initial version
  06.02.08 pk  SaveRunRecStep               TN3972    pass aWritePipOrTube to SaveRunRec
  07.02.08 wl  WriteToRunWithMsg            TN4009    Format von Seq in %d geändert
  14.04.08 wl  TRunStepBuilder              TN4060    does not inherit TModule any more
  14.04.08 wl                               TN4060    does not use ModuleTypeInfo any more
  17.04.08 pk                               TN3972    StrToField: also handle empty string
  20.06.08 pk                               TN4139    WB global object replaced by LayoutManager
  03.07.08 wl                                         TN4157
  01.08.08 pk  WriteToRun                   TN4194    Code for Checking if slot occupied removed. Caused problems if some rack moves were done in run/runst
  02.09.08 pk                               TN4215    various changes
  08.09.08 pk                               TN4215    Moved to MethodCompile: MultiStrToFieldRecurse, CreateOptionsBuildField,etc
  06.10.08 pk  fOptionsField                TN4258    removed
  10.11.08 pk  fRunSteps, fHelper           TN4279    removed
  10.11.08 pk DoCreateRunSteps              TN4279    new aRunSteps param
  09.12.08 pk fCode                         TN4279    removed
  19.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  31.08.09 pk                               TN4753    uses changed
  31.05.10 wl  fActionName                  TN5120   ersetz fComparableActionName & fOriginalName
  17.06.10 wl                               TN5150   uses geändert
  15.11.10 pk                               TN5340   Changes to prevent memory leak
  14.12.11 wl                               TN5765   uses geändert
  13.02.13 wl  TRunStepBuilder.Create       TN6075   jetzt virtual;
  -------------------------------------------------------------------------------------------------- }

unit RunStepBuilder;


interface


uses
    Classes,
    MethodStep,
    TypeInfo,
    RunStepInfo,
    MethodStepDataFields,
    MethBuildField,
    ParserWrapperOptimal,
    RunStepBuilderHelper,
    MethodTypes,
    RunStep;

type
    TRunStepBuilderClass = class of TRunStepBuilder;

    TRunStepBuilder = class
    protected
        fPrepared: boolean;
        fActionName: string;
        fLineIndex: integer;
        function GetMethodStep(): TMethodStep; virtual;

    public
        constructor Create(const aName: string); virtual;
        destructor Destroy; override;

        property LineIndex: integer read fLineIndex write fLineIndex;
        property MStep: TMethodStep read GetMethodStep;
        property ActionName: string read fActionName;
    end;

    TRunStepByMethodStepBuilder = class(TRunStepBuilder)
    private
        fMDataLink: TMethodStepDataLink;
        fOnAddEditFunctions: TNotifyEvent;
    protected
        fMethodStep: TMethodStep;
        function GetMethodStep(): TMethodStep; override;
        function GetMethodStepClass(): TMethodStepClass; virtual;
        function CreateRunStepByName(): TRunStep;
        function CreateRunStepByNameAndAdd(aRunSteps: TCompositeRunStep): TRunStep;
        procedure DoCreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper); virtual;
    public
        //
        constructor Create(const aName: string); override;
        destructor Destroy(); override;
        procedure Prepare;
        procedure CreateRunSteps(aRunSteps: TCompositeRunStep; aHelper: TRunStepBuilderHelper);

        property MethodStepClass: TMethodStepClass read GetMethodStepClass;
        property OnAddEditFunctions: TNotifyEvent read fOnAddEditFunctions write fOnAddEditFunctions;
        property MDataLink: TMethodStepDataLink read fMDataLink write fMDataLink;
    end;

    TRunStepBuilderCreator = class
    public
        function CreateMethodStep(aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent): TMethodStep; virtual; abstract;
        function CreateRunStepBuilder(const aActionName: string): TRunStepBuilder; virtual; abstract;
    end;

    TRunStepBuilderMetaClassCreator = class(TRunStepBuilderCreator)
    private
        fMethodStepClass: TMethodStepClass;
        fRunStepBuilderClass: TRunStepBuilderClass;
    public
        constructor Create(aRunStepBuilderClass: TRunStepBuilderClass; aMethodStepClass: TMethodStepClass);
        function CreateMethodStep(aActionName: string; aMDataLink: TMethodStepDataLink;
            aOnAddEditFunctions: TNotifyEvent): TMethodStep; override;
        function CreateRunStepBuilder(const aActionName: string): TRunStepBuilder; override;
    end;

    TRunStepBuilderTypeInfo = class(TTypeInfo)
    protected
        fRunStepBuilderCreator: TRunStepBuilderCreator;
    public
        constructor Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string;
            aRunStepBuilderCreator: TRunStepBuilderCreator);
        destructor Destroy(); override;
        property RunStepBuilderCreator: TRunStepBuilderCreator read fRunStepBuilderCreator;
    end;


implementation


uses
    SysUtils,
    MethodGUIParsing,
    parserToken,
    parserTree,
    LogManager,
    TipMapUtils,
    AppTypes,
    Rack,
    LiqHDataAdaptor,
    RunStepFactory;

{ TRunStepBuilder }

constructor TRunStepBuilder.Create(const aName: string);
begin
    inherited Create();

    fPrepared := false;
    fActionName := aName;
end;

destructor TRunStepBuilder.Destroy;
begin

    inherited;
end;

function TRunStepBuilder.GetMethodStep: TMethodStep;
begin
    result := nil;
end;

constructor TRunStepByMethodStepBuilder.Create(const aName: string);
begin
    inherited Create(aName);
    fMethodStep := nil;
end;

destructor TRunStepByMethodStepBuilder.Destroy();
begin
    FreeAndNil(fMethodStep);
    inherited;
end;

function TRunStepByMethodStepBuilder.GetMethodStep: TMethodStep;
begin
    result := fMethodStep;
end;

function TRunStepByMethodStepBuilder.GetMethodStepClass: TMethodStepClass;
begin
    result := nil; // möglich
end;

procedure TRunStepByMethodStepBuilder.Prepare;
begin
    // kein Inherited !!

    if fPrepared then
        EXIT;

    if Assigned(self.MethodStepClass) then
        fMethodStep := self.MethodStepClass.Create(self.ActionName, fMDataLink, fOnAddEditFunctions);
    fPrepared := true;
end;

function TRunStepByMethodStepBuilder.CreateRunStepByName(): TRunStep;
begin
    result := TRunStepFactory.CreateRunStepByName(fActionName);
end;

function TRunStepByMethodStepBuilder.CreateRunStepByNameAndAdd(aRunSteps: TCompositeRunStep): TRunStep;
begin
    result := CreateRunStepByName();
    aRunSteps.AddStep(result);
end;

procedure TRunStepByMethodStepBuilder.DoCreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
begin
end;

procedure TRunStepByMethodStepBuilder.CreateRunSteps(aRunSteps: TCompositeRunStep;
    aHelper: TRunStepBuilderHelper);
begin
    self.DoCreateRunSteps(aRunSteps, aHelper);
end;

constructor TRunStepBuilderMetaClassCreator.Create(aRunStepBuilderClass: TRunStepBuilderClass;
    aMethodStepClass: TMethodStepClass);
begin
    inherited Create();
    fRunStepBuilderClass := aRunStepBuilderClass;
    fMethodStepClass := aMethodStepClass;
end;

function TRunStepBuilderMetaClassCreator.CreateMethodStep(aActionName: string;
    aMDataLink: TMethodStepDataLink; aOnAddEditFunctions: TNotifyEvent): TMethodStep;
begin
    result := fMethodStepClass.Create(aActionName, aMDataLink, aOnAddEditFunctions);
end;

function TRunStepBuilderMetaClassCreator.CreateRunStepBuilder(const aActionName: string): TRunStepBuilder;
begin
    result := fRunStepBuilderClass.Create(aActionName);
end;

{ TRunStepBuilderTypeInfo }

constructor TRunStepBuilderTypeInfo.Create(const aTypeName, aTypeInfoVersion, aLibName, aLibVersion: string;
    aRunStepBuilderCreator: TRunStepBuilderCreator);
begin
    inherited Create(aTypeName, aTypeInfoVersion, aLibName, aLibVersion);
    fRunStepBuilderCreator := aRunStepBuilderCreator;
end;

destructor TRunStepBuilderTypeInfo.Destroy;
begin
    fRunStepBuilderCreator.Free;
    inherited;
end;


end.
