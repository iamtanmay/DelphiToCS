{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  : TRunStep is an object that represents a step / record in the run
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  16.02.05 pk                               TN2315   New
  28.02.05 pk  TPipetteRunStep              TN2314.1 Implements IRunRecIterator
  29.03.05 pk  TRunStep                     TN2365   New property : Description
  06.04.05 pk  TRunStep                     TN2373   Implement IDataAction
  19.04.05 pk  CreateGroupRunStep           TN2390   Create Group by parsing Group Options using GUIParser
  24.06.05 wl  TMessageWithBeeperRunStep    TN2459   entfernt
  07.11.05 pk  TRunStep                     TN2737   New : RereadData
  08.11.05 wl  TDeviceRunStep               TN2745   fState ist vom typ TSwitchState
  25.03.06 pk                               TN2999   All unneeded runsteps removed
  10.04.06 pk  IDataAction                  TN3032   New state functions : IsStartNewRunState, etc.
  11.04.06 pk  IDataAction                  TN3032   New state functions removed again
  11.04.06 pk  GetRunState                  TN3032   first check isStartNewRunState before other states
  12.04.06 pk  IDataAction                  TN3042   New : GetDescription
  18.04.06 pk  GetColor                     TN3048   Show color
  18.04.06 pk  TDelayRunStep                TN3048   New runstep for delay with special color
  20.04.06 pk                               TN3054   TPipetteRunStep.GetDescription - try to return the descr of first action
  08.05.06 pk  TMultiTipRunStep             TN3087   was previously TPipetteRunStep
  08.05.06 pk  TPipetteRunStep              TN3087   inherits from TMultiTipRunStep
  08.05.06 pk  TXYMoveRunStep, TZPosMove..  TN3087   New
  08.05.06 pk  TGroupedRunStep              TN3081   TRunLoadRunStep and TGroupRunStep inherit from TGroupedRunStep
  08.05.06 pk  FlushSchedState              TN3081   use GroupID index to find runstep
  29.05.06 pk  FlushSchedState              TN3081   requires indextype as a parameter
  30.05.06 pk  TRunStartRunStep             TN3120   New
  30.05.06 pk  IDataAction                  TN3120   New : GetFailedState
  06.06.06 pk  FlushSchedData               TN3133   Do not iterate through substeps
  04.12.06 wl  TMultiTipSubRunStep.FlushRunStateDB  TN3445   LiqFlag wird jetzt richtig geschrieben
  24.07.07 wl  TWashProgRunStep             TN3792   new
  06.08.07 wl  TRunStep.RereadDataRaw       TN3811.3  .Dataset statt .GetDataset
  09.11.07 pk                               TN3922   Dataset changed to DataProvider
  13.11.07 wl  TPowderDetectionRunStep      TN3844   neu
  27.02.08 wl  TZTipMovementRunStep         TN4011   new
  24.04.08 wl  TPumpAspirate/PumpDispenseRunStep  TN4050   neu
  03.07.08 wl                               TN4157
  09.07.08 pk  FlushRunStateDB              TN4157   Tipindex - 1 changed to just Tipindex
  16.07.08 pk  SetTipLiqDetErrorType        TN4157   New
  08.09.08 pk                               TN4215   Massive Changes
  20.09.08 pk  TipIndex                     TN4215   removed
  22.09.08 pk  TMultiTipSubRunStep.Create   TN4215   Tip initialized to -1
  06.11.08 pk  TRunStepIterator             TN4280   code from TCompositeRunStepIterator
  17.02.09 pk  GetStepDescription           TN4232   New
  19.02.09 pk  DoReadData,DoWriteData       TN4438   nicht mehr virtual
  30.03.09 pk  TCompositeRunStep            TN4490   List is not TStreamableObjectList
  19.06.09 pk  TRunStepIterator             TN4620   New: AddStep, AddSteps
  25.08.09 pk  TPipetteRunStep              TN4745   removed
  27.08.09 pk                               TN4753   uses InterfacedNoRef removed
  12.09.09 wl                               TN4740   TipIntegerArray durch TIntArray ersetzt
  15.12.09 pk  TMultiTipRunStep             TN4943   New AllowedTips property
  04.02.10 pk                               TN4972   Changes for Restart
  07.06.10 pk  BookmarkCurrentCursorPos     TN5077   New
  26.10.10 pk  RestartAtStepAllowed         TN5297   New
  15.11.10 pk                               TN5340   Changes to prevent memory leak
  14.12.11 wl                                      TN5765   uses geändert
  02.02.11 wl  TMultiRackPosRunStep                TN5791   enthält jetzt ein Rack-Array
  09.08.12 wl  TGroupedRunStep                     TN5946   --> SubMethodRunStep
  14.08.13 wl  TRunStepListIterator                TN6218   Hirarchie vereinfacht
  14.08.13 wl  TRunStepListIterator                TN6218   neu: CurrentBatchIndex
  -------------------------------------------------------------------------------------------------- }

unit RunStep;


interface


uses
    Graphics,
    AppTypes,
    GeneralTypes,
    RunStepInfo,
    Streamable;

type
    TRunStepClass = class of TRunStep;

    TRunStep = class(TStreamable)
    private
        fRunStepInfo: TRunStepInfo;
        function GetStepName(): string;
    protected
        function DoGetDescription(): string; virtual;
        function GetDescription(): string; virtual;
        function GetStepDescription(): string; virtual;
        function GetColor(): TColor; virtual;
        function GetRestartAtStepAllowed(): boolean; virtual;
    public
        constructor Create(); override;
        destructor Destroy(); override;

        property StepName: string read GetStepName;
        property Description: string read GetDescription;
        property StepDescription: string read GetStepDescription;
        property RestartAtStepAllowed: boolean read GetRestartAtStepAllowed;
        property Color: TColor read GetColor;
    published
        property RunStepInfo: TRunStepInfo read fRunStepInfo write fRunStepInfo;
    end;

    TCompositeRunStep = class(TRunStep)
    protected
        fList: TStreamableObjectList;
        function GetStepAt(aIndex: integer): TRunStep; virtual;
        function GetCount(): integer; virtual;
    public
        constructor Create(aOwnsSteps: boolean); reintroduce; overload;
        constructor Create(); overload; override;
        destructor Destroy(); override;
        procedure Clear();
        procedure AddStep(const aStep: TRunStep); virtual;
        procedure AddSteps(const aSteps: TCompositeRunStep);
        procedure RemoveStep(const aStep: TRunStep);
        function IsEmpty(): boolean;
        property this[aIndex: integer]: TRunStep read GetStepAt; default;
        property Count: integer read GetCount;
    published
        property List: TStreamableObjectList read fList write fList;
    end;

    TRunStepList = class(TCompositeRunStep);

    TRunStepListIterator = class
    private
        fDataCursor: integer;
        fCurrentBatchIndex: integer;
        fCompositeRunStep: TCompositeRunStep;
        function GetCurrentStep: TRunStep;
    public
        constructor Create(aCompositeRunStep: TCompositeRunStep);

        procedure MoveFirst();
        procedure MoveNext();
        function IsEOF(): boolean;

        property CompositeRunStep: TCompositeRunStep read fCompositeRunStep;
        property DataCursor: integer read fDataCursor write fDataCursor;
        property CurrentBatchIndex: integer read fCurrentBatchIndex write fCurrentBatchIndex;
        property CurrentStep: TRunStep read GetCurrentStep;
    end;

    TRunStepListLinearIterator = class(TRunStepListIterator)
    private
        fBookmarkedCursorPos: integer;
    public
        procedure BookmarkCurrentCursorPos();
        property BookmarkedCursorPos: integer read fBookmarkedCursorPos;
    end;

    TMultiTipSubRunStep = class(TRunStep)
    protected
        fTip: integer;
        fAllowedTips: TIPMAP;
        fPipDeviceName: string;
    public
        constructor Create(); override;
    published
        property PipDeviceName: string read fPipDeviceName write fPipDeviceName;
        property Tip: integer read fTip write fTip;
        property AllowedTips: TIPMAP read fAllowedTips write fAllowedTips;
    end;

    TMultiTipRunStepClass = class of TMultiTipRunStep;

    TMultiTipRunStep = class(TCompositeRunStep)
    protected
        fUsedTips: TIPMAP;
        fAllowedTips: TIPMAP;
        fPipDeviceName: string;
        function UsedTipsToStr(): string;
    public
        constructor Create(aOwnsSteps: boolean);
    published
        property UsedTips: TIPMAP read fUsedTips write fUsedTips;
        property AllowedTips: TIPMAP read fAllowedTips write fAllowedTips;
        property PipDeviceName: string read fPipDeviceName write fPipDeviceName;
    end;

    TMultiRackPosSubRunStep = class(TMultiTipSubRunStep)
    protected
        fRackName: string;
        fPosition: integer;
    published
        property RackName: string read fRackName write fRackName;
        property Position: integer read fPosition write fPosition;
    end;

    TMultiRackPosRunStep = class(TMultiTipRunStep)
    protected
        fRackNames: TArray<string>;
        fPositions: TIntArray;
        function PositionsToStr(): string;
        function RackNamesToStr(): string;
    public
        procedure SetPositionAt(aIndex: integer; const aPos: integer);
        procedure SetRackNameAt(aIndex: integer; const aRackName: string);
    end;

    TXYMoveRunStep = class(TMultiTipRunStep);

    TZPosMoveRunStep = class(TMultiTipRunStep);

    TZTipMoveRunStep = class(TMultiTipRunStep);

    TPowderDetectionRunStep = class(TMultiTipRunStep);

    TPumpAspirateRunStep = class(TMultiTipRunStep);
    TPumpDispenseRunStep = class(TMultiTipRunStep);

    TBlockAddRunStep = class(TCompositeRunStep)
    protected
        function DoGetBlockDescription: string; virtual; abstract;
        function DoGetDescription: string; override;
    public
        constructor Create(); override;
    end;


implementation


uses
    SysUtils,
    TipMapUtils,
    ArrayUtils;

{ TRunStep }

constructor TRunStep.Create();
begin
    inherited Create;
    fRunStepInfo := nil;
end;

destructor TRunStep.Destroy();
begin
    FreeAndNil(fRunStepInfo);
    inherited;
end;

function TRunStep.DoGetDescription(): string;
begin
    result := '';
end;

function TRunStep.GetDescription(): string;
begin
    result := self.StepName + ': ' + DoGetDescription();
end;

function TRunStep.GetStepDescription(): string;
begin
    result := DoGetDescription();
end;

function TRunStep.GetColor(): TColor;
begin
    result := clNone;
end;

function TRunStep.GetStepName: string;
begin
    result := fRunStepInfo.DefaultName;
end;

function TRunStep.GetRestartAtStepAllowed: boolean;
begin
    result := true;
end;

{ TCompositeRunStep }

constructor TCompositeRunStep.Create(aOwnsSteps: boolean);
begin
    inherited Create();
    fList := TStreamableObjectList.Create(aOwnsSteps);
end;

constructor TCompositeRunStep.Create();
begin
    Create(false);
end;

destructor TCompositeRunStep.Destroy();
begin
    FreeAndNil(fList);
    inherited;
end;

procedure TCompositeRunStep.AddSteps(const aSteps: TCompositeRunStep);
var
    x: integer;
begin
    for x := 0 to aSteps.Count - 1 do
    begin
        self.AddStep(aSteps[x]);
    end;
end;

procedure TCompositeRunStep.Clear;
begin
    fList.Clear();
end;

procedure TCompositeRunStep.AddStep(const aStep: TRunStep);
begin
    fList.Add(aStep);
end;

procedure TCompositeRunStep.RemoveStep(const aStep: TRunStep);
begin
    fList.Remove(aStep);
end;

function TCompositeRunStep.GetCount(): integer;
begin
    result := fList.Count;
end;

function TCompositeRunStep.IsEmpty(): boolean;
begin
    result := self.Count = 0;
end;

function TCompositeRunStep.GetStepAt(aIndex: integer): TRunStep;
begin
    result := fList[aIndex] as TRunStep;
end;

{ TMultiTipRunStep }

constructor TMultiTipRunStep.Create(aOwnsSteps: boolean);
begin
    inherited Create(aOwnsSteps);
    fAllowedTips := TTipMapUtils.EmptyTipMap;
end;

function TMultiTipRunStep.UsedTipsToStr: string;
begin
    result := TTipmapUtils.TipmapToStr(fUsedTips);
end;

{ TRunStepListIterator }

constructor TRunStepListIterator.Create(aCompositeRunStep: TCompositeRunStep);
begin
    inherited Create();
    fCompositeRunStep := aCompositeRunStep;
    MoveFirst();
    fCurrentBatchIndex := 0;
end;

procedure TRunStepListIterator.MoveFirst();
begin
    fDataCursor := 0;
end;

procedure TRunStepListIterator.MoveNext();
begin
    Inc(fDataCursor);
end;

function TRunStepListIterator.IsEOF(): boolean;
begin
    result := fDataCursor >= fCompositeRunStep.Count;
end;

function TRunStepListIterator.GetCurrentStep: TRunStep;
begin
    EXIT(fCompositeRunStep[fDataCursor]);
end;

{ TMultiRackPosRunStep }

function TMultiRackPosRunStep.PositionsToStr: string;
begin
    result := TArrayUtils.ArrayToBracketText(fPositions);
end;

function TMultiRackPosRunStep.RackNamesToStr: string;
begin
    result := TArrayUtils.ArrayToBracketText(fRackNames);
end;

procedure TMultiRackPosRunStep.SetPositionAt(aIndex: integer; const aPos: integer);
begin
    if ( high(fPositions) < aIndex) then
    begin
        SetLength(fPositions, aIndex + 1);
    end;

    fPositions[aIndex] := aPos;
end;

procedure TMultiRackPosRunStep.SetRackNameAt(aIndex: integer; const aRackName: string);
begin
    if ( high(fRackNames) < aIndex) then
    begin
        SetLength(fRackNames, aIndex + 1);
    end;

    fRackNames[aIndex] := aRackName;
end;

{ TMultiTipSubRunStep }

constructor TMultiTipSubRunStep.Create;
begin
    inherited Create();
    fTip := -1;
    fPipDeviceName := '';
end;

{ TRunStepListLinearIterator }

procedure TRunStepListLinearIterator.BookmarkCurrentCursorPos;
begin
    fBookmarkedCursorPos := self.DataCursor;
end;

{ TBlockAddRunStep }

constructor TBlockAddRunStep.Create;
begin
    inherited Create(false);
end;

function TBlockAddRunStep.DoGetDescription: string;
var
    x: integer;
begin
    result := '';
    for x := 0 to self.List.Count - 1 do
    begin
        if result <> '' then
            result := result + #13#10;
        result := result + self[x].Description;
    end;
    result := DoGetBlockDescription + ' Add: ' + result;
end;


end.
