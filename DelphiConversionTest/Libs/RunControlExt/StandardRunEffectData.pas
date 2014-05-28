{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------- }

unit StandardRunEffectData;


interface


uses
    RunEffectData,
    IdentItemData,
    CallStackDataCache,
    ReferenceIdentValueMemoryDataCache,
    RunStep,
    Streamable,
    RunStepBlockDataCache;

type
    TIdentValueChangedRunEffectData = class(TRunEffectData)
    private
        fIsGlobalIdent: boolean;
        fIdentName: string;
        fOldValue: TStreamableItem;
    public
        constructor Create(const aIdentName: string; const aOldIdentValueData: TStreamableItem;
            const aIsGlobalIdent: boolean); reintroduce;
        destructor Destroy(); override;
    published
        property IsGlobalIdent: boolean read fIsGlobalIdent write fIsGlobalIdent;
        property IdentName: string read fIdentName write fIdentName;
        property OldValue: TStreamableItem read fOldValue write fOldValue;
    end;

    TSubMethodCallRunEffectData = class(TRunEffectData)
    public
        constructor Create(); override;
    end;

    TSubMethodEndRunEffectData = class(TRunEffectData)
    protected
        fCurrentStackFrame: TCallStackFrameData;
    public
        constructor Create(const aCurrentStackFrame: TCallStackFrameData); reintroduce;
        destructor Destroy(); override;
    published
        property CurrentStackFrame: TCallStackFrameData read fCurrentStackFrame write fCurrentStackFrame;
    end;

    TAllocateReferenceRunEffectData = class(TRunEffectData)
    private
        fRefID: integer;
    public
        constructor Create(const aRefID: integer); reintroduce;
    published
        property RefID: integer read fRefID write fRefID;
    end;

    TDeAllocateReferenceRunEffectData = class(TRunEffectData)
    private
        fReferenceIdentValueMemoryData: TReferenceIdentValueMemoryData;
    public
        constructor Create(const aReferenceIdentValueMemoryData: TReferenceIdentValueMemoryData); reintroduce;
        destructor Destroy(); override;
    published
        property ReferenceIdentValueMemoryData: TReferenceIdentValueMemoryData
            read fReferenceIdentValueMemoryData write fReferenceIdentValueMemoryData;
    end;

    TDatasetRunEffectData = class(TRunEffectData)
    private
        fRefID: integer;
    public
        constructor Create(const aRefID: integer); reintroduce;
    published
        property RefID: integer read fRefID write fRefID;
    end;

    TDatasetCursorMoveRunEffectData = class(TDatasetRunEffectData)
    private
        fOldCursorPos: integer;
        fNewCursorPos: integer;
    public
        constructor Create(const aRefID: integer; const aOldCursorPos, aNewCursorPos: integer); reintroduce;
    published
        property OldCursorPos: integer read fOldCursorPos write fOldCursorPos;
        property NewCursorPos: integer read fNewCursorPos write fNewCursorPos;
    end;

    TPendingRunStepsMoveCursorRunEffectData = class(TRunEffectData)
    private
        fOldCursorPos: integer;
        fNewCursorPos: integer;
    public
        constructor Create(const aOldCursorPos, aNewCursorPos: integer); reintroduce;
    published
        property OldCursorPos: integer read fOldCursorPos write fOldCursorPos;
        property NewCursorPos: integer read fNewCursorPos write fNewCursorPos;
    end;

    TPendingRunStepsClearRunEffectData = class(TRunEffectData)
    private
        fOldSteps: TRunStepList;
    public
        destructor Destroy(); override;
        constructor Create(const aOldSteps: TRunStepList); reintroduce;
    published
        property OldSteps: TRunStepList read fOldSteps write fOldSteps;
    end;

    TPendingRunStepsAddStepsRunEffectData = class(TRunEffectData)
    public
        constructor Create(); override;
    end;

    TRunStepBlockAddRunEffectData = class(TRunEffectData)
    private
        fBlockDataID: string;
    public
        constructor Create(const aBlockDataID: string); reintroduce;
    published
        property BlockDataID: string read fBlockDataID write fBlockDataID;
    end;

    TRunStepBlockRemoveRunEffectData = class(TRunEffectData)
    private
        fRunStepBlockData: TRunStepBlockData;
    public
        constructor Create(const aRunStepBlockData: TRunStepBlockData); reintroduce;
    published
        property RunStepBlockData: TRunStepBlockData read fRunStepBlockData write fRunStepBlockData;
    end;

    TRunStepBlockAddStepRunEffectData = class(TRunEffectData)
    private
        fBlockDataID: string;
        fNumStepsAdded: integer;
    public
        constructor Create(const aBlockDataID: string; const aNumStepsAdded: integer); reintroduce;
    published
        property BlockDataID: string read fBlockDataID write fBlockDataID;
        property NumStepsAdded: integer read fNumStepsAdded write fNumStepsAdded;
    end;

    TPipDeviceTipTypeChangedRunEffectData = class(TRunEffectData)
    private
        fPipDeviceName: string;
        fTipIndex: integer;
        fOldTipTypeName: string;
    public
        constructor Create(const aPipDeviceName: string; const aTipIndex: integer;
            const aOldTipTypeName: string); reintroduce;
    published
        property PipDeviceName: string read fPipDeviceName write fPipDeviceName;
        property TipIndex: integer read fTipIndex write fTipIndex;
        property OldTipTypeName: string read fOldTipTypeName write fOldTipTypeName;
    end;


implementation


{ TIdentValueChangedRunEffectData }
uses
    SysUtils,
    TypeMapTranslator;

constructor TIdentValueChangedRunEffectData.Create(const aIdentName: string;
    const aOldIdentValueData: TStreamableItem; const aIsGlobalIdent: boolean);
begin
    inherited Create();
    fIsGlobalIdent := aIsGlobalIdent;
    fIdentName := aIdentName;

    // dont need to copy.
    fOldValue := TObjectCopy<TStreamableItem>.Copy(aOldIdentValueData);
end;

destructor TIdentValueChangedRunEffectData.Destroy();
begin
    FreeAndNil(fOldValue);
    inherited;
end;
{ TSubMethodCallRunEffectData }

constructor TSubMethodCallRunEffectData.Create();
begin
    inherited;
end;
{ TSubMethodEndRunEffectData }

constructor TSubMethodEndRunEffectData.Create(const aCurrentStackFrame: TCallStackFrameData);
begin
    inherited Create();
    fCurrentStackFrame := TObjectCopy<TCallStackFrameData>.Copy(aCurrentStackFrame);
end;

destructor TSubMethodEndRunEffectData.Destroy;
begin
    FreeAndNil(fCurrentStackFrame);
    inherited;
end;

{ TDatasetCursorMoveRunEffectData }

constructor TDatasetRunEffectData.Create(const aRefID: integer);
begin
    inherited Create();
    fRefID := aRefID;
end;

{ TDatasetCursorMoveRunEffectData }

constructor TDatasetCursorMoveRunEffectData.Create(const aRefID: integer;
    const aOldCursorPos, aNewCursorPos: integer);
begin
    inherited Create(aRefID);
    fOldCursorPos := aOldCursorPos;
    fNewCursorPos := aNewCursorPos;
end;

{ TAllocateReferenceRunEffectData }

constructor TAllocateReferenceRunEffectData.Create(const aRefID: integer);
begin
    inherited Create();
    fRefID := aRefID;
end;

{ TDeAllocateReferenceRunEffectData }

constructor TDeAllocateReferenceRunEffectData.Create(const aReferenceIdentValueMemoryData
    : TReferenceIdentValueMemoryData);
begin
    inherited Create();
    fReferenceIdentValueMemoryData := TObjectCopy<TReferenceIdentValueMemoryData>.Copy
        (aReferenceIdentValueMemoryData);
end;

destructor TDeAllocateReferenceRunEffectData.Destroy;
begin
    FreeAndNil(fReferenceIdentValueMemoryData);
    inherited;
end;

{ TPendingRunStepsMoveCursorRunEffectData }

constructor TPendingRunStepsMoveCursorRunEffectData.Create(const aOldCursorPos, aNewCursorPos: integer);
begin
    inherited Create();

    fOldCursorPos := aOldCursorPos;
    fNewCursorPos := aNewCursorPos;
end;

{ TPendingRunStepsClearRunEffectData }

constructor TPendingRunStepsClearRunEffectData.Create(const aOldSteps: TRunStepList);
begin
    inherited Create();
    fOldSteps := TObjectCopy<TRunStepList>.Copy(aOldSteps);
end;

destructor TPendingRunStepsClearRunEffectData.Destroy;
begin
    FreeAndNil(fOldSteps);
    inherited;
end;

{ TPendingRunStepsAddStepsRunEffectData }

constructor TPendingRunStepsAddStepsRunEffectData.Create();
begin
    inherited Create();

end;

{ TRunStepBlockAddRunEffectData }

constructor TRunStepBlockAddRunEffectData.Create(const aBlockDataID: string);
begin
    inherited Create();
    fBlockDataID := aBlockDataID;
end;
{ TRunStepBlockRemoveRunEffectData }

constructor TRunStepBlockRemoveRunEffectData.Create(const aRunStepBlockData: TRunStepBlockData);
begin
    inherited Create();
    fRunStepBlockData := TObjectCopy<TRunStepBlockData>.Copy(aRunStepBlockData);
end;

{ TRunStepBlockAddStepRunEffectData }

constructor TRunStepBlockAddStepRunEffectData.Create(const aBlockDataID: string;
    const aNumStepsAdded: integer);
begin
    inherited Create();
    fBlockDataID := aBlockDataID;
    fNumStepsAdded := aNumStepsAdded;
end;
{ TPipDeviceTipTypeChangedRunEffectData }

constructor TPipDeviceTipTypeChangedRunEffectData.Create(const aPipDeviceName: string;
    const aTipIndex: integer; const aOldTipTypeName: string);
begin
    inherited Create();
    fPipDeviceName := aPipDeviceName;
    fTipIndex := aTipIndex;
    fOldTipTypeName := aOldTipTypeName;
end;


end.
