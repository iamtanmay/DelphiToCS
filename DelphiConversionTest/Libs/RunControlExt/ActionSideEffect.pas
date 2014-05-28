unit ActionSideEffect;
{-----------------------------------------------------------------------------------------------------------------------
 Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
 Author       : Payman Kamali (pk)
 Description  :
 -----------------------------------------------------------------------------------------------------------------------
 Revision History:
 date     op  method                                track-no  improvement/change
 -------- --  ------------------------------------  --------- ----------------------------------------------------------
 17.02.09 pk                                        TN4232      Initial Revision
 20.02.09 pk                                        TN4232      register TCallSubMethodSideEffectData
 04.02.10 pk                                        TN4972      Various changes
 04.15.10 pk  TPipDeviceTipTypeChangedSideEffect    TN5050      New
 -----------------------------------------------------------------------------------------------------------------------}
interface
uses
    TraceManager, ActionData, IdentItemData, DataExchangeDatasetWrapper, ReferenceIdentValueMemoryDataCache,
    RunStep, RunStepBlockDataCache, CallStackDataCache;

type
    TIdentValueChangedSideEffectData = class( TActionSideEffectData )
      private
        fIsGlobalIdent : boolean;
        fIdentName : string;
        fOldValue : TIdentValueData;
      public
        constructor Create( const aIdentName : string; const aOldIdentValueData : TIdentValueData; const aIsGlobalIdent : boolean );  reintroduce;
        destructor Destroy(); override;
      published
        property IsGlobalIdent : boolean read fIsGlobalIdent write fIsGlobalIdent;
        property IdentName : string read fIdentName write fIdentName;
        property OldValue : TIdentValueData read fOldValue write fOldValue;
    end;

    TSubMethodCallSideEffectData = class( TActionSideEffectData )
      public
        constructor Create(); override;
    end;

    TSubMethodEndSideEffectData = class( TActionSideEffectData )
      protected
        fCurrentStackFrame : TCallStackFrameData;
      public
        constructor Create( const aCurrentStackFrame : TCallStackFrameData );   reintroduce;
        destructor Destroy(); override;
      published
        property CurrentStackFrame : TCallStackFrameData read fCurrentStackFrame write fCurrentStackFrame;
    end;

    TAllocateReferenceSideEffectData = class( TActionSideEffectData )
      private
        fRefID : integer;
      public
        constructor Create( const aRefID : integer ); reintroduce;
      published
        property RefID : integer read fRefID write fRefID;
    end;

    TDeAllocateReferenceSideEffectData = class( TActionSideEffectData )
      private
        fReferenceIdentValueMemoryData : TReferenceIdentValueMemoryData;
      public
        constructor Create( const aReferenceIdentValueMemoryData : TReferenceIdentValueMemoryData );  reintroduce;
        destructor Destroy(); override;
      published
        property ReferenceIdentValueMemoryData : TReferenceIdentValueMemoryData read fReferenceIdentValueMemoryData write fReferenceIdentValueMemoryData;
    end;

    TDatasetSideEffectData = class( TActionSideEffectData )
      private
        fRefID : integer;
      public
        constructor Create( const aRefID : integer ); reintroduce;
      published
        property RefID : integer read fRefID write fRefID;
    end;

    TDatasetCursorMoveSideEffectData = class( TDatasetSideEffectData )
      private
        fOldCursorPos : integer;
        fNewCursorPos : integer;
      public
        constructor Create( const aRefID : integer; const aOldCursorPos, aNewCursorPos : integer );   reintroduce;
      published
        property OldCursorPos : integer read fOldCursorPos write fOldCursorPos;
        property NewCursorPos : integer read fNewCursorPos write fNewCursorPos;
    end;

    TPendingRunStepsMoveCursorSideEffectData = class( TActionSideEffectData )
      private
        fOldCursorPos : integer;
        fNewCursorPos : integer;
      public
        constructor Create( const aOldCursorPos, aNewCursorPos : integer );   reintroduce;
      published
        property OldCursorPos : integer read fOldCursorPos write fOldCursorPos;
        property NewCursorPos : integer read fNewCursorPos write fNewCursorPos;
    end;

    TPendingRunStepsClearSideEffectData = class( TActionSideEffectData )
      private
        fOldSteps : TRunStepList;
      public
        destructor Destroy(); override;
        constructor Create( const aOldSteps : TRunStepList ); reintroduce;
      published
        property OldSteps : TRunStepList read fOldSteps write fOldSteps;
    end;

    TPendingRunStepsAddStepsSideEffectData = class( TActionSideEffectData )
      public
        constructor Create(); override;
    end;

    TRunStepBlockAddSideEffectData = class( TActionSideEffectData )
      private
        fBlockDataID : string;
      public
        constructor Create( const aBlockDataID : string );   reintroduce;
      published
        property BlockDataID : string read fBlockDataID write fBlockDataID;
    end;

    TRunStepBlockRemoveSideEffectData = class( TActionSideEffectData )
      private
        fRunStepBlockData : TRunStepBlockData;
      public
        constructor Create( const aRunStepBlockData : TRunStepBlockData ); reintroduce;
      published
        property RunStepBlockData : TRunStepBlockData read fRunStepBlockData write fRunStepBlockData;
    end;

    TRunStepBlockAddStepSideEffectData = class( TActionSideEffectData )
      private
        fBlockDataID : string;
        fNumStepsAdded : integer;
      public
        constructor Create( const aBlockDataID : string; const aNumStepsAdded : integer );   reintroduce;
      published
        property BlockDataID : string read fBlockDataID write fBlockDataID;
        property NumStepsAdded : integer read fNumStepsAdded write fNumStepsAdded;
    end;

    TPipDeviceTipTypeChangedSideEffectData = class( TActionSideEffectData )
      private
        fPipDeviceName : string;
        fTipIndex : integer;
        fOldTipTypeName: string;
      public
        constructor Create( const aPipDeviceName : string; const aTipIndex : integer; const aOldTipTypeName: string ); reintroduce;
      published
        property PipDeviceName : string read fPipDeviceName write fPipDeviceName;
        property TipIndex : integer read fTipIndex write fTipIndex;
        property OldTipTypeName : string read fOldTipTypeName write fOldTipTypeName;
    end;

    TActionSideEffect = class
      protected
        fSideEffectData : TActionSideEffectData;
        procedure DoUndo( const aProcessDetails: TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData  ); virtual; abstract;
      public
        constructor Create( const aSideEffectData : TActionSideEffectData );   reintroduce;
        destructor Destroy(); override;
        procedure Undo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData  );
    end;


    TIdentValueChangedSideEffect = class( TActionSideEffect )
      private
        function GetSideEffectData: TIdentValueChangedSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TIdentValueChangedSideEffectData read GetSideEffectData;
    end;

    TSubMethodCallSideEffect = class( TActionSideEffect )
      private
        function GetSideEffectData: TSubMethodCallSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TSubMethodCallSideEffectData read GetSideEffectData;
    end;

    TSubMethodEndSideEffect = class( TActionSideEffect )
      private
        function GetSideEffectData: TSubMethodEndSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TSubMethodEndSideEffectData read GetSideEffectData;
    end;


    TDatasetSideEffect = class( TActionSideEffect )
    end;

    TAllocateReferenceSideEffect = class( TActionSideEffect )
      private
        function GetSideEffectData: TAllocateReferenceSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TAllocateReferenceSideEffectData read GetSideEffectData;
    end;

    TDeallocateReferenceSideEffect = class( TActionSideEffect )
      private
        function GetSideEffectData: TDeallocateReferenceSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TDeallocateReferenceSideEffectData read GetSideEffectData;
    end;

    TDatasetCursorMoveSideEffect = class( TDatasetSideEffect )
      private
        function GetSideEffectData: TDatasetCursorMoveSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TDatasetCursorMoveSideEffectData read GetSideEffectData;
    end;

    TPendingRunStepsMoveCursorSideEffect = class( TDatasetSideEffect )
      private
        function GetSideEffectData: TPendingRunStepsMoveCursorSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TPendingRunStepsMoveCursorSideEffectData read GetSideEffectData;
    end;

    TPendingRunStepsClearSideEffect = class( TDatasetSideEffect )
      private
        function GetSideEffectData: TPendingRunStepsClearSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TPendingRunStepsClearSideEffectData read GetSideEffectData;
    end;

    TPendingRunStepsAddStepsSideEffect = class( TDatasetSideEffect )
      private
        function GetSideEffectData: TPendingRunStepsAddStepsSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TPendingRunStepsAddStepsSideEffectData read GetSideEffectData;
    end;

    TRunStepBlockAddSideEffect = class( TDatasetSideEffect )
      private
        function GetSideEffectData: TRunStepBlockAddSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TRunStepBlockAddSideEffectData read GetSideEffectData;
    end;

    TRunStepBlockRemoveSideEffect = class( TDatasetSideEffect )
      private
        function GetSideEffectData: TRunStepBlockRemoveSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TRunStepBlockRemoveSideEffectData read GetSideEffectData;
    end;

    TRunStepBlockAddStepSideEffect = class( TDatasetSideEffect )
      private
        function GetSideEffectData: TRunStepBlockAddStepSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TRunStepBlockAddStepSideEffectData read GetSideEffectData;
    end;

    TPipDeviceTipTypeChangedSideEffect = class( TDatasetSideEffect )
      private
        function GetSideEffectData: TPipDeviceTipTypeChangedSideEffectData;
      protected
        procedure DoUndo( const aProcessDetails : TProcessDetailsItem; const aThreadDetails : TThreadDetailsItem; const aActionData : TActionData ); override;
      public
        property SideEffectData : TPipDeviceTipTypeChangedSideEffectData read GetSideEffectData;
    end;

implementation
uses
    Classes, SysUtils, MethodCompiledFile, ArgClass, TypeMapTranslator;


{ TActionSideEffect }

constructor TActionSideEffect.Create(
  const aSideEffectData: TActionSideEffectData);
begin
    inherited Create();
    fSideEffectData := aSideEffectData;
end;

destructor TActionSideEffect.Destroy;
begin
    FreeAndNil( fSideEffectData );
    inherited;
end;

procedure TActionSideEffect.Undo(  const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem; const aActionData : TActionData );
begin
    self.DoUndo( aProcessDetails, aThreadDetails, aActionData );
end;

{ TIdentValueChangedSideEffectData }

constructor TIdentValueChangedSideEffectData.Create(
  const aIdentName : string;
  const aOldIdentValueData: TIdentValueData; const aIsGlobalIdent : boolean);
begin
    inherited Create();
    fIsGlobalIdent := aIsGlobalIdent;
    fIdentName := aIdentName;

    // dont need to copy.
    fOldValue := TObjectCopy<TIdentValueData>.Copy( aOldIdentValueData );
end;

destructor TIdentValueChangedSideEffectData.Destroy();
begin
    FreeAndNil( fOldValue );
    inherited;
end;

{ TIdentValueChangedSideEffect }

procedure TIdentValueChangedSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
  const aActionData : TActionData );
var
    xIdentItemData : TIdentItemData;
    xOldValue : TIdentValueData;
begin
    xOldValue := TObjectCopy<TIdentValueData>.Copy( self.SideEffectData.OldValue );

    if self.SideEffectData.IsGlobalIdent then begin
        xIdentItemData := aProcessDetails.HeapDataCache.FindIdent( self.SideEffectData.IdentName );
        if not Assigned( xIdentItemData ) then
            raise Exception.CreateFmt('Could not find global identifier [%s]', [ self.SideEffectData.IdentName ] );
        xIdentItemData.IdentValue := xOldValue; //TIdentListData.CloneIdentValueData( self.SideEffectData.OldValue );
        aProcessDetails.HeapDataCache.IdentChanged( xIdentItemData );
    end
    else begin
        xIdentItemData := aThreadDetails.CallStackDataCache.FindLocalIdent( self.SideEffectData.IdentName );
        if not Assigned( xIdentItemData ) then
            raise Exception.CreateFmt('Could not find local identifier [%s]', [ self.SideEffectData.IdentName ] );
        xIdentItemData.IdentValue := xOldValue;
        aThreadDetails.CallStackDataCache.LocalIdentChanged( xIdentItemData );
    end;
end;

function TIdentValueChangedSideEffect.GetSideEffectData: TIdentValueChangedSideEffectData;
begin
    result := fSideEffectData as TIdentValueChangedSideEffectData;
end;

{ TSubMethodCallSideEffectData }

constructor TSubMethodCallSideEffectData.Create();
begin
    inherited;
end;

{ TSubMethodCallSideEffect }

procedure TSubMethodCallSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
begin
    aThreadDetails.CallStackDataCache.RemoveLastCallStackFrameData();

end;

function TSubMethodCallSideEffect.GetSideEffectData: TSubMethodCallSideEffectData;
begin
    result := fSideEffectData as TSubMethodCallSideEffectData;
end;

{ TSubMethodEndSideEffectData }

constructor TSubMethodEndSideEffectData.Create( const aCurrentStackFrame : TCallStackFrameData );
begin
    inherited Create();
    fCurrentStackFrame := TObjectCopy<TCallStackFrameData>.Copy( aCurrentStackFrame );
end;

destructor TSubMethodEndSideEffectData.Destroy;
begin
    FreeAndNil( fCurrentStackFrame );
    inherited;
end;

{ TSubMethodEndSideEffect }

procedure TSubMethodEndSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
var
    xStackFrame : TCallStackFrameData;
begin
    xStackFrame := TObjectCopy<TCallStackFrameData>.Copy( ( fSideEffectData as TSubMethodEndSideEffectData ).CurrentStackFrame );
    aThreadDetails.CallStackDataCache.AddCallStackFrameData( xStackFrame );
end;

function TSubMethodEndSideEffect.GetSideEffectData: TSubMethodEndSideEffectData;
begin
    result := fSideEffectData as TSubMethodEndSideEffectData;
end;

{ TDatasetCursorMoveSideEffectData }

constructor TDatasetSideEffectData.Create(
  const aRefID: integer);
begin
    inherited Create();
    fRefID := aRefID;
end;


{ TDatasetCursorMoveSideEffectData }

constructor TDatasetCursorMoveSideEffectData.Create( const aRefID : integer;
  const aOldCursorPos, aNewCursorPos: integer);
begin
    inherited Create( aRefID );
    fOldCursorPos := aOldCursorPos;
    fNewCursorPos := aNewCursorPos;
end;

{ TDatasetCursorMoveSideEffect }

procedure TDatasetCursorMoveSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
var
    xData : TReferenceIdentValueMemoryData;
    xObjData : TDataExDSWrapperReferenceIdentValueMemoryObjData;
begin
    xData := aProcessDetails.ReferenceIdentValueMemoryListDataCache.Find( self.SideEffectData.RefID );
    xObjData := xData.Obj as TDataExDSWrapperReferenceIdentValueMemoryObjData;
    xObjData.CursorPos := self.SideEffectData.OldCursorPos;
    aProcessDetails.ReferenceIdentValueMemoryListDataCache.DataChanged( xData );
end;

function TDatasetCursorMoveSideEffect.GetSideEffectData: TDatasetCursorMoveSideEffectData;
begin
    result := fSideEffectData as TDatasetCursorMoveSideEffectData;
end;

{ TAllocateReferenceSideEffectData }

constructor TAllocateReferenceSideEffectData.Create(
  const aRefID: integer);
begin
    inherited Create();
    fRefID := aRefID;
end;

{ TDeAllocateReferenceSideEffectData }

constructor TDeAllocateReferenceSideEffectData.Create(
  const aReferenceIdentValueMemoryData: TReferenceIdentValueMemoryData);
begin
    inherited Create();
    fReferenceIdentValueMemoryData := TObjectCopy<TReferenceIdentValueMemoryData>.Copy( aReferenceIdentValueMemoryData );
end;


destructor TDeAllocateReferenceSideEffectData.Destroy;
begin
    FreeAndNil( fReferenceIdentValueMemoryData );
    inherited;
end;

{ TAllocateReferenceSideEffect }

procedure TAllocateReferenceSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
begin
    aProcessDetails.ReferenceIdentValueMemoryListDataCache.Remove( self.SideEffectData.RefID );
end;

function TAllocateReferenceSideEffect.GetSideEffectData: TAllocateReferenceSideEffectData;
begin
    result := fSideEffectData as TAllocateReferenceSideEffectData;
end;


{ TDeallocateReferenceSideEffect }

procedure TDeallocateReferenceSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
var
    xReferenceIdentValueMemoryData : TReferenceIdentValueMemoryData;
begin
    xReferenceIdentValueMemoryData := TObjectCopy< TReferenceIdentValueMemoryData >.Copy( self.SideEffectData.ReferenceIdentValueMemoryData );
    aProcessDetails.ReferenceIdentValueMemoryListDataCache.Add( xReferenceIdentValueMemoryData );
end;

function TDeallocateReferenceSideEffect.GetSideEffectData: TDeallocateReferenceSideEffectData;
begin
    result := fSideEffectData as TDeallocateReferenceSideEffectData;
end;

{ TPendingRunStepsMoveCursorSideEffectData }

constructor TPendingRunStepsMoveCursorSideEffectData.Create( const aOldCursorPos, aNewCursorPos: integer );
begin
    inherited Create();

    fOldCursorPos := aOldCursorPos;
    fNewCursorPos := aNewCursorPos;
end;

{ TPendingRunStepsMoveCursorSideEffect }

procedure TPendingRunStepsMoveCursorSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
begin
    aThreadDetails.PendingStepsDataCache.SetCursorPos( self.SideEffectData.OldCursorPos );
end;

function TPendingRunStepsMoveCursorSideEffect.GetSideEffectData: TPendingRunStepsMoveCursorSideEffectData;
begin
    result := fSideEffectData as TPendingRunStepsMoveCursorSideEffectData;
end;

{ TPendingRunStepsClearSideEffectData }

constructor TPendingRunStepsClearSideEffectData.Create( const aOldSteps: TRunStepList );
begin
    inherited Create();
    fOldSteps := TObjectCopy< TRunStepList >.Copy( aOldSteps );
end;

destructor TPendingRunStepsClearSideEffectData.Destroy;
begin
    FreeAndNil( fOldSteps );
    inherited;
end;

{ TPendingRunStepsClearSideEffect }

procedure TPendingRunStepsClearSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
var
    xOldSteps : TRunStepList;
begin
    xOldSteps := TObjectCopy<TRunStepList>.Copy( self.SideEffectData.OldSteps );
    aThreadDetails.PendingStepsDataCache.AddSteps( xOldSteps );
end;

function TPendingRunStepsClearSideEffect.GetSideEffectData: TPendingRunStepsClearSideEffectData;
begin
    result := fSideEffectData as TPendingRunStepsClearSideEffectData;
end;


{ TPendingRunStepsAddStepsSideEffectData }

constructor TPendingRunStepsAddStepsSideEffectData.Create();
begin
    inherited Create();

end;

procedure TPendingRunStepsAddStepsSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
begin
    aThreadDetails.PendingStepsDataCache.ClearSteps();
end;

function TPendingRunStepsAddStepsSideEffect.GetSideEffectData: TPendingRunStepsAddStepsSideEffectData;
begin
    result := fSideEffectData as TPendingRunStepsAddStepsSideEffectData;
end;

{ TRunStepBlockAddSideEffectData }

constructor TRunStepBlockAddSideEffectData.Create( const aBlockDataID : string );
begin
    inherited Create();
    fBlockDataID := aBlockDataID;
end;

{ TRunStepBlockAddSideEffect }

procedure TRunStepBlockAddSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
begin
    aThreadDetails.BlockListDataCache.RemoveRunStepBlock( self.SideEffectData.BlockDataID );
end;

function TRunStepBlockAddSideEffect.GetSideEffectData: TRunStepBlockAddSideEffectData;
begin
    result := fSideEffectData as TRunStepBlockAddSideEffectData;
end;


{ TRunStepBlockRemoveSideEffectData }

constructor TRunStepBlockRemoveSideEffectData.Create( const aRunStepBlockData : TRunStepBlockData );
begin
    inherited Create();
    fRunStepBlockData := TObjectCopy<TRunStepBlockData>.Copy( aRunStepBlockData );
end;

{ TRunStepBlockRemoveSideEffect }

procedure TRunStepBlockRemoveSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
var
    xBlockData : TRunStepBlockData;
begin
    xBlockData := TObjectCopy<TRunStepBlockData>.Copy( self.SideEffectData.RunStepBlockData );
    aThreadDetails.BlockListDataCache.AddRunStepBlock( xBlockData );
end;

function TRunStepBlockRemoveSideEffect.GetSideEffectData: TRunStepBlockRemoveSideEffectData;
begin
    result := fSideEffectData as TRunStepBlockRemoveSideEffectData;
end;

{ TRunStepBlockAddStepSideEffectData }

constructor TRunStepBlockAddStepSideEffectData.Create( const aBlockDataID : string; const aNumStepsAdded : integer );
begin
    inherited Create();
    fBlockDataID := aBlockDataID;
    fNumStepsAdded := aNumStepsAdded;
end;

{ TRunStepBlockAddStepSideEffect }

procedure TRunStepBlockAddStepSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
begin
    aThreadDetails.BlockListDataCache.RemoveLastStepsFromRunStepBlock( self.SideEffectData.BlockDataID, self.SideEffectData.NumStepsAdded );
end;

function TRunStepBlockAddStepSideEffect.GetSideEffectData: TRunStepBlockAddStepSideEffectData;
begin
    result := fSideEffectData as TRunStepBlockAddStepSideEffectData;
end;

{ TPipDeviceTipTypeChangedSideEffectData }

constructor TPipDeviceTipTypeChangedSideEffectData.Create(const aPipDeviceName: string; const aTipIndex: integer;
  const aOldTipTypeName: string);
begin
    inherited Create();
    fPipDeviceName := aPipDeviceName;
    fTipIndex := aTipIndex;
    fOldTipTypeName := aOldTipTypeName;
end;
{ TPipDeviceTipTypeChangedSideEffect }

procedure TPipDeviceTipTypeChangedSideEffect.DoUndo(
  const aProcessDetails: TProcessDetailsItem;
  const aThreadDetails: TThreadDetailsItem;
  const aActionData: TActionData);
begin
    aProcessDetails.PipDeviceListDataCache.PipDeviceSetTipTypeName( self.SideEffectData.PipDeviceName,
                                            self.SideEffectData.TipIndex, self.SideEffectData.OldTipTypeName );
end;

function TPipDeviceTipTypeChangedSideEffect.GetSideEffectData: TPipDeviceTipTypeChangedSideEffectData;
begin
    result := fSideEffectData as TPipDeviceTipTypeChangedSideEffectData;
end;
end.
