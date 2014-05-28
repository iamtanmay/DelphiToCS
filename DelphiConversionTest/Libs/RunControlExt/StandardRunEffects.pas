{ -----------------------------------------------------------------------------------------------------------------------
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
  01.03.12 wl                                        TN5822   uses geändert
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit StandardRunEffects;


interface


uses
    RunEffect,
    ActionData,
    IdentItemData,
    DataExchangeDatasetWrapper,
    ReferenceIdentValueMemoryDataCache,
    RunStep,
    RunStepBlockDataCache,
    CallStackDataCache,
    TraceProcessDetails,
    TraceThreadDetails,
    RunEffectData,
    StandardRunEffectData;

type
    TIdentValueChangedRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TIdentValueChangedRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TIdentValueChangedRunEffectData read GetRunEffectData;
    end;

    TSubMethodCallRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TSubMethodCallRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TSubMethodCallRunEffectData read GetRunEffectData;
    end;

    TSubMethodEndRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TSubMethodEndRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TSubMethodEndRunEffectData read GetRunEffectData;
    end;

    TDatasetRunEffect = class(TRunEffect)
    end;

    TAllocateReferenceRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TAllocateReferenceRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TAllocateReferenceRunEffectData read GetRunEffectData;
    end;

    TDeallocateReferenceRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TDeallocateReferenceRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TDeallocateReferenceRunEffectData read GetRunEffectData;
    end;

    TDatasetCursorMoveRunEffect = class(TDatasetRunEffect)
    private
        function GetRunEffectData: TDatasetCursorMoveRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TDatasetCursorMoveRunEffectData read GetRunEffectData;
    end;

    TRunStepBlockAddRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TRunStepBlockAddRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TRunStepBlockAddRunEffectData read GetRunEffectData;
    end;

    TRunStepBlockRemoveRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TRunStepBlockRemoveRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TRunStepBlockRemoveRunEffectData read GetRunEffectData;
    end;

    TRunStepBlockAddStepRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TRunStepBlockAddStepRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TRunStepBlockAddStepRunEffectData read GetRunEffectData;
    end;

    TPipDeviceTipTypeChangedRunEffect = class(TRunEffect)
    private
        function GetRunEffectData: TPipDeviceTipTypeChangedRunEffectData;
    protected
        procedure DoUndo(const aProcessDetails: TProcessDetailsItem; const aThreadDetails: TThreadDetailsItem;
            const aActionData: TActionData); override;
    public
        property RunEffectData: TPipDeviceTipTypeChangedRunEffectData read GetRunEffectData;
    end;


implementation


uses
    SysUtils,  Streamable,
    TypeMapTranslator;

{ TIdentValueChangedRunEffect }

procedure TIdentValueChangedRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xIdentItemData: TIdentItemData;
    xOldValue: TStreamableItem;
begin
    xOldValue := TObjectCopy<TStreamableItem>.Copy(self.RunEffectData.OldValue);

    if self.RunEffectData.IsGlobalIdent then
    begin
        xIdentItemData := aProcessDetails.HeapDataCache.FindIdent(self.RunEffectData.IdentName);
        if not Assigned(xIdentItemData) then
            raise Exception.CreateFmt('Could not find global identifier [%s]',
                [self.RunEffectData.IdentName]);
        xIdentItemData.IdentValue := xOldValue;
        // TIdentListData.CloneIdentValueData( self.RunEffectData.OldValue );
        aProcessDetails.HeapDataCache.IdentChanged(xIdentItemData);
    end
    else
    begin
        xIdentItemData := aThreadDetails.CallStackDataCache.FindLocalIdent(self.RunEffectData.IdentName);
        if not Assigned(xIdentItemData) then
            raise Exception.CreateFmt('Could not find local identifier [%s]', [self.RunEffectData.IdentName]);
        xIdentItemData.IdentValue := xOldValue;
        aThreadDetails.CallStackDataCache.LocalIdentChanged(xIdentItemData);
    end;
end;

function TIdentValueChangedRunEffect.GetRunEffectData: TIdentValueChangedRunEffectData;
begin
    result := fRunEffectData as TIdentValueChangedRunEffectData;
end;

{ TSubMethodCallRunEffect }

procedure TSubMethodCallRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
begin
    aThreadDetails.CallStackDataCache.RemoveLastCallStackFrameData();

end;

function TSubMethodCallRunEffect.GetRunEffectData: TSubMethodCallRunEffectData;
begin
    result := fRunEffectData as TSubMethodCallRunEffectData;
end;

{ TSubMethodEndRunEffect }

procedure TSubMethodEndRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xStackFrame: TCallStackFrameData;
begin
    xStackFrame := TObjectCopy<TCallStackFrameData>.Copy((fRunEffectData as TSubMethodEndRunEffectData)
        .CurrentStackFrame);
    aThreadDetails.CallStackDataCache.AddCallStackFrameData(xStackFrame);
end;

function TSubMethodEndRunEffect.GetRunEffectData: TSubMethodEndRunEffectData;
begin
    result := fRunEffectData as TSubMethodEndRunEffectData;
end;

{ TDatasetCursorMoveRunEffect }

procedure TDatasetCursorMoveRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xData: TReferenceIdentValueMemoryData;
    xObjData: TDataExDSWrapperReferenceIdentValueMemoryObjData;
begin
    xData := aProcessDetails.ReferenceIdentValueMemoryListDataCache.Find(self.RunEffectData.RefID);
    xObjData := xData.Obj as TDataExDSWrapperReferenceIdentValueMemoryObjData;
    xObjData.CursorPos := self.RunEffectData.OldCursorPos;
    aProcessDetails.ReferenceIdentValueMemoryListDataCache.DataChanged(xData);
end;

function TDatasetCursorMoveRunEffect.GetRunEffectData: TDatasetCursorMoveRunEffectData;
begin
    result := fRunEffectData as TDatasetCursorMoveRunEffectData;
end;

{ TAllocateReferenceRunEffect }

procedure TAllocateReferenceRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
begin
    aProcessDetails.ReferenceIdentValueMemoryListDataCache.Remove(self.RunEffectData.RefID);
end;

function TAllocateReferenceRunEffect.GetRunEffectData: TAllocateReferenceRunEffectData;
begin
    result := fRunEffectData as TAllocateReferenceRunEffectData;
end;

{ TDeallocateReferenceRunEffect }

procedure TDeallocateReferenceRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xReferenceIdentValueMemoryData: TReferenceIdentValueMemoryData;
begin
    xReferenceIdentValueMemoryData := TObjectCopy<TReferenceIdentValueMemoryData>.Copy
        (self.RunEffectData.ReferenceIdentValueMemoryData);
    aProcessDetails.ReferenceIdentValueMemoryListDataCache.Add(xReferenceIdentValueMemoryData);
end;

function TDeallocateReferenceRunEffect.GetRunEffectData: TDeallocateReferenceRunEffectData;
begin
    result := fRunEffectData as TDeallocateReferenceRunEffectData;
end;

{ TRunStepBlockAddRunEffect }

procedure TRunStepBlockAddRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
begin
    aThreadDetails.BlockListDataCache.RemoveRunStepBlock(self.RunEffectData.BlockDataID);
end;

function TRunStepBlockAddRunEffect.GetRunEffectData: TRunStepBlockAddRunEffectData;
begin
    result := fRunEffectData as TRunStepBlockAddRunEffectData;
end;

{ TRunStepBlockRemoveRunEffect }

procedure TRunStepBlockRemoveRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
var
    xBlockData: TRunStepBlockData;
begin
    xBlockData := TObjectCopy<TRunStepBlockData>.Copy(self.RunEffectData.RunStepBlockData);
    aThreadDetails.BlockListDataCache.AddRunStepBlock(xBlockData);
end;

function TRunStepBlockRemoveRunEffect.GetRunEffectData: TRunStepBlockRemoveRunEffectData;
begin
    result := fRunEffectData as TRunStepBlockRemoveRunEffectData;
end;

{ TRunStepBlockAddStepRunEffect }

procedure TRunStepBlockAddStepRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
begin
    aThreadDetails.BlockListDataCache.RemoveLastStepsFromRunStepBlock(self.RunEffectData.BlockDataID,
        self.RunEffectData.NumStepsAdded);
end;

function TRunStepBlockAddStepRunEffect.GetRunEffectData: TRunStepBlockAddStepRunEffectData;
begin
    result := fRunEffectData as TRunStepBlockAddStepRunEffectData;
end;

{ TPipDeviceTipTypeChangedRunEffect }

procedure TPipDeviceTipTypeChangedRunEffect.DoUndo(const aProcessDetails: TProcessDetailsItem;
    const aThreadDetails: TThreadDetailsItem; const aActionData: TActionData);
begin
    aProcessDetails.PipDeviceListDataCache.PipDeviceSetTipTypeName(self.RunEffectData.PipDeviceName,
        self.RunEffectData.TipIndex, self.RunEffectData.OldTipTypeName);
end;

function TPipDeviceTipTypeChangedRunEffect.GetRunEffectData: TPipDeviceTipTypeChangedRunEffectData;
begin
    result := fRunEffectData as TPipDeviceTipTypeChangedRunEffectData;
end;


end.
