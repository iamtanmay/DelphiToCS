{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232      Initial Revision
  13.05.09 wl  TReferenceIdentValueMemoryDataMaker.CreateReferenceObjFromDataObj  TN4564    Aufruf von TDataExchangeDatasetWrapper mit zusätzlichen Parameter
  04.02.10 pk                                        TN4972      Various changes
  08.04.10 pk  TRunStepBlockDataMaker                TN4996      New RunStepBlockParams param
  04.15.10 pk  TPipDeviceDataMaker                   TN5050      New
  07.05.10 pk                                        TN5092      Changes for arrays
  17.05.10 pk                                        TN5092      Changes needed for Restart of arrays
  07.06.10 pk                                        TN5077      TPendingStepsDataMaker removed
  25.06.10 wl  TDataExDSWrapperReferenceIdentValueMemoryObjData   TN5161    neu: SourceFileName, OrderBy
  01.03.12 wl                                                     TN5822   uses geändert
  27.03.13 wl                                                     TN6045   uses geändert
  25.06.13 wl                                                     TN6178   uses Identifier
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunTraceDataMaker;


interface


uses
    IdentItemData,
    ProcessTraceDataCache,
    HeapDataCache,
    CallStackDataCache,
    ReferenceIdentValueMemoryDataCache,
    ActionDataCache,
    RunStepBlockDataCache,
    PendingStepsDataCache,
    PipDeviceDataCache,
    Streamable,
    Identifier,
    ListDataCache,
    MemoryClasses,
    ReferenceIdentValueMemory,
    RunStep,
    RunStepBlock;

type
    TIdentItemDataMaker = class
    public
        class function CreateIdentValueData(const aIdentValue: TStreamableItem): TStreamableItem;
        class function CreateIdentValue(const aIdentValueData: TStreamableItem): TStreamableItem;
        class procedure SetIdentItemValueData(const aIdentItem: TIdentifier;
            const aIdentItemData: TIdentItemData);
        class procedure SetIdentItemValue(const aIdentItemData: TIdentItemData;
            const aIdentItem: TIdentifier);
    end;

    TCallStackDataMaker = class
    private
        class procedure SetIdentList(const aIdentListData: TIdentListData; const aIdentList: TIdentifierList);
        class procedure SetStackFrames(const aCallStackFrameListData: TCallStackFrameListData;
            const aCallStack: TCallStack);
    public
        class procedure SetArgsData(const aCallArgs: TIdentifierList; const aCallArgsData: TIdentListData);
        class function CreateStackFrameData(const aCallStackFrame: TCallStackFrame): TCallStackFrameData;

        class procedure SetCallStack(const aCallStackData: TCallStackData; const aCallStack: TCallStack);
    end;

    THeapDataMaker = class
    public
        class procedure LoadHeapDataToHeap(const aHeap: TThreadSafeIdentifierList;
            const aHeapData: THeapData);
    end;

    TReferenceIdentValueMemoryDataMaker = class
    public
        class procedure InitData(const aValue: TReferenceIdentValueMemory;
            const aData: TReferenceIdentValueMemoryData);
        class procedure SetData(const aValue: TReferenceIdentValueMemory;
            const aData: TReferenceIdentValueMemoryData);
        class function CreateReferenceObjFromDataObj(const aDataObj
            : TReferenceIdentValueMemoryObjData): TObject;
        class procedure LoadListDataToReferenceIdentValueMemoryManager
            (const aListData: TReferenceIdentValueMemoryListData);
    end;

    TRunStepBlockDataMaker = class
    public
        class function CreateBlockData(const aBlock: TRunStepBlock): TRunStepBlockData;
        class function GetBlockDataID(const aBlock: TRunStepBlock): string;
        class procedure LoadBlockDataToBlockManager(const aBlockData: TRunStepBlockData);
        class procedure LoadBlockListDataToBlockManager(const aBlockListData: TRunStepBlockListData);
    end;

    TPipDeviceDataMaker = class
    public
        class procedure LoadTipTypes(const aPipDeviceListData: TListData);
    end;


implementation


uses
    SysUtils,
    DataExchangeDatasetWrapper,
    RelativeMemAddressData,
    RunStepBuilderProcessor,
    RunStepBlockManager,
    PipDeviceManager,
    IntfPipDevice,
    TypeMapTranslator;

{ TIdentItemDataMaker }

class function TIdentItemDataMaker.CreateIdentValueData(const aIdentValue: TStreamableItem): TStreamableItem;
begin
    result := TObjectCopy<TStreamableItem>.Copy(aIdentValue);
end;

class function TIdentItemDataMaker.CreateIdentValue(const aIdentValueData: TStreamableItem): TStreamableItem;
begin
    result := TObjectCopy<TStreamableItem>.Copy(aIdentValueData);
end;

class procedure TIdentItemDataMaker.SetIdentItemValueData(const aIdentItem: TIdentifier;
    const aIdentItemData: TIdentItemData);
begin
    aIdentItemData.IdentValue.Free;
    aIdentItemData.IdentValue := CreateIdentValueData(aIdentItem.Value);
end;

class procedure TIdentItemDataMaker.SetIdentItemValue(const aIdentItemData: TIdentItemData;
    const aIdentItem: TIdentifier);
begin
    aIdentItem.Value.Free;
    aIdentItem.Value := CreateIdentValue(aIdentItemData.IdentValue);
end;

{ TCallStackDataMaker }

class procedure TCallStackDataMaker.SetArgsData(const aCallArgs: TIdentifierList;
    const aCallArgsData: TIdentListData);
var
    x: integer;
    xIdentItemData: TIdentItemData;
begin
    for x := 0 to aCallArgs.Count - 1 do
    begin
        xIdentItemData := TIdentItemData.Create();
        xIdentItemData.IdentName := aCallArgs[x].Key;
        aCallArgsData.Add(xIdentItemData);
        // xIdentItemData.ID := IntToStr( aCallArgsData.Count - 1 );
        TIdentItemDataMaker.SetIdentItemValueData(aCallArgs[x], xIdentItemData);
    end;
end;

class function TCallStackDataMaker.CreateStackFrameData(const aCallStackFrame: TCallStackFrame)
    : TCallStackFrameData;
begin
    result := TCallStackFrameData.Create();
    // ReturnAddress
    result.ReturnAddress := TRelativeMemAddressData.Create();
    result.ReturnAddress.LabelName := aCallStackFrame.ReturnAddress.LabelName;
    result.ReturnAddress.RelativeAddress := aCallStackFrame.ReturnAddress.RelativeAddress;
    // Call Args
    result.CallArgs := TIdentListData.Create();
    SetArgsData(aCallStackFrame.CallArgs, result.CallArgs);
    // Local Vars
    result.LocalIdents := TIdentListData.Create();
    SetArgsData(aCallStackFrame.LocalVars, result.LocalIdents);
end;

class procedure TCallStackDataMaker.SetIdentList(const aIdentListData: TIdentListData;
    const aIdentList: TIdentifierList);
var
    x: integer;
    xIdent: TIdentifier;
    xIdentData: TIdentItemData;
begin
    for x := 0 to aIdentListData.Count - 1 do
    begin
        xIdentData := aIdentListData[x];
        xIdent := TIdentifier.Create(xIdentData.IdentName, false);
        TIdentItemDataMaker.SetIdentItemValue(xIdentData, xIdent);
        aIdentList.Add(xIdent);
    end;
end;

class procedure TCallStackDataMaker.SetStackFrames(const aCallStackFrameListData: TCallStackFrameListData;
    const aCallStack: TCallStack);
var
    x: integer;
    xCurrentFrameData: TCallStackFrameData;
begin
    for x := 0 to aCallStackFrameListData.List.Count - 1 do
    begin
        xCurrentFrameData := aCallStackFrameListData.List[x] as TCallStackFrameData;
        aCallStack.Push();
        aCallStack.CurrentFrame.ReturnAddress.LabelName := xCurrentFrameData.ReturnAddress.LabelName;
        aCallStack.CurrentFrame.ReturnAddress.RelativeAddress :=
            xCurrentFrameData.ReturnAddress.RelativeAddress;
        SetIdentList(xCurrentFrameData.CallArgs, aCallStack.CurrentFrame.CallArgs);
        SetIdentList(xCurrentFrameData.LocalIdents, aCallStack.CurrentFrame.LocalVars);
    end;
end;

class procedure TCallStackDataMaker.SetCallStack(const aCallStackData: TCallStackData;
    const aCallStack: TCallStack);
begin
    if aCallStackData.Frames.List.Count = 0 then
        EXIT;
    SetStackFrames(aCallStackData.Frames, aCallStack);
end;

class procedure THeapDataMaker.LoadHeapDataToHeap(const aHeap: TThreadSafeIdentifierList;
    const aHeapData: THeapData);
var
    x: integer;
    xIdent: TIdentifier;
    xIdentData: TIdentItemData;
begin
    for x := 0 to aHeapData.Count - 1 do
    begin
        xIdentData := aHeapData[x];
        xIdent := TIdentifier.Create(xIdentData.IdentName, false);
        TIdentItemDataMaker.SetIdentItemValue(xIdentData, xIdent);
        aHeap.Add(xIdent);
    end;
end;

{ TReferenceIdentValueMemoryDataMaker }

class procedure TReferenceIdentValueMemoryDataMaker.SetData(const aValue: TReferenceIdentValueMemory;
    const aData: TReferenceIdentValueMemoryData);
begin
    (aData.Obj as TDataExDSWrapperReferenceIdentValueMemoryObjData).DefName :=
        (aValue.Obj as TDataExchangeDatasetWrapper).DefName;
    (aData.Obj as TDataExDSWrapperReferenceIdentValueMemoryObjData).CursorPos :=
        (aValue.Obj as TDataExchangeDatasetWrapper).CurrentRecordNo;
    (aData.Obj as TDataExDSWrapperReferenceIdentValueMemoryObjData).IsOpen :=
        (aValue.Obj as TDataExchangeDatasetWrapper).IsOpen;
    (aData.Obj as TDataExDSWrapperReferenceIdentValueMemoryObjData).Filter :=
        (aValue.Obj as TDataExchangeDatasetWrapper).Filter;
    (aData.Obj as TDataExDSWrapperReferenceIdentValueMemoryObjData).OrderBy :=
        (aValue.Obj as TDataExchangeDatasetWrapper).OrderBy;
    (aData.Obj as TDataExDSWrapperReferenceIdentValueMemoryObjData).SourceFileName :=
        (aValue.Obj as TDataExchangeDatasetWrapper).SourceFileName;
end;

class procedure TReferenceIdentValueMemoryDataMaker.InitData(const aValue: TReferenceIdentValueMemory;
    const aData: TReferenceIdentValueMemoryData);
begin
    aData.RefID := aValue.ID;
    if aData.Obj = nil then
    begin
        if aValue.Obj is TDataExchangeDatasetWrapper then
        begin
            aData.Obj := TDataExDSWrapperReferenceIdentValueMemoryObjData.Create();
        end;
    end;
    SetData(aValue, aData);
end;

class function TReferenceIdentValueMemoryDataMaker.CreateReferenceObjFromDataObj
    (const aDataObj: TReferenceIdentValueMemoryObjData): TObject;
var
    xDataExchangeDatasetWrapper: TDataExchangeDatasetWrapper;
begin
    result := nil;
    if aDataObj is TDataExDSWrapperReferenceIdentValueMemoryObjData then
    begin
        xDataExchangeDatasetWrapper := TDataExchangeDatasetWrapper.Create
            ((aDataObj as TDataExDSWrapperReferenceIdentValueMemoryObjData).DefName,
            (aDataObj as TDataExDSWrapperReferenceIdentValueMemoryObjData).Filter,
            (aDataObj as TDataExDSWrapperReferenceIdentValueMemoryObjData).OrderBy,
            (aDataObj as TDataExDSWrapperReferenceIdentValueMemoryObjData).SourceFileName);
        if (aDataObj as TDataExDSWrapperReferenceIdentValueMemoryObjData).IsOpen then
        begin
            xDataExchangeDatasetWrapper.Open();
            xDataExchangeDatasetWrapper.CursorMove(false,
                (aDataObj as TDataExDSWrapperReferenceIdentValueMemoryObjData).CursorPos);
        end;
        result := xDataExchangeDatasetWrapper;
    end;
end;

class procedure TReferenceIdentValueMemoryDataMaker.LoadListDataToReferenceIdentValueMemoryManager
    (const aListData: TReferenceIdentValueMemoryListData);
var
    x: integer;
    xObject: TObject;
    xData: TReferenceIdentValueMemoryData;
begin
    for x := 0 to aListData.Count - 1 do
    begin
        xData := aListData[x];
        xObject := CreateReferenceObjFromDataObj(xData.Obj);
        TReferenceIdentValueMemoryManager.Instance.AddWithID(xData.RefID, xObject);
    end;
end;

{ TRunStepBlockDataMaker }

class function TRunStepBlockDataMaker.GetBlockDataID(const aBlock: TRunStepBlock): string;
begin
    // this must be unique
    result := aBlock.BlockTypeName;
end;

class procedure TRunStepBlockDataMaker.LoadBlockDataToBlockManager(const aBlockData: TRunStepBlockData);
begin
    TRunStepBlockManager.Instance.BeginBlock(aBlockData.BlockOwnerID, aBlockData.BlockTypeName,
        aBlockData.BlockParams, false);
    TRunStepBlockManager.Instance.AddStepsToBlock(aBlockData.BlockOwnerID, aBlockData.BlockTypeName,
        aBlockData.RunSteps, false);
end;

class procedure TRunStepBlockDataMaker.LoadBlockListDataToBlockManager(const aBlockListData
    : TRunStepBlockListData);
var
    x: integer;
begin
    for x := 0 to aBlockListData.List.Count - 1 do
    begin
        LoadBlockDataToBlockManager(aBlockListData.List[x] as TRunStepBlockData);
    end;

end;

class function TRunStepBlockDataMaker.CreateBlockData(const aBlock: TRunStepBlock): TRunStepBlockData;
begin
    result := TRunStepBlockData.Create(GetBlockDataID(aBlock), aBlock.OwnerID, aBlock.BlockTypeName,
        aBlock.RunStepBlockParams);
end;

{ TPipDeviceDataMaker }

class procedure TPipDeviceDataMaker.LoadTipTypes(const aPipDeviceListData: TListData);
var
    x: integer;
    xPipDeviceData: TPipDeviceData;
    xPipDevice: IPipDevice;
begin
    for x := 0 to aPipDeviceListData.Count - 1 do
    begin
        xPipDeviceData := aPipDeviceListData[x] as TPipDeviceData;
        xPipDevice := gPipDeviceManager.FindPipDevice_ByName(xPipDeviceData.PipDeviceName);
        xPipDevice.LoadTips(xPipDeviceData.GetTipTypes());
    end;
end;


end.
