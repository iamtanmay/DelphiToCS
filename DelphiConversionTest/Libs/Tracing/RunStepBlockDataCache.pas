unit RunStepBlockDataCache;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.02.10 pk                                        TN4972    Initial Revision
  08.04.10 pk  TRunStepBlockData                     TN4996    New RunStepBlockParams
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    Streamable,
    XMLReaderWriter,
    RunStep,
    RunStepBlock;

type

    TRunStepBlockData = class(TStreamable)
    private
        fBlockDataID: string;
        fBlockOwnerID: string;
        fBlockTypeName: string;
        fBlockParams: TRunStepBlockParams;
        fRunSteps: TRunStepList;
    public
        constructor Create(); overload; override;
        constructor Create(const aBlockDataID: string; const aBlockOwnerID: string;
            const aBlockTypeName: string; const aBlockParams: TRunStepBlockParams); reintroduce; overload;
        destructor Destroy(); override;
    published
        property BlockDataID: string read fBlockDataID write fBlockDataID;
        property BlockOwnerID: string read fBlockOwnerID write fBlockOwnerID;
        property BlockTypeName: string read fBlockTypeName write fBlockTypeName;
        property BlockParams: TRunStepBlockParams read fBlockParams write fBlockParams;
        property RunSteps: TRunStepList read fRunSteps write fRunSteps;
    end;

    TRunStepBlockListData = class(TStreamable)
    private
        fList: TStreamableObjectList;
    public
        constructor Create(); override;
        destructor Destroy(); override;
    published
        property List: TStreamableObjectList read fList write fList;
    end;

    TRunStepBlockListDataCache = class
    private
        fRunStepBlocks: TRunStepBlockListData;
        fReaderWriter: TXMLReaderWriter;
    public
        constructor Create(const aPathName: string);
        destructor Destroy(); override;
        procedure Init();
        procedure Clear();
        procedure Flush();
        function read(): boolean;
        procedure AddRunStepBlock(const aRunStepBlockData: TRunStepBlockData);
        procedure RemoveRunStepBlock(const aBlockDataID: string);
        function FindRunStepBlock(const aBlockDataID: string): TRunStepBlockData;
        procedure AddStepsToRunStepBlock(const aBlockDataID: string; const aRunSteps: TRunStepList);
        procedure RemoveLastStepsFromRunStepBlock(const aBlockDataID: string; const aNumSteps: integer);
        property RunStepBlocks: TRunStepBlockListData read fRunStepBlocks;
    end;


implementation


uses
    SysUtils,
    Classes,
    windows;

const
    cAutoFlushBufferCycle = 0;

    { TRunStepBlockListData }

constructor TRunStepBlockListData.Create;
begin
    inherited;
    fList := TStreamableObjectList.Create();
end;

destructor TRunStepBlockListData.Destroy;
begin
    fList.Free;
    inherited;
end;

{ TRunStepBlockListDataCache }
constructor TRunStepBlockListDataCache.Create(const aPathName: string);
begin
    inherited Create();
    fReaderWriter := TXMLReaderWriter.Create(aPathName, cAutoFlushBufferCycle);
    fRunStepBlocks := nil;
end;

destructor TRunStepBlockListDataCache.Destroy;
begin
    FreeAndNil(fRunStepBlocks);
    fReaderWriter.Free;
    inherited;
end;

function TRunStepBlockListDataCache.FindRunStepBlock(const aBlockDataID: string): TRunStepBlockData;
var
    x: integer;
begin
    result := nil;
    for x := 0 to fRunStepBlocks.List.Count - 1 do
    begin
        if SameText((fRunStepBlocks.List.Items[x] as TRunStepBlockData).BlockDataID, aBlockDataID) then
        begin
            result := fRunStepBlocks.List.Items[x] as TRunStepBlockData;
            EXIT;
        end;
    end;

end;

procedure TRunStepBlockListDataCache.Flush;
begin
    fReaderWriter.WriteToFile();
end;

procedure TRunStepBlockListDataCache.AddRunStepBlock(const aRunStepBlockData: TRunStepBlockData);
begin
    fRunStepBlocks.List.Add(aRunStepBlockData);
    // aRunStepBlockData.ID := IntToStr( fRunStepBlocks.List.Count - 1 );
    fReaderWriter.DataChanged();
    // fReaderWriter.AddObjectToItemsVirtualObject( aRunStepBlockData, fRunStepBlocks.List.Tag );
    // outputdebugstring( pchar( 'AddRunStepBlock' + IntToStr( Longint(aRunStepBlockData.RunSteps.List.Tag) ) ) );
end;

procedure TRunStepBlockListDataCache.RemoveRunStepBlock(const aBlockDataID: string);
var
    xRunStepBlockData: TRunStepBlockData;
begin
    xRunStepBlockData := FindRunStepBlock(aBlockDataID);
    fReaderWriter.DataChanged();
    // fReaderWriter.RemoveObjectFromItemsVirtualObject( xRunStepBlockData, fRunStepBlocks.List.Tag );
    fRunStepBlocks.List.Remove(xRunStepBlockData);
end;

procedure TRunStepBlockListDataCache.AddStepsToRunStepBlock(const aBlockDataID: string;
    const aRunSteps: TRunStepList);
var
    xRunStepBlockData: TRunStepBlockData;
    xRunStep: TRunStep;
    x: integer;
begin
    xRunStepBlockData := FindRunStepBlock(aBlockDataID);
    ASSERT(Assigned(xRunStepBlockData));

    for x := 0 to aRunSteps.Count - 1 do
    begin
        xRunStep := aRunSteps[x];
        xRunStepBlockData.RunSteps.AddStep(xRunStep);
        // xRunStep.ID := IntToStr( xRunStepBlockData.RunSteps.List.Count - 1 );
        fReaderWriter.DataChanged();
        // fReaderWriter.AddObjectToItemsVirtualObject( xRunStep, xRunStepBlockData.RunSteps.List.Tag  );
    end;

end;

procedure TRunStepBlockListDataCache.RemoveLastStepsFromRunStepBlock(const aBlockDataID: string;
    const aNumSteps: integer);
var
    xRunStepBlockData: TRunStepBlockData;
    xTotalSteps: integer;
    x: integer;
    xRunStep: TRunStep;
begin
    xRunStepBlockData := FindRunStepBlock(aBlockDataID);
    ASSERT(Assigned(xRunStepBlockData));
    xTotalSteps := xRunStepBlockData.RunSteps.List.Count;
    // outputdebugstring( pchar( 'RemoveLast' + IntToStr( Longint( xRunStepBlockData.RunSteps.List.Tag ) ) ) );
    for x := xTotalSteps - 1 downto xTotalSteps - aNumSteps do
    begin
        xRunStep := xRunStepBlockData.RunSteps[x];
        ASSERT(Assigned(xRunStep));
        fReaderWriter.DataChanged();
        // fReaderWriter.RemoveObjectFromItemsVirtualObject( xRunStep, xRunStepBlockData.RunSteps.List.Tag  );
        xRunStepBlockData.RunSteps.RemoveStep(xRunStep);
    end;

    // fReaderWriter.SetObjectToNode( xRunStepBlockData.RunSteps.List, xRunStepBlockData.RunSteps.List.Tag);
end;

function TRunStepBlockListDataCache.Read(): boolean;
begin
    result := false;
    if not fReaderWriter.ReadFromFile() then
        EXIT;
    fRunStepBlocks := fReaderWriter.CreateObjectFromRootNode<TRunStepBlockListData>();

    if not Assigned(fRunStepBlocks) then
        EXIT;

    result := true;
end;

procedure TRunStepBlockListDataCache.Init;
begin
    read();
    if not Assigned(fRunStepBlocks) then
    begin
        fRunStepBlocks := TRunStepBlockListData.Create();
        fReaderWriter.AddObjectToRootNode(fRunStepBlocks);
    end;
end;

// function TRunStepBlockListDataCache.GetCount: integer;
// begin
// result := fRunStepBlocks.List.Count;
// end;
//
//
// function TRunStepBlockListDataCache.GetRunStepBlockDataAt(
// aIndex: integer): TRunStepBlockData;
// begin
// result := fRunStepBlocks.List[ aIndex ] as TRunStepBlockData;
// end;

procedure TRunStepBlockListDataCache.Clear;
begin
    FreeAndNil(fRunStepBlocks);
    fReaderWriter.DisActivate();
end;

{ TRunStepBlockData }

constructor TRunStepBlockData.Create(const aBlockDataID: string; const aBlockOwnerID: string;
    const aBlockTypeName: string; const aBlockParams: TRunStepBlockParams);
begin
    inherited Create();
    fBlockDataID := aBlockDataID;
    fBlockOwnerID := aBlockOwnerID;
    fBlockTypeName := aBlockTypeName;
    fBlockParams := aBlockParams;
    fRunSteps := TRunStepList.Create(false);
end;

constructor TRunStepBlockData.Create();
begin
    Create('', '', '', nil);
end;

destructor TRunStepBlockData.Destroy();
begin
    FreeAndNil(fRunSteps);
    inherited;
end;


// initialization
// RegisterClasses( [ TRunStepBlockData, TRunStepBlockListData ] );
end.
