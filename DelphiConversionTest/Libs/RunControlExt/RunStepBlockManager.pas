{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  19.06.09 pk                                        TN4538    Initial Revision
  21.07.09 pk  TRunStepBlock.BlockType               TN4668    New
  25.08.09 pk  EndAllBlocksForCurrentThread          TN4745    New
  21.09.09 pk                                        TN4791    Exception is raised if Block begin is called when a block already exists
  04.02.10 pk                                        TN4972    Changes for Restart
  14.04.11 wl  EndAllBlocksForCurrentThread          TN5553    Parameter "MustFind" für DestroyBlockForID auf false gesetzt
  19.05.11 ts  EndAllBlocksForCurrentThread          TN5585    Schleife darf maximal so oft durchlaufen werden, wie fList lang ist, sonst Endlosschleife möglich
  27.03.13 wl                                        TN6045   uses geändert
  15.08.13 wl                                        TN6223   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunStepBlockManager;


interface


uses
    RunStepBlock,
    RunStep;

type
    TRunStepBlockManager = class
    private
        fList: TRunStepBlockList;
        class var uInstance: TRunStepBlockManager;
        constructor Create();
        procedure CreateBlockForID(const aBlockOwnerID: string; const aBlockTypeName: string;
            const aParams: TRunStepBlockParams; const aTrace: boolean);
        procedure DestroyBlockForID(const aBlockOwnerID: string; const aBlockTypeName: string;
            const aMustFind: boolean; const aTrace: boolean);
        function GetBlockIDForCurrentThread(): string;
        function GetBlockTypeName(const aBlockType: TRunStepBlockClass): string;
        function GetBlockForCurrentThread(const aBlockType: TRunStepBlockClass): TRunStepBlock;
    public
        destructor Destroy(); override;
        procedure BeginBlock(const aBlockOwnerID: string; const aBlockTypeName: string;
            const aParams: TRunStepBlockParams; const aTrace: boolean);
        procedure EndBlock(const aBlockOwnerID: string; const aBlockTypeName: string; const aTrace: boolean);
        function AddStepsToBlock(const aBlockOwnerID: string; const aBlockTypeName: string;
            const aSteps: TRunStepList; const aTrace: boolean): boolean;
        procedure BeginBlockForCurrentThread(const aBlockType: TRunStepBlockClass;
            const aParams: TRunStepBlockParams);
        procedure EndBlockForCurrentThread(const aBlockType: TRunStepBlockClass);
        function BlockExistsForCurrentThread(const aBlockType: TRunStepBlockClass): boolean;
        function AddStepsForCurrentThread(const aBlockType: TRunStepBlockClass;
            const aSteps: TRunStepList): boolean;
        procedure CopyStepsForCurrentThread(const aBlockType: TRunStepBlockClass;
            const aTargetRunSteps: TRunStepList; out oBlockParams: TRunStepBlockParams); // overload;
        procedure EndAllBlocksForCurrentThread();
        class procedure CreateInstance();
        class procedure DestroyInstance();
        class property Instance: TRunStepBlockManager read uInstance;
    end;


implementation


uses
    SysUtils,
    ThreadAPI,
    RunTraceManager,
    TypeMapTranslator;

{ TRunStepBlockManager }

constructor TRunStepBlockManager.Create;
begin
    inherited Create();
    fList := TRunStepBlockList.Create(true);
end;

destructor TRunStepBlockManager.Destroy;
begin
    fList.Free;
    inherited;
end;

function TRunStepBlockManager.GetBlockIDForCurrentThread(): string;
begin
    result := TThreadAPI.GetCurrentThreadDescription();
end;

function TRunStepBlockManager.GetBlockTypeName(const aBlockType: TRunStepBlockClass): string;
begin
    result := aBlockType.ClassName;
end;

procedure TRunStepBlockManager.CreateBlockForID(const aBlockOwnerID: string; const aBlockTypeName: string;
    const aParams: TRunStepBlockParams; const aTrace: boolean);
var
    xBlock: TRunStepBlock;
begin
    xBlock := TRunStepBlock.Create(aBlockOwnerID, aBlockTypeName, aParams);
    fList.Add(xBlock);

    if aTrace then
        TRunTraceManager.Instance.AddBlock(xBlock);
end;

procedure TRunStepBlockManager.DestroyBlockForID(const aBlockOwnerID: string; const aBlockTypeName: string;
    const aMustFind: boolean; const aTrace: boolean);
var
    xBlock: TRunStepBlock;
begin
    xBlock := fList.FindByID(aBlockOwnerID, aBlockTypeName, aMustFind);
    if not Assigned(xBlock) then
        EXIT;

    if aTrace then
        TRunTraceManager.Instance.RemoveBlock(xBlock);
    fList.Remove(xBlock);
end;

function TRunStepBlockManager.GetBlockForCurrentThread(const aBlockType: TRunStepBlockClass): TRunStepBlock;
var
    xBlockID: string;
    xBlockTypeName: string;
begin
    xBlockID := self.GetBlockIDForCurrentThread();
    xBlockTypeName := GetBlockTypeName(aBlockType);
    result := fList.FindByID(xBlockID, xBlockTypeName, false);
end;

procedure TRunStepBlockManager.BeginBlock(const aBlockOwnerID: string; const aBlockTypeName: string;
    const aParams: TRunStepBlockParams; const aTrace: boolean);
var
    xBlock: TRunStepBlock;
begin
    xBlock := fList.FindByID(aBlockOwnerID, aBlockTypeName, false);
    if Assigned(xBlock) then
        raise Exception.Create('Cannot create a new block, a block already exists');

    CreateBlockForID(aBlockOwnerID, aBlockTypeName, aParams, aTrace);
end;

procedure TRunStepBlockManager.EndBlock(const aBlockOwnerID: string; const aBlockTypeName: string;
    const aTrace: boolean);
var
    xBlock: TRunStepBlock;
begin
    xBlock := fList.FindByID(aBlockOwnerID, aBlockTypeName, false);
    if not Assigned(xBlock) then
        raise Exception.Create('Cannot delete the block, no block was found');

    DestroyBlockForID(aBlockOwnerID, aBlockTypeName, true, aTrace);
end;

procedure TRunStepBlockManager.EndAllBlocksForCurrentThread();
const
    cTraceWhenEndAllBlocks = false;
var
    xBlockID: string;
    i: integer;
begin
    i := fList.Count;
    xBlockID := self.GetBlockIDForCurrentThread();
    while ((fList.Count > 0) and (i > 0)) do
    begin
        DestroyBlockForID(xBlockID, cBlockTypeNameAll, false, cTraceWhenEndAllBlocks);
        Dec(i);
    end;
end;

procedure TRunStepBlockManager.BeginBlockForCurrentThread(const aBlockType: TRunStepBlockClass;
    const aParams: TRunStepBlockParams);
var
    xBlockOwnerID: string;
    xBlockTypeName: string;
begin
    xBlockOwnerID := self.GetBlockIDForCurrentThread();
    xBlockTypeName := GetBlockTypeName(aBlockType);
    BeginBlock(xBlockOwnerID, xBlockTypeName, aParams, true);
end;

function TRunStepBlockManager.BlockExistsForCurrentThread(const aBlockType: TRunStepBlockClass): boolean;
var
    xBlock: TRunStepBlock;
begin
    xBlock := GetBlockForCurrentThread(aBlockType);
    result := Assigned(xBlock);
end;

procedure TRunStepBlockManager.EndBlockForCurrentThread(const aBlockType: TRunStepBlockClass);
var
    xBlockOwnerID: string;
    xBlockTypeName: string;
begin
    xBlockOwnerID := self.GetBlockIDForCurrentThread();
    xBlockTypeName := GetBlockTypeName(aBlockType);
    EndBlock(xBlockOwnerID, xBlockTypeName, true);
end;

function TRunStepBlockManager.AddStepsToBlock(const aBlockOwnerID: string; const aBlockTypeName: string;
    const aSteps: TRunStepList; const aTrace: boolean): boolean;
var
    xBlock: TRunStepBlock;
begin
    xBlock := fList.FindByID(aBlockOwnerID, aBlockTypeName, false);
    result := Assigned(xBlock);
    if not Assigned(xBlock) then
        EXIT;
    xBlock.RunStepList.AddSteps(aSteps);
    if aTrace then
        TRunTraceManager.Instance.AddStepsToBlock(xBlock, aSteps);
end;

function TRunStepBlockManager.AddStepsForCurrentThread(const aBlockType: TRunStepBlockClass;
    const aSteps: TRunStepList): boolean;
var
    xBlockOwnerID: string;
    xBlockTypeName: string;
begin
    xBlockOwnerID := self.GetBlockIDForCurrentThread();
    xBlockTypeName := GetBlockTypeName(aBlockType);
    result := AddStepsToBlock(xBlockOwnerID, xBlockTypeName, aSteps, true);
end;

procedure TRunStepBlockManager.CopyStepsForCurrentThread(const aBlockType: TRunStepBlockClass;
    const aTargetRunSteps: TRunStepList; out oBlockParams: TRunStepBlockParams);
var
    xBlock: TRunStepBlock;
begin
    xBlock := GetBlockForCurrentThread(aBlockType);
    if not Assigned(xBlock) then
        EXIT;
    aTargetRunSteps.AddSteps(xBlock.RunStepList);
    oBlockParams := TObjectCopy<TRunStepBlockParams>.Copy(xBlock.RunStepBlockParams);
end;

class procedure TRunStepBlockManager.CreateInstance;
begin
    uInstance := TRunStepBlockManager.Create();
end;

class procedure TRunStepBlockManager.DestroyInstance;
begin
    FreeAndNil(uInstance);
end;


end.
