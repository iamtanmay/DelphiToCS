{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.02.10 pk                                        TN4972     Initial Revision
  08.04.10 pk  TRunStepBlockParams                   TN4996     New: Each block should create its own special param. (first used for MultiPip)
  11.07.12 ts  TRunStepBlockList.FindByID            TN5933     List index out of bounds error avoided by try except block
  27.02.13 wl                                        TN6045   uses Generics.Collections
  27.03.13 wl                                        TN6045   uses geändert
  14.08.13 wl                                        TN6218   verwendet TRunStepListIterator
  ----------------------------------------------------------------------------------------------------------------------- }

unit RunStepBlock;


interface


uses
    Generics.Collections,
    RunStep,
    Streamable;

const
    cBlockTypeNameAll = '*';

type
    TRunStepBlockClass = class of TRunStep;

    TRunStepBlockParams = class(TStreamable)
    end;

    TRunStepBlock = class
    private
        fOwnerID: string;
        fBlockTypeName: string;
        fRunStepList: TRunStepList;
        fRunStepListIterator: TRunStepListIterator;
        fRunStepBlockParams: TRunStepBlockParams;
    public
        constructor Create(const aOwnerID: string; const aBlockTypeName: string;
            const aRunStepBlockParams: TRunStepBlockParams);
        destructor Destroy(); override;
        function IsSameBlockTypeName(const aBlockTypeName: string): boolean;
        property OwnerID: string read fOwnerID;
        property BlockTypeName: string read fBlockTypeName;
        property RunStepList: TRunStepList read fRunStepList;
        property RunStepListIterator: TRunStepListIterator read fRunStepListIterator
            write fRunStepListIterator;
        property RunStepBlockParams: TRunStepBlockParams read fRunStepBlockParams;
    end;

    TRunStepBlockList = class(TObjectList<TRunStepBlock>)
    public
        procedure Extract(aIndex: integer);
        function IndexOf(const aOwnerID: string; const aBlockTypeName: string): integer;
        function FindByID(const aOwnerID: string; const aBlockTypeName: string; aMustFind: boolean)
            : TRunStepBlock;
        procedure AddItems(aItems: TRunStepBlockList);
    end;


implementation


uses
    SysUtils,
    Classes;

{ TRunStepBlock }

constructor TRunStepBlock.Create(const aOwnerID: string; const aBlockTypeName: string;
    const aRunStepBlockParams: TRunStepBlockParams);
begin
    inherited Create();
    fOwnerID := aOwnerID;
    fBlockTypeName := aBlockTypeName;
    fRunStepBlockParams := aRunStepBlockParams;
    fRunStepList := TRunStepList.Create(false);
    fRunStepListIterator := TRunStepListIterator.Create(fRunStepList);
end;

destructor TRunStepBlock.Destroy;
begin
    fRunStepList.Free;
    inherited;
end;

function TRunStepBlock.IsSameBlockTypeName(const aBlockTypeName: string): boolean;
begin
    result := (aBlockTypeName = cBlockTypeNameAll) or SameText(fBlockTypeName, aBlockTypeName);
end;

{ TRunStepBlockList }

function TRunStepBlockList.IndexOf(const aOwnerID: string; const aBlockTypeName: string): integer;
var
    x: integer;
begin
    result := -1;
    for x := 0 to self.Count - 1 do
    begin
        if SameText(self[x].OwnerID, aOwnerID) and self[x].IsSameBlockTypeName(aBlockTypeName) then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

function TRunStepBlockList.FindByID(const aOwnerID: string; const aBlockTypeName: string; aMustFind: boolean)
    : TRunStepBlock;
var
    xIndex: integer;
begin
    result := nil;
    try
        xIndex := self.IndexOf(aOwnerID, aBlockTypeName);
    except
        xIndex := -1;
    end;
    if xIndex < 0 then
    begin
        if aMustFind then
            raise Exception.CreateFmt('RunStep Block %s could not be found', [aOwnerID]);
        EXIT;
    end;

    result := self[xIndex];
end;

procedure TRunStepBlockList.Extract(aIndex: integer);
begin
    ASSERT(aIndex >= 0);
    inherited Extract(self[aIndex]);
end;

procedure TRunStepBlockList.AddItems(aItems: TRunStepBlockList);
var
    x: integer;
begin
    for x := 0 to aItems.Count - 1 do
    begin
        self.Add(aItems[x]);
    end;
end;


end.
