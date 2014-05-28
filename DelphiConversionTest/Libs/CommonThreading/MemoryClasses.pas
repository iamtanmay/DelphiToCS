{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  06.11.08 pk                                        TN4280    Initial Revision
  10.11.08 pk                                        TN4280    AddStringIdent removed
  17.02.09 pk  TIdentValue                           TN4232    New
  04.11.09 pk                                  	     TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  01.03.12 wl                                        TN5822   uses Streamable statt ArgClass
  01.03.12 wl  TIntListItem, TIntMap                 TN5822   von IntMap hierher
  27.03.13 wl                                        TN6045   uses Generics.Collections
  25.06.13 wl  TIdentItem, TIdentList, THeap         TN6178   --> Identifier.pas
  ----------------------------------------------------------------------------------------------------------------------- }

unit MemoryClasses;


interface


uses
    SyncObjs,
    Generics.Collections,
    GeneralTypes,
    Identifier,
    ListClasses,
    Streamable;

type
    TIntMap = class
    private
        fList: TList<integer>;
        function GetCount(): integer;
    public
        constructor Create( { const aSorted : boolean } );
        destructor Destroy(); override;
        function IndexOf(const aInt: integer): integer;
        procedure InsertInts(aInsertAt: integer; const aInts: TIntArray);
        procedure AddInts(const aInts: TIntArray);
        procedure DeleteInts(const aInts: TIntArray);
        function ContainsInt(const aInt: integer): boolean;
        procedure Clear();
        function ToArray(): TArray<integer>;
        property Count: integer read GetCount;
    end;

    TMemAddress = class
    public
        class function BOFAddress(): integer;
    end;

    TRelativeMemAddress = class(TMemAddress)
    private
        fLabelName: string;
        fRelativeAddress: integer;
    public
        constructor Create();
        procedure Increment();
        property LabelName: string read fLabelName write fLabelName;
        property RelativeAddress: integer read fRelativeAddress write fRelativeAddress;
    end;

    TCallStackFrame = class
    private
        fReturnAddress: TRelativeMemAddress;
        fCallArgs: TIdentifierList;
        fLocalVars: TIdentifierList;
        fReturnKeyNames: TList<string>;
        fReturnKeyValues: TList<TStreamableItem>;
    public
        constructor Create();
        destructor Destroy; override;
        property ReturnAddress: TRelativeMemAddress read fReturnAddress;
        property CallArgs: TIdentifierList read fCallArgs;
        property LocalVars: TIdentifierList read fLocalVars;
        property ReturnKeyNames: TList<string>read fReturnKeyNames;
        property ReturnKeyValues: TList<TStreamableItem>read fReturnKeyValues;
    end;

    TCallStackFrameList = class
    private
        fList: TObjectList<TCallStackFrame>;
        function Getthis(aIndex: integer): TCallStackFrame;
        function GetCount: integer;
        function GetLastIndex: integer;
        property LastIndex: integer read GetLastIndex;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure Add(aFrame: TCallStackFrame);
        procedure DeleteLast();
        function GetLast(): TCallStackFrame;
        property this[aIndex: integer]: TCallStackFrame read Getthis; default;
        property Count: integer read GetCount;
    end;

    TCallStack = class
    private
        fStackFrames: TCallStackFrameList;
        fCurrentFrame: TCallStackFrame;
    public
        constructor Create();
        destructor Destroy(); override;
        function Peek(): TCallStackFrame;
        procedure Push();
        procedure Pop();
        function IsEmpty(): boolean;
        property StackFrames: TCallStackFrameList read fStackFrames;
        property CurrentFrame: TCallStackFrame read fCurrentFrame;
    end;

    // überflüssige Klasse, die durch TStreamableObjectList ersetzt werden sollte
    TMsgArgs = class(TStreamable)
    private
        fList: TStreamableObjectList;
        function GetItem(aIndex: integer): TStreamableItem;
        procedure SetItem(aIndex: integer; aItem: TStreamableItem);
        function GetCount: integer;
        function GetCapacity: integer;
        procedure SetCapacity(const Value: integer);
    public
        constructor Create; override;
        destructor Destroy; override;
        class function ArgsArrayOf(const aArgs: array of TStreamableItem): TMsgArgs;
        class function EmptyArgs(): TMsgArgs;
        procedure Delete(const aIndex: integer);
        function Add(aItem: TStreamableItem): integer;
        property this[aIndex: integer]: TStreamableItem read GetItem write SetItem; default;
        property Count: integer read GetCount;
    published
        property List: TStreamableObjectList read fList write fList;
        property Capacity: integer read GetCapacity write SetCapacity;
    end;


implementation


uses
    SysUtils;

{ TIntMap }

constructor TIntMap.Create();
begin
    inherited Create;
    fList := TList<integer>.Create();
end;

destructor TIntMap.Destroy();
begin
    FreeAndNil(fList);

    inherited;
end;

function TIntMap.IndexOf(const aInt: integer): integer;
begin
    result := fList.IndexOf(aInt);
end;

procedure TIntMap.AddInts(const aInts: TIntArray);
var
    i: integer;
begin
    for i := 0 to high(aInts) do
    begin
        fList.Add(aInts[i]);
    end;
end;

procedure TIntMap.InsertInts(aInsertAt: integer; const aInts: TIntArray);
var
    i: integer;
begin
    for i := 0 to high(aInts) do
    begin
        fList.Insert(aInsertAt + i, aInts[i]);
    end;
end;

procedure TIntMap.DeleteInts(const aInts: TIntArray);
var
    i, xIndex: integer;
begin
    for i := 0 to high(aInts) do
    begin
        xIndex := IndexOf(aInts[i]);
        if xIndex < 0 then
            CONTINUE;
        fList.Delete(xIndex);
    end;
end;

function TIntMap.ContainsInt(const aInt: integer): boolean;
begin
    result := IndexOf(aInt) >= 0;
end;

procedure TIntMap.Clear();
begin
    fList.Clear;
end;

function TIntMap.ToArray(): TIntArray;
begin
    EXIT(fList.ToArray);
end;

function TIntMap.GetCount(): integer;
begin
    result := fList.Count;
end;

{ TCallStackFrameList }

constructor TCallStackFrameList.Create;
begin
    inherited Create();
    fList := TObjectList<TCallStackFrame>.Create(true);
end;

destructor TCallStackFrameList.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

procedure TCallStackFrameList.Add(aFrame: TCallStackFrame);
begin
    fList.Add(aFrame)
end;

function TCallStackFrameList.GetLastIndex: integer;
begin
    result := self.Count - 1;
end;

procedure TCallStackFrameList.DeleteLast;
begin
    fList.Delete(self.LastIndex);
end;

function TCallStackFrameList.GetCount: integer;
begin
    result := fList.Count;
end;

function TCallStackFrameList.GetLast: TCallStackFrame;
begin
    result := nil;
    if self.Count = 0 then
        EXIT;
    result := self[self.LastIndex];
end;

function TCallStackFrameList.Getthis(aIndex: integer): TCallStackFrame;
begin
    ASSERT(((aIndex >= 0) and (aIndex < self.Count)), Format('TCallStackFrameList: index %d out of bounds',
        [aIndex]));;
    result := fList[aIndex];
end;

{ TCallStackFrame }

constructor TCallStackFrame.Create;
begin
    inherited Create();
    fReturnAddress := TRelativeMemAddress.Create;
    fCallArgs := TIdentifierList.Create;
    fLocalVars := TIdentifierList.Create;
    fReturnKeyNames := TList<string>.Create;
    fReturnKeyValues := TList<TStreamableItem>.Create;
end;

destructor TCallStackFrame.Destroy();
begin
    FreeAndNil(fReturnAddress);
    FreeAndNil(fLocalVars);
    FreeAndNil(fCallArgs);
    FreeAndNil(fReturnKeyNames);
    FreeAndNil(fReturnKeyValues);
    inherited;
end;

{ TCallStack }

constructor TCallStack.Create;
begin
    inherited Create();
    fStackFrames := TCallStackFrameList.Create();
    fCurrentFrame := nil;
end;

destructor TCallStack.Destroy;
begin
    FreeAndNil(fStackFrames);
    inherited;
end;

procedure TCallStack.Push();
begin
    fCurrentFrame := TCallStackFrame.Create();
    fStackFrames.Add(fCurrentFrame);
end;

procedure TCallStack.Pop();
begin
    fStackFrames.DeleteLast();
    fCurrentFrame := self.Peek();
end;

function TCallStack.Peek: TCallStackFrame;
begin
    result := fStackFrames.GetLast();
end;

function TCallStack.IsEmpty: boolean;
begin
    result := fStackFrames.Count = 0;
end;

{ TMemAddress }

class function TMemAddress.BOFAddress: integer;
begin
    result := -1;
end;

{ TRelativeMemAddress }

constructor TRelativeMemAddress.Create;
begin
    inherited Create();
    fLabelName := '';
    fRelativeAddress := BOFAddress;
end;

procedure TRelativeMemAddress.Increment;
begin
    Inc(fRelativeAddress);
end;

{ TMsgArgs }

function TMsgArgs.GetCapacity: integer;
begin
    result := fList.Capacity;
end;

function TMsgArgs.GetCount: integer;
begin
    result := fList.Count;
end;

function TMsgArgs.GetItem(aIndex: integer): TStreamableItem;
var
    xItem: TCustomStreamable;
begin
    result := nil;
    xItem := fList[aIndex];
    if not Assigned(xItem) then
        EXIT;

    result := xItem as TStreamableItem;
end;

procedure TMsgArgs.SetCapacity(const Value: integer);
begin
    fList.Capacity := Value;
end;

procedure TMsgArgs.SetItem(aIndex: integer; aItem: TStreamableItem);
begin
    fList.Items[aIndex] := aItem;
end;

constructor TMsgArgs.Create;
begin
    inherited Create;

    fList := TStreamableObjectList.Create();
end;

procedure TMsgArgs.Delete(const aIndex: integer);
begin
    fList.Remove(fList[aIndex]);
end;

destructor TMsgArgs.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

class function TMsgArgs.EmptyArgs(): TMsgArgs;
begin
    result := nil;
end;

function TMsgArgs.Add(aItem: TStreamableItem): integer;
begin
    fList.Add(aItem);
    result := fList.Count - 1;
end;

class function TMsgArgs.ArgsArrayOf(const aArgs: array of TStreamableItem): TMsgArgs;
var
    i: integer;
    x: TStreamableItem;
begin
    result := TMsgArgs.Create();
    // result.Capacity := High( aArgs ) + 1;
    for i := 0 to high(aArgs) do
    begin
        x := aArgs[i];
        result.Add(x);
    end;
end;


end.
