{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.06.05 pk                               TN2449 Initial revision
  05.07.05 pk  RemoveNonInputIdents         TN2494 New : remove idents from the list which do not require use input
  05.07.05 pk  GetIdentsNeedingInput        TN2494 New parmas : checkhasvalue, checkNonInput
  15.08.05 pk  GetIdentsNeedingInput        TN2560 Bug : some parameters were regarded as noninput although they were input parameters
  25.08.05 pk  TParserIdentPair             TN2547 fStoredIdent type changed TParserStoredIdent
  18.11.05 pk  IdentsNeedingInput           TN2779 call function CanUseValue instead of HasValue
  20.02.07 wl                               TN3016   entfernt: uses ParserWrapper
  21.02.07 pk  TParserIdentPairList         TN3583  fList changed from TStringList to TObjectList. Items are now freed when fList is freed
  27.08.07 pk  TParserResourceManager       TN3788  New
  27.08.07 pk  TParserScope                 TN3788  New
  27.08.07 pk  IParserSymbolTable           TN3788  moved to ParserIdentifier
  06.11.08 pk                               TN4279   uses ParserIdentDataType
  07.01.09 pk  TParserResourceManager       TN4279  removed
  07.01.09 pk                               TN4380.1 various functions removed
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  07.05.10 pk                               TN5092   changes needed for new array type
  01.03.12 wl                               TN5822   TArg statt TAttrValue
  14.12.12 wl                               TN6054   STR_GLOBAL_METHOD_NAME von ParserIdentifier hierher
  20.02.13 wl                               TN6055   Struktur überarbeitet und z.T. vereinfacht
  22.02.13 wl  IdentsNeedingInput           TN6094   Value kann nicht mehr von ParserStoredIdentifier übernommen werden
  15.03.13 wl  RemoveNonInputIdents         TN6055   Bugfix
  25.06.13 wl  TParserIdentifierUtils       TN6178   IsNonInputIdent,NeedsInput von ParserIdentifier hierher
  -------------------------------------------------------------------------------------------------- }

unit ParserSymbolTable;


interface


uses
    Generics.Collections,
    ParserIdentifier,
    ParserIdentDataType;

type
    TParserIdentTypes = set of TParserIdentType;

    TParserIdentifierUtils = record
    public
        class function IsNonInputIdent(aIdent: TParserIdentifier): boolean; static;
        class function NeedsInput(aIdent: TParserIdentifier): boolean; static;
    end;

    TInputNeededCheck = (vnNoCheck, vnInputNeeded, vnInputNotNeeded);

    TParserIdentifierList = class(TObjectList<TParserIdentifier>)
    private
        fScopeName: string;
        function IdentIndexByName(const aIdentName: string): integer;
    public
        constructor Create(const aScopeName: string);
        class function CreateFromList(aIdentifierList: TParserIdentifierList): TParserIdentifierList;
        destructor Destroy(); override;
        //
        function IdentByName(const aIdentName: string): TParserIdentifier;
        procedure SetIdentifierValue(const aIdentName: string; const aValue: TArg);
        procedure SetIdentifierValueAt(const aIndex: integer; const aValue: TArg);
        procedure RemoveNonInputIdents();
        procedure ClearIdentValues();

        procedure AddIdentifier(const aIdentName: string; const aValue: TArg);
        procedure IdentsNeedingInput(aResultList: TList<TParserIdentifier>;
            const aCheckHasValue, aCheckNonInputIdent: boolean);
        //
        property ScopeName: string read fScopeName write fScopeName;
    end;

    TParserScope = class
    private
        fIdentScope: TParserIdentifierList;
        fOwnsIdentScope: boolean;
    public
        constructor Create(aIdentScope: TParserIdentifierList; aOwnsIdentScope: boolean);
        destructor Destroy(); override;
        property IdentScope: TParserIdentifierList read fIdentScope;
    end;

    TSymbolTable = class
    private const
        STR_GLOBAL_METHOD_NAME = '_GLOBAL_METHOD_';
    private
        fGlobalScope: TParserScope;
        fLocalScope: TParserScope;
        fScopeStack: TStack<TParserScope>;
        function GetGlobalList: TParserIdentifierList;
        function GetLocalList: TParserIdentifierList;
    public
        constructor Create();
        destructor Destroy(); override;
        //
        procedure BeginScope(aIdentScope: TParserIdentifierList);
        function EndScope(): TParserIdentifierList;
        procedure ClearAllIdentifierValues();
        function IdentsNeedingInput(aCheckHasValue, aCheckNonInputIdent: boolean): TArray<TParserIdentifier>;
        procedure RemoveNonInputIdents();
        //
        property GlobalList: TParserIdentifierList read GetGlobalList;
        property LocalList: TParserIdentifierList read GetLocalList;
        property LocalScope: TParserScope read fLocalScope;
    end;


implementation


uses
    SysUtils,
    TypeMapTranslator;

{ TParserIdentifierUtils }

class function TParserIdentifierUtils.NeedsInput(aIdent: TParserIdentifier): boolean;
begin
    result := false;

    // check if value exists or is not necessary
    if (aIdent.Value <> nil) or IsNonInputIdent(aIdent) then
        EXIT;

    result := true;
end;

class function TParserIdentifierUtils.IsNonInputIdent(aIdent: TParserIdentifier): boolean;
const
    CHR_IDENT_RUNTYPE = '!';
    CHRS_IDENT_INTEGER = ['0' .. '9'];
    CHR_IDENT_NON_INPUT_TYPE = 'c';
var
    xIdentLen: integer;
    xIdentType: TParserIdentType;
begin
    xIdentType := TParserIdentifier.GetIdentType(aIdent.Name);
    if xIdentType = itLocal then
    begin
        // Fall 1: _$1xyz
        xIdentLen := Length(aIdent.Name);
        if (xIdentLen >= (3)) and (CharInSet(aIdent.Name[(3)], CHRS_IDENT_INTEGER)) then
            EXIT(true);

        // Fall 2: _$c1
        if (xIdentLen >= (4)) and (aIdent.Name[3] = CHR_IDENT_NON_INPUT_TYPE) and
            (CharInSet(aIdent.Name[(4)], CHRS_IDENT_INTEGER)) then
            EXIT(true);
    end
    else
    begin
        // Fall 1: _1xyz
        xIdentLen := Length(aIdent.Name);
        if (xIdentLen >= (2)) and (CharInSet(aIdent.Name[(2)], CHRS_IDENT_INTEGER)) then
            EXIT(true);

        // Fall 2: _c1
        if (xIdentLen >= (3)) and (aIdent.Name[2] = CHR_IDENT_NON_INPUT_TYPE) and
            (CharInSet(aIdent.Name[(3)], CHRS_IDENT_INTEGER)) then
            EXIT(true);

        // Fall 3: _!xyz
        if (xIdentLen >= 2) and (aIdent.Name[2] = CHR_IDENT_RUNTYPE) then
            EXIT(true);
    end;

    EXIT(false);
end;

{ TParserIdentifierList }

constructor TParserIdentifierList.Create(const aScopeName: string);
begin
    inherited Create(true);

    fScopeName := aScopeName;
end;

destructor TParserIdentifierList.Destroy();
begin
    self.Clear;

    inherited;
end;

function TParserIdentifierList.IdentIndexByName(const aIdentName: string): integer;
var
    i: integer;
begin
    result := -1;
    for i := 0 to self.Count - 1 do
    begin
        if SameText(aIdentName, self[i].Name) then
        begin
            result := i;
            EXIT;
        end;
    end;
end;

function TParserIdentifierList.IdentByName(const aIdentName: string): TParserIdentifier;
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
    begin
        if SameText(aIdentName, self[x].Name) then
            EXIT(self[x]);
    end;
    EXIT(nil);
end;

procedure TParserIdentifierList.AddIdentifier(const aIdentName: string; const aValue: TArg);
var
    xSamIdentifier: TParserIdentifier;
    xIndex: integer;
begin
    // search for ident name in list
    xIndex := self.IdentIndexByName(aIdentName);

    if xIndex < 0 then
    begin
        // ident name does not exist -> create new ident
        xSamIdentifier := TParserIdentifier.Create(aIdentName);
        if aValue <> nil then
            xSamIdentifier.Value := aValue;
        self.Add(xSamIdentifier);
    end
    else
    begin
        // set value of known identifier
        if aValue <> nil then
            SetIdentifierValueAt(xIndex, aValue);
    end;
end;

procedure TParserIdentifierList.SetIdentifierValue(const aIdentName: string; const aValue: TArg);
var
    i: integer;
begin
    i := self.IdentIndexByName(aIdentName);
    SetIdentifierValueAt(i, aValue);
end;

procedure TParserIdentifierList.SetIdentifierValueAt(const aIndex: integer; const aValue: TArg);
var
    xIdentifier: TParserIdentifier;
begin
    xIdentifier := self[aIndex];
    xIdentifier.Value := aValue;
end;

procedure TParserIdentifierList.RemoveNonInputIdents();
var
    i: integer;
    xIdentifier: TParserIdentifier;
begin
    for i := self.Count - 1 downto 0 do
    begin
        xIdentifier := self[i];
        if not TParserIdentifierUtils.IsNonInputIdent(xIdentifier) then
            CONTINUE;
        self.Delete(i);
    end;
end;

procedure TParserIdentifierList.ClearIdentValues();
var
    i: integer;
    xIdentifier: TParserIdentifier;
begin
    for i := 0 to self.Count - 1 do
    begin
        xIdentifier := self[i];
        xIdentifier.ClearValue;
    end;
end;

class function TParserIdentifierList.CreateFromList(aIdentifierList: TParserIdentifierList)
    : TParserIdentifierList;
var
    i: integer;
    xExistingIdent, xNewIdent: TParserIdentifier;

begin
    result := TParserIdentifierList.Create(aIdentifierList.ScopeName);
    for i := 0 to aIdentifierList.Count - 1 do
    begin
        xExistingIdent := aIdentifierList[i];
        xNewIdent := TParserIdentifier.Create(xExistingIdent.Name);
        result.Add(xNewIdent);
    end;
end;

procedure TParserIdentifierList.IdentsNeedingInput(aResultList: TList<TParserIdentifier>;
    const aCheckHasValue, aCheckNonInputIdent: boolean);
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
    begin
        if aCheckHasValue and (self[x] <> nil) then
            CONTINUE;
        if aCheckNonInputIdent and TParserIdentifierUtils.IsNonInputIdent(self[x]) then
            CONTINUE;

        aResultList.Add(self[x]);
    end;
end;

{ TSymbolTable }

constructor TSymbolTable.Create();
begin
    inherited;
    fScopeStack := TStack<TParserScope>.Create();
    fGlobalScope := TParserScope.Create(TParserIdentifierList.Create(STR_GLOBAL_METHOD_NAME), true);
    fLocalScope := nil;
end;

destructor TSymbolTable.Destroy();
begin
    fScopeStack.Free;
    fGlobalScope.Free;
    inherited;
end;

function TSymbolTable.GetGlobalList: TParserIdentifierList;
begin
    result := fGlobalScope.IdentScope;
end;

function TSymbolTable.GetLocalList: TParserIdentifierList;
begin
    result := nil;
    if Assigned(fLocalScope) then
        result := fLocalScope.IdentScope;
end;

procedure TSymbolTable.BeginScope(aIdentScope: TParserIdentifierList);
begin
    if aIdentScope = nil then
    begin
        ASSERT((fScopeStack.Count = 0));
    end
    else
    begin
        fScopeStack.Push(fLocalScope);
    end;

    fLocalScope := TParserScope.Create(aIdentScope, false);
end;

function TSymbolTable.EndScope(): TParserIdentifierList;
var
    xPreviousScope: TParserScope;
begin
    result := nil;
    xPreviousScope := fLocalScope;
    if not Assigned(xPreviousScope) then
    begin
        ASSERT(fScopeStack.Count = 0);
        self.GlobalList.Clear;
    end
    else
    begin
        result := xPreviousScope.IdentScope;
        xPreviousScope.Free;
        fLocalScope := fScopeStack.Peek;
        fScopeStack.Pop;
    end;
end;

function TSymbolTable.IdentsNeedingInput(aCheckHasValue, aCheckNonInputIdent: boolean)
    : TArray<TParserIdentifier>;
var
    aResultList: TList<TParserIdentifier>;
begin
    aResultList := TList<TParserIdentifier>.Create;
    try
        self.GlobalList.IdentsNeedingInput(aResultList, aCheckHasValue, aCheckNonInputIdent);
        self.LocalList.IdentsNeedingInput(aResultList, aCheckHasValue, aCheckNonInputIdent);
        EXIT(aResultList.ToArray);
    finally
        FreeAndNil(aResultList);
    end;
end;

procedure TSymbolTable.RemoveNonInputIdents();
begin
    self.GlobalList.RemoveNonInputIdents();
    self.LocalList.RemoveNonInputIdents();
end;

procedure TSymbolTable.ClearAllIdentifierValues();
begin
    self.GlobalList.ClearIdentValues();
    self.LocalList.ClearIdentValues();
end;

{ TParserScope }

constructor TParserScope.Create(aIdentScope: TParserIdentifierList; aOwnsIdentScope: boolean);
begin
    inherited Create();
    fOwnsIdentScope := aOwnsIdentScope;
    fIdentScope := aIdentScope;
end;

destructor TParserScope.Destroy;
begin
    if fOwnsIdentScope then
        fIdentScope.Free;
    inherited;
end;


end.
