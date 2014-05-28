{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  27.03.13 wl                                      TN6045   Neu: Diese Klassen sollen nicht benutzt werden
  11.04.13 wl  T..Comparer                         TN6045   --> GeneralTypes
  ----------------------------------------------------------------------------------------------------------- }

unit HiddenListClasses;


interface


uses
    Generics.Collections,
    Generics.Defaults,
    GeneralTypes;

type
    TKeyValueItemList<TKey, TValue> = class(TObjectList < TKeyValueItem < TKey, TValue >> );

    TKeyValueListValuesPropAdaptor<TKey, TValue> = class
    private
        fList: TKeyValueItemList<TKey, TValue>;
        function GetValueAt(aIndex: integer): TValue;
        procedure SetValueAt(aIndex: integer; const aValue: TValue);
    public
        constructor Create(const aList: TKeyValueItemList<TKey, TValue>);
        property this[aIndex: integer]: TValue read GetValueAt write SetValueAt; default;
    end;

    TKeyValueList<TKey, TValue> = class
    private
        fList: TKeyValueItemList<TKey, TValue>;
        fValuesPropAdaptor: TKeyValueListValuesPropAdaptor<TKey, TValue>;
        fSorted: boolean;
        fDuplicates: TKeyValueListDuplicates;
        fComparer: IComparer<TKey>;
        fOwnsValues: boolean;
        procedure QuickSort(L, R: Integer);
        function CreateItemWithValue(const aKey: TKey; aValue: TValue): TKeyValueItem<TKey, TValue>;
        procedure InsertItemWithValue(const aIndex: integer; const aKey: TKey; const aValue: TValue);
        function Find(const aKey: TKey; out oIndex: Integer): Boolean;
        function CompareKeys(const aKey1, aKey2: TKey): integer;
        function IndexOfUnsorted(const aKey: TKey): integer;
        function GetItemAt(aIndex: integer): TKeyValueItem<TKey, TValue>;
        function GetCount: integer;
        procedure CustomSort();
        function GetItemKeyAt(aIndex: integer): TKey;
        procedure SetItemKeyAt(aIndex: integer; const aValue: TKey);
    protected
        procedure FreeValueAt(aIndex: integer); virtual;
    public
        constructor Create(const aComparer: IComparer<TKey>); overload;
        constructor Create(const aOwnsValues: boolean; const aComparer: IComparer<TKey>); overload;
        constructor Create(const aOwnsValues: boolean; const aComparer: IComparer<TKey>;
            const aSortDuplicates: TKeyValueListDuplicates); overload;
        destructor Destroy(); override;
        procedure Exchange(const aIndex1, aIndex2: integer);
        function AddValue(const aKey: TKey; const aValue: TValue): integer;
        function Add(const aKey: TKey): integer;
        procedure Delete(const aIndex: integer);
        procedure Clear();
        procedure Sort();
        function IndexOf(const aKey: TKey): integer;

        // dictionary-style of methods
        function TryGetValue(const aKey: TKey; out oValue: TValue): boolean;
        function ContainsKey(const aKey: TKey): boolean;
        procedure Remove(const aKey: TKey);
        procedure AddOrSetValue(const aKey: TKey; const aValue: TValue);

        property Count: integer read GetCount;
        property Values: TKeyValueListValuesPropAdaptor<TKey, TValue>read fValuesPropAdaptor;
        property this[aIndex: integer]: TKey read GetItemKeyAt write SetItemKeyAt; default;
    end;

    TKeyObjectValueList<TKey> = class(TKeyValueList<TKey, TObject>)
    private
        function GetValuesPropAdaptor: TKeyValueListValuesPropAdaptor<TKey, TObject>;
    protected
        procedure FreeValueAt(aIndex: integer); override;
    public
        function AddObject(const aKey: TKey; const aValue: TObject): integer;
        // procedure InsertObject( const aIndex : integer; const aKey : TKey; const aValue : TObject );
        function IndexOfObject(aObj: TObject): integer;
        property Objects: TKeyValueListValuesPropAdaptor<TKey, TObject>read GetValuesPropAdaptor;
    end;

    TCustomStringKeyObjectValueList = class(TKeyObjectValueList<string>)
    public
        constructor Create(); overload;
        constructor Create(const aSortDuplicates: TKeyValueListDuplicates); overload;
        constructor Create(const aSortDuplicates: TKeyValueListDuplicates;
            const aSortCaseSensitive: boolean); overload;

        constructor Create(const aOwnsObjects: boolean); overload;
        constructor Create(const aOwnsObjects: boolean;
            const aSortDuplicates: TKeyValueListDuplicates); overload;
        constructor Create(const aOwnsObjects: boolean; const aSortDuplicates: TKeyValueListDuplicates;
            const aSortCaseSensitive: boolean); overload;

        function ToArray(): TStringArray;
        procedure AddRange(const aArray: array of string);
    end;


implementation


uses
    SysUtils;

{ TKeyValueListValuesPropAdaptor<TKey, TValue> }

constructor TKeyValueListValuesPropAdaptor<TKey, TValue>.Create(const aList: TKeyValueItemList<TKey, TValue>);
begin
    inherited Create();
    fList := aList;
end;

function TKeyValueListValuesPropAdaptor<TKey, TValue>.GetValueAt(aIndex: integer): TValue;
begin
    result := fList[aIndex].Value;
end;

procedure TKeyValueListValuesPropAdaptor<TKey, TValue>.SetValueAt(aIndex: integer; const aValue: TValue);
begin
    fList[aIndex].Value := aValue;
end;

{ TKeyValueList<TKey, TValue> }

constructor TKeyValueList<TKey, TValue>.Create(const aOwnsValues: boolean; const aComparer: IComparer<TKey>);
begin
    inherited Create();
    fComparer := aComparer;
    fList := TKeyValueItemList<TKey, TValue>.Create(true);
    fValuesPropAdaptor := TKeyValueListValuesPropAdaptor<TKey, TValue>.Create(fList);
    fSorted := false;
    fDuplicates := dupIgnore;
    fOwnsValues := aOwnsValues;
end;

constructor TKeyValueList<TKey, TValue>.Create(const aComparer: IComparer<TKey>);
begin
    Create(true, aComparer);
end;

constructor TKeyValueList<TKey, TValue>.Create(const aOwnsValues: boolean; const aComparer: IComparer<TKey>;
    const aSortDuplicates: TKeyValueListDuplicates);
begin
    Create(aOwnsValues, aComparer);
    fSorted := true;
    fDuplicates := aSortDuplicates;
end;

destructor TKeyValueList<TKey, TValue>.Destroy();
begin
    Clear();
    FreeAndNil(fList);
    FreeAndNil(fValuesPropAdaptor);
    fComparer := nil;
    inherited;
end;

function TKeyValueList<TKey, TValue>.CreateItemWithValue(const aKey: TKey; aValue: TValue)
    : TKeyValueItem<TKey, TValue>;
begin
    result := TKeyValueItem<TKey, TValue>.Create(aKey, aValue);
end;

procedure TKeyValueList<TKey, TValue>.FreeValueAt(aIndex: integer);
begin

end;

procedure TKeyValueList<TKey, TValue>.Delete(const aIndex: integer);
begin
    if fOwnsValues then
        FreeValueAt(aIndex);
    fList.Delete(aIndex);
end;

procedure TKeyValueList<TKey, TValue>.InsertItemWithValue(const aIndex: integer; const aKey: TKey;
    const aValue: TValue);
begin
    fList.Insert(aIndex, CreateItemWithValue(aKey, aValue));
end;

procedure TKeyValueList<TKey, TValue>.AddOrSetValue(const aKey: TKey; const aValue: TValue);
var
    xIndex: integer;
begin
    xIndex := self.IndexOf(aKey);
    if xIndex < 0 then
        self.AddValue(aKey, aValue)
    else
        self.Values[xIndex] := aValue;
end;

function TKeyValueList<TKey, TValue>.AddValue(const aKey: TKey; const aValue: TValue): integer;
begin
    if not fSorted then
    begin
        result := self.Count
    end
    else
    begin
        if Find(aKey, Result) then
            case fDuplicates of
                dupIgnore:
                    Exit;
                dupError:
                    raise Exception.Create('Duplicate key');
            end;
    end;

    InsertItemWithValue(result, aKey, aValue);
end;

function TKeyValueList<TKey, TValue>.Add(const aKey: TKey): integer;
begin
    result := AddValue(aKey, default (TValue));
end;

procedure TKeyValueList<TKey, TValue>.Exchange(const aIndex1, aIndex2: integer);
var
    xItem1, xItem2: TKeyValueItem<TKey, TValue>;
    xTempKey: TKey;
    xTempValue: TValue;
begin
    xItem1 := GetItemAt(aIndex1);
    xItem2 := GetItemAt(aIndex2);
    xTempKey := xItem1.Key;
    xTempValue := xItem1.Value;
    xItem1.Key := xItem2.Key;
    xItem1.Value := xItem2.Value;
    xItem2.Key := xTempKey;
    xItem2.Value := xTempValue;
end;

procedure TKeyValueList<TKey, TValue>.Clear;
var
    x: integer;
begin
    for x := self.Count - 1 downto 0 do
        Delete(x);
end;

function TKeyValueList<TKey, TValue>.GetCount: integer;
begin
    result := fList.Count;
end;

function TKeyValueList<TKey, TValue>.GetItemAt(aIndex: integer): TKeyValueItem<TKey, TValue>;
begin
    result := fList[aIndex];
end;

function TKeyValueList<TKey, TValue>.GetItemKeyAt(aIndex: integer): TKey;
begin
    result := GetItemAt(aIndex).Key;
end;

procedure TKeyValueList<TKey, TValue>.SetItemKeyAt(aIndex: integer; const aValue: TKey);
begin
    GetItemAt(aIndex).Key := aValue;
end;

function TKeyValueList<TKey, TValue>.IndexOf(const aKey: TKey): integer;
begin
    if fSorted then
    begin
        if not Find(aKey, result) then
            result := -1
    end
    else
    begin
        result := IndexOfUnsorted(aKey);
    end;
end;

function TKeyValueList<TKey, TValue>.CompareKeys(const aKey1, aKey2: TKey): integer;
begin
    ASSERT(Assigned(fComparer));
    result := fComparer.Compare(aKey1, aKey2);
end;

function TKeyValueList<TKey, TValue>.ContainsKey(const aKey: TKey): boolean;
begin
    result := IndexOf(aKey) >= 0;
end;

function TKeyValueList<TKey, TValue>.IndexOfUnsorted(const aKey: TKey): integer;
var
    x: integer;
begin
    result := -1;

    for x := 0 to self.Count - 1 do
    begin
        if CompareKeys(GetItemKeyAt(x), aKey) = 0 then
        begin
            result := x;
            EXIT;
        end;
    end;
end;

function TKeyValueList<TKey, TValue>.Find(const aKey: TKey; out oIndex: Integer): Boolean;
var
    L, H, I, C: Integer;
begin
    Result := False;
    L := 0;
    H := self.Count - 1;
    while L <= H do
    begin
        I := (L + H) shr 1;
        C := CompareKeys(GetItemKeyAt(I), aKey);
        if C < 0 then
            L := I + 1
        else
        begin
            H := I - 1;
            if C = 0 then
            begin
                Result := True;
                if fDuplicates <> dupAccept then
                    L := I;
            end;
        end;
    end;
    oIndex := L;
end;

procedure TKeyValueList<TKey, TValue>.QuickSort(L, R: Integer);
var
    I, J, P: Integer;
begin
    repeat
        I := L;
        J := R;
        P := (L + R) shr 1;
        repeat
            while CompareKeys(GetItemKeyAt(I), GetItemKeyAt(P)) < 0 do
                Inc(I);
            while CompareKeys(GetItemKeyAt(J), GetItemKeyAt(P)) > 0 do
                Dec(J);
            if I <= J then
            begin
                if I <> J then
                    Exchange(I, J);
                if P = I then
                    P := J
                else if P = J then
                    P := I;
                Inc(I);
                Dec(J);
            end;
        until I > J;
        if L < J then
            QuickSort(L, J);
        L := I;
    until I >= R;
end;

procedure TKeyValueList<TKey, TValue>.Remove(const aKey: TKey);
var
    xIndex: integer;
begin
    xIndex := self.IndexOf(aKey);
    if xIndex < 0 then
        EXIT;
    self.Delete(xIndex);
end;

procedure TKeyValueList<TKey, TValue>.CustomSort();
begin
    if not fSorted and (self.Count > 1) then
    begin
        QuickSort(0, self.Count - 1);
    end;
end;

procedure TKeyValueList<TKey, TValue>.Sort;
begin
    CustomSort();
end;

function TKeyValueList<TKey, TValue>.TryGetValue(const aKey: TKey; out oValue: TValue): boolean;
var
    xIndex: integer;
begin
    xIndex := self.IndexOf(aKey);
    result := xIndex >= 0;
    if result then
        oValue := self.Values[xIndex]
    else
        oValue := default (TValue);
end;

{ TKeyObjectValueList<TKey> }

function TKeyObjectValueList<TKey>.AddObject(const aKey: TKey; const aValue: TObject): integer;
begin
    result := self.AddValue(aKey, aValue);
end;

function TKeyObjectValueList<TKey>.IndexOfObject(aObj: TObject): integer;
var
    i: integer;
begin
    result := -1;
    for i := 0 to Count - 1 do
    begin
        if Objects[i] = aObj then
        begin
            result := i;
            EXIT;
        end
    end;
end;

procedure TKeyObjectValueList<TKey>.FreeValueAt(aIndex: integer);
var
    xValue: TObject;
begin
    xValue := GetItemAt(aIndex).Value;
    FreeAndNil(xValue);
end;

function TKeyObjectValueList<TKey>.GetValuesPropAdaptor: TKeyValueListValuesPropAdaptor<TKey, TObject>;
begin
    result := self.Values;
end;

{ TCustomStringKeyObjectValueList }

constructor TCustomStringKeyObjectValueList.Create(const aOwnsObjects: boolean;
    const aSortDuplicates: TKeyValueListDuplicates; const aSortCaseSensitive: boolean);
var
    xComparer: IComparer<string>;
begin
    if aSortCaseSensitive then
        xComparer := TCustomStringComparer.Create()
    else
        xComparer := TCustomTextComparer.Create();

    inherited Create(aOwnsObjects, xComparer, aSortDuplicates);
end;

constructor TCustomStringKeyObjectValueList.Create(const aSortDuplicates: TKeyValueListDuplicates;
    const aSortCaseSensitive: boolean);
begin
    Create(false, aSortDuplicates, aSortCaseSensitive);
end;

constructor TCustomStringKeyObjectValueList.Create(const aOwnsObjects: boolean;
    const aSortDuplicates: TKeyValueListDuplicates);
begin
    Create(aOwnsObjects, aSortDuplicates, false);
end;

constructor TCustomStringKeyObjectValueList.Create(const aSortDuplicates: TKeyValueListDuplicates);
begin
    Create(false, aSortDuplicates);
end;

constructor TCustomStringKeyObjectValueList.Create(const aOwnsObjects: boolean);
begin
    Create(aOwnsObjects, dupError);
end;

constructor TCustomStringKeyObjectValueList.Create();
begin
    Create(false);
end;

procedure TCustomStringKeyObjectValueList.AddRange(const aArray: array of string);
var
    x: integer;
begin
    for x := 0 to high(aArray) do
        self.Add(aArray[x]);
end;

function TCustomStringKeyObjectValueList.ToArray: TStringArray;
var
    x: integer;
begin
    SetLength(result, self.Count);

    for x := 0 to self.Count - 1 do
        result[x] := self[x];
end;


end.
