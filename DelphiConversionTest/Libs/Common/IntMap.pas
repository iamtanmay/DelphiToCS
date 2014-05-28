{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : IntList & IntMap
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  03.08.07 wl  TIntList, TIntMap            TN3811.2 von ObjStructures getrennt
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }

unit IntMap;


interface


uses

    GeneralTypes,
    ListClasses;

type
    TIntListItemKeyType = integer;

    TIntListItem = class
    private
        fKey: TIntListItemKeyType;
        fObject: TObject;
    public
        constructor Create(aKey: TIntListItemKeyType; aObject: TObject);

        property Key: TIntListItemKeyType read fKey write fKey;
        property Obj: TObject read fObject write fObject;
    end;

    {
      TIntListItemCompare = function( const aKey1, aKey2 : TIntListItemKeyType ) : integer;
      TIntList = class;
      TIntListCompare = function( aList : TIntList; aIndex1, aIndex2 : integer ) : integer;
      TIntList = class
      private
      fCompareFunc : TIntListItemCompare;
      fListCompareFunc : TIntListCompare;
      fList : TObjectList;
      fSorted : boolean;
      fDuplicates : TDuplicates;
      fOwnsObjects : boolean;
      function  GetElement( aIndex : integer ) : TIntListItem;
      procedure SetElement( aIndex : integer; aElement : TIntListItem );
      function  GetItem( aIndex : integer ) : TIntListItemKeyType;
      procedure SetItem( aIndex : integer; aKey : TIntListItemKeyType );
      function  GetObject( aIndex : integer ) : TObject;
      procedure SetObject( aIndex : integer; aObj : TObject );
      function  GetCount() : integer;
      property  Elements[ aIndex : integer ] : TIntListItem read GetElement write SetElement;
      procedure DoInsertObject( aIndex : integer;  aKey : TIntListItemKeyType; aObj : TObject );
      procedure SetSorted( aValue : boolean );
      procedure QuickSort(L, R: Integer; SCompare: TIntListCompare);
      procedure ExchangeItems( aIndex1, aIndex2 : integer );
      public
      constructor Create();
      destructor Destroy(); override;
      function Find( aKey : TIntListItemKeyType; var Index: Integer ): Boolean;
      function IndexOf( const aKey : TIntListItemKeyType ) : integer;
      function IndexOfObject( aObj : TObject ) : integer;

      procedure Insert( aIndex : integer;  aKey : TIntListItemKeyType );
      function  Add( aKey : TIntListItemKeyType ) : integer;
      procedure InsertObject( aIndex : integer;  aKey : TIntListItemKeyType; aObj : TObject );
      function  AddObject(aKey : TIntListItemKeyType; aObj : TObject ) : integer;
      procedure Delete( aIndex : integer );
      procedure Clear();
      procedure Sort();

      property Sorted : boolean read fSorted write SetSorted;
      property Duplicates : TDuplicates read fDuplicates write fDuplicates;
      property OwnsObjects : boolean read fOwnsObjects write fOwnsObjects;
      property Items[ aIndex : integer  ] : TIntListItemKeyType read GetItem write SetItem; default;
      property Objects[ aIndex : integer ] : TObject read GetObject write SetObject;
      property Count : integer read GetCount;
      property CompareFunc : TIntListItemCompare read fCompareFunc write fCompareFunc;
      property ListCompareFunc : TIntListCompare read fListCompareFunc write fListCompareFunc;
      end;
    }
    TIntMap = class
    private
        fList: TIntegerValueList;
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
        function ToIntArray(): TIntArray;
        property Count: integer read GetCount;
        class function IntArgsToIntArray(const aInts: array of integer): TIntArray;
    end;


    // ##################################################################################################


implementation


uses
    SysUtils;

{
  function DefaultItemCompFunc( const aKey1, aKey2 : TIntListItemKeyType ) : integer;
  begin
  result := aKey1 - aKey2;
  end;

  function DefaultListCompFunc( aList : TIntList; aIndex1, aIndex2 : integer ) : integer;
  begin
  result := aList.CompareFunc( aList[ aIndex1 ], aList[ aIndex2 ] );
  end;
}
{ TIntListItem }

constructor TIntListItem.Create(aKey: TIntListItemKeyType; aObject: TObject);
begin
    inherited Create();

    fKey := aKey;
    fObject := aObject;
end;

// { TIntList }
//
// constructor TIntList.Create();
// begin
// inherited Create();
//
// fCompareFunc := DefaultItemCompFunc;
// fListCompareFunc := DefaultListCompFunc;
// fList := TObjectList.Create( false );
// fSorted := false;
// fDuplicates := dupAccept;
// fOwnsObjects := false;
// end;
//
// destructor TIntList.Destroy();
// begin
// Clear();
// fList.Free;
//
// inherited;
// end;
//
// function TIntList.GetCount() : integer;
// begin
// result := fList.Count;
// end;
//
// function TIntList.GetElement( aIndex : integer ) : TIntListItem;
// begin
// result := fList.Items[ aIndex ] as TIntListItem;
// end;
//
// procedure TIntList.SetElement( aIndex : integer; aElement : TIntListItem );
// begin
// fList.Items[ aIndex ] := aElement;
// end;
//
// function TIntList.Find( aKey : TIntListItemKeyType; var Index: Integer ): Boolean;
// var
// L, H, I, C: Integer;
// begin
// result := False;
// L := 0;
// H := self.Count - 1;
// while L <= H do
// begin
// I := (L + H) shr 1;
// C := fCompareFunc( Items[I], aKey );
// if C < 0 then L := I + 1 else
// begin
// H := I - 1;
// if C = 0 then
// begin
// result := True;
// if Duplicates <> dupAccept then L := I;
// end;
// end;
// end;
// Index := L;
// end;
//
// function TIntList.IndexOfObject( aObj : TObject ) : integer;
// var
// i : integer;
// begin
// result := -1;
// for i := 0 to Count - 1 do begin
// if Objects[ i ] = aObj then begin
// result := i;
// EXIT;
// end
// end;
// end;
//
// function TIntList.IndexOf( const aKey : TIntListItemKeyType ) : integer;
// var
// i : integer;
// begin
// if fSorted then begin
// if not Find( aKey, result ) then result := -1
// end
// else begin
// result := -1;
// for i := 0 to Count - 1 do begin
// if fCompareFunc( Items[I], aKey ) = 0 then begin
// result := i;
// EXIT;
// end
// end;
// end;
// end;
//
// function TIntList.GetItem( aIndex : integer ) : TIntListItemKeyType;
// begin
// result := Elements[ aIndex ].Key;
// end;
//
// procedure TIntList.SetItem( aIndex : integer; aKey : TIntListItemKeyType );
// begin
// Elements[ aIndex ].Key := aKey;
// end;
//
// function TIntList.GetObject( aIndex : integer ) : TObject;
// begin
// result := Elements[ aIndex ].Obj;
// end;
//
// procedure TIntList.SetObject( aIndex : integer; aObj : TObject );
// begin
// Elements[ aIndex ].Obj := aObj;
// end;
//
// procedure TIntList.DoInsertObject( aIndex : integer;  aKey : TIntListItemKeyType; aObj : TObject );
// begin
// fList.Insert( aIndex, TIntListItem.Create( aKey, aObj ) );
// end;
//
// procedure TIntList.InsertObject( aIndex : integer;  aKey : TIntListItemKeyType; aObj : TObject );
// begin
// ASSERT( not fSorted, 'Insert cannot be used on sorted list' );
// DoInsertObject( aIndex, aKey, aObj );
// end;
//
// function TIntList.AddObject( aKey : TIntListItemKeyType; aObj : TObject ) : integer;
// begin
// if not Sorted then
// result := Count
// else
// if Find(aKey, result) then
// case Duplicates of
// dupIgnore: Exit;
// dupError: raise Exception.CreateFmt( 'Key [%d] already exists', [ aKey ] );
// end;
//
// DoInsertObject( result, aKey, aObj );
// end;
//
// procedure TIntList.Insert( aIndex : integer;  aKey : TIntListItemKeyType );
// begin
// InsertObject( aIndex, aKey, nil );
// end;
//
// function TIntList.Add( aKey : TIntListItemKeyType ) : integer;
// begin
// result := AddObject( aKey, nil );
// end;
//
// procedure TIntList.Delete( aIndex : integer );
// begin
// if fOwnsObjects then Objects[ aIndex ].Free;
// Elements[ aIndex ].Free;
// fList.Delete( aIndex );
// end;
//
// procedure TIntList.Clear();
// begin
// while Count > 0 do begin
// Delete( 0 );
// end;
// end;
//
// procedure TIntList.Sort();
// begin
// if Count = 0 then EXIT;
// QuickSort(0, Count - 1, fListCompareFunc );
// end;
//
// procedure TIntList.SetSorted( aValue : boolean );
// begin
// if FSorted <> aValue then
// begin
// if aValue then Sort;
// FSorted := aValue;
// end;
// end;
//
// procedure TIntList.ExchangeItems( aIndex1, aIndex2 : integer );
// var
// xTemp : TIntListItem;
// begin
// xTemp := Elements[ aIndex1 ];
// Elements[ aIndex1 ] := Elements[ aIndex2 ];
// Elements[ aIndex2 ] := xTemp;
// end;
//
// procedure TIntList.QuickSort(L, R: Integer; SCompare: TIntListCompare);
// var
// I, J, P: Integer;
// begin
// repeat
// I := L;
// J := R;
// P := (L + R) shr 1;
// repeat
// while SCompare(self, I, P) < 0 do Inc(I);
// while SCompare(self, J, P) > 0 do Dec(J);
// if I <= J then
// begin
// ExchangeItems(I, J);
// if P = I then
// P := J
// else if P = J then
// P := I;
// Inc(I);
// Dec(J);
// end;
// until I > J;
// if L < J then QuickSort(L, J, SCompare);
// L := I;
// until I >= R;
// end;

{ TIntMap }

constructor TIntMap.Create();
begin
    inherited Create;
    fList := TIntegerValueList.Create();
    //
    // if aSorted then
    // fList := TIntegerKeyObjectValueList.Create( dupIgnore )
    // else
    // fList := TIntegerKeyObjectValueList.Create() ;
end;

destructor TIntMap.Destroy();
begin
    FreeAndNil(fList);

    inherited;
end;

class function TIntMap.IntArgsToIntArray(const aInts: array of integer): TIntArray;
var
    i: integer;
begin
    SetLength(result, high(aInts) + 1);
    for i := 0 to high(aInts) do
    begin
        result[i] := aInts[i];
    end;
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

function TIntMap.ToIntArray(): TIntArray;
var
    i: integer;
begin
    SetLength(result, fList.Count);
    for i := 0 to fList.Count - 1 do
    begin
        result[i] := fList[i];
    end;
end;

function TIntMap.GetCount(): integer;
begin
    result := fList.Count;
end;


end.
