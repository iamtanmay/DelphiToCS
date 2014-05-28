{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk  TReferenceArg                         TN4232      New
  04.11.09 pk                               	        TN4843   	Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  07.05.10 pk                                        TN5092      TArg is now the same as TStreamableItem
  12.05.10 pk  ItemArrayToStr                        TN5092      Leading comma removed
  17.05.10 pk  TArrayArg                             TN5092      fValue is now TArgs- changes for Restart for new Array Type
  08.07.11 wl  TArgUtils                             TN5626   neue Methoden zur Darstellung von Arrays
  18.07.11 wl  TArgUtils.ItemToStr                   TN5631   neue überladene Funktionen
  01.03.12 wl  TArrayArg                             TN5820   entfernt
  ----------------------------------------------------------------------------------------------------------------------- }

unit ArgClass;


interface


uses
    SysUtils,
    ListClasses,
    Variants,
    Streamable;

type
    // Arg object
    TArg = TStreamableItem;
    TIntArg = TIntStreamableItem;
    TCardArg = TCardStreamableItem;
    TStrArg = TStrStreamableItem;
    TObjArg = TObjStreamableItem;
    TBoolArg = TBoolStreamableItem;
    TDblArg = TFloatStreamableItem;
    TKeyArgValueList = TStreamableKeyValueList;

    TReferenceArg = class(TArg)
    private
        fRefID: integer;
    protected
        function GetAsInt: integer; override;
        function GetAsStr: string; override;
        procedure SetAsInt(aValue: integer); override;
    public
        constructor Create(const aRefID: integer); reintroduce;
    published
        property RefID: integer read fRefID write fRefID;
    end;

    TArgs = class(TStreamable)
    private
        fList: TStreamableObjectList;
        function GetItem(aIndex: integer): TArg;
        procedure SetItem(aIndex: integer; aItem: TArg);
        function GetCount: integer;
        function GetCapacity: integer;
        procedure SetCapacity(const Value: integer);
    public
        constructor Create; override;
        destructor Destroy; override;
        class function ArgsArrayOf(const aArgs: array of TArg): TArgs;
        class function EmptyArgs(): TArgs;
        procedure Delete(const aIndex: integer);
        function Add(aItem: TArg): integer;
        property this[aIndex: integer]: TArg read GetItem write SetItem; default;
        property Count: integer read GetCount;
    published
        property List: TStreamableObjectList read fList write fList;
        property Capacity: integer read GetCapacity write SetCapacity;

    end;

    TArrayArg = class(TArg)
    private
        fValue: TArgs;

    const
        cArgArrayBaseOffset = 1;
        cArgArrayIndexUndefined = low(integer);
        procedure CheckBounds(const aUserIndex: integer);
        function GetItem(aUserIndex: integer): TArg;
        function GetArrayLength: integer;
        procedure SetArrayLength(const aValue: integer);
        procedure SetItem(aUserIndex: integer; const aValue: TArg);
        procedure ClearItems(const aFirstUserIndex, aLastUserIndex: integer);
        function UserIndexToInternalIndex(const aUserIndex: integer): integer;
        function InternalIndexToUserIndex(const aIndex: integer): integer;
        procedure ClearLength();
        function GetMinUserIndex(): integer;
        function GetMaxUserIndex(): integer;
    protected
        function GetAsStr: string; override;
    public
        constructor Create(); override;
        destructor Destroy(); override;
        class function UndefinedBoundsIndex(): integer;

        property this[aUserIndex: integer]: TArg read GetItem write SetItem; default;
        property ArrayLength: integer read GetArrayLength write SetArrayLength;
        property MinUserIndex: integer read GetMinUserIndex;
        property MaxUserIndex: integer read GetMaxUserIndex;
    published
        property Value: TArgs read fValue write fValue;
    end;

    TArgUtils = class
    private
        class function ItemArrayToStr(const aArray: TArrayArg;
            const aFormatSettings: TFormatSettings): string;
    public
        class function ItemToStr(const aValue: TArg; const aArrayIndex: integer;
            const aFormatSettings: TFormatSettings): string; overload;
        class function ItemToStr(const aValue: TArg; const aArrayIndex: integer): string; overload;
        class function ItemToStr(const aValue: TArg; const aFormatSettings: TFormatSettings): string;
            overload;
        class function ItemToStr(const aValue: TArg): string; overload;
        class function ListToStr(const aList: TKeyArgValueList; const aFormatSettings: TFormatSettings)
            : string; overload;
        class function ListToStr(const aList: TKeyArgValueList): string; overload;
    end;


implementation


uses
    GeneralTypes;

{ TArgUtils }

class function TArgUtils.ItemArrayToStr(const aArray: TArrayArg;
    const aFormatSettings: TFormatSettings): string;
var
    x: integer;
begin
    // "lesbare Schreibweise" für Logging oder GUI

    result := '[';
    for x := aArray.MinUserIndex to aArray.MaxUserIndex do
    begin
        if x > aArray.MinUserIndex then
            result := result + ', ';
        result := result + ItemToStr(aArray, x, aFormatSettings);
    end;
    result := result + ']';
end;

class function TArgUtils.ItemToStr(const aValue: TArg; const aArrayIndex: integer;
    const aFormatSettings: TFormatSettings): string;
var
    xArrayArg: TArrayArg;
begin
    if aValue is TDblArg then
    begin
        result := FloatToStr(aValue.AsFloat, aFormatSettings);
    end
    else if aValue is TIntArg then
        result := IntToStr(aValue.AsInt)
    else if aValue is TStrArg then
        result := aValue.AsStr
    else if aValue is TBoolArg then
        result := BoolToStr(aValue.AsBool, true)
    else if aValue is TArrayArg then
    begin
        xArrayArg := (aValue as TArrayArg);
        if (aArrayIndex < 0) then
        begin
            result := ItemArrayToStr(xArrayArg, aFormatSettings);
        end
        else
        begin
            result := ItemToStr(xArrayArg[aArrayIndex], TArrayArg.UndefinedBoundsIndex, aFormatSettings);
        end;
    end
    else if aValue is TReferenceArg then
    begin
        result := IntToStr(aValue.AsInt);
    end;
end;

class function TArgUtils.ItemToStr(const aValue: TArg; const aArrayIndex: integer): string;
begin
    result := ItemToStr(aValue, aArrayIndex, TFormatUtils.GetSettingsEnglishUS);
end;

class function TArgUtils.ItemToStr(const aValue: TArg; const aFormatSettings: TFormatSettings): string;
begin
    result := ItemToStr(aValue, TArrayArg.UndefinedBoundsIndex, aFormatSettings);
end;

class function TArgUtils.ItemToStr(const aValue: TArg): string;
begin
    result := ItemToStr(aValue, TArrayArg.UndefinedBoundsIndex, TFormatUtils.GetSettingsEnglishUS);
end;

class function TArgUtils.ListToStr(const aList: TKeyArgValueList): string;
begin
    result := ListToStr(aList, TFormatUtils.GetSettingsEnglishUS);
end;

class function TArgUtils.ListToStr(const aList: TKeyArgValueList;
    const aFormatSettings: TFormatSettings): string;
var
    x: integer;
    xItem: TKeyValueItem;
begin
    result := '';
    for x := 0 to aList.Count - 1 do
    begin
        xItem := aList[x];
        if result <> '' then
            result := result + cListStringDelim;
        if xItem.Key <> '' then
            result := result + xItem.Key + cListKeyValueSeparator;
        result := result + ItemToStr(xItem.Value, TArrayArg.UndefinedBoundsIndex, aFormatSettings);
    end;
end;

{ TReferenceArg }

constructor TReferenceArg.Create(const aRefID: integer);
begin
    inherited Create;
    SetAsInt(aRefID);
end;

function TReferenceArg.GetAsInt: integer;
begin
    result := fRefID;
end;

procedure TReferenceArg.SetAsInt(aValue: integer);
begin
    fRefID := aValue;
end;

function TReferenceArg.GetAsStr: string;
begin
    result := IntToStr(fRefID);
end;

{ TArgs }

function TArgs.GetCapacity: integer;
begin
    result := fList.Capacity;
end;

function TArgs.GetCount: integer;
begin
    result := fList.Count;
end;

function TArgs.GetItem(aIndex: integer): TArg;
var
    xItem: TCustomStreamable;
begin
    result := nil;
    xItem := fList[aIndex];
    if not Assigned(xItem) then
        EXIT;

    result := xItem as TArg;
end;

procedure TArgs.SetCapacity(const Value: integer);
begin
    fList.Capacity := Value;
end;

procedure TArgs.SetItem(aIndex: integer; aItem: TArg);
begin
    fList.Items[aIndex] := aItem;
end;

constructor TArgs.Create;
begin
    inherited Create;

    fList := TStreamableObjectList.Create();
end;

procedure TArgs.Delete(const aIndex: integer);
begin
    fList.Remove(fList[aIndex]);
end;

destructor TArgs.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

class function TArgs.EmptyArgs(): TArgs;
begin
    result := nil;
end;

function TArgs.Add(aItem: TArg): integer;
begin
    fList.Add(aItem);
    result := fList.Count - 1;
end;

class function TArgs.ArgsArrayOf(const aArgs: array of TArg): TArgs;
var
    i: integer;
    x: TArg;
begin
    result := TArgs.Create();
    // result.Capacity := High( aArgs ) + 1;
    for i := 0 to high(aArgs) do
    begin
        x := aArgs[i];
        result.Add(x);
    end;
end;

{ TArrayArg }

constructor TArrayArg.Create();
begin
    inherited Create();
    fValue := TArgs.Create();
    ClearLength;
end;

destructor TArrayArg.Destroy;
begin
    FreeAndNil(fValue);
    inherited;
end;

class function TArrayArg.UndefinedBoundsIndex(): integer;
begin
    result := cArgArrayIndexUndefined;
end;

procedure TArrayArg.ClearLength();
begin
    fValue.Capacity := 0;
end;

procedure TArrayArg.ClearItems(const aFirstUserIndex, aLastUserIndex: integer);
var
    x: integer;
begin
    for x := aFirstUserIndex to aLastUserIndex - 1 do
        fValue.Delete(UserIndexToInternalIndex(x));
end;

function TArrayArg.UserIndexToInternalIndex(const aUserIndex: integer): integer;
begin
    result := aUserIndex - cArgArrayBaseOffset;
end;

function TArrayArg.InternalIndexToUserIndex(const aIndex: integer): integer;
begin
    result := aIndex + cArgArrayBaseOffset;
end;

function TArrayArg.GetMinUserIndex(): integer;
begin
    result := InternalIndexToUserIndex(0);
end;

function TArrayArg.GetAsStr: string;
var
    x: integer;
begin
    // "interne Schreibweise"

    result := '[';
    for x := self.MinUserIndex to self.MaxUserIndex do
    begin
        if x > self.MinUserIndex then
            result := result + ',';
        result := result + TArgUtils.ItemToStr(self[x], -1, TFormatUtils.GetSettingsEnglishUS);
    end;
    result := result + ']';
end;

function TArrayArg.GetMaxUserIndex(): integer;
begin
    result := InternalIndexToUserIndex(self.ArrayLength - 1);
end;

procedure TArrayArg.CheckBounds(const aUserIndex: integer);
begin
    if (aUserIndex < self.MinUserIndex) or (aUserIndex > self.MaxUserIndex) then
    begin
        raise Exception.CreateFmt('Array Index [%d] out of bounds', [aUserIndex]);
    end;
end;

function TArrayArg.GetItem(aUserIndex: integer): TArg;
begin
    CheckBounds(aUserIndex);
    result := fValue[UserIndexToInternalIndex(aUserIndex)];
end;

function TArrayArg.GetArrayLength: integer;
begin
    result := fValue.Capacity;
end;

procedure TArrayArg.SetItem(aUserIndex: integer; const aValue: TArg);
begin
    CheckBounds(aUserIndex);
    fValue[UserIndexToInternalIndex(aUserIndex)] := aValue;
end;

procedure TArrayArg.SetArrayLength(const aValue: integer);
var
    xNumberOfItemsToClear: integer;
begin
    if (aValue < self.ArrayLength) then
    begin
        xNumberOfItemsToClear := (self.ArrayLength - aValue);
        ClearItems(self.MaxUserIndex - xNumberOfItemsToClear + 1, self.MaxUserIndex);
    end;

    fValue.Capacity := aValue;
end;


end.
