{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.02.10 pk                                        TN4972    Initial Revision
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit TypeMap;


interface


uses
    Generics.Collections;

type
    TMapKind = (makNone, makObject, makList, makNil);

    TMapItem = class
    private
        function GetIsValueEmpty: boolean;
    protected
        fName: string;
        fValue: variant;
        fValueType: string;
        fValueTypeName: string;
    public
        constructor Create(const aName: string);
        destructor Destroy(); override;

        property IsValueEmpty: boolean read GetIsValueEmpty;
        property name: string read fName write fName;
        property Value: variant read fValue write fValue;
        property ValueType: string read fValueType write fValueType;
        property ValueTypeName: string read fValueTypeName write fValueTypeName;
    end;

    TTypeMap = class(TMapItem)
    private
        fItems: TObjectList<TMapItem>;
        function IndexOfName(const aName: string): integer;
        function GetCount: integer;
        function GetFieldAt(aIndex: integer): TMapItem;
    public
        constructor Create(const aName: string);
        destructor Destroy(); override;

        function AddItem(const aName: string): TMapItem;
        function AddTypeMap(const aName: string): TTypeMap;
        procedure Clear();
        procedure Delete(const aName: string);
        function Find(const aName: string): TMapItem;
        property Count: integer read GetCount;
        property this[aIndex: integer]: TMapItem read GetFieldAt; default;
    end;


implementation


uses
    SysUtils,
    Variants;

{ TMapItem }

constructor TMapItem.Create(const aName: string);
begin
    inherited Create();
    fName := aName;
    fValue := Unassigned;
end;

destructor TMapItem.Destroy;
begin
    VarClear(fValue);
    inherited;
end;

function TMapItem.GetIsValueEmpty: boolean;
begin
    result := VarIsEmpty(fValue);
end;

{ TTypeMap }

constructor TTypeMap.Create(const aName: string);
begin
    inherited Create(aName);
    fItems := TObjectList<TMapItem>.Create();
end;

destructor TTypeMap.Destroy;
begin
    FreeAndNil(fItems);
    inherited;
end;

function TTypeMap.AddItem(const aName: string): TMapItem;
begin
    result := TMapItem.Create(aName);
    fItems.Add(result);
end;

function TTypeMap.AddTypeMap(const aName: string): TTypeMap;
begin
    result := TTypeMap.Create(aName);
    fItems.Add(result);
end;

procedure TTypeMap.Clear;
begin
    fItems.Clear;
end;

procedure TTypeMap.Delete(const aName: string);
var
    xIndex: integer;
begin
    xIndex := IndexOfName(aName);
    if xIndex < 0 then
        EXIT;

    fItems.Delete(xIndex);
end;

function TTypeMap.Find(const aName: string): TMapItem;
var
    xIndex: integer;
begin
    xIndex := IndexOfName(aName);
    if xIndex < 0 then
        EXIT(nil);

    EXIT(fItems[xIndex]);
end;

function TTypeMap.GetFieldAt(aIndex: integer): TMapItem;
begin
    result := fItems[aIndex];
end;

function TTypeMap.GetCount: integer;
begin
    result := fItems.Count;
end;

function TTypeMap.IndexOfName(const aName: string): integer;
var
    x: integer;
begin
    for x := 0 to fItems.Count - 1 do
    begin
        if SameText(fItems[x].Name, aName) then
        begin
            EXIT(x);
        end;
    end;

    EXIT(-1);
end;


end.
