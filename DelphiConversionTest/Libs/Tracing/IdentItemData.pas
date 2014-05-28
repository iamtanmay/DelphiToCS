{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.02.10 pk                                        TN4972    Various Changes
  17.05.10 pk  TArgData                              TN5092    is now a TArg since TArg is streamable
  01.03.12 wl                                        TN5822   uses geändert
  27.03.13 wl                                        TN6045   uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit IdentItemData;


interface


uses
    Streamable;

type
    TIdentItemData = class(TStreamable)
    protected
        fIdentName: string;
        fIdentValue: TStreamableItem;
    public
        constructor Create(); override;
    published
        property IdentName: string read fIdentName write fIdentName;
        property IdentValue: TStreamableItem read fIdentValue write fIdentValue;
    end;

    TIdentListData = class(TStreamable)
    private
        fList: TStreamableObjectList;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TIdentItemData;
    public
        constructor Create(); override;
        destructor Destroy(); override;
        procedure Add(aIdent: TIdentItemData);
        function FindIdentByName(const aIdentName: string): TIdentItemData;
        property Count: integer read GetCount;
        property Idents[aIndex: integer]: TIdentItemData read GetItemAt; default;

        class function CloneIdentValueData(const aIdentValueData: TStreamableItem): TStreamableItem;
    published
        property List: TStreamableObjectList read fList write fList;
    end;


implementation


uses
    SysUtils,
    Classes,
    TypeMapTranslator;

{ TIdentItemData }

constructor TIdentItemData.Create();
begin
    inherited Create();
    fIdentName := '';
    fIdentValue := nil;
end;

{ TIdentListData }

procedure TIdentListData.Add(aIdent: TIdentItemData);
begin
    fList.Add(aIdent);
end;

constructor TIdentListData.Create();
begin
    inherited Create();
    fList := TStreamableObjectList.Create();
end;

destructor TIdentListData.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

function TIdentListData.FindIdentByName(const aIdentName: string): TIdentItemData;
var
    x: integer;
begin
    result := nil;
    for x := 0 to fList.Count - 1 do
    begin
        if self[x].IdentName = aIdentName then
        begin
            result := self[x];
            EXIT;
        end;
    end;
end;

function TIdentListData.GetCount: integer;
begin
    result := fList.Count;
end;

function TIdentListData.GetItemAt(aIndex: integer): TIdentItemData;
begin
    result := fList[aIndex] as TIdentItemData;
end;

class function TIdentListData.CloneIdentValueData(const aIdentValueData: TStreamableItem): TStreamableItem;
begin
    result := TObjectCopy<TStreamableItem>.Copy(aIdentValueData);
end;


end.
