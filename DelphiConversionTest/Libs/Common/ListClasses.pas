{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  04.11.09 pk                                        TN4843    Initial revision
  11.11.09 pk  TreeList                              TN4856    New
  19.11.09 pk  TKeyValueList.Clear                   TN4843    safer/more efficient to count backwards
  04.12.09 pk  TKeyValueList.InsertItem              TN4918    removed. now use ony InsertItemWithValue
  17.03.10 wl  TGenericTree...                       TN5031    --> GenericTree.pas
  17.03.10 wl  TKeyValueList<TKey,TValue>.Find       TN5031    Index parameter out statt var
  15.11.10 pk                                        TN5340    Changes to prevent memory leak
  29.11.10 wl  TStringValueList.ToArray              TN5370    reintroduce für Delphi XE (eventuell Funktion überflüssig)
  18.04.12 wl  TKeyValueItem                         TN5870   --> GeneralTypes
  10.04.13 wl  TKeyValueListDuplicates               TN6045   --> GeneralTypes
  10.04.13 wl                                        TN6045   div. Klassen --> HiddenListClasses
  11.04.13 wl  TStringKeyStringValueList             TN6045   --> MethodGUIParsing
  ----------------------------------------------------------------------------------------------------------------------- }

unit ListClasses;


interface


uses
    HiddenListClasses,
    GeneralTypes;

type
    TStringKeyObjectValueList = class(TCustomStringKeyObjectValueList);

    TIntegerKeyObjectValueList = class(TKeyObjectValueList<integer>)
    public
        constructor Create(); overload;
        constructor Create(const aOwnsObjects: boolean); overload;
        constructor Create(const aSortDuplicates: TKeyValueListDuplicates); overload;
        constructor Create(const aOwnsObjects: boolean;
            const aSortDuplicates: TKeyValueListDuplicates); overload;
    end;


implementation


{ TIntegerKeyObjectValueList }

constructor TIntegerKeyObjectValueList.Create(const aOwnsObjects: boolean);
begin
    inherited Create(aOwnsObjects, TCustomIntegerComparer.Create());
end;

constructor TIntegerKeyObjectValueList.Create;
begin
    Create(false);
end;

constructor TIntegerKeyObjectValueList.Create(const aOwnsObjects: boolean;
    const aSortDuplicates: TKeyValueListDuplicates);
begin
    inherited Create(aOwnsObjects, TCustomIntegerComparer.Create(), aSortDuplicates);
end;

constructor TIntegerKeyObjectValueList.Create(const aSortDuplicates: TKeyValueListDuplicates);
begin
    Create(false, aSortDuplicates);
end;


end.
