unit RunStepBuilderList;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  08.01.08 wl                               TN3972    initial version
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }


interface


uses
    ListClasses,
    RunStepBuilder;

type
    TRunStepBuilderList = class
    protected
        FList: TGenericObjectList<TRunStepBuilder>;
    private
        function GetCount: integer;
        function GetItem(aIndex: Integer): TRunStepBuilder;
        procedure SetOwnsObjects(aOwnsObjects: boolean);
        function GetOwnsObjects: boolean;
    public
        // constructor/destructor
        constructor Create;
        destructor Destroy; override;
        // Public Methods
        function Add(aItem: TRunStepBuilder): integer;
        procedure Remove(aItem: TRunStepBuilder);
        procedure Insert(aIndex: Integer; aItem: TRunStepBuilder);
        // Properties
        property Count: integer read GetCount;
        property Items[index: Integer]: TRunStepBuilder read GetItem; default;
        property OwnsObjects: boolean read GetOwnsObjects write SetOwnsObjects;
    end;


    // ##################################################################################################


implementation


{ TRunStepBuilderList }

function TRunStepBuilderList.Add(aItem: TRunStepBuilder): Integer;
begin
    result := FList.Add(aItem);
end;

constructor TRunStepBuilderList.Create;
begin
    inherited;

    FList := TGenericObjectList<TRunStepBuilder>.Create();
end;

destructor TRunStepBuilderList.Destroy;
begin
    FList.Free;

    inherited;
end;

function TRunStepBuilderList.GetCount: integer;
begin
    result := FList.Count;
end;

function TRunStepBuilderList.GetItem(aIndex: Integer): TRunStepBuilder;
begin
    result := FList.Items[aIndex] as TRunStepBuilder;
end;

function TRunStepBuilderList.GetOwnsObjects: boolean;
begin
    result := FList.OwnsObjects;
end;

procedure TRunStepBuilderList.Insert(aIndex: Integer; aItem: TRunStepBuilder);
begin
    FList.Insert(aIndex, aItem);
end;

procedure TRunStepBuilderList.Remove(aItem: TRunStepBuilder);
begin
    FList.Remove(aItem);
end;

procedure TRunStepBuilderList.SetOwnsObjects(aOwnsObjects: boolean);
begin
    FList.OwnsObjects := aOwnsObjects;
end;


end.
