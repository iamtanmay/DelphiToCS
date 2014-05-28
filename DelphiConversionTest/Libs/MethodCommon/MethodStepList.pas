unit MethodStepList;
{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : List of method steps
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------- ----------------------------------------------
  31.08.05 wl                               TN2541.0  initial version
  08.01.08 wl  Remove,Insert                TN3972    neu
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  -------------------------------------------------------------------------------------------------- }


interface


uses
    ListClasses,
    MethodStep;

type
    TMethodStepList = class
    protected
        FList: TGenericObjectList<TMethodStep>;
    private
        function GetCount: integer;
        function GetItem(aIndex: Integer): TMethodStep;
        procedure SetOwnsObjects(aOwnsObjects: boolean);
        function GetOwnsObjects: boolean;
    public
        // constructor/destructor
        constructor Create;
        destructor Destroy; override;
        // Public Methods
        function Add(aItem: TMethodStep): integer;
        procedure Remove(aItem: TMethodStep);
        procedure Insert(aIndex: Integer; aItem: TMethodStep);
        // Properties
        property Count: integer read GetCount;
        property Items[index: Integer]: TMethodStep read GetItem; default;
        property OwnsObjects: boolean read GetOwnsObjects write SetOwnsObjects;
    end;


    // ##################################################################################################


implementation


{ TMethodStepList }

function TMethodStepList.Add(aItem: TMethodStep): Integer;
begin
    result := FList.Add(aItem);
end;

constructor TMethodStepList.Create;
begin
    inherited;

    FList := TGenericObjectList<TMethodStep>.Create;
end;

destructor TMethodStepList.Destroy;
begin
    FList.Free;

    inherited;
end;

function TMethodStepList.GetCount: integer;
begin
    result := FList.Count;
end;

function TMethodStepList.GetItem(aIndex: Integer): TMethodStep;
begin
    result := FList.Items[aIndex] as TMethodStep;
end;

function TMethodStepList.GetOwnsObjects: boolean;
begin
    result := FList.OwnsObjects;
end;

procedure TMethodStepList.Insert(aIndex: Integer; aItem: TMethodStep);
begin
    FList.Insert(aIndex, aItem);
end;

procedure TMethodStepList.Remove(aItem: TMethodStep);
begin
    FList.Remove(aItem);
end;

procedure TMethodStepList.SetOwnsObjects(aOwnsObjects: boolean);
begin
    FList.OwnsObjects := aOwnsObjects;
end;


end.
