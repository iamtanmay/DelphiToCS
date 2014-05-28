{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  25.06.13 wl                                      TN6178    Initial Revision
  25.06.13 wl                                      TN6178    TIdentifier = TIdentItem, TIdentList von MemoryClasses hierher
  ----------------------------------------------------------------------------------------------------------- }

unit Identifier;


interface


uses
    SyncObjs,
    Generics.Collections,
    Streamable;

type
    // Objekte, die Value nicht verwalten:
    TIdentRec = record
        Key: string;
        Value: TStreamableItem;
    end;

    TIdentRecList = class(TList<TIdentRec>)
    public
        function FindValueByName(const aIdentName: string): TStreamableItem;
    end;

    // Objekte, die Value verwalten und zerstören:
    TIdentifier = class
    strict private
        fKey: string;
        fValue: TStreamableItem;

        procedure SetValue(const aValue: TStreamableItem);
    protected
        fOwnsValue: boolean;
        { TODO -owl : Memory Leak Danger: Value wird nicht unbedingt zerstört }
    public
        constructor Create(const aKey: string; aOwnsValue: boolean);
        destructor Destroy; override;

        procedure ClearValue();
        function GetIdentRec: TIdentRec;

        property Value: TStreamableItem read fValue write SetValue;
        property Key: string read fKey;
    end;

    TIdentifierList = class(TObjectList<TIdentifier>)
    public
        function FindIdentByName(const aIdentName: string): TIdentifier;
    end;

    TThreadSafeIdentifierList = class
    private
        fList: TIdentifierList;
        fCriticalSection: TCriticalSection;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): TObject;
    public
        constructor Create;
        destructor Destroy(); override;
        procedure Add(const aValue: TIdentifier);
        function FindIdentByName(const aIdentName: string): TIdentifier;
        property Count: integer read GetCount;
        property this[aIndex: integer]: TObject read GetItemAt; default;
    end;


implementation


uses
    SysUtils;

{ TIdentifier }

constructor TIdentifier.Create(const aKey: string; aOwnsValue: boolean);
begin
    inherited Create;
    fKey := aKey;
    fValue := nil;
    fOwnsValue := aOwnsValue;
end;

destructor TIdentifier.Destroy;
begin
    if fOwnsValue then
        FreeAndNil(fValue);

    inherited;
end;

function TIdentifier.GetIdentRec: TIdentRec;
begin
    result.Key := self.Key;
    result.Value := self.Value;
end;

procedure TIdentifier.ClearValue();
begin
    FreeAndNil(fValue);
end;

procedure TIdentifier.SetValue(const aValue: TStreamableItem);
begin
    if fOwnsValue and (fValue <> nil) then
        ClearValue();

    fValue := aValue;
end;

{ TIdentRecList }

function TIdentRecList.FindValueByName(const aIdentName: string): TStreamableItem;
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
    begin
        if (self[x].Key = aIdentName) then
            EXIT(self[x].Value);
    end;
    EXIT(nil);
end;

{ TIdentifierList }

function TIdentifierList.FindIdentByName(const aIdentName: string): TIdentifier;
var
    x: integer;
begin
    for x := 0 to self.Count - 1 do
    begin
        if (self[x].Key = aIdentName) then
            EXIT(self[x]);
    end;
    EXIT(nil);
end;

{ TThreadSafeIdentifierList }

constructor TThreadSafeIdentifierList.Create;
begin
    inherited Create();
    fCriticalSection := TCriticalSection.Create();
    fList := TIdentifierList.Create;
end;

destructor TThreadSafeIdentifierList.Destroy;
begin
    FreeAndNil(fList);
    FreeAndNil(fCriticalSection);
    inherited;
end;

procedure TThreadSafeIdentifierList.Add(const aValue: TIdentifier);
begin
    fCriticalSection.Enter();
    try
        fList.Add(aValue);
    finally
        fCriticalSection.Leave();
    end;
end;

function TThreadSafeIdentifierList.FindIdentByName(const aIdentName: string): TIdentifier;
begin
    fCriticalSection.Enter();
    try
        EXIT(fList.FindIdentByName(aIdentName));
    finally
        fCriticalSection.Leave();
    end;
end;

function TThreadSafeIdentifierList.GetCount: integer;
begin
    fCriticalSection.Enter();
    try
        result := fList.Count;
    finally
        fCriticalSection.Leave();
    end;
end;

function TThreadSafeIdentifierList.GetItemAt(aIndex: integer): TObject;
begin
    fCriticalSection.Enter();
    try
        result := fList[aIndex];
    finally
        fCriticalSection.Leave();
    end;
end;


end.
