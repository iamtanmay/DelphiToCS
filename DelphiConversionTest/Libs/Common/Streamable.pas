{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  17.02.09 pk                                        TN4232      Initial Revision
  24.02.09 pk  TStreamable                           TN4232      fParentID
  24.02.09 pk  TCustomStreamable                     TN4232      previously called TStreamable
  25.02.09 pk  TStreamableStringList                 TN4232      New
  04.03.09 pk  TStreamableKeyValueList               TN4232      New
  30.03.09 pk  TStreamableObjectList                 TN4490      New Clear
  08.06.09 pk  TStreamableItem                       TN4585.2    New
  08.06.09 pk  TStreamableIntList                    TN4585.2    Now based on TStreamableItem
  06.07.09 pk  TDateTimeStreamableItem               TN4585.4    New
  13.07.09 pk  TAnsiStrStreamableItem                TN4585.4    New
  30.07.09 pk  TStreamableStringList                 TN4585.4    GetAsObj
  26.08.09 wl  TStreamableItem                       TN4746      alle Konvertierfunktionen führen zu Exceptions
  26.08.09 wl  TStrStreamableItem.SetAsInt           TN4746      String kann auch als integer gesetzt werden (mehr Möglichkeiten denkbar)
  04.11.09 pk                               	        TN4843      Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  04.02.10 pk                                        TN4972      changes for Restart
  13.04.10 wl                                        TN5044      uses StringUtilities
  07.05.10 pk  TKeyValueItem                         TN5092      fValue changed from string to StreamableItem
  17.05.10 pk  TStreamableGenericList                TN5092      New Capacity property, so that List can now be used as an array
  19.10.10 pk                                        TN5305      changes needed for CoreClient/Server
  31.05.11 wl  TFloatStreamableItem.GetAsStr         TN5594      entfernt (wegen regionalen Einstellungen)
  10.04.13 wl                                        TN6045      uses geändert
  ----------------------------------------------------------------------------------------------------------------------- }

unit Streamable;


interface


uses
    Classes,
    SysUtils,
    Generics.Collections;

const
    cListStringDelim = ',';
    cListKeyValueSeparator = '=';

type
    EStreamableConversionError = class(Exception);

    TCustomStreamable = class(TPersistent)
    public
        constructor Create(); virtual;
    end;

    TStreamable = class(TCustomStreamable)
    end;

    TStreamableItem = class(TCustomStreamable)
    protected
        function GetAsInt: integer; virtual;
        procedure SetAsInt(aValue: integer); virtual;
        function GetAsFloat: extended; virtual;
        procedure SetAsFloat(aValue: extended); virtual;
        function GetAsCard: cardinal; virtual;
        procedure SetAsCard(aValue: cardinal); virtual;
        function GetAsStr: string; virtual;
        procedure SetAsStr(const aValue: string); virtual;
        function GetAsObj: TObject; virtual;
        procedure SetAsObj(aValue: TObject); virtual;
        function GetAsBool: boolean; virtual;
        procedure SetAsBool(aValue: boolean); virtual;
        function GetAsDateTime: TDateTime; virtual;
        procedure SetAsDateTime(const aValue: TDateTime); virtual;
        function GetAsVar(): variant; virtual;
        procedure SetAsVar(const Value: variant); virtual;
        function GetAsStrArray(): TArray<string>; virtual;
        procedure SetAsStrArray(const aArray: TArray<string>); virtual;
        function GetAsIntArray(): TArray<integer>; virtual;
        procedure SetAsIntArray(const aArray: TArray<integer>); virtual;

    public
        property AsInt: integer read GetAsInt write SetAsInt;
        property AsCard: cardinal read GetAsCard write SetAsCard;
        property AsStr: string read GetAsStr write SetAsStr;
        property AsBool: boolean read GetAsBool write SetAsBool;
        property AsFloat: extended read GetAsFloat write SetAsFloat;
        property AsObj: TObject read GetAsObj write SetAsObj;
        property AsVar: variant read GetAsVar write SetAsVar;
        property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
        property AsStrArray: TArray<string>read GetAsStrArray write SetAsStrArray;
        property AsIntArray: TArray<integer>read GetAsIntArray write SetAsIntArray;
    end;

    TNullStreamableItem = class(TStreamableItem)
    public
        constructor Create(); override;
    end;

    TIntStreamableItem = class(TStreamableItem)
    protected
        fValue: integer;
        function GetAsInt: integer; override;
        procedure SetAsInt(aValue: integer); override;
        function GetAsStr: string; override;
        function GetAsFloat: extended; override;
    public
        constructor Create(); overload; override;
        constructor Create(const aValue: integer); reintroduce; overload;
    published
        property Value: integer read fValue write fValue;
    end;

    TCardStreamableItem = class(TStreamableItem)
    protected
        fValue: cardinal;
        function GetAsCard: cardinal; override;
        procedure SetAsCard(aValue: cardinal); override;
    public
        constructor Create(); overload; override;
        constructor Create(const aValue: cardinal); reintroduce; overload;
    published
        property Value: cardinal read fValue write fValue;
    end;

    TStrStreamableItem = class(TStreamableItem)
    protected
        fValue: string;
        function GetAsStr: string; override;
        procedure SetAsStr(const aValue: string); override;
        procedure SetAsInt(aValue: integer); override;
    public
        constructor Create(); overload; override;
        constructor Create(const aValue: string); reintroduce; overload;
    published
        property Value: string read fValue write fValue;
    end;

    TAnsiStrStreamableItem = class(TStreamableItem)
    protected
        fValue: ansistring;
        function GetAsStr: string; override;
        procedure SetAsStr(const aValue: string); override;
        procedure SetAsInt(aValue: integer); override;
    public
        constructor Create(); overload; override;
        constructor Create(const aValue: ansistring); reintroduce; overload;
    published
        property Value: ansistring read fValue write fValue;
    end;

    TObjStreamableItem = class(TStreamableItem)
    protected
        fValue: TObject;
        function GetAsObj: TObject; override;
        procedure SetAsObj(aValue: TObject); override;
    public
        constructor Create(); overload; override;
        constructor Create(const aValue: TObject); reintroduce; overload;
        { TODO -owl : Memory Leak Danger: Value wird nicht zerstört }
    published
        property Value: TObject read fValue write fValue;
    end;

    TBoolStreamableItem = class(TStreamableItem)
    protected
        fValue: boolean;
        function GetAsBool: boolean; override;
        procedure SetAsBool(aValue: boolean); override;
    public
        constructor Create(); overload; override;
        constructor Create(const aValue: boolean); reintroduce; overload;
    published
        property Value: boolean read fValue write fValue;
    end;

    TFloatStreamableItem = class(TStreamableItem)
    protected
        fValue: extended;
        function GetAsFloat: extended; override;
        procedure SetAsFloat(aValue: extended); override;
    public
        constructor Create(); overload; override;
        constructor Create(const aValue: extended); reintroduce; overload;
    published
        property Value: extended read fValue write fValue;
    end;

    TDateTimeStreamableItem = class(TStreamableItem)
    protected
        fValue: TDateTime;
        function GetAsDateTime: TDateTime; override;
        procedure SetAsDateTime(const aValue: TDateTime); override;
    public
        constructor Create(); overload; override;
        constructor Create(const aValue: TDateTime); reintroduce; overload;
    published
        property Value: TDateTime read fValue write fValue;
    end;

    TVariantStreamableItem = class(TStreamableItem)
    protected
        fValue: variant;
        function GetAsVar(): variant; override;
        procedure SetAsVar(const aValue: variant); override;
    public
        constructor Create(); overload; override;
        constructor Create(const aValue: variant); reintroduce; overload;
    published
        property Value: variant read fValue write fValue;
    end;

    TStreamableGenericList<T: TCustomStreamable> = class(TStreamableItem)
    protected
        fList: TObjectList<T>;
        function GetCount: integer;
        function GetItemAt(aIndex: integer): T;
        function GetOwnsObjects: boolean;
        procedure SetOwnsObjects(const aValue: boolean);
        procedure SetCapacity(const aValue: integer);
        function GetCapacity(): integer;
        function GetAsObj: TObject; override;
        function CreateList(aOwnsObjects: boolean): TObjectList<T>; virtual;
    public
        constructor Create(); overload; override;
        constructor Create(aOwnsObjects: boolean); reintroduce; overload;
        destructor Destroy(); override;
        procedure Clear();
        procedure Add(const aObject: T);
        procedure Remove(const aObject: T);
        procedure Extract(const aObject: T);
        property Capacity: integer read GetCapacity write SetCapacity;
        property Count: integer read GetCount;
        property this[aIndex: integer]: T read GetItemAt; default;
    published
        property OwnsObjects: boolean read GetOwnsObjects write SetOwnsObjects;
    end;

    TStreamableInternalList = class(TObjectList<TCustomStreamable>);

    TStreamableObjectList = class(TStreamableGenericList<TCustomStreamable>)
    protected
        function CreateList(aOwnsObjects: boolean): TObjectList<TCustomStreamable>; override;
    published
        property Items: TObjectList<TCustomStreamable>read fList;
    end;

    TStreamableStringList = class(TStreamableObjectList)
    private
        function GetItemAt(aIndex: integer): string;
    protected
        function GetAsStr(): string; override;
        procedure SetAsStr(const aValue: string); override;
        function GetAsStrArray(): TArray<string>; override;
        procedure SetAsStrArray(const aArray: TArray<string>); override;
        function GetAsObj(): TObject; override;
    public
        procedure Add(const aValue: string);
        procedure FromStringArray(const aArray: TArray<string>);
        function ToStringArray(): TArray<string>;
        procedure FromString(const aValue: string);
        function ToString(): string; override;
        property this[aIndex: integer]: string read GetItemAt; default;
    end;

    TStreamableIntList = class(TStreamableObjectList)
    private
        function GetItemAt(aIndex: integer): integer;
    protected
        function GetAsIntArray(): TArray<integer>; override;
        procedure SetAsIntArray(const aArray: TArray<integer>); override;
    public
        procedure Add(const aValue: integer);
        property this[aIndex: integer]: integer read GetItemAt; default;
    end;

    TStreamableKeyValueItem = class(TStreamable)
    private
        fKey: string;
        fValue: TStreamableItem;
    public
        constructor Create(); overload; override;
        constructor Create(const aKey: string; const aValue: TStreamableItem); reintroduce; overload;
        { TODO -owl : Memory Leak Danger: Value wird nicht zerstört }
        function ToString(): string; override;
    published
        property Key: string read fKey write fKey;
        property Value: TStreamableItem read fValue write fValue;
    end;

    TStreamableKeyValueList = class(TStreamableObjectList)
    private
        function GetItemAt(aIndex: integer): TStreamableKeyValueItem;
    public
        procedure Add(const aKey: string; const aValue: TStreamableItem);
        function ToString(): string; override;
        property this[aIndex: integer]: TStreamableKeyValueItem read GetItemAt; default;
    end;


implementation


uses
    TypeRegistry,
    StringUtilities,
    Variants;

{ TCustomStreamable }

constructor TCustomStreamable.Create;
begin
    inherited Create();
end;

{ TStreamableGenericList<T> }

constructor TStreamableGenericList<T>.Create(aOwnsObjects: boolean);
begin
    inherited Create();
    fList := CreateList(aOwnsObjects);
end;

constructor TStreamableGenericList<T>.Create();
begin
    Create(true);
end;

destructor TStreamableGenericList<T>.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

function TStreamableGenericList<T>.CreateList(aOwnsObjects: boolean): TObjectList<T>;
begin
    result := TObjectList<T>.Create(aOwnsObjects);
end;

procedure TStreamableGenericList<T>.Extract(const aObject: T);
begin
    fList.Extract(aObject);
end;

function TStreamableGenericList<T>.GetCount(): integer;
begin
    result := fList.Count;
end;

procedure TStreamableGenericList<T>.Add(const aObject: T);
begin
    fList.Add(aObject);
end;

procedure TStreamableGenericList<T>.Remove(const aObject: T);
begin
    fList.Remove(aObject);
end;

procedure TStreamableGenericList<T>.SetCapacity(const aValue: integer);
begin
    fList.Count := aValue;
end;

function TStreamableGenericList<T>.GetAsObj: TObject;
begin
    result := self;
end;

function TStreamableGenericList<T>.GetCapacity(): integer;
begin
    result := fList.Count;
end;

procedure TStreamableGenericList<T>.SetOwnsObjects(const aValue: boolean);
begin
    fList.OwnsObjects := aValue;
end;

function TStreamableGenericList<T>.GetOwnsObjects: boolean;
begin
    result := fList.OwnsObjects;
end;

function TStreamableGenericList<T>.GetItemAt(aIndex: integer): T;
begin
    result := fList[aIndex];
end;

procedure TStreamableGenericList<T>.Clear;
begin
    fList.Clear();
end;

{ TStreamableStringList }

procedure TStreamableStringList.Add(const aValue: string);
begin
    inherited Add(TStrStreamableItem.Create(aValue));
end;

function TStreamableStringList.GetItemAt(aIndex: integer): string;
begin
    result := (( inherited Items[aIndex]) as TStrStreamableItem).AsStr;
end;

procedure TStreamableStringList.SetAsStrArray(const aArray: TArray<string>);
var
    x: integer;
begin
    for x := 0 to high(aArray) do
    begin
        self.Add(aArray[x]);
    end;
end;

function TStreamableStringList.GetAsStrArray: TArray<string>;
var
    x: integer;
begin
    SetLength(result, self.Count);
    for x := 0 to self.Count - 1 do
    begin
        result[x] := self[x];
    end;
end;

procedure TStreamableStringList.SetAsStr(const aValue: string);
begin
    self.AsStrArray := TStringUtilities.StringToStringArray(aValue, cListStringDelim);
end;

function TStreamableStringList.GetAsObj: TObject;
begin
    result := self;
end;

function TStreamableStringList.GetAsStr: string;
begin
    result := TStringUtilities.StringArrayToString(self.AsStrArray, cListStringDelim);
end;

function TStreamableStringList.ToStringArray: TArray<string>;
begin
    result := self.AsStrArray;
end;

procedure TStreamableStringList.FromStringArray(const aArray: TArray<string>);
begin
    self.AsStrArray := aArray;
end;

procedure TStreamableStringList.FromString(const aValue: string);
begin
    self.AsStr := aValue;
end;

function TStreamableStringList.ToString: string;
begin
    result := self.AsStr;
end;

{ TStreamableIntList }

procedure TStreamableIntList.Add(const aValue: integer);
begin
    inherited Add(TIntStreamableItem.Create(aValue));
end;

function TStreamableIntList.GetItemAt(aIndex: integer): integer;
begin
    result := (( inherited Items[aIndex]) as TIntStreamableItem).AsInt;
end;

procedure TStreamableIntList.SetAsIntArray(const aArray: TArray<integer>);
var
    x: integer;
begin
    for x := 0 to high(aArray) do
    begin
        self.Add(aArray[x]);
    end;
end;

function TStreamableIntList.GetAsIntArray: TArray<integer>;
var
    x: integer;
begin
    SetLength(result, self.Count);
    for x := 0 to self.Count - 1 do
    begin
        result[x] := self[x];
    end;
end;

{ TStreamableKeyValueItem }

constructor TStreamableKeyValueItem.Create(const aKey: string; const aValue: TStreamableItem);
begin
    inherited Create();
    fKey := aKey;
    fValue := aValue;
end;

function TStreamableKeyValueItem.ToString: string;
begin
    if self.Key <> '' then
        result := result + self.Key + cListKeyValueSeparator;
    result := result + self.Value.ToString;
end;

constructor TStreamableKeyValueItem.Create;
begin
    Create('', nil);

end;

{ TStreamableKeyValueList }

procedure TStreamableKeyValueList.Add(const aKey: string; const aValue: TStreamableItem);
begin
    inherited Add(TStreamableKeyValueItem.Create(aKey, aValue));
end;

function TStreamableKeyValueList.GetItemAt(aIndex: integer): TStreamableKeyValueItem;
begin
    result := (( inherited Items[aIndex]) as TStreamableKeyValueItem);
end;

function TStreamableKeyValueList.ToString: string;
var
    x: integer;
    xItem: TStreamableKeyValueItem;
begin
    result := '';
    for x := 0 to self.Count - 1 do
    begin
        xItem := self[x];
        if result <> '' then
            result := cListStringDelim;
        if xItem.Key <> '' then
            result := result + xItem.Key + cListKeyValueSeparator;
        result := result + xItem.ToString;
    end;
end;

{ TStreamableItem }

function TStreamableItem.GetAsInt: integer;
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.GetAsInt');
end;

procedure TStreamableItem.SetAsInt(aValue: integer);
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.SetAsInt');
end;

function TStreamableItem.GetAsCard: cardinal;
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.GetAsCard');
end;

procedure TStreamableItem.SetAsCard(aValue: cardinal);
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.SetAsCard');
end;

function TStreamableItem.GetAsStr: string;
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.GetAsStr');
end;

procedure TStreamableItem.SetAsStr(const aValue: string);
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.SetAsStr');
end;

function TStreamableItem.GetAsObj: TObject;
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.GetAsObj');
end;

procedure TStreamableItem.SetAsObj(aValue: TObject);
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.SetAsObj');
end;

function TStreamableItem.GetAsBool: boolean;
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.GetAsBool');
end;

procedure TStreamableItem.SetAsBool(aValue: boolean);
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.SetAsBool');
end;

function TStreamableItem.GetAsDateTime: TDateTime;
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.GetAsDateTime');
end;

function TStreamableItem.GetAsFloat: extended;
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.GetAsFloat');
end;

procedure TStreamableItem.SetAsDateTime(const aValue: TDateTime);
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.SetAsDateTime');
end;

procedure TStreamableItem.SetAsFloat(aValue: extended);
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.SetAsFloat');
end;

function TStreamableItem.GetAsStrArray: TArray<string>;
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.GetAsStrArray');
end;

function TStreamableItem.GetAsVar: variant;
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.GetAsVar');
end;

procedure TStreamableItem.SetAsStrArray(const aArray: TArray<string>);
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.SetAsStrArray');
end;

procedure TStreamableItem.SetAsVar(const Value: variant);
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.SetAsVar');
end;

function TStreamableItem.GetAsIntArray: TArray<integer>;
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.GetAsIntArray');
end;

procedure TStreamableItem.SetAsIntArray(const aArray: TArray<integer>);
begin
    raise EStreamableConversionError.Create('Undefined conversion ' + self.ClassName + '.SetAsIntArray');
end;

{ TNullStreamableItem }

constructor TNullStreamableItem.Create;
begin
    inherited Create();
end;

{ TIntStreamableItem }

constructor TIntStreamableItem.Create();
begin
    Create(0);
end;

constructor TIntStreamableItem.Create(const aValue: integer);
begin
    inherited Create();
    fValue := aValue;
end;

function TIntStreamableItem.GetAsInt: integer;
begin
    result := fValue;
end;

procedure TIntStreamableItem.SetAsInt(aValue: integer);
begin
    fValue := aValue;
end;

function TIntStreamableItem.GetAsStr: string;
begin
    result := IntToStr(fValue);
end;

function TIntStreamableItem.GetAsFloat: extended;
begin
    result := fValue;
end;

{ TCardStreamableItem }

constructor TCardStreamableItem.Create();
begin
    Create(0);
end;

constructor TCardStreamableItem.Create(const aValue: cardinal);
begin
    inherited Create();
    fValue := aValue;
end;

function TCardStreamableItem.GetAsCard: cardinal;
begin
    result := fValue;
end;

procedure TCardStreamableItem.SetAsCard(aValue: cardinal);
begin
    fValue := aValue;
end;

{ TStrStreamableItem }

constructor TStrStreamableItem.Create();
begin
    Create('');
end;

constructor TStrStreamableItem.Create(const aValue: string);
begin
    inherited Create();
    fValue := aValue;
end;

function TStrStreamableItem.GetAsStr: string;
begin
    result := fValue;
end;

procedure TStrStreamableItem.SetAsInt(aValue: integer);
begin
    self.SetAsStr(IntToStr(aValue));
end;

procedure TStrStreamableItem.SetAsStr(const aValue: string);
begin
    fValue := aValue;

end;

{ TAnsiStrStreamableItem }

constructor TAnsiStrStreamableItem.Create();
begin
    Create('');
end;

constructor TAnsiStrStreamableItem.Create(const aValue: ansistring);
begin
    inherited Create();
    fValue := aValue;
end;

function TAnsiStrStreamableItem.GetAsStr: string;
begin
    result := string(fValue);
end;

procedure TAnsiStrStreamableItem.SetAsInt(aValue: integer);
begin
    self.SetAsStr(IntToStr(aValue));
end;

procedure TAnsiStrStreamableItem.SetAsStr(const aValue: string);
begin
    fValue := ansistring(aValue);
end;

{ TObjStreamableItem }
constructor TObjStreamableItem.Create();
begin
    Create(nil);
end;

constructor TObjStreamableItem.Create(const aValue: TObject);
begin
    inherited Create();
    fValue := aValue;
end;

function TObjStreamableItem.GetAsObj: TObject;
begin
    result := fValue;
end;

procedure TObjStreamableItem.SetAsObj(aValue: TObject);
begin
    fValue := aValue;
end;

{ TBoolStreamableItem }

constructor TBoolStreamableItem.Create();
begin
    Create(false);
end;

constructor TBoolStreamableItem.Create(const aValue: boolean);
begin
    inherited Create();
    fValue := aValue;
end;

function TBoolStreamableItem.GetAsBool: boolean;
begin
    result := fValue;
end;

procedure TBoolStreamableItem.SetAsBool(aValue: boolean);
begin
    fValue := aValue;
end;

{ TFloatStreamableItem }

constructor TFloatStreamableItem.Create();
begin
    Create(0.0);
end;

constructor TFloatStreamableItem.Create(const aValue: extended);
begin
    inherited Create();
    fValue := aValue;
end;

function TFloatStreamableItem.GetAsFloat: extended;
begin
    result := fValue;
end;

procedure TFloatStreamableItem.SetAsFloat(aValue: extended);
begin
    fValue := aValue;
end;

{ TDateTimeStreamableItem }

constructor TDateTimeStreamableItem.Create();
begin
    Create(0);
end;

constructor TDateTimeStreamableItem.Create(const aValue: TDateTime);
begin
    inherited Create();
    fValue := aValue;
end;

function TDateTimeStreamableItem.GetAsDateTime: TDateTime;
begin
    result := fValue;
end;

procedure TDateTimeStreamableItem.SetAsDateTime(const aValue: TDateTime);
begin
    fValue := aValue;
end;

{ TVariantStreamableItem }

constructor TVariantStreamableItem.Create();
begin
    Create(0);
end;

constructor TVariantStreamableItem.Create(const aValue: Variant);
begin
    inherited Create();
    fValue := aValue;
end;

function TVariantStreamableItem.GetAsVar: variant;
begin
    result := fValue;
end;

procedure TVariantStreamableItem.SetAsVar(const aValue: variant);
begin
    fValue := aValue;
end;

{ TStreamableObjectList }

function TStreamableObjectList.CreateList(aOwnsObjects: boolean): TObjectList<TCustomStreamable>;
begin
    result := TStreamableInternalList.Create(aOwnsObjects);
end;


initialization


TTypeRegistry.InstanceRegisterTypes([TStreamableObjectList, TStreamableInternalList, TStreamableStringList,
    TStreamableIntList, TStreamableKeyValueList, TStreamableKeyValueItem]);

TTypeRegistry.InstanceRegisterTypes([TNullStreamableItem, TIntStreamableItem, TCardStreamableItem,
    TStrStreamableItem, TAnsiStrStreamableItem, TBoolStreamableItem, TFloatStreamableItem,
    TVariantStreamableItem, TObjStreamableItem]);


end.
