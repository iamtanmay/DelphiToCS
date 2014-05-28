{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------- --------  ----------------------------------------------------------
  06.11.08 pk                                        TN4279    Initial Revision
  17.02.09 pk  TParserIdentDataType                  TN4232    new idtReference
  07.05.10 pk  TParserIdentDataType                  TN5092    new idtBool
  27.06.11 wl  TParserIdentDataType                  TN5609   neu: idtArray
  01.03.12 wl  TParserIdentDataType                  TN5820   idtVariant entfernt, idtNumber in idtFloat umbenannt
  01.03.12 wl  STR_TOKEN_TRUE/FALSE                  TN5820   von ParserTokenizer hierher
  01.03.12 wl  STR_TOKEN_TYPE_                       TN5820   neu: const-Werte für STRING,FLOAT,..
  01.03.12 wl  TAttrValueUtils                       TN5820   von ParserEvalNode hierher
  01.03.12 wl  TArg,...                              TN5822   alles aus ArgClass hierher
  01.03.12 wl  TArgUtils                             TN5822   zusammengefasst mit TAttrValueUtils
  05.07.12 wl  TArgUtils.CreateArgByVariantValue     TN5917   von DataExchangeDatasetWrapper hierher
  05.07.12 wl  TArgUtils.DefineTypeByValueArray      TN5917   von MethBuildField hierher
  05.07.12 wl  TArgUtils.CreateArgByValue,CreateArgByVariantValue  TN5917   neu: erzeugen jetzt auch TBoolArg!
  30.08.12 ts  TArgUtils.DefineTypeByValue           TN5966   führende Nullen bei strings werden nicht als Zahlen interpretiert
  27.03.13 wl  TArrayArg                             TN6045   properties geändert
  12.08.13 wl  TArrayArg.GetAsStr                    TN6214   geändert: Format bisher [a,b,c], jetzt A(a,b,c)
  14.02.14 ts  TArgUtils.DefineTypeByValueArray      TN6361   integer und float ergeben float als type (kein string)
  20.03.14 ts  TArrayArg.ClearItems                  TN6317   Argument out of range, delete order changed
  ----------------------------------------------------------------------------------------------------------------------- }

unit ParserIdentDataType;


interface


uses
    SysUtils,
    Streamable;

const
    STR_TOKEN_TRUE = 'TRUE';
    STR_TOKEN_FALSE = 'FALSE';

    STR_TOKEN_TYPE_FLOAT = 'FLOAT';
    STR_TOKEN_TYPE_STRING = 'STRING';
    STR_TOKEN_TYPE_BOOLEAN = 'BOOLEAN';
    STR_TOKEN_TYPE_INTEGER = 'INTEGER';
    STR_TOKEN_TYPE_ARRAY = 'ARRAY';
    STR_TOKEN_TYPE_OBJECT = 'OBJECT';

type
    EParserConversionException = class(Exception);
    TParserIdentDataType = (idtNone, idtString, idtFloat, idtInteger, idtBool, idtReference, idtArray);

    // Arg object
    TArg = TStreamableItem;
    TIntArg = TIntStreamableItem;
    TStrArg = TStrStreamableItem;
    TBoolArg = TBoolStreamableItem;
    TDblArg = TFloatStreamableItem;

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

    TArrayArg = class(TArg)
    private const
        cArgArrayBaseOffset = 1;
        cArgArrayIndexUndefined = low(integer);
    strict private
        fList: TStreamableObjectList;
        procedure CheckBounds(const aUserIndex: integer);
        function GetArrayLength: integer;
        procedure SetArrayLength(const aValue: integer);
        procedure ClearItems(const aFirstUserIndex, aLastUserIndex: integer);
        class function UserIndexToInternalIndex(const aUserIndex: integer): integer; static;
        class function InternalIndexToUserIndex(const aIndex: integer): integer; static;
        class function GetMinUserIndex(): integer; static;
        function GetMaxUserIndex(): integer;
        function GetItemByStandardIndex(aIndex: integer): TArg;
        procedure SetItemByStandardIndex(aIndex: integer; const aValue: TArg);
    strict protected
        function GetAsStr: string; override;
    public
        constructor Create(); override;
        destructor Destroy(); override;

        class function UndefinedBoundsIndex(): integer;

        function GetAsStringArray: TArray<string>;
        function GetItemByUserIndex(aUserIndex: integer): TArg;
        procedure SetItemByUserIndex(aUserIndex: integer; const aValue: TArg);
        property ArrayLength: integer read GetArrayLength write SetArrayLength;
        property Count: integer read GetArrayLength;
        class property MinUserIndex: integer read GetMinUserIndex;
        property MaxUserIndex: integer read GetMaxUserIndex;
        property Items[aIndex: integer]: TStreamableItem read GetItemByStandardIndex
            write SetItemByStandardIndex; default;
    published
        property List: TStreamableObjectList read fList write fList;
    end;

    TArgUtils = record
    private
        class function ItemArrayToStr(const aArray: TArrayArg): string; static;
    public
        class function ItemToStr(const aValue: TArg; const aUserIndex: integer): string; overload; static;
        class function ItemToStr(const aValue: TArg): string; overload; static;
        class function ListToStr(const aList: TStreamableKeyValueList): string; static;

        class function IdentTypeToLogStr(const aIdentValue: TArg; const aArrayIndex: integer): string; static;
        class function IdentifyTypeByName(const aTypeName: string): TParserIdentDataType; static;
        class function StringToBool(const aValue: string): boolean; static;
        class function TryStrToBoolean(const aValue: string; out oBoolValue: boolean): boolean; static;
        class function BoolValueToStr(aValue: boolean): string; static;
        class function BoolValueToInt(aValue: boolean): integer; static;
        class function DataValueToStr(aValue: TArg): string; static;
        class function DataValueToFloat(aValue: TArg): double; static;
        class function DataValueToInt(aValue: TArg): integer; static;
        class function DataValueToBool(aValue: TArg): boolean; static;
        class function DataValueToType(aValue: TArg): TParserIdentDataType; static;

        class function DefineTypeByValue(const aValue: string): TParserIdentDataType; static;
        class function DefineTypeByVariantValue(const aValue: variant; aAllowBoolean: boolean)
            : TParserIdentDataType; static;
        class function DefineTypeByValueArray(const aValues: TArray<string>): TParserIdentDataType; static;

        class function CreateArgByValue(const aValue: string): TArg; static;
        class function CreateArrayArgByValue(const aValues: TArray<string>): TArg; static;
        class function CreateArgByVariantValue(const aValue: variant; aAllowBoolean: boolean): TArg; static;

        class function CreateArgByType(const aValue: string; aDataType: TParserIdentDataType): TArg; static;
        class function CreateArrayArgByType(const aValues: TArray<string>; aDataType: TParserIdentDataType)
            : TArg; static;
        class function CreateArgByTypeVariant(const aValue: variant; aDataType: TParserIdentDataType)
            : TArg; static;
        class function CreateArgByTypeOrValue(const aValue: string; aDataType: TParserIdentDataType)
            : TArg; static;

        class function CopyAttrValue(const aValue: TArg): TArg; static;
    end;


implementation


uses
    Variants,
    StringUtilities,
    ArrayFormat,
    GeneralTypes;

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

{ TArrayArg }

constructor TArrayArg.Create();
begin
    inherited Create();
    fList := TStreamableObjectList.Create();
    fList.Capacity := 0; // ??
end;

destructor TArrayArg.Destroy;
begin
    FreeAndNil(fList);
    inherited;
end;

class function TArrayArg.UndefinedBoundsIndex(): integer;
begin
    result := cArgArrayIndexUndefined;
end;

procedure TArrayArg.ClearItems(const aFirstUserIndex, aLastUserIndex: integer);
var
    x: integer;
begin
    for x := aLastUserIndex downto aFirstUserIndex + 1 do
        fList.Remove(fList[UserIndexToInternalIndex(x)]);
end;

class function TArrayArg.UserIndexToInternalIndex(const aUserIndex: integer): integer;
begin
    result := aUserIndex - cArgArrayBaseOffset;
end;

class function TArrayArg.InternalIndexToUserIndex(const aIndex: integer): integer;
begin
    result := aIndex + cArgArrayBaseOffset;
end;

class function TArrayArg.GetMinUserIndex(): integer;
begin
    result := InternalIndexToUserIndex(0);
end;

function TArrayArg.GetAsStringArray: TArray<string>;
var
    xUserIndex: integer;
begin
    SetLength(result, self.ArrayLength);
    for xUserIndex := self.MinUserIndex to self.MaxUserIndex do
    begin
        result[UserIndexToInternalIndex(xUserIndex)] :=
            TArgUtils.ItemToStr(GetItemByUserIndex(xUserIndex), -1);
    end;
end;

function TArrayArg.GetAsStr: string;
begin
    // "interne Schreibweise"
    EXIT(TArrayFormat.StringArrayToArrayFormat(self.GetAsStringArray));
end;

function TArrayArg.GetMaxUserIndex(): integer;
begin
    result := self.ArrayLength;
end;

procedure TArrayArg.CheckBounds(const aUserIndex: integer);
begin
    if (aUserIndex < self.MinUserIndex) or (aUserIndex > self.MaxUserIndex) then
    begin
        raise Exception.CreateFmt('Array Index [%d] out of bounds', [aUserIndex]);
    end;
end;

function TArrayArg.GetItemByStandardIndex(aIndex: integer): TArg;
begin
    EXIT(fList[aIndex] as TArg);
end;

function TArrayArg.GetItemByUserIndex(aUserIndex: integer): TArg;
begin
    CheckBounds(aUserIndex);
    result := fList[UserIndexToInternalIndex(aUserIndex)] as TArg;
end;

function TArrayArg.GetArrayLength: integer;
begin
    result := fList.Capacity;
end;

procedure TArrayArg.SetItemByStandardIndex(aIndex: integer; const aValue: TArg);
begin
    fList.Items[aIndex] := aValue;
end;

procedure TArrayArg.SetItemByUserIndex(aUserIndex: integer; const aValue: TArg);
begin
    CheckBounds(aUserIndex);
    fList.Items[UserIndexToInternalIndex(aUserIndex)] := aValue;
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

    fList.Capacity := aValue;
end;

{ TArgUtils }

class function TArgUtils.ItemArrayToStr(const aArray: TArrayArg): string;
var
    x: integer;
begin
    // "lesbare Schreibweise" für Logging oder GUI

    result := '[';
    for x := aArray.MinUserIndex to aArray.MaxUserIndex do
    begin
        if x > aArray.MinUserIndex then
            result := result + TFormatUtils.CurrentListSeparator;
        result := result + ItemToStr(aArray, x);
    end;
    result := result + ']';
end;

class function TArgUtils.ItemToStr(const aValue: TArg; const aUserIndex: integer): string;
var
    xArrayArg: TArrayArg;
begin
    if aValue is TDblArg then
    begin
        result := FloatToStr(aValue.AsFloat, TFormatUtils.GetSettingsEnglishUS);
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
        if (aUserIndex < 0) then // Eventuell ist das nicht richtig, es sollte <= 0 heißen
        begin
            result := ItemArrayToStr(xArrayArg);
        end
        else
        begin
            result := ItemToStr(xArrayArg.GetItemByUserIndex(aUserIndex), TArrayArg.UndefinedBoundsIndex);
        end;
    end
    else if aValue is TReferenceArg then
    begin
        result := IntToStr(aValue.AsInt);
    end;
end;

class function TArgUtils.ItemToStr(const aValue: TArg): string;
begin
    result := ItemToStr(aValue, TArrayArg.UndefinedBoundsIndex);
end;

class function TArgUtils.ListToStr(const aList: TStreamableKeyValueList): string;
var
    x: integer;
    xItem: TStreamableKeyValueItem;
begin
    result := '';
    for x := 0 to aList.Count - 1 do
    begin
        xItem := aList[x];
        if result <> '' then
            result := result + cListStringDelim;
        if xItem.Key <> '' then
            result := result + xItem.Key + cListKeyValueSeparator;
        result := result + ItemToStr(xItem.Value, TArrayArg.UndefinedBoundsIndex);
    end;
end;

class function TArgUtils.BoolValueToInt(aValue: boolean): integer;
begin
    if aValue then
        EXIT(-1)
    else
        EXIT(0);
end;

class function TArgUtils.BoolValueToStr(aValue: boolean): string;
begin
    if aValue then
        EXIT(STR_TOKEN_TRUE)
    else
        EXIT(STR_TOKEN_FALSE);
end;

class function TArgUtils.DataValueToType(aValue: TArg): TParserIdentDataType;
begin
    if aValue is TStrArg then
        EXIT(idtString);
    if aValue is TIntArg then
        EXIT(idtInteger);
    if aValue is TDblArg then
        EXIT(idtFloat);
    if aValue is TBoolArg then
        EXIT(idtBool);
    if aValue is TReferenceArg then
        EXIT(idtReference);
    if aValue is TArrayArg then
        EXIT(idtArray);

    EXIT(idtNone);
end;

class function TArgUtils.IdentifyTypeByName(const aTypeName: string): TParserIdentDataType;
begin
    if (UpperCase(aTypeName) = STR_TOKEN_TYPE_INTEGER) then
        EXIT(idtInteger);
    if (UpperCase(aTypeName) = STR_TOKEN_TYPE_FLOAT) or (UpperCase(aTypeName) = 'DOUBLE') then
        EXIT(idtFloat);
    if (UpperCase(aTypeName) = STR_TOKEN_TYPE_BOOLEAN) then
        EXIT(idtBool);
    if (UpperCase(aTypeName) = STR_TOKEN_TYPE_STRING) then
        EXIT(idtString);

    // noch kein CAST zu Array und Reference

    EXIT(idtNone);
end;

class function TArgUtils.DataValueToBool(aValue: TArg): boolean;
begin
    case DataValueToType(aValue) of
        idtString:
            // Als TRUE gilt: TRUE, YES oder eine Zahl, die nicht 0 ist
            if (UpperCase(aValue.AsStr) = 'YES') or (UpperCase(aValue.AsStr) = STR_TOKEN_TRUE) or
                (StrToFloatDef(aValue.AsStr, 0, TFormatUtils.GetSettingsEnglishUS) <> 0) then
                EXIT(true)
            else
                EXIT(false);
        idtInteger:
            EXIT(aValue.AsInt <> 0);
        idtFloat:
            EXIT(aValue.AsFloat <> 0);
        idtBool:
            EXIT(aValue.AsBool);
        idtReference:
            EXIT(aValue.AsInt <> 0); // nicht sinnvoll, aber warum nicht
        // idtArray:
        // ASSERT(false); // nicht vorgesehen
        else
            raise EParserConversionException.Create('Conversion to BOOLEAN not possible');
    end
end;

class function TArgUtils.DataValueToFloat(aValue: TArg): double;
begin
    case DataValueToType(aValue) of
        idtString:
            EXIT(StrToFloatDef(aValue.AsStr, 0, TFormatUtils.GetSettingsEnglishUS));
        idtInteger:
            EXIT(aValue.AsInt);
        idtFloat:
            EXIT(aValue.AsFloat);
        idtBool:
            EXIT(BoolValueToInt(aValue.AsBool));
        idtReference:
            EXIT(aValue.AsInt);
        // idtArray:
        // ASSERT(false); // nicht vorgesehen
        else
            raise EParserConversionException.Create('Conversion to FLOAT not possible');
    end
end;

class function TArgUtils.DataValueToInt(aValue: TArg): integer;
begin
    case DataValueToType(aValue) of
        idtString:
            EXIT(StrToIntDef(aValue.AsStr, 0));
        idtInteger:
            EXIT(aValue.AsInt);
        idtFloat:
            EXIT(Round(aValue.AsFloat));
        idtBool:
            EXIT(BoolValueToInt(aValue.AsBool));
        idtReference:
            EXIT(aValue.AsInt);
        // idtArray:
        // ASSERT(false); // nicht vorgesehen
        else
            raise EParserConversionException.Create('Conversion to INTEGER not possible');
    end
end;

class function TArgUtils.DataValueToStr(aValue: TArg): string;
begin
    case DataValueToType(aValue) of
        idtString:
            EXIT(aValue.AsStr);
        idtInteger:
            EXIT(IntToStr(aValue.AsInt));
        idtFloat:
            EXIT(FloatToStr(aValue.AsFloat, TFormatUtils.GetSettingsEnglishUS));
        idtBool:
            EXIT(BoolValueToStr(aValue.AsBool));
        idtReference:
            EXIT(IntToStr(aValue.AsInt));
        idtArray:
            EXIT(aValue.AsStr);
        else
            raise EParserConversionException.Create('Conversion to STRING not possible');
    end
end;

class function TArgUtils.IdentTypeToLogStr(const aIdentValue: TArg; const aArrayIndex: integer): string;
var
    xArrayArg: TArrayArg;
begin
    if aIdentValue is TDblArg then
        EXIT(STR_TOKEN_TYPE_FLOAT);
    if aIdentValue is TIntArg then
        EXIT(STR_TOKEN_TYPE_INTEGER);
    if aIdentValue is TStrArg then
        EXIT(STR_TOKEN_TYPE_STRING);
    if aIdentValue is TBoolArg then
        EXIT(STR_TOKEN_TYPE_BOOLEAN);

    if aIdentValue is TArrayArg then
    begin
        xArrayArg := (aIdentValue as TArrayArg);
        if aArrayIndex = TArrayArg.UndefinedBoundsIndex then
        begin
            if (xArrayArg.ArrayLength > 0) then
                EXIT(STR_TOKEN_TYPE_ARRAY + ' OF ' +
                    IdentTypeToLogStr(xArrayArg.GetItemByUserIndex(TArrayArg.MinUserIndex),
                    TArrayArg.UndefinedBoundsIndex))
            else
                EXIT(STR_TOKEN_TYPE_ARRAY);
        end
        else
        begin
            EXIT(IdentTypeToLogStr(xArrayArg.GetItemByUserIndex(aArrayIndex),
                TArrayArg.UndefinedBoundsIndex));
        end;
    end;

    if aIdentValue is TReferenceArg then
        EXIT(STR_TOKEN_TYPE_OBJECT);

    EXIT('');
end;

class function TArgUtils.DefineTypeByValue(const aValue: string): TParserIdentDataType;
var
    xTempInt: integer;
    xTempDbl: double;
    xTempBool: boolean;
begin
    // Ist es ein Boolean?
    if TryStrToBoolean(aValue, xTempBool) then
        EXIT(idtBool);

    // Eine Zahl, die mit X beginnt, wird von StrToInt als Hex interpretiert (das wollen wir aber nicht)
    if Pos('X', Uppercase(aValue)) = 1 then
        EXIT(idtString);

    // Ein string, der mit 0 beginnt, mehr als eine Stelle hat und kein Komma, soll nicht als Zahl interpretiert werden
    if ((Pos('0', aValue) = 1) and (length(aValue) > 1) and
        (Pos(TFormatUtils.GetSettingsEnglishUS.DecimalSeparator, aValue) = 0)) then
        EXIT(idtString);

    // Ist es ein Integer?
    if TryStrToInt(aValue, xTempInt) then
        EXIT(idtInteger);

    // Ein einzelnes '.' soll nicht als Float interpretiert werden
    if (aValue = TFormatUtils.GetSettingsEnglishUS.DecimalSeparator) then
        EXIT(idtString);

    // Ist es ein Float bzw. Double?
    if TryStrToFloat(aValue, xTempDbl, TFormatUtils.GetSettingsEnglishUS) then
        EXIT(idtFloat);

    // im Zweifelsfall immer String
    EXIT(idtString);
end;

class function TArgUtils.DefineTypeByValueArray(const aValues: TArray<string>): TParserIdentDataType;
var
    x: integer;
    xDataType, xLastDataType: TParserIdentDataType;
begin
    xDataType := idtString;
    xLastDataType := idtString;

    for x := 0 to high(aValues) do
    begin
        xDataType := DefineTypeByValue(aValues[x]);

        // für den Fall, dass die Typen nicht übereinstimmen: Definiere alles als string!
        if (x > 0) and (xDataType <> xLastDataType) then
            // wenn Integer und Float, Typ kann float sein, da integer auch als float interpretiert werden kann
            if ((xDataType = idtFloat) and (xLastDataType = idtInteger)) or
                ((xDataType = idtInteger) and (xLastDataType = idtFloat)) then
                xDataType := idtFloat
            else
                EXIT(idtString);

        xLastDataType := xDataType;
    end;
    EXIT(xDataType);
end;

class function TArgUtils.CreateArrayArgByType(const aValues: TArray<string>;
    aDataType: TParserIdentDataType): TArg;
var
    x: integer;
    xArrayArg: TArrayArg;
begin
    xArrayArg := TArrayArg.Create();
    xArrayArg.ArrayLength := Length(aValues);
    for x := 0 to high(aValues) do
        xArrayArg[x] := CreateArgByType(aValues[x], aDataType);

    EXIT(xArrayArg);
end;

class function TArgUtils.CreateArrayArgByValue(const aValues: TArray<string>): TArg;
var
    xDataType: TParserIdentDataType;
begin
    xDataType := DefineTypeByValueArray(aValues);
    EXIT(CreateArrayArgByType(aValues, xDataType));
end;

class function TArgUtils.CreateArgByValue(const aValue: string): TArg;
var
    xDataType: TParserIdentDataType;
begin
    xDataType := DefineTypeByValue(aValue);
    EXIT(CreateArgByType(aValue, xDataType));
end;

class function TArgUtils.CreateArgByTypeOrValue(const aValue: string; aDataType: TParserIdentDataType): TArg;
begin
    if (aDataType = idtNone) then
        aDataType := DefineTypeByValue(aValue);

    EXIT(CreateArgByType(aValue, aDataType));
end;

class function TArgUtils.CreateArgByTypeVariant(const aValue: variant; aDataType: TParserIdentDataType): TArg;
begin
    case aDataType of
        idtString:
            EXIT(TStrArg.Create(aValue));
        idtInteger:
            EXIT(TIntArg.Create(aValue)); // Exception, wenn kein Integer!
        idtFloat:
            EXIT(TDblArg.Create(aValue)); // Exception, wenn kein Double!
        idtBool:
            EXIT(TBoolArg.Create(aValue)); // Exception, wenn kein Boolean!
        else
            raise EParserConversionException.Create('No DataType found for Value ' + aValue);
    end;
end;

class function TArgUtils.CreateArgByType(const aValue: string; aDataType: TParserIdentDataType): TArg;
begin
    case aDataType of
        idtString:
            EXIT(TStrArg.Create(aValue));
        idtInteger:
            EXIT(TIntArg.Create(StrToInt(aValue))); // Exception, wenn kein Integer!
        idtFloat:
            EXIT(TDblArg.Create(StrToFloat(aValue, TFormatUtils.GetSettingsEnglishUS)));
        idtBool:
            EXIT(TBoolArg.Create(StringToBool(aValue))); // Exception, wenn kein Boolean!
        else
            raise EParserConversionException.Create('No DataType found for Value ' + aValue);
    end;
end;

class function TArgUtils.DefineTypeByVariantValue(const aValue: variant; aAllowBoolean: boolean)
    : TParserIdentDataType;
begin
    // bei AllowBoolean = false wird der Wert zum Ordinal-Wert!
    if aAllowBoolean and (FindVarData(aValue)^.VType = varBoolean) then
        EXIT(idtBool);

    if VarIsOrdinal(aValue) then
        EXIT(idtInteger);

    if VarIsFloat(aValue) then
        EXIT(idtFloat);

    EXIT(idtString);
end;

class function TArgUtils.CreateArgByVariantValue(const aValue: variant; aAllowBoolean: boolean): TArg;
var
    xDataType: TParserIdentDataType;
begin
    xDataType := DefineTypeByVariantValue(aValue, aAllowBoolean);
    EXIT(CreateArgByTypeVariant(aValue, xDataType));
end;

class function TArgUtils.StringToBool(const aValue: string): boolean;
var
    xBoolValue: boolean;
begin
    if TryStrToBoolean(aValue, xBoolValue) then
        EXIT(xBoolValue)
    else
        raise EParserConversionException.Create('Conversion to BOOLEAN not possible');
end;

class function TArgUtils.TryStrToBoolean(const aValue: string; out oBoolValue: boolean): boolean;
begin
    // Ist es ein Boolean?
    if SameText(aValue, STR_TOKEN_TRUE) then
    begin
        oBoolValue := true;
        EXIT(true);
    end;

    if SameText(aValue, STR_TOKEN_FALSE) then
    begin
        oBoolValue := false;
        EXIT(true);
    end;

    EXIT(false);
end;

class function TArgUtils.CopyAttrValue(const aValue: TArg): TArg;
begin
    if aValue is TStrArg then
        result := TStrArg.Create(aValue.AsStr)
    else if aValue is TIntArg then
        result := TIntArg.Create(aValue.AsInt)
    else if aValue is TDblArg then
        result := TDblArg.Create(aValue.AsFloat)
    else if aValue is TBoolArg then
        result := TBoolArg.Create(aValue.AsBool)
    else if aValue is TReferenceArg then
        result := TReferenceArg.Create(aValue.AsInt)
    else
        result := CreateArgByValue(aValue.AsStr);
end;


end.
