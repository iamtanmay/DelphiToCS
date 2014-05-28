{ ----------------------------------------------------------------------------------------------------------------------
  Copyright  2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : Utility functions for dynamic Arrays
  ----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -------------------------------------------------------------------
  31.08.09 wl  ArrayToBracketText           TN4740   ersetzt gmStrArrText
  04.09.09 wl                               TN4740   erweitert
  21.09.09 pk  ArrayToText                  TN4740   Bug: Delimiter was not added to result
  02.10.09 pk  CopyArry                     TN4802   New
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  14.11.09 wl                               TN4869   benutzt TGeneralUtils.GetCurrentListSeparator und TMathUtils-Methoden
  14.11.09 wl  StringToStringArray          TN4869   EXIT durch BREAK ersetzt, sonst Fehler bei Übersetzung
  14.12.10 wl                               TN5411   TFormatUtils statt TGeneralUtils
  11.04.11 wl  CopyArray                    TN5549   ist jetzt generisch
  08.07.11 wl  StringToDoubleArray          TN5626   mit FormatSettings
  06.02.12 wl  StringToInt/DoubleArrayDef   TN5794   neu
  11.09.13 wl  WriteValueToArray,GetDefinedArray    TN6249   ist jetzt generisch
  11.09.13 wl  GetTrueArray,GetFalseArray    TN6249   neu
  ---------------------------------------------------------------------------------------------------------------------- }

unit ArrayUtils;


interface


uses
    SysUtils;

type
    TArrayUtils = class
    public
        class function ArrayToText(const aArray: TArray<string>): string; overload;
        class function ArrayToText(const aArray: TArray<integer>): string; overload;
        class function ArrayToText(const aArray: TArray<double>): string; overload;

        class function ArrayToBracketText(const aArray: TArray<string>): string; overload;
        class function ArrayToBracketText(const aArray: TArray<integer>): string; overload;
        class function ArrayToBracketText(const aArray: TArray<double>): string; overload;

        class function StringToStringArray(const aStr: string; const aListSeparator: string): TArray<string>;
        class function StringToIntArray(const aStr: string; const aListSeparator: string): TArray<integer>;
        class function StringToDoubleArray(const aStr: string; const aListSeparator: string;
            const aFormatSettings: TFormatSettings): TArray<double>;
        class function StringToIntArrayDef(const aStr: string; const aListSeparator: string;
            aDefault: Integer): TArray<integer>;
        class function StringToDoubleArrayDef(const aStr: string; const aListSeparator: string;
            aDefault: double; const aFormatSettings: TFormatSettings): TArray<double>;

        class procedure WriteValueToArray<T>(aArray: TArray<T>; const aValue: T);

        class function GetDefinedIntArray(const aValue: integer; aLength: integer): TArray<integer>;
        class function GetDefinedDoubleArray(const aValue: double; aLength: integer): TArray<double>;
        class function GetDefinedArray<T>(const aValue: T; aLength: integer): TArray<T>;

        class function GetNullIntArray(aLength: integer): TArray<integer>;
        class function GetNullDoubleArray(aLength: integer): TArray<double>;
        class function GetTrueArray(aLength: integer): TArray<boolean>;
        class function GetFalseArray(aLength: integer): TArray<boolean>;

        class function MaxIntValue(const aArray: array of integer): integer;
        class function MaxDoubleValue(const aArray: array of double): double;

        class function MinIntValue(const aArray: array of integer): integer;
        class function MinDoubleValue(const aArray: array of double): double;

        class function CopyArray<T>(const aArray: TArray<T>): TArray<T>; overload;
        class function CopyArray<T>(const aArray: array of T): TArray<T>; overload;
    end;


implementation


uses
    GeneralTypes,
    MathUtils;

{ TArrayUtils }

class function TArrayUtils.ArrayToBracketText(const aArray: TArray<double>): string;
begin
    // schreibt ein TArray<double> in der Form [100.7,0.5,...]

    result := '[' + TArrayUtils.ArrayToText(aArray) + ']';
end;

class function TArrayUtils.ArrayToBracketText(const aArray: TArray<integer>): string;
begin
    // schreibt ein TArray<integer> in der Form [100,1000,...]

    result := '[' + TArrayUtils.ArrayToText(aArray) + ']';
end;

class function TArrayUtils.ArrayToBracketText(const aArray: TArray<string>): string;
begin
    // schreibt ein TipStringArray in der Form [AAA,BBB,...]

    result := '[' + TArrayUtils.ArrayToText(aArray) + ']';
end;

class function TArrayUtils.ArrayToText(const aArray: TArray<double>): string;
var
    xDelim: string;
    x: integer;
begin
    result := '';
    xDelim := '';
    for x := 0 to high(aArray) do
    begin
        if (x = 1) then
            xDelim := TFormatUtils.CurrentListSeparator;
        result := result + xDelim + FloatToStr(aArray[x]);
    end;
end;

class function TArrayUtils.CopyArray<T>(const aArray: TArray<T>): TArray<T>;
var
    x: integer;
begin
    SetLength(result, Length(aArray));
    for x := 0 to high(aArray) do
        result[x] := aArray[x];
end;

class function TArrayUtils.CopyArray<T>(const aArray: array of T): TArray<T>;
var
    x: integer;
begin
    SetLength(result, Length(aArray));
    for x := 0 to high(aArray) do
        result[x] := aArray[x];
end;

class function TArrayUtils.ArrayToText(const aArray: TArray<integer>): string;
var
    xDelim: string;
    x: integer;
begin
    result := '';
    xDelim := '';
    for x := 0 to high(aArray) do
    begin
        if (x = 1) then
            xDelim := TFormatUtils.CurrentListSeparator;
        result := result + xDelim + IntToStr(aArray[x]);
    end;
end;

class function TArrayUtils.ArrayToText(const aArray: TArray<string>): string;
var
    xDelim: string;
    x: integer;
begin
    result := '';
    xDelim := '';
    for x := 0 to high(aArray) do
    begin
        if (x = 1) then
            xDelim := TFormatUtils.CurrentListSeparator;
        result := result + xDelim + aArray[x];
    end;
end;

class function TArrayUtils.GetDefinedArray<T>(const aValue: T; aLength: integer): TArray<T>;
begin
    SetLength(result, aLength);
    WriteValueToArray<T>(result, aValue);
end;

class function TArrayUtils.GetDefinedDoubleArray(const aValue: double; aLength: integer): TArray<double>;
begin
    EXIT(GetDefinedArray<double>(aValue, aLength));
end;

class function TArrayUtils.GetDefinedIntArray(const aValue: integer; aLength: integer): TArray<integer>;
begin
    EXIT(GetDefinedArray<integer>(aValue, aLength));
end;

class function TArrayUtils.GetFalseArray(aLength: integer): TArray<boolean>;
begin
    EXIT(GetDefinedArray<boolean>(false, aLength));
end;

class function TArrayUtils.GetNullDoubleArray(aLength: integer): TArray<double>;
begin
    EXIT(GetDefinedArray<double>(0, aLength));
end;

class function TArrayUtils.GetNullIntArray(aLength: integer): TArray<integer>;
begin
    EXIT(GetDefinedArray<integer>(0, aLength));
end;

class function TArrayUtils.GetTrueArray(aLength: integer): TArray<boolean>;
begin
    EXIT(GetDefinedArray<boolean>(true, aLength));
end;

class function TArrayUtils.MaxDoubleValue(const aArray: array of double): double;
begin
    result := TMathUtils.MaxDoubleValue(aArray);
end;

class function TArrayUtils.MaxIntValue(const aArray: array of integer): integer;
begin
    result := TMathUtils.MaxIntValue(aArray);
end;

class function TArrayUtils.MinDoubleValue(const aArray: array of double): double;
begin
    result := TMathUtils.MinDoubleValue(aArray);
end;

class function TArrayUtils.MinIntValue(const aArray: array of integer): integer;
begin
    result := TMathUtils.MinIntValue(aArray);
end;

class function TArrayUtils.StringToDoubleArray(const aStr, aListSeparator: string;
    const aFormatSettings: TFormatSettings): TArray<double>;
var
    x: integer;
    xStringArray: TArray<string>;
begin
    xStringArray := StringToStringArray(aStr, aListSeparator);
    SetLength(result, Length(xStringArray));
    for x := 0 to high(xStringArray) do
    begin
        result[x] := StrToFloat(xStringArray[x], aFormatSettings);
    end;
end;

class function TArrayUtils.StringToDoubleArrayDef(const aStr, aListSeparator: string; aDefault: double;
    const aFormatSettings: TFormatSettings): TArray<double>;
var
    x: integer;
    xStringArray: TArray<string>;
begin
    xStringArray := StringToStringArray(aStr, aListSeparator);
    SetLength(result, Length(xStringArray));
    for x := 0 to high(xStringArray) do
    begin
        result[x] := StrToFloatDef(xStringArray[x], aDefault);
    end;
end;

class function TArrayUtils.StringToIntArray(const aStr, aListSeparator: string): TArray<integer>;
var
    x: integer;
    xStringArray: TArray<string>;
begin
    xStringArray := StringToStringArray(aStr, aListSeparator);
    SetLength(result, Length(xStringArray));
    for x := 0 to high(xStringArray) do
    begin
        result[x] := StrToInt(xStringArray[x]);
    end;
end;

class function TArrayUtils.StringToIntArrayDef(const aStr, aListSeparator: string; aDefault: Integer)
    : TArray<integer>;
var
    x: integer;
    xStringArray: TArray<string>;
begin
    xStringArray := StringToStringArray(aStr, aListSeparator);
    SetLength(result, Length(xStringArray));
    for x := 0 to high(xStringArray) do
    begin
        result[x] := StrToIntDef(xStringArray[x], aDefault);
    end;
end;

class function TArrayUtils.StringToStringArray(const aStr, aListSeparator: string): TArray<string>;
var
    xCount: integer;
    xStr: string;
    xPos: integer;
    xLen, xDelimLen: integer;
begin
    SetLength(result, 0);
    xStr := aStr;
    xCount := 0;
    xDelimLen := Length(aListSeparator);
    while true do
    begin
        xLen := Length(xStr);
        if xLen = 0 then
            BREAK;

        xPos := Pos(aListSeparator, xStr);
        if xPos = 0 then
            xPos := xLen
        else
            xPos := xPos - 1;

        Inc(xCount);

        // Array um neuen Wert erweitern
        SetLength(result, xCount);
        result[xCount - 1] := Copy(xStr, 1, xPos);

        // String kürzen
        xStr := Copy(xStr, 1 + xPos + xDelimLen, Length(xStr));
    end;
end;

class procedure TArrayUtils.WriteValueToArray<T>(aArray: TArray<T>; const aValue: T);
var
    x: integer;
begin
    for x := 0 to high(aArray) do
    begin
        aArray[x] := aValue;
    end;
end;


end.
