{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2010 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  : String handling utility methods
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  13.04.10 wl                                        TN5044    Initial Revision
  13.04.10 wl                                        TN5044    enthält TStringUtils aus UtilLib und Methoden aus Utility.pas
  27.04.11 wl  StringToFixedList                     TN5563    Die Anzahl wird vorher festgelegt: Der letzte parameter kann den ganzen Rest mit Delimitern enthalten
  ----------------------------------------------------------------------------------------------------------------------- }

unit StringUtilities;


interface


uses
    Classes,
    GeneralTypes,
    Generics.Collections;

type
    TStringUtilities = class
    private const
        STR_PAD: string = '                                                    ';
    public
        class function GetFromString(pString: string; pFirst, pCount: integer): string;
        class function GetNameWithoutNumber(aText: string): string;

        class function ParseString1(var aStrResult: string; aStrText, aStrDelimiter: string;
            aIntIndex: integer = 0): integer;
        class function PadString(S: string; aIntMaxLength: integer): string;
        class function PadWith(strText: string; intPadUntil: integer; strPad: string = '0'): string;
        class function DefIfNull(strText: string; strDefault: string): string;
        class function CopyTillEnd(aStrText: string; aIntIndex: integer): string;
        class function Extract(var S: string; index, Count, Skip: Integer): string;

        class procedure SplitStr(aStr: string; aDelim: string; var aStr1, aStr2: string);

        class function ReplaceString(strOrig, strToFind, strReplace: string): string;

        class function ListToString(aList: TList<string>; aStrDelimiter: string): string;
        class function StringToList(aLstResult: TList<string>; aStrText, aStrDelimiter: string): boolean;
        class function StringToFixedList(aLstResult: TList<string>; aStrText, aStrDelimiter: string;
            aFixedLength: integer): boolean;
        class function StringToStringArray(aStrText, aStrDelimiter: string): TArray<string>;
        class function StringToFixedStringArray(aStrText, aStrDelimiter: string; aFixedLength: integer)
            : TArray<string>;

        class function StringArrayToString(aArr: TArray<string>; aStrDelimiter: string;
            aStartIndex, aFinishIndex: integer): string; overload;
        class function StringArrayToString(aArr: TArray<string>; aStrDelimiter: string): string; overload;
    end;


implementation


uses
    SysUtils;

class function TStringUtilities.GetFromString(pString: string; pFirst, pCount: integer): string;
var
    i: integer;
begin
    if pFirst > 1 then
        Delete(pString, 1, (pFirst - 1));
    i := Length(pString);
    if (i > pCount) then
        Delete(pString, pCount + 1, (i - pCount));
    result := pString;
end;

class function TStringUtilities.GetNameWithoutNumber(aText: string): string;
// Diese Funktion entfernt eine an den Namen angehängte Zahl
// Befindet sich an der letzten Position des Textes keine Zahl, ist result = ''
begin
    if (aText = '') then
        EXIT;

    result := '';

    while (CharInSet(aText[length(aText)], [#48 .. #57])) do
    begin

        // am Ende befindet sich eine Zahl!
        System.Delete(aText, length(aText), 1);
        result := aText;
    end;
end;

class function TStringUtilities.CopyTillEnd(aStrText: string; aIntIndex: integer): string;
begin
    result := Copy(aStrText, aIntIndex, Length(aStrText));
end;

class function TStringUtilities.Extract(var S: string; index, Count, Skip: Integer): string;
// result is index upto and including Count
// S is right side from index + count + skip until end of string
begin
    result := Copy(S, index, Count);
    S := CopyTillEnd(S, index + Count + Skip);
end;

class function TStringUtilities.ParseString1(var aStrResult: string; aStrText, aStrDelimiter: string;
    aIntIndex: integer = 0): integer;
var
    strTemp: string;
    intPos, intCount: integer;
begin
    intPos := 0;
    result := 0;
    intCount := 0;
    aStrResult := '';
    strTemp := '';

    while true do
    begin
        intPos := Pos(aStrDelimiter, aStrText);
        if intPos = 0 then
            Break;
        strTemp := Extract(aStrText, 1, intPos - 1, 1);
        if intCount = aIntIndex then
            Break;
        inc(intCount);
    end;

    if intCount <> aIntIndex then
        Exit;
    result := 1;
    if intPos = 0 then
        aStrResult := aStrText
    else
        aStrResult := strTemp;
end;

class procedure TStringUtilities.SplitStr(aStr: string; aDelim: string; var aStr1, aStr2: string);
// --------------------------------------------------------------------------------------------------
// Example SplitStr( 'P1;P2;P3', ';' x1, x2 )
// Gives  x1 = P1
// x2 = P2
// --------------------------------------------------------------------------------------------------
var
    xLen, xPos: integer;
begin
    xLen := Length(aStr);

    aStr1 := aStr;
    aStr2 := '';
    xPos := Pos(aDelim, aStr);
    if xPos <= 0 then
        Exit;

    aStr1 := Copy(aStr, 1, xPos - 1);
    aStr2 := Copy(aStr, xPos + 1, xLen);
end;

class function TStringUtilities.ReplaceString(strOrig, strToFind, strReplace: string): string;
var
    intFoundPos: integer;
    intFindLen: integer;
    intNewLen: integer;
    strTemp, strNew: string;
begin
    intFindLen := length(strToFind);
    strNew := strOrig;
    intNewLen := length(strNew);

    intFoundPos := pos(strToFind, strNew);
    while intFoundPos > 0 do
    begin

        strTemp := copy(strNew, 1, intFoundPos - 1);
        strTemp := strTemp + strReplace;
        strTemp := strTemp + copy(strNew, intFoundPos + intFindLen, intNewLen);
        strNew := strTemp;
        intNewLen := length(strNew);
        intFoundPos := pos(strToFind, strNew);
    end;
    result := strNew;
end;

class function TStringUtilities.ListToString(aList: TList<string>; aStrDelimiter: string): string;
var
    i: integer;
    strTemp, strAnd: string;
begin
    strAnd := '';
    strTemp := '';
    for i := 0 to aList.Count - 1 do
    begin
        if i > 0 then
            strAnd := aStrDelimiter;
        strTemp := strTemp + strAnd + aList[i];
    end;
    result := strTemp;
end;

class function TStringUtilities.StringToList(aLstResult: TList<string>;
    aStrText, aStrDelimiter: string): boolean;
var
    intPos: integer;
begin
    result := true;
    if Length(aStrText) = 0 then
        Exit;
    while true do
    begin
        intPos := Pos(aStrDelimiter, aStrText);
        if intPos = 0 then
        begin
            aLstResult.Add(aStrText);
            Break;
        end;
        aLstResult.Add(Extract(aStrText, 1, intPos - 1, Length(aStrDelimiter)));
    end;
end;

class function TStringUtilities.StringToFixedList(aLstResult: TList<string>; aStrText, aStrDelimiter: string;
    aFixedLength: integer): boolean;
var
    intPos, x: integer;
begin
    result := true;
    if Length(aStrText) = 0 then
        EXIT;
    while true do
    begin
        intPos := Pos(aStrDelimiter, aStrText);

        if (intPos = 0) // Trennzeichen kommt nicht mehr vor
            or ((aFixedLength - 1) = aLstResult.Count)
        // FixedLength: der letzte Parameter darf Trennzeichen enthalten
        then
        begin
            aLstResult.Add(aStrText);
            Break;
        end;

        aLstResult.Add(Extract(aStrText, 1, intPos - 1, Length(aStrDelimiter)));
    end;

    for x := aLstResult.Count to aFixedLength - 1 do
    begin
        aLstResult.Add(''); // leere Strings hinzufügen
    end;
end;

class function TStringUtilities.StringToFixedStringArray(aStrText, aStrDelimiter: string;
    aFixedLength: integer): TArray<string>;
var
    xList: TList<string>;
begin
    xList := TList<string>.Create;
    try
        StringToFixedList(xList, aStrText, aStrDelimiter, aFixedLength);
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

class function TStringUtilities.StringToStringArray(aStrText, aStrDelimiter: string): TArray<string>;
var
    xList: TList<string>;
begin
    xList := TList<string>.Create;
    try
        StringToList(xList, aStrText, aStrDelimiter);
        result := xList.ToArray;
    finally
        FreeAndNil(xList);
    end;
end;

class function TStringUtilities.StringArrayToString(aArr: TArray<string>; aStrDelimiter: string): string;
begin
    result := StringArrayToString(aArr, aStrDelimiter, 0, high(aArr));
end;

class function TStringUtilities.StringArrayToString(aArr: TArray<string>; aStrDelimiter: string;
    aStartIndex, aFinishIndex: integer): string;
var
    i: integer;
    xDelim: string;
begin
    xDelim := '';
    result := '';
    for i := aStartIndex to aFinishIndex do
    begin
        if i = (aStartIndex + 1) then
            xDelim := aStrDelimiter;
        result := result + xDelim + aArr[i];
    end;
end;

class function TStringUtilities.PadString(S: string; aIntMaxLength: integer): string;
var
    intPadLength, intLen: integer;
    strFormat, strPad: string;
begin
    intLen := Length(S);
    intPadLength := aIntMaxLength - intLen;
    if intPadLength <= 0 then
        Exit;
    strFormat := '%.' + IntToStr(intPadLength) + 's';
    strPad := format(strFormat, [STR_PAD]);
    result := S + strPad;
end;

class function TStringUtilities.PadWith(strText: string; intPadUntil: integer; strPad: string): string;
var
    intNumZeros: integer;
begin
    intNumZeros := intPadUntil - Length(strText);
    while (intNumZeros <> 0) do
    begin
        strText := strPad + strText;
        dec(intNumZeros);
    end;
    result := strText;
end;

class function TStringUtilities.DefIfNull(strText: string; strDefault: string): string;
begin
    if strText = '' then
        result := strDefault
    else
        result := strText;
end;


end.
