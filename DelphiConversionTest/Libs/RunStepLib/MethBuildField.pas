{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no  improvement/change
  -------- --  ---------------------------  --------  ----------------------------------------------
  08.01.08 wl  TMethBuildField              TN3972    aus unit MethodBuildParsing in eigene Unit
  06.10.08 pk                               TN4258    inherits from TParseField
  06.11.08 pk  TMethBuildField              TN4279    removed
  09.12.08 pk  NextLineIndex                TN4279    result for case of TEndWhileEvalNode changed
  04.11.09 pk                               TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  07.05.10 pk  TParserIdentValueUtils       TN5092    New
  07.05.10 pk  TMethBuildEvaluableField     TN5092    New Evaluate function
  22.06.10 pk  TMethBuildControlFlow        TN5088    removed
  15.11.10 pk                               TN5340    Changes to prevent memory leak
  14.12.10 wl                               TN5411    TFormatUtils statt TValueConverter
  27.06.11 wl                               TN5609   neu: TAttrValueUtils
  23.11.11 ts  GetIdentDataType             TN5742    TryStrToInt interpretiert Zahlen mit führendem x als hex-Zahlen
  01.03.12 wl  IdentTypeToLogStr            TN5820   --> ParserIdentDataType
  01.03.12 wl  GetIdentDataTypeForArray     TN5820   neu: bestimmt den möglichen DataType für ein komplettes Array
  01.03.12 wl                               TN5822   TArg statt TAttrValue
  05.07.12 wl  TParserIdentValueUtils       TN5917   einige Funktionen entfernt, die auch in TArgUtils enthalten sind
  05.07.12 wl  TMethBuildCompositeField     TN5917   neu: Evaluate-Funktion
  11.04.13 wl                               TN6045   TArrayArg-Änderungen
  25.06.13 wl                               TN6178   uses Identifier
  -------------------------------------------------------------------------------------------------- }

unit MethBuildField;


interface


uses
    Generics.Collections,
    Streamable,
    ParserTree,
    ParseField,
    Identifier,
    ParserEvalTable,
    ParserIdentDataType,
    MemoryClasses;

type
    TParseFieldList = class(TObjectList<TParseField>);

    TMethBuildField = class(TParseField)
    public
        constructor Create(const aFieldName: string; const aUnparsedVal: string);
    end;

    TMethBuildConstField = class(TMethBuildField)
    protected
        function GetAsString(): string; override;
    public
        constructor Create(const aFieldName: string; const aUnparsedVal: string);
        function Evaluate: TArg; override;
    end;

    TMethBuildEvaluableField = class(TMethBuildField)
    protected
        fParseResult: TParseEvalResult;
        fEvalTable: TParserEvalTable;
        function GetAsString(): string; override;
    public
        constructor Create(const aFieldName: string; const aUnparsedVal: string;
            const aEvalTable: TParserEvalTable);
        destructor Destroy(); override;
        function Evaluate: TArg; override;
        property ParseResult: TParseEvalResult read fParseResult;
    end;

    TMethBuildCompositeField = class(TMethBuildField)
    private
        fFields: TObjectList<TParseField>;
    protected
        function GetAsString(): string; override;
    public
        constructor Create(const aFieldName: string; const aUnparsedValue: string);
        destructor Destroy(); override;
        function Evaluate: TArg; override;
        procedure AddField(aField: TParseField);
    end;

    TParserIdentValueUtils = class
    public
        class function IdentValueToAttrValue(const aValue: TStreamableItem; const aArrayIndex: integer): TArg;
        class function ArrayIdentValueToAttrValue(const aValue: TStreamableItem): TArg;
        class function AttrValueToIdentValue(const aValue: TArg): TStreamableItem;
        class function ArrayAttrValueToIdentValue(const aArrayValue: TArrayArg): TStreamableItem;

        class function IdentValueToLogStr(const aIdentValue: TStreamableItem;
            const aArrayIndex: integer): string;
        class function IdentToLogStr(const aIdent: TIdentifier; const aArrayIndex: integer): string;
    end;


implementation


uses
    SysUtils,
    ParserEvalNode,
    ParserOperatorEvalNodeBasic,
    MethodGUIParsing,
    GeneralTypes;

{ TMethBuildField }

constructor TMethBuildField.Create(const aFieldName: string; const aUnparsedVal: string);
begin
    inherited Create(aFieldName, aUnparsedVal);
end;

{ TMethBuildConstField }

constructor TMethBuildConstField.Create(const aFieldName: string; const aUnparsedVal: string);
begin
    inherited Create(aFieldName, aUnparsedVal);
end;

function TMethBuildConstField.Evaluate: TArg;
begin
    EXIT(TArgUtils.CreateArgByValue(fUnparsedVal));
end;

function TMethBuildConstField.GetAsString(): string;
var
    xValue: TArg;
begin
    xValue := Evaluate();
    try
        result := TArgUtils.DataValueToStr(xValue);
    finally
        xValue.Free;
    end;
end;

{ TMethBuildEvaluableField }

constructor TMethBuildEvaluableField.Create(const aFieldName: string; const aUnparsedVal: string;
    const aEvalTable: TParserEvalTable);
begin
    inherited Create(aFieldName, aUnparsedVal);
    fParseResult := TParseEvalResult.Create();
    fEvalTable := aEvalTable;
end;

destructor TMethBuildEvaluableField.Destroy();
begin
    fParseResult.Free;
    inherited;
end;

function TMethBuildEvaluableField.Evaluate: TArg;
begin
    EXIT(fParseResult.Evaluate(fEvalTable));
end;

function TMethBuildEvaluableField.GetAsString(): string;
var
    xValue: TArg;
begin
    xValue := Evaluate();
    try
        result := TArgUtils.DataValueToStr(xValue);
    finally
        xValue.Free;
    end;
end;

{ TMethBuildCompositeField }

constructor TMethBuildCompositeField.Create(const aFieldName: string; const aUnparsedValue: string);
const
    cOwnObjects = false;
begin
    inherited Create(aFieldName, aUnparsedValue);
    fFields := TParseFieldList.Create(cOwnObjects);
end;

destructor TMethBuildCompositeField.Destroy;
begin
    fFields.Free;
    inherited;
end;

function TMethBuildCompositeField.Evaluate: TArg;
begin
    // ??? Was tun ???
    EXIT(nil);
end;

procedure TMethBuildCompositeField.AddField(aField: TParseField);
begin
    fFields.Add(aField);
end;

function TMethBuildCompositeField.GetAsString: string;
var
    xList: TStringKeyStringValueList;
    x: integer;
    xChildField: TParseField;
begin
    xList := TMethodGUIParser.CreateKeyValueList();
    try
        for x := 0 to fFields.Count - 1 do
        begin
            xChildField := (fFields[x] as TParseField);
            xList.AddValue(xChildField.FieldName, xChildField.AsString);
        end;
        result := TMethodGUIParser.ListToOptionStr(xList);
    finally
        xList.Free;
    end;
end;

{ TParserIdentValueUtils }

class function TParserIdentValueUtils.IdentValueToLogStr(const aIdentValue: TStreamableItem;
    const aArrayIndex: integer): string;
begin
    result := TArgUtils.ItemToStr(aIdentValue, aArrayIndex);
end;

class function TParserIdentValueUtils.IdentToLogStr(const aIdent: TIdentifier;
    const aArrayIndex: integer): string;
var
    xTypeLogStr: string;
    xValueLogStr, xIdentStr: string;
begin
    xValueLogStr := IdentValueToLogStr(aIdent.Value, aArrayIndex);
    xTypeLogStr := TArgUtils.IdentTypeToLogStr(aIdent.Value, aArrayIndex);

    if xTypeLogStr = '' then
        raise Exception.CreateFmt('Identifier %s has an invalid type', [aIdent.Key]);

    if aArrayIndex < 0 then
        xIdentStr := aIdent.Key
    else
        xIdentStr := aIdent.Key + '[' + IntToStr(aArrayIndex) + ']';

    EXIT('Set Ident: ' + xIdentStr + ', Type: ' + xTypeLogStr + ', Value: ' + xValueLogStr);
end;

class function TParserIdentValueUtils.ArrayIdentValueToAttrValue(const aValue: TStreamableItem): TArg;
var
    xIdentValueArray: TArrayArg;
    xAttrArray: TArrayArg;
    x: integer;
begin
    xIdentValueArray := (aValue as TArrayArg);
    xAttrArray := TArrayArg.Create();

    xAttrArray.ArrayLength := xIdentValueArray.ArrayLength;

    for x := 0 to xIdentValueArray.Count - 1 do
    begin
        xAttrArray[x] := IdentValueToAttrValue(xIdentValueArray[x], TArrayArg.UndefinedBoundsIndex);
    end;

    result := xAttrArray;
end;

class function TParserIdentValueUtils.IdentValueToAttrValue(const aValue: TStreamableItem;
    const aArrayIndex: integer): TArg;
var
    xArrayArg: TArrayArg;
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
    else if aValue is TArrayArg then
    begin

        if aArrayIndex = TArrayArg.UndefinedBoundsIndex then
        begin
            result := ArrayIdentValueToAttrValue(aValue);
        end
        else
        begin
            xArrayArg := (aValue as TArrayArg);
            result := IdentValueToAttrValue(xArrayArg.GetItemByUserIndex(aArrayIndex),
                TArrayArg.UndefinedBoundsIndex);
        end;
    end
    else
        result := nil;
end;

class function TParserIdentValueUtils.ArrayAttrValueToIdentValue(const aArrayValue: TArrayArg)
    : TStreamableItem;
var
    xIdentValueArray: TArrayArg;
    x: integer;
begin
    xIdentValueArray := TArrayArg.Create();
    xIdentValueArray.ArrayLength := aArrayValue.ArrayLength;

    for x := 0 to aArrayValue.Count - 1 do
    begin
        xIdentValueArray[x] := IdentValueToAttrValue(aArrayValue[x], TArrayArg.UndefinedBoundsIndex);
    end;

    EXIT(xIdentValueArray);
end;

class function TParserIdentValueUtils.AttrValueToIdentValue(const aValue: TArg): TStreamableItem;
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
    else if aValue is TArrayArg then
        result := ArrayAttrValueToIdentValue(aValue as TArrayArg)

    else
        result := nil;
end;


end.
