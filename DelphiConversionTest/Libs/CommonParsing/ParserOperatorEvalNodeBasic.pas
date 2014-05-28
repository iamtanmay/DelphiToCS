{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  : These EvalNodes are more specific that the ones in the ParserEvalNode unit, but
  are still gerneral.
  ***This unit should not access any application-specific units!!!!
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.08.07 pk                               TN3788   Initial revision
  08.09.08 pk  IsMatchingControlFlow        TN4215   New
  06.11.08 pk  EvalOp                       TN4279   New EvalTable, EvalutatedOperand params
  21.07.09 pk  TSelectBitOperatorEvalNode   TN4669   New
  07.05.10 pk                               TN5092   various changes
  27.06.11 wl  TLengthOperatorEvalNode      TN5609   LEN ermittelt jetzt auch die Länge eines Arrays
  27.06.11 wl  TSubArrOperatorEvalNode      TN5609   SUB fertigt eine reduzierte Kopie eines Arrays an
  27.06.11 wl                               TN5609   neu: TAttrValueUtils
  08.07.11 wl  TSubArrOperatorEvalNode      TN5609   Ist jetzt viel robuster und legt auch leere Arrays an
  18.07.11 wl  TConcatOperatorEvalNode.EvalOp  TN5631   Statt AsStr wird TArgUtils.ItemToStr verwendet
  18.07.11 wl                               TN5628   neu: Statistik-Funktionen für Arrays: MIN,MAX,SUM,AVR,STDDEV
  18.07.11 wl  TNewArrayOperatorEvalNode    TN5628.1 neu: A() erzeugt neues Array
  02.08.11 wl                               TN5481   alle mathematischen Funktionen, die es früher schon mal gab
  02.08.11 wl  TLogarithmBase10OperatorEvalNode  TN5481   neu
  02.08.11 wl  TPowerOperatorEvalNode       TN5481   man kann jetzt schreiben 2 POWER 5 (ergibt 32)
  02.08.11 wl  TCountOperatorEvalNode       TN5609.1 LEN für Arrays heißt jetzt COUNT
  17.08.11 wl  TCountOperatorEvalNode       TN5609.1 Compiler-Fehler entfernt
  22.08.11 wl  TCastOperatorEvalNode        TN5665   neu: kann die 4 Typen STRING, INTEGER, DOUBLE, BOOLEAN jedes in jeden umformen
  17.10.11 wl  TLogicEvalNode.EQ            TN5717   --> TOperatorEvalNode
  17.10.11 wl  TIndexOfOperatorEvalNode     TN5717   neu: findet Element in Array
  21.02.12 wl  TFormatPercentOperatorEvalNode               TN5805   neu
  21.02.12 wl  TRelativeStandardDeviationOperatorEvalNode   TN5805   neu
  01.03.12 wl  TPadstrOperatorEvalNode      TN5820   pfptStr statt pfptChar (macht keinen Unterschied)
  01.03.12 wl  TCastOperatorEvalNode        TN5820   Cast-Funktionen --> TAttrValueUtils
  01.03.12 wl                               TN5822   TArg statt TAttrValue
  02.03.12 wl  TTypeOfOperatorEvalNode      TN5823   neu: Ermittelt den Typ(-namen) einer Variablen
  06.03.12 wl  TNewArrayOperatorEvalNode    TN5824   Kommt auch mit nur einem Element klar
  06.03.12 wl  TArrayToBitsOperatorEvalNode TN5824   neu
  06.03.12 wl  TBitsToArrayOperatorEvalNode TN5824   neu
  05.07.12 wl                               TN5917   an Änderungen in ParserEvalNode angepasst
  27.03.13 wl                               TN6045   TArrayArg-Änderungen
  28.05.13 ts  TJsonEvalOperatorEvalNode    TN6152   neu
  28.06.13 wl  TJsonAddOperatorEvalNode     TN6191   neu
  28.06.13 wl  TJsonRemoveOperatorEvalNode  TN6191   neu
  -------------------------------------------------------------------------------------------------- }

unit ParserOperatorEvalNodeBasic;


interface


uses
    ParserEvalNode,
    ParserIdentifier,
    ParserIdentDataType,
    ParserEvalTable;

type
    // TBinaryOpertors
    TConcatOperatorEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TFuncParamsDelimOperatorEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TAddOperatorEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TSubtractOperatorEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TMultOperatorEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TDivOperatorEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TModOperatorEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TIntDivOperatorEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TLogicEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function LT(aOp1, aOp2: TArg): boolean;
        function LTE(aOp1, aOp2: TArg): boolean;
    end;

    TLessThanOperatorEvalNode = class(TLogicEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TLessThanEqualOperatorEvalNode = class(TLogicEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TEqualOperatorEvalNode = class(TLogicEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TNotEqualOperatorEvalNode = class(TLogicEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TGreaterThanEqualOperatorEvalNode = class(TLogicEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TGreaterThanOperatorEvalNode = class(TLogicEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TAndOperatorEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TOrOperatorEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    // TUnaryOperators
    TUnaryMinusOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TSubArrOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TSubstrOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TPosOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TIndexOfOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TUpperCaseOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TLengthOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TCountOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TTrimOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TRevstrOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TPadstrOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TXModOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TXDivOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TNowOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TAbsOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TRoundOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TTruncOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TRandomOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TSelectBitOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TIsBitSelectedOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TCosineOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TCosineHyperbolicOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TSineOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TSineHyperbolicOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TTangentOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TArcTangentOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TSqareRootOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TExponentOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TLogarithmNaturalOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TLogarithmBase10OperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TPowerOperatorEvalNode = class(TBinaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; override;
    end;

    TMinValueOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TMaxValueOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TSumOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TAverageOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TStandardDeviationOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TRelativeStandardDeviationOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TNewArrayOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TCastOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TFormatPercentOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TTypeOfOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TBitsToArrayOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TArrayToBitsOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TJsonEvalOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TJsonAddOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TJsonRemoveOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;


implementation


uses
    Math,
    SysUtils,
    StrUtils,
    DBXJSON,
    ParserIdentToJSONUtils,
    GeneralTypes;

function NumValToInt(aEvaluatedOperand: TArg): integer;
begin
    if aEvaluatedOperand is TDblArg then
        result := Trunc(aEvaluatedOperand.AsFloat)
    else
        result := aEvaluatedOperand.AsInt;
end;

function NumValToDbl(aEvaluatedOperand: TArg): double;
begin
    if aEvaluatedOperand is TDblArg then
        result := aEvaluatedOperand.AsFloat
    else
        result := aEvaluatedOperand.AsInt;
end;

function TLogicEvalNode.LT(aOp1, aOp2: TArg): boolean;
begin
    CheckNumTypeBoth(aOp1, aOp2);
    if (IsDbl(aOp1) or IsDbl(aOp2)) then
        result := (aOp1.AsFloat < aOp2.AsFloat)
    else
        result := (aOp1.AsInt < aOp2.AsInt);
end;

function TLogicEvalNode.LTE(aOp1, aOp2: TArg): boolean;
begin
    CheckNumTypeBoth(aOp1, aOp2);
    if (IsDbl(aOp1) or IsDbl(aOp2)) then
        result := (aOp1.AsFloat <= aOp2.AsFloat)
    else
        result := (aOp1.AsInt <= aOp2.AsInt);
end;

function Padstr(const aStr: string; const aPadChar: string; aIndex: integer; aLength: integer): string;
var
    xPart1, xPart2, xPart3: string;
begin
    xPart1 := Copy(aStr, 1, aIndex - 1);
    xPart2 := StringOfChar(aPadChar[1], aLength - Length(aStr));
    xPart3 := Copy(aStr, aIndex, Length(aStr));
    result := xPart1 + xPart2 + xPart3;
end;

function XMod(aValue: integer; aStartVal: integer; aEndVal: integer): integer;
begin
    result := (aValue - aStartVal) mod (aEndVal - aStartVal + 1) + aStartVal;
end;

function XDiv(aValue: integer; aStartVal: integer; aEndVal: integer): integer;
begin
    result := (aValue - aStartVal) div (aEndVal - aStartVal + 1) + aStartVal;
end;

function TConcatOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    result := TStrArg.Create(TArgUtils.ItemToStr(aEvaluatedOperand1) +
        TArgUtils.ItemToStr(aEvaluatedOperand2));
end;

function TFuncParamsDelimOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    result := TFuncParams.Create(aEvaluatedOperand1, aEvaluatedOperand2);
end;

function TAddOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    CheckNumTypeBoth(aEvaluatedOperand1, aEvaluatedOperand2);
    if (IsDbl(aEvaluatedOperand1) or IsDbl(aEvaluatedOperand2)) then
        result := TDblArg.Create(aEvaluatedOperand1.AsFloat + aEvaluatedOperand2.AsFloat)
    else
        result := TIntArg.Create(aEvaluatedOperand1.AsInt + aEvaluatedOperand2.AsInt);
end;

function TSubtractOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    CheckNumTypeBoth(aEvaluatedOperand1, aEvaluatedOperand2);
    if (IsDbl(aEvaluatedOperand1) or IsDbl(aEvaluatedOperand2)) then
        result := TDblArg.Create(aEvaluatedOperand1.AsFloat - aEvaluatedOperand2.AsFloat)
    else
        result := TIntArg.Create(aEvaluatedOperand1.AsInt - aEvaluatedOperand2.AsInt);
end;

function TMultOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    CheckNumTypeBoth(aEvaluatedOperand1, aEvaluatedOperand2);
    if (IsDbl(aEvaluatedOperand1) or IsDbl(aEvaluatedOperand2)) then
        result := TDblArg.Create(aEvaluatedOperand1.AsFloat * aEvaluatedOperand2.AsFloat)
    else
        result := TIntArg.Create(aEvaluatedOperand1.AsInt * aEvaluatedOperand2.AsInt);
end;

function TDivOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    result := TDblArg.Create(aEvaluatedOperand1.AsFloat / aEvaluatedOperand2.AsFloat);
end;

function TModOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    CheckIntTypeBoth(aEvaluatedOperand1, aEvaluatedOperand2);
    result := TIntArg.Create(aEvaluatedOperand1.AsInt mod aEvaluatedOperand2.AsInt);
end;

function TIntDivOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    CheckIntTypeBoth(aEvaluatedOperand1, aEvaluatedOperand2);
    result := TIntArg.Create(aEvaluatedOperand1.AsInt div aEvaluatedOperand2.AsInt);
end;

function TLessThanOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    result := TBoolArg.Create(LT(aEvaluatedOperand1, aEvaluatedOperand2));
end;

function TLessThanEqualOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    result := TBoolArg.Create(LTE(aEvaluatedOperand1, aEvaluatedOperand2));
end;

function TEqualOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    result := TBoolArg.Create(AreEqual(aEvaluatedOperand1, aEvaluatedOperand2));
end;

function TNotEqualOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    result := TBoolArg.Create(not AreEqual(aEvaluatedOperand1, aEvaluatedOperand2));
end;

function TGreaterThanEqualOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    result := TBoolArg.Create(not LT(aEvaluatedOperand1, aEvaluatedOperand2));
end;

function TGreaterThanOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    result := TBoolArg.Create(not LTE(aEvaluatedOperand1, aEvaluatedOperand2));
end;

function TAndOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    result := TBoolArg.Create(GetBoolVal(aEvaluatedOperand1, '1') and GetBoolVal(aEvaluatedOperand2, '2'));
end;

function TOrOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
begin
    result := TBoolArg.Create(GetBoolVal(aEvaluatedOperand1, '1') or GetBoolVal(aEvaluatedOperand2, '2'));
end;

{ TSubArrOperatorEvalNode }

function TSubArrOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
    xResult: TArrayArg;
    xDest, xSource, xOpArrLength, xDestArrLength: integer;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptArr, pfptInt, pfptInt]);
    xParams := (aEvaluatedOperand as TFuncParams).Arr;

    // Länge des Ursprungsarrays ermitteln
    xOpArrLength := (xParams[0] as TArrayArg).ArrayLength;
    xDestArrLength := xOpArrLength + 1 - xParams[1].AsInt; // Dest-Array kann nicht länger werden als Source
    if (xDestArrLength > xParams[2].AsInt) then
        xDestArrLength := xParams[2].AsInt;
    if (xDestArrLength < 0) then
        xDestArrLength := 0;

    xResult := TArrayArg.Create();
    xResult.ArrayLength := xDestArrLength;

    // alle Array-Elemente kopieren
    for xDest := xResult.MinUserIndex to xResult.MaxUserIndex do
    begin
        xSource := xDest + xParams[1].AsInt - 1;
        xResult.SetItemByUserIndex(xDest, TArgUtils.CopyAttrValue((xParams[0] as TArrayArg)
            .GetItemByUserIndex(xSource)));
    end;

    EXIT(xResult);
end;

{ TSubstrOperatorEvalNode }

function TSubstrOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptStr, pfptInt, pfptInt]);
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    result := TStrArg.Create(Copy(xParams[0].AsStr, xParams[1].AsInt, xParams[2].AsInt));
end;

{ TPosOperatorEvalNode }

function TPosOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
    xTemp1, xTemp2: string;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptStr, pfptStr, pfptInt, pfptInt]);
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    xTemp1 := xParams[0].AsStr;
    xTemp2 := xParams[1].AsStr;
    if xParams[3].AsInt = 0 then
    begin
        xTemp1 := UpperCase(xTemp1);
        xTemp2 := UpperCase(xTemp2);
    end;

    result := TIntArg.Create(PosEx(xTemp1, xTemp2, xParams[2].AsInt));
end;

{ TIndexOfOperatorEvalNode }

function TIndexOfOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
    x: integer;
begin
    // 1. irgendein Format
    // 2. Array
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    CheckFuncParamsType(xParams[1], [pfptArr]);

    for x := TArrayArg.MinUserIndex to (xParams[1] as TArrayArg).MaxUserIndex do
    begin
        if AreEqual(xParams[0], (xParams[1] as TArrayArg).GetItemByUserIndex(x)) then
            EXIT(TIntArg.Create(x));
    end;

    EXIT(TIntArg.Create(0));
end;

{ TUpperCaseOperatorEvalNode }

function TUpperCaseOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptStr]);
    result := TStrArg.Create(UpperCase(aEvaluatedOperand.AsStr));
end;

function TLengthOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    // Length für einen String
    CheckFuncParamsType(aEvaluatedOperand, [pfptStr]);
    result := TIntArg.Create(Length(aEvaluatedOperand.AsStr));
end;

function TCountOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    // Array-Length ermitteln
    CheckFuncParamsType(aEvaluatedOperand, [pfptArr]);
    EXIT(TIntArg.Create((aEvaluatedOperand as TArrayArg).ArrayLength));
end;

function TTrimOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptStr]);
    result := TStrArg.Create(Trim(aEvaluatedOperand.AsStr));
end;

function TRevstrOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptStr]);
    result := TStrArg.Create(ReverseString(aEvaluatedOperand.AsStr));
end;

function TPadstrOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptStr, pfptStr, pfptInt, pfptInt]);
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    result := TStrArg.Create(Padstr(xParams[0].AsStr, xParams[1].AsStr, xParams[2].AsInt, xParams[3].AsInt));
end;

function TXModOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptInt, pfptInt, pfptInt]);
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    result := TIntArg.Create(XMod(xParams[0].AsInt, xParams[1].AsInt, xParams[2].AsInt));
end;

function TXDivOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptInt, pfptInt, pfptInt]);
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    result := TIntArg.Create(XDiv(xParams[0].AsInt, xParams[1].AsInt, xParams[2].AsInt));
end;

function TNowOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg;
var
    xRes: string;
begin
    DateTimeToString(xRes, aEvaluatedOperand.AsStr, Now());
    result := TStrArg.Create(xRes);
end;

function TAbsOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(Abs(aEvaluatedOperand.AsFloat))
    else
        result := TIntArg.Create(Abs(aEvaluatedOperand.AsInt));
end;

function TRoundOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TIntArg.Create(Round(aEvaluatedOperand.AsFloat))
    else
        result := TIntArg.Create(Round(aEvaluatedOperand.AsInt));
end;

function TTruncOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TIntArg.Create(Trunc(aEvaluatedOperand.AsFloat))
    else
        result := TIntArg.Create(Trunc(aEvaluatedOperand.AsInt));
end;

function TUnaryMinusOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(-(aEvaluatedOperand.AsFloat))
    else
        result := TIntArg.Create(-(aEvaluatedOperand.AsInt));
end;

function TRandomOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckIntType(aEvaluatedOperand);
    if aEvaluatedOperand.AsInt = -1 then
    begin
        Randomize();
        result := TIntArg.Create(0);
    end
    else if aEvaluatedOperand.AsInt > 0 then
    begin
        result := TIntArg.Create(Random(aEvaluatedOperand.AsInt));
    end
    else
        raise Exception.CreateFmt('The argument for the rand function [%d] must be greater than zero',
            [aEvaluatedOperand.AsInt]);
end;

{ TSelectBitOperatorEvalNode }

function TSelectBitOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
    xBitIndex: integer;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptNum, pfptNum]);
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    xBitIndex := NumValToInt(xParams[1]) - 1;
    result := TIntArg.Create(NumValToInt(xParams[0]) or (1 shl xBitIndex));
end;

{ TIsBitSelectedOperatorEvalNode }

function TIsBitSelectedOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
    xBitIndex: integer;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptNum, pfptNum]);
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    xBitIndex := NumValToInt(xParams[1]) - 1;
    result := TBoolArg.Create(((NumValToInt(xParams[0]) shr xBitIndex) and 1) = 1);
end;

{ TCosineOperatorEvalNode }

function TCosineOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(System.Cos(aEvaluatedOperand.AsFloat))
    else
        result := TDblArg.Create(System.Cos(aEvaluatedOperand.AsInt));
end;

{ TCosineHyperbolicOperatorEvalNode }

function TCosineHyperbolicOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(Math.CosH(aEvaluatedOperand.AsFloat))
    else
        result := TDblArg.Create(Math.CosH(aEvaluatedOperand.AsInt));
end;

{ TSineOperatorEvalNode }

function TSineOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(System.Sin(aEvaluatedOperand.AsFloat))
    else
        result := TDblArg.Create(System.Sin(aEvaluatedOperand.AsInt));
end;

{ TSineHyperbolicOperatorEvalNode }

function TSineHyperbolicOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(Math.SinH(aEvaluatedOperand.AsFloat))
    else
        result := TDblArg.Create(Math.SinH(aEvaluatedOperand.AsInt));
end;

{ TTangentOperatorEvalNode }

function TTangentOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(Math.Tan(aEvaluatedOperand.AsFloat))
    else
        result := TDblArg.Create(Math.Tan(aEvaluatedOperand.AsInt));
end;

{ TArcTangentOperatorEvalNode }

function TArcTangentOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(System.ArcTan(aEvaluatedOperand.AsFloat))
    else
        result := TDblArg.Create(System.ArcTan(aEvaluatedOperand.AsInt));
end;

{ TSqareRootOperatorEvalNode }

function TSqareRootOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(System.Sqrt(aEvaluatedOperand.AsFloat))
    else
        result := TDblArg.Create(System.Sqrt(aEvaluatedOperand.AsInt));
end;

{ TExponentOperatorEvalNode }

function TExponentOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(System.Exp(aEvaluatedOperand.AsFloat))
    else
        result := TDblArg.Create(System.Exp(aEvaluatedOperand.AsInt));
end;

{ TLogarithmNaturalOperatorEvalNode }

function TLogarithmNaturalOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(System.Ln(aEvaluatedOperand.AsFloat))
    else
        result := TDblArg.Create(System.Ln(aEvaluatedOperand.AsInt));
end;

{ TLogarithmBase10OperatorEvalNode }

function TLogarithmBase10OperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    CheckNumType(aEvaluatedOperand);
    if IsDbl(aEvaluatedOperand) then
        result := TDblArg.Create(Math.Log10(aEvaluatedOperand.AsFloat))
    else
        result := TDblArg.Create(Math.Log10(aEvaluatedOperand.AsInt));
end;

{ TPowerOperatorEvalNode }

function TPowerOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg;
var
    xBase, xExponent: extended;
begin
    if IsDbl(aEvaluatedOperand1) then
        xBase := aEvaluatedOperand1.AsFloat
    else
        xBase := aEvaluatedOperand1.AsInt;

    if IsDbl(aEvaluatedOperand2) then
        xExponent := aEvaluatedOperand2.AsFloat
    else
        xExponent := aEvaluatedOperand2.AsInt;

    result := TDblArg.Create(Math.Power(xBase, xExponent));
end;

{ TMinValueOperatorEvalNode }

function TMinValueOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    x, xResultInt: integer;
    xResultDouble: double;
    xAllInt: boolean;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptArr]);

    xResultDouble := Math.MaxDouble; // Höchster Double-Wert
    xResultInt := 2147483646; // Höchster Int-Wert
    xAllInt := true;

    for x := 0 to (aEvaluatedOperand as TArrayArg).Count - 1 do
    begin
        if IsDbl((aEvaluatedOperand as TArrayArg)[x]) then
        begin
            if (xResultDouble > (aEvaluatedOperand as TArrayArg)[x].AsFloat) then
                xResultDouble := (aEvaluatedOperand as TArrayArg)[x].AsFloat;
            xAllInt := false;
        end
        else
        begin
            if (xResultDouble > (aEvaluatedOperand as TArrayArg)[x].AsInt) then
                xResultDouble := (aEvaluatedOperand as TArrayArg)[x].AsInt;
            if (xResultInt > (aEvaluatedOperand as TArrayArg)[x].AsInt) then
                xResultInt := (aEvaluatedOperand as TArrayArg)[x].AsInt;
        end;
    end;

    if xAllInt then
        result := TIntArg.Create(xResultInt)
    else
        result := TDblArg.Create(xResultDouble);
end;

{ TMaxValueOperatorEvalNode }

function TMaxValueOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    x, xResultInt: integer;
    xResultDouble: double;
    xAllInt: boolean;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptArr]);

    xResultDouble := Math.MinDouble; // Niedrigster Double-Wert
    xResultInt := -2147483647; // Niedrigster Int-Wert
    xAllInt := true;

    for x := 0 to (aEvaluatedOperand as TArrayArg).Count - 1 do
    begin
        if IsDbl((aEvaluatedOperand as TArrayArg)[x]) then
        begin
            if (xResultDouble < (aEvaluatedOperand as TArrayArg)[x].AsFloat) then
                xResultDouble := (aEvaluatedOperand as TArrayArg)[x].AsFloat;
            xAllInt := false;
        end
        else
        begin
            if (xResultDouble < (aEvaluatedOperand as TArrayArg)[x].AsInt) then
                xResultDouble := (aEvaluatedOperand as TArrayArg)[x].AsInt;
            if (xResultInt < (aEvaluatedOperand as TArrayArg)[x].AsInt) then
                xResultInt := (aEvaluatedOperand as TArrayArg)[x].AsInt;
        end;
    end;

    if xAllInt then
        result := TIntArg.Create(xResultInt)
    else
        result := TDblArg.Create(xResultDouble);
end;

{ TSumOperatorEvalNode }

function TSumOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg;
var
    x, xResultInt: integer;
    xResultDouble: double;
    xAllInt: boolean;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptArr]);

    xResultDouble := 0;
    xResultInt := 0;
    xAllInt := true;

    for x := 0 to (aEvaluatedOperand as TArrayArg).Count - 1 do
    begin
        if IsDbl((aEvaluatedOperand as TArrayArg)[x]) then
        begin
            xResultDouble := xResultDouble + (aEvaluatedOperand as TArrayArg)[x].AsFloat;
            xAllInt := false;
        end
        else
        begin
            xResultDouble := xResultDouble + (aEvaluatedOperand as TArrayArg)[x].AsInt;
            xResultInt := xResultInt + (aEvaluatedOperand as TArrayArg)[x].AsInt;
        end;
    end;

    if xAllInt then
        result := TIntArg.Create(xResultInt)
    else
        result := TDblArg.Create(xResultDouble);
end;

{ TAverageOperatorEvalNode }

function TAverageOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    x: integer;
    xResultDouble: double;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptArr]);

    xResultDouble := 0;

    for x := 0 to (aEvaluatedOperand as TArrayArg).Count - 1 do
    begin
        if IsDbl((aEvaluatedOperand as TArrayArg)[x]) then
        begin
            xResultDouble := xResultDouble + (aEvaluatedOperand as TArrayArg)[x].AsFloat;
        end
        else
        begin
            xResultDouble := xResultDouble + (aEvaluatedOperand as TArrayArg)[x].AsInt;
        end;
    end;

    result := TDblArg.Create(xResultDouble / (aEvaluatedOperand as TArrayArg).ArrayLength);
end;

{ TStandardDeviationOperatorEvalNode }

function TStandardDeviationOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    x, xOpArrLength: integer;
    xDoubleArr: TArray<double>;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptArr]);

    // Länge des Ursprungsarrays ermitteln
    xOpArrLength := (aEvaluatedOperand as TArrayArg).ArrayLength;
    SetLength(xDoubleArr, xOpArrLength);

    for x := 0 to (aEvaluatedOperand as TArrayArg).Count - 1 do
    begin
        if IsDbl((aEvaluatedOperand as TArrayArg)[x]) then
        begin
            xDoubleArr[x] := (aEvaluatedOperand as TArrayArg)[x].AsFloat;
        end
        else
        begin
            xDoubleArr[x] := (aEvaluatedOperand as TArrayArg)[x].AsInt;
        end;
    end;

    if (xOpArrLength > 0) then
        result := TDblArg.Create(Math.StdDev(xDoubleArr))
    else
        result := TDblArg.Create(0); // damit es nicht kracht
end;

{ TRelativeStandardDeviationOperatorEvalNode }

function TRelativeStandardDeviationOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    x, xOpArrLength: integer;
    xDoubleArr: array of double;
    xResultDouble: double;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptArr]);

    xResultDouble := 0;

    // Länge des Ursprungsarrays ermitteln
    xOpArrLength := (aEvaluatedOperand as TArrayArg).ArrayLength;

    SetLength(xDoubleArr, xOpArrLength);

    for x := 0 to (aEvaluatedOperand as TArrayArg).Count - 1 do
    begin
        if IsDbl((aEvaluatedOperand as TArrayArg)[x]) then
        begin
            xDoubleArr[x] := (aEvaluatedOperand as TArrayArg)[x].AsFloat;
            xResultDouble := xResultDouble + xDoubleArr[x];
        end
        else
        begin
            xDoubleArr[x] := (aEvaluatedOperand as TArrayArg)[x].AsInt;
            xResultDouble := xResultDouble + xDoubleArr[x];
        end;
    end;

    if (xOpArrLength > 0) then
        result := TDblArg.Create(Math.StdDev(xDoubleArr) / (xResultDouble / xOpArrLength))
    else
        result := TDblArg.Create(0); // damit es nicht kracht
end;

{ TNewArrayOperatorEvalNode }

function TNewArrayOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
    xResult: TArrayArg;
    xDest, xArrLength: integer;
begin
    if (aEvaluatedOperand is TFuncParams) then
    begin
        xParams := (aEvaluatedOperand as TFuncParams).Arr;

        // Länge des Ursprungsarrays ermitteln
        xArrLength := Length(xParams);

        xResult := TArrayArg.Create();
        xResult.ArrayLength := xArrLength;

        // alle Array-Elemente kopieren
        for xDest := 1 to xArrLength do
        begin
            xResult.SetItemByUserIndex(xDest, TArgUtils.CopyAttrValue(xParams[xDest - 1]));
        end;

        EXIT(xResult);
    end
    else
    begin
        // Spezialfall: Es gibt nur ein Array-Element
        xResult := TArrayArg.Create();
        xResult.ArrayLength := 1;
        xResult.SetItemByUserIndex(1, TArgUtils.CopyAttrValue(aEvaluatedOperand));
        EXIT(xResult);
    end;
end;

{ TCastOperatorEvalNode }

function TCastOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
    xCastToType: TParserIdentDataType;
begin
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    xCastToType := TArgUtils.IdentifyTypeByName(xParams[1].AsStr);

    if (xCastToType = idtInteger) then
        EXIT(TIntArg.Create(TArgUtils.DataValueToInt(xParams[0])));

    if (xCastToType = idtFloat) then
        EXIT(TDblArg.Create(TArgUtils.DataValueToFloat(xParams[0])));

    if (xCastToType = idtBool) then
        EXIT(TBoolArg.Create(TArgUtils.DataValueToBool(xParams[0])));

    if (xCastToType = idtString) then
        EXIT(TStrArg.Create(TArgUtils.DataValueToStr(xParams[0])));

    raise Exception.Create('Mismatched parameter for CAST: ' + xParams[1].AsStr);
end;

{ TFormatPercentOperatorEvalNode }

function TFormatPercentOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
    xRoundTo: integer;
    xPercentVal: double;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptNum, pfptNum]);
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    xRoundTo := NumValToInt(xParams[1]);
    xPercentVal := NumValToDbl(xParams[0]) * 100;
    result := TStrArg.Create(Format('%.' + IntToStr(xRoundTo) + 'f', [xPercentVal]) + ' %');
end;

{ TTypeOfOperatorEvalNode }

function TTypeOfOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    EXIT(TStrArg.Create(TArgUtils.IdentTypeToLogStr(aEvaluatedOperand, TArrayArg.UndefinedBoundsIndex)));
end;

{ TBitsToArrayOperatorEvalNode }

function TBitsToArrayOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xResult: TArrayArg;
    xArrLength: integer;
    xBitmap: integer;
    x: integer;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptNum]);

    xArrLength := 0;
    xResult := TArrayArg.Create();
    xResult.ArrayLength := xArrLength;
    xBitmap := NumValToInt(aEvaluatedOperand);
    for x := 0 to 30 do // maximal 31 Bits
    begin
        if (((xBitmap shr x) and 1) = 1) then
        begin
            inc(xArrLength);
            xResult.ArrayLength := xArrLength;
            xResult.SetItemByUserIndex(xArrLength, TIntArg.Create(x + 1));
        end;
    end;

    EXIT(xResult);
end;

{ TArrayToBitsOperatorEvalNode }

function TArrayToBitsOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    x, xOpArrLength: integer;
    xBitmap: integer;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptArr]);

    xBitmap := 0;

    // Länge des Ursprungsarrays ermitteln
    xOpArrLength := (aEvaluatedOperand as TArrayArg).ArrayLength;
    for x := 1 to xOpArrLength do
    begin
        xBitmap := xBitmap or (1 shl ((aEvaluatedOperand as TArrayArg).GetItemByUserIndex(x).AsInt - 1));
    end;

    result := TIntArg.Create(xBitmap);
end;

{ TJsonEvalOperatorEvalNode }

function TJsonEvalOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
    xIdentifier, xJSONString, xResult: string;
    xPosOfIdentifier: integer;
    xType: TParserIdentDataType;
    xArr: TArrayArg;
    xArrLength: integer;
begin
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    xIdentifier := xParams[1].AsStr;
    xJSONString := xParams[0].AsStr;
    xPosOfIdentifier := Pos(xIdentifier, xJSONString);
    if xPosOfIdentifier > 0 then
    begin
        xJSONString := Copy(xJSONString, xPosOfIdentifier + Length(xIdentifier) + 1, Length(xJSONString));
        while (xJSONString[1] = ':') or (xJSONString[1] = ' ') do
            xJSONString := Copy(xJSONString, 2, Length(xJSONString) - 1);
        if xJSONString[1] = '"' then // String
        begin
            xJSONString := Copy(xJSONString, 2, Length(xJSONString) - 1);
            xResult := Copy(xJSONString, 1, Pos('"', xJSONString) - 1);
            xType := TArgUtils.DefineTypeByValue(xResult);
            EXIT(TArgUtils.CreateArgByType(xResult, xType));
        end;
        if xJSONString[1] = '[' then // Array
        begin
            xArrLength := 0;
            xArr := TArrayArg.Create();
            xArr.ArrayLength := xArrLength;
            if xJSONString[2] = ']' then
                EXIT(xArr);
            xJSONString := Copy(xJSONString, 2, Pos(']', xJSONString) - 2);
            xJSONString := StringReplace(xJSONString, '"', '', [rfReplaceAll]);
            while Pos(',', xJSONString) > 0 do
            begin
                inc(xArrLength);
                xArr.ArrayLength := xArrLength;
                xResult := Copy(xJSONString, 1, Pos(',', xJSONString) - 1);
                xJSONString := Copy(xJSONString, Pos(',', xJSONString) + 1,
                    Length(xJSONString) - Pos(',', xJSONString));
                xType := TArgUtils.DefineTypeByValue(xResult);
                xArr.SetItemByUserIndex(xArrLength, TArgUtils.CreateArgByType(xResult, xType));
            end;
            inc(xArrLength);
            xArr.ArrayLength := xArrLength;
            xType := TArgUtils.DefineTypeByValue(xJSONString);
            xArr.SetItemByUserIndex(xArrLength, TArgUtils.CreateArgByType(xJSONString, xType));
            EXIT(xArr);
        end;
        if Pos(',', xJSONString) > 0 then
            xResult := Copy(xJSONString, 1, Pos(',', xJSONString) - 1)
        else
            xResult := Copy(xJSONString, 1, Length(xJSONString) - 1);
        xType := TArgUtils.DefineTypeByValue(xResult);
        EXIT(TArgUtils.CreateArgByType(xResult, xType));
    end
    else
    begin
        xType := TArgUtils.DefineTypeByValue(xParams[2].AsStr);
        result := TArgUtils.CreateArgByType(xParams[2].AsStr, xType);
    end;
end;

{ TJsonAddOperatorEvalNode }

function TJsonAddOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
    xIdentifier, xJSONText: string;
    xJSONObject: TJSONObject;
begin
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    xJSONText := xParams[0].AsStr;
    xIdentifier := xParams[1].AsStr;
    xJSONObject := TJSONObject.Create;
    try
        xJSONObject.Parse(BytesOf(xJSONText), 0);

        // wir löschen zuerst den Identifier, sonst würde er zweimal angelegt
        xJSONObject.RemovePair(xIdentifier);

        xJSONObject.AddPair(xIdentifier, TParserIdentToJSONUtils.GetJSONValue(xParams[2]));
        result := TArgUtils.CreateArgByType(xJSONObject.ToString, idtString);
    finally
        FreeAndNil(xJSONObject);
    end;
end;

{ TJsonRemoveOperatorEvalNode }

function TJsonRemoveOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xParams: TArray<TArg>;
    xIdentifier, xJSONText: string;
    xJSONObject: TJSONObject;
begin
    xParams := (aEvaluatedOperand as TFuncParams).Arr;
    xJSONText := xParams[0].AsStr;
    xIdentifier := xParams[1].AsStr;
    xJSONObject := TJSONObject.Create;
    try
        xJSONObject.Parse(BytesOf(xJSONText), 0);
        xJSONObject.RemovePair(xIdentifier);
        result := TArgUtils.CreateArgByType(xJSONObject.ToString, idtString);
    finally
        FreeAndNil(xJSONObject);
    end;
end;


end.
