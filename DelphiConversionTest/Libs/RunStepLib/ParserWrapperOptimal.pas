{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.06.05 pk                               TN2449   Initial revision
  15.08.05 pk  Tokenize                     TN2559   Escape character \ can now be used to include a quotation mark in a string ( eg. " \"xyz \" " --> "xyz" )
  15.08.05 pk  TParserTokenizer             TN2560   New : extracted from TParserWrapperOpt
  15.08.05 pk                               TN2560   New : AddConcatSymbolsToList, RemoveConcatSymbolsFromList
  15.08.05 pk                               TN2560   New : RemoveQuotes. Remove the "" symbols from the given string
  25.08.05 pk                               TN2573   Support for RAND function, Logical OR, Logical AND
  06.10.05 pk  TParserTokenizer             TN2646   Various changes to try to read minus sign inside of string - this feature still does not work
  06.10.05 pk  TParserWrapperOpt            TN2656   Changes to make unary minus effect only the element that follows and not entire statement
  14.11.05 pk  ContainsVariable             TN2720   New
  27.10.06 wl  TParserTokenizer.FuncToken   TN3331   kennt neue Funktion DLLFU()
  27.10.06 wl  TParserTokenizer.FuncToken   TN3331   vorbereitet: IMPOR, SLSQL, USQL, COPYF
  21.07.06 pk                               TN3583   Various changes to avoid memory leaks
  01.08.07 pk  TParserTokenizer.FuncToken   TN3814   New functions: SUBSTR, POS, NOW
  27.08.07 pk                               TN3788   MASSIVE CHANGES needed to allow for new parser functions to be added to the parser more easily
  09.01.08 wl                               TN3972   interne Änderung
  08.09.08 pk                               TN4215   IMPOR, SLSQL, USQL, COPYF removed for now
  19.09.08 pk TOKEN_DLLFUNCTION             TN4215   removed
  06.11.08 pk TParserWrapperOpt             TN4279   no longer implements IParserSymbolTable
  17.02.09 pk                               TN4232   DATA functions removed
  21.07.09 pk CreateEvalNodeFactory         TN4669   SELECTBIT and ISBITSELECTED added
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  12.11.09 pk ParseTokens                   TN4843   check xSymbolStack.Count = 0 instead of assigned( peek )
  07.05.10 pk CreateAttributeGrammar        TN5092   changes needed for new array type
  12.05.10 pk CreateParseTable              TN5109   new production rule: 'S', ttBracketOpen, 'S->E'
  27.06.11 wl  CreateEvalNodeFactory        TN5609   neu: STR_TOKEN_SUBARR (SUB)
  18.07.11 wl  CreateEvalNodeFactory        TN5628   neu: Statistik-Funktionen für Arrays: MIN,MAX,SUM,AVR,STDDEV
  18.07.11 wl  CreateEvalNodeFactory        TN5628.1 neu: A() erzeugt neues Array
  02.08.11 wl  CreateEvalNodeFactory        TN5481   alle mathematischen Funktionen, die es früher schon mal gab
  02.08.11 wl  CreateEvalNodeFactory        TN5481   neu: POWER, LOG10
  02.08.11 wl  CreateEvalNodeFactory        TN5481   Round-Funktion kann jetzt RND oder ROUND geschrieben werden
  02.08.11 wl  CreateEvalNodeFactory        TN5609.1 LEN für Arrays heißt jetzt COUNT
  18.08.11 wl  CreateEvalNodeFactory        TN5665   neu: CAST
  17.10.11 wl  CreateEvalNodeFactory        TN5717   neu: INDEXOF findet Element in Array
  21.02.12 wl  CreateEvalNodeFactory        TN5805   neu: RSD, FMT_PERCENT
  02.03.12 wl  CreateEvalNodeFactory        TN5823   neu: TYPEOF
  06.03.12 wl  CreateEvalNodeFactory        TN5824   neu: BITSTOARRAY, ARRAYTOBITS
  14.12.12 wl  Create                       TN6054   benötigt MethodName
  14.12.12 wl  TParserInstances             TN6054   Verwaltung der globalen Instanzen, getrennt von TParserWrapperOpt
  28.05.13 ts  CreateEvalNodeFactory        TN6152   neu: STR_TOKEN_JSON_EVAL
  25.06.13 wl                               TN6178   an ParserSymbolTable angepasst
  28.06.13 wl  CreateEvalNodeFactory        TN6191   neu: STR_TOKEN_JSON_ADD,_REMOVE
  -------------------------------------------------------------------------------------------------- }

unit ParserWrapperOptimal;


interface


uses
    ParserWrapperCommon,
    ParserSymbolTable,
    ParserToken,
    ParserTree,
    ParserGrammar,
    ParserEvalNodeFactory,
    ParserTokenizer;

type
    TParserInstances = record
    strict private
        class var uParseTable: TParseTable;
        class var uEvalNodeFactory: TParserEvalNodeFactory;
        class function CreateEvalNodeFactory: TParserEvalNodeFactory; static;
        class function CreateAttributeGrammar(): TAttributeGrammar; static;
        class function EvalNodeFactoryInst: TParserEvalNodeFactory; static;
        class function CreateParseTable(): TParseTable; static;
    public
        class procedure DestroyInstances; static;
        class function ParseTableInst(): TParseTable; static;
    end;

    TParserWrapperOpt = class(TParserWrapperCommon)
    private
        fTokenizer: TParserTokenizer;
    public
        destructor Destroy(); override;
        constructor Create(const aMethodName: string);

        procedure ParseBeginScope(aLocalList: TParserIdentifierList);
        procedure ParseEndScope();

        procedure BeginScope(aLocalList: TParserIdentifierList);
        procedure EndScope();

        function Tokenize(const aInput: string): TTokenList;
        procedure ParseTokens(aParseTree: TParseTree);
    end;


implementation


uses
    SysUtils,
    Generics.Collections,

    ParserAttribute,
    ParserOperatorEvalNodeBasic;

{ TParserWrapperOpt }

constructor TParserWrapperOpt.Create(const aMethodName: string);
begin
    inherited Create(aMethodName);
    fTokenizer := TParserTokenizer.Create();
    fTokenizer.OnIdentFound := AddIdentToList;
end;

destructor TParserWrapperOpt.Destroy();
begin
    fTokenizer.Free;
    inherited;
end;

procedure TParserWrapperOpt.ParseBeginScope(aLocalList: TParserIdentifierList);
begin
    self.SymbolTable.BeginScope(aLocalList);
end;

procedure TParserWrapperOpt.ParseEndScope();
begin
    self.SymbolTable.EndScope();
end;

procedure TParserWrapperOpt.BeginScope(aLocalList: TParserIdentifierList);
var
    xLocalList: TParserIdentifierList;
begin
    // This is also the beginscope function used during the evaluation of evalnodes

    // make a copy list of all idents in aLocalList because we dont
    // want to change the original idents in aLocalList because the aLocalList may be re-used
    xLocalList := TParserIdentifierList.CreateFromList(aLocalList);
    self.SymbolTable.BeginScope(xLocalList);
end;

procedure TParserWrapperOpt.EndScope();

var
    xLocalList: TParserIdentifierList;
begin
    xLocalList := self.SymbolTable.EndScope();
    xLocalList.Free;
end;

function TParserWrapperOpt.Tokenize(const aInput: string): TTokenList;
begin
    EXIT(fTokenizer.Tokenize(aInput));
end;

procedure TParserWrapperOpt.ParseTokens(aParseTree: TParseTree);
var
    xSymbolStack: TStack<TSymbol>;
    xNodeStack: TStack<TParseTreeNode>;
    xTokenIndex: integer;
    xToken: TToken;
    xCurStackSymbol: TSymbol;
    xProdRule: TProductionRule;
    xStackTokenType: ParserToken.TTokenType;
    i: integer;
    xCurrentTreeNode: TParseTreeNode;
    xBodyNode: TParseTreeNode;
    xBodySymbol: TSymbol;
    xParseTable: TParseTable;

    function NextToken(): TToken;
    begin
        result := nil;
        if xTokenIndex >= aParseTree.TokenList.Count then
            EXIT;
        result := aParseTree.TokenList[xTokenIndex] as TToken;
        Inc(xTokenIndex);
    end;

begin
    xParseTable := TParserInstances.ParseTableInst();
    xSymbolStack := TStack<TSymbol>.Create();
    xNodeStack := TStack<TParseTreeNode>.Create();

    xTokenIndex := 0;
    xSymbolStack.Push(xParseTable.StartSymbol);
    aParseTree.AddRootNodeFromStartSymbol(xParseTable.StartSymbol);
    xNodeStack.Push(aParseTree.Root);
    xToken := NextToken;

    while true do
    begin
        if xSymbolStack.Count = 0 then
            BREAK;
        xCurStackSymbol := xSymbolStack.Peek();
        xSymbolStack.Pop;

        xCurrentTreeNode := xNodeStack.Peek();
        xNodeStack.Pop;

        if xCurStackSymbol is TSymbolTerminal then
        begin
            xStackTokenType := (xCurStackSymbol as TSymbolTerminal).TokenType;

            if xStackTokenType = xToken.TokenType then
            begin
                ASSERT(xCurrentTreeNode is TTerminalParseTreeNode);
                (xCurrentTreeNode as TTerminalParseTreeNode).Token := xToken;
                xToken := NextToken;
            end
            else
                ASSERT(false);

        end
        else
        begin // * X is a non-terminal */
            xProdRule := xParseTable.GetProduction((xCurStackSymbol as TSymbolNonTerminal).KeyName,
                xToken.TokenType);
            if not(Assigned(xProdRule)) then
                raise Exception.CreateFmt('TokenList : %s', [aParseTree.TokenList.TokensToStr()]);

            xCurrentTreeNode.AddSymbolAttributes(xProdRule.Head);
            for i := xProdRule.Body.Count - 1 downto 0 do
            begin
                xBodySymbol := xProdRule.Body[i];
                xSymbolStack.Push(xBodySymbol);
                xBodyNode := aParseTree.AddNodeFromSymbol(xBodySymbol, xCurrentTreeNode);
                xNodeStack.Push(xBodyNode);
            end;
        end;
    end;

    xSymbolStack.Free;
    xNodeStack.Free;
end;

{ TParserInstances }

class procedure TParserInstances.DestroyInstances;
begin
    uParseTable.Free;
    uEvalNodeFactory.Free;
end;

class function TParserInstances.ParseTableInst(): TParseTable;
begin
    if not Assigned(uParseTable) then
        uParseTable := CreateParseTable();

    result := uParseTable;
end;

class function TParserInstances.EvalNodeFactoryInst(): TParserEvalNodeFactory;
begin
    if not Assigned(uEvalNodeFactory) then
        uEvalNodeFactory := CreateEvalNodeFactory();

    result := uEvalNodeFactory;
end;

class function TParserInstances.CreateEvalNodeFactory(): TParserEvalNodeFactory;
begin
    result := TParserEvalNodeFactory.Create();

    // Basic Binary Operators
    result.AddImplement(STR_TOKEN_FUNCPARDELIM, TFuncParamsDelimOperatorEvalNode);
    result.AddImplement(STR_TOKEN_CONCAT, TConcatOperatorEvalNode);
    result.AddImplement(STR_TOKEN_ADD, TAddOperatorEvalNode);
    result.AddImplement(STR_TOKEN_SUBTRACT, TSubtractOperatorEvalNode);
    result.AddImplement(STR_TOKEN_MULTIPLY, TMultOperatorEvalNode);
    result.AddImplement(STR_TOKEN_DIVIDE, TDivOperatorEvalNode);
    result.AddImplement(STR_TOKEN_MOD, TModOperatorEvalNode);
    result.AddImplement(STR_TOKEN_DIV, TIntDivOperatorEvalNode);
    result.AddImplement(STR_TOKEN_OR, TOrOperatorEvalNode);
    result.AddImplement(STR_TOKEN_AND, TAndOperatorEvalNode);
    result.AddImplement(STR_TOKEN_EQUALS, TEqualOperatorEvalNode);
    result.AddImplement(STR_TOKEN_LESSTHAN, TLessThanOperatorEvalNode);
    result.AddImplement(STR_TOKEN_GREATERTHAN, TGreaterThanOperatorEvalNode);
    result.AddImplement(STR_TOKEN_LESSTHANEQUALS, TLessThanEqualOperatorEvalNode);
    result.AddImplement(STR_TOKEN_GREATERTHANEQUALS, TGreaterThanEqualOperatorEvalNode);
    result.AddImplement(STR_TOKEN_NOTEQUALS, TNotEqualOperatorEvalNode);

    // Basic Unary Operators
    result.AddImplement(STR_TOKEN_UNARYMINUS, TUnaryMinusOperatorEvalNode);

    // Basic functions - these functions use general units
    result.AddImplement(STR_TOKEN_SUBARR, TSubArrOperatorEvalNode);
    result.AddImplement(STR_TOKEN_SUBSTR, TSubstrOperatorEvalNode);
    result.AddImplement(STR_TOKEN_UPPER, TUpperCaseOperatorEvalNode);
    result.AddImplement(STR_TOKEN_POS, TPosOperatorEvalNode);
    result.AddImplement(STR_TOKEN_LEN, TLengthOperatorEvalNode);
    result.AddImplement(STR_TOKEN_COUNT, TCountOperatorEvalNode);
    result.AddImplement(STR_TOKEN_TRIM, TTrimOperatorEvalNode);
    result.AddImplement(STR_TOKEN_REVSTR, TRevstrOperatorEvalNode);
    result.AddImplement(STR_TOKEN_PAD, TPadstrOperatorEvalNode);
    result.AddImplement(STR_TOKEN_XMOD, TXModOperatorEvalNode);
    result.AddImplement(STR_TOKEN_XDIV, TXDivOperatorEvalNode);
    result.AddImplement(STR_TOKEN_NOW, TNowOperatorEvalNode);
    result.AddImplement(STR_TOKEN_ABS, TAbsOperatorEvalNode);
    result.AddImplement(STR_TOKEN_TRUNC, TTruncOperatorEvalNode);
    result.AddImplement(STR_TOKEN_ROUND1, TRoundOperatorEvalNode);
    result.AddImplement(STR_TOKEN_ROUND2, TRoundOperatorEvalNode);
    result.AddImplement(STR_TOKEN_RANDOM, TRandomOperatorEvalNode);
    result.AddImplement(STR_TOKEN_SELECTBIT, TSelectBitOperatorEvalNode);
    result.AddImplement(STR_TOKEN_ISBITSELECTED, TIsBitSelectedOperatorEvalNode);
    result.AddImplement(STR_TOKEN_MIN, TMinValueOperatorEvalNode);
    result.AddImplement(STR_TOKEN_MAX, TMaxValueOperatorEvalNode);
    result.AddImplement(STR_TOKEN_SUM, TSumOperatorEvalNode);
    result.AddImplement(STR_TOKEN_AVR, TAverageOperatorEvalNode);
    result.AddImplement(STR_TOKEN_STDDEV, TStandardDeviationOperatorEvalNode);
    result.AddImplement(STR_TOKEN_NEWARRAY, TNewArrayOperatorEvalNode);
    result.AddImplement(STR_TOKEN_CAST, TCastOperatorEvalNode);
    result.AddImplement(STR_TOKEN_INDEXOF, TIndexOfOperatorEvalNode);
    result.AddImplement(STR_TOKEN_TYPEOF, TTypeOfOperatorEvalNode);

    result.AddImplement(STR_TOKEN_COS, TCosineOperatorEvalNode);
    result.AddImplement(STR_TOKEN_COSH, TCosineHyperbolicOperatorEvalNode);
    result.AddImplement(STR_TOKEN_SIN, TSineOperatorEvalNode);
    result.AddImplement(STR_TOKEN_SINH, TSineHyperbolicOperatorEvalNode);
    result.AddImplement(STR_TOKEN_TAN, TTangentOperatorEvalNode);
    result.AddImplement(STR_TOKEN_ARCTAN, TArcTangentOperatorEvalNode);
    result.AddImplement(STR_TOKEN_SQRT, TSqareRootOperatorEvalNode);
    result.AddImplement(STR_TOKEN_EXP, TExponentOperatorEvalNode);
    result.AddImplement(STR_TOKEN_LOG, TLogarithmNaturalOperatorEvalNode);
    result.AddImplement(STR_TOKEN_LOG10, TLogarithmBase10OperatorEvalNode);
    result.AddImplement(STR_TOKEN_POWER, TPowerOperatorEvalNode);
    result.AddImplement(STR_TOKEN_FMT_PERCENT, TFormatPercentOperatorEvalNode);
    result.AddImplement(STR_TOKEN_RSD, TRelativeStandardDeviationOperatorEvalNode);
    result.AddImplement(STR_TOKEN_BITSTOARRAY, TBitsToArrayOperatorEvalNode);
    result.AddImplement(STR_TOKEN_ARRAYTOBITS, TArrayToBitsOperatorEvalNode);
    result.AddImplement(STR_TOKEN_JSON_EVAL, TJsonEvalOperatorEvalNode);
    result.AddImplement(STR_TOKEN_JSON_ADD, TJsonAddOperatorEvalNode);
    result.AddImplement(STR_TOKEN_JSON_REMOVE, TJsonRemoveOperatorEvalNode);
end;

class function TParserInstances.CreateAttributeGrammar(): TAttributeGrammar;
var
    xEvalNodeFactory: TParserEvalNodeFactory;
    xAttributeGrammar: TAttributeGrammar;
    xH: TSymbolNonTerminal;
    xB1, xB2, xB3, xB4: TSymbol;

    procedure SynthEval(const aSelfAttrName: string = STR_ATTR_NAME_EVAL);
    begin
        xH.AddAttr(TSynthAttribute.Create(STR_ATTR_NAME_EVAL, TAttrRef.Create(alSelf, aSelfAttrName),
            TAttrRef.Create(alParent, STR_ATTR_NAME_EVAL)));
    end;

    procedure DoubleTerminal();
    begin
        xB1.AddAttr(TTerminalSynthAttribute.Create(STR_ATTR_NAME_EVAL));
        xB1.AddAttr(TSimpleAttribute.Create(STR_ATTR_NAME_OPERAND1, TAttrRef.Create(alFirstChild,
            STR_ATTR_NAME_EVAL)));
        xB2.AddAttr(TSimpleAttribute.Create(STR_ATTR_NAME_OPERAND1, TAttrRef.Create(alParent,
            STR_ATTR_NAME_OPERAND1)));
    end;

    procedure DoubleTerminalF();
    begin
        xB1.AddAttr(TTerminalSynthAttribute.Create(STR_ATTR_NAME_EVAL));
        // xB1.AddAttr( TSimpleAttribute.Create( STR_ATTR_NAME_EVAL, TAttrRef.Create( alFirstChild, STR_ATTR_NAME_EVAL ) ) );
        xB2.AddAttr(TSimpleAttribute.Create(STR_ATTR_NAME_OPERAND1, TAttrRef.Create(alParent,
            STR_ATTR_NAME_OPERAND1)));
    end;

    procedure BinaryOp();
    begin
        SynthEval();
        xB1.AddAttr(TTerminalAttribute.Create(STR_ATTR_NAME_SCANVAL,
            xEvalNodeFactory.CreateBinaryOperatorNode));
        xB2.AddAttr(TBinaryFuncInhAttribute.Create(STR_ATTR_NAME_OPERAND1, TAttrRef.Create(alParent,
            STR_ATTR_NAME_OPERAND1), TAttrRef.Create(alFirstChild, STR_ATTR_NAME_EVAL),
            TAttrRef.Create(alFirstSibling, STR_ATTR_NAME_SCANVAL)));
        xB2.AddAttr(TSynthAttribute.Create(STR_ATTR_NAME_EVAL, TAttrRef.Create(alSelf, STR_ATTR_NAME_EVAL),
            TAttrRef.Create(alParent, STR_ATTR_NAME_EVAL)));
    end;

    procedure Epsilon();
    begin
        SynthEval(STR_ATTR_NAME_OPERAND1);
    end;

    procedure Terminal();
    begin
        xB1.AddAttr(TSynthAttribute.Create(STR_ATTR_NAME_EVAL, TAttrRef.Create(alSelf, STR_ATTR_NAME_SCANVAL),
            TAttrRef.Create(alParent, STR_ATTR_NAME_EVAL)));
        xB1.AddAttr(TTerminalAttribute.Create(STR_ATTR_NAME_SCANVAL, xEvalNodeFactory.CreateTerminalNode));
    end;

    procedure Func();
    begin
        xB3.AddAttr(TSimpleAttribute.Create(STR_ATTR_NAME_OPERAND1, TAttrRef.Create(alFirstChild,
            STR_ATTR_NAME_EVAL)));
        xB3.AddAttr(TUnaryFuncSynthAttribute.Create(STR_ATTR_NAME_EVAL, TAttrRef.Create(alSelf,
            STR_ATTR_NAME_EVAL), TAttrRef.Create(alFirstSibling, STR_ATTR_NAME_SCANVAL),
            TAttrRef.Create(alParent, STR_ATTR_NAME_EVAL)));
        xB1.AddAttr(TTerminalAttribute.Create(STR_ATTR_NAME_SCANVAL,
            xEvalNodeFactory.CreateUnaryOperatorNode));
    end;

    procedure ArrayVar();
    begin
        xB3.AddAttr(TSimpleAttribute.Create(STR_ATTR_NAME_OPERAND1, TAttrRef.Create(alFirstChild,
            STR_ATTR_NAME_EVAL)));
        xB3.AddAttr(TUnaryFuncSynthAttribute.Create(STR_ATTR_NAME_EVAL, TAttrRef.Create(alSelf,
            STR_ATTR_NAME_EVAL), TAttrRef.Create(alFirstSibling, STR_ATTR_NAME_SCANVAL),
            TAttrRef.Create(alParent, STR_ATTR_NAME_EVAL)));
        xB1.AddAttr(TTerminalAttribute.Create(STR_ATTR_NAME_SCANVAL, xEvalNodeFactory.CreateArrayVarNode));
    end;

    procedure UnaryOp();
    begin
        xB2.AddAttr(TSimpleAttribute.Create(STR_ATTR_NAME_OPERAND1, TAttrRef.Create(alSelf,
            STR_ATTR_NAME_EVAL)));
        xB2.AddAttr(TUnaryFuncSynthAttribute.Create(STR_ATTR_NAME_EVAL, TAttrRef.Create(alSelf,
            STR_ATTR_NAME_EVAL), TAttrRef.Create(alFirstSibling, STR_ATTR_NAME_SCANVAL),
            TAttrRef.Create(alParent, STR_ATTR_NAME_EVAL)));
        xB1.AddAttr(TTerminalAttribute.Create(STR_ATTR_NAME_SCANVAL,
            xEvalNodeFactory.CreateUnaryOperatorNode));
    end;

begin
    xEvalNodeFactory := EvalNodeFactoryInst();
    xAttributeGrammar := TAttributeGrammar.Create();

    xH := TSymbolNonTerminal.Create('S', clNone);
    xB1 := TSymbolTerminal.Create(ttAssign, clLeft);
    xB2 := TSymbolNonTerminal.Create('E', clRight);
    xAttributeGrammar.AddProduction('S->:=E', xH, [xB1, xB2]);
    xH.AddAttr(TTerminalSynthAttribute.Create(STR_ATTR_NAME_EVAL));
    xB1.AddAttr(TTerminalAttribute.Create(STR_ATTR_NAME_SCANVAL, xEvalNodeFactory.CreateTerminalNode));
    xB2.AddAttr(TSimpleAttribute.Create(STR_ATTR_NAME_OPERAND1, TAttrRef.Create(alFirstChild,
        STR_ATTR_NAME_EVAL)));
    xB2.AddAttr(TUnaryFuncSynthAttribute.Create(STR_ATTR_NAME_EVAL, TAttrRef.Create(alSelf,
        STR_ATTR_NAME_EVAL), TAttrRef.Create(alFirstSibling, STR_ATTR_NAME_SCANVAL),
        TAttrRef.Create(alParent, STR_ATTR_NAME_EVAL)));

    xH := TSymbolNonTerminal.Create('S', clNone);
    xB1 := TSymbolNonTerminal.Create('E', clLeft);
    xAttributeGrammar.AddProduction('S->E', xH, [xB1]);
    xH.AddAttr(TTerminalSynthAttribute.Create(STR_ATTR_NAME_EVAL));
    xB1.AddAttr(TSimpleAttribute.Create(STR_ATTR_NAME_OPERAND1, TAttrRef.Create(alFirstChild,
        STR_ATTR_NAME_EVAL)));
    xB1.AddAttr(TSynthAttribute.Create(STR_ATTR_NAME_EVAL, TAttrRef.Create(alSelf, STR_ATTR_NAME_EVAL),
        TAttrRef.Create(alParent, STR_ATTR_NAME_EVAL)));

    xH := TSymbolNonTerminal.Create('S', clNone);
    xB1 := TSymbolTerminal.Create(ttFlowBegin, clLeft);
    xB2 := TSymbolNonTerminal.Create('Q', clRight);
    xAttributeGrammar.AddProduction('S->whileQ', xH, [xB1, xB2]);
    xH.AddAttr(TTerminalSynthAttribute.Create(STR_ATTR_NAME_EVAL));
    xB1.AddAttr(TTerminalAttribute.Create(STR_ATTR_NAME_SCANVAL, xEvalNodeFactory.CreateFlowContrBeginNode));

    xB2.AddAttr(TSimpleAttribute.Create(STR_ATTR_NAME_OPERAND1, TAttrRef.Create(alFirstChild,
        STR_ATTR_NAME_EVAL)));
    xB2.AddAttr(TUnaryFuncSynthAttribute.Create(STR_ATTR_NAME_EVAL, TAttrRef.Create(alSelf,
        STR_ATTR_NAME_EVAL), TAttrRef.Create(alFirstSibling, STR_ATTR_NAME_SCANVAL),
        TAttrRef.Create(alParent, STR_ATTR_NAME_EVAL)));

    xH := TSymbolNonTerminal.Create('S', clNone);
    xB1 := TSymbolTerminal.Create(ttFlowEnd, clLeft);
    xAttributeGrammar.AddProduction('S->endwhile', xH, [xB1]);
    xH.AddAttr(TTerminalSynthAttribute.Create(STR_ATTR_NAME_EVAL));
    xB1.AddAttr(TTerminalAttribute.Create(STR_ATTR_NAME_SCANVAL, xEvalNodeFactory.CreateFlowContrEndNode));
    xB1.AddAttr(TSynthAttribute.Create(STR_ATTR_NAME_EVAL, TAttrRef.Create(alSelf, STR_ATTR_NAME_SCANVAL),
        TAttrRef.Create(alParent, STR_ATTR_NAME_EVAL)));

    // *** P -> EP^ | ,P | Epsilon
    xH := TSymbolNonTerminal.Create('P', clNone);
    xB1 := TSymbolNonTerminal.Create('E', clLeft);
    xB2 := TSymbolNonTerminal.Create('P^', clRight);
    xAttributeGrammar.AddProduction('P->EP^', xH, [xB1, xB2]);
    DoubleTerminal();

    xH := TSymbolNonTerminal.Create('P^', clNone);
    xB1 := TSymbolTerminal.Create(ttFuncParDelim, clLeft);
    xB2 := TSymbolNonTerminal.Create('P', clRight);
    xAttributeGrammar.AddProduction('P^->,P', xH, [xB1, xB2]);
    BinaryOp();

    xH := TSymbolNonTerminal.Create('P^', clNone);
    xAttributeGrammar.AddProduction('P^->', xH, []);
    Epsilon();

    // *** E -> QE^ | &E | Epsilon
    xH := TSymbolNonTerminal.Create('E', clNone);
    xB1 := TSymbolNonTerminal.Create('Q', clLeft);
    xB2 := TSymbolNonTerminal.Create('E^', clRight);
    xAttributeGrammar.AddProduction('E->QE^', xH, [xB1, xB2]);
    DoubleTerminal();

    xH := TSymbolNonTerminal.Create('E^', clNone);
    xB1 := TSymbolTerminal.Create(ttConcat, clLeft);
    xB2 := TSymbolNonTerminal.Create('E', clRight);
    xAttributeGrammar.AddProduction('E^->&E', xH, [xB1, xB2]);
    BinaryOp();

    xH := TSymbolNonTerminal.Create('E^', clNone);
    xAttributeGrammar.AddProduction('E^->', xH, []);
    Epsilon();

    // *** Q -> TQ^ | +Q | -Q | Epsilon
    xH := TSymbolNonTerminal.Create('Q', clNone);
    xB1 := TSymbolNonTerminal.Create('T', clLeft);
    xB2 := TSymbolNonTerminal.Create('Q^', clRight);
    xAttributeGrammar.AddProduction('Q->TQ^', xH, [xB1, xB2]);
    DoubleTerminal();

    xH := TSymbolNonTerminal.Create('Q^', clNone);
    xB1 := TSymbolTerminal.Create(ttPlus, clLeft);
    xB2 := TSymbolNonTerminal.Create('Q', clRight);
    xAttributeGrammar.AddProduction('Q^->+Q', xH, [xB1, xB2]);
    BinaryOp();

    xH := TSymbolNonTerminal.Create('Q^', clNone);
    xB1 := TSymbolTerminal.Create(ttMinus, clLeft);
    xB2 := TSymbolNonTerminal.Create('Q', clRight);
    xAttributeGrammar.AddProduction('Q^->-Q', xH, [xB1, xB2]);
    BinaryOp();

    xH := TSymbolNonTerminal.Create('Q^', clNone);
    xB1 := TSymbolTerminal.Create(ttComp, clLeft);
    xB2 := TSymbolNonTerminal.Create('Q', clRight);
    xAttributeGrammar.AddProduction('Q^->cQ', xH, [xB1, xB2]);
    BinaryOp();

    xH := TSymbolNonTerminal.Create('Q^', clNone);
    xB1 := TSymbolTerminal.Create(ttLogic, clLeft);
    xB2 := TSymbolNonTerminal.Create('Q', clRight);
    xAttributeGrammar.AddProduction('Q^->lQ', xH, [xB1, xB2]);
    BinaryOp();

    xH := TSymbolNonTerminal.Create('Q^', clNone);
    xAttributeGrammar.AddProduction('Q^->', xH, []);
    Epsilon();

    // *** T -> FT^ | *T | /T | Epsilon
    xH := TSymbolNonTerminal.Create('T', clNone);
    xB1 := TSymbolNonTerminal.Create('F', clLeft);
    xB2 := TSymbolNonTerminal.Create('T^', clRight);
    xAttributeGrammar.AddProduction('T->FT^', xH, [xB1, xB2]);
    DoubleTerminalF();

    xH := TSymbolNonTerminal.Create('T^', clNone);
    xB1 := TSymbolTerminal.Create(ttMultDiv, clLeft);
    xB2 := TSymbolNonTerminal.Create('T', clRight);
    xAttributeGrammar.AddProduction('T^->*T', xH, [xB1, xB2]);
    BinaryOp();

    xH := TSymbolNonTerminal.Create('T^', clNone);
    xAttributeGrammar.AddProduction('T^->', xH, []);
    Epsilon();

    // *** F -> k | v | (Q) | -Q | FuncQ)
    xH := TSymbolNonTerminal.Create('F', clNone);
    xB1 := TSymbolTerminal.Create(ttConst, clLeft);
    xAttributeGrammar.AddProduction('F->k', xH, [xB1]);
    Terminal();

    xH := TSymbolNonTerminal.Create('F', clNone);
    xB1 := TSymbolTerminal.Create(ttVar, clLeft);
    xAttributeGrammar.AddProduction('F->v', xH, [xB1]);
    Terminal();

    xH := TSymbolNonTerminal.Create('F', clNone);
    xB1 := TSymbolTerminal.Create(ttArrayVar, clLeft);
    xB2 := TSymbolTerminal.Create(ttSquareBracketOpen, clNone);
    xB3 := TSymbolNonTerminal.Create('E', clRight);
    xB4 := TSymbolTerminal.Create(ttSquareBracketClose, clNone);
    xAttributeGrammar.AddProduction('F->v[E]', xH, [xB1, xB2, xB3, xB4]);
    ArrayVar();

    xH := TSymbolNonTerminal.Create('F', clNone);
    xB1 := TSymbolTerminal.Create(ttBracketOpen, clNone);
    xB2 := TSymbolNonTerminal.Create('Q', clLeft);
    xB3 := TSymbolTerminal.Create(ttBracketClose, clNone);
    xAttributeGrammar.AddProduction('F->(Q)', xH, [xB1, xB2, xB3]);
    xB2.AddAttr(TSimpleAttribute.Create(STR_ATTR_NAME_OPERAND1, TAttrRef.Create(alFirstChild,
        STR_ATTR_NAME_EVAL)));
    xB2.AddAttr(TSynthAttribute.Create(STR_ATTR_NAME_EVAL, TAttrRef.Create(alSelf, STR_ATTR_NAME_EVAL),
        TAttrRef.Create(alParent, STR_ATTR_NAME_EVAL)));

    xH := TSymbolNonTerminal.Create('F', clNone);
    xB1 := TSymbolTerminal.Create(ttFunc, clLeft);
    xB2 := TSymbolTerminal.Create(ttBracketOpen, clNone);
    xB3 := TSymbolNonTerminal.Create('P', clRight);
    xB4 := TSymbolTerminal.Create(ttBracketClose, clNone);
    xAttributeGrammar.AddProduction('F->Func(P)', xH, [xB1, xB2, xB3, xB4]);
    Func();

    xH := TSymbolNonTerminal.Create('F', clNone);
    xB1 := TSymbolTerminal.Create(ttMinus, clLeft);
    xB2 := TSymbolNonTerminal.Create('F', clRight);
    xAttributeGrammar.AddProduction('F->-F', xH, [xB1, xB2]);
    UnaryOp();

    result := xAttributeGrammar;

end;

class function TParserInstances.CreateParseTable(): TParseTable;
var
    xAttributeGrammar: TAttributeGrammar;
    xParseTable: TParseTable;
begin
    xAttributeGrammar := CreateAttributeGrammar();
    xParseTable := TParseTable.Create(TSymbolNonTerminal.Create('S', clNone), xAttributeGrammar);
    xParseTable.AddProduction('S', ttConst, 'S->E');
    xParseTable.AddProduction('S', ttVar, 'S->E');
    xParseTable.AddProduction('S', ttArrayVar, 'S->E');
    xParseTable.AddProduction('S', ttFunc, 'S->E');
    xParseTable.AddProduction('S', ttMinus, 'S->E');
    xParseTable.AddProduction('S', ttAssign, 'S->:=E');
    xParseTable.AddProduction('S', ttFlowBegin, 'S->whileQ');
    xParseTable.AddProduction('S', ttFlowEnd, 'S->endwhile');
    xParseTable.AddProduction('S', ttBracketOpen, 'S->E');

    xParseTable.AddProduction('P', ttBracketOpen, 'P->EP^');
    xParseTable.AddProduction('P', ttConst, 'P->EP^');
    xParseTable.AddProduction('P', ttVar, 'P->EP^');
    xParseTable.AddProduction('P', ttArrayVar, 'P->EP^');
    xParseTable.AddProduction('P', ttMinus, 'P->EP^');
    xParseTable.AddProduction('P', ttFunc, 'P->EP^');

    xParseTable.AddProduction('P^', ttFuncParDelim, 'P^->,P');
    xParseTable.AddProduction('P^', ttBracketClose, 'P^->');
    xParseTable.AddProduction('P^', ttEpsilon, 'P^->');

    xParseTable.AddProduction('E', ttBracketOpen, 'E->QE^');
    xParseTable.AddProduction('E', ttSquareBracketOpen, 'E->QE^');
    xParseTable.AddProduction('E', ttConst, 'E->QE^');
    xParseTable.AddProduction('E', ttVar, 'E->QE^');
    xParseTable.AddProduction('E', ttArrayVar, 'E->QE^');
    xParseTable.AddProduction('E', ttMinus, 'E->QE^');
    xParseTable.AddProduction('E', ttFunc, 'E->QE^');

    xParseTable.AddProduction('E^', ttConcat, 'E^->&E');
    xParseTable.AddProduction('E^', ttBracketClose, 'E^->');
    xParseTable.AddProduction('E^', ttSquareBracketClose, 'E^->');
    xParseTable.AddProduction('E^', ttFuncParDelim, 'E^->');
    xParseTable.AddProduction('E^', ttEpsilon, 'E^->');

    xParseTable.AddProduction('Q', ttBracketOpen, 'Q->TQ^');
    xParseTable.AddProduction('Q', ttSquareBracketOpen, 'Q->TQ^');
    xParseTable.AddProduction('Q', ttConst, 'Q->TQ^');
    xParseTable.AddProduction('Q', ttVar, 'Q->TQ^');
    xParseTable.AddProduction('Q', ttArrayVar, 'Q->TQ^');
    xParseTable.AddProduction('Q', ttMinus, 'Q->TQ^');
    xParseTable.AddProduction('Q', ttFunc, 'Q->TQ^');
    xParseTable.AddProduction('Q', ttComp, 'Q->TQ^');

    xParseTable.AddProduction('Q^', ttPlus, 'Q^->+Q');
    xParseTable.AddProduction('Q^', ttMinus, 'Q^->-Q');
    xParseTable.AddProduction('Q^', ttComp, 'Q^->cQ');
    xParseTable.AddProduction('Q^', ttLogic, 'Q^->lQ');
    xParseTable.AddProduction('Q^', ttBracketClose, 'Q^->');
    xParseTable.AddProduction('Q^', ttSquareBracketClose, 'Q^->');
    xParseTable.AddProduction('Q^', ttEpsilon, 'Q^->');
    xParseTable.AddProduction('Q^', ttFuncParDelim, 'Q^->');
    xParseTable.AddProduction('Q^', ttConcat, 'Q^->');

    xParseTable.AddProduction('T', ttBracketOpen, 'T->FT^');
    xParseTable.AddProduction('T', ttSquareBracketOpen, 'T->FT^');
    xParseTable.AddProduction('T', ttConst, 'T->FT^');
    xParseTable.AddProduction('T', ttVar, 'T->FT^');
    xParseTable.AddProduction('T', ttArrayVar, 'T->FT^');
    xParseTable.AddProduction('T', ttMinus, 'T->FT^');
    xParseTable.AddProduction('T', ttFunc, 'T->FT^');

    xParseTable.AddProduction('T^', ttMultDiv, 'T^->*T');
    xParseTable.AddProduction('T^', ttBracketClose, 'T^->');
    xParseTable.AddProduction('T^', ttSquareBracketClose, 'T^->');
    xParseTable.AddProduction('T^', ttEpsilon, 'T^->');
    xParseTable.AddProduction('T^', ttComp, 'T^->');
    xParseTable.AddProduction('T^', ttFuncParDelim, 'T^->');
    xParseTable.AddProduction('T^', ttConcat, 'T^->');
    xParseTable.AddProduction('T^', ttPlus, 'T^->');
    xParseTable.AddProduction('T^', ttMinus, 'T^->');
    xParseTable.AddProduction('T^', ttLogic, 'T^->');

    xParseTable.AddProduction('F', ttConst, 'F->k');
    xParseTable.AddProduction('F', ttVar, 'F->v');
    xParseTable.AddProduction('F', ttArrayVar, 'F->v[E]');
    xParseTable.AddProduction('F', ttBracketOpen, 'F->(Q)');
    xParseTable.AddProduction('F', ttMinus, 'F->-F');
    xParseTable.AddProduction('F', ttFunc, 'F->Func(P)');

    EXIT(xParseTable);
end;


initialization


finalization


TParserInstances.DestroyInstances;


end.
