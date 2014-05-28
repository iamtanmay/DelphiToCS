{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  : The parse tree is built using the rules of the attribute grammar.
  The attribute grammar will also give the parse tree attributes which tell the tree how the nodes of
  the tree are related to oneanother.
  The purpose of the parse tree is to build an AST(Abstract Syntax Tree).
  The AST is built in two passes: first pass for inherited attributes, second pass for synthetic attributes
  Once the AST is built the tree will have a pointer to the MainEvalNode.
  Calling MainEvalNode.Evaluate should first evaluate the results of the child AST nodes (or operands) and then
  evaluate the node itself using the results of the child AST nodes
  The point is that we can call MainEvalNode.Evaluate many times and get different evaluation results based on the
  current values in the SymbolTable, without having to parse the input string from scratch everytime
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.06.05 pk                               TN2449   Initial revision
  25.08.05 pk                               TN2573   Support for Logical AND, logical OR, Rand(-1) and Rand( x )
  27.10.06 wl  TCallDllFuncEvalNode                  TN3331   Neu: ruft eine beliebige DLL-Funktion auf
  27.10.06 wl  TParseTreeNode.GetUnaryFuncAttrVal    TN3331   erzeugt auch TCallDllFuncEvalNode
  27.10.06 wl  pftImport,SQLUpdate,SQLWrite,FileCopy TN3331   weitere Funktionen vorbereitet
  21.02.07 pk                                        TN3583   Various changes to prevent memory leaks
  01.08.07 pk  pftSubtrFunc, PosFunc, NowFunc...     TN3814   New functions
  27.08.07 pk                                        TN3788   Various Changes
  06.11.08 pk  TParserResult                         TN4279   fValue removed
  04.11.09 pk                               	    TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  01.03.12 wl                                       TN5822   TArg statt TAttrValue
  21.03.13 wl                                       TN6045   verwendet Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit ParserTree;


interface


uses
    Generics.Collections,
    GeneralTypes,
    ListClasses,
    ParserToken,
    ParserAttribute,
    ParserGrammar,
    ParserIdentifier,
    ParserIdentDataType,
    ParserEvalNode,
    ParserEvalTable;

type
    TParseFuncType = (pftUnknown, pftFuncParDelim, pftConcat, pftPlus, pftMinus, pftMult, pftDiv, pftMod,
        pftIntDiv, pftAssign, pftLT, pftLTE, pftE, pftNE, pftGTE, pftGT, pftSubstrFunc, pftPosFunc,
        pftUpperFunc, pftNowFunc, pftLenFunc, pftTrimFunc, pftRevstrFunc, pftPadFunc, pftXModFunc,
        pftXDivFunc, pftAbsFunc, pftRoundFunc, pftTruncFunc, pftRandomFunc, pftCallDllFunc, pftImportFunc,
        pftSQLUpdateFunc, pftSQLSelectFunc, pftFileCopyFunc, pftAnd, pftOr);

    // ******* Parse Tree objects
    TValuedAttribute = class
    protected
        fAttribute: TSymbolAttribute;
        fValue: TEvalNode;
    public
        constructor Create(aAttribute: TSymbolAttribute);
        property Value: TEvalNode read fValue write fValue;
        property Attr: TSymbolAttribute read fAttribute;
    end;

    TValuedAttributeList = class(TStringKeyObjectValueList)
    public
        constructor Create();
        function AttrByName(const aAttrName: string): TValuedAttribute;
        function AttrBySymbolAttrClass(const aSymbolAttrClass: TSymbolAttributeClass): TValuedAttribute;
    end;

    TParseCondResult = (crNone, crTrue, crFalse);

    TParseResult = class
    public
        function Evaluate(const aEvalTable: TParserEvalTable): TArg; virtual; abstract;
    end;

    TParseEvalResult = class(TParseResult)
    private
        fMainEvalNode: TEvalNode;
        fContainsVar: boolean;
        fEvalNodes: TObjectList<TEvalNode>;
        function GetEvalOnce(): boolean;

    public
        constructor Create();
        destructor Destroy; override;
        procedure AddEvalNode(aEvalNode: TEvalNode);
        function Evaluate(const aEvalTable: TParserEvalTable): TArg; override;
        property EvalOnce: boolean read GetEvalOnce;
        property ContainsVar: boolean read fContainsVar write fContainsVar;
        property MainEvalNode: TEvalNode read fMainEvalNode write fMainEvalNode;
    end;

    TParseTreeEvalProperties = class
    protected
        fParseResult: TParseEvalResult;
    public
        property ParseResult: TParseEvalResult read fParseResult write fParseResult;
    end;

    TParseTreeProperties = class
    protected
        fEvalProps: TParseTreeEvalProperties;
    public
        constructor Create();
        property EvalProps: TParseTreeEvalProperties read fEvalProps write fEvalProps;
    end;

    TParseTreeNode = class
    private
        fParentNode: TParseTreeNode;
        fAttributes: TValuedAttributeList;
        fTreeProps: TParseTreeProperties;
        function GetAttrVal(const aAttrName: string): TEvalNode;
        function GetRelativeAttrVal(aAttrRef: TAttrRef): TEvalNode;
        function GetUnaryFuncAttrVal(aAttr: TUnaryFuncAttribute): TEvalNode;
        function GetBinaryFuncAttrVal(aAttr: TBinaryFuncAttribute): TEvalNode;

        function GetTerminalAttrVal(const aAttrName: string): TEvalNode; virtual;
        function GetRelativeAttr(aAttrRef: TAttrRef): TValuedAttribute;
        procedure AddAttributes(aAttributes: TSymbolAttributeList);
    public
        constructor Create(aTreeProps: TParseTreeProperties);
        destructor Destroy(); override;
        function TreeNodeToStr(): string; virtual; abstract;
        function GetRelativeNode(aLocation: TAttrLocation): TParseTreeNode; virtual;
        procedure DetermineAttrVal(aAttr: TValuedAttribute; aSynthesized: boolean); virtual;
        procedure DetermineSynthAttr(aAttr: TSynthAttribute);
        procedure PassAttributes(const aAttrNames, aSynthAttrNames: array of string); virtual; abstract;
        procedure Pass(const aAttrNames: array of string; aSynthesized: boolean); virtual;
        procedure AddSymbolAttributes(aSymbol: TSymbol);

        function ContainsVar: boolean; virtual; abstract;
        property ParentNode: TParseTreeNode read fParentNode write fParentNode;
        property Attributes: TValuedAttributeList read fAttributes;
        property TreeProps: TParseTreeProperties read fTreeProps;
    end;

    TTerminalParseTreeNode = class(TParseTreeNode)
    protected
        fToken: TToken;
        fTerminal: TSymbolTerminal;
        function GetTerminalAttrVal(const aAttrName: string): TEvalNode; override;
    public
        constructor Create(aTreeProps: TParseTreeProperties; aTerminal: TSymbolTerminal; aToken: TToken);
        destructor Destroy(); override;
        procedure PassAttributes(const aAttrNames, aSynthAttrNames: array of string); override;
        function TreeNodeToStr(): string; override;
        function ContainsVar: boolean; override;
        property Token: TToken write fToken;
    end;

    TNonTerminalParseTreeNode = class(TParseTreeNode)
    protected
        fNonTerminal: TSymbolNonTerminal;
        fLeft, fRight: TParseTreeNode;
    public
        constructor Create(aTreeProps: TParseTreeProperties; aNonTerminal: TSymbolNonTerminal);
        destructor Destroy; override;
        function TreeNodeToStr(): string; override;
        procedure PassAttributes(const aAttrNames, aSynthAttrNames: array of string); override;
        function GetRelativeNode(aLocation: TAttrLocation): TParseTreeNode; override;

        function ContainsVar: boolean; override;
        property Left: TParseTreeNode read fLeft write fLeft;
        property Right: TParseTreeNode read fRight write fRight;
    end;

    TParseTree = class
    protected
        fRoot: TParseTreeNode;
        fScope: string;
        fTokenList: TTokenList;
        fNodeList: TObjectList<TParseTreeNode>;
        fTreeProps: TParseTreeProperties;
        procedure PassAttributes();
        procedure BuildAST();
    public
        constructor Create(aTokenList: TTokenList);
        destructor Destroy(); override;
        procedure AddRootNodeFromStartSymbol(aStartSymbol: TSymbolNonTerminal);
        function AddNodeFromSymbol(aSymbol: TSymbol; aParentNode: TParseTreeNode): TParseTreeNode;
        procedure DetermineParseResult();
        procedure BuildEvalResult(aParseResult: TParseEvalResult);
        procedure SetChildAt(aParent: TNonTerminalParseTreeNode; aChild: TParseTreeNode;
            aLocation: TSymbolChildLocation);
        procedure DetermineConstainsVar();
        // property NodeList: TObjectList<TParseTreeNode> read fNodeList;
        property Root: TParseTreeNode read fRoot write fRoot;
        property TokenList: TTokenList read fTokenList;
        property TreeProps: TParseTreeProperties read fTreeProps;
    end;


implementation


uses
    SysUtils,
    StrUtils;

{ TValuedAttributeList }

constructor TValuedAttributeList.Create();
begin
    inherited Create(true);
end;

function TValuedAttributeList.AttrByName(const aAttrName: string): TValuedAttribute;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := self.IndexOf(aAttrName);
    if xIndex < 0 then
        EXIT;
    result := self.Objects[xIndex] as TValuedAttribute;
end;

function TValuedAttributeList.AttrBySymbolAttrClass(const aSymbolAttrClass: TSymbolAttributeClass)
    : TValuedAttribute;
var
    x: integer;
    xAttr: TValuedAttribute;
begin
    result := nil;
    for x := 0 to self.Count - 1 do
    begin
        xAttr := self.Objects[x] as TValuedAttribute;
        if xAttr.Attr is aSymbolAttrClass then
        begin
            result := xAttr;
            EXIT;
        end;
    end;
end;

{ TParseTreeProperties }

constructor TParseTreeProperties.Create();
begin
    inherited Create();
    fEvalProps := TParseTreeEvalProperties.Create();
end;

{ TParseTreeNode }

constructor TParseTreeNode.Create(aTreeProps: TParseTreeProperties);
begin
    inherited Create;
    fTreeProps := aTreeProps;
    fAttributes := TValuedAttributeList.Create();
end;

destructor TParseTreeNode.Destroy();
begin
    FreeAndNil(fAttributes);
    inherited;
end;

procedure TParseTreeNode.AddAttributes(aAttributes: TSymbolAttributeList);
var
    xAttribute: TSymbolAttribute;
    i: integer;
begin
    if not Assigned(aAttributes) then
        EXIT;
    for i := 0 to aAttributes.Count - 1 do
    begin
        xAttribute := aAttributes.Objects[i] as TSymbolAttribute;
        fAttributes.AddObject(xAttribute.AttributeName, TValuedAttribute.Create(xAttribute));
    end;
end;

procedure TParseTreeNode.AddSymbolAttributes(aSymbol: TSymbol);
begin
    AddAttributes(aSymbol.Attributes);
    AddAttributes(aSymbol.SynthAttributes);
end;

function TParseTreeNode.GetRelativeNode(aLocation: TAttrLocation): TParseTreeNode;
begin
    case aLocation of
        alParent:
            result := self.ParentNode;
        alSelf:
            result := self;
        alFirstSibling:
            begin
                ASSERT(self.ParentNode is TNonTerminalParseTreeNode);
                result := (self.ParentNode as TNonTerminalParseTreeNode).Left as TParseTreeNode;
            end;
        alParentFirstSibling:
            result := self.ParentNode.GetRelativeNode(alFirstSibling);
        else
            result := nil;
    end;
end;

function TParseTreeNode.GetAttrVal(const aAttrName: string): TEvalNode;
var
    xAttr: TValuedAttribute;
begin
    xAttr := fAttributes.AttrByName(aAttrName);
    ASSERT(Assigned(xAttr), Format('Attribute %s not found', [aAttrName]));
    result := xAttr.Value;
end;

function TParseTreeNode.GetRelativeAttrVal(aAttrRef: TAttrRef): TEvalNode;
begin
    result := GetRelativeAttr(aAttrRef).Value
end;

function TParseTreeNode.GetRelativeAttr(aAttrRef: TAttrRef): TValuedAttribute;
var
    xRelativeNode: TParseTreeNode;
begin
    xRelativeNode := GetRelativeNode(aAttrRef.Location);
    ASSERT(Assigned(xRelativeNode), Format('No RelativeNode %s', [aAttrRef.UseAttrName]));
    result := xRelativeNode.Attributes.AttrByName(aAttrRef.UseAttrName);
    ASSERT(Assigned(result), Format('Attribute %s not found', [aAttrRef.UseAttrName]));
end;

function TParseTreeNode.GetUnaryFuncAttrVal(aAttr: TUnaryFuncAttribute): TEvalNode;
var
    xOperand, xOperator: TEvalNode;
begin
    xOperand := GetRelativeAttrVal(aAttr.Operand1);
    xOperator := GetRelativeAttrVal(aAttr.Func);
    ASSERT(xOperator is TUnaryOperatorEvalNode);
    (xOperator as TUnaryOperatorEvalNode).Operand := xOperand;
    result := xOperator;
end;

function TParseTreeNode.GetBinaryFuncAttrVal(aAttr: TBinaryFuncAttribute): TEvalNode;
var
    xOperand1, xOperand2, xOperator: TEvalNode;
begin
    xOperand1 := GetRelativeAttrVal(aAttr.Operand1);
    xOperand2 := GetRelativeAttrVal(aAttr.Operand2);
    xOperator := GetRelativeAttrVal(aAttr.Func);
    ASSERT(xOperator is TBinaryOperatorEvalNode);
    (xOperator as TBinaryOperatorEvalNode).Operand1 := xOperand1;
    (xOperator as TBinaryOperatorEvalNode).Operand2 := xOperand2;
    result := xOperator;
end;

function TParseTreeNode.GetTerminalAttrVal(const aAttrName: string): TEvalNode;
begin
    result := nil;
end;

procedure TParseTreeNode.DetermineSynthAttr(aAttr: TSynthAttribute);
var
    xTarget, xSource: TValuedAttribute;
begin
    xTarget := GetRelativeAttr(aAttr.TargetRef);
    xSource := GetRelativeAttr(aAttr.SourceRef);
    xTarget.Value := xSource.Value;
end;

procedure TParseTreeNode.DetermineAttrVal(aAttr: TValuedAttribute; aSynthesized: boolean);
begin
    try
        if aSynthesized then
        begin
            if aAttr.Attr is TBinaryFuncSynthAttribute then
            begin
                GetRelativeAttr((aAttr.Attr as TBinaryFuncSynthAttribute).TargetRef).Value :=
                    GetBinaryFuncAttrVal((aAttr.Attr as TBinaryFuncSynthAttribute));
            end
            else if aAttr.Attr is TUnaryFuncSynthAttribute then
            begin
                GetRelativeAttr((aAttr.Attr as TUnaryFuncSynthAttribute).TargetRef).Value :=
                    GetUnaryFuncAttrVal((aAttr.Attr as TUnaryFuncSynthAttribute));
            end

            else if aAttr.Attr is TSynthAttribute then
                DetermineSynthAttr(aAttr.Attr as TSynthAttribute);
        end
        else
        begin
            if aAttr is TValuedAttribute then
            begin
                ASSERT(not Assigned(aAttr.Value));
            end;
            if aAttr.Attr is TTerminalAttribute then
            begin
                // aAttr.Value );    // only free when calculating terminalattribute value
                aAttr.Value := GetTerminalAttrVal(aAttr.Attr.AttributeName);
            end
            else if aAttr.Attr is TSimpleAttribute then
            begin
                aAttr.Value := GetRelativeAttrVal((aAttr.Attr as TSimpleAttribute).Ref);
            end
            else if aAttr.Attr is TBinaryFuncInhAttribute then
            begin
                aAttr.Value := GetBinaryFuncAttrVal((aAttr.Attr as TBinaryFuncInhAttribute));
            end

        end;

    except
        on E: Exception do
        begin
            raise Exception.Create(#13#10 + aAttr.Attr.AttrToStr + #13#10 + E.Message);
        end;
    end;
end;

procedure TParseTreeNode.Pass(const aAttrNames: array of string; aSynthesized: boolean);
var
    i: integer;
    xAttr: TValuedAttribute;
begin
    for i := 0 to high(aAttrNames) do
    begin
        xAttr := fAttributes.AttrByName(aAttrNames[i]);
        if not Assigned(xAttr) then
            CONTINUE;
        DetermineAttrVal(xAttr, aSynthesized);
    end;
end;

{ TTerminalParseTreeNode }

constructor TTerminalParseTreeNode.Create(aTreeProps: TParseTreeProperties; aTerminal: TSymbolTerminal;
    aToken: TToken);
begin
    inherited Create(aTreeProps);
    fTerminal := aTerminal;
    fToken := aToken;
end;

destructor TTerminalParseTreeNode.Destroy();
begin
    inherited;
end;

procedure TTerminalParseTreeNode.PassAttributes(const aAttrNames, aSynthAttrNames: array of string);
begin
    Pass(aAttrNames, false);
    Pass(aSynthAttrNames, true);
end;

function TTerminalParseTreeNode.TreeNodeToStr(): string;
begin
    result := 'Terminal Node: ' + fToken.TokenToStr();
end;

function TTerminalParseTreeNode.ContainsVar: boolean;
begin
    result := fToken.TokenType in [ttVar, ttAssign];
end;

function TTerminalParseTreeNode.GetTerminalAttrVal(const aAttrName: string): TEvalNode;
var
    xAttr: TValuedAttribute;
begin
    xAttr := self.fAttributes.AttrBySymbolAttrClass(TTerminalAttribute);
    Assert(Assigned(xAttr));

    result := (xAttr.Attr as TTerminalAttribute).OnCreateEvalNode(fToken);
    if Assigned(result) then
        fTreeProps.EvalProps.ParseResult.AddEvalNode(result);
end;

{ TNonTerminalParseTreeNode }

constructor TNonTerminalParseTreeNode.Create(aTreeProps: TParseTreeProperties;
    aNonTerminal: TSymbolNonTerminal);
begin
    inherited Create(aTreeProps);
    fNonTerminal := aNonTerminal;
    fLeft := nil;
    fRight := nil;
end;

destructor TNonTerminalParseTreeNode.Destroy;
begin
    inherited;
end;

function TNonTerminalParseTreeNode.TreeNodeToStr(): string;
begin
    result := 'Nonterminal Node: ' + fNonTerminal.KeyName;
end;

function TNonTerminalParseTreeNode.GetRelativeNode(aLocation: TAttrLocation): TParseTreeNode;
begin
    case aLocation of
        alFirstChild:
            begin
                ASSERT(Assigned(fLeft));
                result := fLeft;
            end;
        alLastChild:
            begin
                ASSERT(Assigned(fRight));
                result := fRight;
            end;
        else
            result := inherited GetRelativeNode(aLocation);
    end;
end;

procedure TNonTerminalParseTreeNode.PassAttributes(const aAttrNames, aSynthAttrNames: array of string);
begin
    try
        if Assigned(fLeft) then
            fLeft.PassAttributes(aAttrNames, aSynthAttrNames);

        Pass(aAttrNames, false);

        if Assigned(fRight) then
            fRight.PassAttributes(aAttrNames, aSynthAttrNames);

        Pass(aSynthAttrNames, true);
    except
        on E: Exception do
        begin
            raise Exception.Create(#13#10 + self.TreeNodeToStr + E.Message);
        end;
    end;
end;

function TNonTerminalParseTreeNode.ContainsVar: boolean;
begin
    result := false;
    if Assigned(fLeft) then
        result := fLeft.ContainsVar();

    if result then
        EXIT;
    if Assigned(fRight) then
        result := fRight.ContainsVar();
end;

{ TParseTree }

constructor TParseTree.Create(aTokenList: TTokenList);
begin
    inherited Create;
    fTokenList := aTokenList;
    fNodeList := TObjectList<TParseTreeNode>.Create();
    fRoot := nil;
    fTreeProps := TParseTreeProperties.Create();
end;

destructor TParseTree.Destroy();
begin
    FreeAndNil(fNodeList);
    FreeAndNil(fRoot);
    inherited;
end;

procedure TParseTree.AddRootNodeFromStartSymbol(aStartSymbol: TSymbolNonTerminal);
begin
    fRoot := TNonTerminalParseTreeNode.Create(fTreeProps, aStartSymbol);
end;

function TParseTree.AddNodeFromSymbol(aSymbol: TSymbol; aParentNode: TParseTreeNode): TParseTreeNode;
begin
    if aSymbol is TSymbolTerminal then
        result := TTerminalParseTreeNode.Create(fTreeProps, aSymbol as TSymbolTerminal, nil)
    else
        result := TNonTerminalParseTreeNode.Create(fTreeProps, aSymbol as TSymbolNonTerminal);

    fNodeList.Add(result); // collect all nodes in this list so that we can destroy them later

    if aSymbol.ChildLocation <> clNone then
    begin
        result.AddSymbolAttributes(aSymbol);

        ASSERT(aParentNode is TNonTerminalParseTreeNode);
        SetChildAt(aParentNode as TNonTerminalParseTreeNode, result, aSymbol.ChildLocation);
    end;
end;

procedure TParseTree.SetChildAt(aParent: TNonTerminalParseTreeNode; aChild: TParseTreeNode;
    aLocation: TSymbolChildLocation);
begin
    if aLocation = clLeft then
        aParent.Left := aChild
    else if aLocation = clRight then
        aParent.Right := aChild
    else
        ASSERT(false);

    aChild.ParentNode := aParent;
end;

procedure TParseTree.DetermineConstainsVar();
begin
    ASSERT(Assigned(fRoot));
    fTreeProps.EvalProps.ParseResult.ContainsVar := fRoot.ContainsVar;
end;

procedure TParseTree.PassAttributes();

begin
    fRoot.PassAttributes([STR_ATTR_NAME_SCANVAL, STR_ATTR_NAME_OPERAND1], [STR_ATTR_NAME_EVAL]);
end;

procedure TParseTree.BuildAST();
begin
    PassAttributes();
end;

procedure TParseTree.DetermineParseResult();
begin
    BuildAST();
    fTreeProps.EvalProps.ParseResult.MainEvalNode := fRoot.GetAttrVal(STR_ATTR_NAME_EVAL);
end;

procedure TParseTree.BuildEvalResult(aParseResult: TParseEvalResult);
begin
    with fTreeProps.EvalProps do
    begin
        ParseResult := aParseResult;
    end;
    DetermineConstainsVar();
    DetermineParseResult();
end;

{ TValuedAttribute }

constructor TValuedAttribute.Create(aAttribute: TSymbolAttribute);
begin
    inherited Create;
    fAttribute := aAttribute;
    fValue := nil;
end;

{ TParseEvalResult }

constructor TParseEvalResult.Create();
begin
    inherited Create;
    fContainsVar := false;
    fMainEvalNode := nil;
    fEvalNodes := TObjectList<TEvalNode>.Create();
end;

destructor TParseEvalResult.Destroy();
begin
    FreeAndNil(fEvalNodes);
    inherited;
end;

function TParseEvalResult.GetEvalOnce(): boolean;
begin
    result := not fContainsVar;
end;

procedure TParseEvalResult.AddEvalNode(aEvalNode: TEvalNode);
begin
    fEvalNodes.Add(aEvalNode);
end;

function TParseEvalResult.Evaluate(const aEvalTable: TParserEvalTable): TArg;
begin
    EXIT(fMainEvalNode.Evaluate(aEvalTable));
end;


end.
