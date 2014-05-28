{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  : An attribute contains information on how the nodes in the parse tree relate to oneanother and
  how the AST (Abstract Syntax Tree) should be created.
  An Inherited Attribute always passes its values down the tree.
  A  Synthetic Attribute always passes its values up the tree.
  In the parse tree there should be two passes when building the AST.
  In the first pass all Inherited attributes should
  be resolved.
  In the second pass all Snythetic attributes should be resolved.
  The synthetic attributes will usually use the results of the first pass, that's why we need two passes.
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.08.07 pk                               TN3788   Initial Revision
  -------------------------------------------------------------------------------------------------- }

unit ParserAttribute;


interface


uses
    ParserToken,
    ParserEvalNode;

type
    TAttrLocation = (alParent, alSelf, alFirstChild, alLastChild, alFirstSibling, alParentFirstSibling);

    TAttrRef = class
    protected
        fLocation: TAttrLocation;
        fUseAttrName: string;
    public
        constructor Create(aLocation: TAttrLocation; const aUseAttrName: string);
        function AttrLocationToStr(): string;
        function AttrRefToStr(): string;

        property Location: TAttrLocation read fLocation;
        property UseAttrName: string read fUseAttrName;
    end;

    TSymbolAttributeClass = class of TSymbolAttribute;

    TSymbolAttribute = class
    protected
        fAttributeName: string;
    public
        constructor Create(const aAttrName: string);
        function AttrNameToStr(): string;
        function AttrToStr(): string; virtual;
        property AttributeName: string read fAttributeName;
    end;

    TTermAttribute = class(TSymbolAttribute)
    public
        function AttrToStr(): string; override;
    end;

    TOnParserCreateEvalNode = function(aToken: TToken): TEvalNode of object;

    TTerminalAttribute = class(TTermAttribute)
    private
        fOnCreateEvalNode: TOnParserCreateEvalNode;
    public
        constructor Create(const aAttrName: string; aOnCreateEvalNode: TOnParserCreateEvalNode);
        property OnCreateEvalNode: TOnParserCreateEvalNode read fOnCreateEvalNode;
    end;

    TSimpleAttribute = class(TSymbolAttribute)
    protected
        fRef: TAttrRef;
    public
        constructor Create(const aAttrName: string; aAttrRef: TAttrRef);
        destructor Destroy(); override;
        function AttrToStr(): string; override;
        property Ref: TAttrRef read fRef;
    end;

    TTerminalSynthAttribute = class(TTermAttribute);

    TSynthAttribute = class(TSymbolAttribute)
    private
        fSourceRef: TAttrRef;
        fTargetRef: TAttrRef;
    public
        constructor Create(const aAttrName: string; aSourceRef: TAttrRef; aTargetRef: TAttrRef);
        destructor Destroy(); override;
        function AttrToStr(): string; override;
        property SourceRef: TAttrRef read fSourceRef write fSourceRef;
        property TargetRef: TAttrRef read fTargetRef write fTargetRef;
    end;

    TUnaryFuncAttribute = class(TSymbolAttribute)
    protected
        fOperand1: TAttrRef;
        fFunc: TAttrRef;
    public
        constructor Create(const aAttrName: string; aOperand1, aFunc: TAttrRef);
        destructor Destroy(); override;
        function AttrToStr(): string; override;
        property Operand1: TAttrRef read fOperand1;
        property Func: TAttrRef read fFunc;
    end;

    TUnaryFuncSynthAttribute = class(TUnaryFuncAttribute)
    protected
        fTargetRef: TAttrRef;
    public
        constructor Create(const aAttrName: string; aSourceOperand1, aSourceFunc, aTargetRef: TAttrRef);
        destructor Destroy(); override;
        property TargetRef: TAttrRef read fTargetRef;
    end;

    TBinaryFuncAttribute = class(TUnaryFuncAttribute)
    protected
        fOperand2: TAttrRef;
    public
        constructor Create(const aAttrName: string; aOperand1, aOperand2, aFunc: TAttrRef);
        destructor Destroy(); override;
        function AttrToStr(): string; override;
        property Operand2: TAttrRef read fOperand2;
    end;

    TBinaryFuncInhAttribute = class(TBinaryFuncAttribute);

    TBinaryFuncSynthAttribute = class(TBinaryFuncAttribute)
    protected
        fTargetRef: TAttrRef;
    public
        constructor Create(const aAttrName: string; aSourceOperand1, aSourceOperand2, aSourceFunc,
            aTargetRef: TAttrRef);
        destructor Destroy(); override;
        property TargetRef: TAttrRef read fTargetRef;
    end;


implementation


uses
    SysUtils;

constructor TAttrRef.Create(aLocation: TAttrLocation; const aUseAttrName: string);
begin
    inherited Create;
    fLocation := aLocation;
    fUseAttrName := aUseAttrName;
end;

function TAttrRef.AttrLocationToStr(): string;
begin
    case fLocation of
        alParent:
            result := 'Parent';
        alSelf:
            result := 'Self';
        alFirstChild:
            result := 'FirstChild';
        alLastChild:
            result := 'LastChild';
        alFirstSibling:
            result := 'FirstSibling';
        alParentFirstSibling:
            result := 'ParentFirstSibling';
        else
            result := '';
    end;
end;

function TAttrRef.AttrRefToStr(): string;
begin
    result := Format('%s.%s', [AttrLocationToStr(), fUseAttrName]);
end;

constructor TSymbolAttribute.Create(const aAttrName: string);
begin
    inherited Create();
    fAttributeName := aAttrName;
end;

function TSymbolAttribute.AttrNameToStr(): string;
begin
    result := 'AttrName : ' + fAttributeName;
end;

function TSymbolAttribute.AttrToStr(): string;
begin
    result := AttrNameToStr();
end;

constructor TSimpleAttribute.Create(const aAttrName: string; aAttrRef: TAttrRef);
begin
    inherited Create(aAttrName);
    fRef := aAttrRef;
end;

destructor TSimpleAttribute.Destroy();
begin
    fRef.Free;
    inherited;
end;

function TSimpleAttribute.AttrToStr(): string;
begin
    result := Format('Simple%s - Ref:%s', [AttrNameToStr(), fRef.AttrRefToStr()]);
end;

function TTermAttribute.AttrToStr(): string;
begin
    result := 'Terminal' + AttrNameToStr();
end;

constructor TSynthAttribute.Create(const aAttrName: string; aSourceRef: TAttrRef; aTargetRef: TAttrRef);
begin
    inherited Create(aAttrName);
    fSourceRef := aSourceRef;
    fTargetRef := aTargetRef;
end;

destructor TSynthAttribute.Destroy();
begin
    fSourceRef.Free;
    fTargetRef.Free;
    inherited;
end;

function TSynthAttribute.AttrToStr(): string;
begin
    result := Format('Synth%s - SourceRef:%s, TargetRef:%s', [AttrNameToStr(), fSourceRef.AttrRefToStr(),
        fTargetRef.AttrRefToStr()]);
end;

constructor TUnaryFuncAttribute.Create(const aAttrName: string; aOperand1, aFunc: TAttrRef);
begin
    inherited Create(aAttrName);
    fOperand1 := aOperand1;
    fFunc := aFunc;
end;

destructor TUnaryFuncAttribute.Destroy();
begin
    fOperand1.Free;
    fFunc.Free;
    inherited;
end;

function TUnaryFuncAttribute.AttrToStr(): string;
begin
    result := Format('UnaryFunc%s - Func:%s, Op:%s', [AttrNameToStr(), fFunc.AttrRefToStr(),
        fOperand1.AttrRefToStr()]);
end;

constructor TUnaryFuncSynthAttribute.Create(const aAttrName: string;
    aSourceOperand1, aSourceFunc, aTargetRef: TAttrRef);
begin
    inherited Create(aAttrName, aSourceOperand1, aSourceFunc);
    fTargetRef := aTargetRef;
end;

destructor TUnaryFuncSynthAttribute.Destroy();
begin
    fTargetRef.Free;
    inherited;
end;

constructor TBinaryFuncAttribute.Create(const aAttrName: string; aOperand1, aOperand2, aFunc: TAttrRef);
begin
    inherited Create(aAttrName, aOperand1, aFunc);
    fOperand2 := aOperand2;
end;

destructor TBinaryFuncAttribute.Destroy();
begin
    fOperand2.Free;
    inherited;
end;

constructor TBinaryFuncSynthAttribute.Create(const aAttrName: string;
    aSourceOperand1, aSourceOperand2, aSourceFunc, aTargetRef: TAttrRef);
begin
    inherited Create(aAttrName, aSourceOperand1, aSourceOperand2, aSourceFunc);
    fTargetRef := aTargetRef;
end;

destructor TBinaryFuncSynthAttribute.Destroy();
begin
    fTargetRef.Free;
    inherited;
end;

function TBinaryFuncAttribute.AttrToStr(): string;
begin
    result := Format('BinaryFunc%s - Func:%s, Op1:%s, Op2:%s', [AttrNameToStr(), fFunc.AttrRefToStr(),
        fOperand1.AttrRefToStr(), fOperand2.AttrRefToStr()]);
end;
{ TTerminalAttribute }

constructor TTerminalAttribute.Create(const aAttrName: string; aOnCreateEvalNode: TOnParserCreateEvalNode);
begin
    inherited Create(aAttrName);
    fOnCreateEvalNode := aOnCreateEvalNode;
end;


end.
