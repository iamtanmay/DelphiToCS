{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  : The purpose of the TEvalNode is to evaluate a result given the values in the SymbolTable
  This means, that the Evaluate() function can be called many times, and each time the current values
  in the SymbolTable will be used for the evaluation
  The EvalNodes are usually created by the ParseTree when buiding the AST(Abstract Syntax Tree)
  An EvalNode may have pointers to other evalnodes thereby forming a tree structure; In this case the
  child nodes (or operands) are evaluated before the EvalNode itself is evaluated
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.08.07 pk                               TN3788   Initial revision
  04.09.07 pk                               TN       uses ArgClass
  08.09.08 pk  fCaption                     TN4215   New
  06.11.08 pk  TEvalNode.Evaluate           TN4279   new EvalTable param
  06.11.08 pk  TEValNode                    TN4279   fValue removed
  17.02.09 pk  CopyAttrValue                TN4232   New
  17.02.09 pk                               TN4232   Various changes
  19.08.09 wl                               TN4227   es werden immer englische Einstellungen benutzt
  07.05.10 pk  TArrayVarOperatorEvalNode    TN5092   New
  07.05.10 pk                               TN5095   New boolean type
  14.12.10 wl  StrToDataValue               TN5411   DecimalSeparator soll immer '.' sein
  27.06.11 wl  CheckFuncParamType           TN5609   neu: IsArray
  27.06.11 wl                               TN5609   neu: TAttrValueUtils
  17.10.11 wl  TOperatorEvalNode.EQ         TN5717   von TLogicEvalNode hierher
  01.03.12 wl  TParseFuncParamType          TN5820   pfptChar entfernt
  01.03.12 wl  DataValueToStr               TN5820   funktioniert jetzt auch für BOOLEAN
  01.03.12 wl  TAttrValueUtils              TN5820   --> ParserIdentDataType
  01.03.12 wl                               TN5822   TArg statt TAttrValue
  05.07.12 wl  TOperatorEvalNode.AreEqual   TN5917   IstGleich-Methode ist jetzt toleranter bei Boolean-Typen
  -------------------------------------------------------------------------------------------------- }

unit ParserEvalNode;


interface


uses
    ParserEvalTable,
    ParserIdentDataType;

type
    TFuncParams = class(TArg)
    protected
        fArr: TArray<TArg>;
    public
        constructor Create(aOtherArgs: TArg; aAddedValue: TArg); reintroduce;
        property Arr: TArray<TArg>read fArr;
    end;

    TEvalNodeClass = class of TEvalNode;

    TEvalNode = class
    protected
        fCaption: string;
    public
        constructor Create(const aCaption: string);
        destructor Destroy(); override;
        function Evaluate(const aEvalTable: TParserEvalTable): TArg; virtual; abstract;
        property Caption: string read fCaption;
    end;

    TVarEvalNode = class(TEvalNode)
    protected
        fVarName: string;
    public
        constructor Create(const aCaption: string; const aVarName: string);
        function Evaluate(const aEvalTable: TParserEvalTable): TArg; override;
    end;

    TConstEvalNode = class(TEvalNode)
    protected
        fValue: TArg;
    public
        constructor Create(const aCaption: string; aConstVal: TArg);
        function Evaluate(const aEvalTable: TParserEvalTable): TArg; override;
    end;

    TParseFuncParamType = (pfptInt, pfptDbl, pfptNum, pfptStr, pfptBool, pfptRef, pfptArr);

    TOperatorEvalNode = class(TEvalNode)
    private
        function CheckFuncParamType(aParam: TArg; aExpectedType: TParseFuncParamType): boolean;
        function ExpectedParamsAsStr(const aExpectedParams: array of TParseFuncParamType): string;
        class function FuncParamTypeToStr(aType: TParseFuncParamType): string;
    protected
        class procedure CheckIntType(aOp: TArg; aOpNumber: char = #0);
        class procedure CheckIntTypeBoth(aOp1, aOp2: TArg);
        class procedure CheckNumType(aOp: TArg; aOpNumber: char = #0);
        class procedure CheckNumTypeBoth(aOp1, aOp2: TArg);
        class function GetBoolVal(aOp: TArg; aOpNumber: char = #0): boolean;

        class function IsDbl(aOp: TArg): boolean;
        class function IsInt(aOp: TArg): boolean;
        class function IsNum(aOp: TArg): boolean;
        class function IsStr(aOp: TArg): boolean;
        class function IsBool(aOp: TArg): boolean;
        class function IsRef(aOp: TArg): boolean;
        class function IsArray(aOp: TArg): boolean;

        class function AreEqual(aOp1, aOp2: TArg): boolean;

        procedure CheckFuncParamsType(aParams: TArg; const aExpectedParams: array of TParseFuncParamType);

    public
        destructor Destroy(); override;
    end;

    TUnaryOperatorEvalNodeClass = class of TUnaryOperatorEvalNode;

    TUnaryOperatorEvalNode = class(TOperatorEvalNode)
    protected
        fOperand: TEvalNode;
        procedure EvaluateOperand(const aEvalTable: TParserEvalTable; var vEvaluatedOperand: TArg); virtual;
        procedure CheckOperand(const aEvaluatedOperand: TArg); virtual;
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg;
            virtual; abstract;
    public
        constructor Create(const aCaption: string);
        function Evaluate(const aEvalTable: TParserEvalTable): TArg; override;
        property Operand: TEvalNode read fOperand write fOperand;
    end;

    TBinaryOperatorEvalNodeClass = class of TBinaryOperatorEvalNode;

    TBinaryOperatorEvalNode = class(TOperatorEvalNode)
    protected
        fOperand1: TEvalNode;
        fOperand2: TEvalNode;
        procedure EvaluateOperands(const aEvalTable: TParserEvalTable;
            var vEvaluatedOperand1, vEvaluatedOperand2: TArg); virtual;
        function EvalOp(const aEvalTable: TParserEvalTable;
            const aEvaluatedOperand1, aEvaluatedOperand2: TArg): TArg; virtual; abstract;
    public
        constructor Create(const aCaption: string);
        function Evaluate(const aEvalTable: TParserEvalTable): TArg; override;
        property Operand1: TEvalNode read fOperand1 write fOperand1;
        property Operand2: TEvalNode read fOperand2 write fOperand2;
    end;

    TArrayVarOperatorEvalNode = class(TUnaryOperatorEvalNode)
    protected
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TAssignOperatorEvalNode = class(TUnaryOperatorEvalNode)
    private
        fID: string;
        procedure SetVarVal(const aEvalTable: TParserEvalTable; const aIdentName: string; aValue: TArg);
    public
        constructor Create(const aCaption: string; const aID: string);
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
    end;

    TFlowContrBeginEvalNode = class(TUnaryOperatorEvalNode)
    public
        function EvalOp(const aEvalTable: TParserEvalTable; const aEvaluatedOperand: TArg): TArg; override;
        function IsMatchingControlFlow(aEvalNode: TEvalNode): boolean; virtual;
    end;

    TFlowContrEndEvalNode = class(TEvalNode)
    end;


implementation


uses
    SysUtils,
    GeneralTypes;

{ TFuncParams }

constructor TFuncParams.Create(aOtherArgs: TArg; aAddedValue: TArg);
var
    x: integer;
    xArgs: TFuncParams;
begin
    inherited Create();
    if aOtherArgs is TFuncParams then
    begin

        xArgs := aOtherArgs as TFuncParams;
        SetLength(fArr, Length(xArgs.Arr) + 1);
        for x := 0 to high(xArgs.Arr) do
            fArr[x] := xArgs.Arr[x];
    end
    else
    begin
        SetLength(fArr, 2);
        fArr[0] := aOtherArgs;
    end;

    fArr[ high(fArr)] := aAddedValue;
end;

constructor TEvalNode.Create(const aCaption: string);
begin
    inherited Create();
    fCaption := aCaption;
end;

destructor TEvalNode.Destroy();
begin
    inherited;
end;

constructor TVarEvalNode.Create(const aCaption: string; const aVarName: string);
begin
    inherited Create(aCaption);
    fVarName := aVarName
end;

function TVarEvalNode.Evaluate(const aEvalTable: TParserEvalTable): TArg;
var
    xValue: TArg;
begin
    aEvalTable.GetIdentifierValue(fVarName, TArrayArg.UndefinedBoundsIndex, xValue);
    result := xValue;
    // result := CopyAttrValue( xValue );
end;

constructor TConstEvalNode.Create(const aCaption: string; aConstVal: TArg);
begin
    inherited Create(aCaption);
    fValue := aConstVal;
end;

function TConstEvalNode.Evaluate(const aEvalTable: TParserEvalTable): TArg;
begin
    result := fValue;
end;

{ TOperatorEvalNode }

destructor TOperatorEvalNode.Destroy();
begin
    inherited;
end;

class function TOperatorEvalNode.IsStr(aOp: TArg): boolean;
begin
    EXIT(aOp is TStrArg);
end;

class function TOperatorEvalNode.IsDbl(aOp: TArg): boolean;
begin
    EXIT(aOp is TDblArg);
end;

class function TOperatorEvalNode.IsBool(aOp: TArg): boolean;
begin
    EXIT(aOp is TBoolArg);
end;

class function TOperatorEvalNode.IsInt(aOp: TArg): boolean;
begin
    EXIT(aOp is TIntArg);
end;

class function TOperatorEvalNode.IsRef(aOp: TArg): boolean;
begin
    EXIT(aOp is TReferenceArg);
end;

class function TOperatorEvalNode.IsArray(aOp: TArg): boolean;
begin
    EXIT(aOp is TArrayArg);
end;

class function TOperatorEvalNode.IsNum(aOp: TArg): boolean;
begin
    EXIT(IsInt(aOp) or IsDbl(aOp));
end;

class function TOperatorEvalNode.GetBoolVal(aOp: TArg; aOpNumber: char = #0): boolean;
begin
    if (aOp is TBoolArg) then
    begin
        EXIT((aOp as TBoolArg).AsBool)
    end
    else if IsInt(aOp) then
    begin
        EXIT((aOp as TIntArg).AsInt <> 0);
    end
    else
        raise Exception.Create('Operand' + aOpNumber + ' is not a boolean type');
end;

class procedure TOperatorEvalNode.CheckIntType(aOp: TArg; aOpNumber: char = #0);
begin
    if not IsInt(aOp) then
        raise Exception.Create('Operand' + aOpNumber + ' is not an integer type');
end;

class procedure TOperatorEvalNode.CheckIntTypeBoth(aOp1, aOp2: TArg);
begin
    CheckIntType(aOp1, '1');
    CheckIntType(aOp2, '2');
end;

class procedure TOperatorEvalNode.CheckNumType(aOp: TArg; aOpNumber: char = #0);
begin
    if not IsNum(aOp) then
        raise Exception.Create('Operand' + aOpNumber + ' is not a number type');
end;

class procedure TOperatorEvalNode.CheckNumTypeBoth(aOp1, aOp2: TArg);
begin
    CheckNumType(aOp1, '1');
    CheckNumType(aOp2, '2');
end;

class function TOperatorEvalNode.FuncParamTypeToStr(aType: TParseFuncParamType): string;
begin
    case aType of
        pfptStr:
            EXIT('string');
        pfptInt:
            EXIT('integer');
        pfptDbl:
            EXIT('float');
        pfptNum:
            EXIT('number');
        pfptBool:
            EXIT('boolean');
        pfptRef:
            EXIT('reference');
        pfptArr:
            EXIT('array');
    end;
end;

function TOperatorEvalNode.ExpectedParamsAsStr(const aExpectedParams: array of TParseFuncParamType): string;
var
    x: integer;
    xTypeAsStr: string;
begin
    for x := 0 to high(aExpectedParams) do
    begin
        xTypeAsStr := FuncParamTypeToStr(aExpectedParams[x]);
        if x > 0 then
            result := result + ', ';

        result := result + xTypeAsStr;
    end;
end;

function TOperatorEvalNode.CheckFuncParamType(aParam: TArg; aExpectedType: TParseFuncParamType): boolean;
begin
    case aExpectedType of
        pfptInt:
            result := IsInt(aParam);
        pfptDbl:
            result := IsDbl(aParam);
        pfptNum:
            result := IsNum(aParam);
        pfptBool:
            result := IsBool(aParam);
        pfptRef:
            result := IsRef(aParam);
        pfptArr:
            result := IsArray(aParam);
        else
            result := true;
    end;
end;

procedure TOperatorEvalNode.CheckFuncParamsType(aParams: TArg;
    const aExpectedParams: array of TParseFuncParamType);
var
    xFuncParams: TFuncParams;
    x: integer;
begin
    if aParams is TFuncParams then
    begin
        xFuncParams := aParams as TFuncParams;
        if Length(aExpectedParams) <> Length(xFuncParams.Arr) then
            raise Exception.CreateFmt('Mismatched number of parameters. Expected: %s',
                [ExpectedParamsAsStr(aExpectedParams)]);

        for x := 0 to high(aExpectedParams) do
            if not CheckFuncParamType(xFuncParams.Arr[x], aExpectedParams[x]) then
                raise Exception.CreateFmt('Mismatched parameter %d type. Expected: %s',
                    [x, FuncParamTypeToStr(aExpectedParams[x])]);
    end
    else
    begin
        if Length(aExpectedParams) <> 1 then
            raise Exception.CreateFmt('Mismatched number of parameters. Expected: %s',
                [ExpectedParamsAsStr(aExpectedParams)]);

        if not CheckFuncParamType(aParams, aExpectedParams[0]) then
            raise Exception.CreateFmt('Mismatched parameter type. Expected: %s',
                [FuncParamTypeToStr(aExpectedParams[0])]);
    end;
end;

class function TOperatorEvalNode.AreEqual(aOp1, aOp2: TArg): boolean;
begin
    // beide sind integer:
    if (IsInt(aOp1) and IsInt(aOp2)) then
        EXIT(aOp1.AsInt = aOp2.AsInt);

    // beide sind integer oder float:
    if (IsNum(aOp1) and IsNum(aOp2)) then
        EXIT(aOp1.AsFloat = aOp2.AsFloat);

    // einer von beiden ist boolean:
    if (IsBool(aOp1) or IsBool(aOp2)) then
        EXIT(GetBoolVal(aOp1, '1') = GetBoolVal(aOp2, '2'));

    // eventuell könnte man jetzt beides zum string casten

    EXIT(aOp1.AsStr = aOp2.AsStr);
end;

{ TUnaryOperatorEvalNode }

constructor TUnaryOperatorEvalNode.Create(const aCaption: string);
begin
    inherited Create(aCaption);
    fOperand := nil;
end;

function TUnaryOperatorEvalNode.Evaluate(const aEvalTable: TParserEvalTable): TArg;
var
    xOperand: TArg;
begin

    EvaluateOperand(aEvalTable, xOperand);
    CheckOperand(xOperand);
    result := EvalOp(aEvalTable, xOperand);
end;

procedure TUnaryOperatorEvalNode.EvaluateOperand(const aEvalTable: TParserEvalTable;
    var vEvaluatedOperand: TArg);
begin
    vEvaluatedOperand := fOperand.Evaluate(aEvalTable);
end;

procedure TUnaryOperatorEvalNode.CheckOperand(const aEvaluatedOperand: TArg);
begin
    //
end;

{ TBinaryOperatorEvalNode }

constructor TBinaryOperatorEvalNode.Create(const aCaption: string);
begin
    inherited Create(aCaption);
    fOperand1 := nil;
    fOperand2 := nil;
end;

procedure TBinaryOperatorEvalNode.EvaluateOperands(const aEvalTable: TParserEvalTable;
    var vEvaluatedOperand1, vEvaluatedOperand2: TArg);
begin
    vEvaluatedOperand1 := fOperand1.Evaluate(aEvalTable);
    vEvaluatedOperand2 := fOperand2.Evaluate(aEvalTable);
end;

function TBinaryOperatorEvalNode.Evaluate(const aEvalTable: TParserEvalTable): TArg;
var
    xOperand1, xOperand2: TArg;
begin

    EvaluateOperands(aEvalTable, xOperand1, xOperand2);
    // CheckOperand();
    result := EvalOp(aEvalTable, xOperand1, xOperand2);
end;

constructor TAssignOperatorEvalNode.Create(const aCaption: string; const aID: string);
begin
    inherited Create(aCaption);
    fID := aID;
end;

procedure TAssignOperatorEvalNode.SetVarVal(const aEvalTable: TParserEvalTable; const aIdentName: string;
    aValue: TArg);
begin
    aEvalTable.SetIdentifierValue(aIdentName, aValue);
end;

function TAssignOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    result := nil;
    SetVarVal(aEvalTable, fID, aEvaluatedOperand);
end;

{ TFlowContrBeginEvalNode }

function TFlowContrBeginEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
begin
    result := TBoolArg.Create(aEvaluatedOperand.AsBool);
end;

function TFlowContrBeginEvalNode.IsMatchingControlFlow(aEvalNode: TEvalNode): boolean;
begin
    result := false;
end;

{ TArrayVarOperatorEvalNode }

function TArrayVarOperatorEvalNode.EvalOp(const aEvalTable: TParserEvalTable;
    const aEvaluatedOperand: TArg): TArg;
var
    xValue: TArg;
begin
    CheckFuncParamsType(aEvaluatedOperand, [pfptInt]);
    aEvalTable.GetIdentifierValue(fCaption, aEvaluatedOperand.AsInt, xValue);
    result := xValue;
end;


end.
