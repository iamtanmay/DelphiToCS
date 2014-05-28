{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.06.05 pk                               TN2449 Initial revision
  15.08.05 pk  TToken                       TN2560 New field : fText.  Store the unparsed value
  25.08.05 pk  TLogicToken                  TN2573 New : support for logic operators
  27.10.06 wl  TUnaryFuncType               TN3331 New: uftDllCall, uftImport, uftSQLUpdate, uftSQLSelect, uftFileCopy
  01.08.07 pk  TFuncType                    TN3814 Used to be TUnaryFuncType
  01.08.07 pk  TFuncType                    TN3814 New: uftSubtr, Pos, Now...
  27.08.07 pk                               TN3788 Various Changes
  06.11.08 pk                               TN4279   uses changed
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  07.05.10 pk  TArrayVarToken               TN5092   New
  21.02.12 wl                               TN5805   Konstanten entfernt
  29.02.12 wl  TBoolToken                   TN5821   Bugfix: Der Wert wurde bisher beim Create gar nicht geschrieben!
  05.07.12 wl  TBoolToken                   TN5917   abgeschafft, denn ein Bool-Token ist Const-Token mit dem DataType bool!
  -------------------------------------------------------------------------------------------------- }

unit ParserToken;


interface


uses
    Generics.Collections,
    ParserIdentifier,
    ParserIdentDataType;

const
    STR_ATTR_NAME_OPERAND1 = 'OP1';
    STR_ATTR_NAME_SCANVAL = 'SCN';
    STR_ATTR_NAME_EVAL = 'EVL';

type
    TTokenType = (ttEpsilon, ttConst, ttVar, ttArrayVar, ttAssign, ttFlowBegin, ttFlowEnd, ttConcat,
        ttFuncParDelim, ttComp, ttPlus, ttMinus, ttMultDiv, ttBracketOpen, ttBracketClose,
        ttSquareBracketOpen, ttSquareBracketClose, ttFunc, ttLogic, ttNot);

    TToken = class
    protected
        fTokenType: TTokenType;
        fText: string;
    public
        constructor Create(const aText: string; aTokenType: TTokenType);
        function TokenToStr(): string;
        class function TokenTypeToStr(aTokenType: TTokenType): string;
        property TokenType: TTokenType read fTokenType;
        property Text: string read fText write fText;
    end;

    TTokenList = class(TObjectList<TToken>)
    public
        function TokensToStr(): string;
    end;

    TEpsilonToken = class(TToken)
    public
        constructor Create(const aText: string);
    end;

    TConstToken = class(TToken)
    protected
        fValue: string;
        fDataType: TParserIdentDataType;
        fHasQuotes: boolean;
    public
        constructor Create(const aText: string; aHasQuotes: boolean; const aValue: string;
            aDataType: TParserIdentDataType);
        procedure Concat(const aValue: string);
        property Value: string read fValue;
        property DataType: TParserIdentDataType read fDataType;
        property HasQuotes: boolean read fHasQuotes;
    end;

    TAssignToken = class(TToken)
    protected
        fID: string;
    public
        constructor Create(const aText: string; const aID: string);
        property ID: string read fID;
    end;

    TCompToken = class(TToken)
    public
        constructor Create(const aText: string);
    end;

    TPlusToken = class(TToken)
    public
        constructor Create(const aText: string);
    end;

    TMinusToken = class(TToken)
    public
        constructor Create(const aText: string);
    end;

    TMultDivToken = class(TToken)
    public
        constructor Create(const aText: string);
    end;

    TConcatToken = class(TToken)
    end;

    TFuncParDelimToken = class(TToken)
    public
        constructor Create(const aText: string);
    end;

    TLogicType = (ltAnd, ltOr);

    TLogicToken = class(TToken)
    private
        fLogicType: TLogicType;
    public
        constructor Create(const aText: string; aLogicType: TLogicType);
        property LogicType: TLogicType read fLogicType;
    end;

    TBracketOpenToken = class(TToken);
    TBracketCloseToken = class(TToken);

    TSquareBracketOpenToken = class(TToken);
    TSquareBracketCloseToken = class(TToken);

    TVarToken = class(TToken)
    protected
        fVarName: string;
    public
        constructor Create(const aText: string; const aVarName: string);
        property VarName: string read fVarName;
    end;

    TContFlowType = (cfIf, cfEndIf, cfWhile, cfEndWhile);

    TFlowToken = class(TToken)
    protected
        fFlowType: TContFlowType;
    public
        constructor Create(const aText: string; aTokenType: TTokenType; aFlowType: TContFlowType);
        property FlowType: TContFlowType read fFlowType;
    end;

    TFlowBeginToken = class(TFlowToken)
    public
        constructor Create(const aText: string; aFlowType: TContFlowType);
    end;

    TFlowEndToken = class(TFlowToken)
    public
        constructor Create(const aText: string; aFlowType: TContFlowType);
    end;

    TFuncToken = class(TToken)
    public
        constructor Create(const aText: string);
    end;

    TArrayVarToken = class(TToken)
    public
        constructor Create(const aText: string);
    end;


implementation


uses
    SysUtils,
    UtilLib;

{ TToken }

constructor TToken.Create(const aText: string; aTokenType: TTokenType);
begin
    inherited Create;
    fTokenType := aTokenType;
    fText := aText;
end;

function TToken.TokenToStr(): string;
begin
    result := TokenTypeToStr(fTokenType);
end;

class function TToken.TokenTypeToStr(aTokenType: TTokenType): string;
begin
    case aTokenType of
        ttEpsilon:
            result := '';
        ttConst:
            result := 'Const';
        ttVar:
            result := 'Var';
        ttAssign:
            result := 'Assign';
        ttFlowBegin:
            result := 'FlowBegin';
        ttFlowEnd:
            result := 'FlowEnd';
        ttConcat:
            result := 'Concat';
        ttFuncParDelim:
            result := 'FuncParamDelim';
        ttComp:
            result := 'Compare';
        ttPlus:
            result := 'Plus';
        ttMinus:
            result := 'Minus';
        ttMultDiv:
            result := 'MultDiv';
        ttBracketOpen:
            result := 'BracketOpen';
        ttBracketClose:
            result := 'BracketClose';
        ttFunc:
            result := 'Func';
        ttSquareBracketOpen:
            result := 'SquareBracketOpen';
        ttSquareBracketClose:
            result := 'SquareBracketClose';
        ttArrayVar:
            result := 'ArrayVar';
        ttLogic:
            result := 'Logic';
        else
            result := '';
    end;
end;

{ TTokenList }

function TTokenList.TokensToStr(): string;
var
    i: integer;
begin
    result := '';
    for i := 0 to Count - 1 do
        result := result + ' ' + self[i].TokenToStr;
end;

{ TEpsilonToken }

constructor TEpsilonToken.Create(const aText: string);
begin
    inherited Create(aText, ttEpsilon);
end;

{ TConstToken }

constructor TConstToken.Create(const aText: string; aHasQuotes: boolean; const aValue: string;
    aDataType: TParserIdentDataType);
begin
    inherited Create(aText, ttConst);
    fHasQuotes := aHasQuotes;
    fValue := aValue;
    fDataType := aDataType;
end;

procedure TConstToken.Concat(const aValue: string);
begin
    fValue := fValue + ' ' + aValue;
    fText := fText + ' ' + fText;
end;

{ TAssignToken }

constructor TAssignToken.Create(const aText: string; const aID: string);
begin
    inherited Create(aText, ttAssign);
    fID := aID;
end;

{ TCompToken }

constructor TCompToken.Create(const aText: string);
begin
    inherited Create(aText, ttComp);
end;

{ TPlusToken }

constructor TPlusToken.Create(const aText: string);
begin
    inherited Create(aText, ttPlus);
end;

{ TMinusToken }

constructor TMinusToken.Create(const aText: string);
begin
    inherited Create(aText, ttMinus);
end;

{ TMultDivToken }

constructor TMultDivToken.Create(const aText: string);
begin
    inherited Create(aText, ttMultDiv);
end;

{ TVarToken }

constructor TVarToken.Create(const aText: string; const aVarName: string);
begin
    inherited Create(aText, ttVar);
    fVarName := aVarName;
end;

{ TFlowBeginToken }

constructor TFlowBeginToken.Create(const aText: string; aFlowType: TContFlowType);
begin
    inherited Create(aText, ttFlowBegin, aFlowType);
end;

{ TFlowEndToken }

constructor TFlowEndToken.Create(const aText: string; aFlowType: TContFlowType);
begin
    inherited Create(aText, ttFlowEnd, aFlowType);
end;

{ TFlowToken }

constructor TFlowToken.Create(const aText: string; aTokenType: TTokenType; aFlowType: TContFlowType);
begin
    inherited Create(aText, aTokenType);
    fFlowType := aFlowType;
end;

{ TFuncToken }

constructor TFuncToken.Create(const aText: string);
begin
    inherited Create(aText, ttFunc);
end;

{ TArrayVarToken }

constructor TArrayVarToken.Create(const aText: string);
begin
    inherited Create(aText, ttArrayVar);
end;

{ TLogicToken }

constructor TLogicToken.Create(const aText: string; aLogicType: TLogicType);
begin
    inherited Create(aText, ttLogic);
    fLogicType := aLogicType;
end;

{ TFuncParDelimToken }

constructor TFuncParDelimToken.Create(const aText: string);
begin
    inherited Create(aText, ttFuncParDelim);
end;


end.
