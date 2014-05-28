{ --------------------------------------------------------------------------------------------------
  Copyright © 2007 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  : Tokenizes an input string into a list of Tokens
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  27.08.07 pk                               TN3788   Initial revision
  06.11.08 pk                               TN4279   uses changed
  21.07.09 pk                               TN4669   New
  04.11.09 pk                               TN4843 Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  07.05.10 pk                               TN5092   New DetermineSquareBracketOpenToken
  07.05.10 pk                               TN5095   True/False tokens
  22.06.10 pk                               TN5088   Tokenizing of AssignToken( := ) and ControlFlowToken (while, If) removed
  27.06.11 wl                               TN5609   neu: SUB
  18.07.11 wl                               TN5628   neu: Statistik-Funktionen für Arrays: MIN,MAX,SUM,AVR,STDDEV
  18.07.11 wl                               TN5628.1 neu: A() erzeugt neues Array
  02.08.11 wl  'OR', 'AND', '='             TN5646   zusätzlich zur C++-Syntax '||', '&&', '==' darf auch SQL-Syntax verwendet werden: 'OR', 'AND', '='
  02.08.11 wl  SQRT,COS,SIN,TAN,....        TN5481   alle mathematischen Funktionen, die es früher schon mal gab
  02.08.11 wl                               TN5481   neu: POWER, LOG10
  02.08.11 wl  ROUND                        TN5481   Round-Funktion kann jetzt RND oder ROUND geschrieben werden
  18.08.11 wl  CAST                         TN5665   neu
  17.10.11 wl  INDEXOF                      TN5717   neu
  21.02.12 wl  RSD, FMT_PERCENT             TN5805   neu
  01.03.12 wl  STR_TOKEN_TRUE/FALSE         TN5820   --> ParserIdentDataType
  02.03.12 wl  TYPEOF                       TN5823   neu
  06.03.12 wl  BITSTOARRAY, ARRAYTOBITS     TN5824   neu
  05.07.12 wl  Tokenize                     TN5917   TBoolToken abgeschafft
  28.05.13 ts  STR_TOKEN_JSON_EVAL          TN6152   neu
  28.06.13 wl  STR_TOKEN_JSON_ADD,_REMOVE   TN6191   neu
  -------------------------------------------------------------------------------------------------- }

unit ParserTokenizer;


interface


uses
    Classes,
    ParserToken;

const
    STR_TOKEN_FUNCPARDELIM = ',';
    STR_TOKEN_SPACE = ' ';
    STR_TOKEN_BRACKETOPEN = '(';
    STR_TOKEN_BRACKETCLOSE = ')';
    STR_TOKEN_SQBRACKETOPEN = '[';
    STR_TOKEN_SQBRACKETCLOSE = ']';

    STR_TOKEN_CONCAT = '&';
    STR_TOKEN_ADD = '+';
    STR_TOKEN_SUBTRACT = '-';
    STR_TOKEN_MULTIPLY = '*';
    STR_TOKEN_DIVIDE = '/';
    STR_TOKEN_MOD = 'MOD';
    STR_TOKEN_DIV = 'DIV';
    STR_TOKEN_OR = 'OR'; // Alte C++-Syntax || funktioniert aber weiterhin
    STR_TOKEN_AND = 'AND'; // Alte C++-Syntax && funktioniert aber weiterhin
    STR_TOKEN_EQUALS = '='; // Alte C++-Syntax == funktioniert aber weiterhin
    STR_TOKEN_LESSTHAN = '<';
    STR_TOKEN_GREATERTHAN = '>';
    STR_TOKEN_LESSTHANEQUALS = '<=';
    STR_TOKEN_GREATERTHANEQUALS = '>=';
    STR_TOKEN_NOTEQUALS = '<>';
    STR_TOKEN_UNARYMINUS = '-';

    STR_TOKEN_SUBSTR = 'SUBSTR';
    STR_TOKEN_SUBARR = 'SUB';
    STR_TOKEN_UPPER = 'UPPER'; // SQL syntax
    STR_TOKEN_POS = 'POS';
    STR_TOKEN_LEN = 'LEN';
    STR_TOKEN_COUNT = 'COUNT';
    STR_TOKEN_TRIM = 'TRIM'; // SQL syntax
    STR_TOKEN_REVSTR = 'REVSTR';
    STR_TOKEN_PAD = 'PAD';
    STR_TOKEN_XMOD = 'XMOD';
    STR_TOKEN_XDIV = 'XDIV';
    STR_TOKEN_NOW = 'NOW';
    STR_TOKEN_ABS = 'ABS'; // SQL syntax
    STR_TOKEN_TRUNC = 'TRUNC'; // SQL syntax
    STR_TOKEN_ROUND1 = 'RND';
    STR_TOKEN_ROUND2 = 'ROUND'; // SQL syntax
    STR_TOKEN_RANDOM = 'RAND';
    STR_TOKEN_SELECTBIT = 'SELECTBIT';
    STR_TOKEN_ISBITSELECTED = 'ISBITSELECTED';
    STR_TOKEN_MIN = 'MIN';
    STR_TOKEN_MAX = 'MAX';
    STR_TOKEN_SUM = 'SUM'; // SQL syntax
    STR_TOKEN_AVR = 'AVR'; // SQL syntax
    STR_TOKEN_STDDEV = 'STDDEV'; // SQL syntax
    STR_TOKEN_NEWARRAY = 'A';
    STR_TOKEN_CAST = 'CAST';
    STR_TOKEN_INDEXOF = 'INDEXOF';
    STR_TOKEN_TYPEOF = 'TYPEOF';
    STR_TOKEN_BITSTOARRAY = 'BITSTOARRAY';
    STR_TOKEN_ARRAYTOBITS = 'ARRAYTOBITS';

    STR_TOKEN_COS = 'COS'; // SQL syntax
    STR_TOKEN_COSH = 'COSH';
    STR_TOKEN_SIN = 'SIN'; // SQL syntax
    STR_TOKEN_SINH = 'SINH';
    STR_TOKEN_TAN = 'TAN';
    STR_TOKEN_ARCTAN = 'ARCTAN'; // SQL syntax
    STR_TOKEN_SQRT = 'SQRT'; // SQL syntax
    STR_TOKEN_EXP = 'EXP'; // SQL syntax
    STR_TOKEN_LOG = 'LOG'; // SQL syntax
    STR_TOKEN_LOG10 = 'LOG10';
    STR_TOKEN_POWER = 'POWER';
    STR_TOKEN_RSD = 'RSD';
    STR_TOKEN_FMT_PERCENT = 'FMT_PERCENT';

    STR_TOKEN_JSON_EVAL = 'JSON_EVAL';
    STR_TOKEN_JSON_ADD = 'JSON_ADD';
    STR_TOKEN_JSON_REMOVE = 'JSON_REMOVE';

type
    TOnTokenizerIdentFound = procedure(const aIdentName: string) of object;

    TParserTokenizer = class
    protected
        fOnIdentFound: TOnTokenizerIdentFound;
        fTokenList: TTokenList;
        fPrevToken: TToken;
        fCurChar, fNextChar, fOverNextChar: char;
        fAccum, fUpperAccum: string;
        fIsString: boolean;
        fCurIndex: integer;
        fInput: string;
        fInputLen: integer;

        procedure Accumulate();
        procedure ResetAccum();
        procedure AddToken(aToken: TToken);

        procedure SyntaxError(const aMessage: string);
        function VarToken(): boolean;
        procedure ConstToken();
        function ModDivToken(): boolean;
        function DetermineNonConstToken(): boolean;
        procedure DetermineToken();
        procedure DetermineBracketOpenToken();
        procedure DetermineSquareBracketOpenToken();
        function GetPrevTextToken(): TConstToken;

    public
        constructor Create();
        destructor Destroy(); override;
        function Tokenize(const aInput: string): TTokenList;
        class function TokenizeStatic(const aInput: string): TTokenList;
        class function RemoveQuotes(const aStr: string): string;
        property OnIdentFound: TOnTokenizerIdentFound read fOnIdentFound write fOnIdentFound;
    end;


implementation


uses
    SysUtils,
    ParserIdentifier,
    ParserIdentDataType;

{ TParserTokenizer }

constructor TParserTokenizer.Create;
begin
    inherited Create();
end;

destructor TParserTokenizer.Destroy();
begin
    inherited Destroy();
end;

procedure TParserTokenizer.SyntaxError(const aMessage: string);
begin
    raise Exception.Create('Syntax Error ->' + aMessage);
end;

function TParserTokenizer.ModDivToken(): boolean;
begin
    result := false;

    if fUpperAccum = STR_TOKEN_MOD then
        AddToken(TMultDivToken.Create(fAccum))
    else if fUpperAccum = STR_TOKEN_DIV then
        AddToken(TMultDivToken.Create(fAccum))
    else if fUpperAccum = STR_TOKEN_POWER then
        AddToken(TMultDivToken.Create(fAccum))
    else if fUpperAccum = STR_TOKEN_AND then // Neue SQL-Syntax: AND
        AddToken(TLogicToken.Create(STR_TOKEN_AND, ltAnd))
    else if fUpperAccum = STR_TOKEN_OR then // Neue SQL-Syntax: OR
        AddToken(TLogicToken.Create(STR_TOKEN_OR, ltOr))
    else
        EXIT;

    result := true;
end;

function TParserTokenizer.VarToken(): boolean;
begin
    result := false;
    if TParserIdentifier.GetIdentType(fAccum) = itNone then
        EXIT;

    AddToken(TVarToken.Create(fAccum, fAccum));

    if Assigned(fOnIdentFound) then
        fOnIdentFound(fAccum);
    result := true;
end;

procedure TParserTokenizer.ConstToken();
var
    xConstToken: TConstToken;
begin
    // allow fusion of two constants without need of concatenation symbol in order
    // to stay compatible with old parser. Example : Hello World is allowed eventhough it is made
    // up of two const tokens Hello and World
    xConstToken := GetPrevTextToken();
    if Assigned(xConstToken) then
    begin
        xConstToken.Concat(fAccum);
        EXIT;
    end;

    AddToken(TConstToken.Create(fAccum, false, fAccum, idtNone));
end;

function TParserTokenizer.DetermineNonConstToken(): boolean;
begin
    result := true;
    if VarToken() then
        EXIT;

    if ModDivToken() then
        EXIT;
    result := false;
end;

procedure TParserTokenizer.DetermineToken();
begin
    if fAccum = '' then
        EXIT;
    fUpperAccum := UpperCase(fAccum);

    if not DetermineNonConstToken() then
        ConstToken();

    ResetAccum();
end;

procedure TParserTokenizer.DetermineBracketOpenToken();
begin
    if fAccum = '' then
        EXIT;

    fUpperAccum := UpperCase(fAccum);

    if DetermineNonConstToken() then
    begin

    end
    else
    begin
        AddToken(TFuncToken.Create(fUpperAccum));
    end;
    ResetAccum();

end;

procedure TParserTokenizer.DetermineSquareBracketOpenToken();
begin
    if fAccum = '' then
        EXIT;

    fUpperAccum := UpperCase(fAccum);

    AddToken(TArrayVarToken.Create(fAccum));
    if Assigned(fOnIdentFound) then
        fOnIdentFound(fAccum);

    ResetAccum();

end;

procedure TParserTokenizer.ResetAccum();
begin
    fAccum := '';
    fUpperAccum := '';
end;

procedure TParserTokenizer.Accumulate();
begin
    fAccum := fAccum + fCurChar;
end;

procedure TParserTokenizer.AddToken(aToken: TToken);
begin
    fPrevToken := aToken;
    fTokenList.Add(aToken);
end;

function TParserTokenizer.GetPrevTextToken(): TConstToken;
var
    xConstToken: TConstToken;
begin
    result := nil;
    if not Assigned(fPrevToken) then
        EXIT;

    if (fPrevToken is TConstToken) then
    begin
        xConstToken := fPrevToken as TConstToken;
        if not xConstToken.HasQuotes then
        begin
            result := xConstToken;
        end;
    end;
end;

function TParserTokenizer.Tokenize(const aInput: string): TTokenList;
begin
    fPrevToken := nil;
    fTokenList := TTokenList.Create;
    result := fTokenList;

    fCurIndex := 0;
    fInput := aInput;

    ResetAccum();
    fIsString := false;

    fInputLen := Length(fInput);
    while true do
    begin
        Inc(fCurIndex);

        fCurChar := #0;
        fNextChar := #0;
        fOverNextChar := #0;

        if fCurIndex <= fInputLen then
        begin
            fCurChar := fInput[fCurIndex];
            if fCurIndex + 1 <= fInputLen then
                fNextChar := fInput[fCurIndex + 1];
            if fCurIndex + 2 <= fInputLen then
                fOverNextChar := fInput[fCurIndex + 2];
        end;

        if fIsString then
        begin
            if (fCurChar = '"') then
            begin
                AddToken(TConstToken.Create('"' + fAccum + '"', true, fAccum, idtString));
                fIsString := false;
                fAccum := '';
                CONTINUE;
            end
            else if (fCurChar = '\') and (fNextChar = '"') then
            begin
                fAccum := fAccum + fNextChar;
                Inc(fCurIndex);
            end

            else if fCurChar = #0 then
                SyntaxError('No end quotation marks')
            else
                Accumulate();
            CONTINUE;
        end;

        case fCurChar of
            '\':
                begin
                    if fNextChar = '"' then
                    begin
                        fAccum := fAccum + fNextChar;
                        Inc(fCurIndex);
                    end
                    else
                        Accumulate();
                end;
            '"':
                begin
                    DetermineToken();
                    fIsString := true;
                end;
            '=':
                begin
                    if fNextChar = '=' then
                    begin
                        // Alte C++-Syntax: ==
                        DetermineToken();
                        Inc(fCurIndex);
                        AddToken(TCompToken.Create(STR_TOKEN_EQUALS));
                    end
                    else
                    begin
                        // Neue SQL-Syntax: =
                        DetermineToken();
                        AddToken(TCompToken.Create(STR_TOKEN_EQUALS));
                    end
                end;
            STR_TOKEN_LESSTHAN:
                begin
                    DetermineToken();
                    if fNextChar = '=' then
                    begin
                        Inc(fCurIndex);
                        AddToken(TCompToken.Create(STR_TOKEN_LESSTHANEQUALS));
                    end
                    else if fNextChar = '>' then
                    begin
                        Inc(fCurIndex);
                        AddToken(TCompToken.Create(STR_TOKEN_NOTEQUALS));
                    end
                    else
                        AddToken(TCompToken.Create(STR_TOKEN_LESSTHAN));

                end;
            STR_TOKEN_GREATERTHAN:
                begin
                    DetermineToken();
                    if fNextChar = '=' then
                    begin
                        Inc(fCurIndex);
                        AddToken(TCompToken.Create(STR_TOKEN_GREATERTHANEQUALS));
                    end
                    else
                        AddToken(TCompToken.Create(STR_TOKEN_GREATERTHAN));

                end;
            STR_TOKEN_ADD:
                begin
                    DetermineToken();
                    AddToken(TPlusToken.Create(STR_TOKEN_ADD));
                end;
            STR_TOKEN_SUBTRACT:
                begin
                    DetermineToken();
                    AddToken(TMinusToken.Create(STR_TOKEN_SUBTRACT));
                end;
            STR_TOKEN_MULTIPLY:
                begin
                    DetermineToken();
                    AddToken(TMultDivToken.Create(STR_TOKEN_MULTIPLY));
                end;
            STR_TOKEN_DIVIDE:
                begin
                    DetermineToken();
                    AddToken(TMultDivToken.Create(STR_TOKEN_DIVIDE));
                end;
            STR_TOKEN_BRACKETOPEN:
                begin
                    DetermineBracketOpenToken();
                    AddToken(TBracketOpenToken.Create(STR_TOKEN_BRACKETOPEN, ttBracketOpen));
                end;
            STR_TOKEN_BRACKETCLOSE:
                begin
                    DetermineToken();
                    AddToken(TBracketCloseToken.Create(STR_TOKEN_BRACKETCLOSE, ttBracketClose));
                end;

            STR_TOKEN_FUNCPARDELIM:
                begin
                    DetermineToken();
                    AddToken(TFuncParDelimToken.Create(STR_TOKEN_FUNCPARDELIM));
                end;
            STR_TOKEN_SQBRACKETOPEN:
                begin
                    DetermineSquareBracketOpenToken();
                    AddToken(TSquareBracketOpenToken.Create(STR_TOKEN_SQBRACKETOPEN, ttSquareBracketOpen));
                end;
            STR_TOKEN_SQBRACKETCLOSE:
                begin
                    DetermineToken();
                    AddToken(TSquareBracketCloseToken.Create(STR_TOKEN_SQBRACKETCLOSE, ttSquareBracketClose));
                end;
            STR_TOKEN_CONCAT:
                begin
                    DetermineToken();
                    if fNextChar = '&' then
                    begin
                        // Alte C++-Syntax: &&
                        Inc(fCurIndex);
                        AddToken(TLogicToken.Create(STR_TOKEN_AND, ltAnd));
                    end
                    else
                        AddToken(TConcatToken.Create(STR_TOKEN_CONCAT, ttConcat));
                end;
            '|':
                begin
                    DetermineToken();
                    if fNextChar = '|' then
                    begin
                        // Alte C++-Syntax: ||
                        Inc(fCurIndex);
                        AddToken(TLogicToken.Create(STR_TOKEN_OR, ltOr));
                    end
                    else
                        Accumulate();
                end;

            STR_TOKEN_SPACE:
                begin
                    DetermineToken();
                end;

            #0:
                begin
                    DetermineToken();
                    AddToken(TEpsilonToken.Create(''));
                    BREAK;
                end;
            else
                Accumulate();
        end;
    end;
end;

class function TParserTokenizer.TokenizeStatic(const aInput: string): TTokenList;
var
    xTokenizer: TParserTokenizer;
begin
    xTokenizer := TParserTokenizer.Create;
    try
        result := xTokenizer.Tokenize(aInput)
    finally
        FreeAndNil(xTokenizer);
    end;
end;

class function TParserTokenizer.RemoveQuotes(const aStr: string): string;
var
    xToken: TToken;
    i: integer;
    xTokenList: TTokenList;
begin
    result := '';

    xTokenList := TokenizeStatic(aStr);
    try

        for i := 0 to xTokenList.Count - 1 do
        begin
            xToken := xTokenList[i];
            if (xToken is TConstToken) then
                result := result + (xToken as TConstToken).Value
            else
                result := result + xToken.Text
        end;
    finally
        FreeAndNil(xTokenList);
    end;
end;


end.
