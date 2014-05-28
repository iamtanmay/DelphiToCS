{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.06.05 pk                               TN2449   Initial revision
  18.11.05 pk  StrToDataValue               TN2781   Check if value=DecimalSeparator before using strtofloat
  21.02.07 pk  Destroy                      TN3583   Missing destructors added
  21.02.07 pk  TParseTable.Create           TN3583   New StartSymbol parameter
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  10.04.13 wl                               TN6045   uses Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit ParserGrammar;


interface


uses
    Generics.Collections,
    ListClasses,
    ParserToken,
    ParserAttribute;

type
    TSymbolAttributeList = class(TStringKeyObjectValueList)
    public
        constructor Create();
        procedure AddAttr(aAttribute: TSymbolAttribute);
    end;

    // SYMBOL
    TSymbolChildLocation = (clNone, clLeft, clRight);

    TSymbol = class
    protected
        fAttributes, fSynthAttributes: TSymbolAttributeList;
        fChildLocation: TSymbolChildLocation;
    public
        constructor Create(aChildLocation: TSymbolChildLocation);
        destructor Destroy(); override;
        procedure AddAttr(aAttr: TSymbolAttribute);
        property Attributes: TSymbolAttributeList read fAttributes write fAttributes;
        property SynthAttributes: TSymbolAttributeList read fSynthAttributes write fSynthAttributes;
        property ChildLocation: TSymbolChildLocation read fChildLocation;

    end;

    TSymbolTerminal = class(TSymbol)
    private
        fTokenType: TTokenType;
    public
        constructor Create(aTokenType: TTokenType; aChildLocation: TSymbolChildLocation);
        property TokenType: TTokenType read fTokenType;
    end;

    // S, E, Z
    TSymbolNonTerminal = class(TSymbol)
    private
        fKeyName: string;
    public
        constructor Create(const aKeyName: string; aChildLocation: TSymbolChildLocation);
        property KeyName: string read fKeyName;
    end;

    // S -> vZ
    TProductionRule = class
        fRuleName: string;
        fHead: TSymbolNonTerminal;
        fBody: TObjectList<TSymbol>;
        constructor Create(aRuleName: string; aHead: TSymbolNonTerminal; aBody: TObjectList<TSymbol>);
        destructor Destroy(); override;
        property Head: TSymbolNonTerminal read fHead;
        property Body: TObjectList<TSymbol>read fBody;
    end;

    TProductionList = class(TStringKeyObjectValueList)
    public
        constructor Create();
        function GetRuleByName(const aRuleName: string): TProductionRule;
        procedure AddProduction(const aRuleName: string; aProductionHead: TSymbolNonTerminal;
            const aProductionBody: array of TSymbol);
    end;

    TAttributeGrammar = class(TProductionList);

    TParseTable = class(TStringKeyObjectValueList)
    private
        fStartSymbol: TSymbolNonTerminal;
        fProductionList: TProductionList;
        function MakeKey(const aProdSymbolName: string; aInputTokenType: TTokenType): string;
    public
        constructor Create(aStartSymbol: TSymbolNonTerminal; aProductionList: TProductionList);
        destructor Destroy(); override;
        procedure AddProduction(const aProdSymbolName: string; aInputTokenType: TTokenType;
            const aRuleName: string);
        function GetProduction(const aProdSymbolName: string; aInputTokenType: TTokenType): TProductionRule;
        property StartSymbol: TSymbolNonTerminal read fStartSymbol;
    end;


implementation


uses
    SysUtils,
    UtilLib;

constructor TSymbol.Create(aChildLocation: TSymbolChildLocation);
begin
    inherited Create;
    fChildLocation := aChildLocation;
    fAttributes := nil;
end;

destructor TSymbol.Destroy();
begin
    // if Assigned( fAttributes ) then
    // FreeList( fAttributes );
    FreeAndNil(fAttributes);
    inherited;
end;

procedure TSymbol.AddAttr(aAttr: TSymbolAttribute);
begin
    if not Assigned(fAttributes) then
        fAttributes := TSymbolAttributeList.Create();

    fAttributes.AddAttr(aAttr);
end;

constructor TSymbolNonTerminal.Create(const aKeyName: string; aChildLocation: TSymbolChildLocation);
begin
    inherited Create(aChildLocation);
    fKeyName := aKeyName;
end;

constructor TSymbolTerminal.Create(aTokenType: TTokenType; aChildLocation: TSymbolChildLocation);
begin
    inherited Create(aChildLocation);
    fTokenType := aTokenType;
end;

{ TSymbolAttributeList }
constructor TSymbolAttributeList.Create;
begin
    inherited Create();
end;

procedure TSymbolAttributeList.AddAttr(aAttribute: TSymbolAttribute);
begin
    AddObject(aAttribute.AttributeName, aAttribute);
end;

constructor TProductionRule.Create(aRuleName: string; aHead: TSymbolNonTerminal; aBody: TObjectList<TSymbol>);
begin
    inherited Create;
    fRuleName := aRuleName;
    fHead := aHead;
    fBody := aBody;
end;

destructor TProductionRule.Destroy();
begin
    FreeAndNil(fHead);
    FreeAndNil(fBody);
    inherited;
end;

constructor TProductionList.Create();
begin
    inherited Create(true);
end;

function TProductionList.GetRuleByName(const aRuleName: string): TProductionRule;
var
    xIndex: integer;
begin
    xIndex := self.IndexOf(aRuleName);
    Assert(xIndex >= 0, Format('Rule %s not found in production list', [aRuleName]));
    result := self.Objects[xIndex] as TProductionRule;
end;

procedure TProductionList.AddProduction(const aRuleName: string; aProductionHead: TSymbolNonTerminal;
    const aProductionBody: array of TSymbol);
var
    xProduction: TProductionRule;
    xProdBody: TObjectList<TSymbol>;
    i: integer;
begin
    xProdBody := TObjectList<TSymbol>.Create();
    for i := 0 to high(aProductionBody) do
        xProdBody.Add(aProductionBody[i]);

    xProduction := TProductionRule.Create(aRuleName, aProductionHead, xProdBody);
    self.AddObject(aRuleName, xProduction);
end;

constructor TParseTable.Create(aStartSymbol: TSymbolNonTerminal; aProductionList: TProductionList);
begin
    inherited Create();
    fStartSymbol := aStartSymbol;
    fProductionList := aProductionList;
end;

destructor TParseTable.Destroy();
begin
    FreeAndNil(fProductionList);
    FreeAndNil(fStartSymbol);
    inherited;
end;

function TParseTable.MakeKey(const aProdSymbolName: string; aInputTokenType: TTokenType): string;
begin
    result := Format('%d-%s', [Integer(aInputTokenType), aProdSymbolName]);
end;

procedure TParseTable.AddProduction(const aProdSymbolName: string; aInputTokenType: TTokenType;
    const aRuleName: string);
var
    xRule: TProductionRule;
begin
    xRule := fProductionList.GetRuleByName(aRuleName);
    self.AddObject(MakeKey(aProdSymbolName, aInputTokenType), xRule);
end;

function TParseTable.GetProduction(const aProdSymbolName: string; aInputTokenType: TTokenType)
    : TProductionRule;
var
    xIndex: integer;
begin
    result := nil;
    xIndex := self.IndexOf(MakeKey(aProdSymbolName, aInputTokenType));
    if xIndex = -1 then
        EXIT;
    result := self.Objects[xIndex] as TProductionRule;
end;


end.
