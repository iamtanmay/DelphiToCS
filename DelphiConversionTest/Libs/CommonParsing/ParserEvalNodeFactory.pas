{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.06.05 pk                               TN2449   Initial revision
  08.09.08 pk                               TN4215   New Caption field for EvalNode
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  07.05.10 pk  CreateArrayVarNode           TN5092   New
  27.06.11 wl                               TN5609   neu: TAttrValueUtils
  01.03.12 wl                               TN5820   uses geändert
  01.03.12 wl                               TN5822   TArg statt TAttrValue
  05.07.12 wl  CreateTerminalNode           TN5917   TBoolToken abgeschafft
  21.03.13 wl                               TN6045   verwendet Generics.Collections
  -------------------------------------------------------------------------------------------------- }

unit ParserEvalNodeFactory;


interface


uses
    Generics.Collections,
    ParserToken,
    ParserEvalNode;

type
    TParserEvalNodeImplementItem = class
    protected
        fName: string;
        fImplementClass: TEvalNodeClass;
    public
        constructor Create(const aName: string; aImplementClass: TEvalNodeClass);
        property name: string read fName write fName;
        property ImplementClass: TEvalNodeClass read fImplementClass;
    end;

    TParserEvalNodeFactory = class
    private
        fImplementList: TObjectList<TParserEvalNodeImplementItem>;
        function FindImplementItem(aImplementList: TObjectList<TParserEvalNodeImplementItem>;
            const aName: string; aSubClass: TEvalNodeClass): TParserEvalNodeImplementItem;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure AddImplement(const aName: string; aImplementClass: TEvalNodeClass);
        function CreateTerminalNode(aToken: TToken): TEvalNode;
        function CreateFlowContrBeginNode(aToken: TToken): TEvalNode;
        function CreateFlowContrEndNode(aToken: TToken): TEvalNode;
        function CreateArrayVarNode(aToken: TToken): TEvalNode;
        function CreateUnaryOperatorNode(aToken: TToken): TEvalNode;
        function CreateBinaryOperatorNode(aToken: TToken): TEvalNode;
    end;


implementation


uses
    SysUtils,
    ParserIdentDataType;

{ TParserEvalNodeImplementItem }

constructor TParserEvalNodeImplementItem.Create(const aName: string; aImplementClass: TEvalNodeClass);
begin
    inherited Create();
    fName := aName;
    fImplementClass := aImplementClass;
end;

{ TParserEvalNodeFactory }

function TParserEvalNodeFactory.FindImplementItem(aImplementList: TObjectList<TParserEvalNodeImplementItem>;
    const aName: string; aSubClass: TEvalNodeClass): TParserEvalNodeImplementItem;
var
    x: integer;
    xItem: TParserEvalNodeImplementItem;
begin
    result := nil;
    for x := 0 to aImplementList.Count - 1 do
    begin
        xItem := aImplementList[x];
        if xItem.ImplementClass.InheritsFrom(aSubClass) and SameText(xItem.Name, aName) then
        begin
            result := xItem;
            EXIT;
        end;
    end;
end;

constructor TParserEvalNodeFactory.Create;
begin
    inherited Create();
    fImplementList := TObjectList<TParserEvalNodeImplementItem>.Create();
end;

destructor TParserEvalNodeFactory.Destroy;
begin
    FreeAndNil(fImplementList);
    inherited;
end;

procedure TParserEvalNodeFactory.AddImplement(const aName: string; aImplementClass: TEvalNodeClass);
begin
    fImplementList.Add(TParserEvalNodeImplementItem.Create(aName, aImplementClass));
end;

function TParserEvalNodeFactory.CreateTerminalNode(aToken: TToken): TEvalNode;

begin
    result := nil;
    if aToken is TVarToken then
    begin
        result := TVarEvalNode.Create(aToken.Text, (aToken as TVarToken).VarName);
    end
    else if aToken is TConstToken then
    begin
        with aToken as TConstToken do
            result := TConstEvalNode.Create(aToken.Text, TArgUtils.CreateArgByTypeOrValue(Value, DataType));
    end

    else if (aToken is TEpsilonToken) then
    begin
        // do nothing;
    end

    else if aToken is TAssignToken then
        result := TAssignOperatorEvalNode.Create(aToken.Text, (aToken as TAssignToken).ID)

end;

function TParserEvalNodeFactory.CreateFlowContrBeginNode(aToken: TToken): TEvalNode;
var
    xImplementItem: TParserEvalNodeImplementItem;
    xOpName: string;
begin
    xOpName := aToken.Text;
    xImplementItem := FindImplementItem(fImplementList, xOpName, TFlowContrBeginEvalNode);
    if not Assigned(xImplementItem) then
        raise Exception.CreateFmt('Flowcontrol Begin %s not found', [xOpName]);
    result := xImplementItem.ImplementClass.Create(xOpName);
end;

function TParserEvalNodeFactory.CreateFlowContrEndNode(aToken: TToken): TEvalNode;
var
    xImplementItem: TParserEvalNodeImplementItem;
    xOpName: string;
begin
    xOpName := aToken.Text;
    xImplementItem := FindImplementItem(fImplementList, xOpName, TFlowContrEndEvalNode);
    if not Assigned(xImplementItem) then
        raise Exception.CreateFmt('Flowcontrol End %s not found', [xOpName]);
    result := xImplementItem.ImplementClass.Create(xOpName);
end;

function TParserEvalNodeFactory.CreateArrayVarNode(aToken: TToken): TEvalNode;
begin
    result := TArrayVarOperatorEvalNode.Create(aToken.Text);
end;

function TParserEvalNodeFactory.CreateUnaryOperatorNode(aToken: TToken): TEvalNode;
var
    xImplementItem: TParserEvalNodeImplementItem;
    xOpName: string;
begin
    xOpName := aToken.Text;
    xImplementItem := FindImplementItem(fImplementList, xOpName, TUnaryOperatorEvalNode);
    if not Assigned(xImplementItem) then
        raise Exception.CreateFmt('Unary function %s not found', [xOpName]);
    result := xImplementItem.ImplementClass.Create(xOpName);
end;

function TParserEvalNodeFactory.CreateBinaryOperatorNode(aToken: TToken): TEvalNode;
var
    xImplementItem: TParserEvalNodeImplementItem;
    xOpName: string;
begin
    xOpName := aToken.Text;
    xImplementItem := FindImplementItem(fImplementList, xOpName, TBinaryOperatorEvalNode);
    if not Assigned(xImplementItem) then
        raise Exception.CreateFmt('Binary function %s not found', [xOpName]);
    result := xImplementItem.ImplementClass.Create(xOpName);
end;


end.
