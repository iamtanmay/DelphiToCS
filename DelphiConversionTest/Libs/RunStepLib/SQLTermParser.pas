{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk), Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  06.07.09 pk                                        TN4585.4   Initial revision, code from SQLTermpParserInterface
  04.11.09 pk                               	    TN4843     Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  10.04.13 wl                                        TN6045   uses Generics.Collections
  ----------------------------------------------------------------------------------------------------------------------- }

unit SQLTermParser;


interface


uses
    GeneralTypes,
    Generics.Collections,
    SQLTermsDataAdaptor;

const
    STR_SQLTERM_PARAM_VARIABLEFORMAT = '#P%d#';

type
    TSamIdentFieldTypes = (iftName, iftVolume, iftRemark, iftPosition, iftCnt);

    TSQLParserSetValComponent = (pscEdit, pscLabel1, pscLabel2, pscButtonEdit, pscButtonPath);

    TParserSetValueFormMode = (psmBuildMethod, bsmEditMethodParameter);

    TSQLParserIdentifier = class
    private
        fName: string;
        fValue: string;
        //
        procedure SetValue(aValue: string);
        function GetValue: string;
    public
        constructor Create(aName: string);
        property Value: string read GetValue write SetValue;
        property name: string read fName;
    end;

    TSQLParserStoredIdent = class(TSQLParserIdentifier)
    private
        fDefValue: string;
        fDescription: string;
        //
        procedure SetDefValue(aValue: string);
        function GetDefValue: string;
    public
        property DefValue: string read GetDefValue write SetDefValue;
        property Description: string read fDescription write fDescription;
    end;

    TSQLParserIdentPair = class
    private
        fIdent: TSQLParserIdentifier;
        fStoredIdent: TSQLParserStoredIdent;
    public
        constructor Create(aIdent: TSQLParserIdentifier; aStoredIdent: TSQLParserStoredIdent);
        property Ident: TSQLParserIdentifier read fIdent write fIdent;
        property StoredIdent: TSQLParserStoredIdent read fStoredIdent write fStoredIdent;
    end;

    TSQLParserIdentPairList = class
    protected
        FList: TObjectList<TSQLParserIdentPair>;
        function GetItemAt(aIndex: integer): TSQLParserIdentPair;
        function GetCount: integer;
    public
        constructor Create();
        destructor Destroy(); override;
        procedure AddItem(aItem: TSQLParserIdentPair);
        procedure DeleteItem(aIndex: integer);
        property Items[aIndex: integer]: TSQLParserIdentPair read GetItemAt; default;
        property Count: integer read GetCount;
    end;

    TSQLTermParser = class
    public
        class function ParseSQLStrAtRun(const aSQLStr, aCurrentRunName, aGlobalRunLayoutName: string;
            aCurLivePriority: integer; const aSQLArgs: TStringArray): string;

        class procedure ParseSQLVariables(aIdentsNeedingInput: TSQLParserIdentPairList;
            const aSQLTerm: string; const aSQLArgs: TStringArray);
    end;


implementation


uses
    SysUtils;

const
    // aus SQLResult entnommen:
    STR_SQLTERM_PARAM_CURRENTRUNNAME = '#METHODNAME#';
    STR_SQLTERM_PARAM_RUNLAYOUTNAME = '#RUNNAME#';
    STR_SQLTERM_PARAM_PRIORITY = '#PRIORITY#';

    { TSQLParserIdentifier }

constructor TSQLParserIdentifier.Create(aName: string);
begin
    inherited Create;
    fName := aName;
end;

function TSQLParserIdentifier.GetValue: string;
begin
    result := fValue;
end;

procedure TSQLParserIdentifier.SetValue(aValue: string);
begin
    fValue := aValue;
end;

{ TSQLParserStoredIdent }

function TSQLParserStoredIdent.GetDefValue: string;
begin
    result := fDefValue;
end;

procedure TSQLParserStoredIdent.SetDefValue(aValue: string);
begin
    fDefValue := aValue;
end;

{ TSQLParserIdentPair }

constructor TSQLParserIdentPair.Create(aIdent: TSQLParserIdentifier; aStoredIdent: TSQLParserStoredIdent);
begin
    inherited Create;

    fIdent := aIdent;
    fStoredIdent := aStoredIdent;
end;

{ TSQLParserIdentPairList }

procedure TSQLParserIdentPairList.AddItem(aItem: TSQLParserIdentPair);
begin
    fList.Add(aItem);
end;

constructor TSQLParserIdentPairList.Create;
begin
    inherited;
    fList := TObjectList<TSQLParserIdentPair>.Create(true);
end;

procedure TSQLParserIdentPairList.DeleteItem(aIndex: integer);
begin
    fList.Delete(aIndex);
end;

destructor TSQLParserIdentPairList.Destroy;
begin
    fList.Free;

    inherited;
end;

function TSQLParserIdentPairList.GetCount: integer;
begin
    result := fList.Count;
end;

function TSQLParserIdentPairList.GetItemAt(aIndex: integer): TSQLParserIdentPair;
begin
    result := fList[aIndex];
end;

{ TSQLTermParser }
class function TSQLTermParser.ParseSQLStrAtRun(const aSQLStr, aCurrentRunName, aGlobalRunLayoutName: string;
    aCurLivePriority: integer; const aSQLArgs: TStringArray): string;
// Ersetzt Platzhalter im SQLString
var
    i: integer;
begin
    result := aSQLStr;

    result := StringReplace(result, STR_SQLTERM_PARAM_CURRENTRUNNAME, aCurrentRunName,
        [rfReplaceAll, rfIgnoreCase]);
    result := StringReplace(result, STR_SQLTERM_PARAM_RUNLAYOUTNAME, aGlobalRunLayoutName,
        [rfReplaceAll, rfIgnoreCase]);
    result := StringReplace(result, STR_SQLTERM_PARAM_PRIORITY, IntToStr(aCurLivePriority),
        [rfReplaceAll, rfIgnoreCase]);

    for i := 0 to Length(aSQLArgs) - 1 do
    begin
        result := StringReplace(result, Format(STR_SQLTERM_PARAM_VARIABLEFORMAT, [i]), aSQLArgs[i],
            [rfReplaceAll, rfIgnoreCase]);
    end;
end;

class procedure TSQLTermParser.ParseSQLVariables(aIdentsNeedingInput: TSQLParserIdentPairList;
    const aSQLTerm: string; const aSQLArgs: TStringArray);
var
    x: integer;
    xParamName: string;
    xIdent: TSQLParserIdentifier;
    xStoredIdent: TSQLParserStoredIdent;
begin
    x := 0;
    while (true) do
    begin
        xParamName := Format(STR_SQLTERM_PARAM_VARIABLEFORMAT, [x]);
        if Pos(xParamName, UpperCase(aSQLTerm)) = 0 then
            EXIT;

        xIdent := TSQLParserIdentifier.Create(xParamName);
        xStoredIdent := TSQLParserStoredIdent.Create(xParamName);
        if (Length(aSQLArgs) > x) then
        begin
            xIdent.Value := aSQLArgs[x];
            xStoredIdent.Value := aSQLArgs[x];
        end;
        aIdentsNeedingInput.AddItem(TSQLParserIdentPair.Create(xIdent, xStoredIdent));

        inc(x);
    end;
end;


end.
