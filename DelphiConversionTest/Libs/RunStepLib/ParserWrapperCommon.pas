{ --------------------------------------------------------------------------------------------------
  Copyright © 2005 Zinsser Analytic GmbH - All rights reserved.
  Author       : pk
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  07.06.05 pk                               TN2449 Initial revision
  05.07.05 pk  RemoveNonInputIdents         TN2494 New : remove idents from the list which do not require use input
  05.07.05 pk  GetIdentsNeedingInput        TN2494 New parmas : checkhasvalue, checkNonInput
  25.08.05 pk  LoadStoredIdents             TN2547 Also read in Runtime variables
  25.08.05 pk  SetupRunVars                 TN2547 Initialize RunVar readerwriter
  14.11.05 pk  ContainsVariable             TN2720 New
  18.11.05 pk  GetIdentsNeedingInput        TN2779 Write runtime variables to db and remove from resultlist
  18.11.05 pk  GetIdentsNeedingInput        TN2779 call ReaderWriter.WriteValue only if ReaderWriter is assigned
  27.04.06 wl  SetParamList                 TN3072    Neue Identifier sind jetzt vom Typ idtVariant
  05.05.06 pk  SetupRunVars                 TN3084  requires Priority
  05.07.06 pk  LoadStoredIdents             TN3181  new parameter to determine whether the method parameters should be read from settings
  21.07.06 pk  GetIdentsNeedingInput        TN3583  Do not free identpair item, it will be freed with deleteitem
  27.08.07 pk                               TN3788  reference to New ParserStoredIdentifier.Pas unit
  03.07.08 wl                                         TN4157
  06.08.08 pk  LoadStoredIdentsForReaderWriter        TN4165.1 calls ReadIntoCache
  15.10.08 pk  AddIdentifiersUsingList      TN4258  List changed to keyvalue paramarray
  06.11.08 pk                               TN4279   uses ParserIdentDataType
  07.01.09 pk                               TN4380.1 various functions removed
  04.03.09 pk                               TN4232    uses GeneralTypes
  10.08.09 wl                               TN4702   Strings werden jetzt direkt geladen
  04.11.09 pk                               TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  07.05.10 pk                               TN5092   changes needed for new array type
  01.03.12 wl                               TN5822   TArg statt TAttrValue
  14.12.12 wl                               TN6054   verwendet MethodVariablesDataCache statt ParserIdentReaderWriter
  14.12.12 wl  Create                       TN6054   benötigt MethodName
  20.02.13 wl                               TN6055   an Änderungen in ParserSymbolTable & ParserStoredIdent angepasst
  22.02.13 wl  CreateStoredIdents           TN6094   --> EditMethodParams
  22.02.13 wl  GetIdentsNeedingInput        TN6094   --> EditMethodParams
  25.06.13 wl                               TN6178   interne Variablen (mit #) abgeschafft
  -------------------------------------------------------------------------------------------------- }

unit ParserWrapperCommon;


interface


uses
    Generics.Collections,
    MethodTypes,
    InterfacedNoRef,
    ParserIdentifier,
    ParserSymbolTable,
    ParserIdentDataType;

type
    TParserWrapperCommon = class(TInterfacedNoRef)
    private
        fMethodName: string;
        fSymbolTable: TSymbolTable;
        function IndentListByType(aIdentType: TParserIdentType): TParserIdentifierList;

        function IdentListByIdentName(const aIdentName: string; out oList: TParserIdentifierList)
            : TParserIdentType;
    protected
        procedure AddIdentToList(const aName: string; const aValue: TArg); overload;
        procedure AddIdentToList(const aName: string); overload;
    public
        destructor Destroy(); override;
        constructor Create(const aMethodName: string);

        function IdentsNeedingInput(aCheckHasValue, aCheckNonInputIdent: boolean): TArray<TParserIdentifier>;
        procedure AddIdentifiersUsingList(const aParams: TKeyArgValueList; aAllowedTypes: TParserIdentTypes);
        procedure RemoveNonInputIdents();

        property SymbolTable: TSymbolTable read fSymbolTable;
    end;


implementation


uses
    SysUtils,
    MethodVariablesDataAdaptor,
    MethodVariableTypes,
    TypeMapTranslator;

{ TParserWrapperCommon }

constructor TParserWrapperCommon.Create(const aMethodName: string);
begin
    inherited Create;

    fMethodName := aMethodName;
    fSymbolTable := TSymbolTable.Create();
end;

destructor TParserWrapperCommon.Destroy();
begin
    fSymbolTable.Free;

    inherited;
end;

function TParserWrapperCommon.IndentListByType(aIdentType: TParserIdentType): TParserIdentifierList;
begin
    result := fSymbolTable.GlobalList;

    if aIdentType = itLocal then
    begin
        if Assigned(fSymbolTable.LocalList) then
            result := fSymbolTable.LocalList;
    end
end;

function TParserWrapperCommon.IdentListByIdentName(const aIdentName: string; out oList: TParserIdentifierList)
    : TParserIdentType;
begin
    oList := nil;
    result := TParserIdentifier.GetIdentType(aIdentName);
    oList := IndentListByType(result);
    ASSERT(Assigned(oList), Format('IdentList not found for %s', [aIdentName]));
end;

procedure TParserWrapperCommon.AddIdentToList(const aName: string);
begin
    AddIdentToList(aName, nil);
end;

procedure TParserWrapperCommon.AddIdentToList(const aName: string; const aValue: TArg);
// --------------------------------------------------------------------------------------------------
// Creates a new identifier called aName with the value aValue in either the local or global list
// depending on how the identifier is named
// If the identifier already exists, just sets the value to aValue
var
    xIdentList: TParserIdentifierList;
begin
    IdentListByIdentName(aName, xIdentList);
    ASSERT(Assigned(xIdentList));
    xIdentList.AddIdentifier(aName, aValue);
end;

procedure TParserWrapperCommon.AddIdentifiersUsingList(const aParams: TKeyArgValueList;
    aAllowedTypes: TParserIdentTypes);
// --------------------------------------------------------------------------------------------------
// for those identifier in the aList.Names which have a type that is in aAllowedTypes set the values
// to the given values in aList.Values
var
    i: integer;
    xIdentName: string;
    xIdentType: TParserIdentType;
begin
    for i := 0 to aParams.Count - 1 do
    begin
        xIdentName := aParams[i].Key;
        xIdentType := TParserIdentifier.GetIdentType(xIdentName);
        if not(xIdentType in aAllowedTypes) then
            CONTINUE;
        AddIdentToList(xIdentName, TObjectCopy<TArg>.Copy(aParams[i].Value));
    end;
end;

function TParserWrapperCommon.IdentsNeedingInput(aCheckHasValue, aCheckNonInputIdent: boolean)
    : TArray<TParserIdentifier>;
begin
    EXIT(fSymbolTable.IdentsNeedingInput(aCheckHasValue, aCheckNonInputIdent));
end;

procedure TParserWrapperCommon.RemoveNonInputIdents();
begin
    fSymbolTable.RemoveNonInputIdents();
end;


end.
