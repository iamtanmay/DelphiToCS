{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.12.08 pk                                        TN4279    from MethodBuildCommon.pas
  07.01.09 pk  EditMethodParameters                  TN4380.1  less dependence on Parser object
  07.01.09 pk  GetIdentNamesAndValuesList            TN4380.1  from TSymbolTable
  07.01.09 wl  EditMethodParameters                  TN4380.1  Parser.EndScope was too early
  08.01.09 pk  EditMethodParameters                  TN4380.1  result set to true when no idents found
  04.03.09 pk                                        TN4232    uses GeneralTypes
  10.06.09 pk  EditMethodParameters                  TN4600    TCompilerMessageList changed to TCompilerResult
  06.07.09 pk  EditMethodParameters                  TN4585.4  aParameters changed to TStreamableKeyValueList
  04.11.09 pk                               	    TN4843    Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  01.04.10 pk  EditMethodParameters                  TN5003.2  ParserInputModal new EnterKeyNavigationOnly parameter
  23.04.10 pk  EditMethodParameters                  TN5062    New Parameter aIsBuildMode
  07.05.10 pk  Editor_EditMethodParameters           TN5092    New
  01.03.12 wl                                        TN5822   uses geändert
  14.12.12 wl                                        TN6054   Parser_InputModal, neuer Parameter: aDataCache:TMethodVariablesDataCache
  20.02.13 wl                                        TN6055   an Änderungen in ParserSymbolTable & ParserStoredIdent angepasst
  22.02.13 wl  ReadStoredIdents                      TN6094   von ParserWrapperCommon hierher, Speicherlecks bereinigt
  22.02.13 wl  GetIdentsNeedingInput                 TN6094   von ParserWrapperCommon hierher, Speicherlecks bereinigt
  22.02.13 wl  WriteStoredIdents                     TN6094   schreibt geänderte Parameter-Daten und löscht dabei nicht benötigte Parameter
  22.04.13 wl                                        TN6095   Comparer wird direkt für das Sortieren erzeugt (nicht beim Create)
  14.05.13 wl                                        TN6095   verwendet MethodVarPagesDataAdaptor
  25.06.13 wl                                        TN6178   verwendet TParserIdentifierUtils (ParserSymbolTable)
  12.08.13 wl  Editor_EditMethodParameters           TN6214   TStrArg.Create für Parameter
  ----------------------------------------------------------------------------------------------------------------------- }

unit EditMethodParams;


interface


uses
    Generics.Defaults,
    Generics.Collections,
    SysUtils,
    GeneralTypes,
    MethodTypes,
    MethodVariableTypes,
    ParserStoredIdentifier,
    ParserSymbolTable,
    BasicEditFunctions,
    ParserWrapperOptimal,
    ParserIdentDataType;

type
    EBuildException = class(Exception)
    protected
        fMethodName: string;
        fLineIndex: integer;
    public
        constructor CreateFmt(const aMethodName: string; aLineIndex: integer; const aText: string;
            const aArgs: array of const );
        property LineIndex: integer read fLineIndex;
        property MName: string read fMethodName;
    end;

    TEditMethodParams = class
    private
        class function IdentByName(const aStoredIdents: TArray<TMethodVariableRec>; const aIdentName: string)
            : TMethodVariableData;
        class procedure GetIdentNamesAndValuesList(aIdents: TList<TParserStoredIdent>;
            const aResultList: TKeyArgValueList; aInputNeededCheck: TInputNeededCheck);
        class function RequestValues(aParser: TParserWrapperOpt; const aMethodName: string;
            aParameters: TKeyArgValueList; const aShowEditor, aCheckValues: boolean;
            aEditFunctions: TBasicEditFunctions): boolean;
    protected
        class function ReadStoredIdents(const aMethodName: string): TArray<TMethodVariableRec>;
        class function ReadMethodVarPages(const aMethodName: string): TArray<TMethodVarPageRec>;
        class procedure GetIdentsNeedingInput(aParser: TParserWrapperOpt;
            aResultList: TList<TParserStoredIdent>; aCheckHasValue, aCheckNonInputIdent: boolean;
            const aStoredIdents: TArray<TMethodVariableRec>);
    public
        class function EditMethodParameters(const aMethodName: string; const aParameters: TKeyArgValueList;
            const aAllowGlobalIdents, aShowEditor, aCheckValues: boolean;
            aEditFunctions: TBasicEditFunctions): boolean;
        class function Editor_EditMethodParameters(const aSubMethodName: string;
            const aParameters: TKeyValueParamArray; out oResultParameters: TKeyValueParamArray;
            aEditFunctions: TBasicEditFunctions; aCheckValues: boolean): boolean;
    end;


implementation


uses
    Controls,
    MethBuildDataset,
    LayoutManager,
    MethodCompile,
    MethodVariablesDataAdaptor,
    MethodVarPagesDataAdaptor,
    ParserIdentifier,
    MethodGUIParsing,
    ParserValueRequest,
    TypeMapTranslator;

{ EBuildException }

constructor EBuildException.CreateFmt(const aMethodName: string; aLineIndex: integer; const aText: string;
    const aArgs: array of const );
begin
    inherited CreateFmt(aText, aArgs);
    fMethodName := aMethodName;
    fLineIndex := aLineIndex;
end;

{ TEditMethodParams }

class procedure TEditMethodParams.GetIdentNamesAndValuesList(aIdents: TList<TParserStoredIdent>;
    const aResultList: TKeyArgValueList; aInputNeededCheck: TInputNeededCheck);
var
    i: integer;
    xIdentifier: TParserIdentifier;
begin
    for i := 0 to aIdents.Count - 1 do
    begin
        xIdentifier := aIdents[i].LinkedIdent;

        if ((aInputNeededCheck = vnInputNeeded) and (not TParserIdentifierUtils.NeedsInput(xIdentifier))) or
            ((aInputNeededCheck = vnInputNotNeeded) and (TParserIdentifierUtils.NeedsInput(xIdentifier))) then
            CONTINUE;

        aResultList.Add(xIdentifier.Name, TObjectCopy<TArg>.Copy(xIdentifier.Value));
    end;
end;

class function TEditMethodParams.IdentByName(const aStoredIdents: TArray<TMethodVariableRec>;
    const aIdentName: string): TMethodVariableData;
var
    x: integer;
begin
    for x := 0 to high(aStoredIdents) do
    begin
        if SameText(aIdentName, aStoredIdents[x].VariableName) then
            EXIT(TMethodVariableUtils.MethodVariableRecToData(aStoredIdents[x]));
    end;
    EXIT(TMethodVariableUtils.GetEmptyRec());
end;

class procedure TEditMethodParams.GetIdentsNeedingInput(aParser: TParserWrapperOpt;
    aResultList: TList<TParserStoredIdent>; aCheckHasValue, aCheckNonInputIdent: boolean;
    const aStoredIdents: TArray<TMethodVariableRec>);
var
    xIdentsNeedingInput: TArray<TParserIdentifier>;
    x: integer;
    xRec: TMethodVariableData;
begin
    xIdentsNeedingInput := aParser.IdentsNeedingInput(aCheckHasValue, aCheckNonInputIdent);

    for x := 0 to high(xIdentsNeedingInput) do
    begin
        xRec := IdentByName(aStoredIdents, xIdentsNeedingInput[x].name);
        aResultList.Add(TParserStoredIdent.Create(xIdentsNeedingInput[x], xRec, x));
    end;
end;

class function TEditMethodParams.ReadMethodVarPages(const aMethodName: string): TArray<TMethodVarPageRec>;
var
    xDA: TMethodVarPagesDataAdaptor;
begin
    xDA := TMethodVarPagesDataAdaptor.Create;
    try
        EXIT(xDA.ReadRecs(aMethodName));
    finally
        FreeAndNil(xDA);
    end;
end;

class function TEditMethodParams.ReadStoredIdents(const aMethodName: string): TArray<TMethodVariableRec>;
var
    xDA: TMethodVariablesDataAdaptor;
begin
    xDA := TMethodVariablesDataAdaptor.Create;
    try
        EXIT(xDA.ReadRecs(aMethodName));
    finally
        FreeAndNil(xDA);
    end;
end;

class function TEditMethodParams.RequestValues(aParser: TParserWrapperOpt; const aMethodName: string;
    aParameters: TKeyArgValueList; const aShowEditor, aCheckValues: boolean;
    aEditFunctions: TBasicEditFunctions): boolean;
var
    xIdentsNeedingInput: TObjectList<TParserStoredIdent>;
begin
    result := false;
    xIdentsNeedingInput := TObjectList<TParserStoredIdent>.Create();
    try
        GetIdentsNeedingInput(aParser, xIdentsNeedingInput, false, true, ReadStoredIdents(aMethodName));

        if aShowEditor then
        begin
            // ask the user to input values
            result := TParserValueRequest.ShowModal(aMethodName, xIdentsNeedingInput,
                ReadMethodVarPages(aMethodName), aCheckValues, true, false, aEditFunctions) = mrOK;
        end;

        GetIdentNamesAndValuesList(xIdentsNeedingInput, aParameters, vnInputNotNeeded);
    finally
        FreeAndNil(xIdentsNeedingInput);
    end;
end;

class function TEditMethodParams.EditMethodParameters(const aMethodName: string;
    const aParameters: TKeyArgValueList; const aAllowGlobalIdents, aShowEditor, aCheckValues: boolean;
    aEditFunctions: TBasicEditFunctions): boolean;
var
    xParser: TParserWrapperOpt;
    xMethBuildDatasets: TObjectList<TMethBuildDataset>;
    xCompilerResult: TCompilerResult;
begin
    if (aMethodName = '') then
        EXIT(false);

    result := false;
    xMethBuildDatasets := TObjectList<TMethBuildDataset>.Create();
    try
        xParser := TParserWrapperOpt.Create(aMethodName);
        try
            xCompilerResult := TCompilerResult.Create();
            try
                TMethodCompile.InstPrepareSteps(aMethodName, xParser, xMethBuildDatasets, xCompilerResult);
                if xCompilerResult.ErrorMessages.Count > 0 then
                    raise EBuildException.CreateFmt(xCompilerResult.ErrorMessages[0].MName,
                        xCompilerResult.ErrorMessages[0].LineIndex,
                        xCompilerResult.ErrorMessages[0].Text, []);
            finally
                xCompilerResult.Free;
            end;

            ASSERT(xMethBuildDatasets.Count > 0);

            if not aAllowGlobalIdents then
            begin
                xParser.SymbolTable.GlobalList.Clear();
            end;

            xParser.BeginScope(xMethBuildDatasets[0].IdentList);
            try
                xParser.RemoveNonInputIdents();

                xParser.AddIdentifiersUsingList(aParameters, [itLocal]);
                aParameters.Clear;

                result := RequestValues(xParser, aMethodName, aParameters, aShowEditor, aCheckValues,
                    aEditFunctions);
            finally
                xParser.EndScope();
            end;
        finally
            xParser.Free;
        end;
    finally
        xMethBuildDatasets.Free;
    end;
end;

class function TEditMethodParams.Editor_EditMethodParameters(const aSubMethodName: string;
    const aParameters: TKeyValueParamArray; out oResultParameters: TKeyValueParamArray;
    aEditFunctions: TBasicEditFunctions; aCheckValues: boolean): boolean;
var
    xParameters: TKeyArgValueList;
    x: integer;
begin
    xParameters := TKeyArgValueList.Create(true);
    try
        for x := 0 to high(aParameters) do
        begin
            xParameters.Add(aParameters[x].Key, TStrArg.Create(aParameters[x].Value));
        end;

        result := EditMethodParameters(aSubMethodName, xParameters, false, true, aCheckValues,
            aEditFunctions);

        SetLength(oResultParameters, xParameters.Count);
        for x := 0 to xParameters.Count - 1 do
        begin
            oResultParameters[x].Key := xParameters[x].Key;
            oResultParameters[x].Value := TParserStoredIdent.ValueToStr(xParameters[x].Value);
        end;

    finally
        xParameters.Free;
    end;
end;


end.
