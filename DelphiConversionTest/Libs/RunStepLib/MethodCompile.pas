{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk), Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  19.09.08 pk  ReadMethod                            TN4215     New
  23.09.08 wl                                        TN4236   Verweis auf MethodFieldnames entfernt
  06.10.08 pk                                        TN4258   no longer use methodguiparsing to parse suboptions
  15.10.08 pk  SettingToFieldRecurse                 TN4258   IsConstant is checked for each sub-setting
  06.11.08 pk  TMethodCompile                        TN4279   new fEvalTable
  07.11.08 pk  StrToField                            TN4306   new IsConstant param
  08.12.08 pk  FindSubMethodNamesInParams            TN4279   new: Compile methods used in events
  09.12.08 pk                                        TN4279   uses changed
  07.01.09 pk  Create                                TN4380.1 Takes Parser as Param instead of creating it
  20.02.09 wl  SettingToFieldRecurse                 TN4438   Description statt Caption
  06.04.09 pk  CompileDisplayEvents                  TN4503   New
  10.06.09 pk                                        TN4600   TCompilerMessageList changed to TCompilerResult
  10.08.09 wl                                        TN4702   Strings werden jetzt direkt geladen
  27.08.09 pk                                        TN4753   uses ErrorManager removed
  04.11.09 pk                                	    TN4843   Replace TObjectList, TStringList, TStrings with Generics Lists and TStringArray
  24.11.09 ts  CompileLiqHandlingParams              TN4741   new: Compile methods used in liquid handling parameters
  22.06.10 pk  PrepareControlFlowForDataset          TN5088   now based on TControlFlowBlockRunStepBuilder
  29.10.10 pk  CompileRestartEvents                  TN5320   New
  24.01.11 wl  CompileLiqHandlingParams              TN5442   Bugfix: Bei UseWashMethod ohne Name wurden alle Methoden geprüft
  05.03.11 wl                                        TN5472   RelatedItemParam ersetzt NameParam
  29.06.11 wl  TMethodCompile.StrToField             TN5618   Wenn EvaluateIndex: Index wird evaluiert, der Rest nicht!
  15.11.11 wl  CompileGetAndPutTipMethodParams       TN5738   neu: Für GetTip-/PutTip-Methoden
  23.11.12 wl                                        TN6015.1 Es gibt keine Instanz mehr von TDisplayComponentSettingsManager
  14.12.12 wl  InstCompile                           TN6054   ParserWrapper: geändertes Create
  22.04.13 wl                                        TN6045   uses Generics.Collections
  25.06.13 wl  PrepareControlFlowForDataset          TN6178  TControlFlowReturnRunStepBuilder, Relative Address: Adresse der letzten Zeile
  30.08.13 wl  InstCompile                           TN6236   verwendet TMethodSettingsDataAdaptor
  ----------------------------------------------------------------------------------------------------------------------- }

unit MethodCompile;


interface


uses
    Generics.Collections,
    MethodDataAdaptorExt,
    ParserWrapperOptimal,
    MethBuildDataset,
    MethodStep,
    RunStepBuilder,
    ParseField,
    CustomSetting,
    ControlFlowRunStepBuilder,
    ParserEvalTable;

type
    TCompilerMessage = class
    protected
        fMethodName: string;
        fLineIndex: integer;
        fText: string;
    public
        constructor Create(const aMethodName: string; aLineIndex: integer; const aText: string);
        property LineIndex: integer read fLineIndex;
        property MName: string read fMethodName;
        property Text: string read fText;
    end;

    TCompilerResult = class
    protected
        fErrorMessages: TObjectList<TCompilerMessage>;
        fTotalMethods: integer;
    public
        constructor Create();
        destructor Destroy(); override;
        property ErrorMessages: TObjectList<TCompilerMessage>read fErrorMessages;
        property TotalMethods: integer read fTotalMethods write fTotalMethods;
    end;

    TMethodCompile = class
    strict private
        fIsDesignTimeBuild: boolean;
        fCallStack: TStack<string>;
        fMethodDataAdaptor: TMethodDataAdaptorExt;
        fMethBuildDatasets: TObjectList<TMethBuildDataset>;
        fParser: TParserWrapperOpt;
        fEvalTable: TParserEvalTable;
        fCompilerResult: TCompilerResult;
        fCompileSubMethods: boolean;

        procedure CreateBuildFieldIntern(aParser: TParserWrapperOpt; aMethodStep: TMethodStep);
        procedure SettingToFieldRecurse(aParser: TParserWrapperOpt; aSetting: TCustomSetting);
        function StrToField(aParser: TParserWrapperOpt; const aFieldName, aUnparsedValue: string;
            const aEvaluateType: TCustomSettingEvaluateType): TParseField;
        procedure PrepareControlFlow(aMethBuildDatasets: TObjectList<TMethBuildDataset>);
        procedure PrepareControlFlowForDataset(aMethBuildDataset: TMethBuildDataset);
        procedure ReadMethod(aMethodName: string; aRunStepBuilderList: TObjectList<TRunStepBuilder>);
        function GetLineDescription(const aLine: TControlFlowBlockRunStepBuilder): string;

        procedure CompileMethod(const aMainMethodName: string);
        procedure CompileDisplayEvents(const aMainDisplayComponentName: string);
        procedure CompileLiqHandlingParams;
        procedure CompileGetAndPutTipMethodParams;
        procedure CompileRestartEvents(const aRestartEventName: string);
        function CreateMethBuildDataset(const aMethodName: string): TMethBuildDataset;
        function FindMethBuildDataset(const aMethodName: string): TMethBuildDataset;

        procedure CompileSubMethod(const aSubMethodName: string);
        procedure FindSubMethodNamesInParams(aSetting: TCustomSetting);
        procedure CompileAnySubMethods(aMethBuildDataset: TMethBuildDataset);
        procedure Clear();
        procedure PrepareSteps(aMethBuildDataset: TMethBuildDataset);
    public
        constructor Create(aMethBuildDatasets: TObjectList<TMethBuildDataset>;
            const aParser: TParserWrapperOpt; const aEvalTable: TParserEvalTable;
            aCompilerResult: TCompilerResult; aIsDesignTimeBuild: boolean; aCompileSubMethods: boolean);
        destructor Destroy(); override;

        class procedure InstPrepareSteps(const aMethodName: string; const aParser: TParserWrapperOpt;
            aMethBuildDatasets: TObjectList<TMethBuildDataset>; aCompilerResult: TCompilerResult);

        class procedure InstCompile(const aMethodName: string;
            aMethBuildDatasets: TObjectList<TMethBuildDataset>; const aEvalTable: TParserEvalTable;
            aCompilerResult: TCompilerResult; aIsDesignTimeBuild: boolean);
        class procedure InstCompileJustCheck(const aMethodName: string; aCompilerResult: TCompilerResult);
    end;


implementation


uses
    SysUtils,
    MethodDataAdaptor,
    ParserTree,
    MethBuildField,
    ParserToken,
    RunStepBuilderTypeDictionary,
    MethodStepSettingRunStart,
    CustomLeafSettings,
    TipTypeDataAdaptor,
    CommonTypes,
    MethodSettingsDataAdaptor,
    DisplayComponentSettingsManager,
    LiqHDataAdaptor,
    LiqHTypes;

{ TCompilerMessage }

constructor TCompilerMessage.Create(const aMethodName: string; aLineIndex: integer; const aText: string);
begin
    inherited Create();

    fMethodName := aMethodName;
    fLineIndex := aLineIndex;
    fText := aText;
end;

{ TCompilerResult }

constructor TCompilerResult.Create;
begin
    inherited Create();

    fErrorMessages := TObjectList<TCompilerMessage>.Create(true);
end;

destructor TCompilerResult.Destroy;
begin
    fErrorMessages.Free;
    inherited;
end;

{ TMethodCompile }

constructor TMethodCompile.Create(aMethBuildDatasets: TObjectList<TMethBuildDataset>;
    const aParser: TParserWrapperOpt; const aEvalTable: TParserEvalTable; aCompilerResult: TCompilerResult;
    aIsDesignTimeBuild, aCompileSubMethods: boolean);
begin
    inherited Create();

    fMethBuildDatasets := aMethBuildDatasets;
    fEvalTable := aEvalTable;
    fIsDesignTimeBuild := aIsDesignTimeBuild;
    fCompilerResult := aCompilerResult;
    fCompileSubMethods := aCompileSubMethods;
    fCallStack := TStack<string>.Create();
    fMethodDataAdaptor := TMethodDataAdaptorExt.Create();
    fParser := aParser;
end;

procedure TMethodCompile.Clear();
begin
    fCallStack.Clear();
end;

destructor TMethodCompile.Destroy();
begin
    Clear();
    fMethodDataAdaptor.Free;
    fCallStack.Free;
    inherited;
end;

function TMethodCompile.StrToField(aParser: TParserWrapperOpt; const aFieldName: string;
    const aUnparsedValue: string; const aEvaluateType: TCustomSettingEvaluateType): TParseField;
var
    xTokens: TTokenList;
    xParseTree: TParseTree;
    xManipulatedValue: string;
begin
    if aUnparsedValue = '' then
    begin
        result := TMethBuildConstField.Create(aFieldName, '');
        EXIT;
    end;

    xTokens := aParser.Tokenize(aUnparsedValue);
    try
        if (xTokens.Count = 2) and (xTokens[0] is TConstToken) then
        begin
            // ASSERT( ( xTokens[ 0 ] as TConstToken ).Value = xUnparsedVal ); does not work for quoted strings
            result := TMethBuildConstField.Create(aFieldName, (xTokens[0] as TConstToken).Value);
            EXIT;
        end
        else if aEvaluateType = TCustomSettingEvaluateType.Constant then
        begin
            // Z.B. für Comments und Iterate-Feld
            result := TMethBuildConstField.Create(aFieldName, aUnparsedValue);
            EXIT;
        end
        else if aEvaluateType = TCustomSettingEvaluateType.EvaluateIndex then
        begin
            if (xTokens[0].TokenType = TTokenType.ttArrayVar) and
                (xTokens[1].TokenType = TTokenType.ttSquareBracketOpen) then
            begin
                // xToken[0] ist der Name der Variable und darf nicht geparst werden
                // Das machen wir mit einem Trick: Wir setzen "" um den ersten Wert
                xManipulatedValue := '"' + xTokens[0].Text + xTokens[1].Text + '"&' +
                    Copy(aUnparsedValue, Length(xTokens[0].Text) + 2,
                    Length(aUnparsedValue) - Length(xTokens[0].Text) - 2) + '&"]"';
                FreeAndNil(xTokens);
                xTokens := aParser.Tokenize(xManipulatedValue);
            end
            else
            begin
                // normale Variable ohne Index
                result := TMethBuildConstField.Create(aFieldName, aUnparsedValue);
                EXIT;
            end;
        end;

        result := TMethBuildEvaluableField.Create(aFieldName, aUnparsedValue, fEvalTable);
        xParseTree := TParseTree.Create(xTokens);
        try
            aParser.ParseTokens(xParseTree);
            xParseTree.BuildEvalResult((result as TMethBuildEvaluableField).ParseResult as TParseEvalResult);
        finally
            xParseTree.Free;
        end;
    finally
        xTokens.Free;
    end;
end;

procedure TMethodCompile.SettingToFieldRecurse(aParser: TParserWrapperOpt; aSetting: TCustomSetting);
var
    x: integer;
    xUnparsedValue: string;
    xFieldName: string;
    xCompositeSetting: TCustomCompositeSetting;
    xChildSetting: TCustomSetting;
begin
    xUnparsedValue := aSetting.Value;
    xFieldName := aSetting.Description;

    // base case 1: Not a list
    if not(aSetting is TCustomCompositeSetting) then
    begin
        try
            aSetting.ParseField := StrToField(aParser, xFieldName, xUnparsedValue, aSetting.EvaluateType);
        except
            on E: Exception do
                raise Exception.CreateFmt('Could not parse [%s]. Details: %s', [xUnparsedValue, E.Message]);
        end;
    end
    else
    begin
        // recursive case:
        xCompositeSetting := (aSetting as TCustomCompositeSetting);
        aSetting.ParseField := TMethBuildCompositeField.Create(xFieldName, xUnparsedValue);
        for x := 0 to xCompositeSetting.Params.Count - 1 do
        begin
            xChildSetting := xCompositeSetting.Params[x];
            SettingToFieldRecurse(aParser, xChildSetting);
            (aSetting.ParseField as TMethBuildCompositeField).AddField(xChildSetting.ParseField);
        end;
    end;
end;

procedure TMethodCompile.CreateBuildFieldIntern(aParser: TParserWrapperOpt; aMethodStep: TMethodStep);
begin
    SettingToFieldRecurse(aParser, aMethodStep.Settings);
end;

procedure TMethodCompile.PrepareSteps(aMethBuildDataset: TMethBuildDataset);
var
    x: integer;
    xStep: TRunStepByMethodStepBuilder;
begin
    fParser.ParseBeginScope(aMethBuildDataset.IdentList);

    // Jeden MethodStep lesen
    for x := 0 to aMethBuildDataset.Steps.Count - 1 do
    begin
        if not Assigned(aMethBuildDataset.Steps[x]) then
            CONTINUE;
        xStep := aMethBuildDataset.Steps[x] as TRunStepByMethodStepBuilder;
        // xStep.LineIndex := x;
        try
            CreateBuildFieldIntern(fParser, xStep.MStep);
        except
            on E: Exception do
                fCompilerResult.ErrorMessages.Add(TCompilerMessage.Create(aMethBuildDataset.MName,
                    xStep.LineIndex, E.Message));
        end;
    end;

    fParser.ParseEndScope();
end;

procedure TMethodCompile.FindSubMethodNamesInParams(aSetting: TCustomSetting);
var
    xCompositeSetting: TCustomCompositeSetting;
    xSubMethodName: string;
    x: integer;
begin
    if aSetting is TMethodStepSetting_RunStartEvent then
    begin
        xSubMethodName := (aSetting as TMethodStepSetting_RunStartEvent).RunName.Value;
        if xSubMethodName <> '' then
            CompileSubMethod(xSubMethodName);
    end
    else if aSetting is TCustomCompositeSetting then
    begin
        xCompositeSetting := (aSetting as TCustomCompositeSetting);
        for x := 0 to xCompositeSetting.Params.Count - 1 do
        begin
            FindSubMethodNamesInParams(xCompositeSetting.Params[x]);
        end;
    end;
end;

procedure TMethodCompile.CompileAnySubMethods(aMethBuildDataset: TMethBuildDataset);
var
    xStep: TRunStepByMethodStepBuilder;
    x: integer;
    xRelatedItemParam: TCustomSetting;
    xSubMethodName: string;
begin
    // add the submethod name to the call stack to avoid circular calls
    fCallStack.Push(aMethBuildDataset.MName);
    try
        for x := 0 to aMethBuildDataset.Steps.Count - 1 do
        begin
            if not Assigned(aMethBuildDataset.Steps[x]) then
                CONTINUE;
            xStep := aMethBuildDataset.Steps[x] as TRunStepByMethodStepBuilder;
            try
                FindSubMethodNamesInParams(xStep.MStep.MainSubOptionSetting);

                xRelatedItemParam := xStep.MStep.RelatedItemParam;
                if (xRelatedItemParam is TCustomSetting_MethodName) then
                begin
                    xSubMethodName := xRelatedItemParam.Value;
                    CompileSubMethod(xSubMethodName);
                end;

            except
                on E: Exception do
                begin
                    fCompilerResult.ErrorMessages.Add(TCompilerMessage.Create(aMethBuildDataset.MName,
                        xStep.LineIndex, E.Message));
                end;
            end;
        end;
    finally
        fCallStack.Pop();
    end;
end;

procedure TMethodCompile.ReadMethod(aMethodName: string; aRunStepBuilderList: TObjectList<TRunStepBuilder>);
var
    xDA: TMethodDataAdaptor;
    xMethodRec: TMethodRec;
    xStep: TRunStepBuilder;
    xLineIndex: integer;
begin
    xDA := TMethodDataAdaptor.Create();
    try
        xDA.SelectAndOpenMethod(aMethodName, false, true);
        try
            xLineIndex := -1;
            while not xDA.DataProvider.Eof do
            begin

                xMethodRec := xDA.ReadMethodrecFromDataSet(xDA.DataProvider);
                xDA.DataProvider.Next;
                Inc(xLineIndex);

                if (xMethodRec.Inactive) or
                    (not TRunStepBuilderTypeDictionary.Instance.IsActionExecutable(xMethodRec.Action)) then
                begin
                    aRunStepBuilderList.Add(nil);
                    CONTINUE;
                end;

                // Create RunStepBuilder
                xStep := TRunStepBuilderTypeDictionary.Instance.CreateRunStepBuilderWithDefDataLink
                    (xMethodRec.Action);
                if Assigned(xStep) then
                begin
                    xStep.MStep.M_Seq := xMethodRec.Seq;
                    xStep.MStep.M_Options := xMethodRec.Options;
                    xStep.MStep.M_Comments := xMethodRec.Comment;
                    xStep.MStep.InactiveAsBool := xMethodRec.Inactive;
                    xStep.LineIndex := xLineIndex;
                end
                else
                begin
                    fCompilerResult.ErrorMessages.Add(TCompilerMessage.Create(aMethodName, xLineIndex,
                        'Method step ' + xMethodRec.Action + ' does not exist!'));
                end;

                aRunStepBuilderList.Add(xStep);
            end;
        finally
            xDA.Close();
        end;
    finally
        xDA.Free;
    end;
end;

function TMethodCompile.CreateMethBuildDataset(const aMethodName: string): TMethBuildDataset;
begin
    // if not fMethodDataAdaptor.MethodExists(aMethodName) then
    // raise Exception.CreateFmt('Method [%s] does not exist', [aMethodName]);

    result := TMethBuildDataset.Create(aMethodName);

    // neu: Methode lesen
    ReadMethod(aMethodName, result.Steps);

    // Build vorbereiten
    PrepareSteps(result);
end;

function TMethodCompile.FindMethBuildDataset(const aMethodName: string): TMethBuildDataset;
var
    x: integer;
begin
    for x := 0 to fMethBuildDatasets.Count - 1 do
    begin
        if (fMethBuildDatasets[x].MName = aMethodName) then
            EXIT(fMethBuildDatasets[x]);
    end;

    EXIT(nil);
end;

procedure TMethodCompile.CompileSubMethod(const aSubMethodName: string);
var
    xMethBuildDataset: TMethBuildDataset;
    x: string;
begin
    for x in fCallStack do
    begin
        if SameText(x, aSubMethodName) then
            raise Exception.CreateFmt('Circular call detected for method [%s]', [aSubMethodName]);
    end;

    // does a list of parsed method steps already exist for this submethod?
    xMethBuildDataset := FindMethBuildDataset(aSubMethodName);
    if Assigned(xMethBuildDataset) then
        EXIT;

    // create a new list of parsed method steps
    xMethBuildDataset := CreateMethBuildDataset(aSubMethodName);
    fMethBuildDatasets.Add(xMethBuildDataset);

    // Check to see if there are any submethods
    if fCompileSubMethods then
    begin
        CompileAnySubMethods(xMethBuildDataset);
    end;
end;

procedure TMethodCompile.CompileMethod(const aMainMethodName: string);
begin
    CompileSubMethod(aMainMethodName);
end;

procedure TMethodCompile.CompileDisplayEvents(const aMainDisplayComponentName: string);
var
    xEventNames: TArray<string>;
    x: integer;
begin
    if aMainDisplayComponentName = '' then
        EXIT;

    xEventNames := TDisplayComponentSettingsManager.GetAllDisplayComponentEventNames
        (aMainDisplayComponentName);
    for x := 0 to Length(xEventNames) - 1 do
    begin
        self.CompileSubMethod(xEventNames[x]);
    end;
end;

procedure TMethodCompile.CompileGetAndPutTipMethodParams;
var
    xDA: TTipTypeDataAdaptor;
    xTipTypeNames: TArray<string>;
    x: integer;
    xTipType: TTipType;
begin
    xDA := TTipTypeDataAdaptor.Create;
    try
        xTipTypeNames := xDA.ReadAllNames;
        for x := 0 to high(xTipTypeNames) do
        begin
            xDA.ReadTipType(xTipTypeNames[x], xTipType);

            if (xTipType.MethodNameGetTip <> '') then
                self.CompileSubMethod(xTipType.MethodNameGetTip);
            if (xTipType.MethodNamePutTip <> '') then
                self.CompileSubMethod(xTipType.MethodNamePutTip);
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TMethodCompile.CompileLiqHandlingParams();
var
    xLiqHNames: TArray<string>;
    x: integer;
    xLiqHandlingRec: TLiqHandlingRec;
    xDA: TLiqHDataAdaptor;
begin
    xDA := TLiqHDataAdaptor.Create;
    try
        xLiqHNames := xDA.ReadAllNames;
        for x := 0 to high(xLiqHNames) do
        begin
            xDA.ReadRec(xLiqHNames[x], xLiqHandlingRec);

            if (xLiqHandlingRec.Wash.Flag <> 0) // wird überhaupt gewaschen?
                and (xLiqHandlingRec.Wash.UseMethod) and (xLiqHandlingRec.Wash.WashMethodName <> '') then
                self.CompileSubMethod(xLiqHandlingRec.Wash.WashMethodName);
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

procedure TMethodCompile.CompileRestartEvents(const aRestartEventName: string);
begin
    if aRestartEventName = '' then
        EXIT;

    self.CompileSubMethod(aRestartEventName);
end;

function TMethodCompile.GetLineDescription(const aLine: TControlFlowBlockRunStepBuilder): string;
begin
    result := aLine.MStep.GetSummary();
end;

procedure TMethodCompile.PrepareControlFlowForDataset(aMethBuildDataset: TMethBuildDataset);
var
    i: integer;
    xLine, xConditionLine: TControlFlowBlockRunStepBuilder;
    xContFlowStack: TStack<TControlFlowBlockRunStepBuilder>;

begin
    xContFlowStack := TStack<TControlFlowBlockRunStepBuilder>.Create();
    try
        for i := 0 to aMethBuildDataset.Steps.Count - 1 do
        begin
            if not(aMethBuildDataset.Steps[i] is TControlFlowBlockRunStepBuilder) then
                CONTINUE;
            xLine := aMethBuildDataset.Steps[i] as TControlFlowBlockRunStepBuilder;

            if xLine is TControlFlowBlockBeginRunStepBuilder then
            begin
                xContFlowStack.Push(xLine);
            end
            else if xLine is TControlFlowBlockEndRunStepBuilder then
            begin
                if xContFlowStack.Count = 0 then
                begin

                    fCompilerResult.ErrorMessages.Add(TCompilerMessage.Create(aMethBuildDataset.MName,
                        xLine.LineIndex,
                        Format('End control flow symbol [%s] was found without a begin control flow symbol',
                        [GetLineDescription(xLine)])));
                    CONTINUE;
                end;
                xConditionLine := xContFlowStack.Peek();
                if (not(xConditionLine is TControlFlowBlockBeginRunStepBuilder)) or
                    (not(xConditionLine as TControlFlowBlockBeginRunStepBuilder)
                    .IsMatchingControlFlow(xLine)) then
                begin
                    fCompilerResult.ErrorMessages.Add(TCompilerMessage.Create(aMethBuildDataset.MName,
                        xLine.LineIndex,
                        Format('End control flow symbol [%s] does not match begin control flow symbol [%s]',
                        [GetLineDescription(xLine), GetLineDescription(xConditionLine)])));
                    CONTINUE;
                end;
                xContFlowStack.Pop;
                ASSERT(Assigned(xConditionLine));
                xConditionLine.RelativeAddress := xLine.LineIndex;
                xLine.RelativeAddress := xConditionLine.LineIndex;
            end
            else if (xLine is TControlFlowReturnRunStepBuilder) then
            begin
                // Relative Address: Adresse der letzten Zeile
                xLine.RelativeAddress := aMethBuildDataset.Steps[aMethBuildDataset.Steps.Count - 1].LineIndex;
            end;
        end;

        // if there is something still left on stack it is an error
        if xContFlowStack.Count > 0 then
        begin
            xLine := xContFlowStack.Peek();
            fCompilerResult.ErrorMessages.Add(TCompilerMessage.Create(aMethBuildDataset.MName,
                xLine.LineIndex,
                Format('Begin control flow symbol [%s] was found without a matching end control flow symbol',
                [GetLineDescription(xLine)])));
        end;

    finally
        xContFlowStack.Free;
    end;
end;

procedure TMethodCompile.PrepareControlFlow(aMethBuildDatasets: TObjectList<TMethBuildDataset>);
var
    x: integer;
begin
    for x := 0 to aMethBuildDatasets.Count - 1 do
    begin
        PrepareControlFlowForDataset(aMethBuildDatasets[x]);
    end;
end;

class procedure TMethodCompile.InstCompile(const aMethodName: string;
    aMethBuildDatasets: TObjectList<TMethBuildDataset>; const aEvalTable: TParserEvalTable;
    aCompilerResult: TCompilerResult; aIsDesignTimeBuild: boolean);
var
    xDA: TMethodSettingsDataAdaptor;
    xRec: TMethodSettingsRec;
    xParser: TParserWrapperOpt;
    xMethodCompile: TMethodCompile;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        xRec := xDA.ReadRec(aMethodName);
    finally
        FreeAndNil(xDA);
    end;

    xParser := TParserWrapperOpt.Create(aMethodName);
    try
        xMethodCompile := TMethodCompile.Create(aMethBuildDatasets, xParser, aEvalTable, aCompilerResult,
            aIsDesignTimeBuild, true);
        try
            xMethodCompile.CompileMethod(aMethodName);
            xMethodCompile.CompileDisplayEvents(xRec.DisplayComponentName);
            xMethodCompile.CompileLiqHandlingParams;
            xMethodCompile.CompileGetAndPutTipMethodParams;
            xMethodCompile.CompileRestartEvents(xRec.RestartEventName);
            xMethodCompile.PrepareControlFlow(aMethBuildDatasets);
            aCompilerResult.TotalMethods := aMethBuildDatasets.Count;
        finally
            xMethodCompile.Free;
        end;
    finally
        xParser.Free;
    end;
end;

class procedure TMethodCompile.InstCompileJustCheck(const aMethodName: string;
    aCompilerResult: TCompilerResult);
var
    xMethBuildDatasets: TObjectList<TMethBuildDataset>;
begin
    xMethBuildDatasets := TObjectList<TMethBuildDataset>.Create();
    try
        TMethodCompile.InstCompile(aMethodName, xMethBuildDatasets, nil, aCompilerResult, true);
    finally
        xMethBuildDatasets.Free;
    end;
end;

class procedure TMethodCompile.InstPrepareSteps(const aMethodName: string; const aParser: TParserWrapperOpt;
    aMethBuildDatasets: TObjectList<TMethBuildDataset>; aCompilerResult: TCompilerResult);
var
    xMethodCompile: TMethodCompile;
begin
    xMethodCompile := TMethodCompile.Create(aMethBuildDatasets, aParser, nil, aCompilerResult, true, false);
    try
        xMethodCompile.CompileMethod(aMethodName);
    finally
        xMethodCompile.Free;
    end;
end;


end.
