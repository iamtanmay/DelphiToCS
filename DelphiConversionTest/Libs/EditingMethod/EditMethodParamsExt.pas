{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  22.04.13 wl                                      TN6095   Initial Revision
  14.05.13 wl                                      TN6095   verwendet MethodVarPagesDataAdaptor
  ----------------------------------------------------------------------------------------------------------- }

unit EditMethodParamsExt;


interface


uses
    Generics.Collections,
    EditMethodParams,
    ParserWrapperOptimal,
    ParserStoredIdentifier,
    MethodVariableTypes,
    MethodTypes;

type
    TEditMethodParamsExt = class(TEditMethodParams)
    private
        class procedure WriteStoredIdents(const aMethodName: string;
            aIdents: TObjectList<TParserStoredIdent>);
        class procedure WriteMethodVarPages(const aMethodName: string;
            const aPages: TArray<TMethodVarPageRec>);
    public
        class function EditValuesInRunner: boolean;
        class procedure RequestStoredValues(aParser: TParserWrapperOpt; const aMethodName: string);
        class procedure EditMethodStoredParameters(const aMethodName: string);
    end;


implementation


uses
    SysUtils,
    Controls,
    Generics.Defaults,
    CommonTypes,
    MethodVariablesEditor,
    MethodCompile,
    MethodVariablesDataAdaptor,
    MethodVarPagesDataAdaptor,
    MethBuildDataset,
    ParserIdentifier,
    AppSettings;

{ TEditMethodParamsExt }

class function TEditMethodParamsExt.EditValuesInRunner: boolean;
var
    xIniAccess: IWinlissyIniAccess;
begin

    xIniAccess := TAppSettings.CreateAppIni;
    EXIT(xIniAccess.ReadBool('Display', 'EditVariablesInRunner'));
end;

class procedure TEditMethodParamsExt.WriteMethodVarPages(const aMethodName: string;
    const aPages: TArray<TMethodVarPageRec>);
var
    xDA: TMethodVarPagesDataAdaptor;
begin
    xDA := TMethodVarPagesDataAdaptor.Create;
    try
        // Alle löschen
        xDA.DeleteName(aMethodName);

        // Alle neu schreiben
        xDA.WriteRecs(aMethodName, aPages);
    finally
        FreeAndNil(xDA);
    end;
end;

class procedure TEditMethodParamsExt.WriteStoredIdents(const aMethodName: string;
    aIdents: TObjectList<TParserStoredIdent>);
var
    xDA: TMethodVariablesDataAdaptor;
    x: integer;
begin
    xDA := TMethodVariablesDataAdaptor.Create;
    try
        // Alle löschen
        xDA.DeleteName(aMethodName);

        // Alle neu schreiben
        for x := 0 to aIdents.Count - 1 do
        begin
            xDA.WriteVariableRec(aMethodName, aIdents[x].name, aIdents[x].Data);
        end;
    finally
        FreeAndNil(xDA);
    end;
end;

class procedure TEditMethodParamsExt.RequestStoredValues(aParser: TParserWrapperOpt;
    const aMethodName: string);
var
    xIdentsNeedingInput: TObjectList<TParserStoredIdent>;
    xForm: TfrmMethodVariablesEditor;
begin
    xIdentsNeedingInput := TObjectList<TParserStoredIdent>.Create();
    try
        GetIdentsNeedingInput(aParser, xIdentsNeedingInput, false, true, ReadStoredIdents(aMethodName));

        if xIdentsNeedingInput.Count <= 0 then
            EXIT;

        // MethodVariablesEditor
        xForm := TfrmMethodVariablesEditor.Create(nil);
        try
            xForm.SetIdents(aMethodName, xIdentsNeedingInput, self.ReadMethodVarPages(aMethodName));
            if (xForm.ShowModal = mrOK) then
            begin
                WriteStoredIdents(aMethodName, xIdentsNeedingInput);
                WriteMethodVarPages(aMethodName, xForm.PageInfos);
            end;
        finally
            FreeAndNil(xForm);
        end;
    finally
        FreeAndNil(xIdentsNeedingInput);
    end;
end;

class procedure TEditMethodParamsExt.EditMethodStoredParameters(const aMethodName: string);
var
    xParser: TParserWrapperOpt;
    xMethBuildDatasets: TObjectList<TMethBuildDataset>;
    xCompilerResult: TCompilerResult;
begin
    if (aMethodName = '') then
        EXIT;

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

            // if not aAllowGlobalIdents then
            // begin
            xParser.SymbolTable.GlobalList.Clear();
            // end;

            xParser.BeginScope(xMethBuildDatasets[0].IdentList);
            try
                xParser.RemoveNonInputIdents();

                // xParser.AddIdentifiersUsingList(aParameters, [itLocal]);
                // aParameters.Clear;

                RequestStoredValues(xParser, aMethodName);
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


end.
