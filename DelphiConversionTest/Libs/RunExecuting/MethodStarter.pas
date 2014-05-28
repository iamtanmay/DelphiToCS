{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2011 Zinsser Analytic GmbH - All rights reserved.
  Author       : Wolfgang Lyncke (wl)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  14.12.11 wl  TMethodStarter            TN5765   neue Klasse
  27.12.11 wl                            TN5768   an geändertes TRunGlobals angepasst
  20.04.12 wl                            TN5946   TSourceDataType entfernt
  07.08.12 wl  StartMethodThread         TN5946   InitAtMethodEnd wird hier nicht mehr bestimmt
  07.08.12 wl  MethodStart               TN5946   RunNameAllowed entfernt
  15.08.13 wl                            TN6223   uses geändert
  30.08.13 wl                            TN6236   unit-Name geändert
  ----------------------------------------------------------------------------------------------------------- }

unit MethodStarter;


interface


uses
    MethodCompiledFile,
    ExecHandler,
    MethodSettingsDataAdaptor,
    AppTypes;

type
    TMethodStarter = record
    private
        class function ReadMethodSettings(const aMethodName: string): TMethodSettingsRec; static;
        class function StartMethodThread(const aMethodName: string; aProcessDescription: string;
            const aCompiledFile: TMethodCompiledFile; const aRunModes: TSamplerThreadModes;
            const aIsSimulated: boolean; const aRestartParams: TExecHandlerRestartParams): boolean; static;
        class function Compile(const aMainMethodName: string; const aCompiledFile: TMethodCompiledFile)
            : boolean; static;
        class function CheckIsRestart(const aMethodName: string; const aCompiledFile: TMethodCompiledFile;
            out oRestartParams: TExecHandlerRestartParams): boolean; static;
    public
        class function MethodStart(const aMethodName: string; aRunModes: TSamplerThreadModes;
            const aIsSimulated: boolean): boolean; static;
    end;


implementation


uses
    Controls,
    SysUtils,
    ThreadClasses,
    GUIManager,
    ThreadAPI,
    RunGlobals,
    GUIManagerRun,
    MethodCompile,
    LogManager,
    RunFlow,
    ProcessTraceDataCache,
    TraceManager,
    RunTraceManager,
    ProcessInfoDataCache,
    ActionIDDataCache,
    ActionDataCache,
    MemoryClasses,
    TraceThreadDetails;

{ TMethodStarter }

class function TMethodStarter.ReadMethodSettings(const aMethodName: string): TMethodSettingsRec;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        EXIT(xDA.ReadRec(aMethodName));
    finally
        FreeAndNil(xDA);
    end;
end;

class function TMethodStarter.StartMethodThread(const aMethodName: string; aProcessDescription: string;
    const aCompiledFile: TMethodCompiledFile; const aRunModes: TSamplerThreadModes;
    const aIsSimulated: boolean; const aRestartParams: TExecHandlerRestartParams): boolean;
var
    xExecHandler: TExecHandler;
    xInitAtBegin, xInitAtEnd, xNoMessages: boolean;
    xAddressSpace: TAddressSpace;
    xRec: TMethodSettingsRec;
begin
    xInitAtBegin := mdInitFirst in aRunModes;
    xInitAtEnd := mdInitAtEnd in aRunModes;
    xNoMessages := mdNoMessages in aRunModes;

    if Pos('NOINIT', Uppercase(aMethodName)) > 0 then
        xInitAtBegin := false;

    xRec := ReadMethodSettings(aMethodName);

    if (not xRec.InitAtStart) then
    begin
        xInitAtBegin := false;
        xInitAtEnd := false;
    end;

    xAddressSpace := TMethodAddressSpace.Create();
    xAddressSpace.Load(aCompiledFile);

    xExecHandler := TExecHandler.Create(xRec, xInitAtBegin, xInitAtEnd, true, xNoMessages, aIsSimulated,
        aRestartParams);

    TThreadAPI.CreateProcess(aProcessDescription, xExecHandler, xAddressSpace, aMethodName, false,
        aIsSimulated);

    result := true;
end;

class function TMethodStarter.Compile(const aMainMethodName: string;
    const aCompiledFile: TMethodCompiledFile): boolean;
var
    xCompilerResult: TCompilerResult;
    xOldCursor: TCursor;
begin
    result := true;
    xOldCursor := gGUIManager.SetCursor(crHourGlass);
    try
        xCompilerResult := TCompilerResult.Create();
        try
            TMethodCompile.InstCompile(aMainMethodName, aCompiledFile.CompiledCode.Code,
                aCompiledFile.CompiledCode.EvalTable, xCompilerResult, true);
            if xCompilerResult.ErrorMessages.Count = 0 then
                EXIT;
            raise Exception.Create(Format('Submethod: %s' + #13#10 + 'Line: %d' + #13#10 + '%s',
                [xCompilerResult.ErrorMessages[0].MName, xCompilerResult.ErrorMessages[0].LineIndex + 1,
                xCompilerResult.ErrorMessages[0].Text]));
        finally
            xCompilerResult.Free;
        end;
    finally
        gGUIManager.SetCursor(xOldCursor)
    end;
end;

class function TMethodStarter.CheckIsRestart(const aMethodName: string;
    const aCompiledFile: TMethodCompiledFile; out oRestartParams: TExecHandlerRestartParams): boolean;
var
    xRelativeMemAddress: TRelativeMemAddress;
    xThreadDetails: TThreadDetailsItem;
    xMainThreadTraceName: string;
    xContinueAtID: TActionID;
    xProcessInfoDataCache: TProcessInfoDataCache;
    xTraceName: string;
    xProcessListDataCache: TProcessTraceListDataCache;
    xContinueAtSegmentIndex: integer;
begin
    result := true;
    oRestartParams.IsRestart := false;
    oRestartParams.TraceName := '';
    if not TRunTraceManager.Instance.IsEnabled then
        EXIT;

    xProcessListDataCache := TRunTraceManager.CreateProcesListDataCache();
    try
        xProcessListDataCache.Init();
        xProcessInfoDataCache := TRunTraceManager.CreateProcessInfoDataCacheBySourceDataName
            (xProcessListDataCache, aMethodName);
        if not Assigned(xProcessInfoDataCache) then
            EXIT;
        try
            xTraceName := xProcessInfoDataCache.ProcessInfoData.TraceName;

            xRelativeMemAddress := TRelativeMemAddress.Create();
            try
                xMainThreadTraceName := TRunTraceManager.GetMainThreadTraceNameForProcess(xTraceName);
                oRestartParams.IsRestart := xMainThreadTraceName <> '';
                if not oRestartParams.IsRestart then
                    EXIT;
                // if the trace stopped at the last line in the main method no restart

                xThreadDetails := TRunTraceManager.Instance.CreateThreadDetailsForThreadTrace(xTraceName,
                    xMainThreadTraceName);
                try
                    if not xThreadDetails.ProgramCounterDataCache.Read() then
                    begin
                        TLogManager.Instance.Log
                            ('Restart not possible: Program-Counter file could not be read.', true);
                        oRestartParams.IsRestart := false;
                        EXIT;
                    end;
                    xRelativeMemAddress.LabelName :=
                        xThreadDetails.ProgramCounterDataCache.ProgramCounterData.LabelName;
                    xRelativeMemAddress.RelativeAddress :=
                        xThreadDetails.ProgramCounterDataCache.ProgramCounterData.RelativeAddress;
                    oRestartParams.IsRestart := not xProcessInfoDataCache.ProcessInfoData.IsCompleted;
                    if not oRestartParams.IsRestart then
                        EXIT;

                    xThreadDetails.ActionListDataCache.Read;

                    // xContinueAtID = -1 : continue at next line
                    // xContinueAtID = 0 : new run

                    result := TGUIManagerRun.Instance.AskRestart_PromptModal(aMethodName,
                        xProcessInfoDataCache.ProcessInfoData.DateLastStopped,
                        xThreadDetails.ActionListDataCache, TRunTraceManager.Instance.RestartOnlyAtMarks,
                        xContinueAtID, xContinueAtSegmentIndex) = mrOK;
                    if not result then
                        EXIT;
                    oRestartParams.IsRestart := xContinueAtID <> 0;
                    oRestartParams.ContinueAtActionID := xContinueAtID;
                    oRestartParams.ContinueAtSegmentIndex := xContinueAtSegmentIndex;
                    if not oRestartParams.IsRestart then
                        EXIT;

                    // TRunTraceManager.Instance.RewindThreadTraceToActionID( xTraceName, xMainThreadTraceName, xContinueAtID );

                    // xProcessInfoDataCache.ChangeIsRestart( true );
                    oRestartParams.TraceName := xTraceName;

                finally
                    xThreadDetails.Free;
                end;
            finally
                xRelativeMemAddress.Free;
            end;

        finally
            if not oRestartParams.IsRestart then
            begin
                TRunTraceManager.RemoveProcessTrace(xProcessListDataCache, xTraceName);
            end;
            xProcessInfoDataCache.Free;
        end;
    finally
        xProcessListDataCache.Free;
    end;

end;

class function TMethodStarter.MethodStart(const aMethodName: string; aRunModes: TSamplerThreadModes;
    const aIsSimulated: boolean): boolean;
var
    xCompiledFile: TMethodCompiledFile;
    xIsSimulated: boolean;
    xLogText: string;
    xRestartParams: TExecHandlerRestartParams;
    xProcessDescription: string;
begin
    result := false;

    if (aMethodName <> TRunGlobals.Instance.PMethodName) then
    begin
        if not TRunGlobals.Instance.ChangeGlobalName(aMethodName, '', false, false) then
            EXIT;
    end;

    if (aMethodName = '') then
        EXIT;

    xCompiledFile := TMethodCompiledFile.Create();
    result := Compile(aMethodName, xCompiledFile);
    if not result then
        EXIT;

    result := CheckIsRestart(aMethodName, xCompiledFile, xRestartParams);
    if not result then
        EXIT;

    xProcessDescription := xRestartParams.TraceName;
    if not xRestartParams.IsRestart then
    begin
        xProcessDescription := TRunTraceManager.MakeTraceName(aMethodName);
    end;

    TRunTraceManager.Instance.InitProcessList();

    if not result then
        EXIT;

    xIsSimulated := gRunFlow.AppSimulationMode or aIsSimulated;
    TGUIManagerRun.Instance.SimulationMode_Enable(xIsSimulated);
    xLogText := Format('Starting Method %s', [aMethodName]);
    gLogManager.Log(xLogText, false);

    result := StartMethodThread(aMethodName, xProcessDescription, xCompiledFile, aRunModes, xIsSimulated,
        xRestartParams);
end;


end.
