{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  18.03.13 ts                                      TN6109   ehemals ProxyServerCallHandler von ZAProxy übernommen
  23.04.13 wl                                      TN6135   uses geändert
  21.08.13 ts                                      TN6109   RequestStartableMethods und SimulateMethod
  30.08.13 wl                                      TN6236   uses geändert
  11.03.14 tp  StringAddEndingBracket, StringAddCommas, RequestMethodLayouts(),
  RequestMethodIcons(), RequestBuildingBlockBools(),  RequestEditableBools(),
  RequestMethodParameters(), RequestAllLayouts()   TN6375   Functions for remote GUI
  11.04.14 tp StringReplacePointyBrackets          TN6375   Function for remote GUI
  16.04.14 ts StringReplacePointyBrackets          TN6375   angepasst
  08.05.14 ts StringReplacePointyBrackets          TN6375   erneut angepasst
  ----------------------------------------------------------------------------------------------------------- }

unit RunnerServerCallHandler;


interface


type
    TRunnerServerCallHandler = class
    private const
        cMessageStartMethod = 9001;
        cMessageInterruptStart = 9002;
        cMessageInterruptFinish = 9003;
        cMessageSetGlobalError = 9004;
        cMessageUserStopInterrupt = 9009;
        cMessageRequestStartableMethods = 9010;
        cMessageSimulateMethod = 9011;

        cAskStatusIdle = 1;
        cAskStatusActive = 2;
        cAskStatusUserInputPending = 3;
    public
        function RegisterController(const aClientRunID: string): boolean;
        function UnRegisterController(const aClientRunID: string): boolean;
        function GetNewClientRunID: string;
        procedure AddClientToRun(const aClientRunID: string);
        function SetControlEventMask(const aIDs: TArray<integer>): boolean;
        function StartMethod(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientRunID: string): boolean;
        function SimulateMethod(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientRunID: string): boolean;
        function InterruptStart(const aInterruptText: string): boolean;
        function InterruptFinish(const aInterruptText: string): boolean;
        function UserStopInterrupt(const aInterruptText: string): boolean;
        procedure SetGlobalError(const aErrorText: string);
        function AskStatus(): integer;
        function RequestStartableMethods(): string;

        function RequestMethodLayouts(): string;
        function RequestMethodIcons(): string;
        function RequestBuildingBlockBools(): string;
        function RequestEditableBools(): string;
        function RequestMethodParameters(): string;
        function RequestAllLayouts(): string;

        function StringAddCommas(aQuery: string): string;
        function StringAddEndingBracket(aQuery: string): string;
        function StringReplacePointyBrackets(aQuery: string): string;
        procedure SQLWriteQuery(aQuery: string);
        function SQLReadQuery(aQuery: string): string;

        function VerifierQuery(aQuery: string): string;

        // Events
        function GetPendingControlEventID(const aClientID: string): integer;

        function GetErrorBoxEvent(out oErrorID: integer): boolean;
        procedure SetErrorBoxEventHandled(const aResultModal: integer);

        function GetMessageBoxEvent(out oText, oCaption: string; out oType: integer): boolean;
        procedure SetMessageBoxEventHandled(const aResultModal: integer);

        function GetProcessStartedEvent(out oSourceDataName, oProcessID: string): boolean;
        procedure SetProcessStartedEventHandled(const aResultSuccess: boolean);

        function GetProcessFinishedEvent(out oProcessID: string; out oIsError: boolean): boolean;
        procedure SetProcessFinishedEventHandled(const aResultSuccess: boolean);
    end;


implementation


uses
    SysUtils,
    MethodStarter,
    AppTypes,
    StringUtilities,
    MethodVariableTypes,
    MethodVariablesDataAdaptor,
    QueryDataAdaptor,
    SQLTermsDataAdaptor,
    MethodSettingsDataAdaptor;

{ TRunnerServerCallHandler }

function TRunnerServerCallHandler.SetControlEventMask(const aIDs: TArray<integer>): boolean;
begin
    // result := TCoreControllerClientCalls.Instance.SetControlEventMask(aIDs);
    result := true;
end;

function TRunnerServerCallHandler.StartMethod(const aMethodName: string; const aDontShowMessages: boolean;
    const aClientRunID: string): boolean;
begin
    result := TMethodStarter.MethodStart(aMethodName, [mdNoMessages, mdDeleteAfterRun, mdAlwaysRestart,
        mdInitFirst], false);
end;

function TRunnerServerCallHandler.SimulateMethod(const aMethodName: string; const aDontShowMessages: boolean;
    const aClientRunID: string): boolean;
begin
    result := TMethodStarter.MethodStart(aMethodName, [mdNoMessages, mdDeleteAfterRun, mdAlwaysRestart,
        mdInitFirst], true);
end;

function TRunnerServerCallHandler.InterruptStart(const aInterruptText: string): boolean;
begin
    // result := TCoreControllerClientCalls.Instance.InterruptStart(aInterruptText);
    result := true;
end;

function TRunnerServerCallHandler.RegisterController(const aClientRunID: string): boolean;
begin
    // result := TCoreClient.Instance.RegisterController(aClientRunID);
    result := true;
end;

function TRunnerServerCallHandler.UnRegisterController(const aClientRunID: string): boolean;
begin
    // result := TCoreClient.Instance.UnRegisterController(aClientRunID);
    result := true;
end;

function TRunnerServerCallHandler.InterruptFinish(const aInterruptText: string): boolean;
begin
    // result := TCoreControllerClientCalls.Instance.InterruptFinish(aInterruptText);
    result := true;
end;

function TRunnerServerCallHandler.UserStopInterrupt(const aInterruptText: string): boolean;
begin
    // result := TCoreControllerClientCalls.Instance.UserStopInterrupt(aInterruptText);
    result := true;
end;

procedure TRunnerServerCallHandler.SetGlobalError(const aErrorText: string);
begin
    // TCoreControllerClientCalls.Instance.SetGlobalError(aErrorText);
end;

procedure TRunnerServerCallHandler.AddClientToRun(const aClientRunID: string);
begin
    // TCoreClient.Instance.AddClientToRun(aClientRunID);
end;

function TRunnerServerCallHandler.RequestStartableMethods: string;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        EXIT(TStringUtilities.StringArrayToString(xDA.ReadStartableMethodNames, ','));
    finally
        FreeAndNil(xDA);
    end;
end;

function TRunnerServerCallHandler.RequestMethodLayouts(): string;
var
    xI: integer;
    xJ: integer;
    xDA: TMethodSettingsDataAdaptor;
    xMethodRec: MethodSettingsDataAdaptor.TMethodSettingsRec;
    xMethodRecs: TArray<MethodSettingsDataAdaptor.TMethodSettingsRec>;
    xMethodNames: TArray<string>;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        xMethodNames := xDA.ReadStartableMethodNames;
        xJ := 0;

        xMethodRecs := xDA.ReadAllRecs();
        for xI := 0 to Length(xMethodRecs) - 1 do
        begin
            xMethodRec := xMethodRecs[xI];
            if xMethodRec.Startable then
            begin
                xMethodNames[xJ] := xMethodRec.LayoutName;
                xJ := xJ + 1;
            end;
        end;

        EXIT(TStringUtilities.StringArrayToString(xMethodNames, ','));
    finally
        FreeAndNil(xDA);
    end;
end;

function TRunnerServerCallHandler.RequestMethodIcons(): string;
var
    xI: integer;
    xJ: integer;
    xDA: TMethodSettingsDataAdaptor;
    xMethodRec: MethodSettingsDataAdaptor.TMethodSettingsRec;
    xMethodRecs: TArray<MethodSettingsDataAdaptor.TMethodSettingsRec>;
    xMethodNames: TArray<string>;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        xMethodNames := xDA.ReadStartableMethodNames;
        xJ := 0;

        xMethodRecs := xDA.ReadAllRecs();
        for xI := 0 to Length(xMethodRecs) - 1 do
        begin
            xMethodRec := xMethodRecs[xI];
            if xMethodRec.Startable then
            begin
                xMethodNames[xJ] := xMethodRec.ImageFileName;
                xJ := xJ + 1;
            end;
        end;

        EXIT(TStringUtilities.StringArrayToString(xMethodNames, ','));
    finally
        FreeAndNil(xDA);
    end;
end;

function TRunnerServerCallHandler.RequestBuildingBlockBools(): string;
var
    xI: integer;
    xJ: integer;
    xDA: TMethodSettingsDataAdaptor;
    xMethodRec: MethodSettingsDataAdaptor.TMethodSettingsRec;
    xMethodRecs: TArray<MethodSettingsDataAdaptor.TMethodSettingsRec>;
    xMethodNames: TArray<string>;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        xMethodNames := xDA.ReadStartableMethodNames;
        xJ := 0;

        xMethodRecs := xDA.ReadAllRecs();
        for xI := 0 to Length(xMethodRecs) - 1 do
        begin
            xMethodRec := xMethodRecs[xI];
            if xMethodRec.Startable then
            begin
                xMethodNames[xJ] := BoolToStr(xMethodRec.IsBuildingBlock);
                xJ := xJ + 1;
            end;
        end;

        EXIT(TStringUtilities.StringArrayToString(xMethodNames, ','));
    finally
        FreeAndNil(xDA);
    end;
end;

function TRunnerServerCallHandler.RequestEditableBools(): string;
var
    xI: integer;
    xJ: integer;
    xDA: TMethodSettingsDataAdaptor;
    xMethodRec: MethodSettingsDataAdaptor.TMethodSettingsRec;
    xMethodRecs: TArray<MethodSettingsDataAdaptor.TMethodSettingsRec>;
    xMethodNames: TArray<string>;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        xMethodNames := xDA.ReadStartableMethodNames;
        xJ := 0;

        xMethodRecs := xDA.ReadAllRecs();
        for xI := 0 to Length(xMethodRecs) - 1 do
        begin
            xMethodRec := xMethodRecs[xI];
            if xMethodRec.Startable then
            begin
                xMethodNames[xJ] := BoolToStr(xMethodRec.EditInRunner);
                xJ := xJ + 1;
            end;
        end;

        EXIT(TStringUtilities.StringArrayToString(xMethodNames, ','));
    finally
        FreeAndNil(xDA);
    end;
end;

function TRunnerServerCallHandler.RequestMethodParameters(): string;
var
    xI: integer;
    xJ: integer;
    xMethodSettingsDA: TMethodSettingsDataAdaptor;
    xDA: TMethodVariablesDataAdaptor;
    xMethodNames: TArray<string>;
    xVariableStrings: TArray<string>;
    xVariableString: TArray<string>;
    xVariables: TArray<TMethodVariableRec>;
begin
    xDA := TMethodVariablesDataAdaptor.Create;
    xMethodSettingsDA := TMethodSettingsDataAdaptor.Create;
    try
        xMethodNames := xMethodSettingsDA.ReadStartableMethodNames;
        SetLength(xVariableStrings, Length(xMethodNames));

        for xI := 0 to Length(xMethodNames) - 1 do
        begin

            xVariables := xDA.ReadRecs(xMethodNames[xI]);
            SetLength(xVariableString, Length(xVariables));
            for xJ := 0 to Length(xVariables) - 1 do
            begin
                xVariableString[xJ] := xVariables[xJ].VariableName + '.' +
                    IntToStr(xVariables[xJ].RequestOrder) + '.' + xVariables[xJ].RequestText + '.' +
                    xVariables[xJ].DefaultValue + '.' + xVariables[xJ].PickList + '.' +
                    BoolToStr(xVariables[xJ].MinCheck) + '.' + FloatToStr(xVariables[xJ].MinValue) + '.' +
                    BoolToStr(xVariables[xJ].MaxCheck) + '.' + FloatToStr(xVariables[xJ].MaxValue) + '.' +
                    IntToStr(xVariables[xJ].DataType) + '.' + BoolToStr(xVariables[xJ].DataIsArray) + '.' +
                    BoolToStr(xVariables[xJ].DialogHide) + '.' +
                    IntToStr(xVariables[xJ].ArrayLengthRefToOrder);
            end;
            xVariableStrings[xI] := TStringUtilities.StringArrayToString(xVariableString, ';');
        end;
        EXIT(TStringUtilities.StringArrayToString(xVariableStrings, '|'));
    finally
        FreeAndNil(xDA);
        FreeAndNil(xMethodSettingsDA);
    end;
end;

function TRunnerServerCallHandler.RequestAllLayouts(): string;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        EXIT(TStringUtilities.StringArrayToString(xDA.ReadStartableMethodNames, ','));
    finally
        FreeAndNil(xDA);
    end;
end;

// For TCP IP communication, the ending bracket '|' is replaced with ','
function TRunnerServerCallHandler.StringAddCommas(aQuery: string): string;
var
    xString: string;

begin
    xString := StringReplace(aQuery, '|', ',', [rfReplaceAll, rfIgnoreCase]);
    result := xString;
end;

// For TCP IP communication, the ending bracket ')' is replaced with '#'
function TRunnerServerCallHandler.StringAddEndingBracket(aQuery: string): string;
var
    xString: string;

begin
    xString := StringReplace(aQuery, '#', ')', [rfReplaceAll, rfIgnoreCase]);
    result := xString;
end;

// For TCP IP communication, the round brackets '(,)' are replaced with '<, >'
function TRunnerServerCallHandler.StringReplacePointyBrackets(aQuery: string): string;
var
    xString: string;

begin
    // are these '<, >' characters only used for arrays??
    xString := StringReplace(aQuery, 'A<', 'A(', [rfReplaceAll, rfIgnoreCase]);
    xString := StringReplace(xString, '>', ')', [rfReplaceAll, rfIgnoreCase]);
    result := xString;
end;

procedure TRunnerServerCallHandler.SQLWriteQuery(aQuery: string);
var
    xQuery: TQueryDataAdaptor;
begin
    xQuery := TQueryDataAdaptor.Create('METHOD');
    try
        xQuery.ExecSQL(StringReplacePointyBrackets(StringAddEndingBracket(StringAddCommas(aQuery))));
    finally
        FreeAndNil(xQuery);
    end;
end;

function TRunnerServerCallHandler.SQLReadQuery(aQuery: string): string;
var
    x: integer;
    xI: integer;
    fRecordCount: integer;
    fFieldCount: integer;
    xQuery: TQueryDataAdaptor;
    xRecStrings: TArray<string>;
    xMultipleRecStrings: TArray<string>;
begin

    // Open CALIBPresets to read Preset data
    xQuery := TQueryDataAdaptor.Create('METHOD');

    try
        // Read all the records
        xQuery.SelectAndOpen(StringReplacePointyBrackets
            (StringAddEndingBracket(StringAddCommas(aQuery))), true);

        // Number of records in the Preset table
        fRecordCount := xQuery.DataProvider.RecordCount;

        // Number of fields in each record
        fFieldCount := xQuery.DataProvider.Fields.Count;

        // Initialize internal arrays according to number of records
        SetLength(xMultipleRecStrings, fRecordCount);
        SetLength(xRecStrings, fFieldCount);

        // Read all the records and store in internal arrays
        if fRecordCount > 0 then
        begin
            if (fFieldCount > 0) then
            begin
                x := 0;
                while not xQuery.DataProvider.Eof do
                begin
                    for xI := 0 to fFieldCount - 1 do
                    begin
                        xRecStrings[xI] := xQuery.DataProvider.Fields[xI].AsString;
                    end;
                    xMultipleRecStrings[x] := TStringUtilities.StringArrayToString(xRecStrings, '.');
                    x := x + 1;
                    xQuery.DataProvider.Next;
                end;
            end;
        end;
        EXIT(TStringUtilities.StringArrayToString(xMultipleRecStrings, '|'));
    finally
        xQuery.Free;
    end;
end;

function TRunnerServerCallHandler.VerifierQuery(aQuery: string): string;
var
    xDA: TMethodSettingsDataAdaptor;
begin
    xDA := TMethodSettingsDataAdaptor.Create;
    try
        EXIT(TStringUtilities.StringArrayToString(xDA.ReadStartableMethodNames, ','));
    finally
        FreeAndNil(xDA);
    end;
end;

function TRunnerServerCallHandler.AskStatus: integer;
begin
    result := 0;
end;

function TRunnerServerCallHandler.GetPendingControlEventID(const aClientID: string): integer;
begin
    // result := TCoreControllerClientCalls.Instance.GetPendingControlEventID();
    result := 0;
end;

function TRunnerServerCallHandler.GetErrorBoxEvent(out oErrorID: integer): boolean;
begin
    // result := TCoreControllerClientCalls.Instance.GetErrorBoxEvent(oErrorID);
    result := true;
end;

procedure TRunnerServerCallHandler.SetErrorBoxEventHandled(const aResultModal: integer);
begin
    // TCoreControllerClientCalls.Instance.SetErrorBoxEventHandled(aResultModal);
end;

function TRunnerServerCallHandler.GetMessageBoxEvent(out oText, oCaption: string; out oType: integer)
    : boolean;
begin
    // result := TCoreControllerClientCalls.Instance.GetMessageBoxEvent(oText, oCaption, oType);
    result := true;
end;

function TRunnerServerCallHandler.GetNewClientRunID: string;
begin
    // result := TCoreClient.Instance.GetNewClientRunID();
    result := '';
end;

procedure TRunnerServerCallHandler.SetMessageBoxEventHandled(const aResultModal: integer);
begin
    // TCoreControllerClientCalls.Instance.SetMessageBoxEventHandled(aResultModal);
end;

function TRunnerServerCallHandler.GetProcessStartedEvent(out oSourceDataName, oProcessID: string): boolean;
begin
    // result := TCoreControllerClientCalls.Instance.GetProcessStartedEvent(oSourceDataName, oProcessID);
    result := true;
end;

procedure TRunnerServerCallHandler.SetProcessStartedEventHandled(const aResultSuccess: boolean);
begin
    // TCoreControllerClientCalls.Instance.SetProcessStartedEventHandled(aResultSuccess);
end;

function TRunnerServerCallHandler.GetProcessFinishedEvent(out oProcessID: string;
    out oIsError: boolean): boolean;
begin
    // result := TCoreControllerClientCalls.Instance.GetProcessFinishedEvent(oProcessID, oIsError);
    result := true;
end;

procedure TRunnerServerCallHandler.SetProcessFinishedEventHandled(const aResultSuccess: boolean);
begin
    // TCoreControllerClientCalls.Instance.SetProcessFinishedEventHandled(aResultSuccess);
end;


end.
