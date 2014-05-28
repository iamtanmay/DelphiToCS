{ -----------------------------------------------------------------------------------------------------------
  Copyright © 2013 Zinsser Analytic GmbH - All rights reserved.
  Author       : Thomas Schubert (ts)
  Description  :
  -----------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                              track-no improvement/change
  -------- --  ----------------------------------  -------- -------------------------------------------------
  18.03.13 ts                                      TN6109   ehemals ProxyServerCalls von ZAProxy übernommen
  23.04.13 wl                                      TN6135   uses geändert
  21.08.13 ts                                      TN6109   RequestStartableMethods und SimulateMethod
  11.03.14 tp  RequestMethodLayouts(), RequestMethodIcons(),
  RequestBuildingBlockBools(),  RequestEditableBools(),
  RequestMethodParameters(), RequestAllLayouts()   TN6375   Functions for remote GUI
  ----------------------------------------------------------------------------------------------------------- }

unit RunnerServerCalls;


interface


uses
    RunnerServerCallHandler;

type
    TRunnerServerCalls = class
    private
        fCallHandler: TRunnerServerCallHandler;
        fClientRunID: string;
        fControlEventMessageIDs: TArray<integer>;
        function SetControlEventMaskIntern: boolean;
    public
        constructor Create();
        function RegisterController(): boolean;
        function UnRegisterController(): boolean;
        function SetControlEventMask(const aIDs: TArray<integer>): boolean;
        function StartMethod(const aMethodName: string; const aDontShowMessages: boolean): boolean;
        function SimulateMethod(const aMethodName: string; const aDontShowMessages: boolean): boolean;
        function InterruptStart(const aInterruptText: string): boolean;
        function InterruptFinish(const aInterruptText: string): boolean;
        function UserStopInterrupt(const aInterruptText: string): boolean;
        procedure SetGlobalError(const aErrorText: string);
        function AskStatus(): integer;
        function GetPendingControlEventID(const aClientID: string): integer;
        function RequestStartableMethods(): string;

        function RequestMethodLayouts(): string;
        function RequestMethodIcons(): string;
        function RequestBuildingBlockBools(): string;
        function RequestEditableBools(): string;
        function RequestMethodParameters(): string;
        function RequestAllLayouts(): string;

        procedure SQLWriteQuery(aQuery: string);
        function SQLReadQuery(aQuery: string): string;

        function VerifierQuery(aQuery: string): string;

        function GetErrorBoxEvent(out oErrorID: integer): boolean;
        procedure SetErrorBoxEventHandled(const aResultModal: integer);

        function GetMessageBoxEvent(out oText, oCaption: string; out oType: integer): boolean;
        procedure SetMessageBoxEventHandled(const aResultModal: integer);

        function GetProcessStartedEvent(out oSourceDataName, oProcessID: string): boolean;
        procedure SetProcessStartedEventHandled(const aResultSuccess: boolean);

        function GetProcessFinishedEvent(out oProcessID: string; out oIsError: boolean): boolean;
        procedure SetProcessFinishedEventHandled(const aResultSuccess: boolean);

        property ControlEventMessageIDs: TArray<integer>read fControlEventMessageIDs
            write fControlEventMessageIDs;
    end;


implementation


uses
    ThreadClasses;

{ TRunnerServerCalls }

function TRunnerServerCalls.SetControlEventMask(const aIDs: TArray<integer>): boolean;
begin
    result := fCallHandler.SetControlEventMask(aIDs);
end;

function TRunnerServerCalls.SetControlEventMaskIntern(): boolean;
begin
    result := fCallHandler.SetControlEventMask(fControlEventMessageIDs);
end;

function TRunnerServerCalls.StartMethod(const aMethodName: string; const aDontShowMessages: boolean): boolean;
begin
    result := fCallHandler.StartMethod(aMethodName, aDontShowMessages, fClientRunID);
end;

function TRunnerServerCalls.SimulateMethod(const aMethodName: string;
    const aDontShowMessages: boolean): boolean;
begin
    result := fCallHandler.SimulateMethod(aMethodName, aDontShowMessages, fClientRunID);
end;

constructor TRunnerServerCalls.Create;
begin
    inherited Create();
    fCallHandler := TRunnerServerCallHandler.Create();
    SetLength(fControlEventMessageIDs, 0);
end;

function TRunnerServerCalls.InterruptStart(const aInterruptText: string): boolean;
begin
    result := fCallHandler.InterruptStart(aInterruptText);
end;

function TRunnerServerCalls.RegisterController(): boolean;
begin
    if self.fClientRunID = '' then
    begin
        result := SetControlEventMaskIntern();
        fClientRunID := fCallHandler.GetNewClientRunID();
        fCallHandler.AddClientToRun(fClientRunID);
        if not result then
            EXIT;
    end;

    result := fCallHandler.RegisterController(fClientRunID);
end;

function TRunnerServerCalls.UnRegisterController: boolean;
begin
    result := fCallHandler.UnRegisterController(fClientRunID);
end;

function TRunnerServerCalls.InterruptFinish(const aInterruptText: string): boolean;
begin
    result := fCallHandler.InterruptFinish(aInterruptText);
end;

function TRunnerServerCalls.UserStopInterrupt(const aInterruptText: string): boolean;
begin
    result := fCallHandler.UserStopInterrupt(aInterruptText);
end;

procedure TRunnerServerCalls.SetGlobalError(const aErrorText: string);
begin
    fCallHandler.SetGlobalError(aErrorText);
end;

function TRunnerServerCalls.RequestStartableMethods: string;
begin
    result := fCallHandler.RequestStartableMethods();
end;

function TRunnerServerCalls.RequestMethodLayouts: string;
begin
    result := fCallHandler.RequestMethodLayouts();
end;

function TRunnerServerCalls.RequestMethodIcons: string;
begin
    result := fCallHandler.RequestMethodIcons();
end;

function TRunnerServerCalls.RequestBuildingBlockBools: string;
begin
    result := fCallHandler.RequestBuildingBlockBools();
end;

function TRunnerServerCalls.RequestEditableBools: string;
begin
    result := fCallHandler.RequestEditableBools();
end;

function TRunnerServerCalls.RequestMethodParameters: string;
begin
    result := fCallHandler.RequestMethodParameters();
end;

function TRunnerServerCalls.RequestAllLayouts: string;
begin
    result := fCallHandler.RequestStartableMethods();
end;

procedure TRunnerServerCalls.SQLWriteQuery(aQuery: string);
begin
    fCallHandler.SQLWriteQuery(aQuery);
end;

function TRunnerServerCalls.SQLReadQuery(aQuery: string): string;
begin
    result := fCallHandler.SQLReadQuery(aQuery);
end;

function TRunnerServerCalls.VerifierQuery(aQuery: string): string;
begin
    result := fCallHandler.VerifierQuery(aQuery);
end;

function TRunnerServerCalls.AskStatus: integer;
begin
    result := fCallHandler.AskStatus();
end;

function TRunnerServerCalls.GetPendingControlEventID(const aClientID: string): integer;
begin
    result := fCallHandler.GetPendingControlEventID(aClientID);
end;

function TRunnerServerCalls.GetErrorBoxEvent(out oErrorID: integer): boolean;
begin
    result := fCallHandler.GetErrorBoxEvent(oErrorID);
end;

procedure TRunnerServerCalls.SetErrorBoxEventHandled(const aResultModal: integer);
begin
    fCallHandler.SetErrorBoxEventHandled(aResultModal);
end;

function TRunnerServerCalls.GetMessageBoxEvent(out oText, oCaption: string; out oType: integer): boolean;
begin
    result := fCallHandler.GetMessageBoxEvent(oText, oCaption, oType);
end;

procedure TRunnerServerCalls.SetMessageBoxEventHandled(const aResultModal: integer);
begin
    fCallHandler.SetMessageBoxEventHandled(aResultModal);
end;

function TRunnerServerCalls.GetProcessStartedEvent(out oSourceDataName, oProcessID: string): boolean;
begin
    result := fCallHandler.GetProcessStartedEvent(oSourceDataName, oProcessID);
end;

procedure TRunnerServerCalls.SetProcessStartedEventHandled(const aResultSuccess: boolean);
begin
    fCallHandler.SetProcessStartedEventHandled(aResultSuccess);
end;

function TRunnerServerCalls.GetProcessFinishedEvent(out oProcessID: string; out oIsError: boolean): boolean;
begin
    result := fCallHandler.GetProcessFinishedEvent(oProcessID, oIsError);
end;

procedure TRunnerServerCalls.SetProcessFinishedEventHandled(const aResultSuccess: boolean);
begin
    fCallHandler.SetProcessFinishedEventHandled(aResultSuccess);
end;


end.
