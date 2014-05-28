unit ServerCoreServerCalls;


{ -----------------------------------------------------------------------------------------------------------------------
  Copyright © 2009 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  -----------------------------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                                track-no  improvement/change
  -------- --  ------------------------------------  --------- ----------------------------------------------------------
  09.06.09 pk                                        TN4585.2    Initial Revision
  18.06.09 pk                                        TN4585.2    ProcessFinished New aIsError Parameter
  06.07.09 pk  StartMethod                           TN4585.4    Parameters changed
  30.07.09 pk                                        TN4585.5    Various Changes
  09.03.10 pk                                        TN5015      Massive changes in ThreadClasses
  17.03.12 pk                                        TN5809.1  Changes for ZACoreClientX dll
  ----------------------------------------------------------------------------------------------------------------------- }
interface


uses
    GeneralTypes,
    CoreServerCallHandler;

type

    TServerCoreServerCalls = class
    protected
        fCallHandler: TCoreServerCallHandler;
    public
        constructor Create();
        function StartMethod(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientGroupID: string): boolean;
        function SimulateMethod(const aMethodName: string; const aDontShowMessages: boolean;
            const aClientGroupID: string): boolean;
        function InterruptStart(const aInterruptText: string): boolean;
        function InterruptFinish(const aInterruptText: string): boolean;
        function UserStopInterrupt(const aInterruptText: string): boolean;
        procedure SetGlobalError(const aErrorText: string);
        procedure RegisterEventHost(const aClientID: string; const aAddress: string; const aPort: integer);
        procedure UnRegisterEventHost(const aClientID : string);
        function AskStatus(): integer;

        procedure InitCallHandlerThread();
        class procedure CreateInstance();
        class procedure DestroyInstace();
        class function Instance(): TServerCoreServerCalls;

    end;


implementation


uses
       ThreadAPI;

var
    uInstance: TServerCoreServerCalls = nil;

    { TServerCoreServerCalls }

class procedure TServerCoreServerCalls.CreateInstance;
begin
    uInstance := TServerCoreServerCalls.Create();
end;

class procedure TServerCoreServerCalls.DestroyInstace;
begin
    uInstance.Free;
end;

class function TServerCoreServerCalls.Instance: TServerCoreServerCalls;
begin
    result := uInstance;
end;

function TServerCoreServerCalls.StartMethod(const aMethodName: string; const aDontShowMessages: boolean;
    const aClientGroupID: string): boolean;
begin
    result := fCallHandler.StartMethod(aMethodName, aDontShowMessages, aClientGroupID);
end;

function TServerCoreServerCalls.SimulateMethod(const aMethodName: string; const aDontShowMessages: boolean;
    const aClientGroupID: string): boolean;
begin
    result := fCallHandler.SimulateMethod(aMethodName, aDontShowMessages, aClientGroupID);
end;

procedure TServerCoreServerCalls.InitCallHandlerThread();
begin
    TThreadAPI.CreateThread('Core Server CallHandler', false, true, false, fCallHandler);
    fCallHandler.Unpause();
end;

constructor TServerCoreServerCalls.Create;
begin
    inherited Create();
    fCallHandler := TCoreServerCallHandler.Create();
end;

function TServerCoreServerCalls.InterruptStart(const aInterruptText: string): boolean;
begin
    result := fCallHandler.InterruptStart(aInterruptText);
end;

procedure TServerCoreServerCalls.RegisterEventHost(const aClientID, aAddress: string; const aPort: integer);
begin
    fCallHandler.RegisterEventHost(aClientID, aAddress, aPort);
end;

procedure TServerCoreServerCalls.UnRegisterEventHost(const aClientID : string);
begin
    fCallHandler.UnRegisterEventHost(aClientID);
end;

function TServerCoreServerCalls.InterruptFinish(const aInterruptText: string): boolean;
begin
    result := fCallHandler.InterruptFinish(aInterruptText);
end;

function TServerCoreServerCalls.UserStopInterrupt(const aInterruptText: string): boolean;
begin
    result := fCallHandler.UserStopInterrupt(aInterruptText);
end;

procedure TServerCoreServerCalls.SetGlobalError(const aErrorText: string);
begin
    fCallHandler.SetGlobalError(aErrorText);
end;

function TServerCoreServerCalls.AskStatus: integer;
begin
    result := fCallHandler.AskStatus();
end;


end.
