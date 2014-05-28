{ --------------------------------------------------------------------------------------------------
  Copyright © 2008 Zinsser Analytic GmbH - All rights reserved.
  Author       : Payman Kamali (pk)
  Description  :
  --------------------------------------------------------------------------------------------------
  Revision History:
  date     op  method                       track-no improvement/change
  -------- --  ---------------------------  -------- -----------------------------------------------
  25.09.08 wl                               TN4242    sehr vereinfacht, weil es nur noch RunStartEvent gibt
  15.10.08 pk  ExecuteRunStartWithNameAndParams TN4258 Params changed from string to TKeyValueParamArray
  06.11.08 pk  ExecuteRunStartWithNameAndParams TN4279 no build needed
  07.11.08 pk  gmStoreIdentValueInMemory        TN4306 new
  10.11.08 pk  StartThreadedMethod              TN4280 New
  10.11.08 pk  gmStoreIdentValueInMemory        TN4279 call TMethodEvalTable.GetIdentDataType to get datatype
  17.11.08 pk                                   TN4280    ThreadAPI instead of ThreadManagerRun
  24.11.08 pk  StartThreadedMethod              TN4280    return SysHandleID
  09.12.08 pk                                   TN4279   reference to GroupRunStepInfo removed
  17.02.09 pk  PrepareCallMethod, JumpLocal     TN4232   New
  20.02.09 pk                                   TN4232   Changes for multithreaded trace
  20.02.09 pk  ExecuteRunStartWithNameAndParams TN4232   create TBasicLiveRunHandler with IsEvent set to true
  25.02.09 pk  PrepareCallMethod                TN4279   New IsEvent Param
  04.03.09 pk                                   TN4232   uses GeneralTypes
  16.03.09 pk  GetCurrentSubMethodName          TN4470   New
  06.04.09 pk  DisplayComponentSimpleEvent      TN4503   New
  04.02.10 pk                                   TN4972   Changes for Restart
  09.03.10 pk                                   TN5015   Massive Changes in ThreadClasses
  07.05.10 pk                                   TN5092   various changes needed for new parser array type
  29.10.10 pk  DisplayComponentSimpleEvent      TN5320   RequestExecuteEvent has new WaitTillFinished Parameter
  01.03.12 wl  StoreParserIdentRWRawValue       TN5822   entfernt
  14.12.12 wl                                   TN6054   uses geändert
  29.05.13 ts  JoinThread                       TN6161   CloseSysHandle nur ausführen, wenn result von JoinThread = true
  25.06.13 wl  PrepareCallMethod                TN6178   hier entfernt
  15.08.13 wl                                   TN6223   uses geändert
  01.10.13 wl                                   TN6251   Rückgabewerte für Threads vorbereitet
  -------------------------------------------------------------------------------------------------- }

unit RunEventManager;


interface


uses
    Generics.Collections,
    Streamable,
    EventManager,
    ActionHandler,
    DisplayComponentIntf,
    RunStepBuilderProcessor,
    MethodTypes;

type
    ReturnKeyValuesDictionary = class(TDictionary < cardinal, TArray < TStreamableItem >> )
    public
        destructor Destroy; override;
    end;

    TRunEventManager = class(TEventManager)
    private
        fReturnValueList: ReturnKeyValuesDictionary;
        procedure EnterReturnKeyValuesToList(aSender: TObject; aReturnKeyValues: TArray<TStreamableItem>);
        function CreateThreadIntern(const aThreadTraceName: string; const aStartInfo: TRunStartInfo;
            const aCreateSuspended, aCloseHandleOnFinish: boolean): cardinal;
        function StartThreadedMethodIntern(const aMethodName: string; const aParams: TKeyArgValueList;
            const aIsMainThread: boolean; const aCreateSuspended, aCloseHandleOnFinish: boolean): cardinal;
    public
        constructor Create();
        destructor Destroy; override;

        procedure ExecuteRunStartWithNameAndParams(const aSituationLogText, aRunName: string;
            const aParams: TKeyArgValueList); override;
        function RestartThreadByTraceName(const aThreadTraceName: string; const aCloseHandleOnFinish: boolean)
            : cardinal; override;
        procedure StartThreadedMethod(const aMethodName: string; const aParams: TKeyArgValueList;
            const aIsMainThread: boolean); override;
        function StartJoinableThreadedMethod(const aMethodName: string; const aParams: TKeyArgValueList)
            : cardinal; override;
        function JoinThread(const aThreadSysHandleID: cardinal; const aMaxWaitTime: cardinal)
            : boolean; override;
        procedure JumpLocal(const aAddress: integer); override;
        procedure StoreParserIdentRWValue(const aKey, aValue: string); override;
        function GetCurrentSubMethodName: string; override;
        procedure DisplayComponentSimpleEvent(const aSender: IDisplayComponent;
            const aEventName: string); override;
        procedure AddStepsToPendingRunSteps(const aRunStepList: TObject; const aTrace: boolean); override;
        procedure PendingRunStepsMoveCursor(const aCursorPos: integer); override;
    end;


implementation


uses
    SysUtils,
    ThreadAPI,
    MethodCompiledFile,
    Executable,
    RunStep,
    LogManager,
    DisplayComponentEventHandler;

{ TRunEventManager }

constructor TRunEventManager.Create;
begin
    inherited;
    fReturnValueList := ReturnKeyValuesDictionary.Create;
end;

destructor TRunEventManager.Destroy;
begin
    FreeAndNil(fReturnValueList);

    inherited;
end;

procedure TRunEventManager.StoreParserIdentRWValue(const aKey, aValue: string);
begin
    TMethodEvalTable.SetIdentValDefineType(aKey, aValue);
end;

procedure TRunEventManager.EnterReturnKeyValuesToList(aSender: TObject;
    aReturnKeyValues: TArray<TStreamableItem>);
begin

end;

procedure TRunEventManager.ExecuteRunStartWithNameAndParams(const aSituationLogText, aRunName: string;
    const aParams: TKeyArgValueList);
var
    xHandler: TExecutable;
    xStartInfo: TRunStartInfo;
begin
    gLogManager.Log('Event >' + aSituationLogText + '< Name: ' + aRunName, true);
    // + ' - Parameter: ' +  TMethodGUIParser.KeyValueParamArrayToStr( aParams ),;

    xStartInfo.IsRestart := false;
    xStartInfo.MethodName := aRunName;
    xStartInfo.Params := aParams;

    xHandler := TEventLiveRunHandler.Create(xStartInfo, true);
    try
        xHandler.Execute();
    finally
        xHandler.Free;
    end;
end;

function TRunEventManager.CreateThreadIntern(const aThreadTraceName: string; const aStartInfo: TRunStartInfo;
    const aCreateSuspended, aCloseHandleOnFinish: boolean): cardinal;
var
    xActionHandler: TLiveRunHandler;
begin
    xActionHandler := TLiveRunHandler.Create(aStartInfo, false, EnterReturnKeyValuesToList);
    result := TThreadAPI.CreateThread(aThreadTraceName, aCreateSuspended, aCloseHandleOnFinish, true,
        xActionHandler);
end;

function TRunEventManager.RestartThreadByTraceName(const aThreadTraceName: string;
    const aCloseHandleOnFinish: boolean): cardinal;
var
    xStartInfo: TRunStartInfo;
begin
    // if isrestart, other StartInfo fields will not be used
    xStartInfo.IsRestart := true;
    result := CreateThreadIntern(aThreadTraceName, xStartInfo, false, aCloseHandleOnFinish);
end;

function TRunEventManager.StartThreadedMethodIntern(const aMethodName: string;
    const aParams: TKeyArgValueList; const aIsMainThread: boolean;
    const aCreateSuspended, aCloseHandleOnFinish: boolean): cardinal;
var
    xStartInfo: TRunStartInfo;
begin
    xStartInfo.IsMainThread := aIsMainThread;
    xStartInfo.IsRestart := false;
    xStartInfo.MethodName := aMethodName;
    xStartInfo.Params := aParams;
    result := CreateThreadIntern('T~' + aMethodName, xStartInfo, aCreateSuspended, aCloseHandleOnFinish);
end;

procedure TRunEventManager.StartThreadedMethod(const aMethodName: string; const aParams: TKeyArgValueList;
    const aIsMainThread: boolean);
begin
    StartThreadedMethodIntern(aMethodName, aParams, aIsMainThread, false, true);
end;

function TRunEventManager.StartJoinableThreadedMethod(const aMethodName: string;
    const aParams: TKeyArgValueList): cardinal;
begin
    result := StartThreadedMethodIntern(aMethodName, aParams, false, false, false);
end;

function TRunEventManager.JoinThread(const aThreadSysHandleID: cardinal;
    const aMaxWaitTime: cardinal): boolean;
begin
    result := true;
    try
        result := TThreadAPI.JoinThread(aThreadSysHandleID, aMaxWaitTime);
    finally
        if result then
            TThreadAPI.CloseSysHandle(aThreadSysHandleID);
    end;
end;

procedure TRunEventManager.JumpLocal(const aAddress: integer);
begin
    TRunStepBuilderProcessor.JumpLocal(aAddress);
end;

function TRunEventManager.GetCurrentSubMethodName: string;
begin
    result := TRunStepBuilderProcessor.GetCurrentSubMethodName();
end;

procedure TRunEventManager.DisplayComponentSimpleEvent(const aSender: IDisplayComponent;
    const aEventName: string);
var
    xContextID: string;
    xDisplayComponentEventHandlerIntf: IDisplayComponentEventHandler;
begin
    xContextID := aSender.ContextID + ' DisplayEventHandler';
    xDisplayComponentEventHandlerIntf := TDisplayComponentEventHandlerManager.Instance.FindEventHandler
        (xContextID);
    ASSERT(Assigned(xDisplayComponentEventHandlerIntf));
    xDisplayComponentEventHandlerIntf.RequestExecuteEvent(aSender, aEventName, false);
end;

procedure TRunEventManager.AddStepsToPendingRunSteps(const aRunStepList: TObject; const aTrace: boolean);
begin
    ASSERT(aRunStepList is TRunStepList);
    TRunStepBuilderProcessor.AddStepsToPendingStepsForCurrentThread(aRunStepList as TRunStepList, aTrace);
end;

procedure TRunEventManager.PendingRunStepsMoveCursor(const aCursorPos: integer);
begin
    TRunStepBuilderProcessor.PendingStepsMoveCursorForCurrentThread(aCursorPos);
end;

{ ReturnKeyValuesDictionary }

destructor ReturnKeyValuesDictionary.Destroy;
var
    xPair: TPair<cardinal, TArray<TStreamableItem>>;
    x: integer;
begin
    for xPair in self do
    begin
        for x := 0 to high(xPair.Value) do
            xPair.Value[x].Free;
    end;

    inherited;
end;


end.
